// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// Forecast VA in year `target` from the teacher-year rows in `rows`:
// M[target, yrs] * inv(M[yrs, yrs] + diag(1 / weight)) * scores
static double forecast(const std::vector<arma::uword>& rows, const arma::uvec& yr,
                       const arma::vec& score, const arma::vec& weight,
                       const arma::mat& M, arma::uword target) {
  arma::uvec idx(rows);
  arma::uvec yrs = yr.elem(idx);

  arma::mat S = M.submat(yrs, yrs);
  S.diag() += 1 / weight.elem(idx);

  arma::rowvec m_row = M.row(target);
  arma::vec m = m_row.elem(yrs);

  return arma::dot(m, arma::solve(S, score.elem(idx)));
}

// Leave-out VA for every teacher x target year within one by-group.
//
// Rows are teacher-years sorted by teacher, with `tid` identifying the teacher.
// `yr` and `targets` are 1-based year indices into M. Rows with a missing score
// are ignored. A teacher-target pair is returned only if the teacher has at
// least one scored year other than the target.
//
// Returns 1-based `row` (first row of the teacher) and `target` (index into
// `targets`) with the leave-one-year-out forecast `tv`. If `quasi`, also
// `tv_f` (also leaves out target + 1) and `tv_l` (also leaves out target - 1).
// [[Rcpp::export(cpp_tv)]]
List cpp_tv(IntegerVector tid, IntegerVector yr, NumericVector score,
            NumericVector weight, IntegerVector targets, arma::mat M,
            bool quasi) {
  const int n = tid.size();
  arma::uvec yr0(n);
  for (int i = 0; i < n; ++i) yr0[i] = yr[i] - 1;
  const arma::vec score_v(score.begin(), n, false);
  const arma::vec weight_v(weight.begin(), n, false);

  std::vector<int> out_row, out_target;
  std::vector<double> out_tv, out_f, out_l;
  std::vector<arma::uword> scored, base, lead_out, lag_out;

  int start = 0;
  while (start < n) {
    int end = start;
    while (end < n && tid[end] == tid[start]) ++end;

    scored.clear();
    for (int i = start; i < end; ++i) {
      if (!ISNAN(score[i])) scored.push_back(i);
    }

    for (int t = 0; t < targets.size(); ++t) {
      base.clear();
      lead_out.clear();
      lag_out.clear();
      for (arma::uword i : scored) {
        int d = yr[i] - targets[t];
        if (d == 0) continue;
        base.push_back(i);
        if (d != 1) lead_out.push_back(i);
        if (d != -1) lag_out.push_back(i);
      }
      if (base.empty()) continue;

      arma::uword target = targets[t] - 1;
      out_row.push_back(start + 1);
      out_target.push_back(t + 1);
      out_tv.push_back(forecast(base, yr0, score_v, weight_v, M, target));
      if (quasi) {
        out_f.push_back(lead_out.empty() ? NA_REAL : forecast(lead_out, yr0, score_v, weight_v, M, target));
        out_l.push_back(lag_out.empty() ? NA_REAL : forecast(lag_out, yr0, score_v, weight_v, M, target));
      }
    }

    start = end;
  }

  return List::create(
    _["row"] = out_row,
    _["target"] = out_target,
    _["tv"] = out_tv,
    _["tv_f"] = out_f,
    _["tv_l"] = out_l
  );
}
