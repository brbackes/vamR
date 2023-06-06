// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(cpp_tv)]]
arma::vec cpp_tv(List L, arma::mat M) {
  double length = L.length();
  arma::vec x(length) ;
  for (int i = 0; i < L.length(); ++i) {
    List df = L[i];
    NumericVector tmp = df[4];
    int year_i = tmp[0];
    arma::vec scores = df[1];
    arma::vec weight = df[2];
    arma::vec yrs = df[3];
    
    arma::mat A(scores.n_rows, M.n_cols);
    for (int j = 0; j < scores.n_rows; ++j) {
      A(j, yrs(j)-1) = 1;
    }
    
    arma::mat term1 = M.row(year_i - 1) * A.t() * inv(A * M * A.t() + arma::diagmat(1 / weight)) * scores;
    x(i) = term1(0,0);
    
  }
  
  return(x);
  
}