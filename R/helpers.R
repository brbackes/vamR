
is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# leave-out value added for one by-group
#
# tch_yr: teacher-year data for one group, keyed by .teacher and .year, with
#   class_mean, weight, and n_tested
# M: autocovariance matrix (lag 0 to data span) for the group
# quasi: also compute the two-year leave-outs
#
# returns one row per teacher x year (any year in the group's data) for which
# the teacher has a scored year other than that year
compute_tv <- function(tch_yr, M, quasi = FALSE) {

  years <- sort(unique(tch_yr$.year))
  year_index <- min(years) - 1

  res <- cpp_tv(
    tid = rleid(tch_yr$.teacher),
    yr = as.integer(tch_yr$.year - year_index),
    score = as.numeric(tch_yr$class_mean),
    weight = as.numeric(tch_yr$weight),
    targets = as.integer(years - year_index),
    M = M,
    quasi = quasi
  )

  out <- data.table(
    .teacher = tch_yr$.teacher[res$row],
    .year = years[res$target],
    tv = res$tv
  )
  if (quasi) {
    out[, `:=`(tv_2yr_f = res$tv_f, tv_2yr_l = res$tv_l)]
  }

  out
}
