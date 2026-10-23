# Estimate Teacher Value-Added

This program uses student-level outcomes (typically test scores) to
compute teacher value-added (VA) measures. The estimation procedure
accounts for drift in teacher VA over time. This package is based on
Michael Stepner's Stata program and intended to produce identical
results with the speed gains made possible by R.

## Usage

``` r
vam(
  y = NULL,
  teacher = NULL,
  year = NULL,
  class = NULL,
  controls = NULL,
  by = NULL,
  tfx_resid = NULL,
  absorb = NULL,
  driftlimit = NULL,
  data = NULL,
  quasi = FALSE,
  tv_name = "tv",
  scores_name = "score_r",
  cfr_test = FALSE,
  cfr_school = NULL,
  cfr_grade = NULL,
  cfr_subject = NULL,
  cfr_weight = NULL,
  return_df_only = FALSE,
  return_cfr_test_only = FALSE,
  return_data = TRUE,
  show_regressions = FALSE
)
```

## Arguments

- y:

  Required. The outcome variable in quotations.

- teacher:

  Required. The name of the teacher identifier in quotations.

- year:

  Required. The name of the year identifier in quotations.

- class:

  Required. The name of the classroom identifier in quotations.

- controls:

  Controls to be used in the residualization.

- by:

  Perform VA estimation separately for each by-group. Ex by =
  c("subject", "level").

- tfx_resid:

  Absorb fixed effects during y residualization, but include those fixed
  effects in the residual.

- absorb:

  Residualize y on absorbed fixed effects.

- driftlimit:

  Estimate only \# autocovariances; set all further autocovariances
  equal to the last estimate.

- data:

  Required. Data frame to use.

- quasi:

  Default FALSE. Generates two additional leave-out VA measures, which
  are typically used for quasi-experimental tests. This option adds
  `tv_2yr_l`, which leaves out the forecast year and the prior year, and
  `tv_2yr_f`, which leaves out the forecast year and the following year.
  Note that if `tv_name` is specified, those names will also apply to
  the `_2yr_f` and `_2yr_l` variables.

- tv_name:

  Default "tv". String denoting the name of the value-added forecast.

- scores_name:

  Default "score_r". String denoting the name of the score residual.

- cfr_test:

  Default FALSE. Conducts the CFR quasi-experimental test for forecast
  bias in value-added estimates. Collapses data to
  school-grade-year-subject cells with average `y` and average
  value-added. Regresses change in `y` on change in leave-out
  value-added, using the 2-year leave-outs generated with the `quasi`
  command as in CFR. Regression includes year fixed effects and clusters
  standard errors by school-cohort. If this is TRUE, you must specify
  `cfr_school`, `cfr_grade`, and `cfr_subject` so that the data can be
  collapsed appropriately. If this is TRUE, the `quasi` estimates will
  automatically be calculated.

- cfr_school:

  Required if `cfr_test` is TRUE. Name of the school variable used in
  the collapse for the `cfr_test`. Not used in value-added calculations.

- cfr_grade:

  Required if `cfr_test` is TRUE. Name of the grade variable used in the
  collapse for the `cfr_test`. Not used in value-added calculations.

- cfr_subject:

  Required if `cfr_test` is TRUE. Name of the subject variable used in
  the collapse for the `cfr_test`. Not used in value-added calculations.

- cfr_weight:

  Optional if `cfr_test` is TRUE. Name of the teacher weight variable
  used in the collapse for `cfr_test` (typically a teacher dosage
  variable greater than 0 and no greater than 1). Not used in the
  value-added calculations.

- return_df_only:

  Default FALSE. If TRUE, return only the original dataframe with
  value-added and residuals appended. If FALSE, return a named list with
  `Parameters` (the variance estimates), `Covariances` (the
  autocovariances by lag), `Value_Added` (a teacher-year-(by) dataset
  with the value-added estimates), `CFR_Test` (if `cfr_test` is TRUE),
  `Specification` (the arguments used, with `driftlimit` filled in if it
  was not specified), and, if `return_data` is TRUE, `Data` (the
  original data with value-added and residuals appended). Note that
  `Value_Added` will include teacher estimates for years in which they
  were not originally present in the data: this is a difference from the
  Stata version. To restrict to years in which a teacher was in the
  data, use `Data`.

- return_data:

  Default TRUE. If TRUE, the returned list includes `Data`, the original
  data with value-added and residuals appended. This is usually most of
  the size of the result, so set it to FALSE when estimating many
  models. Cannot be specified together with `return_df_only` or
  `return_cfr_test_only`.

- return_cfr_test_only:

  Default FALSE. If TRUE, return only the coefficient and standard error
  of the `cfr_test` result. Useful for checking the bias of many
  different specifications without having to hold each resulting set of
  `tv` estimates in memory.

## Value

A dataframe or list depending on `return_df_only` and
`return_cfr_test_only`.

## Examples

``` r
# \donttest{
try({
returned <- vam(
  by = c("lvl", "subject"), 
  data = df_prepped, 
  controls = controls, 
  teacher = "mepid",
  class = "section_id",
  year = "syear",
  tfx_resid = "mepid", 
  driftlimit = 7,
  y = "test",
  return_df_only = FALSE,
  tv_name = "tv",
  scores_name = "score_r"
)
})
#> ── VAM estimation ─────────────────────────────────── vamR version 0.0.0.9011 ──
#> ℹ Initial function checks.
#> ✖ Initial function checks. [9ms]
#> 
#> Error in eval(expr, envir) : object 'df_prepped' not found
# }
```
