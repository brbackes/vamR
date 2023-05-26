## VAM R Package

### Installation 
To use the development version (which is the only version), you
can install from [GitHub](https://github.com/brbackes/vamR/) with:

``` r
if (!require("pak")) install.packages("pak")
pak::pak("brbackes/vamR")
```
### Usage (short)

Example usage:

``` r
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
```

Please see the help file by typing `?vam` in R.

### How can we know that `vam.ado` and `vamR` produce identical results?

Whenever the package is checked, it automatically runs a test that ensures identical output across `vamR` and `vam.ado`. 
This test estimates teacher value-added by subject (math and ELA) and level (elementary and middle) in Massachusetts using data from grades 4-8 in 2012-2022 and raises
an error if any changes to the package cause the output from `vamR` to differ from `vam.ado`.

* [Here is the Stata .do file that prepares the data and runs `vam.ado`](https://github.com/brbackes/vamR/blob/master/data-raw/vam_sample.do)
* [Here is the .R file that loads the data, runs `vamR`, and compares the output to `vam.ado`](https://github.com/brbackes/vamR/blob/master/tests/testthat/test-replicate_stata.R)
* The test raises an error if the correlation between value-added estimates in `vam.ado` and `vamR` is less than 0.99999 in any subject-level combination
* The test raises an error if the number of non-missing teacher-year-subject-level value-added estimates differs across `vam.ado` and `vamR`
* Note that the underlying data live on our secure server; I cannot share or upload the data

### Usage (longer)

The trick to replicating Stata's `vam.ado` output is making sure the residualization regression in R's `fixest` gives the exact same output as Stata's `areg` (ignoring the constant term 
which is not displayed in `fixest` when there are fixed effects).
This sounds obvious, but if you have a complicated set of covariates and interactions, it is not always trivial. Here is an example workflow that has worked for me. Note that saving a .csv from Stata
and using `fread` to read in the dataset is much faster than reading the .dta file into R with `haven`. Of course, this is only relevant if you have used Stata to clean and prepare the data, or want to compare output from `vam.ado` 
and `vamR`.

Preparing the set of covariates:

``` r
df <- data.table::fread("1_data/output/vam_merge_for_va.csv")

# prepare covariates: higher-order polynomials and grade*year interaction
df_prepped <- df %>%
  mutate(
    lag_mat_2 = lag_mat_nce ^ 2,
    lag_mat_3 = lag_mat_nce ^ 3,
    lag_ela_2 = lag_ela_nce ^ 2,
    lag_ela_3 = lag_ela_nce ^ 3,
    lag_std_noncog_factor_2 = lag_std_noncog_factor ^ 2,
    lag_std_noncog_factor_3 = lag_std_noncog_factor ^ 3,
    scx_voc2 = scx_voc ^ 2,
    scx_voc3 = scx_voc ^ 3
  ) %>%
  group_by(grade, syear) %>%
  mutate(grade_year = cur_group_id() %>% as_factor()) %>%
  ungroup()

# some individual-level covariates that will be interacted with school year and subject
controls_ind <- df_prepped %>%
  dplyr::select(
    starts_with("lag_mat_"),
    starts_with("lag_ela_"),
    starts_with("lag_std_noncog_")
    ) %>%
  tbl_vars()
  
# some individual-level covariates that will be interacted with subject only
controls_ind2 <- df_prepped %>%
  dplyr::select(
    starts_with("vb_"),
    starts_with("sped"),
    lep, male, frl, adv_math, art_elec, fgn_lang, supp_crs, esl_crs,
    grade_year
  ) %>%
  tbl_vars()

# same stuff for class-level controls. Specifying separately like this
# can be helpful when estimating different models that have different sets of covariates
controls_class <- df_prepped %>%
  dplyr::select( 
    crx_lag_mat_nce, crx_lag_ela_nce, crx_lag_std_noncog_factor
    ) %>%
  tbl_vars()

controls_class2 <- df_prepped %>%
  dplyr::select( 
    starts_with("crx_vb_"),
    starts_with("crx_sped"),
    crx_lep, crx_male, crx_frl
    ) %>%
  tbl_vars()

# here is how to mash everything together with different sets of interactions with different sets of controls
reg_controls <- 
  c(
    paste(paste("i(syear, ref = 2018) * i(subject, ref = 'ELA') * ", c(controls_ind, controls_class))),
    paste(paste("i(subject, ref = 'ELA') * ", c(controls_ind2, controls_class2)))
  ) %>%
  reformulate()
```

Below is how to to verify the output from `fixest` (this is what `vamR` uses under the hood). In the below, `split`
needs to be included if you're going to include something in the `by()` portion of `vam`.

``` r
model <- fixest::feols(outcome ~ .[reg_controls] | teacher_id, data = df_prepped, split = ~.group_id)
```

The output (i.e., coefficients and number of observations) in each group should be identical to Stata's `areg` absorbing the teacher ID with the same controls and interaction terms. 
**If this is not the case then the output of `vam.ado` and `vamR` will differ.**

To make sure output is identical, I've found it helpful to manually specify the omitted group as in above (e.g., `i(syear, ref = 2018)`). Otherwise, `fixest` can do strange things with
omitted groups and interaction terms. You can check the omitted group used by `areg` by running the regression in `areg` or just assuming it'll omit the first group.

### Some more tips

* `quasi` hasn't been implemented yet
* If you have a large dataframe with lots of missing outcome variables or covariates, you can speed things up by dropping them before feeding the dataframe to `vamR`. This is because the program carries around the entire dataframe (for now. Hopefully this will be fixed in the future).
* If R complains about anything related to how the interaction terms `i(*)` are specified, try making sure the `fixest` package is loaded with `library(fixest)`.

### Why is it faster?

For anyone curious, a look under the hood at why things are faster in `fixest` than `vam.ado`. Annecdotally, I've found `vamR` to be 4-5 times faster than `vam.ado`, with larger speed gains coming when more `by()` groups are included and the residualization regression is more complicated, especially with interaction terms. Before getting into it, note that `vam.ado` was extremely well-written given the constraints at the time (a Stata package written in 2013); this is not a criticism! This package would not exist if `vam.ado` weren't so (a) useful and (b) clearly written.

* `fixest` is much, much faster than `areg`
* With the exception of calculating the actual `tv` estimates, everything is done all at once instead of separately by each `by()` group
* Most of the calculations use `data.table` as a backend, which is extremely fast for collapsing and manipulating data
* The biggest bottleneck right now is the calculation of the `tv` estimates, which is somewhat slow and relies on a bunch of matrix manipulation. Suggestions on how to speed this up welcome!

### Acknowledgements

Thank you to Michael Stepner for the guidance provided by `vam.ado` and to James Cowan for help with using `fixest` with complicated regressors.
