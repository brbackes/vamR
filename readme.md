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
model <- fixest::feols(outcome ~ .[reg_controls] | mepid, data = df_prepped, split = ~.group)
```

The output (i.e., coefficients and number of observations) in each group should be similar to Stata's `areg` absorbing the teacher ID with the same controls and interaction terms. 
**If this is not the case then the output of `vam.ado` and `vamR` will differ.

To make sure output is identical, I've found it helpful to manually specify the omitted group as in above (e.g., `i(syear, ref = 2018)`). Otherwise, `fixest` can do strange things with
omitted groups and interaction terms. You can check the omitted group used by `areg` by running the regression in `areg` or just assuming it'll omit the first group.

### Some more tips

* If you have a large dataframe with lots of missing outcome variables or covariates, you can speed things up by dropping them before feeding the dataframe to `vamR`. This is because the program carries around the entire dataframe (for now. Hopefully this will be fixed in the future).

