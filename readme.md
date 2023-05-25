To use the development version (which is the only version), you
can install from [GitHub](https://github.com/brbackes/vamR/) with:

``` r
if (!require("pak")) install.packages("pak")
pak::pak("brbackes/vamR")
```

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
