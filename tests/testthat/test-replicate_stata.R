test_that("Identical output generated from vam.ado and vamR", {
  
  # compare vam results with pre-computed stata results from vam.ado
  # this only works with data saved locally (because I cannot put it on github)
  
  raw_df <- data.table::fread("../../../vamR_test_data/vam_data.csv") |>
    tibble::as_tibble()
  
  df_prepped <- raw_df |>
    dplyr::mutate(
      lag_mat_2 = lag_mat_nce ^ 2,
      lag_mat_3 = lag_mat_nce ^ 3,
      lag_ela_2 = lag_ela_nce ^ 2,
      lag_ela_3 = lag_ela_nce ^ 3,
      lag_std_noncog_factor_2 = lag_std_noncog_factor ^ 2,
      lag_std_noncog_factor_3 = lag_std_noncog_factor ^ 3,
      factor_syear = factor(syear)
    ) |>
    dplyr::ungroup()
  
  controls <- df_prepped |>
    dplyr::select(
      tidyselect::starts_with("lag_mat_"),
      tidyselect::starts_with("lag_ela_"),
      tidyselect::starts_with("lag_std_noncog_"),
      tidyselect::starts_with("sped"),
      tidyselect::starts_with("crx_sped"),
      lep, male, frl, 
      asian, black, hpi, hisp, amind, mult,
      crx_asian, crx_black, crx_hpi, crx_hisp, crx_amind, crx_mult,
      crx_lep, crx_male, crx_frl, 
      crx_lag_mat_nce, crx_lag_ela_nce, crx_lag_std_noncog_factor,
      factor_syear
    ) |>
    dplyr::tbl_vars()
  
  controls <- paste(paste("i(grade, ", controls), ")") |>
    reformulate()
  
  # teacher fe
  ret <- vam(by = c("lvl", "subject"), 
             data = df_prepped, 
             controls = controls, 
             teacher = "mepid",
             class = "section_id",
             year = "syear",
             tfx_resid = "mepid", 
             driftlimit = 7,
             y = "test",
             return_df_only = FALSE,
             tv_name = "tv_R",
             scores_name = "score_r_R"
  )
  
  
  # created in the stata file in same dir
  tv <- haven::read_dta("../../../vamR_test_data/tv.dta")
  
  tvr <- ret[[4]] |>
    dplyr::mutate(subject = ifelse(subject == "ELA", 51, 52))
  
  df <- tv |>
    dplyr::rename(
      tv_stata = tv
    ) |>
    dplyr::left_join(tvr, by = c("mepid", "syear", "lvl", "subject")) |>
    dplyr::filter(!is.na(tv_R))

  cors <- df |>
    dplyr::group_by(subject, lvl) |>
    dplyr::summarize(va_cor=cor(tv_R, tv_stata), tch_yr_obs = dplyr::n(), .groups = "keep") |>
    dplyr::arrange(lvl, subject) |>
    as.data.frame()
  
  # test 1: all correlations (by subject / level) greater than 0.99999
  expect_gt(min(cors$va_cor), 0.99999)  
  
  # test 2: matched at least 25k teachers in every subject / level
  expect_gt(min(cors$tch_yr_obs), 25000)
  
})
