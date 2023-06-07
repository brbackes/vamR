test_that("Identical output generated from vam.ado and vamR", {
  
  # compare vam results with pre-computed stata results from vam.ado
  # this only works with data saved locally (because I cannot put it on github)
  
  # created here: https://github.com/brbackes/vamR/blob/master/data-raw/vam_sample.do
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
             quasi = TRUE,
             cfr_test = TRUE,
             cfr_school = "sch_code",
             cfr_grade = "grade",
             cfr_subject = "subject",
             cfr_weight = "teach_weight",
             tv_name = "tv_R",
             scores_name = "score_r_R"
  )
  
  # created here: https://github.com/brbackes/vamR/blob/master/data-raw/vam_sample.do
    tv <- haven::read_dta("../../../vamR_test_data/tv.dta")
    cfr_stata <- data.table::fread("../../../vamR_test_data/cfr.csv") |> tibble::as_tibble() |> dplyr::select(1:3)
  
  tvr <- ret[[1]] |>
    dplyr::mutate(subject = ifelse(subject == "ELA", 51, 52)) |>
    dplyr::select(lvl, subject, mepid, syear, tv_R, tv_R_2yr_f, tv_R_2yr_l) |>
    dplyr::group_by(lvl, subject, mepid, syear) |>
    dplyr::slice_head(n = 1)
  
  # equal number of teacher-year-subject-level non-missing tv obs
  # among teachers present in given year in original data
  expect_equal(
    nrow(tv  |> dplyr::filter(!is.na(tv))), 
    nrow(tvr |> dplyr::filter(!is.na(tv_R)))
  )
  
  # equal leave 2 yr out lag
  expect_equal(
    nrow(tv  |> dplyr::filter(!is.na(tv_2yr_l))), 
    nrow(tvr |> dplyr::filter(!is.na(tv_R_2yr_l)))
  )
  
  # equal leave 2 yr out lead
  expect_equal(
    nrow(tv  |> dplyr::filter(!is.na(tv_2yr_f))), 
    nrow(tvr |> dplyr::filter(!is.na(tv_R_2yr_f)))
  )
  
  df <- tv |>
    dplyr::full_join(tvr, by = c("mepid", "syear", "lvl", "subject")) |>
    dplyr::filter(!is.na(tv_R))
  
  # test 2: the number of matched non-missing obs is the same as the number
  # of non-missing obs from vam.ado
  expect_equal(
    nrow(tv  |> dplyr::filter(!is.na(tv))), 
    nrow(df)
  )

  # base tv
  cors <- df |>
    dplyr::group_by(subject, lvl) |>
    dplyr::summarize(va_cor=cor(tv_R, tv), tch_yr_obs = dplyr::n(), .groups = "keep") |>
    dplyr::arrange(lvl, subject) |>
    as.data.frame()
  
  # all correlations (by subject / level) greater than 0.99999
  expect_gt(min(cors$va_cor), 0.99999)  
  
  # matched at least 25k teachers in every subject / level (probably redundant with test 2)
  expect_gt(min(cors$tch_yr_obs), 25000)
  
  # 2yr tv l
  cors <- df |>
    dplyr::filter(!is.na(tv_R_2yr_l)) |>
    dplyr::group_by(subject, lvl) |>
    dplyr::summarize(va_cor=cor(tv_R_2yr_l, tv_2yr_l), tch_yr_obs = dplyr::n(), .groups = "keep") |>
    dplyr::arrange(lvl, subject) |>
    as.data.frame()
  
  # all correlations (by subject / level) greater than 0.99999
  expect_gt(min(cors$va_cor), 0.99999)  
  
  # matched at least 25k teachers in every subject / level (probably redundant with test 2)
  expect_gt(min(cors$tch_yr_obs), 25000)
  
  # 2yr tv f
  cors <- df |>
    dplyr::filter(!is.na(tv_R_2yr_f)) |>
    dplyr::group_by(subject, lvl) |>
    dplyr::summarize(va_cor=cor(tv_R_2yr_f, tv_2yr_f), tch_yr_obs = dplyr::n(), .groups = "keep") |>
    dplyr::arrange(lvl, subject) |>
    as.data.frame()
  
  # all correlations (by subject / level) greater than 0.99999
  expect_gt(min(cors$va_cor), 0.99999)  
  
  # matched at least 25k teachers in every subject / level (probably redundant with test 2)
  expect_gt(min(cors$tch_yr_obs), 25000)
  
  # quasi-experimental test
  cfr_r <- ret[[5]]
  expect_equal(cfr_r$quasi_b, cfr_stata$b, tolerance = 0.0001)
  
})
