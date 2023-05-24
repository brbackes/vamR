library(data.table)
library(tidyverse)



# created in the stata file in same dir
raw_df <- data.table::fread("data-raw/vam_data.csv") |>
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


# prepare covariates

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


# Interact with grade
controls = paste(paste("i(grade, ", controls), ")") |>
  reformulate()

controls

# controls <- "lag_mat_nce"
df_prepped$subject
df_prepped$lvl

# library(data.table)

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
tv <- haven::read_dta("data-raw/tv.dta")

tvr <- ret[[4]] |>
  dplyr::mutate(subject = ifelse(subject == "ELA", 51, 52))

df <- tv |>
  dplyr::rename(
    tv_stata = tv
  ) |>
  dplyr::left_join(tvr, by = c("mepid", "syear", "lvl", "subject")) |>
  dplyr::filter(!is.na(tv_R))

df

cor(df |> dplyr::pull(tv_R), df |> dplyr::pull(tv_stata))

# 
# 
# cor(df %>% filter(lvl == 0, subject == 51) %>% pull(tv_R), df %>% filter(lvl == 0, subject == 51) %>% pull(tv_stata))
# cor(df %>% filter(lvl == 0, subject == 52) %>% pull(tv_R), df %>% filter(lvl == 0, subject == 52) %>% pull(tv_stata))
# cor(df %>% filter(lvl == 1, subject == 51) %>% pull(tv_R), df %>% filter(lvl == 1, subject == 51) %>% pull(tv_stata))
# cor(df %>% filter(lvl == 1, subject == 52) %>% pull(tv_R), df %>% filter(lvl == 1, subject == 52) %>% pull(tv_stata))



