library(data.table)
library(dtplyr)
library(tidyverse)
library(haven)
library(fixest)

# created in the stata file in same dir
raw_df <- haven::read_dta("vam_data.dta")

vam <- function(
    y = NULL,
    teacher = NULL,
    year = NULL,
    class = NULL,
    controls = NULL,
    by = NULL,
    tfx_resid = NULL,
    absorb = NULL,
    driftlimit = NULL,
    quasi = FALSE,
    data = NULL
) {
  
  tictoc::tic("vam")
  
  #########################################
  # function stuff and checks
  
  if ("tv" %in% names(data)) {
    stop("The dataset loaded in memory when vam is run cannot have a variable named tv.")
  }
  
  if ("score_r" %in% names(data)) {
    stop("The dataset loaded in memory when vam is run cannot have a variable named score_r.")
  }
  
  if ("tv_2yr_l" %in% names(data) & quasi == TRUE) {
    stop("The dataset loaded in memory when vam is run cannot have a variable named tv_2yr_l.")
  }
  
  if ("tv_2yr_f" %in% names(data) & quasi == TRUE) {
    stop("The dataset loaded in memory when vam is run cannot have a variable named tv_2yr_f")
  }
  
  if ("tv_ss" %in% names(data) & quasi == TRUE) {
    stop("The dataset loaded in memory when vam is run cannot have a variable named tv_ss")
  }
  
  if (!is.null(absorb) & !is.null(tfx_resid)) {
    stop("Cannot specify an absorb variable and a tfx_resid variable simultaneously.")
  }
  
  if (is.null(data)) {
    stop("You must provide a dataframe using the data argument.")
  }
  
  # handle the "by" variables by making a .group variable for convenience
  if (!is.null(by)) {
    reg_df <- data %>%
      dplyr::group_by(!!!rlang::syms(by)) %>%
      mutate(.group = cur_group_id()) %>%
      ungroup()
    
    # list of groups and .group for later convenience
    groups <- reg_df %>%
      group_by(.group) %>%
      slice_head(n = 1) %>% 
      select(.group, !!!rlang::syms(by))
  } else {
    message("No by variables provided. All calculations will be run on full data.")
    reg_df <- data %>%
      mutate(.group = 1) %>%
      ungroup()
  }
  
  n_groups <- max(reg_df$.group)


  #########################################
  # residualization
  
  # Turn into a fixest control vector
  setFixest_fml(..ctrl = controls)
  setFixest_fml(..lhs = paste0(y) %>% reformulate())

  # teacher fe
  if (!is.null(tfx_resid)) {
    tch <- sym(tfx_resid)
    message("Residualizing with teacher FE")
    model <- feols(..lhs ~ ..ctrl | .[tfx_resid], data = reg_df, split = ~.group)

    resids <- resid(model, type = "response", na.rm = FALSE)
    preds <- map_df(1:n_groups, ~{
      
      reg_df$score_r <- resids[, .x]
      reg_df <- reg_df %>% 
        filter(.group == .x)
      
      # add in the teacher fe
      reg_df$tfe <-    predict(as.list(model)[[.x]], reg_df, fixef = TRUE) %>% pull({{tfx_resid}})
      reg_df %>%
        mutate(score_r = score_r + tfe)
      
    })
    
    pars <- map_df(1:n_groups, ~{
      tibble::tibble(
        .group = .x, 
        npar = model[[.x]]$coefficients %>% length(),
        nobs = model[[.x]]$nobs
        )
    })
  }
  
  # other fe that's not teacher
  if (!is.null(absorb)) {
    message(glue::glue("Residualizing with a non-teacher FE: {absorb}"))
    model <- feols(..lhs ~ ..ctrl | .[absorb], data = reg_df, split = ~.group)
    
    resids <- resid(model, type = "response", na.rm = FALSE)
    preds <- map_df(1:n_groups, ~{
      
      reg_df$score_r <- resids[, .x]
      reg_df %>% filter(.group == .x)
      
    })
    
    pars <- map_df(1:n_groups, ~{
      tibble::tibble(
        .group = .x, 
        npar = model[[.x]]$nparams,
        nobs = model[[.x]]$nobs
        )
    })
  
  }
  
  # no fe
  if (is.null(absorb) & is.null(tfx_resid)) {
    message("Residualizing with no fixed effects")
    model <- feols(..lhs ~ ..ctrl, data = reg_df, split = ~.group)
    
    resids <- resid(model, type = "response", na.rm = FALSE)
    preds <- map_df(1:n_groups, ~{
      
      reg_df$score_r <- resids[, .x]
      reg_df %>% filter(.group == .x)
      
    })
    
    pars <- map_df(1:n_groups, ~{
      tibble::tibble(
        .group = .x, 
        npar = model[[.x]]$coefficients %>% length(),
        nobs = model[[.x]]$nobs
      )
    })
  }
  
  #########################################
  # calculate total and individual variances
  
  # start using data.table. this is way, way faster than dplyr
  message("Moving data to data.table and computing variances")
  dt <- data.table(preds)
  
  dt[, `:=`(c("n_tested", "class_mean", 
              "index", "individual_dev_from_class"), {
                n_tested <- sum(!is.na(score_r))
                class_mean <- mean(score_r, na.rm = TRUE)
                index <- 1:.N
                individual_dev_from_class <- score_r - class_mean
                .(n_tested, class_mean, index, individual_dev_from_class)
              }), by = .(.group, get(teacher), get(year), get(class))]
  preds <- as_tibble(dt)
  
  # calculate more parameters
  pars <- map_df(1:n_groups, ~{
    filtered <- dt[.group == .x]
    tibble::tibble(
      v = var(filtered$score_r, na.rm = TRUE),
      class_v = var(filtered$individual_dev_from_class, na.rm = TRUE),
      num_class = sum(filtered$index == 1 & filtered$n_tested != 0)
      )
  }) %>%
    bind_cols(pars) %>%
    mutate(
      var_total = v * ((nobs - 1)/(nobs - npar)),
      var_ind = class_v * ((nobs - 1)/(nobs - num_class - npar + 1)),
      sd_total = var_total ^ .5,
      sd_ind = var_ind ^ .5
    )
  
  #########################################
  # collapse to class
  
  set.seed(1979)
  message("collapsing to class")
  
  # https://stackoverflow.com/questions/16325641/how-to-extract-the-first-n-rows-per-group#comment23381259_16325932
  # i have no idea how this works but is a million times faster than anything else
  collapsed <- dt[dt[, .I[1], by = .(.group, get(teacher), get(year), get(class))]$V1]   # first obs by group
  
  # same-year covariances
  collapsed$rand <- runif(nrow(collapsed))      # create a random number of each row
  collapsed <- collapsed[order(collapsed$rand),]  # sort by the random number
  collapsed[,classnum := seq_along(rand), by  = .(.group, get(teacher), get(year))]   # get class number by random number
  
  class_pars <- map_df(1 : n_groups, ~{
    filtered <- collapsed[.group == .x]
    
    if (max(filtered$classnum) == 1) {
      message(glue::glue("Group {.x}: All teachers have one class in each year"))
      tibble::tibble(cov_sameyear = 0)
    } else {
      
    # this has lag in the function but seems to create the lead? R is confusing sometimes
    filtered[, lead_class_mean :=  shift(class_mean, n = 1L, fill = NA, type = "lag"), by = .(.group, get(teacher), get(year))]
    filtered[, lead_n_tested :=  shift(n_tested, n = 1L, fill = NA, type = "lag"), by = .(.group, get(teacher), get(year))]
    filtered[, weight :=  n_tested + lead_n_tested]
    
    covs <- filtered %>%
      as_tibble() %>%
      filter(!is.na(weight), weight > 0, !is.na(class_mean), !is.na(lead_class_mean)) %>%
      select(class_mean, lead_class_mean, weight)
    
    weighted_corr <- cov.wt(covs[,1:2], wt = covs$weight, cor = TRUE)
    tibble::tibble(cov_sameyear = weighted_corr$cov[2,1])
    }
    
  })
  
  pars <- bind_cols(pars, class_pars) %>%
    mutate(
      var_class = var_total - var_ind - cov_sameyear,
      sd_tch_yr = cov_sameyear ^ .5,
      sd_class = var_class ^ .5
      )
  
  #########################################
  # collapse to teacher-year
  
  message("collapsing to teacher-year")
  # merge on the variances
  collapsed <- collapsed[data.table(pars), on =.(.group)]
  collapsed[, weight :=  1/(var_class + var_ind/n_tested)]
  tch_yr <- collapsed[, .(class_mean = weighted.mean(class_mean, w = weight, 
                                    na.rm = TRUE), weight = sum(weight), n_tested = sum(n_tested)), 
     keyby = .(.group, get(teacher), get(year))]
  
  # note: teacher and syear have been renamed to get and get.1
  
  # checks on drift limit
  data_span <- max(tch_yr$get.1) - min(tch_yr$get.1)
  if (data_span < driftlimit) {
    stop(glue::glue("You specified a drift limit of {driftlimit} but there are only {data_span} lags of teacher data. Back to the drawing room!"))
  }
  if (is.null(driftlimit)) {
    message(glue::glue("No drift limit specified. Using all data which has limit of {data_span}."))
    lags_limit <- data_span
  } 
  if (data_span >= driftlimit) {
    message(glue::glue("You specified a drift limit of {driftlimit}"))
    lags_limit <- driftlimit
  }
  
  #########################################
  # calculate lags
  
  setorder(tch_yr, cols = ".group", "get", "get.1")             # Sort data.table
  
  message("Calculating lags")
  lags <- map_df(1:n_groups, ~{
    
    # prepare for calculating lags. this is kind of slow but i couldn't get it to work right in data.table
    group <- tch_yr[.group == .x] %>%
      as_tibble() %>%
      group_by(.group) %>%
      complete(get, get.1) %>%
      arrange(get, get.1) %>%
      group_by(.group, get)
    
    # cov for each possible lag
    map_df(1:lags_limit, function(d) {
      prepped <- group %>%
        mutate(
          lead_class_mean = lag(class_mean, d),
          lead_n_tested = lag(n_tested, d),
          weight = n_tested + lead_n_tested
        )
      
      covs <- prepped %>%
        as_tibble() %>%
        filter(!is.na(weight), weight > 0, !is.na(class_mean), !is.na(lead_class_mean)) %>%
        select(class_mean, lead_class_mean, weight)
      
      weighted_corr <- cov.wt(covs[,1:2], wt = covs$weight, cor = TRUE)
      tibble::tibble(
        lag = d,
        cov_sameyear = weighted_corr$cov[2,1],
        corr = weighted_corr$cor[2,1],
        nobs = weighted_corr$n.obs,
        .group = .x
      )
    })
    
  })
  
  #########################################
  # calculate tv
  
  if (!is.null(driftlimit)) {
    message(glue::glue("Drift limit specified: {driftlimit}"))
  }
  
  # matrix m in CFR code
  mat <- pars %>%
    select(.group, cov_sameyear) %>%
    mutate(lag = 0) %>%
    bind_rows(lags %>% select(.group, lag, cov_sameyear)) %>%
    arrange(.group, lag)
  
  if (data_span > driftlimit) {
    # fill in past driftlimit
  }
  
  # TODO

  #########################################
  # final messages at the end
  pars <- groups %>% left_join(pars, by = ".group") %>% ungroup()
  lags <- groups %>% left_join(lags, by = ".group", multiple = "all") %>% ungroup()
  
  message("Standard deviations: total, classes, students, teachers same year")
  walk(1:n_groups, ~{
    pars %>%
      filter(.group == .x) %>%
      ungroup() %>%
      select(!!!rlang::syms(by), sd_total, sd_class, sd_ind, sd_tch_yr) %>%
      print()
    lags %>%
      filter(.group == .x) %>%
      ungroup() %>%
      print()
    
  })

  tictoc::toc()
  
  return(list(
    preds, # original data with scores added on
    pars %>% select(-.group),     # variance parameters and stuff
    lags %>% select(-.group),      # lag stuff
    tch_yr     # tch-yr dataset
    ))

}


#########################################
# usage

# school fe
ret <- vam(by = "lvl", 
           data = raw_df, 
           controls = c("lag_mat_nce", "lag_ela_nce", "lag_std_noncog_factor"), 
           teacher = "mepid",
           class = "section_id",
           year = "syear",
           absorb = "sch_code", 
           driftlimit = 3,
           y = "test")

# teacher fe
ret <- vam(lvl, 
           data = raw_df, 
           controls = c("lag_mat_nce", "lag_ela_nce", "lag_std_noncog_factor"), 
           teacher = "mepid",
           class = "section_id",
           year = "syear",
           tfx_resid = "mepid", 
           driftlimit = 3,
           y = "test")


  ret[[2]]
  
  ret[[3]]
  
  ret[[4]] %>%
     as_tibble() %>%
     filter(.group == 1, get == "50010086")
  
  # st_view(Z=.,.,(teacher_var,time_var,weights_var,scores_var))
  

  # st_store(obs,va_var_ind,
    # driftcalc(M,time-year_index,Z_obs[.,2]:-year_index,Z_obs[.,3],Z_obs[.,4])
  #)

  
   