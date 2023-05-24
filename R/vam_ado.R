#' Estimate Teacher Value-Added
#' @description This program uses student-level outcomes (typically test scores) to compute teacher value-added (VA) measures.  
#' The estimation procedure accounts for drift in teacher VA over time. 
#' This package is based on Michael Stepner's Stata program and intended to produce identical results with the speed gains made possible by R.

#' @param y Required. The outcome variable in quotations.
#' @param teacher Required. The name of the teacher identifier in quotations.
#' @param year Required. The name of the year identifier in quotations.
#' @param class Required. The name of the classroom identifier in quotations.
#' @param controls Controls to be used in the residualization.
#' @param by Perform VA estimation separately for each by-group. Ex by = c("subject", "level")
#' @param tfx_resid Absorb fixed effects during y residualization, but include those fixed effects in the residual
#' @param absorb Residualize y on absorbed fixed effects
#' @param driftlimit estimate only # autocovariances; set all further autocovariances equal to the last estimate
#' @param quasi Currently not implemented.
#' @param data Required. Data frame to use.
#' @param return_df_only Default FALSE. If TRUE, return only the original dataframe with value-added and residuals appended. If FALSE,
#' return a list of (a) the original df with value-added and residuals, (b) the variance estimates, (c) the autocovariances by lag, and
#' (d) a teacher-year-(by) dataset with the value-added estimates
#' @param tv_name Default "tv". String denoting the name of the value-added forecast
#' @param scores_name Default "score_r". String denoting the name of the score residual.
#'
#' @return A dataframe or list depending on `return_df_only`.
#' @export
#' @import data.table
#'
#' @examples
#' \donttest{
#' returned <- vam(
#'   by = c("lvl", "subject"), 
#'   data = df_prepped, 
#'   controls = controls, 
#'   teacher = "mepid",
#'   class = "section_id",
#'   year = "syear",
#'   tfx_resid = "mepid", 
#'   driftlimit = 7,
#'   y = "test",
#'   return_df_only = FALSE,
#'   tv_name = "tv",
#'   scores_name = "score_r"
#' )
#' }
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
    data = NULL,
    return_df_only = FALSE,
    tv_name = "tv",
    scores_name = "score_r"
) {
  
  tictoc::tic("vam")
  cli::cli_progress_step("Initial function checks.")
  
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
    reg_df <- data |>
      dplyr::group_by(!!!rlang::syms(by)) |>
      dplyr::mutate(.group = dplyr::cur_group_id()) |>
      dplyr::ungroup()
  } else {
    cli::cli_alert_warning("No by variables provided. All calculations will be run on full data.")
    reg_df <- data |>
      dplyr::mutate(.group = 1) |>
      dplyr::ungroup()
  }
  
  # list of groups and .group for later convenience
  groups <- reg_df |>
    dplyr::group_by(.group) |>
    dplyr::slice_head(n = 1) |> 
    dplyr::select(.group, !!!rlang::syms(by))
  
  n_groups <- max(reg_df$.group)


  #########################################
  # residualization
  
  # Turn into a fixest control vector
  fixest::setFixest_fml(..ctrl = controls)

  # teacher fe
  if (!is.null(tfx_resid)) {
    cli::cli_progress_step("Residualizing with teacher FE.")
    model <- fixest::feols(.[y] ~ ..ctrl | .[tfx_resid], data = reg_df, split = ~.group)

    resids <- resid(model, type = "response", na.rm = FALSE)
    preds <- purrr::map_df(1:n_groups, ~{
      
      sample_df <- reg_df
      sample_df$score_r <- resids[, .x]
      sample_df <- sample_df |> 
        dplyr::filter(.group == .x)
      
      # add in the teacher fe
      sample_df$tfe <-    predict(as.list(model)[[.x]], sample_df, fixef = TRUE) |> dplyr::pull({{tfx_resid}})
      sample_df |>
        dplyr::mutate(
          mean_tfe = mean(tfe, na.rm = TRUE),
          score_r = score_r + tfe - mean_tfe
          ) |>
        dplyr::select(-tfe, -mean_tfe)
      
    })
    
    pars <- purrr::map_df(1:n_groups, ~{
      tibble::tibble(
        .group = .x, 
        npar = model[[.x]]$coefficients |> length(),
        nobs = model[[.x]]$nobs
        )
    })
  }
  
  # other fe that's not teacher
  if (!is.null(absorb)) {
    cli::cli_progress_step("Residualizing with a non-teacher FE: {absorb}")
    model <- fixest::feols(.[y] ~ ..ctrl | .[absorb], data = reg_df, split = ~.group)
    
    resids <- resid(model, type = "response", na.rm = FALSE)
    preds <- purrr::map_df(1:n_groups, ~{
      
      reg_df$score_r <- resids[, .x]
      reg_df |> dplyr::filter(.group == .x)
      
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
    cli::cli_progress_step("Residualizing with no fixed effects.")
    model <- fixest::feols(.[y] ~ ..ctrl, data = reg_df, split = ~.group)
    
    resids <- resid(model, type = "response", na.rm = FALSE)
    preds <- purrr::map_df(1:n_groups, ~{
      
      reg_df$score_r <- resids[, .x]
      reg_df |> dplyr::filter(.group == .x)
      
    })
    
    pars <- map_df(1:n_groups, ~{
      tibble::tibble(
        .group = .x, 
        npar = model[[.x]]$coefficients |> length(),
        nobs = model[[.x]]$nobs
      )
    })
  }
  
  #########################################
  # calculate total and individual variances
  
  # start using data.table. this is way, way faster than dplyr
  cli::cli_progress_step("Moving data to data.table and computing variances.")
  
  library(data.table)  # this is horrible practice...
  dt <- data.table(preds)
  
  dt[, `:=`(c("n_tested", "class_mean", 
              "index", "individual_dev_from_class"), {
                n_tested <- sum(!is.na(score_r))
                class_mean <- mean(score_r, na.rm = TRUE)
                index <- 1:.N
                individual_dev_from_class <- score_r - class_mean
                .(n_tested, class_mean, index, individual_dev_from_class)
              }), by = .(.group, get(teacher), get(year), get(class))]
  preds <- tibble::as_tibble(dt)   # used to hand back data to user later
  
  # calculate more parameters
  pars <- purrr::map_df(1:n_groups, ~{
    filtered <- dt[.group == .x]
    tibble::tibble(
      v = var(filtered$score_r, na.rm = TRUE),
      class_v = var(filtered$individual_dev_from_class, na.rm = TRUE),
      num_class = sum(filtered$index == 1 & filtered$n_tested != 0)
      )
  }) |>
    dplyr::bind_cols(pars) |>
    dplyr::mutate(
      var_total = v * ((nobs - 1)/(nobs - npar)),
      var_ind = class_v * ((nobs - 1)/(nobs - num_class - npar + 1)),
      sd_total = var_total ^ .5,
      sd_ind = var_ind ^ .5
    )
  
  #########################################
  # collapse to class
  
  set.seed(1979)
  cli::cli_progress_step("Collapsing to class.")
  
  # https://stackoverflow.com/questions/16325641/how-to-extract-the-first-n-rows-per-group#comment23381259_16325932
  # i have no idea how this works but is a million times faster than anything else
  collapsed <- dt[dt[, .I[1], by = .(.group, get(teacher), get(year), get(class))]$V1]   # first obs by group
  
  # same-year covariances
  collapsed$rand <- runif(nrow(collapsed))      # create a random number of each row
  collapsed <- collapsed[order(collapsed$rand),]  # sort by the random number
  collapsed[,classnum := seq_along(rand), by  = .(.group, get(teacher), get(year))]   # get class number by random number
  
  class_pars <- purrr::map_df(1 : n_groups, ~{
    filtered <- collapsed[.group == .x]
    
    if (max(filtered$classnum) == 1) {
      cli::cli_alert_warning("Group {.x}: All teachers have one class in each year")
      tibble::tibble(cov_sameyear = 0)
    } else {
      
    # this has lag in the function but seems to create the lead? R is confusing sometimes
    filtered[, lead_class_mean :=  shift(class_mean, n = 1L, fill = NA, type = "lag"), by = .(.group, get(teacher), get(year))]
    filtered[, lead_n_tested :=  shift(n_tested, n = 1L, fill = NA, type = "lag"), by = .(.group, get(teacher), get(year))]
    filtered[, weight :=  n_tested + lead_n_tested]
    
    covs <- filtered |>
      tibble::as_tibble() |>
      dplyr::filter(!is.na(weight), weight > 0, !is.na(class_mean), !is.na(lead_class_mean)) |>
      dplyr::select(class_mean, lead_class_mean, weight)
    
    weighted_corr <- cov.wt(covs[,1:2], wt = covs$weight, cor = TRUE)
    tibble::tibble(cov_sameyear = weighted_corr$cov[2,1])
    }
    
  })
  
  pars <- dplyr::bind_cols(pars, class_pars) |>
    dplyr::mutate(
      var_class = var_total - var_ind - cov_sameyear,
      sd_tch_yr = cov_sameyear ^ .5,
      sd_class = var_class ^ .5
      )
  
  #########################################
  # collapse to teacher-year
  
  cli::cli_progress_step("Collapsing to teacher-year")
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
    cli::cli_alert_info("No drift limit specified. Using all data which has limit of {data_span}.")
    lags_limit <- data_span
  } 
  if (data_span >= driftlimit) {
    cli::cli_alert_info("You specified a drift limit of {driftlimit}")
    lags_limit <- driftlimit
  }
  

  #########################################
  # calculate lags
  
  setorder(tch_yr, cols = ".group", "get", "get.1")             # Sort data.table
  
  lags <- purrr::map_df(1:n_groups, ~{
    cli::cli_progress_step("Calculating lags for group {.x} of {n_groups}")
    
    # prepare for calculating lags. this is kind of slow but i couldn't get it to work right in data.table
    group <- tch_yr[.group == .x] |>
      tibble::as_tibble() |>
      dplyr::group_by(.group) |>
      tidyr::complete(get, get.1) |>
      dplyr::arrange(get, get.1) |>
      dplyr::group_by(.group, get)
    
    # cov for each possible lag
    purrr::map_df(1:lags_limit, function(d) {
      prepped <- group |>
        dplyr::mutate(
          lead_class_mean = dplyr::lag(class_mean, d),
          lead_n_tested = dplyr::lag(n_tested, d),
          weight = n_tested + lead_n_tested
        )
      
      covs <- prepped |>
        tibble::as_tibble() |>
        dplyr::filter(!is.na(weight), weight > 0, !is.na(class_mean), !is.na(lead_class_mean)) |>
        dplyr::select(class_mean, lead_class_mean, weight)
      
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
  
  # vector m in CFR code
  mat <- pars |>
    dplyr::select(.group, cov_sameyear) |>
    dplyr::mutate(lag = 0) |>
    dplyr::bind_rows(lags |> dplyr::select(.group, lag, cov_sameyear)) |>
    dplyr::arrange(.group, lag)
  
  if (data_span > driftlimit) {
    mat <- purrr::map_df(1 : n_groups, ~{
      tibble::tibble(
        .group = .x,
        cov_sameyear = mat |> dplyr::filter(.group == .x) |> dplyr::slice_tail(n = 1) |> dplyr::pull(cov_sameyear),
        lag = (driftlimit + 1) : (data_span)
      )
    }) |>
      dplyr::bind_rows(mat) |>
      dplyr::arrange(.group, lag)
  }
  
  cli::cli_alert_warning("Covariances used for VA computations:")
  purrr::walk(1:n_groups, ~{
    print(groups |> dplyr::right_join(
      mat |> dplyr::filter(.group == .x, lag > 0),
      by = ".group", multiple = "all"
      ))
  })
  
  # matrix M in CFR code
  matrix_M <- purrr::map_df(1:n_groups, ~{
    mat |>
      dplyr::filter(.group == .x) |>
      dplyr::mutate(M = vectorToStripeDiag(cov_sameyear))
  })

  # calculate tv
  tch_yr <- purrr::map_df(1 : n_groups, function(g){
    cli::cli_progress_step("Calculating tv for group {g} of {n_groups}")
    group_df <- tch_yr |> dplyr::filter(.group == g)
    group_M <- matrix_M |> dplyr::filter(.group == g) |> dplyr::pull(M)
 
    compute_tv_matrix(group_df, group_M) |>
      dplyr::mutate(.group = g)
    
  }) |> 
  dplyr::rename({{teacher}} := get, {{year}} := get.1, "{tv_name}" := tv)
  
  cli::cli_progress_step("Merging tv estimates back to original data and finishing up.")
  
  preds <- preds |>
    dplyr::left_join(tch_yr, by = dplyr::join_by({{teacher}}, {{year}}, .group)) |>
    dplyr::select(-.group, -n_tested, -class_mean, -index, -individual_dev_from_class) |>
    dplyr::rename("{scores_name}" := score_r)
  
  if (return_df_only == TRUE) {
    tictoc::toc()
    cli::cli_progress_step("Done.")
    return(preds)
  }

  #########################################
  # final messages at the end
  pars <- groups |> dplyr::left_join(pars, by = ".group") |> dplyr::ungroup()
  lags <- groups |> dplyr::left_join(lags, by = ".group", multiple = "all") |> dplyr::ungroup()
  tch_yr <- groups |> dplyr::left_join(tch_yr, by = ".group", multiple = "all") |> dplyr::ungroup()
  
  cli::cli_alert_info("Standard deviations: total, classes, students, teachers same year")
  purrr::walk(1:n_groups, ~{
    pars |>
      dplyr::filter(.group == .x) |>
      dplyr::ungroup() |>
      dplyr::select(!!!rlang::syms(by), sd_total, sd_class, sd_ind, sd_tch_yr) |>
      print()
    lags |>
      dplyr::filter(.group == .x) |>
      dplyr::ungroup() |>
      dplyr::select(!!!rlang::syms(by), cov_sameyear, corr, nobs) |>
      print()
    
  })
  
  cli::cli_progress_step("Done.")

  tictoc::toc()
  
  # once finished, remove .group from pars and lags
  return(list(
    preds , # original data with scores added on
    pars |> dplyr::select(-.group, -v, -class_v),     # variance parameters and stuff
    lags |> dplyr::select(-.group),      # lag stuff
    tch_yr |> dplyr::select(-.group)     # tch-yr dataset
    ))

}


