#' Estimate Teacher Value-Added
#' @description This program uses student-level outcomes (typically test scores) to compute teacher value-added (VA) measures.  
#' The estimation procedure accounts for drift in teacher VA over time. 
#' This package is based on Michael Stepner's Stata program and intended to produce identical results with the speed gains made possible by R.

#' @param y Required. The outcome variable in quotations.
#' @param teacher Required. The name of the teacher identifier in quotations.
#' @param year Required. The name of the year identifier in quotations.
#' @param class Required. The name of the classroom identifier in quotations.
#' @param controls Controls to be used in the residualization.
#' @param by Perform VA estimation separately for each by-group. Ex by = c("subject", "level").
#' @param tfx_resid Absorb fixed effects during y residualization, but include those fixed effects in the residual.
#' @param absorb Residualize y on absorbed fixed effects.
#' @param data Required. Data frame to use.
#' @param driftlimit Estimate only # autocovariances; set all further autocovariances equal to the last estimate.
#' @param quasi Default FALSE. Generates two additional leave-out VA measures, which are typically used for quasi-experimental tests. This option adds `tv_2yr_l`, 
#' which leaves out the forecast year and the prior year, and `tv_2yr_f`, which leaves out the forecast year and the following year. Note that if
#' `tv_name` is specified, those names will also apply to the `_2yr_f` and `_2yr_l` variables.
#' @param tv_name Default "tv". String denoting the name of the value-added forecast.
#' @param scores_name Default "score_r". String denoting the name of the score residual.
#' @param cfr_test Default FALSE. Conducts the CFR quasi-experimental test for forecast bias in value-added estimates. Collapses data to school-grade-year-subject
#' cells with average `y` and average value-added. Regresses change in `y` on change in leave-out value-added, using the 2-year leave-outs generated
#' with the `quasi` command as in CFR. Regression includes year fixed effects and clusters standard errors by school-cohort. If this is TRUE,
#' you must specify `cfr_school`, `cfr_grade`, and `cfr_subject` so that the data can be collapsed appropriately. If this is TRUE, the `quasi`
#' estimates will automatically be calculated.
#' @param cfr_school Required if `cfr_test` is TRUE. Name of the school variable used in the collapse for the `cfr_test`. Not used in value-added
#' calculations.
#' @param cfr_grade Required if `cfr_test` is TRUE. Name of the grade variable used in the collapse for the `cfr_test`. Not used in value-added
#' calculations.
#' @param cfr_subject Required if `cfr_test` is TRUE. Name of the subject variable used in the collapse for the `cfr_test`. Not used in value-added
#' calculations.
#' @param cfr_weight Optional if `cfr_test` is TRUE. Name of the teacher weight variable used in the collapse for `cfr_test` (typically a 
#' teacher dosage variable greater than 0 and no greater than 1). Not used in the value-added calculations.
#' @param return_df_only Default FALSE. If TRUE, return only the original dataframe with value-added and residuals appended. If FALSE,
#' return a list of (a) the original df with value-added and residuals, (b) the variance estimates, (c) the autocovariances by lag, and
#' (d) a teacher-year-(by) dataset with the value-added estimates. Note that (d) will include teacher estimates for years in which they were
#' not originally present in the data: this is a difference from the Stata version. To restrict to years in which a teacher was in the data,
#' use (a) which is the original data with the value-added estimates appended.
#' @param return_cfr_test_only Default FALSE. If TRUE, return only the coefficient and standard error of the `cfr_test` result. Useful for checking
#' the bias of many different specifications without having to hold each resulting set of `tv` estimates in memory.
#'
#' @return A dataframe or list depending on `return_df_only` and `return_cfr_test_only`.
#' @export
#' @import data.table
#' @import fixest
#' @importFrom stats resid runif var
#'
#' @examples
#' \donttest{
#' try({
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
#' })
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
    return_cfr_test_only = FALSE
) {

  tictoc::tic("vam")

  message <- "VAM estimation"
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(message), glue::glue("\033[1m{message}\033[22m")),
      right = paste0("vamR version ", utils::packageVersion("vamR")),
      width = getOption("width")
    )
  )
  cli::cli_progress_step("Initial function checks.")

  #########################################
  # function stuff and checks

  if (is.null(data)) {
    cli::cli_abort("You must provide a dataframe using the data argument.")
  }

  if (return_df_only && return_cfr_test_only) {
    cli::cli_abort("You asked to only return the the joined data and also to only return the CFR test results. Please pick one or the other or leave all the return only's blank.")
  }

  if (tv_name %in% names(data)) {
    cli::cli_abort(glue::glue("The dataset loaded in memory when vam is run cannot have a variable named {tv_name}."))
  }

  if (scores_name %in% names(data)) {
    cli::cli_abort(glue::glue("The dataset loaded in memory when vam is run cannot have a variable named {scores_name}."))
  }

  if (!is.null(absorb) && !is.null(tfx_resid)) {
    cli::cli_abort("Cannot specify an absorb variable and a tfx_resid variable simultaneously.")
  }

  want_cfr <- cfr_test || return_cfr_test_only
  want_quasi <- quasi || want_cfr

  if (want_cfr && (is.null(cfr_school) || is.null(cfr_grade) || is.null(cfr_subject))) {
    cli::cli_abort("You must provide school, grade, and subject variable names to run CFR test")
  }

  # work on a copy so the caller's data is never modified by reference
  reg_df <- if (is.data.table(data)) copy(data) else as.data.table(data)

  # .group numbers the by-groups in sorted order. rows are then (stably) sorted
  # by group, which fixes the order of the random class pairing below
  if (!is.null(by)) {
    reg_df[, .group := frankv(.SD, ties.method = "dense", na.last = TRUE), .SDcols = by]
  } else {
    cli::cli_alert_warning("No by variables provided. All calculations will be run on full data.")
    reg_df[, .group := 1L]
  }
  setorder(reg_df, .group)

  # list of groups and .group for later convenience
  groups <- reg_df[!duplicated(.group), c(".group", by), with = FALSE]
  n_groups <- nrow(groups)

  #########################################
  # residualization

  if (!is.null(tfx_resid)) {
    cli::cli_progress_step("Residualizing with teacher FE.")
  } else if (!is.null(absorb)) {
    cli::cli_progress_step("Residualizing with a non-teacher FE: {absorb}")
  } else {
    cli::cli_progress_step("Residualizing with no fixed effects.")
  }

  fe <- c(tfx_resid, absorb)
  if (is.null(fe)) {
    model <- fixest::feols(.[y] ~ .[controls], data = reg_df, split = ~.group)
  } else {
    # keep singleton / perfect-fit fixed effects, as Stata does
    model <- fixest::feols(.[y] ~ .[controls] | .[fe], data = reg_df, split = ~.group, fixef.rm = "none")
  }

  # one column of residuals per group, NA outside it: take each row's own group
  resids <- as.matrix(resid(model, type = "response", na.rm = FALSE))
  reg_df[, .score_r := resids[cbind(seq_len(.N), .group)]]
  rm(resids)

  # add the teacher FE back into the residual, centered within group
  if (!is.null(tfx_resid)) {
    reg_df[, .score_r := {
      fe_coef <- fixest::fixef(model[[.BY$.group]])[[tfx_resid]]
      tfe <- fe_coef[match(as.character(tch), names(fe_coef))]
      .score_r + tfe - mean(tfe, na.rm = TRUE)
    }, by = .group, env = list(tch = tfx_resid)]
  }

  # absorbed FE count toward the degrees-of-freedom correction; teacher FE do not
  pars <- data.table(
    .group = seq_len(n_groups),
    npar = vapply(seq_len(n_groups), function(g) {
      as.integer(if (is.null(absorb)) length(model[[g]]$coefficients) else model[[g]]$nparams)
    }, integer(1)),
    nobs = vapply(seq_len(n_groups), function(g) as.integer(model[[g]]$nobs), integer(1))
  )
  rm(model)

  #########################################
  # calculate total and individual variances

  cli::cli_progress_step("Computing variances.")

  reg_df[, .dev := .score_r - mean(.score_r, na.rm = TRUE), by = c(".group", teacher, year, class)]
  pars[
    reg_df[, .(v = var(.score_r, na.rm = TRUE), class_v = var(.dev, na.rm = TRUE)), keyby = .group],
    on = ".group",
    `:=`(v = i.v, class_v = i.class_v)
  ]
  reg_df[, .dev := NULL]

  #########################################
  # collapse to class

  cli::cli_progress_step("Collapsing to class.")

  # one row per class, in order of first appearance
  classes <- reg_df[
    ,
    .(n_tested = sum(!is.na(.score_r)), class_mean = mean(.score_r, na.rm = TRUE)),
    by = .(.group, .teacher = tch, .year = yr, .class = cls),
    env = list(tch = teacher, yr = year, cls = class)
  ]

  pars[classes[, .(num_class = sum(n_tested != 0)), keyby = .group], on = ".group", num_class := i.num_class]
  pars[, `:=`(
    var_total = v * ((nobs - 1) / (nobs - npar)),
    var_ind = class_v * ((nobs - 1) / (nobs - num_class - npar + 1))
  )]
  pars[, `:=`(sd_total = var_total^.5, sd_ind = var_ind^.5)]

  # same-year covariance: pair each class with another class of the same
  # teacher-year, in random order
  set.seed(1979)
  classes[, .rand := runif(.N)]
  setorder(classes, .rand)
  classes[, `:=`(lag_class_mean = shift(class_mean), lag_n_tested = shift(n_tested)), by = .(.group, .teacher, .year)]

  sameyear_cov <- classes[, {
    w <- n_tested + lag_n_tested
    if (all(is.na(lag_n_tested))) {
      g <- .BY$.group
      cli::cli_alert_warning("Group {g}: All teachers have one class in each year")
      .(cov_sameyear = 0)
    } else {
      keep <- !is.na(w) & w > 0 & !is.na(class_mean) & !is.na(lag_class_mean)
      covs <- stats::cov.wt(cbind(class_mean, lag_class_mean)[keep, , drop = FALSE], wt = w[keep])
      .(cov_sameyear = covs$cov[2, 1])
    }
  }, keyby = .group]

  pars[sameyear_cov, on = ".group", cov_sameyear := i.cov_sameyear]
  pars[, var_class := var_total - var_ind - cov_sameyear]
  pars[, `:=`(sd_tch_yr = cov_sameyear^.5, sd_class = var_class^.5)]
  setcolorder(pars, c(".group", "v", "class_v", "num_class", "npar", "nobs"))

  #########################################
  # collapse to teacher-year

  cli::cli_progress_step("Collapsing to teacher-year")

  classes[pars, on = ".group", weight := 1 / (i.var_class + i.var_ind / n_tested)]

  # class_mean is weighted.mean(class_mean, weight, na.rm = TRUE), via grouped sums
  classes[, `:=`(
    .wx = fifelse(is.na(class_mean), 0, weight * class_mean),
    .w = fifelse(is.na(class_mean), 0, weight)
  )]
  tch_yr <- classes[
    ,
    .(.wx = sum(.wx), .w = sum(.w), weight = sum(weight), n_tested = sum(n_tested)),
    keyby = .(.group, .teacher, .year)
  ]
  tch_yr[, class_mean := .wx / .w]
  tch_yr[, c(".wx", ".w") := NULL]

  # checks on drift limit
  data_span <- max(tch_yr$.year) - min(tch_yr$.year)
  if (is.null(driftlimit)) {
    cli::cli_alert_info("No drift limit specified. Using all data which has limit of {data_span}.")
    driftlimit <- data_span
  } else if (driftlimit > data_span) {
    cli::cli_abort("You specified a drift limit of {driftlimit} but there are only {data_span} lags of teacher data. Back to the drawing board!")
  } else {
    cli::cli_alert_info("You specified a drift limit of {driftlimit}")
  }

  #########################################
  # calculate lags

  cli::cli_progress_step("Calculating lag year covariances")

  lags <- rbindlist(lapply(seq_len(driftlimit), function(d) {
    # pair each teacher-year with the same teacher d years earlier
    earlier <- tch_yr[, .(.group, .teacher, .year = .year + d, lag_class_mean = class_mean, lag_n_tested = n_tested)]
    pairs <- earlier[tch_yr, on = .(.group, .teacher, .year), nomatch = NULL]
    pairs[, w := n_tested + lag_n_tested]
    pairs[!is.na(class_mean) & !is.na(lag_class_mean) & !is.na(w), {
      covs <- stats::cov.wt(cbind(class_mean, lag_class_mean), wt = w, cor = TRUE)
      .(lag = d, cov_sameyear = covs$cov[2, 1], corr = covs$cor[2, 1], nobs = covs$n.obs)
    }, keyby = .group]
  }))
  setorder(lags, .group, lag)

  #########################################
  # calculate tv

  # covariance by lag from 0 to data_span (vector m in CFR code). lags beyond
  # driftlimit are set to the driftlimit covariance
  cov_by_lag <- function(g) {
    m <- c(pars[.group == g, cov_sameyear], lags[.group == g, cov_sameyear])
    c(m, rep(m[length(m)], data_span + 1 - length(m)))
  }

  if (want_quasi) {
    cli::cli_progress_step("Calculating leave-one-year-out and leave-two-year-out value added")
  } else {
    cli::cli_progress_step("Calculating leave-one-year-out value added")
  }

  va <- rbindlist(lapply(seq_len(n_groups), function(g) {
    # matrix M in CFR code
    M <- stats::toeplitz(cov_by_lag(g))
    compute_tv(tch_yr[.group == g], M, quasi = want_quasi)[, .group := g]
  }))
  setcolorder(va, ".group")
  setorder(va, .group, .year, .teacher)

  tv_cols <- if (want_quasi) c("tv", "tv_2yr_f", "tv_2yr_l") else "tv"
  new_tv_cols <- paste0(tv_name, sub("^tv", "", tv_cols))
  setnames(va, tv_cols, new_tv_cols)

  cli::cli_progress_step("Merging tv estimates back to original data.")

  reg_df[
    va,
    on = stats::setNames(c(".group", ".teacher", ".year"), c(".group", teacher, year)),
    (new_tv_cols) := mget(paste0("i.", new_tv_cols))
  ]
  reg_df[, .group := NULL]
  setnames(reg_df, ".score_r", scores_name)

  if (want_cfr) {
    cli::cli_progress_step("Performing CFR quasi-experimental test.")

    f_name <- paste0(tv_name, "_2yr_f")
    l_name <- paste0(tv_name, "_2yr_l")

    cfr_cols <- c(
      .school = cfr_school,
      .grade = cfr_grade,
      .year = year,
      .subject = cfr_subject,
      .tv_f = f_name,
      .tv_l = l_name,
      .score = y,
      .dosage = cfr_weight
    )
    cfr_preds <- reg_df[
      !is.na(tv_f) & !is.na(tv_l) & !is.na(score),
      .SD,
      .SDcols = unname(cfr_cols),
      env = list(tv_f = f_name, tv_l = l_name, score = y)
    ]
    setnames(cfr_preds, names(cfr_cols))
    if (is.null(cfr_weight)) {
      cfr_preds[, .dosage := 1]
    }

    # collapse to school-grade-subject-year
    cfr_data <- cfr_preds[
      ,
      .(score = mean(.score), tv_f = mean(.tv_f), tv_l = mean(.tv_l), num = sum(.dosage)),
      keyby = .(.school, .grade, .year, .subject)
    ]
    cfr_data[, cohort := .year - .grade]
    cfr_data[, pid := .GRP, by = .(.school, .grade, .subject)]
    cfr_data[, sch_cohort := .GRP, by = .(.school, cohort)]

    # changes from the prior year within school-grade-subject
    prior <- cfr_data[, .(pid, .year = .year + 1L, prior_num = num, prior_tv_f = tv_f, prior_score = score)]
    cfr_data[prior, on = .(pid, .year), `:=`(
      tot_obs = num + i.prior_num,
      diff = tv_l - i.prior_tv_f,
      d_mean_score = score - i.prior_score
    )]

    model_cfr <- fixest::feols(
      d_mean_score ~ diff | .year, data = cfr_data, weights = ~tot_obs, cluster = ~sch_cohort, fixef.rm = "none"
    )
    cfr <- tibble::tibble(
      quasi_b = as.numeric(model_cfr$coefficients[1]),
      quasi_se = as.numeric(model_cfr$se[1])
    )
  }

  #########################################
  # final messages at the end

  pars <- groups[pars, on = ".group"]
  lags <- groups[lags, on = ".group"]

  cli::cli_alert_info("Standard deviations: total, classes, students, teachers same year")
  for (g in seq_len(n_groups)) {
    print(tibble::as_tibble(pars[.group == g, c(by, "sd_total", "sd_class", "sd_ind", "sd_tch_yr"), with = FALSE]))
    print(tibble::as_tibble(lags[.group == g, c(by, "lag", "cov_sameyear", "corr", "nobs"), with = FALSE]))
  }

  if (return_cfr_test_only) {
    tictoc::toc()
    cli::cli_progress_step("Done.")
    return(cfr)
  }

  data_out <- tibble::as_tibble(reg_df)

  if (return_df_only) {
    tictoc::toc()
    cli::cli_progress_step("Done.")
    return(data_out)
  }

  setnames(va, c(".teacher", ".year"), c(teacher, year))

  return_list <- list(
    "Data" = data_out,                                                        # original data with scores added on
    "Parameters" = tibble::as_tibble(pars[, !c(".group", "v", "class_v")]),   # variance parameters and stuff
    "Covariances" = tibble::as_tibble(lags[, !".group"]),                     # lag stuff
    "Value_Added" = tibble::as_tibble(groups[va, on = ".group"][, !".group"]) # tch-yr dataset
  )
  if (cfr_test) {
    return_list$CFR_Test <- cfr                                               # cfr test coef and se
  }

  cli::cli_progress_step("Done.")
  tictoc::toc()

  return_list

}
