#' Compute Power Measures from Posterior rvars (Backend-Agnostic)
#'
#' This function computes power analysis measures from posterior samples in rvar format.
#' It works with posteriors from any backend (brms, NPE, etc.) as long as they are
#' converted to rvar format first.
#'
#' @param posterior_rvars A draws_rvars object containing posterior samples for target parameters
#' @param target_params Character vector of parameter names to analyze
#' @param thresholds_success Numeric vector of success thresholds (one per parameter)
#' @param thresholds_futility Numeric vector of futility thresholds (one per parameter)
#' @param p_sig_scs Probability threshold for declaring success
#' @param p_sig_ftl Probability threshold for declaring futility
#'
#' @return A data frame containing power analysis measures
#' @importFrom stats median
#' @keywords internal
compute_measures <- function(posterior_rvars, target_params, thresholds_success,
                             thresholds_futility, p_sig_scs, p_sig_ftl) {

  # Compute measures for each parameter
  measures_list <- purrr::map(target_params, function(param) {
    # Extract rvar for this parameter
    param_rvar <- posterior_rvars[[param]]

    # extract thresholds
    if (length(target_params) > 1) {
      threshold_success <- thresholds_success[which(target_params == param)]
      # if the threshold is NA, use the first one
      if (is.na(threshold_success)) {
        threshold_success <- thresholds_success[1]
      }
      threshold_futility <- thresholds_futility[which(target_params == param)]
      # if the threshold is NA, use the first one
      if (is.na(threshold_futility)) {
        threshold_futility <- thresholds_futility[1]
      }
    } else {
      threshold_success <- thresholds_success
      threshold_futility <- thresholds_futility
    }
    # calculate the probability of success / futility
    success_prob <- posterior::Pr(param_rvar > threshold_success)
    futility_prob <- posterior::Pr(param_rvar < threshold_futility)
    # significance
    sig_success <- as.numeric(success_prob >= p_sig_scs, na.rm = TRUE)
    sig_futility <- as.numeric(futility_prob >= p_sig_ftl, na.rm = TRUE)
    # parameter estimates
    est_median <- stats::median(param_rvar)
    est_mad <- posterior::mad(param_rvar)
    est_mean <- mean(param_rvar)
    est_sd <- posterior::sd(param_rvar)
    # convergence metrics
    rhat <- posterior::rhat(param_rvar)
    ess_bulk <- posterior::ess_bulk(param_rvar)
    ess_tail <- posterior::ess_tail(param_rvar)

    # combine results into a list
    out_list <- list(
      par_name = param,
      thr_scs = threshold_success,
      thr_ftl = threshold_futility,
      pr_scs = success_prob,
      pr_ftl = futility_prob,
      dec_scs = sig_success,
      dec_ftl = sig_futility,
      post_med = est_median,
      post_mad = est_mad,
      post_mn = est_mean,
      post_sd = est_sd,
      rhat = rhat,
      ess_bulk = ess_bulk,
      ess_tail = ess_tail
    )

    return(out_list)
  })

  # compute combined probabilities and powers
  if (length(target_params) > 1) {
    # Convert rvars to matrix for combined calculations
    # Extract draws as matrix: each column is a parameter
    posterior_matrix_list <- lapply(target_params, function(param) {
      as.vector(posterior::draws_of(posterior_rvars[[param]]))
    })
    posterior_samples <- do.call(cbind, posterior_matrix_list)

    # extract thresholds and broadcast if necessary
    if (length(target_params) > length(thresholds_success)) {
      thresholds_success_combined <- rep(thresholds_success[1], length(target_params))
    } else {
      thresholds_success_combined <- thresholds_success
    }
    if (length(target_params) > length(thresholds_futility)) {
      thresholds_futility_combined <- rep(thresholds_futility[1], length(target_params))
    } else {
      thresholds_futility_combined <- thresholds_futility
    }

    # Combined probability algorithm (AND decision rule for multiple parameters)
    # All parameters must simultaneously exceed threshold (conjunctive logic)
    # Algorithm:
    #   1. Convert each parameter to binary: exceeds threshold? (1/0)
    #   2. Take minimum across parameters for each draw (AND operation)
    #   3. Average across draws to get probability
    # Example: P(param1 > 0.2 AND param2 > 0.3 AND param3 > 0.1)
    combined_success_prob <- mean(apply(ifelse(
      posterior_samples > thresholds_success_combined, 1, 0
    ), 1, min))
    combined_futility_prob <- mean(apply(
      ifelse(posterior_samples < thresholds_futility_combined, 1, 0),
      1,
      min
    ))

    # calculate combined significance
    combined_sig_success <- as.numeric(combined_success_prob >= p_sig_scs, na.rm = TRUE)
    combined_sig_futility <- as.numeric(combined_futility_prob >= p_sig_ftl, na.rm = TRUE)

    # combine results into a list
    measures_list_combined <- list(
      par_name = "union",
      thr_scs = NA,
      thr_ftl = NA,
      pr_scs = combined_success_prob,
      pr_ftl = combined_futility_prob,
      dec_scs = combined_sig_success,
      dec_ftl = combined_sig_futility,
      post_med = NA,
      post_mad = NA,
      post_mn = NA,
      post_sd = NA,
      rhat = NA,
      ess_bulk = NA,
      ess_tail = NA
    )
  }

  # remove success_samples and futility_samples from the list
  measures_list <- purrr::map(measures_list, function(x) {
    x$success_samples <- NULL
    x$futility_samples <- NULL
    return(x)
  })

  # make data.frames and rbind()
  measures_df <- do.call(rbind, measures_list)

  if (length(target_params) > 1) {
    # create a data frame for combined measures
    measures_df_combined <- do.call(cbind, measures_list_combined)
    # add combined measures
    measures_df <- as.data.frame(rbind(measures_df, measures_df_combined))
  } else {
    measures_df <- as.data.frame(measures_df)
  }

  return(measures_df)
}


#' Compute Power Measures from brms Model Fit (Wrapper for Backward Compatibility)
#'
#' This function extracts posterior samples from a fitted brms model and computes
#' various power analysis measures. It is a wrapper around compute_measures() that
#' first extracts rvars from the brmsfit object.
#'
#' @param brmsfit A fitted brms model object containing posterior samples
#' @param design A list or S7 design object containing the experimental design specification
#'
#' @return A data frame containing power analysis measures
#' @seealso [compute_measures()]
#' @keywords internal
compute_measures_brmsfit <- function(brmsfit, design) {
  # Validate inputs
  if (!inherits(brmsfit, "brmsfit")) {
    cli::cli_abort(c(
      "{.arg brmsfit} must be a fitted brms model object",
      "x" = "You supplied {.cls {class(brmsfit)}}",
      "i" = "Provide a fitted {.cls brmsfit} object"
    ))
  }

  # Extract target parameters from design
  if (inherits(design, "rctbayespower::rctbp_design") || inherits(design, "rctbp_design")) {
    target_params <- design@target_params
  } else if (is.list(design)) {
    target_params <- design$target_params
  } else {
    cli::cli_abort(c(
      "Invalid design object",
      "i" = "This is an internal error - please report"
    ))
  }

  # Extract posterior rvars from brms model
  posterior_rvars <- brms::as_draws_rvars(brmsfit, variable = target_params)

  # Call backend-agnostic compute_measures
  compute_measures(posterior_rvars, design)
}


#' Summarize Power Analysis Simulation Results
#'
#' This function aggregates raw simulation results across multiple runs to compute
#' summary statistics including power estimates, parameter estimates, convergence
#' metrics, and Monte Carlo standard errors.
#'
#' @param results_df_raw A data frame containing raw simulation results with columns:
#'   \itemize{
#'     \item `sim_cond`: Condition identifier
#'     \item `par_name`: Parameter name
#'     \item `thr_scs`: Success threshold for the parameter
#'     \item `thr_ftl`: Futility threshold for the parameter
#'     \item `pr_scs`: Probability of success for each simulation
#'     \item `pr_ftl`: Probability of futility for each simulation
#'     \item `dec_scs`: Binary success decision indicator
#'     \item `dec_ftl`: Binary futility decision indicator
#'     \item `post_med`: Posterior median estimates
#'     \item `post_mad`: Posterior median absolute deviation
#'     \item `post_mn`: Posterior mean estimates
#'     \item `post_sd`: Posterior standard deviation
#'     \item `rhat`: R-hat convergence diagnostic
#'     \item `ess_bulk`: Bulk effective sample size
#'     \item `ess_tail`: Tail effective sample size
#'     \item `converged`: Convergence status indicator
#'     \item `error_msg`: Error messages (if any)
#'   }
#' @param n_sims Integer specifying the total number of simulations run
#'
#' @return A data frame with summarized results grouped by condition and parameter,
#'   containing mean estimates and Monte Carlo standard errors (MCSE) for all metrics:
#'   \itemize{
#'     \item Probability estimates: `pr_scs_mean`, `pr_ftl_mean` with `*_mcse`
#'     \item Power estimates: `pwr_scs_mean`, `pwr_ftl_mean` with `*_mcse`
#'     \item Parameter estimates: `post_med_mean`, `post_mn_mean`, `post_mad_mean`, `post_sd_mean`
#'     \item Convergence metrics: `rhat_mean`, `ess_bulk_mean`, `ess_tail_mean`, `conv_rate_mean`
#'   }
#'   Each metric includes corresponding `*_mcse` columns with Monte Carlo standard errors.
#'
#' @details
#' The function groups results by condition ID, parameter, and thresholds, then computes:
#' \itemize{
#'   \item Mean values across simulations for all continuous metrics
#'   \item Power as the proportion of simulations meeting significance criteria
#'   \item Convergence rate as the proportion of successfully converged simulations
#'   \item Monte Carlo standard errors for uncertainty quantification
#'   \item Concatenated error messages for debugging purposes
#' }
#'
#' @seealso [compute_measures_brmsfit()]
#' @keywords internal
summarize_sims <- function(results_df_raw, n_sims) {
  # Validate input
  if (!is.data.frame(results_df_raw) || nrow(results_df_raw) == 0) {
    cli::cli_abort(c(
      "{.arg results_df_raw} must be a non-empty data frame",
      "x" = "You supplied {.type {results_df_raw}} with {.val {nrow(results_df_raw)}} rows",
      "i" = "This is an internal error - please report"
    ))
  }
  # remove rows with NA in sim_cond or par_name
  results_df_raw <- results_df_raw |>
    dplyr::filter(!is.na(sim_cond) & !is.na(par_name))


  results_summarized <- results_df_raw |>
    dplyr::group_by(sim_cond, par_name, thr_scs, thr_ftl) |>
    dplyr::summarise(
      pr_scs_mean = mean(pr_scs, na.rm = TRUE),
      pr_scs_mcse = calculate_mcse_mean(pr_scs, n_sims),
      pr_ftl_mean = mean(pr_ftl, na.rm = TRUE),
      pr_ftl_mcse = calculate_mcse_mean(pr_ftl, n_sims),
      pwr_scs_mean = mean(dec_scs, na.rm = TRUE),
      pwr_scs_mcse = calculate_mcse_power(dec_scs, n_sims),
      pwr_ftl_mean = mean(dec_ftl, na.rm = TRUE),
      pwr_ftl_mcse = calculate_mcse_power(dec_ftl, n_sims),
      post_med_mean = mean(.data$post_med, na.rm = TRUE),
      post_med_mcse = calculate_mcse_mean(.data$post_med, n_sims),
      post_mad_mean = mean(.data$post_mad, na.rm = TRUE),
      post_mad_mcse = calculate_mcse_mean(.data$post_mad, n_sims),
      post_mn_mean = mean(.data$post_mn, na.rm = TRUE),
      post_mn_mcse = calculate_mcse_mean(.data$post_mn, n_sims),
      post_sd_mean = mean(.data$post_sd, na.rm = TRUE),
      post_sd_mcse = calculate_mcse_mean(.data$post_sd, n_sims),
      rhat_mean = mean(rhat, na.rm = TRUE),
      rhat_mcse = calculate_mcse_mean(rhat, n_sims),
      ess_bulk_mean = mean(ess_bulk, na.rm = TRUE),
      ess_bulk_mcse = calculate_mcse_mean(ess_bulk, n_sims),
      ess_tail_mean = mean(ess_tail, na.rm = TRUE),
      ess_tail_mcse = calculate_mcse_mean(ess_tail, n_sims),
      conv_rate_mean = mean(converged, na.rm = TRUE),
      conv_rate_mcse = calculate_mcse_power(converged, n_sims),
      .groups = "drop"
    ) |>
    # make shorter names
    dplyr::rename(
      pr_scs = pr_scs_mean,
      se_pr_scs = pr_scs_mcse,
      pr_ftl = pr_ftl_mean,
      se_pr_ftl = pr_ftl_mcse,
      pwr_scs = pwr_scs_mean,
      se_pwr_scs = pwr_scs_mcse,
      pwr_ftl = pwr_ftl_mean,
      se_pwr_ftl = pwr_ftl_mcse,
      post_med = post_med_mean,
      se_post_med = post_med_mcse,
      post_mad = post_mad_mean,
      se_post_mad = post_mad_mcse,
      post_mn = post_mn_mean,
      se_post_mn = post_mn_mcse,
      post_sd = post_sd_mean,
      se_post_sd = post_sd_mcse,
      rhat = rhat_mean,
      se_rhat = rhat_mcse,
      ess_bulk = ess_bulk_mean,
      se_ess_bulk = ess_bulk_mcse,
      ess_tail = ess_tail_mean,
      se_ess_tail = ess_tail_mcse,
      conv_rate = conv_rate_mean,
      se_conv_rate = conv_rate_mcse
    )

  return(results_summarized)
}
