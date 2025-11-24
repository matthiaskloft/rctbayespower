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
#' @param p_sig_success Probability threshold for declaring success
#' @param p_sig_futility Probability threshold for declaring futility
#'
#' @return A data frame containing power analysis measures
#' @importFrom stats median
#' @keywords internal
compute_measures <- function(posterior_rvars, target_params, thresholds_success,
                             thresholds_futility, p_sig_success, p_sig_futility) {

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
    sig_success <- as.numeric(success_prob >= p_sig_success, na.rm = TRUE)
    sig_futility <- as.numeric(futility_prob >= p_sig_futility, na.rm = TRUE)
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
      parameter = param,
      threshold_success = threshold_success,
      threshold_futility = threshold_futility,
      success_prob = success_prob,
      futility_prob = futility_prob,
      power_success = sig_success,
      power_futility = sig_futility,
      median = est_median,
      mad = est_mad,
      mean = est_mean,
      sd = est_sd,
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

    # calculate combined probabilities
    combined_success_prob <- mean(apply(ifelse(
      posterior_samples > thresholds_success_combined, 1, 0
    ), 1, min))
    combined_futility_prob <- mean(apply(
      ifelse(posterior_samples < thresholds_futility_combined, 1, 0),
      1,
      min
    ))

    # calculate combined significance
    combined_sig_success <- as.numeric(combined_success_prob >= p_sig_success, na.rm = TRUE)
    combined_sig_futility <- as.numeric(combined_futility_prob >= p_sig_futility, na.rm = TRUE)

    # combine results into a list
    measures_list_combined <- list(
      parameter = "union",
      threshold_success = NA,
      threshold_futility = NA,
      success_prob = combined_success_prob,
      futility_prob = combined_futility_prob,
      power_success = combined_sig_success,
      power_futility = combined_sig_futility,
      median = NA,
      mad = NA,
      mean = NA,
      sd = NA,
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
    stop("'brmsfit' must be a fitted brms model object")
  }

  # Extract target parameters from design
  if (inherits(design, "rctbayespower::rctbp_design") || inherits(design, "rctbp_design")) {
    target_params <- design@target_params
  } else if (is.list(design)) {
    target_params <- design$target_params
  } else {
    stop("Invalid design object")
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
#'     \item `id_cond`: Condition identifier
#'     \item `parameter`: Parameter name
#'     \item `threshold_success`: Success threshold for the parameter
#'     \item `threshold_futility`: Futility threshold for the parameter
#'     \item `success_prob`: Probability of success for each simulation
#'     \item `futility_prob`: Probability of futility for each simulation
#'     \item `sig_success`: Binary success significance indicator
#'     \item `sig_futility`: Binary futility significance indicator
#'     \item `est_median`: Posterior median estimates
#'     \item `est_mad`: Posterior median absolute deviation
#'     \item `est_mean`: Posterior mean estimates
#'     \item `est_sd`: Posterior standard deviation
#'     \item `rhat`: R-hat convergence diagnostic
#'     \item `ess_bulk`: Bulk effective sample size
#'     \item `ess_tail`: Tail effective sample size
#'     \item `converged`: Convergence status indicator
#'     \item `error`: Error messages (if any)
#'   }
#' @param n_sims Integer specifying the total number of simulations run
#'
#' @return A data frame with summarized results grouped by condition and parameter,
#'   containing mean estimates and Monte Carlo standard errors (SE) for all metrics:
#'   \itemize{
#'     \item Probability estimates: `prob_success`, `prob_futility`
#'     \item Power estimates: `power_success`, `power_futility`
#'     \item Parameter estimates: `median`, `mean`, `mad`, `sd`
#'     \item Convergence metrics: `rhat`, `ess_bulk`, `ess_tail`, `conv_rate`
#'   }
#'   Each metric includes corresponding `_se` columns with standard errors.
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
    stop("results_df_raw must be a non-empty data frame")
  }
  # remove rows with NA in id_cond or parameter
  results_df_raw <- results_df_raw |>
    dplyr::filter(!is.na(id_cond) & !is.na(parameter))


  results_summarized <- results_df_raw |>
    dplyr::group_by(id_cond, parameter, threshold_success, threshold_futility) |>
    dplyr::summarise(
      success_prob_mean = mean(success_prob, na.rm = TRUE),
      success_prob_mcse = calculate_mcse_mean(success_prob, n_sims),
      futility_prob_mean = mean(futility_prob, na.rm = TRUE),
      futility_prob_mcse = calculate_mcse_mean(futility_prob, n_sims),
      success_power_mean = mean(power_success, na.rm = TRUE),
      success_power_mcse = calculate_mcse_power(power_success, n_sims),
      futility_power_mean = mean(power_futility, na.rm = TRUE),
      futility_power_mcse = calculate_mcse_power(power_futility, n_sims),
      est_median_mean = mean(.data$median, na.rm = TRUE),
      est_median_mcse = calculate_mcse_mean(.data$median, n_sims),
      est_mad_mean = mean(.data$mad, na.rm = TRUE),
      est_mad_mcse = calculate_mcse_mean(.data$mad, n_sims),
      est_mean_mean = mean(.data$mean, na.rm = TRUE),
      est_mean_mcse = calculate_mcse_mean(.data$mean, n_sims),
      est_sd_mean = mean(.data$sd, na.rm = TRUE),
      est_sd_mcse = calculate_mcse_mean(.data$sd, n_sims),
      rhat_mean = mean(rhat, na.rm = TRUE),
      rhat_mcse = calculate_mcse_mean(rhat, n_sims),
      ess_bulk_mean = mean(ess_bulk, na.rm = TRUE),
      ess_bulk_mcse = calculate_mcse_mean(ess_bulk, n_sims),
      ess_tail_mean = mean(ess_tail, na.rm = TRUE),
      ess_tail_mcse = calculate_mcse_mean(ess_tail, n_sims),
      convergence_rate_mean = mean(converged, na.rm = TRUE),
      convergence_rate_mcse = calculate_mcse_power(converged, n_sims),
      .groups = "drop"
    ) |>
    # make shorter names
    dplyr::rename(
      prob_success = success_prob_mean,
      prob_success_se = success_prob_mcse,
      prob_futility = futility_prob_mean,
      prob_futility_se = futility_prob_mcse,
      power_success = success_power_mean,
      power_success_se = success_power_mcse,
      power_futility = futility_power_mean,
      power_futility_se = futility_power_mcse,
      median = est_median_mean,
      median_se = est_median_mcse,
      mad = est_mad_mean,
      mad_se = est_mad_mcse,
      mean = est_mean_mean,
      mean_se = est_mean_mcse,
      sd = est_sd_mean,
      sd_se = est_sd_mcse,
      rhat = rhat_mean,
      rhat_se = rhat_mcse,
      ess_bulk = ess_bulk_mean,
      ess_bulk_se = ess_bulk_mcse,
      ess_tail = ess_tail_mean,
      ess_tail_se = ess_tail_mcse,
      conv_rate = convergence_rate_mean,
      conv_rate_se = convergence_rate_mcse
    )

  return(results_summarized)
}
