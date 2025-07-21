#' Compute Power Measures from brms Model Fit
#'
#' This function extracts posterior samples from a fitted brms model and computes
#' various power analysis measures including success/futility probabilities,
#' significance indicators, parameter estimates, and convergence diagnostics.
#' For multiple target parameters, it also computes combined (union) measures.
#'
#' @param brmsfit A fitted brms model object containing posterior samples
#' @param design A list containing the experimental design specification with the following components:
#'   \itemize{
#'     \item \code{target_params}: Character vector of parameter names to analyze
#'     \item \code{thresholds_success}: Numeric vector of success thresholds for each parameter
#'     \item \code{thresholds_futility}: Numeric vector of futility thresholds for each parameter
#'     \item \code{p_sig_success}: Probability threshold for declaring success significance
#'     \item \code{p_sig_futility}: Probability threshold for declaring futility significance
#'   }
#'
#' @return A data frame containing power analysis measures with the following columns:
#'   \itemize{
#'     \item \code{parameter}: Parameter name or "union" for combined measures
#'     \item \code{threshold_success}: Success threshold used for the parameter
#'     \item \code{threshold_futility}: Futility threshold used for the parameter
#'     \item \code{success_prob}: Probability that parameter exceeds success threshold
#'     \item \code{futility_prob}: Probability that parameter falls below futility threshold
#'     \item \code{sig_success}: Binary indicator (1/0) if success probability meets significance threshold
#'     \item \code{sig_futility}: Binary indicator (1/0) if futility probability meets significance threshold
#'     \item \code{est_median}: Posterior median estimate of the parameter
#'     \item \code{est_mad}: Posterior median absolute deviation of the parameter
#'     \item \code{est_mean}: Posterior mean estimate of the parameter
#'     \item \code{est_sd}: Posterior standard deviation of the parameter
#'     \item \code{rhat}: R-hat convergence diagnostic
#'     \item \code{ess_bulk}: Effective sample size for bulk estimates
#'     \item \code{ess_tail}: Effective sample size for tail estimates
#'   }
#'
#' @details
#' For single parameters, the function computes individual measures. For multiple target
#' parameters, it additionally computes combined (union) measures where success requires
#' ALL parameters to exceed their respective success thresholds simultaneously, and
#' futility requires ALL parameters to fall below their respective futility thresholds.
#'
#' If threshold vectors are shorter than the number of target parameters, the first
#' threshold value is recycled for additional parameters.
#'
#' @examples
#' \dontrun{
#' # Assume you have a fitted brms model and design specification
#' design <- list(
#'   target_params = c("b_armtreat", "b_Intercept"),
#'   thresholds_success = c(0.2, 0.0),
#'   thresholds_futility = c(0.0, 0.0),
#'   p_sig_success = 0.975,
#'   p_sig_futility = 0.95
#' )
#' measures <- compute_measures_brmsfit(fitted_model, design)
#' }
#'
#' @seealso [power_grid_analysis()], [simulate_single_run()]
#' @importFrom stats median
#' @keywords internal
compute_measures_brmsfit <- function(brmsfit, design) {
  # Validate inputs
  if (!inherits(brmsfit, "brmsfit")) {
    stop("brmsfit must be a fitted brms model object")
  }
  
  target_params <- design$target_params
  
  # Compute measures
  measures_list <- purrr::map(target_params, function(param) {
    # extract posterior samples and compute power metrics
    posterior_samples <- brms::as_draws_rvars(brmsfit, variable = param)
    
    # extract thresholds
    if (length(target_params) > 1) {
      threshold_success <- design$thresholds_success[which(target_params == param)]
      # if the threshold is NA, use the first one
      if (is.na(threshold_success)) {
        threshold_success <- design$thresholds_success[1]
      }
      threshold_futility <- design$thresholds_futility[which(target_params == param)]
      # if the threshold is NA, use the first one
      if (is.na(threshold_futility)) {
        threshold_futility <- design$thresholds_futility[1]
      }
    } else {
      threshold_success <- design$thresholds_success
      threshold_futility <- design$thresholds_futility
    }
    # calculate the probability of success / futility
    success_prob <- posterior::Pr(posterior_samples[[param]] > threshold_success)
    futility_prob <- posterior::Pr(posterior_samples[[param]] < threshold_futility)
    # significance
    sig_success <- as.numeric(success_prob >= design$p_sig_success, na.rm = TRUE)
    sig_futility <- as.numeric(futility_prob >= design$p_sig_futility, na.rm = TRUE)
    # parameter estimates
    est_median <- stats::median(posterior_samples[[param]])
    est_mad <- posterior::mad(posterior_samples[[param]])
    est_mean <- mean(posterior_samples[[param]])
    est_sd <- posterior::sd(posterior_samples[[param]])
    # convergence metrics
    rhat <- posterior::rhat(posterior_samples[[param]])
    ess_bulk <- posterior::ess_bulk(posterior_samples[[param]])
    ess_tail <- posterior::ess_tail(posterior_samples[[param]])
    
    # combine results into a list
    out_list <- list(
      parameter = param,
      threshold_success = threshold_success,
      threshold_futility = threshold_futility,
      success_prob = success_prob,
      futility_prob = futility_prob,
      sig_success = sig_success,
      sig_futility = sig_futility,
      est_median = est_median,
      est_mad = est_mad,
      est_mean = est_mean,
      est_sd = est_sd,
      rhat = rhat,
      ess_bulk = ess_bulk,
      ess_tail = ess_tail
    )
    
    return(out_list)
  })
  
  # compute combined probabilities and powers
  if (length(target_params) > 1) {
    # extract posterior samples and compute power metrics
    posterior_samples <- brms::as_draws_matrix(brmsfit, variable = target_params)
    # extract thresholds and broadcast if necessary
    if (length(target_params) > length(design$thresholds_success)) {
      thresholds_success <- rep(design$thresholds_success[1], length(target_params))
    } else {
      thresholds_success <- design$thresholds_success
    }
    if (length(target_params) > length(design$thresholds_futility)) {
      thresholds_futility <- rep(design$thresholds_futility[1], length(target_params))
    } else {
      thresholds_futility <- design$thresholds_futility
    }
    
    # calculate combined probabilities
    combined_success_prob <- mean(apply(ifelse(
      posterior_samples > thresholds_success, 1, 0
    ), 1, min))
    combined_futility_prob <- mean(apply(
      ifelse(posterior_samples < thresholds_futility, 1, 0),
      1,
      min
    ))
    
    # calculate combined significance
    combined_sig_success <- as.numeric(combined_success_prob >= design$p_sig_success, na.rm = TRUE)
    combined_sig_futility <- as.numeric(combined_futility_prob >= design$p_sig_futility, na.rm = TRUE)
    
    # combine results into a list
    measures_list_combined <- list(
      parameter = "union",
      threshold_success = NA,
      threshold_futility = NA,
      success_prob = combined_success_prob,
      futility_prob = combined_futility_prob,
      sig_success = combined_sig_success,
      sig_futility = combined_sig_success,
      est_median = NA,
      est_mad = NA,
      est_mean = NA,
      est_sd = NA,
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


#' Summarize Power Analysis Simulation Results
#'
#' This function aggregates raw simulation results across multiple runs to compute
#' summary statistics including power estimates, parameter estimates, convergence
#' metrics, and Monte Carlo standard errors.
#'
#' @param results_df_raw A data frame containing raw simulation results with columns:
#'   \itemize{
#'     \item \code{id_cond}: Condition identifier
#'     \item \code{parameter}: Parameter name
#'     \item \code{threshold_success}: Success threshold for the parameter
#'     \item \code{threshold_futility}: Futility threshold for the parameter
#'     \item \code{success_prob}: Probability of success for each simulation
#'     \item \code{futility_prob}: Probability of futility for each simulation
#'     \item \code{sig_success}: Binary success significance indicator
#'     \item \code{sig_futility}: Binary futility significance indicator
#'     \item \code{est_median}: Posterior median estimates
#'     \item \code{est_mad}: Posterior median absolute deviation
#'     \item \code{est_mean}: Posterior mean estimates
#'     \item \code{est_sd}: Posterior standard deviation
#'     \item \code{rhat}: R-hat convergence diagnostic
#'     \item \code{ess_bulk}: Bulk effective sample size
#'     \item \code{ess_tail}: Tail effective sample size
#'     \item \code{converged}: Convergence status indicator
#'     \item \code{error}: Error messages (if any)
#'   }
#' @param n_simulations Integer specifying the total number of simulations run
#'
#' @return A data frame with summarized results grouped by condition and parameter,
#'   containing mean estimates and Monte Carlo standard errors (MCSE) for all metrics:
#'   \itemize{
#'     \item Power estimates: \code{pow_success}, \code{pow_futility}
#'     \item Probability estimates: \code{success_prob}, \code{futility_prob}
#'     \item Parameter estimates: \code{est_median}, \code{est_mean}, \code{est_mad}, \code{est_sd}
#'     \item Convergence metrics: \code{rhat}, \code{ess_bulk}, \code{ess_tail}, \code{convergence_rate}
#'     \item Error summary: \code{error}
#'   }
#'   Each metric includes corresponding \code{_mcse} columns with Monte Carlo standard errors.
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
#' @seealso [compute_measures_brmsfit()], [power_grid_analysis()]
#' @keywords internal
summarize_sims <- function(results_df_raw, n_simulations) {
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
      success_prob = mean(success_prob, na.rm = TRUE),
      success_prob_mcse = calculate_mcse_mean(success_prob, n_simulations),
      futility_prob = mean(futility_prob, na.rm = TRUE),
      futility_prob_mcse = calculate_mcse_mean(futility_prob, n_simulations),
      pow_success = mean(sig_success, na.rm = TRUE),
      pow_success_mcse = calculate_mcse_power(sig_success, n_simulations),
      pow_futility = mean(sig_futility, na.rm = TRUE),
      pow_futility_mcse = calculate_mcse_power(sig_futility, n_simulations),
      est_median = mean(est_median, na.rm = TRUE),
      est_median_mcse = calculate_mcse_mean(est_median, n_simulations),
      est_mad = mean(est_mad, na.rm = TRUE),
      est_mad_mcse = calculate_mcse_mean(est_mad, n_simulations),
      est_mean = mean(est_mean, na.rm = TRUE),
      est_mean_mcse = calculate_mcse_mean(est_mean, n_simulations),
      est_sd = mean(est_sd, na.rm = TRUE),
      est_sd_mcse = calculate_mcse_mean(est_sd, n_simulations),
      rhat = mean(rhat, na.rm = TRUE),
      rhat_mcse = calculate_mcse_mean(rhat, n_simulations),
      ess_bulk = mean(ess_bulk, na.rm = TRUE),
      ess_bulk_mcse = calculate_mcse_mean(ess_bulk, n_simulations),
      ess_tail = mean(ess_tail, na.rm = TRUE),
      ess_tail_mcse = calculate_mcse_mean(ess_tail, n_simulations),
      convergence_rate = sum(converged, na.rm = TRUE) / n_simulations,
      convergence_rate_mcse = calculate_mcse_power(convergence_rate, n_simulations),
      .groups = "drop"
    )
  
  return(results_summarized)
}
