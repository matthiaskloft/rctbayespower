#' The 'rctbayespower' package.
#'
#' @description
#' Bayesian power analysis for randomized controlled trials (RCTs) using brms and Stan.
#' Provides tools for estimating power curves, determining optimal sample sizes, and
#' incorporating prior knowledge about treatment effects using region of practical
#' equivalence (ROPE) for decision making.
#'
#' @section Main Functions:
#' \describe{
#'   \item{[power_analysis()]}{Comprehensive power analysis varying sample sizes and effect sizes}
#'   \item{[build_model()]}{Create model specifications for power analysis}
#'   \item{[build_design()]}{Create experimental design configurations}
#'   \item{[build_conditions()]}{Generate analysis conditions from design parameters}
#'   \item{[simulate_single_run()]}{Execute single simulation run for power analysis}
#'   \item{[validate_weighting_function()]}{Testing weighting function implementations}
#'   \item{[plot()]}{Visualization of power analysis results}
#' }
#'
#' @section Key Features:
#' \itemize{
#'   \item Design prior integration for weighted power computation
#'   \item Comprehensive plotting system with multiple visualization options
#'   \item Robust parallelization with proper parameter handling
#'   \item ANCOVA models with baseline covariate support
#' }
#'
#' @name rctbayespower-package
#' @aliases rctbayespower
#' @keywords internal
#'
"_PACKAGE"

## Suppress R CMD check warnings for undefined global variables
if (getRversion() >= "2.15.1") {
  # Global variables for R CMD check
  utils::globalVariables(
    c(
      "parameter",
      "threshold_success",
      "threshold_futility",
      "id_cond",
      "success_prob",
      "futility_prob",
      "sig_success",
      "sig_futility",
      "est_median",
      "est_mad",
      "est_mean",
      "est_sd",
      "rhat",
      "ess_bulk",
      "ess_tail",
      "converged",
      "error",
      "arm",
      "baseline",
      "convergence_rate",
      "measures",
      "res",
      "i",
      "required_parameters",
      "covariate",
      # Variables from summarize_sims function
      "power_success",
      "power_futility",
      "success_prob_mean",
      "success_prob_mcse",
      "futility_prob_mean",
      "futility_prob_mcse",
      "success_power_mean",
      "success_power_mcse",
      "futility_power_mean",
      "futility_power_mcse",
      "est_median_mean",
      "est_median_mcse",
      "est_mad_mean",
      "est_mad_mcse",
      "est_mean_mean",
      "est_mean_mcse",
      "est_sd_mean",
      "est_sd_mcse",
      "rhat_mean",
      "rhat_mcse",
      "ess_bulk_mean",
      "ess_bulk_mcse",
      "ess_tail_mean",
      "ess_tail_mcse",
      "convergence_rate_mean",
      "convergence_rate_mcse"
    )
  )
}
