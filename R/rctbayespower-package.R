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
#'   \item{[plot.rctbayespower_sim_result()]}{Visualization of power analysis results}
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
      "i"
    )
  )
}
