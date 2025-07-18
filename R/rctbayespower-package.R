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
#'   \item{\code{\link{power_analysis}}}{Flexible Bayesian power analysis with custom models and data generation}
#'   \item{\code{\link{power_analysis_ancova}}}{Convenience wrapper for standard RCT designs with baseline covariates}
#'   \item{\code{\link{power_grid_analysis}}}{Comprehensive grid analysis varying sample sizes and effect sizes}
#'   \item{\code{\link{validate_power_design}}}{Pre-validation of analysis designs with model compilation}
#'   \item{\code{\link{validate_weighting_function}}}{Testing weighting function implementations}
#'   \item{\code{\link{plot.rctbayespower_grid}}}{Visualization of power grid analysis results}
#' }
#'
#' @section Key Features:
#' \itemize{
#'   \item Support for continuous, binary, and count outcomes
#'   \item Model caching for improved performance in grid analyses
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
