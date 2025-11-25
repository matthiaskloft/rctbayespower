#' The 'rctbayespower' package.
#'
#' @description
#' Bayesian power analysis for randomized controlled trials (RCTs) using brms and Stan.
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
      # New column names
      "par_name",
      "thr_scs",
      "thr_ftl",
      "sim_cond",
      "sim_iter",
      "sim_anlys",
      "pr_scs",
      "pr_ftl",
      "dec_scs",
      "dec_ftl",
      "post_med",
      "post_mad",
      "post_mn",
      "post_sd",
      "pwr_scs",
      "pwr_ftl",
      "error_msg",
      # Variables from summarize_sims function
      "pr_scs_mean",
      "pr_scs_mcse",
      "pr_ftl_mean",
      "pr_ftl_mcse",
      "pwr_scs_mean",
      "pwr_scs_mcse",
      "pwr_ftl_mean",
      "pwr_ftl_mcse",
      "post_med_mean",
      "post_med_mcse",
      "post_mad_mean",
      "post_mad_mcse",
      "post_mn_mean",
      "post_mn_mcse",
      "post_sd_mean",
      "post_sd_mcse",
      "conv_rate_mean",
      "conv_rate_mcse",
      "rhat_mean",
      "rhat_mcse",
      "ess_bulk_mean",
      "ess_bulk_mcse",
      "ess_tail_mean",
      "ess_tail_mcse",
      # Standard diagnostics (unchanged)
      "rhat",
      "ess_bulk",
      "ess_tail",
      "converged",
      # Other variables
      "arm",
      "baseline",
      "convergence_rate",
      "measures",
      "res",
      "i",
      "required_parameters",
      "covariate",
      "x",
      "n"
    )
  )
}
