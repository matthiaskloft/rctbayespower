# =============================================================================
# CORE SIMULATION FUNCTION (DEPRECATED)
# =============================================================================
# DEPRECATION NOTICE: This file is deprecated as of 2025-11.
#
# The simulate_single_run() function has been superseded by:
#   - R/worker_functions.R: worker_process_single()
#   - R/backend_brms.R: estimate_single_brms()
#   - R/backend_bf.R: estimate_single_bf()
#
# This file is kept for backward compatibility with legacy code in R/legacy/.
# It will be removed in a future version.
# =============================================================================

#' Single Run Simulation for RCT Bayesian Power Analysis (DEPRECATED)
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' This function is deprecated. Use [worker_process_single()] instead.
#'
#' @param condition_arguments A single entry from the condition_arguments list
#' @param id_iter Iteration identifier
#' @param design A rctbp_design object
#'
#' @return A data frame with power analysis measures
#'
#' @keywords internal
simulate_single_run <- function(condition_arguments,
                                id_iter,
                                design) {

  # Forward to new worker function
  worker_process_single(
    id_cond = condition_arguments$id_cond,
    id_iter = id_iter,
    condition_args = condition_arguments,
    design = design
  )
}
