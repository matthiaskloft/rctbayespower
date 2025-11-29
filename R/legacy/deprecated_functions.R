# =============================================================================
# ARCHIVED DEPRECATED FUNCTIONS
# =============================================================================
# ARCHIVE NOTICE: These functions were removed from the main package as of
# 2025-11. They are archived here for reference only.
#
# These functions are NOT loaded by the package.
#
# Replacements:
#   - get_model()          -> build_design(model_name = ...)
#   - build_model()        -> build_design()
#   - simulate_single_run() -> worker_process_single()
# =============================================================================

# -----------------------------------------------------------------------------
# get_model() - Archived from R/class_design.R
# -----------------------------------------------------------------------------

#' Get a Predefined Model (Legacy)
#'
#' @description
#' This function is superseded by [build_design()] with `model_name` parameter.
#' It was kept for backward compatibility during development.
#'
#' @param name Character. Name of the predefined model.
#' @param backend Character. Backend to use: "brms" (default) or "bf".
#'
#' @return An S7 object of class "rctbp_model" (legacy class)
#'
#' @keywords internal
get_model <- function(name, backend = c("brms", "bf")) {
  backend <- match.arg(backend)

  # Return legacy model object for backward compatibility
  get_predefined_model(name, backend)
}

# -----------------------------------------------------------------------------
# build_model() - Archived from R/class_design.R
# -----------------------------------------------------------------------------

#' Build a Custom Model (Legacy)
#'
#' @description
#' This function is superseded by [build_design()] with explicit components.
#' It was kept for backward compatibility during development.
#'
#' @param ... Arguments passed to [build_design()]
#'
#' @return An S7 object
#'
#' @keywords internal
build_model <- function(...) {
  cli::cli_abort(c(
    "build_model() has been superseded by build_design()",
    "i" = "Use build_design() with model_name or explicit components instead",
    "i" = "See ?build_design for the new API"
  ))
}

# -----------------------------------------------------------------------------
# simulate_single_run() - Archived from R/simulate_single_run.R
# -----------------------------------------------------------------------------

#' Single Run Simulation for RCT Bayesian Power Analysis (Legacy)
#'
#' @description
#' This function is superseded. Use [worker_process_single()] instead.
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
