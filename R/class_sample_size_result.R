# =============================================================================
# S7 CLASS: rctbp_sample_size_result
# =============================================================================
# Result class for single-objective sample size optimization.
# Stores optimal n, feasibility status, convergence trace, and archive.

#' @importFrom S7 new_class class_any class_data.frame class_numeric
#'   class_character class_logical class_list new_property
#' @noRd
rctbp_sample_size_result <- S7::new_class(
  "rctbp_sample_size_result",
  properties = list(
    # Reference design
    design = S7::class_any,

    # Optimal sample size (integer-valued numeric)
    n_optimal = S7::new_property(
      class = S7::class_numeric,
      default = NA_real_
    ),

    # Estimated power at n_optimal
    power_optimal = S7::new_property(
      class = S7::class_numeric,
      default = NA_real_
    ),

    # User's target power
    target_power = S7::new_property(
      class = S7::class_numeric,
      default = 0.80
    ),

    # Whether target was met
    feasible = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    ),

    # Convergence trace: eval, n_total, power, score, best_score
    convergence = S7::new_property(
      class = S7::class_data.frame,
      default = data.frame()
    ),

    # All evaluated points: n_total, power, score, n_sims
    archive = S7::new_property(
      class = S7::class_data.frame,
      default = data.frame()
    ),

    # Fitted surrogate model (R6, not serializable)
    surrogate_fit = S7::new_property(
      class = S7::class_any,
      default = NULL
    ),

    # Surrogate type used
    surrogate_type = S7::new_property(
      class = S7::class_character,
      default = "gp_power"
    ),

    # Score configuration metadata
    score_config = S7::new_property(
      class = S7::class_list,
      default = list(scale = "log", shape = "linear")
    ),

    # Execution metadata
    n_sims = S7::new_property(class = S7::class_numeric, default = NA_real_),
    n_evals = S7::new_property(class = S7::class_numeric, default = NA_real_),
    elapsed_time = S7::new_property(class = S7::class_numeric, default = NA_real_)
  ),

  validator = function(self) {
    if (!is.null(self@design) &&
        !inherits(self@design, "rctbp_design") &&
        !inherits(self@design, "rctbayespower::rctbp_design")) {
      return("'design' must be a valid rctbp_design object or NULL.")
    }

    if (!is.na(self@n_optimal) && self@n_optimal != round(self@n_optimal)) {
      return("'n_optimal' must be integer-valued.")
    }

    valid_surrogates <- c("gp_power", "gp_score", "rf")
    if (!self@surrogate_type %in% valid_surrogates) {
      return(paste0(
        "'surrogate_type' must be one of: ",
        paste(valid_surrogates, collapse = ", ")
      ))
    }

    NULL
  }
)

# =============================================================================
# S7 METHOD: print() for rctbp_sample_size_result
# =============================================================================

#' Print Method for rctbp_sample_size_result
#'
#' @param x An rctbp_sample_size_result object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns x
#' @name print.rctbp_sample_size_result
#' @export
S7::method(print, rctbp_sample_size_result) <- function(x, ...) {
  cli::cli_h1("Sample Size Optimization Result")

  # Main result
  if (x@feasible) {
    cli::cli_alert_success("Optimal n = {x@n_optimal} (power = {round(x@power_optimal, 3)})")
  } else {
    cli::cli_alert_warning("No feasible solution found")
    if (!is.na(x@n_optimal)) {
      cli::cli_text("Best n = {x@n_optimal} (power = {round(x@power_optimal, 3)})")
    }
  }

  cli::cli_text("Target power: {x@target_power}")

  # Execution info
  cli::cli_h2("Execution")
  cli::cli_text("Surrogate: {x@surrogate_type}")
  cli::cli_text("Evaluations: {x@n_evals}")
  cli::cli_text("Simulations per eval: {x@n_sims}")
  if (!is.na(x@elapsed_time)) {
    cli::cli_text("Elapsed time: {round(x@elapsed_time, 2)} min")
  }

  invisible(x)
}

# =============================================================================
# S7 METHOD: summary() for rctbp_sample_size_result
# =============================================================================

#' Summary Method for rctbp_sample_size_result
#'
#' @param object An rctbp_sample_size_result object
#' @param ... Additional arguments (unused)
#'
#' @return The archive data frame
#' @name summary.rctbp_sample_size_result
#' @export
S7::method(summary, rctbp_sample_size_result) <- function(object, ...) {
  cli::cat_line("-- Optimization Archive --")
  print(object@archive)
  invisible(object@archive)
}

# =============================================================================
# S3 WRAPPERS
# =============================================================================

#' @rdname print.rctbp_sample_size_result
#' @export
print.rctbp_sample_size_result <- function(x, ...) {
  S7::method(print, rctbp_sample_size_result)(x, ...)
}

#' @rdname summary.rctbp_sample_size_result
#' @export
summary.rctbp_sample_size_result <- function(object, ...) {
  S7::method(summary, rctbp_sample_size_result)(object, ...)
}
