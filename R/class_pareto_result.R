# =============================================================================
# S7 CLASS: rctbp_pareto_result
# =============================================================================
# Result class for Pareto optimization of trial designs.
# Stores Pareto front, archive of all evaluations, and selected knee point.

#' @importFrom S7 new_class class_list class_any class_data.frame class_numeric
#'   class_character new_property
#' @noRd
rctbp_pareto_result <- S7::new_class(
  "rctbp_pareto_result",
  properties = list(
    # Reference to rctbp_design
    design = S7::class_any,

    # Pareto-optimal solutions
    pareto_front = S7::new_property(
      class = S7::class_data.frame,
      default = data.frame()
    ),

    # All evaluations
    archive = S7::new_property(
      class = S7::class_data.frame,
      default = data.frame()
    ),

    # Selected knee point
    selected_design = S7::new_property(
      class = S7::class_data.frame,
      default = data.frame()
    ),

    # Convergence trace
    convergence = S7::new_property(
      class = S7::class_data.frame,
      default = data.frame()
    ),

    # Optimization metadata
    optimization_type = S7::new_property(
      class = S7::class_character,
      default = "custom"  # "power_n", "power_effect", "effect_n", "custom"
    ),

    # Objectives specification
    objectives = S7::new_property(
      class = S7::class_list,
      default = list()
    ),

    # Search parameter bounds
    search = S7::new_property(
      class = S7::class_list,
      default = list()
    ),

    # Execution metadata
    n_sims = S7::new_property(class = S7::class_numeric, default = NA_real_),
    n_evals = S7::new_property(class = S7::class_numeric, default = NA_real_),
    elapsed_time = S7::new_property(class = S7::class_numeric, default = NA_real_),

    # mlr3mbo/bbotk objects for advanced access
    mbo_objects = S7::new_property(
      class = S7::class_list | NULL,
      default = NULL
    )
  ),

  validator = function(self) {
    # Validate design reference
    if (!is.null(self@design) &&
        !inherits(self@design, "rctbp_design") &&
        !inherits(self@design, "rctbayespower::rctbp_design")) {
      return("'design' must be a valid rctbp_design object or NULL.")
    }

    # Validate optimization_type
    valid_types <- c("power_n", "power_effect", "effect_n", "custom")
    if (!self@optimization_type %in% valid_types) {
      return(paste0(
        "'optimization_type' must be one of: ",
        paste(valid_types, collapse = ", ")
      ))
    }

    NULL
  }
)

# =============================================================================
# S7 METHOD: print() for rctbp_pareto_result
# =============================================================================

#' Print Method for rctbp_pareto_result
#'
#' @param x An rctbp_pareto_result object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns x
#' @name print.rctbp_pareto_result
#' @export
S7::method(print, rctbp_pareto_result) <- function(x, ...) {
  cli::cli_h1("Pareto Optimization Result")

  # Optimization type
  type_labels <- c(
    power_n = "Power vs Sample Size",
    power_effect = "Power vs Effect Size",
    effect_n = "Effect Size vs Sample Size",
    custom = "Custom Pareto Optimization"
  )
  cli::cli_text("Type: {.val {type_labels[x@optimization_type]}}")

  # Objectives
  if (length(x@objectives) > 0) {
    obj_str <- vapply(names(x@objectives), function(nm) {
      paste0(nm, " (", x@objectives[[nm]], ")")
    }, character(1))
    cli::cli_text("Objectives: {paste(obj_str, collapse = ', ')}")
  }

  # Execution info
  cli::cli_h2("Execution")
  cli::cli_text("Evaluations: {x@n_evals}")
  cli::cli_text("Simulations per eval: {x@n_sims}")
  if (!is.na(x@elapsed_time)) {
    cli::cli_text("Elapsed time: {round(x@elapsed_time, 2)} min")
  }

  # Pareto front summary
  cli::cli_h2("Pareto Front")
  n_pareto <- nrow(x@pareto_front)
  cli::cli_text("{n_pareto} Pareto-optimal solution{?s}")

  if (n_pareto > 0 && length(x@objectives) == 2) {
    obj_names <- names(x@objectives)
    if (all(obj_names %in% names(x@pareto_front))) {
      obj1_range <- range(x@pareto_front[[obj_names[1]]], na.rm = TRUE)
      obj2_range <- range(x@pareto_front[[obj_names[2]]], na.rm = TRUE)
      cli::cli_text("{obj_names[1]}: [{round(obj1_range[1], 3)}, {round(obj1_range[2], 3)}]")
      cli::cli_text("{obj_names[2]}: [{round(obj2_range[1], 3)}, {round(obj2_range[2], 3)}]")
    }
  }

  # Selected design
  if (nrow(x@selected_design) > 0) {
    cli::cli_h2("Selected Design (Knee Point)")
    for (col in names(x@selected_design)) {
      val <- x@selected_design[[col]]
      if (is.numeric(val)) {
        cli::cli_text("{col}: {round(val, 4)}")
      }
    }
  }

  invisible(x)
}

# =============================================================================
# S7 METHOD: summary() for rctbp_pareto_result
# =============================================================================

#' Summary Method for rctbp_pareto_result
#'
#' @param object An rctbp_pareto_result object
#' @param ... Additional arguments (unused)
#'
#' @return The Pareto front data frame
#' @name summary.rctbp_pareto_result
#' @export
S7::method(summary, rctbp_pareto_result) <- function(object, ...) {
  cat("Pareto Front:\n")
  print(object@pareto_front)
  invisible(object@pareto_front)
}
