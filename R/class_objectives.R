# =============================================================================
# S7 CLASS DEFINITIONS: Bayesian Optimization Classes
# =============================================================================
# Classes for Bayesian optimization of trial designs:
# - rctbp_objectives: Specification of optimization problem
# - rctbp_optimization_result: Results from optimization

# =============================================================================
# HELPER CLASS: rctbp_target
# =============================================================================
# Represents a target constraint for an objective (e.g., power >= 0.80)

#' Create a Target Constraint for Optimization
#'
#' Defines a target value for an objective, used to find the smallest sample
#' size (or other search parameter) that achieves the target. For example,
#' `target(0.80)` finds the minimum n_total where power >= 80%.
#'
#' @param value Numeric target value to achieve
#' @param direction Comparison direction: ">=" (default) or "<="
#'
#' @return An object of class "rctbp_target"
#'
#' @export
#' @seealso [build_objectives()], [optimization()]
#'
#' @examples
#' \dontrun{
#' # Find minimum sample size for 80% power
#' obj <- build_objectives(
#'   design = design,
#'   search = list(n_total = c(50, 500)),
#'   objectives = list(pwr_eff = target(0.80)),
#'   constant = list(...)
#' )
#' }
target <- function(value, direction = ">=") {
  if (!is.numeric(value) || length(value) != 1 || is.na(value)) {
    cli::cli_abort(c(
      "{.arg value} must be a single numeric value",
      "x" = "You supplied {.val {value}}"
    ))
  }
  if (!direction %in% c(">=", "<=")) {
    cli::cli_abort(c(
      "{.arg direction} must be '>=' or '<='",
      "x" = "You supplied {.val {direction}}"
    ))
  }

  structure(
    list(value = value, direction = direction),
    class = "rctbp_target"
  )
}

#' @export
print.rctbp_target <- function(x, ...) {
  cat(sprintf("target(%s %s)\n", x$direction, x$value))
  invisible(x)
}

# =============================================================================
# S7 CLASS: rctbp_objectives
# =============================================================================
# Specification of an optimization problem:
# - design: Reference to rctbp_design
# - search: Parameter bounds or simplex specs for optimization
# - search_specs: Parsed simplex search specifications (internal)
# - objectives: What to optimize (maximize, minimize, or target)
# - constant: Fixed parameters

#' @importFrom S7 new_class class_list class_any new_property class_character class_function
rctbp_objectives <- S7::new_class(
  "rctbp_objectives",
  properties = list(
    # Reference to rctbp_design object
    design = S7::class_any,

    # Named list of parameter bounds: list(n_total = c(50, 500))
    # Can also contain simplex specs: list(p_alloc = search_p_alloc(min = 0.1))
    search = S7::new_property(
      class = S7::class_list,
      default = list()
    ),

    # Internal: parsed simplex search specifications
    # Contains processed info for p_alloc, analysis_at simplex searches
    search_specs = S7::new_property(
      class = S7::class_list,
      default = list()
    ),

    # Objectives specification: list(pwr_eff = "maximize") or list(pwr_eff = target(0.80))
    objectives = S7::new_property(
      class = S7::class_list,
      default = list()
    ),

    # Fixed parameters (same for all evaluations)
    constant = S7::new_property(
      class = S7::class_list,
      default = list()
    ),

    # Optional user-defined cost function
    cost_fn = S7::new_property(
      class = S7::class_function | NULL,
      default = NULL
    ),

    # Secondary objectives: params to minimize/maximize after target achieved
    # e.g., list(n_total = "minimize"). Auto-inferred if NULL
    secondary = S7::new_property(
      class = S7::class_list,
      default = list()
    ),

    # Internal: parsed objective information
    objective_info = S7::new_property(
      class = S7::class_list,
      default = list()
    )
  ),

  validator = function(self) {
    # Validate design
    if (!inherits(self@design, "rctbp_design") &&
        !inherits(self@design, "rctbayespower::rctbp_design")) {
      return("'design' must be a valid rctbp_design object.")
    }

    # Validate search has at least one parameter
    if (length(self@search) == 0) {
      return("'search' must specify at least one parameter with bounds.")
    }

    # Validate search parameters have 2-element bounds OR are simplex specs
    for (param_name in names(self@search)) {
      spec <- self@search[[param_name]]

      # Skip validation for simplex search specs (handled by build_objectives)
      if (inherits(spec, "rctbp_search_p_alloc") ||
          inherits(spec, "rctbp_search_looks")) {
        next
      }

      # Standard bounds validation
      if (!is.numeric(spec) || length(spec) != 2) {
        return(sprintf(
          "'search$%s' must be a numeric vector of length 2 (lower, upper bounds) or a simplex search spec.",
          param_name
        ))
      }
      if (spec[1] >= spec[2]) {
        return(sprintf(
          "'search$%s' lower bound must be less than upper bound.",
          param_name
        ))
      }
    }

    # Validate objectives has at least one objective
    if (length(self@objectives) == 0) {
      return("'objectives' must specify at least one objective.")
    }

    # Validate objectives are valid types
    valid_obj_types <- c("maximize", "minimize", "max", "min")
    for (obj_name in names(self@objectives)) {
      obj_val <- self@objectives[[obj_name]]
      if (!inherits(obj_val, "rctbp_target") &&
          !(is.character(obj_val) && obj_val %in% valid_obj_types)) {
        return(sprintf(
          "'objectives$%s' must be 'maximize', 'minimize', or target().",
          obj_name
        ))
      }
    }

    # Check for overlapping parameters between search and constant
    overlap <- intersect(names(self@search), names(self@constant))
    if (length(overlap) > 0) {
      return(sprintf(
        "Parameters cannot be in both 'search' and 'constant': %s",
        paste(overlap, collapse = ", ")
      ))
    }

    NULL
  }
)

# =============================================================================
# S7 CLASS: rctbp_optimization_result
# =============================================================================

#' Optimization Result Class
#'
#' S7 class containing results from Bayesian optimization of trial designs.
#'
#' @section Properties:
#' \describe{
#'   \item{objectives}{Reference to the original [rctbp_objectives] specification}
#'   \item{archive}{Data frame of all evaluations (parameters and objective values)}
#'   \item{result}{Data frame with optimal design(s) - single row for single-objective,
#'     multiple rows for Pareto front}
#'   \item{pareto_front}{Data frame with Pareto-optimal solutions for multi-objective
#'     optimization (NULL for single-objective)}
#'   \item{convergence}{Data frame tracking best objective value over iterations}
#'   \item{best_power_analysis}{Full [rctbp_power_analysis] object from the best solution}
#'   \item{reference_values}{Named list of reference values from warmup phase used for
#'     secondary objective scaling (e.g., `list(n_total = 150)`)}
#'   \item{n_sims}{Number of simulations used per evaluation}
#'   \item{n_evals}{Total number of evaluations performed}
#'   \item{elapsed_time}{Total optimization time in seconds}
#'   \item{early_stopped}{Logical indicating if optimization stopped early}
#'   \item{backend_used}{Backend used for power analysis ("brms" or "bf")}
#'   \item{optimization_type}{Type of optimization: "single", "target", or "multi"}
#'   \item{mbo_objects}{Named list of mlr3mbo/bbotk objects for advanced access:
#'     \describe{
#'       \item{instance}{bbotk OptimInstance object containing the optimization state}
#'       \item{surrogate}{mlr3mbo SurrogateLearner (e.g., Gaussian Process)}
#'       \item{acq_function}{mlr3mbo acquisition function (e.g., Expected Improvement)}
#'       \item{acq_optimizer}{mlr3mbo acquisition function optimizer}
#'       \item{optimizer}{bbotk Optimizer object}
#'       \item{loop_function}{mlr3mbo loop function (bayesopt_ego or bayesopt_parego)}
#'     }
#'   }
#' }
#'
#' @seealso [optimization()], [build_objectives()], [plot.rctbp_optimization_result]
#' @name rctbp_optimization_result
#' @importFrom S7 new_class class_list class_any class_data.frame class_numeric class_character new_property
rctbp_optimization_result <- S7::new_class(
  "rctbp_optimization_result",
  properties = list(
    # Reference to rctbp_objectives
    objectives = S7::class_any,

    # All evaluations (data frame with parameters and objective values)
    archive = S7::new_property(
      class = S7::class_data.frame,
      default = data.frame()
    ),

    # Optimal design(s) - single row for single-objective, multiple for Pareto
    result = S7::new_property(
      class = S7::class_data.frame,
      default = data.frame()
    ),

    # Pareto front for multi-objective (NULL if single)
    pareto_front = S7::new_property(
      class = S7::class_data.frame | NULL,
      default = NULL
    ),

    # Convergence trace
    convergence = S7::new_property(
      class = S7::class_data.frame,
      default = data.frame()
    ),

    # Best power analysis result (full rctbp_power_analysis from best solution)
    best_power_analysis = S7::new_property(
      class = S7::class_any,
      default = NULL
    ),

    # Reference values from warmup phase (for secondary objectives)
    # e.g., list(n_total = 150, thr_dec_eff = 0.95)
    reference_values = S7::new_property(
      class = S7::class_list,
      default = list()
    ),

    # Metadata
    n_sims = S7::new_property(class = S7::class_numeric, default = NA_real_),
    n_evals = S7::new_property(class = S7::class_numeric, default = NA_real_),
    elapsed_time = S7::new_property(class = S7::class_numeric, default = NA_real_),
    early_stopped = S7::new_property(class = S7::class_logical, default = FALSE),
    backend_used = S7::new_property(class = S7::class_character, default = ""),

    # Optimization type detected
    optimization_type = S7::new_property(
      class = S7::class_character,
      default = "single"  # "single", "target", or "multi"
    ),

    # mlr3mbo/bbotk objects for advanced access
    # Contains: instance, surrogate, acq_function, acq_optimizer, optimizer
    mbo_objects = S7::new_property(
      class = S7::class_list | NULL,
      default = NULL
    )
  ),

  validator = function(self) {
    # Validate objectives reference
    if (!inherits(self@objectives, "rctbp_objectives") &&
        !inherits(self@objectives, "rctbayespower::rctbp_objectives") &&
        !is.null(self@objectives)) {
      return("'objectives' must be a valid rctbp_objectives object or NULL.")
    }

    # Validate optimization_type
    if (!self@optimization_type %in% c("single", "target", "multi")) {
      return("'optimization_type' must be 'single', 'target', or 'multi'.")
    }

    NULL
  }
)

# =============================================================================
# S7 METHOD: print() for rctbp_objectives
# =============================================================================

#' Print Method for rctbp_objectives
#'
#' @param x An rctbp_objectives object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns x
#' @name print.rctbp_objectives
#' @export
S7::method(print, rctbp_objectives) <- function(x, ...) {
  cli::cli_h1("Optimization Objectives")

  # Design info
  cli::cli_h2("Design")
  cli::cli_text("Model: {.val {x@design@model_name}}")
  cli::cli_text("Backend: {.val {x@design@backend}}")
  cli::cli_text("Target parameters: {.val {paste(x@design@target_params, collapse = ', ')}}")

  # Search space
  cli::cli_h2("Search Space")
  for (param_name in names(x@search)) {
    bounds <- x@search[[param_name]]
    cli::cli_bullets(c("*" = "{param_name}: [{bounds[1]}, {bounds[2]}]"))
  }

  # Objectives
  cli::cli_h2("Objectives")
  for (obj_name in names(x@objectives)) {
    obj_val <- x@objectives[[obj_name]]
    if (inherits(obj_val, "rctbp_target")) {
      cli::cli_bullets(c("*" = "{obj_name}: target({obj_val$direction} {obj_val$value})"))
    } else {
      cli::cli_bullets(c("*" = "{obj_name}: {obj_val}"))
    }
  }

  # Fixed parameters (show count, not all details)
  if (length(x@constant) > 0) {
    cli::cli_h2("Constant Parameters")
    cli::cli_text("{length(x@constant)} parameter{?s} fixed")
    # Show key ones
    key_params <- c("b_arm_treat", "thr_dec_eff", "thr_dec_fut")
    for (p in key_params) {
      if (p %in% names(x@constant)) {
        val <- x@constant[[p]]
        if (is.numeric(val) && length(val) == 1) {
          cli::cli_bullets(c("*" = "{p}: {round(val, 3)}"))
        }
      }
    }
  }

  invisible(x)
}

# =============================================================================
# S7 METHOD: print() for rctbp_optimization_result
# =============================================================================

#' Print Method for rctbp_optimization_result
#'
#' @param x An rctbp_optimization_result object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns x
#' @name print.rctbp_optimization_result
#' @export
S7::method(print, rctbp_optimization_result) <- function(x, ...) {
  cli::cli_h1("Optimization Results")

  # Status
  if (nrow(x@archive) == 0) {
    cli::cli_alert_warning("Optimization not yet run")
    cli::cli_text("Use {.code optimization()} to execute")
    return(invisible(x))
  }

  # Summary
  runtime <- format_duration(x@elapsed_time)
  cli::cli_alert_success(
    "Completed in {runtime} | {x@n_evals} evaluations x {x@n_sims} sims | {x@backend_used}"
  )

  # Type-specific output
  cli::cli_h2("Optimization Type: {.val {x@optimization_type}}")

  if (x@optimization_type == "multi" && !is.null(x@pareto_front)) {
    cli::cli_text("Pareto front: {nrow(x@pareto_front)} solutions")
    cli::cli_h2("Pareto Front")
    # Show first few solutions
    n_show <- min(5, nrow(x@pareto_front))
    print(x@pareto_front[seq_len(n_show), , drop = FALSE])
    if (nrow(x@pareto_front) > n_show) {
      cli::cli_text("... and {nrow(x@pareto_front) - n_show} more")
    }
  } else {
    cli::cli_h2("Optimal Design")
    if (nrow(x@result) > 0) {
      print(x@result)

      # Show surrogate prediction with 95% CI
      if ("surrogate_se" %in% names(x@result) && !is.na(x@result$surrogate_se[1])) {
        obj_name <- names(x@objectives@objectives)[1]
        pred <- x@result$surrogate_pred[1]
        ci_lo <- x@result$surrogate_ci_lower[1]
        ci_hi <- x@result$surrogate_ci_upper[1]
        cli::cli_text("")
        cli::cli_alert_info(
          "Surrogate prediction: {obj_name} = {round(pred, 3)} (95% CI: [{round(ci_lo, 3)}, {round(ci_hi, 3)}])"
        )
      }
    }
  }

  # Show reference values from warmup phase (if any)
  if (length(x@reference_values) > 0) {
    # Filter to numeric values only
    numeric_refs <- x@reference_values[vapply(x@reference_values, is.numeric, logical(1))]
    if (length(numeric_refs) > 0) {
      cli::cli_text("")
      ref_str <- paste(
        paste0(names(numeric_refs), "=", round(unlist(numeric_refs), 1)),
        collapse = ", "
      )
      cli::cli_alert_info("Warmup reference values: {ref_str}")
    }
  }

  # Hints
  cli::cli_text("")
  cli::cli_rule()
  cli::cli_alert_info("All evaluations: {.code result@archive}")
  cli::cli_alert_info("Best power analysis: {.code result@best_power_analysis}")
  cli::cli_alert_info("mlr3mbo objects: {.code result@mbo_objects}")
  cli::cli_alert_info("Visualize: {.code plot(result)}")


  invisible(x)
}
