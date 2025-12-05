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
#'   \item{result}{Data frame with optimal design. For single/target optimization,
#'     includes surrogate model predictions: `surrogate_pred`, `surrogate_se`,
#'     `surrogate_ci_lower`, `surrogate_ci_upper`. For multi-objective, same as
#'     `pareto_front`.}
#'   \item{pareto_front}{Data frame with Pareto-optimal solutions for multi-objective
#'     optimization (NULL for single/target optimization)}
#'   \item{convergence}{Data frame tracking best objective value over iterations}
#'   \item{best_power_analysis}{Full [rctbp_power_analysis] object from final confirmation
#'     simulation at the optimal point}
#'   \item{optimum_surrogate}{List with surrogate-based optimum selection results:
#'     `params` (optimal parameters), `predicted_mean`, `predicted_se`, `ci_lower`,
#'     `ci_upper`, `domain_used` (constrained search domain). NULL for multi-objective.}
#'   \item{optimum_confident}{List with confident optimum from empirical stepping:
#'     `params` (optimal parameters where CI excludes target), `value` (actual
#'     simulated objective), `se`, `ci_lower`, `ci_upper`, `n_evals` (number of
#'     evaluations to find). NULL if no point confidently achieves target within
#'     `max_final_evals` or for multi-objective.}
#'   \item{n_sims}{Maximum number of simulations per evaluation (final fidelity level)}
#'   \item{n_evals}{Total number of evaluations performed}
#'   \item{elapsed_time}{Total optimization time in minutes}
#'   \item{early_stopped}{Logical indicating if optimization stopped early via patience}
#'   \item{backend_used}{Backend used for power analysis ("brms" or "bf")}
#'   \item{optimization_type}{Type of optimization: "single", "target", or "multi"}
#'   \item{mbo_objects}{Named list of mlr3mbo/bbotk objects for advanced access:
#'     \describe{
#'       \item{instance}{bbotk OptimInstance object containing the optimization state}
#'       \item{surrogate}{mlr3mbo SurrogateLearner (GP or RF)}
#'       \item{acq_function}{mlr3mbo acquisition function}
#'       \item{acq_optimizer}{mlr3mbo acquisition function optimizer}
#'       \item{optimizer}{bbotk Optimizer object}
#'     }
#'   }
#' }
#'
#' @section Optimum Selection Methods:
#' Two optimum estimates are provided for target optimization:
#' \describe{
#'   \item{Surrogate Optimum}{Searches over constrained parameter domain (trimmed
#'     by IQR) using surrogate model predictions. Controlled by `trim_param_space`.
#'     This is the point estimate of the optimal design. Results stored in
#'     `optimum_surrogate`. Use [find_surrogate_optimum()] to recompute.}
#'   \item{Confident Optimum}{Found via empirical stepping from surrogate optimum.
#'     Starting at the surrogate optimum, simulations are run with increasing
#'     parameter values until the CI excludes the target. More conservative than
#'     the surrogate optimum. Results stored in `optimum_confident`. Step size
#'     controlled by `profile_resolution`, max steps by `max_final_evals`.}
#' }
#'
#' @seealso [optimization()], [build_objectives()], [plot.rctbp_optimization_result],
#'   [find_surrogate_optimum()]
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

    # Optimum selection results
    # Surrogate search finds optimum via surrogate model in constrained domain
    optimum_surrogate = S7::new_property(
      class = S7::class_list | NULL,
      default = NULL
    ),
    # Confident optimum: where CI excludes target (statistically confident)
    optimum_confident = S7::new_property(
      class = S7::class_list | NULL,
      default = NULL
    ),
    # Surrogate profile: predictions along search dimension(s) for visualization
    surrogate_profile = S7::new_property(
      class = S7::class_data.frame | NULL,
      default = NULL
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

  # Status: not yet run

  if (nrow(x@archive) == 0) {
    cli::cli_alert_warning("Optimization not yet run")
    cli::cli_text("Use {.code optimization()} to execute")
    return(invisible(x))
  }

  # ===========================================================================
  # SUMMARY
  # ===========================================================================
  runtime <- format_duration(x@elapsed_time)
  status_msg <- if (x@early_stopped) "Early stopped" else "Completed"
  cli::cli_alert_success(
    "{status_msg} in {runtime} | {x@n_evals} evals | {x@backend_used}"
  )

  # ===========================================================================
  # MULTI-OBJECTIVE: PARETO FRONT
  # ===========================================================================
  if (x@optimization_type == "multi" && !is.null(x@pareto_front)) {
    cli::cli_h2("Pareto Front ({nrow(x@pareto_front)} solutions)")
    n_show <- min(5, nrow(x@pareto_front))
    print(x@pareto_front[seq_len(n_show), , drop = FALSE])
    if (nrow(x@pareto_front) > n_show) {
      cli::cli_text("... and {nrow(x@pareto_front) - n_show} more")
    }
    cli::cli_text("")
    cli::cli_rule()
    cli::cli_alert_info("All evaluations: {.code result@archive}")
    cli::cli_alert_info("Visualize: {.code plot(result)}")
    return(invisible(x))
  }

  # ===========================================================================
  # SINGLE/TARGET OBJECTIVE: OPTIMAL DESIGN
  # ===========================================================================
  cli::cli_h2("Optimal Design")

  if (nrow(x@result) > 0) {
    # Show key result columns (exclude surrogate_ columns from main display)
    display_cols <- setdiff(
      names(x@result),
      c("surrogate_pred", "surrogate_se", "surrogate_ci_lower", "surrogate_ci_upper")
    )
    print(x@result[, display_cols, drop = FALSE])
  }

  # ===========================================================================
  # OPTIMUM COMPARISON (if both methods available)
  # ===========================================================================
  has_surr <- !is.null(x@optimum_surrogate) && length(x@optimum_surrogate) > 0
  has_conf <- !is.null(x@optimum_confident) && length(x@optimum_confident) > 0

  if (has_surr || has_conf) {
    cli::cli_h2("Optimum Selection")

    obj_name <- if (!is.null(x@objectives)) names(x@objectives@objectives)[1] else "objective"

    if (has_surr) {
      surr <- x@optimum_surrogate
      surr_n <- if ("n_total" %in% names(surr$params)) round(surr$params$n_total) else "N/A"
      surr_pred <- round(surr$predicted_mean, 3)
      surr_ci <- paste0("[", round(surr$ci_lower, 3), ", ", round(surr$ci_upper, 3), "]")

      # Show domain constraint if available
      if (!is.null(surr$domain_used)) {
        domain_str <- paste(
          vapply(names(surr$domain_used), function(p) {
            bounds <- surr$domain_used[[p]]
            sprintf("%s:[%.0f,%.0f]", p, bounds[1], bounds[2])
          }, character(1)),
          collapse = ", "
        )
        cli::cli_alert_info("Surrogate: n={surr_n}, {obj_name}={surr_pred} {surr_ci} (domain: {domain_str})")
      } else {
        cli::cli_alert_info("Surrogate: n={surr_n}, {obj_name}={surr_pred} {surr_ci}")
      }
    }

    if (has_conf) {
      conf <- x@optimum_confident
      conf_n <- if ("n_total" %in% names(conf$params)) round(conf$params$n_total) else "N/A"
      conf_val <- round(conf$value, 3)
      conf_ci <- paste0("[", round(conf$ci_lower, 3), ", ", round(conf$ci_upper, 3), "]")
      n_evals_str <- if (!is.null(conf$n_evals)) paste0(" (", conf$n_evals, " evals)") else ""
      cli::cli_alert_info("Confident (CI excludes target): n={conf_n}, {obj_name}={conf_val} {conf_ci}{n_evals_str}")
    } else if (has_surr) {
      cli::cli_alert_warning("No confident optimum found")
    }
  }

  # ===========================================================================
  # HINTS
  # ===========================================================================
  cli::cli_text("")
  cli::cli_rule()
  cli::cli_alert_info("Archive: {.code result@archive} ({nrow(x@archive)} rows)")
  cli::cli_alert_info("Power analysis: {.code result@best_power_analysis}")
  if (has_surr) {
    cli::cli_alert_info("Re-compute surrogate optimum: {.code find_surrogate_optimum(result)}")
  }
  cli::cli_alert_info("Visualize: {.code plot(result)}")

  invisible(x)
}
