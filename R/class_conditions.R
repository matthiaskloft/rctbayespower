# =============================================================================
# S7 CLASS DEFINITION: rctbp_conditions
# =============================================================================
# Stores parameter combinations for power analysis simulations. Creates an
# expanded grid of varying parameters combined with static values, then
# organizes arguments into simulation args (for data generation) and
# decision args (for analysis criteria).

#' @importFrom S7 new_class class_list class_any class_data.frame
rctbp_conditions <- S7::new_class("rctbp_conditions",
  properties = list(
    conditions_grid = S7::class_data.frame,      # All parameter combinations
    condition_arguments = S7::class_list,         # Structured args per condition
    design = S7::class_any,                       # rctbp_design object
    condition_values = S7::class_list,            # User-specified varying params
    static_values = S7::class_list                # User-specified constant params
  ),
  # Validator ensures grid and arguments are consistent
  validator = function(self) {
    # Validate conditions_grid has rows
    if (nrow(self@conditions_grid) == 0) {
      return("'conditions_grid' must have at least one row.")
    }

    # Validate one argument set per condition row
    if (length(self@condition_arguments) != nrow(self@conditions_grid)) {
      return("'condition_arguments' length must match 'conditions_grid' rows.")
    }

    # Validate design object
    if (!inherits(self@design, "rctbayespower::rctbp_design") && !inherits(self@design, "rctbp_design")) {
      return("'design' must be a valid rctbp_design object.")
    }

    NULL  # All validations passed
  }
)

# =============================================================================
# CONSTRUCTOR FUNCTION: build_conditions()
# =============================================================================
# Creates all parameter combinations from condition_values using expand_grid,
# combines with static_values, and separates into simulation and decision
# argument sets per condition.

#' Build Conditions for Power Analysis
#'
#' Creates a structured set of conditions and argument lists for power analysis
#' simulations. This function takes varying condition parameters and static
#' parameters, validates them against the design requirements, and creates
#' all necessary argument combinations for simulation runs.
#'
#' @param design An rctbp_design object that defines the study design
#' @param condition_values A named list where each element contains vectors of
#'   parameter values to vary across conditions. All combinations will be created.
#' @param static_values A named list of parameter values that remain constant
#'   across all conditions
#'
#' @return An S7 object of class "rctbp_conditions" containing:
#'   \item{conditions_grid}{A data.frame with all parameter combinations}
#'   \item{condition_arguments}{A list of argument lists for each condition,
#'     separated into simulation and interim analysis arguments}
#'   \item{design}{The original rctbp_design object}
#'   \item{condition_values}{The original condition_values list}
#'   \item{static_values}{The original static_values list}
#'
#' @details The function performs several validation steps:
#' \itemize{
#'   \item Checks that condition_values and static_values don't have overlapping names
#'   \item Validates that all required parameters are provided
#'   \item Ensures p_alloc is properly specified as a list
#'   \item Creates expanded grid of all condition combinations
#' }
#'
#' \strong{Interim Analysis Inheritance:}
#' Interim analysis parameters (`analysis_at`, `interim_function`, `adaptive`) are
#' inherited from the design object as defaults. This means:
#' \itemize{
#'   \item If specified in `build_design()`, they apply to all conditions automatically
#'   \item Users can override per-condition via `condition_values` or `static_values`
#'   \item If not specified anywhere, defaults to single-look design (no interim analyses)
#' }
#' This allows specifying interim specs once at design level while still permitting
#' condition-specific overrides when needed.
#'
#' @examples
#' \dontrun{
#' # Create conditions for sample size and effect size analysis
#' conditions <- build_conditions(
#'   design = my_design,
#'   condition_values = list(
#'     n_total = c(100, 200, 300),
#'     effect_size = c(0.2, 0.5, 0.8)
#'   ),
#'   static_values = list(
#'     p_alloc = list(c(0.5, 0.5)),
#'     baseline_effect = 0.1
#'   )
#' )
#'
#' # Print the conditions
#' print(conditions)
#' }
#'
#' @export
build_conditions <- function(design, condition_values, static_values) {
  # validate design (allow both namespaced and non-namespaced class for testing)
  if (!inherits(design, "rctbayespower::rctbp_design") && !inherits(design, "rctbp_design")) {
    cli::cli_abort(c(
      "{.arg design} must be a valid rctbp_design object",
      "x" = "Got object of class {.cls {class(design)}}",
      "i" = "Use {.fn build_design} to create a valid design object"
    ))
  }

  # validate inputs
  if (!is.list(condition_values)) {
    cli::cli_abort(c(
      "{.arg condition_values} must be a list",
      "x" = "You supplied {.type {condition_values}}"
    ))
  }
  if (!is.list(static_values)) {
    cli::cli_abort(c(
      "{.arg static_values} must be a list",
      "x" = "You supplied {.type {static_values}}"
    ))
  }

  # gather provided parameter names
  params_given <- c(names(condition_values), names(static_values))

  # check for overlapping names between condition_values and static_values
  params_overlap <- intersect(names(condition_values), names(static_values))
  if (length(params_overlap) > 0) {
    cli::cli_abort(c(
      "Redundant parameters found in both {.arg condition_values} and {.arg static_values}",
      "x" = "Overlapping parameters: {.val {params_overlap}}",
      "i" = "Each parameter must appear in only one list"
    ))
  }
  # check for duplicated parameter names overall (within or across lists)
  params_redundant <- unique(params_given[duplicated(params_given)])
  if (length(params_redundant) > 0) {
    cli::cli_abort(c(
      "Duplicated parameter names detected",
      "x" = "Duplicated parameters: {.val {params_redundant}}",
      "i" = "Each parameter name must be unique"
    ))
  }

  # required parameters
  params_needed <- required_fn_args(design, print = FALSE)

  # Sensible defaults for non-sequential trial design
  # Parameters that have defaults and don't need to be specified by user
  params_with_defaults <- c("analysis_at", "interim_function", "adaptive")

  # Exclude parameters with defaults from required validation
  params_required <- setdiff(params_needed$params_all, params_with_defaults)

  # check for missing param values (excluding those with defaults)
  if (!all(params_required %in% params_given)) {
    missing_params <- setdiff(params_required, params_given)
    cli::cli_abort(c(
      "Missing required parameters",
      "x" = "The following parameters must be specified: {.val {missing_params}}",
      "i" = "Add these to {.arg condition_values} or {.arg static_values}"
    ))
  }

  # Merge inputs
  all_values <- c(condition_values, static_values)

  # Ensure p_alloc is wrapped in a list for proper grid expansion
  # Rationale: p_alloc is a vector (e.g., c(0.5, 0.5)), but expand_grid treats
  # vectors as multiple conditions. Wrapping as list(c(0.5, 0.5)) keeps it intact.
  if ("p_alloc" %in% names(condition_values) &&
      !is.list(all_values[["p_alloc"]])) {
    condition_values$p_alloc <- list(condition_values$p_alloc)
  }
  if ("p_alloc" %in% names(static_values) &&
      !is.list(all_values[["p_alloc"]])) {
    static_values$p_alloc <- list(static_values$p_alloc)
  }

  # =============================================================================
  # GRID EXPANSION & ARGUMENT ORGANIZATION
  # =============================================================================
  # Algorithm:
  # 1. Create all combinations of condition_values (Cartesian product)
  # 2. Add condition IDs for tracking
  # 3. For each condition row, extract varying params and merge with static
  # 4. Separate params into sim_args (data generation) and decision_args (analysis)
  # 5. Apply defaults for optional decision parameters

  # Create condition grid (Cartesian product of all condition_values)
  df_grid <- do.call(tidyr::expand_grid, condition_values)
  df_grid <- tibble::rowid_to_column(df_grid, var = "id_cond")

  # Convert each row to a list for processing
  condition_arguments_flat <- apply(df_grid, 1, as.list)

  # Combine simulation and decision arguments per condition
  condition_arguments <- lapply(condition_arguments_flat, function(condition) {
    # --- Simulation arguments ---
    sim_args <- list()
    for (param in params_needed$params_sim) {
      if (param %in% names(condition)) {
        sim_args[[param]] <- condition[[param]]
      } else if (param %in% names(static_values)) {
        sim_args[[param]] <- static_values[[param]]
      } else {
        cli::cli_abort(c(
          "Missing simulation parameter: {.val {param}}",
          "i" = "Add {.val {param}} to {.arg condition_values} or {.arg static_values}"
        ))
      }
    }

    # --- Decision arguments (per-condition) ---
    decision_args <- list()

    # Inherit interim parameters from design as defaults
    # Rationale: Design-level interim specs apply to all conditions unless overridden.
    # Users can override per-condition via condition_values or static_values.
    # If design doesn't specify, fall back to single-look defaults (NULL/FALSE).
    #
    # Special handling: if analysis_at is set but interim_function is NULL,
    # use interim_continue() as default (sequential monitoring without stopping)
    inherited_interim_fn <- design@interim_function
    if (!is.null(design@analysis_at) && is.null(inherited_interim_fn)) {
      inherited_interim_fn <- interim_continue()
    }

    decision_defaults <- list(
      analysis_at = design@analysis_at,           # Inherit from design (NULL = final only)
      interim_function = inherited_interim_fn,    # Inherit from design or default to continue
      adaptive = design@adaptive                  # Inherit from design (FALSE = non-adaptive)
    )

    for (param in params_needed$params_decision) {
      if (param %in% names(condition)) {
        decision_args[[param]] <- condition[[param]]
      } else if (param %in% names(static_values)) {
        decision_args[[param]] <- static_values[[param]]
      } else if (param %in% names(decision_defaults)) {
        # Apply default for non-sequential parameters
        decision_args[[param]] <- decision_defaults[[param]]
      } else {
        cli::cli_abort(c(
          "Missing decision parameter: {.val {param}}",
          "i" = "Add {.val {param}} to {.arg condition_values} or {.arg static_values}"
        ))
      }
    }

    # Ensure interim_function is set when analysis_at is specified
    # (handles case where user specifies analysis_at per-condition/static but not interim_function)
    if (!is.null(decision_args$analysis_at) && is.null(decision_args$interim_function)) {
      decision_args$interim_function <- interim_continue()
    }

    # Return both sets of args
    list(
      id_cond = condition$id_cond,
      sim_args = sim_args,
      decision_args = decision_args
    )
  })


  # Create S7 object - validation happens automatically
  conditions_obj <- rctbp_conditions(
    conditions_grid = df_grid,
    condition_arguments = condition_arguments,
    design = design,
    condition_values = condition_values,
    static_values = static_values
  )

  return(conditions_obj)
}

# =============================================================================
# S7 METHOD: print()
# =============================================================================
# Displays conditions grid showing all parameter combinations and summary info.

#' Print Method for rctbp_conditions Objects
#'
#' Prints a formatted summary of condition grids created by [build_conditions()].
#' Shows the condition grid with all parameter combinations and provides
#' summary information about the number of conditions and parameters.
#'
#' @param x An S7 object of class "rctbp_conditions" created by [build_conditions()]
#' @param ... Additional arguments passed to [print()]
#'
#' @return Invisibly returns the input object
#' @importFrom S7 method
#' @name print.rctbp_conditions
#' @export
#'
#' @examples
#' \dontrun{
#' conditions <- build_conditions(design, condition_values, static_values)
#' print(conditions) # or just: conditions
#' }
#'
#' @export
S7::method(print, rctbp_conditions) <- function(x, ...) {
  report <- build_report.rctbp_conditions(x)
  render_report(report)
  invisible(x)
}
