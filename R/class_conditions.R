# =============================================================================
# LINK HELPER FUNCTION
# =============================================================================
# Helper function to mark parameters that should vary together (co-vary 1-to-1)
# rather than forming a Cartesian product.

#' Link Parameters for Co-variation
#'
#' Marks parameters that should vary together (1-to-1) rather than forming
#' a Cartesian product. Use within the `crossed` argument of [build_conditions()].
#'
#' @param ... Named parameters that should co-vary. All must have the same
#'   number of levels.
#'
#' @return An object of class "rctbp_link" containing the linked parameters.
#'
#' @details
#' When used inside `crossed`, linked parameters vary together in lockstep
#' rather than creating all combinations. For example:
#' \itemize{
#'   \item `link(n = c(100, 200), schedule = list(c(50,100), c(100,200)))`
#'     creates 2 conditions where n and schedule vary together
#'   \item Regular params in `crossed` still form Cartesian products with
#'     linked groups
#' }
#'
#' @examples
#' \dontrun{
#' # Simple example: analysis_at varies with n_total
#' build_conditions(
#'   design = my_design,
#'   crossed = list(
#'     link(
#'       n_total = c(80, 160),
#'       analysis_at = list(c(40, 80), c(80, 160))
#'     ),
#'     b_arm_treat = c(0, 0.3)
#'   ),
#'   constant = list(p_sig_scs = 0.975, p_sig_ftl = 0.5)
#' )
#' # Creates 4 conditions: 2 (n_total × analysis_at linked) × 2 b_arm_treat
#' }
#'
#' @export
link <- function(...) {
  params <- list(...)

  # Validate names are provided

  if (is.null(names(params)) || any(names(params) == "")) {
    cli::cli_abort(c(
      "All parameters in {.fn link} must be named",
      "i" = "Use {.code link(param1 = values1, param2 = values2)}"
    ))
  }

  # Get length of each parameter
  lengths <- vapply(params, function(x) {
    if (is.list(x)) length(x) else length(x)
  }, integer(1))

  # Validate all have same length
  if (length(unique(lengths)) > 1) {
    len_info <- paste(names(params), "=", lengths, collapse = ", ")
    cli::cli_abort(c(
      "All parameters in {.fn link} must have the same number of levels",
      "x" = "Got lengths: {len_info}",
      "i" = "Each linked parameter must have one value per level"
    ))
  }

  structure(params, class = "rctbp_link")
}

# =============================================================================
# S7 CLASS DEFINITION: rctbp_conditions
# =============================================================================
# Stores parameter combinations for power analysis simulations. Creates an
# expanded grid of varying parameters combined with static values, then
# organizes arguments into simulation args (for data generation) and
# decision args (for analysis criteria).

#' @importFrom S7 new_class class_list class_any class_data.frame new_property
rctbp_conditions <- S7::new_class("rctbp_conditions",
  properties = list(
    conditions_grid = S7::class_data.frame,       # All parameter combinations
    condition_arguments = S7::class_list,         # Structured args per condition
    design = S7::class_any,                       # rctbp_design object
    crossed = S7::class_list,                     # Cartesian product params (may include link() groups)
    constant = S7::class_list,                    # Constant params across conditions
    target_pwr = S7::new_property(                # Target power for optimal condition
      class = S7::class_any,                      # Allow NULL or numeric
      default = NULL
    ),
    linked_params = S7::new_property(             # List of linked parameter groups
      class = S7::class_list,                     # Each element is a character vector of param names
      default = list()
    )
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

    # Validate target_pwr (NULL is allowed)
    if (!is.null(self@target_pwr)) {
      if (!is.numeric(self@target_pwr) || length(self@target_pwr) != 1 ||
          is.na(self@target_pwr) || self@target_pwr < 0 || self@target_pwr > 1) {
        return("'target_pwr' must be NULL or a single numeric value between 0 and 1.")
      }
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
#' simulations. Parameters are organized into two categories: `crossed` (varying)
#' and `constant` (fixed). Use [link()] within `crossed` to make parameters
#' co-vary 1-to-1 instead of forming a Cartesian product.
#'
#' @param design An rctbp_design object that defines the study design
#' @param crossed A named list of parameter vectors. All combinations (Cartesian
#'   product) will be created. Use [link()] to group parameters that should
#'   co-vary instead of cross.
#' @param constant A named list of parameter values that remain constant across
#'   all conditions.
#' @param target_pwr Target power level for identifying optimal conditions
#'   (default NULL). If NULL, the condition with highest power is shown.
#'   If specified (0 to 1), finds the smallest sample size achieving at
#'   least this power when displaying results.
#' @param condition_values Deprecated. Use `crossed` instead.
#' @param static_values Deprecated. Use `constant` instead.
#' @param linked Deprecated. Use [link()] inside `crossed` instead.
#'
#' @return An S7 object of class "rctbp_conditions" containing:
#'   \item{conditions_grid}{A data.frame with all parameter combinations}
#'   \item{condition_arguments}{A list of argument lists for each condition,
#'     separated into simulation and decision arguments}
#'   \item{design}{The original rctbp_design object}
#'   \item{crossed}{The original crossed specification (may contain link() groups)}
#'   \item{constant}{Constant parameters across all conditions}
#'   \item{target_pwr}{Target power level for optimal condition identification}
#'
#' @details
#' \strong{Parameter Categories:}
#' \itemize{
#'   \item \code{crossed}: Cartesian product - all combinations created
#'   \item \code{link()}: Within crossed, marks params to co-vary (1-to-1)
#'   \item \code{constant}: Same value for all conditions
#' }
#'
#' \strong{Example with linked parameters:}
#' \preformatted{
#' crossed = list(
#'   link(
#'     n_total = c(80, 160),
#'     analysis_at = list(c(40, 80), c(80, 160))
#'   ),
#'   b_arm_treat = c(0, 0.3)
#' )
#' }
#' Here, `n_total` and `analysis_at` co-vary (80 with c(40,80), 160 with c(80,160)),
#' and both are crossed with `b_arm_treat`, creating 4 conditions.
#'
#' @examples
#' \dontrun{
#' # Simple example without link()
#' conditions <- build_conditions(
#'   design = my_design,
#'   crossed = list(n_total = c(100, 200), b_arm_treat = c(0.3, 0.5)),
#'   constant = list(
#'     p_sig_scs = 0.975, p_sig_ftl = 0.5,
#'     thresh_scs = 0.2, thresh_ftl = 0
#'   )
#' )
#'
#' # With link() for co-varying parameters
#' conditions <- build_conditions(
#'   design = my_design,
#'   crossed = list(
#'     link(n_total = c(80, 160), analysis_at = list(c(40,80), c(80,160))),
#'     b_arm_treat = c(0, 0.3)
#'   ),
#'   constant = list(p_sig_scs = 0.975, p_sig_ftl = 0.5)
#' )
#' }
#'
#' @export
build_conditions <- function(design,
                             crossed = NULL,
                             constant = NULL,
                             target_pwr = NULL,
                             condition_values = NULL,
                             static_values = NULL,
                             linked = NULL) {
  # validate design (allow both namespaced and non-namespaced class for testing)
  if (!inherits(design, "rctbayespower::rctbp_design") && !inherits(design, "rctbp_design")) {
    cli::cli_abort(c(
      "{.arg design} must be a valid rctbp_design object",
      "x" = "Got object of class {.cls {class(design)}}",
      "i" = "Use {.fn build_design} to create a valid design object"
    ))
  }

  # ===========================================================================
  # BACKWARD COMPATIBILITY: Map old argument names to new ones
  # ===========================================================================
  if (!is.null(condition_values) && is.null(crossed)) {
    cli::cli_inform(c(
      "i" = "{.arg condition_values} is deprecated, use {.arg crossed} instead"
    ))
    crossed <- condition_values
  }
  if (!is.null(static_values) && is.null(constant)) {
    cli::cli_inform(c(
      "i" = "{.arg static_values} is deprecated, use {.arg constant} instead"
    ))
    constant <- static_values
  }
  if (!is.null(linked)) {
    cli::cli_inform(c(
      "i" = "{.arg linked} is deprecated, use {.fn link} inside {.arg crossed} instead"
    ))
    # Attempt to convert old linked format to new format
    # Old format: linked = list(n_total = list(analysis_at = list(...)))
    # New format: crossed contains link(n_total = ..., analysis_at = ...)
    # This is complex, so we'll just warn and not attempt conversion
  }

  # Initialize to empty lists if NULL
  if (is.null(crossed)) crossed <- list()
  if (is.null(constant)) constant <- list()

  # Store original crossed for the S7 object

  crossed_original <- crossed

  # ===========================================================================
  # VALIDATE INPUTS
  # ===========================================================================
  if (!is.list(crossed)) {
    cli::cli_abort(c(
      "{.arg crossed} must be a list",
      "x" = "You supplied {.type {crossed}}"
    ))
  }
  if (!is.list(constant)) {
    cli::cli_abort(c(
      "{.arg constant} must be a list",
      "x" = "You supplied {.type {constant}}"
    ))
  }

  # validate target_pwr (NULL is allowed)
  if (!is.null(target_pwr)) {
    if (!is.numeric(target_pwr) || length(target_pwr) != 1 ||
        is.na(target_pwr) || target_pwr < 0 || target_pwr > 1) {
      cli::cli_abort(c(
        "{.arg target_pwr} must be NULL or a single numeric value between 0 and 1",
        "x" = "You supplied {.val {target_pwr}}"
      ))
    }
  }

  # ===========================================================================
  # PROCESS CROSSED: Handle link() objects
  # ===========================================================================
  # Separate crossed into:
  # - Regular params: expand via Cartesian product
  # - rctbp_link objects: co-vary as units, then cross with regular params
  #
  # Strategy:
  # 1. Identify link() objects in crossed
  # 2. Convert each link() to a data frame where params co-vary
  # 3. Cross all data frames together using tidyr::expand_grid

  # Helper to check if an element is an rctbp_link object
  is_link <- function(x) inherits(x, "rctbp_link")

  # Separate link objects from regular params
  link_indices <- vapply(crossed, is_link, logical(1))
  link_objects <- crossed[link_indices]
  regular_params <- crossed[!link_indices]

  # Track linked parameter groups for the S7 object
  # Each link() creates a group of parameters that co-vary
  linked_params <- lapply(link_objects, names)

  # Convert each element (link or regular param) to a data frame for crossing
  units_to_cross <- list()

  # Process regular params - each becomes its own unit
  for (param_name in names(regular_params)) {
    param_values <- regular_params[[param_name]]
    # Create a data frame with this param
    df <- tibble::tibble(!!param_name := param_values)
    units_to_cross <- c(units_to_cross, list(df))
  }

  # Process link objects - each becomes a single unit (data frame) with multiple columns
  for (i in seq_along(link_objects)) {
    link_obj <- link_objects[[i]]
    n_levels <- length(link_obj[[1]])  # All have same length (validated in link())

    # Build data frame row by row
    df_rows <- vector("list", n_levels)
    for (j in seq_len(n_levels)) {
      row <- lapply(link_obj, function(param_values) {
        if (is.list(param_values)) {
          param_values[[j]]
        } else {
          param_values[j]
        }
      })
      df_rows[[j]] <- row
    }
    # Convert to tibble with list columns for complex values
    df <- tibble::tibble(!!!setNames(
      lapply(names(link_obj), function(nm) {
        vals <- lapply(df_rows, function(row) row[[nm]])
        # If all values are simple (length 1), simplify to vector
        if (all(vapply(vals, length, integer(1)) == 1) &&
            all(vapply(vals, function(v) !is.list(v), logical(1)))) {
          unlist(vals)
        } else {
          vals  # Keep as list column
        }
      }),
      names(link_obj)
    ))
    units_to_cross <- c(units_to_cross, list(df))
  }

  # Flatten crossed for parameter name collection (regular params + all params from link objects)
  crossed_flat <- c(regular_params, unlist(link_objects, recursive = FALSE))

  # Gather all parameter names for validation
  params_given <- c(names(crossed_flat), names(constant))

  # Check for overlapping names between crossed and constant
  params_overlap <- intersect(names(crossed_flat), names(constant))
  if (length(params_overlap) > 0) {
    cli::cli_abort(c(
      "Redundant parameters found in both {.arg crossed} and {.arg constant}",
      "x" = "Overlapping parameters: {.val {params_overlap}}",
      "i" = "Each parameter must appear in only one argument"
    ))
  }

  # check for duplicated parameter names overall
  params_redundant <- unique(params_given[duplicated(params_given)])
  if (length(params_redundant) > 0) {
    cli::cli_abort(c(
      "Duplicated parameter names detected",
      "x" = "Duplicated parameters: {.val {params_redundant}}",
      "i" = "Each parameter name must be unique"
    ))
  }

  # Check that 'adaptive' is only in constant (not yet supported in crossed)
  if ("adaptive" %in% names(crossed_flat)) {
    cli::cli_abort(c(
      "{.arg adaptive} must be specified in {.arg constant}, not {.arg crossed}",
      "i" = "Varying {.arg adaptive} across conditions is not yet supported"
    ))
  }

  # required parameters
  params_needed <- show_condition_args(design, print = FALSE)

  # Sensible defaults for non-sequential trial design
  # Parameters that have defaults and don't need explicit specification
  # NOTE: p_sig_scs, p_sig_ftl are now REQUIRED in conditions (no longer inherited from design)
  # analysis_at is optional (NULL = single-look design)
  params_with_defaults <- c("analysis_at", "interim_function", "adaptive")

  # Exclude parameters with defaults from required validation
  params_required <- setdiff(params_needed$params_all, params_with_defaults)

  # check for missing param values (excluding those with defaults)
  if (!all(params_required %in% params_given)) {
    missing_params <- setdiff(params_required, params_given)
    cli::cli_abort(c(
      "Missing required parameters",
      "x" = "The following parameters must be specified: {.val {missing_params}}",
      "i" = "Add these to {.arg crossed} or {.arg constant}"
    ))
  }

  # Ensure p_alloc is wrapped in a list for proper grid expansion
  # Rationale: p_alloc is a vector (e.g., c(0.5, 0.5)), but expand_grid treats
  # vectors as multiple conditions. Wrapping as list(c(0.5, 0.5)) keeps it intact.
  if ("p_alloc" %in% names(constant) && !is.list(constant[["p_alloc"]])) {
    constant$p_alloc <- list(constant$p_alloc)
  }

  # =============================================================================
  # GRID EXPANSION & ARGUMENT ORGANIZATION
  # =============================================================================
  # Algorithm:
  # 1. Cross all units (regular params and link groups) together
  # 2. Add condition IDs for tracking
  # 3. For each condition row, extract crossed params, merge constant
  # 4. Separate params into sim_args (data generation) and decision_args (analysis)
  # 5. Apply defaults for optional decision parameters

  # Create condition grid by crossing all units together
  if (length(units_to_cross) > 0) {
    df_grid <- Reduce(function(a, b) tidyr::expand_grid(a, b), units_to_cross)
  } else {
    # No crossed params - single condition
    df_grid <- tibble::tibble(.rows = 1)
  }
  df_grid <- tibble::rowid_to_column(df_grid, var = "id_cond")

  # Convert each row to a list for processing

  # NOTE: Cannot use apply(df_grid, 1, as.list) because apply() coerces to matrix,
  # which converts all columns to character if any column contains characters.
  # Instead, iterate over rows manually to preserve types.
  condition_arguments_flat <- lapply(seq_len(nrow(df_grid)), function(i) {
    row <- df_grid[i, , drop = FALSE]
    # Convert tibble row to named list, handling list columns properly
    lapply(stats::setNames(names(row), names(row)), function(col_name) {
      val <- row[[col_name]]
      # Unlist single-element vectors, but keep list columns as-is
      if (is.list(val)) val[[1]] else val
    })
  })

  # Combine simulation and decision arguments per condition
  condition_arguments <- lapply(condition_arguments_flat, function(condition) {
    # --- Simulation arguments ---
    sim_args <- list()
    for (param in params_needed$params_sim) {
      if (param %in% names(condition)) {
        # From crossed (including link() parameters)
        sim_args[[param]] <- condition[[param]]
      } else if (param %in% names(constant)) {
        # From constant
        sim_args[[param]] <- constant[[param]]
      } else {
        cli::cli_abort(c(
          "Missing simulation parameter: {.val {param}}",
          "i" = "Add {.val {param}} to {.arg crossed} or {.arg constant}"
        ))
      }
    }

    # --- Decision arguments (per-condition) ---
    decision_args <- list()

    # Decision parameter defaults
    # NOTE: p_sig_scs, p_sig_ftl are REQUIRED - must be specified in conditions
    # analysis_at is optional (NULL = single-look design with final analysis only)
    # interim_function = NULL is valid for sequential monitoring without stopping rules
    decision_defaults <- list(
      analysis_at = NULL,           # NULL = single final analysis
      interim_function = NULL,      # NULL = no stopping rules
      adaptive = FALSE              # FALSE = non-adaptive design
    )

    for (param in params_needed$params_decision) {
      if (param %in% names(condition)) {
        # From crossed (including link() parameters)
        decision_args[[param]] <- condition[[param]]
      } else if (param %in% names(constant)) {
        # From constant
        decision_args[[param]] <- constant[[param]]
      } else if (param %in% names(decision_defaults)) {
        # Apply default for non-sequential parameters
        decision_args[[param]] <- decision_defaults[[param]]
      } else {
        cli::cli_abort(c(
          "Missing decision parameter: {.val {param}}",
          "i" = "Add {.val {param}} to {.arg crossed} or {.arg constant}"
        ))
      }
    }

    # Process analysis_at: accept both proportions and absolute sample sizes
    n_total <- sim_args$n_total
    if (!is.null(decision_args$analysis_at)) {
      if (is.null(n_total)) {
        cli::cli_abort(c(
          "'n_total' is required when 'analysis_at' is specified",
          "i" = "Add 'n_total' to {.arg crossed} or {.arg constant}"
        ))
      }

      analysis_vals <- decision_args$analysis_at

      # Validate all values are positive
      if (any(analysis_vals <= 0)) {
        cli::cli_abort(c(
          "'analysis_at' values must be positive",
          "x" = "Got values: {.val {analysis_vals}}",
          "i" = "Use proportions (0, 1] or sample sizes (1, n_total]"
        ))
      }

      # Detect format: proportions (all <= 1) vs absolute sample sizes (any > 1)
      is_proportions <- all(analysis_vals <= 1)

      if (is_proportions) {
        # Convert proportions to integer sample sizes
        analysis_vals <- as.integer(round(analysis_vals * n_total))
      } else {
        # Absolute sample sizes - validate they are <= n_total
        if (any(analysis_vals > n_total)) {
          bad_vals <- analysis_vals[analysis_vals > n_total]
          cli::cli_abort(c(
            "'analysis_at' values must not exceed 'n_total' ({n_total})",
            "x" = "Values exceeding n_total: {.val {bad_vals}}",
            "i" = "Use proportions (0, 1] or sample sizes (1, n_total]"
          ))
        }
        # Ensure integer values
        analysis_vals <- as.integer(round(analysis_vals))
      }

      # Auto-append n_total if not already the last element
      if (analysis_vals[length(analysis_vals)] != n_total) {
        analysis_vals <- c(analysis_vals, as.integer(n_total))
      }

      decision_args$analysis_at <- analysis_vals
    }

    # =========================================================================
    # PRE-RESOLVE BOUNDARY FUNCTIONS
    # =========================================================================
    # Boundary functions (p_sig_scs, p_sig_ftl) cannot be serialized to PSOCK workers.
    # Pre-resolve them to numeric vectors using information fractions.
    # Format: vector of thresholds, one per analysis point (interim + final).

    # Determine analysis points for boundary resolution
    # If analysis_at is NULL (single-look), use n_total as sole analysis point
    analysis_points <- if (!is.null(decision_args$analysis_at)) {
      decision_args$analysis_at
    } else {
      n_total  # Single final analysis
    }

    # Calculate information fractions for boundary resolution
    info_fracs <- analysis_points / n_total

    # Pre-resolve p_sig_scs if it's a function
    # Boundary functions accept vector of info_fracs and return vector of thresholds
    if (is.function(decision_args$p_sig_scs)) {
      decision_args$p_sig_scs_display <- "boundary_function"
      decision_args$p_sig_scs <- decision_args$p_sig_scs(info_fracs)
    }

    # Pre-resolve p_sig_ftl if it's a function
    if (is.function(decision_args$p_sig_ftl)) {
      decision_args$p_sig_ftl_display <- "boundary_function"
      decision_args$p_sig_ftl <- decision_args$p_sig_ftl(info_fracs)
    }

    # Return both sets of args
    list(
      id_cond = condition$id_cond,
      sim_args = sim_args,
      decision_args = decision_args
    )
  })

  # ===========================================================================
  # UPDATE GRID WITH RESOLVED VALUES
  # ===========================================================================
  # Replace boundary functions in grid with their resolved numerical values
  # so the display shows actual thresholds, not function objects.
  for (col in c("p_sig_scs", "p_sig_ftl")) {
    if (col %in% names(df_grid) && is.list(df_grid[[col]])) {
      df_grid[[col]] <- lapply(seq_along(df_grid[[col]]), function(i) {
        original_val <- df_grid[[col]][[i]]
        if (is.function(original_val)) {
          # Get resolved value from condition_arguments
          condition_arguments[[i]]$decision_args[[col]]
        } else {
          original_val
        }
      })
    }
  }

  # Create S7 object - validation happens automatically
  # Store crossed_original to preserve user's input (including link() objects)
  conditions_obj <- rctbp_conditions(
    conditions_grid = df_grid,
    condition_arguments = condition_arguments,
    design = design,
    crossed = crossed_original,
    constant = constant,
    target_pwr = target_pwr,
    linked_params = linked_params
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
