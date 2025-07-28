# S7 Class Definition for RCT Bayesian Power Conditions
#' @importFrom S7 new_class class_list class_any class_data.frame
rctbp_conditions <- S7::new_class("rctbp_conditions",
  properties = list(
    conditions_grid = S7::class_data.frame,
    condition_arguments = S7::class_list,
    design = S7::class_any,  # rctbp_design objects
    model = S7::new_property(
      getter = function(self)
        self@design@model
    ),
    condition_values = S7::class_list,
    static_values = S7::class_list
  ),
  validator = function(self) {
    # Validate conditions_grid
    if (nrow(self@conditions_grid) == 0) {
      return("'conditions_grid' must have at least one row.")
    }
    # Validate condition_arguments length matches conditions_grid
    if (length(self@condition_arguments) != nrow(self@conditions_grid)) {
      return("'condition_arguments' length must match 'conditions_grid' rows.")
    }
    # Validate design object (allow both namespaced and non-namespaced for testing)
    if (!inherits(self@design, "rctbayespower::rctbp_design") && !inherits(self@design, "rctbp_design")) {
      return("'design' must be a valid rctbp_design object.")
    }
    # If all validations pass, return NULL
    NULL
  }
)

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
    stop("'design' must be a valid rctbp_design object.")
  }

  # validate inputs
  if (!is.list(condition_values)) {
    stop("'condition_values' must be a list.")
  }
  if (!is.list(static_values)) {
    stop("'static_values' must be a list.")
  }

  # gather provided parameter names
  params_given <- c(names(condition_values), names(static_values))

  # check for overlapping names between condition_values and static_values
  params_overlap <- intersect(names(condition_values), names(static_values))
  if (length(params_overlap) > 0) {
    stop(
      paste0(
        "Redundant parameter(s) found in both 'condition_values' and 'static_values': ",
        paste(params_overlap, collapse = ", ")
      )
    )
  }
  # check for duplicated parameter names overall (within or across lists)
  params_redundant <- unique(params_given[duplicated(params_given)])
  if (length(params_redundant) > 0) {
    stop(
      paste0(
        "Duplicated parameter names detected (possibly within the same list): ",
        paste(params_redundant, collapse = ", ")
      )
    )
  }

  # required parameters
  params_needed <- required_fn_args(design, print = FALSE)

  # check for missing param values
  if (!all(params_needed$params_all %in% params_given)) {
    stop(paste(
      "The following parameters are missing and must be specified:",
      paste(
        setdiff(params_needed$params_all, params_given),
        collapse = ", "
      )
    ))
  }

  # merge inputs
  all_values <- c(condition_values, static_values)

  # check p_alloc
  if ("p_alloc" %in% names(condition_values) &&
      !is.list(all_values[["p_alloc"]])) {
    condition_values$p_alloc <- list(condition_values$p_alloc)
  }
  if ("p_alloc" %in% names(static_values) &&
      !is.list(all_values[["p_alloc"]])) {
    static_values$p_alloc <- list(static_values$p_alloc)
  }

  # expansion of conditions ----------------------------------------------------

  # create condition grid (data frame of combinations)
  df_grid <- do.call(tidyr::expand_grid, condition_values)
  # add id per condition
  df_grid <- tibble::rowid_to_column(df_grid, var = "id_cond")

  # Convert each row into a list of named values
  condition_arguments_flat <- apply(df_grid, 1, as.list)

  # Combine simulation and interim arguments per condition
  condition_arguments <- lapply(condition_arguments_flat, function(condition) {
    # --- Simulation arguments ---
    sim_args <- list()
    for (param in params_needed$params_sim) {
      if (param %in% names(condition)) {
        sim_args[[param]] <- condition[[param]]
      } else if (param %in% names(static_values)) {
        sim_args[[param]] <- static_values[[param]]
      } else {
        stop(
          sprintf(
            "Parameter '%s' is missing in condition_values or static_values.",
            param
          )
        )
      }
    }
    # --- Interim arguments ---
    interim_args <- NULL
    if (!is.null(params_needed$params_interim)) {
      interim_args <- list()
      for (param in params_needed$params_interim) {
        if (param %in% names(condition)) {
          interim_args[[param]] <- condition[[param]]
        } else if (param %in% names(static_values)) {
          interim_args[[param]] <- static_values[[param]]
        } else {
          stop(
            sprintf(
              "Parameter '%s' is missing in condition_values or static_values.",
              param
            )
          )
        }
      }
    }
    # Return both sets of args
    list(
      id_cond = condition$id_cond,
      sim_args = sim_args,
      interim_args = interim_args
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


# S7 Method for Print (uses existing base print generic)

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
  cat("\nS7 Object of class: 'rctbp_conditions'\n")
  cat("--------------------------------------------------\n\n")

  # Print basic info
  n_conditions <- nrow(x@conditions_grid)
  n_params <- ncol(x@conditions_grid) - 1  # Subtract 1 for id_cond column
  n_static_params <- length(x@static_values)

  cat("Number of conditions:", n_conditions, "\n")
  cat("Number of varying parameters:", n_params, "\n")
  cat("Number of static parameters:", n_static_params, "\n\n")

  # Print the conditions grid
  cat("Condition Grid:\n")
  print(x@conditions_grid, ...)

  invisible(x)
}
