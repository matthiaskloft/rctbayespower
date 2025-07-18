#' Build Condition Grid for Power Analysis
#'
#' Creates a structured condition grid and argument lists for power analysis
#' simulations. This function takes varying condition parameters and static
#' parameters, validates them against the design requirements, and creates
#' all necessary argument combinations for simulation runs.
#'
#' @param design An rctbayespower_design object that defines the study design
#' @param condition_values A named list where each element contains vectors of
#'   parameter values to vary across conditions. All combinations will be created.
#' @param static_values A named list of parameter values that remain constant
#'   across all conditions
#'
#' @return An rctbayespower_conditions object containing:
#'   \item{conditions_grid}{A data.frame with all parameter combinations}
#'   \item{condition_arguments}{A list of argument lists for each condition,
#'     separated into simulation and interim analysis arguments}
#'   \item{design}{The original rctbayespower_design object}
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
#' # Create condition grid for sample size and effect size analysis
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
build_conditions <- function(design,
                             condition_values,
                             static_values) {
  # validate design
  if (!inherits(design, "rctbayespower_design")) {
    stop("'design' must be a valid rctbayespower_design object.")
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
  params_needed <- required_parameters(design, print = FALSE)

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
  if ("p_alloc" %in% names(all_values)) {
    if (!is.list(all_values[["p_alloc"]])) {
      stop("'p_alloc' must be provided as a list, e.g., list(c(0.5, 0.5)).")
    }
  } else {
    stop("'p_alloc' is missing from condition_values and static_values.")
  }

  # expansion of conditions ----------------------------------------------------

  # create condition grid (data frame of combinations)
  df_grid <- do.call(tidyr::expand_grid, condition_values)

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
    list(sim_args = sim_args, interim_args = interim_args)
  })


  # list to return
  return_list <- list(
    conditions_grid = df_grid,
    condition_arguments = condition_arguments,
    design = design
  )

  # assign class
  class(return_list) <- "rctbayespower_conditions"
  # add attribute: number of conditions
  attr(return_list, "n_conditions") <- nrow(df_grid)
  # add attribute: number of varying parameters
  attr(return_list, "n_params") <- length(condition_values)
  # add attribute: number of static parameters
  attr(return_list, "n_static_params") <- length(static_values)

  # return
  return_list
}


#' Print Method for rctbayespower_conditions Objects
#'
#' Prints a formatted summary of condition grids created by [build_conditions()].
#' Shows the condition grid with all parameter combinations and provides
#' summary information about the number of conditions and parameters.
#'
#' @param x An rctbayespower_conditions object created by [build_conditions()]
#' @param ... Additional arguments passed to [print()]
#'
#' @return Invisibly returns the input object
#'
#' @examples
#' \dontrun{
#' conditions <- build_conditions(design, condition_values, static_values)
#' print(conditions) # or just: conditions
#' }
#'
#' @export
print.rctbayespower_conditions <- function(x, ...) {
  cat("\nrctbayespower_conditions object\n")
  cat("==============================\n\n")

  # Print basic info
  n_conditions <- nrow(x$conditions_grid)
  n_params <- ncol(x$conditions_grid)

  cat("Number of conditions:", n_conditions, "\n")
  cat("Number of varying parameters:", n_params, "\n\n")

  # Print the conditions grid
  cat("Condition Grid:\n")
  print(x$conditions_grid, ...)

  invisible(x)
}



#' Get Function Arguments Without Default Values
#'
#' Extracts the names of function arguments that do not have default values.
#' This is a utility function used for identifying required parameters.
#'
#' @param fn A function object to analyze
#'
#' @return A character vector containing the names of arguments without default values
#'
#' @keywords internal
#'
#' @examples
#' # Define a function with mixed default/non-default arguments
#' test_fn <- function(a, b = 1, c, d = "default") {}
#' get_args_without_defaults(test_fn) # Returns c("a", "c")
#'
get_args_without_defaults <- function(fn) {
  fmls <- formals(fn)
  is_missing_default <- vapply(fmls, function(x) {
    identical(x, quote(expr = ))
  }, logical(1))
  names(fmls)[is_missing_default]
}


#' Identify Required Parameters for a Design
#'
#' Extracts the required parameters (those without default values) from the
#' data simulation function and interim analysis function of an rctbayespower
#' design object. This helps users identify which parameters must be specified
#' before running simulations.
#'
#' @param design An rctbayespower_design object
#' @param print Logical. If TRUE (default), prints the required parameters to console
#'
#' @return A character vector containing the names of all required parameters
#'
#' @examples
#' \dontrun{
#' # Create a simple design object first
#' design <- list(
#'   data_simulation_fn = function(n, effect_size) {},
#'   interim_analysis_fn = NULL
#' )
#' class(design) <- "rctbayespower_design"
#'
#' # Check required parameters
#' required_parameters(design)
#' }
#'
#' @export
required_parameters_design <- function(design, print = TRUE) {
  # check that design is a valid rctbayespower_design object
  if (!inherits(design, "rctbayespower_design")) {
    stop("'design' must be a valid rctbayespower_design object.")
  }

  # get args without defaults for the data simulation function
  params_sim <- get_args_without_defaults(design$data_simulation_fn)

  # if interim analysis function is not NULL, get args without defaults
  if (!is.null(design$interim_analysis_fn)) {
    params_interim <-
      get_args_without_defaults(design$interim_analysis_fn)
    params <- c(params_sim, params_interim)
  } else {
    params_interim <- NULL
    params <- params_sim
  }

  # print the parameters if requested
  if (print) {
    # print the parameters needed for the design
    cat("\nRequired parameters without defaults.\n")
    cat("\nSimulation function:\n")
    cat(paste(params_sim, collapse = ", "), "\n")
    cat("\nInterim function:\n")
    cat(paste(params_interim, collapse = ", "), "\n")
  }

  # return the parameters needed for the design
  invisible(list(
    params_sim = params_sim,
    params_interim = params_interim,
    params_all = params
  ))
}


#' Identify Required Parameters for a Model
#'
#' Extracts the required parameters (those without default values) from the
#' brms design function of an rctbayespower model object.
#'
#' @param model An rctbayespower_model object
#' @param print Logical. If TRUE (default), prints the required parameters to console
#'
#' @return A character vector containing the names of required parameters (returned invisibly)
#'
#' @examples
#' \dontrun{
#' # Create a simple model object first
#' model <- list(
#'   brms_design_fn = function(n, effect_size) {}
#' )
#' class(model) <- "rctbayespower_model"
#'
#' # Check required parameters
#' required_parameters_model(model)
#' }
#'
#' @export
required_parameters_model <- function(model, print = TRUE) {
  # check that model is a valid rctbayespower_model object
  if (!inherits(model, "rctbayespower_model")) {
    stop("'model' must be a valid rctbayespower_model object.")
  }

  # get args without defaults for the model
  params <- get_args_without_defaults(model$data_simulation_fn)

  # print the parameters if requested
  if (print) {
    cat("\nRequired parameters without defaults for the model.\n")
    cat("\nSimulation function:\n")
    cat(paste(params, collapse = ", "), "\n")
  }

  invisible(params)
}

#' Identify Required Parameters for Design or Model Objects
#'
#' Generic wrapper function that identifies required parameters for either
#' rctbayespower_design or rctbayespower_model objects by dispatching to the
#' appropriate specific function.
#'
#' @param object Either an rctbayespower_design or rctbayespower_model object
#' @param print Logical. If TRUE (default), prints the required parameters to console
#'
#' @return For design objects: a list with simulation, interim, and all parameters.
#'   For model objects: a character vector of required parameters.
#'   Both returned invisibly.
#'
#' @examples
#' \dontrun{
#' # Works with both design and model objects
#' required_parameters(my_design)
#' required_parameters(my_model)
#' }
#'
#' @export
required_parameters <- function(object, print = TRUE) {
  if (inherits(object, "rctbayespower_design")) {
    return(required_parameters_design(object, print))
  } else if (inherits(object, "rctbayespower_model")) {
    return(required_parameters_model(object, print))
  } else {
    stop("'object' must be either an rctbayespower_design or rctbayespower_model object.")
  }
}
