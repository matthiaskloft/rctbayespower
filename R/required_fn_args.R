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
#' required_fn_args(my_object)
#' }
#'
#' @export
required_fn_args <- function(object, print = TRUE) {
  if (inherits(object, "rctbayespower_design")) {
    return(required_fn_args_design(object, print))
  } else if (inherits(object, "rctbayespower_model")) {
    return(required_fn_args_model(object, print))
  } else {
    stop("'object' must be either an rctbayespower_design or rctbayespower_model object.")
  }
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
get_arg_defaults <- function(fn) {
  fmls <- formals(fn)
  env <- environment(fn)

  result <- lapply(fmls, function(default_expr) {
    if (identical(default_expr, quote(expr =))) {
      list(
        has_default = FALSE,
        is_null = FALSE,
        value = quote(expr =)
      )
    } else {
      val <- try(eval(default_expr, envir = env), silent = TRUE)
      list(
        has_default = TRUE,
        is_null = is.null(val),
        value = val
      )
    }
  })

  # Extract names for special cases
  missing_default <- names(result)[!vapply(result, `[[`, logical(1), "has_default")]
  null_default    <- names(result)[vapply(result, `[[`, logical(1), "is_null")]
  # Remove unevaluable or "no-default" entries from evaluated_defaults
  evaluated_defaults <- lapply(result, `[[`, "value")
  evaluated_defaults <- evaluated_defaults[!names(evaluated_defaults) %in% missing_default]
  # Always include evaluated_defaults
  output <- list(evaluated_defaults = evaluated_defaults)

  # Only include if non-empty
  if (length(missing_default) > 0) {
    output$missing_default <- missing_default
  }
  if (length(null_default) > 0) {
    output$null_default <- null_default
  }

  output
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
#' @keywords internal
required_fn_args_design <- function(design, print = TRUE) {
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
    cat("\nArguments that need user specification.\n")
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
#' data simulation function of an rctbayespower model object.
#'
#' @param model An rctbayespower_model object
#' @param print Logical. If TRUE (default), prints the required parameters to console
#'
#' @return A character vector containing the names of required parameters (returned invisibly)
#'
#' @keywords internal
required_fn_args_model <- function(model, print = TRUE) {
  # check that model is a valid rctbayespower_model object
  if (!inherits(model, "rctbayespower_model")) {
    stop("'model' must be a valid rctbayespower_model object.")
  }

  # get args without defaults for the model
  params <- get_args_without_defaults(model$data_simulation_fn)

  # print the parameters if requested
  if (print) {
    cat("\nArguments that need user specification.\n")
    cat("\nSimulation function:\n")
    cat(paste(params, collapse = ", "), "\n")
  }

  invisible(params)
}


#' Get Function Arguments Without Default Values
#'
#' Internal utility function that extracts the names of function arguments
#' that do not have default values (either missing or explicitly NULL).
#'
#' @param fn A function object to analyze
#'
#' @return A character vector containing the names of arguments without default values
#'
#' @keywords internal
#'
get_args_without_defaults <- function(fn) {
  # Get the args
  args_list <- get_arg_defaults(fn)

  missing <- args_list$missing_default
  nulls <- args_list$null_default

  # Combine the names of missing and null args
  c(missing, nulls)
}
