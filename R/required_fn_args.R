#' Identify Required Parameters for Design or Model Objects
#'
#' Generic wrapper function that identifies required parameters for either
#' rctbp_design or rctbp_model objects by dispatching to the
#' appropriate specific function.
#'
#' @param object Either an rctbp_design or rctbp_model object
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
  if (inherits(object, "rctbayespower::rctbp_design") || inherits(object, "rctbp_design")) {
    return(required_fn_args_design(object, print))
  } else if (inherits(object, "rctbayespower::rctbp_model") || inherits(object, "rctbp_model")) {
    return(required_fn_args_model(object, print))
  } else {
    cli::cli_abort(c(
      "{.arg object} must be either an rctbp_design or rctbp_model object",
      "x" = "You supplied {.cls {class(object)}}",
      "i" = "Provide a design or model object created with {.fn build_design} or {.fn build_model}"
    ))
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
#' @param design An rctbp_design object
#' @param print Logical. If TRUE (default), prints the required parameters to console
#'
#' @return A character vector containing the names of all required parameters
#'
#' @keywords internal
required_fn_args_design <- function(design, print = TRUE) {
  # check that design is a valid rctbp_design object (allow both namespaced and non-namespaced for testing)
  if (!inherits(design, "rctbayespower::rctbp_design") && !inherits(design, "rctbp_design")) {
    cli::cli_abort(c(
      "{.arg design} must be a valid rctbp_design object",
      "x" = "You supplied {.cls {class(design)}}",
      "i" = "Create a design object using {.fn build_design}"
    ))
  }

  # get args without defaults for the data simulation function
  params_sim <- get_args_without_defaults(design@model@data_simulation_fn)

  # Decision parameters (per-condition): thresholds, analysis_at, interim_function, adaptive
  params_decision <- c("thresholds_success", "thresholds_futility", "analysis_at",
                       "interim_function", "adaptive")

  params_all <- c(params_sim, params_decision)

  # print the parameters if requested
  if (print) {
    cli::cli_h3("Arguments that need user specification")
    cli::cli_text("")

    cli::cli_text("{.strong Simulation function parameters:}")
    if (length(params_sim) > 0) {
      param_items <- params_sim
      names(param_items) <- rep("*", length(param_items))
      cli::cli_bullets(param_items)
    } else {
      cli::cli_text("  {.emph (none)}")
    }
    cli::cli_text("")

    cli::cli_text("{.strong Decision parameters (per-condition):}")
    if (length(params_decision) > 0) {
      param_items <- params_decision
      names(param_items) <- rep("*", length(param_items))
      cli::cli_bullets(param_items)
    } else {
      cli::cli_text("  {.emph (none)}")
    }
  }

  # return the parameters needed for the design
  invisible(list(
    params_sim = params_sim,
    params_decision = params_decision,
    params_all = params_all
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
  # check that model is a valid rctbayespower_model object (allow both namespaced and non-namespaced for testing)
  if (!inherits(model, "rctbayespower::rctbp_model") && !inherits(model, "rctbp_model")) {
    cli::cli_abort(c(
      "{.arg model} must be a valid rctbp_model object",
      "x" = "You supplied {.cls {class(model)}}",
      "i" = "Create a model object using {.fn build_model}"
    ))
  }

  # get args without defaults for the model
  params <- get_args_without_defaults(model@data_simulation_fn)

  # print the parameters if requested
  if (print) {
    cli::cli_h3("Arguments that need user specification")
    cli::cli_text("")

    cli::cli_text("{.strong Simulation function parameters:}")
    if (length(params) > 0) {
      param_items <- params
      names(param_items) <- rep("*", length(param_items))
      cli::cli_bullets(param_items)
    } else {
      cli::cli_text("  {.emph (none)}")
    }
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
