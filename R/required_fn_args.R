#' Show Available Target Parameters
#'
#' Displays the parameter names available for use as `target_params` in [build_design()].
#' These are the model parameters that can be tested for success/futility.
#'
#' @param x One of: an rctbp_design object, a predefined model name (character),
#'   a brmsfit object, or a BayesFlow model. If NULL, shows usage info.
#'
#' @return A character vector of available parameter names (returned invisibly).
#'   If x is NULL, returns NULL invisibly after showing usage info.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # From a predefined model name
#' show_target_params("ancova_cont_2arms")
#'
#' # From a design object
#' design <- build_design(model_name = "ancova_cont_2arms", target_params = "b_arm2")
#' show_target_params(design)
#'
#' # From a brmsfit object
#' show_target_params(my_brmsfit)
#' }
show_target_params <- function(x = NULL) {
  # Handle NULL - show usage info
  if (is.null(x)) {
    cli::cli_inform(c(
      "i" = "Usage: {.fn show_target_params}(x)",
      "i" = "Pass a model name, {.cls rctbp_design}, {.cls brmsfit}, or BayesFlow model"
    ))
    return(invisible(NULL))
  }

  # Handle character string (predefined model name)
  if (is.character(x)) {
    if (length(x) != 1) {
      cli::cli_abort(c(
        "{.arg x} must be a single model name",
        "i" = "Use {.fn show_predefined_models} to see available models"
      ))
    }

    # Load the model to get parameter names
    model_components <- load_predefined_model_components(x, backend = "brms")
    params <- grep("^b_", brms::variables(model_components$inference_model), value = TRUE)

  # Handle rctbp_design object
  } else if (inherits(x, "rctbayespower::rctbp_design") || inherits(x, "rctbp_design")) {
    params <- x@par_names_inference

  # Handle brmsfit object
  } else if (inherits(x, "brmsfit")) {
    params <- grep("^b_", brms::variables(x), value = TRUE)

  # Handle BayesFlow model (Python object via reticulate)
  } else if (inherits(x, "python.builtin.object")) {
    params <- get_bf_parameter_names(x)

  } else {
    cli::cli_abort(c(
      "{.arg x} must be a model name, rctbp_design, brmsfit, or BayesFlow model",
      "x" = "You supplied {.cls {class(x)}}"
    ))
  }

  cli::cli_h3("Available Target Parameters")
  if (length(params) > 0) {
    param_items <- params
    names(param_items) <- rep("*", length(param_items))
    cli::cli_bullets(param_items)
  } else {
    cli::cli_text("  {.emph (none available)}")
  }

  invisible(params)
}


#' Show Required Arguments for build_conditions()
#'
#' Displays the required arguments for [build_conditions()] based on a design object.
#' This function helps users understand which parameters need to be specified
#' in `crossed` or `constant` arguments. Use [link()] inside `crossed` for
#' parameters that should co-vary 1-to-1.
#'
#' @param design An rctbp_design object (default NULL). If NULL, shows usage info.
#' @param print Logical. If TRUE (default), prints the required parameters to console
#'
#' @return A list with simulation and decision parameters (returned invisibly).
#'   If design is NULL, returns NULL invisibly after showing usage info.
#'
#' @examples
#' \dontrun{
#' # Show usage info
#' show_condition_args()
#'
#' # Show required args for a design
#' design <- build_design(model_name = "ancova_cont_2arms", target_params = "b_arm2")
#' show_condition_args(design)
#' }
#'
#' @export
show_condition_args <- function(design = NULL, print = TRUE) {
  # Handle NULL - show usage info
  if (is.null(design)) {
    if (print) {
      cli::cli_inform(c(
        "i" = "Usage: {.fn show_condition_args}(design)",
        "i" = "Pass an {.cls rctbp_design} object to see required arguments for {.fn build_conditions}"
      ))
    }
    return(invisible(NULL))
  }

  # Validate design object
  if (!inherits(design, "rctbayespower::rctbp_design") && !inherits(design, "rctbp_design")) {
    cli::cli_abort(c(
      "{.arg design} must be an rctbp_design object",
      "x" = "You supplied {.cls {class(design)}}",
      "i" = "Create a design object using {.fn build_design}"
    ))
  }

  # Get args without defaults for the data simulation function

  params_sim <- get_args_without_defaults(design@sim_fn)

  # Decision parameters (per-condition): thresholds, boundaries, analysis_at, interim
  params_decision <- c("thr_fx_eff", "thr_fx_fut", "thr_dec_eff", "thr_dec_fut",
                       "analysis_at", "interim_function", "adaptive")

  params_all <- c(params_sim, params_decision)

  # Print the parameters if requested
  if (print) {
    cli::cli_h3("Required Arguments for build_conditions()")
    cli::cli_text("")

    # Simulation parameters
    cli::cli_text("{.strong Simulation parameters:}")
    if (length(params_sim) > 0) {
      param_items <- params_sim
      names(param_items) <- rep("*", length(param_items))
      cli::cli_bullets(param_items)
    } else {
      cli::cli_text("  {.emph (none required)}")
    }
    cli::cli_text("")

    # Decision parameters
    cli::cli_text("{.strong Decision parameters:}")
    cli::cli_bullets(c(
      "*" = "thr_dec_eff {.emph (probability threshold for efficacy decision; numeric or boundary function)}",
      "*" = "thr_dec_fut {.emph (probability threshold for futility decision; numeric or boundary function)}",
      "*" = "thr_fx_eff {.emph (effect size threshold for efficacy; ROPE lower bound)}",
      "*" = "thr_fx_fut {.emph (effect size threshold for futility; ROPE upper bound)}"
    ))
    cli::cli_text("  {.emph See {.fn show_boundaries} for available boundary functions}")
    cli::cli_text("")

    # Optional sequential design parameters
    cli::cli_text("{.strong Optional (sequential designs):}")
    cli::cli_bullets(c(
      "*" = "analysis_at {.emph (default NULL = single final analysis)}",
      "*" = "adaptive {.emph (default FALSE; constant only)}"
    ))
    cli::cli_text("")

    # Usage note
    cli::cli_text("{.emph Parameters go into one of two arguments:}")
    cli::cli_text("{.emph   - crossed: Cartesian product (all combinations)}")
    cli::cli_text("{.emph   - constant: same value for all conditions}")
    cli::cli_text("")
    cli::cli_text("{.emph Use link() inside crossed for co-varying params:}")
    cli::cli_text("{.emph   link(b_arm_treat = c(0, 0.3), thr_dec_eff = c(0.95, boundary_pocock(.95)))}")
  }

  # Return the parameters needed
  invisible(list(
    params_sim = params_sim,
    params_decision = params_decision,
    params_all = params_all
  ))
}

#' @rdname show_condition_args
#' @export
required_fn_args <- function(design = NULL, print = TRUE) {
  cli::cli_inform(c(
    "i" = "{.fn required_fn_args} is deprecated, use {.fn show_condition_args} instead"
  ))
  show_condition_args(design, print)
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
