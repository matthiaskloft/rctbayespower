# S7 Class Definition for RCT Bayesian Power Model
#' @importFrom S7 new_class class_character class_numeric class_integer class_function class_any new_union
rctbp_model <- S7::new_class("rctbp_model",
  properties = list(
    data_simulation_fn = S7::class_function,
    brms_model = S7::class_any,  # brmsfit objects don't have S7 class
    model_name = S7::class_character,
    n_endpoints = S7::class_numeric,
    endpoint_types = S7::class_character,
    n_arms = S7::class_numeric,
    n_repeated_measures = S7::class_numeric| NULL,
    parameter_names_sim_fn = S7::class_character,
    parameter_names_brms = S7::class_character
  ),
  validator = function(self) {
    # Validate n_endpoints
    if (length(self@n_endpoints) != 1 || self@n_endpoints <= 0) {
      return("'n_endpoints' must be a positive numeric value.")
    }
    # Validate endpoint_types
    if (length(self@endpoint_types) != self@n_endpoints ||
        any(!self@endpoint_types %in% c("continuous", "binary", "count"))) {
      return("'endpoint_types' must be a character vector of length 'n_endpoints' with valid types.")
    }
    # Validate n_arms
    if (length(self@n_arms) != 1 || self@n_arms <= 0) {
      return("'n_arms' must be a positive numeric value.")
    }
    # Validate n_repeated_measures
    if (!is.null(self@n_repeated_measures) &&
        (length(self@n_repeated_measures) != 1 || self@n_repeated_measures < 0)) {
      return("'n_repeated_measures' must be a non-negative numeric value.")
    }
    # Validate brms_model
    if (!inherits(self@brms_model, "brmsfit")) {
      return("'brms_model' must be a valid brmsfit object.")
    }
    # Validate data_simulation_fn parameters
    required_params <- c("n_total", "p_alloc")
    if (!all(required_params %in% self@parameter_names_sim_fn)) {
      return("'data_simulation_fn' must have parameters 'n_total' and 'p_alloc'.")
    }
    # If all validations pass, return NULL
    NULL
  }
)

#' Create a Build Model Object
#'
#' Constructs a build_model object that encapsulates a data simulation
#' function, a compiled brms model, and associated metadata for power analysis.
#' This object serves as the foundation for Bayesian power analysis in RCTs.
#'
#' @param data_simulation_fn A function that simulates data for the RCT. Must take
#'   parameters n_total, p_alloc, and further parameters needed for
#'   data simulation.
#' @param brms_model A fitted brmsfit object that serves as the template model.
#'   This should be compiled without posterior draws (chains = 0) for efficiency.
#' @param n_endpoints Number of endpoints in the study (must be positive integer)
#' @param endpoint_types Character vector specifying the type of each endpoint.
#'   Valid types are "continuous", "binary", "count". Length must match n_endpoints.
#' @param n_arms Number of arms in the study including control arm (must be positive integer)
#' @param n_repeated_measures Number of repeated measures per participant.
#'   Use NULL or 0 for single time point studies.
#' @param model_name Optional character string providing a descriptive name for the model
#' @param pre_defined_model Optional character string specifying a predefined model
#'   to use instead of creating a custom model. Currently supported values:
#'   \itemize{
#'     \item "ancova_cont" - ANCOVA model for continuous outcomes with baseline covariate
#'   }
#'   When specified, other parameters are ignored and the predefined model is returned.
#'
#' @details
#' The build_model class encapsulates all components needed for power
#' analysis simulation:
#'
#'
#' \strong{Predefined Models:} For convenience, users can specify pre_defined_model
#' to use ready-made model configurations. This is the recommended approach for
#' standard analyses. When using predefined models, other parameters are ignored.
#'
#'
#' \strong{Custom Models:} For advanced users, custom models can be created by
#' providing all required parameters:
#'
#'
#' \strong{Data Simulation Function:} Must accept n_total (total sample size),
#' p_alloc (vector of allocation probabilities), and true_parameter_values
#' (named list of parameter values). The function should return a data.frame with
#' simulated baseline data ready for outcome generation.
#'
#' \strong{BRMS Model:} A compiled brms model that will be used as a template.
#' The model should be fitted with minimal chains (e.g., chains = 0) to serve
#' as a compilation template only.
#'
#' \strong{Validation:} The function validates that the data simulation function
#' has the required parameter structure and that the brms model is properly fitted.
#'
#' @return An S7 object of class "rctbp_model" containing the specified properties
#'
#' @export
#' @importFrom stringr str_subset
#' @seealso [build_design()], [build_model_ancova()]
#'
#' @examples
#' \dontrun{
#' # Method 1: Use predefined model (recommended)
#' ancova_model <- build_model(pre_defined_model = "ancova_cont")
#' }
build_model <- function(pre_defined_model = NULL,
                        data_simulation_fn,
                        brms_model,
                        n_endpoints = NULL,
                        endpoint_types = NULL,
                        n_arms = NULL,
                        n_repeated_measures = NULL,
                        model_name = NULL) {
  # pre-defined model ----------------------------------------------------------

  # validate pre_defined_model
  if (!is.null(pre_defined_model)) {
    if (!is.character(pre_defined_model)) {
      stop("'pre_defined_model' must be a character string or NULL.")
    }
    # create function name
    fn_name <- paste0("build_model", "_", pre_defined_model)
    if (exists(fn_name, mode = "function")) {
      fn <- get(fn_name)
      model <- fn()
      # return invisibly
      return(model)
    } else {
      stop(
        cat(
          "Pre-defined model",
          paste0("\"", pre_defined_model, "\""),
          "was not found! The 'pre_defined_model' must be one of the predefined models (see documentation)."
        )
      )
    }
  }

  # custom model ---------------------------------------------------------------

  # Early validation before S7 object creation
  if (!is.function(data_simulation_fn)) {
    stop("'data_simulation_fn' must be a valid function.")
  }
  if (!is.null(model_name) && !is.character(model_name)) {
    stop("'model_name' must be a character string or NULL.")
  }

  # retrieve the argument names data_simulation_fn
  parameter_names_sim_fn <- names(formals(data_simulation_fn))

  # retrieve parameter names from brms model
  parameter_names_brms <- stringr::str_subset(brms::variables(brms_model), pattern = "^b_")

  # strip brms model from posterior draws
  brms_model <- suppressMessages(stats::update(brms_model, chains = 0, silent = 2))

  # Set default model_name if NULL
  if (is.null(model_name)) {
    model_name <- "Custom Model"
  }

  # Create S7 object - validation happens automatically
  model_obj <- rctbp_model(
    data_simulation_fn = data_simulation_fn,
    brms_model = brms_model,
    model_name = model_name,
    n_endpoints = n_endpoints,
    endpoint_types = endpoint_types,
    n_arms = n_arms,
    n_repeated_measures = n_repeated_measures,
    parameter_names_sim_fn = parameter_names_sim_fn,
    parameter_names_brms = parameter_names_brms
  )

  return(model_obj)
}


# S7 Method for Print (uses existing base print generic)
#' @importFrom S7 method

#' Print Method for rctbp_model Objects
#'
#' Displays a summary of a rctbp_model object, including model specifications,
#' parameter information, and function details.
#'
#' @param x An S7 object of class "rctbp_model"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @export
S7::method(print, rctbp_model) <- function(x, ...) {
  cat("\nS7 Object of class: 'rctbp_model'\n")
  cat("--------------------------------------------------\n\n")

  cat("Model name:", x@model_name, "\n")
  cat("Number of endpoints:", x@n_endpoints, "\n")
  cat("Endpoint types:", paste(x@endpoint_types, collapse = ", "), "\n")
  cat("Number of arms:", x@n_arms, "\n")
  cat("Number of repeated measures:", 
      if (is.null(x@n_repeated_measures)) "NULL" else x@n_repeated_measures, "\n")
  cat("Parameter names - simulation function:",
      paste(x@parameter_names_sim_fn, collapse = ", "),
      "\n")
  cat("Parameter names - brms model:", paste(x@parameter_names_brms, collapse = ", "), "\n")
  cat("\nBrms model:\n")
  print(x@brms_model)
  
  invisible(x)
}

