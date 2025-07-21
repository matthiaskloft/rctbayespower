#' Create an build_model Object
#'
#' Constructs an build_model object that encapsulates a data simulation
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
#' @return An object of class "build_model" containing:
#' \describe{
#'   \item{data_simulation_fn}{The provided data simulation function}
#'   \item{brms_model}{The compiled brms model template}
#' }
#' The object has the following attributes:
#' \describe{
#'   \item{model_name}{Descriptive name for the model}
#'   \item{n_endpoints}{Number of study endpoints}
#'   \item{endpoint_types}{Types of endpoints}
#'   \item{n_arms}{Number of arms including control arm}
#'   \item{n_repeated_measures}{Number of repeated measures}
#'   \item{parameter_names_sim_fn}{Parameter names extracted from simulation function}
#'   \item{parameter_names_brms}{Parameter names from brms model}
#' }
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

  # validate model
  if (!inherits(brms_model, "brmsfit")) {
    stop("'brms_model' must be a valid brmsfit object.")
  }
  if (!is.function(data_simulation_fn)) {
    stop("'data_simulation_fn' must be a valid function.")
  }

  # validate n_endpoints
  if (is.null(n_endpoints) ||
      !is.numeric(n_endpoints) || n_endpoints <= 0) {
    stop("'n_endpoints' must be a positive numeric value.")
  }

  # validate endpoint_types, must have length n_endpoints and be valid types
  if (is.null(endpoint_types) || !is.character(endpoint_types) ||
      length(endpoint_types) != n_endpoints ||
      any(!endpoint_types %in% c("continuous", "binary", "count"))) {
    stop(
      "'endpoint_types' must be a character vector of length 'n_endpoints' with valid types."
    )
  }

  # validate n_arms
  if (is.null(n_arms) ||
      !is.numeric(n_arms) || n_arms <= 0) {
    stop("'n_arms' must be a positive numeric value.")
  }

  # validate n_repeated_measures
  if (!is.null(n_repeated_measures) &&
      (!is.numeric(n_repeated_measures) ||
       n_repeated_measures < 0)) {
    stop("'n_repeated_measures' must be a non-negative numeric value.")
  }

  # validate model_name
  if (!is.null(model_name) && !is.character(model_name)) {
    stop("'model_name' must be a character string or NULL.")
  }

  # retrieve the argument names data_simulation_fn
  parameter_names_sim_fn <- names(formals(data_simulation_fn))

  # check that the data_simulation_fn has the required parameters n_total and
  # p_alloc
  if (!("n_total" %in% parameter_names_sim_fn) ||
      !("p_alloc" %in% parameter_names_sim_fn)) {
    stop("'data_simulation_fn' must have parameters 'n_total' and 'p_alloc'.")
  }

  # retriev parameter names from brms model
  parameter_names_brms <- stringr::str_subset(brms::variables(brms_model), pattern = "^b_")

  # strip brms model from posterior draws
  brms_model <- suppressMessages(stats::update(brms_model, chains = 0, silent = 2))

  # create the output list with the data simulation function and the brms model
  output_list <- list(
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

  # assign class to the output list
  class(output_list) <- "rctbayespower_model"

  return(output_list)
}


#' Print Method for build_model Objects
#'
#' Displays a summary of an build_model object, including model specifications,
#' parameter information, and function details.
#'
#' @param x An object of class "build_model"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @export
#' @method print rctbayespower_model
print.rctbayespower_model <- function(x, ...) {
  cat("\nObject of class: 'rctbayespower_model'\n")
  cat("--------------------------------------------------\n\n")

  cat("Model name:", attr(x, "model_name"), "\n")
  cat("Number of endpoints:", attr(x, "n_endpoints"), "\n")
  cat("Endpoint types:", paste(attr(x, "endpoint_types"), collapse = ", "), "\n")
  cat("Number of arms:", attr(x, "n_arms"), "\n")
  cat("Number of repeated measures:", x$n_repeated_measures, "\n")
  cat("Parameter names - simulation function:",
      x$parameter_names_sim_fn,
      "\n")
  cat("Parameter names - brms model:", x$parameter_names_brms, "\n")

  cat("Arguments to specify for simulation function:\n")
  print(required_fn_args_model(x, print = FALSE))
  cat("\nBrms model:\n")
  print(x$brms_model)
}
