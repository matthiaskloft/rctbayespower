# =============================================================================
# S7 CLASS DEFINITION: rctbp_model
# =============================================================================
# Encapsulates all components needed for power analysis simulation including
# data simulation function, posterior estimation model (brms or NPE), and
# trial metadata (endpoints, arms, repeated measures).

#' @importFrom S7 new_class class_character class_numeric class_integer class_function class_any new_union new_property
rctbp_model <- S7::new_class(
  "rctbp_model",
  properties = list(
    # Core components
    data_simulation_fn = S7::class_function,  # Generates trial data
    brms_model = S7::class_any | NULL,        # Template brmsfit object (backend = "brms")
    bayesflow_model = S7::class_any | NULL,   # Neural posterior model (backend = "npe")
    backend_args = S7::new_property(S7::class_list, default = list()),

    # Model metadata
    predefined_model = S7::new_union(S7::class_character, NULL),  # Name if using predefined
    model_name = S7::class_character,
    n_endpoints = S7::class_numeric,
    endpoint_types = S7::class_character,     # "continuous", "binary", or "count"
    n_arms = S7::class_numeric,
    n_repeated_measures = S7::class_numeric | NULL,

    # Computed properties (getters extract information dynamically)
    backend = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        # Backend determined by which model is present
        if (!is.null(self@brms_model)) {
          return("brms")
        } else if (!is.null(self@bayesflow_model)) {
          return("npe")
        } else {
          return(NA_character_)
        }
      }
    ),
    parameter_names_sim_fn = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        # Extract parameter names from data simulation function signature
        names(formals(self@data_simulation_fn))
      }
    ),
    parameter_names_brms = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        # Extract fixed effects parameter names from brms model (prefix "b_")
        if (!is.null(self@brms_model)) {
          stringr::str_subset(brms::variables(self@brms_model), pattern = "^b_")
        } else {
          character(0)
        }
      }
    )
  ),
  # Validator ensures object consistency and catches configuration errors early
  validator = function(self) {
    # Validate exactly one backend model is provided (not both, not neither)
    has_brms <- !is.null(self@brms_model)
    has_bayesflow <- !is.null(self@bayesflow_model)

    if (!has_brms && !has_bayesflow) {
      return("Either 'brms_model' or 'bayesflow_model' must be provided.")
    }

    if (has_brms && has_bayesflow) {
      return("Only one of 'brms_model' or 'bayesflow_model' can be provided, not both.")
    }

    # Validate model type matches backend
    if (has_brms && !inherits(self@brms_model, "brmsfit")) {
      return("'brms_model' must be a valid brmsfit object.")
    }

    # Validate backend_args is a list
    if (!is.list(self@backend_args)) {
      return("'backend_args' must be a list.")
    }

    # Validate trial structure parameters
    if (length(self@n_endpoints) != 1 || self@n_endpoints <= 0) {
      return("'n_endpoints' must be a positive numeric value.")
    }
    if (length(self@endpoint_types) != self@n_endpoints ||
        any(!self@endpoint_types %in% c("continuous", "binary", "count"))) {
      return(
        "'endpoint_types' must be a character vector of length 'n_endpoints' with valid types."
      )
    }
    if (length(self@n_arms) != 1 || self@n_arms <= 0) {
      return("'n_arms' must be a positive numeric value.")
    }
    if (!is.null(self@n_repeated_measures) &&
        (length(self@n_repeated_measures) != 1 ||
         self@n_repeated_measures < 0)) {
      return("'n_repeated_measures' must be a non-negative numeric value.")
    }

    # Validate data_simulation_fn has required parameters
    required_params <- c("n_total", "p_alloc")
    if (!all(required_params %in% self@parameter_names_sim_fn)) {
      return("'data_simulation_fn' must have parameters 'n_total' and 'p_alloc'.")
    }

    NULL  # All validations passed
  }
)

# =============================================================================
# CONSTRUCTOR FUNCTION: build_model()
# =============================================================================
# Creates rctbp_model objects with validation. Supports both predefined models
# (from internal registry) and custom models with user-specified components.

#' Create a Build Model Object
#'
#' Constructs a build_model object that encapsulates a data simulation
#' function, a posterior estimation model (brms or Bayesflow/NPE), and associated
#' metadata for power analysis. This object serves as the foundation for Bayesian
#' power analysis in RCTs with support for multiple backends.
#'
#' @param data_simulation_fn A function that simulates data for the RCT. Must take
#'   parameters n_total, p_alloc, and further parameters needed for
#'   data simulation.
#' @param brms_model A fitted brmsfit object that serves as the template model.
#'   This should be compiled without posterior draws (chains = 0) for efficiency.
#'   Provide either brms_model or bayesflow_model, not both.
#' @param bayesflow_model A trained neural posterior estimation model (keras/tensorflow).
#'   Use this for NPE backend instead of brms. Provide either brms_model or
#'   bayesflow_model, not both.
#' @param backend_args Named list of backend-specific arguments. For brms: chains,
#'   iter, cores, etc. For NPE: batch_size, n_posterior_samples, etc.
#' @param n_endpoints Number of endpoints in the study (must be positive integer)
#' @param endpoint_types Character vector specifying the type of each endpoint.
#'   Valid types are "continuous", "binary", "count". Length must match n_endpoints.
#' @param n_arms Number of arms in the study including control arm (must be positive integer)
#' @param n_repeated_measures Number of repeated measures per participant.
#'   Use NULL or 0 for single time point studies.
#' @param model_name Optional character string providing a descriptive name for the model
#' @param predefined_model Optional character string specifying a predefined model
#'   to use instead of creating a custom model. Currently supported values:
#'   \itemize{
#'     \item "ancova_cont_2arms" - ANCOVA model for continuous outcomes with baseline covariate and 2 arms
#'     \item "ancova_cont_3arms" - ANCOVA model for continuous outcomes with baseline covariate and 3 arms
#'   }
#'   When specified, other parameters are ignored and the predefined model is returned.
#'   Use [list_predefined_models()] to see all available models.
#'
#' @details
#' The build_model class encapsulates all components needed for power
#' analysis simulation:
#'
#'
#' \strong{Predefined Models:} For convenience, users can specify predefined_model
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
#' ancova_model <- build_model(predefined_model = "ancova_cont_2arms")
#' }
build_model <- function(predefined_model = NULL,
                        data_simulation_fn,
                        brms_model = NULL,
                        bayesflow_model = NULL,
                        backend_args = list(),
                        n_endpoints = NULL,
                        endpoint_types = NULL,
                        n_arms = NULL,
                        n_repeated_measures = NULL,
                        model_name = NULL) {
  # pre-defined model ----------------------------------------------------------

  # validate predefined_model
  if (!is.null(predefined_model)) {
    if (!is.character(predefined_model)) {
      cli::cli_abort(c(
        "{.arg predefined_model} must be a character string or NULL",
        "x" = "You supplied {.type {predefined_model}}"
      ))
    }
    # get model from internal environment
    model <- get_predefined_model(predefined_model)
    # return invisibly
    return(model)
  }
  # custom model ---------------------------------------------------------------

  # Early validation before S7 object creation
  if (!is.function(data_simulation_fn)) {
    cli::cli_abort(c(
      "{.arg data_simulation_fn} must be a valid function",
      "x" = "You supplied {.type {data_simulation_fn}}"
    ))
  }
  if (!is.null(model_name) && !is.character(model_name)) {
    cli::cli_abort(c(
      "{.arg model_name} must be a character string or NULL",
      "x" = "You supplied {.type {model_name}}"
    ))
  }

  # Validate that exactly one model is provided
  if (is.null(brms_model) && is.null(bayesflow_model)) {
    cli::cli_abort(c(
      "Either {.arg brms_model} or {.arg bayesflow_model} must be provided",
      "x" = "Both are NULL",
      "i" = "Provide one model specification"
    ))
  }
  if (!is.null(brms_model) && !is.null(bayesflow_model)) {
    cli::cli_abort(c(
      "Only one of {.arg brms_model} or {.arg bayesflow_model} can be provided",
      "x" = "Both were supplied",
      "i" = "Choose one model backend"
    ))
  }

  # Strip brms model from posterior draws (chains=0 for efficiency)
  # Rationale: brms_model serves as a compilation template only; posterior
  # draws are generated fresh in each simulation iteration
  if (!is.null(brms_model)) {
    brms_model <- suppressMessages(stats::update(brms_model, chains = 0, silent = 2))
  }

  # Set default model_name if NULL
  if (is.null(model_name)) {
    backend_type <- if (!is.null(brms_model)) "brms" else "NPE"
    model_name <- paste0("Custom Model (", backend_type, ")")
  }

  # Create S7 object - validation happens automatically
  model_obj <- rctbp_model(
    predefined_model = predefined_model,
    data_simulation_fn = data_simulation_fn,
    brms_model = brms_model,
    bayesflow_model = bayesflow_model,
    backend_args = backend_args,
    model_name = model_name,
    n_endpoints = n_endpoints,
    endpoint_types = endpoint_types,
    n_arms = n_arms,
    n_repeated_measures = n_repeated_measures
  )

  return(model_obj)
}

# =============================================================================
# S7 METHOD: print()
# =============================================================================
# Formats rctbp_model objects for console display showing model specifications
# and parameter information.

#' Print Method for rctbp_model Objects
#'
#' Displays a summary of a rctbp_model object, including model specifications,
#' parameter information, and function details.
#'
#' @param x An S7 object of class "rctbp_model"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @importFrom S7 method
#' @name print.rctbp_model
#' @export
S7::method(print, rctbp_model) <- function(x, ...) {
  report <- build_report.rctbp_model(x)
  render_report(report)
  invisible(x)
}

# =============================================================================
# UTILITY FUNCTIONS: Predefined Model Registry
# =============================================================================
# Functions to discover and access models stored in package internal environment
# (sysdata.rda). Predefined models provide ready-made configurations for
# common trial designs.

#' List available predefined models
#'
#' This function inspects the package's internal environment and returns the names
#' of all predefined model objects that inherit from the `rctbp_model` class.
#' These models are prebuilt and stored internally in the package via `sysdata.rda`
#' and are not exported to users directly. This function allows discovery of
#' available predefined models programmatically.
#'
#' @param filter_string Optional character string for filtering model names.
#'   If provided, only models whose names match this pattern (via [base::grepl()])
#'   will be returned. Use this to find specific types of models (e.g., "ancova").
#'
#' @details
#' The returned model names can be used directly with [build_model()] by passing
#' them to the `predefined_model` parameter:
#'
#' `model <- build_model(predefined_model = "model_name")`
#'
#' This provides a convenient way to discover and use prebuilt models without
#' needing to specify all model parameters manually.
#'
#' @return A character vector of object names corresponding to predefined models.
#'   Returns an empty character vector if no models are found or if the filter
#'   excludes all available models.
#'
#' @examples
#' # List all available predefined models
#' list_predefined_models()
#'
#' # Filter for ANCOVA models only
#' list_predefined_models(filter_string = "ancova")
#'
#' # Use discovered model with build_model()
#' available_models <- list_predefined_models()
#' if (length(available_models) > 0) {
#'   model <- build_model(predefined_model = available_models[1])
#' }
#'
#' @export
list_predefined_models <- function(filter_string = NULL) {
  ns <- asNamespace("rctbayespower")

  all_objs <- ls(envir = ns, all.names = TRUE)
  matches <- vapply(all_objs, function(obj_name) {
    obj <- get(obj_name, envir = ns)
    inherits(obj, "rctbayespower::rctbp_model")
  }, logical(1L))

  # filter matching strings
  if (!is.null(filter_string)) {
    matches <- matches & grepl(filter_string, all_objs)
  }

  names(matches[matches])
}


#' Get a predefined model by name
#'
#' Access an internal predefined model by name.
#'
#' @param model_name Character scalar, name of the predefined model.
#' @return The model object.
#' @export
get_predefined_model <- function(model_name) {
  ns <- asNamespace("rctbayespower")
  if (!model_name %in% ls(envir = ns)) {
    cli::cli_abort(c(
      "Model {.val {model_name}} not found",
      "i" = "Use {.fn list_predefined_models} to see available models"
    ), call = FALSE)
  }
  model <- get(model_name, envir = ns)
  if (!inherits(model, "rctbayespower::rctbp_model")) {
    cli::cli_abort(c(
      "Object {.val {model_name}} is not an rctbp_model",
      "x" = "Got {.cls {class(model)}}"
    ), call = FALSE)
  }
  model
}
