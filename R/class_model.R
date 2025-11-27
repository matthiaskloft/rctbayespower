# =============================================================================
# S7 CLASS DEFINITION: rctbp_model
# =============================================================================
# Encapsulates all components needed for power analysis simulation including
# data simulation function, posterior estimation models (brms AND/OR BayesFlow),
# and trial metadata (endpoints, arms, repeated measures).
#
# Dual Backend Support:
#   - Models can have brms_model, bayesflow_model, or BOTH
#   - backend property controls which is used: "brms", "bf", or "auto"
#   - "auto" prefers BayesFlow if available (faster), else brms
#   - active_backend resolves "auto" to actual backend

#' @importFrom S7 new_class class_character class_numeric class_integer class_function class_any new_union new_property
rctbp_model <- S7::new_class(
  "rctbp_model",
  properties = list(
    # Core components
    data_simulation_fn = S7::class_function,  # Generates trial data
    brms_model = S7::class_any | NULL,        # Template brmsfit object (backend = "brms")
    bayesflow_model = S7::class_any | NULL,   # Neural posterior model (backend = "bf")

    # Backend selection: "brms", "bf", or "auto"
    # "auto" = use whichever is available (prefer bf if both)
    backend = S7::new_property(
      class = S7::class_character,
      default = "auto",
      setter = function(self, value) {
        if (!value %in% c("brms", "bf", "auto")) {
          cli::cli_abort(c(
            "{.arg backend} must be 'brms', 'bf', or 'auto'",
            "x" = "You supplied {.val {value}}"
          ))
        }
        self@backend <- value
        self
      }
    ),

    # Backend-specific arguments
    backend_args = S7::new_property(S7::class_list, default = list()),
    backend_args_brms = S7::new_property(S7::class_list, default = list()),
    backend_args_bf = S7::new_property(
      S7::class_list,
      default = list(batch_size = 64L, n_posterior_samples = 1000L)
    ),

    # Model metadata
    predefined_model = S7::new_union(S7::class_character, NULL),  # Name if using predefined
    model_name = S7::class_character,
    n_endpoints = S7::class_numeric,
    endpoint_types = S7::class_character,     # "continuous", "binary", or "count"
    n_arms = S7::class_numeric,
    n_repeated_measures = S7::class_numeric | NULL,

    # Computed property: resolved backend (handles "auto" fallback)
    active_backend = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        if (self@backend != "auto") {
          # Explicit backend requested - validate availability
          if (self@backend == "brms" && is.null(self@brms_model)) {
            cli::cli_abort("Backend 'brms' requested but no brms model available")
          }
          if (self@backend == "bf" && is.null(self@bayesflow_model)) {
            cli::cli_abort("Backend 'bf' requested but no BayesFlow model available")
          }
          return(self@backend)
        }

        # Auto fallback: prefer BayesFlow if available (faster), else brms
        if (!is.null(self@bayesflow_model)) return("bf")
        if (!is.null(self@brms_model)) return("brms")
        cli::cli_abort("No backend available - provide brms_model or bayesflow_model")
      }
    ),

    # Computed property: simulation parameter names
    parameter_names_sim_fn = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        # Extract parameter names from data simulation function signature
        names(formals(self@data_simulation_fn))
      }
    ),

    # Computed property: brms parameter names
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
    # Validate at least one backend model is provided
    has_brms <- !is.null(self@brms_model)
    has_bayesflow <- !is.null(self@bayesflow_model)

    if (!has_brms && !has_bayesflow) {
      return("At least one of 'brms_model' or 'bayesflow_model' must be provided.")
    }

    # Validate model type if provided
    if (has_brms && !inherits(self@brms_model, "brmsfit")) {
      return("'brms_model' must be a valid brmsfit object.")
    }

    # Validate backend_args are lists
    if (!is.list(self@backend_args)) {
      return("'backend_args' must be a list.")
    }
    if (!is.list(self@backend_args_brms)) {
      return("'backend_args_brms' must be a list.")
    }
    if (!is.list(self@backend_args_bf)) {
      return("'backend_args_bf' must be a list.")
    }

    # Validate backend value
    if (!self@backend %in% c("brms", "bf", "auto")) {
      return("'backend' must be 'brms', 'bf', or 'auto'.")
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
#' function, posterior estimation model(s) (brms and/or BayesFlow), and associated
#' metadata for power analysis. This object serves as the foundation for Bayesian
#' power analysis in RCTs with support for multiple backends.
#'
#' @param data_simulation_fn A function that simulates data for the RCT. Must take
#'   parameters n_total, p_alloc, and further parameters needed for
#'   data simulation.
#' @param brms_model A fitted brmsfit object that serves as the template model.
#'   This should be compiled without posterior draws (chains = 0) for efficiency.
#'   Can provide brms_model, bayesflow_model, or both.
#' @param bayesflow_model A trained BayesFlow neural posterior estimation model
#'   (keras model via reticulate). Can provide brms_model, bayesflow_model, or both.
#' @param backend Which backend to use: "brms", "bf", or "auto" (default).
#'   "auto" prefers BayesFlow if available (faster), else uses brms.
#' @param backend_args Named list of backend-specific arguments (legacy, use
#'   backend_args_brms or backend_args_bf instead).
#' @param backend_args_brms List of brms-specific arguments: chains, iter, cores, etc.
#' @param backend_args_bf List of BayesFlow-specific arguments: batch_size,
#'   n_posterior_samples, etc. Default: list(batch_size = 64L, n_posterior_samples = 1000L)
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
#' \strong{Dual Backend Support:} Models can have both brms and BayesFlow backends
#' simultaneously. The `backend` parameter controls which is used at runtime:
#' \itemize{
#'   \item "brms" - Force brms backend
#'   \item "bf" - Force BayesFlow backend
#'   \item "auto" - Use BayesFlow if available (faster), else brms
#' }
#'
#' You can switch backends at any time: `model@backend <- "brms"`
#'
#' \strong{Predefined Models:} For convenience, users can specify predefined_model
#' to use ready-made model configurations. This is the recommended approach for
#' standard analyses. When using predefined models, other parameters are ignored.
#'
#' \strong{Custom Models:} For advanced users, custom models can be created by
#' providing all required parameters.
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
#' \strong{BayesFlow Model:} A trained BayesFlow approximator (.keras file) loaded
#' via Python/reticulate. See [load_bf_model()] for loading pre-trained models.
#'
#' @return An S7 object of class "rctbp_model" containing the specified properties
#'
#' @export
#' @importFrom stringr str_subset
#' @seealso [build_design()], [build_model_ancova()], [add_bf_backend()],
#'   [load_bf_model()], [load_brms_model()]
#'
#' @examples
#' \dontrun{
#' # Method 1: Use predefined model (recommended)
#' ancova_model <- build_model(predefined_model = "ancova_cont_2arms")
#'
#' # Method 2: Add BayesFlow backend later
#' model <- build_model(predefined_model = "ancova_cont_2arms")
#' bf_model <- load_bf_model("ancova_cont_2arms")
#' model <- add_bf_backend(model, bf_model)
#'
#' # Method 3: Force specific backend
#' model@backend <- "brms"  # Use brms even if BayesFlow available
#' }
build_model <- function(predefined_model = NULL,
                        data_simulation_fn,
                        brms_model = NULL,
                        bayesflow_model = NULL,
                        backend = "auto",
                        backend_args = list(),
                        backend_args_brms = list(),
                        backend_args_bf = list(
                          batch_size = 64L,
                          n_posterior_samples = 1000L
                        ),
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

  # Validate backend parameter
  if (!backend %in% c("brms", "bf", "auto")) {
    cli::cli_abort(c(
      "{.arg backend} must be 'brms', 'bf', or 'auto'",
      "x" = "You supplied {.val {backend}}"
    ))
  }

  # Validate at least one model is provided
  if (is.null(brms_model) && is.null(bayesflow_model)) {
    cli::cli_abort(c(
      "At least one of {.arg brms_model} or {.arg bayesflow_model} must be provided",
      "x" = "Both are NULL",
      "i" = "Provide at least one model specification"
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
    backends_present <- c()
    if (!is.null(brms_model)) backends_present <- c(backends_present, "brms")
    if (!is.null(bayesflow_model)) backends_present <- c(backends_present, "bf")
    backend_str <- paste(backends_present, collapse = "+")
    model_name <- paste0("Custom Model (", backend_str, ")")
  }

  # Create S7 object - validation happens automatically
  model_obj <- rctbp_model(
    predefined_model = predefined_model,
    data_simulation_fn = data_simulation_fn,
    brms_model = brms_model,
    bayesflow_model = bayesflow_model,
    backend = backend,
    backend_args = backend_args,
    backend_args_brms = backend_args_brms,
    backend_args_bf = backend_args_bf,
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


# =============================================================================
# UTILITY FUNCTIONS: Backend Management
# =============================================================================

#' Add BayesFlow Backend to Existing Model
#'
#' Adds a BayesFlow model to an existing rctbp_model that only has brms.
#' This enables dual-backend support, allowing comparison between backends
#' or switching to the faster BayesFlow backend.
#'
#' @param model Existing rctbp_model object
#' @param bayesflow_model BayesFlow/Keras model (Python object via reticulate).
#'   Load using [load_bf_model()] or Python's keras.models.load_model().
#' @param backend_args_bf BayesFlow configuration (batch_size, n_posterior_samples)
#' @param set_active Whether to switch to BayesFlow backend (default TRUE)
#'
#' @return The modified model with BayesFlow backend added
#'
#' @export
#' @seealso [build_model()], [load_bf_model()]
#'
#' @examples
#' \dontrun{
#' # Start with brms-only model
#' model <- build_model(predefined_model = "ancova_cont_2arms")
#' model@active_backend  # "brms"
#'
#' # Add BayesFlow backend
#' bf_model <- load_bf_model("ancova_cont_2arms")
#' model <- add_bf_backend(model, bf_model)
#' model@active_backend  # "bf" (auto prefers BayesFlow)
#'
#' # Force brms backend even with BayesFlow available
#' model@backend <- "brms"
#' model@active_backend  # "brms"
#' }
add_bf_backend <- function(model, bayesflow_model,
                            backend_args_bf = list(
                              batch_size = 64L,
                              n_posterior_samples = 1000L
                            ),
                            set_active = TRUE) {

  # Validate input
  if (!inherits(model, "rctbayespower::rctbp_model") &&
      !inherits(model, "rctbp_model")) {
    cli::cli_abort(c(
      "{.arg model} must be an rctbp_model object",
      "x" = "Got {.cls {class(model)}}"
    ))
  }

  if (is.null(bayesflow_model)) {
    cli::cli_abort("{.arg bayesflow_model} cannot be NULL")
  }

  # Add BayesFlow model and args
  model@bayesflow_model <- bayesflow_model
  model@backend_args_bf <- backend_args_bf

  # Optionally switch to BayesFlow backend
  if (set_active) {
    model@backend <- "bf"
  }

  model
}


#' Add brms Backend to Existing Model
#'
#' Adds a brms model to an existing rctbp_model that only has BayesFlow.
#' This enables dual-backend support.
#'
#' @param model Existing rctbp_model object
#' @param brms_model Compiled brmsfit template model
#' @param backend_args_brms brms configuration (chains, iter, etc.)
#' @param set_active Whether to switch to brms backend (default FALSE)
#'
#' @return The modified model with brms backend added
#'
#' @export
#' @seealso [build_model()], [load_brms_model()]
add_brms_backend <- function(model, brms_model,
                              backend_args_brms = list(),
                              set_active = FALSE) {

  # Validate input
  if (!inherits(model, "rctbayespower::rctbp_model") &&
      !inherits(model, "rctbp_model")) {
    cli::cli_abort(c(
      "{.arg model} must be an rctbp_model object",
      "x" = "Got {.cls {class(model)}}"
    ))
  }

  if (is.null(brms_model)) {
    cli::cli_abort("{.arg brms_model} cannot be NULL")
  }

  if (!inherits(brms_model, "brmsfit")) {
    cli::cli_abort(c(
      "{.arg brms_model} must be a brmsfit object",
      "x" = "Got {.cls {class(brms_model)}}"
    ))
  }

  # Strip posterior draws for efficiency
  brms_model <- suppressMessages(stats::update(brms_model, chains = 0, silent = 2))

  # Add brms model and args
  model@brms_model <- brms_model
  model@backend_args_brms <- backend_args_brms

  # Optionally switch to brms backend
  if (set_active) {
    model@backend <- "brms"
  }

  model
}
