# =============================================================================
# S7 CLASS DEFINITION: rctbp_model
# =============================================================================
# Encapsulates all components needed for power analysis simulation including
# data simulation function, posterior estimation model, and trial metadata.
#
# Single Backend Design:
#   - sim_fn: Data simulation function
#   - inference_model: brmsfit OR BayesFlow model (one at a time)
#   - backend: "brms" or "bf" (indicates which type is stored)
#
# Users can manually switch backends by loading a different inference_model.

#' @importFrom S7 new_class class_character class_numeric class_integer class_function class_any new_union new_property
rctbp_model <- S7::new_class(
  "rctbp_model",
  properties = list(
    # =========================================================================
    # Core properties
    # =========================================================================
    sim_fn = S7::new_property(class = S7::class_any, default = NULL),
    inference_model = S7::new_property(class = S7::class_any, default = NULL),

    # Backend indicator: "brms" or "bf"
    backend = S7::new_property(
      class = S7::class_character,
      default = "brms",
      setter = function(self, value) {
        if (!value %in% c("brms", "bf")) {
          cli::cli_abort(c(
            "{.arg backend} must be 'brms' or 'bf'",
            "x" = "You supplied {.val {value}}"
          ))
        }
        self@backend <- value
        self
      }
    ),

    # Backend-specific arguments
    backend_args_brms = S7::new_property(S7::class_list, default = list()),
    backend_args_bf = S7::new_property(
      S7::class_list,
      default = list(batch_size = NULL, n_posterior_samples = 1000L)
    ),

    # =========================================================================
    # Model metadata
    # =========================================================================
    predefined_model = S7::new_union(S7::class_character, NULL),
    model_name = S7::class_character,
    n_endpoints = S7::class_numeric,
    endpoint_types = S7::class_character,
    n_arms = S7::class_numeric,
    n_repeated_measures = S7::class_numeric | NULL,

    # =========================================================================
    # Computed properties: parameter names
    # =========================================================================

    # Parameter names from the simulation function (computed)
    par_names_sim = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        if (!is.null(self@sim_fn)) {
          names(formals(self@sim_fn))
        } else {
          character(0)
        }
      }
    ),

    # Parameter names available for inference (fixed effects)
    # Works for both brms and BayesFlow backends
    par_names_inference = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        if (is.null(self@inference_model)) {
          return(character(0))
        }

        if (self@backend == "brms" && inherits(self@inference_model, "brmsfit")) {
          # Extract from brmsfit (fixed effects with "b_" prefix)
          grep("^b_", brms::variables(self@inference_model), value = TRUE)
        } else if (self@backend == "bf") {
          # Extract from BayesFlow/keras model
          get_bf_parameter_names(self@inference_model)
        } else {
          character(0)
        }
      }
    )
  ),

  # Validator ensures object consistency
  validator = function(self) {
    # Validate inference_model is provided
    if (is.null(self@inference_model)) {
      return("'inference_model' must be provided.")
    }

    # Validate backend matches model type
    if (self@backend == "brms" && !inherits(self@inference_model, "brmsfit")) {
      return("Backend is 'brms' but 'inference_model' is not a brmsfit object.")
    }

    # Validate backend_args are lists
    if (!is.list(self@backend_args_brms)) {
      return("'backend_args_brms' must be a list.")
    }
    if (!is.list(self@backend_args_bf)) {
      return("'backend_args_bf' must be a list.")
    }

    # Validate backend value
    if (!self@backend %in% c("brms", "bf")) {
      return("'backend' must be 'brms' or 'bf'.")
    }

    # Validate simulation function is provided
    if (is.null(self@sim_fn)) {
      return("'sim_fn' must be provided.")
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

    # Validate sim_fn has required parameters
    if (!is.null(self@sim_fn)) {
      required_params <- c("n_total", "p_alloc")
      if (!all(required_params %in% names(formals(self@sim_fn)))) {
        return("'sim_fn' must have parameters 'n_total' and 'p_alloc'.")
      }
    }

    NULL
  }
)

# =============================================================================
# HELPER FUNCTIONS: Parameter Name Extraction
# =============================================================================

#' Extract Parameter Names from BayesFlow/Keras Model
#'
#' Retrieves parameter names from a BayesFlow model by extracting them directly
#' from the adapter transforms. Finds the Rename transform that maps TO
#' "inference_variables" (the BayesFlow convention for target parameters).
#'
#' @param model A keras/BayesFlow model object
#'
#' @return Character vector of parameter names
#' @keywords internal
get_bf_parameter_names <- function(model) {
  # Extract from adapter transforms directly
  # Find Rename transform that maps TO "inference_variables"
  tryCatch({
    transforms <- model$adapter$transforms

    for (t in transforms) {
      to_key <- tryCatch(t$to_key, error = function(e) NULL)
      if (identical(to_key, "inference_variables")) {
        return(as.character(t$from_key))
      }
    }
    character(0)
  }, error = function(e) character(0))
}

# =============================================================================
# CONSTRUCTOR FUNCTIONS: get_model() and build_model()
# =============================================================================

#' Get a Predefined Model
#'
#' Retrieves a predefined model by name. This is the recommended way to get
#' models for standard analyses.
#'
#' @param name Character. Name of the predefined model. Use [list_predefined_models()]
#'   to see available options. Currently supported:
#'   \itemize{
#'     \item "ancova_cont_2arms" - ANCOVA for continuous outcomes, 2 arms
#'     \item "ancova_cont_3arms" - ANCOVA for continuous outcomes, 3 arms
#'   }
#' @param backend Character. Backend to use: "brms" (default) or "bf".
#'   \itemize{
#'     \item "brms": Load brms model (default, always available)
#'     \item "bf": Try BayesFlow, fall back to brms with warning if unavailable
#'   }
#'
#' @return An S7 object of class "rctbp_model"
#'
#' @details
#' By default, models use the brms backend which is always available.
#' To use BayesFlow, specify `backend = "bf"`. If BayesFlow is unavailable,
#' the function falls back to brms with a warning.
#'
#' To use a different backend, call `get_model()` again with the desired backend.
#'
#' @export
#' @seealso [build_model()] for custom models, [list_predefined_models()],
#'   [check_bf_available()]
#'
#' @examples
#' # Get model with default brms backend
#' model <- get_model("ancova_cont_2arms")
#'
#' # Check which backend is active
#' model@backend
#'
#' \dontrun{
#' # Request BayesFlow backend (falls back to brms if unavailable)
#' model <- get_model("ancova_cont_2arms", backend = "bf")
#' }
get_model <- function(name, backend = c("brms", "bf")) {
  # Validate name
  if (missing(name) || !is.character(name) || length(name) != 1) {
    cli::cli_abort(c(
      "{.arg name} must be a single character string",
      "i" = "Use {.fn list_predefined_models} to see available models"
    ))
  }

  backend <- match.arg(backend)

  # Get base model from registry (always builds with brms)
  model <- get_predefined_model(name)

  if (backend == "bf") {
    # Try to load BayesFlow, fall back to brms with warning
    bf_success <- FALSE

    if (check_bf_available(silent = TRUE)) {
      tryCatch({
        bf_model <- load_bf_model(name)
        # Set backend BEFORE inference_model (validator checks backend matches model type)
        model@backend <- "bf"
        model@inference_model <- bf_model

        # Switch to Python sim_fn if available
        if (check_python_sims_available(silent = TRUE)) {
          model@sim_fn <- create_python_sim_fn(name)
        }

        bf_success <- TRUE
        cli::cli_alert_success("Using BayesFlow backend")
      }, error = function(e) {
        # Restore brms backend on error
        model@backend <- "brms"
        # Will fall back to brms below
      })
    }

    if (!bf_success) {
      cli::cli_warn(c(
        "BayesFlow backend unavailable, using brms",
        "i" = "To use BayesFlow: install Python + bayesflow package"
      ))
    }
  }

  model
}


#' Build a Custom Model
#'
#' Creates a custom rctbp_model from user-supplied components. For most users,
#' [get_model()] with predefined models is recommended instead.
#'
#' @param sim_fn A function that simulates trial data. Must accept parameters
#'   `n_total` and `p_alloc` at minimum.
#' @param inference_model A brmsfit object (for brms backend) or BayesFlow model
#'   (for bf backend).
#' @param backend Which backend to use: "brms" (default) or "bf".
#' @param backend_args_brms List of brms-specific arguments (chains, iter, etc.)
#' @param backend_args_bf List of BayesFlow-specific arguments.
#'   Default: list(batch_size = 64L, n_posterior_samples = 1000L)
#' @param n_endpoints Number of endpoints (positive integer)
#' @param endpoint_types Character vector of endpoint types ("continuous", "binary", "count")
#' @param n_arms Number of arms including control (positive integer)
#' @param n_repeated_measures Number of repeated measures (NULL or 0 for single timepoint)
#' @param model_name Descriptive name for the model
#'
#' @return An S7 object of class "rctbp_model"
#'
#' @export
#' @seealso [get_model()] for predefined models
#'
#' @examples
#' \dontrun{
#' # Create custom model with brms backend
#' model <- build_model(
#'   sim_fn = my_sim_function,
#'   inference_model = my_compiled_brms,
#'   backend = "brms",
#'   n_endpoints = 1,
#'   endpoint_types = "continuous",
#'   n_arms = 2
#' )
#' }
build_model <- function(sim_fn = NULL,
                        inference_model = NULL,
                        backend = c("brms", "bf"),
                        backend_args_brms = list(),
                        backend_args_bf = list(
                          batch_size = NULL,
                          n_posterior_samples = 1000L
                        ),
                        n_endpoints = NULL,
                        endpoint_types = NULL,
                        n_arms = NULL,
                        n_repeated_measures = NULL,
                        model_name = NULL) {

  backend <- match.arg(backend)

  # Validate sim_fn
  if (is.null(sim_fn)) {
    cli::cli_abort(c(
      "{.arg sim_fn} must be provided",
      "i" = "Provide a function that simulates trial data"
    ))
  }
  if (!is.function(sim_fn)) {
    cli::cli_abort(c(
      "{.arg sim_fn} must be a function",
      "x" = "You supplied {.type {sim_fn}}"
    ))
  }

  # Validate inference_model
  if (is.null(inference_model)) {
    cli::cli_abort(c(
      "{.arg inference_model} must be provided",
      "i" = "Provide a brmsfit object (backend='brms') or BayesFlow model (backend='bf')"
    ))
  }

  # Validate backend matches model type
  if (backend == "brms" && !inherits(inference_model, "brmsfit")) {
    cli::cli_abort(c(
      "Backend 'brms' selected but {.arg inference_model} is not a brmsfit",
      "x" = "You supplied {.type {inference_model}}"
    ))
  }

  if (!is.null(model_name) && !is.character(model_name)) {
    cli::cli_abort(c(
      "{.arg model_name} must be a character string or NULL",
      "x" = "You supplied {.type {model_name}}"
    ))
  }

  # Strip brms model posterior draws (template only)
  if (backend == "brms" && inherits(inference_model, "brmsfit")) {
    inference_model <- suppressMessages(
      stats::update(inference_model, chains = 0, silent = 2)
    )
  }

  # Set default model_name
  if (is.null(model_name)) {
    model_name <- paste0("Custom Model (", backend, ")")
  }

  # Create S7 object
  model_obj <- rctbp_model(
    predefined_model = NULL,
    sim_fn = sim_fn,
    inference_model = inference_model,
    backend = backend,
    backend_args_brms = backend_args_brms,
    backend_args_bf = backend_args_bf,
    model_name = model_name,
    n_endpoints = n_endpoints,
    endpoint_types = endpoint_types,
    n_arms = n_arms,
    n_repeated_measures = n_repeated_measures
  )

  model_obj
}


# =============================================================================
# S7 METHOD: print()
# =============================================================================

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

#' List available predefined models
#'
#' This function inspects the package's internal environment and returns the names
#' of all predefined model objects that inherit from the `rctbp_model` class.
#'
#' @param filter_string Character scalar or NULL. If provided, only model names
#'   containing this string (case-sensitive) are returned. If NULL (default),
#'   all model names are returned.
#'
#' @return A character vector of model names available for use with
#'   [get_model()]. Returns empty vector if no models match.
#'
#' @export
#'
#' @examples
#' # List all predefined models
#' available_models <- list_predefined_models()
#' print(available_models)
#'
#' # Filter to only ANCOVA models
#' ancova_models <- list_predefined_models("ancova")
#' print(ancova_models)
#'
#' \dontrun{
#' # Use a predefined model
#'   model <- get_model(available_models[1])
#' }
#'
#' @export
list_predefined_models <- function(filter_string = NULL) {
  # Available predefined models
  models <- c("ancova_cont_2arms", "ancova_cont_3arms")

  # filter matching strings
  if (!is.null(filter_string)) {
    models <- models[grepl(filter_string, models)]
  }

  models
}


#' Get a predefined model by name
#'
#' Builds a predefined model, caching the compiled brmsfit for efficiency.
#' On first use, compiles the brms model and caches it locally.
#' On subsequent uses, loads the cached brmsfit and creates a fresh rctbp_model.
#'
#' @param model_name Character scalar, name of the predefined model.
#' @return The model object (rctbp_model).
#' @export
get_predefined_model <- function(model_name) {
  # Registry of predefined models and their builder functions
  model_registry <- list(
    ancova_cont_2arms = list(
      builder = build_model_ancova_cont_2arms,
      description = "2-arm continuous ANCOVA"
    ),
    ancova_cont_3arms = list(
      builder = build_model_ancova_cont_3arms,
      description = "3-arm continuous ANCOVA"
    )
  )

  if (!model_name %in% names(model_registry)) {
    cli::cli_abort(c(
      "Unknown predefined model: {.val {model_name}}",
      "i" = "Available models: {.val {names(model_registry)}}"
    ), call = FALSE)
  }

  # Check for cached brmsfit
  cache_dir <- get_model_cache_dir("brms")
  cache_file <- file.path(cache_dir, paste0(model_name, ".rds"))

  if (file.exists(cache_file)) {
    # Load cached brmsfit and build fresh rctbp_model
    if (should_show(1)) {
      cli::cli_alert_info("Loading cached brms model: {.val {model_name}}")
    }
    brmsfit <- readRDS(cache_file)

    # Build fresh model with cached brmsfit
    model <- build_predefined_from_brmsfit(model_name, brmsfit)
  } else {
    # First use: compile brms model and cache
    if (should_show(1)) {
      cli::cli_alert_info("Compiling brms model: {.val {model_name}} (first use, will be cached)")
    }
    builder_fn <- model_registry[[model_name]]$builder
    model <- builder_fn()
    if (should_show(1)) {
      cli::cli_alert_success("Model compilation done")
    }

    # Cache the brmsfit (not the S7 wrapper)
    saveRDS(model@inference_model, cache_file)
    if (should_show(1)) {
      cli::cli_alert_success("Cached for future use")
    }
  }

  model
}

#' Build predefined model from cached brmsfit
#'
#' Internal helper to create a fresh rctbp_model from a cached brmsfit.
#'
#' @param model_name Name of the predefined model
#' @param brmsfit Cached brmsfit object
#'
#' @return Fresh rctbp_model object
#' @keywords internal
build_predefined_from_brmsfit <- function(model_name, brmsfit) {
  # Model-specific simulation functions
  if (model_name == "ancova_cont_2arms") {
    # Get default parameters from 2-arm model
    n_arms <- 2
    contrasts <- "contr.treatment"
    p_alloc <- c(0.5, 0.5)
    intercept <- 0
    b_arm_treat <- NULL
    b_covariate <- NULL
    sigma <- 1

    # Create simulation function
    sim_fn <- local({
      default_n_arms <- n_arms
      default_contrasts <- contrasts
      default_p_alloc <- p_alloc
      default_intercept <- intercept
      default_b_arm_treat <- b_arm_treat
      default_b_covariate <- b_covariate
      default_sigma <- sigma

      function(n_total, n_arms = default_n_arms, contrasts = default_contrasts,
               p_alloc = default_p_alloc, intercept = default_intercept,
               b_arm_treat = default_b_arm_treat, b_covariate = default_b_covariate,
               sigma = default_sigma) {
        # Create contrast matrix
        if (is.character(contrasts)) {
          contrasts_fn <- get(contrasts)
          contrast_matrix <- contrasts_fn(n_arms)
        } else {
          contrast_matrix <- contrasts
        }

        # Simulate data
        df <- data.frame(
          covariate = stats::rnorm(n_total),
          arm = factor(
            sample(seq_len(n_arms) - 1, n_total, prob = p_alloc, replace = TRUE),
            levels = seq_len(n_arms) - 1,
            labels = c("ctrl", paste0("treat_", seq_len(n_arms - 1)))
          )
        )
        stats::contrasts(df$arm) <- contrast_matrix
        df$outcome <- stats::rnorm(
          n_total,
          mean = intercept +
            stats::model.matrix(~ arm, data = df)[, -1, drop = FALSE] %*% b_arm_treat +
            df$covariate * b_covariate,
          sd = sigma
        )
        df
      }
    })

    model <- rctbp_model(
      sim_fn = sim_fn,
      inference_model = brmsfit,
      model_name = "ANCOVA",
      n_endpoints = 1L,
      endpoint_types = "continuous",
      n_arms = 2L,
      n_repeated_measures = 0L
    )
    model@predefined_model <- "ancova_cont_2arms"

  } else if (model_name == "ancova_cont_3arms") {
    # Get default parameters from 3-arm model
    n_arms <- 3
    contrasts <- "contr.treatment"
    p_alloc <- rep(1, 3) / 3
    intercept <- 0
    b_arm_treat <- NULL
    b_covariate <- NULL
    sigma <- 1

    # Create simulation function
    sim_fn <- local({
      default_n_arms <- n_arms
      default_contrasts <- contrasts
      default_p_alloc <- p_alloc
      default_intercept <- intercept
      default_b_arm_treat <- b_arm_treat
      default_b_covariate <- b_covariate
      default_sigma <- sigma

      function(n_total, n_arms = default_n_arms, contrasts = default_contrasts,
               p_alloc = default_p_alloc, intercept = default_intercept,
               b_arm_treat = default_b_arm_treat, b_covariate = default_b_covariate,
               sigma = default_sigma) {
        # Create contrast matrix
        if (is.character(contrasts)) {
          contrasts_fn <- get(contrasts)
          contrast_matrix <- contrasts_fn(n_arms)
        } else {
          contrast_matrix <- contrasts
        }

        # Simulate data
        df <- data.frame(
          covariate = stats::rnorm(n_total),
          arm = factor(
            sample(seq_len(n_arms) - 1, n_total, prob = p_alloc, replace = TRUE),
            levels = seq_len(n_arms) - 1,
            labels = c("ctrl", paste0("treat_", seq_len(n_arms - 1)))
          )
        )
        stats::contrasts(df$arm) <- contrast_matrix
        df$outcome <- stats::rnorm(
          n_total,
          mean = intercept +
            stats::model.matrix(~ arm, data = df)[, -1, drop = FALSE] %*% b_arm_treat +
            df$covariate * b_covariate,
          sd = sigma
        )
        df
      }
    })

    model <- rctbp_model(
      sim_fn = sim_fn,
      inference_model = brmsfit,
      model_name = "ANCOVA",
      n_endpoints = 1L,
      endpoint_types = "continuous",
      n_arms = 3L,
      n_repeated_measures = 0L
    )
    model@predefined_model <- "ancova_cont_3arms"

  } else {
    cli::cli_abort("Unknown model: {.val {model_name}}")
  }

  model
}


