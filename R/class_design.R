# =============================================================================
# S7 CLASS DEFINITION: rctbp_design (MERGED)
# =============================================================================
# Encapsulates all components for power analysis: simulation function, inference
# model, trial metadata, and analysis configuration. This is the primary object
# users interact with.
#
# Previously model and design were separate classes. They have been merged to
# simplify the API. The design now stores all model properties directly.
#
# Single Backend Design:
#   - sim_fn: Data simulation function
#   - inference_model: brmsfit OR BayesFlow model (one at a time)
#   - backend: "brms" or "bf" (indicates which type is stored)

#' @importFrom S7 new_class class_character class_numeric class_integer class_function class_any new_union new_property
rctbp_design <- S7::new_class(
  "rctbp_design",
  properties = list(
    # =========================================================================
    # Core model properties (merged from rctbp_model)
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
      default = list(batch_size = NULL, n_posterior_samples = 1000L, envname = NULL)
    ),

    # Model metadata
    predefined_model = S7::new_union(S7::class_character, NULL),
    model_name = S7::class_character,
    n_endpoints = S7::class_numeric,
    endpoint_types = S7::class_character,
    n_arms = S7::class_numeric,
    n_repeated_measures = S7::class_numeric | NULL,

    # =========================================================================
    # Computed properties: parameter names (from model)
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
    # Stored explicitly because brms models are stripped of draws for efficiency
    par_names_inference = S7::new_property(
      class = S7::class_character,
      default = character(0)
    ),

    # =========================================================================
    # Design-specific properties
    # =========================================================================
    target_params = S7::class_character,    # Parameters to analyze (must match par_names_inference)
    design_name = S7::class_character | NULL
  ),

  # Validator ensures object consistency
  validator = function(self) {
    # =========================================================================
    # Model validation (from rctbp_model)
    # =========================================================================

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

    # =========================================================================
    # Design validation
    # =========================================================================

    # Check required parameters are provided
    if (is.null(self@target_params) || length(self@target_params) == 0) {
      return("'target_params' cannot be NULL or empty.")
    }

    # Validate target_params exist in the inference model
    if (!all(self@target_params %in% self@par_names_inference)) {
      return(paste(
        "'target_params' must be a subset of the parameter names in the model:",
        paste(self@par_names_inference, collapse = ", ")
      ))
    }

    NULL  # All validations passed
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
# CONSTRUCTOR FUNCTION: build_design()
# =============================================================================
# User-friendly interface to create merged rctbp_design objects.
# Accepts either:
#   1. A predefined model name (model_name = "ancova_cont_2arms")
#   2. Explicit model components (sim_fn, inference_model, etc.)

#' Build a Design for Power Analysis
#'
#' Creates an S7 rctbp_design object that encapsulates all components needed for
#' Bayesian power analysis: simulation function, inference model, trial metadata,
#' and analysis configuration.
#'
#' @param model_name Character. Name of a predefined model. Use [show_predefined_models()]
#'   to see available options. If provided, model components are loaded automatically.
#'   Takes precedence over explicit component arguments.
#' @param sim_fn A function that simulates trial data. Must accept parameters
#'   `n_total` and `p_alloc` at minimum. Required if `model_name` is NULL.
#' @param inference_model A brmsfit object (for brms backend) or BayesFlow model
#'   (for bf backend). Required if `model_name` is NULL.
#' @param backend Which backend to use: "brms" (default) or "bf".
#'   \itemize{
#'     \item "brms": Load brms model (default, always available)
#'     \item "bf": Try BayesFlow, fall back to brms with warning if unavailable
#'   }
#' @param backend_args_brms List of brms-specific arguments (chains, iter, etc.)
#' @param backend_args_bf List of BayesFlow-specific arguments:
#'   \describe{
#'     \item{batch_size}{Batch size for inference (default: NULL, uses n_sims)}
#'     \item{n_posterior_samples}{Number of posterior samples (default: 1000)}
#'     \item{envname}{Python virtual environment name to use (default: NULL for auto-detect)}
#'   }
#' @param n_endpoints Number of endpoints (positive integer). Required if `model_name` is NULL.
#' @param endpoint_types Character vector of endpoint types ("continuous", "binary", "count").
#'   Required if `model_name` is NULL.
#' @param n_arms Number of arms including control (positive integer). Required if `model_name` is NULL.
#' @param n_repeated_measures Number of repeated measures (NULL or 0 for single timepoint)
#' @param target_params Character vector specifying which model parameters to
#'   analyze for power. Must be valid parameter names from the inference model.
#'   Use `design@par_names_inference` to discover available names. Required.
#' @param design_name Optional character string providing a descriptive name.
#'
#' @details
#' This function creates a design object that contains the model specification
#' (simulation function, inference model) and target parameters. Decision
#' thresholds (`p_sig_scs`, `p_sig_ftl`, `thresh_scs`, `thresh_ftl`) and
#' sequential analysis settings (`analysis_at`) are specified in [build_conditions()].
#'
#' \strong{Predefined Models:} For most users, specify `model_name` to use a
#' predefined model configuration. Available models can be listed with
#' [show_predefined_models()].
#'
#' \strong{Custom Models:} For custom analyses, provide explicit `sim_fn` and
#' `inference_model` arguments along with trial metadata.
#'
#' @return An S7 object of class "rctbp_design"
#'
#' @export
#' @seealso [build_conditions()], [show_predefined_models()], [power_analysis()]
#'
#' @examples
#' \dontrun{
#' # Using a predefined model (recommended)
#' design <- build_design(
#'   model_name = "ancova_cont_2arms",
#'   target_params = "b_arm2"
#' )
#'
#' # Check available parameter names
#' design@par_names_inference
#' design@par_names_sim
#'
#' # Custom model (advanced)
#' design <- build_design(
#'   sim_fn = my_sim_function,
#'   inference_model = my_brmsfit,
#'   backend = "brms",
#'   n_endpoints = 1,
#'   endpoint_types = "continuous",
#'   n_arms = 2,
#'   target_params = "b_treatment"
#' )
#' }
build_design <- function(model_name = NULL,
                         sim_fn = NULL,
                         inference_model = NULL,
                         backend = c("brms", "bf"),
                         backend_args_brms = list(),
                         backend_args_bf = list(
                           batch_size = NULL,
                           n_posterior_samples = 1000L,
                           envname = NULL
                         ),
                         n_endpoints = NULL,
                         endpoint_types = NULL,
                         n_arms = NULL,
                         n_repeated_measures = NULL,
                         target_params,
                         design_name = NULL) {

  backend <- match.arg(backend)

  # =========================================================================
  # Load predefined model if specified
  # =========================================================================
  if (!is.null(model_name)) {
    if (!is.character(model_name) || length(model_name) != 1) {
      cli::cli_abort(c(
        "{.arg model_name} must be a single character string",
        "i" = "Use {.fn show_predefined_models} to see available models"
      ))
    }

    # Load model components from predefined model
    model_components <- load_predefined_model_components(model_name, backend)

    # Use predefined components (user-supplied args override)
    sim_fn <- model_components$sim_fn
    inference_model <- model_components$inference_model
    backend <- model_components$backend
    n_endpoints <- model_components$n_endpoints
    endpoint_types <- model_components$endpoint_types
    n_arms <- model_components$n_arms
    n_repeated_measures <- model_components$n_repeated_measures
  }

  # =========================================================================
  # Validate required arguments
  # =========================================================================
  if (is.null(sim_fn)) {
    cli::cli_abort(c(
      "{.arg sim_fn} must be provided",
      "i" = "Provide a simulation function or specify {.arg model_name}"
    ))
  }
  if (!is.function(sim_fn)) {
    cli::cli_abort(c(
      "{.arg sim_fn} must be a function",
      "x" = "You supplied {.type {sim_fn}}"
    ))
  }

  if (is.null(inference_model)) {
    cli::cli_abort(c(
      "{.arg inference_model} must be provided",
      "i" = "Provide a brmsfit/BayesFlow model or specify {.arg model_name}"
    ))
  }

  # Validate backend matches model type
  if (backend == "brms" && !inherits(inference_model, "brmsfit")) {
    cli::cli_abort(c(
      "Backend 'brms' selected but {.arg inference_model} is not a brmsfit",
      "x" = "You supplied {.type {inference_model}}"
    ))
  }

  # =========================================================================
  # Extract parameter names BEFORE stripping the model
  # =========================================================================
  # Cached brms models have draws from test run, so brms::variables() works
  if (backend == "brms" && inherits(inference_model, "brmsfit")) {
    par_names_inference <- grep("^b_", brms::variables(inference_model), value = TRUE)
  } else if (backend == "bf") {
    par_names_inference <- get_bf_parameter_names(inference_model)
  } else {
    par_names_inference <- character(0)
  }

  # Strip brms model posterior draws (template only)
  if (backend == "brms" && inherits(inference_model, "brmsfit")) {
    inference_model <- suppressMessages(
      stats::update(inference_model, chains = 0, silent = 2)
    )
  }

  # Set default design_name
  if (is.null(design_name)) {
    if (!is.null(model_name)) {
      design_name <- model_name
    } else {
      design_name <- paste0("Custom Design (", backend, ")")
    }
  }

  # =========================================================================
  # Create S7 object
  # =========================================================================
  rctbp_design(
    sim_fn = sim_fn,
    inference_model = inference_model,
    backend = backend,
    backend_args_brms = backend_args_brms,
    backend_args_bf = backend_args_bf,
    predefined_model = model_name,
    model_name = if (!is.null(model_name)) model_name else "Custom Model",
    n_endpoints = n_endpoints,
    endpoint_types = endpoint_types,
    n_arms = n_arms,
    n_repeated_measures = n_repeated_measures,
    par_names_inference = par_names_inference,
    target_params = target_params,
    design_name = design_name
  )
}

# =============================================================================
# HELPER: Load Predefined Model Components
# =============================================================================

#' Load Predefined Model Components
#'
#' Internal helper to load all components of a predefined model.
#'
#' @param model_name Name of the predefined model
#' @param backend Requested backend ("brms" or "bf")
#'
#' @return List with sim_fn, inference_model, backend, and metadata
#' @keywords internal
load_predefined_model_components <- function(model_name, backend) {
  # Registry of predefined models
  model_registry <- list(
    ancova_cont_2arms = list(
      description = "2-arm continuous ANCOVA",
      n_endpoints = 1L,
      endpoint_types = "continuous",
      n_arms = 2L,
      n_repeated_measures = 0L
    ),
    ancova_cont_3arms = list(
      description = "3-arm continuous ANCOVA",
      n_endpoints = 1L,
      endpoint_types = "continuous",
      n_arms = 3L,
      n_repeated_measures = 0L
    )
  )

  if (!model_name %in% names(model_registry)) {
    cli::cli_abort(c(
      "Unknown predefined model: {.val {model_name}}",
      "i" = "Available models: {.val {names(model_registry)}}"
    ), call = FALSE)
  }

  registry_entry <- model_registry[[model_name]]

  # Load brmsfit from cache or compile
  cache_dir <- get_model_cache_dir("brms")
  cache_file <- file.path(cache_dir, paste0(model_name, ".rds"))

  if (file.exists(cache_file)) {
    if (should_show(1)) {
      cli::cli_alert_info("Loading cached brms model: {.val {model_name}}")
    }
    brmsfit <- readRDS(cache_file)
  } else {
    if (should_show(1)) {
      cli::cli_alert_info("Compiling brms model: {.val {model_name}} (first use, will be cached)")
    }
    builder_fn <- get_model_builder(model_name)
    temp_model <- builder_fn()
    brmsfit <- temp_model@inference_model
    saveRDS(brmsfit, cache_file)
    if (should_show(1)) {
      cli::cli_alert_success("Cached for future use")
    }
  }

  # Create simulation function
  sim_fn <- create_sim_fn_for_model(model_name)

  # Handle backend preference
  actual_backend <- "brms"
  actual_inference_model <- brmsfit

  if (backend == "bf") {
    bf_success <- FALSE
    if (check_bf_available(silent = TRUE)) {
      tryCatch({
        bf_model <- load_bf_model(model_name)
        actual_backend <- "bf"
        actual_inference_model <- bf_model

        # Switch to Python sim_fn if available
        if (check_python_sims_available(silent = TRUE)) {
          sim_fn <- create_python_sim_fn(model_name)
        }

        bf_success <- TRUE
        cli::cli_alert_success("Using BayesFlow backend")
      }, error = function(e) {
        # Will fall back to brms
      })
    }

    if (!bf_success) {
      cli::cli_warn(c(
        "BayesFlow backend unavailable, using brms",
        "i" = "To use BayesFlow: install Python + bayesflow package"
      ))
    }
  }

  list(
    sim_fn = sim_fn,
    inference_model = actual_inference_model,
    backend = actual_backend,
    n_endpoints = registry_entry$n_endpoints,
    endpoint_types = registry_entry$endpoint_types,
    n_arms = registry_entry$n_arms,
    n_repeated_measures = registry_entry$n_repeated_measures
  )
}

#' Get Model Builder Function
#'
#' Internal helper to get the builder function for a predefined model.
#'
#' @param model_name Name of the predefined model
#' @return Builder function
#' @keywords internal
get_model_builder <- function(model_name) {
  builders <- list(
    ancova_cont_2arms = build_model_ancova_cont_2arms,
    ancova_cont_3arms = build_model_ancova_cont_3arms
  )
  builders[[model_name]]
}

#' Create Simulation Function for Model
#'
#' Internal helper to create the simulation function for a predefined model.
#'
#' @param model_name Name of the predefined model
#' @return Simulation function
#' @keywords internal
create_sim_fn_for_model <- function(model_name) {
  if (model_name == "ancova_cont_2arms") {
    create_ancova_sim_fn(n_arms = 2)
  } else if (model_name == "ancova_cont_3arms") {
    create_ancova_sim_fn(n_arms = 3)
  } else {
    cli::cli_abort("No simulation function for model: {.val {model_name}}")
  }
}

#' Create ANCOVA Simulation Function
#'
#' Internal helper to create simulation function for ANCOVA models.
#'
#' @param n_arms Number of arms
#' @return Simulation function
#' @keywords internal
create_ancova_sim_fn <- function(n_arms) {
  default_n_arms <- n_arms
  default_contrasts <- "contr.treatment"
  default_p_alloc <- rep(1, n_arms) / n_arms
  default_intercept <- 0
  default_b_arm_treat <- NULL
  default_b_covariate <- NULL
  default_sigma <- 1

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
}

# =============================================================================
# S7 METHOD: print()
# =============================================================================

#' Print Method for rctbp_design Objects
#'
#' Displays a comprehensive summary of a rctbp_design object, showing
#' model specifications and analysis configuration.
#'
#' @param x An S7 object of class "rctbp_design"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @importFrom S7 method
#' @name print.rctbp_design
#' @export
S7::method(print, rctbp_design) <- function(x, ...) {
  report <- build_report.rctbp_design(x)
  render_report(report)
  invisible(x)
}

# =============================================================================
# UTILITY FUNCTIONS: Predefined Model Registry
# =============================================================================

#' Show Predefined Models
#'
#' Returns the names of all predefined model configurations available for use
#' with [build_design()].
#'
#' @param filter_string Character scalar or NULL. If provided, only model names
#'   containing this string (case-sensitive) are returned.
#'
#' @return A character vector of model names
#'
#' @export
#'
#' @examples
#' # List all predefined models
#' show_predefined_models()
#'
#' # Filter to ANCOVA models
#' show_predefined_models("ancova")
show_predefined_models <- function(filter_string = NULL) {
  models <- c("ancova_cont_2arms", "ancova_cont_3arms")

  if (!is.null(filter_string)) {
    models <- models[grepl(filter_string, models)]
  }

  models
}

#' @rdname show_predefined_models
#' @export
list_predefined_models <- function(filter_string = NULL) {
  cli::cli_inform(c(
    "i" = "{.fn list_predefined_models} is deprecated, use {.fn show_predefined_models} instead"
  ))
  show_predefined_models(filter_string)
}
