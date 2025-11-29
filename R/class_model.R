# =============================================================================
# S7 CLASS DEFINITION: rctbp_model (DEPRECATED)
# =============================================================================
# This class is deprecated. Use rctbp_design instead.
#
# Kept for backward compatibility with existing code that uses:
# - get_model() / build_model()
# - models_ancova.R builder functions
#
# The rctbp_design class now contains all model properties directly.

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
      default = list(batch_size = NULL, n_posterior_samples = 1000L, envname = NULL)
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
    par_names_inference = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        if (is.null(self@inference_model)) {
          return(character(0))
        }

        if (self@backend == "brms" && inherits(self@inference_model, "brmsfit")) {
          grep("^b_", brms::variables(self@inference_model), value = TRUE)
        } else if (self@backend == "bf") {
          get_bf_parameter_names_legacy(self@inference_model)
        } else {
          character(0)
        }
      }
    )
  ),

  # Validator ensures object consistency
  validator = function(self) {
    if (is.null(self@inference_model)) {
      return("'inference_model' must be provided.")
    }

    if (self@backend == "brms" && !inherits(self@inference_model, "brmsfit")) {
      return("Backend is 'brms' but 'inference_model' is not a brmsfit object.")
    }

    if (!is.list(self@backend_args_brms)) {
      return("'backend_args_brms' must be a list.")
    }
    if (!is.list(self@backend_args_bf)) {
      return("'backend_args_bf' must be a list.")
    }

    if (!self@backend %in% c("brms", "bf")) {
      return("'backend' must be 'brms' or 'bf'.")
    }

    if (is.null(self@sim_fn)) {
      return("'sim_fn' must be provided.")
    }

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
# HELPER: Legacy parameter name extraction
# =============================================================================

#' Extract Parameter Names from BayesFlow Model (Legacy)
#'
#' @param model A keras/BayesFlow model object
#' @return Character vector of parameter names
#' @keywords internal
get_bf_parameter_names_legacy <- function(model) {
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
# BACKWARD COMPATIBILITY: get_predefined_model()
# =============================================================================
# Used by deprecated get_model() and for building legacy rctbp_model objects

#' Get a Predefined Model (Legacy)
#'
#' Internal function to build legacy rctbp_model objects for backward compatibility.
#'
#' @param model_name Name of the predefined model
#' @param backend Backend to use
#'
#' @return Legacy rctbp_model object
#' @keywords internal
get_predefined_model <- function(model_name, backend = "brms") {
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
    if (should_show(1)) {
      cli::cli_alert_info("Loading cached brms model: {.val {model_name}}")
    }
    brmsfit <- readRDS(cache_file)
    model <- build_predefined_from_brmsfit_legacy(model_name, brmsfit)
  } else {
    if (should_show(1)) {
      cli::cli_alert_info("Compiling brms model: {.val {model_name}} (first use, will be cached)")
    }
    builder_fn <- model_registry[[model_name]]$builder
    model <- builder_fn()
    if (should_show(1)) {
      cli::cli_alert_success("Model compilation done")
    }

    saveRDS(model@inference_model, cache_file)
    if (should_show(1)) {
      cli::cli_alert_success("Cached for future use")
    }
  }

  # Handle BayesFlow backend request
  if (backend == "bf") {
    bf_success <- FALSE

    if (check_bf_available(silent = TRUE)) {
      tryCatch({
        bf_model <- load_bf_model(model_name)
        model@backend <- "bf"
        model@inference_model <- bf_model

        if (check_python_sims_available(silent = TRUE)) {
          model@sim_fn <- create_python_sim_fn(model_name)
        }

        bf_success <- TRUE
        cli::cli_alert_success("Using BayesFlow backend")
      }, error = function(e) {
        model@backend <- "brms"
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

#' Build Legacy Model from Cached brmsfit
#'
#' @param model_name Name of the predefined model
#' @param brmsfit Cached brmsfit object
#' @return Legacy rctbp_model object
#' @keywords internal
build_predefined_from_brmsfit_legacy <- function(model_name, brmsfit) {
  if (model_name == "ancova_cont_2arms") {
    n_arms <- 2
    sim_fn <- create_legacy_ancova_sim_fn(n_arms)

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
    n_arms <- 3
    sim_fn <- create_legacy_ancova_sim_fn(n_arms)

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

#' Create Legacy ANCOVA Simulation Function
#'
#' @param n_arms Number of arms
#' @return Simulation function
#' @keywords internal
create_legacy_ancova_sim_fn <- function(n_arms) {
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
    if (is.character(contrasts)) {
      contrasts_fn <- get(contrasts)
      contrast_matrix <- contrasts_fn(n_arms)
    } else {
      contrast_matrix <- contrasts
    }

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
# S7 METHOD: print() for legacy model
# =============================================================================

#' Print Method for rctbp_model Objects
#'
#' @param x An S7 object of class "rctbp_model"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object
#' @importFrom S7 method
#' @name print.rctbp_model
#' @export
S7::method(print, rctbp_model) <- function(x, ...) {
  report <- build_report.rctbp_model(x)
  render_report(report)
  invisible(x)
}
