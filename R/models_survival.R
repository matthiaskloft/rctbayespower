# =============================================================================
# SURVIVAL MODEL: EXPONENTIAL
# =============================================================================
# Model builder, 2-arm wrapper, and sim_fn factory for exponential survival
# (time-to-event) analysis. Follows the ANCOVA registry pattern.
#
# Compatible with brms: brm(time | cens(censored) ~ arm, family = weibull())

# =============================================================================
# GENERIC BUILDER
# =============================================================================

#' Create Exponential Survival Model
#'
#' Creates a model object for exponential survival (time-to-event) power
#' analysis. Compiles a brms weibull model and returns an `rctbp_model` object
#' for use with the predefined model registry.
#'
#' @param prior_intercept Prior for the intercept (log baseline scale).
#'   If NULL, uses `normal(0, 5)`.
#' @param prior_treatment Prior for the treatment effect (log hazard ratio).
#'   If NULL, uses `normal(0, 2)`.
#' @param n_arms Number of arms in the trial (currently must be 2).
#' @param p_alloc Allocation probability vector (length 2, sums to 1).
#'
#' @details
#' The model uses a Weibull family in brms. With shape = 1, the Weibull
#' reduces to the exponential distribution. brms estimates the shape parameter
#' from data; it is filtered out of `par_names_inference` by [build_design()].
#'
#' \strong{brms formula:} `time | cens(censored) ~ arm`
#'
#' This function returns an `rctbp_model` object (not `rctbp_design`). The
#' predefined model registry extracts `@inference_model` from it and pairs it
#' with a sim_fn from [create_survival_exp_sim_fn()].
#'
#' @return An S7 object of class `rctbp_model`
#'
#' @export
#' @seealso [build_model_survival_exp_2arms()], [build_design()],
#'   [show_predefined_models()]
#'
#' @examples
#' \dontrun{
#' model <- build_model_survival_exp(n_arms = 2, p_alloc = c(0.5, 0.5))
#' }
build_model_survival_exp <- function(prior_intercept = NULL,
                                     prior_treatment = NULL,
                                     n_arms = 2,
                                     p_alloc = c(0.5, 0.5)) {
  # Validate n_arms — only 2-arm designs supported currently

  if (n_arms != 2) {
    cli::cli_abort(
      "{.arg n_arms} must be 2. Multi-arm survival models are not yet supported."
    )
  }
  if (length(p_alloc) != 2) {
    cli::cli_abort(
      "{.arg p_alloc} must have length 2 (one per arm), not {length(p_alloc)}."
    )
  }
  if (abs(sum(p_alloc) - 1) > 1e-8) {
    cli::cli_abort("{.arg p_alloc} must sum to 1.")
  }

  # Default priors
  if (is.null(prior_intercept)) {
    prior_intercept <- brms::set_prior("normal(0, 5)", class = "Intercept")
  }
  if (is.null(prior_treatment)) {
    prior_treatment <- brms::set_prior("normal(0, 2)", class = "b")
  }

  priors <- prior_intercept + prior_treatment

  # Minimal sample data for compilation
  sample_data <- data.frame(
    time = c(1, 2, 3, 4),
    censored = c(0, 0, 1, 0),
    arm = factor(c("ctrl", "ctrl", "treat_1", "treat_1"),
                 levels = c("ctrl", "treat_1"))
  )

  # Compile brms model (suppress compilation messages)
  # Use chains = 1, iter = 500 (NOT chains = 0) — matches ANCOVA builders.
  # chains = 0 may cause brms::variables() to return empty, breaking
  # par_names_inference extraction in build_design().
  compiled <- suppressMessages(suppressWarnings(
    brms::brm(
      formula = brms::bf(time | cens(censored) ~ arm),
      data = sample_data,
      family = brms::weibull(),
      prior = priors,
      chains = 1,
      iter = 500,
      refresh = 0,
      silent = 2
    )
  ))

  # Placeholder sim_fn — must pass rctbp_model validator
  # (requires n_total and p_alloc in formals)
  # The registry ignores this and uses create_sim_fn_for_model() instead.
  placeholder_sim_fn <- function(n_total, p_alloc = NULL, ...) {
    cli::cli_abort("This is a placeholder sim_fn. Use {.fn build_design} with {.arg predefined_model} instead.")
  }

  rctbp_model(
    sim_fn = placeholder_sim_fn,
    inference_model = compiled,
    model_name = "Exponential Survival",
    n_endpoints = 1L,
    endpoint_types = "survival",
    n_arms = as.integer(n_arms),
    n_repeated_measures = 0L
  )
}


# =============================================================================
# 2-ARM WRAPPER
# =============================================================================

#' Create 2-Arm Exponential Survival Model
#'
#' Convenience wrapper around [build_model_survival_exp()] that sets 2-arm
#' defaults and marks the model as predefined.
#'
#' @param ... Additional arguments passed to [build_model_survival_exp()].
#'   Can override `prior_intercept`, `prior_treatment`, `n_arms`, `p_alloc`.
#'
#' @return An S7 object of class `rctbp_model` with `@predefined_model` set
#'   to `"survival_exp_2arms"`.
#'
#' @export
#' @seealso [build_model_survival_exp()], [build_design()],
#'   [show_predefined_models()]
#'
#' @examples
#' \dontrun{
#' model <- build_model_survival_exp_2arms()
#' }
build_model_survival_exp_2arms <- function(...) {
  dots <- list(...)
  default_args <- list(
    prior_intercept = NULL,
    prior_treatment = NULL,
    n_arms = 2,
    p_alloc = c(0.5, 0.5)
  )
  final_args <- modifyList(default_args, dots)
  model <- do.call(build_model_survival_exp, final_args)
  model@predefined_model <- "survival_exp_2arms"
  invisible(model)
}


# =============================================================================
# SIM_FN FACTORY
# =============================================================================

#' Create Exponential Survival Simulation Function
#'
#' Internal factory that creates an `rctbp_sim_fn` for exponential survival
#' data. Effect parameters (`baseline_hazard`, `hazard_ratio`) have NULL
#' defaults so `get_args_without_defaults()` treats them as required.
#'
#' @param n_arms Number of arms
#' @return rctbp_sim_fn object (callable with schema)
#' @keywords internal
create_survival_exp_sim_fn <- function(n_arms) {
  if (n_arms != 2) {
    cli::cli_abort(
      "{.arg n_arms} must be 2. Multi-arm survival sim_fn is not yet supported."
    )
  }

  default_p_alloc <- rep(1, n_arms) / n_arms

  fn <- function(n_total,
                 p_alloc = default_p_alloc,
                 baseline_hazard = NULL,
                 hazard_ratio = NULL,
                 accrual_rate = NULL,
                 followup_time = NULL) {
    # Validate inputs
    if (!is.numeric(n_total) || length(n_total) != 1 || n_total < 1) {
      cli::cli_abort("{.arg n_total} must be a positive integer")
    }
    if (!is.numeric(p_alloc) || length(p_alloc) != 2 || abs(sum(p_alloc) - 1) > 1e-8) {
      cli::cli_abort("{.arg p_alloc} must be a numeric vector of length 2 that sums to 1")
    }
    if (is.null(baseline_hazard) || baseline_hazard <= 0) {
      cli::cli_abort("{.arg baseline_hazard} must be a positive number")
    }
    if (is.null(hazard_ratio) || hazard_ratio <= 0) {
      cli::cli_abort("{.arg hazard_ratio} must be a positive number")
    }
    if (is.null(accrual_rate) || accrual_rate <= 0) {
      cli::cli_abort("{.arg accrual_rate} must be a positive number")
    }
    if (is.null(followup_time) || followup_time < 0) {
      cli::cli_abort("{.arg followup_time} must be non-negative")
    }

    # Arm assignment
    arm_idx <- sample(0:1, n_total, replace = TRUE, prob = p_alloc)
    arm <- factor(arm_idx, levels = 0:1, labels = c("ctrl", "treat_1"))

    # Enrollment times (staggered entry, uniform accrual)
    enrollment_time <- generate_enrollment_times(
      n_total, accrual_rate, accrual_pattern = "uniform"
    )

    # Exponential event times
    hazard <- ifelse(arm_idx == 0, baseline_hazard,
                      baseline_hazard * hazard_ratio)
    event_time <- stats::rexp(n_total, rate = hazard)

    # Administrative censoring from staggered enrollment
    max_calendar_time <- max(enrollment_time) + followup_time
    admin_censor_time <- max_calendar_time - enrollment_time

    # Observed time and censoring indicator
    time <- pmin(event_time, admin_censor_time)
    censored <- as.integer(event_time > admin_censor_time)

    data.frame(
      time = time,
      censored = censored,
      arm = arm,
      enrollment_time = enrollment_time
    )
  }

  build_sim_fn(
    fn = fn,
    test_args = list(
      n_total = 20L,
      p_alloc = rep(1, n_arms) / n_arms,
      baseline_hazard = 0.1,
      hazard_ratio = 0.7,
      accrual_rate = 10,
      followup_time = 12
    )
  )
}
