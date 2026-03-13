# =============================================================================
# SURVIVAL MODEL: EXPONENTIAL (Registry Pattern)
# =============================================================================
# Builder, 2-arm wrapper, and sim_fn factory for exponential survival models.
# Follows the ANCOVA registry pattern: builder returns rctbp_model, registry
# extracts @inference_model and pairs with create_survival_exp_sim_fn().
#
# Compatible with brms: brm(time | cens(censored) ~ arm, family = weibull())

# =============================================================================
# GENERIC BUILDER: build_model_survival_exp()
# =============================================================================

#' Create Exponential Survival Model
#'
#' Creates a model object for exponential survival (time-to-event) power analysis.
#' Returns an `rctbp_model` object whose `@inference_model` is extracted by the
#' predefined model registry in [build_design()].
#'
#' @param prior_intercept Prior for the intercept (log baseline scale).
#'   If NULL, uses `normal(0, 5)`. Must be a brmsprior object.
#' @param prior_treatment Prior for the treatment coefficient.
#'   If NULL, uses `normal(0, 2)`. Must be a brmsprior object.
#' @param n_arms Number of arms (default 2).
#' @param p_alloc Allocation probability vector (default equal allocation).
#'
#' @return An S7 object of class `rctbp_model`
#'
#' @export
#' @seealso [build_design()], [show_predefined_models()]
#'
#' @examples
#' \dontrun{
#' model <- build_model_survival_exp()
#' }
build_model_survival_exp <- function(prior_intercept = NULL,
                                     prior_treatment = NULL,
                                     n_arms = 2,
                                     p_alloc = c(0.5, 0.5)) {
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
    cli::cli_abort("This is a placeholder sim_fn. Use build_design(model_name = ...) instead.")
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
# 2-ARM WRAPPER: build_model_survival_exp_2arms()
# =============================================================================

#' Create 2-Arm Exponential Survival Model
#'
#' Convenience wrapper around [build_model_survival_exp()] with 2-arm defaults.
#' Sets `@predefined_model` to `"survival_exp_2arms"` for registry lookup.
#'
#' @param ... Additional arguments passed to [build_model_survival_exp()].
#'
#' @return An S7 object of class `rctbp_model` with `@predefined_model` set
#'
#' @export
#' @seealso [build_model_survival_exp()], [build_design()]
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
# SIM_FN FACTORY: create_survival_exp_sim_fn()
# =============================================================================

#' Create Simulation Function for Exponential Survival Model
#'
#' Internal factory that creates an `rctbp_sim_fn` object for the exponential
#' survival model. Effect parameters (`baseline_hazard`, `hazard_ratio`) have
#' NULL defaults so `get_args_without_defaults()` treats them as required.
#'
#' @param n_arms Number of arms
#' @return rctbp_sim_fn object (callable with schema)
#' @keywords internal
create_survival_exp_sim_fn <- function(n_arms) {
  default_p_alloc <- rep(1, n_arms) / n_arms

  fn <- function(n_total,
                 p_alloc = default_p_alloc,
                 baseline_hazard = NULL,
                 hazard_ratio = NULL,
                 accrual_rate = NULL,
                 followup_time = NULL) {
    # Validate inputs
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
