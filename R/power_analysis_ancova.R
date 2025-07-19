#' Power Analysis for ANCOVA Model (Multiple Outcome Types)
#'
#' Convenience wrapper for [power_analysis()] using the ANCOVA model with baseline covariate.
#' This function automates the setup for a standard randomized controlled trial with
#' a baseline covariate following the exact template workflow. Supports continuous,
#' binary, and count outcomes.
#'
#' @param n_control Number of participants in control group
#' @param n_treatment Number of participants in treatment group
#' @param outcome_type Type of outcome ("continuous", "binary", "count")
#' @param effect_size True treatment effect (raw scale: continuous = raw difference, binary = log odds ratio, count = log rate ratio)
#' @param baseline_effect True baseline covariate effect (raw scale)
#' @param intercept_value True intercept value (raw scale)
#' @param sigma_value True residual standard deviation (for continuous outcomes only)
#' @param threshold_success Upper threshold for success determination (not needed if compile_models_only=TRUE)
#' @param threshold_futility Lower threshold for futility determination (not needed if compile_models_only=TRUE)
#' @param p_sig_success Probability threshold for declaring success (default 0.975)
#' @param p_sig_futility Probability threshold for declaring futility (default 0.5)
#' @param n_simulations Number of simulation iterations
#' @param priors_treatment Prior for treatment effect (default: student_t(3, 0, 2))
#' @param priors_baseline Prior for baseline effect (default: student_t(3, 0, 1))
#' @param priors_intercept Prior for intercept (default: student_t(3, 0, 2))
#' @param priors_sigma Prior for residual SD (default: student_t(3, 0, 2), continuous only)
#' @param brms_args Arguments passed to brms for model fitting. If empty, uses power_analysis() defaults: algorithm="sampling", iter=1200, warmup=200, chains=2, cores=1. User can override any of these or add additional arguments.
#' @param seed Random seed for reproducibility
#' @param n_cores Number of cores for parallel processing. If n_cores > 1, simulations will run in parallel. Default is 1.
#' @param progress_updates Number of progress updates to show during parallel processing. Default is 10. Set to 0 to disable progress updates.
#' @param compile_models_only If TRUE, only compile the brms models and return them without running simulations. Used for model caching in power_grid_analysis(). Default is FALSE.
#' @param ... Additional arguments passed to power_analysis()
#'
#' @return A list containing power analysis results, or if compile_models_only=TRUE, a list with compiled models and arguments for later use. See [power_analysis()] for details.
#' Power metrics include Monte Carlo Standard Errors (MCSE) for all power and probability estimates.
#' @export
#' @importFrom stats rnorm gaussian poisson
#' @importFrom brms bernoulli
#'
#' @seealso [power_analysis()]
#' @examples
#' \donttest{
#' # Continuous outcome ANCOVA
#' power_result <- power_analysis_ancova(
#'   n_control = 50,
#'   n_treatment = 50,
#'   outcome_type = "continuous",
#'   effect_size = 0.5,
#'   baseline_effect = 0.2,
#'   threshold_success = 0.2,
#'   threshold_futility = 0,
#'   p_sig_success = 0.95,
#'   p_sig_futility = 0.8,
#'   n_simulations = 1,
#'   brms_args = list(algorithm = "meanfield"),
#'   n_cores = 1,
#'   progress_updates = 5
#' )
#' }
power_analysis_ancova <- function(n_control,
                                  n_treatment,
                                  outcome_type = "continuous",
                                  effect_size,
                                  baseline_effect,
                                  intercept_value = 0,
                                  sigma_value = 1,
                                  threshold_success = NULL,
                                  threshold_futility = NULL,
                                  p_sig_success = 0.975,
                                  p_sig_futility = 0.5,
                                  n_simulations = 1000,
                                  priors_treatment = "student_t(3, 0, 2)",
                                  priors_baseline = "student_t(3, 0, 1)",
                                  priors_intercept = "student_t(3, 0, 2)",
                                  priors_sigma = "student_t(3, 0, 2)",
                                  brms_args = list(
                                    algorithm = "sampling",
                                    iter = 1500,
                                    warmup = 500,
                                    chains = 2,
                                    cores = 1,
                                    init = .1,
                                    control = list(adapt_delta = 0.9)
                                  ),
                                  seed = NULL,
                                  n_cores = 1,
                                  progress_updates = 10,
                                  compile_models_only = FALSE,
                                  ...) {
  # Validate ANCOVA-specific required parameters
  if (missing(effect_size) || is.null(effect_size)) {
    stop("'effect_size' is required and must be specified.")
  }
  if (missing(baseline_effect) || is.null(baseline_effect)) {
    stop("'baseline_effect' is required and must be specified.")
  }
  # Validate ANCOVA-specific parameters only
  # (Other parameters will be validated by power_analysis())
  if (!is.numeric(effect_size) || length(effect_size) != 1) {
    stop("'effect_size' must be a single numeric value.")
  }
  if (!is.numeric(baseline_effect) ||
    length(baseline_effect) != 1) {
    stop("'baseline_effect' must be a single numeric value.")
  }

  # Validate outcome type
  if (!outcome_type %in% c("continuous", "binary", "count")) {
    stop("'outcome_type' must be one of: 'continuous', 'binary', 'count'")
  }

  # Validate ANCOVA-specific optional numeric parameters
  if (!is.numeric(intercept_value) ||
    length(intercept_value) != 1) {
    stop("'intercept_value' must be a single numeric value.")
  }
  if (!is.numeric(sigma_value) ||
    length(sigma_value) != 1 || sigma_value <= 0) {
    stop("'sigma_value' must be a single positive number.")
  }
  # Validate ANCOVA-specific prior specifications
  if (!is.character(priors_treatment) ||
    length(priors_treatment) != 1) {
    stop("'priors_treatment' must be a single character string.")
  }
  if (!is.character(priors_baseline) ||
    length(priors_baseline) != 1) {
    stop("'priors_baseline' must be a single character string.")
  }
  if (!is.character(priors_intercept) ||
    length(priors_intercept) != 1) {
    stop("'priors_intercept' must be a single character string.")
  }
  if (!is.character(priors_sigma) || length(priors_sigma) != 1) {
    stop("'priors_sigma' must be a single character string.")
  }

  # Create data simulation function following template exactly
  simulate_data <- function(n_control, n_treatment) {
    data.frame(
      outcome = rnorm(n_control + n_treatment),
      # placeholder, will be overwritten
      baseline = rnorm(n_control + n_treatment),
      group = factor(
        rep(c(0, 1), times = c(n_control, n_treatment)),
        levels = c(0, 1),
        labels = c("ctrl", "treat")
      )
    )
  }

  # Define model formulas (exactly as in template)
  model_formula_true_params <- brms::bf(outcome ~ baseline + group, center = FALSE)
  model_formula_estimation <- brms::bf(outcome ~ baseline + group)

  # Define distributional family
  family <- switch(outcome_type,
    "continuous" = gaussian(),
    "binary" = bernoulli(),
    "count" = poisson()
  )

  # Set up priors with true parameter values (constants as in template)
  priors_true_params <- c(
    brms::set_prior(
      paste0("constant(", baseline_effect, ")"),
      class = "b",
      coef = "baseline"
    ),
    brms::set_prior(
      paste0("constant(", effect_size, ")"),
      class = "b",
      coef = "grouptreat"
    ),
    brms::set_prior(
      paste0("constant(", intercept_value, ")"),
      class = "b",
      coef = "Intercept"
    )
  )

  # Add sigma prior only for continuous outcomes
  if (outcome_type == "continuous") {
    priors_true_params <- c(
      priors_true_params,
      brms::set_prior(paste0("constant(", sigma_value, ")"), class = "sigma")
    )
  }

  # Set up estimation priors (exactly as in template)
  priors_estimation <- c(
    brms::set_prior(priors_treatment, class = "b"),
    brms::set_prior(priors_baseline, class = "b", coef = "baseline"),
    brms::set_prior(priors_intercept, class = "Intercept")
  )

  # Add sigma prior only for continuous outcomes
  if (outcome_type == "continuous") {
    priors_estimation <- c(
      priors_estimation,
      brms::set_prior(priors_sigma, class = "sigma")
    )
  }

  # Define target parameter (exactly as in template)
  target_param <- "grouptreat"

  # Call the main power analysis function
  results <- power_analysis(
    n_control = n_control,
    n_treatment = n_treatment,
    simulate_data_fn = simulate_data,
    model_formula_true_params = model_formula_true_params,
    model_formula_estimation = model_formula_estimation,
    family = family,
    priors_true_params = priors_true_params,
    priors_estimation = priors_estimation,
    target_param = target_param,
    threshold_success = threshold_success,
    threshold_futility = threshold_futility,
    p_sig_success = p_sig_success,
    p_sig_futility = p_sig_futility,
    n_simulations = n_simulations,
    brms_args = brms_args,
    seed = seed,
    n_cores = n_cores,
    progress_updates = progress_updates,
    compile_models_only = compile_models_only,
    ...
  )

  return(results)
}
