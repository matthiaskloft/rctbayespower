#' Power Analysis for ANCOVA Model (Multiple Outcome Types)
#'
#' Convenience wrapper for power analysis using the ANCOVA model with baseline covariate.
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
#' @param threshold_success Upper threshold for success determination
#' @param threshold_futility Lower threshold for futility determination (required)
#' @param p_sig_success Probability threshold for declaring success (default 0.95)
#' @param p_sig_futility Probability threshold for declaring futility (default 0.5)
#' @param n_simulations Number of simulation iterations
#' @param priors_treatment Prior for treatment effect (default: student_t(3, 0, 2))
#' @param priors_baseline Prior for baseline effect (default: student_t(3, 0, 1))
#' @param priors_intercept Prior for intercept (default: student_t(3, 0, 2))
#' @param priors_sigma Prior for residual SD (default: student_t(3, 0, 2), continuous only)
#' @param brms_args Arguments passed to brms for model fitting. If empty, uses power_analysis() defaults: algorithm="sampling", iter=1200, warmup=200, chains=2, cores=1. User can override any of these or add additional arguments.
#' @param seed Random seed for reproducibility
#' @param n_cores Number of cores for parallel processing. If n_cores > 1, simulations will run in parallel. Default is number of available cores minus 1.
#' @param progress_updates Number of progress updates to show during parallel processing. Default is 10. Set to 0 to disable progress updates.
#' @param compile_models_only If TRUE, only compile the brms models and return them without running simulations. Used for model caching in power_grid_analysis(). Default is FALSE.
#'
#' @return A list containing power analysis results, or if compile_models_only=TRUE, a list with compiled models and arguments for later use
#' @export
#'
#' @examples
#' \dontrun{
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
#'   n_simulations = 100
#' )
#'
#' # Binary outcome ANCOVA
#' power_result <- power_analysis_ancova(
#'   n_control = 75,
#'   n_treatment = 75,
#'   outcome_type = "binary",
#'   effect_size = 0.693,  # log odds ratio for OR = 2
#'   baseline_effect = 0.405,  # log odds ratio for OR = 1.5
#'   intercept_value = 0,  # 50% baseline probability
#'   p_sig_success = 0.95,
#'   p_sig_futility = 0.6,
#'   n_simulations = 200
#' )
#'
#' # Count outcome ANCOVA
#' power_result <- power_analysis_ancova(
#'   n_control = 60,
#'   n_treatment = 60,
#'   outcome_type = "count",
#'   effect_size = 0.693,  # log rate ratio for RR = 2
#'   baseline_effect = 0.405,  # log rate ratio for RR = 1.5
#'   intercept_value = 1.099,  # log(3) for baseline rate = 3
#'   p_sig_success = 0.95,
#'   p_sig_futility = 0.7,
#'   n_simulations = 150
#' )
#'
#' # ANCOVA with custom brms_args and parallel processing with progress
#' power_result_custom <- power_analysis_ancova(
#'   n_control = 50,
#'   n_treatment = 50,
#'   outcome_type = "continuous",
#'   effect_size = 0.5,
#'   baseline_effect = 0.2,
#'   threshold_success = 0.2,
#'   threshold_futility = 0,
#'   p_sig_success = 0.95,
#'   p_sig_futility = 0.5,
#'   n_simulations = 100,
#'   brms_args = list(algorithm = "meanfield", iter = 800),
#'   n_cores = 3,  # Use 3 cores for parallel processing
#'   progress_updates = 5  # Show 5 progress updates
#' )
#' }
power_analysis_ancova <- function(n_control,
                                  n_treatment,
                                  outcome_type = "continuous",
                                  effect_size,
                                  baseline_effect,
                                  intercept_value = 0,
                                  sigma_value = 1,
                                  threshold_success,
                                  threshold_futility,
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
                                  n_cores = parallel::detectCores() - 1,
                                  progress_updates = 10,
                                  compile_models_only = FALSE) {
  # Input validation
  
  # Check required parameters
  if (missing(n_control) || is.null(n_control)) {
    stop("n_control is required and must be specified.")
  }
  if (missing(n_treatment) || is.null(n_treatment)) {
    stop("n_treatment is required and must be specified.")
  }
  if (missing(effect_size) || is.null(effect_size)) {
    stop("effect_size is required and must be specified.")
  }
  if (missing(baseline_effect) || is.null(baseline_effect)) {
    stop("baseline_effect is required and must be specified.")
  }
  if (missing(threshold_success) || is.null(threshold_success)) {
    stop("threshold_success is required and must be specified.")
  }
  if (missing(threshold_futility) || is.null(threshold_futility)) {
    stop("threshold_futility is required and must be specified.")
  }
  
  # Validate parameter types and ranges
  if (!is.numeric(n_control) ||
      length(n_control) != 1 || n_control <= 0) {
    stop("n_control must be a single positive number.")
  }
  if (!is.numeric(n_treatment) ||
      length(n_treatment) != 1 || n_treatment <= 0) {
    stop("n_treatment must be a single positive number.")
  }
  if (!is.numeric(effect_size) || length(effect_size) != 1) {
    stop("effect_size must be a single numeric value.")
  }
  if (!is.numeric(baseline_effect) ||
      length(baseline_effect) != 1) {
    stop("baseline_effect must be a single numeric value.")
  }
  
  # Validate outcome type
  if (!outcome_type %in% c("continuous", "binary", "count")) {
    stop("outcome_type must be one of: 'continuous', 'binary', 'count'")
  }
  
  # Validate optional numeric parameters
  if (!is.numeric(intercept_value) ||
      length(intercept_value) != 1) {
    stop("intercept_value must be a single numeric value.")
  }
  if (!is.numeric(sigma_value) ||
      length(sigma_value) != 1 || sigma_value <= 0) {
    stop("sigma_value must be a single positive number.")
  }
  if (!is.numeric(threshold_success) ||
      length(threshold_success) != 1) {
    stop("threshold_success must be a single numeric value.")
  }
  if (!is.numeric(threshold_futility) ||
      length(threshold_futility) != 1) {
    stop("threshold_futility must be a single numeric value.")
  }
  if (!is.numeric(p_sig_success) ||
      length(p_sig_success) != 1 ||
      p_sig_success <= 0 || p_sig_success >= 1) {
    stop("p_sig_success must be a single number between 0 and 1.")
  }
  if (!is.numeric(p_sig_futility) ||
      length(p_sig_futility) != 1 ||
      p_sig_futility <= 0 || p_sig_futility >= 1) {
    stop("p_sig_futility must be a single number between 0 and 1.")
  }
  if (!is.numeric(n_simulations) ||
      length(n_simulations) != 1 || n_simulations < 1) {
    stop("n_simulations must be a single positive integer.")
  }
  if (!is.numeric(n_cores) || length(n_cores) != 1 || n_cores < 1) {
    stop("n_cores must be a single positive integer.")
  }
  
  # Validate prior specifications
  if (!is.character(priors_treatment) ||
      length(priors_treatment) != 1) {
    stop("priors_treatment must be a single character string.")
  }
  if (!is.character(priors_baseline) ||
      length(priors_baseline) != 1) {
    stop("priors_baseline must be a single character string.")
  }
  if (!is.character(priors_intercept) ||
      length(priors_intercept) != 1) {
    stop("priors_intercept must be a single character string.")
  }
  if (!is.character(priors_sigma) || length(priors_sigma) != 1) {
    stop("priors_sigma must be a single character string.")
  }
  
  # Validate brms_args
  if (!is.list(brms_args)) {
    stop("brms_args must be a list.")
  }
  
  # Validate seed if provided
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1)) {
    stop("seed must be a single numeric value or NULL.")
  }
  
  # Validate logical consistency
  if (threshold_success <= threshold_futility) {
    stop("threshold_success must be greater than threshold_futility.")
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
  family <- switch(
    outcome_type,
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
    priors_true_params <- c(priors_true_params,
                            brms::set_prior(paste0("constant(", sigma_value, ")"), class = "sigma"))
  }
  
  # Set up estimation priors (exactly as in template)
  priors_estimation <- c(
    brms::set_prior(priors_treatment, class = "b"),
    brms::set_prior(priors_baseline, class = "b", coef = "baseline"),
    brms::set_prior(priors_intercept, class = "Intercept")
  )
  
  # Add sigma prior only for continuous outcomes
  if (outcome_type == "continuous") {
    priors_estimation <- c(priors_estimation,
                           brms::set_prior(priors_sigma, class = "sigma"))
  }
  
  # Define target parameter (exactly as in template)
  target_param <- "grouptreat"
  
  # If compile_models_only is TRUE, use validate_power_design to compile models and return them
  if (compile_models_only) {
    cat("Compiling ANCOVA models without running simulations...\n")
    
    # Use validate_power_design to compile the models
    validation_result <- validate_power_design(
      n_control = n_control,
      n_treatment = n_treatment,
      simulate_data_fn = simulate_data,
      model_formula_true_params = model_formula_true_params,
      model_formula_estimation = model_formula_estimation,
      family = family,
      priors_true_params = priors_true_params,
      priors_estimation = priors_estimation,
      target_param = target_param,
      brms_args = brms_args
    )
    
    # Return compiled models and arguments for later use
    return(list(
      brms_design_true_params = validation_result$brms_design_true_params,
      brms_design_estimation = validation_result$brms_design_estimation,
      # Store the arguments that will be needed for power_analysis calls
      power_analysis_args = list(
        simulate_data_fn = simulate_data,
        model_formula_true_params = model_formula_true_params,
        model_formula_estimation = model_formula_estimation,
        family = family,
        priors_true_params = priors_true_params,
        priors_estimation = priors_estimation,
        target_param = target_param,
        brms_args = brms_args
      ),
      # Store the ANCOVA-specific parameters
      ancova_params = list(
        outcome_type = outcome_type,
        effect_size = effect_size,
        baseline_effect = baseline_effect,
        intercept_value = intercept_value,
        sigma_value = sigma_value,
        p_sig_success = p_sig_success,
        p_sig_futility = p_sig_futility,
        priors_treatment = priors_treatment,
        priors_baseline = priors_baseline,
        priors_intercept = priors_intercept,
        priors_sigma = priors_sigma,
        seed = seed,
        n_cores = n_cores,
        progress_updates = progress_updates
      )
    ))
  }
  
  # Call the main power analysis function
  power_analysis(
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
    progress_updates = progress_updates
  )
}