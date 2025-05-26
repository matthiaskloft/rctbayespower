#' Bayesian Power Analysis for RCTs
#'
#' Conduct Bayesian power analysis for randomized controlled trials using brms.
#' This function estimates the probability of detecting a treatment effect of a 
#' given size using Bayesian methods.
#'
#' @param n_control Number of participants in control group
#' @param n_treatment Number of participants in treatment group  
#' @param effect_size Expected effect size
#' @param outcome_type Type of outcome ("continuous", "binary", "count")
#' @param prior_specification Prior specification for the treatment effect
#' @param rope_limits ROPE (Region of Practical Equivalence) limits
#' @param prob_threshold Probability threshold for declaring an effect
#' @param n_simulations Number of simulation iterations
#' @param baseline_mean Baseline mean for continuous outcomes
#' @param baseline_sd Baseline standard deviation for continuous outcomes
#' @param baseline_prob Baseline probability for binary outcomes
#' @param baseline_rate Baseline rate for count outcomes
#' @param covariates List of covariate specifications
#' @param model_formula Custom model formula (optional)
#' @param brms_args Additional arguments passed to brms
#' @param seed Random seed for reproducibility
#' @param parallel Whether to run simulations in parallel
#' @param cores Number of cores for parallel processing
#'
#' @return A list containing power analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic power analysis for continuous outcome
#' power_result <- power_analysis(
#'   n_control = 50,
#'   n_treatment = 50,
#'   effect_size = 0.5,
#'   outcome_type = "continuous",
#'   n_simulations = 100
#' )
#' 
#' # Power analysis with covariates
#' covariates <- list(
#'   age = list(type = "continuous", mean = 45, sd = 10),
#'   sex = list(type = "binary", prob = 0.5)
#' )
#' 
#' power_result <- power_analysis(
#'   n_control = 75,
#'   n_treatment = 75,
#'   effect_size = 0.3,
#'   outcome_type = "continuous",
#'   covariates = covariates,
#'   n_simulations = 200
#' )
#' }
power_analysis <- function(n_control,
                          n_treatment,
                          effect_size,
                          outcome_type = "continuous",
                          prior_specification = "default",
                          rope_limits = c(-0.1, 0.1),
                          prob_threshold = 0.95,
                          n_simulations = 1000,
                          baseline_mean = 0,
                          baseline_sd = 1,
                          baseline_prob = 0.5,
                          baseline_rate = 1,
                          covariates = NULL,
                          model_formula = NULL,
                          brms_args = list(),
                          seed = NULL,
                          parallel = FALSE,
                          cores = 2) {
  
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package 'brms' is required for this function.")
  }
  
  if (!requireNamespace("bayestestR", quietly = TRUE)) {
    stop("Package 'bayestestR' is required for this function.")
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Storage for results
  power_results <- vector("list", n_simulations)
  successful_fits <- 0
  
  # Progress tracking
  cat("Running", n_simulations, "power simulations...\n")
  
  # Define default model formula
  if (is.null(model_formula)) {
    if (is.null(covariates)) {
      model_formula <- "outcome ~ treatment"
    } else {
      covariate_terms <- paste(names(covariates), collapse = " + ")
      model_formula <- paste("outcome ~ treatment +", covariate_terms)
    }
  }
  
  # Simulation loop
  for (i in 1:n_simulations) {
    if (i %% 100 == 0) {
      cat("Simulation", i, "of", n_simulations, "\n")
    }
    
    tryCatch({
      # Generate data
      sim_data <- simulate_rct_data(
        n_control = n_control,
        n_treatment = n_treatment,
        effect_size = effect_size,
        outcome_type = outcome_type,
        baseline_mean = baseline_mean,
        baseline_sd = baseline_sd,
        baseline_prob = baseline_prob,
        baseline_rate = baseline_rate,
        covariates = covariates,
        seed = NULL  # Let each simulation have different seed
      )
      
      # Set up model family
      family_spec <- switch(outcome_type,
                           "continuous" = gaussian(),
                           "binary" = bernoulli(),
                           "count" = poisson())
      
      # Set up priors
      if (prior_specification == "default") {
        priors <- switch(outcome_type,
                        "continuous" = brms::prior(normal(0, 1), class = "b", coef = "treatment"),
                        "binary" = brms::prior(normal(0, 1), class = "b", coef = "treatment"),
                        "count" = brms::prior(normal(0, 1), class = "b", coef = "treatment"))
      } else {
        priors <- prior_specification
      }
      
      # Fit Bayesian model
      brms_args_default <- list(
        formula = as.formula(model_formula),
        data = sim_data,
        family = family_spec,
        prior = priors,
        chains = 2,
        iter = 1000,
        cores = 1,
        refresh = 0,
        silent = 2
      )
      
      # Merge user-specified arguments
      brms_args_final <- modifyList(brms_args_default, brms_args)
      
      # Fit model
      fit <- do.call(brms::brm, brms_args_final)
      
      # Extract treatment effect
      treatment_samples <- brms::posterior_samples(fit, pars = "b_treatment")$b_treatment
      
      # Calculate power metrics
      rope_result <- bayestestR::rope(treatment_samples, range = rope_limits)
      pd_result <- bayestestR::p_direction(treatment_samples)
      
      # Store results
      power_results[[i]] <- list(
        simulation = i,
        treatment_effect_mean = mean(treatment_samples),
        treatment_effect_sd = sd(treatment_samples),
        treatment_effect_median = median(treatment_samples),
        prob_direction = pd_result$pd,
        rope_prob = rope_result$ROPE_Percentage,
        outside_rope = 1 - rope_result$ROPE_Percentage,
        effect_positive = mean(treatment_samples > 0),
        effect_significant = ifelse(outcome_type == "continuous",
                                   mean(abs(treatment_samples) > rope_limits[2]),
                                   mean(treatment_samples > rope_limits[2])),
        converged = TRUE
      )
      
      successful_fits <- successful_fits + 1
      
    }, error = function(e) {
      # Handle convergence failures
      power_results[[i]] <- list(
        simulation = i,
        converged = FALSE,
        error_message = as.character(e)
      )
    })
  }
  
  # Process results
  successful_results <- power_results[sapply(power_results, function(x) !is.null(x) && x$converged)]
  
  if (length(successful_results) == 0) {
    stop("No simulations converged successfully. Check your model specification.")
  }
  
  # Calculate summary statistics
  power_summary <- list(
    n_simulations = n_simulations,
    successful_fits = successful_fits,
    convergence_rate = successful_fits / n_simulations,
    
    # Power estimates
    power_rope = mean(sapply(successful_results, function(x) x$outside_rope > prob_threshold)),
    power_direction = mean(sapply(successful_results, function(x) x$prob_direction > prob_threshold)),
    power_positive = mean(sapply(successful_results, function(x) x$effect_positive > prob_threshold)),
    power_significant = mean(sapply(successful_results, function(x) x$effect_significant)),
    
    # Effect size estimates
    mean_effect_estimate = mean(sapply(successful_results, function(x) x$treatment_effect_mean)),
    sd_effect_estimate = sd(sapply(successful_results, function(x) x$treatment_effect_mean)),
    
    # Study parameters
    study_parameters = list(
      n_control = n_control,
      n_treatment = n_treatment,
      true_effect_size = effect_size,
      outcome_type = outcome_type,
      rope_limits = rope_limits,
      prob_threshold = prob_threshold
    ),
    
    # Raw results
    simulation_results = successful_results
  )
  
  class(power_summary) <- "rctbayespower"
  
  cat("\nPower Analysis Complete!\n")
  cat("Successful fits:", successful_fits, "out of", n_simulations, "\n")
  cat("Power (ROPE):", round(power_summary$power_rope, 3), "\n")
  cat("Power (Direction):", round(power_summary$power_direction, 3), "\n")
  
  return(power_summary)
}
