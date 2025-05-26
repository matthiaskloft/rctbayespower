#' Simulate RCT Data
#'
#' Generate simulated data for a randomized controlled trial with specified parameters.
#'
#' @param n_control Number of participants in control group
#' @param n_treatment Number of participants in treatment group
#' @param effect_size Expected effect size (Cohen's d)
#' @param outcome_type Type of outcome variable ("continuous", "binary", "count")
#' @param baseline_mean Baseline mean for continuous outcomes
#' @param baseline_sd Baseline standard deviation for continuous outcomes
#' @param baseline_prob Baseline probability for binary outcomes
#' @param baseline_rate Baseline rate for count outcomes
#' @param covariates List of covariate specifications
#' @param seed Random seed for reproducibility
#'
#' @return A data frame with simulated RCT data
#' @export
#'
#' @examples
#' # Continuous outcome
#' data <- simulate_rct_data(n_control = 50, n_treatment = 50, 
#'                          effect_size = 0.5, outcome_type = "continuous")
#' 
#' # Binary outcome  
#' data <- simulate_rct_data(n_control = 100, n_treatment = 100,
#'                          effect_size = 0.3, outcome_type = "binary",
#'                          baseline_prob = 0.2)
simulate_rct_data <- function(n_control, 
                             n_treatment,
                             effect_size,
                             outcome_type = "continuous",
                             baseline_mean = 0,
                             baseline_sd = 1,
                             baseline_prob = 0.5,
                             baseline_rate = 1,
                             covariates = NULL,
                             seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Validate inputs
  if (!outcome_type %in% c("continuous", "binary", "count")) {
    stop("outcome_type must be one of: 'continuous', 'binary', 'count'")
  }
  
  n_total <- n_control + n_treatment
  
  # Create treatment assignment
  treatment <- c(rep(0, n_control), rep(1, n_treatment))
  
  # Generate outcome based on type
  if (outcome_type == "continuous") {
    # For continuous outcomes, effect_size is Cohen's d
    control_outcome <- rnorm(n_control, baseline_mean, baseline_sd)
    treatment_outcome <- rnorm(n_treatment, 
                              baseline_mean + effect_size * baseline_sd, 
                              baseline_sd)
    outcome <- c(control_outcome, treatment_outcome)
    
  } else if (outcome_type == "binary") {
    # For binary outcomes, convert effect size to odds ratio
    control_odds <- baseline_prob / (1 - baseline_prob)
    treatment_odds <- control_odds * exp(effect_size)
    treatment_prob <- treatment_odds / (1 + treatment_odds)
    
    control_outcome <- rbinom(n_control, 1, baseline_prob)
    treatment_outcome <- rbinom(n_treatment, 1, treatment_prob)
    outcome <- c(control_outcome, treatment_outcome)
    
  } else if (outcome_type == "count") {
    # For count outcomes, effect size is log rate ratio
    control_outcome <- rpois(n_control, baseline_rate)
    treatment_outcome <- rpois(n_treatment, baseline_rate * exp(effect_size))
    outcome <- c(control_outcome, treatment_outcome)
  }
  
  # Create basic data frame
  data <- data.frame(
    id = 1:n_total,
    treatment = treatment,
    outcome = outcome
  )
  
  # Add covariates if specified
  if (!is.null(covariates)) {
    for (cov_name in names(covariates)) {
      cov_spec <- covariates[[cov_name]]
      if (cov_spec$type == "continuous") {
        data[[cov_name]] <- rnorm(n_total, cov_spec$mean, cov_spec$sd)
      } else if (cov_spec$type == "binary") {
        data[[cov_name]] <- rbinom(n_total, 1, cov_spec$prob)
      }
    }
  }
  
  return(data)
}
