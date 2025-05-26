#' Effect Size Analysis for Bayesian RCTs
#'
#' Analyze the relationship between true effect sizes and estimated effect sizes
#' in Bayesian RCTs, including bias and precision assessments.
#'
#' @param n_control Number of participants in control group
#' @param n_treatment Number of participants in treatment group  
#' @param true_effect_sizes Vector of true effect sizes to test
#' @param outcome_type Type of outcome ("continuous", "binary", "count")
#' @param n_simulations Number of simulations per effect size
#' @param ... Additional arguments passed to power_analysis()
#'
#' @return A list containing effect size analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze effect size estimation
#' effect_analysis <- effect_size_analysis(
#'   n_control = 50,
#'   n_treatment = 50, 
#'   true_effect_sizes = c(0, 0.2, 0.5, 0.8),
#'   outcome_type = "continuous",
#'   n_simulations = 200
#' )
#' }
effect_size_analysis <- function(n_control,
                                n_treatment,
                                true_effect_sizes = c(0, 0.2, 0.5, 0.8),
                                outcome_type = "continuous", 
                                n_simulations = 500,
                                ...) {
  
  cat("Running effect size analysis\n")
  cat("Sample sizes: Control =", n_control, ", Treatment =", n_treatment, "\n")
  cat("True effect sizes:", paste(true_effect_sizes, collapse = ", "), "\n\n")
  
  results <- vector("list", length(true_effect_sizes))
  
  for (i in seq_along(true_effect_sizes)) {
    true_effect <- true_effect_sizes[i]
    
    cat("Analyzing true effect size:", true_effect, "\n")
    
    # Run power analysis to get effect size estimates
    power_result <- power_analysis(
      n_control = n_control,
      n_treatment = n_treatment,
      effect_size = true_effect,
      outcome_type = outcome_type,
      n_simulations = n_simulations,
      ...
    )
    
    # Extract effect size estimates from all simulations
    effect_estimates <- sapply(power_result$simulation_results, 
                              function(x) x$treatment_effect_mean)
    
    # Calculate bias and precision metrics
    bias <- mean(effect_estimates) - true_effect
    mse <- mean((effect_estimates - true_effect)^2)
    rmse <- sqrt(mse)
    coverage <- mean(sapply(power_result$simulation_results, function(sim) {
      # Calculate 95% credible interval coverage
      # This is a simplified version - in practice you'd want credible intervals
      lower <- sim$treatment_effect_mean - 1.96 * sim$treatment_effect_sd
      upper <- sim$treatment_effect_mean + 1.96 * sim$treatment_effect_sd
      true_effect >= lower && true_effect <= upper
    }))
    
    results[[i]] <- list(
      true_effect = true_effect,
      mean_estimate = mean(effect_estimates),
      median_estimate = median(effect_estimates),
      sd_estimate = sd(effect_estimates),
      bias = bias,
      mse = mse,
      rmse = rmse,
      coverage_95 = coverage,
      min_estimate = min(effect_estimates),
      max_estimate = max(effect_estimates),
      power_rope = power_result$power_rope,
      power_direction = power_result$power_direction,
      convergence_rate = power_result$convergence_rate,
      raw_estimates = effect_estimates
    )
    
    cat("  Mean estimate:", round(mean(effect_estimates), 3), "\n")
    cat("  Bias:", round(bias, 3), "\n")
    cat("  RMSE:", round(rmse, 3), "\n")
    cat("  Coverage (95%):", round(coverage, 3), "\n\n")
  }
  
  # Create summary data frame
  summary_df <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      true_effect = x$true_effect,
      mean_estimate = x$mean_estimate,
      median_estimate = x$median_estimate,
      sd_estimate = x$sd_estimate,
      bias = x$bias,
      mse = x$mse,
      rmse = x$rmse,
      coverage_95 = x$coverage_95,
      power_rope = x$power_rope,
      power_direction = x$power_direction,
      convergence_rate = x$convergence_rate
    )
  }))
  
  # Overall bias assessment
  overall_bias <- mean(summary_df$bias)
  overall_rmse <- sqrt(mean(summary_df$mse))
  overall_coverage <- mean(summary_df$coverage_95)
  
  summary_result <- list(
    n_control = n_control,
    n_treatment = n_treatment,
    outcome_type = outcome_type,
    n_simulations = n_simulations,
    
    # Summary statistics
    summary_table = summary_df,
    overall_bias = overall_bias,
    overall_rmse = overall_rmse,
    overall_coverage = overall_coverage,
    
    # Detailed results
    detailed_results = results
  )
  
  class(summary_result) <- "rctbayespower_effectsize"
  
  cat("Effect Size Analysis Complete!\n")
  cat("Overall bias:", round(overall_bias, 4), "\n")
  cat("Overall RMSE:", round(overall_rmse, 4), "\n")
  cat("Overall coverage (95%):", round(overall_coverage, 3), "\n")
  
  return(summary_result)
}

#' Print method for effect size analysis objects
#' @param x An rctbayespower_effectsize object
#' @param ... Additional arguments (unused)
#' @export
print.rctbayespower_effectsize <- function(x, ...) {
  cat("Bayesian RCT Effect Size Analysis\n")
  cat("=================================\n\n")
  
  cat("Study Parameters:\n")
  cat("  Sample size (control):", x$n_control, "\n")
  cat("  Sample size (treatment):", x$n_treatment, "\n")
  cat("  Outcome type:", x$outcome_type, "\n")
  cat("  Simulations per effect size:", x$n_simulations, "\n\n")
  
  cat("Overall Performance:\n")
  cat("  Overall bias:", round(x$overall_bias, 4), "\n")
  cat("  Overall RMSE:", round(x$overall_rmse, 4), "\n")
  cat("  Overall coverage (95%):", round(x$overall_coverage, 3), "\n\n")
  
  cat("Effect Size Summary:\n")
  print(round(x$summary_table, 3))
}
