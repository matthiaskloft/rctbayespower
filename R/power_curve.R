#' Bayesian Power Curve Analysis
#'
#' Generate power curves across different effect sizes for Bayesian RCTs.
#' This function helps visualize how power changes with effect size.
#'
#' @param n_control Number of participants in control group
#' @param n_treatment Number of participants in treatment group
#' @param effect_sizes Vector of effect sizes to test
#' @param outcome_type Type of outcome ("continuous", "binary", "count")
#' @param n_simulations Number of simulations per effect size
#' @param ... Additional arguments passed to power_analysis()
#'
#' @return A list containing power curve results
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate power curve
#' power_curve_result <- bayesian_power_curve(
#'   n_control = 50,
#'   n_treatment = 50,
#'   effect_sizes = seq(0, 1, by = 0.1),
#'   outcome_type = "continuous",
#'   n_simulations = 200
#' )
#' }
bayesian_power_curve <- function(n_control,
                                n_treatment,
                                effect_sizes = seq(0, 1, by = 0.1),
                                outcome_type = "continuous",
                                n_simulations = 500,
                                ...) {
  
  cat("Generating Bayesian power curve\n")
  cat("Sample sizes: Control =", n_control, ", Treatment =", n_treatment, "\n")
  cat("Effect sizes:", paste(effect_sizes, collapse = ", "), "\n\n")
  
  results <- vector("list", length(effect_sizes))
  
  for (i in seq_along(effect_sizes)) {
    effect_size <- effect_sizes[i]
    
    cat("Testing effect size:", effect_size, "\n")
    
    # Run power analysis
    power_result <- power_analysis(
      n_control = n_control,
      n_treatment = n_treatment,
      effect_size = effect_size,
      outcome_type = outcome_type,
      n_simulations = n_simulations,
      ...
    )
    
    results[[i]] <- list(
      effect_size = effect_size,
      power_rope = power_result$power_rope,
      power_direction = power_result$power_direction,
      power_significant = power_result$power_significant,
      mean_effect_estimate = power_result$mean_effect_estimate,
      convergence_rate = power_result$convergence_rate
    )
    
    cat("  Power (ROPE):", round(power_result$power_rope, 3), "\n")
    cat("  Power (Direction):", round(power_result$power_direction, 3), "\n\n")
  }
  
  # Create summary data frame
  power_curve_df <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      effect_size = x$effect_size,
      power_rope = x$power_rope,
      power_direction = x$power_direction,
      power_significant = x$power_significant,
      mean_effect_estimate = x$mean_effect_estimate,
      convergence_rate = x$convergence_rate
    )
  }))
  
  summary_result <- list(
    n_control = n_control,
    n_treatment = n_treatment,
    outcome_type = outcome_type,
    
    # Power curve data
    power_curve = power_curve_df,
    
    # Individual results
    detailed_results = results
  )
  
  class(summary_result) <- "rctbayespower_curve"
  
  cat("Power Curve Analysis Complete!\n")
  
  return(summary_result)
}
