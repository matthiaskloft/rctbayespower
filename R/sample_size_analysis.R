#' Sample Size Analysis for Bayesian RCTs
#'
#' Determine optimal sample size for achieving desired power in Bayesian RCTs.
#' This function runs power analyses across a range of sample sizes to find
#' the minimum sample size needed to achieve target power.
#'
#' @param effect_size Expected effect size
#' @param target_power Desired power level (default: 0.8)
#' @param outcome_type Type of outcome ("continuous", "binary", "count")
#' @param sample_sizes Vector of sample sizes to test (per group)
#' @param allocation_ratio Ratio of treatment to control (default: 1)
#' @param n_simulations Number of simulations per sample size
#' @param ... Additional arguments passed to power_analysis()
#'
#' @return A list containing sample size analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Find sample size for 80% power
#' sample_size_result <- sample_size_analysis(
#'   effect_size = 0.5,
#'   target_power = 0.8,
#'   outcome_type = "continuous",
#'   sample_sizes = seq(20, 100, by = 10),
#'   n_simulations = 100
#' )
#' }
sample_size_analysis <- function(effect_size,
                                target_power = 0.8,
                                outcome_type = "continuous",
                                sample_sizes = seq(20, 200, by = 20),
                                allocation_ratio = 1,
                                n_simulations = 500,
                                ...) {
  
  cat("Running sample size analysis for effect size:", effect_size, "\n")
  cat("Target power:", target_power, "\n")
  cat("Testing sample sizes:", paste(sample_sizes, collapse = ", "), "\n\n")
  
  results <- vector("list", length(sample_sizes))
  
  for (i in seq_along(sample_sizes)) {
    n_per_group <- sample_sizes[i]
    n_control <- n_per_group
    n_treatment <- round(n_per_group * allocation_ratio)
    
    cat("Testing sample size:", n_per_group, "per group (total:", n_control + n_treatment, ")\n")
    
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
      n_per_group = n_per_group,
      n_total = n_control + n_treatment,
      n_control = n_control,
      n_treatment = n_treatment,
      power_rope = power_result$power_rope,
      power_direction = power_result$power_direction,
      power_significant = power_result$power_significant,
      convergence_rate = power_result$convergence_rate
    )
    
    cat("  Power (ROPE):", round(power_result$power_rope, 3), "\n")
    cat("  Power (Direction):", round(power_result$power_direction, 3), "\n\n")
  }
  
  # Create summary data frame
  power_df <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      n_per_group = x$n_per_group,
      n_total = x$n_total,
      power_rope = x$power_rope,
      power_direction = x$power_direction,
      power_significant = x$power_significant,
      convergence_rate = x$convergence_rate
    )
  }))
  
  # Find minimum sample size for target power
  adequate_power_rope <- power_df$power_rope >= target_power
  adequate_power_direction <- power_df$power_direction >= target_power
  
  min_n_rope <- if (any(adequate_power_rope)) {
    min(power_df$n_per_group[adequate_power_rope])
  } else {
    NA
  }
  
  min_n_direction <- if (any(adequate_power_direction)) {
    min(power_df$n_per_group[adequate_power_direction])
  } else {
    NA
  }
  
  summary_result <- list(
    effect_size = effect_size,
    target_power = target_power,
    outcome_type = outcome_type,
    allocation_ratio = allocation_ratio,
    
    # Minimum sample sizes
    min_n_rope = min_n_rope,
    min_n_direction = min_n_direction,
    
    # Power curve data
    power_curve = power_df,
    
    # Individual results
    detailed_results = results
  )
  
  class(summary_result) <- "rctbayespower_samplesize"
  
  cat("Sample Size Analysis Complete!\n")
  if (!is.na(min_n_rope)) {
    cat("Minimum sample size (ROPE):", min_n_rope, "per group\n")
  } else {
    cat("Target power not achieved with tested sample sizes (ROPE)\n")
  }
  
  if (!is.na(min_n_direction)) {
    cat("Minimum sample size (Direction):", min_n_direction, "per group\n")
  } else {
    cat("Target power not achieved with tested sample sizes (Direction)\n")
  }
  
  return(summary_result)
}
