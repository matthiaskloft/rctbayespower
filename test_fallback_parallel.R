# Test the function with fallback parallel implementation (parallel package)
library(rctbayespower)

cat("=== Testing fallback parallel implementation ===\n")

# This should use the older parallel package since purrr < 1.1.0
result <- tryCatch({
  power_grid_analysis(
    target_power_success = 0.8,
    sample_sizes = c(50, 100),
    effect_sizes = c(0.3),
    threshold_success = 0.2,
    threshold_futility = 0,
    power_analysis_fn = "power_analysis_ancova",
    outcome_type = "continuous",
    baseline_effect = 0.2,
    n_simulations = 3,  # Very small for testing
    n_cores = 2  # Enable parallel processing
  )
}, error = function(e) {
  cat("Error occurred:", e$message, "\n")
  return(NULL)
})

if (!is.null(result)) {
  cat("SUCCESS! Function completed without crashing\n")
  cat("Power surface preview:\n")
  print(result$power_surface[, c("n_total", "effect_size", "power_success", "power_futility")])
  
  # Check for NA values
  na_count <- sum(is.na(result$power_surface$power_success))
  if (na_count > 0) {
    cat("WARNING: Found", na_count, "NA values in power_success\n")
    # Show errors if available
    if (!is.null(result$power_surface$error)) {
      errors <- result$power_surface$error[!is.na(result$power_surface$error)]
      if (length(errors) > 0) {
        cat("Errors found:\n")
        for (i in seq_along(errors)) {
          cat("  ", i, ":", errors[i], "\n")
        }
      }
    }
  } else {
    cat("EXCELLENT: No NA values found!\n")
  }
} else {
  cat("Function failed to complete\n")
}