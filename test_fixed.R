# Test the improved power_grid_analysis function
library(rctbayespower)

cat("=== Testing improved power_grid_analysis function ===\n")

# Test 1: Missing required parameter (should give informative error)
cat("\n--- Test 1: Missing required parameter ---\n")
tryCatch({
  result1 <- power_grid_analysis(
    target_power_success = 0.8,
    sample_sizes = c(50, 100),
    effect_sizes = c(0.3),
    threshold_success = 0.2,
    threshold_futility = 0,
    power_analysis_fn = "power_analysis_ancova"
    # Missing outcome_type and baseline_effect - should give clear error
  )
}, error = function(e) {
  cat("Expected error caught:", e$message, "\n")
})

# Test 2: Valid parameters with fast defaults (should work better now)
cat("\n--- Test 2: Valid parameters with fast defaults ---\n")
result2 <- tryCatch({
  power_grid_analysis(
    target_power_success = 0.8,
    sample_sizes = c(50, 100),
    effect_sizes = c(0.3),
    threshold_success = 0.2,
    threshold_futility = 0,
    power_analysis_fn = "power_analysis_ancova",
    outcome_type = "continuous",
    baseline_effect = 0.2,
    n_simulations = 3  # Very small for testing
  )
}, error = function(e) {
  cat("Unexpected error:", e$message, "\n")
  return(NULL)
})

if (!is.null(result2)) {
  cat("Success! Power surface:\n")
  print(result2$power_surface[, c("n_total", "effect_size", "power_success", "power_futility")])
} else {
  cat("Function failed\n")
}