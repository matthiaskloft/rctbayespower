# Very simple debug test
library(rctbayespower)

# Try with multiple sample sizes
result <- tryCatch({
  power_grid_analysis(
    target_power_success = 0.8,
    sample_sizes = c(50, 100),  # Multiple sample sizes
    effect_sizes = c(0.3), # Just one effect size
    threshold_success = 0.2,
    threshold_futility = 0,
    power_analysis_fn = "power_analysis_ancova",
    outcome_type = "continuous",
    baseline_effect = 0.2,
    n_simulations = 5,  # Very small
    brms_args = list(algorithm = "fixed_param", iter = 10),  # Fastest possible
    n_cores = 1
  )
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  return(list(error = e$message))
})

cat("Result type:", class(result), "\n")
if (!is.null(result$error)) {
  cat("Error occurred:", result$error, "\n")
} else {
  cat("Power surface:\n")
  print(result$power_surface)
}