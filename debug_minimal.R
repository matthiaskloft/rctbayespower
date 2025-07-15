# Minimal debug to bypass brms compilation issues
library(rctbayespower)

# Check if we can at least see where it fails by looking at the cached models situation
# and try to understand why we get NAs

# Let's see what happens if we try to run the power_analysis_ancova directly first
tryCatch({
  cat("=== Direct power_analysis_ancova test ===\n")
  direct_result <- power_analysis_ancova(
    n_control = 25,
    n_treatment = 25,
    outcome_type = "continuous",
    effect_size = 0.3,
    baseline_effect = 0.2,
    threshold_success = 0.2,
    threshold_futility = 0,
    n_simulations = 2,  # Minimal
    brms_args = list(
      algorithm = "fixed_param",
      iter = 5,
      chains = 1
    ),
    n_cores = 1
  )
  cat("Direct ANCOVA result structure:\n")
  str(direct_result, max.level = 1)
  cat("power_success:", direct_result$power_success, "\n")
  cat("power_futility:", direct_result$power_futility, "\n")
  
}, error = function(e) {
  cat("Direct ANCOVA failed:", e$message, "\n")
})