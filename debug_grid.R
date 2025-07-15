# Debug script for power_grid_analysis NA issue
library(rctbayespower)

# Minimal test case
result <- power_grid_analysis(
  target_power_success = 0.8,
  sample_sizes = c(50, 100),
  effect_sizes = c(0.3, 0.5),
  threshold_success = 0.2,
  threshold_futility = 0,
  power_analysis_fn = "power_analysis_ancova",
  outcome_type = "continuous",
  baseline_effect = 0.2,
  n_simulations = 10,  # Very small for quick test
  brms_args = list(algorithm = "meanfield"),  # Fast algorithm
  n_cores = 1,
  progress_updates = 5
)

# Print results
print("Result structure:")
str(result)

print("Power surface:")
print(result$power_surface)