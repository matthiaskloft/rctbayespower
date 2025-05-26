# Package Validation Script for rctbayespower
# This script tests core functionality without requiring brms/Stan

library(rctbayespower)

cat("=== Testing rctbayespower Package ===\n")

# Test 1: Data simulation
cat("\n1. Testing data simulation...\n")
tryCatch({
  data_cont <- simulate_rct_data(
    n_control = 20,
    n_treatment = 20,
    effect_size = 0.5,
    outcome_type = "continuous",
    seed = 123
  )
  cat("✓ Continuous data simulation: OK\n")
  
  data_bin <- simulate_rct_data(
    n_control = 30,
    n_treatment = 30,
    effect_size = 0.3,
    outcome_type = "binary",
    baseline_prob = 0.3,
    seed = 456
  )
  cat("✓ Binary data simulation: OK\n")
  
  data_count <- simulate_rct_data(
    n_control = 25,
    n_treatment = 25,
    effect_size = 0.4,
    outcome_type = "count",
    baseline_rate = 2,
    seed = 789
  )
  cat("✓ Count data simulation: OK\n")
}, error = function(e) {
  cat("✗ Data simulation failed:", e$message, "\n")
})

# Test 2: Data simulation with covariates
cat("\n2. Testing data simulation with covariates...\n")
tryCatch({
  covariates <- list(
    age = list(type = "continuous", mean = 50, sd = 10),
    sex = list(type = "binary", prob = 0.5)
  )
  
  data_cov <- simulate_rct_data(
    n_control = 15,
    n_treatment = 15,
    effect_size = 0.5,
    outcome_type = "continuous",
    covariates = covariates,
    seed = 111
  )
  cat("✓ Covariate simulation: OK\n")
}, error = function(e) {
  cat("✗ Covariate simulation failed:", e$message, "\n")
})

# Test 3: Input validation
cat("\n3. Testing input validation...\n")
tryCatch({
  # This should trigger an error
  simulate_rct_data(n_control = 0, n_treatment = 10, effect_size = 0.5)
  cat("✗ Input validation failed: Should have caught invalid sample size\n")
}, error = function(e) {
  cat("✓ Input validation: OK (caught invalid input)\n")
})

# Test 4: Check package structure
cat("\n4. Checking package structure...\n")
funcs <- ls("package:rctbayespower")
expected_funcs <- c("power_analysis", "sample_size_analysis", "bayesian_power_curve", 
                   "effect_size_analysis", "simulate_rct_data", "plot_power_curve")

missing_funcs <- setdiff(expected_funcs, funcs)
if (length(missing_funcs) == 0) {
  cat("✓ All expected functions exported\n")
} else {
  cat("✗ Missing functions:", paste(missing_funcs, collapse = ", "), "\n")
}

# Test 5: Plotting (mock data)
cat("\n5. Testing plotting with mock data...\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  tryCatch({
    # Create mock power curve data
    mock_data <- list(
      effect_sizes = c(0, 0.2, 0.5, 0.8),
      power_rope = c(0.05, 0.2, 0.7, 0.9),
      power_direction = c(0.1, 0.3, 0.8, 0.95),
      n_control = 50,
      n_treatment = 50,
      outcome_type = "continuous"
    )
    class(mock_data) <- "rctbayespower_curve"
    
    p <- plot_power_curve(mock_data)
    cat("✓ Plotting functionality: OK\n")
  }, error = function(e) {
    cat("✗ Plotting failed:", e$message, "\n")
  })
} else {
  cat("- ggplot2 not available, skipping plot test\n")
}

cat("\n=== Validation Complete ===\n")
cat("Note: Full power analysis requires brms/Stan installation\n")
cat("Use build_package.R for complete package building instructions\n")
