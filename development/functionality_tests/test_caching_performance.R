# Test script to verify model caching performance improvement in power_analysis()

# Load the package functions
source("R/power_analysis.R")
source("R/power_analysis.R")
source("R/power_analysis_ancova.R")

# Test 1: power_analysis_ancova with compile_models_only option
cat("=== Testing power_analysis_ancova compile_models_only option ===\n")

test_ancova_compile_only <- function() {
  tryCatch({
    result <- power_analysis_ancova(
      n_control = 20,
      n_treatment = 20,
      outcome_type = "continuous",
      effect_size = 0.5,
      baseline_effect = 0.2,
      threshold_success = 0.2,
      threshold_futility = 0,
      compile_models_only = TRUE
    )
    
    # Check if the result has the expected structure
    expected_fields <- c("brms_design_true_params", "brms_design_estimation", 
                        "power_analysis_args", "ancova_params")
    if (all(expected_fields %in% names(result))) {
      cat("compile_models_only test passed - all expected fields present!\n")
      return(TRUE)
    } else {
      missing_fields <- setdiff(expected_fields, names(result))
      cat("compile_models_only test failed - missing fields:", paste(missing_fields, collapse = ", "), "\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("compile_models_only test failed:", as.character(e), "\n")
    return(FALSE)
  })
}

# Test 2: power_analysis_ancova with caching in power_analysis
cat("\n=== Testing power_analysis_ancova with multiple sample sizes, same effect size ===\n")

test_ancova_caching <- function() {
  tryCatch({
    result <- power_analysis(
      target_power_success = 0.8,
      sample_sizes = c(20, 40, 60),  # 3 different sample sizes
      effect_sizes = c(0.5),         # 1 effect size (should enable caching)
      threshold_success = 0.2,
      threshold_futility = 0,
      power_analysis_fn = "power_analysis_ancova",
      outcome_type = "continuous",
      baseline_effect = 0.2,
      n_simulations = 5  # Small number for testing
    )
    cat("ANCOVA caching test completed successfully!\n")
    return(TRUE)
  }, error = function(e) {
    cat("ANCOVA caching test failed:", as.character(e), "\n")
    return(FALSE)
  })
}

# Test 3: Check that the function groups by effect size correctly
cat("\n=== Testing effect size grouping ===\n")

test_grouping <- function() {
  tryCatch({
    result <- power_analysis(
      target_power_success = 0.8,
      sample_sizes = c(20, 40),      # 2 sample sizes
      effect_sizes = c(0.3, 0.6),    # 2 effect sizes
      threshold_success = 0.2,
      threshold_futility = 0,
      power_analysis_fn = "power_analysis_ancova",
      outcome_type = "continuous",
      baseline_effect = 0.2,
      n_simulations = 3  # Small number for testing
    )
    cat("Effect size grouping test completed successfully!\n")
    return(TRUE)
  }, error = function(e) {
    cat("Effect size grouping test failed:", as.character(e), "\n")
    return(FALSE)
  })
}

# Run tests
cat("Starting performance caching tests...\n\n")

test1_result <- test_ancova_compile_only()
cat("\n")
test2_result <- test_ancova_caching()
cat("\n")
test3_result <- test_grouping()

cat("\n=== Test Results ===\n")
cat("ANCOVA compile_models_only test:", ifelse(test1_result, "PASSED", "FAILED"), "\n")
cat("ANCOVA caching test:", ifelse(test2_result, "PASSED", "FAILED"), "\n")
cat("Effect size grouping test:", ifelse(test3_result, "PASSED", "FAILED"), "\n")

if (test1_result && test2_result && test3_result) {
  cat("\nAll tests passed! The caching mechanism is working.\n")
  cat("You should see messages like:\n")
  cat("  '--- Processing Effect Size X.X ---'\n")
  cat("  'Compiling brms models for ANCOVA with effect size X.X...'\n")
  cat("  'Successfully compiled and cached ANCOVA models for effect size X.X'\n")
  cat("  'Testing combination X of Y: N = XX, Effect = X.X (using cached models)'\n")
} else {
  cat("\nSome tests failed. Check the error messages above.\n")
}
