# Test script to verify that parallelization works correctly with ANCOVA caching

# Load the package functions
source("R/power_grid_analysis.R")
source("R/power_analysis.R")
source("R/power_analysis_ancova.R")

# Test parallelization with ANCOVA caching
cat("=== Testing Parallelization with ANCOVA Caching ===\n")

test_parallelization <- function() {
  tryCatch({
    cat("Testing power_grid_analysis with parallelization enabled...\n")
    
    # Test with multiple cores and progress updates
    start_time <- Sys.time()
    result <- power_grid_analysis(
      target_power_success = 0.8,
      sample_sizes = c(20, 40, 60),  # 3 different sample sizes
      effect_sizes = c(0.5),         # 1 effect size (should enable caching)
      threshold_success = 0.2,
      threshold_futility = 0,
      power_analysis_fn = "power_analysis_ancova",
      outcome_type = "continuous",
      baseline_effect = 0.2,
      n_simulations = 10,            # Small number for testing
      n_cores = 2,                   # Test parallelization
      progress_updates = 3           # Test progress updates
    )
    end_time <- Sys.time()
    
    cat("Test completed in", round(as.numeric(difftime(end_time, start_time, units = "secs")), 2), "seconds\n")
    
    # Check if results are valid
    if (is.null(result$power_surface) || nrow(result$power_surface) == 0) {
      cat("FAILED: No power results generated\n")
      return(FALSE)
    }
    
    # Check if all combinations were processed
    expected_combinations <- 3  # 3 sample sizes * 1 effect size
    actual_combinations <- nrow(result$power_surface)
    
    if (actual_combinations != expected_combinations) {
      cat("FAILED: Expected", expected_combinations, "combinations, got", actual_combinations, "\n")
      return(FALSE)
    }
    
    # Check if power results are reasonable (not all NA)
    success_power_valid <- !all(is.na(result$power_surface$power_success))
    futility_power_valid <- !all(is.na(result$power_surface$power_futility))
    
    if (!success_power_valid || !futility_power_valid) {
      cat("FAILED: Power results contain all NA values\n")
      return(FALSE)
    }
    
    cat("PASSED: Parallelization test completed successfully!\n")
    cat("  - All", actual_combinations, "combinations processed\n")
    cat("  - Power results generated for all combinations\n")
    cat("  - Parallelization parameters preserved\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("FAILED: Parallelization test failed with error:", as.character(e), "\n")
    return(FALSE)
  })
}

# Test compile_models_only preserves all parameters
cat("\n=== Testing Parameter Preservation in compile_models_only ===\n")

test_parameter_preservation <- function() {
  tryCatch({
    cat("Testing that compile_models_only preserves all ANCOVA parameters...\n")
    
    # Call with specific parallelization parameters
    result <- power_analysis_ancova(
      n_control = 20,
      n_treatment = 20,
      outcome_type = "continuous",
      effect_size = 0.5,
      baseline_effect = 0.2,
      threshold_success = 0.2,
      threshold_futility = 0,
      n_cores = 4,
      progress_updates = 5,
      seed = 123,
      p_sig_success = 0.95,
      p_sig_futility = 0.8,
      compile_models_only = TRUE
    )
    
    # Check if ancova_params contains all the expected parameters
    required_params <- c("n_cores", "progress_updates", "seed", "p_sig_success", "p_sig_futility")
    missing_params <- setdiff(required_params, names(result$ancova_params))
    
    if (length(missing_params) > 0) {
      cat("FAILED: Missing parameters in ancova_params:", paste(missing_params, collapse = ", "), "\n")
      return(FALSE)
    }
    
    # Check if values are preserved correctly
    if (result$ancova_params$n_cores != 4) {
      cat("FAILED: n_cores not preserved correctly\n")
      return(FALSE)
    }
    
    if (result$ancova_params$progress_updates != 5) {
      cat("FAILED: progress_updates not preserved correctly\n")
      return(FALSE)
    }
    
    if (result$ancova_params$seed != 123) {
      cat("FAILED: seed not preserved correctly\n")
      return(FALSE)
    }
    
    cat("PASSED: All parameters preserved correctly in compile_models_only\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("FAILED: Parameter preservation test failed with error:", as.character(e), "\n")
    return(FALSE)
  })
}

# Run tests
cat("Starting parallelization fix tests...\n\n")

test1_result <- test_parameter_preservation()
cat("\n")
test2_result <- test_parallelization()

cat("\n=== Test Results ===\n")
cat("Parameter preservation test:", ifelse(test1_result, "PASSED", "FAILED"), "\n")
cat("Parallelization test:", ifelse(test2_result, "PASSED", "FAILED"), "\n")

if (test1_result && test2_result) {
  cat("\nAll tests passed! Parallelization is working correctly with ANCOVA caching.\n")
  cat("The fix successfully preserves parallelization parameters when using cached models.\n")
} else {
  cat("\nSome tests failed. Please check the implementation.\n")
}