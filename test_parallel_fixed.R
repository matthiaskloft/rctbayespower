# Test the corrected in_parallel() implementation
library(rctbayespower)

cat("=== Testing corrected purrr 1.1.0 in_parallel() implementation ===\n")

# Check if mirai and purrr 1.1.0 are available
purrr_version <- packageVersion("purrr")
has_mirai <- requireNamespace("mirai", quietly = TRUE)

cat("purrr version:", as.character(purrr_version), "\n")
cat("mirai available:", has_mirai, "\n")

if (purrr_version >= "1.1.0" && has_mirai) {
  cat("\n--- Testing with corrected parallel implementation ---\n")
  
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
    cat("SUCCESS! Power surface:\n")
    print(result$power_surface[, c("n_total", "effect_size", "power_success", "power_futility")])
    
    # Check if any values are NA
    na_count <- sum(is.na(result$power_surface$power_success))
    if (na_count > 0) {
      cat("WARNING: Still has", na_count, "NA values in power_success\n")
    } else {
      cat("EXCELLENT: No NA values found!\n")
    }
  }
  
} else {
  cat("Skipping parallel test - purrr 1.1.0 and mirai not available\n")
}