# Simple test script to verify ANCOVA validation logic
# This simulates the validation without needing the full package dependencies

# Test validation functions (extracted from build_model_ancova)
test_ancova_validation <- function(n_arms = NULL, p_alloc = NULL, b_arm_treat = NULL, sigma = NULL) {
  
  # Validate p_alloc
  if (!is.null(p_alloc) && !is.null(n_arms)) {
    if (length(p_alloc) != n_arms) {
      stop("'p_alloc' must have length equal to 'n_arms'.")
    }
    if (abs(sum(p_alloc) - 1) > 1e-6) {
      stop("'p_alloc' must sum to 1.")
    }
  }
  
  # Validate b_arm_treat
  if (!is.null(b_arm_treat) && !is.null(n_arms)) {
    if (length(b_arm_treat) != (n_arms - 1)) {
      stop("'b_arm_treat' must have length equal to 'n_arms - 1'.")
    }
  }
  
  # Validate sigma
  if (!is.null(sigma) && sigma <= 0) {
    stop("'sigma' must be positive.")
  }
  
  return("Validation passed")
}

# Test cases
cat("Testing validation logic...\n")

# Test 1: Valid inputs should pass
tryCatch({
  result <- test_ancova_validation(n_arms = 2, p_alloc = c(0.5, 0.5), b_arm_treat = 0.5, sigma = 1)
  cat("✓ Test 1 passed: Valid inputs\n")
}, error = function(e) {
  cat("✗ Test 1 failed:", e$message, "\n")
})

# Test 2: Invalid p_alloc length should fail
tryCatch({
  result <- test_ancova_validation(n_arms = 2, p_alloc = c(0.5, 0.3, 0.2), b_arm_treat = 0.5, sigma = 1)
  cat("✗ Test 2 failed: Should have thrown error for wrong p_alloc length\n")
}, error = function(e) {
  cat("✓ Test 2 passed: Correctly caught p_alloc length error:", e$message, "\n")
})

# Test 3: p_alloc not summing to 1 should fail
tryCatch({
  result <- test_ancova_validation(n_arms = 2, p_alloc = c(0.6, 0.6), b_arm_treat = 0.5, sigma = 1)
  cat("✗ Test 3 failed: Should have thrown error for p_alloc not summing to 1\n")
}, error = function(e) {
  cat("✓ Test 3 passed: Correctly caught p_alloc sum error:", e$message, "\n")
})

# Test 4: Invalid b_arm_treat length should fail
tryCatch({
  result <- test_ancova_validation(n_arms = 2, p_alloc = c(0.5, 0.5), b_arm_treat = c(0.5, 0.3), sigma = 1)
  cat("✗ Test 4 failed: Should have thrown error for wrong b_arm_treat length\n")
}, error = function(e) {
  cat("✓ Test 4 passed: Correctly caught b_arm_treat length error:", e$message, "\n")
})

# Test 5: Negative sigma should fail
tryCatch({
  result <- test_ancova_validation(n_arms = 2, p_alloc = c(0.5, 0.5), b_arm_treat = 0.5, sigma = -1)
  cat("✗ Test 5 failed: Should have thrown error for negative sigma\n")
}, error = function(e) {
  cat("✓ Test 5 passed: Correctly caught negative sigma error:", e$message, "\n")
})

cat("Validation testing complete!\n")