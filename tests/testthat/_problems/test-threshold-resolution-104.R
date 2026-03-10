# Extracted from test-threshold-resolution.R:104

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
n_total <- 100
current_n <- 50
analysis_data <- data.frame(
    outcome = rnorm(current_n),
    arm = factor(sample(0:1, current_n, replace = TRUE))
  )
scheduled_frac <- current_n / n_total
actual_frac <- nrow(analysis_data) / n_total
expect_equal(scheduled_frac, actual_frac)
obf_fn <- boundary_obf()
