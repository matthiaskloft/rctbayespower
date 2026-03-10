# Extracted from test-threshold-resolution.R:66

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
n_total <- 100
current_n <- 50
analysis_data <- data.frame(
    outcome = rnorm(40),
    arm = factor(sample(0:1, 40, replace = TRUE))
  )
scheduled_info_frac <- current_n / n_total
expect_equal(scheduled_info_frac, 0.5)
actual_info_frac <- nrow(analysis_data) / n_total
expect_equal(actual_info_frac, 0.4)
obf_fn <- boundary_obf()
