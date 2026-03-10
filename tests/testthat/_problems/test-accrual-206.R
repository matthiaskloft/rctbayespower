# Extracted from test-accrual.R:206

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
expect_error(validate_accrual_params(analysis_timing = "events"), "must be one of")
