# Extracted from test-threshold-resolution.R:84

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
n_total <- 100
current_n <- 50
info_frac <- current_n / n_total
expect_equal(info_frac, 0.5)
obf_fn <- boundary_obf()
