# Extracted from test-threshold-resolution.R:151

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
n_total <- 200
obf_fn <- boundary_obf()
