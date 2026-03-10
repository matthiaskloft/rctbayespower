# Extracted from test-threshold-resolution.R:18

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
obf_fn <- boundary_obf(threshold = 0.95)
val_early <- resolve_threshold(obf_fn, info_frac = 0.25)
val_late <- resolve_threshold(obf_fn, info_frac = 0.75)
expect_gt(val_early, val_late)
