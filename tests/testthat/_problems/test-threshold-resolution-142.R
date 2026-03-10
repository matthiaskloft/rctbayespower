# Extracted from test-threshold-resolution.R:142

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
n_total <- 200
obf_fn <- boundary_obf(threshold = 0.95)
mild_frac <- 95 / n_total
thr_mild <- resolve_threshold(obf_fn, mild_frac)
severe_frac <- 60 / n_total
thr_severe <- resolve_threshold(obf_fn, severe_frac)
expect_gt(thr_severe, thr_mild)
