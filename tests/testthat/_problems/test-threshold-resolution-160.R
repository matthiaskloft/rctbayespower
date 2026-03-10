# Extracted from test-threshold-resolution.R:160

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
look_info <- data.frame(
    id_look = 1:3,
    n_analyzed = c(40, 80, 120)
  )
n_total <- 150
obf_fn <- boundary_obf(alpha = 0.025)
result <- resolve_boundary_vector(obf_fn, look_info, n_total)
