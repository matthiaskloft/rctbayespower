# Extracted from test-dual-routing.R:64

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
design <- mock_design()
conditions <- build_conditions(
    design,
    crossed = list(
      b_arm_treat = 0.3,
      n_total = 100
    ),
    constant = list(
      accrual_rate = 5,
      followup_time = 12,
      thr_fx_eff = 0, thr_fx_fut = 0,
      thr_dec_eff = 0.95, thr_dec_fut = 0.05
    )
  )
