# Extracted from test-dual-routing.R:69

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
      p_alloc = c(0.5, 0.5),
      intercept = 0, b_covariate = 0, sigma = 1,
      accrual_rate = 5,
      followup_time = 12,
      thr_fx_eff = 0, thr_fx_fut = 0,
      thr_dec_eff = 0.95, thr_dec_fut = 0.05
    )
  )
cond <- conditions@condition_list[[1]]
