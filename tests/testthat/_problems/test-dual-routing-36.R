# Extracted from test-dual-routing.R:36

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
survival_sim_fn <- function(n_total, p_alloc, baseline_hazard = NULL,
                               hazard_ratio = NULL, accrual_rate = NULL,
                               followup_time = NULL) {
    data.frame(
      outcome = rnorm(n_total),
      arm = factor(sample(0:1, n_total, replace = TRUE, prob = p_alloc))
    )
  }
design <- mock_design(sim_fn = survival_sim_fn)
conditions <- build_conditions(
    design,
    crossed = list(
      baseline_hazard = 0.1,
      hazard_ratio = 0.7,
      n_total = 100
    ),
    constant = list(
      accrual_rate = 5,
      followup_time = 12,
      thr_fx_eff = 0, thr_fx_fut = 0,
      thr_dec_eff = 0.95, thr_dec_fut = 0.05
    )
  )
