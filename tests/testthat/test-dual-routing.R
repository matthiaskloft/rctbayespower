# test-dual-routing.R
# Tests for dual routing of params that appear in both sim and analysis contexts

test_that("DUAL_ROUTE_PARAMS contains expected parameters", {
  expect_true("accrual_rate" %in% DUAL_ROUTE_PARAMS)
  expect_true("followup_time" %in% DUAL_ROUTE_PARAMS)
})

test_that("dual routing copies accrual_rate to analysis_args when in sim_fn formals", {
  # Create a sim_fn that has accrual_rate as a formal (like survival sim_fns)
  survival_sim_fn <- function(n_total, p_alloc, baseline_hazard = NULL,
                               hazard_ratio = NULL, accrual_rate = NULL,
                               followup_time = NULL) {
    data.frame(
      outcome = rnorm(n_total),
      arm = factor(sample(0:1, n_total, replace = TRUE, prob = p_alloc))
    )
  }

  design <- mock_design(sim_fn = survival_sim_fn)

  # accrual_rate should route to both sim_args and analysis_args
  conditions <- build_conditions(
    design,
    crossed = list(
      baseline_hazard = 0.1,
      hazard_ratio = 0.7,
      n_total = 100
    ),
    constant = list(
      p_alloc = c(0.5, 0.5),
      accrual_rate = 5,
      followup_time = 12,
      thr_fx_eff = 0, thr_fx_fut = 0,
      thr_dec_eff = 0.95, thr_dec_fut = 0.05
    )
  )

  # Access the internal condition list
  cond_params <- conditions@params_by_cond[[1]]

  # accrual_rate should be in both sim_args and analysis_args
  expect_true("accrual_rate" %in% names(cond_params$sim_args))
  expect_true("accrual_rate" %in% names(cond_params$analysis_args))
  expect_equal(cond_params$sim_args$accrual_rate, 5)
  expect_equal(cond_params$analysis_args$accrual_rate, 5)
})

test_that("ANCOVA sim_fn without accrual_rate formal: no dual routing", {
  # Standard ANCOVA sim_fn doesn't have accrual_rate in formals
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

  cond_params <- conditions@params_by_cond[[1]]

  # accrual_rate should be in analysis_args but NOT sim_args (not a formal)
  expect_false("accrual_rate" %in% names(cond_params$sim_args))
  expect_true("accrual_rate" %in% names(cond_params$analysis_args))
})

test_that("analysis_timing = 'events' is accepted in validation", {
  # Should not error
  expect_no_error(
    validate_accrual_params(
      accrual_rate = 10,
      accrual_pattern = NULL,
      followup_time = NULL,
      analysis_timing = "events",
      calendar_analysis_at = NULL,
      dropout = NULL
    )
  )
})

test_that("analysis_timing = 'invalid_value' is rejected", {
  expect_error(
    validate_accrual_params(
      accrual_rate = NULL,
      accrual_pattern = NULL,
      followup_time = NULL,
      analysis_timing = "invalid_value",
      calendar_analysis_at = NULL,
      dropout = NULL
    ),
    "analysis_timing"
  )
})
