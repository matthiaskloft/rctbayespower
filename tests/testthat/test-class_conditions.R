# Tests for rctbp_conditions class (R/class_conditions.R)

# =============================================================================
# link() HELPER
# =============================================================================

test_that("link creates rctbp_link object", {
  l <- link(n_total = c(100, 200), effect = c(0.3, 0.5))
  expect_s3_class(l, "rctbp_link")
  expect_equal(l$n_total, c(100, 200))
  expect_equal(l$effect, c(0.3, 0.5))
})

test_that("link rejects unnamed parameters", {
  expect_cli_abort(link(c(100, 200), c(0.3, 0.5)))
})

test_that("link rejects mismatched lengths", {
  expect_cli_abort(link(a = c(1, 2), b = c(1, 2, 3)))
})

test_that("link handles list values", {
  l <- link(
    n_total = c(100, 200),
    analysis_at = list(c(50, 100), c(100, 200))
  )
  expect_s3_class(l, "rctbp_link")
  expect_equal(length(l$analysis_at), 2)
})

# =============================================================================
# build_conditions() - BASIC CONSTRUCTION
# =============================================================================

test_that("build_conditions creates valid conditions object", {
  d <- mock_design()
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = c(100, 200)),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      b_arm_treat = 0.3,
      intercept = 0,
      b_covariate = 0.3,
      sigma = 1,
      thr_dec_eff = 0.975,
      thr_dec_fut = 0.5,
      thr_fx_eff = 0.2,
      thr_fx_fut = 0
    )
  )

  expect_s3_class(cond, "rctbayespower::rctbp_conditions")
  expect_equal(nrow(cond@grid), 2)
  expect_equal(length(cond@params_by_cond), 2)
})

test_that("build_conditions creates Cartesian product", {
  d <- mock_design()
  cond <- build_conditions(
    design = d,
    crossed = list(
      n_total = c(100, 200),
      b_arm_treat = c(0.3, 0.5)
    ),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      intercept = 0,
      b_covariate = 0.3,
      sigma = 1,
      thr_dec_eff = 0.975,
      thr_dec_fut = 0.5,
      thr_fx_eff = 0.2,
      thr_fx_fut = 0
    )
  )

  # 2 x 2 = 4 conditions
  expect_equal(nrow(cond@grid), 4)
  expect_equal(length(cond@params_by_cond), 4)
})

test_that("build_conditions handles link() correctly", {
  d <- mock_design(trial_type = "group_sequential")
  cond <- build_conditions(
    design = d,
    crossed = list(
      link(
        n_total = c(100, 200),
        analysis_at = list(c(50, 100), c(100, 200))
      ),
      b_arm_treat = c(0.3, 0.5)
    ),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      intercept = 0,
      b_covariate = 0.3,
      sigma = 1,
      thr_dec_eff = 0.975,
      thr_dec_fut = 0.5,
      thr_fx_eff = 0.2,
      thr_fx_fut = 0
    )
  )

  # 2 (linked) x 2 (b_arm_treat) = 4
  expect_equal(nrow(cond@grid), 4)
  expect_equal(length(cond@linked_params), 1)
  expect_true(all(c("n_total", "analysis_at") %in% cond@linked_params[[1]]))
})

# =============================================================================
# build_conditions() - PARAMETER SEPARATION
# =============================================================================

test_that("build_conditions separates sim_args and analysis_args", {
  d <- mock_design()
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = 100),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      b_arm_treat = 0.3,
      intercept = 0,
      b_covariate = 0.3,
      sigma = 1,
      thr_dec_eff = 0.975,
      thr_dec_fut = 0.5,
      thr_fx_eff = 0.2,
      thr_fx_fut = 0
    )
  )

  params <- cond@params_by_cond[[1]]
  expect_true("sim_args" %in% names(params))
  expect_true("analysis_args" %in% names(params))

  # Sim args should contain simulation parameters
  expect_true("n_total" %in% names(params$sim_args))
  expect_true("p_alloc" %in% names(params$sim_args))
  expect_true("b_arm_treat" %in% names(params$sim_args))

  # Analysis args should contain threshold parameters
  expect_true("thr_dec_eff" %in% names(params$analysis_args))
  expect_true("thr_fx_eff" %in% names(params$analysis_args))
  expect_true("trial_type" %in% names(params$analysis_args))
})

# =============================================================================
# build_conditions() - INPUT VALIDATION
# =============================================================================

test_that("build_conditions rejects invalid design", {
  expect_cli_abort(
    build_conditions(
      design = "not a design",
      crossed = list(n_total = 100)
    )
  )
})

test_that("build_conditions rejects non-list crossed", {
  d <- mock_design()
  expect_cli_abort(
    build_conditions(design = d, crossed = "not a list")
  )
})

test_that("build_conditions rejects non-list constant", {
  d <- mock_design()
  expect_cli_abort(
    build_conditions(design = d, constant = "not a list")
  )
})

test_that("build_conditions rejects overlapping crossed/constant params", {
  d <- mock_design()
  expect_cli_abort(
    build_conditions(
      design = d,
      crossed = list(n_total = c(100, 200)),
      constant = list(
        n_total = 150,
        p_alloc = list(c(0.5, 0.5)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0
      )
    )
  )
})

test_that("build_conditions rejects missing required parameters", {
  d <- mock_design()
  expect_cli_abort(
    build_conditions(
      design = d,
      crossed = list(n_total = c(100, 200))
      # Missing all other required params
    )
  )
})

test_that("build_conditions validates target_pwr range", {
  d <- mock_design()
  expect_cli_abort(
    build_conditions(
      design = d,
      crossed = list(n_total = 100),
      constant = list(
        p_alloc = list(c(0.5, 0.5)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0
      ),
      target_pwr = 1.5
    )
  )
})

test_that("build_conditions rejects p_alloc not summing to 1", {
  d <- mock_design()
  expect_cli_abort(
    build_conditions(
      design = d,
      crossed = list(n_total = 100),
      constant = list(
        p_alloc = list(c(0.3, 0.3)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0
      )
    )
  )
})

# =============================================================================
# build_conditions() - TRIAL TYPE VALIDATION
# =============================================================================

test_that("build_conditions errors when group_sequential lacks analysis_at", {
  d <- mock_design(trial_type = "group_sequential")
  expect_cli_abort(
    build_conditions(
      design = d,
      crossed = list(n_total = 100),
      constant = list(
        p_alloc = list(c(0.5, 0.5)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0
      )
    )
  )
})

test_that("build_conditions warns when fixed has analysis_at", {
  d <- mock_design(trial_type = "fixed")
  expect_cli_warn(
    build_conditions(
      design = d,
      crossed = list(n_total = 100),
      constant = list(
        p_alloc = list(c(0.5, 0.5)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0,
        analysis_at = c(50, 100)
      )
    )
  )
})

# =============================================================================
# build_conditions() - ANALYSIS_AT PROCESSING
# =============================================================================

test_that("build_conditions converts proportional analysis_at to integers", {
  d <- mock_design(trial_type = "group_sequential")
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = 200),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0,
      analysis_at = c(0.5, 1.0)
    )
  )

  analysis_at <- cond@params_by_cond[[1]]$analysis_args$analysis_at
  expect_equal(analysis_at, c(100L, 200L))
})

test_that("build_conditions auto-appends n_total to analysis_at", {
  d <- mock_design(trial_type = "group_sequential")
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = 200),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0,
      analysis_at = c(100)
    )
  )

  analysis_at <- cond@params_by_cond[[1]]$analysis_args$analysis_at
  expect_equal(analysis_at[length(analysis_at)], 200L)
})

test_that("build_conditions rejects analysis_at exceeding n_total", {
  d <- mock_design(trial_type = "group_sequential")
  expect_cli_abort(
    build_conditions(
      design = d,
      crossed = list(n_total = 100),
      constant = list(
        p_alloc = list(c(0.5, 0.5)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0,
        analysis_at = c(50, 150)
      )
    )
  )
})

# =============================================================================
# build_conditions() - BOUNDARY FUNCTION RESOLUTION
# =============================================================================

test_that("build_conditions pre-resolves boundary functions", {
  d <- mock_design(trial_type = "group_sequential")
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = 200),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
      thr_dec_eff = boundary_obf(threshold = 0.95),
      thr_dec_fut = boundary_linear(0.30, 0.50),
      thr_fx_eff = 0.2, thr_fx_fut = 0,
      analysis_at = c(100, 200)
    )
  )

  eff <- cond@params_by_cond[[1]]$analysis_args$thr_dec_eff
  fut <- cond@params_by_cond[[1]]$analysis_args$thr_dec_fut

  # Should be numeric vectors, not functions

  expect_type(eff, "double")
  expect_type(fut, "double")
  expect_length(eff, 2)
  expect_length(fut, 2)

  # OBF: first threshold > second (conservative early)
  expect_true(eff[1] > eff[2])
  # Linear: second threshold > first (ascending)
  expect_true(fut[2] > fut[1])
})

# =============================================================================
# build_conditions() - DEPRECATED PARAMETERS
# =============================================================================

test_that("build_conditions maps condition_values to crossed with message", {
  d <- mock_design()
  expect_message(
    cond <- build_conditions(
      design = d,
      condition_values = list(n_total = c(100, 200)),
      constant = list(
        p_alloc = list(c(0.5, 0.5)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0
      )
    ),
    "deprecated"
  )
  expect_equal(nrow(cond@grid), 2)
})

test_that("build_conditions rejects linked parameter", {
  d <- mock_design()
  expect_cli_abort(
    build_conditions(
      design = d,
      crossed = list(n_total = c(100, 200)),
      constant = list(
        p_alloc = list(c(0.5, 0.5)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0
      ),
      linked = list()
    )
  )
})

# =============================================================================
# build_conditions() - SINGLE-LEVEL WARNING
# =============================================================================

test_that("build_conditions warns about single-level crossed params", {
  d <- mock_design()
  expect_cli_warn(
    build_conditions(
      design = d,
      crossed = list(n_total = 100),
      constant = list(
        p_alloc = list(c(0.5, 0.5)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0
      )
    )
  )
})

# =============================================================================
# build_conditions() - INVERTED ROPE WARNING
# =============================================================================

test_that("build_conditions warns about inverted ROPE boundaries", {
  d <- mock_design()
  expect_cli_warn(
    build_conditions(
      design = d,
      crossed = list(n_total = c(100, 200)),
      constant = list(
        p_alloc = list(c(0.5, 0.5)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0, thr_fx_fut = 0.2  # inverted: eff <= fut
      )
    )
  )
})

# =============================================================================
# rctbp_conditions VALIDATOR
# =============================================================================

test_that("rctbp_conditions validates grid has rows", {
  d <- mock_design()
  expect_error(
    rctbp_conditions(
      grid = data.frame(),
      params_by_cond = list(),
      design = d,
      crossed = list(),
      constant = list()
    ),
    "grid"
  )
})

test_that("rctbp_conditions validates params_by_cond matches grid", {
  d <- mock_design()
  expect_error(
    rctbp_conditions(
      grid = data.frame(id_cond = 1:2),
      params_by_cond = list(list()),  # Only 1, grid has 2
      design = d,
      crossed = list(),
      constant = list()
    ),
    "params_by_cond"
  )
})

test_that("rctbp_conditions validates design type", {
  expect_error(
    rctbp_conditions(
      grid = data.frame(id_cond = 1),
      params_by_cond = list(list()),
      design = "not a design",
      crossed = list(),
      constant = list()
    ),
    "design"
  )
})

# =============================================================================
# build_conditions() - ACCRUAL PARAMETERS
# =============================================================================

test_that("build_conditions stores accrual params in analysis_args", {
  d <- mock_design()
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = c(100, 200)),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0,
      accrual_rate = 5,
      followup_time = 4
    )
  )

  args1 <- cond@params_by_cond[[1]]$analysis_args
  expect_equal(args1$accrual_rate, 5)
  expect_equal(args1$followup_time, 4)
  expect_equal(args1$accrual_pattern, "uniform")
  expect_equal(args1$analysis_timing, "sample_size")
  expect_null(args1$calendar_analysis_at)
})

test_that("build_conditions accrual params work in crossed", {
  d <- mock_design()
  cond <- build_conditions(
    design = d,
    crossed = list(
      n_total = c(100, 200),
      accrual_rate = c(5, 10)
    ),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0
    )
  )

  # 2 x 2 = 4 conditions
  expect_equal(nrow(cond@grid), 4)
  # First condition has accrual_rate = 5
  expect_equal(cond@params_by_cond[[1]]$analysis_args$accrual_rate, 5)
})

test_that("build_conditions uses accrual defaults when not specified", {
  d <- mock_design()
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = c(100, 200)),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0
    )
  )

  args1 <- cond@params_by_cond[[1]]$analysis_args
  # Defaults applied
  expect_null(args1$accrual_rate)
  expect_equal(args1$accrual_pattern, "uniform")
  expect_equal(args1$followup_time, 0)
  expect_equal(args1$analysis_timing, "sample_size")
  expect_null(args1$calendar_analysis_at)
})

test_that("build_conditions validates bad accrual_rate", {
  d <- mock_design()
  expect_cli_abort(
    build_conditions(
      design = d,
      crossed = list(n_total = c(100, 200)),
      constant = list(
        p_alloc = list(c(0.5, 0.5)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0,
        accrual_rate = -1
      )
    )
  )
})

test_that("build_conditions validates calendar timing requires accrual_rate", {
  d <- mock_design()
  expect_cli_abort(
    build_conditions(
      design = d,
      crossed = list(n_total = c(100, 200)),
      constant = list(
        p_alloc = list(c(0.5, 0.5)),
        b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0,
        analysis_timing = "calendar",
        calendar_analysis_at = c(12, 24)
      )
    )
  )
})

test_that("build_conditions converts calendar timing to analysis_at", {
  d <- mock_design(trial_type = "group_sequential")
  # 200 patients, accrual_rate=10 -> enrollment takes ~20 months
  # followup_time=3 -> first patient analyzable at t=3
  # calendar_analysis_at=c(12, 24) -> compute approx n analyzable at those times
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = 200),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.3, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0,
      accrual_rate = 10,
      followup_time = 3,
      analysis_timing = "calendar",
      calendar_analysis_at = c(12, 24)
    )
  )

  analysis_at <- cond@params_by_cond[[1]]$analysis_args$analysis_at
  # Should be integer vector derived from calendar times
  expect_true(is.integer(analysis_at))
  # At t=12, ~90 patients analyzable (enrolled by t=9, need 3mo followup)
  # At t=24, all 200 analyzable
  expect_true(analysis_at[1] > 0 && analysis_at[1] < 200)
  expect_equal(analysis_at[length(analysis_at)], 200L)
  # Should be strictly increasing
  expect_false(is.unsorted(analysis_at, strictly = TRUE))
})

test_that("build_conditions warns when calendar times collapse to same sample size", {
  d <- mock_design(trial_type = "group_sequential")
  # Two very close calendar times that map to the same n_analyzable
  # Use n_total in constant to avoid the single-level-crossed warning
  warnings <- testthat::capture_warnings(
    build_conditions(
      design = d,
      crossed = list(b_arm_treat = c(0.2, 0.4)),
      constant = list(
        n_total = 200,
        p_alloc = list(c(0.5, 0.5)),
        intercept = 0, b_covariate = 0.3, sigma = 1,
        thr_dec_eff = 0.975, thr_dec_fut = 0.5,
        thr_fx_eff = 0.2, thr_fx_fut = 0,
        accrual_rate = 10,
        followup_time = 3,
        analysis_timing = "calendar",
        calendar_analysis_at = c(3.0, 3.05, 24)
      )
    )
  )
  expect_true(any(grepl("same sample size", warnings)))
})

# =============================================================================
# PRINT METHOD
# =============================================================================

test_that("print.rctbp_conditions produces output without error", {
  cond <- mock_conditions()
  output <- capture_cli(print(cond))
  expect_true(length(output) > 0)
})
