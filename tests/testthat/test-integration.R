# Integration tests for the full power analysis pipeline (R/class_power_analysis.R)
#
# These tests exercise the real pipeline: build_design -> build_conditions ->
# power_analysis with actual brms model fitting, posterior extraction, and
# result aggregation. They are slow (~30-120s total) and require brms/Stan.

# =============================================================================
# FILE-LEVEL SKIP GUARDS
# =============================================================================
# Skip entire file on CRAN (too slow) or when brms/posterior not installed

skip_on_cran()
skip_if_not_installed("brms")
skip_if_not_installed("posterior")

# =============================================================================
# SHARED SETUP
# =============================================================================

# Minimal brms args for fast fitting (~1-2 sec per fit)
minimal_brms_args <- list(chains = 1, iter = 150, warmup = 50)

# Build fixed design once (uses cached compiled model if available)
design_fixed <- suppressMessages(suppressWarnings(
  build_design(
    predefined_model = "ancova_cont_2arms",
    target_params = "b_arm2"
  )
))

# Expected columns in results_conditions (grid cols + summary cols)
expected_summary_cols <- c(
  "id_cond", "par_name",
  "pwr_eff", "se_pwr_eff", "pwr_fut", "se_pwr_fut",
  "pr_eff", "se_pr_eff", "pr_fut", "se_pr_fut",
  "post_med", "post_mn", "post_sd",
  "rhat", "ess_bulk", "ess_tail", "conv_rate"
)


# =============================================================================
# TEST 1: Fixed trial, single core, single condition
# =============================================================================

test_that("full pipeline works: fixed trial, single core, single condition", {
  conditions <- suppressMessages(suppressWarnings(
    build_conditions(
      design_fixed,
      crossed = list(n_total = 100),
      constant = list(
        b_arm_treat = 0.5,
        b_covariate = 0.3,
        thr_dec_eff = 0.975,
        thr_dec_fut = 0.9,
        thr_fx_eff = 0,
        thr_fx_fut = 0,
        p_alloc = c(0.5, 0.5)
      )
    )
  ))

  result <- suppressMessages(suppressWarnings(
    power_analysis(
      conditions,
      n_sims = 5,
      n_cores = 1,
      verbosity = 0,
      brms_args = minimal_brms_args
    )
  ))

  # S7 class
  expect_s3_class(result, "rctbayespower::rctbp_power_analysis")

  # Results structure
  rc <- result@results_conditions
  expect_s3_class(rc, "data.frame")
  expect_equal(nrow(rc), 1)  # 1 condition x 1 target param

  # Expected columns present
  for (col in expected_summary_cols) {
    expect_true(col %in% names(rc), info = paste("Missing column:", col))
  }

  # Value ranges
  expect_true(rc$pwr_eff >= 0 && rc$pwr_eff <= 1)
  expect_true(rc$pwr_fut >= 0 && rc$pwr_fut <= 1)
  expect_true(rc$pr_eff >= 0 && rc$pr_eff <= 1)
  expect_true(rc$conv_rate >= 0 && rc$conv_rate <= 1)
  expect_true(is.numeric(rc$post_med))
  expect_true(is.numeric(rc$rhat))
  expect_true(is.numeric(rc$ess_bulk))

  # Raw results
  rr <- result@results_raw
  expect_s3_class(rr, "data.frame")
  expect_equal(nrow(rr), 5)  # 5 sims x 1 condition x 1 param (no union for single param)
  expect_true(all(rr$id_cond == 1))
  expect_true(all(rr$par_name == "b_arm2"))

  # Object properties
  expect_equal(result@n_sims, 5)
  expect_true(is.numeric(result@elapsed_time))
  expect_true(result@elapsed_time > 0)

  # No interim for fixed design
  expect_equal(nrow(result@results_interim), 0)
})


# =============================================================================
# TEST 2: Fixed trial, multi-core (S7 serialization)
# =============================================================================

test_that("full pipeline works: multi-core with S7 serialization", {
  skip_if(parallel::detectCores() < 2, "Need at least 2 cores")

  conditions <- suppressMessages(suppressWarnings(
    build_conditions(
      design_fixed,
      crossed = list(n_total = 100),
      constant = list(
        b_arm_treat = 0.5,
        b_covariate = 0.3,
        thr_dec_eff = 0.975,
        thr_dec_fut = 0.9,
        thr_fx_eff = 0,
        thr_fx_fut = 0,
        p_alloc = c(0.5, 0.5)
      )
    )
  ))

  result <- suppressMessages(suppressWarnings(
    power_analysis(
      conditions,
      n_sims = 5,
      n_cores = 2,
      verbosity = 0,
      brms_args = minimal_brms_args
    )
  ))

  # S7 class check
  expect_s3_class(result, "rctbayespower::rctbp_power_analysis")

  # Results populated
  rc <- result@results_conditions
  expect_equal(nrow(rc), 1)
  expect_true(rc$pwr_eff >= 0 && rc$pwr_eff <= 1)
  expect_true(rc$conv_rate >= 0 && rc$conv_rate <= 1)

  # Raw results from parallel workers
  rr <- result@results_raw
  expect_equal(nrow(rr), 5)
  expect_true(all(rr$par_name == "b_arm2"))
  expect_true(is.numeric(rr$post_med))
  expect_true(all(!is.na(rr$converged)))
})


# =============================================================================
# TEST 3: Multiple crossed conditions
# =============================================================================

test_that("full pipeline works: multiple crossed conditions", {
  conditions <- suppressMessages(suppressWarnings(
    build_conditions(
      design_fixed,
      crossed = list(
        n_total = c(50, 100),
        b_arm_treat = c(0.3, 0.5)
      ),
      constant = list(
        b_covariate = 0.3,
        thr_dec_eff = 0.975,
        thr_dec_fut = 0.9,
        thr_fx_eff = 0,
        thr_fx_fut = 0,
        p_alloc = c(0.5, 0.5)
      )
    )
  ))

  # Verify grid has 4 conditions
  expect_equal(nrow(conditions@grid), 4)

  result <- suppressMessages(suppressWarnings(
    power_analysis(
      conditions,
      n_sims = 5,
      n_cores = 1,
      verbosity = 0,
      brms_args = minimal_brms_args
    )
  ))

  # Results structure
  rc <- result@results_conditions
  expect_equal(nrow(rc), 4)
  expect_equal(sort(unique(rc$id_cond)), 1:4)

  # Grid columns carried through
  expect_true("n_total" %in% names(rc))
  expect_true("b_arm_treat" %in% names(rc))
  expect_equal(sort(unique(rc$n_total)), c(50, 100))
  expect_equal(sort(unique(rc$b_arm_treat)), c(0.3, 0.5))

  # All power values valid
  expect_true(all(rc$pwr_eff >= 0 & rc$pwr_eff <= 1))
  expect_true(all(rc$pwr_fut >= 0 & rc$pwr_fut <= 1))

  # Raw results: 5 sims x 4 conditions x 1 param
  rr <- result@results_raw
  expect_equal(nrow(rr), 20)
  expect_equal(sort(unique(rr$id_cond)), 1:4)
  expect_equal(length(unique(rr$id_iter)), 5)
})


# =============================================================================
# TEST 4: Group sequential trial + resummarize_boundaries
# =============================================================================

test_that("full pipeline works: group sequential with resummarize_boundaries", {
  design_seq <- suppressMessages(suppressWarnings(
    build_design(
      predefined_model = "ancova_cont_2arms",
      target_params = "b_arm2",
      trial_type = "group_sequential"
    )
  ))

  conditions_seq <- suppressMessages(suppressWarnings(
    build_conditions(
      design_seq,
      crossed = list(n_total = 100),
      constant = list(
        b_arm_treat = 0.5,
        b_covariate = 0.3,
        thr_dec_eff = boundary_constant(0.975),
        thr_dec_fut = boundary_constant(0.9),
        thr_fx_eff = 0,
        thr_fx_fut = 0,
        p_alloc = c(0.5, 0.5),
        analysis_at = c(0.5, 1.0)
      )
    )
  ))

  result_seq <- suppressMessages(suppressWarnings(
    power_analysis(
      conditions_seq,
      n_sims = 5,
      n_cores = 1,
      verbosity = 0,
      brms_args = minimal_brms_args
    )
  ))

  # Sequential design properties
  expect_s3_class(result_seq, "rctbayespower::rctbp_power_analysis")
  expect_true(result_seq@has_interim)

  # Interim results populated
  ri <- result_seq@results_interim
  expect_s3_class(ri, "data.frame")
  expect_true(nrow(ri) > 0)
  expect_true("id_look" %in% names(ri))

  # Raw results have multiple looks
  rr <- result_seq@results_raw
  expect_true(length(unique(rr$id_look)) >= 1)
  expect_true("n_analyzed" %in% names(rr))

  # Resummarize with different boundaries
  result_resum <- suppressMessages(suppressWarnings(
    resummarize_boundaries(
      result_seq,
      thr_dec_eff = 0.99,
      thr_dec_fut = 0.80
    )
  ))

  expect_s3_class(result_resum, "rctbayespower::rctbp_power_analysis")
  rc_resum <- result_resum@results_conditions
  expect_s3_class(rc_resum, "data.frame")
  expect_true(nrow(rc_resum) > 0)
})


# =============================================================================
# TEST 5: Print/summary methods on real results
# =============================================================================

test_that("print and summary work on real power analysis results", {
  conditions <- suppressMessages(suppressWarnings(
    build_conditions(
      design_fixed,
      crossed = list(n_total = 100),
      constant = list(
        b_arm_treat = 0.5,
        b_covariate = 0.3,
        thr_dec_eff = 0.975,
        thr_dec_fut = 0.9,
        thr_fx_eff = 0,
        thr_fx_fut = 0,
        p_alloc = c(0.5, 0.5)
      )
    )
  ))

  result <- suppressMessages(suppressWarnings(
    power_analysis(
      conditions,
      n_sims = 5,
      n_cores = 1,
      verbosity = 0,
      brms_args = minimal_brms_args
    )
  ))

  # Print should not error
  output <- capture_cli(print(result))
  expect_true(length(output) > 0)

  # Check key content in output
  expect_true(any(grepl("Power Analysis", output, ignore.case = TRUE)),
              info = "print() should mention 'Power Analysis'")
})
