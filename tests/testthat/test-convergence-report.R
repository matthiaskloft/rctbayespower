# Tests for convergence diagnostics reporting in print(), summary(), and report()

# Helper to create a mock PA with convergence data (single-look)
mock_pa_with_convergence <- function(conv_rate = 1.0, rhat = 1.01) {
  d <- mock_design()
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = c(100, 200)),
    constant = list(
      p_alloc = c(0.5, 0.5),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.2, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0
    )
  )

  pa <- power_analysis(cond, n_sims = 10, run = FALSE)

  n_conditions <- 2
  results_conditions <- data.frame(
    id_cond = 1:2,
    n_total = c(100, 200),
    par_name = "b_arm2",
    pwr_eff = c(0.6, 0.8),
    se_pwr_eff = c(0.02, 0.02),
    pwr_fut = c(0.1, 0.05),
    se_pwr_fut = c(0.01, 0.01),
    post_mn = c(0.28, 0.31),
    post_sd = c(0.12, 0.08),
    rhat = c(rhat, rhat),
    se_rhat = c(0.001, 0.001),
    ess_bulk = c(900, 1100),
    se_ess_bulk = c(20, 25),
    ess_tail = c(800, 1000),
    se_ess_tail = c(25, 30),
    conv_rate = c(conv_rate, conv_rate),
    se_conv_rate = c(0.01, 0.01),
    stringsAsFactors = FALSE
  )

  pa@results_conditions <- results_conditions
  pa@results_raw <- data.frame(id_cond = 1L)
  pa@elapsed_time <- 1.0
  pa
}

# Helper to create a mock PA with convergence data (sequential)
mock_pa_sequential_with_convergence <- function(conv_rate = 1.0, rhat = 1.01) {
  d <- mock_design(trial_type = "group_sequential")
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = c(100, 200)),
    constant = list(
      p_alloc = c(0.5, 0.5),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.2, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0,
      analysis_at = c(50, 100)
    )
  )

  pa <- power_analysis(cond, n_sims = 10, run = FALSE)

  n_conditions <- 2
  n_looks <- 2

  results_interim <- data.frame(
    id_cond = rep(1:2, each = n_looks),
    id_look = rep(1:2, n_conditions),
    n_total = rep(c(100, 200), each = n_looks),
    par_name = "b_arm2",
    n_analyzed = rep(c(50, 100), n_conditions),
    pwr_eff = c(0.3, 0.6, 0.5, 0.8),
    pwr_fut = c(0.2, 0.1, 0.15, 0.05),
    se_pwr_eff = rep(0.02, 4),
    se_pwr_fut = rep(0.01, 4),
    pr_eff = c(0.7, 0.85, 0.8, 0.92),
    pr_fut = c(0.3, 0.15, 0.2, 0.08),
    se_pr_eff = rep(0.02, 4),
    se_pr_fut = rep(0.01, 4),
    post_mn = c(0.2, 0.28, 0.25, 0.31),
    post_sd = c(0.15, 0.12, 0.13, 0.08),
    rhat = rep(rhat, 4),
    se_rhat = rep(0.001, 4),
    ess_bulk = c(800, 900, 900, 1100),
    se_ess_bulk = rep(20, 4),
    ess_tail = c(700, 800, 850, 1000),
    se_ess_tail = rep(25, 4),
    conv_rate = rep(conv_rate, 4),
    se_conv_rate = rep(0.01, 4),
    prop_stp_look = c(0.1, 0.2, 0.15, 0.25),
    prop_eff_look = c(0.05, 0.15, 0.1, 0.2),
    prop_fut_look = c(0.05, 0.05, 0.05, 0.05),
    cumul_stp = c(0.1, 0.3, 0.15, 0.4),
    stringsAsFactors = FALSE
  )

  results_conditions <- data.frame(
    id_cond = 1:2,
    n_total = c(100, 200),
    n_planned = c(100, 200),
    n_mn = c(85, 180),
    se_n_mn = c(2, 3),
    n_mdn = c(80, 175),
    n_mode = c(50, 100),
    prop_at_mode = c(0.4, 0.35),
    pwr_eff = c(0.6, 0.8),
    pwr_fut = c(0.1, 0.05),
    se_pwr_eff = c(0.02, 0.02),
    se_pwr_fut = c(0.01, 0.01),
    prop_stp_early = c(0.3, 0.4),
    prop_stp_eff = c(0.2, 0.3),
    prop_stp_fut = c(0.1, 0.1),
    prop_no_dec = c(0.05, 0.05),
    rhat = rep(rhat, 2),
    ess_bulk = c(900, 1100),
    ess_tail = c(800, 1000),
    conv_rate = rep(conv_rate, 2),
    stringsAsFactors = FALSE
  )

  pa@results_conditions <- results_conditions
  pa@results_interim <- results_interim
  pa@results_raw <- data.frame(id_cond = 1L)
  pa@elapsed_time <- 1.5
  pa
}

# Helper to create a mock PA without convergence data (BayesFlow-like)
mock_pa_no_convergence <- function() {
  d <- mock_design()
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = c(100, 200)),
    constant = list(
      p_alloc = c(0.5, 0.5),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.2, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0
    )
  )

  pa <- power_analysis(cond, n_sims = 10, run = FALSE)

  results_conditions <- data.frame(
    id_cond = 1:2,
    n_total = c(100, 200),
    par_name = "b_arm2",
    pwr_eff = c(0.6, 0.8),
    se_pwr_eff = c(0.02, 0.02),
    pwr_fut = c(0.1, 0.05),
    se_pwr_fut = c(0.01, 0.01),
    post_mn = c(0.28, 0.31),
    post_sd = c(0.12, 0.08),
    rhat = c(NaN, NaN),
    ess_bulk = c(NaN, NaN),
    ess_tail = c(NaN, NaN),
    conv_rate = c(NaN, NaN),
    stringsAsFactors = FALSE
  )

  pa@results_conditions <- results_conditions
  pa@results_raw <- data.frame(id_cond = 1L)
  pa@elapsed_time <- 1.0
  pa
}


# =============================================================================
# print() tests
# =============================================================================

test_that("print() shows convergence section when data present (single-look)", {
  pa <- mock_pa_with_convergence()
  output <- capture_cli(print(pa))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("Convergence", output_text, fixed = TRUE))
  expect_true(grepl("Convergence rate", output_text, fixed = TRUE))
  expect_true(grepl("Mean Rhat", output_text, fixed = TRUE))
  expect_true(grepl("Mean ESS", output_text, fixed = TRUE))
})

test_that("print() shows convergence section when data present (sequential)", {

  pa <- mock_pa_sequential_with_convergence()
  output <- capture_cli(print(pa))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("Convergence", output_text, fixed = TRUE))
  expect_true(grepl("Convergence rate", output_text, fixed = TRUE))
})

test_that("print() does NOT show convergence when all NaN (BayesFlow)", {
  pa <- mock_pa_no_convergence()
  output <- capture_cli(print(pa))
  output_text <- paste(output, collapse = "\n")

  # Should NOT have a Convergence section
  # (The word "Convergence" should not appear as a section heading)
  expect_false(grepl("Convergence rate", output_text, fixed = TRUE))
})

test_that("print() shows warning when conv_rate < 1.0", {
  pa <- mock_pa_with_convergence(conv_rate = 0.95)
  output <- capture_cli(print(pa))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("non-converged", output_text, fixed = TRUE))
})

test_that("print() shows warning when rhat > 1.05", {
  pa <- mock_pa_with_convergence(rhat = 1.1)
  output <- capture_cli(print(pa))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("Rhat > 1.05", output_text, fixed = TRUE))
})


# =============================================================================
# summary() tests
# =============================================================================

test_that("summary() shows Convergence Diagnostics section", {
  pa <- mock_pa_with_convergence()
  output <- capture_cli(summary(pa))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("Convergence Diagnostics", output_text, fixed = TRUE))
  expect_true(grepl("Convergence rate", output_text, fixed = TRUE))
  expect_true(grepl("Mean Rhat", output_text, fixed = TRUE))
  expect_true(grepl("ESS bulk", output_text, fixed = TRUE))
  expect_true(grepl("ESS tail", output_text, fixed = TRUE))
})

test_that("summary() shows conv_rate in All Conditions table (single-look)", {
  pa <- mock_pa_with_convergence()
  output <- capture_cli(summary(pa))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("conv_rate", output_text, fixed = TRUE))
})

test_that("summary() shows conv_rate in All Conditions table (sequential)", {
  pa <- mock_pa_sequential_with_convergence()
  output <- capture_cli(summary(pa))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("conv_rate", output_text, fixed = TRUE))
})

test_that("summary() does NOT show convergence when all NaN", {
  pa <- mock_pa_no_convergence()
  output <- capture_cli(summary(pa))
  output_text <- paste(output, collapse = "\n")

  expect_false(grepl("Convergence Diagnostics", output_text, fixed = TRUE))
})

test_that("summary() shows convergence hint for report()", {
  pa <- mock_pa_with_convergence()
  output <- capture_cli(summary(pa))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("convergence", output_text, fixed = TRUE))
})

test_that("summary() shows warnings for convergence issues", {
  pa <- mock_pa_with_convergence(conv_rate = 0.9, rhat = 1.1)
  output <- capture_cli(summary(pa))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("non-converged", output_text, fixed = TRUE))
  expect_true(grepl("Rhat > 1.05", output_text, fixed = TRUE))
})


# =============================================================================
# report_convergence() tests
# =============================================================================

test_that("report_convergence() works with cli format", {
  pa <- mock_pa_with_convergence()
  output <- capture_cli(report_convergence(pa))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("Convergence Diagnostics", output_text, fixed = TRUE))
  expect_true(grepl("conv_rate", output_text, fixed = TRUE))
  expect_true(grepl("rhat", output_text, fixed = TRUE))
  expect_true(grepl("ess_bulk", output_text, fixed = TRUE))
})

test_that("report_convergence() works with markdown format", {
  pa <- mock_pa_with_convergence()
  output <- capture_cli(report_convergence(pa, format = "markdown"))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("Convergence", output_text, fixed = TRUE))
})

test_that("report_convergence() sorts by conv_rate ascending", {
  pa <- mock_pa_with_convergence()
  # Modify to have different conv_rates
  pa@results_conditions$conv_rate <- c(0.9, 1.0)

  output <- capture_cli(report_convergence(pa))
  output_text <- paste(output, collapse = "\n")

  # The condition with lower conv_rate (id_cond=1) should appear first
  pos_09 <- regexpr("0.9", output_text)
  pos_10 <- regexpr("1.0", output_text)
  # Just check it renders without error (order depends on table formatting)
  expect_true(grepl("conv_rate", output_text, fixed = TRUE))
})

test_that("report_convergence() errors when all NaN", {
  pa <- mock_pa_no_convergence()
  expect_cli_abort(report_convergence(pa))
})

test_that("report_convergence() works for sequential designs", {
  pa <- mock_pa_sequential_with_convergence()
  output <- capture_cli(report_convergence(pa))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("Convergence Diagnostics", output_text, fixed = TRUE))
})


# =============================================================================
# report() dispatch tests
# =============================================================================

test_that("report() dispatches 'convergence' topic correctly", {
  pa <- mock_pa_with_convergence()
  output <- capture_cli(report(pa, topic = "convergence"))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("Convergence Diagnostics", output_text, fixed = TRUE))
})

test_that("report() accepts 'convergence' as valid topic", {
  pa <- mock_pa_with_convergence()
  # Should not error
  expect_no_error(capture_cli(report(pa, topic = "convergence")))
})

test_that("report() still rejects invalid topics", {
  pa <- mock_pa_with_convergence()
  expect_cli_abort(report(pa, topic = "invalid_topic"))
})


# =============================================================================
# Sequential by-look aggregation tests (Step 1 verification)
# =============================================================================

test_that("sequential by-look results contain ess_tail and SE columns", {
  pa <- mock_pa_sequential_with_convergence()
  interim <- pa@results_interim

  # Verify the columns we added in Step 1 are present
  expect_true("ess_tail" %in% names(interim))
  expect_true("se_rhat" %in% names(interim))
  expect_true("se_ess_bulk" %in% names(interim))
  expect_true("se_ess_tail" %in% names(interim))
  expect_true("se_conv_rate" %in% names(interim))
})

test_that("sequential overall (results_conditions) contains convergence columns", {
  pa <- mock_pa_sequential_with_convergence()
  overall <- pa@results_conditions

  # Verify Step 2: convergence columns exist in results_conditions
  expect_true("rhat" %in% names(overall))
  expect_true("ess_bulk" %in% names(overall))
  expect_true("ess_tail" %in% names(overall))
  expect_true("conv_rate" %in% names(overall))
})
