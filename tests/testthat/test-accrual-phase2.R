# Tests for Phase 2 accrual features: print/summary trial duration,
# accrual plot, and enrolled-vs-analyzed display.

# Helper to create a mock power analysis object with accrual data
mock_pa_with_accrual <- function(n_conditions = 2, n_looks = 3) {
  d <- mock_design(trial_type = "group_sequential")

  # Build a minimal conditions object
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = if (n_conditions == 1) 200 else c(100, 200)),
    constant = list(
      p_alloc = c(0.5, 0.5),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.2, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0,
      analysis_at = if (n_looks == 3) c(50, 75, 100) else c(50, 100),
      accrual_rate = 10,
      followup_time = 3
    )
  )

  pa <- power_analysis(cond, n_sims = 10, run = FALSE)

  # Populate results_interim with accrual columns
  n_params <- 1
  rows <- n_conditions * n_looks * n_params

  results_interim <- data.frame(
    id_cond = rep(seq_len(n_conditions), each = n_looks * n_params),
    id_look = rep(rep(seq_len(n_looks), each = n_params), n_conditions),
    n_total = rep(if (n_conditions == 1) 200 else c(100, 200), each = n_looks * n_params),
    par_name = rep("b_arm2", rows),
    n_analyzed = rep(rep(c(50, 75, 100)[seq_len(n_looks)], each = n_params), n_conditions),
    pwr_eff = runif(rows, 0.3, 0.9),
    pwr_fut = runif(rows, 0.1, 0.3),
    pr_eff = runif(rows, 0.5, 0.95),
    pr_fut = runif(rows, 0.1, 0.4),
    se_pwr_eff = runif(rows, 0.01, 0.05),
    se_pwr_fut = runif(rows, 0.01, 0.05),
    se_pr_eff = runif(rows, 0.01, 0.05),
    se_pr_fut = runif(rows, 0.01, 0.05),
    post_mn = rnorm(rows, 0.3, 0.1),
    post_sd = runif(rows, 0.05, 0.15),
    rhat = runif(rows, 1.0, 1.01),
    ess_bulk = runif(rows, 800, 1200),
    conv_rate = rep(1, rows),
    # Accrual columns
    calendar_time_mn = rep(rep(c(8, 11, 13)[seq_len(n_looks)], each = n_params), n_conditions),
    calendar_time_mdn = rep(rep(c(7.8, 10.5, 12.8)[seq_len(n_looks)], each = n_params), n_conditions),
    n_enrolled_mn = rep(rep(c(80, 100, 100)[seq_len(n_looks)], each = n_params), n_conditions),
    # Per-look stopping columns
    prop_stp_look = runif(rows, 0, 0.3),
    prop_eff_look = runif(rows, 0, 0.2),
    prop_fut_look = runif(rows, 0, 0.1),
    cumul_stp = rep(cumsum(runif(n_looks, 0, 0.2))[seq_len(n_looks)], n_conditions * n_params),
    stringsAsFactors = FALSE
  )

  # Populate results_conditions with accrual overall stats
  results_conditions <- data.frame(
    id_cond = seq_len(n_conditions),
    n_total = if (n_conditions == 1) 200 else c(100, 200),
    n_planned = if (n_conditions == 1) 200 else c(100, 200),
    n_mn = if (n_conditions == 1) 180 else c(85, 180),
    se_n_mn = rep(2, n_conditions),
    n_mdn = if (n_conditions == 1) 175 else c(80, 175),
    n_mode = if (n_conditions == 1) 100 else c(50, 100),
    prop_at_mode = rep(0.4, n_conditions),
    prop_stp_early = rep(0.3, n_conditions),
    prop_stp_eff = rep(0.2, n_conditions),
    prop_stp_fut = rep(0.1, n_conditions),
    prop_no_dec = rep(0.05, n_conditions),
    # Accrual overall stats
    trial_dur_mn = if (n_conditions == 1) 13 else c(10, 13),
    trial_dur_mdn = if (n_conditions == 1) 12.5 else c(9.5, 12.5),
    enrollment_dur_mn = if (n_conditions == 1) 10 else c(5, 10),
    stringsAsFactors = FALSE
  )

  pa@results_conditions <- results_conditions
  pa@results_interim <- results_interim
  pa@results_raw <- data.frame(id_cond = 1L)
  pa@elapsed_time <- 1.5

  pa
}


# Helper to create a mock PA without accrual (sequential, but no accrual columns)
mock_pa_no_accrual <- function() {
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
  rows <- n_conditions * n_looks

  pa@results_interim <- data.frame(
    id_cond = rep(1:2, each = n_looks),
    id_look = rep(1:2, n_conditions),
    n_total = rep(c(100, 200), each = n_looks),
    par_name = rep("b_arm2", rows),
    n_analyzed = rep(c(50, 100), n_conditions),
    pwr_eff = runif(rows, 0.3, 0.9),
    pwr_fut = runif(rows, 0.1, 0.3),
    pr_eff = runif(rows, 0.5, 0.95),
    pr_fut = runif(rows, 0.1, 0.4),
    se_pwr_eff = runif(rows, 0.01, 0.05),
    se_pwr_fut = runif(rows, 0.01, 0.05),
    se_pr_eff = runif(rows, 0.01, 0.05),
    se_pr_fut = runif(rows, 0.01, 0.05),
    post_mn = rnorm(rows, 0.3, 0.1),
    post_sd = runif(rows, 0.05, 0.15),
    rhat = runif(rows, 1.0, 1.01),
    ess_bulk = runif(rows, 800, 1200),
    conv_rate = rep(1, rows),
    prop_stp_look = runif(rows, 0, 0.3),
    prop_eff_look = runif(rows, 0, 0.2),
    prop_fut_look = runif(rows, 0, 0.1),
    cumul_stp = rep(cumsum(runif(n_looks, 0, 0.2)), n_conditions),
    stringsAsFactors = FALSE
  )

  pa@results_conditions <- data.frame(
    id_cond = 1:2,
    n_total = c(100, 200),
    n_planned = c(100, 200),
    n_mn = c(85, 180),
    se_n_mn = c(2, 2),
    n_mdn = c(80, 175),
    n_mode = c(50, 100),
    prop_at_mode = c(0.4, 0.4),
    prop_stp_early = c(0.3, 0.3),
    prop_stp_eff = c(0.2, 0.2),
    prop_stp_fut = c(0.1, 0.1),
    prop_no_dec = c(0.05, 0.05),
    stringsAsFactors = FALSE
  )

  pa@results_raw <- data.frame(id_cond = 1L)
  pa@elapsed_time <- 1.0

  pa
}


# =============================================================================
# Print: Trial Duration section
# =============================================================================

test_that("print shows Trial Duration when accrual data is present", {
  pa <- mock_pa_with_accrual()
  output <- capture_cli(print(pa))
  combined <- paste(output, collapse = "\n")

  expect_true(grepl("Trial Duration", combined))
  expect_true(grepl("Mean duration", combined))
  expect_true(grepl("Enrollment phase", combined))
})

test_that("print does NOT show Trial Duration without accrual data", {
  pa <- mock_pa_no_accrual()
  output <- capture_cli(print(pa))
  combined <- paste(output, collapse = "\n")

  expect_false(grepl("Trial Duration", combined))
})


# =============================================================================
# Summary: Trial Duration section
# =============================================================================

test_that("summary shows Trial Duration when accrual data is present", {
  pa <- mock_pa_with_accrual()
  output <- capture_cli(summary(pa))
  combined <- paste(output, collapse = "\n")

  expect_true(grepl("Trial Duration", combined))
  expect_true(grepl("Mean trial duration", combined))
  expect_true(grepl("Median trial duration", combined))
  expect_true(grepl("enrollment duration", combined, ignore.case = TRUE))
})

test_that("summary does NOT show Trial Duration without accrual data", {
  pa <- mock_pa_no_accrual()
  output <- capture_cli(summary(pa))
  combined <- paste(output, collapse = "\n")

  expect_false(grepl("Trial Duration", combined))
})

test_that("summary All Conditions table includes trial_dur_mn when accrual present", {
  pa <- mock_pa_with_accrual()
  output <- capture_cli(summary(pa))
  combined <- paste(output, collapse = "\n")

  expect_true(grepl("trial_dur_mn", combined))
})


# =============================================================================
# Plot: type = "accrual"
# =============================================================================

test_that("plot(type='accrual') produces a ggplot with accrual data", {
  pa <- mock_pa_with_accrual()
  p <- plot(pa, type = "accrual", interactive = FALSE)

  expect_s3_class(p, "ggplot")
  # Force rendering to catch aes/data issues
  expect_no_error(ggplot2::ggplot_build(p))
  # Check the plot has the expected layers
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomLine" %in% layer_types)
  expect_true("GeomPoint" %in% layer_types)
  expect_true("GeomVline" %in% layer_types)
})

test_that("plot(type='accrual') errors without interim data", {
  # Create a single-look PA (no interim)
  d <- mock_design()
  cond <- build_conditions(
    design = d,
    crossed = list(n_total = 100),
    constant = list(
      p_alloc = c(0.5, 0.5),
      b_arm_treat = 0.3, intercept = 0, b_covariate = 0.2, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0
    )
  )
  pa <- power_analysis(cond, n_sims = 10, run = FALSE)
  pa@results_conditions <- data.frame(
    id_cond = 1L, n_total = 100,
    pwr_eff = 0.8, pwr_fut = 0.1,
    pr_eff = 0.9, pr_fut = 0.15,
    se_pwr_eff = 0.02, se_pwr_fut = 0.01,
    se_pr_eff = 0.02, se_pr_fut = 0.01,
    par_name = "b_arm2",
    stringsAsFactors = FALSE
  )
  pa@results_raw <- data.frame(id_cond = 1L)
  pa@elapsed_time <- 0.5

  expect_error(
    plot(pa, type = "accrual", interactive = FALSE),
    "sequential design"
  )
})

test_that("plot(type='accrual') errors without accrual columns", {
  pa <- mock_pa_no_accrual()

  expect_error(
    plot(pa, type = "accrual", interactive = FALSE),
    "accrual modeling"
  )
})

test_that("plot(type='accrual') works with single condition (no faceting)", {
  pa <- mock_pa_with_accrual(n_conditions = 1)
  p <- plot(pa, type = "accrual", interactive = FALSE)

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  # Single condition should not facet
  expect_equal(class(p$facet)[1], "FacetNull")
})

test_that("plot(type='accrual') facets with multiple conditions", {
  pa <- mock_pa_with_accrual(n_conditions = 2)
  p <- plot(pa, type = "accrual", interactive = FALSE)

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_equal(class(p$facet)[1], "FacetWrap")
})

test_that("plot(type='accrual') with interactive=TRUE returns plotly", {
  pa <- mock_pa_with_accrual()
  p <- plot(pa, type = "accrual", interactive = TRUE)

  expect_s3_class(p, "plotly")
})

test_that("plot(type='accrual') errors when calendar_time_mn is all NA", {
  pa <- mock_pa_with_accrual()
  pa@results_interim$calendar_time_mn <- NA_real_

  expect_error(
    plot(pa, type = "accrual", interactive = FALSE),
    "accrual modeling"
  )
})


# =============================================================================
# Print/summary edge cases
# =============================================================================

test_that("print handles partial accrual metadata (no enrollment_dur_mn)", {
  pa <- mock_pa_with_accrual()
  pa@results_conditions$enrollment_dur_mn <- NULL

  output <- capture_cli(print(pa))
  combined <- paste(output, collapse = "\n")

  # Trial Duration section should still appear

  expect_true(grepl("Trial Duration", combined))
  expect_true(grepl("Mean duration", combined))
  # But enrollment phase should be absent
  expect_false(grepl("Enrollment phase", combined))
})

test_that("summary handles partial accrual metadata (no trial_dur_mdn)", {
  pa <- mock_pa_with_accrual()
  pa@results_conditions$trial_dur_mdn <- NULL

  output <- capture_cli(summary(pa))
  combined <- paste(output, collapse = "\n")

  expect_true(grepl("Trial Duration", combined))
  expect_true(grepl("Mean trial duration", combined))
  expect_false(grepl("Median trial duration", combined))
})

test_that("print shows correct numeric values for trial duration", {
  pa <- mock_pa_with_accrual()
  output <- capture_cli(print(pa))
  combined <- paste(output, collapse = "\n")

  # Mock has trial_dur_mn of 10 and 13 → range "10-13"
  expect_true(grepl("10-13", combined))
  # Enrollment phase: 5-10
  expect_true(grepl("5-10", combined))
})
