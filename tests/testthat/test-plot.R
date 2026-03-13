# Tests for plot functions
#
# ALL plot() calls use interactive = FALSE to produce ggplot2 objects.

# =============================================================================
# Phase 0: mock_power_analysis() smoke tests
# =============================================================================

test_that("mock_power_analysis variants construct without error", {
  expect_no_error(mock_power_analysis("1d"))
  expect_no_error(mock_power_analysis("2d"))
  expect_no_error(mock_power_analysis("accrual"))
  expect_no_error(mock_power_analysis("accrual_no_cols"))
})

test_that("mock_power_analysis variants have correct structure", {
  mock_1d <- mock_power_analysis("1d")
  expect_true(inherits(mock_1d, "rctbayespower::rctbp_power_analysis") ||
                inherits(mock_1d, "rctbp_power_analysis"))
  expect_equal(nrow(mock_1d@results_conditions), 3)
  expect_false(mock_1d@has_interim)

  mock_2d <- mock_power_analysis("2d")
  expect_equal(nrow(mock_2d@results_conditions), 6)
  expect_true("b_arm_treat" %in% names(mock_2d@conditions@grid))

  mock_accrual <- mock_power_analysis("accrual")
  expect_true(mock_accrual@has_interim)
  expect_true("calendar_time_mn" %in% names(mock_accrual@results_interim))

  mock_no_cols <- mock_power_analysis("accrual_no_cols")
  expect_true(mock_no_cols@has_interim)
  expect_false("calendar_time_mn" %in% names(mock_no_cols@results_interim))
})

# =============================================================================
# Phase 1: Plot helpers
# =============================================================================

test_that("rctbp_colors returns named vector with Efficacy and Futility", {
  colors <- rctbayespower:::rctbp_colors()
  expect_named(colors, c("Efficacy", "Futility"))
  expect_type(colors, "character")
})

test_that("rctbp_theme returns a ggplot2 theme object", {
  theme <- rctbayespower:::rctbp_theme()
  expect_s3_class(theme, "theme")
})

test_that("pivot_plot_data_long produces measure and outcome columns", {
  mock_1d <- mock_power_analysis("1d")
  result <- rctbayespower:::pivot_plot_data_long(
    mock_1d@results_conditions, decision = "both", metric = "both"
  )
  expect_true("measure" %in% names(result))
  expect_true("outcome" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true(all(result$measure %in% c("Power", "Probability")))
  expect_true(all(result$outcome %in% c("Efficacy", "Futility")))
})

test_that("pivot_plot_data_long filters by decision", {
  mock_1d <- mock_power_analysis("1d")
  result_eff <- rctbayespower:::pivot_plot_data_long(
    mock_1d@results_conditions, decision = "success", metric = "both"
  )
  expect_true(all(result_eff$outcome == "Efficacy"))

  result_fut <- rctbayespower:::pivot_plot_data_long(
    mock_1d@results_conditions, decision = "futility", metric = "both"
  )
  expect_true(all(result_fut$outcome == "Futility"))
})

test_that("pivot_plot_data_long filters by metric", {
  mock_1d <- mock_power_analysis("1d")
  result_pwr <- rctbayespower:::pivot_plot_data_long(
    mock_1d@results_conditions, decision = "both", metric = "power"
  )
  expect_true(all(result_pwr$measure == "Power"))

  result_pr <- rctbayespower:::pivot_plot_data_long(
    mock_1d@results_conditions, decision = "both", metric = "prob"
  )
  expect_true(all(result_pr$measure == "Probability"))
})

test_that("pivot_plot_data_long warns when include_se = TRUE with no SE columns", {
  mock_1d <- mock_power_analysis("1d")
  expect_cli_warn(
    rctbayespower:::pivot_plot_data_long(
      mock_1d@results_conditions, decision = "both", metric = "both",
      include_se = TRUE
    )
  )
  # Also check that returned df lacks se column
  result <- suppressWarnings(
    rctbayespower:::pivot_plot_data_long(
      mock_1d@results_conditions, decision = "both", metric = "both",
      include_se = TRUE
    )
  )
  expect_false("se" %in% names(result))
})

test_that("to_interactive returns plotly object", {
  skip_if_not_installed("plotly")
  mock_1d <- mock_power_analysis("1d")
  p <- suppressWarnings(
    plot(mock_1d, type = "power_curve", interactive = FALSE)
  )
  result <- rctbayespower:::to_interactive(p)
  expect_s3_class(result, "plotly")
})

# =============================================================================
# Phase 2: Dispatcher + Power Curve
# =============================================================================

test_that("plot power_curve returns ggplot for 1d mock", {
  mock_1d <- mock_power_analysis("1d")
  # Default group_by = "effect_size" falls back to "decision" for 1d (warns)
  p <- suppressWarnings(
    plot(mock_1d, type = "power_curve", interactive = FALSE)
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot auto-detects power_curve for 1d", {
  mock_1d <- mock_power_analysis("1d")
  p <- suppressWarnings(
    plot(mock_1d, type = "auto", interactive = FALSE)
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot auto-detects heatmap for 2d", {
  mock_2d <- mock_power_analysis("2d")
  p <- plot(mock_2d, type = "auto", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot rejects invalid type", {
  mock_1d <- mock_power_analysis("1d")
  expect_cli_abort(plot(mock_1d, type = "invalid", interactive = FALSE))
})

test_that("plot with group_by = 'decision' works for 1d", {
  mock_1d <- mock_power_analysis("1d")
  p <- plot(mock_1d, type = "power_curve", group_by = "decision",
            interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot with group_by = 'effect_size' works for 2d", {
  mock_2d <- mock_power_analysis("2d")
  p <- plot(mock_2d, type = "power_curve", group_by = "effect_size",
            interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("show_target with group_by = 'decision' draws target lines", {
  mock_1d <- mock_power_analysis("1d")
  p <- plot(mock_1d, type = "power_curve", group_by = "decision",
            show_target = TRUE, interactive = FALSE)
  expect_s3_class(p, "ggplot")
  # Check that hline layers exist (target lines)
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomHline" %in% layer_types)
})

test_that("facet_by = 'sample_size' with 2d mock produces faceted plot", {
  mock_2d <- mock_power_analysis("2d")
  p <- plot(mock_2d, type = "power_curve", facet_by = "sample_size",
            interactive = FALSE)
  expect_s3_class(p, "ggplot")
  # facet should be set
  expect_false(is.null(p$facet))
})

# =============================================================================
# Phase 3: Heatmap
# =============================================================================

test_that("heatmap works for 2d mock", {
  mock_2d <- mock_power_analysis("2d")
  p <- plot(mock_2d, type = "heatmap", interactive = FALSE)
  expect_s3_class(p, "ggplot")
  # Check for geom_tile layer
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomTile" %in% layer_types)
})

test_that("heatmap rejects 1d mock", {
  mock_1d <- mock_power_analysis("1d")
  expect_cli_abort(plot(mock_1d, type = "heatmap", interactive = FALSE))
})

# =============================================================================
# Phase 4: Comparison
# =============================================================================

test_that("comparison works for 1d mock (sample_only)", {
  mock_1d <- mock_power_analysis("1d")
  p <- plot(mock_1d, type = "comparison", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("comparison rejects 2d mock (analysis_type = 'both')", {
  mock_2d <- mock_power_analysis("2d")
  expect_cli_abort(plot(mock_2d, type = "comparison", interactive = FALSE))
})

# =============================================================================
# Phase 5: Accrual
# =============================================================================

test_that("accrual rejects non-sequential design (1d mock)", {
  mock_1d <- mock_power_analysis("1d")
  expect_cli_abort(
    plot(mock_1d, type = "accrual", interactive = FALSE),
  )
})

test_that("accrual rejects missing accrual columns", {
  mock_no_cols <- mock_power_analysis("accrual_no_cols")
  expect_cli_abort(
    plot(mock_no_cols, type = "accrual", interactive = FALSE),
    regexp = "accrual"
  )
})

test_that("accrual works for accrual mock", {
  mock_accrual <- mock_power_analysis("accrual")
  p <- plot(mock_accrual, type = "accrual", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})
