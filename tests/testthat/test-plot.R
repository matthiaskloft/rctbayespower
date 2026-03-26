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

# =============================================================================
# Phase 6: Parameter validation edge cases
# =============================================================================

test_that("plot rejects invalid metric", {
  mock_1d <- mock_power_analysis("1d")
  expect_cli_abort(plot(mock_1d, type = "power_curve", metric = "invalid",
                        interactive = FALSE),
                   regexp = "metric.*must be")
})

test_that("plot rejects invalid decision", {
  mock_1d <- mock_power_analysis("1d")
  expect_cli_abort(plot(mock_1d, type = "power_curve", decision = "invalid",
                        interactive = FALSE),
                   regexp = "decision.*must be")
})

test_that("plot rejects invalid group_by", {
  mock_1d <- mock_power_analysis("1d")
  expect_cli_abort(plot(mock_1d, type = "power_curve", group_by = "invalid",
                        interactive = FALSE),
                   regexp = "group_by.*must be")
})

test_that("plot rejects invalid facet_by", {
  mock_1d <- mock_power_analysis("1d")
  expect_cli_abort(plot(mock_1d, type = "power_curve", facet_by = "invalid",
                        interactive = FALSE),
                   regexp = "facet_by.*must be")
})

test_that("plot rejects facet_by with more than 2 elements", {
  mock_1d <- mock_power_analysis("1d")
  expect_cli_abort(
    plot(mock_1d, type = "power_curve",
         facet_by = c("decision", "metric", "sample_size"),
         interactive = FALSE),
    regexp = "facet_by.*1 or 2"
  )
})

# =============================================================================
# Phase 7: Metric and decision filtering
# =============================================================================

test_that("plot with metric = 'power' only shows power", {
  mock_1d <- mock_power_analysis("1d")
  p <- plot(mock_1d, type = "power_curve", metric = "power",
            group_by = "decision", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot with metric = 'prob' only shows probability", {
  mock_1d <- mock_power_analysis("1d")
  p <- plot(mock_1d, type = "power_curve", metric = "prob",
            group_by = "decision", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot with decision = 'success' only shows efficacy", {
  mock_1d <- mock_power_analysis("1d")
  p <- plot(mock_1d, type = "power_curve", decision = "success",
            group_by = "decision", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot with decision = 'futility' only shows futility", {
  mock_1d <- mock_power_analysis("1d")
  p <- plot(mock_1d, type = "power_curve", decision = "futility",
            group_by = "decision", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# Phase 8: Group by variations
# =============================================================================

test_that("plot with group_by = 'metric' works for 1d", {
  mock_1d <- mock_power_analysis("1d")
  p <- plot(mock_1d, type = "power_curve", group_by = "metric",
            interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot with group_by = 'sample_size' works for 2d", {
  mock_2d <- mock_power_analysis("2d")
  p <- plot(mock_2d, type = "power_curve", group_by = "sample_size",
            interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# Phase 9: 2D facet_grid
# =============================================================================

test_that("2D facet_grid works with decision and metric", {
  mock_2d <- mock_power_analysis("2d")
  p <- plot(mock_2d, type = "power_curve",
            facet_by = c("decision", "metric"),
            group_by = "effect_size", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("2D facet_grid works with decision and effect_size", {
  mock_2d <- mock_power_analysis("2d")
  p <- plot(mock_2d, type = "power_curve",
            facet_by = c("decision", "effect_size"),
            group_by = "decision", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# Phase 10: Target power parameter
# =============================================================================

test_that("explicit target_power draws gray target line", {
  mock_1d <- mock_power_analysis("1d")
  p <- plot(mock_1d, type = "power_curve", metric = "power",
            group_by = "decision", target_power = 0.8,
            show_target = TRUE, interactive = FALSE)
  expect_s3_class(p, "ggplot")
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomHline" %in% layer_types)
})

test_that("heatmap with explicit target_power adds contour", {
  mock_2d <- mock_power_analysis("2d")
  p <- plot(mock_2d, type = "heatmap", metric = "power",
            target_power = 0.7, interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# Phase 11: Heatmap variations
# =============================================================================

test_that("heatmap with metric = 'power' shows single facet", {
  mock_2d <- mock_power_analysis("2d")
  p <- plot(mock_2d, type = "heatmap", metric = "power",
            decision = "success", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("heatmap with decision = 'both' shows efficacy and futility", {
  mock_2d <- mock_power_analysis("2d")
  p <- plot(mock_2d, type = "heatmap", decision = "both",
            metric = "power", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# Phase 12: Comparison plot variations
# =============================================================================

test_that("comparison works for effect_only analysis", {
  # Create a mock with only effect size varying (single n_total)
  design <- mock_design()
  eff_vals <- c(0.2, 0.4, 0.6)
  grid <- data.frame(id_cond = 1:3, n_total = 100, b_arm_treat = eff_vals)

  params_by_cond <- lapply(1:3, function(i) {
    list(
      id_cond = i,
      sim_args = list(n_total = 100, p_alloc = c(0.5, 0.5),
                      b_arm_treat = eff_vals[i], intercept = 0,
                      b_covariate = 0.3, sigma = 1),
      analysis_args = list(thr_fx_eff = 0.2, thr_fx_fut = 0,
                           thr_dec_eff = 0.975, thr_dec_fut = 0.5,
                           analysis_at = NULL, interim_function = NULL,
                           trial_type = "fixed")
    )
  })

  conditions <- rctbp_conditions(
    grid = grid, params_by_cond = params_by_cond, design = design,
    crossed = list(b_arm_treat = eff_vals),
    constant = list(n_total = 100, thr_dec_eff = 0.975, thr_dec_fut = 0.5)
  )

  results_conditions <- data.frame(
    id_cond = 1:3, n_total = 100, b_arm_treat = eff_vals,
    par_name = "b_arm2",
    pwr_eff = c(0.4, 0.6, 0.85), pwr_fut = c(0.4, 0.25, 0.1),
    pr_eff = c(0.35, 0.55, 0.80), pr_fut = c(0.35, 0.20, 0.08),
    post_median = c(0.20, 0.35, 0.55), post_q025 = c(0.05, 0.15, 0.35),
    post_q975 = c(0.35, 0.55, 0.75)
  )

  pa <- rctbp_power_analysis(
    n_sims = 1L, n_cores = 1L, conditions = conditions,
    results_conditions = results_conditions,
    results_interim = data.frame(), results_raw = data.frame()
  )

  p <- plot(pa, type = "comparison", interactive = FALSE)
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# Phase 13: MCSE ribbon handling
# =============================================================================

test_that("pivot_plot_data_long includes SE when columns present", {
  mock_1d <- mock_power_analysis("1d")
  df <- mock_1d@results_conditions
  # Add SE columns
  df$se_pwr_eff <- c(0.01, 0.02, 0.015)
  df$se_pwr_fut <- c(0.01, 0.01, 0.005)
  df$se_pr_eff <- c(0.012, 0.018, 0.013)
  df$se_pr_fut <- c(0.009, 0.008, 0.004)

  result <- rctbayespower:::pivot_plot_data_long(
    df, decision = "both", metric = "both", include_se = TRUE
  )
  expect_true("se" %in% names(result))
  expect_true(all(!is.na(result$se)))
})

test_that("pivot_plot_data_long warns on partial SE columns", {
  mock_1d <- mock_power_analysis("1d")
  df <- mock_1d@results_conditions
  # Add only some SE columns
  df$se_pwr_eff <- c(0.01, 0.02, 0.015)

  expect_cli_warn(
    rctbayespower:::pivot_plot_data_long(
      df, decision = "both", metric = "both", include_se = TRUE
    )
  )
})

# =============================================================================
# Phase 14: Interactive conversion
# =============================================================================

test_that("to_interactive with legend_position = 'right' works", {
  skip_if_not_installed("plotly")
  mock_1d <- mock_power_analysis("1d")
  p <- suppressWarnings(
    plot(mock_1d, type = "power_curve", interactive = FALSE)
  )
  result <- rctbayespower:::to_interactive(p, legend_position = "right")
  expect_s3_class(result, "plotly")
})

test_that("plot with interactive = TRUE returns plotly", {
  skip_if_not_installed("plotly")
  mock_1d <- mock_power_analysis("1d")
  p <- suppressWarnings(
    plot(mock_1d, type = "power_curve", group_by = "decision",
         interactive = TRUE)
  )
  expect_s3_class(p, "plotly")
})

# =============================================================================
# Phase 15: Pareto optimization plots
# =============================================================================

test_that("pareto front plot works", {
  result <- mock_pareto_result()
  p <- plot(result, type = "pareto")
  expect_s3_class(p, "ggplot")
})

test_that("pareto front plot without selected design works", {
  result <- mock_pareto_result()
  result@selected_design <- data.frame()
  p <- rctbayespower:::plot_pareto_front(result, highlight_selected = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("pareto front returns NULL for empty pareto_front", {
  result <- mock_pareto_result()
  result@pareto_front <- data.frame()
  expect_cli_warn(
    p <- rctbayespower:::plot_pareto_front(result),
    regexp = "No Pareto-optimal"
  )
  expect_null(p)
})

test_that("pareto convergence returns NULL for empty convergence", {
  result <- mock_pareto_result()
  expect_cli_warn(
    p <- rctbayespower:::plot_pareto_convergence(result),
    regexp = "No convergence"
  )
  expect_null(p)
})

test_that("pareto convergence works with data", {
  result <- mock_pareto_result()
  result@convergence <- data.frame(
    eval = 1:5,
    value = c(0.5, 0.6, 0.55, 0.7, 0.75),
    best_so_far = c(0.5, 0.6, 0.6, 0.7, 0.75)
  )
  p <- rctbayespower:::plot_pareto_convergence(result)
  expect_s3_class(p, "ggplot")
})

test_that("pareto convergence with show_all = FALSE works", {
  result <- mock_pareto_result()
  result@convergence <- data.frame(
    eval = 1:5,
    value = c(0.5, 0.6, 0.55, 0.7, 0.75),
    best_so_far = c(0.5, 0.6, 0.6, 0.7, 0.75)
  )
  p <- rctbayespower:::plot_pareto_convergence(result, show_all = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("pareto search plot works with 1 param", {
  result <- mock_pareto_result()
  p <- rctbayespower:::plot_pareto_search(result)
  expect_s3_class(p, "ggplot")
})

test_that("pareto search plot works with 2 params", {
  result <- mock_pareto_result()
  result@search <- list(n_total = c(50, 300), b_arm_treat = c(0.1, 0.8))
  result@archive$b_arm_treat <- c(0.2, 0.3, 0.5, 0.6, 0.7)
  p <- rctbayespower:::plot_pareto_search(result)
  expect_s3_class(p, "ggplot")
})

test_that("pareto search returns NULL for empty search params", {
  result <- mock_pareto_result()
  result@search <- list()
  expect_cli_warn(
    p <- rctbayespower:::plot_pareto_search(result),
    regexp = "No search parameters"
  )
  expect_null(p)
})

test_that("plot pareto type = 'all' returns list of plots", {
  result <- mock_pareto_result()
  result@convergence <- data.frame(
    eval = 1:5,
    value = c(0.5, 0.6, 0.55, 0.7, 0.75),
    best_so_far = c(0.5, 0.6, 0.6, 0.7, 0.75)
  )
  plots <- plot(result, type = "all")
  expect_type(plots, "list")
  expect_named(plots, c("pareto", "convergence", "search"))
})

test_that("plot pareto rejects empty archive", {
  result <- mock_pareto_result()
  result@archive <- data.frame()
  expect_cli_abort(plot(result), regexp = "No results to plot")
})

# =============================================================================
# Phase 16: Accrual plot with multiple conditions
# =============================================================================

test_that("accrual plot has facets for multiple conditions", {
  mock_accrual <- mock_power_analysis("accrual")
  p <- plot(mock_accrual, type = "accrual", interactive = FALSE)
  expect_s3_class(p, "ggplot")
  # Should have facet_wrap for 2 conditions
  expect_false(is.null(p$facet))
})

# =============================================================================
# Phase 17: Power curve for sequential design
# =============================================================================

test_that("power_curve works for sequential design (accrual mock)", {
  mock_accrual <- mock_power_analysis("accrual")
  p <- suppressWarnings(
    plot(mock_accrual, type = "power_curve", interactive = FALSE)
  )
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# Phase 18: get_pareto_axis_labels helper
# =============================================================================

test_that("get_pareto_axis_labels returns friendly names", {
  labels <- rctbayespower:::get_pareto_axis_labels("power_n", "pwr_eff", "n_total")
  expect_equal(labels$x, "Power (Efficacy)")
  expect_equal(labels$y, "Sample Size")
})

test_that("get_pareto_axis_labels falls back to raw names", {
  labels <- rctbayespower:::get_pareto_axis_labels("custom", "custom_obj", "other_obj")
  expect_equal(labels$x, "custom_obj")
  expect_equal(labels$y, "other_obj")
})
