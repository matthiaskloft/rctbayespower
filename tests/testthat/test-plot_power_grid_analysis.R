# Test file for plot_power_grid_analysis.R
# Tests for plotting functions using mock objects

library(testthat)
library(rctbayespower)
library(ggplot2)

# Helper function to create mock rctbayespower object for plotting
create_mock_rctbayespower_plot <- function(outcome_type = "continuous") {
  # Create a simple mock object with required fields for plotting
  effect_estimates <- rnorm(95, mean = 0.5, sd = 0.15)

  result <- list(
    n_simulations = 100,
    successful_fits = 95,
    convergence_rate = 0.95,
    power_success = 0.8,
    power_futility = 0.2,
    mean_prob_success = 0.75,
    mean_prob_futility = 0.25,
    mean_effect_estimate = mean(effect_estimates),
    median_effect_estimate = median(effect_estimates),
    sd_mean_effect_estimate = sd(effect_estimates),
    sd_median_effect_estimate = sd(effect_estimates) * 0.8,
    study_parameters = list(
      n_control = 50,
      n_treatment = 50,
      target_param = "grouptreat",
      threshold_success = 0.3,
      threshold_futility = 0.1,
      p_sig_success = 0.95,
      p_sig_futility = 0.5
    ),

    # Add simulation results for plotting
    simulation_results = lapply(1:95, function(i) {
      list(
        fixef = data.frame(
          Estimate = effect_estimates[i],
          Est.Error = 0.1,
          Q2.5 = effect_estimates[i] - 0.2,
          Q97.5 = effect_estimates[i] + 0.2,
          row.names = "grouptreat"
        ),
        posterior_draws = rnorm(1000, mean = effect_estimates[i], sd = 0.1)
      )
    })
  )

  class(result) <- "rctbayespower"
  return(result)
}

# Helper function to create mock rctbayespower_grid object for plotting
create_mock_rctbayespower_grid_plot <- function(analysis_type = "sample_only",
                                                with_design_prior = FALSE) {
  # Create power surface data for plotting
  if (analysis_type == "sample_only") {
    power_surface <- data.frame(
      n_total = c(60, 80, 100, 120),
      n_control = c(30, 40, 50, 60),
      n_treatment = c(30, 40, 50, 60),
      effect_size = rep(0.5, 4),
      power_success = c(0.65, 0.75, 0.85, 0.92),
      power_futility = c(0.15, 0.25, 0.35, 0.45),
      mean_prob_success = c(0.6, 0.72, 0.82, 0.89),
      mean_prob_futility = c(0.18, 0.28, 0.38, 0.48),
      convergence_rate = rep(0.95, 4)
    )
  } else if (analysis_type == "effect_only") {
    power_surface <- data.frame(
      n_total = rep(80, 4),
      n_control = rep(40, 4),
      n_treatment = rep(40, 4),
      effect_size = c(0.3, 0.5, 0.7, 0.9),
      power_success = c(0.45, 0.75, 0.92, 0.98),
      power_futility = c(0.05, 0.25, 0.45, 0.65),
      mean_prob_success = c(0.42, 0.72, 0.89, 0.96),
      mean_prob_futility = c(0.08, 0.28, 0.48, 0.68),
      convergence_rate = rep(0.95, 4)
    )
  } else { # both
    power_surface <- expand.grid(
      n_total = c(60, 80, 100),
      effect_size = c(0.3, 0.5, 0.7)
    )
    power_surface$n_control <- floor(power_surface$n_total * 0.5)
    power_surface$n_treatment <- ceiling(power_surface$n_total * 0.5)
    power_surface$power_success <- pmax(0.1, pmin(
      0.98,
      0.3 + 0.5 * (power_surface$n_total - 60) / 40 + 0.4 * (power_surface$effect_size - 0.3) / 0.4
    ))
    power_surface$power_futility <- pmax(0.05, pmin(
      0.9,
      0.1 + 0.3 * (power_surface$n_total - 60) / 40 + 0.2 * (power_surface$effect_size - 0.3) / 0.4
    ))
    power_surface$mean_prob_success <- power_surface$power_success - 0.05
    power_surface$mean_prob_futility <- power_surface$power_futility + 0.05
    power_surface$convergence_rate <- rep(0.95, nrow(power_surface))
  }

  # Create integrated power if design prior provided
  integrated_power <- NULL
  if (with_design_prior && analysis_type != "sample_only") {
    sample_sizes_unique <- unique(power_surface$n_total)
    integrated_power <- data.frame(
      n_total = sample_sizes_unique,
      integrated_power_success = aggregate(power_surface$power_success,
        by = list(power_surface$n_total),
        FUN = mean
      )$x,
      integrated_power_futility = aggregate(power_surface$power_futility,
        by = list(power_surface$n_total),
        FUN = mean
      )$x,
      integrated_prob_success = aggregate(power_surface$mean_prob_success,
        by = list(power_surface$n_total),
        FUN = mean
      )$x,
      integrated_prob_futility = aggregate(power_surface$mean_prob_futility,
        by = list(power_surface$n_total),
        FUN = mean
      )$x
    )
  }

  result <- list(
    target_power_success = 0.8,
    target_power_futility = 0.8,
    threshold_success = 0.3,
    threshold_futility = 0.1,
    sample_sizes = if (analysis_type == "effect_only") 80 else c(60, 80, 100),
    effect_sizes = if (analysis_type == "sample_only") 0.5 else c(0.3, 0.5, 0.7),
    design_prior = if (with_design_prior) "normal(0.5, 0.15)" else NULL,
    design_prior_type = if (with_design_prior) "brms" else "none",
    analysis_type = analysis_type,
    percent_group_treat = 50,
    power_analysis_fn = "power_analysis_ancova",
    analysis_time_minutes = 8.5,
    power_surface = power_surface,
    integrated_power = integrated_power,

    # Backward compatibility
    power_curve = power_surface,
    effect_size = if (analysis_type == "sample_only") 0.5 else NULL,
    optimal_combinations_success = power_surface[power_surface$power_success >= 0.8, ],
    optimal_combinations_futility = power_surface[power_surface$power_futility >= 0.8, ],
    detailed_results = vector("list", nrow(power_surface)),
    analysis_parameters = list(
      outcome_type = "continuous",
      baseline_effect = 0.3,
      n_simulations = 100,
      n_cores = 2
    )
  )

  class(result) <- "rctbayespower_grid"
  return(result)
}

# Tests for rctbayespower plotting
test_that("plot.rctbayespower works correctly", {
  mock_result <- create_mock_rctbayespower_plot()

  # Test basic plotting
  p <- plot(mock_result)
  expect_s3_class(p, "ggplot")

  # Test that plot has appropriate elements
  expect_true(length(p$layers) > 0)
  expect_true("data" %in% names(p))

  # Test different plot types - these should give warnings but not errors
  expect_warning(plot(mock_result, type = "effect_distribution"))
  expect_warning(plot(mock_result, type = "power_summary"))
})

# Tests for rctbayespower_grid plotting
test_that("plot.rctbayespower_grid works with sample_only analysis", {
  mock_grid <- create_mock_rctbayespower_grid_plot("sample_only")

  # Test auto-detection (should create power curve)
  p <- plot(mock_grid)
  expect_s3_class(p, "ggplot")

  # Test explicit power curve
  p_curve <- plot(mock_grid, type = "power_curve")
  expect_s3_class(p_curve, "ggplot")

  # Test power curve plot (replaces sample_size)
  p_sample <- plot(mock_grid, type = "power_curve")
  expect_s3_class(p_sample, "ggplot")
})

test_that("plot.rctbayespower_grid works with effect_only analysis", {
  mock_grid <- create_mock_rctbayespower_grid_plot("effect_only")

  # Test auto-detection (should create power curve)
  p <- plot(mock_grid)
  expect_s3_class(p, "ggplot")

  # Test explicit power curve
  p_curve <- plot(mock_grid, type = "power_curve")
  expect_s3_class(p_curve, "ggplot")

  # Test power curve plot (replaces effect_size)
  p_effect <- plot(mock_grid, type = "power_curve")
  expect_s3_class(p_effect, "ggplot")
})

test_that("plot.rctbayespower_grid works with both analysis", {
  mock_grid <- create_mock_rctbayespower_grid_plot("both")

  # Test auto-detection (should create heatmap)
  p <- plot(mock_grid)
  expect_s3_class(p, "ggplot")

  # Test explicit heatmap
  p_heatmap <- plot(mock_grid, type = "heatmap")
  expect_s3_class(p_heatmap, "ggplot")

  # Test power curve with faceting
  p_curve <- plot(mock_grid, type = "power_curve")
  expect_s3_class(p_curve, "ggplot")
})

test_that("plot.rctbayespower_grid works with design prior and integrated power", {
  mock_grid <- create_mock_rctbayespower_grid_plot("effect_only", with_design_prior = TRUE)

  # Test integrated power plot
  p_integrated <- plot(mock_grid, type = "integrated")
  expect_s3_class(p_integrated, "ggplot")

  # Test power curve with integrated power shown
  p_curve <- plot(mock_grid, type = "power_curve", show_integrated = TRUE)
  expect_s3_class(p_curve, "ggplot")
})

test_that("plot.rctbayespower_grid handles different metrics", {
  mock_grid <- create_mock_rctbayespower_grid_plot("sample_only")

  # Test different metrics
  p_success <- plot(mock_grid, metric = "success")
  expect_s3_class(p_success, "ggplot")

  p_futility <- plot(mock_grid, metric = "futility")
  expect_s3_class(p_futility, "ggplot")

  p_both <- plot(mock_grid, metric = "both")
  expect_s3_class(p_both, "ggplot")
})

test_that("plot.rctbayespower_grid handles faceting options", {
  mock_grid <- create_mock_rctbayespower_grid_plot("both")

  # Test faceting by effect size
  p_facet_effect <- plot(mock_grid, type = "power_curve", facet_by = "effect_size")
  expect_s3_class(p_facet_effect, "ggplot")

  # Test faceting by sample size
  p_facet_sample <- plot(mock_grid, type = "power_curve", facet_by = "sample_size")
  expect_s3_class(p_facet_sample, "ggplot")
})

test_that("plot.rctbayespower_grid handles comparison plots", {
  mock_grid <- create_mock_rctbayespower_grid_plot("sample_only")

  # Test comparison plot
  p_comparison <- plot(mock_grid, type = "comparison")
  expect_s3_class(p_comparison, "ggplot")
})

test_that("plotting functions handle target power lines", {
  mock_grid <- create_mock_rctbayespower_grid_plot("sample_only")

  # Test that target power lines are added
  p <- plot(mock_grid, type = "power_curve")
  expect_s3_class(p, "ggplot")

  # Check that target power is referenced in the plot
  # (This is a basic check; more detailed checks would require inspecting plot layers)
  expect_true(length(p$layers) > 1) # Should have more than just the main data layer
})

test_that("plotting functions handle invalid inputs gracefully", {
  mock_grid <- create_mock_rctbayespower_grid_plot("sample_only")

  # Test invalid plot type
  expect_error(plot(mock_grid, type = "invalid_type"))

  # Test invalid metric
  expect_error(plot(mock_grid, metric = "invalid_metric"))

  # Test invalid facet_by
  expect_error(plot(mock_grid, facet_by = "invalid_facet"))
})

test_that("plotting functions work with missing optional data", {
  mock_grid <- create_mock_rctbayespower_grid_plot("sample_only")

  # Remove optional integrated power data
  mock_grid$integrated_power <- NULL

  # Should still work for basic plots
  expect_no_error(plot(mock_grid, type = "power_curve"))

  # Should error for integrated power plot
  expect_error(plot(mock_grid, type = "integrated"))
})

test_that("plotting functions handle data validation", {
  mock_grid <- create_mock_rctbayespower_grid_plot("sample_only")

  # Test with corrupted power_surface
  mock_grid$power_surface <- data.frame(incomplete = 1:3)

  # Should handle missing columns gracefully
  expect_error(plot(mock_grid), "Missing required column")
})

test_that("plotting functions produce appropriate labels and titles", {
  mock_grid <- create_mock_rctbayespower_grid_plot("sample_only")

  p <- plot(mock_grid, type = "power_curve")

  # Check that plot has labels
  expect_true(!is.null(p$labels$x))
  expect_true(!is.null(p$labels$y))
  expect_true(!is.null(p$labels$title))
})

test_that("plotting functions handle different outcome types", {
  # Test with different mock outcomes
  mock_continuous <- create_mock_rctbayespower_plot("continuous")
  mock_binary <- create_mock_rctbayespower_plot("binary")
  mock_count <- create_mock_rctbayespower_plot("count")

  expect_no_error(plot(mock_continuous))
  expect_no_error(plot(mock_binary))
  expect_no_error(plot(mock_count))
})

test_that("plotting functions handle edge cases", {
  # Test with minimal data
  mock_grid <- create_mock_rctbayespower_grid_plot("sample_only")

  # Reduce to single data point
  mock_grid$power_surface <- mock_grid$power_surface[1, ]

  # Should handle gracefully
  expect_no_error(plot(mock_grid, type = "power_curve"))
})
