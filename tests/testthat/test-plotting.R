test_that("plot_power_curve creates ggplot objects", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  # Create mock data structure that mimics power curve results
  mock_power_curve <- list(
    effect_sizes = c(0, 0.2, 0.5, 0.8),
    power_rope = c(0.05, 0.15, 0.65, 0.90),
    power_direction = c(0.10, 0.25, 0.75, 0.95),
    power_significant = c(0.05, 0.20, 0.70, 0.92),
    n_control = 50,
    n_treatment = 50,
    outcome_type = "continuous"
  )
  class(mock_power_curve) <- "rctbayespower_curve"
  
  # Test that plot function works
  expect_error(
    plot_power_curve(mock_power_curve),
    NA  # Should not throw an error
  )
  
  # Create mock sample size data
  mock_sample_size <- list(
    sample_sizes = c(20, 30, 40, 50),
    power_rope = c(0.40, 0.60, 0.75, 0.85),
    power_direction = c(0.45, 0.65, 0.80, 0.90),
    target_power = 0.8,
    effect_size = 0.5,
    outcome_type = "continuous"
  )
  class(mock_sample_size) <- "rctbayespower_samplesize"
  
  expect_error(
    plot_power_curve(mock_sample_size, type = "sample_size"),
    NA  # Should not throw an error
  )
})

test_that("plot_power_curve validates input parameters", {
  expect_error(
    plot_power_curve("invalid_input"),
    "x must be an object of class"
  )
  
  mock_data <- list(effect_sizes = c(0, 0.5), power_rope = c(0.1, 0.8))
  class(mock_data) <- "rctbayespower_curve"
  
  expect_error(
    plot_power_curve(mock_data, power_metric = "invalid"),
    "power_metric must be one of"
  )
})
