# Test file for S3 methods
# Tests for print, summary, and plot methods using mock objects

library(testthat)
library(rctbayespower)

# Helper function to create mock rctbayespower objects
create_mock_rctbayespower <- function(outcome_type = "continuous",
                                      convergence_rate = 0.95,
                                      n_simulations = 100) {
  # Create realistic simulation results
  n_successful <- floor(n_simulations * convergence_rate)
  
  # Mock posterior draws for treatment effect
  effect_estimates <- rnorm(n_successful, mean = 0.5, sd = 0.15)
  
  # Mock simulation results
  simulation_results <- lapply(1:n_successful, function(i) {
    list(
      fixef = data.frame(
        Estimate = effect_estimates[i],
        Est.Error = 0.1,
        Q2.5 = effect_estimates[i] - 0.2,
        Q97.5 = effect_estimates[i] + 0.2
      ),
      posterior_draws = rnorm(1000, mean = effect_estimates[i], sd = 0.1),
      prob_success = rbinom(1, 1, 0.8),
      prob_futility = rbinom(1, 1, 0.2)
    )
  })
  
  # Create mock result object
  result <- list(
    n_simulations = n_simulations,
    successful_fits = n_successful,
    convergence_rate = convergence_rate,
    power_success = mean(sapply(simulation_results, function(x)
      x$prob_success)),
    power_futility = mean(sapply(simulation_results, function(x)
      x$prob_futility)),
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
    true_parameters = list(
      fixef = data.frame(
        Estimate = c(0.0, 0.3, 0.5),
        row.names = c("Intercept", "baseline", "grouptreat")
      ),
      ranef = NULL
    ),
    model_formula_true_params = brms::bf(outcome ~ baseline + group, center = FALSE),
    model_formula_estimation = brms::bf(outcome ~ baseline + group),
    family = if (outcome_type == "continuous")
      stats::gaussian()
    else if (outcome_type == "binary")
      stats::binomial()
    else
      stats::poisson(),
    simulation_results = simulation_results
  )
  
  class(result) <- "rctbayespower"
  return(result)
}

# Helper function to create mock rctbayespower_grid objects
create_mock_rctbayespower_grid <- function(analysis_type = "sample_only",
                                           with_design_prior = FALSE) {
  # Create power surface data
  sample_sizes <- c(60, 80, 100)
  effect_sizes <- if (analysis_type == "sample_only")
    0.5
  else
    c(0.3, 0.5, 0.7)
  
  # Generate grid combinations
  if (analysis_type == "sample_only") {
    combinations <- data.frame(
      n_total = sample_sizes,
      n_control = floor(sample_sizes * 0.5),
      n_treatment = ceiling(sample_sizes * 0.5),
      effect_size = rep(0.5, length(sample_sizes))
    )
  } else if (analysis_type == "effect_only") {
    combinations <- data.frame(
      n_total = rep(80, length(effect_sizes)),
      n_control = rep(40, length(effect_sizes)),
      n_treatment = rep(40, length(effect_sizes)),
      effect_size = effect_sizes
    )
  } else {
    # both
    combinations <- expand.grid(n_total = sample_sizes, effect_size = effect_sizes)
    combinations$n_control <- floor(combinations$n_total * 0.5)
    combinations$n_treatment <- ceiling(combinations$n_total * 0.5)
  }
  
  # Add power results
  combinations$power_success <- pmax(0.1, pmin(
    0.95,
    0.3 + 0.4 * (combinations$n_total - 60) / 40 + 0.3 * (combinations$effect_size - 0.3) / 0.4
  ))
  combinations$power_futility <- pmax(0.05, pmin(
    0.9,
    0.2 + 0.3 * (combinations$n_total - 60) / 40 + 0.2 * (combinations$effect_size - 0.3) / 0.4
  ))
  combinations$mean_prob_success <- combinations$power_success + rnorm(nrow(combinations), 0, 0.05)
  combinations$mean_prob_futility <- combinations$power_futility + rnorm(nrow(combinations), 0, 0.05)
  combinations$convergence_rate <- runif(nrow(combinations), 0.9, 1.0)
  
  # Create integrated power (if design prior provided)
  integrated_power <- NULL
  if (with_design_prior && analysis_type != "sample_only") {
    sample_sizes_unique <- unique(combinations$n_total)
    integrated_power <- data.frame(
      n_total = sample_sizes_unique,
      integrated_power_success = aggregate(
        combinations$power_success,
        by = list(combinations$n_total),
        FUN = mean
      )$x,
      integrated_power_futility = aggregate(
        combinations$power_futility,
        by = list(combinations$n_total),
        FUN = mean
      )$x,
      integrated_prob_success = aggregate(
        combinations$mean_prob_success,
        by = list(combinations$n_total),
        FUN = mean
      )$x,
      integrated_prob_futility = aggregate(
        combinations$mean_prob_futility,
        by = list(combinations$n_total),
        FUN = mean
      )$x
    )
  }
  
  # Find optimal combinations
  optimal_success <- combinations[combinations$power_success >= 0.8, ]
  optimal_futility <- combinations[combinations$power_futility >= 0.8, ]
  
  result <- list(
    target_power_success = 0.8,
    target_power_futility = 0.8,
    threshold_success = 0.3,
    threshold_futility = 0.1,
    sample_sizes = if (analysis_type == "effect_only")
      80
    else
      sample_sizes,
    effect_sizes = if (analysis_type == "sample_only")
      0.5
    else
      effect_sizes,
    design_prior = if (with_design_prior)
      "normal(0.5, 0.15)"
    else
      NULL,
    design_prior_type = if (with_design_prior)
      "brms"
    else
      "none",
    analysis_type = analysis_type,
    percent_group_treat = 50,
    power_analysis_fn = "power_analysis_ancova",
    analysis_time_minutes = 5.2,
    power_surface = combinations,
    integrated_power = integrated_power,
    optimal_combinations_success = optimal_success,
    optimal_combinations_futility = optimal_futility,
    min_n_success = if (nrow(optimal_success) > 0)
      min(optimal_success$n_total)
    else
      NA,
    min_n_futility = if (nrow(optimal_futility) > 0)
      min(optimal_futility$n_total)
    else
      NA,
    min_n_integrated_success = if (!is.null(integrated_power)) {
      valid_success <- integrated_power$n_total[integrated_power$integrated_power_success >= 0.8]
      if (length(valid_success) > 0)
        min(valid_success)
      else
        NA
    } else {
      NA
    },
    min_n_integrated_futility = if (!is.null(integrated_power)) {
      valid_futility <- integrated_power$n_total[integrated_power$integrated_power_futility >= 0.8]
      if (length(valid_futility) > 0)
        min(valid_futility)
      else
        NA
    } else {
      NA
    },
    
    # Backward compatibility
    power_curve = combinations,
    effect_size = if (analysis_type == "sample_only")
      0.5
    else
      NULL,
    detailed_results = vector("list", nrow(combinations)),
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

# Tests for rctbayespower class
test_that("rctbayespower S3 methods work correctly", {
  mock_result <- create_mock_rctbayespower()
  
  # Test class assignment
  expect_s3_class(mock_result, "rctbayespower")
  
  # Test print method
  expect_output(print(mock_result), "Bayesian RCT Power Analysis Results")
  expect_output(print(mock_result), "Sample size \\(control\\):")
  expect_output(print(mock_result), "Power - Success:")
  expect_output(print(mock_result), "Power - Futility:")
  expect_output(print(mock_result), "Convergence rate:")
  
  # Test summary method
  summary_result <- summary(mock_result)
  expect_type(summary_result, "list")
  expect_true("power_metrics" %in% names(summary_result))
  expect_true("effect_estimates" %in% names(summary_result))
  expect_true("study_info" %in% names(summary_result))
  
  # Test that summary output is printed
  expect_output(summary(mock_result),
                "=== Bayesian Power Analysis Summary ===")
  expect_output(summary(mock_result), "Study Design:")
  expect_output(summary(mock_result), "Power Analysis Results:")
  
  # Test plot method (should not error)
  expect_no_error(plot(mock_result))
})

# Tests for rctbayespower_grid class
test_that("rctbayespower_grid S3 methods work correctly", {
  # Test sample_only analysis
  mock_grid_sample <- create_mock_rctbayespower_grid("sample_only")
  
  expect_s3_class(mock_grid_sample, "rctbayespower_grid")
  
  # Test print method
  expect_output(print(mock_grid_sample), "Bayesian RCT Sample Size Analysis")
  expect_output(print(mock_grid_sample), "Fixed effect size:")
  expect_output(print(mock_grid_sample), "Sample sizes tested:")
  expect_output(print(mock_grid_sample), "Target power - Success:")
  
  # Test summary method
  summary_result <- summary(mock_grid_sample)
  expect_type(summary_result, "list")
  expect_true("analysis_info" %in% names(summary_result))
  expect_true("power_surface" %in% names(summary_result))
  
  expect_output(summary(mock_grid_sample),
                "Bayesian RCT Sample Size Analysis - Detailed Summary")
  expect_output(summary(mock_grid_sample), "Analysis Parameters:")
  
  # Test plot method
  expect_no_error(plot(mock_grid_sample))
})

test_that("rctbayespower_grid works with effect_only analysis", {
  mock_grid_effect <- create_mock_rctbayespower_grid("effect_only")
  
  expect_s3_class(mock_grid_effect, "rctbayespower_grid")
  expect_output(print(mock_grid_effect), "Effect sizes tested:")
  expect_no_error(plot(mock_grid_effect))
})

test_that("rctbayespower_grid works with both analysis and design prior", {
  mock_grid_both <- create_mock_rctbayespower_grid("both", with_design_prior = TRUE)
  
  expect_s3_class(mock_grid_both, "rctbayespower_grid")
  expect_output(print(mock_grid_both), "Design prior:")
  expect_output(print(mock_grid_both), "Integrated power")
  expect_no_error(plot(mock_grid_both))
})

# Test S3 method dispatch
test_that("S3 method dispatch works correctly", {
  mock_single <- create_mock_rctbayespower()
  mock_grid <- create_mock_rctbayespower_grid()
  
  # Test that S3 methods exist by checking if they can be retrieved
  expect_true(is.function(getS3method("print", "rctbayespower")))
  expect_true(is.function(getS3method("summary", "rctbayespower")))
  expect_true(is.function(getS3method("plot", "rctbayespower")))
  
  expect_true(is.function(getS3method("print", "rctbayespower_grid")))
  expect_true(is.function(getS3method("summary", "rctbayespower_grid")))
  expect_true(is.function(getS3method("plot", "rctbayespower_grid")))
  
  # Test that different classes use different methods
  print_single <- capture.output(print(mock_single))
  print_grid <- capture.output(print(mock_grid))
  
  expect_true(any(grepl(
    "Bayesian RCT Power Analysis Results", print_single
  )))
  expect_true(any(grepl("Bayesian RCT.*Analysis", print_grid)))
})

# Test error handling
test_that("S3 methods handle invalid objects gracefully", {
  # Test with invalid class
  invalid_obj <- list(some_data = 1:10)
  class(invalid_obj) <- "rctbayespower"
  
  # Methods should handle missing required fields gracefully
  expect_error(print(invalid_obj), "Missing required field")
  expect_error(summary(invalid_obj), "Missing required field")
  
  # Test with completely wrong object
  expect_error(rctbayespower:::print.rctbayespower("not an object"))
})

# Test different outcome types
test_that("S3 methods work with different outcome types", {
  mock_continuous <- create_mock_rctbayespower("continuous")
  mock_binary <- create_mock_rctbayespower("binary")
  mock_count <- create_mock_rctbayespower("count")
  
  expect_no_error(print(mock_continuous))
  expect_no_error(print(mock_binary))
  expect_no_error(print(mock_count))
  
  expect_no_error(summary(mock_continuous))
  expect_no_error(summary(mock_binary))
  expect_no_error(summary(mock_count))
})
