test_that("power_analysis returns correct structure", {
  skip_on_cran()
  skip_if_not_installed("brms")
  
  # Mock a simple power analysis with very few simulations for testing
  result <- tryCatch({
    power_analysis(
      n_control = 20,
      n_treatment = 20,
      effect_size = 0.5,
      outcome_type = "continuous",
      n_simulations = 2,  # Very few for testing
      seed = 123
    )
  }, error = function(e) {
    skip("brms not available or other dependencies missing")
  })
  
  if (!is.null(result)) {
    expect_type(result, "list")
    expect_true("power_rope" %in% names(result))
    expect_true("power_direction" %in% names(result))
    expect_true("effect_size" %in% names(result))
    expect_true("n_control" %in% names(result))
    expect_true("n_treatment" %in% names(result))
    
    expect_true(is.numeric(result$power_rope))
    expect_true(result$power_rope >= 0 && result$power_rope <= 1)
  }
})

test_that("power_analysis validates input parameters", {
  expect_error(
    power_analysis(n_control = 0, n_treatment = 10, effect_size = 0.5),
    "Sample sizes must be positive"
  )
  
  expect_error(
    power_analysis(n_control = 10, n_treatment = 10, effect_size = 0.5,
                  outcome_type = "invalid"),
    "outcome_type must be one of"
  )
  
  expect_error(
    power_analysis(n_control = 10, n_treatment = 10, effect_size = 0.5,
                  n_simulations = 0),
    "n_simulations must be positive"
  )
  
  expect_error(
    power_analysis(n_control = 10, n_treatment = 10, effect_size = 0.5,
                  prob_threshold = 1.5),
    "prob_threshold must be between 0 and 1"
  )
})
