test_that("simulate_rct_data creates correct data structure", {
  skip_on_cran()
  
  # Test continuous outcome
  data_cont <- simulate_rct_data(
    n_control = 10,
    n_treatment = 10,
    effect_size = 0.5,
    outcome_type = "continuous",
    seed = 123
  )
  
  expect_s3_class(data_cont, "data.frame")
  expect_equal(nrow(data_cont), 20)
  expect_true(all(c("group", "outcome") %in% colnames(data_cont)))
  expect_equal(sum(data_cont$group == "control"), 10)
  expect_equal(sum(data_cont$group == "treatment"), 10)
  expect_true(is.numeric(data_cont$outcome))
  
  # Test binary outcome
  data_bin <- simulate_rct_data(
    n_control = 20,
    n_treatment = 20,
    effect_size = 0.3,
    outcome_type = "binary",
    baseline_prob = 0.3,
    seed = 456
  )
  
  expect_s3_class(data_bin, "data.frame")
  expect_equal(nrow(data_bin), 40)
  expect_true(all(data_bin$outcome %in% c(0, 1)))
  
  # Test count outcome
  data_count <- simulate_rct_data(
    n_control = 15,
    n_treatment = 15,
    effect_size = 0.4,
    outcome_type = "count",
    baseline_rate = 2,
    seed = 789
  )
  
  expect_s3_class(data_count, "data.frame")
  expect_equal(nrow(data_count), 30)
  expect_true(all(data_count$outcome >= 0))
  expect_true(all(data_count$outcome == floor(data_count$outcome)))
})

test_that("simulate_rct_data handles covariates correctly", {
  skip_on_cran()
  
  covariates <- list(
    age = list(type = "continuous", mean = 50, sd = 10),
    sex = list(type = "binary", prob = 0.5)
  )
  
  data_cov <- simulate_rct_data(
    n_control = 10,
    n_treatment = 10,
    effect_size = 0.5,
    outcome_type = "continuous",
    covariates = covariates,
    seed = 111
  )
  
  expect_true(all(c("age", "sex") %in% colnames(data_cov)))
  expect_true(is.numeric(data_cov$age))
  expect_true(all(data_cov$sex %in% c(0, 1)))
})

test_that("simulate_rct_data validates input parameters", {
  expect_error(
    simulate_rct_data(n_control = 0, n_treatment = 10, effect_size = 0.5),
    "Sample sizes must be positive"
  )
  
  expect_error(
    simulate_rct_data(n_control = 10, n_treatment = 10, effect_size = 0.5, 
                     outcome_type = "invalid"),
    "outcome_type must be one of"
  )
  
  expect_error(
    simulate_rct_data(n_control = 10, n_treatment = 10, effect_size = 0.5,
                     outcome_type = "binary", baseline_prob = 1.5),
    "baseline_prob must be between 0 and 1"
  )
})
