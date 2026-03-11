# =============================================================================
# Tests for Binary ANCOVA Model
# =============================================================================

# =============================================================================
# SIM FN VALIDATION
# =============================================================================

test_that("binary sim fn rejects invalid inputs", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)

  expect_error(sim_fn(n_total = -1, intercept = 0, b_arm_treat = 0, b_covariate = 0))
})

test_that("build_model_ancova_bin rejects NULL n_arms", {
  expect_error(
    build_model_ancova_bin(n_arms = NULL),
    "n_arms"
  )
})

test_that("build_model_ancova_bin rejects invalid n_arms", {
  expect_error(
    build_model_ancova_bin(n_arms = 1),
    "n_arms"
  )
})

test_that("binary sim fn rejects NULL b_arm_treat", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)

  # NULL b_arm_treat causes matrix multiply error
  expect_error(sim_fn(n_total = 20, b_arm_treat = NULL))
})

test_that("binary sim fn has no sigma parameter", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)
  fn_formals <- names(formals(sim_fn))
  expect_false("sigma" %in% fn_formals)
})

test_that("binary sim fn in build_model rejects invalid contrast string", {
  skip_on_ci()
  expect_error(
    build_model_ancova_bin(
      n_arms = 2, contrasts = "contr.fake",
      p_alloc = c(0.5, 0.5), intercept = 0,
      b_arm_treat = 0, b_covariate = 0
    ),
    "contrasts"
  )
})

# =============================================================================
# SIM FN OUTPUT
# =============================================================================

test_that("binary sim fn produces correct columns and types", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)
  set.seed(42)
  df <- sim_fn(n_total = 100, intercept = 0, b_arm_treat = 0, b_covariate = 0)

  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 100)
  expect_true(all(c("covariate", "arm", "outcome") %in% names(df)))
})

test_that("binary sim fn outcome is strictly 0 or 1", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)
  set.seed(42)
  df <- sim_fn(n_total = 200, intercept = 0, b_arm_treat = 0.5, b_covariate = 0.3)

  expect_true(all(df$outcome %in% c(0L, 1L)))
})

test_that("binary sim fn arm levels are correct", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)
  set.seed(42)
  df <- sim_fn(n_total = 100, intercept = 0, b_arm_treat = 0, b_covariate = 0)

  expect_equal(levels(df$arm), c("ctrl", "treat_1"))
})

# =============================================================================
# SIM FN STATISTICAL PROPERTIES
# =============================================================================

test_that("binary sim fn: null effect produces approximately equal rates", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)
  set.seed(42)
  df <- sim_fn(n_total = 5000, intercept = 0, b_arm_treat = 0, b_covariate = 0)

  rate_ctrl <- mean(df$outcome[df$arm == "ctrl"])
  rate_treat <- mean(df$outcome[df$arm == "treat_1"])

  # With intercept = 0 (50% rate) and no treatment effect, rates should be similar
  expect_true(abs(rate_ctrl - rate_treat) < 0.1)
  expect_true(abs(rate_ctrl - 0.5) < 0.1)
})

test_that("binary sim fn: large positive b_arm_treat produces higher treatment rate", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)
  set.seed(42)
  df <- sim_fn(n_total = 5000, intercept = 0, b_arm_treat = 2, b_covariate = 0)

  rate_ctrl <- mean(df$outcome[df$arm == "ctrl"])
  rate_treat <- mean(df$outcome[df$arm == "treat_1"])

  expect_true(rate_treat > rate_ctrl)
})

test_that("binary sim fn: intercept controls baseline rate", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)
  set.seed(42)
  # intercept = qlogis(0.3) should give ~30% control rate
  df <- sim_fn(n_total = 10000, intercept = qlogis(0.3), b_arm_treat = 0, b_covariate = 0)

  rate_ctrl <- mean(df$outcome[df$arm == "ctrl"])
  expect_true(abs(rate_ctrl - 0.3) < 0.05)
})

# =============================================================================
# BRMS COMPILATION (via build_model_ancova_bin)
# =============================================================================

test_that("build_model_ancova_bin compiles without error", {
  skip_on_ci()
  model <- build_model_ancova_bin(
    n_arms = 2,
    contrasts = "contr.treatment",
    p_alloc = c(0.5, 0.5),
    intercept = 0,
    b_arm_treat = 0,
    b_covariate = 0
  )
  expect_s3_class(model, "rctbayespower::rctbp_model")
})

test_that("build_model_ancova_bin uses bernoulli family", {
  skip_on_ci()
  model <- build_model_ancova_bin(
    n_arms = 2,
    contrasts = "contr.treatment",
    p_alloc = c(0.5, 0.5),
    intercept = 0,
    b_arm_treat = 0,
    b_covariate = 0
  )
  family <- model@inference_model$family$family
  expect_equal(family, "bernoulli")
})

test_that("build_model_ancova_bin has no sigma parameter", {
  skip_on_ci()
  model <- build_model_ancova_bin(
    n_arms = 2,
    contrasts = "contr.treatment",
    p_alloc = c(0.5, 0.5),
    intercept = 0,
    b_arm_treat = 0,
    b_covariate = 0
  )
  vars <- brms::variables(model@inference_model)
  expect_false(any(grepl("sigma", vars)))
})

test_that("build_model_ancova_bin discovers fixed effect parameters", {
  skip_on_ci()
  model <- build_model_ancova_bin(
    n_arms = 2,
    contrasts = "contr.treatment",
    p_alloc = c(0.5, 0.5),
    intercept = 0,
    b_arm_treat = 0,
    b_covariate = 0
  )
  vars <- brms::variables(model@inference_model)
  b_vars <- grep("^b_", vars, value = TRUE)
  expect_true("b_Intercept" %in% b_vars)
  expect_true("b_covariate" %in% b_vars)
  expect_true(any(grepl("^b_arm", b_vars)))
})

# =============================================================================
# PREDEFINED MODEL
# =============================================================================

test_that("show_predefined_models includes ancova_bin_2arms", {
  models <- show_predefined_models()
  expect_true("ancova_bin_2arms" %in% models)
})

test_that("show_predefined_models('bin') returns binary model", {
  models <- show_predefined_models("bin")
  expect_equal(models, "ancova_bin_2arms")
})

test_that("create_ancova_bin_sim_fn returns rctbp_sim_fn", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)
  expect_s3_class(sim_fn, "rctbayespower::rctbp_sim_fn")
})

test_that("create_ancova_bin_sim_fn lists correct params (no sigma)", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)
  params <- sim_fn@sim_fn_params
  expect_true("b_arm_treat" %in% params)
  expect_true("b_covariate" %in% params)
  expect_false("sigma" %in% params)
})

test_that("create_ancova_bin_sim_fn treats effect params as required", {
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)
  # get_args_without_defaults returns params with NULL defaults as required
  required_params <- get_args_without_defaults(sim_fn)
  expect_true("intercept" %in% required_params)
  expect_true("b_arm_treat" %in% required_params)
  expect_true("b_covariate" %in% required_params)
})

test_that("build_model_ancova_bin_2arms is exported and callable", {
  expect_true(is.function(build_model_ancova_bin_2arms))
})

# =============================================================================
# BATCH SIM FN
# =============================================================================

test_that("batch sim fn produces correct dimensions", {
  set.seed(42)
  batch <- simulate_data_ancova_bin_2arms_batch(
    n_sims = 10, n_total = 50,
    intercept = 0, b_arm_treat = 0, b_covariate = 0
  )
  expect_equal(dim(batch$outcome), c(10, 50))
  expect_equal(dim(batch$covariate), c(10, 50))
  expect_equal(dim(batch$group), c(10, 50))
  expect_equal(batch$N, 50)
})

test_that("batch sim fn outcome values are in {0, 1}", {
  set.seed(42)
  batch <- simulate_data_ancova_bin_2arms_batch(
    n_sims = 10, n_total = 100,
    intercept = 0, b_arm_treat = 0.5, b_covariate = 0.3
  )
  expect_true(all(batch$outcome %in% c(0L, 1L)))
})

test_that("batch sim fn rejects invalid inputs", {
  expect_error(
    simulate_data_ancova_bin_2arms_batch(n_sims = 0, n_total = 50),
    "n_sims"
  )
  expect_error(
    simulate_data_ancova_bin_2arms_batch(n_sims = 10, n_total = -1),
    "n_total"
  )
  expect_error(
    simulate_data_ancova_bin_2arms_batch(n_sims = "a", n_total = 50),
    "n_sims"
  )
  expect_error(
    simulate_data_ancova_bin_2arms_batch(n_sims = 10, n_total = "b"),
    "n_total"
  )
  expect_error(
    simulate_data_ancova_bin_2arms_batch(n_sims = 10, n_total = 50, p_alloc = 1.5),
    "p_alloc"
  )
})

test_that("batch sim fn is statistically consistent with standard sim fn", {
  set.seed(42)
  n_sims <- 500
  n_total <- 200
  intercept <- qlogis(0.3)
  b_arm_treat <- log(2)

  # Batch version
  batch <- simulate_data_ancova_bin_2arms_batch(
    n_sims = n_sims, n_total = n_total,
    intercept = intercept, b_arm_treat = b_arm_treat, b_covariate = 0
  )
  batch_mean_rate <- mean(batch$outcome)

  # Standard version (multiple calls)
  sim_fn <- create_ancova_bin_sim_fn(n_arms = 2)
  set.seed(42)
  standard_rates <- vapply(seq_len(100), function(i) {
    df <- sim_fn(n_total = n_total, intercept = intercept,
                 b_arm_treat = b_arm_treat, b_covariate = 0)
    mean(df$outcome)
  }, numeric(1))
  standard_mean_rate <- mean(standard_rates)

  # Both should produce similar overall response rates (within tolerance)
  expect_true(abs(batch_mean_rate - standard_mean_rate) < 0.05)
})

# =============================================================================
# RENAME VERIFICATION
# =============================================================================

test_that("build_model_ancova_cont exists and is exported", {
  expect_true(is.function(build_model_ancova_cont))
})

test_that("build_model_ancova_cont_2arms still works", {
  skip_on_ci()
  model <- build_model_ancova_cont_2arms(
    intercept = 0, b_arm_treat = 0.5, b_covariate = 0.3
  )
  expect_s3_class(model, "rctbayespower::rctbp_model")
  expect_equal(model@endpoint_types, "continuous")
})
