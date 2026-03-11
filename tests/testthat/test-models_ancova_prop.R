# =============================================================================
# Tests for Proportional ANCOVA Model (Logit-Normal)
# =============================================================================

# =============================================================================
# SIM FN VALIDATION
# =============================================================================

test_that("proportional sim fn rejects invalid inputs", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)

  expect_error(sim_fn(n_total = -1, intercept = 0, b_arm_treat = 0, b_covariate = 0, sigma = 1))
})

test_that("build_model_ancova_prop rejects NULL n_arms", {
  expect_error(
    build_model_ancova_prop(n_arms = NULL),
    "n_arms"
  )
})

test_that("build_model_ancova_prop rejects invalid n_arms", {
  expect_error(
    build_model_ancova_prop(n_arms = 1),
    "n_arms"
  )
})

test_that("proportional sim fn rejects NULL b_arm_treat", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)

  expect_error(sim_fn(n_total = 20, b_arm_treat = NULL, intercept = 0, b_covariate = 0, sigma = 1))
})

test_that("proportional sim fn rejects NULL sigma", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)

  expect_error(sim_fn(n_total = 20, intercept = 0, b_arm_treat = 0, b_covariate = 0, sigma = NULL))
})

test_that("proportional sim fn has sigma parameter", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  fn_formals <- names(formals(sim_fn))
  expect_true("sigma" %in% fn_formals)
})

# =============================================================================
# REQUIRED PARAMETERS
# =============================================================================

test_that("get_args_without_defaults returns correct required params", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  required_params <- get_args_without_defaults(sim_fn)
  expect_true("intercept" %in% required_params)
  expect_true("b_arm_treat" %in% required_params)
  expect_true("b_covariate" %in% required_params)
  expect_true("sigma" %in% required_params)
})

# =============================================================================
# SIM FN OUTPUT
# =============================================================================

test_that("proportional sim fn produces correct columns and types", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  set.seed(42)
  df <- sim_fn(n_total = 100, intercept = 0, b_arm_treat = 0, b_covariate = 0, sigma = 1)

  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 100)
  expect_true(all(c("covariate", "arm", "outcome") %in% names(df)))
})

test_that("proportional sim fn outcome is numeric and unbounded (logit scale)", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  set.seed(42)
  df <- sim_fn(n_total = 200, intercept = 0, b_arm_treat = 0.5, b_covariate = 0.3, sigma = 1)

  expect_true(is.numeric(df$outcome))
  # Logit-scale values can be negative or positive (unbounded)
  expect_true(any(df$outcome < 0))
  expect_true(any(df$outcome > 0))
})

test_that("proportional sim fn arm levels are correct", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  set.seed(42)
  df <- sim_fn(n_total = 100, intercept = 0, b_arm_treat = 0, b_covariate = 0, sigma = 1)

  expect_equal(levels(df$arm), c("ctrl", "treat_1"))
})

# =============================================================================
# SIM FN STATISTICAL PROPERTIES
# =============================================================================

test_that("proportional sim fn: null effect produces similar means across arms", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  set.seed(42)
  df <- sim_fn(n_total = 5000, intercept = 0, b_arm_treat = 0, b_covariate = 0, sigma = 1)

  mean_ctrl <- mean(df$outcome[df$arm == "ctrl"])
  mean_treat <- mean(df$outcome[df$arm == "treat_1"])

  expect_true(abs(mean_ctrl - mean_treat) < 0.2)
})

test_that("proportional sim fn: positive b_arm_treat produces higher treatment mean", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  set.seed(42)
  df <- sim_fn(n_total = 5000, intercept = 0, b_arm_treat = 2, b_covariate = 0, sigma = 1)

  mean_ctrl <- mean(df$outcome[df$arm == "ctrl"])
  mean_treat <- mean(df$outcome[df$arm == "treat_1"])

  expect_true(mean_treat > mean_ctrl)
})

test_that("proportional sim fn: sigma controls variability", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)

  set.seed(42)
  df_small <- sim_fn(n_total = 5000, intercept = 0, b_arm_treat = 0, b_covariate = 0, sigma = 0.3)
  set.seed(42)
  df_large <- sim_fn(n_total = 5000, intercept = 0, b_arm_treat = 0, b_covariate = 0, sigma = 2)

  expect_true(sd(df_small$outcome) < sd(df_large$outcome))
})

test_that("proportional sim fn: back-transformed outcomes cluster near expected proportion", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  set.seed(42)
  # intercept = 0 (logit scale) => 50% proportion, small sigma for tight clustering
  df <- sim_fn(n_total = 5000, intercept = 0, b_arm_treat = 0, b_covariate = 0, sigma = 0.5)

  props <- stats::plogis(df$outcome)
  expect_true(mean(props) > 0.3 && mean(props) < 0.7)
})

# =============================================================================
# BACK-TRANSFORM SANITY
# =============================================================================

test_that("plogis(outcome) values are strictly in (0, 1) and finite", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  set.seed(42)
  df <- sim_fn(n_total = 1000, intercept = qlogis(0.3), b_arm_treat = 0.5,
               b_covariate = 0.2, sigma = 0.5)

  props <- stats::plogis(df$outcome)
  expect_true(all(props > 0 & props < 1))
  expect_true(all(is.finite(props)))
})

# =============================================================================
# BRMS COMPILATION (via build_model_ancova_prop)
# =============================================================================

test_that("build_model_ancova_prop compiles without error", {
  skip_on_ci()
  model <- build_model_ancova_prop(
    n_arms = 2,
    contrasts = "contr.treatment",
    p_alloc = c(0.5, 0.5),
    intercept = 0,
    b_arm_treat = 0,
    b_covariate = 0,
    sigma = 1
  )
  expect_s3_class(model, "rctbayespower::rctbp_model")
})

test_that("build_model_ancova_prop uses gaussian family", {
  skip_on_ci()
  model <- build_model_ancova_prop(
    n_arms = 2,
    contrasts = "contr.treatment",
    p_alloc = c(0.5, 0.5),
    intercept = 0,
    b_arm_treat = 0,
    b_covariate = 0,
    sigma = 1
  )
  family <- model@inference_model$family$family
  expect_equal(family, "gaussian")
})

test_that("build_model_ancova_prop has sigma parameter in brms variables", {
  skip_on_ci()
  model <- build_model_ancova_prop(
    n_arms = 2,
    contrasts = "contr.treatment",
    p_alloc = c(0.5, 0.5),
    intercept = 0,
    b_arm_treat = 0,
    b_covariate = 0,
    sigma = 1
  )
  vars <- brms::variables(model@inference_model)
  expect_true(any(grepl("sigma", vars)))
})

test_that("build_model_ancova_prop discovers fixed effect parameters", {
  skip_on_ci()
  model <- build_model_ancova_prop(
    n_arms = 2,
    contrasts = "contr.treatment",
    p_alloc = c(0.5, 0.5),
    intercept = 0,
    b_arm_treat = 0,
    b_covariate = 0,
    sigma = 1
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

test_that("show_predefined_models includes ancova_prop_2arms", {
  models <- show_predefined_models()
  expect_true("ancova_prop_2arms" %in% models)
})

test_that("show_predefined_models('prop') returns proportional model", {
  models <- show_predefined_models("prop")
  expect_equal(models, "ancova_prop_2arms")
})

test_that("create_ancova_prop_sim_fn returns rctbp_sim_fn", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  expect_s3_class(sim_fn, "rctbayespower::rctbp_sim_fn")
})

test_that("create_ancova_prop_sim_fn lists correct params (includes sigma)", {
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  params <- sim_fn@sim_fn_params
  expect_true("b_arm_treat" %in% params)
  expect_true("b_covariate" %in% params)
  expect_true("sigma" %in% params)
})

test_that("build_model_ancova_prop_2arms is exported and callable", {
  expect_true(is.function(build_model_ancova_prop_2arms))
})

# =============================================================================
# WRAPPER REQUIRED PARAMS
# =============================================================================

test_that("wrapper sim_fn errors when called without b_arm_treat", {
  skip_on_ci()
  model <- build_model_ancova_prop_2arms(b_arm_treat = 0.5, b_covariate = 0.2)
  # Call sim_fn without b_arm_treat (it defaults to NULL from closure)
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  expect_error(sim_fn(n_total = 20, intercept = 0, b_covariate = 0, sigma = 1))
})

test_that("wrapper sim_fn errors when called without b_covariate", {
  skip_on_ci()
  model <- build_model_ancova_prop_2arms(b_arm_treat = 0.5, b_covariate = 0.2)
  # Call sim_fn without b_covariate (it defaults to NULL from closure)
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  expect_error(sim_fn(n_total = 20, intercept = 0, b_arm_treat = 0, sigma = 1))
})

# =============================================================================
# BATCH SIM FN
# =============================================================================

test_that("batch sim fn produces correct dimensions", {
  set.seed(42)
  batch <- simulate_data_ancova_prop_2arms_batch(
    n_sims = 10, n_total = 50,
    intercept = 0, b_arm_treat = 0, b_covariate = 0, sigma = 1
  )
  expect_equal(dim(batch$outcome), c(10, 50))
  expect_equal(dim(batch$covariate), c(10, 50))
  expect_equal(dim(batch$group), c(10, 50))
  expect_equal(batch$N, 50)
})

test_that("batch sim fn outcome is on logit scale (unbounded)", {
  set.seed(42)
  batch <- simulate_data_ancova_prop_2arms_batch(
    n_sims = 10, n_total = 100,
    intercept = 0, b_arm_treat = 0.5, b_covariate = 0.3, sigma = 1
  )
  # Logit-scale values should be continuous and unbounded
  expect_true(is.numeric(batch$outcome))
  expect_true(any(batch$outcome < 0))
  expect_true(any(batch$outcome > 0))
})

test_that("batch sim fn sigma controls variability", {
  set.seed(42)
  batch_small <- simulate_data_ancova_prop_2arms_batch(
    n_sims = 50, n_total = 100,
    intercept = 0, b_arm_treat = 0, b_covariate = 0, sigma = 0.3
  )
  set.seed(42)
  batch_large <- simulate_data_ancova_prop_2arms_batch(
    n_sims = 50, n_total = 100,
    intercept = 0, b_arm_treat = 0, b_covariate = 0, sigma = 2
  )
  expect_true(sd(batch_small$outcome) < sd(batch_large$outcome))
})

test_that("batch sim fn rejects invalid inputs", {
  expect_error(
    simulate_data_ancova_prop_2arms_batch(n_sims = 0, n_total = 50),
    "n_sims"
  )
  expect_error(
    simulate_data_ancova_prop_2arms_batch(n_sims = 10, n_total = -1),
    "n_total"
  )
  expect_error(
    simulate_data_ancova_prop_2arms_batch(n_sims = "a", n_total = 50),
    "n_sims"
  )
  expect_error(
    simulate_data_ancova_prop_2arms_batch(n_sims = 10, n_total = "b"),
    "n_total"
  )
  expect_error(
    simulate_data_ancova_prop_2arms_batch(n_sims = 10, n_total = 50, p_alloc = 1.5),
    "p_alloc"
  )
  expect_error(
    simulate_data_ancova_prop_2arms_batch(n_sims = 10, n_total = 50, sigma = -1),
    "sigma"
  )
})

test_that("batch sim fn is statistically consistent with standard sim fn", {
  set.seed(42)
  n_sims <- 500
  n_total <- 200
  intercept <- qlogis(0.3)
  b_arm_treat <- 0.5

  # Batch version
  batch <- simulate_data_ancova_prop_2arms_batch(
    n_sims = n_sims, n_total = n_total,
    intercept = intercept, b_arm_treat = b_arm_treat, b_covariate = 0, sigma = 0.5
  )
  batch_mean <- mean(batch$outcome)

  # Standard version (multiple calls)
  sim_fn <- create_ancova_prop_sim_fn(n_arms = 2)
  set.seed(42)
  standard_means <- vapply(seq_len(100), function(i) {
    df <- sim_fn(n_total = n_total, intercept = intercept,
                 b_arm_treat = b_arm_treat, b_covariate = 0, sigma = 0.5)
    mean(df$outcome)
  }, numeric(1))
  standard_mean <- mean(standard_means)

  # Both should produce similar overall means (within tolerance)
  expect_true(abs(batch_mean - standard_mean) < 0.1)
})
