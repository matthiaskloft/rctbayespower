# Tests for rctbp_design class (R/class_design.R)

# =============================================================================
# DIRECT S7 CONSTRUCTION (bypassing build_design)
# =============================================================================

test_that("rctbp_design validates inference_model is provided", {
  expect_error(
    rctbp_design(
      sim_fn = mock_sim_fn(),
      inference_model = NULL,
      backend = "brms",
      display_name = "test",
      n_endpoints = 1, endpoint_types = "continuous",
      n_arms = 2, n_repeated_measures = 0,
      par_names_inference = "b_arm2",
      target_params = "b_arm2"
    ),
    "inference_model"
  )
})

test_that("rctbp_design validates backend matches model type", {
  expect_error(
    rctbp_design(
      sim_fn = mock_sim_fn(),
      inference_model = list(not = "brmsfit"),
      backend = "brms",
      display_name = "test",
      n_endpoints = 1, endpoint_types = "continuous",
      n_arms = 2, n_repeated_measures = 0,
      par_names_inference = "b_arm2",
      target_params = "b_arm2"
    ),
    "brmsfit"
  )
})

test_that("rctbp_design validates sim_fn is provided", {
  expect_error(
    rctbp_design(
      sim_fn = NULL,
      inference_model = mock_brmsfit(),
      backend = "brms",
      display_name = "test",
      n_endpoints = 1, endpoint_types = "continuous",
      n_arms = 2, n_repeated_measures = 0,
      par_names_inference = "b_arm2",
      target_params = "b_arm2"
    ),
    "sim_fn"
  )
})

test_that("rctbp_design validates target_params exist in par_names_inference", {
  expect_error(
    rctbp_design(
      sim_fn = mock_sim_fn(),
      inference_model = mock_brmsfit(),
      backend = "brms",
      display_name = "test",
      n_endpoints = 1, endpoint_types = "continuous",
      n_arms = 2, n_repeated_measures = 0,
      par_names_inference = c("b_Intercept", "b_arm2"),
      target_params = "b_nonexistent"
    ),
    "target_params"
  )
})

test_that("rctbp_design validates n_endpoints is positive", {
  expect_error(
    rctbp_design(
      sim_fn = mock_sim_fn(),
      inference_model = mock_brmsfit(),
      backend = "brms",
      display_name = "test",
      n_endpoints = 0, endpoint_types = "continuous",
      n_arms = 2, n_repeated_measures = 0,
      par_names_inference = "b_arm2",
      target_params = "b_arm2"
    ),
    "n_endpoints"
  )
})

test_that("rctbp_design validates endpoint_types", {
  expect_error(
    rctbp_design(
      sim_fn = mock_sim_fn(),
      inference_model = mock_brmsfit(),
      backend = "brms",
      display_name = "test",
      n_endpoints = 1, endpoint_types = "invalid_type",
      n_arms = 2, n_repeated_measures = 0,
      par_names_inference = "b_arm2",
      target_params = "b_arm2"
    ),
    "endpoint_types"
  )
})

test_that("rctbp_design validates n_arms is positive", {
  expect_error(
    rctbp_design(
      sim_fn = mock_sim_fn(),
      inference_model = mock_brmsfit(),
      backend = "brms",
      display_name = "test",
      n_endpoints = 1, endpoint_types = "continuous",
      n_arms = 0, n_repeated_measures = 0,
      par_names_inference = "b_arm2",
      target_params = "b_arm2"
    ),
    "n_arms"
  )
})

test_that("rctbp_design validates plain sim_fn has required params", {
  bad_fn <- function(x, y) data.frame(z = x + y)
  expect_error(
    rctbp_design(
      sim_fn = bad_fn,
      inference_model = mock_brmsfit(),
      backend = "brms",
      display_name = "test",
      n_endpoints = 1, endpoint_types = "continuous",
      n_arms = 2, n_repeated_measures = 0,
      par_names_inference = "b_arm2",
      target_params = "b_arm2"
    ),
    "sim_fn"
  )
})

# =============================================================================
# mock_design() HELPER
# =============================================================================

test_that("mock_design creates valid design object", {
  d <- mock_design()
  expect_s3_class(d, "rctbayespower::rctbp_design")
  expect_equal(d@backend, "brms")
  expect_equal(d@target_params, "b_arm2")
  expect_equal(d@trial_type, "fixed")
  expect_equal(d@n_arms, 2)
  expect_equal(d@n_endpoints, 1)
})

test_that("mock_design supports trial_type parameter", {
  d <- mock_design(trial_type = "group_sequential")
  expect_equal(d@trial_type, "group_sequential")
})

# =============================================================================
# PROPERTY GETTERS
# =============================================================================

test_that("par_names_sim returns formals for plain function", {
  d <- mock_design()
  params <- d@par_names_sim
  expect_true("n_total" %in% params)
  expect_true("p_alloc" %in% params)
})

test_that("par_names_sim returns sim_fn_params for rctbp_sim_fn", {
  sim_fn <- mock_rctbp_sim_fn()
  d <- mock_design(sim_fn = sim_fn)
  params <- d@par_names_sim
  expect_true("n_total" %in% params)
  expect_true("p_alloc" %in% params)
})

test_that("par_names_inference returns stored values", {
  d <- mock_design()
  expect_equal(d@par_names_inference, c("b_Intercept", "b_arm2", "b_covariate"))
})

# =============================================================================
# BACKEND SETTER
# =============================================================================

test_that("backend setter rejects invalid values", {
  d <- mock_design()
  expect_cli_abort(d@backend <- "invalid")
})

# =============================================================================
# TRIAL TYPE SETTER
# =============================================================================

test_that("trial_type setter rejects invalid values", {
  d <- mock_design()
  expect_cli_abort(d@trial_type <- "invalid_type")
})

test_that("trial_type setter accepts valid values", {
  d <- mock_design()
  d@trial_type <- "group_sequential"
  expect_equal(d@trial_type, "group_sequential")
  d@trial_type <- "adaptive"
  expect_equal(d@trial_type, "adaptive")
  d@trial_type <- "fixed"
  expect_equal(d@trial_type, "fixed")
})

# =============================================================================
# show_predefined_models()
# =============================================================================

test_that("show_predefined_models returns all models", {
  models <- show_predefined_models()
  expect_type(models, "character")
  expect_true(length(models) >= 2)
  expect_true("ancova_cont_2arms" %in% models)
  expect_true("ancova_cont_3arms" %in% models)
})

test_that("show_predefined_models filters correctly", {
  models <- show_predefined_models("2arms")
  expect_equal(models, "ancova_cont_2arms")

  models <- show_predefined_models("3arms")
  expect_equal(models, "ancova_cont_3arms")

  models <- show_predefined_models("nonexistent")
  expect_length(models, 0)
})

# =============================================================================
# build_design() CONSTRUCTOR
# =============================================================================

test_that("build_design rejects missing sim_fn and predefined_model", {
  expect_cli_abort(build_design(target_params = "b_arm2"))
})

test_that("build_design rejects missing inference_model", {
  expect_cli_abort(
    build_design(
      sim_fn = mock_sim_fn(),
      target_params = "b_arm2",
      n_endpoints = 1,
      endpoint_types = "continuous",
      n_arms = 2
    )
  )
})

test_that("build_design rejects non-function sim_fn", {
  expect_cli_abort(
    build_design(
      sim_fn = "not a function",
      inference_model = mock_brmsfit(),
      target_params = "b_arm2",
      n_endpoints = 1,
      endpoint_types = "continuous",
      n_arms = 2
    )
  )
})

test_that("build_design rejects invalid predefined_model", {
  expect_cli_abort(build_design(
    predefined_model = "nonexistent_model",
    target_params = "b_arm2"
  ))
})

test_that("build_design rejects non-character predefined_model", {
  expect_cli_abort(build_design(
    predefined_model = 42,
    target_params = "b_arm2"
  ))
})

test_that("build_design validates trial_type", {
  expect_cli_abort(
    build_design(
      sim_fn = mock_sim_fn(),
      inference_model = mock_brmsfit(),
      target_params = "b_arm2",
      n_endpoints = 1,
      endpoint_types = "continuous",
      n_arms = 2,
      trial_type = "invalid"
    )
  )
})

test_that("build_design validates backend matches model type", {
  expect_cli_abort(
    build_design(
      sim_fn = mock_sim_fn(),
      inference_model = list(not = "brmsfit"),
      backend = "brms",
      target_params = "b_arm2",
      n_endpoints = 1,
      endpoint_types = "continuous",
      n_arms = 2
    )
  )
})

# =============================================================================
# PRINT METHOD
# =============================================================================

test_that("print.rctbp_design produces output without error", {
  d <- mock_design()
  output <- capture_cli(print(d))
  expect_true(length(output) > 0)
})
