# =============================================================================
# Tests for Exponential Survival Model
# =============================================================================

# =============================================================================
# SIM FN FACTORY
# =============================================================================

test_that("create_survival_exp_sim_fn returns rctbp_sim_fn", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  expect_s3_class(sim_fn, "rctbayespower::rctbp_sim_fn")
})

test_that("create_survival_exp_sim_fn has correct test_args", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  ta <- sim_fn@test_args
  expect_equal(ta$n_total, 20L)
  expect_equal(ta$baseline_hazard, 0.1)
  expect_equal(ta$hazard_ratio, 0.7)
  expect_equal(ta$accrual_rate, 10)
  expect_equal(ta$followup_time, 12)
})

test_that("create_survival_exp_sim_fn is callable", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(
    n_total = 50,
    baseline_hazard = 0.1,
    hazard_ratio = 0.7,
    accrual_rate = 10,
    followup_time = 12
  )
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 50)
})

# =============================================================================
# SIM FN VALIDATION
# =============================================================================

test_that("survival sim_fn rejects NULL baseline_hazard", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  expect_error(
    sim_fn(n_total = 20, baseline_hazard = NULL, hazard_ratio = 0.7,
           accrual_rate = 10, followup_time = 12),
    "baseline_hazard"
  )
})

test_that("survival sim_fn rejects non-positive baseline_hazard", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  expect_error(
    sim_fn(n_total = 20, baseline_hazard = -1, hazard_ratio = 0.7,
           accrual_rate = 10, followup_time = 12),
    "baseline_hazard"
  )
})

test_that("survival sim_fn rejects NULL hazard_ratio", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  expect_error(
    sim_fn(n_total = 20, baseline_hazard = 0.1, hazard_ratio = NULL,
           accrual_rate = 10, followup_time = 12),
    "hazard_ratio"
  )
})

test_that("survival sim_fn rejects NULL accrual_rate", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  expect_error(
    sim_fn(n_total = 20, baseline_hazard = 0.1, hazard_ratio = 0.7,
           accrual_rate = NULL, followup_time = 12),
    "accrual_rate"
  )
})

test_that("survival sim_fn rejects negative followup_time", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  expect_error(
    sim_fn(n_total = 20, baseline_hazard = 0.1, hazard_ratio = 0.7,
           accrual_rate = 10, followup_time = -1),
    "followup_time"
  )
})

test_that("survival sim_fn rejects invalid n_total", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  expect_error(
    sim_fn(n_total = 0, baseline_hazard = 0.1, hazard_ratio = 0.7,
           accrual_rate = 10, followup_time = 12),
    "n_total"
  )
  expect_error(
    sim_fn(n_total = -5, baseline_hazard = 0.1, hazard_ratio = 0.7,
           accrual_rate = 10, followup_time = 12),
    "n_total"
  )
})

test_that("survival sim_fn rejects invalid p_alloc", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  expect_error(
    sim_fn(n_total = 20, p_alloc = c(0.5, 0.3), baseline_hazard = 0.1,
           hazard_ratio = 0.7, accrual_rate = 10, followup_time = 12),
    "p_alloc"
  )
  expect_error(
    sim_fn(n_total = 20, p_alloc = c(1), baseline_hazard = 0.1,
           hazard_ratio = 0.7, accrual_rate = 10, followup_time = 12),
    "p_alloc"
  )
})

test_that("create_survival_exp_sim_fn rejects n_arms != 2", {
  expect_error(
    create_survival_exp_sim_fn(n_arms = 3),
    "n_arms"
  )
})

# =============================================================================
# BUILDER VALIDATION
# =============================================================================

test_that("build_model_survival_exp rejects n_arms != 2", {
  expect_error(
    build_model_survival_exp(n_arms = 3),
    "n_arms"
  )
})

test_that("build_model_survival_exp rejects wrong p_alloc length", {
  expect_error(
    build_model_survival_exp(p_alloc = c(0.33, 0.33, 0.34)),
    "p_alloc"
  )
})

test_that("build_model_survival_exp rejects p_alloc not summing to 1", {
  expect_error(
    build_model_survival_exp(p_alloc = c(0.5, 0.3)),
    "p_alloc"
  )
})

# =============================================================================
# SIM FN OUTPUT
# =============================================================================

test_that("survival sim_fn returns correct data structure", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(
    n_total = 100,
    baseline_hazard = 0.1,
    hazard_ratio = 0.7,
    accrual_rate = 10,
    followup_time = 12
  )

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 100)
  expect_true(all(c("time", "censored", "arm", "enrollment_time") %in% names(data)))
  expect_true(is.numeric(data$time))
  expect_true(is.integer(data$censored))
  expect_true(is.factor(data$arm))
  expect_true(is.numeric(data$enrollment_time))
  expect_true(all(data$time > 0))
  expect_true(all(data$censored %in% c(0L, 1L)))
  expect_equal(levels(data$arm), c("ctrl", "treat_1"))
})

test_that("censored column follows brms convention (0=event, 1=censored)", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(
    n_total = 200,
    baseline_hazard = 0.01,
    hazard_ratio = 0.7,
    accrual_rate = 10,
    followup_time = 1
  )
  expect_true(any(data$censored == 0L))
  expect_true(any(data$censored == 1L))
  expect_true(all(data$censored %in% c(0L, 1L)))
})

test_that("all events observed with Inf followup", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(
    n_total = 200,
    baseline_hazard = 0.1,
    hazard_ratio = 0.7,
    accrual_rate = 10,
    followup_time = Inf
  )
  expect_equal(sum(data$censored), 0)
})

test_that("censoring increases with shorter followup", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data_long <- sim_fn(
    n_total = 500,
    baseline_hazard = 0.01,
    hazard_ratio = 0.7,
    accrual_rate = 10,
    followup_time = 100
  )
  cens_long <- mean(data_long$censored)

  set.seed(42)
  data_short <- sim_fn(
    n_total = 500,
    baseline_hazard = 0.01,
    hazard_ratio = 0.7,
    accrual_rate = 10,
    followup_time = 1
  )
  cens_short <- mean(data_short$censored)

  expect_gt(cens_short, cens_long)
})

test_that("hazard_ratio produces survival difference", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(
    n_total = 5000,
    baseline_hazard = 0.1,
    hazard_ratio = 0.5,
    accrual_rate = 100,
    followup_time = 100
  )

  ctrl_times <- data$time[data$arm == "ctrl"]
  treat_times <- data$time[data$arm == "treat_1"]
  expect_gt(mean(treat_times), mean(ctrl_times))
})

test_that("enrollment_time is sorted and non-negative", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(
    n_total = 100,
    baseline_hazard = 0.1,
    hazard_ratio = 0.7,
    accrual_rate = 5,
    followup_time = 12
  )

  expect_true(all(data$enrollment_time >= 0))
  expect_true(!is.unsorted(data$enrollment_time))
})

test_that("allocation respects p_alloc", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(
    n_total = 10000,
    p_alloc = c(0.7, 0.3),
    baseline_hazard = 0.1,
    hazard_ratio = 0.7,
    accrual_rate = 100,
    followup_time = 12
  )

  prop_ctrl <- mean(data$arm == "ctrl")
  expect_true(abs(prop_ctrl - 0.7) < 0.03)
})

test_that("followup_time = 0 edge case runs without error", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(
    n_total = 20,
    baseline_hazard = 0.1,
    hazard_ratio = 0.7,
    accrual_rate = 10,
    followup_time = 0
  )
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 20)
})

# =============================================================================
# PREDEFINED MODEL REGISTRY
# =============================================================================

test_that("show_predefined_models includes survival_exp_2arms", {
  models <- show_predefined_models()
  expect_true("survival_exp_2arms" %in% models)
})

test_that("show_predefined_models filter works for survival", {
  models <- show_predefined_models("survival")
  expect_equal(models, "survival_exp_2arms")
})

# =============================================================================
# BUILD MODEL INTEGRATION (requires brms compilation)
# =============================================================================

test_that("build_model_survival_exp returns valid rctbp_model", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if(
    !nzchar(Sys.getenv("RCTBP_TEST_STAN")),
    "Stan compilation — set RCTBP_TEST_STAN=1 to run"
  )

  model <- build_model_survival_exp(n_arms = 2, p_alloc = c(0.5, 0.5))
  expect_s3_class(model, "rctbayespower::rctbp_model")
  expect_equal(model@endpoint_types, "survival")
  expect_equal(model@n_arms, 2L)
  expect_equal(model@n_endpoints, 1L)
})

test_that("build_model_survival_exp_2arms sets predefined_model", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if(
    !nzchar(Sys.getenv("RCTBP_TEST_STAN")),
    "Stan compilation — set RCTBP_TEST_STAN=1 to run"
  )

  model <- build_model_survival_exp_2arms()
  expect_equal(model@predefined_model, "survival_exp_2arms")
})

test_that("registry round-trip: build_design with survival_exp_2arms", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if(
    !nzchar(Sys.getenv("RCTBP_TEST_STAN")),
    "Stan compilation — set RCTBP_TEST_STAN=1 to run"
  )

  design <- build_design(
    predefined_model = "survival_exp_2arms",
    target_params = "b_armtreat_1"
  )
  expect_s3_class(design, "rctbayespower::rctbp_design")
  expect_equal(design@endpoint_types, "survival")
  expect_equal(design@n_arms, 2L)
  expect_true("b_armtreat_1" %in% design@par_names_inference)
})

test_that("show_target_params returns b_armtreat_1 for survival", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if(
    !nzchar(Sys.getenv("RCTBP_TEST_STAN")),
    "Stan compilation — set RCTBP_TEST_STAN=1 to run"
  )

  params <- show_target_params("survival_exp_2arms")
  expect_true("b_armtreat_1" %in% params)
  expect_false("shape" %in% params)
})

test_that("show_condition_args shows survival-specific params", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if(
    !nzchar(Sys.getenv("RCTBP_TEST_STAN")),
    "Stan compilation — set RCTBP_TEST_STAN=1 to run"
  )

  design <- build_design(
    predefined_model = "survival_exp_2arms",
    target_params = "b_armtreat_1"
  )
  args <- show_condition_args(design, print = FALSE)
  required_names <- args$arg[args$type == "required"]
  expect_true("baseline_hazard" %in% required_names)
  expect_true("hazard_ratio" %in% required_names)
  expect_true("accrual_rate" %in% required_names)
  expect_true("followup_time" %in% required_names)
})

test_that("build_conditions with dual-route params works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if(
    !nzchar(Sys.getenv("RCTBP_TEST_STAN")),
    "Stan compilation — set RCTBP_TEST_STAN=1 to run"
  )

  design <- build_design(
    predefined_model = "survival_exp_2arms",
    target_params = "b_armtreat_1"
  )
  conditions <- build_conditions(
    design,
    n_total = 100,
    constant = list(
      baseline_hazard = 0.1,
      hazard_ratio = 0.7,
      accrual_rate = 10,
      followup_time = 12
    ),
    thr_dec_eff = 0.975,
    thr_fx_eff = 0,
    analysis_at = 100
  )
  expect_s3_class(conditions, "rctbayespower::rctbp_conditions")
})

test_that("end-to-end: design + conditions + sim_fn produces valid data", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if(
    !nzchar(Sys.getenv("RCTBP_TEST_STAN")),
    "Stan compilation — set RCTBP_TEST_STAN=1 to run"
  )

  design <- build_design(
    predefined_model = "survival_exp_2arms",
    target_params = "b_armtreat_1"
  )
  set.seed(42)
  data <- design@sim_fn(
    n_total = 50,
    baseline_hazard = 0.1,
    hazard_ratio = 0.7,
    accrual_rate = 10,
    followup_time = 12
  )
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 50)
  expect_true(all(c("time", "censored", "arm") %in% names(data)))
})
