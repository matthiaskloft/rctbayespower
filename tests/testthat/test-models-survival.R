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

test_that("survival sim_fn is callable via test_args", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- do.call(sim_fn, sim_fn@test_args)
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 20)
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

test_that("survival sim_fn rejects non-positive hazard_ratio", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  expect_error(
    sim_fn(n_total = 20, baseline_hazard = 0.1, hazard_ratio = -1,
           accrual_rate = 10, followup_time = 12),
    "hazard_ratio"
  )
})

test_that("survival sim_fn rejects non-positive accrual_rate", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  expect_error(
    sim_fn(n_total = 20, baseline_hazard = 0.1, hazard_ratio = 0.7,
           accrual_rate = 0, followup_time = 12),
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

# =============================================================================
# SIM FN OUTPUT STRUCTURE
# =============================================================================

test_that("survival sim_fn returns correct data structure", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(n_total = 100, baseline_hazard = 0.1, hazard_ratio = 0.7,
                 accrual_rate = 10, followup_time = 12)

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
  data <- sim_fn(n_total = 200, baseline_hazard = 0.1, hazard_ratio = 0.7,
                 accrual_rate = 10, followup_time = 1)

  # With short followup, should have some censored (1) observations
  expect_true(any(data$censored == 1L))
  # And some events (0)
  expect_true(any(data$censored == 0L))
})

test_that("all events observed with Inf followup", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(n_total = 200, baseline_hazard = 0.1, hazard_ratio = 0.7,
                 accrual_rate = 10, followup_time = Inf)

  expect_equal(sum(data$censored), 0)
})

test_that("censoring increases with shorter followup", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data_long <- sim_fn(n_total = 500, baseline_hazard = 0.01, hazard_ratio = 0.7,
                      accrual_rate = 10, followup_time = 100)
  cens_long <- mean(data_long$censored)

  set.seed(42)
  data_short <- sim_fn(n_total = 500, baseline_hazard = 0.01, hazard_ratio = 0.7,
                       accrual_rate = 10, followup_time = 1)
  cens_short <- mean(data_short$censored)

  expect_gt(cens_short, cens_long)
})

test_that("hazard_ratio produces survival difference", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(n_total = 5000, baseline_hazard = 0.1, hazard_ratio = 0.5,
                 accrual_rate = 100, followup_time = 100)

  ctrl_times <- data$time[data$arm == "ctrl"]
  treat_times <- data$time[data$arm == "treat_1"]
  expect_gt(mean(treat_times), mean(ctrl_times))
})

test_that("enrollment_time is sorted and non-negative", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(n_total = 100, baseline_hazard = 0.1, hazard_ratio = 0.7,
                 accrual_rate = 5, followup_time = 12)

  expect_true(all(data$enrollment_time >= 0))
  expect_true(!is.unsorted(data$enrollment_time))
})

test_that("allocation respects p_alloc", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(n_total = 10000, p_alloc = c(0.7, 0.3),
                 baseline_hazard = 0.1, hazard_ratio = 0.7,
                 accrual_rate = 100, followup_time = 12)

  prop_ctrl <- mean(data$arm == "ctrl")
  expect_true(abs(prop_ctrl - 0.7) < 0.03)
})

# =============================================================================
# REGISTRY INTEGRATION (no brms compilation)
# =============================================================================

test_that("show_predefined_models includes survival_exp_2arms", {
  models <- show_predefined_models()
  expect_true("survival_exp_2arms" %in% models)
})

test_that("show_predefined_models filters survival models", {
  models <- show_predefined_models("survival")
  expect_equal(models, "survival_exp_2arms")
})

test_that("followup_time = 0 edge case: sim_fn runs", {
  sim_fn <- create_survival_exp_sim_fn(n_arms = 2)
  set.seed(42)
  data <- sim_fn(n_total = 20, baseline_hazard = 0.1, hazard_ratio = 0.7,
                 accrual_rate = 10, followup_time = 0)
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 20)
})

# =============================================================================
# BUILDER AND WRAPPER (requires brms compilation)
# =============================================================================

test_that("build_model_survival_exp returns valid rctbp_model", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("brms")
  skip("Survival model compilation — run manually")

  model <- build_model_survival_exp()
  expect_s3_class(model, "rctbayespower::rctbp_model")
  expect_equal(model@endpoint_types, "survival")
  expect_equal(model@n_arms, 2L)
})

test_that("build_model_survival_exp_2arms sets predefined_model", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("brms")
  skip("Survival model compilation — run manually")

  model <- build_model_survival_exp_2arms()
  expect_equal(model@predefined_model, "survival_exp_2arms")
})

test_that("build_design with survival_exp_2arms works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("brms")
  skip("Survival model compilation — run manually")

  design <- build_design(
    predefined_model = "survival_exp_2arms",
    target_params = "b_armtreat_1"
  )
  expect_s3_class(design, "rctbayespower::rctbp_design")
  expect_equal(design@endpoint_types, "survival")
  expect_true("b_armtreat_1" %in% design@par_names_inference)
})
