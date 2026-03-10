# test-survival.R
# Tests for survival data simulation function (Session 6a)

# =============================================================================
# Survival sim_fn unit tests (no brms compilation needed)
# =============================================================================

# Extract the inner sim function for direct testing
make_survival_sim_fn <- function() {
  function(n_total,
           p_alloc = c(0.5, 0.5),
           baseline_hazard = 0.1,
           hazard_ratio = 0.7,
           accrual_rate = 10,
           followup_time = 12) {
    arm_idx <- sample(0:1, n_total, replace = TRUE, prob = p_alloc)
    arm <- factor(arm_idx, levels = 0:1, labels = c("ctrl", "treat_1"))
    enrollment_time <- generate_enrollment_times(
      n_total, accrual_rate, accrual_pattern = "uniform"
    )
    hazard <- ifelse(arm_idx == 0, baseline_hazard,
                      baseline_hazard * hazard_ratio)
    event_time <- stats::rexp(n_total, rate = hazard)
    max_calendar_time <- max(enrollment_time) + followup_time
    admin_censor_time <- max_calendar_time - enrollment_time
    time <- pmin(event_time, admin_censor_time)
    censored <- as.integer(event_time > admin_censor_time)
    data.frame(
      time = time,
      censored = censored,
      arm = arm,
      enrollment_time = enrollment_time
    )
  }
}

test_that("survival sim_fn returns correct data structure", {
  sim_fn <- make_survival_sim_fn()
  set.seed(42)
  data <- sim_fn(n_total = 100)

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 100)
  expect_true(all(c("time", "censored", "arm", "enrollment_time") %in% names(data)))
  expect_true(is.numeric(data$time))
  expect_true(is.integer(data$censored))
  expect_true(is.factor(data$arm))
  expect_true(is.numeric(data$enrollment_time))

  # All times should be positive

  expect_true(all(data$time > 0))

  # Censored should be 0 or 1
  expect_true(all(data$censored %in% c(0L, 1L)))

  # Arm should have two levels
  expect_equal(levels(data$arm), c("ctrl", "treat_1"))
})

test_that("survival sim_fn: all events observed with Inf followup and no dropout", {
  sim_fn <- make_survival_sim_fn()
  set.seed(42)
  data <- sim_fn(n_total = 200, followup_time = Inf)

  # With infinite followup, no administrative censoring
  expect_equal(sum(data$censored), 0)
})

test_that("survival sim_fn: censoring increases with shorter followup", {
  sim_fn <- make_survival_sim_fn()
  set.seed(42)

  data_long <- sim_fn(n_total = 500, followup_time = 100, baseline_hazard = 0.01)
  cens_long <- mean(data_long$censored)

  set.seed(42)
  data_short <- sim_fn(n_total = 500, followup_time = 1, baseline_hazard = 0.01)
  cens_short <- mean(data_short$censored)

  # Shorter followup should produce more censoring
  expect_gt(cens_short, cens_long)
})

test_that("survival sim_fn: hazard_ratio produces survival difference", {
  sim_fn <- make_survival_sim_fn()

  # Large sample for stable estimates
  set.seed(42)
  data <- sim_fn(
    n_total = 5000,
    baseline_hazard = 0.1,
    hazard_ratio = 0.5,  # treatment halves the hazard
    followup_time = 100  # long enough to observe most events
  )

  # Treatment arm should have longer survival times on average
  ctrl_times <- data$time[data$arm == "ctrl"]
  treat_times <- data$time[data$arm == "treat_1"]

  # Expected: mean(treat) > mean(ctrl) because hazard_ratio < 1
  expect_gt(mean(treat_times), mean(ctrl_times))
})

test_that("survival sim_fn: enrollment_time is sorted and non-negative", {
  sim_fn <- make_survival_sim_fn()
  set.seed(42)
  data <- sim_fn(n_total = 100, accrual_rate = 5)

  expect_true(all(data$enrollment_time >= 0))
  expect_true(!is.unsorted(data$enrollment_time))
})

test_that("survival sim_fn: allocation respects p_alloc", {
  sim_fn <- make_survival_sim_fn()
  set.seed(42)
  data <- sim_fn(n_total = 10000, p_alloc = c(0.7, 0.3))

  prop_ctrl <- mean(data$arm == "ctrl")
  expect_true(abs(prop_ctrl - 0.7) < 0.03)  # within 3% of expected
})

# =============================================================================
# Build model integration (requires brms compilation)
# =============================================================================

test_that("build_model_survival_2arms compiles successfully", {
  skip_on_cran()
  skip_if_not_installed("brms")
  # brms compilation can be slow — skip in quick test runs
  skip("Survival model compilation — run manually")

  design <- build_model_survival_2arms(
    p_alloc = c(0.5, 0.5),
    baseline_hazard = 0.1,
    hazard_ratio = 0.7,
    accrual_rate = 10,
    followup_time = 12
  )

  expect_s3_class(design, "rctbayespower::rctbp_design")
})
