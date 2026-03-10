# test-event-driven.R
# Tests for event-driven subsetting (Session 6b)

# =============================================================================
# subset_by_events
# =============================================================================

make_survival_data <- function(n = 100, seed = 42) {
  set.seed(seed)
  enrollment_time <- sort(runif(n, 0, 10))
  event_time <- rexp(n, rate = 0.1)
  max_cal <- max(enrollment_time) + 12
  admin_censor <- max_cal - enrollment_time
  time <- pmin(event_time, admin_censor)
  censored <- as.integer(event_time > admin_censor)

  data.frame(
    time = time,
    censored = censored,
    arm = factor(rep(c("ctrl", "treat_1"), each = n / 2)),
    enrollment_time = enrollment_time
  )
}

test_that("subset_by_events returns correct subset for target events", {
  data <- make_survival_data(n = 200)

  # Count total events
  total_events <- sum(data$censored == 0)
  target <- min(total_events, 50)

  result <- subset_by_events(data, target_events = target)

  # Should have n_events attribute = target
  expect_equal(attr(result, "n_events"), target)
  expect_true(!is.null(attr(result, "calendar_time")))
  expect_false("enrollment_time" %in% names(result))
})

test_that("subset_by_events: event count matches target", {
  data <- make_survival_data(n = 500, seed = 123)
  total_events <- sum(data$censored == 0)
  target <- min(total_events, 30)

  result <- subset_by_events(data, target_events = target)

  # The number of events in result should equal target
  n_events <- sum(result$censored == 0)
  expect_equal(n_events, target)
})

test_that("subset_by_events handles target_not_met", {
  data <- make_survival_data(n = 50)
  total_events <- sum(data$censored == 0)

  # Request more events than available
  result <- subset_by_events(data, target_events = total_events + 100)

  expect_true(attr(result, "target_not_met"))
  expect_equal(attr(result, "n_events"), total_events)
})

test_that("subset_by_events handles zero events", {
  # All censored
  data <- data.frame(
    time = rep(1, 10),
    censored = rep(1L, 10),
    arm = factor(rep("ctrl", 10)),
    enrollment_time = seq(0, 9)
  )

  result <- subset_by_events(data, target_events = 5)

  expect_equal(nrow(result), 0)
  expect_true(attr(result, "target_not_met"))
  expect_equal(attr(result, "n_events"), 0L)
})

test_that("subset_by_events censors patients who haven't had events yet", {
  data <- make_survival_data(n = 200)
  total_events <- sum(data$censored == 0)
  target <- max(1, floor(total_events / 2))

  result <- subset_by_events(data, target_events = target)

  # Some patients in the result should be censored (they were enrolled but

  # their event hasn't occurred yet at the analysis calendar time)
  expect_true(sum(result$censored == 1) > 0)
})

test_that("subset_by_events requires survival columns", {
  data <- data.frame(outcome = rnorm(10), arm = factor(rep("ctrl", 10)))

  expect_error(
    subset_by_events(data, target_events = 5),
    "enrollment_time"
  )
})

# =============================================================================
# Integration with subset_analysis_data
# =============================================================================

test_that("subset_analysis_data dispatches to event-driven when analysis_timing = 'events'", {
  data <- make_survival_data(n = 200)
  total_events <- sum(data$censored == 0)
  target <- min(total_events, 30)

  result <- subset_analysis_data(
    data, current_n = target,
    analysis_timing = "events"
  )

  # n_events should equal target exactly (re-censoring preserves events at the
  # cutoff boundary via the 1e-10 floating-point tolerance)
  expect_equal(attr(result, "n_events"), target)
  expect_false("enrollment_time" %in% names(result))
})

test_that("subset_analysis_data defaults to sample_size timing", {
  # Non-survival data: should use the original sample_size path
  data <- data.frame(
    outcome = rnorm(100),
    arm = factor(rep(0:1, 50))
  )

  result <- subset_analysis_data(data, current_n = 50)
  expect_equal(nrow(result), 50)
})

# =============================================================================
# Worker function enrollment time preservation
# =============================================================================

test_that("worker_functions skips enrollment_time generation for survival data", {
  # Survival sim_fn returns enrollment_time as part of its output
  sim_fn <- function(n_total, p_alloc = c(0.5, 0.5), accrual_rate = 10,
                     baseline_hazard = 0.1, hazard_ratio = 0.7,
                     followup_time = 12) {
    enrollment_time <- generate_enrollment_times(n_total, accrual_rate)
    data.frame(
      time = rexp(n_total, rate = 0.1),
      censored = 0L,
      arm = factor(sample(c("ctrl", "treat_1"), n_total, replace = TRUE)),
      enrollment_time = enrollment_time
    )
  }

  # Simulate what worker_functions does
  full_data <- sim_fn(n_total = 100)
  original_enrollment <- full_data$enrollment_time

  # The condition: accrual_rate is set but data already has enrollment_time
  accrual_rate <- 10
  if (!is.null(accrual_rate) && !"enrollment_time" %in% names(full_data)) {
    full_data$enrollment_time <- generate_enrollment_times(
      nrow(full_data), accrual_rate
    )
  }

  # enrollment_time should NOT be overwritten
  expect_equal(full_data$enrollment_time, original_enrollment)
})

# =============================================================================
# Re-censoring correctness
# =============================================================================

test_that("re-censored patients have time == calendar_cutoff - enrollment_time", {
  # Create data where some events occur after the analysis cutoff
  data <- make_survival_data(n = 200, seed = 123)
  # Tag rows to track through subsetting (survives subset_by_events)
  data$.row_id <- seq_len(nrow(data))

  total_events <- sum(data$censored == 0)
  target <- max(1, floor(total_events / 2))

  result <- subset_by_events(data, target_events = target, followup_time = 0)
  cal_time <- attr(result, "calendar_time")

  # subset_by_events removes enrollment_time, so look up originals via .row_id
  for (i in seq_len(nrow(result))) {
    orig_row <- data[result$.row_id[i], ]
    enroll_t <- orig_row$enrollment_time
    event_cal_time <- enroll_t + orig_row$time
    was_event <- orig_row$censored == 0

    if (was_event && event_cal_time <= cal_time + 1e-10) {
      # Event should be preserved
      expect_equal(result$censored[i], 0L)
      expect_equal(result$time[i], orig_row$time, tolerance = 1e-10)
    } else {
      # Should be re-censored at cutoff
      expect_equal(result$censored[i], 1L)
      expected_time <- cal_time - enroll_t
      expect_equal(result$time[i], expected_time, tolerance = 1e-10)
    }
  }
})
