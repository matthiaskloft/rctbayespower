# Extracted from test-event-driven.R:188

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
data <- make_survival_data(n = 200, seed = 123)
total_events <- sum(data$censored == 0)
target <- max(1, floor(total_events / 2))
result <- subset_by_events(data, target_events = target, followup_time = 0)
cal_time <- attr(result, "calendar_time")
for (i in seq_len(nrow(result))) {
    orig_idx <- which(abs(data$enrollment_time - result$enrollment_time[i]) < 1e-12)
    stopifnot(length(orig_idx) == 1)

    event_cal_time <- data$enrollment_time[orig_idx] + data$time[orig_idx]
    was_event <- data$censored[orig_idx] == 0

    if (was_event && event_cal_time <= cal_time + 1e-10) {
      # Event should be preserved
      expect_equal(result$censored[i], 0L)
      expect_equal(result$time[i], data$time[orig_idx], tolerance = 1e-10)
    } else {
      # Should be re-censored at cutoff
      expect_equal(result$censored[i], 1L)
      expected_time <- cal_time - result$enrollment_time[i]
      expect_equal(result$time[i], expected_time, tolerance = 1e-10)
    }
  }
