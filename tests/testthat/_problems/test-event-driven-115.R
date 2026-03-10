# Extracted from test-event-driven.R:115

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
data <- make_survival_data(n = 200)
total_events <- sum(data$censored == 0)
target <- min(total_events, 30)
result <- subset_analysis_data(
    data, current_n = target,
    analysis_timing = "events"
  )
expect_equal(attr(result, "n_events"), target)
