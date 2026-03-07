# =============================================================================
# generate_enrollment_times()
# =============================================================================

test_that("uniform enrollment produces correct length and spacing", {
  times <- generate_enrollment_times(100, accrual_rate = 10)
  expect_length(times, 100)
  expect_equal(times[1], 0)
  expect_equal(diff(times), rep(0.1, 99))
})

test_that("poisson enrollment produces correct length and is sorted", {
  set.seed(42)
  times <- generate_enrollment_times(100, accrual_rate = 10,
                                      accrual_pattern = "poisson")
  expect_length(times, 100)
  expect_false(is.unsorted(times))
  expect_gt(times[1], 0)
})

test_that("ramp enrollment produces correct length and is sorted", {
  times <- generate_enrollment_times(100, accrual_rate = 10,
                                      accrual_pattern = "ramp")
  expect_length(times, 100)
  expect_false(is.unsorted(times))
  expect_equal(times[1], 0)
  expect_gt(times[2] - times[1], times[100] - times[99])
})

test_that("custom accrual_pattern function works", {
  custom_fn <- function(n, rate) seq(0, n / rate, length.out = n)
  times <- generate_enrollment_times(50, accrual_rate = 5,
                                      accrual_pattern = custom_fn)
  expect_length(times, 50)
  expect_false(is.unsorted(times))
})

test_that("custom accrual_pattern with wrong length aborts", {
  bad_fn <- function(n, rate) c(1, 2, 3)
  expect_error(
    generate_enrollment_times(50, accrual_rate = 5, accrual_pattern = bad_fn),
    "must return 50 values"
  )
})

test_that("single patient enrollment works for all patterns", {
  for (pattern in c("uniform", "poisson", "ramp")) {
    set.seed(1)
    times <- generate_enrollment_times(1, accrual_rate = 10,
                                        accrual_pattern = pattern)
    expect_length(times, 1)
  }
})

test_that("unknown accrual_pattern string aborts", {
  expect_error(
    generate_enrollment_times(50, accrual_rate = 5, accrual_pattern = "linear"),
    "Unknown"
  )
})


# =============================================================================
# patients_with_data() and patients_enrolled()
# =============================================================================

test_that("patients_with_data returns correct mask with followup", {
  enrollment <- c(0, 1, 2, 3, 4)
  mask <- patients_with_data(enrollment, followup_time = 2, calendar_time = 4)
  expect_equal(mask, c(TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("patients_enrolled returns correct mask", {
  enrollment <- c(0, 1, 2, 3, 4)
  mask <- patients_enrolled(enrollment, calendar_time = 2.5)
  expect_equal(mask, c(TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("enrolled but unfinished patients are correctly identified", {
  enrollment <- c(0, 1, 2, 3, 4)
  enrolled <- patients_enrolled(enrollment, calendar_time = 4)
  analyzable <- patients_with_data(enrollment, followup_time = 2, calendar_time = 4)
  in_followup <- enrolled & !analyzable

  expect_equal(sum(enrolled), 5)
  expect_equal(sum(analyzable), 3)
  expect_equal(in_followup, c(FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("followup_time = 0 means all enrolled patients are analyzable", {
  enrollment <- c(0, 1, 2, 3, 4)
  calendar_time <- 3

  enrolled <- patients_enrolled(enrollment, calendar_time)
  analyzable <- patients_with_data(enrollment, followup_time = 0,
                                    calendar_time = calendar_time)
  expect_equal(enrolled, analyzable)
})

test_that("before trial starts, no one is enrolled or analyzable", {
  enrollment <- c(0, 1, 2)
  expect_equal(sum(patients_enrolled(enrollment, calendar_time = -1)), 0)
  expect_equal(sum(patients_with_data(enrollment, 0, calendar_time = -1)), 0)
})

test_that("single patient edge case works", {
  enrollment <- c(0)
  expect_true(patients_enrolled(enrollment, calendar_time = 0))
  expect_true(patients_with_data(enrollment, followup_time = 0, calendar_time = 0))
  expect_false(patients_with_data(enrollment, followup_time = 5, calendar_time = 3))
})

test_that("large followup means all enrolled but none analyzable", {
  enrollment <- c(0, 1, 2, 3, 4)
  calendar_time <- 4
  followup_time <- 100

  enrolled <- patients_enrolled(enrollment, calendar_time)
  analyzable <- patients_with_data(enrollment, followup_time, calendar_time)

  expect_equal(sum(enrolled), 5)
  expect_equal(sum(analyzable), 0)
})


# =============================================================================
# calendar_to_available_n()
# =============================================================================

test_that("calendar_to_available_n returns correct breakdown", {
  enrollment <- c(0, 1, 2, 3, 4)
  followup <- 2
  cal_times <- c(2, 4, 6)

  result <- calendar_to_available_n(cal_times, enrollment, followup)

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("calendar_time", "n_analyzable",
                                 "n_enrolled", "n_in_followup"))
  expect_equal(result$calendar_time, cal_times)

  expect_equal(result$n_analyzable, c(1L, 3L, 5L))
  expect_equal(result$n_enrolled, c(3L, 5L, 5L))
  expect_equal(result$n_in_followup, c(2L, 2L, 0L))
})

test_that("n_in_followup always equals n_enrolled - n_analyzable", {
  enrollment <- generate_enrollment_times(100, accrual_rate = 10)
  followup <- 3
  cal_times <- seq(1, 15, by = 1)

  result <- calendar_to_available_n(cal_times, enrollment, followup)
  expect_equal(result$n_in_followup, result$n_enrolled - result$n_analyzable)
})


# =============================================================================
# validate_accrual_params()
# =============================================================================

test_that("validate_accrual_params accepts valid inputs", {
  expect_true(validate_accrual_params())
  expect_true(validate_accrual_params(accrual_rate = 10))
  expect_true(validate_accrual_params(followup_time = 0))
  expect_true(validate_accrual_params(followup_time = 6))
  expect_true(validate_accrual_params(analysis_timing = "sample_size"))
  expect_true(validate_accrual_params(analysis_timing = "calendar",
                                       accrual_rate = 10,
                                       calendar_analysis_at = c(12, 24)))
})

test_that("validate_accrual_params rejects invalid accrual_rate", {
  expect_error(validate_accrual_params(accrual_rate = -1), "positive")
  expect_error(validate_accrual_params(accrual_rate = 0), "positive")
  expect_error(validate_accrual_params(accrual_rate = "fast"), "positive")
})

test_that("validate_accrual_params rejects invalid followup_time", {
  expect_error(validate_accrual_params(followup_time = -1), "non-negative")
})

test_that("validate_accrual_params rejects invalid analysis_timing", {
  expect_error(validate_accrual_params(analysis_timing = "events"), "must be one of")
})

test_that("validate_accrual_params rejects non-increasing calendar_analysis_at", {
  expect_error(validate_accrual_params(calendar_analysis_at = c(24, 12)),
               "strictly increasing")
  expect_error(validate_accrual_params(calendar_analysis_at = c(12, 12)),
               "strictly increasing")
})

test_that("validate_accrual_params requires accrual_rate for calendar timing", {
  expect_error(
    validate_accrual_params(analysis_timing = "calendar",
                             calendar_analysis_at = c(12, 24)),
    "accrual_rate.*required"
  )
})

test_that("validate_accrual_params requires calendar_analysis_at for calendar timing", {
  expect_error(
    validate_accrual_params(analysis_timing = "calendar",
                             accrual_rate = 10),
    "calendar_analysis_at.*required"
  )
})


# =============================================================================
# Accrual-aware subsetting (integration with patients_with_data)
# =============================================================================

test_that("accrual-aware subsetting selects correct patients", {
  # Simulate a scenario: 100 patients, accrual_rate=10, followup=2
  enrollment <- generate_enrollment_times(100, accrual_rate = 10)
  followup <- 2

  # At the point where 50 patients have completed follow-up:
  completion_times <- sort(enrollment + followup)
  calendar_time <- completion_times[50]
  mask <- patients_with_data(enrollment, followup, calendar_time)

  # Exactly 50 patients should be analyzable
  expect_equal(sum(mask), 50)

  # All analyzable patients enrolled early enough
  expect_true(all(enrollment[mask] + followup <= calendar_time))
})

test_that("accrual subsetting with followup=0 equals row-index subsetting", {
  enrollment <- generate_enrollment_times(100, accrual_rate = 10)
  followup <- 0

  # With followup=0, the nth patient to complete is the nth to enroll
  completion_times <- sort(enrollment + followup)
  calendar_time <- completion_times[50]
  mask <- patients_with_data(enrollment, followup, calendar_time)

  expect_equal(sum(mask), 50)
})
