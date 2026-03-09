# =============================================================================
# dropout() constructor
# =============================================================================

test_that("dropout() with proportion creates valid object", {
  d <- dropout(0.2)
  expect_s3_class(d, "rctbp_dropout")
  expect_true(is.function(d))
  expect_equal(attr(d, "dropout_params")$type, "proportion")
  expect_equal(attr(d, "dropout_params"), list(rate = 0.2, type = "proportion"))
})

test_that("dropout() with hazard creates valid object", {
  d <- dropout(0.03, type = "hazard")
  expect_s3_class(d, "rctbp_dropout")
  expect_equal(attr(d, "dropout_params")$type, "hazard")
  expect_equal(attr(d, "dropout_params"), list(rate = 0.03, type = "hazard"))
})

test_that("dropout() proportion converts to correct hazard", {
  d <- dropout(0.2)
  # hazard = -log(1 - 0.2) / followup_time
  expected_hazard <- -log(1 - 0.2) / 12
  expect_equal(d(12), expected_hazard)
})

test_that("dropout() hazard returns rate directly regardless of followup_time", {
  d <- dropout(0.03, type = "hazard")
  expect_equal(d(12), 0.03)
  expect_equal(d(24), 0.03)
  expect_equal(d(1), 0.03)
})

test_that("dropout() vectorized creates list of objects", {
  result <- dropout(c(0.1, 0.2, 0.3))
  expect_type(result, "list")
  expect_length(result, 3)
  for (d in result) {
    expect_s3_class(d, "rctbp_dropout")
  }
  expect_equal(attr(result[[1]], "dropout_params")$rate, 0.1)
  expect_equal(attr(result[[3]], "dropout_params")$rate, 0.3)
})

test_that("dropout() recycles type to match rate length", {
  result <- dropout(c(0.2, 0.1), type = c("proportion", "hazard"))
  expect_equal(attr(result[[1]], "dropout_params")$type, "proportion")
  expect_equal(attr(result[[2]], "dropout_params")$type, "hazard")
})

test_that("dropout() validates proportion range", {
  expect_error(dropout(0), "between 0 and 1")
  expect_error(dropout(1), "between 0 and 1")
  expect_error(dropout(-0.1), "between 0 and 1")
  expect_error(dropout(1.5), "between 0 and 1")
})

test_that("dropout() validates hazard positivity", {
  expect_error(dropout(0, type = "hazard"), "must be positive")
  expect_error(dropout(-0.1, type = "hazard"), "must be positive")
})

test_that("dropout() validates invalid type", {
  expect_error(dropout(0.2, type = "invalid"), "proportion.*hazard")
})

test_that("dropout() validates non-numeric rate", {
  expect_error(dropout("high"), "numeric")
  expect_error(dropout(NA_real_), "non-missing")
})

test_that("dropout() callable validates followup_time", {
  d <- dropout(0.2)
  expect_error(d(0), "followup_time.*positive")
  expect_error(d(-1), "followup_time.*positive")
  expect_error(d(NA), "followup_time.*positive")
})

test_that("format.rctbp_dropout produces correct string", {
  d <- dropout(0.2)
  formatted <- format(d)
  expect_match(formatted, "dropout.*rate.*0.2.*proportion")
})

test_that("format.rctbp_dropout works", {
  d <- dropout(0.03, type = "hazard")
  expect_match(format(d), "dropout.*rate.*0.03.*hazard")
})


# =============================================================================
# generate_dropout_times()
# =============================================================================

test_that("generate_dropout_times returns correct length", {
  set.seed(42)
  times <- generate_dropout_times(100, hazard_rate = 0.05)
  expect_length(times, 100)
  expect_true(all(times > 0))
})

test_that("generate_dropout_times produces exponential distribution", {
  set.seed(42)
  times <- generate_dropout_times(10000, hazard_rate = 0.1)
  # Mean of Exp(0.1) should be 1/0.1 = 10
  expect_equal(mean(times), 10, tolerance = 0.5)
})


# =============================================================================
# subset_analysis_data() with dropout
# =============================================================================

test_that("subset_analysis_data excludes dropped patients", {
  # 10 patients with enrollment times
  full_data <- data.frame(
    y = 1:10,
    enrollment_time = 0:9
  )
  followup_time <- 5

  # All patients complete (dropout_time > followup_time)
  full_data$dropout_time <- rep(100, 10)
  result_no_drop <- subset_analysis_data(full_data, current_n = 5,
                                          followup_time = followup_time)
  expect_equal(nrow(result_no_drop), 5)
  expect_equal(attr(result_no_drop, "n_dropped"), 0L)

  # First 3 patients drop out immediately (enrollment_time 0, 1, 2)
  # Patients 4-10 (enrollment_time 3-9) complete normally
  # Request current_n = 5 completers: patients 4-8 (enroll at 3-7, complete at 8-12)
  # At calendar_time = 12 (5th completer), patients 0-2 have time_mask=TRUE but dropped
  full_data$dropout_time <- c(rep(0, 3), rep(100, 7))
  result_with_drop <- subset_analysis_data(full_data, current_n = 5,
                                            followup_time = followup_time)
  expect_equal(nrow(result_with_drop), 5)
  expect_equal(attr(result_with_drop, "n_dropped"), 3L)
  expect_false("enrollment_time" %in% names(result_with_drop))
  expect_false("dropout_time" %in% names(result_with_drop))
})

test_that("subset_analysis_data handles all-dropout gracefully", {
  # 10 patients, all drop out
  full_data <- data.frame(
    y = 1:10,
    enrollment_time = 0:9,
    dropout_time = rep(0.001, 10)  # All drop out almost immediately
  )
  result <- subset_analysis_data(full_data, current_n = 5, followup_time = 5)
  expect_true(isTRUE(attr(result, "target_not_met")))
  expect_equal(nrow(result), 0)
  expect_equal(attr(result, "n_enrolled"), 10L)
  expect_equal(attr(result, "n_dropped"), 10L)
})

test_that("subset_analysis_data handles partial target_not_met", {
  # 10 patients, 3 complete (patients 4, 5, 6 at enrollment_time 3, 4, 5)
  # Request current_n = 5 but only 3 completers available
  full_data <- data.frame(
    y = 1:10,
    enrollment_time = 0:9,
    dropout_time = c(rep(0, 4), rep(100, 3), rep(0, 3))
  )
  result <- subset_analysis_data(full_data, current_n = 5, followup_time = 5)
  expect_true(isTRUE(attr(result, "target_not_met")))
  expect_equal(nrow(result), 3)
  expect_true(attr(result, "n_dropped") > 0)
})

test_that("subset_analysis_data with dropout=0 equivalent to no dropout", {
  full_data <- data.frame(
    y = 1:10,
    enrollment_time = seq(0, 9, by = 1)
  )
  followup_time <- 5

  # No dropout
  result_no <- subset_analysis_data(full_data, current_n = 5,
                                     followup_time = followup_time)

  # Dropout time > followup = effectively no dropout
  full_data2 <- full_data
  full_data2$dropout_time <- rep(1000, 10)
  result_with <- subset_analysis_data(full_data2, current_n = 5,
                                       followup_time = followup_time)

  expect_equal(nrow(result_no), nrow(result_with))
  expect_equal(result_no$y, result_with$y)
  expect_equal(attr(result_no, "calendar_time"),
               attr(result_with, "calendar_time"))
})


# =============================================================================
# validate_accrual_params() with dropout
# =============================================================================

test_that("validate_accrual_params requires accrual_rate with dropout", {
  d <- dropout(0.2)
  expect_error(
    validate_accrual_params(dropout = d, followup_time = 12),
    "accrual_rate.*required"
  )
})

test_that("validate_accrual_params requires positive followup_time with dropout", {
  d <- dropout(0.2)
  expect_error(
    validate_accrual_params(dropout = d, accrual_rate = 5, followup_time = 0),
    "followup_time.*positive"
  )
})

test_that("validate_accrual_params accepts valid dropout spec", {
  d <- dropout(0.2)
  expect_invisible(
    validate_accrual_params(dropout = d, accrual_rate = 5, followup_time = 12)
  )
})

test_that("validate_accrual_params rejects non-rctbp_dropout objects", {
  expect_error(
    validate_accrual_params(dropout = 0.2, accrual_rate = 5, followup_time = 12),
    "rctbp_dropout"
  )
})
