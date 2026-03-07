# Tests for utility functions

# =============================================================================
# format_duration() (from class_power_analysis.R)
# =============================================================================

test_that("format_duration handles seconds", {
  expect_match(rctbayespower:::format_duration(0.5), "sec")
})

test_that("format_duration handles minutes", {
  expect_match(rctbayespower:::format_duration(5), "min")
})

test_that("format_duration handles hours", {
  expect_match(rctbayespower:::format_duration(120), "hr|hour")
})

# =============================================================================
# get_cpu_info() (from class_power_analysis.R)
# =============================================================================

test_that("get_cpu_info returns character string or NULL", {
  result <- rctbayespower:::get_cpu_info()
  if (!is.null(result)) {
    expect_type(result, "character")
    expect_true(nchar(result) > 0)
  } else {
    expect_null(result)
  }
})
