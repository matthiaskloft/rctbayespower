# Tests for as.data.frame() methods

# =============================================================================
# rctbp_power_analysis
# =============================================================================

test_that("as.data.frame returns results_conditions by default", {
  pa <- mock_power_analysis("1d")
  result <- as.data.frame(pa)
  expect_s3_class(result, "data.frame")
  expect_identical(result, pa@results_conditions)
})

test_that("as.data.frame returns results_raw with what = 'raw'", {
  pa <- mock_power_analysis("1d")
  result <- as.data.frame(pa, what = "raw")
  expect_identical(result, pa@results_raw)
})

test_that("as.data.frame returns results_interim with what = 'interim'", {
  pa <- mock_power_analysis("accrual")
  result <- as.data.frame(pa, what = "interim")
  expect_identical(result, pa@results_interim)
  expect_true(nrow(result) > 0)
})

test_that("as.data.frame errors for interim on fixed design", {
  pa <- mock_power_analysis("1d")
  expect_error(
    as.data.frame(pa, what = "interim"),
    "Interim results are not available",
    class = "rlang_error"
  )
})

test_that("as.data.frame errors for invalid what value", {
  pa <- mock_power_analysis("1d")
  expect_error(
    as.data.frame(pa, what = "invalid"),
    "'arg' should be one of"
  )
})

test_that("as.data.frame default matches what = 'conditions'", {
  pa <- mock_power_analysis("2d")
  expect_identical(as.data.frame(pa), as.data.frame(pa, what = "conditions"))
})

# =============================================================================
# rctbp_conditions
# =============================================================================

test_that("as.data.frame returns grid for conditions", {
  cond <- mock_conditions()
  result <- as.data.frame(cond)
  expect_s3_class(result, "data.frame")
  expect_identical(result, cond@grid)
})

# =============================================================================
# rctbp_pareto_result
# =============================================================================

test_that("as.data.frame returns pareto_front by default", {
  pr <- mock_pareto_result()
  result <- as.data.frame(pr)
  expect_s3_class(result, "data.frame")
  expect_identical(result, pr@pareto_front)
})

test_that("as.data.frame returns archive with what = 'archive'", {
  pr <- mock_pareto_result()
  result <- as.data.frame(pr, what = "archive")
  expect_identical(result, pr@archive)
})

test_that("as.data.frame returns selected_design with what = 'selected'", {
  pr <- mock_pareto_result()
  result <- as.data.frame(pr, what = "selected")
  expect_identical(result, pr@selected_design)
})

test_that("as.data.frame returns convergence with what = 'convergence'", {
  pr <- mock_pareto_result()
  result <- as.data.frame(pr, what = "convergence")
  expect_identical(result, pr@convergence)
})

test_that("as.data.frame pareto_result errors for invalid what", {
  pr <- mock_pareto_result()
  expect_error(
    as.data.frame(pr, what = "invalid"),
    "'arg' should be one of"
  )
})

test_that("as.data.frame default matches what = 'pareto'", {
  pr <- mock_pareto_result()
  expect_identical(as.data.frame(pr), as.data.frame(pr, what = "pareto"))
})
