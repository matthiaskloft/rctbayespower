# Tests for rctbp_sim_fn class (R/class_sim_fn.R)

# =============================================================================
# build_sim_fn() CONSTRUCTOR
# =============================================================================

test_that("build_sim_fn creates valid rctbp_sim_fn", {
  fn <- function(n_total, effect = 0.5) {
    data.frame(outcome = rnorm(n_total, effect), x = rnorm(n_total))
  }
  sim_fn <- build_sim_fn(fn, test_args = list(n_total = 10L, effect = 0.3))

  expect_s3_class(sim_fn, "rctbayespower::rctbp_sim_fn")
  expect_true(is.function(sim_fn))
  expect_equal(sim_fn@test_args$n_total, 10L)
})

test_that("build_sim_fn is directly callable", {
  fn <- function(n_total, sd = 1) {
    data.frame(outcome = rnorm(n_total, sd = sd))
  }
  sim_fn <- build_sim_fn(fn, test_args = list(n_total = 5L))

  result <- sim_fn(n_total = 20, sd = 2)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 20)
})

test_that("build_sim_fn caches test_output", {
  fn <- function(n_total) data.frame(y = rnorm(n_total))
  sim_fn <- build_sim_fn(fn, test_args = list(n_total = 5L))

  expect_false(is.null(sim_fn@test_output))
  expect_s3_class(sim_fn@test_output, "data.frame")
  expect_equal(nrow(sim_fn@test_output), 5)
})

test_that("build_sim_fn rejects non-function", {
  expect_cli_abort(build_sim_fn("not a function"))
})

test_that("build_sim_fn rejects non-list test_args", {
  fn <- function(n_total) data.frame(y = 1)
  expect_cli_abort(build_sim_fn(fn, test_args = "not a list"))
})

test_that("build_sim_fn rejects function without n_total", {
  fn <- function(x, y) data.frame(outcome = x + y)
  expect_error(build_sim_fn(fn, test_args = list(x = 1, y = 2)))
})

test_that("build_sim_fn rejects invalid test_args names", {
  fn <- function(n_total, effect) data.frame(y = rnorm(n_total))
  expect_error(
    build_sim_fn(fn, test_args = list(n_total = 5, bad_name = 1))
  )
})

test_that("build_sim_fn aborts on test simulation failure", {
  fn <- function(n_total) stop("intentional error")
  expect_cli_abort(build_sim_fn(fn, test_args = list(n_total = 5L)))
})

# =============================================================================
# COMPUTED PROPERTIES
# =============================================================================

test_that("sim_fn_params returns correct parameter names", {
  fn <- function(n_total, p_alloc, effect_size) data.frame(y = 1)
  sim_fn <- build_sim_fn(fn, test_args = list(n_total = 5L, p_alloc = c(0.5, 0.5), effect_size = 0.3))

  expect_equal(sim_fn@sim_fn_params, c("n_total", "p_alloc", "effect_size"))
})

test_that("output_schema detects data.frame columns correctly", {
  fn <- function(n_total) {
    data.frame(
      outcome = rnorm(n_total),
      covariate = rnorm(n_total),
      arm = factor(sample(0:1, n_total, replace = TRUE), levels = 0:1),
      flag = c(TRUE, FALSE)[sample(1:2, n_total, replace = TRUE)]
    )
  }
  sim_fn <- build_sim_fn(fn, test_args = list(n_total = 10L))

  schema <- sim_fn@output_schema
  expect_true("outcome" %in% names(schema))
  expect_true("covariate" %in% names(schema))
  # arm -> group workaround
  expect_true("group" %in% names(schema))
  expect_false("arm" %in% names(schema))
  expect_true("flag" %in% names(schema))

  # Check transforms
  expect_equal(schema$group$transform, "factor_to_numeric")
  expect_equal(schema$flag$transform, "logical_to_int")
  expect_null(schema$outcome$transform)
})

# =============================================================================
# derive_output_schema_from_data()
# =============================================================================

test_that("derive_output_schema handles data.frame", {
  df <- data.frame(
    y = c(1.0, 2.0),
    x = c(0.5, 1.5),
    grp = factor(c("a", "b"))
  )
  schema <- rctbayespower:::derive_output_schema_from_data(df)
  expect_true("y" %in% names(schema))
  expect_true("x" %in% names(schema))
  expect_true("grp" %in% names(schema))
  expect_equal(schema$grp$transform, "factor_to_numeric")
})

test_that("derive_output_schema handles batch dict", {
  batch <- list(
    outcome = matrix(rnorm(20), nrow = 2),
    group = matrix(c(rep(0, 5), rep(1, 5), rep(0, 5), rep(1, 5)), nrow = 2),
    n_total = 10L
  )
  schema <- rctbayespower:::derive_output_schema_from_data(batch)
  expect_true("outcome" %in% names(schema))
  expect_true("group" %in% names(schema))
  # n_total should be excluded (metadata)
  expect_false("n_total" %in% names(schema))
})

test_that("derive_output_schema applies arm->group workaround", {
  df <- data.frame(
    outcome = c(1, 2),
    arm = factor(c(0, 1), levels = 0:1)
  )
  schema <- rctbayespower:::derive_output_schema_from_data(df)
  expect_true("group" %in% names(schema))
  expect_false("arm" %in% names(schema))
  expect_equal(schema$group$source, "arm")
})

# =============================================================================
# validate_sim_output()
# =============================================================================

test_that("validate_sim_output accepts valid data.frame", {
  expect_true(rctbayespower:::validate_sim_output(
    data.frame(y = 1:3, x = 4:6)
  ))
})

test_that("validate_sim_output accepts valid batch dict", {
  expect_true(rctbayespower:::validate_sim_output(
    list(outcome = matrix(1:6, nrow = 2), n_total = 3)
  ))
})

test_that("validate_sim_output rejects empty data.frame", {
  expect_cli_abort(rctbayespower:::validate_sim_output(
    data.frame()
  ))
})

test_that("validate_sim_output rejects zero-row data.frame", {
  expect_cli_abort(rctbayespower:::validate_sim_output(
    data.frame(y = numeric(0))
  ))
})

test_that("validate_sim_output rejects non-data.frame/dict", {
  expect_cli_abort(rctbayespower:::validate_sim_output("not valid"))
  expect_cli_abort(rctbayespower:::validate_sim_output(42))
})
