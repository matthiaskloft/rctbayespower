# Tests for parameter introspection functions (R/required_fn_args.R)

# =============================================================================
# get_arg_defaults()
# =============================================================================

test_that("get_arg_defaults identifies missing defaults", {
  fn <- function(x, y, z = 1) NULL
  result <- rctbayespower:::get_arg_defaults(fn)

  expect_true("missing_default" %in% names(result))
  expect_true("x" %in% result$missing_default)
  expect_true("y" %in% result$missing_default)
  expect_false("z" %in% result$missing_default)
})

test_that("get_arg_defaults identifies NULL defaults", {
  fn <- function(x, y = NULL) NULL
  result <- rctbayespower:::get_arg_defaults(fn)

  expect_true("null_default" %in% names(result))
  expect_true("y" %in% result$null_default)
})

test_that("get_arg_defaults evaluates default values", {
  fn <- function(x, y = 42, z = "hello") NULL
  result <- rctbayespower:::get_arg_defaults(fn)

  expect_equal(result$evaluated_defaults$y, 42)
  expect_equal(result$evaluated_defaults$z, "hello")
})

# =============================================================================
# get_args_without_defaults()
# =============================================================================

test_that("get_args_without_defaults combines missing and null", {
  fn <- function(x, y = NULL, z = 1) NULL
  result <- rctbayespower:::get_args_without_defaults(fn)

  expect_true("x" %in% result)
  expect_true("y" %in% result)
  expect_false("z" %in% result)
})

test_that("get_args_without_defaults returns empty for all-defaulted", {
  fn <- function(x = 1, y = 2) NULL
  result <- rctbayespower:::get_args_without_defaults(fn)
  expect_length(result, 0)
})

# =============================================================================
# show_condition_args()
# =============================================================================

test_that("show_condition_args returns params list for valid design", {
  d <- mock_design()
  result <- show_condition_args(d, print = FALSE)

  expect_type(result, "list")
  expect_true("params_sim" %in% names(result))
  expect_true("params_decision" %in% names(result))
  expect_true("params_all" %in% names(result))

  # Decision params always include thresholds
  expect_true("thr_dec_eff" %in% result$params_decision)
  expect_true("thr_fx_eff" %in% result$params_decision)
})

test_that("show_condition_args returns NULL for NULL design", {
  result <- show_condition_args(NULL, print = FALSE)
  expect_null(result)
})

test_that("show_condition_args rejects non-design input", {
  expect_cli_abort(show_condition_args("not a design"))
})

test_that("show_condition_args prints output when print=TRUE", {
  d <- mock_design()
  output <- capture_cli(show_condition_args(d, print = TRUE))
  expect_true(length(output) > 0)
})

# =============================================================================
# show_target_params()
# =============================================================================

test_that("show_target_params returns params for design object", {
  d <- mock_design()
  output <- capture.output(result <- show_target_params(d))
  expect_type(result, "character")
  expect_true("b_arm2" %in% result)
})

test_that("show_target_params returns NULL for NULL input", {
  output <- capture.output(result <- show_target_params(NULL))
  expect_null(result)
})

test_that("show_target_params rejects invalid input type", {
  expect_cli_abort(show_target_params(42))
})
