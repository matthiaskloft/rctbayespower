# Tests for shared result utilities (R/utils_results.R) and
# posterior quantile profile storage across backends.

# =============================================================================
# QUANTILE_PROBS / QUANTILE_COLS CONSTANTS
# =============================================================================

test_that("QUANTILE_PROBS has exactly 9 probability values", {
  probs <- rctbayespower:::QUANTILE_PROBS
  expect_length(probs, 9)
  expect_equal(probs, c(0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975))
})

test_that("QUANTILE_COLS has exactly 9 column names", {
  cols <- rctbayespower:::QUANTILE_COLS
  expect_length(cols, 9)
  expect_true(all(grepl("^post_q[0-9]{3}$", cols)))
})

test_that("QUANTILE_COLS contains expected boundary and central quantile names", {
  cols <- rctbayespower:::QUANTILE_COLS
  expect_true("post_q025" %in% cols)   # 2.5th percentile
  expect_true("post_q050" %in% cols)   # 5th percentile
  expect_true("post_q500" %in% cols)   # median
  expect_true("post_q950" %in% cols)   # 95th percentile
  expect_true("post_q975" %in% cols)   # 97.5th percentile
})

# =============================================================================
# create_error_result()
# =============================================================================

test_that("create_error_result returns a data.frame with one row", {
  result <- rctbayespower:::create_error_result(
    id_iter = 1L, id_cond = 2L, id_analysis = 0L, error_msg = "test error"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
})

test_that("create_error_result includes all 9 quantile columns with NA values", {
  result <- rctbayespower:::create_error_result(
    id_iter = 1L, id_cond = 1L, id_analysis = 0L, error_msg = "oops"
  )
  q_cols <- rctbayespower:::QUANTILE_COLS
  for (qc in q_cols) {
    expect_true(qc %in% names(result),
                info = paste("Missing quantile column:", qc))
    expect_true(is.na(result[[qc]]),
                info = paste("Expected NA in column:", qc))
  }
})

test_that("create_error_result stores the error message and ids correctly", {
  result <- rctbayespower:::create_error_result(
    id_iter = 5L, id_cond = 3L, id_analysis = 2L, error_msg = "model failed"
  )
  expect_equal(result$id_iter, 5L)
  expect_equal(result$id_cond, 3L)
  expect_equal(result$id_look, 2L)
  expect_equal(result$error_msg, "model failed")
  expect_equal(result$converged, 0L)
})

test_that("create_error_result has NA for all posterior summary columns", {
  result <- rctbayespower:::create_error_result(
    id_iter = 1L, id_cond = 1L, id_analysis = 0L, error_msg = "error"
  )
  post_cols <- c("post_med", "post_mad", "post_mn", "post_sd",
                 "pr_eff", "pr_fut", "dec_eff", "dec_fut",
                 "rhat", "ess_bulk", "ess_tail")
  for (col in post_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
    expect_true(is.na(result[[col]]), info = paste("Expected NA in:", col))
  }
})

test_that("create_error_result quantile columns are ordered between post_sd and rhat", {
  result <- rctbayespower:::create_error_result(
    id_iter = 1L, id_cond = 1L, id_analysis = 0L, error_msg = "test"
  )
  col_names <- names(result)
  post_sd_idx <- which(col_names == "post_sd")
  rhat_idx <- which(col_names == "rhat")
  q025_idx <- which(col_names == "post_q025")
  q975_idx <- which(col_names == "post_q975")

  expect_true(length(post_sd_idx) > 0, info = "post_sd not found in result")
  expect_true(length(rhat_idx) > 0, info = "rhat not found in result")
  expect_true(q025_idx > post_sd_idx,
              info = "post_q025 should come after post_sd")
  expect_true(q975_idx < rhat_idx,
              info = "post_q975 should come before rhat")
})

# =============================================================================
# summarize_post_brms_single() — quantile columns
# =============================================================================

test_that("summarize_post_brms_single returns all quantile columns with correct values", {
  skip_if_not_installed("posterior")

  # Create a known draws_array: 100 iterations, 2 chains, 1 variable
  # All values set to a known sequence so quantiles are predictable
  set.seed(42)
  mat <- matrix(stats::rnorm(200, mean = 0.5, sd = 0.1), nrow = 100, ncol = 2)
  draws_arr <- posterior::as_draws_array(mat)
  # Rename the first variable to the target parameter name
  draws_arr <- posterior::rename_variables(draws_arr, b_arm2 = `...1`)

  result <- rctbayespower:::summarize_post_brms_single(
    draws_arr = draws_arr,
    target_param = "b_arm2",
    thr_fx_eff = 0.2,
    thr_fx_fut = 0.0,
    thr_dec_eff = 0.975,
    thr_dec_fut = 0.5
  )

  q_cols <- rctbayespower:::QUANTILE_COLS
  for (qc in q_cols) {
    expect_true(qc %in% names(result),
                info = paste("Missing quantile column:", qc))
    expect_false(is.na(result[[qc]]),
                 info = paste("Unexpected NA in:", qc))
  }

  # Quantiles should be ordered (monotone non-decreasing)
  quantile_values <- unlist(result[, q_cols])
  expect_true(all(diff(quantile_values) >= 0),
              info = "Quantile values should be non-decreasing")
})

test_that("summarize_post_brms_single quantile columns match manual calculation", {
  skip_if_not_installed("posterior")

  set.seed(123)
  draws_vec <- stats::rnorm(400, mean = 1.0, sd = 0.5)
  # Build draws_array: 200 iterations x 2 chains x 1 var
  mat <- matrix(draws_vec, nrow = 200, ncol = 2)
  draws_arr <- posterior::as_draws_array(mat)
  draws_arr <- posterior::rename_variables(draws_arr, b_treat = `...1`)

  result <- rctbayespower:::summarize_post_brms_single(
    draws_arr = draws_arr,
    target_param = "b_treat",
    thr_fx_eff = 0.0,
    thr_fx_fut = -0.5,
    thr_dec_eff = 0.975,
    thr_dec_fut = 0.5
  )

  # Manual quantiles from all draws
  all_draws <- as.numeric(draws_arr[, , "b_treat"])
  expected_q025 <- stats::quantile(all_draws, 0.025, names = FALSE)
  expected_q500 <- stats::quantile(all_draws, 0.500, names = FALSE)
  expected_q975 <- stats::quantile(all_draws, 0.975, names = FALSE)

  expect_equal(result$post_q025, expected_q025, tolerance = 1e-10)
  expect_equal(result$post_q500, expected_q500, tolerance = 1e-10)
  expect_equal(result$post_q975, expected_q975, tolerance = 1e-10)
})

# =============================================================================
# summarize_post_brms() — union row has NA quantile columns
# =============================================================================

test_that("union row in summarize_post_brms has NA quantile columns", {
  skip_if_not_installed("posterior")

  set.seed(7)
  # Two-parameter draws_array: 100 iter x 2 chains x 2 vars
  mat1 <- matrix(stats::rnorm(200, 0.5, 0.1), nrow = 100, ncol = 2)
  mat2 <- matrix(stats::rnorm(200, 0.3, 0.1), nrow = 100, ncol = 2)
  draws_arr <- posterior::as_draws_array(cbind(mat1[, 1], mat2[, 1]))
  draws_arr <- posterior::rename_variables(draws_arr, b_arm2 = `...1`, b_cov = `...2`)

  result <- rctbayespower:::summarize_post_brms(
    draws_arr = draws_arr,
    target_params = c("b_arm2", "b_cov"),
    thr_fx_eff = c(0.2, 0.1),
    thr_fx_fut = c(0.0, 0.0),
    thr_dec_eff = 0.975,
    thr_dec_fut = 0.5
  )

  # Should have 3 rows: b_arm2, b_cov, union
  expect_equal(nrow(result), 3L)
  union_row <- result[result$par_name == "union", ]
  expect_equal(nrow(union_row), 1L)

  q_cols <- rctbayespower:::QUANTILE_COLS
  for (qc in q_cols) {
    expect_true(is.na(union_row[[qc]]),
                info = paste("Expected NA in union row for:", qc))
  }
})
