# Tests for result summarization (R/compute_measures.R)

# =============================================================================
# summarize_sims() - SINGLE LOOK
# =============================================================================

test_that("summarize_sims aggregates results correctly", {
  results <- mock_raw_results(n_sims = 20, n_conditions = 2)
  summary <- rctbayespower:::summarize_sims(results, n_sims = 20)

  expect_s3_class(summary, "data.frame")
  expect_equal(nrow(summary), 2)  # One row per condition

  # Check expected columns exist
  expect_true("id_cond" %in% names(summary))
  expect_true("pwr_eff" %in% names(summary))
  expect_true("pwr_fut" %in% names(summary))
  expect_true("pr_eff" %in% names(summary))
  expect_true("post_med" %in% names(summary))
  expect_true("conv_rate" %in% names(summary))

  # Power should be proportion of dec_eff == 1
  cond1_results <- results[results$id_cond == 1, ]
  expected_pwr <- mean(cond1_results$dec_eff)
  expect_equal(summary$pwr_eff[summary$id_cond == 1], expected_pwr)
})

test_that("summarize_sims includes MCSE columns", {
  results <- mock_raw_results(n_sims = 20)
  summary <- rctbayespower:::summarize_sims(results, n_sims = 20)

  # Standard error columns should exist
  expect_true("se_pwr_eff" %in% names(summary))
  expect_true("se_pr_eff" %in% names(summary))
  expect_true("se_post_med" %in% names(summary))
})

test_that("summarize_sims handles all-NA results gracefully", {
  results <- mock_raw_results(n_sims = 5)
  # Set all id_cond to NA (simulates all simulations failing)
  results$id_cond <- NA
  expect_warning(
    summary <- rctbayespower:::summarize_sims(results, n_sims = 5),
    "filtered"
  )
  expect_equal(nrow(summary), 0)
})

test_that("summarize_sims rejects empty data.frame", {
  expect_cli_abort(
    rctbayespower:::summarize_sims(data.frame(), n_sims = 10)
  )
})

test_that("summarize_sims detects and delegates interim results", {
  # Create results with interim columns
  results <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results$id_look <- 1L
  results$n_analyzed <- 100L
  results$stopped <- FALSE
  results$stop_reason <- NA_character_

  # Duplicate for second look
  results2 <- results
  results2$id_look <- 2L
  results2$n_analyzed <- 200L
  combined <- rbind(results, results2)

  summary <- rctbayespower:::summarize_sims(combined, n_sims = 10)

  # Should return list (interim format) not data.frame

  expect_type(summary, "list")
  expect_true("by_look" %in% names(summary))
  expect_true("overall" %in% names(summary))
})

# =============================================================================
# summarize_sims_with_interim()
# =============================================================================

test_that("summarize_sims_with_interim returns correct structure", {
  # Create two-look results
  results1 <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results1$id_look <- 1L
  results1$n_analyzed <- 100L
  results1$stopped <- FALSE
  results1$stop_reason <- NA_character_

  results2 <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results2$id_look <- 2L
  results2$n_analyzed <- 200L
  results2$stopped <- FALSE
  results2$stop_reason <- NA_character_

  combined <- rbind(results1, results2)

  summary <- rctbayespower:::summarize_sims_with_interim(combined, n_sims = 10)

  expect_type(summary, "list")
  expect_s3_class(summary$by_look, "data.frame")
  expect_s3_class(summary$overall, "data.frame")

  # by_look should have rows for each look
  expect_true(nrow(summary$by_look) >= 2)

  # overall should have planned N
  expect_true("n_planned" %in% names(summary$overall))
  expect_true("n_mn" %in% names(summary$overall))
})

test_that("summarize_sims_with_interim rejects missing columns", {
  results <- mock_raw_results(n_sims = 5)
  # Missing required interim columns
  expect_cli_abort(
    rctbayespower:::summarize_sims_with_interim(results, n_sims = 5)
  )
})

# =============================================================================
# MCSE CALCULATIONS (R/MCSE.R)
# =============================================================================

test_that("calculate_mcse_power computes correctly", {
  # All successes -> MCSE = 0
  expect_equal(rctbayespower:::calculate_mcse_power(rep(1, 100), 100), 0)

  # All failures -> MCSE = 0
  expect_equal(rctbayespower:::calculate_mcse_power(rep(0, 100), 100), 0)

  # 50/50 -> MCSE = sqrt(0.5 * 0.5 / n)
  expect_equal(
    rctbayespower:::calculate_mcse_power(c(rep(1, 50), rep(0, 50)), 100),
    sqrt(0.25 / 100),
    tolerance = 1e-10
  )
})

test_that("calculate_mcse_power handles edge cases", {
  expect_true(is.na(rctbayespower:::calculate_mcse_power(numeric(0), 100)))
  expect_true(is.na(rctbayespower:::calculate_mcse_power(c(1, 0), 0)))
})

test_that("calculate_mcse_power handles logical input", {
  result <- rctbayespower:::calculate_mcse_power(c(TRUE, FALSE, TRUE), 3)
  expect_type(result, "double")
  expect_true(result > 0)
})

test_that("calculate_mcse_mean computes correctly", {
  values <- c(1, 2, 3, 4, 5)
  result <- rctbayespower:::calculate_mcse_mean(values, 5)
  expected <- sd(values) / sqrt(5)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("calculate_mcse_mean handles edge cases", {
  expect_true(is.na(rctbayespower:::calculate_mcse_mean(numeric(0), 100)))
  expect_true(is.na(rctbayespower:::calculate_mcse_mean(c(1, 2), 0)))
})

test_that("calculate_mcse_mean removes NAs", {
  values <- c(1, 2, NA, 4, 5)
  result <- rctbayespower:::calculate_mcse_mean(values, 5)
  expect_false(is.na(result))
})

test_that("calculate_mcse_mean uses length(values) not n_sims for denominator", {
  # n_sims is accepted but only used for the early-exit check (n_sims == 0).
  # The actual MCSE uses length(values) after NA removal as the sample size.
  values <- c(1, 2, 3, 4, 5)
  result_n100 <- rctbayespower:::calculate_mcse_mean(values, n_sims = 100)
  result_n5 <- rctbayespower:::calculate_mcse_mean(values, n_sims = 5)

  # Both return sd(values) / sqrt(length(values)), regardless of n_sims

  expect_equal(result_n100, result_n5)
  expect_equal(result_n5, sd(values) / sqrt(length(values)))
})
