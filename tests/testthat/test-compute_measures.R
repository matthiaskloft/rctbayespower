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

# =============================================================================
# QUANTILE COLUMNS IN SUMMARIZE_SIMS()
# =============================================================================

test_that("QUANTILE_COLS has exactly 9 column names with correct pattern", {
  cols <- rctbayespower:::QUANTILE_COLS
  expect_length(cols, 9)
  expect_true(all(grepl("^post_q[0-9]{3}$", cols)))
  expect_true("post_q025" %in% cols)
  expect_true("post_q500" %in% cols)
  expect_true("post_q975" %in% cols)
})

test_that("summarize_sims includes quantile columns with mean and SE", {
  results <- mock_raw_results(n_sims = 10, n_conditions = 1)
  # Add quantile columns to raw results (normally produced by backend functions)
  q_cols <- rctbayespower:::QUANTILE_COLS
  for (qc in q_cols) {
    results[[qc]] <- stats::runif(nrow(results), 0, 1)
  }

  summary <- rctbayespower:::summarize_sims(results, n_sims = 10)

  for (qc in q_cols) {
    expect_true(qc %in% names(summary),
                info = paste("Missing quantile column:", qc))
    expect_true(paste0("se_", qc) %in% names(summary),
                info = paste("Missing SE column for:", qc))
  }
})

# =============================================================================
# effective_n with dropout (per-sim n_analyzed at final look)
# =============================================================================

test_that("effective_n uses per-sim n_analyzed, not n_planned, with mixed dropout", {
  # 10 sims, 2 looks, none stopped
  # Sims 1-5: n_analyzed = 90 at look 2 (dropout reduced)
  # Sims 6-10: n_analyzed = 100 at look 2 (no dropout)
  results1 <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results1$id_look <- 1L
  results1$n_analyzed <- 50L
  results1$stopped <- FALSE
  results1$stop_reason <- NA_character_
  results1$n_dropped <- 0L

  results2 <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results2$id_look <- 2L
  results2$n_analyzed <- ifelse(results2$id_iter <= 5, 90L, 100L)
  results2$stopped <- FALSE
  results2$stop_reason <- NA_character_
  results2$n_dropped <- ifelse(results2$id_iter <= 5, 10L, 0L)

  combined <- rbind(results1, results2)
  summary <- rctbayespower:::summarize_sims_with_interim(combined, n_sims = 10)

  # Overall should have exactly one row per condition (no row duplication)
  expect_equal(nrow(summary$overall), 1L)
  overall <- summary$overall

  # n_mn should be mean of actual n_analyzed at final look: (5*90 + 5*100)/10 = 95
  expect_equal(overall$n_mn, 95)
  # dropout_pct: mean of per-sim dropout fraction
  # sims 1-5: 10/(90+10)=0.1, sims 6-10: 0/(100+0)=0
  expect_equal(overall$dropout_pct, 0.05, tolerance = 1e-10)
  # n_planned is the max n_analyzed across condition = 100
  expect_equal(overall$n_planned, 100)
})

test_that("effective_n uses n_analyzed_final (not n_planned) even without n_dropped", {
  # Regression test: n_analyzed at final look differs from n_planned (max).
  # Old code used n_planned for all non-stopped sims; new code uses per-sim
  # n_analyzed at the final look. Without this fix, n_mn would be 100 (n_planned).
  results1 <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results1$id_look <- 1L
  results1$n_analyzed <- 50L
  results1$stopped <- FALSE
  results1$stop_reason <- NA_character_

  results2 <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results2$id_look <- 2L
  # Sims 1-5 have 90, sims 6-10 have 100 (no n_dropped column)
  results2$n_analyzed <- ifelse(results2$id_iter <= 5, 90L, 100L)
  results2$stopped <- FALSE
  results2$stop_reason <- NA_character_

  combined <- rbind(results1, results2)
  summary <- rctbayespower:::summarize_sims_with_interim(combined, n_sims = 10)

  # n_planned = 100 (max), but n_mn should be 95 (actual per-sim mean)
  overall <- summary$overall[!duplicated(summary$overall$id_cond), ]
  expect_equal(overall$n_planned, 100)
  expect_equal(overall$n_mn, 95)
})

test_that("effective_n uses stop_n for stopped sims and n_analyzed for others", {
  # Sims 1-3 stopped at look 1 (n=50), sims 4-10 reach look 2 with dropout (n=85)
  results1 <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results1$id_look <- 1L
  results1$n_analyzed <- 50L
  results1$stopped <- ifelse(results1$id_iter <= 3, TRUE, FALSE)
  results1$stop_reason <- ifelse(results1$id_iter <= 3, "stop_efficacy", NA_character_)
  results1$n_dropped <- 0L

  results2 <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results2$id_look <- 2L
  results2$n_analyzed <- 85L
  results2$stopped <- FALSE
  results2$stop_reason <- NA_character_
  results2$n_dropped <- 15L

  combined <- rbind(results1, results2)
  summary <- rctbayespower:::summarize_sims_with_interim(combined, n_sims = 10)

  # n_mn = (3*50 + 7*85) / 10 = (150 + 595) / 10 = 74.5
  expect_equal(summary$overall$n_mn, 74.5)
})

test_that("effective_n uses stop_n for all sims when all stop early", {
  # All 10 sims stop at look 1 (n=50)
  results1 <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results1$id_look <- 1L
  results1$n_analyzed <- 50L
  results1$stopped <- TRUE
  results1$stop_reason <- "stop_efficacy"
  results1$n_dropped <- 5L

  results2 <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results2$id_look <- 2L
  results2$n_analyzed <- 100L
  results2$stopped <- FALSE
  results2$stop_reason <- NA_character_
  results2$n_dropped <- 0L

  combined <- rbind(results1, results2)
  summary <- rctbayespower:::summarize_sims_with_interim(combined, n_sims = 10)

  # All sims stopped at look 1 with n=50, so n_mn = 50
  expect_equal(summary$overall$n_mn, 50)
})

test_that("summarize_sims_with_interim computes convergence metrics correctly", {
  results1 <- mock_raw_results(n_sims = 10, n_conditions = 1)
  results1$id_look <- 1L
  results1$n_analyzed <- 50L
  results1$stopped <- FALSE
  results1$stop_reason <- NA_character_
  # Vary convergence metrics across sims
  results1$rhat <- seq(1.0, 1.1, length.out = 10)
  results1$ess_bulk <- seq(800, 1200, length.out = 10)
  results1$ess_tail <- seq(600, 1000, length.out = 10)
  results1$converged <- c(rep(TRUE, 8), rep(FALSE, 2))

  results2 <- results1
  results2$id_look <- 2L
  results2$n_analyzed <- 100L
  # Better convergence at final look
  results2$rhat <- seq(1.0, 1.02, length.out = 10)
  results2$ess_bulk <- seq(1000, 1500, length.out = 10)
  results2$ess_tail <- seq(800, 1200, length.out = 10)
  results2$converged <- c(rep(TRUE, 9), FALSE)

  combined <- rbind(results1, results2)
  summary <- rctbayespower:::summarize_sims_with_interim(combined, n_sims = 10)

  # by_look should have convergence columns with correct means at final look
  look2 <- summary$by_look[summary$by_look$id_look == 2, ]
  expect_equal(look2$rhat, mean(results2$rhat), tolerance = 1e-10)
  expect_equal(look2$ess_bulk, mean(results2$ess_bulk), tolerance = 1e-10)
  expect_true("ess_tail" %in% names(look2))
  expect_equal(look2$ess_tail, mean(results2$ess_tail), tolerance = 1e-10)
  expect_equal(look2$conv_rate, 0.9)
  # SE columns should be finite
  expect_true(is.finite(look2$se_rhat))
  expect_true(is.finite(look2$se_ess_bulk))
  expect_true(is.finite(look2$se_ess_tail))
  expect_true(is.finite(look2$se_conv_rate))

  # overall should have convergence from final look
  expect_true("conv_rate" %in% names(summary$overall))
  expect_true("rhat" %in% names(summary$overall))
  expect_true("ess_tail" %in% names(summary$overall))
})

test_that("summarize_sims quantile columns appear between post_sd and rhat", {
  results <- mock_raw_results(n_sims = 10, n_conditions = 1)
  q_cols <- rctbayespower:::QUANTILE_COLS
  for (qc in q_cols) {
    results[[qc]] <- stats::runif(nrow(results), 0, 1)
  }

  summary <- rctbayespower:::summarize_sims(results, n_sims = 10)
  col_names <- names(summary)

  post_sd_idx <- which(col_names == "post_sd")
  rhat_idx <- which(col_names == "rhat")
  q025_idx <- which(col_names == "post_q025")
  q975_idx <- which(col_names == "post_q975")

  expect_true(length(post_sd_idx) > 0, info = "post_sd not found")
  expect_true(length(rhat_idx) > 0, info = "rhat not found")
  expect_true(length(q025_idx) > 0, info = "post_q025 not found")
  expect_true(q025_idx > post_sd_idx, info = "post_q025 should come after post_sd")
  expect_true(q975_idx < rhat_idx, info = "post_q975 should come before rhat")
})
