# =============================================================================
# summarize_sims_with_interim() — accrual metrics
# =============================================================================

# Helper to build a minimal results_df_raw with interim columns
make_interim_results <- function(n_sims = 10, n_looks = 2,
                                  with_accrual = FALSE,
                                  stopped_iters = 1:3) {
  rows <- list()
  analysis_ns <- c(50, 100)
  for (iter in seq_len(n_sims)) {
    for (look in seq_len(n_looks)) {
      row <- data.frame(
        par_name = "b_arm_treat",
        thr_fx_eff = 0, thr_fx_fut = 0,
        thr_dec_eff = 0.975, thr_dec_fut = 0.90,
        pr_eff = runif(1, 0.5, 1),
        pr_fut = runif(1, 0, 0.5),
        dec_eff = 0L, dec_fut = 0L,
        post_med = rnorm(1, 0.5, 0.1),
        post_mad = 0.1, post_mn = 0.5, post_sd = 0.1,
        rhat = 1.0, ess_bulk = 1000, ess_tail = 800,
        id_iter = iter, id_cond = 1L,
        id_look = look,
        n_analyzed = analysis_ns[look],
        calendar_time = NA_real_,
        n_enrolled = NA_integer_,
        enrollment_duration = NA_real_,
        stopped = FALSE,
        stop_reason = NA_character_,
        converged = 1L,
        error_msg = NA_character_,
        stringsAsFactors = FALSE
      )
      if (with_accrual) {
        row$calendar_time <- look * 12.0  # 12 months per look
        row$n_enrolled <- as.integer(analysis_ns[look] + 10L * look)
        row$enrollment_duration <- 8.0
      }
      rows <- c(rows, list(row))
    }
  }
  df <- do.call(rbind, rows)

  # Mark specified sims as stopped at look 1
  for (iter in stopped_iters) {
    idx <- which(df$id_iter == iter & df$id_look == 1)
    df$dec_eff[idx] <- 1L
    df$stop_reason[idx] <- "stop_efficacy"
    # Mark look 2 as stopped too
    idx2 <- which(df$id_iter == iter & df$id_look == 2)
    df$stopped[idx2] <- TRUE
    df$stop_reason[idx2] <- "stop_efficacy"
  }

  # Add quantile columns (required by summarize_sims_with_interim)
  for (qc in rctbayespower:::QUANTILE_COLS) {
    df[[qc]] <- rnorm(nrow(df), 0.5, 0.1)
  }

  df
}


test_that("by_look includes accrual metrics when calendar_time present", {
  set.seed(42)
  df <- make_interim_results(n_sims = 10, with_accrual = TRUE)
  result <- rctbayespower:::summarize_sims_with_interim(df, n_sims = 10)

  expect_true("calendar_time_mn" %in% names(result$by_look))
  expect_true("calendar_time_mdn" %in% names(result$by_look))
  expect_true("n_enrolled_mn" %in% names(result$by_look))

  # Check values are reasonable (look 1 = 12, look 2 = 24)
  look1 <- result$by_look[result$by_look$id_look == 1, ]
  expect_equal(look1$calendar_time_mn[1], 12.0)
  look2 <- result$by_look[result$by_look$id_look == 2, ]
  expect_equal(look2$calendar_time_mn[1], 24.0)
})


test_that("overall includes trial duration metrics when accrual present", {
  set.seed(42)
  df <- make_interim_results(n_sims = 10, with_accrual = TRUE)
  result <- rctbayespower:::summarize_sims_with_interim(df, n_sims = 10)

  expect_true("trial_dur_mn" %in% names(result$overall))
  expect_true("trial_dur_mdn" %in% names(result$overall))
  expect_true("enrollment_dur_mn" %in% names(result$overall))

  # Stopped sims (1-3) use look 1 calendar_time = 12
  # Non-stopped sims (4-10) use look 2 calendar_time = 24
  # Mean: (3*12 + 7*24) / 10 = (36 + 168) / 10 = 20.4
  expect_equal(result$overall$trial_dur_mn[1], 20.4)
  # Median: 7 of 10 sims at 24, so median is 24
  expect_equal(result$overall$trial_dur_mdn[1], 24.0)
  expect_equal(result$overall$enrollment_dur_mn[1], 8.0)
})


test_that("accrual columns absent when calendar_time is all NA", {
  set.seed(42)
  # Backend always includes columns, but with NA when no accrual
  df <- make_interim_results(n_sims = 10, with_accrual = FALSE)
  result <- rctbayespower:::summarize_sims_with_interim(df, n_sims = 10)

  expect_false("calendar_time_mn" %in% names(result$by_look))
  expect_false("trial_dur_mn" %in% names(result$overall))
})


test_that("all sims stopped early: trial_dur equals look-1 calendar_time", {
  set.seed(42)
  df <- make_interim_results(n_sims = 10, with_accrual = TRUE,
                              stopped_iters = 1:10)
  result <- rctbayespower:::summarize_sims_with_interim(df, n_sims = 10)

  # All stopped at look 1 (calendar_time = 12)
  expect_equal(result$overall$trial_dur_mn[1], 12.0)
  expect_equal(result$overall$trial_dur_mdn[1], 12.0)
  expect_equal(result$overall$prop_stp_early[1], 1.0)
})


test_that("no sims stopped: trial_dur equals final-look calendar_time", {
  set.seed(42)
  df <- make_interim_results(n_sims = 10, with_accrual = TRUE,
                              stopped_iters = integer(0))
  result <- rctbayespower:::summarize_sims_with_interim(df, n_sims = 10)

  # All run to final look (calendar_time = 24)
  expect_equal(result$overall$trial_dur_mn[1], 24.0)
  expect_equal(result$overall$trial_dur_mdn[1], 24.0)
  expect_equal(result$overall$prop_stp_early[1], 0.0)
})
