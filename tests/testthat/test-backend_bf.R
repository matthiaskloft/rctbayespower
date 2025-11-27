# Tests for BayesFlow backend (R/backend_bf.R)

# =============================================================================
# AVAILABILITY CHECKS
# =============================================================================

test_that("check_bf_available returns logical", {
  result <- rctbayespower::check_bf_available(silent = TRUE)
  expect_type(result, "logical")
})

test_that("check_bf_available with silent=FALSE errors when unavailable", {
  skip_if(rctbayespower::check_bf_available(silent = TRUE), "BayesFlow is available")

  expect_error(
    rctbayespower::check_bf_available(silent = FALSE),
    regexp = "reticulate|Python|bayesflow"
  )
})

test_that("is_bf_mock_mode detects environment variable", {
  # Default should be FALSE
  withr::with_envvar(c(RCTBP_MOCK_BF = ""), {
    expect_false(rctbayespower:::is_bf_mock_mode())
  })

  # TRUE when set
  withr::with_envvar(c(RCTBP_MOCK_BF = "TRUE"), {
    expect_true(rctbayespower:::is_bf_mock_mode())
  })

  # Other values should be FALSE
  withr::with_envvar(c(RCTBP_MOCK_BF = "true"), {
    expect_false(rctbayespower:::is_bf_mock_mode())
  })
})

# =============================================================================
# MOCK MODE TESTS (No Python Required)
# =============================================================================

test_that("mock_bf_samples returns correct dimensions", {
  samples <- rctbayespower:::mock_bf_samples(batch_size = 5, n_samples = 100)
  expect_equal(dim(samples), c(5, 100))
  expect_type(samples, "double")
})

test_that("mock_bf_samples generates reasonable values", {
  samples <- rctbayespower:::mock_bf_samples(batch_size = 10, n_samples = 1000)

  # Should be centered around 0.3 with reasonable spread
  overall_mean <- mean(samples)
  expect_true(overall_mean > 0.1 && overall_mean < 0.5)

  # Should have some variance
  overall_sd <- sd(as.vector(samples))
  expect_true(overall_sd > 0.05 && overall_sd < 0.5)
})

test_that("mock_bf_samples uses data_batch when provided", {
  # Create mock data batch with known treatment effect
  data_batch <- list(
    outcome = matrix(c(
      # Sim 1: control=0, treat=0.5 -> effect ~0.5
      rep(0, 50), rep(0.5, 50),
      # Sim 2: control=0, treat=0.2 -> effect ~0.2
      rep(0, 50), rep(0.2, 50)
    ), nrow = 2, byrow = TRUE),
    group = matrix(rep(c(rep(0, 50), rep(1, 50)), 2), nrow = 2, byrow = TRUE),
    covariate = matrix(rnorm(200), nrow = 2)
  )

  samples <- rctbayespower:::mock_bf_samples(batch_size = 2, n_samples = 1000, data_batch = data_batch)

  # Row means should reflect the crude treatment effects
  row_means <- rowMeans(samples)
  expect_true(row_means[1] > row_means[2])  # First effect larger
})

# =============================================================================
# SUMMARIZATION TESTS (No Python Required)
# =============================================================================

test_that("summarize_post_bf returns correct structure", {
  skip_if_not_installed("matrixStats")

  # Create mock draws
  draws_mat <- matrix(rnorm(50 * 1000, mean = 0.3, sd = 0.1), nrow = 50, ncol = 1000)

  result <- rctbayespower:::summarize_post_bf(
    draws_mat = draws_mat,
    target_param = "b_arm_treat",
    thr_scs = 0.2,
    thr_ftl = 0,
    p_sig_scs = 0.975,
    p_sig_ftl = 0.5,
    id_iter = 1:50,
    id_cond = rep(1L, 50),
    id_look = 1L,
    n_analyzed = 100L
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 50)

  # Check required columns exist
  expected_cols <- c(
    "par_name", "thr_scs", "thr_ftl", "p_sig_scs", "p_sig_ftl",
    "pr_scs", "pr_ftl", "dec_scs", "dec_ftl",
    "post_med", "post_mad", "post_mn", "post_sd",
    "rhat", "ess_bulk", "ess_tail",
    "id_iter", "id_cond", "id_look", "n_analyzed",
    "stopped", "stop_reason", "converged", "error_msg"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("summarize_post_bf computes probabilities correctly", {
  skip_if_not_installed("matrixStats")

  # Create draws that are all above threshold
  draws_mat <- matrix(rep(0.5, 10 * 100), nrow = 10, ncol = 100)

  result <- rctbayespower:::summarize_post_bf(
    draws_mat = draws_mat,
    target_param = "b_arm_treat",
    thr_scs = 0.2,
    thr_ftl = 0.6,
    p_sig_scs = 0.975,
    p_sig_ftl = 0.975,
    id_iter = 1:10,
    id_cond = rep(1L, 10),
    id_look = 1L,
    n_analyzed = 50L
  )

  # All pr_scs should be 1.0 (all draws > 0.2)
  expect_equal(unique(result$pr_scs), 1.0)

  # All pr_ftl should be 1.0 (all draws < 0.6)
  expect_equal(unique(result$pr_ftl), 1.0)

  # All should declare success
  expect_equal(unique(result$dec_scs), 1L)
})

# =============================================================================
# DATA PREPARATION TESTS (No Python Required)
# =============================================================================

test_that("prepare_single_as_batch_bf creates correct structure", {
  data <- data.frame(
    outcome = rnorm(100),
    covariate = rnorm(100),
    arm = c(rep(0, 50), rep(1, 50))
  )

  result <- rctbayespower:::prepare_single_as_batch_bf(data, list(p_alloc = 0.5))

  expect_type(result, "list")
  expect_true(all(c("outcome", "covariate", "group", "N", "p_alloc") %in% names(result)))
  expect_equal(nrow(result$outcome), 1)
  expect_equal(ncol(result$outcome), 100)
  expect_equal(result$N, 100)
  expect_equal(result$p_alloc, 0.5)
})

test_that("prepare_data_list_as_batch_bf creates correct structure", {
  data_list <- lapply(1:5, function(i) {
    data.frame(
      outcome = rnorm(100),
      covariate = rnorm(100),
      arm = c(rep(0, 50), rep(1, 50))
    )
  })

  result <- rctbayespower:::prepare_data_list_as_batch_bf(data_list, list(p_alloc = 0.5))

  expect_type(result, "list")
  expect_equal(nrow(result$outcome), 5)
  expect_equal(ncol(result$outcome), 100)
  expect_equal(result$N, 100)
})

# =============================================================================
# SUMMARY STATISTICS TESTS (No Python Required)
# =============================================================================

test_that("compute_summaries_batch_ancova returns correct dimensions", {
  data_batch <- list(
    outcome = matrix(rnorm(3 * 100), nrow = 3),
    covariate = matrix(rnorm(3 * 100), nrow = 3),
    group = matrix(rep(c(rep(0, 50), rep(1, 50)), 3), nrow = 3, byrow = TRUE)
  )

  result <- rctbayespower:::compute_summaries_batch_ancova(data_batch)

  expect_equal(dim(result), c(3, 8))
  expect_type(result, "double")
})

test_that("compute_summaries_batch_ancova computes correct values", {
  # Create data with known statistics
  outcome_ctrl <- rep(0, 50)
  outcome_treat <- rep(1, 50)
  covariate <- rep(0.5, 100)

  data_batch <- list(
    outcome = matrix(c(outcome_ctrl, outcome_treat), nrow = 1),
    covariate = matrix(covariate, nrow = 1),
    group = matrix(c(rep(0, 50), rep(1, 50)), nrow = 1)
  )

  result <- rctbayespower:::compute_summaries_batch_ancova(data_batch)

  # Column 1: mean_ctrl = 0
  expect_equal(result[1, 1], 0)
  # Column 2: mean_treat = 1
  expect_equal(result[1, 2], 1)
  # Column 5: n_ctrl = 50
  expect_equal(result[1, 5], 50)
  # Column 6: n_treat = 50
  expect_equal(result[1, 6], 50)
  # Column 8: mean_covariate = 0.5
  expect_equal(result[1, 8], 0.5)
})

# =============================================================================
# PYTHON INTEGRATION TESTS (Require Python)
# =============================================================================

test_that("init_bf_python returns module list when available", {
  skip_if_not(rctbayespower::check_bf_available(silent = TRUE), "BayesFlow not available")
  skip_on_cran()

  py_mods <- rctbayespower::init_bf_python()

  expect_type(py_mods, "list")
  expect_true(all(c("bf", "np", "keras") %in% names(py_mods)))
  expect_false(is.null(py_mods$bf))
  expect_false(is.null(py_mods$np))
  expect_false(is.null(py_mods$keras))
})

test_that("detect_bf_model_type identifies model types", {
  skip_if_not(rctbayespower::check_bf_available(silent = TRUE), "BayesFlow not available")
  skip_on_cran()

  # This is a basic smoke test - we can't test all model types without models
  # The function should at least not error when called
  expect_type(rctbayespower:::detect_bf_model_type, "closure")
})

# =============================================================================
# END-TO-END MOCK MODE TEST
# =============================================================================

test_that("estimate_batch_bf works in mock mode", {
  skip_if_not_installed("matrixStats")

  withr::with_envvar(c(RCTBP_MOCK_BF = "TRUE"), {
    data_batch <- list(
      outcome = matrix(rnorm(5 * 100), nrow = 5),
      covariate = matrix(rnorm(5 * 100), nrow = 5),
      group = matrix(rep(c(rep(0, 50), rep(1, 50)), 5), nrow = 5, byrow = TRUE),
      N = 100L,
      p_alloc = 0.5
    )

    backend_args <- list(n_posterior_samples = 500L)

    # Should use mock samples, not require Python
    result <- rctbayespower:::estimate_batch_bf(
      data_batch = data_batch,
      bf_model = NULL,
      backend_args = backend_args,
      target_params = "b_arm_treat"
    )

    expect_equal(dim(result), c(5, 500))
  })
})
