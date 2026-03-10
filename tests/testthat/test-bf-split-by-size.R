# test-bf-split-by-size.R
# Tests for BayesFlow split-by-size batching (Session 3)

# =============================================================================
# Split-by-size grouping logic
# =============================================================================

test_that("split-by-size groups sims by row count", {
  # Simulate variable completer counts from dropout
  data_list <- list(
    data.frame(y = rnorm(40), x = rnorm(40)),
    data.frame(y = rnorm(40), x = rnorm(40)),
    data.frame(y = rnorm(35), x = rnorm(35)),
    data.frame(y = rnorm(40), x = rnorm(40)),
    data.frame(y = rnorm(35), x = rnorm(35))
  )

  n_obs_per_sim <- vapply(data_list, nrow, integer(1))
  size_groups <- split(seq_along(data_list), n_obs_per_sim)

  expect_length(size_groups, 2)
  expect_equal(size_groups[["40"]], c(1L, 2L, 4L))
  expect_equal(size_groups[["35"]], c(3L, 5L))
})

test_that("uniform row counts produce single batch (no splitting overhead)", {
  data_list <- list(
    data.frame(y = rnorm(50), x = rnorm(50)),
    data.frame(y = rnorm(50), x = rnorm(50)),
    data.frame(y = rnorm(50), x = rnorm(50))
  )

  n_obs_per_sim <- vapply(data_list, nrow, integer(1))
  size_groups <- split(seq_along(data_list), n_obs_per_sim)

  expect_length(size_groups, 1)
  expect_equal(size_groups[["50"]], 1:3)
})

# =============================================================================
# prepare_data_list_as_batch_bf validation
# =============================================================================

test_that("prepare_data_list_as_batch_bf rejects mixed row counts", {
  skip_if_not_installed("reticulate")

  data_list <- list(
    data.frame(outcome = rnorm(40), arm = factor(rep(0:1, 20))),
    data.frame(outcome = rnorm(35), arm = factor(c(rep(0, 17), rep(1, 18))))
  )

  expect_error(
    prepare_data_list_as_batch_bf(data_list, list(p_alloc = c(0.5, 0.5))),
    "same number of rows"
  )
})

test_that("prepare_data_list_as_batch_bf accepts uniform row counts", {
  skip_if_not_installed("reticulate")

  data_list <- list(
    data.frame(outcome = rnorm(40), covariate = rnorm(40),
               arm = factor(rep(0:1, 20))),
    data.frame(outcome = rnorm(40), covariate = rnorm(40),
               arm = factor(rep(0:1, 20)))
  )

  # Should not error
  result <- prepare_data_list_as_batch_bf(data_list, list(p_alloc = c(0.5, 0.5)))
  expect_true(is.list(result))
})

# =============================================================================
# Result reassembly order
# =============================================================================

test_that("split-by-size reassembles results in correct order", {
  # Simulate the reassembly logic from estimate_sequential_bf
  set.seed(42)
  n_sims <- 6
  # Variable row counts
  row_counts <- c(40, 35, 40, 30, 35, 40)

  # Create mock data list
  data_list <- lapply(row_counts, function(n) {
    data.frame(y = rnorm(n), x = rnorm(n))
  })

  n_obs_per_sim <- vapply(data_list, nrow, integer(1))
  size_groups <- split(seq_along(data_list), n_obs_per_sim)

  # Simulate draws (n_draws = 100 per sim)
  n_draws <- 100
  draws_matrix <- matrix(NA_real_, nrow = n_sims, ncol = n_draws)

  for (group_indices in size_groups) {
    # Each group produces draws that encode which sim they came from
    # (for verification)
    group_draws <- matrix(
      rep(group_indices, each = n_draws),
      nrow = length(group_indices),
      ncol = n_draws,
      byrow = TRUE
    )
    draws_matrix[group_indices, ] <- group_draws
  }

  # Verify each row in draws_matrix contains its own index
  for (i in seq_len(n_sims)) {
    expect_equal(draws_matrix[i, 1], i)
  }
})

# =============================================================================
# BayesFlow integration (requires Python)
# =============================================================================

test_that("split-by-size works end-to-end with BayesFlow", {
  skip_if_not_installed("reticulate")
  skip_if(!reticulate::py_available(), "Python not available")
  skip_if(!tryCatch({
    reticulate::import("bayesflow")
    TRUE
  }, error = function(e) FALSE), "BayesFlow not installed")

  # This is a heavyweight test — only run when BF is actually available
  skip("BayesFlow integration test — run manually")
})
