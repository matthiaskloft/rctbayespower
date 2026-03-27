# Tests for single-objective optimization helpers (R/optimization_single.R)

# =============================================================================
# compute_normalized_distance()
# =============================================================================

test_that("compute_normalized_distance returns 1 at n_min (log scale)", {
  result <- compute_normalized_distance(n = 50, n_range = c(50, 500), scale = "log")
  expect_equal(result, 1, tolerance = 1e-10)
})

test_that("compute_normalized_distance returns 0 at n_max (log scale)", {
  result <- compute_normalized_distance(n = 500, n_range = c(50, 500), scale = "log")
  expect_equal(result, 0, tolerance = 1e-10)
})

test_that("compute_normalized_distance returns 1 at n_min (raw scale)", {
  result <- compute_normalized_distance(n = 50, n_range = c(50, 500), scale = "raw")
  expect_equal(result, 1, tolerance = 1e-10)
})

test_that("compute_normalized_distance returns 0 at n_max (raw scale)", {
  result <- compute_normalized_distance(n = 500, n_range = c(50, 500), scale = "raw")
  expect_equal(result, 0, tolerance = 1e-10)
})

test_that("compute_normalized_distance is strictly decreasing from n_min to n_max (log)", {
  n_seq <- c(50, 100, 200, 350, 500)
  dists <- vapply(n_seq, compute_normalized_distance,
                  numeric(1), n_range = c(50, 500), scale = "log")
  expect_true(all(diff(dists) < 0))
})

test_that("compute_normalized_distance is strictly decreasing from n_min to n_max (raw)", {
  n_seq <- c(50, 100, 200, 350, 500)
  dists <- vapply(n_seq, compute_normalized_distance,
                  numeric(1), n_range = c(50, 500), scale = "raw")
  expect_true(all(diff(dists) < 0))
})

test_that("compute_normalized_distance midpoint is 0.5 on raw scale", {
  result <- compute_normalized_distance(n = 275, n_range = c(50, 500), scale = "raw")
  expect_equal(result, 0.5, tolerance = 1e-10)
})

test_that("compute_normalized_distance log midpoint is 0.5 on log scale", {
  # geometric midpoint: sqrt(50 * 500) = sqrt(25000) ≈ 158.11
  n_mid <- sqrt(50 * 500)
  result <- compute_normalized_distance(n = n_mid, n_range = c(50, 500), scale = "log")
  expect_equal(result, 0.5, tolerance = 1e-10)
})

test_that("compute_normalized_distance output is in [0, 1] for interior points", {
  n_seq <- seq(50, 500, by = 50)
  for (scale in c("log", "raw")) {
    dists <- vapply(n_seq, compute_normalized_distance,
                    numeric(1), n_range = c(50, 500), scale = scale)
    expect_true(all(dists >= 0 & dists <= 1))
  }
})

# =============================================================================
# compute_feasibility_score()
# =============================================================================

test_that("compute_feasibility_score returns 0 when power < target", {
  score <- compute_feasibility_score(
    power = 0.6, n = 100, n_range = c(50, 500),
    target_power = 0.8, scale = "log", shape = "linear"
  )
  expect_equal(score, 0)
})

test_that("compute_feasibility_score returns 0 when power equals target minus epsilon", {
  score <- compute_feasibility_score(
    power = 0.7999, n = 200, n_range = c(50, 500),
    target_power = 0.8, scale = "log", shape = "linear"
  )
  expect_equal(score, 0)
})

test_that("compute_feasibility_score is 1 at n_min when power >= target (linear/log)", {
  score <- compute_feasibility_score(
    power = 0.9, n = 50, n_range = c(50, 500),
    target_power = 0.8, scale = "log", shape = "linear"
  )
  expect_equal(score, 1, tolerance = 1e-10)
})

test_that("compute_feasibility_score is 0 at n_max when power >= target (linear/log)", {
  score <- compute_feasibility_score(
    power = 0.9, n = 500, n_range = c(50, 500),
    target_power = 0.8, scale = "log", shape = "linear"
  )
  expect_equal(score, 0, tolerance = 1e-10)
})

test_that("compute_feasibility_score is 1 at n_min for all shape x scale combinations", {
  shapes <- c("linear", "quadratic", "root")
  scales <- c("log", "raw")
  for (shape in shapes) {
    for (scale in scales) {
      score <- compute_feasibility_score(
        power = 0.9, n = 50, n_range = c(50, 500),
        target_power = 0.8, scale = scale, shape = shape
      )
      expect_equal(score, 1, tolerance = 1e-10,
                   label = paste0("shape=", shape, ", scale=", scale))
    }
  }
})

test_that("compute_feasibility_score is 0 at n_max for all shape x scale combinations", {
  shapes <- c("linear", "quadratic", "root")
  scales <- c("log", "raw")
  for (shape in shapes) {
    for (scale in scales) {
      score <- compute_feasibility_score(
        power = 0.9, n = 500, n_range = c(50, 500),
        target_power = 0.8, scale = scale, shape = shape
      )
      expect_equal(score, 0, tolerance = 1e-10,
                   label = paste0("shape=", shape, ", scale=", scale))
    }
  }
})

test_that("compute_feasibility_score linear/log is monotonically decreasing for feasible points", {
  n_seq <- c(50, 100, 200, 350, 500)
  scores <- vapply(n_seq, function(n) {
    compute_feasibility_score(
      power = 0.9, n = n, n_range = c(50, 500),
      target_power = 0.8, scale = "log", shape = "linear"
    )
  }, numeric(1))
  expect_true(all(diff(scores) <= 0))
})

test_that("compute_feasibility_score linear/raw is monotonically decreasing for feasible points", {
  n_seq <- c(50, 100, 200, 350, 500)
  scores <- vapply(n_seq, function(n) {
    compute_feasibility_score(
      power = 0.9, n = n, n_range = c(50, 500),
      target_power = 0.8, scale = "raw", shape = "linear"
    )
  }, numeric(1))
  expect_true(all(diff(scores) <= 0))
})

test_that("compute_feasibility_score quadratic/log is monotonically decreasing for feasible points", {
  n_seq <- c(50, 100, 200, 350, 500)
  scores <- vapply(n_seq, function(n) {
    compute_feasibility_score(
      power = 0.9, n = n, n_range = c(50, 500),
      target_power = 0.8, scale = "log", shape = "quadratic"
    )
  }, numeric(1))
  expect_true(all(diff(scores) <= 0))
})

test_that("compute_feasibility_score quadratic/raw is monotonically decreasing for feasible points", {
  n_seq <- c(50, 100, 200, 350, 500)
  scores <- vapply(n_seq, function(n) {
    compute_feasibility_score(
      power = 0.9, n = n, n_range = c(50, 500),
      target_power = 0.8, scale = "raw", shape = "quadratic"
    )
  }, numeric(1))
  expect_true(all(diff(scores) <= 0))
})

test_that("compute_feasibility_score root/log is monotonically decreasing for feasible points", {
  n_seq <- c(50, 100, 200, 350, 500)
  scores <- vapply(n_seq, function(n) {
    compute_feasibility_score(
      power = 0.9, n = n, n_range = c(50, 500),
      target_power = 0.8, scale = "log", shape = "root"
    )
  }, numeric(1))
  expect_true(all(diff(scores) <= 0))
})

test_that("compute_feasibility_score root/raw is monotonically decreasing for feasible points", {
  n_seq <- c(50, 100, 200, 350, 500)
  scores <- vapply(n_seq, function(n) {
    compute_feasibility_score(
      power = 0.9, n = n, n_range = c(50, 500),
      target_power = 0.8, scale = "raw", shape = "root"
    )
  }, numeric(1))
  expect_true(all(diff(scores) <= 0))
})

test_that("compute_feasibility_score quadratic shape gives lower score than linear at interior points", {
  n_interior <- 200
  score_linear <- compute_feasibility_score(
    power = 0.9, n = n_interior, n_range = c(50, 500),
    target_power = 0.8, scale = "log", shape = "linear"
  )
  score_quadratic <- compute_feasibility_score(
    power = 0.9, n = n_interior, n_range = c(50, 500),
    target_power = 0.8, scale = "log", shape = "quadratic"
  )
  expect_lt(score_quadratic, score_linear)
})

test_that("compute_feasibility_score root shape gives higher score than linear at interior points", {
  n_interior <- 200
  score_linear <- compute_feasibility_score(
    power = 0.9, n = n_interior, n_range = c(50, 500),
    target_power = 0.8, scale = "log", shape = "linear"
  )
  score_root <- compute_feasibility_score(
    power = 0.9, n = n_interior, n_range = c(50, 500),
    target_power = 0.8, scale = "log", shape = "root"
  )
  expect_gt(score_root, score_linear)
})

test_that("compute_feasibility_score output is in [0, 1] for all combinations", {
  shapes <- c("linear", "quadratic", "root")
  scales <- c("log", "raw")
  n_seq <- c(50, 150, 300, 500)

  for (shape in shapes) {
    for (scale in scales) {
      # Feasible points
      for (n in n_seq) {
        score <- compute_feasibility_score(
          power = 0.9, n = n, n_range = c(50, 500),
          target_power = 0.8, scale = scale, shape = shape
        )
        expect_gte(score, 0, label = paste0("shape=", shape, ", scale=", scale, ", n=", n))
        expect_lte(score, 1, label = paste0("shape=", shape, ", scale=", scale, ", n=", n))
      }
      # Infeasible point
      score_infeasible <- compute_feasibility_score(
        power = 0.5, n = 100, n_range = c(50, 500),
        target_power = 0.8, scale = scale, shape = shape
      )
      expect_equal(score_infeasible, 0,
                   label = paste0("infeasible: shape=", shape, ", scale=", scale))
    }
  }
})

# =============================================================================
# generate_single_bo_initial()
# =============================================================================

test_that("generate_single_bo_initial returns a numeric vector", {
  result <- generate_single_bo_initial(n_range = c(50, 500), n_init = 5)
  expect_type(result, "double")
})

test_that("generate_single_bo_initial all points are within n_range", {
  n_range <- c(50, 500)
  result <- generate_single_bo_initial(n_range = n_range, n_init = 10)
  expect_true(all(result >= n_range[1]))
  expect_true(all(result <= n_range[2]))
})

test_that("generate_single_bo_initial includes n_min and n_max", {
  n_range <- c(50, 500)
  result <- generate_single_bo_initial(n_range = n_range, n_init = 5)
  expect_true(n_range[1] %in% result)
  expect_true(n_range[2] %in% result)
})

test_that("generate_single_bo_initial returns integer-valued points", {
  result <- generate_single_bo_initial(n_range = c(50, 500), n_init = 8)
  expect_equal(result, round(result))
})

test_that("generate_single_bo_initial returns at most n_init points (deduplication via unique)", {
  # unique() may reduce count if round() creates duplicates in a narrow range
  result <- generate_single_bo_initial(n_range = c(50, 500), n_init = 7)
  expect_lte(length(result), 7)
})

test_that("generate_single_bo_initial returns no duplicate values", {
  result <- generate_single_bo_initial(n_range = c(50, 500), n_init = 10)
  expect_equal(length(result), length(unique(result)))
})

test_that("generate_single_bo_initial is approximately log-spaced", {
  result <- generate_single_bo_initial(n_range = c(10, 1000), n_init = 5)
  # After log transform, points should be approximately equally spaced
  log_vals <- log(result)
  diffs <- diff(log_vals)
  # All gaps should be roughly equal (CV of gaps should be small)
  cv <- sd(diffs) / mean(diffs)
  expect_lt(cv, 0.2)
})

test_that("generate_single_bo_initial with n_init = 1 returns a single point", {
  result <- generate_single_bo_initial(n_range = c(50, 500), n_init = 1)
  expect_length(result, 1)
})

test_that("generate_single_bo_initial with n_init = 2 returns n_min and n_max", {
  n_range <- c(50, 500)
  result <- generate_single_bo_initial(n_range = n_range, n_init = 2)
  expect_setequal(result, n_range)
})
