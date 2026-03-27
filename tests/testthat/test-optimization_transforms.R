# Tests for optimization transform functions (R/optimization_transforms.R)

# =============================================================================
# LOGIT / INVLOGIT TRANSFORMS
# =============================================================================

test_that("logit_transform and invlogit_transform roundtrip recovers input", {
  p_vals <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  for (p in p_vals) {
    expect_equal(invlogit_transform(logit_transform(p)), p, tolerance = 1e-10)
  }
})

test_that("logit_transform maps (0,1) to real line with correct direction", {
  # Values below 0.5 should map to negative logits
  expect_lt(logit_transform(0.3), 0)
  # Values above 0.5 should map to positive logits
  expect_gt(logit_transform(0.7), 0)
  # 0.5 maps to 0
  expect_equal(logit_transform(0.5), 0, tolerance = 1e-10)
})

test_that("logit_transform clips boundary values instead of returning Inf/-Inf", {
  # Exact 0 and 1 should be clipped, not produce Inf/-Inf
  result_zero <- logit_transform(0)
  result_one  <- logit_transform(1)
  expect_true(is.finite(result_zero))
  expect_true(is.finite(result_one))
  expect_lt(result_zero, -10)
  expect_gt(result_one, 10)
})

test_that("logit_transform handles custom eps clipping", {
  eps <- 0.01
  result_zero <- logit_transform(0, eps = eps)
  result_one  <- logit_transform(1, eps = eps)
  # With eps = 0.01, logit(0.01) = log(0.01/0.99) ≈ -4.595
  expect_equal(result_zero, log(eps / (1 - eps)), tolerance = 1e-10)
  expect_equal(result_one, log((1 - eps) / eps), tolerance = 1e-10)
})

test_that("logit_transform is vectorized", {
  p <- c(0.2, 0.5, 0.8)
  result <- logit_transform(p)
  expect_length(result, 3)
  expect_equal(invlogit_transform(result), p, tolerance = 1e-10)
})

test_that("invlogit_transform output is always in (0, 1)", {
  x_vals <- c(-10, -1, 0, 1, 10)
  result <- invlogit_transform(x_vals)
  expect_true(all(result > 0 & result < 1))
})

test_that("invlogit_transform is symmetric around 0", {
  x <- 2.0
  expect_equal(invlogit_transform(x) + invlogit_transform(-x), 1, tolerance = 1e-10)
})

# =============================================================================
# compute_p_feas()
# =============================================================================

test_that("compute_p_feas returns value in [0, 1]", {
  result <- compute_p_feas(mu = 0, sigma = 1, target = 0.8)
  expect_gte(result, 0)
  expect_lte(result, 1)
})

test_that("compute_p_feas returns ~0.5 when mu equals logit(target)", {
  target <- 0.8
  logit_target <- log(target / (1 - target))
  result <- compute_p_feas(mu = logit_target, sigma = 1, target = target)
  expect_equal(result, 0.5, tolerance = 1e-10)
})

test_that("compute_p_feas increases as mu increases", {
  # Higher mean on logit scale → higher P(power >= target)
  r1 <- compute_p_feas(mu = -2, sigma = 1, target = 0.8)
  r2 <- compute_p_feas(mu = 0,  sigma = 1, target = 0.8)
  r3 <- compute_p_feas(mu = 2,  sigma = 1, target = 0.8)
  expect_lt(r1, r2)
  expect_lt(r2, r3)
})

test_that("compute_p_feas with very small sigma is near 0 or 1", {
  target <- 0.8
  logit_target <- log(target / (1 - target))
  # mu well below logit(target): near 0
  r_low  <- compute_p_feas(mu = logit_target - 5, sigma = 1e-6, target = target)
  # mu well above logit(target): near 1
  r_high <- compute_p_feas(mu = logit_target + 5, sigma = 1e-6, target = target)
  expect_lt(r_low, 0.01)
  expect_gt(r_high, 0.99)
})

test_that("compute_p_feas handles sigma = 0 gracefully (eps floor)", {
  # sigma = 0 would cause division by zero; eps floor prevents NaN
  result <- compute_p_feas(mu = 0, sigma = 0, target = 0.8)
  expect_true(is.finite(result))
})

test_that("compute_p_feas is vectorized over mu", {
  mus <- c(-2, 0, 2)
  result <- compute_p_feas(mu = mus, sigma = 1, target = 0.8)
  expect_length(result, 3)
  expect_true(all(result >= 0 & result <= 1))
})

# =============================================================================
# ILR TRANSFORMS: ilr_inverse() and ilr_forward()
# =============================================================================

test_that("ilr_inverse output sums to 1 for k=2", {
  y <- 1.5
  p <- ilr_inverse(y)
  expect_equal(sum(p), 1, tolerance = 1e-10)
  expect_true(all(p > 0))
})

test_that("ilr_inverse output sums to 1 for k=3", {
  y <- c(0.5, -0.3)
  p <- ilr_inverse(y)
  expect_equal(sum(p), 1, tolerance = 1e-10)
  expect_true(all(p > 0))
  expect_length(p, 3)
})

test_that("ilr_inverse output sums to 1 for k=4", {
  y <- c(1.0, -0.5, 0.2)
  p <- ilr_inverse(y)
  expect_equal(sum(p), 1, tolerance = 1e-10)
  expect_true(all(p > 0))
  expect_length(p, 4)
})

test_that("ilr_inverse at zero vector gives uniform distribution", {
  # y = 0 → all components equal
  for (k in 2:4) {
    y <- rep(0, k - 1)
    p <- ilr_inverse(y)
    expect_equal(p, rep(1 / k, k), tolerance = 1e-10)
  }
})

test_that("ilr_forward and ilr_inverse roundtrip for k=2", {
  p_orig <- c(0.3, 0.7)
  y <- ilr_forward(p_orig)
  p_back <- ilr_inverse(y)
  expect_equal(p_back, p_orig, tolerance = 1e-10)
})

test_that("ilr_forward and ilr_inverse roundtrip for k=3", {
  p_orig <- c(0.2, 0.5, 0.3)
  y <- ilr_forward(p_orig)
  expect_length(y, 2)
  p_back <- ilr_inverse(y)
  expect_equal(p_back, p_orig, tolerance = 1e-10)
})

test_that("ilr_forward and ilr_inverse roundtrip for k=4", {
  p_orig <- c(0.1, 0.4, 0.3, 0.2)
  y <- ilr_forward(p_orig)
  expect_length(y, 3)
  p_back <- ilr_inverse(y)
  expect_equal(p_back, p_orig, tolerance = 1e-10)
})

test_that("ilr_forward returns k-1 coordinates for k-simplex", {
  for (k in 2:5) {
    p <- rep(1 / k, k)
    y <- ilr_forward(p)
    expect_length(y, k - 1)
  }
})

test_that("ilr_forward of uniform simplex returns zero vector", {
  for (k in 2:4) {
    p <- rep(1 / k, k)
    y <- ilr_forward(p)
    expect_equal(y, rep(0, k - 1), tolerance = 1e-10)
  }
})

# =============================================================================
# constrained_simplex()
# =============================================================================

test_that("constrained_simplex output sums to 1 when q sums to 1", {
  # q is a uniform simplex
  q <- c(0.25, 0.25, 0.25, 0.25)
  min_prop <- 0.05
  k <- 4
  p <- constrained_simplex(q, min_prop, k)
  expect_equal(sum(p), 1, tolerance = 1e-10)
})

test_that("constrained_simplex enforces min_prop lower bound", {
  # Even the smallest q component should give >= min_prop
  q <- c(0.01, 0.49, 0.5)
  min_prop <- 0.1
  k <- 3
  p <- constrained_simplex(q, min_prop, k)
  expect_true(all(p >= min_prop))
})

test_that("constrained_simplex with q = uniform gives uniform p", {
  q <- c(1/3, 1/3, 1/3)
  min_prop <- 0.1
  k <- 3
  p <- constrained_simplex(q, min_prop, k)
  expect_equal(p, c(1/3, 1/3, 1/3), tolerance = 1e-10)
})

test_that("constrained_simplex with min_prop = 0 is identity", {
  q <- c(0.2, 0.5, 0.3)
  p <- constrained_simplex(q, min_prop = 0, k = 3)
  expect_equal(p, q, tolerance = 1e-10)
})

test_that("constrained_simplex output sum is preserved across min_prop values", {
  q <- ilr_inverse(c(1.0, -0.5))  # valid simplex
  for (min_prop in c(0.0, 0.05, 0.1, 0.15)) {
    k <- 3
    if (min_prop * k < 1) {
      p <- constrained_simplex(q, min_prop, k)
      expect_equal(sum(p), 1, tolerance = 1e-10)
    }
  }
})

# =============================================================================
# apply_simplex_transforms() — structured return
# =============================================================================

test_that("apply_simplex_transforms returns list with crossed, ilr_values, simplex_values", {
  xs <- list(p_alloc = 0.6, n_total = 100)
  specs <- list(
    p_alloc = list(type = "p_alloc", n_arms = 2, n_dims = 1, min_prop = 0.1)
  )
  result <- apply_simplex_transforms(xs, specs)

  expect_type(result, "list")
  expect_named(result, c("crossed", "ilr_values", "simplex_values"))
})

test_that("apply_simplex_transforms 2-arm: crossed p_alloc is list(c(ctrl, treat))", {
  xs <- list(p_alloc = 0.6, n_total = 100)
  specs <- list(
    p_alloc = list(type = "p_alloc", n_arms = 2, n_dims = 1, min_prop = 0.1)
  )
  result <- apply_simplex_transforms(xs, specs)

  # treat = 0.6, ctrl = 0.4
  expect_equal(result$crossed$p_alloc, list(c(0.4, 0.6)))
  expect_equal(result$simplex_values$p_alloc, c(0.4, 0.6))
})

test_that("apply_simplex_transforms 2-arm: non-simplex params are preserved in crossed", {
  xs <- list(p_alloc = 0.5, n_total = 200)
  specs <- list(
    p_alloc = list(type = "p_alloc", n_arms = 2, n_dims = 1, min_prop = 0.1)
  )
  result <- apply_simplex_transforms(xs, specs)
  expect_equal(result$crossed$n_total, 200)
})

test_that("apply_simplex_transforms k-arm: ILR params are removed from crossed", {
  xs <- list(
    p_alloc_ilr_1 = 0.5,
    p_alloc_ilr_2 = -0.3,
    n_total = 100
  )
  specs <- list(
    p_alloc = list(type = "p_alloc", n_arms = 3, n_dims = 2, min_prop = 0.1)
  )
  result <- apply_simplex_transforms(xs, specs)

  # ILR sub-params should be removed
  expect_null(result$crossed$p_alloc_ilr_1)
  expect_null(result$crossed$p_alloc_ilr_2)
  # Main param should appear
  expect_false(is.null(result$crossed$p_alloc))
})

test_that("apply_simplex_transforms k-arm: simplex sums to 1 after transform", {
  xs <- list(
    p_alloc_ilr_1 = 0.8,
    p_alloc_ilr_2 = -0.4,
    n_total = 100
  )
  specs <- list(
    p_alloc = list(type = "p_alloc", n_arms = 3, n_dims = 2, min_prop = 0.05)
  )
  result <- apply_simplex_transforms(xs, specs)

  p <- result$simplex_values$p_alloc
  expect_equal(sum(p), 1, tolerance = 1e-10)
  expect_true(all(p >= 0.05))
})

test_that("apply_simplex_transforms handles empty search_specs (no transforms)", {
  xs <- list(n_total = 150)
  specs <- list()
  result <- apply_simplex_transforms(xs, specs)

  # No transforms: crossed should equal xs
  expect_equal(result$crossed$n_total, 150)
  expect_length(result$ilr_values, 0)
  expect_length(result$simplex_values, 0)
})

test_that("apply_simplex_transforms 2-look: interim_prop preserved in crossed", {
  xs <- list(interim_prop = 0.4, n_total = 100)
  specs <- list(
    interim_prop = list(
      type = "looks", n_looks = 2, n_dims = 1, min_spacing = 0.1
    )
  )
  result <- apply_simplex_transforms(xs, specs)
  expect_equal(result$crossed$interim_prop, 0.4)
  expect_equal(result$simplex_values$interim_prop, 0.4)
})
