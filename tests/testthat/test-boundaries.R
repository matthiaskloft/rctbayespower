# Tests for stopping boundary functions (R/boundaries.R)

# =============================================================================
# boundary_obf() - O'Brien-Fleming
# =============================================================================

test_that("boundary_obf creates valid boundary function with threshold", {
  f <- boundary_obf(threshold = 0.95)
  expect_s3_class(f, "boundary_function")
  expect_true(is.function(f))
  expect_equal(attr(f, "boundary_type"), "obf")
  expect_equal(attr(f, "boundary_params")$threshold, 0.95)
})

test_that("boundary_obf with threshold produces correct shape", {
  f <- boundary_obf(threshold = 0.95)
  thresholds <- f(c(0.25, 0.50, 0.75, 1.0))

  expect_length(thresholds, 4)
  # OBF: very conservative early, relaxes to threshold at final
  expect_true(all(thresholds >= 0.95))
  expect_equal(thresholds[4], 0.95, tolerance = 1e-10)
  # Decreasing: first threshold > last threshold
  expect_true(thresholds[1] > thresholds[4])
})

test_that("boundary_obf with alpha creates valid function", {
  f <- boundary_obf(alpha = 0.025)
  expect_s3_class(f, "boundary_function")
  thresholds <- f(c(0.5, 1.0))
  expect_length(thresholds, 2)
  # All thresholds should be valid probabilities

  expect_true(all(thresholds > 0.5 & thresholds < 1))
})

test_that("boundary_obf rejects both alpha and threshold", {
  expect_cli_abort(boundary_obf(alpha = 0.025, threshold = 0.95))
})

test_that("boundary_obf rejects neither alpha nor threshold", {
  expect_cli_abort(boundary_obf())
})

test_that("boundary_obf validates alpha range", {
  expect_cli_abort(boundary_obf(alpha = 0))
  expect_cli_abort(boundary_obf(alpha = 0.6))
  expect_cli_abort(boundary_obf(alpha = -0.1))
})

test_that("boundary_obf validates threshold range", {
  expect_cli_abort(boundary_obf(threshold = 0.5))
  expect_cli_abort(boundary_obf(threshold = 1.0))
  expect_cli_abort(boundary_obf(threshold = 0.3))
})

# =============================================================================
# boundary_pocock()
# =============================================================================

test_that("boundary_pocock with threshold returns constant", {
  f <- boundary_pocock(threshold = 0.95)
  thresholds <- f(c(0.25, 0.50, 0.75, 1.0))
  expect_equal(thresholds, rep(0.95, 4))
})

test_that("boundary_pocock with alpha creates valid function", {
  f <- boundary_pocock(alpha = 0.025)
  expect_s3_class(f, "boundary_function")
  thresholds <- f(c(0.5, 1.0))
  expect_length(thresholds, 2)
  expect_true(all(thresholds > 0.5 & thresholds < 1))
})

test_that("boundary_pocock rejects both parameters", {
  expect_cli_abort(boundary_pocock(alpha = 0.025, threshold = 0.95))
})

test_that("boundary_pocock rejects neither parameter", {
  expect_cli_abort(boundary_pocock())
})

# =============================================================================
# boundary_constant()
# =============================================================================

test_that("boundary_constant returns fixed threshold", {
  f <- boundary_constant(0.95)
  expect_equal(f(c(0.25, 0.5, 0.75, 1.0)), rep(0.95, 4))
  expect_equal(f(1.0), 0.95)
})

test_that("boundary_constant validates threshold range", {
  expect_cli_abort(boundary_constant(-0.1))
  expect_cli_abort(boundary_constant(1.5))
})

test_that("boundary_constant has correct metadata", {
  f <- boundary_constant(0.90)
  expect_equal(attr(f, "boundary_type"), "constant")
  expect_equal(attr(f, "boundary_params")$threshold, 0.90)
})

# =============================================================================
# boundary_linear()
# =============================================================================

test_that("boundary_linear interpolates correctly", {
  f <- boundary_linear(start = 0.999, end = 0.975)
  expect_equal(f(0), 0.999, tolerance = 1e-10)
  expect_equal(f(1), 0.975, tolerance = 1e-10)
  # Midpoint
  expect_equal(f(0.5), (0.999 + 0.975) / 2, tolerance = 1e-10)
})

test_that("boundary_linear works for futility (ascending)", {
  f <- boundary_linear(start = 0.30, end = 0.50)
  thresholds <- f(c(0, 0.5, 1.0))
  expect_equal(thresholds, c(0.30, 0.40, 0.50), tolerance = 1e-10)
})

test_that("boundary_linear validates range", {
  expect_cli_abort(boundary_linear(start = -0.1, end = 0.5))
  expect_cli_abort(boundary_linear(start = 0.5, end = 1.5))
})

test_that("boundary_linear works with vector input", {
  f <- boundary_linear(start = 1.0, end = 0.0)
  result <- f(c(0.25, 0.50, 0.75, 1.0))
  expect_equal(result, c(0.75, 0.50, 0.25, 0.0), tolerance = 1e-10)
})

# =============================================================================
# boundary_power()
# =============================================================================

test_that("boundary_power produces correct values at endpoints", {
  f <- boundary_power(base = 0.975, rho = 2)
  # At info_frac = 1, threshold should equal base
  expect_equal(f(1.0), 0.975, tolerance = 1e-10)
  # At info_frac approaching 0, threshold approaches 1
  expect_true(f(0.01) > 0.999)
})

test_that("boundary_power rho controls shape", {
  f_steep <- boundary_power(base = 0.95, rho = 3)
  f_flat <- boundary_power(base = 0.95, rho = 0.5)

  # At midpoint, higher rho should be more conservative (higher threshold)
  expect_true(f_steep(0.5) > f_flat(0.5))
})

test_that("boundary_power validates parameters", {
  expect_cli_abort(boundary_power(base = -0.1))
  expect_cli_abort(boundary_power(base = 1.5))
  expect_cli_abort(boundary_power(rho = 0))
  expect_cli_abort(boundary_power(rho = -1))
})

# =============================================================================
# boundary_hsd()
# =============================================================================

test_that("boundary_hsd validates parameters", {
  expect_cli_abort(boundary_hsd(alpha = 0))
  expect_cli_abort(boundary_hsd(alpha = 0.6))
  expect_cli_abort(boundary_hsd(gamma = -50))
  expect_cli_abort(boundary_hsd(gamma = 50))
})

test_that("boundary_hsd creates valid function", {
  f <- boundary_hsd(alpha = 0.025, gamma = -4)
  expect_s3_class(f, "boundary_function")
  expect_equal(attr(f, "boundary_type"), "hsd")
})

test_that("boundary_hsd requires gsDesign at call time", {
  skip_if_not_installed("gsDesign")
  f <- boundary_hsd()
  thresholds <- f(c(0.5, 1.0))
  expect_length(thresholds, 2)
  expect_true(all(thresholds > 0.5 & thresholds < 1))
})

# =============================================================================
# resolve_threshold()
# =============================================================================

test_that("resolve_threshold handles numeric values", {
  expect_equal(rctbayespower:::resolve_threshold(0.95, 0.5), 0.95)
})

test_that("resolve_threshold handles boundary functions", {
  f <- boundary_constant(0.90)
  expect_equal(rctbayespower:::resolve_threshold(f, 0.5), 0.90)
})

# =============================================================================
# resolve_boundary_vector()
# =============================================================================

test_that("resolve_boundary_vector handles scalar", {
  look_info <- data.frame(id_look = 1:3, n_analyzed = c(50, 100, 150))
  result <- rctbayespower:::resolve_boundary_vector(0.95, look_info, 150)
  expect_equal(result, rep(0.95, 3))
})

test_that("resolve_boundary_vector handles vector", {
  look_info <- data.frame(id_look = 1:3, n_analyzed = c(50, 100, 150))
  result <- rctbayespower:::resolve_boundary_vector(c(0.99, 0.97, 0.95), look_info, 150)
  expect_equal(result, c(0.99, 0.97, 0.95))
})

test_that("resolve_boundary_vector handles NULL", {
  look_info <- data.frame(id_look = 1:2, n_analyzed = c(50, 100))
  result <- rctbayespower:::resolve_boundary_vector(NULL, look_info, 100)
  expect_equal(result, c(NA_real_, NA_real_))
})

test_that("resolve_boundary_vector handles function", {
  look_info <- data.frame(id_look = 1:2, n_analyzed = c(50, 100))
  f <- boundary_constant(0.95)
  result <- rctbayespower:::resolve_boundary_vector(f, look_info, 100)
  expect_equal(result, c(0.95, 0.95))
})

test_that("resolve_boundary_vector rejects wrong-length vector", {
  look_info <- data.frame(id_look = 1:3, n_analyzed = c(50, 100, 150))
  expect_cli_abort(
    rctbayespower:::resolve_boundary_vector(c(0.95, 0.90), look_info, 150)
  )
})

# =============================================================================
# show_boundaries()
# =============================================================================

test_that("show_boundaries returns boundary names invisibly", {
  output <- capture_cli(out <- show_boundaries())
  expect_type(out, "character")
  expect_true("boundary_obf" %in% out)
  expect_true("boundary_wang_tsiatis" %in% out)
  expect_true("boundary_linear" %in% out)
})

# =============================================================================
# boundary_wang_tsiatis() - Threshold mode
# =============================================================================

test_that("boundary_wang_tsiatis creates valid boundary function with threshold", {
  f <- boundary_wang_tsiatis(delta = 0.25, threshold = 0.95)
  expect_s3_class(f, "boundary_function")
  expect_true(is.function(f))
  expect_equal(attr(f, "boundary_type"), "wang_tsiatis")
  expect_equal(attr(f, "boundary_params")$delta, 0.25)
  expect_equal(attr(f, "boundary_params")$threshold, 0.95)
})

test_that("boundary_wang_tsiatis delta=0.5 produces constant threshold", {
  f <- boundary_wang_tsiatis(delta = 0.5, threshold = 0.95)
  thresholds <- f(c(0.25, 0.5, 0.75, 1.0))
  expect_equal(thresholds, rep(0.95, 4))
})

test_that("boundary_wang_tsiatis delta=0 produces OBF-like shape", {
  f <- boundary_wang_tsiatis(delta = 0, threshold = 0.95)
  thresholds <- f(c(0.25, 0.50, 0.75, 1.0))

  expect_length(thresholds, 4)
  # Decreasing: very conservative early, relaxing late
  expect_true(thresholds[1] > thresholds[4])
  # Final look equals threshold
  expect_equal(thresholds[4], 0.95, tolerance = 1e-10)
  # All thresholds >= final threshold (within floating-point tolerance)
  expect_true(all(thresholds >= 0.95 - 1e-10))
})

test_that("boundary_wang_tsiatis delta=0.25 is between OBF and constant", {
  f_obf <- boundary_wang_tsiatis(delta = 0, threshold = 0.95)
  f_mid <- boundary_wang_tsiatis(delta = 0.25, threshold = 0.95)
  f_poc <- boundary_wang_tsiatis(delta = 0.5, threshold = 0.95)

  t_obf <- f_obf(c(0.25, 0.5, 0.75))
  t_mid <- f_mid(c(0.25, 0.5, 0.75))
  t_poc <- f_poc(c(0.25, 0.5, 0.75))

  # Midpoint should be between OBF and Pocock at early looks
  expect_true(all(t_mid < t_obf))
  expect_true(all(t_mid > t_poc))
})

test_that("boundary_wang_tsiatis final look equals threshold for all delta", {
  for (d in c(0, 0.1, 0.25, 0.4, 0.5)) {
    f <- boundary_wang_tsiatis(delta = d, threshold = 0.95)
    expect_equal(f(1.0), 0.95, tolerance = 1e-10,
                 info = paste("delta =", d))
  }
})

test_that("boundary_wang_tsiatis threshold mode works with single look", {
  f <- boundary_wang_tsiatis(delta = 0.25, threshold = 0.95)
  expect_equal(f(1.0), 0.95, tolerance = 1e-10)
})

test_that("boundary_wang_tsiatis delta=0 threshold differs from boundary_obf threshold", {
  # WT threshold mode uses pnorm(qnorm(threshold) * t^(delta-0.5))

  # OBF threshold mode uses spending-function normalization
  # Same qualitative shape but different formulas — not numerically identical
  wt <- boundary_wang_tsiatis(delta = 0, threshold = 0.95)
  obf <- boundary_obf(threshold = 0.95)

  info_frac <- c(0.25, 0.5, 0.75, 1.0)
  wt_vals <- wt(info_frac)
  obf_vals <- obf(info_frac)

  # Both end at 0.95
  expect_equal(wt_vals[4], 0.95, tolerance = 1e-10)
  expect_equal(obf_vals[4], 0.95, tolerance = 1e-10)
  # Both are decreasing (conservative early)
  expect_true(wt_vals[1] > wt_vals[4])
  expect_true(obf_vals[1] > obf_vals[4])
  # But they are NOT identical at intermediate points
  expect_false(isTRUE(all.equal(wt_vals[1:3], obf_vals[1:3])))
})

# =============================================================================
# boundary_wang_tsiatis() - Alpha mode
# =============================================================================

test_that("boundary_wang_tsiatis creates valid function with alpha", {
  skip_if_not_installed("gsDesign")
  f <- boundary_wang_tsiatis(delta = 0.25, alpha = 0.025)
  expect_s3_class(f, "boundary_function")
  expect_equal(attr(f, "boundary_type"), "wang_tsiatis")
  expect_equal(attr(f, "boundary_params")$delta, 0.25)
  expect_equal(attr(f, "boundary_params")$alpha, 0.025)
})

test_that("boundary_wang_tsiatis delta=0 alpha matches boundary_obf alpha", {
  skip_if_not_installed("gsDesign")
  wt <- boundary_wang_tsiatis(delta = 0, alpha = 0.025)
  obf <- boundary_obf(alpha = 0.025)

  info_frac <- c(0.5, 1.0)
  expect_equal(wt(info_frac), obf(info_frac), tolerance = 1e-6)
})

test_that("boundary_wang_tsiatis delta=0.5 alpha matches boundary_pocock alpha", {
  skip_if_not_installed("gsDesign")
  wt <- boundary_wang_tsiatis(delta = 0.5, alpha = 0.025)
  poc <- boundary_pocock(alpha = 0.025)

  info_frac <- c(0.5, 1.0)
  expect_equal(wt(info_frac), poc(info_frac), tolerance = 1e-6)
})

test_that("boundary_wang_tsiatis delta=0.25 alpha produces valid boundaries", {
  skip_if_not_installed("gsDesign")
  f <- boundary_wang_tsiatis(delta = 0.25, alpha = 0.025)
  thresholds <- f(c(0.25, 0.5, 0.75, 1.0))

  expect_length(thresholds, 4)
  expect_true(all(thresholds > 0.5 & thresholds < 1))
  # Should be decreasing (conservative early, relaxing late)
  expect_true(thresholds[1] > thresholds[4])
})

test_that("boundary_wang_tsiatis alpha mode controls Type I error", {
  skip_if_not_installed("gsDesign")
  f <- boundary_wang_tsiatis(delta = 0.25, alpha = 0.025)
  info_frac <- c(0.25, 0.5, 0.75, 1.0)
  thresholds <- f(info_frac)

  # Convert back to z-scale and verify rejection probability ~ alpha
  z_bounds <- stats::qnorm(thresholds)
  prob <- gsDesign::gsProbability(
    k = 4, theta = 0, n.I = info_frac,
    a = rep(-20, 4), b = z_bounds
  )
  total_rejection <- sum(prob$upper$prob)
  expect_equal(total_rejection, 0.025, tolerance = 1e-4)
})

test_that("boundary_wang_tsiatis alpha mode single look", {
  skip_if_not_installed("gsDesign")
  f <- boundary_wang_tsiatis(delta = 0.25, alpha = 0.025)
  expect_equal(f(1.0), 0.975, tolerance = 1e-10)
})

test_that("boundary_wang_tsiatis alpha factory succeeds without gsDesign", {
  f <- boundary_wang_tsiatis(delta = 0.25, alpha = 0.025)
  expect_s3_class(f, "boundary_function")
})

# =============================================================================
# boundary_wang_tsiatis() - Validation
# =============================================================================

test_that("boundary_wang_tsiatis validates delta range", {
  expect_cli_abort(boundary_wang_tsiatis(delta = -0.1, threshold = 0.95))
  expect_cli_abort(boundary_wang_tsiatis(delta = 0.6, threshold = 0.95))
  expect_cli_abort(boundary_wang_tsiatis(delta = "a", threshold = 0.95))
  expect_cli_abort(boundary_wang_tsiatis(delta = c(0.1, 0.2), threshold = 0.95))
})

test_that("boundary_wang_tsiatis rejects both alpha and threshold", {
  expect_cli_abort(
    boundary_wang_tsiatis(delta = 0.25, alpha = 0.025, threshold = 0.95)
  )
})

test_that("boundary_wang_tsiatis rejects neither alpha nor threshold", {
  expect_cli_abort(boundary_wang_tsiatis(delta = 0.25))
})

test_that("boundary_wang_tsiatis validates alpha range", {
  expect_cli_abort(boundary_wang_tsiatis(delta = 0.25, alpha = 0))
  expect_cli_abort(boundary_wang_tsiatis(delta = 0.25, alpha = 0.6))
})

test_that("boundary_wang_tsiatis validates threshold range", {
  expect_cli_abort(boundary_wang_tsiatis(delta = 0.25, threshold = 0))
  expect_cli_abort(boundary_wang_tsiatis(delta = 0.25, threshold = 1))
})
