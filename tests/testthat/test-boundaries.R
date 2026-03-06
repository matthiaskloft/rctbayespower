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
  result <- capture.output(out <- show_boundaries())
  expect_type(out, "character")
  expect_true("boundary_obf" %in% out)
  expect_true("boundary_linear" %in% out)
})
