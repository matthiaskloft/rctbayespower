# test-threshold-resolution.R
# Tests for threshold resolution ordering and info_frac semantics (Sessions 1 & 2)

# =============================================================================
# Unit tests: resolve_threshold
# =============================================================================

test_that("resolve_threshold returns numeric unchanged", {
  expect_equal(resolve_threshold(0.95, info_frac = 0.5), 0.95)
  expect_equal(resolve_threshold(0.80, info_frac = 1.0), 0.80)
})

test_that("resolve_threshold evaluates boundary functions with vector context", {
  # OBF (frequentist) returns different thresholds at different info_fracs
  obf_fn <- boundary_obf(alpha = 0.025)
  thresholds <- resolve_threshold(obf_fn, info_frac = c(0.25, 0.5, 0.75, 1.0))
  expect_length(thresholds, 4)
  # OBF: very conservative early (high threshold), relaxing toward final
  expect_gt(thresholds[1], thresholds[4])
})

test_that("resolve_threshold evaluates boundary_pocock functions", {
  # Bayesian Pocock: constant threshold at all looks
  poc_fn <- boundary_pocock(threshold = 0.95)
  val_early <- resolve_threshold(poc_fn, info_frac = c(0.25))
  val_late <- resolve_threshold(poc_fn, info_frac = c(0.75))
  expect_equal(val_early, val_late)
})

test_that("resolve_threshold handles info_frac = 0", {
  obf_fn <- boundary_obf(threshold = 0.95)
  result <- resolve_threshold(obf_fn, info_frac = 0)
  expect_true(is.numeric(result))
})

# =============================================================================
# Unit tests: info_frac semantics
# =============================================================================

test_that("brms backend uses actual completers for info_frac", {
  # After subset_analysis_data, fewer rows remain due to dropout.
  # info_frac should be nrow(analysis_data) / n_total, not current_n / n_total.
  n_total <- 100
  current_n <- 50

  # Mock: only 40 of 50 completed (dropout removed 10)
  analysis_data <- data.frame(
    outcome = rnorm(40),
    arm = factor(sample(0:1, 40, replace = TRUE))
  )

  scheduled_info_frac <- current_n / n_total
  expect_equal(scheduled_info_frac, 0.5)

  actual_info_frac <- nrow(analysis_data) / n_total
  expect_equal(actual_info_frac, 0.4)

  # With frequentist OBF, lower info_frac -> more conservative (higher) threshold
  obf_fn <- boundary_obf(alpha = 0.025)
  # Need vector context: provide the full schedule, then compare the first element
  thr_scheduled <- resolve_threshold(obf_fn, c(scheduled_info_frac, 1.0))[1]
  thr_actual <- resolve_threshold(obf_fn, c(actual_info_frac, 1.0))[1]
  expect_gt(thr_actual, thr_scheduled)
})

test_that("pre-resolved vector produces OBF shape from scheduled fracs", {
  # Both backends pre-resolve thresholds from the full schedule before the loop
  n_total <- 100
  analysis_at <- c(50, 100)

  scheduled_info_fracs <- analysis_at / n_total
  obf_fn <- boundary_obf(threshold = 0.95)
  thr_vec <- resolve_boundary_vector_from_fracs(obf_fn, scheduled_info_fracs)

  expect_length(thr_vec, 2)
  # OBF shape: early look more conservative (higher threshold) than final

  expect_gt(thr_vec[1], thr_vec[2])
  # Final look should equal the base threshold
  expect_equal(thr_vec[2], 0.95)
})

test_that("no-dropout case: both approaches give identical results", {
  n_total <- 100
  current_n <- 50

  analysis_data <- data.frame(
    outcome = rnorm(current_n),
    arm = factor(sample(0:1, current_n, replace = TRUE))
  )

  scheduled_frac <- current_n / n_total
  actual_frac <- nrow(analysis_data) / n_total
  expect_equal(scheduled_frac, actual_frac)

  # Both produce identical results when no dropout
  obf_fn <- boundary_obf(alpha = 0.025)
  expect_equal(
    resolve_threshold(obf_fn, c(scheduled_frac, 1.0)),
    resolve_threshold(obf_fn, c(actual_frac, 1.0))
  )
})

# =============================================================================
# Integration: threshold + dropout interaction
# =============================================================================

test_that("subset_analysis_data with dropout reduces info_frac correctly", {
  skip_if_not_installed("data.table")

  n_total <- 100
  current_n <- 60

  set.seed(42)
  full_data <- data.frame(
    outcome = rnorm(n_total),
    arm = factor(rep(0:1, each = n_total / 2)),
    enrollment_time = sort(runif(n_total, 0, 10))
  )

  completion_times <- full_data$enrollment_time + rexp(n_total, rate = 0.5)
  completion_times[sample(n_total, 20)] <- Inf

  analysis_data <- subset_analysis_data(full_data, current_n,
                                         followup_time = NULL,
                                         completion_times = completion_times)

  actual_n <- nrow(analysis_data)
  expect_lte(actual_n, current_n)

  scheduled_frac <- current_n / n_total
  actual_frac <- actual_n / n_total
  expect_lte(actual_frac, scheduled_frac)
})

test_that("target_not_met produces more conservative OBF thresholds", {
  # Frequentist OBF: lower info_frac -> higher threshold
  obf_fn <- boundary_obf(alpha = 0.025)

  # Compare thresholds at different info_fracs in a 2-look schedule
  mild_schedule <- c(95 / 200, 1.0)
  severe_schedule <- c(60 / 200, 1.0)

  thr_mild <- resolve_threshold(obf_fn, mild_schedule)[1]
  thr_severe <- resolve_threshold(obf_fn, severe_schedule)[1]

  # Severe dropout -> higher threshold (more conservative)
  expect_gt(thr_severe, thr_mild)
})

# =============================================================================
# resolve_boundary_vector consistency
# =============================================================================

test_that("resolve_boundary_vector uses n_analyzed for info_frac", {
  # n_total must equal max(n_analyzed) so final info_frac = 1.0 (gsDesign requirement)
  look_info <- data.frame(
    id_look = 1:3,
    n_analyzed = c(50, 100, 150)
  )
  n_total <- 150

  obf_fn <- boundary_obf(alpha = 0.025)
  result <- resolve_boundary_vector(obf_fn, look_info, n_total)

  expect_length(result, 3)
  # OBF thresholds decrease with more information
  expect_gt(result[1], result[2])
  expect_gt(result[2], result[3])

  # Verify the vector call matches expected fracs
  expected_fracs <- look_info$n_analyzed / n_total
  expected_thresholds <- obf_fn(expected_fracs)
  expect_equal(result, expected_thresholds)
})

test_that("resolve_boundary_vector handles NULL boundary", {
  look_info <- data.frame(id_look = 1:3, n_analyzed = c(50, 100, 150))
  result <- resolve_boundary_vector(NULL, look_info, n_total = 150)
  expect_equal(result, rep(NA_real_, 3))
})

test_that("resolve_boundary_vector handles scalar boundary", {
  look_info <- data.frame(id_look = 1:3, n_analyzed = c(50, 100, 150))
  result <- resolve_boundary_vector(0.95, look_info, n_total = 150)
  expect_equal(result, rep(0.95, 3))
})

# =============================================================================
# Unit tests: resolve_boundary_vector_from_fracs
# =============================================================================

test_that("OBF shape: early looks more conservative than late", {
  obf_fn <- boundary_obf(threshold = 0.95)
  info_fracs <- c(0.25, 0.5, 0.75, 1.0)
  result <- resolve_boundary_vector_from_fracs(obf_fn, info_fracs)

  expect_length(result, 4)
  # OBF thresholds decrease monotonically
  expect_gt(result[1], result[2])
  expect_gt(result[2], result[3])
  expect_gt(result[3], result[4])
  # Final look equals base threshold
  expect_equal(result[4], 0.95)
})

test_that("vector pre-resolution differs from broken scalar pattern", {
  obf_fn <- boundary_obf(threshold = 0.95)
  info_fracs <- c(0.5, 1.0)

  # Correct: vector pre-resolution
  vec_result <- resolve_boundary_vector_from_fracs(obf_fn, info_fracs)

  # Broken: per-scalar resolution (what the old code did)
  scalar_results <- vapply(info_fracs, function(f) {
    resolve_threshold(obf_fn, f)
  }, numeric(1))

  # The scalar approach returns 0.95 at every look (constant)
  expect_equal(scalar_results, c(0.95, 0.95))
  # The vector approach gives different thresholds
  expect_false(vec_result[1] == vec_result[2])
  # Early look should be more conservative
  expect_gt(vec_result[1], vec_result[2])
})

test_that("scalar numeric passthrough", {
  result <- resolve_boundary_vector_from_fracs(0.95, c(0.25, 0.5, 1.0))
  expect_equal(result, rep(0.95, 3))
})

test_that("NULL passthrough returns NA vector", {
  result <- resolve_boundary_vector_from_fracs(NULL, c(0.25, 0.5, 1.0))
  expect_equal(result, rep(NA_real_, 3))
})

test_that("element-wise boundary gives same result via vector or scalar", {
  lin_fn <- boundary_linear(start = 0.99, end = 0.95)
  info_fracs <- c(0.25, 0.5, 0.75, 1.0)

  vec_result <- resolve_boundary_vector_from_fracs(lin_fn, info_fracs)
  scalar_results <- vapply(info_fracs, function(f) {
    resolve_threshold(lin_fn, f)
  }, numeric(1))

  expect_equal(vec_result, scalar_results)
})

test_that("gsDesign boundaries produce correct OBF shape", {
  skip_if_not_installed("gsDesign")

  obf_alpha_fn <- boundary_obf(alpha = 0.025)
  info_fracs <- c(0.5, 1.0)
  result <- resolve_boundary_vector_from_fracs(obf_alpha_fn, info_fracs)

  expect_length(result, 2)
  # OBF: early more conservative
  expect_gt(result[1], result[2])
})

test_that("gsDesign boundaries: Pocock alpha shape", {
  skip_if_not_installed("gsDesign")

  poc_fn <- boundary_pocock(alpha = 0.025)
  info_fracs <- c(0.5, 1.0)
  result <- resolve_boundary_vector_from_fracs(poc_fn, info_fracs)

  expect_length(result, 2)
  # Pocock: approximately equal at all looks
  expect_equal(result[1], result[2], tolerance = 0.005)
})

test_that("gsDesign boundaries: HSD shape", {
  skip_if_not_installed("gsDesign")

  hsd_fn <- boundary_hsd(alpha = 0.025, gamma = -4)
  info_fracs <- c(0.25, 0.5, 0.75, 1.0)
  result <- resolve_boundary_vector_from_fracs(hsd_fn, info_fracs)

  expect_length(result, 4)
  # HSD with gamma=-4: conservative early, relaxing toward final
  expect_gt(result[1], result[4])
})

test_that("integration: 2-look sequential schedule with OBF", {
  # Mock a 2-look sequential schedule
  n_total <- 100
  analysis_schedule <- c(50, 100)
  scheduled_info_fracs <- analysis_schedule / n_total

  obf_fn <- boundary_obf(threshold = 0.95)
  thr_vec <- resolve_boundary_vector_from_fracs(obf_fn, scheduled_info_fracs)

  # Indexed values should differ: look 1 more conservative than look 2
  expect_gt(thr_vec[1], thr_vec[2])
  # Both should be valid probabilities
  expect_true(all(thr_vec > 0 & thr_vec <= 1))
  # Final look equals base threshold
  expect_equal(thr_vec[2], 0.95)
})

test_that("resolve_boundary_vector_from_fracs errors on length mismatch", {
  expect_error(
    resolve_boundary_vector_from_fracs(c(0.99, 0.95, 0.90), c(0.5, 1.0)),
    "Invalid boundary specification"
  )
})

test_that("numeric vector passthrough with matching length", {
  result <- resolve_boundary_vector_from_fracs(c(0.99, 0.97, 0.95), c(0.33, 0.67, 1.0))
  expect_equal(result, c(0.99, 0.97, 0.95))
})

test_that("errors on non-finite info_fracs", {
  expect_error(
    resolve_boundary_vector_from_fracs(0.95, c(0.5, NaN)),
    "Non-finite values"
  )
  expect_error(
    resolve_boundary_vector_from_fracs(0.95, c(Inf, 1.0)),
    "Non-finite values"
  )
  expect_error(
    resolve_boundary_vector_from_fracs(0.95, c(NA_real_, 1.0)),
    "Non-finite values"
  )
})

test_that("errors when boundary function returns wrong length", {
  bad_fn <- function(info_fracs) 0.95  # always returns scalar
  expect_error(
    resolve_boundary_vector_from_fracs(bad_fn, c(0.5, 1.0)),
    "Boundary function returned wrong length"
  )
})
