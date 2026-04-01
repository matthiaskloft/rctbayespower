# Tests for S7 class rctbp_sample_size_result (R/class_sample_size_result.R)

# covr instrumentation can interfere with S7 class construction
skip_if(identical(Sys.getenv("R_COVR"), "true"), "S7 tests unreliable under covr")

# =============================================================================
# HELPERS
# =============================================================================

make_minimal_archive <- function() {
  data.frame(
    n_total = c(50L, 100L, 200L),
    power   = c(0.60, 0.75, 0.88),
    score   = c(1.00, 0.50, 0.00),
    n_sims  = c(100L, 100L, 100L),
    stringsAsFactors = FALSE
  )
}

make_minimal_convergence <- function() {
  arch <- make_minimal_archive()
  data.frame(
    eval       = seq_len(nrow(arch)),
    n_total    = arch$n_total,
    power      = arch$power,
    score      = arch$score,
    best_score = cummax(arch$score),
    stringsAsFactors = FALSE
  )
}

make_result <- function(...) {
  defaults <- list(
    design        = mock_design(),
    n_optimal     = 100L,
    power_optimal = 0.75,
    target_power  = 0.80,
    feasible      = TRUE,
    convergence   = make_minimal_convergence(),
    archive       = make_minimal_archive(),
    surrogate_fit = NULL,
    surrogate_type = "gp_power",
    score_config  = list(scale = "log", shape = "linear"),
    n_sims        = 100,
    n_evals       = 3,
    elapsed_time  = 0.5
  )
  args <- utils::modifyList(defaults, list(...))
  do.call(rctbp_sample_size_result, args)
}

# =============================================================================
# CONSTRUCTION: valid object
# =============================================================================

test_that("rctbp_sample_size_result constructs with valid properties", {
  result <- make_result()
  expect_s3_class(result, "rctbayespower::rctbp_sample_size_result")
})

test_that("rctbp_sample_size_result stores n_optimal correctly", {
  result <- make_result(n_optimal = 120L)
  expect_equal(result@n_optimal, 120L)
})

test_that("rctbp_sample_size_result stores power_optimal correctly", {
  result <- make_result(power_optimal = 0.82)
  expect_equal(result@power_optimal, 0.82)
})

test_that("rctbp_sample_size_result stores target_power correctly", {
  result <- make_result(target_power = 0.90)
  expect_equal(result@target_power, 0.90)
})

test_that("rctbp_sample_size_result stores feasible = FALSE correctly", {
  result <- make_result(feasible = FALSE)
  expect_false(result@feasible)
})

test_that("rctbp_sample_size_result stores feasible = TRUE correctly", {
  result <- make_result(feasible = TRUE)
  expect_true(result@feasible)
})

test_that("rctbp_sample_size_result stores surrogate_type correctly", {
  result <- make_result(surrogate_type = "rf")
  expect_equal(result@surrogate_type, "rf")
})

test_that("rctbp_sample_size_result stores score_config as list", {
  cfg <- list(scale = "raw", shape = "quadratic")
  result <- make_result(score_config = cfg)
  expect_equal(result@score_config, cfg)
})

test_that("rctbp_sample_size_result stores n_sims correctly", {
  result <- make_result(n_sims = 500)
  expect_equal(result@n_sims, 500)
})

test_that("rctbp_sample_size_result stores n_evals correctly", {
  result <- make_result(n_evals = 15)
  expect_equal(result@n_evals, 15)
})

test_that("rctbp_sample_size_result stores elapsed_time correctly", {
  result <- make_result(elapsed_time = 2.75)
  expect_equal(result@elapsed_time, 2.75)
})

test_that("rctbp_sample_size_result archive has expected columns", {
  result <- make_result()
  expected_cols <- c("n_total", "power", "score", "n_sims")
  expect_true(all(expected_cols %in% names(result@archive)))
})

test_that("rctbp_sample_size_result convergence has expected columns", {
  result <- make_result()
  expected_cols <- c("eval", "n_total", "power", "score", "best_score")
  expect_true(all(expected_cols %in% names(result@convergence)))
})

test_that("rctbp_sample_size_result accepts NULL surrogate_fit", {
  result <- make_result(surrogate_fit = NULL)
  expect_null(result@surrogate_fit)
})

test_that("rctbp_sample_size_result accepts NA n_optimal", {
  result <- make_result(n_optimal = NA_real_)
  expect_true(is.na(result@n_optimal))
})

test_that("rctbp_sample_size_result default n_optimal is NA", {
  result <- rctbp_sample_size_result()
  expect_true(is.na(result@n_optimal))
})

test_that("rctbp_sample_size_result default surrogate_type is gp_power", {
  result <- rctbp_sample_size_result()
  expect_equal(result@surrogate_type, "gp_power")
})

test_that("rctbp_sample_size_result default target_power is 0.80", {
  result <- rctbp_sample_size_result()
  expect_equal(result@target_power, 0.80)
})

test_that("rctbp_sample_size_result default feasible is FALSE", {
  result <- rctbp_sample_size_result()
  expect_false(result@feasible)
})

# =============================================================================
# VALIDATOR: n_optimal must be integer-valued
# =============================================================================

test_that("rctbp_sample_size_result validator rejects non-integer n_optimal", {
  expect_error(
    make_result(n_optimal = 100.5)
  )
})

test_that("rctbp_sample_size_result validator rejects n_optimal = 99.9", {
  expect_error(
    make_result(n_optimal = 99.9)
  )
})

test_that("rctbp_sample_size_result validator accepts integer-valued double (100.0)", {
  # 100.0 rounds to 100 so it is integer-valued
  result <- make_result(n_optimal = 100.0)
  expect_equal(result@n_optimal, 100.0)
})

test_that("rctbp_sample_size_result validator accepts NA n_optimal (skips integer check)", {
  result <- make_result(n_optimal = NA_real_)
  expect_true(is.na(result@n_optimal))
})

# =============================================================================
# VALIDATOR: surrogate_type must be one of gp_power, gp_score, rf
# =============================================================================

test_that("rctbp_sample_size_result validator rejects invalid surrogate_type", {
  expect_error(
    make_result(surrogate_type = "neural")
  )
})

test_that("rctbp_sample_size_result validator rejects empty string surrogate_type", {
  expect_error(
    make_result(surrogate_type = "")
  )
})

test_that("rctbp_sample_size_result validator accepts surrogate_type = 'gp_power'", {
  result <- make_result(surrogate_type = "gp_power")
  expect_equal(result@surrogate_type, "gp_power")
})

test_that("rctbp_sample_size_result validator accepts surrogate_type = 'gp_score'", {
  result <- make_result(surrogate_type = "gp_score")
  expect_equal(result@surrogate_type, "gp_score")
})

test_that("rctbp_sample_size_result validator accepts surrogate_type = 'rf'", {
  result <- make_result(surrogate_type = "rf")
  expect_equal(result@surrogate_type, "rf")
})

# =============================================================================
# VALIDATOR: design must be rctbp_design or NULL
# =============================================================================

test_that("rctbp_sample_size_result validator rejects invalid design type (list)", {
  expect_error(
    make_result(design = list(fake = TRUE))
  )
})

test_that("rctbp_sample_size_result validator rejects invalid design type (data.frame)", {
  expect_error(
    make_result(design = data.frame(x = 1))
  )
})

test_that("rctbp_sample_size_result validator accepts NULL design", {
  result <- make_result(design = NULL)
  expect_null(result@design)
})

test_that("rctbp_sample_size_result validator accepts valid rctbp_design", {
  d <- mock_design()
  result <- make_result(design = d)
  expect_true(inherits(result@design, "rctbayespower::rctbp_design"))
})

# =============================================================================
# S3 METHOD: print()
# =============================================================================

test_that("print() works without error on feasible result", {
  result <- make_result(feasible = TRUE)
  expect_no_error(print(result))
})

test_that("print() works without error on infeasible result", {
  result <- make_result(feasible = FALSE, n_optimal = 200L, power_optimal = 0.72)
  expect_no_error(print(result))
})

test_that("print() returns x invisibly", {
  result <- make_result()
  out <- withVisible(print(result))
  expect_false(out$visible)
  expect_identical(out$value, result)
})

test_that("print() works when elapsed_time is NA", {
  result <- make_result(elapsed_time = NA_real_)
  expect_no_error(print(result))
})

test_that("print() works when n_optimal is NA (infeasible, no best point)", {
  result <- make_result(feasible = FALSE, n_optimal = NA_real_, power_optimal = NA_real_)
  expect_no_error(print(result))
})

# =============================================================================
# S3 METHOD: summary()
# =============================================================================

test_that("summary() works without error", {
  result <- make_result()
  expect_no_error(summary(result))
})

test_that("summary() returns the archive data.frame invisibly", {
  result <- make_result()
  out <- withVisible(summary(result))
  expect_false(out$visible)
  expect_identical(out$value, result@archive)
})

test_that("summary() returns a data.frame", {
  result <- make_result()
  out <- summary(result)
  expect_s3_class(out, "data.frame")
})
