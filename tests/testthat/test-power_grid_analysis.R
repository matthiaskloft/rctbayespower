# Tests for power_analysis(), rctbp_power_analysis class, and related functionality
#
# Uses mock objects from helper-mock-objects.R to avoid brms compilation.
# CLI output captured via capture_cli() from helper-cli.R.

# =============================================================================
# Helper: mock populated power analysis object
# =============================================================================

mock_power_analysis <- function(n_sims = 10, n_conditions = 2) {
  conds <- mock_conditions(n_conditions = n_conditions)
  pa <- power_analysis(conditions = conds, n_sims = n_sims, run = FALSE)
  pa@results_raw <- mock_raw_results(n_sims = n_sims, n_conditions = n_conditions)
  pa@results_conditions <- rctbayespower:::summarize_sims(
    pa@results_raw, n_sims = n_sims
  )
  pa@elapsed_time <- 1.5
  pa
}

eval_silently <- function(expr) {
  withr::with_output_sink(
    textConnection(NULL, "w"),
    withCallingHandlers(expr, message = function(m) invokeRestart("muffleMessage"))
  )
}

# =============================================================================
# Group 1: power_analysis() constructor
# =============================================================================

test_that("power_analysis() creates valid object with run = FALSE", {
  conds <- mock_conditions()
  pa <- power_analysis(conditions = conds, n_sims = 50, run = FALSE)

  expect_s3_class(pa, "rctbayespower::rctbp_power_analysis")
  expect_equal(pa@n_sims, 50)
  expect_equal(pa@n_cores, 1)
  expect_equal(nrow(pa@results_conditions), 0)
  expect_equal(nrow(pa@results_raw), 0)
})

test_that("power_analysis() rejects non-rctbp_conditions objects", {
  expect_cli_abort(power_analysis(conditions = "not_conditions", run = FALSE))
  expect_cli_abort(power_analysis(conditions = data.frame(x = 1), run = FALSE))
  expect_cli_abort(power_analysis(conditions = list(), run = FALSE))
})

test_that("power_analysis() stores .call for reproducibility", {
  conds <- mock_conditions()
  pa <- power_analysis(conditions = conds, n_sims = 25, run = FALSE)

  expect_false(is.null(pa@.call))
  call_str <- deparse(pa@.call)
  expect_true(any(grepl("power_analysis", call_str)))
  expect_true(any(grepl("n_sims", call_str)))
})

# =============================================================================
# Group 2: S7 class validator — rctbp_power_analysis
# =============================================================================

test_that("n_sims validation rejects invalid values", {
  conds <- mock_conditions()
  expect_cli_abort(power_analysis(conditions = conds, n_sims = 0, run = FALSE))
  expect_cli_abort(power_analysis(conditions = conds, n_sims = -5, run = FALSE))
  expect_cli_abort(power_analysis(conditions = conds, n_sims = 1.5, run = FALSE))
  # S7 property type check catches non-numeric before our validator
  expect_error(power_analysis(conditions = conds, n_sims = "ten", run = FALSE))
})

test_that("n_cores validation rejects invalid values", {
  conds <- mock_conditions()
  expect_cli_abort(power_analysis(conditions = conds, n_cores = 0, run = FALSE))
  expect_cli_abort(power_analysis(conditions = conds, n_cores = -1, run = FALSE))
  n_available <- parallel::detectCores()
  expect_cli_abort(
    power_analysis(conditions = conds, n_cores = n_available + 1, run = FALSE)
  )
})

test_that("verbosity validation rejects values outside {0, 1, 2}", {
  conds <- mock_conditions()
  expect_cli_abort(
    power_analysis(conditions = conds, verbosity = 3, run = FALSE)
  )
  expect_cli_abort(
    power_analysis(conditions = conds, verbosity = -1, run = FALSE)
  )
  expect_cli_abort(
    power_analysis(conditions = conds, verbosity = 0.5, run = FALSE)
  )
})

test_that("brms_args validation rejects non-list", {
  conds <- mock_conditions()
  # S7 property type check catches non-list before our validator
  expect_error(
    power_analysis(conditions = conds, brms_args = "not_a_list", run = FALSE)
  )
})

test_that("has_interim returns FALSE when no results", {
  conds <- mock_conditions()
  pa <- power_analysis(conditions = conds, n_sims = 10, run = FALSE)
  expect_false(pa@has_interim)
})

test_that("has_interim returns FALSE for fixed design with results", {
  pa <- mock_power_analysis()
  expect_false(pa@has_interim)
})

# =============================================================================
# Group 3: Design prior parsing
# =============================================================================

test_that("parse_design_prior() returns 'none' for NULL", {
  result <- rctbayespower:::parse_design_prior(NULL, effect_sizes = c(0.2, 0.5))
  expect_equal(result$weight_type, "none")
  expect_null(result$weight_fn)
  expect_null(result$quantile_fn)
})

test_that("parse_design_prior() parses brms normal syntax", {
  result <- suppressMessages(
    rctbayespower:::parse_design_prior(
      "normal(0.5, 0.2)",
      effect_sizes = seq(0, 1, 0.1),
      verbose = FALSE
    )
  )
  expect_equal(result$weight_type, "brms")
  expect_true(is.function(result$weight_fn))

  # Weight function produces finite positive values
  w <- result$weight_fn(0.5)
  expect_true(is.numeric(w))
  expect_true(is.finite(w))
  expect_true(w > 0)
})

test_that("parse_design_prior() handles custom R function", {
  custom_fn <- function(x) stats::dnorm(x, mean = 0.3, sd = 0.1)
  result <- suppressMessages(suppressWarnings(
    rctbayespower:::parse_design_prior(
      custom_fn,
      effect_sizes = seq(0, 1, 0.1),
      verbose = FALSE
    )
  ))
  expect_equal(result$weight_type, "function")
  expect_true(is.function(result$weight_fn))
})

test_that("parse_design_prior() rejects invalid syntax (no parens)", {
  expect_cli_abort(
    rctbayespower:::parse_design_prior(
      "not_a_distribution",
      effect_sizes = c(0.2, 0.5),
      verbose = FALSE
    )
  )
})

test_that("parse_design_prior() rejects function returning non-numeric", {
  bad_fn <- function(x) "not_numeric"
  expect_cli_abort(
    rctbayespower:::parse_design_prior(
      bad_fn,
      effect_sizes = c(0.2, 0.5),
      verbose = FALSE
    )
  )
})

test_that("parse_design_prior() rejects unsupported types", {
  expect_cli_abort(
    rctbayespower:::parse_design_prior(42, effect_sizes = c(0.2, 0.5))
  )
  expect_cli_abort(
    rctbayespower:::parse_design_prior(list(a = 1), effect_sizes = c(0.2, 0.5))
  )
})

test_that("parse_design_prior() warns when effect sizes don't cover prior", {
  # Custom function with known quantiles — coverage check requires quantile_fn
  custom_fn <- function(x) stats::dnorm(x, mean = 0.8, sd = 0.1)
  # Effect sizes c(0.1, 0.2, 0.3) don't reach the 90th percentile
  # Allow multiple warnings (coverage + collapsing to unique values)
  expect_warning(
    rctbayespower:::parse_design_prior(
      custom_fn,
      effect_sizes = c(0.1, 0.2, 0.3),
      verbose = FALSE
    ),
    regexp = "cover design prior",
    class = "rlang_warning"
  )
})

# =============================================================================
# Group 4: find_optimal_condition()
# =============================================================================

test_that("find_optimal_condition() mode 'highest' returns max power", {
  results_summ <- data.frame(
    id_cond = 1:3,
    par_name = "b_arm2",
    pwr_eff = c(0.6, 0.9, 0.75)
  )
  grid <- data.frame(id_cond = 1:3, n_total = c(100, 200, 300))

  result <- rctbayespower:::find_optimal_condition(
    results_summ = results_summ, conditions_grid = grid, target_pwr = NULL
  )

  expect_true(result$found)
  expect_equal(result$mode, "highest")
  expect_equal(result$condition_id, 2)
  expect_equal(result$achieved_pwr, 0.9)
})

test_that("find_optimal_condition() mode 'target' returns smallest n meeting target", {
  results_summ <- data.frame(
    id_cond = 1:3,
    par_name = "b_arm2",
    pwr_eff = c(0.6, 0.85, 0.95)
  )
  grid <- data.frame(id_cond = 1:3, n_total = c(100, 200, 300))

  result <- rctbayespower:::find_optimal_condition(
    results_summ = results_summ, conditions_grid = grid, target_pwr = 0.8
  )

  expect_true(result$found)
  expect_equal(result$mode, "target")
  expect_equal(result$condition_id, 2)
  expect_equal(result$n_total, 200)
})

test_that("find_optimal_condition() returns closest when no condition meets target", {
  results_summ <- data.frame(
    id_cond = 1:2,
    par_name = "b_arm2",
    pwr_eff = c(0.5, 0.7)
  )
  grid <- data.frame(id_cond = 1:2, n_total = c(100, 200))

  result <- rctbayespower:::find_optimal_condition(
    results_summ = results_summ, conditions_grid = grid, target_pwr = 0.9
  )

  expect_false(result$found)
  expect_equal(result$mode, "target")
  expect_false(is.null(result$closest))
  expect_equal(result$closest$achieved_pwr, 0.7)
})

test_that("find_optimal_condition() handles empty inputs gracefully", {
  empty_df <- data.frame(
    id_cond = integer(0), par_name = character(0), pwr_eff = numeric(0)
  )
  empty_grid <- data.frame(id_cond = integer(0), n_total = numeric(0))

  result <- rctbayespower:::find_optimal_condition(
    results_summ = empty_df, conditions_grid = empty_grid, target_pwr = 0.8
  )
  expect_false(result$found)
})

test_that("find_optimal_condition() handles all-NA power column", {
  results_summ <- data.frame(
    id_cond = 1:2,
    par_name = "b_arm2",
    pwr_eff = c(NA_real_, NA_real_)
  )
  grid <- data.frame(id_cond = 1:2, n_total = c(100, 200))

  result <- rctbayespower:::find_optimal_condition(
    results_summ = results_summ, conditions_grid = grid, target_pwr = 0.8
  )
  expect_false(result$found)
})

test_that("find_optimal_condition() aggregates multi-parameter power correctly", {
  # Data where max and mean disagree: max picks cond1 (0.95), mean picks cond2 (0.6)
  results_summ <- data.frame(
    id_cond = c(1, 1, 2, 2),
    par_name = c("b_arm2", "b_arm3", "b_arm2", "b_arm3"),
    pwr_eff = c(0.95, 0.1, 0.6, 0.6)
  )
  grid <- data.frame(id_cond = 1:2, n_total = c(100, 200))

  result <- rctbayespower:::find_optimal_condition(
    results_summ = results_summ, conditions_grid = grid, target_pwr = NULL
  )

  # Uses max per condition: cond1=0.95, cond2=0.6 → picks cond1
  expect_equal(result$condition_id, 1)
  expect_equal(result$achieved_pwr, 0.95)
})

test_that("find_optimal_condition() uses non-default power_col", {
  results_summ <- data.frame(
    id_cond = 1:2,
    par_name = "b_arm2",
    pwr_eff = c(0.9, 0.5),
    pwr_fut = c(0.3, 0.8)
  )
  grid <- data.frame(id_cond = 1:2, n_total = c(100, 200))

  result_eff <- rctbayespower:::find_optimal_condition(
    results_summ = results_summ, conditions_grid = grid,
    target_pwr = NULL, power_col = "pwr_eff"
  )
  result_fut <- rctbayespower:::find_optimal_condition(
    results_summ = results_summ, conditions_grid = grid,
    target_pwr = NULL, power_col = "pwr_fut"
  )

  # pwr_eff highest is cond1, pwr_fut highest is cond2
  expect_equal(result_eff$condition_id, 1)
  expect_equal(result_fut$condition_id, 2)
})

test_that("find_optimal_condition() populates interim stats when provided", {
  results_summ <- data.frame(
    id_cond = 1:2,
    par_name = "b_arm2",
    pwr_eff = c(0.7, 0.9)
  )
  grid <- data.frame(id_cond = 1:2, n_total = c(100, 200))
  interim_overall <- data.frame(
    id_cond = 1:2,
    n_mn = c(80, 160),
    n_mdn = c(75, 155),
    n_mode = c(70, 150),
    prop_at_mode = c(0.4, 0.5),
    prop_stp_early = c(0.3, 0.2),
    prop_stp_eff = c(0.2, 0.15),
    prop_stp_fut = c(0.1, 0.05),
    prop_no_dec = c(0.7, 0.8)
  )

  result <- rctbayespower:::find_optimal_condition(
    results_summ = results_summ, conditions_grid = grid,
    target_pwr = NULL, interim_overall = interim_overall
  )

  expect_false(is.null(result$interim))
  expect_equal(result$interim$n_mn, 160)
  expect_equal(result$interim$prop_stp_early, 0.2)
})

# =============================================================================
# Group 5: print() / summary() output
# =============================================================================

test_that("print() on un-run object mentions 'not yet run'", {
  conds <- mock_conditions()
  pa <- power_analysis(conditions = conds, n_sims = 10, run = FALSE)
  expect_output_contains(print(pa), "not yet run")
})

test_that("print() on populated object produces key sections", {
  pa <- mock_power_analysis()
  expect_output_contains(print(pa), c("Power Analysis", "Efficacy"))
})

test_that("print() returns object invisibly", {
  pa <- mock_power_analysis()
  result <- eval_silently(print(pa))
  expect_s3_class(result, "rctbayespower::rctbp_power_analysis")
})

test_that("summary() on populated object produces output", {
  pa <- mock_power_analysis()
  expect_output_contains(summary(pa), "Power Analysis")
})

test_that("summary() returns object invisibly", {
  pa <- mock_power_analysis()
  result <- eval_silently(summary(pa))
  expect_s3_class(result, "rctbayespower::rctbp_power_analysis")
})

# =============================================================================
# Group 6: get_code() reproducibility
# =============================================================================

test_that("get_code() returns character string containing 'power_analysis'", {
  pa <- mock_power_analysis()
  capture_cli(result <- get_code(pa))
  expect_true(is.character(result))
  expect_true(grepl("power_analysis", result))
})

test_that("get_code() contains stored call arguments with values", {
  # Call power_analysis() directly so match.call() captures the literal value
  conds <- mock_conditions()
  pa <- power_analysis(conditions = conds, n_sims = 42, run = FALSE)
  capture_cli(result <- get_code(pa))
  expect_true(grepl("n_sims", result))
  expect_true(grepl("42", result))
})

test_that("get_code() handles NULL .call with fallback comment", {
  pa <- mock_power_analysis()
  pa@.call <- NULL
  capture_cli(result <- get_code(pa))
  expect_true(grepl("call not recorded", result))
})

# =============================================================================
# Group 7: Sample size / effect size grid awareness
# =============================================================================

test_that("Object with multi-condition grid stores correct grid dimensions", {
  conds <- mock_conditions(n_conditions = 5)
  pa <- power_analysis(conditions = conds, n_sims = 10, run = FALSE)

  expect_equal(nrow(pa@conditions@grid), 5)
  expect_true("n_total" %in% names(pa@conditions@grid))
})

test_that("results_conditions after mock population has one row per condition-parameter", {
  pa <- mock_power_analysis(n_sims = 10, n_conditions = 3)

  # mock_raw_results uses one par_name ("b_arm2") so expect one row per condition
  expect_equal(nrow(pa@results_conditions), 3)
  expect_equal(sort(unique(pa@results_conditions$id_cond)), 1:3)
})

test_that("design computed property accesses conditions design", {
  conds <- mock_conditions()
  pa <- power_analysis(conditions = conds, n_sims = 10, run = FALSE)

  expect_s3_class(pa@design, "rctbayespower::rctbp_design")
  expect_identical(pa@design, pa@conditions@design)
})
