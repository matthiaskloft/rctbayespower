# Tests for get_code() (R/get_code.R)

# =============================================================================
# HELPER: Create objects with stored calls (using real constructors)
# =============================================================================

# build_design() requires a real brmsfit + sim_fn, so use mock objects
# with .call manually set for most tests. Integration tests use build_design().

make_design_with_call <- function() {
  design <- mock_design()
  design@.call <- quote(build_design(
    predefined_model = "ancova_cont_2arms",
    target_params = "b_arm2"
  ))
  design
}

make_conditions_with_call <- function(design = NULL) {
  if (is.null(design)) design <- make_design_with_call()
  cond <- mock_conditions(design = design)
  cond@.call <- quote(build_conditions(
    design = design,
    crossed = list(n_total = c(100, 200)),
    constant = list(
      p_alloc = list(c(0.5, 0.5)),
      intercept = 0, b_covariate = 0.3, sigma = 1,
      thr_dec_eff = 0.975, thr_dec_fut = 0.5,
      thr_fx_eff = 0.2, thr_fx_fut = 0
    )
  ))
  cond
}

make_power_analysis_with_call <- function(conditions = NULL) {
  if (is.null(conditions)) conditions <- make_conditions_with_call()
  pa <- rctbp_power_analysis(
    conditions = conditions,
    n_sims = 100,
    n_cores = 1,
    brms_args = list(),
    .call = quote(power_analysis(
      conditions = conditions, n_sims = 100, n_cores = 4
    ))
  )
  pa
}

# =============================================================================
# BASIC FUNCTIONALITY
# =============================================================================

test_that("get_code() works for rctbp_design", {
  design <- make_design_with_call()
  code <- capture.output(get_code(design))
  expect_true(any(grepl("design <-", code)))
  expect_true(any(grepl("build_design", code)))
  expect_true(any(grepl("ancova_cont_2arms", code)))
})

test_that("get_code() works for rctbp_conditions", {
  cond <- make_conditions_with_call()
  code <- capture.output(get_code(cond))

  # Should contain both design and conditions steps
  expect_true(any(grepl("design <-", code)))
  expect_true(any(grepl("conditions <-", code)))
  expect_true(any(grepl("build_design", code)))
  expect_true(any(grepl("build_conditions", code)))
})

test_that("get_code() works for rctbp_power_analysis", {
  pa <- make_power_analysis_with_call()
  code <- capture.output(get_code(pa))

  # Should contain all three steps
  expect_true(any(grepl("design <-", code)))
  expect_true(any(grepl("conditions <-", code)))
  expect_true(any(grepl("result <-", code)))
  expect_true(any(grepl("build_design", code)))
  expect_true(any(grepl("build_conditions", code)))
  expect_true(any(grepl("power_analysis", code)))
})

# =============================================================================
# RETURN VALUE
# =============================================================================

test_that("get_code() returns character invisibly", {
  design <- make_design_with_call()
  result <- capture.output(code <- get_code(design))
  expect_type(code, "character")
  expect_true(nchar(code) > 0)
})

# =============================================================================
# ARGUMENT PRESERVATION
# =============================================================================

test_that("get_code() preserves user-supplied arguments", {
  pa <- make_power_analysis_with_call()
  code <- capture.output(get_code(pa))
  code_str <- paste(code, collapse = "\n")

  # Design args
  expect_true(grepl("predefined_model", code_str))
  expect_true(grepl("target_params", code_str))

  # Conditions args
  expect_true(grepl("crossed", code_str))
  expect_true(grepl("constant", code_str))

  # Power analysis args
  expect_true(grepl("n_sims = 100", code_str))
  expect_true(grepl("n_cores = 4", code_str))
})

test_that("get_code() replaces object references with variable names", {
  pa <- make_power_analysis_with_call()
  code <- capture.output(get_code(pa))
  code_str <- paste(code, collapse = "\n")

  # conditions step should reference `design` as variable, not the object
  expect_true(grepl("design = design", code_str))
  # power_analysis step should reference `conditions` as variable
  expect_true(grepl("conditions = conditions", code_str))
})

# =============================================================================
# NULL .call FALLBACK
# =============================================================================

test_that("get_code() handles NULL .call gracefully", {
  design <- mock_design()  # No .call set
  code <- capture.output(get_code(design))
  expect_true(any(grepl("call not recorded", code)))
})

test_that("get_code() handles NULL .call in chain", {
  # Conditions with no call, but design has a call
  design <- make_design_with_call()
  cond <- mock_conditions(design = design)
  # cond@.call is NULL

  code <- capture.output(get_code(cond))
  code_str <- paste(code, collapse = "\n")

  # Design should still show
  expect_true(grepl("build_design", code_str))
  # Conditions should show fallback
  expect_true(grepl("call not recorded", code_str))
})

# =============================================================================
# INTERNAL HELPERS
# =============================================================================

test_that("format_call() handles NULL call", {
  result <- format_call(NULL, "design", "build_design")
  expect_true(grepl("call not recorded", result))
})

test_that("format_call() deparses simple call", {
  cl <- quote(build_design(predefined_model = "test", target_params = "b_arm2"))
  result <- format_call(cl, "design", "build_design")
  expect_true(any(grepl("design <-", result)))
  expect_true(any(grepl("build_design", result)))
})

test_that("replace_arg_with_symbol() replaces argument", {
  cl <- quote(build_conditions(design = some_object, crossed = list()))
  result <- replace_arg_with_symbol(cl, "design", "design")
  expect_equal(result[["design"]], as.symbol("design"))
})

test_that("replace_arg_with_symbol() handles NULL call", {
  expect_null(replace_arg_with_symbol(NULL, "design", "design"))
})

test_that("replace_arg_with_symbol() handles missing argument", {
  cl <- quote(build_conditions(crossed = list()))
  result <- replace_arg_with_symbol(cl, "design", "design")
  # Should return unchanged

  expect_equal(result, cl)
})
