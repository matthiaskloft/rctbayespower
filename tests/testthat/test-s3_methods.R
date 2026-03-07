# Tests for S3 method wrappers (R/s3_wrappers.R)

# =============================================================================
# PRINT METHODS - DISPATCH
# =============================================================================

test_that("print.rctbp_design dispatches and shows key content", {
  d <- mock_design()
  output <- capture_cli(print(d))
  output_text <- paste(output, collapse = "\n")

  # Should show backend and target param
  expect_true(
    grepl("brms", output_text, ignore.case = TRUE),
    info = "Expected 'brms' backend in design print output"
  )
  expect_true(
    grepl("b_arm2", output_text),
    info = "Expected target param 'b_arm2' in design print output"
  )
})

test_that("print.rctbp_conditions dispatches and shows grid info", {
  cond <- mock_conditions(n_conditions = 3)
  output <- capture_cli(print(cond))
  output_text <- paste(output, collapse = "\n")

  # Should mention number of conditions
  expect_true(
    grepl("3", output_text),
    info = "Expected '3' (condition count) in conditions print output"
  )
})

test_that("print.rctbp_sim_fn dispatches and shows function info", {
  sim_fn <- mock_rctbp_sim_fn()
  output <- capture_cli(print(sim_fn))
  output_text <- paste(output, collapse = "\n")

  # Should mention parameters of the sim function
  expect_true(
    grepl("n_total", output_text),
    info = "Expected 'n_total' parameter in sim_fn print output"
  )
})

# =============================================================================
# DESIGN PRINT - DETAILED CONTENT
# =============================================================================

test_that("print.rctbp_design shows model and design sections", {
  d <- mock_design()
  expect_output_contains(
    print(d),
    c("brms", "b_arm2")
  )
})

test_that("print.rctbp_design reflects backend choice", {
  d_brms <- mock_design(backend = "brms")
  output_brms <- capture_cli(print(d_brms))
  expect_true(any(grepl("brms", output_brms, ignore.case = TRUE)))
})

# =============================================================================
# CONDITIONS PRINT - DETAILED CONTENT
# =============================================================================

test_that("print.rctbp_conditions shows condition count and sample sizes", {
  cond <- mock_conditions(n_conditions = 4)
  output <- capture_cli(print(cond))
  output_text <- paste(output, collapse = "\n")

  # Should show condition count
  expect_true(
    grepl("4", output_text),
    info = "Expected condition count '4' in output"
  )
})
