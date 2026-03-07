# Tests for output mode system (R/output_system.R)

# =============================================================================
# get_output_mode()
# =============================================================================

test_that("get_output_mode returns default 'cli'", {
  withr::with_options(list(rctbayespower.output_mode = NULL), {
    expect_equal(get_output_mode(), "cli")
  })
})

test_that("get_output_mode returns current mode", {
  withr::with_options(list(rctbayespower.output_mode = "markdown"), {
    expect_equal(get_output_mode(), "markdown")
  })
})

test_that("get_output_mode warns on invalid mode and returns default", {
  withr::with_options(list(rctbayespower.output_mode = "html"), {
    expect_warning(result <- get_output_mode(), class = "rlang_warning")
    expect_equal(result, "cli")
  })
})

# =============================================================================
# set_output_mode()
# =============================================================================

test_that("set_output_mode updates and returns old value", {
  withr::with_options(list(rctbayespower.output_mode = "cli"), {
    old <- set_output_mode("markdown")
    expect_equal(old, "cli")
    expect_equal(get_output_mode(), "markdown")
  })
})

test_that("set_output_mode rejects invalid modes", {
  expect_error(set_output_mode("html"))
  expect_error(set_output_mode("latex"))
})

# =============================================================================
# with_output_mode()
# =============================================================================

test_that("with_output_mode temporarily changes mode", {
  withr::with_options(list(rctbayespower.output_mode = "cli"), {
    result <- with_output_mode("markdown", {
      expect_equal(get_output_mode(), "markdown")
      42
    })
    expect_equal(get_output_mode(), "cli")
    expect_equal(result, 42)
  })
})

test_that("with_output_mode restores on error", {
  withr::with_options(list(rctbayespower.output_mode = "cli"), {
    try(with_output_mode("markdown", stop("test")), silent = TRUE)
    expect_equal(get_output_mode(), "cli")
  })
})

# =============================================================================
# Markdown helpers
# =============================================================================

test_that("markdown helpers produce correct format", {
  expect_equal(rctbayespower:::md_h1("Title"), "# Title\n")
  expect_equal(rctbayespower:::md_h2("Section"), "## Section\n")
  expect_equal(rctbayespower:::md_h3("Sub"), "### Sub\n")
  expect_equal(rctbayespower:::md_rule(), "---\n")
  expect_equal(rctbayespower:::md_code("x"), "`x`")
})

# =============================================================================
# cli_or_md()
# =============================================================================

test_that("cli_or_md routes based on mode", {
  withr::with_options(list(rctbayespower.output_mode = "markdown"), {
    output <- capture.output(rctbayespower:::cli_or_md(
      function() cat("cli output"),
      "markdown output"
    ))
    expect_true(any(grepl("markdown output", output)))
  })
})
