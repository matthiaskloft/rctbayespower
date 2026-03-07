# Tests for verbosity control system (R/verbosity.R)

# =============================================================================
# get_verbosity()
# =============================================================================

test_that("get_verbosity returns default level 1", {
  withr::with_options(list(rctbayespower.verbosity = NULL), {
    expect_equal(get_verbosity(), 1)
  })
})

test_that("get_verbosity returns current level", {
  withr::with_options(list(rctbayespower.verbosity = 0), {
    expect_equal(get_verbosity(), 0)
  })
  withr::with_options(list(rctbayespower.verbosity = 2), {
    expect_equal(get_verbosity(), 2)
  })
})

test_that("get_verbosity warns on invalid level and returns default", {
  withr::with_options(list(rctbayespower.verbosity = 99), {
    expect_warning(result <- get_verbosity(), class = "rlang_warning")
    expect_equal(result, 1)
  })
})

# =============================================================================
# set_verbosity()
# =============================================================================

test_that("set_verbosity updates and returns old value", {
  withr::with_options(list(rctbayespower.verbosity = 1), {
    old <- set_verbosity(0)
    expect_equal(old, 1)
    expect_equal(get_verbosity(), 0)
  })
})

test_that("set_verbosity rejects invalid levels", {
  expect_cli_abort(set_verbosity(3))
  expect_cli_abort(set_verbosity(-1))
  expect_cli_abort(set_verbosity(1.5))
})

# =============================================================================
# with_verbosity()
# =============================================================================

test_that("with_verbosity temporarily changes level", {
  withr::with_options(list(rctbayespower.verbosity = 1), {
    result <- with_verbosity(0, {
      expect_equal(get_verbosity(), 0)
      "return_value"
    })
    # Level restored after block
    expect_equal(get_verbosity(), 1)
    # Return value propagated
    expect_equal(result, "return_value")
  })
})

test_that("with_verbosity restores on error", {
  withr::with_options(list(rctbayespower.verbosity = 1), {
    try(with_verbosity(2, stop("test error")), silent = TRUE)
    expect_equal(get_verbosity(), 1)
  })
})

test_that("with_verbosity rejects invalid levels", {
  expect_cli_abort(with_verbosity(5, NULL))
})

# =============================================================================
# should_show()
# =============================================================================

test_that("should_show respects current verbosity level", {
  withr::with_options(list(rctbayespower.verbosity = 0), {
    expect_true(should_show(0))
    expect_false(should_show(1))
    expect_false(should_show(2))
  })
  withr::with_options(list(rctbayespower.verbosity = 1), {
    expect_true(should_show(0))
    expect_true(should_show(1))
    expect_false(should_show(2))
  })
  withr::with_options(list(rctbayespower.verbosity = 2), {
    expect_true(should_show(0))
    expect_true(should_show(1))
    expect_true(should_show(2))
  })
})

# =============================================================================
# verbosity_message()
# =============================================================================

test_that("verbosity_message respects level", {
  withr::with_options(list(rctbayespower.verbosity = 0), {
    # Level 1 message should be suppressed in quiet mode
    expect_silent(rctbayespower:::verbosity_message("test", level = 1))
  })
})
