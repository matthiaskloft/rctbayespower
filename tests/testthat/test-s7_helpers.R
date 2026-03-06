# Tests for S7 helper functions (R/S7_helpers.R)

# =============================================================================
# update_S7_with_dots()
# =============================================================================

test_that("update_S7_with_dots updates valid properties", {
  d <- mock_design()
  d <- rctbayespower:::update_S7_with_dots(d, design_name = "updated_name")
  expect_equal(d@design_name, "updated_name")
})

test_that("update_S7_with_dots warns on unknown properties", {
  d <- mock_design()
  expect_warning(
    rctbayespower:::update_S7_with_dots(d, nonexistent_prop = "value"),
    "nonexistent_prop"
  )
})

test_that("update_S7_with_dots re-validates after update", {
  d <- mock_design()
  # Setting target_params to invalid should cause validation error
  expect_error(
    rctbayespower:::update_S7_with_dots(d, target_params = "nonexistent")
  )
})
