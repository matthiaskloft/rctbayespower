# Tests for setup_python.R
#
# Only tests validation logic that doesn't require Python or reticulate.

test_that("setup_bf_python rejects unsupported Python version", {
  skip_if_not_installed("reticulate")
  expect_cli_abort(
    setup_bf_python(python_version = "2.7"),
    regexp = "not supported"
  )
})

test_that("setup_bf_python rejects invalid CUDA version", {
  skip_if_not_installed("reticulate")
  expect_error(setup_bf_python(cuda_version = "invalid"))
})

test_that("detect_cuda_version returns 'cpu' without nvidia-smi", {
  # Mock system2 to simulate nvidia-smi not found
  local_mocked_bindings(
    system2 = function(...) stop("not found"),
    .package = "base"
  )
  result <- suppressMessages(rctbayespower:::detect_cuda_version())
  expect_equal(result, "cpu")
})
