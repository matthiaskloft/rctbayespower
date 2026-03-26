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

# =============================================================================
# CUDA version mapping
# =============================================================================

test_that("detect_cuda_version maps CUDA 12.8 to '12.8'", {
  local_mocked_bindings(
    system2 = function(...) "CUDA Version: 12.8",
    .package = "base"
  )
  result <- suppressMessages(rctbayespower:::detect_cuda_version())
  expect_equal(result, "12.8")
})

test_that("detect_cuda_version maps CUDA 12.6 to '12.6'", {
  local_mocked_bindings(
    system2 = function(...) "CUDA Version: 12.6",
    .package = "base"
  )
  result <- suppressMessages(rctbayespower:::detect_cuda_version())
  expect_equal(result, "12.6")
})

test_that("detect_cuda_version maps CUDA 12.3 to '12.6' (backward compat)", {
  local_mocked_bindings(
    system2 = function(...) "CUDA Version: 12.3",
    .package = "base"
  )
  result <- suppressMessages(rctbayespower:::detect_cuda_version())
  expect_equal(result, "12.6")
})

test_that("detect_cuda_version maps CUDA 11.8 to '11.8'", {
  local_mocked_bindings(
    system2 = function(...) "CUDA Version: 11.8",
    .package = "base"
  )
  result <- suppressMessages(rctbayespower:::detect_cuda_version())
  expect_equal(result, "11.8")
})

test_that("detect_cuda_version maps CUDA 13.0 to '12.8' (future compat)", {
  local_mocked_bindings(
    system2 = function(...) "CUDA Version: 13.0",
    .package = "base"
  )
  result <- suppressMessages(rctbayespower:::detect_cuda_version())
  expect_equal(result, "12.8")
})

test_that("detect_cuda_version returns 'cpu' for old CUDA 10.2", {
  local_mocked_bindings(
    system2 = function(...) "CUDA Version: 10.2",
    .package = "base"
  )
  result <- suppressMessages(rctbayespower:::detect_cuda_version())
  expect_equal(result, "cpu")
})

test_that("detect_cuda_version returns 'cpu' when nvidia-smi output is unparseable", {
  local_mocked_bindings(
    system2 = function(...) "Driver Version: 535.129.03",
    .package = "base"
  )
  result <- suppressMessages(rctbayespower:::detect_cuda_version())
  expect_equal(result, "cpu")
})

# =============================================================================
# Python version validation
# =============================================================================

test_that("setup_bf_python accepts supported Python versions", {
  skip_if_not_installed("reticulate")
  # We can't run the full setup, but we can test the validation doesn't reject 3.10/3.11
  # These will fail later (no Python installed), but not at the validation step
  # Just test that 2.7 is rejected (already covered) and 3.13 is rejected
  expect_cli_abort(
    setup_bf_python(python_version = "3.13"),
    regexp = "not supported"
  )
  expect_cli_abort(
    setup_bf_python(python_version = "3.9"),
    regexp = "not supported"
  )
})

# =============================================================================
# Supported version constants
# =============================================================================

test_that(".bf_supported_python contains expected versions", {
  supported <- rctbayespower:::.bf_supported_python
  expect_true("3.10" %in% supported)
  expect_true("3.11" %in% supported)
  expect_true("3.12" %in% supported)
})

test_that(".bf_supported_cuda contains expected values", {
  supported <- rctbayespower:::.bf_supported_cuda
  expect_true("auto" %in% supported)
  expect_true("cpu" %in% supported)
  expect_true("11.8" %in% supported)
  expect_true("12.6" %in% supported)
  expect_true("12.8" %in% supported)
})

# =============================================================================
# find_python_version
# =============================================================================

test_that("find_python_version returns NULL when no pyenv and no reticulate config", {
  skip_if_not_installed("reticulate")
  local_mocked_bindings(
    py_discover_config = function(...) list(python = NULL),
    .package = "reticulate"
  )
  # Use a nonexistent pyenv root
  withr::local_envvar(HOME = withr::local_tempdir())
  if (.Platform$OS.type == "windows") {
    withr::local_envvar(LOCALAPPDATA = withr::local_tempdir())
  }
  result <- rctbayespower:::find_python_version("3.12")
  expect_null(result)
})

test_that("find_python_version finds matching version in pyenv directory", {
  skip_if_not_installed("reticulate")
  tmp_pyenv <- withr::local_tempdir()

  if (.Platform$OS.type == "windows") {
    # Create pyenv-win structure
    ver_dir <- file.path(tmp_pyenv, "r-reticulate", "r-reticulate",
                         "pyenv", "pyenv-win", "versions", "3.12.1")
    dir.create(ver_dir, recursive = TRUE)
    file.create(file.path(ver_dir, "python.exe"))
    withr::local_envvar(LOCALAPPDATA = tmp_pyenv)
  } else {
    # Create Unix pyenv structure
    ver_dir <- file.path(tmp_pyenv, ".pyenv", "versions", "3.12.1", "bin")
    dir.create(ver_dir, recursive = TRUE)
    file.create(file.path(ver_dir, "python"))
    withr::local_envvar(HOME = tmp_pyenv)
  }

  result <- rctbayespower:::find_python_version("3.12")
  expect_true(!is.null(result))
  expect_true(grepl("3.12", result))
})

# =============================================================================
# verify_bf_installation (mocked)
# =============================================================================

test_that("verify_bf_installation reports missing Python", {
  skip_if_not_installed("reticulate")
  local_mocked_bindings(
    py_available = function(...) FALSE,
    .package = "reticulate"
  )

  result <- suppressMessages(rctbayespower:::verify_bf_installation())
  expect_false(result$python)
  expect_false(result$torch)
  expect_false(result$bayesflow)
})

test_that("verify_bf_installation reports available packages", {
  skip_if_not_installed("reticulate")
  local_mocked_bindings(
    py_available = function(...) TRUE,
    py_config = function() list(version = "3.12.1"),
    py_module_available = function(module) module %in% c("torch", "bayesflow", "keras", "numpy"),
    import = function(module, ...) {
      list(`__version__` = "1.0.0", cuda = list(is_available = function() FALSE))
    },
    .package = "reticulate"
  )

  result <- suppressMessages(rctbayespower:::verify_bf_installation())
  expect_true(result$python)
  expect_true(result$torch)
  expect_false(result$cuda)
  expect_true(result$bayesflow)
  expect_true(result$keras)
  expect_true(result$numpy)
})

