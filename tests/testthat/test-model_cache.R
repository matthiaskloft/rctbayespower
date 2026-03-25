# Tests for model_cache.R
#
# Uses withr::local_tempdir() and local_mocked_bindings() to isolate
# cache operations from the real filesystem.

test_that("get_model_cache_dir creates directory and returns correct path", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  result <- rctbayespower:::get_model_cache_dir()
  expect_equal(result, tmp_dir)
  expect_true(dir.exists(result))
})

test_that("get_model_cache_dir with subdir creates subdirectory", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  result <- rctbayespower:::get_model_cache_dir("brms")
  expect_equal(result, file.path(tmp_dir, "brms"))
  expect_true(dir.exists(result))
})

test_that("list_models shows not-cached when cache is empty", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  result <- list_models("brms")
  expect_s3_class(result, "data.frame")
  expect_true(all(result$backend == "brms"))
  expect_true(all(!result$cached))
})

test_that("list_models shows cached after planting file", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  brms_dir <- file.path(tmp_dir, "brms")
  dir.create(brms_dir, recursive = TRUE)
  saveRDS(list(), file.path(brms_dir, "ancova_cont_2arms.rds"))

  result <- list_models("brms")
  cached_row <- result[result$model == "ancova_cont_2arms", ]
  expect_true(cached_row$cached)

  uncached_row <- result[result$model == "ancova_cont_3arms", ]
  expect_false(uncached_row$cached)
})

test_that("clear_model_cache removes planted files", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  brms_dir <- file.path(tmp_dir, "brms")
  dir.create(brms_dir, recursive = TRUE)
  saveRDS(list(), file.path(brms_dir, "ancova_cont_2arms.rds"))
  saveRDS(list(), file.path(brms_dir, "ancova_cont_3arms.rds"))

  cleared <- suppressMessages(clear_model_cache("brms"))
  expect_equal(cleared, 2)
  expect_equal(length(list.files(brms_dir, pattern = "\\.rds$")), 0)
})

test_that("get_cache_size returns size > 0 after planting files", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  brms_dir <- file.path(tmp_dir, "brms")
  dir.create(brms_dir, recursive = TRUE)
  saveRDS(list(a = 1:100), file.path(brms_dir, "ancova_cont_2arms.rds"))

  sizes <- get_cache_size("brms")
  expect_true(sizes["brms"] > 0)
})

test_that("get_cache_size returns 0 for empty cache", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  sizes <- get_cache_size("brms")
  expect_equal(unname(sizes["brms"]), 0)
})

test_that("load_brms_model uses cache hit without downloading", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  # Plant a cached model
  brms_dir <- file.path(tmp_dir, "brms")
  dir.create(brms_dir, recursive = TRUE)
  mock_fit <- mock_brmsfit()
  saveRDS(mock_fit, file.path(brms_dir, "ancova_cont_2arms.rds"))

  # Track download calls
  download_calls <- list()
  local_mocked_bindings(
    download_model = function(predefined_model, dest_path, model_type) {
      download_calls[[length(download_calls) + 1L]] <<- list(predefined_model, dest_path, model_type)
    },
    .package = "rctbayespower"
  )

  result <- suppressMessages(load_brms_model("ancova_cont_2arms"))
  expect_true(inherits(result, "brmsfit"))
  expect_equal(length(download_calls), 0)
})

test_that("load_brms_model rejects unknown model name", {
  expect_cli_abort(load_brms_model("nonexistent_model"))
})

test_that("download_model constructs correct GitHub URL", {
  download_args <- NULL
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      download_args <<- list(url = url, destfile = destfile)
    },
    .package = "utils"
  )

  suppressMessages(
    rctbayespower:::download_model("ancova_cont_2arms", "/tmp/test.rds", "brms")
  )

  expect_true(grepl("brms-models-v0.0.0.9000", download_args$url))
  expect_true(grepl("ancova_cont_2arms.rds", download_args$url))
  expect_true(grepl("github.com/matthiaskloft/rctbayespower", download_args$url))
})

# =============================================================================
# BayesFlow model loading
# =============================================================================

test_that("load_bf_model rejects unknown model name", {
  expect_cli_abort(load_bf_model("nonexistent_model"), regexp = "Unknown BayesFlow")
})

test_that("load_bf_model rejects models not in BF list", {
  # ancova_bin_2arms is brms-only, not available for BF
  expect_cli_abort(load_bf_model("ancova_bin_2arms"), regexp = "Unknown BayesFlow")
})

# =============================================================================
# Download model for BF backend
# =============================================================================

test_that("download_model constructs correct BF URL", {
  download_args <- NULL
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      download_args <<- list(url = url, destfile = destfile)
    },
    .package = "utils"
  )

  suppressMessages(
    rctbayespower:::download_model("ancova_cont_2arms", "/tmp/test.keras", "bf")
  )

  expect_true(grepl("bf-models-v0.0.0.9000", download_args$url))
  expect_true(grepl("ancova_cont_2arms.keras", download_args$url))
})

test_that("download_model aborts on download failure", {
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      stop("Connection timed out")
    },
    .package = "utils"
  )

  expect_cli_abort(
    rctbayespower:::download_model("ancova_cont_2arms", "/tmp/test.rds", "brms"),
    regexp = "Failed to download"
  )
})

# =============================================================================
# Force download
# =============================================================================

test_that("load_brms_model force_download triggers download even when cached", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  brms_dir <- file.path(tmp_dir, "brms")
  dir.create(brms_dir, recursive = TRUE)
  mock_fit <- mock_brmsfit()
  saveRDS(mock_fit, file.path(brms_dir, "ancova_cont_2arms.rds"))

  download_called <- FALSE
  local_mocked_bindings(
    download_model = function(predefined_model, dest_path, model_type) {
      download_called <<- TRUE
      # Re-save the mock so readRDS succeeds
      saveRDS(mock_fit, dest_path)
    },
    .package = "rctbayespower"
  )

  result <- suppressMessages(
    load_brms_model("ancova_cont_2arms", force_download = TRUE)
  )
  expect_true(download_called)
  expect_true(inherits(result, "brmsfit"))
})

# =============================================================================
# Quiet parameter
# =============================================================================

test_that("load_brms_model quiet = TRUE suppresses messages", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  brms_dir <- file.path(tmp_dir, "brms")
  dir.create(brms_dir, recursive = TRUE)
  saveRDS(mock_brmsfit(), file.path(brms_dir, "ancova_cont_2arms.rds"))

  # quiet = TRUE should not emit messages
  expect_no_message(load_brms_model("ancova_cont_2arms", quiet = TRUE))
})

# =============================================================================
# list_models variants
# =============================================================================

test_that("list_models('all') returns both brms and bf models", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  result <- list_models("all")
  expect_s3_class(result, "data.frame")
  expect_true("brms" %in% result$backend)
  expect_true("bf" %in% result$backend)
})

test_that("list_models('bf') only returns BayesFlow models", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  result <- list_models("bf")
  expect_true(all(result$backend == "bf"))
  expect_true("ancova_cont_2arms" %in% result$model)
  expect_true("ancova_cont_3arms" %in% result$model)
})

test_that("list_models('bf') detects cached .keras files", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  bf_dir <- file.path(tmp_dir, "bf")
  dir.create(bf_dir, recursive = TRUE)
  writeLines("fake", file.path(bf_dir, "ancova_cont_2arms.keras"))

  result <- list_models("bf")
  cached_row <- result[result$model == "ancova_cont_2arms", ]
  expect_true(cached_row$cached)
})

# =============================================================================
# clear_model_cache variants
# =============================================================================

test_that("clear_model_cache('all') clears both backends", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  brms_dir <- file.path(tmp_dir, "brms")
  bf_dir <- file.path(tmp_dir, "bf")
  dir.create(brms_dir, recursive = TRUE)
  dir.create(bf_dir, recursive = TRUE)
  saveRDS(list(), file.path(brms_dir, "ancova_cont_2arms.rds"))
  writeLines("fake", file.path(bf_dir, "ancova_cont_2arms.keras"))

  cleared <- suppressMessages(clear_model_cache("all"))
  expect_equal(cleared, 2)
})

test_that("clear_model_cache('bf') only clears BF files", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  brms_dir <- file.path(tmp_dir, "brms")
  bf_dir <- file.path(tmp_dir, "bf")
  dir.create(brms_dir, recursive = TRUE)
  dir.create(bf_dir, recursive = TRUE)
  saveRDS(list(), file.path(brms_dir, "ancova_cont_2arms.rds"))
  writeLines("fake", file.path(bf_dir, "ancova_cont_2arms.keras"))

  cleared <- suppressMessages(clear_model_cache("bf"))
  expect_equal(cleared, 1)
  # brms file should still exist
  expect_true(file.exists(file.path(brms_dir, "ancova_cont_2arms.rds")))
})

test_that("clear_model_cache on empty cache returns 0", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  cleared <- suppressMessages(clear_model_cache("brms"))
  expect_equal(cleared, 0)
})

# =============================================================================
# get_cache_size variants
# =============================================================================

test_that("get_cache_size('all') returns sizes for both backends", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  brms_dir <- file.path(tmp_dir, "brms")
  dir.create(brms_dir, recursive = TRUE)
  saveRDS(list(a = 1:100), file.path(brms_dir, "ancova_cont_2arms.rds"))

  sizes <- get_cache_size("all")
  expect_named(sizes, c("brms", "bf"))
  expect_true(sizes["brms"] > 0)
  expect_equal(unname(sizes["bf"]), 0)
})

test_that("get_cache_size('bf') returns BF size only", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")

  bf_dir <- file.path(tmp_dir, "bf")
  dir.create(bf_dir, recursive = TRUE)
  writeLines(paste(rep("x", 1000), collapse = ""),
             file.path(bf_dir, "ancova_cont_2arms.keras"))

  sizes <- get_cache_size("bf")
  expect_true(sizes["bf"] > 0)
})
