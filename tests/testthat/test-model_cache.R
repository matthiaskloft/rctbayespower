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
    download_model = function(model_name, dest_path, model_type) {
      download_calls[[length(download_calls) + 1L]] <<- list(model_name, dest_path, model_type)
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
