# =============================================================================
# MODEL CACHE SYSTEM
# =============================================================================
# Unified model caching for both brms and BayesFlow backends.
# Models are downloaded from GitHub releases on first use and cached locally.
#
# Directory structure:
#   ~/.cache/rctbayespower/           # Linux
#   ~/Library/Caches/rctbayespower/   # macOS
#   %LOCALAPPDATA%/rctbayespower/     # Windows
#       ├── brms/
#       │   ├── ancova_cont_2arms.rds
#       │   └── ancova_cont_3arms.rds
#       └── bf/
#           ├── ancova_cont_2arms.keras
#           └── ancova_cont_3arms.keras
#
# GitHub Releases:
#   - brms-models-v0.0.0.9000: Pre-compiled brmsfit objects
#   - bf-models-v0.0.0.9000: Pre-trained BayesFlow approximators

# =============================================================================
# CACHE DIRECTORY MANAGEMENT
# =============================================================================

#' Get Model Cache Directory
#'
#' Returns cross-platform cache directory for downloaded models.
#' Creates directory if it doesn't exist.
#'
#' @param subdir Subdirectory within cache ("brms" or "bf")
#'
#' @return Path to cache directory
#' @keywords internal
get_model_cache_dir <- function(subdir = NULL) {
  if (!requireNamespace("rappdirs", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package 'rappdirs' is required for model caching",
      "i" = "Install it with: install.packages('rappdirs')"
    ))
  }

  cache_dir <- rappdirs::user_cache_dir("rctbayespower")
  if (!is.null(subdir)) {
    cache_dir <- file.path(cache_dir, subdir)
  }
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  cache_dir
}


# =============================================================================
# MODEL DOWNLOAD
# =============================================================================

#' Download Model from GitHub Releases
#'
#' Downloads a model file from the package's GitHub releases.
#'
#' @param model_name Name of the model (e.g., "ancova_cont_2arms")
#' @param dest_path Full path where the model should be saved
#' @param model_type Type of model: "brms" or "bf"
#'
#' @keywords internal
download_model <- function(model_name, dest_path, model_type = c("brms", "bf")) {
  model_type <- match.arg(model_type)

  # URL pattern: GitHub releases asset
  base_url <- "https://github.com/matthiaskloft/rctbayespower/releases/download"
  version_tag <- paste0(model_type, "-models-v0.0.0.9000")

  ext <- if (model_type == "brms") ".rds" else ".keras"
  url <- paste0(base_url, "/", version_tag, "/", model_name, ext)

  tryCatch({
    utils::download.file(url, dest_path, mode = "wb", quiet = TRUE)
    backend_name <- if (model_type == "bf") "BayesFlow" else "brms"
    cli::cli_alert_success("{backend_name} model downloaded to cache")
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download {model_type} model",
      "x" = "URL: {url}",
      "i" = "Error: {e$message}",
      "i" = "Check internet connection or try again later"
    ))
  })
}


# =============================================================================
# BRMS MODEL LOADING
# =============================================================================

#' Load Pre-compiled brms Model
#'
#' Downloads compiled brms model on first use and caches locally.
#' The brms model is a template that can be updated with new data.
#'
#' @param model_name Name of predefined model (e.g., "ancova_cont_2arms")
#' @param force_download Re-download even if cached (default FALSE)
#' @param quiet Suppress download messages (default FALSE)
#'
#' @return Compiled brmsfit object ready for [stats::update()]
#' @export
#'
#' @examples
#' \dontrun{
#' # Load predefined brms model
#' model <- load_brms_model("ancova_cont_2arms")
#'
#' # Force re-download
#' model <- load_brms_model("ancova_cont_2arms", force_download = TRUE)
#' }
load_brms_model <- function(model_name, force_download = FALSE, quiet = FALSE) {

  available_models <- c("ancova_cont_2arms", "ancova_cont_3arms")
  if (!model_name %in% available_models) {
    cli::cli_abort(c(
      "Unknown brms model: {.val {model_name}}",
      "i" = "Available models: {.val {available_models}}"
    ))
  }

  cache_dir <- get_model_cache_dir("brms")
  model_file <- file.path(cache_dir, paste0(model_name, ".rds"))

  if (!file.exists(model_file) || force_download) {
    if (!quiet) {
      cli::cli_alert_info("Downloading brms model: {.val {model_name}}")
    }
    download_model(model_name, model_file, "brms")
  } else {
    if (!quiet) {
      cli::cli_alert_info("Loading cached brms model: {.val {model_name}}")
    }
  }

  readRDS(model_file)
}


# =============================================================================
# BAYESFLOW MODEL LOADING
# =============================================================================

#' Load Pre-trained BayesFlow Model
#'
#' Downloads BayesFlow/Keras model on first use and caches locally.
#' Model is loaded via Python/reticulate for full BayesFlow compatibility.
#'
#' @param model_name Name of predefined model (e.g., "ancova_cont_2arms")
#' @param force_download Re-download even if cached (default FALSE)
#' @param quiet Suppress download messages (default FALSE)
#'
#' @return BayesFlow approximator object (Python Keras model via reticulate)
#' @export
#'
#' @examples
#' \dontrun{
#' # Load predefined BayesFlow model
#' bf_model <- load_bf_model("ancova_cont_2arms")
#'
#' # Force re-download
#' bf_model <- load_bf_model("ancova_cont_2arms", force_download = TRUE)
#' }
load_bf_model <- function(model_name, force_download = FALSE, quiet = FALSE) {

  available_models <- c("ancova_cont_2arms", "ancova_cont_3arms")
  if (!model_name %in% available_models) {
    cli::cli_abort(c(
      "Unknown BayesFlow model: {.val {model_name}}",
      "i" = "Available models: {.val {available_models}}"
    ))
  }

  cache_dir <- get_model_cache_dir("bf")
  model_file <- file.path(cache_dir, paste0(model_name, ".keras"))

  if (!file.exists(model_file) || force_download) {
    if (!quiet) {
      cli::cli_alert_info("Downloading BayesFlow model: {.val {model_name}}")
    }
    download_model(model_name, model_file, "bf")
  } else {
    if (!quiet) {
      cli::cli_alert_info("Loading cached BayesFlow model: {.val {model_name}}")
    }
  }

  # Load via Python for full BayesFlow compatibility
  load_bf_model_python(model_file)
}


# =============================================================================
# CACHE MANAGEMENT
# =============================================================================

#' List Available Models
#'
#' Shows all available predefined models and their cache status.
#'
#' @param backend Filter by backend: "all", "brms", or "bf"
#'
#' @return Data frame with model info (model name, backend, cached status)
#' @export
#'
#' @examples
#' \dontrun{
#' # List all models
#' list_models()
#'
#' # List only BayesFlow models
#' list_models("bf")
#' }
list_models <- function(backend = c("all", "brms", "bf")) {
  backend <- match.arg(backend)
  models <- c("ancova_cont_2arms", "ancova_cont_3arms")

  result <- data.frame(model = character(), backend = character(),
                       cached = logical(), stringsAsFactors = FALSE)

  if (backend %in% c("all", "brms")) {
    brms_cache <- get_model_cache_dir("brms")
    brms_cached <- sapply(models, function(m) {
      file.exists(file.path(brms_cache, paste0(m, ".rds")))
    })
    result <- rbind(result, data.frame(
      model = models, backend = "brms", cached = brms_cached,
      stringsAsFactors = FALSE
    ))
  }

  if (backend %in% c("all", "bf")) {
    bf_cache <- get_model_cache_dir("bf")
    bf_cached <- sapply(models, function(m) {
      file.exists(file.path(bf_cache, paste0(m, ".keras")))
    })
    result <- rbind(result, data.frame(
      model = models, backend = "bf", cached = bf_cached,
      stringsAsFactors = FALSE
    ))
  }

  rownames(result) <- NULL
  result
}


#' Clear Model Cache
#'
#' Removes cached model files to free disk space or force re-download.
#'
#' @param backend Which cache to clear: "all", "brms", or "bf"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear all cached models
#' clear_model_cache()
#'
#' # Clear only BayesFlow models
#' clear_model_cache("bf")
#' }
clear_model_cache <- function(backend = c("all", "brms", "bf")) {
  backend <- match.arg(backend)

  cleared <- 0
  if (backend %in% c("all", "brms")) {
    brms_dir <- get_model_cache_dir("brms")
    brms_files <- list.files(brms_dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(brms_files) > 0) {
      unlink(brms_files)
      cleared <- cleared + length(brms_files)
    }
  }

  if (backend %in% c("all", "bf")) {
    bf_dir <- get_model_cache_dir("bf")
    bf_files <- list.files(bf_dir, pattern = "\\.keras$", full.names = TRUE)
    if (length(bf_files) > 0) {
      unlink(bf_files)
      cleared <- cleared + length(bf_files)
    }
  }

  if (cleared > 0) {
    cli::cli_alert_success("Cleared {cleared} cached model(s)")
  } else {
    cli::cli_alert_info("Cache already empty")
  }

  invisible(cleared)
}


#' Get Cache Size
#'
#' Returns the total size of cached models.
#'
#' @param backend Which cache to check: "all", "brms", or "bf"
#'
#' @return Named numeric vector with sizes in bytes
#' @export
#'
#' @examples
#' \dontrun{
#' cache_size <- get_cache_size()
#' print(paste("Total cache:", sum(cache_size) / 1e6, "MB"))
#' }
get_cache_size <- function(backend = c("all", "brms", "bf")) {
  backend <- match.arg(backend)

  sizes <- c(brms = 0, bf = 0)

  if (backend %in% c("all", "brms")) {
    brms_dir <- get_model_cache_dir("brms")
    brms_files <- list.files(brms_dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(brms_files) > 0) {
      sizes["brms"] <- sum(file.info(brms_files)$size, na.rm = TRUE)
    }
  }

  if (backend %in% c("all", "bf")) {
    bf_dir <- get_model_cache_dir("bf")
    bf_files <- list.files(bf_dir, pattern = "\\.keras$", full.names = TRUE)
    if (length(bf_files) > 0) {
      sizes["bf"] <- sum(file.info(bf_files)$size, na.rm = TRUE)
    }
  }

  sizes
}
