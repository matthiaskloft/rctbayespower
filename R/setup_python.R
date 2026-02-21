# =============================================================================
# PYTHON ENVIRONMENT SETUP
# =============================================================================
# Helper functions to set up Python environment for BayesFlow backend.
# Handles virtual environment creation, GPU detection, and package installation.

# =============================================================================
# PYTHON VERSION MANAGEMENT
# =============================================================================

#' Find or Install Python Version
#'
#' Finds an existing Python installation matching the requested version,
#' or installs it via reticulate's pyenv integration. This avoids issues
#' with Windows Store Python stubs.
#'
#' @param version Python version (e.g., "3.12", "3.11")
#'
#' @return Path to Python executable
#' @keywords internal
find_or_install_python <- function(version = "3.12") {
  # First, check if reticulate has pyenv-installed versions
  # These are in: ~/.pyenv/versions/ (Unix) or AppData/Local/r-reticulate/pyenv (Windows)

  # Try to find existing installation matching version
  python_path <- find_python_version(version)


  if (!is.null(python_path)) {
    return(python_path)
  }

  # Not found - install via reticulate
  cli::cli_alert_info("Python {.val {version}} not found, installing via pyenv...")

  tryCatch({
    reticulate::install_python(version = version)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to install Python {.val {version}}",
      "x" = conditionMessage(e),
      "i" = "Try installing Python manually from https://www.python.org/downloads/"
    ))
  })

  # Find the newly installed Python
  python_path <- find_python_version(version)

  if (is.null(python_path)) {
    cli::cli_abort(c(
      "Python was installed but could not be found",
      "i" = "Try restarting R and running setup again"
    ))
  }

  python_path
}


#' Find Python Installation by Version
#'
#' Searches for a Python installation matching the requested version.
#' Checks pyenv locations first (most reliable), then system paths.
#'
#' @param version Python version (e.g., "3.12", "3.11")
#'
#' @return Path to Python executable, or NULL if not found
#' @keywords internal
find_python_version <- function(version) {
  # Normalize version (e.g., "3.12" -> "3.12")
  version_parts <- strsplit(version, "\\.")[[1]]
  major_minor <- paste(version_parts[1:2], collapse = ".")

  # Check reticulate's pyenv installations first (most reliable)
  pyenv_root <- NULL

  if (.Platform$OS.type == "windows") {
    # Windows: AppData/Local/r-reticulate/r-reticulate/pyenv/pyenv-win/versions/
    pyenv_root <- file.path(
      Sys.getenv("LOCALAPPDATA"),
      "r-reticulate", "r-reticulate", "pyenv", "pyenv-win", "versions"
    )
  } else {
    # Unix: ~/.pyenv/versions/
    pyenv_root <- file.path(Sys.getenv("HOME"), ".pyenv", "versions")
  }

  if (!is.null(pyenv_root) && dir.exists(pyenv_root)) {
    # List installed versions
    versions <- list.dirs(pyenv_root, recursive = FALSE, full.names = TRUE)

    # Find matching version (e.g., 3.12.x matches 3.12)
    for (v_path in versions) {
      v_name <- basename(v_path)
      if (startsWith(v_name, major_minor)) {
        # Found matching version
        if (.Platform$OS.type == "windows") {
          python_exe <- file.path(v_path, "python.exe")
        } else {
          python_exe <- file.path(v_path, "bin", "python")
        }

        if (file.exists(python_exe)) {
          return(normalizePath(python_exe, winslash = "/"))
        }
      }
    }
  }

  # Fallback: try py_discover_config but filter out Windows Store
  tryCatch({
    configs <- reticulate::py_discover_config(required_module = NULL)
    if (!is.null(configs$python)) {
      python_path <- configs$python
      # Skip Windows Store Python (contains WindowsApps)
      if (!grepl("WindowsApps", python_path, fixed = TRUE)) {
        # Check version matches
        ver_output <- system2(python_path, "--version", stdout = TRUE, stderr = TRUE)
        if (any(grepl(major_minor, ver_output, fixed = TRUE))) {
          return(normalizePath(python_path, winslash = "/"))
        }
      }
    }
  }, error = function(e) NULL)

  NULL
}


# =============================================================================
# PIP BOOTSTRAPPING
# =============================================================================

#' Bootstrap pip in a Virtual Environment
#'
#' Ensures pip is available in a virtual environment. Uses `ensurepip` first,
#' falling back to `get-pip.py` if that fails. This works around a known
#' CPython bug where `venv` creation silently fails to bootstrap pip on Windows.
#'
#' @param envname Name of the virtual environment
#'
#' @return Invisibly returns TRUE on success
#' @keywords internal
bootstrap_pip <- function(envname) {
  # Normalize to resolve Windows 8.3 short paths (e.g., ONEDRI~1 -> OneDrive)
  python_path <- normalizePath(reticulate::virtualenv_python(envname), winslash = "/")

  # Try ensurepip first
  cli::cli_alert_info("Bootstrapping pip...")
  result <- suppressWarnings(
    system2(python_path, c("-m", "ensurepip", "--upgrade"),
            stdout = TRUE, stderr = TRUE)
  )
  status <- attr(result, "status")

  if (is.null(status) || status == 0) {
    # Verify pip actually works (ensurepip can exit 0 but leave pip broken on Windows)
    verify <- suppressWarnings(
      system2(python_path, c("-m", "pip", "--version"),
              stdout = TRUE, stderr = TRUE)
    )
    verify_status <- attr(verify, "status")
    if (is.null(verify_status) || verify_status == 0) {
      cli::cli_alert_success("pip bootstrapped via ensurepip")
      return(invisible(TRUE))
    }
    cli::cli_alert_warning("ensurepip exited successfully but pip is not functional")
  }

  # ensurepip failed - fall back to get-pip.py
  # First, remove broken pip directories so get-pip.py can install cleanly
  # (get-pip.py --force-reinstall fails when pip has no RECORD file)
  site_pkgs <- file.path(dirname(dirname(python_path)), "Lib", "site-packages")
  broken_pip_dir <- file.path(site_pkgs, "pip")
  if (dir.exists(broken_pip_dir) &&
      !file.exists(file.path(broken_pip_dir, "__main__.py"))) {
    cli::cli_alert_info("Removing broken pip installation before get-pip.py...")
    unlink(broken_pip_dir, recursive = TRUE)
    # Also remove any pip dist-info with missing RECORD file
    dist_infos <- list.dirs(site_pkgs, recursive = FALSE, full.names = TRUE)
    for (di in dist_infos[grepl("^pip-", basename(dist_infos))]) {
      if (!file.exists(file.path(di, "RECORD"))) {
        unlink(di, recursive = TRUE)
      }
    }
  }

  cli::cli_alert_warning("ensurepip failed, downloading get-pip.py...")
  get_pip_path <- tempfile(fileext = ".py")
  on.exit(unlink(get_pip_path), add = TRUE)

  tryCatch({
    utils::download.file(
      "https://bootstrap.pypa.io/get-pip.py",
      get_pip_path,
      quiet = TRUE
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download get-pip.py",
      "x" = conditionMessage(e),
      "i" = "Check your internet connection and try again"
    ))
  })

  result <- system2(python_path, get_pip_path, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")

  if (!is.null(status) && status != 0) {
    cli::cli_abort(c(
      "Failed to bootstrap pip",
      "i" = paste(utils::tail(result, 5), collapse = "\n")
    ))
  }

  cli::cli_alert_success("pip bootstrapped via get-pip.py")
  invisible(TRUE)
}


# =============================================================================
# ENVIRONMENT SETUP
# =============================================================================

# Supported versions (update as dependencies evolve)
.bf_supported_python <- c("3.10", "3.11", "3.12")
.bf_supported_cuda <- c("auto", "cpu", "11.8", "12.6", "12.8")


#' Set Up Python Environment for BayesFlow
#'
#' Creates and configures a Python virtual environment with all dependencies
#' needed for the BayesFlow backend. Automatically detects CUDA version and
#' installs GPU-enabled PyTorch when available.
#'
#' @param envname Name of the virtual environment (default: "r-rctbayespower")
#' @param cuda_version CUDA version to use. Options:
#'   - `"auto"` (default): Detect from nvidia-smi
#'   - `"12.8"`, `"12.6"`, `"11.8"`: Specific PyTorch CUDA build
#'   - `"cpu"`: Install CPU-only PyTorch
#' @param python_version Python version to use. Must be 3.10, 3.11, or 3.12
#'   (BayesFlow 2.0 requirement). Default: "3.12"
#' @param force If TRUE, recreate environment even if it exists (default: FALSE)
#'
#' @return Invisibly returns TRUE on success
#' @export
#'
#' @examples
#' \dontrun{
#' # Auto-detect GPU and set up environment
#' setup_bf_python()
#'
#' # Force CPU-only installation
#' setup_bf_python(cuda_version = "cpu")
#'
#' # Use specific CUDA version
#' setup_bf_python(cuda_version = "12.6")
#' }
setup_bf_python <- function(envname = "r-rctbayespower",
                            cuda_version = "auto",
                            python_version = "3.12",
                            force = FALSE) {

  # -------------------------------------------------------------------------

  # Validate inputs
  # -------------------------------------------------------------------------

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package 'reticulate' is required",
      "i" = "Install with: install.packages('reticulate')"
    ))
  }

  # Validate Python version (BayesFlow 2.0 requires 3.10-3.12)
  py_major_minor <- paste(strsplit(python_version, "\\.")[[1]][1:2], collapse = ".")
  if (!py_major_minor %in% .bf_supported_python) {
    cli::cli_abort(c(
      "Python {.val {python_version}} is not supported by BayesFlow 2.0",
      "x" = "BayesFlow 2.0 requires Python 3.10, 3.11, or 3.12",
      "i" = "Recommended: {.code setup_bf_python(python_version = \"3.12\")}"
    ))
  }

  # Validate CUDA version
  if (!cuda_version %in% .bf_supported_cuda) {
    cli::cli_abort(c(
      "CUDA version {.val {cuda_version}} is not a valid option",
      "x" = "PyTorch 2.7+ supports: {.val {.bf_supported_cuda}}",
      "i" = "Use {.val auto} for automatic detection, or {.val cpu} for CPU-only"
    ))
  }

  cli::cli_h1("Setting up Python environment for BayesFlow")

  # Warn if virtualenv root is on cloud-synced storage (OneDrive, Dropbox, etc.)
  # OneDrive "Files On-Demand" evicts .py files to cloud-only storage, making
  # Python unable to import them (they appear as directories but files are missing).
  venv_root <- normalizePath(reticulate::virtualenv_root(), mustWork = FALSE)
  if (grepl("OneDrive|Dropbox|iCloudDrive|Google Drive", venv_root, ignore.case = TRUE)) {
    local_suggestion <- normalizePath(
      file.path(Sys.getenv("USERPROFILE", Sys.getenv("HOME")), ".virtualenvs"),
      mustWork = FALSE, winslash = "/"
    )
    # Fall back if the USERPROFILE itself is on cloud storage
    if (grepl("OneDrive|Dropbox|iCloudDrive|Google Drive", local_suggestion, ignore.case = TRUE)) {
      local_suggestion <- "C:/virtualenvs"
    }
    cli::cli_abort(c(
      "Virtualenv root is on cloud-synced storage: {.path {venv_root}}",
      "x" = paste0(
        "Cloud sync (OneDrive Files On-Demand, Dropbox) evicts .py files to ",
        "cloud-only storage. Python cannot import from evicted files."
      ),
      "i" = paste0(
        "Fix: run the following before calling {.code setup_bf_python()}:\n",
        "  {.code Sys.setenv(RETICULATE_VIRTUALENV_ROOT = \"", local_suggestion, "\")}"
      ),
      "i" = "Or add that line to your {.file .Renviron} for a permanent fix."
    ))
  }

  # Step 1: Ensure Python version is installed via reticulate's pyenv
  cli::cli_alert_info("Ensuring Python {.val {python_version}} is available...")
  python_path <- find_or_install_python(python_version)
  cli::cli_alert_success("Using Python: {.path {python_path}}")

  # Step 2: Check/create virtual environment
  envpath <- file.path(reticulate::virtualenv_root(), envname)

  if (dir.exists(envpath) && !force) {
    cli::cli_alert_info("Using existing environment: {.path {envpath}}")
  } else {
    if (dir.exists(envpath) && force) {
      cli::cli_alert_warning("Removing existing environment (force = TRUE)")
      unlink(envpath, recursive = TRUE)
    }
    cli::cli_alert_info("Creating virtual environment: {.val {envname}}")
    # Use packages = FALSE to avoid pip bootstrap failures on Windows
    # (CPython bug: ensurepip can silently fail in venv creation)
    reticulate::virtualenv_create(
      envname = envname, python = python_path, packages = FALSE
    )
    bootstrap_pip(envname)
  }

  # Use this environment
  reticulate::use_virtualenv(envname, required = TRUE)

  # Step 2: Detect CUDA version
  if (cuda_version == "auto") {
    cuda_version <- detect_cuda_version()
  }

  # Step 3: Install packages
  install_bf_dependencies(envname = envname, cuda_version = cuda_version)

  # Step 4: Verify installation
  # Re-initialize Python after pip installs (reticulate quirk)
  cli::cli_h2("Verifying installation")
  reticulate::use_virtualenv(envname, required = TRUE)

  # Force Python initialization
  if (!reticulate::py_available(initialize = TRUE)) {
    cli::cli_alert_warning("Could not initialize Python for verification")
    cli::cli_alert_info("Try running {.code bf_status()} after restarting R")
  } else {
    verify_bf_installation()
  }

  cli::cli_alert_success("Setup complete!")
  cli::cli_alert_info("Environment: {.path {envpath}}")

  invisible(TRUE)
}


#' Detect CUDA Version from System
#'
#' Runs nvidia-smi to detect the maximum supported CUDA version.
#'
#' @return Character string with CUDA version (e.g., "12.4") or "cpu" if no GPU
#' @export
#'
#' @examples
#' \dontrun{
#' cuda_ver <- detect_cuda_version()
#' cat("Detected CUDA:", cuda_ver, "\n")
#' }
detect_cuda_version <- function() {
  cli::cli_alert_info("Detecting CUDA version...")

  # Try nvidia-smi
  result <- tryCatch({
    output <- system2("nvidia-smi", stdout = TRUE, stderr = TRUE)
    paste(output, collapse = "\n")
  }, error = function(e) NULL, warning = function(w) NULL)

  if (is.null(result) || length(result) == 0) {
    cli::cli_alert_warning("nvidia-smi not found - using CPU-only PyTorch")
    return("cpu")
  }

  # Parse CUDA version from output
  # Look for pattern like "CUDA Version: 12.4"
  cuda_match <- regmatches(result, regexpr("CUDA Version: ([0-9]+\\.[0-9]+)", result))

  if (length(cuda_match) == 0 || cuda_match == "") {
    cli::cli_alert_warning("Could not parse CUDA version - using CPU-only PyTorch")
    return("cpu")
  }

  cuda_ver <- sub("CUDA Version: ", "", cuda_match)
  cli::cli_alert_success("Detected CUDA version: {.val {cuda_ver}}")

  # Map to supported PyTorch CUDA versions
  cuda_major_minor <- as.numeric(strsplit(cuda_ver, "\\.")[[1]])
  cuda_major <- cuda_major_minor[1]
  cuda_minor <- cuda_major_minor[2]



  # PyTorch 2.7+ supports: cu118, cu126, cu128 (as of 2025)
  # CUDA 12.1 and 12.4 have been removed from latest PyTorch
  # CUDA is backward compatible, so pick highest PyTorch-supported version
  if (cuda_major >= 13) {
    # CUDA 13.x - use cu128 (latest)
    selected <- "12.8"
    cli::cli_alert_info("CUDA {cuda_ver} detected, using PyTorch CUDA 12.8 build")
  } else if (cuda_major == 12) {
    if (cuda_minor >= 8) {
      selected <- "12.8"
    } else if (cuda_minor >= 6) {
      selected <- "12.6"
    } else {
      # CUDA 12.0-12.5: cu121/cu124 removed, use cu126 (backward compatible)
      selected <- "12.6"
      cli::cli_alert_info("CUDA {cuda_ver} detected, using PyTorch CUDA 12.6 build (12.1/12.4 deprecated)")
    }
  } else if (cuda_major == 11 && cuda_minor >= 8) {
    selected <- "11.8"
  } else {
    cli::cli_alert_warning("CUDA {cuda_ver} is older than supported versions - using CPU")
    return("cpu")
  }

  cli::cli_alert_info("Selected PyTorch CUDA version: {.val {selected}}")
  selected
}


#' Install BayesFlow Dependencies
#'
#' Installs PyTorch (with GPU support if available), BayesFlow, Keras, and NumPy.
#'
#' @param envname Name of the virtual environment
#' @param cuda_version CUDA version ("12.6", "12.4", "12.1", "11.8", or "cpu")
#'
#' @return Invisibly returns TRUE on success
#' @export
#'
#' @examples
#' \dontrun{
#' # Install with CUDA 12.4 support
#' install_bf_dependencies(cuda_version = "12.4")
#' }
install_bf_dependencies <- function(envname = "r-rctbayespower",
                                    cuda_version = "auto") {

  if (cuda_version == "auto") {
    cuda_version <- detect_cuda_version()
  }

  # Build PyTorch index URL
  if (cuda_version == "cpu") {
    torch_index <- "https://download.pytorch.org/whl/cpu"
    cli::cli_h2("Installing CPU-only PyTorch")
  } else {
    cuda_tag <- paste0("cu", gsub("\\.", "", cuda_version))
    torch_index <- paste0("https://download.pytorch.org/whl/", cuda_tag)
    cli::cli_h2("Installing PyTorch with CUDA {cuda_version}")
  }

  # Get Python path (normalize to resolve Windows 8.3 short paths)
  python_path <- normalizePath(reticulate::virtualenv_python(envname), winslash = "/")

  # Step 1: Install PyTorch with correct CUDA version
  cli::cli_alert_info("Installing PyTorch from {.url {torch_index}}")

  torch_cmd <- paste(
    shQuote(python_path),
    "-m pip install torch torchvision torchaudio",
    "--index-url", torch_index
  )

  result <- system(torch_cmd, intern = FALSE)
  if (result != 0) {
    cli::cli_abort("Failed to install PyTorch")
  }

  # Step 2: Install BayesFlow and other dependencies
  cli::cli_alert_info("Installing BayesFlow, Keras, and NumPy")

  bf_cmd <- paste(
    shQuote(python_path),
    "-m pip install bayesflow keras numpy"
  )

  result <- system(bf_cmd, intern = FALSE)
  if (result != 0) {
    cli::cli_abort("Failed to install BayesFlow dependencies")
  }

  cli::cli_alert_success("All packages installed")
  invisible(TRUE)
}


#' Verify BayesFlow Installation
#'
#' Checks that all required Python packages are installed and working.
#' Reports GPU availability and package versions.
#'
#' @return List with verification results (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' verify_bf_installation()
#' }
verify_bf_installation <- function() {
  results <- list(
    python = FALSE,
    torch = FALSE,
    cuda = FALSE,
    bayesflow = FALSE,
    keras = FALSE,
    numpy = FALSE
  )

  # Check Python
  if (reticulate::py_available()) {
    results$python <- TRUE
    py_ver <- reticulate::py_config()$version
    cli::cli_alert_success("Python: {.val {py_ver}}")
  } else {
    cli::cli_alert_danger("Python: not available")
    return(invisible(results))
  }

  # Check torch
  if (reticulate::py_module_available("torch")) {
    torch <- reticulate::import("torch")

    # __version__ can fail when the native _C extension is in a broken state
    # (e.g. path with Unicode characters on Windows prevents DLL loading)
    torch_ver <- tryCatch(
      torch$`__version__`,
      error = function(e) {
        tryCatch(
          reticulate::py_eval("import torch; torch.__version__"),
          error = function(e2) NULL
        )
      }
    )

    if (!is.null(torch_ver)) {
      results$torch <- TRUE
      cli::cli_alert_success("PyTorch: {.val {torch_ver}}")

      # Check CUDA
      cuda_ok <- tryCatch(torch$cuda$is_available(), error = function(e) FALSE)
      if (isTRUE(cuda_ok)) {
        results$cuda <- TRUE
        device_name <- tryCatch(torch$cuda$get_device_name(0L), error = function(e) "unknown")
        cuda_ver <- tryCatch(torch$version$cuda, error = function(e) "unknown")
        cli::cli_alert_success("CUDA: {.val {cuda_ver}} ({device_name})")
      } else {
        cli::cli_alert_warning("CUDA: not available (CPU-only)")
      }
    } else {
      cli::cli_alert_danger("PyTorch: import succeeded but module is non-functional")
      cli::cli_alert_info(
        "Likely cause: virtualenv path contains non-ASCII characters (e.g. {.val \u00fc})"
      )
      cli::cli_alert_info(paste0(
        "Fix: set RETICULATE_VIRTUALENV_ROOT to an ASCII-only path before calling ",
        "{.code setup_bf_python()}"
      ))
    }
  } else {
    cli::cli_alert_danger("PyTorch: not installed")
  }

  # Check bayesflow
  if (reticulate::py_module_available("bayesflow")) {
    results$bayesflow <- TRUE
    bf <- reticulate::import("bayesflow")
    bf_ver <- tryCatch(bf$`__version__`, error = function(e) "unknown")
    cli::cli_alert_success("BayesFlow: {.val {bf_ver}}")
  } else {
    cli::cli_alert_danger("BayesFlow: not installed")
  }

  # Check keras
  if (reticulate::py_module_available("keras")) {
    results$keras <- TRUE
    keras <- reticulate::import("keras")
    keras_ver <- keras$`__version__`
    cli::cli_alert_success("Keras: {.val {keras_ver}}")
  } else {
    cli::cli_alert_danger("Keras: not installed")
  }

  # Check numpy
  if (reticulate::py_module_available("numpy")) {
    np <- reticulate::import("numpy")
    np_ver <- tryCatch(np$`__version__`, error = function(e) NULL)
    if (!is.null(np_ver)) {
      results$numpy <- TRUE
      cli::cli_alert_success("NumPy: {.val {np_ver}}")
    } else {
      cli::cli_alert_danger("NumPy: import succeeded but module is non-functional")
    }
  } else {
    cli::cli_alert_danger("NumPy: not installed")
  }

  # Summary
  all_ok <- all(unlist(results[c("python", "torch", "bayesflow", "keras", "numpy")]))

  cli::cli_h2("Status")
  if (all_ok && results$cuda) {
    cli::cli_alert_success("BayesFlow ready with GPU acceleration")
  } else if (all_ok) {
    cli::cli_alert_success("BayesFlow ready (CPU-only)")
  } else {
    missing <- names(results)[!unlist(results)]
    cli::cli_alert_danger("Missing packages: {.val {missing}}")
    cli::cli_alert_info("Run {.code setup_bf_python()} to install")
  }

  invisible(results)
}


#' Show Python Environment Status
#'
#' Displays current Python configuration and BayesFlow availability.
#' Automatically initializes Python if not already active.
#'
#' @param envname Optional name of Python virtual environment to check.
#'   If NULL (default), uses the currently active environment or auto-detects.
#'
#' @return Invisibly returns list with status results
#' @export
#'
#' @examples
#' \dontrun{
#' # Check current/default environment
#' check_bf_status()
#'
#' # Check specific environment
#' check_bf_status(envname = "r-rctbayespower")
#' }


#' Check BayesFlow Environment Status
#'
#' Displays current Python configuration and BayesFlow availability.
#' This is the new, refactored version that uses shared helper functions
#' to reduce code duplication.
#'
#' The function automatically initializes Python if not already active and
#' shows comprehensive status information including Python version, virtual
#' environment, package versions, and GPU availability.
#'
#' @param envname Optional name of Python virtual environment to check.
#'   If NULL (default), uses the currently active environment or auto-detects.
#'
#' @return Invisibly returns list with status results
#' @export
#'
#' @examples
#' \dontrun{
#' # Check current/default environment
#' check_bf_status()
#'
#' # Check specific environment
#' check_bf_status(envname = "rctbp-3-12")
#' }
check_bf_status <- function(envname = NULL) {
  cli::cli_h1("BayesFlow Environment Status")

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_alert_danger("reticulate package not installed")
    return(invisible(NULL))
  }

  # ==========================================================================
  # STEP 1: Environment activation (using shared helper, non-strict mode)
  # ==========================================================================
  tryCatch({
    rctbayespower::setup_bf_environment(envname, strict = FALSE)
  }, error = function(e) {
    cli::cli_alert_danger("Environment setup failed: {conditionMessage(e)}")

    # Show available environments
    envs <- tryCatch(rctbayespower::get_bf_envs(), error = function(e) {
      list(all_envs = character(0), bf_envs = character(0), recommended_env = NULL)
    })

    if (length(envs$bf_envs) > 0) {
      cli::cli_alert_info("Available BayesFlow environments: {.val {envs$bf_envs}}")
    } else if (length(envs$all_envs) > 0) {
      cli::cli_alert_info("Available environments: {.val {envs$all_envs}}")
    } else {
      cli::cli_alert_info("Run {.code setup_bf_python()} to create environment")
    }
    return(invisible(NULL))
  })

  # ==========================================================================
  # STEP 2: Python initialization (using shared helper, non-strict mode)
  # ==========================================================================
  if (!rctbayespower::check_bf_python_ready(report_envs = FALSE)) {
    cli::cli_alert_warning("Python not available")

    # Show available virtualenvs
    envs <- tryCatch(rctbayespower::get_bf_envs(), error = function(e) {
      list(all_envs = character(0), bf_envs = character(0), recommended_env = NULL)
    })

    if (length(envs$bf_envs) > 0) {
      cli::cli_alert_info("Found BayesFlow environments: {.val {envs$bf_envs}}")
      cli::cli_alert_info("Activate with: {.code check_bf_status(envname = \"{envs$recommended_env}\")}")
    } else if (length(envs$all_envs) > 0) {
      cli::cli_alert_info("Available environments: {.val {envs$all_envs}}")
    } else {
      cli::cli_alert_info("Run {.code setup_bf_python()} to create environment")
    }
    return(invisible(NULL))
  }

  # ==========================================================================
  # STEP 3: Status display (unique functionality)
  # ==========================================================================
  config <- reticulate::py_config()

  cli::cli_h2("Python Configuration")
  cli::cli_alert_info("Python: {.path {config$python}}")
  cli::cli_alert_info("Version: {.val {config$version}}")

  if (!is.null(config$virtualenv)) {
    cli::cli_alert_info("Virtual env: {.path {config$virtualenv}}")
  }

  cli::cli_h2("Package Status")
  results <- verify_bf_installation()

  invisible(results)
}
