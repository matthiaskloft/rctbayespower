# =============================================================================
# BAYESFLOW BACKEND
# =============================================================================
# All BayesFlow-specific posterior estimation and summarization.
# Uses fast vectorized matrix operations for maximum performance.
# Models are loaded via Python/reticulate for full BayesFlow compatibility.
#
# Entry points:
#   - estimate_single_bf(): Single analysis estimation (wraps batch of 1)
#   - estimate_sequential_bf(): Sequential/interim analysis estimation (batched)
#   - estimate_batch_bf(): Core batch estimation (single forward pass)
#
# Key functions:
#   - load_bf_model_python(): Load .keras model via Python
#   - summarize_post_bf(): Fast vectorized summarization (NOT compute_measures())
#   - prepare_single_as_batch_bf(): Convert single data.frame to batch format
#   - prepare_data_list_as_batch_bf(): Convert list of data.frames to batch format
#
# BayesFlow Model Types Supported:
#   - bf.BasicWorkflow object (has .sample() method)
#   - bf.approximators.Approximator object (has .sample() method)
#   - Raw Keras model (has .predict() method, returns distribution params)
#
# Output: All functions produce results compatible with summarize_sims()
#
# PRIORITY: Fast computation via vectorized matrix operations.
# This backend does NOT use compute_measures() or rvar operations.

# =============================================================================
# PYTHON ENVIRONMENT MANAGEMENT
# =============================================================================

# Package-level Python module cache
.bf_cache <- new.env(parent = emptyenv())


#' Get BayesFlow Environment Information
#'
#' Returns information about the current Python environment and compute device
#' (CPU or GPU) for BayesFlow inference. Used to display status messages during
#' power analysis.
#'
#' @param envname Optional name of Python virtual environment to check.
#'   If NULL (default), uses the currently active environment.
#'
#' @return A list with:
#'   \itemize{
#'     \item `device`: "GPU" or "CPU"
#'     \item `device_name`: GPU name (e.g., "NVIDIA RTX 4090") or "CPU"
#'     \item `cuda_version`: CUDA version string or NULL for CPU
#'     \item `cpu_name`: CPU processor name or NULL if unavailable
#'     \item `envname`: Name of the Python virtual environment or NULL
#'     \item `python_path`: Path to Python executable
#'     \item `python_version`: Python version string (e.g., "3.12.0")
#'     \item `pkg_versions`: Named list of package versions (bayesflow, keras, torch, numpy)
#'   }
#'   Returns NULL if BayesFlow is not available.
#' @export
#'
#' @examples
#' \dontrun{
#' info <- get_bf_env_info()
#' if (!is.null(info)) {
#'   cat("Device:", info$device_name, "\n")
#'   cat("Environment:", info$envname, "\n")
#'   cat("Python:", info$python_version, "\n")
#'   cat("BayesFlow:", info$pkg_versions$bayesflow, "\n")
#' }
#'
#' # Check specific environment
#' info <- get_bf_env_info(envname = "r-rctbayespower")
#' }
get_bf_env_info <- function(envname = NULL) {
  # Check if Python is already initialized
  python_already_initialized <- reticulate::py_available(initialize = FALSE)

  # If envname specified and Python already initialized, check compatibility
  if (!is.null(envname) && nchar(envname) > 0 && python_already_initialized) {
    current_config <- reticulate::py_config()
    current_path <- normalizePath(current_config$python, winslash = "/", mustWork = FALSE)

    requested_path <- tryCatch({
      venv_python <- reticulate::virtualenv_python(envname)
      normalizePath(venv_python, winslash = "/", mustWork = FALSE)
    }, error = function(e) NULL)

    if (!is.null(requested_path) && !identical(current_path, requested_path)) {
      # Cannot switch - silently use current environment
      envname <- NULL
    }
  }

  # Activate specified environment if provided (only if Python not yet initialized)
  if (!is.null(envname) && nchar(envname) > 0 && !python_already_initialized) {
    tryCatch({
      reticulate::use_virtualenv(envname, required = TRUE)
    }, error = function(e) {
      # Silently fall back to current environment
    })
  }

  # Return NULL if BayesFlow not available
  if (!check_bf_available(silent = TRUE)) {
    return(NULL)
  }

  # Initialize Python if needed
  if (!reticulate::py_available(initialize = TRUE)) {
    return(NULL)
  }

  info <- list(
    device = "CPU",
    device_name = "CPU",
    cuda_version = NULL,
    cpu_name = NULL,
    envname = NULL,
    python_path = NULL,
    python_version = NULL,
    pkg_versions = list(
      bayesflow = NULL,
      keras = NULL,
      torch = NULL,
      numpy = NULL
    )
  )

  # Get Python configuration
  config <- tryCatch(reticulate::py_config(), error = function(e) NULL)
  if (!is.null(config)) {
    info$python_path <- config$python

    # Extract environment name from path
    # Common patterns: .../envs/envname/... or .../virtualenvs/envname/...
    path <- config$python
    if (!is.null(config$virtualenv)) {
      info$envname <- basename(config$virtualenv)
    } else {
      # Try to extract from path
      env_match <- regmatches(path, regexpr("(envs|virtualenvs|venv)[/\\\\]([^/\\\\]+)", path))
      if (length(env_match) > 0 && nchar(env_match) > 0) {
        info$envname <- sub(".*[/\\\\]", "", env_match)
      }
    }
  }

  # Get CPU info and Python version via platform module
  tryCatch({
    platform <- reticulate::import("platform", delay_load = FALSE)
    cpu_name <- platform$processor()
    # On some systems processor() returns empty, try uname().machine as fallback
    if (is.null(cpu_name) || nchar(cpu_name) == 0) {
      cpu_name <- platform$machine()
    }
    if (!is.null(cpu_name) && nchar(cpu_name) > 0) {
      info$cpu_name <- cpu_name
    }
    # Get Python version
    info$python_version <- platform$python_version()
  }, error = function(e) {
    # Platform module not available - leave cpu_name/python_version as NULL
  })

  # Get package versions using importlib.metadata (Python 3.8+)
  tryCatch({
    importlib_metadata <- reticulate::import("importlib.metadata", delay_load = FALSE)
    get_version <- function(pkg) {
      tryCatch(
        importlib_metadata$version(pkg),
        error = function(e) NULL
      )
    }
    info$pkg_versions$bayesflow <- get_version("bayesflow")
    info$pkg_versions$keras <- get_version("keras")
    info$pkg_versions$torch <- get_version("torch")
    info$pkg_versions$numpy <- get_version("numpy")
  }, error = function(e) {
    # importlib.metadata not available - leave versions as NULL
  })

  # Check GPU availability via PyTorch
  tryCatch({
    torch <- reticulate::import("torch", delay_load = FALSE)
    if (torch$cuda$is_available()) {
      info$device <- "GPU"
      info$device_name <- torch$cuda$get_device_name(0L)
      info$cuda_version <- torch$version$cuda
    }
  }, error = function(e) {
    # PyTorch not available or CUDA check failed - stay with CPU
  })

  info
}


#' Check BayesFlow Availability
#'
#' Checks if Python, reticulate, and BayesFlow are available for use.
#' This is a fast check that can be called before attempting inference.
#'
#' @param silent If TRUE, return FALSE instead of error (default FALSE)
#'
#' @return TRUE if BayesFlow is available, FALSE or error otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' if (check_bf_available(silent = TRUE)) {
#'   message("BayesFlow is available!")
#' }
#' }
check_bf_available <- function(silent = FALSE) {
  # Check reticulate package

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    if (silent) return(FALSE)
    cli::cli_abort(c(
      "Package 'reticulate' is required for BayesFlow backend",
      "i" = "Install it with: install.packages('reticulate')"
    ))
  }

  # Check Python available
  if (!reticulate::py_available(initialize = TRUE)) {
    if (silent) return(FALSE)
    cli::cli_abort(c(
      "Python is not available",
      "i" = "Configure Python with: reticulate::use_python() or reticulate::use_condaenv()"
    ))
  }

  # Check BayesFlow module
  bf_available <- reticulate::py_module_available("bayesflow")
  if (!bf_available) {
    if (silent) return(FALSE)
    cli::cli_abort(c(
      "Python package 'bayesflow' is not installed",
      "i" = "Install it with: pip install bayesflow",
      "i" = "Or from R: reticulate::py_install('bayesflow')"
    ))
  }

  TRUE
}


#' Check if Cached Python Objects are Still Valid
#'
#' Verifies that cached Python module references are still alive.
#' Python objects can become stale if the Python session restarts.
#'
#' @return Logical TRUE if cache is valid, FALSE otherwise
#' @keywords internal
is_bf_cache_valid <- function() {
  if (!isTRUE(.bf_cache$initialized)) {
    return(FALSE)
  }

  # Check if Python is still available
  if (!reticulate::py_available(initialize = FALSE)) {
    return(FALSE)
  }

  # Try to access a simple attribute on the cached module
  # This will fail if the Python object reference is stale
  tryCatch({
    # Access __name__ attribute - all modules have this
    if (!is.null(.bf_cache$bf)) {
      .bf_cache$bf$`__name__`
    }
    TRUE
  }, error = function(e) {
    FALSE
  })
}


#' Require BayesFlow Initialization (Internal)
#'
#' Checks that BayesFlow has been initialized via [init_bf_python()].
#' Used internally by functions that need Python modules.
#' Errors with helpful message if not initialized.
#'
#' @return List with bf, np, and keras Python modules
#' @keywords internal
require_bf_init <- function() {
 if (!is_bf_cache_valid()) {
    # Try to auto-reinitialize in two cases:
    # 1. Python is already active (e.g. check_bf_status() ran but init_bf() was skipped)
    # 2. We have a stored envname from a previous init_bf() call (GC-staled objects)
    can_reinit <- reticulate::py_available(initialize = FALSE) ||
      (!is.null(.bf_cache$envname) && nzchar(.bf_cache$envname))

    if (can_reinit) {
      tryCatch({
        # Silent reinit - just refresh the module references
        reinit_bf_cache()
      }, error = function(e) {
        # Reinit failed, give user the standard error
      })
    }

    # Check again after potential reinit
    if (!is_bf_cache_valid()) {
      cli::cli_abort(c(
        "BayesFlow not initialized",
        "i" = "Call {.code init_bf()} to initialize BayesFlow",
        "i" = "Note: This is required after {.code devtools::load_all()} or package reload"
      ))
    }
  }
  list(bf = .bf_cache$bf, np = .bf_cache$np, keras = .bf_cache$keras)
}

#' Reinitialize BayesFlow Cache (Internal)
#'
#' Refreshes the cached Python module references without full reinitialization.
#' Used when Python GC has made cached objects stale.
#'
#' @keywords internal
reinit_bf_cache <- function() {
  # Reimport core modules
  .bf_cache$np <- reticulate::import("numpy", convert = FALSE)
  .bf_cache$keras <- reticulate::import("keras")
  .bf_cache$bf <- reticulate::import("bayesflow")
  .bf_cache$initialized <- TRUE
}


#' Check All BayesFlow Dependencies
#'
#' Validates that all required Python packages are available.
#' More comprehensive than [check_bf_available()] which only checks bayesflow.
#'
#' @param silent If TRUE, return FALSE instead of error (default FALSE)
#'
#' @return TRUE if all dependencies available, FALSE or error otherwise
#' @keywords internal
check_bf_dependencies <- function(silent = FALSE) {
  # First check basic availability
  if (!check_bf_available(silent = TRUE)) {
    if (silent) return(FALSE)
    check_bf_available(silent = FALSE)  # Will error with details
  }

  # Check each required package
  required_pkgs <- c("torch", "keras", "numpy", "bayesflow")
  missing <- character()

  for (pkg in required_pkgs) {
    if (!reticulate::py_module_available(pkg)) {
      missing <- c(missing, pkg)
    }
  }

  if (length(missing) > 0) {
    if (silent) return(FALSE)
    cli::cli_abort(c(
      "Missing required Python packages: {.val {missing}}",
      "i" = "Run {.code setup_bf_python()} to install all dependencies",
      "i" = "Or install manually: {.code pip install {paste(missing, collapse = ' ')}}"
    ))
  }

  TRUE
}


# =============================================================================
# SHARED PYTHON ENVIRONMENT HELPERS
# =============================================================================

#' Discover Available BayesFlow Virtual Environments
#'
#' Discovers available Python virtual environments and identifies which ones
#' are likely BayesFlow environments based on naming patterns.
#'
#' @return Named list with:
#'   \itemize{
#'     \item `all_envs`: All available virtual environments
#'     \item `bf_envs`: Environments matching BayesFlow naming patterns
#'     \item `recommended_env`: First BayesFlow environment or NULL
#'   }
#' @keywords internal
get_bf_envs <- function() {
  venvs <- tryCatch(reticulate::virtualenv_list(), error = function(e) character(0))

  # Filter to environments that actually exist (virtualenv_list can return stale entries)
  venvs <- vapply(venvs, function(env) {
    tryCatch({
      # Check if Python executable exists
      py_path <- reticulate::virtualenv_python(env)
      file.exists(py_path)
    }, error = function(e) FALSE)
  }, logical(1))
  venvs <- names(venvs)[venvs]

  bf_venvs <- venvs[grepl("rctbp|rctbayespower", venvs, ignore.case = TRUE)]

  list(
    all_envs = venvs,
    bf_envs = bf_venvs,
    recommended_env = if (length(bf_venvs) > 0) bf_venvs[1] else NULL
  )
}


#' Setup BayesFlow Python Environment
#'
#' Handles Python environment activation with environment compatibility checking.
#' This centralizes the environment activation logic used by multiple functions.
#'
#' @param envname Name of virtual environment to activate, or NULL for current
#' @param strict If TRUE, abort on environment conflicts; if FALSE, warn and continue
#'
#' @return Named list with:
#'   \itemize{
#'     \item `success`: TRUE if environment setup succeeded
#'     \item `switched_env`: TRUE if environment was activated (vs already active)
#'     \item `final_envname`: The environment name that's actually being used
#'   }
#' @keywords internal
setup_bf_environment <- function(envname = NULL, strict = FALSE) {
  if (is.null(envname) || nchar(envname) == 0) {
    return(list(success = TRUE, switched_env = FALSE, final_envname = NULL))
  }

  python_already_init <- reticulate::py_available(initialize = FALSE)

  if (python_already_init) {
    # Check environment compatibility
    current_config <- reticulate::py_config()
    current_path <- normalizePath(current_config$python, winslash = "/", mustWork = FALSE)

    requested_path <- tryCatch({
      venv_python <- reticulate::virtualenv_python(envname)
      normalizePath(venv_python, winslash = "/", mustWork = FALSE)
    }, error = function(e) NULL)

    if (!is.null(requested_path) && !identical(current_path, requested_path)) {
      current_envname <- basename(dirname(dirname(current_path)))

      error_msg <- c(
        "Cannot switch Python environment in active R session",
        if (strict) "x" else "!" = "Requested: {.val {envname}}",
        if (strict) "x" else "!" = "Currently using: {.val {current_envname}}",
        "i" = "Restart R session to use a different Python environment"
      )

      if (strict) {
        cli::cli_abort(error_msg)
      } else {
        cli::cli_warn(error_msg)
        return(list(success = TRUE, switched_env = FALSE, final_envname = NULL))
      }
    }

    return(list(success = TRUE, switched_env = FALSE, final_envname = envname))
  } else {
    # Python not initialized - activate the environment
    tryCatch({
      reticulate::use_virtualenv(envname, required = TRUE)
      return(list(success = TRUE, switched_env = TRUE, final_envname = envname))
    }, error = function(e) {
      envs <- get_bf_envs()

      error_msg <- c(
        "Could not activate virtual environment {.val {envname}}",
        "x" = conditionMessage(e)
      )

      if (length(envs$bf_envs) > 0) {
        error_msg <- c(error_msg,
          "i" = "Available BayesFlow environments: {.val {envs$bf_envs}}",
          "i" = "Try: {.code init_bf(\"{envs$recommended_env}\")}"
        )
      } else if (length(envs$all_envs) > 0) {
        error_msg <- c(error_msg,
          "i" = "Available environments: {.val {envs$all_envs}}",
          "i" = "Run {.code setup_bf_python()} to create a BayesFlow environment"
        )
      } else {
        error_msg <- c(error_msg,
          "i" = "Run {.code setup_bf_python()} to create a Python environment"
        )
      }

      cli::cli_abort(error_msg)
    })
  }
}


#' Check if BayesFlow Python is Ready
#'
#' Verifies that Python is initialized and optionally reports available
#' environments if initialization fails. This centralizes Python readiness
#' checking with helpful error messages.
#'
#' @param report_envs If TRUE, show helpful environment suggestions on failure
#'
#' @return TRUE if Python is ready, FALSE if not (only when report_envs=FALSE)
#' @keywords internal
check_bf_python_ready <- function(report_envs = TRUE) {
  if (reticulate::py_available(initialize = TRUE)) {
    return(TRUE)
  }

  if (!report_envs) {
    return(FALSE)
  }

  # Python not available - show helpful suggestions
  envs <- get_bf_envs()

  error_msg <- c("Python is not available after environment activation")

  if (length(envs$bf_envs) > 0) {
    error_msg <- c(error_msg,
      "i" = "Found BayesFlow environments: {.val {envs$bf_envs}}",
      "i" = "Try restarting R and calling: {.code init_bf(\"{envs$recommended_env}\")}"
    )
  } else if (length(envs$all_envs) > 0) {
    error_msg <- c(error_msg,
      "i" = "Available environments: {.val {envs$all_envs}}",
      "i" = "Run {.code setup_bf_python()} to create a BayesFlow environment"
    )
  } else {
    error_msg <- c(error_msg,
      "i" = "Run {.code setup_bf_python()} to create a Python environment"
    )
  }

  cli::cli_abort(error_msg)
}


# =============================================================================
# REFACTORED BAYESFLOW INITIALIZATION FUNCTIONS
# =============================================================================

#' Initialize BayesFlow Python Environment and Load Modules
#'
#' Initializes BayesFlow Python environment and loads required modules
#' (BayesFlow, Keras, NumPy) for use in power analysis. This is the new,
#' refactored version that uses shared helper functions to reduce code duplication.
#'
#' **Recommended workflow:**
#' 1. First time: Run `setup_bf_python()` to create dedicated virtual environment
#' 2. Each session: Call `init_bf()` at script start
#'
#' This function:
#' 1. Sets KERAS_BACKEND=torch via R environment variable (before Python init)
#' 2. Returns cached modules if already initialized and valid
#' 3. Activates the specified virtual environment
#' 4. Verifies Python is initialized and all dependencies are available
#' 5. Imports modules eagerly to surface errors immediately
#' 6. Caches modules for reuse
#'
#' @param envname Name of Python virtual environment to use.
#'   Default is "r-rctbayespower" (created by [setup_bf_python()]).
#'   Set to NULL to use the currently active Python environment.
#'   **Important**: Environment can only be set BEFORE Python is initialized
#'   in the R session. Restart R to switch environments.
#' @param verbose If TRUE (default), print initialization status message.
#'
#' @return List with bf, np, and keras Python modules (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # Recommended: Initialize at the start of your script
#' init_bf()  # Uses default "r-rctbayespower" environment
#'
#' # Or specify environment explicitly
#' init_bf("rctbp-3-12")
#'
#' # Then use BayesFlow backend in design
#' design <- build_design(
#'   predefined_model = "ancova_cont_2arms",
#'   backend = "bf",
#'   target_params = "b_arm2"
#' )
#' }
init_bf <- function(envname = "r-rctbayespower", verbose = TRUE) {

  # ==========================================================================
  # STEP 1: Set KERAS_BACKEND before ANY Python initialization
  # ==========================================================================
  if (Sys.getenv("KERAS_BACKEND") == "") {
    Sys.setenv(KERAS_BACKEND = "torch")
  }

  # ==========================================================================
  # STEP 2: Check cache validity FIRST
  # ==========================================================================
  if (is_bf_cache_valid()) {
    if (verbose) {
      info <- get_bf_env_info()
      device_str <- if (!is.null(info) && info$device == "GPU") {
        paste0("GPU (", info$device_name, ")")
      } else {
        "CPU"
      }
      cli::cli_alert_success("BayesFlow ready: {device_str}")
    }
    return(invisible(list(bf = .bf_cache$bf, np = .bf_cache$np, keras = .bf_cache$keras)))
  }

  # ==========================================================================
  # STEP 3: Environment activation (using shared helper)
  # ==========================================================================
  setup_bf_environment(envname, strict = TRUE)

  # ==========================================================================
  # STEP 4: Python initialization (using shared helper)
  # ==========================================================================
  check_bf_python_ready()

  # ==========================================================================
  # STEP 5: Validate all dependencies
  # ==========================================================================
  check_bf_dependencies(silent = FALSE)

  # ==========================================================================
  # STEP 6: Import modules EAGERLY (no delay_load)
  # ==========================================================================
  if (verbose) {
    cli::cli_alert_info("Initializing BayesFlow Python environment...")
  }

  # Import in correct order: numpy -> torch -> keras -> bayesflow
  .bf_cache$np <- tryCatch({
    reticulate::import("numpy", convert = FALSE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to import NumPy",
      "x" = conditionMessage(e),
      "i" = "Run {.code setup_bf_python()} to install dependencies"
    ))
  })

  # torch import (validates CUDA if available)
  torch <- tryCatch({
    reticulate::import("torch")
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to import PyTorch",
      "x" = conditionMessage(e),
      "i" = "Run {.code setup_bf_python()} to install dependencies"
    ))
  })
  # CUDA check is optional — degrade gracefully if torch.cuda is unavailable
  # (e.g., cuda submodule files evicted by OneDrive Files On-Demand)
  .bf_cache$has_cuda <- tryCatch(torch$cuda$is_available(), error = function(e) FALSE)

  .bf_cache$keras <- tryCatch({
    reticulate::import("keras")
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to import Keras",
      "x" = conditionMessage(e),
      "i" = "Ensure KERAS_BACKEND=torch is set before importing",
      "i" = "Run {.code setup_bf_python()} to install dependencies"
    ))
  })

  .bf_cache$bf <- tryCatch({
    reticulate::import("bayesflow")
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to import BayesFlow",
      "x" = conditionMessage(e),
      "i" = "Run {.code setup_bf_python()} to install dependencies"
    ))
  })

  .bf_cache$envname <- envname
  .bf_cache$initialized <- TRUE

  # ==========================================================================
  # STEP 7: Report status
  # ==========================================================================
  if (verbose) {
    info <- get_bf_env_info()
    if (!is.null(info)) {
      device_str <- if (info$device == "GPU") {
        paste0("GPU (", info$device_name, ")")
      } else {
        "CPU"
      }
      env_str <- info$envname %||% "system"
      bf_ver <- info$pkg_versions$bayesflow %||% "unknown"
      cli::cli_alert_success(
        "BayesFlow {.val {bf_ver}} initialized: {device_str}, env: {.val {env_str}}"
      )
    } else {
      cli::cli_alert_success("BayesFlow initialized")
    }
  }

  invisible(list(bf = .bf_cache$bf, np = .bf_cache$np, keras = .bf_cache$keras))
}


# =============================================================================
# PYTHON MODEL LOADING
# =============================================================================

#' Load BayesFlow Model via Python
#'
#' Loads a BayesFlow model from file. Supports multiple formats:
#' - `.keras` files: Loaded via keras.saving.load_model()
#' - Pickled workflow/approximator objects
#'
#' @param model_path Path to model file (.keras or .pkl)
#'
#' @return Python model object (Keras model, BayesFlow Approximator, or Workflow)
#' @keywords internal
load_bf_model_python <- function(model_path) {
  if (!file.exists(model_path)) {
    cli::cli_abort("Model file not found: {.path {model_path}}")
  }

  # Normalize path for Python (expand ~ and make absolute)
  # Python's keras.saving.load_model() doesn't expand ~ like R does
  model_path <- normalizePath(model_path, mustWork = TRUE)

  # Require BayesFlow to be initialized (user must call init_bf_python() first)
  py_mods <- require_bf_init()

  ext <- tools::file_ext(model_path)

  if (ext == "keras") {
    # Load Keras model using cached module
    # Use keras.saving.load_model() with compile=TRUE, safe_mode=FALSE
    # for BayesFlow 2.0+ compatibility
    model <- tryCatch({
      py_mods$keras$saving$load_model(
        model_path,
        compile = TRUE,
        safe_mode = FALSE
      )
    }, error = function(e) {
      cli::cli_abort(c(
        "Failed to load BayesFlow model: {.path {model_path}}",
        "x" = conditionMessage(e),
        "i" = "Ensure model was saved with BayesFlow 2.0+ and Keras 3"
      ))
    })
  } else if (ext %in% c("pkl", "pickle")) {
    # Load pickled BayesFlow object
    pickle <- reticulate::import("pickle")
    builtins <- reticulate::import_builtins()
    f <- builtins$open(model_path, "rb")
    model <- pickle$load(f)
    f$close()
  } else {
    cli::cli_abort(c(
      "Unknown model file extension: {.val {ext}}",
      "i" = "Supported formats: .keras, .pkl"
    ))
  }

  model
}


#' Detect Function Type (R vs Python)
#'
#' Determines whether a function object is an R function or Python callable.
#'
#' @param fn Function or Python callable to check
#'
#' @return Character: "r", "python", or "unknown"
#' @keywords internal
detect_function_type <- function(fn) {
  if (is.function(fn)) return("r")
  if (inherits(fn, c("python.builtin.function", "python.builtin.method"))) return("python")
  if ("__call__" %in% names(fn)) return("python")
  return("unknown")
}


# =============================================================================
# FAST POSTERIOR SUMMARIZATION
# =============================================================================

#' Compute Convergence Diagnostics for BayesFlow Samples
#'
#' Computes rhat, ess_bulk, and ess_tail for BayesFlow posterior samples.
#' For BayesFlow (IID samples), these metrics assess sample quality rather
#' than MCMC chain convergence. Split-rhat is used (splits samples in half).
#'
#' Uses vectorized computation: all simulations are processed as separate
#' "variables" in a single draws_array, allowing batch computation.
#'
#' @param draws_mat Matrix of posterior draws (n_sims x n_post_draws)
#'
#' @return List with rhat, ess_bulk, ess_tail vectors (length = n_sims)
#' @keywords internal
compute_convergence_bf <- function(draws_mat) {
  n_sims <- nrow(draws_mat)
  n_draws <- ncol(draws_mat)

  # Vectorized approach: treat each simulation as a separate "variable"

  # draws_array expects [iteration, chain, variable]
  # - iteration = n_draws (posterior samples)
  # - chain = 1 (BayesFlow produces single "chain" of IID samples)
  # - variable = n_sims (each simulation is a variable)
  draws_arr <- array(
    data = t(draws_mat),  # Transpose: n_draws x n_sims
    dim = c(n_draws, 1L, n_sims),
    dimnames = list(NULL, NULL, paste0("sim_", seq_len(n_sims)))
  )
  draws_obj <- posterior::as_draws_array(draws_arr)

  # Compute diagnostics for all simulations at once (returns named vector)
  # Note: split-rhat is used internally (splits chain in half)
  rhat_vec <- as.numeric(posterior::rhat(draws_obj))
  ess_bulk_vec <- as.numeric(posterior::ess_bulk(draws_obj))
  ess_tail_vec <- as.numeric(posterior::ess_tail(draws_obj))

  list(
    rhat = rhat_vec,
    ess_bulk = ess_bulk_vec,
    ess_tail = ess_tail_vec
  )
}



#' Summarize Posterior Draws - BayesFlow Backend
#'
#' Computes all summary statistics directly from draws matrix using vectorized
#' operations. This is the fast path - does NOT use compute_measures() or rvars.
#'
#' Consistent API with summarize_post_brms() - both produce identical output schema.
#'
#' @param draws_mat Matrix of posterior draws (n_sims x n_post_draws)
#' @param target_param Parameter name (single string)
#' @param thr_fx_eff Efficacy threshold (numeric)
#' @param thr_fx_fut Futility threshold (numeric)
#' @param thr_dec_eff Efficacy probability threshold
#' @param thr_dec_fut Futility probability threshold
#' @param id_iter Vector of iteration IDs (length = n_sims)
#' @param id_cond Vector of condition IDs (length = n_sims)
#' @param id_look Analysis look ID (integer)
#' @param n_analyzed Sample size at this analysis (integer)
#' @param skip_convergence If TRUE (default), skip computing rhat/ess_bulk/ess_tail
#'   for faster execution. These metrics are less informative for NPE since
#'   BayesFlow samples are IID by design. Set to FALSE to compute them.
#'
#' @return Data frame with package output schema (n_sims rows)
#' @keywords internal
summarize_post_bf <- function(draws_mat, target_param,
                               thr_fx_eff, thr_fx_fut, thr_dec_eff, thr_dec_fut,
                               id_iter, id_cond, id_look = 1L, n_analyzed,
                               skip_convergence = TRUE) {

  # Check matrixStats availability (for performance)
  if (!requireNamespace("matrixStats", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package 'matrixStats' is required for BayesFlow backend",
      "i" = "Install it with: install.packages('matrixStats')"
    ))
  }

  n_sims <- nrow(draws_mat)

  # FAST vectorized operations using matrixStats
  # Probability of exceeding efficacy threshold
  pr_eff <- rowMeans(draws_mat > thr_fx_eff)
  # Probability below futility threshold
  pr_fut <- rowMeans(draws_mat < thr_fx_fut)

  # Compute convergence diagnostics (optional - less informative for NPE)
  if (skip_convergence) {
    rhat <- rep(NA_real_, n_sims)
    ess_bulk <- rep(NA_real_, n_sims)
    ess_tail <- rep(NA_real_, n_sims)
  } else {
    convergence <- compute_convergence_bf(draws_mat)
    rhat <- convergence$rhat
    ess_bulk <- convergence$ess_bulk
    ess_tail <- convergence$ess_tail
  }

  data.frame(
    par_name = rep(target_param, n_sims),
    thr_fx_eff = rep(thr_fx_eff, n_sims),
    thr_fx_fut = rep(thr_fx_fut, n_sims),
    thr_dec_eff = rep(thr_dec_eff, n_sims),
    thr_dec_fut = rep(thr_dec_fut, n_sims),
    pr_eff = pr_eff,
    pr_fut = pr_fut,
    dec_eff = as.integer(pr_eff >= thr_dec_eff),
    dec_fut = as.integer(pr_fut >= thr_dec_fut),
    post_med = matrixStats::rowMedians(draws_mat),
    post_mad = matrixStats::rowMads(draws_mat),
    post_mn = rowMeans(draws_mat),
    post_sd = matrixStats::rowSds(draws_mat),
    rhat = rhat,
    ess_bulk = ess_bulk,
    ess_tail = ess_tail,
    id_iter = id_iter,
    id_cond = id_cond,
    id_look = rep(as.integer(id_look), n_sims),
    n_analyzed = rep(as.integer(n_analyzed), n_sims),
    stopped = rep(FALSE, n_sims),
    stop_reason = rep(NA_character_, n_sims),
    converged = rep(1L, n_sims),
    error_msg = rep(NA_character_, n_sims),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# SUMMARY STATISTICS COMPUTATION
# =============================================================================

#' Compute Summary Statistics for ANCOVA Batch (BayesFlow Input)
#'
#' Extracts summary statistics from batch simulation data in the format
#' expected by BayesFlow trained models. The summary statistics must match
#' exactly what the BayesFlow model was trained on.
#'
#' For 2-arm ANCOVA, summaries are:
#' 1. mean_outcome_ctrl, 2. mean_outcome_treat,
#' 3. sd_outcome_ctrl, 4. sd_outcome_treat,
#' 5. n_ctrl, 6. n_treat,
#' 7. cor_outcome_covariate, 8. mean_covariate
#'
#' @param data_batch List with batch-formatted data from prepare_*_batch_bf()
#'   containing: outcome (matrix), covariate (matrix), group (matrix)
#'
#' @return Matrix of summary statistics (batch_size x n_summaries)
#' @keywords internal
compute_summaries_batch_ancova <- function(data_batch) {
  outcome <- data_batch$outcome
  covariate <- data_batch$covariate
  group <- data_batch$group

  batch_size <- nrow(outcome)
  n_summaries <- 8L

  summaries <- matrix(NA_real_, nrow = batch_size, ncol = n_summaries)

  for (i in seq_len(batch_size)) {
    ctrl_mask <- group[i, ] == 0
    treat_mask <- group[i, ] == 1

    # Handle edge cases where one arm might be empty
    mean_ctrl <- if (any(ctrl_mask)) mean(outcome[i, ctrl_mask]) else NA_real_
    mean_treat <- if (any(treat_mask)) mean(outcome[i, treat_mask]) else NA_real_
    sd_ctrl <- if (sum(ctrl_mask) > 1) stats::sd(outcome[i, ctrl_mask]) else NA_real_
    sd_treat <- if (sum(treat_mask) > 1) stats::sd(outcome[i, treat_mask]) else NA_real_

    summaries[i, ] <- c(
      mean_ctrl,
      mean_treat,
      sd_ctrl,
      sd_treat,
      sum(ctrl_mask),
      sum(treat_mask),
      stats::cor(outcome[i, ], covariate[i, ]),
      mean(covariate[i, ])
    )
  }

  summaries
}


# =============================================================================
# DATA PREPARATION (Pre-allocated for O(n) memory complexity)
# =============================================================================

#' Get Batch Field Map for Model Type
#'
#' Returns the field mapping specification for a given model type. Each spec
#' defines how to extract and transform fields from simulation data into
#' batch format for BayesFlow inference.
#'
#' @param model_type Character string identifying the model type.
#'   Supported types: "ancova" (default), "binary", "survival".
#'
#' @return Named list where each element specifies:
#'   \itemize{
#'     \item source: Character vector of field names to try (in order)
#'     \item transform: Optional transformation name ("factor_to_numeric", "logical_to_int")
#'   }
#'
#' @details
#' Field maps define the schema for batch data preparation:
#' \itemize{
#'   \item \strong{ancova}: outcome, covariate, group (continuous outcomes)
#'   \item \strong{binary}: outcome, covariate, group (binary outcomes)
#'   \item \strong{survival}: time, event, covariate, group (time-to-event)
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Get field map for ANCOVA models
#' field_map <- get_batch_field_map("ancova")
#' names(field_map)  # "outcome", "covariate", "group"
#'
#' # Get field map for survival models (planned)
#' field_map <- get_batch_field_map("survival")
#' names(field_map)  # "time", "event", "covariate", "group"
#' }
get_batch_field_map <- function(model_type = "ancova") {
  maps <- list(
    # ANCOVA models (continuous outcome)
    ancova = list(
      outcome = list(source = "outcome"),
      covariate = list(source = "covariate"),
      group = list(source = c("group", "arm"), transform = "factor_to_numeric")
    ),

    # Binary outcome models (planned)
    binary = list(
      outcome = list(source = c("outcome", "response", "y")),
      covariate = list(source = "covariate"),
      group = list(source = c("group", "arm"), transform = "factor_to_numeric")
    ),

    # Survival models (planned) - time-to-event data
    survival = list(
      time = list(source = c("time", "survival_time", "t")),
      event = list(source = c("event", "status", "delta"), transform = "logical_to_int"),
      covariate = list(source = "covariate"),
      group = list(source = c("group", "arm"), transform = "factor_to_numeric")
    )
  )

  if (!model_type %in% names(maps)) {
    cli::cli_abort(c(
      "Unknown model type for batch field mapping: {.val {model_type}}",
      "i" = "Available types: {.val {names(maps)}}",
      "i" = "Use {.arg field_map} parameter to provide custom mapping"
    ))
  }

  maps[[model_type]]
}


#' Apply Field Transformation
#'
#' Applies a named transformation to a vector during batch preparation.
#'
#' @param val Vector to transform
#' @param transform_name Name of transformation: "factor_to_numeric", "logical_to_int"
#'
#' @return Transformed vector
#' @keywords internal
apply_batch_field_transform <- function(val, transform_name) {
  switch(transform_name,
    "factor_to_numeric" = {
      if (is.factor(val)) as.numeric(val) - 1L else val
    },
    "logical_to_int" = {
      as.integer(val)
    },
    val  # Default: return unchanged
 )
}


#' Prepare Single data.frame as Batch Format for BayesFlow
#'
#' Converts a single simulation's data to the batch format expected
#' by BayesFlow models. Handles both:
#' - R data.frame (rows=observations, cols=outcome/covariate/arm)
#' - Python batch dict (matrices with rows=simulations, cols=observations)
#'
#' @param data Single data.frame OR Python batch dict
#' @param backend_args List containing p_alloc and other settings
#' @param field_map Optional field mapping (see [get_batch_field_map()]).
#'   If NULL and sim_fn not provided, uses "ancova" mapping.
#' @param sim_fn Optional rctbp_sim_fn object. If provided and has output_schema,
#'   uses that instead of field_map.
#'
#' @return List with batch-formatted arrays (n_sims = 1)
#' @keywords internal
prepare_single_as_batch_bf <- function(data, backend_args, field_map = NULL, sim_fn = NULL) {
  # Resolve field_map from sim_fn@output_schema if available
  if (is.null(field_map) && !is.null(sim_fn) && inherits(sim_fn, "rctbp_sim_fn")) {
    field_map <- sim_fn@output_schema
  }

  # Fall back to default field map if still not provided
  if (is.null(field_map)) {
    field_map <- get_batch_field_map("ancova")
  }

  # Detect if this is Python batch format (dict with matrices)
  # Uses structural heuristics: contains matrices OR has "n_total" metadata key
  is_batch_dict <- is.list(data) && !is.data.frame(data) &&
    (any(vapply(data, is.matrix, logical(1))) || "n_total" %in% names(data))

  if (is_batch_dict) {
    # Python batch format - extract and ensure matrix format
    result <- lapply(names(field_map), function(fname) {
      spec <- field_map[[fname]]
      val <- NULL
      for (src in spec$source) {
        if (src %in% names(data)) {
          val <- data[[src]]
          break
        }
      }
      if (is.null(val)) return(matrix(NA_real_, nrow = 1, ncol = 1))
      if (is.matrix(val)) val else matrix(val, nrow = 1)
    })
    names(result) <- names(field_map)

    result$N <- data$N %||% ncol(result[[1]])
    result$p_alloc <- data$p_alloc %||% backend_args$p_alloc %||% 0.5
    return(result)
  }

  # R data.frame format (rows=observations)
  n_obs <- nrow(data)
  result <- lapply(names(field_map), function(fname) {
    spec <- field_map[[fname]]
    val <- NULL
    for (src in spec$source) {
      if (src %in% names(data)) {
        val <- data[[src]]
        break
      }
    }
    if (is.null(val)) return(matrix(NA_real_, nrow = 1, ncol = n_obs))

    # Apply transformation if specified
    if (!is.null(spec$transform)) {
      val <- apply_batch_field_transform(val, spec$transform)
    }
    matrix(val, nrow = 1)
  })
  names(result) <- names(field_map)

  result$N <- n_obs
  result$p_alloc <- backend_args$p_alloc %||% 0.5
  result
}


#' Prepare List of data.frames as Batch Format for BayesFlow
#'
#' Converts a list of simulation data to the batch format expected by BayesFlow
#' models. Uses pre-allocated matrices for O(n) memory complexity instead of
#' incremental rbind which has O(n²) complexity.
#'
#' @param data_list List of data.frames OR Python batch dicts
#' @param backend_args List containing p_alloc and other settings
#' @param field_map Optional field mapping specification (see [get_batch_field_map()]).
#'   If NULL and sim_fn not provided, uses "ancova" mapping.
#'   Can also be a model type string like "survival".
#' @param sim_fn Optional rctbp_sim_fn object. If provided and has output_schema,
#'   uses that instead of field_map. This is the preferred method.
#'
#' @return List with batch-formatted arrays:
#'   \itemize{
#'     \item Numeric matrices for each field (batch_size x n_obs)
#'     \item N: integer sample size per simulation
#'     \item p_alloc: allocation probability
#'   }
#'
#' @details
#' \strong{Memory efficiency}: This function pre-allocates all output matrices
#' before filling, achieving O(n) memory complexity. The previous implementation
#' using do.call(rbind, lapply(...)) had O(n²) complexity due to repeated
#' reallocation.
#'
#' \strong{Schema derivation}: When sim_fn is an rctbp_sim_fn object, the schema
#' is derived from its output_schema property. This provides automatic column
#' discovery and transformation specification without hardcoding.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Using sim_fn@output_schema (preferred)
#' batch <- prepare_data_list_as_batch_bf(data_list, backend_args, sim_fn = my_sim_fn)
#'
#' # ANCOVA data (default fallback)
#' batch <- prepare_data_list_as_batch_bf(data_list, backend_args)
#'
#' # Custom field mapping
#' custom_map <- list(
#'   y = list(source = "response"),
#'   x = list(source = "predictor"),
#'   trt = list(source = "treatment", transform = "factor_to_numeric")
#' )
#' batch <- prepare_data_list_as_batch_bf(data_list, backend_args, custom_map)
#' }
prepare_data_list_as_batch_bf <- function(data_list, backend_args, field_map = NULL, sim_fn = NULL) {
  batch_size <- length(data_list)
  if (batch_size == 0) {
    cli::cli_abort("data_list cannot be empty")
  }

  first <- data_list[[1]]

  # Resolve field_map from sim_fn@output_schema if available (preferred method)
  if (is.null(field_map) && !is.null(sim_fn) && inherits(sim_fn, "rctbp_sim_fn")) {
    field_map <- sim_fn@output_schema
  }

  # Fall back to registry or string lookup
  if (is.null(field_map)) {
    field_map <- get_batch_field_map("ancova")
  } else if (is.character(field_map) && length(field_map) == 1) {
    field_map <- get_batch_field_map(field_map)
  }
  # else: assume it's already a proper field_map list

  # Detect format: Python batch dict vs R data.frame
  # Uses structural heuristics: contains matrices OR has "n_total" metadata key
  is_batch_dict <- is.list(first) && !is.data.frame(first) &&
    (any(vapply(first, is.matrix, logical(1))) || "n_total" %in% names(first))

  # Determine observation count per simulation
  if (is_batch_dict) {
    # Find first available field to get dimensions
    for (fname in names(field_map)) {
      for (src in field_map[[fname]]$source) {
        if (src %in% names(first)) {
          ref_field <- first[[src]]
          n_obs <- if (is.matrix(ref_field)) ncol(ref_field) else length(ref_field)
          break
        }
      }
      if (exists("n_obs")) break
    }
    if (!exists("n_obs")) n_obs <- first$N %||% 1L
  } else {
    n_obs <- nrow(first)
  }

  # ===========================================================================
  # PRE-ALLOCATE OUTPUT MATRICES (Key optimization: O(n) vs O(n²))
  # ===========================================================================
  result <- setNames(
    lapply(names(field_map), function(fname) {
      matrix(NA_real_, nrow = batch_size, ncol = n_obs)
    }),
    names(field_map)
  )

  # ===========================================================================
  # FILL MATRICES WITH DIRECT ROW ASSIGNMENT (no reallocation)
  # ===========================================================================
  for (i in seq_len(batch_size)) {
    d <- data_list[[i]]

    for (fname in names(field_map)) {
      spec <- field_map[[fname]]

      # Find source field (try alternatives in order)
      val <- NULL
      for (src in spec$source) {
        if (src %in% names(d)) {
          val <- d[[src]]
          break
        }
      }

      if (is.null(val)) next

      # Handle batch dict format (may have matrix with 1 row)
      if (is_batch_dict && is.matrix(val)) {
        val <- val[1, ]
      }

      # Apply transformation if specified
      if (!is.null(spec$transform)) {
        val <- apply_batch_field_transform(val, spec$transform)
      }

      # Direct assignment to pre-allocated row (O(n_obs), not O(i * n_obs))
      result[[fname]][i, ] <- val
    }
  }

  # Add metadata
  result$N <- n_obs
  result$p_alloc <- first$p_alloc %||% backend_args$p_alloc %||% 0.5

  result
}


# =============================================================================
# BATCH ESTIMATION (CORE)
# =============================================================================

#' Detect BayesFlow Model Type
#'
#' Determines the type of BayesFlow model object for proper inference dispatch.
#'
#' @param bf_model Python model object
#'
#' @return Character: "workflow", "approximator", "keras", or "unknown"
#' @keywords internal
detect_bf_model_type <- function(bf_model) {
  # Check for BayesFlow BasicWorkflow
  if (reticulate::py_has_attr(bf_model, "sample") &&
      reticulate::py_has_attr(bf_model, "fit_online")) {
    return("workflow")
  }

  # Check for BayesFlow Approximator (has sample but not fit_online)
  if (reticulate::py_has_attr(bf_model, "sample") &&
      reticulate::py_has_attr(bf_model, "fit")) {
    return("approximator")
  }

  # Check for raw Keras model
  if (reticulate::py_has_attr(bf_model, "predict") &&
      reticulate::py_has_attr(bf_model, "compile")) {
    return("keras")
  }

  "unknown"
}


#' Sample from BayesFlow Model
#'
#' Dispatches to appropriate sampling method based on model type.
#' Handles BayesFlow Workflow, Approximator, and raw Keras models.
#'
#' @param bf_model Python BayesFlow model object
#' @param summaries_np NumPy array of summary statistics (batch_size x n_summaries)
#' @param n_samples Number of posterior samples to draw
#' @param target_param Name of target parameter (for workflow conditions key)
#'
#' @return NumPy array of posterior samples (batch_size x n_samples) or (batch_size x n_samples x n_params)
#' @keywords internal
sample_bf_model <- function(bf_model, summaries_np, n_samples, target_param = "b_arm_treat") {
  model_type <- detect_bf_model_type(bf_model)
  np <- reticulate::import("numpy")

  samples <- switch(model_type,
    workflow = {
      # BayesFlow BasicWorkflow: sample(conditions=dict, num_samples=int)
      conditions <- reticulate::dict(summaries = summaries_np)
      bf_model$sample(conditions = conditions, num_samples = as.integer(n_samples))
    },

    approximator = {
      # BayesFlow Approximator: sample(data=dict, num_samples=int)
      # The adapter determines which dict keys are expected
      data <- reticulate::dict(summary_variables = summaries_np)
      bf_model$sample(data = data, num_samples = as.integer(n_samples))
    },

    keras = {
      # Raw Keras model: predict() returns distribution parameters
      # We'll assume it's an NPE that outputs [batch, n_params * 2] for mean/std
      # Then sample from Gaussian
      output <- bf_model$predict(summaries_np)
      n_params <- as.integer(ncol(reticulate::py_to_r(output)) / 2L)

      # Extract mean and std (assuming first half is mean, second half is log_std)
      means <- output[, 1:n_params]
      log_stds <- output[, (n_params + 1):(2 * n_params)]
      stds <- np$exp(log_stds)

      # Sample from Gaussian: [batch, n_samples, n_params]
      batch_size <- nrow(reticulate::py_to_r(means))
      samples_shape <- c(as.integer(batch_size), as.integer(n_samples), as.integer(n_params))
      noise <- np$random$randn(samples_shape[[1]], samples_shape[[2]], samples_shape[[3]])

      # Broadcast: means[batch, 1, params] + stds[batch, 1, params] * noise[batch, samples, params]
      means_expanded <- np$expand_dims(means, 1L)
      stds_expanded <- np$expand_dims(stds, 1L)
      means_expanded + stds_expanded * noise
    },

    # Unknown model type
    {
      cli::cli_abort(c(
        "Unknown BayesFlow model type",
        "i" = "Model must be a BasicWorkflow, Approximator, or Keras model",
        "i" = "Model class: {class(bf_model)}"
      ))
    }
  )

  samples
}


#' Check if Mock Mode is Enabled
#'
#' Returns TRUE if BayesFlow mock mode is enabled via environment variable.
#' Used for testing R infrastructure without requiring Python/BayesFlow.
#'
#' @return Logical indicating if mock mode is enabled
#' @keywords internal
is_bf_mock_mode <- function() {
  Sys.getenv("RCTBP_MOCK_BF") == "TRUE"
}


#' Clear GPU Memory Cache
#'
#' Releases unused GPU memory held by PyTorch. Should be called after each
#' batch of BayesFlow inference to prevent memory accumulation during
#' power analysis with many simulations.
#'
#' This function:
#' 1. Runs Python garbage collection (multiple passes for cyclic references)
#' 2. Calls torch.cuda.empty_cache() if CUDA is available
#' 3. Synchronizes CUDA to ensure all operations complete
#' 4. Optionally runs R garbage collection
#'
#' @param r_gc Logical, whether to also run R garbage collection (default TRUE)
#'
#' @return Invisible NULL
#' @export
clear_gpu_memory <- function(r_gc = TRUE) {
  tryCatch({
    # Reuse cached Python modules if available to avoid repeated imports
    # which can affect reticulate state
    if (is.null(.bf_cache$gc_mod)) {
      .bf_cache$gc_mod <- reticulate::import("gc", delay_load = FALSE)
    }
    if (is.null(.bf_cache$torch)) {
      .bf_cache$torch <- reticulate::import("torch", delay_load = FALSE)
    }

    # Run Python garbage collection multiple times
    # (cyclic references may need multiple passes)
    .bf_cache$gc_mod$collect()
    .bf_cache$gc_mod$collect()  # Second pass for cyclic refs

    # Clear CUDA cache if available
    if (.bf_cache$torch$cuda$is_available()) {
      # Synchronize first to ensure all async ops complete
      .bf_cache$torch$cuda$synchronize()
      # Empty the cache
      .bf_cache$torch$cuda$empty_cache()
      # Synchronize again
      .bf_cache$torch$cuda$synchronize()
    }
  }, error = function(e) {
    # Silently ignore - memory cleanup is best-effort
    # Clear cached modules on error so they get reimported next time
    .bf_cache$gc_mod <- NULL
    .bf_cache$torch <- NULL
  })

  # Run R garbage collection to release reticulate references
  # Use regular gc (not full) to avoid potentially invalidating cached Python objects
  if (r_gc) {
    gc(verbose = FALSE, full = FALSE)
  }

  invisible(NULL)
}


#' Generate Mock BayesFlow Samples
#'
#' Generates fake posterior samples for testing purposes.
#' Samples are drawn from a normal distribution centered on a
#' reasonable treatment effect estimate.
#'
#' @param batch_size Number of simulations
#' @param n_samples Number of posterior samples per simulation
#' @param data_batch Optional data batch for data-driven mock
#'
#' @return Matrix (batch_size x n_samples) of mock posterior draws
#' @keywords internal
mock_bf_samples <- function(batch_size, n_samples, data_batch = NULL) {
  # Generate mock samples centered around 0.3 (typical effect size)
  # with some variation based on batch position
  samples <- matrix(
    stats::rnorm(batch_size * n_samples, mean = 0.3, sd = 0.15),
    nrow = batch_size,
    ncol = n_samples
  )

  # If data_batch provided, use summary stats to inform mock
  if (!is.null(data_batch) && all(c("outcome", "group") %in% names(data_batch))) {
    # Compute crude treatment effect estimates from data
    summaries <- compute_summaries_batch_ancova(data_batch)
    # Column 1 = mean_ctrl, Column 2 = mean_treat
    crude_effects <- summaries[, 2] - summaries[, 1]

    # Center mock samples around crude estimates
    for (i in seq_len(batch_size)) {
      samples[i, ] <- stats::rnorm(n_samples, mean = crude_effects[i], sd = 0.15)
    }
  }

  samples
}


#' Estimate Posterior Using BayesFlow (Batch)
#'
#' Processes a batch of simulations through BayesFlow in a single forward pass.
#' This is the core function for neural posterior estimation.
#'
#' The model's adapter handles summary statistic computation internally.
#' We pass raw data (outcome, covariate, group, N, p_alloc) to the model.
#'
#' Mock Mode: Set environment variable RCTBP_MOCK_BF=TRUE to use mock samples
#' instead of actual BayesFlow inference. Useful for testing R infrastructure.
#'
#' @param data_batch List with batched simulation data from prepare_*_bf().
#'   Must contain: outcome (matrix), covariate (matrix), group (matrix), N (int)
#' @param bf_model BayesFlow/Keras model (Python object via reticulate)
#' @param backend_args List with n_posterior_samples (default 1000).
#' @param target_params Character vector of parameter names (first used for single-param models)
#'
#' @return Matrix of posterior draws (batch_size x n_posterior_samples).
#'   For multi-parameter models, only the first target parameter is returned.
#'
#' @details
#' **GPU Memory Management**: Batch size is controlled at the `power_analysis()` level
#' via `bf_args$batch_size`. This determines how many simulations are grouped together
#' before being sent to the GPU. Reduce `batch_size` if encountering OOM errors.
#'
#' @keywords internal
estimate_batch_bf <- function(data_batch, bf_model, backend_args, target_params) {
  # Extract settings
  n_samples <- backend_args$n_posterior_samples %||% 1000L
  n_samples <- as.integer(n_samples)
  batch_size <- nrow(data_batch$outcome)

  # Mock mode for testing without Python
  if (is_bf_mock_mode()) {
    cli::cli_alert_info("Using BayesFlow mock mode (RCTBP_MOCK_BF=TRUE)")
    return(mock_bf_samples(batch_size, n_samples, data_batch))
  }

  # Validate model before proceeding
  if (is.null(bf_model)) {
    cli::cli_abort(c(
      "BayesFlow model is NULL",
      "i" = "The model may not have been loaded correctly",
      "i" = "Check that the model file exists and is a valid BayesFlow model"
    ))
  }

  # Check if model is a valid Python object
  if (!inherits(bf_model, "python.builtin.object")) {
    cli::cli_abort(c(
      "BayesFlow model is not a valid Python object",
      "x" = "Model class: {.cls {class(bf_model)}}",
      "i" = "The model may have been corrupted during serialization",
      "i" = "BayesFlow backend does not support parallel execution"
    ))
  }

  # Require BayesFlow to be initialized
  py_mods <- require_bf_init()
  np <- py_mods$np

  # Convert R matrices to NumPy arrays and create conditions dict
  data_cond <- reticulate::r_to_py(list(
    outcome = np$array(data_batch$outcome, dtype = "float32"),
    covariate = np$array(data_batch$covariate, dtype = "float32"),
    group = np$array(data_batch$group, dtype = "float32"),
    N = as.integer(data_batch$N),
    p_alloc = as.numeric(data_batch$p_alloc)
  ))

  # Call BayesFlow model for posterior samples
  samples_py <- tryCatch({
    bf_model$sample(conditions = data_cond, num_samples = n_samples)
  }, error = function(e) {
    # Clean up input tensors on error
    rm(data_cond)
    clear_gpu_memory(r_gc = FALSE)

    err_msg <- e$message
    # Check for CUDA out of memory error
    if (grepl("OutOfMemory|out of memory|CUDA", err_msg, ignore.case = TRUE)) {
      cli::cli_abort(c(
        "BayesFlow sampling failed: GPU out of memory",
        "x" = err_msg,
        "i" = "Try reducing batch_size: {.code bf_args = list(batch_size = 64)}",
        "i" = "Set memory config: {.code Sys.setenv(PYTORCH_CUDA_ALLOC_CONF = \"expandable_segments:True\")}",
        "i" = "Current batch size: {batch_size}, posterior samples: {n_samples}"
      ))
    }
    cli::cli_abort(c(
      "BayesFlow sampling failed",
      "x" = err_msg,
      "i" = "Check that the model is compatible with the data format"
    ))
  })

  # Convert back to R matrix BEFORE deleting Python objects
  samples_r <- reticulate::py_to_r(samples_py)

  # Explicitly remove Python object references to allow garbage collection
  # This is critical for releasing GPU memory held by tensors
  rm(samples_py, data_cond)

  # Clear GPU memory (includes Python gc + CUDA cache + R gc)
  clear_gpu_memory(r_gc = TRUE)

  # Handle different output shapes
  # Expected: dict with parameter names as keys (e.g., "b_group")
  # Each value has shape [batch_size, n_samples, 1]
  if (is.list(samples_r) && !is.matrix(samples_r)) {
    # Dict output - extract target parameter
    if (target_params[1] %in% names(samples_r)) {
      samples_r <- samples_r[[target_params[1]]]
    } else if (length(samples_r) > 0) {
      # Use first element
      samples_r <- samples_r[[1]]
    }
  }

  # Ensure 2D matrix [batch_size, n_samples]
  if (length(dim(samples_r)) == 3) {
    # [batch, samples, params] -> squeeze last dim -> [batch, samples]
    samples_r <- samples_r[, , 1, drop = TRUE]
  }

  # Ensure proper dimensions
  if (length(dim(samples_r)) == 1 || is.null(dim(samples_r))) {
    # Single simulation - reshape to [1, n_samples]
    samples_r <- matrix(samples_r, nrow = 1)
  }

  # Verify dimensions
  if (nrow(samples_r) != batch_size) {
    cli::cli_warn(c(
      "BayesFlow output dimension mismatch",
      "Expected {batch_size} rows, got {nrow(samples_r)}",
      "i" = "Attempting to transpose"
    ))
    samples_r <- t(samples_r)
  }

  samples_r
}


# =============================================================================
# SINGLE ANALYSIS ESTIMATION
# =============================================================================

#' Estimate Single Analysis with BayesFlow Backend
#'
#' Performs single posterior estimation with no interim analyses using BayesFlow.
#' Supports both single data.frame (batch of 1) and list of data.frames (true batch).
#'
#' @param data Data frame with simulated observations OR list of data frames for batch
#' @param model BayesFlow/Keras model (Python object)
#' @param backend_args List of BayesFlow-specific arguments
#' @param target_params Character vector of parameter names
#' @param thr_fx_eff Numeric vector of efficacy thresholds (ROPE)
#' @param thr_fx_fut Numeric vector of futility thresholds (ROPE)
#' @param thr_dec_eff Probability threshold for efficacy
#' @param thr_dec_fut Probability threshold for futility
#' @param id_iter Iteration identifier (scalar or vector for batch)
#' @param id_cond Condition identifier (scalar or vector for batch)
#' @param sim_fn Optional rctbp_sim_fn object for schema-based batch preparation.
#'   If provided, uses sim_fn@@output_schema for field mapping.
#'
#' @return Data frame with results (1 row per simulation in batch)
#' @keywords internal
estimate_single_bf <- function(data, model, backend_args, target_params,
                                thr_fx_eff, thr_fx_fut,
                                thr_dec_eff, thr_dec_fut, id_iter, id_cond,
                                sim_fn = NULL) {

  # Detect data format:
  # 1. R data.frame (single sim, rows=observations)
  # 2. Python batch dict (single sim, has 'outcome' key with matrix)
  # 3. List of R data.frames (multiple sims)
  # 4. List of Python batch dicts (multiple sims)

  is_python_batch_dict <- function(x) {
    is.list(x) && !is.data.frame(x) && "outcome" %in% names(x)
  }

  if (is.data.frame(data)) {
    # Case 1: Single R data.frame
    batch_size <- 1L
    n_analyzed <- nrow(data)
    data_batch <- prepare_single_as_batch_bf(data, backend_args, sim_fn = sim_fn)

  } else if (is_python_batch_dict(data)) {
    # Case 2: Single Python batch dict
    batch_size <- nrow(data$outcome)
    n_analyzed <- ncol(data$outcome)
    data_batch <- prepare_single_as_batch_bf(data, backend_args, sim_fn = sim_fn)

    # Expand id vectors if needed (Python batch may have multiple rows)
    if (length(id_iter) == 1 && batch_size > 1) id_iter <- rep(id_iter, batch_size)
    if (length(id_cond) == 1 && batch_size > 1) id_cond <- rep(id_cond, batch_size)

  } else if (is.list(data) && length(data) > 0) {
    # Case 3 or 4: List of simulations
    first <- data[[1]]
    batch_size <- length(data)

    if (is.data.frame(first)) {
      # Case 3: List of R data.frames
      n_analyzed <- nrow(first)
    } else if (is_python_batch_dict(first)) {
      # Case 4: List of Python batch dicts (each with 1 row)
      n_analyzed <- ncol(first$outcome)
    } else {
      cli::cli_abort("Unknown data format in estimate_single_bf")
    }

    # Ensure id vectors are correct length
    if (length(id_iter) == 1) id_iter <- rep(id_iter, batch_size)
    if (length(id_cond) == 1) id_cond <- rep(id_cond, batch_size)

    # Prepare batch data
    data_batch <- prepare_data_list_as_batch_bf(data, backend_args, sim_fn = sim_fn)

  } else {
    cli::cli_abort("Unknown data format in estimate_single_bf")
  }

  # Estimate posterior via BayesFlow
  bf_error_msg <- NULL
  draws_matrix <- tryCatch({
    estimate_batch_bf(data_batch, model, backend_args, target_params)
  }, error = function(e) {
    # Capture the actual error message for debugging
    bf_error_msg <<- conditionMessage(e)
    return(NULL)
  })

  if (is.null(draws_matrix)) {
    # Create error results for all simulations in batch with actual error message
    error_msg <- paste("BayesFlow estimation failed:", bf_error_msg %||% "unknown error")
    error_results <- lapply(seq_len(batch_size), function(i) {
      create_error_result(
        id_iter[i],
        id_cond[i],
        id_analysis = 0L,
        error_msg
      )
    })
    return(dplyr::bind_rows(error_results))
  }

  # Resolve probability thresholds (info_frac = 1 for single-look designs)
  current_thr_dec_eff <- resolve_threshold(thr_dec_eff, 1)
  current_thr_dec_fut <- resolve_threshold(thr_dec_fut, 1)

  # Fast direct summarization (NOT through compute_measures)
  result <- summarize_post_bf(
    draws_mat = draws_matrix,
    target_param = target_params[1],  # Single param for now
    thr_fx_eff = thr_fx_eff[1],
    thr_fx_fut = thr_fx_fut[1],
    thr_dec_eff = current_thr_dec_eff,
    thr_dec_fut = current_thr_dec_fut,
    id_iter = id_iter,
    id_cond = id_cond,
    id_look = 0L,
    n_analyzed = n_analyzed,
    skip_convergence = backend_args$skip_convergence %||% TRUE
  )

  # Clean up large objects to free RAM
  rm(draws_matrix, data_batch)

  result
}


# =============================================================================
# SEQUENTIAL ANALYSIS ESTIMATION
# =============================================================================

#' Estimate Sequential Analysis with BayesFlow Backend
#'
#' Performs sequential interim analyses with BayesFlow backend using batch processing.
#' All simulations at the same interim step are processed together in a single
#' BayesFlow forward pass for efficiency.
#'
#' @param full_data_list List of complete datasets (one per simulation in batch)
#' @param model BayesFlow/Keras model (Python object)
#' @param backend_args List of BayesFlow-specific arguments
#' @param target_params Character vector of parameter names
#' @param thr_fx_eff Numeric vector of efficacy thresholds (ROPE)
#' @param thr_fx_fut Numeric vector of futility thresholds (ROPE)
#' @param thr_dec_eff Probability threshold for efficacy (numeric or pre-resolved vector)
#' @param thr_dec_fut Probability threshold for futility (numeric or pre-resolved vector)
#' @param analysis_at Vector of sample sizes for all analyses (including final)
#' @param interim_function Function to make interim decisions (optional)
#' @param id_iter Vector of iteration identifiers (one per sim in batch)
#' @param id_cond Vector of condition identifiers (one per sim in batch)
#' @param sim_fn Optional rctbp_sim_fn object for schema-based batch preparation.
#'   If provided, uses sim_fn@@output_schema for field mapping.
#'
#' @return Data frame with (batch_size x n_analyses) rows
#' @keywords internal
estimate_sequential_bf <- function(full_data_list, model, backend_args, target_params,
                                    thr_fx_eff, thr_fx_fut,
                                    thr_dec_eff, thr_dec_fut, analysis_at,
                                    interim_function, id_iter, id_cond,
                                    sim_fn = NULL) {

  batch_size <- length(full_data_list)
  n_total <- nrow(full_data_list[[1]])
  n_analyses <- length(analysis_at)

  # Initialize tracking for each sim
  stopped <- rep(FALSE, batch_size)
  stop_reason <- rep(NA_character_, batch_size)
  all_results <- vector("list", n_analyses)

  for (id_analysis in seq_along(analysis_at)) {
    current_n <- analysis_at[id_analysis]
    is_final <- id_analysis == n_analyses
    info_frac <- current_n / n_total

    # Resolve probability thresholds for this analysis
    current_thr_dec_eff <- resolve_threshold(thr_dec_eff, info_frac)
    current_thr_dec_fut <- resolve_threshold(thr_dec_fut, info_frac)

    # Identify active (non-stopped) simulations
    active_idx <- which(!stopped)
    if (length(active_idx) == 0 && !is_final) next

    # If all simulations have stopped but we're at final, still analyze for reporting
    if (length(active_idx) == 0) active_idx <- seq_len(batch_size)

    # Subset data to current analysis point for active simulations
    analysis_data_list <- lapply(full_data_list[active_idx], function(fd) fd[1:current_n, ])

    # Prepare batch data
    data_batch <- prepare_data_list_as_batch_bf(analysis_data_list, backend_args, sim_fn = sim_fn)

    # Single BayesFlow forward pass for all active simulations
    bf_error_msg <- NULL
    draws_matrix <- tryCatch({
      estimate_batch_bf(data_batch, model, backend_args, target_params)
    }, error = function(e) {
      # Capture the actual error message for debugging
      bf_error_msg <<- conditionMessage(e)
      return(NULL)
    })

    if (is.null(draws_matrix)) {
      # Create error results for all active simulations with actual error message
      error_msg <- paste("BayesFlow estimation failed:", bf_error_msg %||% "unknown error")
      error_results <- do.call(rbind, lapply(active_idx, function(i) {
        create_error_result(id_iter[i], id_cond[i], id_analysis, error_msg)
      }))
      all_results[[id_analysis]] <- error_results
      next
    }

    # Fast batch summarization
    batch_results <- summarize_post_bf(
      draws_mat = draws_matrix,
      target_param = target_params[1],
      thr_fx_eff = thr_fx_eff[1],
      thr_fx_fut = thr_fx_fut[1],
      thr_dec_eff = current_thr_dec_eff,
      thr_dec_fut = current_thr_dec_fut,
      id_iter = id_iter[active_idx],
      id_cond = id_cond[active_idx],
      id_look = id_analysis,
      n_analyzed = current_n,
      skip_convergence = backend_args$skip_convergence %||% TRUE
    )

    # Clean up large objects to free RAM after each interim analysis
    rm(draws_matrix, data_batch, analysis_data_list)

    # Update stopping state (if not final)
    if (!is_final) {
      # Find newly stopped simulations
      new_stops_eff <- which(batch_results$dec_eff == 1)
      new_stops_fut <- which(batch_results$dec_fut == 1 & batch_results$dec_eff == 0)

      if (length(new_stops_eff) > 0) {
        stopped[active_idx[new_stops_eff]] <- TRUE
        stop_reason[active_idx[new_stops_eff]] <- "stop_efficacy"
      }
      if (length(new_stops_fut) > 0) {
        stopped[active_idx[new_stops_fut]] <- TRUE
        stop_reason[active_idx[new_stops_fut]] <- "stop_futility"
      }

      # Update batch_results with stopping information
      batch_results$stopped <- stopped[active_idx]
      batch_results$stop_reason <- stop_reason[active_idx]
    }

    all_results[[id_analysis]] <- batch_results
  }

  dplyr::bind_rows(all_results)
}
