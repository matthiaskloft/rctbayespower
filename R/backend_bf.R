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
    python_path = NULL
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

  # Get CPU info via Python platform module
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
  }, error = function(e) {
    # Platform module not available - leave cpu_name as NULL

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


#' Initialize BayesFlow Python Environment
#'
#' Imports BayesFlow, Keras, and NumPy modules, caching them for reuse.
#' Called automatically by estimation functions.
#'
#' This function:
#' 1. Activates the specified virtual environment if provided
#' 2. Returns cached modules if already initialized (and envname matches)
#' 3. Declares Python dependencies via py_require() for automatic venv provisioning
#' 4. Sets KERAS_BACKEND=torch before importing keras (required for PyTorch backend)
#' 5. Imports modules with delay_load for CRAN compatibility
#'
#' @param envname Optional name of Python virtual environment to use.
#'   If NULL (default), uses the currently active environment or auto-detects.
#'
#' @return List with bf, np, and keras Python modules
#' @export
#'
#' @examples
#' \dontrun{
#' py_mods <- init_bf_python()
#' py_mods$bf  # BayesFlow module
#' py_mods$keras  # Keras module
#'
#' # Use specific environment
#' py_mods <- init_bf_python(envname = "r-rctbayespower")
#' }
init_bf_python <- function(envname = NULL) {
  # Check if Python is already initialized
 python_already_initialized <- reticulate::py_available(initialize = FALSE)

  # If envname specified and Python already initialized, check compatibility
  if (!is.null(envname) && nchar(envname) > 0 && python_already_initialized) {
    # Get current Python path
    current_config <- reticulate::py_config()
    current_path <- normalizePath(current_config$python, winslash = "/", mustWork = FALSE)

    # Get requested environment path
    requested_path <- tryCatch({
      venv_python <- reticulate::virtualenv_python(envname)
      normalizePath(venv_python, winslash = "/", mustWork = FALSE)
    }, error = function(e) NULL)

    if (!is.null(requested_path) && !identical(current_path, requested_path)) {
      # Extract current env name from path for clearer message
      current_envname <- basename(dirname(dirname(current_path)))

      cli::cli_warn(c(
        "Cannot switch Python environment in active R session",
        "x" = "Requested: {.val {envname}}",
        "!" = "Currently using: {.val {current_envname}}",
        "i" = "Restart R session to use a different Python environment",
        "i" = "Continuing with current environment..."
      ))
      # Don't try to switch - use current environment
      envname <- NULL
    }
  }

  # Activate specified environment if provided (only if Python not yet initialized)
  if (!is.null(envname) && nchar(envname) > 0 && !python_already_initialized) {
    tryCatch({
      reticulate::use_virtualenv(envname, required = TRUE)
    }, error = function(e) {
      cli::cli_warn(c(
        "Could not activate virtual environment {.val {envname}}",
        "i" = "Using current Python environment instead",
        "x" = conditionMessage(e)
      ))
    })
  }

  # Check if cache is valid (same environment)
  cached_envname <- .bf_cache$envname %||% ""
  current_envname <- envname %||% ""

  if (exists("bf", envir = .bf_cache) &&
      exists("np", envir = .bf_cache) &&
      exists("keras", envir = .bf_cache) &&
      !is.null(.bf_cache$bf) &&
      cached_envname == current_envname) {
    return(list(bf = .bf_cache$bf, np = .bf_cache$np, keras = .bf_cache$keras))
  }

  # Ensure availability
  check_bf_available(silent = FALSE)

  # Declare Python dependencies for automatic provisioning

  # py_require() triggers automatic virtual environment setup if needed
  tryCatch({
    reticulate::py_require(
      c("bayesflow>=2.0", "keras>=3.0", "torch", "numpy"),
      python_version = "3.12"
    )
  }, error = function(e) {
    cli::cli_warn(c(
      "Could not configure Python environment automatically",
      "i" = "You may need to install packages manually:",
      " " = "pip install bayesflow keras torch numpy"
    ))
  })

  # Set KERAS_BACKEND before importing keras (critical for PyTorch backend)
  # Only set if not already configured by user
  if (Sys.getenv("KERAS_BACKEND") == "") {
    os <- reticulate::import("os")
    os$environ$`__setitem__`("KERAS_BACKEND", "torch")
    cli::cli_alert_info("Set KERAS_BACKEND=torch for BayesFlow")
  }

  # Import modules with delay_load for CRAN compatibility
  .bf_cache$bf <- reticulate::import("bayesflow", delay_load = TRUE)
  .bf_cache$np <- reticulate::import("numpy", convert = FALSE, delay_load = TRUE)
  .bf_cache$keras <- reticulate::import("keras", delay_load = TRUE)
  .bf_cache$envname <- envname  # Store envname for cache validation

  list(bf = .bf_cache$bf, np = .bf_cache$np, keras = .bf_cache$keras)
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

  # Initialize Python environment (includes cached keras)
  py_mods <- init_bf_python()

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
#' @param thr_scs Success threshold (numeric)
#' @param thr_ftl Futility threshold (numeric)
#' @param p_sig_scs Success probability threshold
#' @param p_sig_ftl Futility probability threshold
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
                               thr_scs, thr_ftl, p_sig_scs, p_sig_ftl,
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
  # Probability of exceeding success threshold
  pr_scs <- rowMeans(draws_mat > thr_scs)
  # Probability below futility threshold
  pr_ftl <- rowMeans(draws_mat < thr_ftl)

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
    thr_scs = rep(thr_scs, n_sims),
    thr_ftl = rep(thr_ftl, n_sims),
    p_sig_scs = rep(p_sig_scs, n_sims),
    p_sig_ftl = rep(p_sig_ftl, n_sims),
    pr_scs = pr_scs,
    pr_ftl = pr_ftl,
    dec_scs = as.integer(pr_scs >= p_sig_scs),
    dec_ftl = as.integer(pr_ftl >= p_sig_ftl),
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
# DATA PREPARATION
# =============================================================================

#' Prepare Single data.frame as Batch Format for BayesFlow
#'
#' Converts a single simulation's data to the batch format expected
#' by BayesFlow models. Handles both:
#' - R data.frame (rows=observations, cols=outcome/covariate/arm)
#' - Python batch dict (matrices with rows=simulations, cols=observations)
#'
#' @param data Single data.frame OR Python batch dict
#' @param backend_args List containing p_alloc and other settings
#'
#' @return List with batch-formatted arrays (n_sims = 1)
#' @keywords internal
prepare_single_as_batch_bf <- function(data, backend_args) {
  # Detect if this is Python batch format (dict with matrices)
  # Python sim_fn returns: list(outcome=matrix, covariate=matrix, group=matrix, N=int)
  if (is.list(data) && !is.data.frame(data) && "outcome" %in% names(data)) {
    # Already in batch format from Python sim_fn
    # Just ensure group key exists (Python uses 'group', some may use 'arm')
    group_data <- data$group %||% data$arm
    return(list(
      outcome = if (is.matrix(data$outcome)) data$outcome else matrix(data$outcome, nrow = 1),
      covariate = if (is.matrix(data$covariate)) data$covariate else matrix(data$covariate, nrow = 1),
      group = if (is.matrix(group_data)) group_data else matrix(group_data, nrow = 1),
      N = data$N %||% ncol(data$outcome),
      p_alloc = data$p_alloc %||% backend_args$p_alloc %||% 0.5
    ))
  }

  # R data.frame format (rows=observations)
  # Need to convert arm factor to numeric if present
  arm_numeric <- if (is.factor(data$arm)) as.numeric(data$arm) - 1 else data$arm
  list(
    outcome = matrix(data$outcome, nrow = 1),
    covariate = matrix(data$covariate, nrow = 1),
    group = matrix(arm_numeric, nrow = 1),
    N = nrow(data),
    p_alloc = backend_args$p_alloc %||% 0.5
  )
}


#' Prepare List of data.frames as Batch Format for BayesFlow
#'
#' Converts a list of simulation data to the batch format expected
#' by BayesFlow models. Handles both:
#' - R data.frames (rows=observations)
#' - Python batch dicts (matrices with rows=simulations)
#'
#' @param data_list List of data.frames OR Python batch dicts
#' @param backend_args List containing p_alloc and other settings
#'
#' @return List with batch-formatted arrays
#' @keywords internal
prepare_data_list_as_batch_bf <- function(data_list, backend_args) {
  # Check first element to detect format
  first <- data_list[[1]]

  if (is.list(first) && !is.data.frame(first) && "outcome" %in% names(first)) {
    # Python batch format - each element is already a batch dict with matrices
    # Stack the matrices vertically
    outcome_list <- lapply(data_list, function(d) {
      if (is.matrix(d$outcome)) d$outcome else matrix(d$outcome, nrow = 1)
    })
    covariate_list <- lapply(data_list, function(d) {
      if (is.matrix(d$covariate)) d$covariate else matrix(d$covariate, nrow = 1)
    })
    group_list <- lapply(data_list, function(d) {
      g <- d$group %||% d$arm
      if (is.matrix(g)) g else matrix(g, nrow = 1)
    })

    list(
      outcome = do.call(rbind, outcome_list),
      covariate = do.call(rbind, covariate_list),
      group = do.call(rbind, group_list),
      N = first$N %||% ncol(first$outcome),
      p_alloc = first$p_alloc %||% backend_args$p_alloc %||% 0.5
    )
  } else {
    # R data.frame format
    list(
      outcome = do.call(rbind, lapply(data_list, function(d) d$outcome)),
      covariate = do.call(rbind, lapply(data_list, function(d) d$covariate)),
      group = do.call(rbind, lapply(data_list, function(d) {
        if (is.factor(d$arm)) as.numeric(d$arm) - 1 else d$arm
      })),
      N = nrow(data_list[[1]]),
      p_alloc = backend_args$p_alloc %||% 0.5
    )
  }
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
#' @param backend_args List with n_posterior_samples (default 1000), etc.
#' @param target_params Character vector of parameter names (first used for single-param models)
#'
#' @return Matrix of posterior draws (batch_size x n_posterior_samples).
#'   For multi-parameter models, only the first target parameter is returned.
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

  # Initialize Python
  py_mods <- init_bf_python()
  np <- py_mods$np

  # Step 1: Convert R matrices to NumPy arrays and create conditions dict in Python
  # Note: Creating dict in R with reticulate::dict() and numpy arrays causes issues
  # So we construct the dict directly in Python using reticulate::r_to_py()
  data_cond <- reticulate::r_to_py(list(
    outcome = np$array(data_batch$outcome, dtype = "float32"),
    covariate = np$array(data_batch$covariate, dtype = "float32"),
    group = np$array(data_batch$group, dtype = "float32"),
    N = as.integer(data_batch$N),
    p_alloc = as.numeric(data_batch$p_alloc)
  ))

  # Step 2: Call BayesFlow model for posterior samples
  samples_py <- tryCatch({
    bf_model$sample(conditions = data_cond, num_samples = n_samples)
  }, error = function(e) {
    cli::cli_abort(c(
      "BayesFlow sampling failed",
      "x" = e$message,
      "i" = "Check that the model is compatible with the data format"
    ))
  })

  # Step 3: Convert back to R matrix
  samples_r <- reticulate::py_to_r(samples_py)

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
#' @param thresholds_success Numeric vector of success thresholds
#' @param thresholds_futility Numeric vector of futility thresholds
#' @param p_sig_scs Probability threshold for success
#' @param p_sig_ftl Probability threshold for futility
#' @param id_iter Iteration identifier (scalar or vector for batch)
#' @param id_cond Condition identifier (scalar or vector for batch)
#'
#' @return Data frame with results (1 row per simulation in batch)
#' @keywords internal
estimate_single_bf <- function(data, model, backend_args, target_params,
                                thresholds_success, thresholds_futility,
                                p_sig_scs, p_sig_ftl, id_iter, id_cond) {

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
    data_batch <- prepare_single_as_batch_bf(data, backend_args)

  } else if (is_python_batch_dict(data)) {
    # Case 2: Single Python batch dict
    batch_size <- nrow(data$outcome)
    n_analyzed <- ncol(data$outcome)
    data_batch <- prepare_single_as_batch_bf(data, backend_args)

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
    data_batch <- prepare_data_list_as_batch_bf(data, backend_args)

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
  current_p_sig_scs <- resolve_threshold(p_sig_scs, 1)
  current_p_sig_ftl <- resolve_threshold(p_sig_ftl, 1)

  # Fast direct summarization (NOT through compute_measures)
  result <- summarize_post_bf(
    draws_mat = draws_matrix,
    target_param = target_params[1],  # Single param for now
    thr_scs = thresholds_success[1],
    thr_ftl = thresholds_futility[1],
    p_sig_scs = current_p_sig_scs,
    p_sig_ftl = current_p_sig_ftl,
    id_iter = id_iter,
    id_cond = id_cond,
    id_look = 0L,
    n_analyzed = n_analyzed,
    skip_convergence = backend_args$skip_convergence %||% TRUE
  )

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
#' @param thresholds_success Numeric vector of success thresholds
#' @param thresholds_futility Numeric vector of futility thresholds
#' @param p_sig_scs Probability threshold for success
#' @param p_sig_ftl Probability threshold for futility
#' @param analysis_at Vector of sample sizes for all analyses (including final)
#' @param interim_function Function to make interim decisions (optional)
#' @param id_iter Vector of iteration identifiers (one per sim in batch)
#' @param id_cond Vector of condition identifiers (one per sim in batch)
#'
#' @return Data frame with (batch_size x n_analyses) rows
#' @keywords internal
estimate_sequential_bf <- function(full_data_list, model, backend_args, target_params,
                                    thresholds_success, thresholds_futility,
                                    p_sig_scs, p_sig_ftl, analysis_at,
                                    interim_function, id_iter, id_cond) {

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
    current_p_sig_scs <- resolve_threshold(p_sig_scs, info_frac)
    current_p_sig_ftl <- resolve_threshold(p_sig_ftl, info_frac)

    # Identify active (non-stopped) simulations
    active_idx <- which(!stopped)
    if (length(active_idx) == 0 && !is_final) next

    # If all simulations have stopped but we're at final, still analyze for reporting
    if (length(active_idx) == 0) active_idx <- seq_len(batch_size)

    # Subset data to current analysis point for active simulations
    analysis_data_list <- lapply(full_data_list[active_idx], function(fd) fd[1:current_n, ])

    # Prepare batch data
    data_batch <- prepare_data_list_as_batch_bf(analysis_data_list, backend_args)

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
      thr_scs = thresholds_success[1],
      thr_ftl = thresholds_futility[1],
      p_sig_scs = current_p_sig_scs,
      p_sig_ftl = current_p_sig_ftl,
      id_iter = id_iter[active_idx],
      id_cond = id_cond[active_idx],
      id_look = id_analysis,
      n_analyzed = current_n,
      skip_convergence = backend_args$skip_convergence %||% TRUE
    )

    # Update stopping state (if not final)
    if (!is_final) {
      # Find newly stopped simulations
      new_stops_scs <- which(batch_results$dec_scs == 1)
      new_stops_ftl <- which(batch_results$dec_ftl == 1 & batch_results$dec_scs == 0)

      if (length(new_stops_scs) > 0) {
        stopped[active_idx[new_stops_scs]] <- TRUE
        stop_reason[active_idx[new_stops_scs]] <- "stop_success"
      }
      if (length(new_stops_ftl) > 0) {
        stopped[active_idx[new_stops_ftl]] <- TRUE
        stop_reason[active_idx[new_stops_ftl]] <- "stop_futility"
      }

      # Update batch_results with stopping information
      batch_results$stopped <- stopped[active_idx]
      batch_results$stop_reason <- stop_reason[active_idx]
    }

    all_results[[id_analysis]] <- batch_results
  }

  dplyr::bind_rows(all_results)
}
