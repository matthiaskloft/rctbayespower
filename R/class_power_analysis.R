# =============================================================================
# S7 CLASS DEFINITION: rctbp_power_analysis
# =============================================================================
# Main power analysis configuration and results container. Stores simulation
# parameters (n_sims, n_cores), conditions object, and placeholders for results.

#' @importFrom S7 new_class class_any class_numeric class_logical class_list class_character
NULL


# =============================================================================
# CPU INFO HELPER
# =============================================================================

# Get CPU Information (internal helper, not exported)
# Returns CPU processor name for display in power analysis output.
# Works cross-platform using R system commands.
get_cpu_info <- function() {
  tryCatch({
    os <- Sys.info()["sysname"]

    cpu_name <- if (os == "Windows") {
      # Windows: use wmic
      result <- system("wmic cpu get name", intern = TRUE, ignore.stderr = TRUE)
      # Filter out empty lines and header
      result <- trimws(result[nchar(trimws(result)) > 0])
      if (length(result) > 1) result[2] else NULL
    } else if (os == "Darwin") {
      # macOS: use sysctl
      result <- system("sysctl -n machdep.cpu.brand_string", intern = TRUE, ignore.stderr = TRUE)
      if (length(result) > 0 && nchar(result[1]) > 0) result[1] else NULL
    } else {
      # Linux: read from /proc/cpuinfo
      if (file.exists("/proc/cpuinfo")) {
        lines <- readLines("/proc/cpuinfo", n = 10, warn = FALSE)
        model_line <- grep("^model name", lines, value = TRUE)
        if (length(model_line) > 0) {
          sub("^model name\\s*:\\s*", "", model_line[1])
        } else NULL
      } else NULL
    }

    if (!is.null(cpu_name) && nchar(cpu_name) > 0) cpu_name else NULL
  }, error = function(e) NULL)
}

rctbp_power_analysis <- S7::new_class(
  "rctbp_power_analysis",
  properties = list(
    # Simulation control parameters
    n_sims = S7::new_property(S7::class_numeric, default = 1),
    n_cores = S7::new_property(S7::class_numeric, default = 1),
    verbosity = S7::new_property(class = S7::class_numeric, default = 1),
    brms_args = S7::class_list | NULL,
    bf_args = S7::class_list | NULL,  # BayesFlow args: n_posterior_samples, batch_size
    design_prior = S7::new_property(S7::class_character |
                                      S7::class_function | NULL, default = NULL),

    # Configuration objects
    conditions = S7::class_any,  # rctbp_conditions object
    design = S7::new_property(getter = function(self) self@conditions@design),

    # Results (populated after run())
    results_conditions = S7::class_data.frame,
    results_interim = S7::class_data.frame,
    results_raw = S7::class_data.frame,
    elapsed_time = S7::new_property(class = S7::class_numeric, default = NA_real_),

    # Backend environment info (populated after run() for BayesFlow)
    bf_device = S7::new_property(class = S7::class_character | NULL, default = NULL),
    bf_env_info = S7::class_list | NULL,  # Full env metadata from get_bf_env_info()

    # Computed property for interim analysis detection
    has_interim = S7::new_property(
      getter = function(self) nrow(self@results_interim) > 0
    )
  ),
  validator = function(self) {
    # Validate conditions object
    if (!inherits(self@conditions, "rctbayespower::rctbp_conditions") &&
        !inherits(self@conditions, "rctbp_conditions")) {
      cli::cli_abort(c(
        "{.arg conditions} must be a valid rctbp_conditions object",
        "x" = "Got object of class {.cls {class(self@conditions)}}",
        "i" = "Use {.fn build_conditions} to create a valid conditions object"
      ))
    }

    # Validate n_sims, positive whole number
    if (!is.numeric(self@n_sims) ||
        length(self@n_sims) != 1 ||
        self@n_sims <= 0 || self@n_sims != round(self@n_sims)) {
      cli::cli_abort(c(
        "{.arg n_sims} must be a positive whole number",
        "x" = "You supplied {.val {self@n_sims}}",
        "i" = "Use an integer >= 1"
      ))
    }

    # Validate n_cores, positive whole number, < available cores
    if (!is.numeric(self@n_cores) ||
        length(self@n_cores) != 1 ||
        self@n_cores <= 0 ||
        self@n_cores != round(self@n_cores)) {
      cli::cli_abort(c(
        "{.arg n_cores} must be a positive whole number",
        "x" = "You supplied {.val {self@n_cores}}",
        "i" = "Use an integer >= 1"
      ))
    }
    # Validate n_cores <= parallel::detectCores()
    if (self@n_cores > parallel::detectCores()) {
      cli::cli_abort(c(
        "{.arg n_cores} must not exceed available cores",
        "x" = "You requested {self@n_cores} cores, but only {parallel::detectCores()} are available",
        "i" = "We recommend using at most {.code parallel::detectCores() - 1} cores"
      ))
    }

    # Validate verbosity
    if (!is.numeric(self@verbosity) ||
        length(self@verbosity) != 1 ||
        !self@verbosity %in% c(0, 1, 2)) {
      cli::cli_abort(c(
        "{.arg verbosity} must be 0, 1, or 2",
        "x" = "You supplied {.val {self@verbosity}}",
        "i" = "Use 0 (quiet), 1 (normal), or 2 (verbose)"
      ))
    }

    # Validate brms_args
    if (!is.list(self@brms_args)) {
      cli::cli_abort(c(
        "{.arg brms_args} must be a list",
        "x" = "You supplied {.type {self@brms_args}}"
      ))
    }

    # If all validations pass, return NULL
    NULL
  }
)

# =============================================================================
# INTERNAL HELPER: format_duration()
# =============================================================================
# Formats time in minutes to a human-readable string (days, hours, mins, secs)
# dropping zero units

format_duration <- function(minutes) {
  if (is.na(minutes) || !is.numeric(minutes)) return("N/A")

  total_seconds <- round(minutes * 60)

  days <- total_seconds %/% 86400
  remaining <- total_seconds %% 86400
  hours <- remaining %/% 3600
  remaining <- remaining %% 3600
  mins <- remaining %/% 60
  secs <- remaining %% 60

  parts <- character()

  if (days > 0) {
    parts <- c(parts, paste0(days, if (days == 1) " day" else " days"))
  }
  if (hours > 0) {
    parts <- c(parts, paste0(hours, if (hours == 1) " hour" else " hours"))
  }
  if (mins > 0) {
    parts <- c(parts, paste0(mins, if (mins == 1) " min" else " mins"))
  }
  if (secs > 0 || length(parts) == 0) {
    parts <- c(parts, paste0(secs, if (secs == 1) " sec" else " secs"))
  }

  paste(parts, collapse = " ")
}

# =============================================================================
# CONSTRUCTOR FUNCTION: power_analysis()
# =============================================================================
# Creates power analysis configuration object and optionally executes it.
# Validates all parameters and prepares for parallel execution.

#' Build Power Analysis Configuration
#'
#' Creates a power analysis configuration object that specifies all parameters
#' needed to conduct Bayesian power analysis for randomized controlled trials.
#' This function creates an S7 object that serves as the main interface for
#' configuring and executing power analysis simulations.
#'
#' @param conditions An rctbp_conditions object containing the experimental
#'   conditions and design parameters for the power analysis. Created by
#'   [build_conditions()].
#' @param ... Additional arguments passed to the rctbp_power_analysis constructor:
#'   \describe{
#'     \item{n_sims}{Number of simulations to run per condition (default 100)}
#'     \item{n_cores}{Number of CPU cores for parallel execution (default 1)}
#'     \item{verbosity}{Output detail level: 0 (quiet), 1 (normal), 2 (verbose)}
#'     \item{brms_args}{List of brms arguments (chains, iter, warmup, cores)}
#'     \item{bf_args}{List of BayesFlow arguments:
#'       \itemize{
#'         \item n_posterior_samples: Number of posterior samples (default: 1000)
#'         \item batch_size: Batch size for inference (default: n_sims)
#'         \item envname: Python virtual environment name (default: NULL for auto-detect)
#'       }
#'     }
#'     \item{design_prior}{Prior specification (NULL, brms syntax string, or function)}
#'   }
#' @param run Logical indicating whether to immediately execute the analysis
#'   (default TRUE). Set to FALSE to create configuration for later execution.
#'
#' @return An S7 object of class "rctbp_power_analysis" containing:
#'   \itemize{
#'     \item All input parameters for simulation control
#'     \item Access to design and model specifications via properties
#'     \item Placeholder slots for results (filled after running analysis)
#'   }
#'
#' @details
#' This function validates input parameters and creates a configuration object
#' that can be executed using [run()]. The resulting object provides access
#' to design and model information through S7 properties:
#'
#' \strong{Key Properties:}
#' \itemize{
#'   \item `design`: Access to the experimental design configuration
#'   \item `model`: Access to the underlying Bayesian model specification
#'   \item `conditions`: The condition grid for analysis
#' }
#'
#' \strong{Parallel Processing:}
#' When `n_cores > 1`, simulations are distributed across multiple cores
#' for improved performance. The function validates that `n_cores` does
#' not exceed available system cores.
#'
#' @seealso [build_conditions()], [build_design()], [run()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Create conditions for power analysis
#' conditions <- build_conditions(design, n_total = c(100, 200))
#'
#' # Basic power analysis (brms backend)
#' result <- power_analysis(conditions, n_sims = 100)
#'
#' # With custom brms arguments
#' result <- power_analysis(
#'   conditions,
#'   n_sims = 1000,
#'   n_cores = 4,
#'   brms_args = list(chains = 4, iter = 2000)
#' )
#'
#' # BayesFlow backend with custom settings
#' result <- power_analysis(
#'   conditions,
#'   n_sims = 1000,
#'   bf_args = list(n_posterior_samples = 2000, batch_size = 128)
#' )
#'
#' # Create without running (for inspection)
#' config <- power_analysis(conditions, n_sims = 100, run = FALSE)
#' config <- run(config)  # Execute later
#' }
power_analysis <- function(conditions, ..., run = TRUE) {
  # Validate conditions early for better error messages
  if (!inherits(conditions, "rctbayespower::rctbp_conditions") &&
      !inherits(conditions, "rctbp_conditions")) {
    cli::cli_abort(c(
      "{.arg conditions} must be a valid rctbp_conditions object",
      "x" = "Got object of class {.cls {class(conditions)}}",
      "i" = "Use {.fn build_conditions} to create a valid conditions object"
    ))
  }

  power_object <- rctbp_power_analysis(conditions = conditions, ...)

  # Run the power analysis immediately if requested
  if (run) {
    power_object <- run(x = power_object)
  }

  return(power_object)
}


#' Run Analysis Objects
#'
#' Generic function for executing analysis objects. This function provides a
#' unified interface for running different types of analysis configurations.
#'
#' @param x An S7 object to run (e.g., rctbp_power_analysis)
#' @param ... Additional arguments passed to specific methods
#'
#' @details
#' This generic function dispatches to appropriate methods based on the class
#' of the input object. Currently supported:
#' \itemize{
#'   \item \code{rctbp_power_analysis}: Executes Bayesian power analysis
#' }
#'
#' @return The result depends on the specific method called. For power analysis
#'   objects, returns the modified object with results stored in the
#'   \code{results_conditions}, \code{results_interim}, and \code{results_raw} properties.
#'
#' @export
#' @importFrom S7 method new_generic
#' @examples
#' \dontrun{
#' # Create and run power analysis
#' power_config <- rctbp_power_analysis(conditions = conditions, n_sims = 100)
#' power_config <- run(power_config)
#' }
run <- S7::new_generic("run", "x")

#' Run Power Analysis
#'
#' Executes the Bayesian power analysis using the configuration specified in
#' the rctbp_power_analysis object. This method contains the core simulation
#' logic and returns comprehensive results.
#'
#' @param x An S7 object of class "rctbp_power_analysis"
#' @param ... Additional arguments (currently unused)
#'
#' @details
#' This method implements the core power analysis algorithm:
#'
#' \strong{Parallel Execution:} When n_cores > 1, simulations are distributed
#' across multiple cores for optimal performance.
#'
#' \strong{Progress Tracking:} Progress is shown through pbapply when available,
#' with fallback to basic progress reporting.
#'
#' \strong{Error Handling:} Robust error handling ensures partial results are
#' preserved even if some simulations fail.
#'
#' \strong{Result Aggregation:} Individual simulation results are aggregated
#' into power curves and summary statistics.
#'
#' @return The modified S7 object of class "rctbp_power_analysis" with results
#'   populated in the following properties:
#'   \itemize{
#'     \item results_conditions: Aggregated power analysis results (data.frame)
#'     \item results_interim: Per-look interim results for sequential designs (data.frame)
#'     \item results_raw: Raw per-simulation data (data.frame)
#'     \item elapsed_time: Total analysis runtime (difftime)
#'   }
#'
#' @export
#' @importFrom parallel detectCores makeCluster stopCluster parLapply clusterEvalQ clusterExport
#' @importFrom utils modifyList
#' @name run.rctbp_power_analysis

# =============================================================================
# S7 METHOD: run() - Main Power Analysis Execution
# =============================================================================
# Executes power analysis simulations across all conditions using parallel
# processing. Handles S7 serialization, worker setup, batching strategy,
# and result aggregation.

S7::method(run, rctbp_power_analysis) <- function(x, ...) {
  # Time start
  start_time <- Sys.time()

  # Overwrite object parameters with dots if provided
  if (length(list(...)) > 0) {
    # Recreate the S7 object with updated parameters
    x <- update_S7_with_dots(x, ...)
  }

  # Set verbosity for this analysis run
  old_verbosity <- set_verbosity(x@verbosity)
  on.exit(set_verbosity(old_verbosity), add = TRUE)


  # Early status message - show immediately before any slow operations
  design <- x@conditions@design
  # NOTE: After API merge, model properties are directly on design (not design@model@*)
  backend <- design@backend
  n_conditions <- nrow(x@conditions@grid)
  total_work_units <- x@n_sims * n_conditions

 # Calculate total model fits (including interim analyses)
  # analysis_at is now in conditions (can vary per condition)
  # Check first condition's decision_args as representative
  n_analyses <- 1L
  first_decision_args <- x@conditions@params_by_cond[[1]]$decision_args
  if (!is.null(first_decision_args$analysis_at)) {
    n_analyses <- length(first_decision_args$analysis_at)
  }
  total_model_fits <- total_work_units * n_analyses

  if (should_show(1)) {
    cli::cli_h3("Power Analysis Configuration")
    cli::cli_text("")
    if (should_show(2)) {
      print(x@conditions@conditions_grid)
      cli::cli_text("")
    }

    config_items <- c(
      "Backend" = backend,
      "Conditions" = n_conditions,
      "Simulations per condition" = x@n_sims
    )
    if (n_analyses > 1) {
      config_items <- c(config_items,
        "Analyses per simulation" = n_analyses,
        "Total model fits" = total_model_fits
      )
    } else {
      config_items <- c(config_items,
        "Total simulations" = total_work_units
      )
    }
    cli::cli_dl(config_items)
    cli::cli_text("")

    # Performance warning for interactive sessions
    if (interactive()) {
      cli::cli_alert_info("Tip: Console output can slow performance. Use {.code verbosity = 0} for faster execution.")
    }
  }

  # Configure brms model with MCMC settings ----------------------------------
  # This updates the compiled model template with the desired sampling settings.
  # Workers will use this pre-configured model when fitting simulated data.

  if (backend == "brms") {
    # Set default brms args
    # refresh = 0 and silent = 2 suppress Stan/brms output to avoid
    # console I/O bottleneck (can cause 6x slowdown in interactive sessions)
    default_brms_args <- list(
      chains = 4,
      iter = 450,
      warmup = 200,
      cores = 1,
      refresh = 0,
      silent = 2
    )
    # Merge with user-provided brms_args
    final_brms_args <- utils::modifyList(default_brms_args, x@brms_args)
    # Assign back to object for reference
    x@brms_args <- final_brms_args

    # Warn if cores > 1
    if (x@brms_args$cores > 1) {
      cli::cli_warn(c(
        "Parallel configuration issue detected",
        "x" = "Multiple cores specified for brms when running simulations in parallel",
        "i" = "Set brms cores to 1 when using parallel simulations to avoid resource conflicts"
      ))
    }

    # Update brms model with MCMC settings
    # Note: This runs a quick fit on the template data to configure the model.
    # Warnings about R-hat/ESS are expected and suppressed since this is just
    # for configuration - actual inference happens in workers with simulated data.
    # Changing chains/iter/warmup does NOT trigger recompilation (Stan code unchanged).
    # See: https://github.com/paul-buerkner/brms/blob/master/R/update.R
    if (should_show(1)) {
      cli::cli_alert_info("Updating brms model with MCMC settings (chains={final_brms_args$chains}, iter={final_brms_args$iter}, warmup={final_brms_args$warmup})")
    }
    # NOTE: After API merge, inference_model is directly on design
    x@conditions@design@inference_model <- suppressWarnings(
      do.call(function(...) {
        stats::update(object = x@conditions@design@inference_model, ...)
      }, x@brms_args)
    )
    if (should_show(1)) {
      cli::cli_alert_success("Model configured")

      # Show CPU info for brms backend
      cpu_name <- get_cpu_info()
      if (!is.null(cpu_name)) {
        cli::cli_dl(c("Device" = paste0("CPU (", cpu_name, ")")))
      }
      cli::cli_text("")
    }

    # Also store in backend_args for workers
    x@conditions@design@backend_args_brms <- final_brms_args

  } else if (backend == "bf") {
    # For BayesFlow backend, merge user-provided bf_args with model defaults
    default_bf_args <- list(
      n_posterior_samples = 1000L,
      batch_size = NULL,
      envname = NULL
    )
    # Get model defaults, then override with user-provided args
    # NOTE: After API merge, backend_args_bf is directly on design
    design_bf_args <- design@backend_args_bf
    if (length(design_bf_args) > 0) {
      default_bf_args <- utils::modifyList(default_bf_args, design_bf_args)
    }
    if (!is.null(x@bf_args) && length(x@bf_args) > 0) {
      default_bf_args <- utils::modifyList(default_bf_args, x@bf_args)
    }
    final_bf_args <- default_bf_args

    # Resolve NULL batch_size to n_sims
    if (is.null(final_bf_args$batch_size)) {
      final_bf_args$batch_size <- x@n_sims
    }

    x@bf_args <- final_bf_args

    # Store in design for workers
    x@conditions@design@backend_args_bf <- final_bf_args

    # Initialize BayesFlow Python environment with specified envname
    init_bf_python(envname = final_bf_args$envname)

    # Get environment info and store in object (always, not just for display)
    bf_info <- get_bf_env_info()

    if (!is.null(bf_info)) {
      device_str <- if (bf_info$device == "GPU") {
        paste0("GPU (", bf_info$device_name, ", CUDA ", bf_info$cuda_version, ")")
      } else {
        # Use R-based CPU info for better names
        cpu_name <- get_cpu_info()
        if (!is.null(cpu_name)) {
          paste0("CPU (", cpu_name, ")")
        } else {
          "CPU"
        }
      }
      env_str <- if (!is.null(bf_info$envname)) bf_info$envname else "default"

      # Store in object for later access
      x@bf_device <- device_str
      x@bf_env_info <- bf_info

      if (should_show(1)) {
        cli::cli_alert_info("Using BayesFlow backend")
        cli::cli_dl(c(
          "Device" = device_str,
          "Environment" = env_str,
          "Posterior samples" = as.character(final_bf_args$n_posterior_samples),
          "Batch size" = as.character(final_bf_args$batch_size)
        ))
      }
    } else {
      if (should_show(1)) {
        cli::cli_alert_info("Using BayesFlow backend (n_posterior_samples={final_bf_args$n_posterior_samples}, batch_size={final_bf_args$batch_size})")
      }
    }
  }



  # Extract variables from object
  n_cores <- x@n_cores
  n_sims <- x@n_sims

  # =============================================================================
  # BAYESFLOW PARALLEL LIMITATION
  # =============================================================================
  # BayesFlow models are Python objects that cannot be serialized to PSOCK workers.
  # Force sequential execution for BayesFlow backend, but use batch processing
  # for efficiency (neural network vectorization provides speedup).
  if (backend == "bf" && n_cores > 1) {
    cli::cli_alert_warning("BayesFlow backend does not support parallel clusters (Python objects cannot be serialized)")
    cli::cli_alert_info("Running sequentially with batch processing for efficiency")
    n_cores <- 1L
  }
  
  # Extract parameters from the object
  conditions <- x@conditions
  design <- x@conditions@design
  design_prior <- x@design_prior

  # =============================================================================
  # S7 SERIALIZATION WORKAROUND
  # =============================================================================
  # Problem: S7 objects contain environments that don't serialize across
  # parallel cluster boundaries (PSOCK clusters). When passed to workers,
  # S7 objects lose their class information and methods fail.
  #
  # Solution: prepare_design_for_workers() flattens S7 objects to nested lists
  # that serialize correctly. Workers access fields using list syntax ($)
  # instead of S7 property syntax (@).
  #
  # Trade-off: Worker code must handle both S7 and list formats, adding
  # complexity, but enables parallel execution without serialization failures.
  design_serialized <- prepare_design_for_workers(design)

  # Detect backend and batching strategy
  # NOTE: After API merge, properties are directly on design
  backend <- design@backend
  batch_size <- if (backend == "bf" && !is.null(design@backend_args_bf$batch_size)) {
    design@backend_args_bf$batch_size
  } else {
    1L  # No batching for brms or when batch_size not specified
  }

  # Create work units: conditions x iterations (NOT interims!)
  # Work units are (id_cond, id_iter) pairs
  work_units <- lapply(seq_len(n_conditions), function(id_cond) {
    lapply(seq_len(n_sims), function(id_iter) {
      list(
        id_cond = id_cond,
        id_iter = id_iter,
        condition_args = conditions@params_by_cond[[id_cond]]
      )
    })
  })
  work_units <- unlist(work_units, recursive = FALSE)

  # Execute simulations
  # Note: should_show(1) check skips pbapply when verbosity=0 for performance
  # (progress bar updates cause significant overhead in interactive sessions)
  if (requireNamespace("pbapply", quietly = TRUE) && should_show(1)) {
    # set cl to NULL for default sequential execution
    cl <- NULL
    if (n_cores > 1) {
      if (should_show(1)) {
        cli::cli_alert_info("Setting up parallel cluster with {n_cores} cores")
      }
      # Set up cluster
      cl <- parallel::makeCluster(n_cores, type = "PSOCK")
      # Ensure cleanup on exit
      
      # Load required packages on workers first
      parallel::clusterEvalQ(cl, {
        library(brms)
        library(dplyr)
        library(progressr)
        library(posterior)
        library(purrr)
        library(stats)
        library(utils)
        library(S7)
        library(stringr)
        
        # For development with devtools/pkgload, try to load the package
        tryCatch({
          if (requireNamespace("rctbayespower", quietly = TRUE)) {
            library(rctbayespower)
          }
        }, error = function(e) {
          # Package might not be properly installed, will rely on explicit exports
        })
      })
      
      # Export required objects to cluster
      parallel::clusterExport(
        cl,
        varlist = c("work_units", "design_serialized", "batch_size", "backend"),
        envir = environment()
      )

      # =============================================================================
      # WORKER EXPORT STRATEGY (Multi-tier fallback)
      # =============================================================================
      # Algorithm: Try multiple export methods to handle different package states
      #
      # Method 1: Namespace export (installed package mode)
      #   - Fastest and most reliable
      #   - Works when package is installed normally (R CMD INSTALL)
      #   - Functions accessible via asNamespace("rctbayespower")
      #
      # Method 2: Environment export (development mode - NOT IMPLEMENTED)
      #   - Fallback for devtools::load_all() workflow
      #   - devtools doesn't register namespace, so Method 1 fails
      #   - Would extract from parent environment
      #
      # Method 3: Graceful degradation
      #   - Proceed with warning if export fails
      #   - Workers may fail at runtime if functions unavailable
      #
      # Rationale: devtools::load_all() doesn't register package namespace like
      # standard installation, requiring flexible export strategy to support
      # both development and production modes.

      functions_to_export <- c(
        # Worker orchestration
        "worker_process_single",
        "worker_process_batch",
        "prepare_design_for_workers",
        # brms backend
        "estimate_single_brms",
        "estimate_sequential_brms",
        "estimate_posterior_brms",
        "extract_posterior_rvars_brms",
        "summarize_post_brms",
        # BayesFlow backend
        "estimate_single_bf",
        "estimate_sequential_bf",
        "estimate_batch_bf",
        "summarize_post_bf",
        # Shared utilities
        "create_error_result",
        "resolve_threshold",
        "compute_measures",
        # MCSE calculations
        "calculate_mcse_power",
        "calculate_mcse_mean",
        "calculate_mcse_integrated_power"
      )

      export_success <- FALSE

      # Try Method 1: Namespace export
      tryCatch({
        ns <- asNamespace("rctbayespower")
        all_exist <- all(sapply(functions_to_export, exists, envir = ns))
        if (all_exist) {
          parallel::clusterExport(cl, varlist = functions_to_export, envir = ns)
          export_success <- TRUE
          if (should_show(2)) {
            cli::cli_alert_success("Functions exported from package namespace")
          }
        }
      }, error = function(e) {
        if (should_show(2)) {
          cli::cli_alert_warning("Namespace export failed: {e$message}")
        }
      })

      # Method 3: Graceful degradation
      if (!export_success && should_show(1)) {
        cli::cli_alert_warning("Could not export all required functions to workers")
      }
    }

    # =============================================================================
    # BATCHING STRATEGY
    # =============================================================================
    # Algorithm: Choose processing strategy based on backend and batch_size
    #
    # batch_size = 1 (brms typical):
    #   - Each worker processes one (condition, iteration) pair
    #   - brms fits are independent --> no batching benefit
    #   - Simple parallelization: distribute work units across cores
    #
    # batch_size > 1 (NPE typical):
    #   - Group multiple work units into batches
    #   - NPE processes batch in single forward pass --> efficient
    #   - Trade-off: Larger batches = fewer parallel tasks but faster per-batch
    #
    # Rationale: NPE models can vectorize across multiple simulations (batch
    # dimension), making batched processing significantly faster. brms fits each
    # simulation independently, so batching adds no benefit.

    if (batch_size == 1L) {
      # Single processing: one work unit per worker call
      results_raw_list <-
        pbapply::pblapply(cl = cl, seq_along(work_units), function(i) {
          tryCatch({
            wu <- work_units[[i]]
            worker_process_single(
              id_cond = wu$id_cond,
              id_iter = wu$id_iter,
              condition_args = wu$condition_args,
              design = design_serialized
            )
          }, error = function(e) {
            create_error_result(
              id_iter = work_units[[i]]$id_iter,
              id_cond = work_units[[i]]$id_cond,
              id_analysis = 0L,
              error_msg = paste("Worker error:", e$message)
            )
          })
        })
    } else {
      # Batch processing: group work units into batches
      n_batches <- ceiling(total_work_units / batch_size)
      batches <- split(work_units, ceiling(seq_along(work_units) / batch_size))

      results_raw_list <-
        pbapply::pblapply(cl = cl, batches, function(batch) {
          tryCatch({
            worker_process_batch(
              work_units = batch,
              design = design_serialized
            )
          }, error = function(e) {
            # Return errors for all work units in failed batch
            lapply(batch, function(wu) {
              create_error_result(
                id_iter = wu$id_iter,
                id_cond = wu$id_cond,
                id_analysis = 0L,
                error_msg = paste("Batch worker error:", e$message)
              )
            }) |> dplyr::bind_rows()
          })
        })
    }
    
    if (n_cores > 1) {
      # Stop cluster after use
      parallel::stopCluster(cl)
    }

  } else {
    # Fallback: no progress bar if pbapply not available
    if (should_show(1)) {
      cli::cli_alert_info("Running {length(work_units)} simulations (pbapply not available, no progress bar)")
    }

    cl <- NULL
    if (n_cores > 1) {
      # Set up cluster
      cl <- parallel::makeCluster(n_cores, type = "PSOCK")

      # Load required packages on workers first
      parallel::clusterEvalQ(cl, {
        library(brms)
        library(dplyr)
        library(progressr)
        library(posterior)
        library(purrr)
        library(stats)
        library(utils)
        library(S7)
        library(stringr)

        # For development with devtools/pkgload, try to load the package
        tryCatch({
          if (requireNamespace("rctbayespower", quietly = TRUE)) {
            library(rctbayespower)
          }
        }, error = function(e) {
          # Package might not be properly installed, will rely on explicit exports
        })
      })

      # Export required objects to cluster
      parallel::clusterExport(
        cl,
        varlist = c("work_units", "design_serialized", "batch_size", "backend"),
        envir = environment()
      )

      functions_to_export <- c(
        # Worker orchestration
        "worker_process_single",
        "worker_process_batch",
        "prepare_design_for_workers",
        # brms backend
        "estimate_single_brms",
        "estimate_sequential_brms",
        "estimate_posterior_brms",
        "extract_posterior_rvars_brms",
        "summarize_post_brms",
        # BayesFlow backend
        "estimate_single_bf",
        "estimate_sequential_bf",
        "estimate_batch_bf",
        "summarize_post_bf",
        # Shared utilities
        "create_error_result",
        "resolve_threshold",
        "compute_measures",
        # MCSE calculations
        "calculate_mcse_power",
        "calculate_mcse_mean",
        "calculate_mcse_integrated_power"
      )

      export_success <- FALSE

      # Try Method 1: Namespace export
      tryCatch({
        ns <- asNamespace("rctbayespower")
        all_exist <- all(sapply(functions_to_export, exists, envir = ns))
        if (all_exist) {
          parallel::clusterExport(cl, varlist = functions_to_export, envir = ns)
          export_success <- TRUE
          if (should_show(2)) {
            cli::cli_alert_success("Functions exported from package namespace")
          }
        }
      }, error = function(e) {
        if (should_show(2)) {
          cli::cli_alert_warning("Namespace export failed: {e$message}")
        }
      })

      # Method 3: Graceful degradation
      if (!export_success && should_show(1)) {
        cli::cli_alert_warning("Could not export all required functions to workers")
      }
    }

    if (batch_size == 1L) {
      # Single processing
      if (n_cores > 1) {
        results_raw_list <- parallel::parLapply(cl, seq_along(work_units), function(i) {
          tryCatch({
            wu <- work_units[[i]]
            worker_process_single(
              id_cond = wu$id_cond,
              id_iter = wu$id_iter,
              condition_args = wu$condition_args,
              design = design_serialized
            )
          }, error = function(e) {
            create_error_result(
              id_iter = work_units[[i]]$id_iter,
              id_cond = work_units[[i]]$id_cond,
              id_analysis = 0L,
              error_msg = paste("Worker error:", e$message)
            )
          })
        })
      } else {
        results_raw_list <- lapply(work_units, function(wu) {
          tryCatch({
            worker_process_single(
              id_cond = wu$id_cond,
              id_iter = wu$id_iter,
              condition_args = wu$condition_args,
              design = design_serialized
            )
          }, error = function(e) {
            create_error_result(
              id_iter = wu$id_iter,
              id_cond = wu$id_cond,
              id_analysis = 0L,
              error_msg = paste("Worker error:", e$message)
            )
          })
        })
      }
    } else {
      # Batch processing
      batches <- split(work_units, ceiling(seq_along(work_units) / batch_size))
      if (n_cores > 1) {
        results_raw_list <- parallel::parLapply(cl, batches, function(batch) {
          tryCatch({
            worker_process_batch(
              work_units = batch,
              design = design_serialized
            )
          }, error = function(e) {
            lapply(batch, function(wu) {
              create_error_result(
                id_iter = wu$id_iter,
                id_cond = wu$id_cond,
                id_analysis = 0L,
                error_msg = paste("Batch worker error:", e$message)
              )
            }) |> dplyr::bind_rows()
          })
        })
      } else {
        results_raw_list <- lapply(batches, function(batch) {
          tryCatch({
            worker_process_batch(
              work_units = batch,
              design = design_serialized
            )
          }, error = function(e) {
            lapply(batch, function(wu) {
              create_error_result(
                id_iter = wu$id_iter,
                id_cond = wu$id_cond,
                id_analysis = 0L,
                error_msg = paste("Batch worker error:", e$message)
              )
            }) |> dplyr::bind_rows()
          })
        })
      }
    }

    # Clean up cluster
    if (n_cores > 1) {
      parallel::stopCluster(cl)
    }
  }
  
  # Filter out NULL results before combining
  results_raw_list <- results_raw_list[!sapply(results_raw_list, is.null)]
  
  if (length(results_raw_list) == 0) {
    stop("All simulations failed. Check your model and data simulation parameters.")
  }
  
  # Check for and report any error messages
  # Always show errors for debugging
  error_results <- dplyr::bind_rows(results_raw_list)
  error_rows <- !is.na(error_results$error_msg)
  if (any(error_rows)) {
    error_msgs <- error_results[error_rows, "error_msg"]
    cat("Simulation errors found:\n")
    cat(paste(unique(error_msgs), collapse = "\n"), "\n")
    cat("Number of errors:", sum(error_rows), "\n")
  }
  
  # Combine results - use bind_rows for robustness
  results_df_raw <- dplyr::bind_rows(results_raw_list)

  # Brief simulation summary (shown at verbosity >= 1)
  if (should_show(1)) {
    n_conds <- length(unique(results_df_raw$id_cond))
    total_sims <- nrow(results_df_raw)
    has_interim <- "id_look" %in% colnames(results_df_raw)

    cli::cli_text("")
    cli::cli_alert_info("Simulations complete: {n_conds} condition{?s}, {n_sims} iteration{?s} each")
  }

  # Average across simulation runs
  results_summary <- summarize_sims(results_df_raw, n_sims)

  # =============================================================================

  # HANDLE INTERIM VS STANDARD RESULT STRUCTURE
  # =============================================================================
  # summarize_sims() returns either:
  # - A data frame (standard single-look analysis)
  # - A list with $by_look and $overall (interim/sequential analysis)
  #
  # Results storage:
  # - results_conditions: per-condition summary (single-look) or overall stopping stats (sequential)
  # - results_interim: per-look metrics (sequential only, empty for single-look)
  # - results_raw: raw per-simulation data
  if (is.list(results_summary) && !is.data.frame(results_summary)) {
    # Sequential analysis results
    # results_interim = per-look metrics joined with conditions
    results_interim_df <- dplyr::full_join(
      conditions@grid,
      results_summary$by_look,
      by = "id_cond"
    )

    # results_conditions = overall stopping stats joined with conditions
    results_conditions_df <- dplyr::full_join(
      conditions@grid,
      results_summary$overall,
      by = "id_cond"
    )
  } else {
    # Standard single-look results
    results_conditions_df <- dplyr::full_join(
      conditions@grid,
      results_summary,
      by = "id_cond"
    )
    results_interim_df <- data.frame()
  }

  # End time

  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")

  # Print results summary (shown at verbosity >= 1)
  if (should_show(1)) {
    cli::cli_alert_success("Analysis complete in {format_duration(as.numeric(elapsed_time))}")

    # Show power range (check both results_conditions and results_interim for pwr_eff)
    pwr_source <- if ("pwr_eff" %in% names(results_conditions_df)) {
      results_conditions_df
    } else if (nrow(results_interim_df) > 0 && "pwr_eff" %in% names(results_interim_df)) {
      results_interim_df
    } else {
      NULL
    }

    if (!is.null(pwr_source)) {
      pwr_range <- range(pwr_source$pwr_eff, na.rm = TRUE)
      cli::cli_alert_info("Power range: {round(pwr_range[1] * 100, 1)}% - {round(pwr_range[2] * 100, 1)}%")

      # Show best condition (highest power)
      best_idx <- which.max(pwr_source$pwr_eff)
      if (length(best_idx) > 0) {
        best_pwr <- round(pwr_source$pwr_eff[best_idx] * 100, 1)
        # n_total may not exist if not in crossed/constant
        if ("n_total" %in% names(pwr_source)) {
          best_n <- pwr_source$n_total[best_idx]
          cli::cli_alert_success("Highest power: {best_pwr}% at n_total = {best_n}")
        } else {
          best_cond <- pwr_source$id_cond[best_idx]
          cli::cli_alert_success("Highest power: {best_pwr}% (condition {best_cond})")
        }
      }
    }

    # Show early stopping summary if sequential analysis
    if (nrow(results_interim_df) > 0 && "prop_stp_early" %in% names(results_conditions_df)) {
      avg_stopped <- mean(results_conditions_df$prop_stp_early, na.rm = TRUE)
      avg_n_mn <- mean(results_conditions_df$n_mn, na.rm = TRUE)
      avg_n_planned <- mean(results_conditions_df$n_planned, na.rm = TRUE)
      cli::cli_alert_info("Early stopping: {round(avg_stopped * 100, 1)}% stopped early, avg N = {round(avg_n_mn, 0)} of {round(avg_n_planned, 0)} planned")
    }

    cli::cli_text("")
    cli::cli_text("Use {.code print(result)} for detailed summary or {.code plot(result)} for visualizations")
  }

  # Update the S7 object with results
  x@results_conditions <- results_conditions_df
  x@results_interim <- results_interim_df
  x@results_raw <- results_df_raw
  x@elapsed_time <- as.numeric(elapsed_time)

  # Show full compact summary at verbosity >= 2
  if (should_show(2)) {
    cli::cli_text("")
    print(x, target_pwr = x@conditions@target_pwr)
  }

  # Return the updated object
  return(x)
  
}


# S7 Method for Print

#' Print Method for rctbp_power_analysis Objects
#'
#' Displays a compact summary of a power analysis object. When results are
#' available, shows decision rates, early stopping statistics, and the
#' optimal/highest power condition.
#'
#' @param x An S7 object of class "rctbp_power_analysis"
#' @param ... Additional arguments:
#'   \describe{
#'     \item{target_pwr}{Target power for finding optimal condition. If NULL
#'       (default), shows the condition with highest achieved power.}
#'   }
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @importFrom S7 method
#' @name print.rctbp_power_analysis
#' @export
S7::method(print, rctbp_power_analysis) <- function(x, ...) {

  dots <- list(...)
  target_pwr <- if (!is.null(dots$target_pwr)) {
    dots$target_pwr
  } else {
    x@conditions@target_pwr
  }

  design <- x@conditions@design
  has_results <- nrow(x@results_conditions) > 0 || nrow(x@results_raw) > 0

  # Note: fmt_range() and fmt_params() are defined in report_builders.R

  # Helper to safely get range (handles missing/empty columns)
  safe_range <- function(col, pct = FALSE, digits = 1) {
    if (is.null(col)) return("N/A")
    vals <- col[!is.na(col)]
    if (length(vals) == 0) return("N/A")
    fmt_range(vals, pct = pct, digits = digits)
  }

  # Header
  cli::cli_h1("Power Analysis Results")

  if (has_results) {
    n_conditions <- nrow(x@conditions@grid)
    has_interim <- x@has_interim
    # For sequential: power metrics in results_interim, overall stats in results_conditions
    # For single-look: power metrics in results_conditions
    results_df <- if (has_interim) x@results_interim else x@results_conditions
    interim_overall <- if (has_interim) x@results_conditions else NULL
    n_looks <- if (has_interim) length(unique(results_df$id_look)) else 1

    # Status line
    runtime <- format_duration(x@elapsed_time)
    design_type <- if (has_interim) paste0("Sequential (", n_looks, " looks)") else "Single-look"

    cli::cli_alert_success("Completed in {runtime} | {n_conditions} conditions x {x@n_sims} sims | {design_type}")

    # Design section
    cli::cli_h2("Design")
    target_params_str <- paste(design@target_params, collapse = ", ")
    p_eff_str <- get_threshold_display(x@conditions, "thr_dec_eff")
    p_fut_str <- get_threshold_display(x@conditions, "thr_dec_fut")
    cli::cli_text("Target: {.field {target_params_str}} | P(eff) >= {p_eff_str}, P(fut) >= {p_fut_str}")
    if (!is.null(target_pwr)) {
      cli::cli_text("Target power: {.strong {round(target_pwr * 100, 0)}%}")
    }

    # Backend and device info
    # NOTE: After API merge, backend is directly on design
    backend <- design@backend
    if (backend == "bf" && !is.null(x@bf_device)) {
      env_name <- if (!is.null(x@bf_env_info$envname)) x@bf_env_info$envname else "default"
      cli::cli_text("Backend: {.field BayesFlow} | Device: {x@bf_device} | Env: {env_name}")
    } else {
      cli::cli_text("Backend: {.field {backend}}")
    }

    # Decision Rates section
    cli::cli_h2("Decision Rates {.emph (range across conditions)}")
    pwr_eff_range <- safe_range(results_df$pwr_eff, pct = TRUE)
    pwr_fut_range <- safe_range(results_df$pwr_fut, pct = TRUE)
    cli::cli_bullets(c(
      "*" = "Efficacy: {pwr_eff_range}",
      "*" = "Futility: {pwr_fut_range}"
    ))
    if (has_interim && !is.null(interim_overall) && "prop_no_dec" %in% names(interim_overall)) {
      no_dec_range <- safe_range(interim_overall$prop_no_dec, pct = TRUE)
      cli::cli_bullets(c("*" = "No decision: {no_dec_range}"))
    }

    # Early Stopping section (only for sequential)
    if (has_interim && !is.null(interim_overall)) {
      cli::cli_h2("Early Stopping {.emph (range across conditions)}")
      bullets <- character()
      if ("prop_stp_early" %in% names(interim_overall)) {
        stp_range <- safe_range(interim_overall$prop_stp_early, pct = TRUE, digits = 0)
        bullets <- c(bullets, "*" = "Stopped early: {stp_range}")
      }
      if ("n_mn" %in% names(interim_overall) && "n_planned" %in% names(interim_overall)) {
        n_mn_range <- safe_range(interim_overall$n_mn, digits = 0)
        n_planned_range <- safe_range(interim_overall$n_planned, digits = 0)
        bullets <- c(bullets, "*" = "Sample size: {n_mn_range} (of {n_planned_range} planned)")
      }
      if (length(bullets) > 0) {
        cli::cli_bullets(bullets)
      }
    }

    # Find optimal condition
    optimal <- find_optimal_condition(
      results_summ = results_df,
      conditions_grid = x@conditions@grid,
      target_pwr = target_pwr,
      interim_overall = interim_overall,
      power_col = "pwr_eff"
    )

    # Optimal/Highest condition section
    if (optimal$found || optimal$mode == "highest") {
      if (optimal$mode == "highest") {
        cli::cli_h2("Highest Power Condition {.emph (id: {optimal$condition_id})}")
      } else {
        cli::cli_h2("Optimal Condition for {round(target_pwr * 100, 0)}% Power {.emph (id: {optimal$condition_id})}")
      }

      param_str <- fmt_params(optimal$condition_params)
      pwr_val <- round(optimal$achieved_pwr * 100, 1)
      cli::cli_bullets(c("*" = "Power: {.strong {pwr_val}%} | {param_str}"))

      if (!is.null(optimal$interim)) {
        int <- optimal$interim
        cli::cli_bullets(c(
          "*" = "Stopped early: {round(int$prop_stp_early * 100, 1)}% | N: mean={round(int$n_mn, 0)}, median={round(int$n_mdn, 0)}, mode={round(int$n_mode, 0)}"
        ))
      }
    } else if (!is.null(optimal$closest)) {
      cli::cli_alert_warning("No condition achieves {round(target_pwr * 100, 0)}% power")
      cli::cli_h2("Closest Condition {.emph (id: {optimal$closest$condition_id})}")

      param_str <- fmt_params(optimal$closest$condition_params)
      pwr_val <- round(optimal$closest$achieved_pwr * 100, 1)
      cli::cli_bullets(c("*" = "Power: {.strong {pwr_val}%} | {param_str}"))

      if (!is.null(optimal$closest$interim)) {
        int <- optimal$closest$interim
        cli::cli_bullets(c(
          "*" = "Stopped early: {round(int$prop_stp_early * 100, 1)}% | N: mean={round(int$n_mn, 0)}, median={round(int$n_mdn, 0)}, mode={round(int$n_mode, 0)}"
        ))
      }
    }
    # Hints
    cli::cli_text("")
    cli::cli_rule()
    if (is.null(target_pwr)) {
      cli::cli_alert_info("Find optimal N for targeted power: {.code print(x, target_pwr = 0.8)}")
    }
    cli::cli_alert_info("Visualize: {.code plot(x)}")

  } else {
    # Pending analysis
    n_conditions <- nrow(x@conditions@grid)
    cli::cli_alert_warning("Analysis not yet run")

    cli::cli_h2("Configuration")
    cli::cli_bullets(c(
      "*" = "Conditions: {n_conditions}",
      "*" = "Simulations: {x@n_sims} per condition",
      "*" = "Cores: {x@n_cores}"
    ))
    cli::cli_text("")
    cli::cli_rule()
    cli::cli_alert_info("Run: {.code run(x)}")
  }

  invisible(x)
}


#' Summary Method for rctbp_power_analysis Objects
#'
#' Displays a comprehensive summary of power analysis results including all
#' conditions and detailed statistics. For a compact overview, use [print()].
#'
#' @param object An S7 object of class "rctbp_power_analysis"
#' @param ... Additional arguments:
#'   \describe{
#'     \item{target_pwr}{Target power for finding optimal condition. If NULL
#'       (default), shows the condition with highest achieved power.}
#'   }
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#'
#' @details
#' The summary follows the same structure as [print()] but provides more detail:
#' \itemize{
#'   \item Design specifications with full boundary descriptions
#'   \item Results summary with power ranges
#'   \item Optimal/highest power condition with full details
#'   \item Per-condition results table
#'   \item Per-look results (for sequential designs)
#' }
#'
#' For topic-specific reports, see [report()].
#'
#' @seealso [print.rctbp_power_analysis()], [report()], [report_early_stopping()],
#'   [report_conditions()]
#'
#' @importFrom S7 method
#' @name summary.rctbp_power_analysis
#' @export
S7::method(summary, rctbp_power_analysis) <- function(object, ...) {
  dots <- list(...)
  target_pwr <- if (!is.null(dots$target_pwr)) {
    dots$target_pwr
  } else {
    object@conditions@target_pwr
  }

  design <- object@conditions@design
  has_results <- nrow(object@results_conditions) > 0 || nrow(object@results_raw) > 0

  # Helper to safely get range (handles missing/empty columns)
  safe_range <- function(x, pct = FALSE, digits = 1) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return("N/A")
    fmt_range(x, pct = pct, digits = digits)
  }

  # Header
  cli::cli_h1("Power Analysis Summary")

  if (has_results) {
    n_conditions <- nrow(object@conditions@grid)
    has_interim <- object@has_interim
    results_df <- if (has_interim) object@results_interim else object@results_conditions
    interim_overall <- if (has_interim) object@results_conditions else NULL
    n_looks <- if (has_interim) length(unique(results_df$id_look)) else 1

    # Status line
    runtime <- format_duration(object@elapsed_time)
    design_type <- if (has_interim) paste0("Sequential (", n_looks, " looks)") else "Single-look"
    cli::cli_alert_success("Completed in {runtime} | {n_conditions} conditions x {object@n_sims} sims | {design_type}")

    # Design section (more verbose than print)
    cli::cli_h2("Design")
    target_params_str <- paste(design@target_params, collapse = ", ")
    p_eff_str <- get_threshold_display(object@conditions, "thr_dec_eff")
    p_fut_str <- get_threshold_display(object@conditions, "thr_dec_fut")

    cli::cli_bullets(c(
      "*" = "Target parameters: {.field {target_params_str}}",
      "*" = paste0("Efficacy probability threshold: ", p_eff_str),
      "*" = paste0("Futility probability threshold: ", p_fut_str)
    ))

    # analysis_at is now in conditions (crossed, linked, or constant)
    analysis_at_display <- get_threshold_display(object@conditions, "analysis_at")
    if (analysis_at_display != "(not specified)") {
      cli::cli_bullets(c("*" = "Analysis timepoints: {analysis_at_display}"))
    }

    if (!is.null(target_pwr)) {
      cli::cli_bullets(c("*" = "Target power: {.strong {round(target_pwr * 100, 0)}%}"))
    }

    # Backend and device info
    # NOTE: After API merge, backend is directly on design
    backend <- design@backend
    if (backend == "bf" && !is.null(object@bf_env_info)) {
      bf_info <- object@bf_env_info
      env_name <- bf_info$envname %||% "default"
      py_ver <- bf_info$python_version %||% "unknown"
      bf_ver <- bf_info$pkg_versions$bayesflow %||% "unknown"
      torch_ver <- bf_info$pkg_versions$torch %||% "unknown"

      cli::cli_bullets(c(
        "*" = "Backend: {.field BayesFlow}",
        "*" = "Device: {object@bf_device}",
        "*" = "Python environment: {env_name}",
        "*" = "Python version: {py_ver}",
        "*" = "Package versions: bayesflow={bf_ver}, torch={torch_ver}"
      ))
    } else if (backend == "bf") {
      cli::cli_bullets(c(
        "*" = "Backend: {.field BayesFlow}",
        "*" = "Device: {object@bf_device}"
      ))
    } else {
      cli::cli_bullets(c("*" = "Backend: {.field {backend}}"))
    }

    # Decision Rates section
    cli::cli_h2("Decision Rates")
    pwr_eff_range <- safe_range(results_df$pwr_eff, pct = TRUE)
    pwr_fut_range <- safe_range(results_df$pwr_fut, pct = TRUE)
    cli::cli_bullets(c(
      "*" = "Efficacy rate (range): {pwr_eff_range}",
      "*" = "Futility rate (range): {pwr_fut_range}"
    ))
    if (has_interim && !is.null(interim_overall) && "prop_no_dec" %in% names(interim_overall)) {
      no_dec_range <- safe_range(interim_overall$prop_no_dec, pct = TRUE)
      cli::cli_bullets(c("*" = "No decision rate (range): {no_dec_range}"))
    }

    # Early Stopping section (for sequential, more verbose)
    if (has_interim && !is.null(interim_overall)) {
      cli::cli_h2("Early Stopping")
      bullets <- character()

      if ("prop_stp_early" %in% names(interim_overall)) {
        stp_range <- safe_range(interim_overall$prop_stp_early, pct = TRUE)
        bullets <- c(bullets, "*" = "Stopped early (total): {stp_range}")
      }
      if ("prop_stp_eff" %in% names(interim_overall)) {
        stp_eff_range <- safe_range(interim_overall$prop_stp_eff, pct = TRUE)
        bullets <- c(bullets, "*" = "Stopped for efficacy: {stp_eff_range}")
      }
      if ("prop_stp_fut" %in% names(interim_overall)) {
        stp_fut_range <- safe_range(interim_overall$prop_stp_fut, pct = TRUE)
        bullets <- c(bullets, "*" = "Stopped for futility: {stp_fut_range}")
      }
      if ("n_mn" %in% names(interim_overall) && "n_planned" %in% names(interim_overall)) {
        n_mn_range <- safe_range(interim_overall$n_mn, digits = 0)
        n_planned_range <- safe_range(interim_overall$n_planned, digits = 0)
        bullets <- c(bullets, "*" = "Mean sample size: {n_mn_range} (of {n_planned_range} planned)")
      }
      if ("n_mdn" %in% names(interim_overall)) {
        n_mdn_range <- safe_range(interim_overall$n_mdn, digits = 0)
        bullets <- c(bullets, "*" = "Median sample size: {n_mdn_range}")
      }

      if (length(bullets) > 0) {
        cli::cli_bullets(bullets)
      }
    }

    # Find optimal condition
    optimal <- find_optimal_condition(
      results_summ = results_df,
      conditions_grid = object@conditions@grid,
      target_pwr = target_pwr,
      interim_overall = interim_overall,
      power_col = "pwr_eff"
    )

    # Optimal/Highest condition section (more verbose than print)
    if (optimal$found || optimal$mode == "highest") {
      if (optimal$mode == "highest") {
        cli::cli_h2("Highest Power Condition")
      } else {
        cli::cli_h2("Optimal Condition for {round(target_pwr * 100, 0)}% Power")
      }

      cli::cli_bullets(c(
        "*" = "Condition ID: {.val {optimal$condition_id}}",
        "*" = "Achieved power: {.strong {round(optimal$achieved_pwr * 100, 1)}%}",
        "*" = "Sample size (n_total): {.val {optimal$n_total}}"
      ))

      # Show all condition parameters (skip n_total since already shown)
      for (pname in names(optimal$condition_params)) {
        if (pname == "n_total") next
        pval <- optimal$condition_params[[pname]]
        if (is.numeric(pval)) pval <- round(pval, 3)
        cli::cli_bullets(c("*" = "{pname}: {.val {pval}}"))
      }

      if (!is.null(optimal$interim)) {
        int <- optimal$interim
        cli::cli_text("")
        cli::cli_text("{.emph Early stopping (this condition):}")
        bullets <- c("*" = "Stopped early: {round(int$prop_stp_early * 100, 1)}%")
        if (!is.null(int$prop_stp_eff) && !is.na(int$prop_stp_eff)) {
          bullets <- c(bullets, "*" = "Stopped for efficacy: {round(int$prop_stp_eff * 100, 1)}%")
        }
        if (!is.null(int$prop_stp_fut) && !is.na(int$prop_stp_fut)) {
          bullets <- c(bullets, "*" = "Stopped for futility: {round(int$prop_stp_fut * 100, 1)}%")
        }
        bullets <- c(bullets, "*" = "Mean N: {round(int$n_mn, 0)}, Median N: {round(int$n_mdn, 0)}, Mode N: {round(int$n_mode, 0)}")
        cli::cli_bullets(bullets)
      }
    } else if (!is.null(optimal$closest)) {
      cli::cli_alert_warning("No condition achieves {round(target_pwr * 100, 0)}% power")
      cli::cli_h2("Closest Condition")

      cli::cli_bullets(c(
        "*" = "Condition ID: {.val {optimal$closest$condition_id}}",
        "*" = "Achieved power: {.strong {round(optimal$closest$achieved_pwr * 100, 1)}%}",
        "*" = "Sample size (n_total): {.val {optimal$closest$n_total}}"
      ))

      for (pname in names(optimal$closest$condition_params)) {
        if (pname == "n_total") next
        pval <- optimal$closest$condition_params[[pname]]
        if (is.numeric(pval)) pval <- round(pval, 3)
        cli::cli_bullets(c("*" = "{pname}: {.val {pval}}"))
      }

      if (!is.null(optimal$closest$interim)) {
        int <- optimal$closest$interim
        cli::cli_text("")
        cli::cli_text("{.emph Early stopping (this condition):}")
        bullets <- c("*" = "Stopped early: {round(int$prop_stp_early * 100, 1)}%")
        if (!is.null(int$prop_stp_eff) && !is.na(int$prop_stp_eff)) {
          bullets <- c(bullets, "*" = "Stopped for efficacy: {round(int$prop_stp_eff * 100, 1)}%")
        }
        if (!is.null(int$prop_stp_fut) && !is.na(int$prop_stp_fut)) {
          bullets <- c(bullets, "*" = "Stopped for futility: {round(int$prop_stp_fut * 100, 1)}%")
        }
        bullets <- c(bullets, "*" = "Mean N: {round(int$n_mn, 0)}, Median N: {round(int$n_mdn, 0)}, Mode N: {round(int$n_mode, 0)}")
        cli::cli_bullets(bullets)
      }
    }

    # Per-condition results table
    cli::cli_h2("All Conditions")
    if (has_interim) {
      # For sequential: get power from final look of results_interim, merge with interim_overall
      final_look <- max(results_df$id_look)
      final_power <- results_df[results_df$id_look == final_look,
                                c("id_cond", "pwr_eff", "pwr_fut"), drop = FALSE]

      # Merge with interim_overall stats
      if (!is.null(interim_overall) && nrow(interim_overall) > 0) {
        cond_table <- merge(
          interim_overall[, intersect(c("id_cond", "n_total", "prop_stp_early", "n_mn"),
                                      names(interim_overall)), drop = FALSE],
          final_power,
          by = "id_cond",
          all.x = TRUE
        )
        # Reorder columns
        col_order <- c("id_cond", "n_total", "pwr_eff", "pwr_fut", "prop_stp_early", "n_mn")
        col_order <- intersect(col_order, names(cond_table))
        cond_table <- cond_table[, col_order, drop = FALSE]
        # Sort by power
        if ("pwr_eff" %in% names(cond_table)) {
          cond_table <- cond_table[order(-cond_table$pwr_eff), , drop = FALSE]
        }
      } else {
        cond_table <- final_power
      }
    } else {
      # Single-look results
      cols <- c("id_cond", "n_total", "pwr_eff", "pwr_fut")
      cols_available <- intersect(cols, names(results_df))
      cond_table <- results_df[, cols_available, drop = FALSE]
      if ("pwr_eff" %in% names(cond_table)) {
        cond_table <- cond_table[order(-cond_table$pwr_eff), , drop = FALSE]
      }
    }
    table_lines <- format_table_cli(cond_table)
    cat(paste(table_lines, collapse = "\n"), "\n")

    # Hints
    cli::cli_text("")
    cli::cli_rule()
    if (is.null(target_pwr)) {
      cli::cli_alert_info("Find optimal N: {.code print(x, target_pwr = 0.8)}")
    }
    cli::cli_alert_info("Visualize: {.code plot(x)}")
    if (has_interim) {
      cli::cli_alert_info("Early stopping details: {.code report(x, 'early_stopping')}")
    }

  } else {
    # Pending analysis
    n_conditions <- nrow(object@conditions@grid)
    cli::cli_alert_warning("Analysis not yet run")

    cli::cli_h2("Configuration")
    cli::cli_bullets(c(
      "*" = "Conditions: {n_conditions}",
      "*" = "Simulations per condition: {object@n_sims}",
      "*" = "Cores: {object@n_cores}",
      "*" = "Verbosity: {object@verbosity}"
    ))

    # Show design details
    cli::cli_h2("Design")
    target_params_str <- paste(design@target_params, collapse = ", ")
    p_eff_str <- get_threshold_display(object@conditions, "thr_dec_eff")
    p_fut_str <- get_threshold_display(object@conditions, "thr_dec_fut")
    cli::cli_bullets(c(
      "*" = "Target parameters: {.field {target_params_str}}",
      "*" = paste0("Efficacy threshold: ", p_eff_str),
      "*" = paste0("Futility threshold: ", p_fut_str)
    ))

    # analysis_at is now in conditions
    analysis_at_display <- get_threshold_display(object@conditions, "analysis_at")
    if (analysis_at_display != "(not specified)") {
      cli::cli_bullets(c("*" = "Analysis timepoints: {analysis_at_display}"))
    }

    # Show conditions grid preview
    cli::cli_h2("Conditions Grid")
    grid_preview <- object@conditions@grid
    table_lines <- format_table_cli(grid_preview)
    cat(paste(table_lines, collapse = "\n"), "\n")

    cli::cli_text("")
    cli::cli_rule()
    cli::cli_alert_info("Run analysis: {.code run(x)}")
  }

  invisible(object)
}
