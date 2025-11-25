# =============================================================================
# S7 CLASS DEFINITION: rctbp_power_analysis
# =============================================================================
# Main power analysis configuration and results container. Stores simulation
# parameters (n_sims, n_cores), conditions object, and placeholders for results.

#' @importFrom S7 new_class class_any class_numeric class_logical class_list class_character

rctbp_power_analysis <- S7::new_class(
  "rctbp_power_analysis",
  properties = list(
    # Simulation control parameters
    n_sims = S7::new_property(S7::class_numeric, default = 1),
    n_cores = S7::new_property(S7::class_numeric, default = 1),
    verbosity = S7::new_property(class = S7::class_numeric, default = 1),
    brms_args = S7::class_list | NULL,
    design_prior = S7::new_property(S7::class_character |
                                      S7::class_function | NULL, default = NULL),

    # Configuration objects
    conditions = S7::class_any,  # rctbp_conditions object
    design = S7::new_property(getter = function(self) self@conditions@design),

    # Results (populated after run())
    summarized_results = S7::class_data.frame,
    raw_results = S7::class_data.frame,
    elapsed_time = S7::new_property(class = S7::class_numeric, default = NA_real_)
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
#' @param run Logical indicating whether to immediately execute the analysis
#'   after creating the configuration object (default TRUE)
#' @param ... Arguments passed to the rctbp_power_analysis constructor, including:
#'   \itemize{
#'     \item conditions: An rctbp_conditions object containing the experimental
#'       conditions and design parameters for the power analysis
#'     \item n_sims: Number of simulations to run per condition (default 100)
#'     \item n_cores: Number of CPU cores to use for parallel execution (default 1).
#'       Must not exceed the number of available cores
#'     \item verbosity: Verbosity level controlling output detail (default 1).
#'       0 = quiet (minimal output), 1 = normal (standard output), 2 = verbose (detailed debug output).
#'       Can also be set globally with options(rctbayespower.verbosity = level).
#'     \item brms_args: List of additional arguments to pass to [brms::brm()]
#'       function (default empty list)
#'     \item design_prior: Prior specification for design parameters. Can be NULL
#'       (no prior), a string with brms syntax, or a function for custom priors
#'   }
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
#' @seealso [build_conditions()], [build_design()], [build_model()], [run()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Create conditions for power analysis
#' conditions <- build_conditions(design, n_total = c(100, 200))
#'
#' # Basic power analysis configuration
#' power_config <- build_power_analysis(conditions, n_sims = 100)
#'
#' # Parallel execution with custom BRMS arguments
#' power_config <- build_power_analysis(
#'   conditions = conditions,
#'   n_sims = 1000,
#'   n_cores = 4,
#'   brms_args = list(chains = 4, iter = 2000)
#' )
#'
#' # Execute the analysis
#' results <- run(power_config)
#' }
power_analysis <- function(run = TRUE, ...) {
  power_object <- rctbp_power_analysis(...)
  
  # Overwrite object parameters with dots if provided and run = FALSE
  # Avoids updating twice, since run() also updates S7 object with dots
  if (!run){
    if (length(list(...)) > 0) {
      # Recreate the S7 object with updated parameters
      power_object <- update_S7_with_dots(power_object, ...)
    }
  }

  # Run the power analysis immediately if requested)
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
#'   \code{summarized_results} and \code{raw_results} properties.
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
#' @return A list of class "rctbayespower_sim_result" containing:
#' \itemize{
#'   \item results_df: Aggregated power analysis results
#'   \item results_df_raw: Raw simulation results from all runs
#'   \item design: The design object used for analysis
#'   \item conditions: The condition specifications used
#'   \item n_sims: Number of simulations per condition
#'   \item elapsed_time: Total analysis runtime
#' }
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

  # update model with brms_args (only for brms backend) ------------
  design <- x@conditions@design
  backend <- design@model@backend

  if (backend == "brms") {
    # set default brms args
    default_brms_args <- list(
      chains = 4,
      iter = 450,
      warmup = 200,
      cores = 1
    )
    # merge with user-provided brms_args
    final_brms_args <- utils::modifyList(default_brms_args, x@brms_args)
    # assign back to object for later use
    x@brms_args <- final_brms_args

    # warn if cores > 1
    if (x@brms_args$cores > 1) {
      cli::cli_warn(c(
        "Parallel configuration issue detected",
        "x" = "Multiple cores specified for brms when running simulations in parallel",
        "i" = "Set brms cores to 1 when using parallel simulations to avoid resource conflicts"
      ))
    }
    # update brms_args in the object
    x@conditions@design@model@brms_model <- do.call(function(...) {
      stats::update(object = x@conditions@design@model@brms_model, ...)
    }, x@brms_args)
  } else if (backend == "npe") {
    # For NPE backend, backend_args are already in model@backend_args
    if (should_show(2)) {
      cat("Using NPE backend with configuration:\n")
      cat("  Batch size:",
          if (!is.null(design@model@backend_args$batch_size))
            design@model@backend_args$batch_size
          else
            1,
          "\n")
    }
  }



  # Extract variables from object
  n_cores <- x@n_cores
  n_sims <- x@n_sims
  
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
  backend <- design@model@backend
  batch_size <- if (backend == "npe" && !is.null(design@model@backend_args$batch_size)) {
    design@model@backend_args$batch_size
  } else {
    1L  # No batching for brms or when batch_size not specified
  }

  # Create work units: conditions × iterations (NOT interims!)
  # Work units are (id_cond, id_iter) pairs
  n_conditions <- length(conditions@condition_arguments)
  work_units <- lapply(seq_len(n_conditions), function(id_cond) {
    lapply(seq_len(n_sims), function(id_iter) {
      list(
        id_cond = id_cond,
        id_iter = id_iter,
        condition_args = conditions@condition_arguments[[id_cond]]
      )
    })
  })
  work_units <- unlist(work_units, recursive = FALSE)

  # Set up parallelization
  total_work_units <- length(work_units)

  # Optional logging
  # Show if verbosity level is 2 (verbose)
  if (should_show(2)) {
    cli::cli_h3("Power Analysis Configuration")
    cli::cli_text("")
    cli::cli_text("{.strong Conditions:}")
    print(conditions@conditions_grid)
    cli::cli_text("")

    cli::cli_dl(c(
      "Backend" = backend,
      "Batch size" = batch_size,
      "Conditions to test" = nrow(conditions@conditions_grid),
      "Simulations per condition" = n_sims,
      "Total work units" = total_work_units
    ))
    cli::cli_text("")

    # Note: analysis_at and adaptive are per-condition parameters
    # and can be viewed in the conditions grid if needed
  }
  
  # Execute simulations
  if (requireNamespace("pbapply", quietly = TRUE)) {
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
        "worker_process_single",
        "worker_process_batch",
        "prepare_design_for_workers",
        "estimate_single_brms",
        "estimate_single_npe",
        "estimate_sequential_brms",
        "estimate_sequential_npe",
        "create_error_result",
        "estimate_posterior",
        "estimate_posterior_brms",
        "estimate_posterior_npe",
        "extract_posterior_rvars",
        "extract_posterior_rvars_brms",
        "extract_posterior_rvars_npe",
        "compute_measures",
        "compute_measures_brmsfit",
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
    #   - brms fits are independent → no batching benefit
    #   - Simple parallelization: distribute work units across cores
    #
    # batch_size > 1 (NPE typical):
    #   - Group multiple work units into batches
    #   - NPE processes batch in single forward pass → efficient
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
        "worker_process_single",
        "worker_process_batch",
        "prepare_design_for_workers",
        "estimate_single_brms",
        "estimate_single_npe",
        "estimate_sequential_brms",
        "estimate_sequential_npe",
        "create_error_result",
        "estimate_posterior",
        "estimate_posterior_brms",
        "estimate_posterior_npe",
        "extract_posterior_rvars",
        "extract_posterior_rvars_brms",
        "extract_posterior_rvars_npe",
        "compute_measures",
        "compute_measures_brmsfit",
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
  error_rows <- !is.na(error_results$error)
  if (any(error_rows)) {
    error_msgs <- error_results[error_rows, "error"]
    cat("Simulation errors found:\n")
    cat(paste(unique(error_msgs), collapse = "\n"), "\n")
    cat("Number of errors:", sum(error_rows), "\n")
  }
  
  # Combine results - use bind_rows for robustness
  results_df_raw <- dplyr::bind_rows(results_raw_list)

  # Debug: Check combined results
  # Show if verbosity level is 2 (verbose)
  if (should_show(2)) {
    cli::cli_h3("Raw Results Summary")
    cli::cli_text("")

    dims <- dim(results_df_raw)
    cli::cli_text("{.strong Combined results dimensions:} {dims[1]} rows × {dims[2]} columns")

    if (!is.null(results_df_raw) && nrow(results_df_raw) > 0) {
      cli::cli_text("{.strong Column names:} {.val {colnames(results_df_raw)}}")

      result_info <- c(
        "Unique conditions" = length(unique(results_df_raw$id_cond)),
        "Unique iterations per condition" = n_sims
      )

      if ("id_analysis" %in% colnames(results_df_raw)) {
        analyses <- paste(sort(unique(results_df_raw$id_analysis)), collapse = ", ")
        result_info <- c(result_info, "Analyses per iteration" = analyses)
      }

      cli::cli_dl(result_info)
    }
    cli::cli_text("")
  }

  # Average across simulation runs
  results_df <- summarize_sims(results_df_raw, n_sims)
  # Add condition IDs and arguments to results
  results_df <- dplyr::full_join(conditions@conditions_grid, results_df, by = "id_cond")
  # End time
  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
  # Print elapsed time
  if (should_show(1)) {
    cli::cli_alert_success("Total analysis time: {round(as.numeric(elapsed_time), 2)} minutes")
  }
  
  # Update the S7 object with results
  x@summarized_results <- results_df
  x@raw_results <- results_df_raw
  x@elapsed_time <- as.numeric(elapsed_time)
  
  # Return the updated object
  return(x)
  
}


# S7 Method for Print

#' Print Method for rctbp_power_analysis Objects
#'
#' Displays a summary of a power analysis configuration object, including
#' analysis parameters, condition information, and design specifications.
#'
#' @param x An S7 object of class "rctbp_power_analysis"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @importFrom S7 method
#' @name print.rctbp_power_analysis
#' @export
S7::method(print, rctbp_power_analysis) <- function(x, ...) {
  report <- build_report.rctbp_power_analysis(x)
  render_report(report)
  invisible(x)
}
