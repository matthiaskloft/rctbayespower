# S7 Class Definition for Power Analysis Configuration
#' @importFrom S7 new_class class_any class_numeric class_logical class_list class_character

rctbp_power_analysis <- S7::new_class(
  "rctbp_power_analysis",
  properties = list(
    n_sims = S7::new_property(S7::class_numeric, default = 1),
    n_cores = S7::new_property(S7::class_numeric, default = 1),
    verbose = S7::new_property(class = S7::class_logical, default = TRUE),
    brms_args = S7::class_list | NULL,
    design_prior = S7::new_property(S7::class_character |
                                      S7::class_function | NULL, default = NULL),
    conditions = S7::class_any,
    design = S7::new_property(getter = function(self) self@conditions@design),
    summarized_results = S7::class_data.frame,
    raw_results = S7::class_data.frame,
    elapsed_time = S7::new_property(class = S7::class_numeric, default = NA_real_)
  ),
  validator = function(self) {
    # Validate conditions object
    if (!inherits(self@conditions, "rctbayespower::rctbp_conditions") &&
        !inherits(self@conditions, "rctbp_conditions")) {
      stop("'conditions' must be a valid rctbp_conditions object")
    }
    
    # Validate n_sims, positive whole number
    if (!is.numeric(self@n_sims) ||
        length(self@n_sims) != 1 ||
        self@n_sims <= 0 || self@n_sims != round(self@n_sims)) {
      stop("'n_sims' must be a positive whole number")
    }
    
    # Validate n_cores, positive whole number, < available cores
    if (!is.numeric(self@n_cores) ||
        length(self@n_cores) != 1 ||
        self@n_cores <= 0 ||
        self@n_cores != round(self@n_cores)) {
      stop("'n_cores' must be a positive whole number")
    }
    # Validate n_cores <= parallel::detectCores()
    if (self@n_cores > parallel::detectCores()) {
      stop(
        "'n_cores' must not exceed available cores. We recommend using at most detectCores() - 1 cores."
      )
    }
    
    # Validate verbose
    if (!is.logical(self@verbose) || length(self@verbose) != 1) {
      stop("'verbose' must be a logical value")
    }
    
    # Validate brms_args
    if (!is.list(self@brms_args)) {
      stop("'brms_args' must be a list")
    }
    
    # If all validations pass, return NULL
    NULL
  }
)

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
#'     \item verbose: Logical indicating whether to display progress information
#'       and analysis details (default TRUE)
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
S7::method(run, rctbp_power_analysis) <- function(x, ...) {
  # Time start
  start_time <- Sys.time()
  
  # Overwrite object parameters with dots if provided
  if (length(list(...)) > 0) {
    # Recreate the S7 object with updated parameters
    x <- update_S7_with_dots(x, ...)
  }
  
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
      warning("Do not use multiple cores for brms when running simulations in parallel!")
    }
    # update brms_args in the object
    x@conditions@design@model@brms_model <- do.call(function(...) {
      stats::update(object = x@conditions@design@model@brms_model, ...)
    }, x@brms_args)
  } else if (backend == "npe") {
    # For NPE backend, backend_args are already in model@backend_args
    if (verbose) {
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
  verbose <- x@verbose
  
  # Extract parameters from the object
  conditions <- x@conditions
  design <- x@conditions@design
  design_prior <- x@design_prior
  
  # Prepare design for parallel workers (S7 objects don't serialize well)
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
  if (verbose) {
    cat("\n=== Power Analysis ===\n")
    cat("Conditions:\n")
    print(conditions@conditions_grid)
    cat("\n")
    cat("Backend:", backend, "\n")
    cat("Batch size:", batch_size, "\n")
    cat("Conditions to test:", nrow(conditions@conditions_grid), "\n")
    cat("Simulations per condition:", n_sims, "\n")
    cat("Total work units:", total_work_units, "\n")
    if (!is.null(design@analysis_at) && length(design@analysis_at) > 0) {
      cat("Interim analyses at: n =", paste(design@analysis_at, collapse = ", "), "\n")
      cat("Adaptive design:", design@adaptive, "\n")
    }
    cat("\n")
  }
  
  # Execute simulations
  if (requireNamespace("pbapply", quietly = TRUE)) {
    # set cl to NULL for default sequential execution
    cl <- NULL
    if (n_cores > 1) {
      message("Setting up parallel cluster with ", n_cores, " cores ...")
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

      # Export package functions - try multiple approaches for robustness
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
      
      # Try multiple approaches to export functions to workers
      export_success <- FALSE
      
      # Method 1: Try namespace export first (for installed package)
      tryCatch({
        ns <- asNamespace("rctbayespower")
        # Check if all functions exist in namespace
        all_exist <- all(sapply(functions_to_export, exists, envir = ns))
        if (all_exist) {
          parallel::clusterExport(cl, varlist = functions_to_export, envir = ns)
          export_success <- TRUE
          if (verbose)
            cat("Functions exported from package namespace\n\n")
        }
      }, error = function(e) {
        if (verbose)
          cat("Namespace export failed:", e$message, "\n\n")
      })
      
      if (!export_success && verbose) {
        cat("Warning: Could not export all required functions to workers\n\n")
      }
    }
    
    # Parallel execution
    # Choose between single and batch processing
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
    # fallback on lapply() if pbapply is not available
    message("Package 'pbapply' not found. Running simulations without progress bar.")

    if (batch_size == 1L) {
      # Single processing
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
    } else {
      # Batch processing
      batches <- split(work_units, ceiling(seq_along(work_units) / batch_size))
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
  
  # Filter out NULL results before combining
  results_raw_list <- results_raw_list[!sapply(results_raw_list, is.null)]
  
  if (length(results_raw_list) == 0) {
    stop("All simulations failed. Check your model and data simulation parameters.")
  }
  
  # Check for and report any error messages
  if (verbose || TRUE) {
    # Always show errors for debugging
    error_results <- dplyr::bind_rows(results_raw_list)
    error_rows <- !is.na(error_results$error)
    if (any(error_rows)) {
      error_msgs <- error_results[error_rows, "error"]
      cat("Simulation errors found:\n")
      cat(paste(unique(error_msgs), collapse = "\n"), "\n")
      cat("Number of errors:", sum(error_rows), "\n")
    }
  }
  
  # Combine results - use bind_rows for robustness
  results_df_raw <- dplyr::bind_rows(results_raw_list)

  # Debug: Check combined results
  if (verbose) {
    cat("\n=== Raw Results Summary ===\n")
    cat("Combined results dimensions:", dim(results_df_raw), "\n")
    if (!is.null(results_df_raw) && nrow(results_df_raw) > 0) {
      cat("Column names:", paste(colnames(results_df_raw), collapse = ", "), "\n")
      cat("Unique conditions:", length(unique(results_df_raw$id_cond)), "\n")
      cat("Unique iterations per condition:", n_sims, "\n")
      if ("id_analysis" %in% colnames(results_df_raw)) {
        cat("Analyses per iteration:",
            paste(sort(unique(results_df_raw$id_analysis)), collapse = ", "), "\n")
      }
    }
    cat("\n")
  }

  # Average across simulation runs
  results_df <- summarize_sims(results_df_raw, n_sims)
  # Add condition IDs and arguments to results
  results_df <- dplyr::full_join(conditions@conditions_grid, results_df, by = "id_cond")
  # End time
  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
  # Print elapsed time
  if (verbose) {
    cat("\nTotal analysis time:",
        round(as.numeric(elapsed_time), 2),
        "minutes\n")
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
  cat("\nS7 Object of class: 'rctbp_power_analysis'\n")
  cat("==================================================\n")
  
  cat("\n=== Design Summary ===\n")
  design <- x@conditions@design
  cat("Target parameters:",
      paste(design@target_params, collapse = ", "),
      "\n")
  cat("Success probability threshold:", design@p_sig_success, "\n")
  cat("Futility probability threshold:",
      design@p_sig_futility,
      "\n")
  
  
  # Check if analysis has been run
  has_results <- nrow(x@summarized_results) > 0 ||
    nrow(x@raw_results) > 0
  
  if (has_results) {
    cat("STATUS: [COMPLETED] Analysis completed\n\n")
    
    # Results Summary
    cat("=== Results Summary ===\n")
    results_df <- x@summarized_results
    cat("Analysis runtime:",
        if (!is.null(x@elapsed_time))
          paste0(round(x@elapsed_time, 2), " minutes")
        else
          "Not available",
        "\n")
    cat("Conditions analyzed:",
        nrow(x@conditions@conditions_grid),
        "\n")
    cat("Simulations per condition:", x@n_sims, "\n")
    cat("Total simulations:",
        nrow(x@conditions@conditions_grid) * x@n_sims,
        "\n")
    
    # Quick power overview
    if (!is.null(results_df)) {
      power_cols <- intersect(names(results_df),
                              c("power_success", "power_futility"))
      if (length(power_cols) > 0) {
        cat("\nPower ranges:\n")
        for (col in power_cols) {
          power_range <- range(results_df[[col]], na.rm = TRUE)
          cat("  ",
              gsub("power_", "", col),
              ":",
              paste0(
                round(power_range[1] * 100, 1),
                "% - ",
                round(power_range[2] * 100, 1),
                "%"
              ),
              "\n")
        }
      }
    }
    
    cat("\n=== Available Actions ===\n")
    cat("- plot() - Create visualizations\n")
    cat("- power_config@summarized_results - Access summarized results\n")
    cat("- power_config@raw_results - Access raw simulation results\n")
    
  } else {
    cat("STATUS: [PENDING] Analysis not yet run\n\n")
    
    # Analysis Configuration
    cat("=== Analysis Configuration ===\n")
    cat("Number of simulations per condition:", x@n_sims, "\n")
    cat("Number of cores for parallel execution:", x@n_cores, "\n")
    cat("Verbose output:", x@verbose, "\n")
    cat(
      "Design prior:",
      if (is.null(x@design_prior))
        "None"
      else
        if (is.function(x@design_prior))
          "Custom function"
      else
        as.character(x@design_prior),
      "\n"
    )
    
    if (length(x@brms_args) > 0) {
      cat("BRMS arguments:\n")
      for (arg_name in names(x@brms_args)) {
        cat("  ", arg_name, ":", x@brms_args[[arg_name]], "\n")
      }
    }
    
    cat("\n=== Analysis Preview ===\n")
    n_conditions <- nrow(x@conditions@conditions_grid)
    total_sims <- n_conditions * x@n_sims
    cat("Total conditions:", n_conditions, "\n")
    cat("Total simulations:", total_sims, "\n")
    
    cat("\n=== Available Actions ===\n")
    cat("- run() - Execute the analysis\n")
    cat("- power_config@conditions - View condition details\n")
  }
  
  invisible(x)
}
