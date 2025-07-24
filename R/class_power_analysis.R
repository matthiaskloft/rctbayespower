# S7 Class Definition for Power Analysis Configuration
#' @importFrom S7 new_class class_any class_numeric class_logical class_list class_character

rctbp_power_analysis <- S7::new_class(
  "rctbp_power_analysis",
  properties = list(
    n_sims = S7::class_numeric,
    n_cores = S7::class_numeric,
    verbose = S7::class_logical,
    brms_args = S7::class_list,
    design_prior = S7::class_character | S7::class_function | NULL,
    conditions = S7::class_any,
    design = S7::new_property(
      getter = function(self)
        self@conditions@design
    ),
    model = S7::new_property(
      getter = function(self)
        self@conditions@design@model
    ),
    summarized_results = S7::class_data.frame | NULL,
    raw_results = S7::class_data.frame | NULL,
    elapsed_time = S7::class_numeric | NULL
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
        self@n_cores <= 0 || self@n_cores != round(self@n_cores)) {
      stop("'n_cores' must be a positive whole number")
    }
    # Validate n_cores <= parallel::detectCores()
    if (self@n_cores > parallel::detectCores()) {
      stop("'n_cores' must not exceed available cores. We recommend using at most detectCores() - 1 cores.")
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

#' Run Analysis Objects
#'
#' Generic function for executing analysis objects. This function provides a
#' unified interface for running different types of analysis configurations.
#'
#' @param object An S7 object to run (e.g., rctbp_power_analysis)
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
run <- S7::new_generic("run", "object")

#' Run Power Analysis
#'
#' Executes the Bayesian power analysis using the configuration specified in
#' the rctbp_power_analysis object. This method contains the core simulation
#' logic and returns comprehensive results.
#'
#' @param object An S7 object of class "rctbp_power_analysis"
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
S7::method(run, rctbp_power_analysis) <- function(object, ...) {
  # Time start
  start_time <- Sys.time()
  
  # Overwrite object parameters with dots if provided
  if (length(list(...)) > 0) {
    # Recreate the S7 object with updated parameters
    object <- update_s7_with_dots(object, ...)
  }
  
  # Extract parameters from the object
  conditions <- object@conditions
  design_prior <- object@design_prior
  n_sims <- object@n_sims
  n_cores <- object@n_cores
  verbose <- object@verbose
  brms_args <- object@brms_args
  
  # Extract design components for parallel workers (S7 objects don't serialize well)
  design_components <- list(
    target_params = design@target_params,
    thresholds_success = design@thresholds_success,
    thresholds_futility = design@thresholds_futility,
    p_sig_success = design@p_sig_success,
    p_sig_futility = design@p_sig_futility,
    n_interim_analyses = design@n_interim_analyses,
    interim_function = design@interim_function,
    design_name = design@design_name,
    model_data_simulation_fn = object@model@data_simulation_fn,
    model_brms_model = object@model@brms_model
  )
  
  # Validate n_cores, must be a positive integer
  if (!is.numeric(n_cores) || n_cores <= 0) {
    n_cores <- 1
    warning("Invalid n_cores value. Using n_cores = 1.")
  }
  
  # Expand condition_arguments_list to match n_sims
  condition_args_list <- rep(conditions@condition_arguments, each = n_sims)
  
  # Extract design from power analysis object
  design <- object@design
  
  # Set up parallelization
  total_runs <- length(condition_args_list)
  
  # Optional logging
  if (verbose) {
    cat("\n=== Power Analysis ===\n")
    cat("Conditions:\n")
    print(conditions@conditions_grid)
    cat("\n")
    cat("Conditions to test:",
        nrow(conditions@conditions_grid),
        "\n")
    cat("Simulations per condition:", n_sims, "\n")
    cat("Total simulations:", total_runs, "\n\n")
    
    if (n_cores > 1) {
      cat("Parallel execution using", n_cores, "cores:", "\n")
    } else {
      cat("Sequential execution:\n")
    }
  }
  
  # Execute simulations
  if (requireNamespace("pbapply", quietly = TRUE)) {
    # set cl to NULL for default sequential execution
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
        varlist = c(
          "condition_args_list",
          "design_components",
          "brms_args"
        ),
        envir = environment()
      )
      
      # Export package functions - try multiple approaches for robustness
      functions_to_export <- c(
        "simulate_single_run",
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
            cat("Functions exported from package namespace\n")
        }
      }, error = function(e) {
        if (verbose)
          cat("Namespace export failed:", e$message, "\n")
      })
      
      # Method 2: Try getting functions from current environment (for devtools/pkgload)
      if (!export_success) {
        # Get function objects directly and assign them on workers
        function_objects <- list()
        for (fn_name in functions_to_export) {
          # Try multiple locations to find the function
          fn_obj <- NULL
          
          # Try namespace first
          tryCatch({
            ns <- asNamespace("rctbayespower")
            if (exists(fn_name, envir = ns)) {
              fn_obj <- get(fn_name, envir = ns)
            }
          }, error = function(e) {
            
          })
          
          # Try global environment if not found in namespace
          if (is.null(fn_obj) &&
              exists(fn_name, envir = .GlobalEnv)) {
            fn_obj <- get(fn_name, envir = .GlobalEnv)
          }
          
          # Try current environment if still not found
          if (is.null(fn_obj) &&
              exists(fn_name, envir = environment())) {
            fn_obj <- get(fn_name, envir = environment())
          }
          
          if (!is.null(fn_obj)) {
            function_objects[[fn_name]] <- fn_obj
          }
        }
        
        # Export the function objects we found
        if (length(function_objects) > 0) {
          for (fn_name in names(function_objects)) {
            parallel::clusterCall(cl, function(name, obj) {
              # NOTE: Assignment to .GlobalEnv is required for R parallel processing
              # This is the standard R idiom for making functions available to worker processes
              # The assignment occurs within the worker environment, not the main session
              assign(name, obj, envir = .GlobalEnv)
            }, fn_name, function_objects[[fn_name]])
          }
          export_success <- TRUE
          if (verbose)
            cat("Functions exported as objects:",
                paste(names(function_objects), collapse = ", "),
                "\n")
        }
      }
      
      if (!export_success && verbose) {
        cat("Warning: Could not export all required functions to workers\n")
      }
    }
    
    # Parallel execution
    results_raw_list <-
      pbapply::pblapply(cl = cl, seq_along(condition_args_list), function(i) {
        # Top-level error capture for any issues in parallel worker
        tryCatch({
          # Test basic functionality first
          if (!exists("condition_args_list") ||
              !exists("design_components")) {
            stop("Required objects not found in worker environment")
          }
          
          # Check and load required functions if they don't exist
          required_functions <- c(
            "simulate_single_run",
            "compute_measures_brmsfit",
            "calculate_mcse_power",
            "calculate_mcse_mean",
            "calculate_mcse_integrated_power"
          )
          
          missing_functions <- c()
          for (fn_name in required_functions) {
            if (!exists(fn_name)) {
              # Try to get from package namespace
              tryCatch({
                if (requireNamespace("rctbayespower", quietly = TRUE)) {
                  # NOTE: Assignment to .GlobalEnv is required for R parallel processing
                  # This makes package functions available within the worker process environment
                  assign(fn_name,
                         get(fn_name, envir = asNamespace("rctbayespower")),
                         envir = .GlobalEnv)
                } else {
                  missing_functions <- c(missing_functions, fn_name)
                }
              }, error = function(e) {
                missing_functions <- c(missing_functions, fn_name)
              })
            }
          }
          
          if (length(missing_functions) > 0) {
            stop(
              "Required functions not found in worker environment: ",
              paste(missing_functions, collapse = ", ")
            )
          }
          
          if (i > length(condition_args_list)) {
            stop("Index out of bounds for condition_args_list")
          }
          
          # Simulate single run and compute measures with detailed error capture
          df_measures <- simulate_single_run(
            condition_arguments = condition_args_list[[i]],
            id_sim = i,
            design = design_components,
            brms_args = brms_args
          )
          
          return(df_measures)
          
        }, error = function(e) {
          # Top-level worker error
          data.frame(
            parameter = "ERROR",
            threshold_success = NA_real_,
            threshold_futility = NA_real_,
            success_prob = NA_real_,
            futility_prob = NA_real_,
            power_success = NA_real_,
            power_futility = NA_real_,
            median = NA_real_,
            mad = NA_real_,
            mean = NA_real_,
            sd = NA_real_,
            rhat = NA_real_,
            ess_bulk = NA_real_,
            ess_tail = NA_real_,
            id_sim = i,
            id_cond = NA_integer_,
            converged = 0L,
            error = paste("Worker error:", e$message)
          )
        })
      })
    
    if (n_cores > 1) {
      # Stop cluster after use
      parallel::stopCluster(cl)
    }
    
  } else {
    # fallback on lapply() if pbapply is not available
    results_raw_list <- lapply(seq_along(condition_args_list), function(i) {
      # Simulate single run and compute measures
      df_measures <- simulate_single_run(
        condition_arguments = condition_args_list[[i]],
        id_sim = i,
        design = design_components,
        brms_args = brms_args
      )
      
      return(df_measures)
    })
  }
  
  # Debug: Check what we got back
  if (verbose) {
    cat("\nResults list length:", length(results_raw_list), "\n")
    cat("First condition_args structure:\n")
    if (length(condition_args_list) > 0) {
      str(condition_args_list[[1]])
    }
    if (length(results_raw_list) > 0) {
      cat("First result class:", class(results_raw_list[[1]]), "\n")
      if (!is.null(results_raw_list[[1]])) {
        cat("First result dimensions:",
            dim(results_raw_list[[1]]),
            "\n")
      } else {
        cat("First result is NULL\n")
      }
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
    error_results <- do.call(rbind, results_raw_list)
    error_rows <- !is.na(error_results$parameter) &
      error_results$parameter == "ERROR" &
      !is.na(error_results$error)
    if (any(error_rows)) {
      error_msgs <- error_results[error_rows, "error"]
      cat("Simulation errors found:\n")
      cat(paste(unique(error_msgs), collapse = "\n"), "\n")
    }
  }
  
  # Combine results - bind_rows is faster than do.call(rbind, ...)
  results_df_raw <- do.call(rbind, results_raw_list)
  
  # Debug: Check combined results
  if (verbose) {
    cat("Combined results dimensions:", dim(results_df_raw), "\n")
    if (!is.null(results_df_raw) && nrow(results_df_raw) > 0) {
      cat("Column names:", paste(colnames(results_df_raw), collapse = ", "), "\n")
    }
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
  object@summarized_results <- results_df
  object@raw_results <- results_df_raw
  object@elapsed_time <- as.numeric(elapsed_time)
  
  # Return the updated object
  return(object)
  
}


# S7 Method for Print
#' @importFrom S7 method

#' Print Method for rctbp_power_analysis Objects
#'
#' Displays a summary of a power analysis configuration object, including
#' analysis parameters, condition information, and design specifications.
#'
#' @param x An S7 object of class "rctbp_power_analysis"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @export
S7::method(print, rctbp_power_analysis) <- function(x, ...) {
  cat("\nS7 Object of class: 'rctbp_power_analysis'\n")
  cat("==================================================\n")
  
  cat("\n=== Design Summary ===\n")
  design <- x@design
  cat("Target parameters:",
      paste(design@target_params, collapse = ", "),
      "\n")
  cat("Success thresholds:",
      paste(design@thresholds_success, collapse = ", "),
      "\n")
  cat("Futility thresholds:",
      paste(design@thresholds_futility, collapse = ", "),
      "\n")
  cat("Success probability threshold:", design@p_sig_success, "\n")
  cat("Futility probability threshold:",
      design@p_sig_futility,
      "\n")
  
  
  # Check if analysis has been run
  has_results <- !is.null(x@summarized_results)
  
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