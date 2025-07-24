#' #' Power Analysis for Bayesian RCTs (Legacy Function - Deprecated)
#' #'
#' #' @deprecated This function is deprecated. Please use the new S7 class approach:
#' #'   \code{power_config <- power_analysis(...); result <- run(power_config)}
#' #'   
#' #' This legacy function provides comprehensive power analysis across multiple conditions
#' #' but has been superseded by a more robust S7 class-based implementation that offers
#' #' better parameter validation, method dispatch, and extensibility.
#' #'
#' #' For new code, use:
#' #' \itemize{
#' #'   \item [run()] method to execute the analysis
#' #' }
#' #'
#' #' @param conditions An S7 conditions object created by [build_conditions()] containing:
#' #'   \itemize{
#' #'     \item design: An rctbp_design object with model specifications
#' #'     \item condition_arguments: List of prepared condition arguments for simulation
#' #'   }
#' #' @param design_prior Optional design prior for integrated power computation. Can be:
#' #'   \itemize{
#' #'     \item A string in brms prior syntax (e.g., "normal(0.3, 0.1)", "student_t(6, 0.5, 0.2)")
#' #'     \item An R function taking effect size as input (e.g., function(x) dnorm(x, 0.5, 0.2))
#' #'     \item NULL for no design prior (default)
#' #'   }
#' #' @param n_sims Number of MCMC iterations per condition (default: 500)
#' #' @param n_cores Number of parallel cores for condition execution (default: 1)
#' #' @param n_progress_updates Show progress every N conditions when running sequentially (default: 10)
#' #' @param verbose Logical. Whether to show detailed progress information (default: FALSE)
#' #' @param brms_args Arguments passed to brms for model fitting. Default includes 'algorithm' = "sampling", 'iter' = 500, 'warmup' = 250, 'chains' = 4, 'cores' = 1. User can override any of these or add additional arguments.
#' #' @param ... Additional arguments passed to brms for model fitting. These have the highest priority and will override both defaults and 'brms_args'.
#' #'
#' #' @details
#' #' This modernized function uses the new object-oriented API and provides several advantages:
#' #'
#' #' \strong{Unified Parameter Management:} All model and analysis specifications are contained
#' #' in the rctbp_design object, ensuring consistency and reducing parameter errors.
#' #'
#' #' \strong{Flexible Condition Specification:} Conditions can vary any combination of sample sizes,
#' #' effect sizes, interim analyses, allocation ratios, and other parameters independently.
#' #'
#' #' \strong{Full Parallelization:} All conditions are executed in parallel when n_cores > 1,
#' #' maximizing computational efficiency across the entire parameter grid.
#' #'
#' #' \strong{Named Effect Sizes:} Effect sizes must be specified as named lists matching the
#' #' target_params from the design object, enabling multi-parameter analysis.
#' #'
#' #' \strong{Extensible Design:} Easy to add new condition parameters (e.g., interim analyses)
#' #' without changing the function signature.
#' #'
#' #' @importFrom parallel detectCores makeCluster stopCluster parLapply clusterEvalQ clusterExport
#' #' @importFrom utils modifyList
#' #' @return A list of class "rctbayespower_sim_result" containing:
#' #' \itemize{
#' #'   \item design: The design object used for analysis
#' #'   \item conditions: The condition specifications used
#' #'   \item target_power_success: Target power level for success
#' #'   \item target_power_futility: Target power level for futility
#' #'   \item power_surface: Data frame with power results for all conditions
#' #'   \item optimal_combinations_success: Conditions achieving target success power
#' #'   \item optimal_combinations_futility: Conditions achieving target futility power
#' #'   \item sample_sizes: Unique sample sizes tested
#' #'   \item unique_effect_combinations: Unique effect size combinations tested
#' #'   \item detailed_results: Full simulation results for each condition
#' #' }
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Create an ANCOVA model and design
#' #' ancova_model <- build_model("ancova_cont_2arms")()
#' #' design <- build_design(
#' #'   build_model = ancova_model,
#' #'   target_params = "b_arms_treat",
#' #'   n_interim_analyses = 0,
#' #'   thresholds_success = 0.2,
#' #'   thresholds_futility = 0.0,
#' #'   p_sig_success = 0.975,
#' #'   p_sig_futility = 0.5
#' #' )
#' #'
#' #' # Create conditions grid
#' #' conditions <- expand_conditions(
#' #'   sample_sizes = c(100),
#' #'   b_arms_treat = c(0.5),
#' #' )
#' #'
#' #' # Run power analysis
#' #' result <- power_analysis(
#' #'   conditions = conditions,
#' #'   n_sims = 10, # Low for example
#' #'   n_cores = 1
#' #' )
#' #' }
#' power_analysis_legacy <- function(conditions,
#'                            design_prior = NULL,
#'                            n_sims = 500,
#'                            n_cores = 1,
#'                            n_progress_updates = 10,
#'                            verbose = FALSE,
#'                            brms_args = list(),
#'                            ...) {
#'   
#'   # Issue deprecation warning
#'   .Deprecated(
#'     new = "power_analysis() + run()",
#'     msg = paste(
#'       "power_analysis_legacy() is deprecated.",
#'       "Use: power_config <- power_analysis(...); result <- run(power_config)",
#'       "The new S7 class approach provides better validation and extensibility."
#'     )
#'   )
#'   # time start
#'   start_time <- Sys.time()
#'   # handlers for progress bar
#'   progressr::handlers(global = TRUE)
#'   
#'   # Validate conditions, must be of class rctbp_conditions (allow both namespaced and non-namespaced for testing)
#'   if (!inherits(conditions, "rctbayespower::rctbp_conditions") && !inherits(conditions, "rctbp_conditions")) {
#'     stop("'conditions' must be a valid rctbp_conditions object")
#'   }
#'   
#'   # Validate n_sims, must be a positive integer
#'   if (!is.numeric(n_sims) || n_sims <= 0) {
#'     stop("'n_sims' must be a positive number")
#'   }
#'   
#'   # Validate n_cores, must be a positive integer
#'   if (!is.numeric(n_cores) || n_cores <= 0) {
#'     n_cores <- 1
#'     warning("Invalid n_cores value. Using n_cores = 1.")
#'   }
#'   
#'   # expand condition_arguments_list to match n_sims
#'   condition_args_list <- rep(conditions@condition_arguments, each = n_sims)
#'   
#'   # Extract design from conditions object
#'   design <- conditions@design
#'   
#'   # Validate design object (allow both namespaced and non-namespaced for testing)
#'   if (!inherits(design, "rctbayespower::rctbp_design") && !inherits(design, "rctbp_design")) {
#'     stop("'design' must be a valid rctbp_design object")
#'   }
#'   
#'   # Set up parallelization -----------------------------------------------------
#'   total_runs <- length(condition_args_list)
#'   
#'   # Optional logging
#'   if (verbose) {
#'     cat("\n=== Power Analysis ===\n")
#'     cat("Conditions:\n")
#'     print(conditions@conditions_grid)
#'     cat("\n")
#'     cat("Conditions to test:",
#'         nrow(conditions@conditions_grid),
#'         "\n")
#'     cat("Simulations per condition:", n_sims, "\n")
#'     cat("Total simulations:", total_runs, "\n\n")
#'     
#'     if (n_cores > 1) {
#'       cat("Parallel execution using", n_cores, "cores:", "\n")
#'     } else {
#'       cat("Sequential execution:\n")
#'     }
#'   }
#'   
#'   # Progress bar for sequential execution
#'   
#'   
#'   # Execute simulations
#'   if (requireNamespace("pbapply", quietly = TRUE)) {
#'     if (n_cores > 1) {
#'       # Set up cluster
#'       cl <- parallel::makeCluster(n_cores, type = "PSOCK")
#'       
#'       # Load required packages on workers first
#'       parallel::clusterEvalQ(cl, {
#'         library(brms)
#'         library(dplyr)
#'         library(progressr)
#'         library(posterior)
#'         library(purrr)
#'         library(stats)
#'         library(utils)
#'         library(S7)
#'         library(stringr)
#'         
#'         # For development with devtools/pkgload, try to load the package
#'         tryCatch({
#'           if (requireNamespace("rctbayespower", quietly = TRUE)) {
#'             library(rctbayespower)
#'           }
#'         }, error = function(e) {
#'           # Package might not be properly installed, will rely on explicit exports
#'         })
#'       })
#'       
#'       # Extract design components for parallel workers (S7 objects don't serialize well)
#'       design_components <- list(
#'         target_params = design@target_params,
#'         thresholds_success = design@thresholds_success,
#'         thresholds_futility = design@thresholds_futility,
#'         p_sig_success = design@p_sig_success,
#'         p_sig_futility = design@p_sig_futility,
#'         n_interim_analyses = design@n_interim_analyses,
#'         interim_function = design@interim_function,
#'         design_name = design@design_name,
#'         model_data_simulation_fn = design@model@data_simulation_fn,
#'         model_brms_model = design@model@brms_model
#'       )
#'       
#'       # Export required objects to cluster
#'       parallel::clusterExport(
#'         cl,
#'         varlist = c(
#'           "condition_args_list",
#'           "design_components", 
#'           "brms_args"
#'         ),
#'         envir = environment()
#'       )
#'       
#'       # Export package functions - try multiple approaches for robustness
#'       functions_to_export <- c(
#'         "simulate_single_run",
#'         "compute_measures_brmsfit", 
#'         "calculate_mcse_power",
#'         "calculate_mcse_mean",
#'         "calculate_mcse_integrated_power"
#'       )
#'       
#'       # Try multiple approaches to export functions to workers
#'       export_success <- FALSE
#'       
#'       # Method 1: Try namespace export first (for installed package)
#'       tryCatch({
#'         ns <- asNamespace("rctbayespower")
#'         # Check if all functions exist in namespace
#'         all_exist <- all(sapply(functions_to_export, exists, envir = ns))
#'         if (all_exist) {
#'           parallel::clusterExport(cl, varlist = functions_to_export, envir = ns)
#'           export_success <- TRUE
#'           if (verbose) cat("Functions exported from package namespace\n")
#'         }
#'       }, error = function(e) {
#'         if (verbose) cat("Namespace export failed:", e$message, "\n")
#'       })
#'       
#'       # Method 2: Try getting functions from current environment (for devtools/pkgload)
#'       if (!export_success) {
#'         # Get function objects directly and assign them on workers
#'         function_objects <- list()
#'         for (fn_name in functions_to_export) {
#'           # Try multiple locations to find the function
#'           fn_obj <- NULL
#'           
#'           # Try namespace first
#'           tryCatch({
#'             ns <- asNamespace("rctbayespower")
#'             if (exists(fn_name, envir = ns)) {
#'               fn_obj <- get(fn_name, envir = ns)
#'             }
#'           }, error = function(e) {})
#'           
#'           # Try global environment if not found in namespace
#'           if (is.null(fn_obj) && exists(fn_name, envir = .GlobalEnv)) {
#'             fn_obj <- get(fn_name, envir = .GlobalEnv)
#'           }
#'           
#'           # Try current environment if still not found
#'           if (is.null(fn_obj) && exists(fn_name, envir = environment())) {
#'             fn_obj <- get(fn_name, envir = environment())
#'           }
#'           
#'           if (!is.null(fn_obj)) {
#'             function_objects[[fn_name]] <- fn_obj
#'           }
#'         }
#'         
#'         # Export the function objects we found
#'         if (length(function_objects) > 0) {
#'           for (fn_name in names(function_objects)) {
#'             parallel::clusterCall(cl, function(name, obj) {
#'               assign(name, obj, envir = .GlobalEnv)
#'             }, fn_name, function_objects[[fn_name]])
#'           }
#'           export_success <- TRUE
#'           if (verbose) cat("Functions exported as objects:", paste(names(function_objects), collapse = ", "), "\n")
#'         }
#'       }
#'       
#'       if (!export_success && verbose) {
#'         cat("Warning: Could not export all required functions to workers\n")
#'       }
#'     } else{
#'       cl <- NULL
#'     }
#'     
#'     # Parallel -----------------------------------------------------------------
#'     results_raw_list <-
#'       pbapply::pblapply(cl = cl, seq_along(condition_args_list), function(i) {
#'         # Top-level error capture for any issues in parallel worker
#'         tryCatch({
#'           # Test basic functionality first
#'           if (!exists("condition_args_list") || !exists("design_components")) {
#'             stop("Required objects not found in worker environment")
#'           }
#'           
#'           # Check and load required functions if they don't exist
#'           required_functions <- c("simulate_single_run", "compute_measures_brmsfit", 
#'                                  "calculate_mcse_power", "calculate_mcse_mean", 
#'                                  "calculate_mcse_integrated_power")
#'           
#'           missing_functions <- c()
#'           for (fn_name in required_functions) {
#'             if (!exists(fn_name)) {
#'               # Try to get from package namespace
#'               tryCatch({
#'                 if (requireNamespace("rctbayespower", quietly = TRUE)) {
#'                   assign(fn_name, get(fn_name, envir = asNamespace("rctbayespower")), envir = .GlobalEnv)
#'                 } else {
#'                   missing_functions <- c(missing_functions, fn_name)
#'                 }
#'               }, error = function(e) {
#'                 missing_functions <- c(missing_functions, fn_name)
#'               })
#'             }
#'           }
#'           
#'           if (length(missing_functions) > 0) {
#'             stop("Required functions not found in worker environment: ", paste(missing_functions, collapse = ", "))
#'           }
#'           
#'           if (i > length(condition_args_list)) {
#'             stop("Index out of bounds for condition_args_list")
#'           }
#'           
#'           # Simulate single run and compute measures with detailed error capture
#'           df_measures <- tryCatch({
#'             simulate_single_run(
#'               condition_arguments = condition_args_list[[i]],
#'               id_sim = i,
#'               design = design_components,
#'               brms_args = brms_args
#'             )
#'           }, error = function(e) {
#'             # Return error info for debugging
#'             data.frame(
#'               parameter = "ERROR",
#'               threshold_success = NA_real_,
#'               threshold_futility = NA_real_,
#'               success_prob = NA_real_,
#'               futility_prob = NA_real_,
#'               power_success = NA_real_,
#'               power_futility = NA_real_,
#'               median = NA_real_,
#'               mad = NA_real_,
#'               mean = NA_real_,
#'               sd = NA_real_,
#'               rhat = NA_real_,
#'               ess_bulk = NA_real_,
#'               ess_tail = NA_real_,
#'               id_sim = i,
#'               id_cond = if(!is.null(condition_args_list[[i]]$id_cond)) condition_args_list[[i]]$id_cond else NA_integer_,
#'               converged = 0L,
#'               error = paste("simulate_single_run error:", e$message)
#'             )
#'           })
#'           
#'           return(df_measures)
#'           
#'         }, error = function(e) {
#'           # Top-level worker error
#'           data.frame(
#'             parameter = "ERROR",
#'             threshold_success = NA_real_,
#'             threshold_futility = NA_real_,
#'             success_prob = NA_real_,
#'             futility_prob = NA_real_,
#'             power_success = NA_real_,
#'             power_futility = NA_real_,
#'             median = NA_real_,
#'             mad = NA_real_,
#'             mean = NA_real_,
#'             sd = NA_real_,
#'             rhat = NA_real_,
#'             ess_bulk = NA_real_,
#'             ess_tail = NA_real_,
#'             id_sim = i,
#'             id_cond = NA_integer_,
#'             converged = 0L,
#'             error = paste("Worker error:", e$message)
#'           )
#'         })
#'       })
#'     
#'     if (n_cores > 1) {
#'       # Stop cluster after use
#'     parallel::stopCluster(cl)
#'     }
#'     
#'     
#'   } else {
#'     # Sequential ---------------------------------------------------------------
#'     results_raw_list <- lapply(seq_along(condition_args_list), function(i) {
#'       # Simulate single run and compute measures
#'       df_measures <- simulate_single_run(
#'         condition_arguments = condition_args_list[[i]],
#'         id_sim = i,
#'         design = design,
#'         brms_args = brms_args
#'       )
#'       
#'       return(df_measures)
#'     })
#'   }
#'   # Debug: Check what we got back
#'   if (verbose) {
#'     cat("\nResults list length:", length(results_raw_list), "\n")
#'     cat("First condition_args structure:\n")
#'     if (length(condition_args_list) > 0) {
#'       str(condition_args_list[[1]])
#'     }
#'     if (length(results_raw_list) > 0) {
#'       cat("First result class:", class(results_raw_list[[1]]), "\n")
#'       if (!is.null(results_raw_list[[1]])) {
#'         cat("First result dimensions:", dim(results_raw_list[[1]]), "\n")
#'       } else {
#'         cat("First result is NULL\n")
#'       }
#'     }
#'   }
#'   
#'   # Filter out NULL results before combining
#'   results_raw_list <- results_raw_list[!sapply(results_raw_list, is.null)]
#'   
#'   if (length(results_raw_list) == 0) {
#'     stop("All simulations failed. Check your model and data simulation parameters.")
#'   }
#'   
#'   # Check for and report any error messages
#'   if (verbose || TRUE) {  # Always show errors for debugging
#'     error_results <- do.call(rbind, results_raw_list)
#'     error_rows <- !is.na(error_results$parameter) & error_results$parameter == "ERROR" & !is.na(error_results$error)
#'     if (any(error_rows)) {
#'       error_msgs <- error_results[error_rows, "error"]
#'       cat("Simulation errors found:\n")
#'       cat(paste(unique(error_msgs), collapse = "\n"), "\n")
#'     }
#'   }
#'   
#'   # Combine results - bind_rows is faster than do.call(rbind, ...)
#'   results_df_raw <- do.call(rbind, results_raw_list)
#'   
#'   # Debug: Check combined results
#'   if (verbose) {
#'     cat("Combined results dimensions:", dim(results_df_raw), "\n")
#'     if (!is.null(results_df_raw) && nrow(results_df_raw) > 0) {
#'       cat("Column names:", paste(colnames(results_df_raw), collapse = ", "), "\n")
#'     }
#'   }
#'   
#'   # Average across simulation runs
#'   results_df <- summarize_sims(results_df_raw, n_sims)
#'   # Add condition IDs and arguments to results
#'   results_df <- dplyr::full_join(conditions@conditions_grid, results_df, by = "id_cond")
#'   # End time
#'   elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
#'   # print elapsed time
#'   if (verbose) {
#'     cat("\nTotal analysis time:",
#'         round(as.numeric(elapsed_time), 2),
#'         "minutes\n")
#'   }
#'   
#'   # prepare return list
#'   return_list <- list(
#'     results_df = results_df,
#'     results_df_raw = results_df_raw,
#'     design = design,
#'     conditions = conditions,
#'     n_sims = n_sims,
#'     elapsed_time = elapsed_time
#'   )
#'   # add class for S3 methods
#'   class(return_list) <- "rctbayespower_sim_result"
#'   
#'   # return the result
#'   invisible(return_list)
#' }
#' 
#' #' Power Analysis for Bayesian RCTs (S7 Class Constructor)
#' #'
#' #' Creates a power analysis configuration object using the modern S7 class system.
#' #' This approach provides better parameter validation, method dispatch, and
#' #' extensibility compared to the legacy functional approach.
#' #'
#' #' @param conditions An S7 conditions object created by [build_conditions()]
#' #' @param design_prior Optional design prior for integrated power computation
#' #' @param n_sims Number of MCMC iterations per condition (default: 500)
#' #' @param n_cores Number of parallel cores for condition execution (default: 1)
#' #' @param n_progress_updates Show progress every N conditions (default: 10)
#' #' @param verbose Logical. Whether to show detailed progress information (default: FALSE)
#' #' @param brms_args Arguments passed to brms for model fitting
#' #' @param ... Additional arguments passed to brms
#' #'
#' #' @details
#' #' This function creates an S7 power analysis configuration object that can be
#' #' executed using [run()]. The S7 approach provides:
#' #'
#' #' \strong{Better Validation:} All parameters are validated upon object creation
#' #' \strong{Method Dispatch:} Clean separation between configuration and execution
#' #' \strong{Extensibility:} Easy to add new analysis methods or parameters
#' #' \strong{Reusability:} Configuration objects can be modified and reused
#' #'
#' #' @return An S7 object of class "rctbp_power_analysis"
#' #' @export
#' #' @seealso [run.rctbp_power_analysis()], [build_conditions()], [build_design()]
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Create power analysis configuration
#' #' power_config <- power_analysis(
#' #'   conditions = my_conditions,
#' #'   n_sims = 100,
#' #'   n_cores = 2
#' #' )
#' #'
#' #' # Execute the analysis
#' #' result <- run(power_config)
#' #'
#' #' # Print results
#' #' print(result)
#' #' }
#' power_analysis <- function(conditions,
#'                           design_prior = NULL,
#'                           n_sims = 500,
#'                           n_cores = 1,
#'                           n_progress_updates = 10,
#'                           verbose = FALSE,
#'                           brms_args = list(),
#'                           ...) {
#'   
#'   # Merge additional arguments into brms_args
#'   dots <- list(...)
#'   if (length(dots) > 0) {
#'     brms_args <- utils::modifyList(brms_args, dots)
#'   }
#'   
#'   # Create S7 object - validation happens automatically
#'   rctbp_power_analysis(
#'     conditions = conditions,
#'     design_prior = design_prior,
#'     n_sims = n_sims,
#'     n_cores = n_cores,
#'     n_progress_updates = n_progress_updates,
#'     verbose = verbose,
#'     brms_args = brms_args
#'   )
#' }
#' 
#' 
#' #
#' #
#' # # Design Prior ---------------------------------------------------------------
#' # # Set design prior parameters to NULL initially
#' # design_prior_parsed <- NULL
#' # weight_fn <- NULL
#' # weight_type <- "none"
#' #
#' # # If multiple target parameters: do not use design prior
#' # if (length(design$target_params) > 1 && !is.null(design_prior)) {
#' #   warning(
#' #     "Design prior is not supported for multiple target parameters. Ignoring design prior."
#' #   )
#' # } else {
#' #   # Parse and validate design prior for integrated power
#' #   design_prior_parsed <- NULL
#' #   weight_fn <- NULL
#' #   weight_type <- "none"
#' #   if (!is.null(design_prior)) {
#' #     effect_names <- conditions$condtio
#' #     # Extract all unique effect sizes for prior parsing
#' #     all_effects_for_prior <- unique(conditions@conditions_grid[, design$target_params])
#' #     design_prior_parsed <- parse_design_prior(design_prior, all_effects_for_prior, verbose = TRUE)
#' #     weight_fn <- design_prior_parsed$weight_fn
#' #     weight_type <- design_prior_parsed$weight_type
#' #   }
#' #
