#' Power Analysis for Bayesian RCTs (New API)
#'
#' Comprehensive power analysis across multiple conditions using the new object-oriented
#' API. This function provides flexible power analysis by varying sample sizes, effect
#' sizes, interim analyses, and other parameters across a grid of conditions.
#'
#' This implementation is optimized for performance through:
#' - Direct index-based parallel execution (no intermediate data structures)
#' - Efficient data combination using dplyr::bind_rows
#' - Minimal console output (controlled by verbose parameter)
#' - Simplified cluster setup without OS detection overhead
#'
#' @param conditions A conditions object created by [build_conditions()] containing:
#'   \itemize{
#'     \item design: An rctbayespower_design object with model specifications
#'     \item condition_arguments: List of prepared condition arguments for simulation
#'   }
#' @param design_prior Optional design prior for integrated power computation. Can be:
#'   \itemize{
#'     \item A string in brms prior syntax (e.g., "normal(0.3, 0.1)", "student_t(6, 0.5, 0.2)")
#'     \item An R function taking effect size as input (e.g., function(x) dnorm(x, 0.5, 0.2))
#'     \item NULL for no design prior (default)
#'   }
#' @param n_simulations Number of MCMC iterations per condition (default: 500)
#' @param n_cores Number of parallel cores for condition execution (default: 1)
#' @param n_progress_updates Show progress every N conditions when running sequentially (default: 10)
#' @param verbose Logical. Whether to show detailed progress information (default: FALSE)
#' @param brms_args Arguments passed to brms for model fitting. Default includes 'algorithm' = "sampling", 'iter' = 500, 'warmup' = 250, 'chains' = 4, 'cores' = 1. User can override any of these or add additional arguments.
#' @param ... Additional arguments passed to brms for model fitting. These have the highest priority and will override both defaults and 'brms_args'.
#'
#' @details
#' This modernized function uses the new object-oriented API and provides several advantages:
#'
#' \strong{Unified Parameter Management:} All model and analysis specifications are contained
#' in the rctbayespower_design object, ensuring consistency and reducing parameter errors.
#'
#' \strong{Flexible Condition Specification:} Conditions can vary any combination of sample sizes,
#' effect sizes, interim analyses, allocation ratios, and other parameters independently.
#'
#' \strong{Full Parallelization:} All conditions are executed in parallel when n_cores > 1,
#' maximizing computational efficiency across the entire parameter grid.
#'
#' \strong{Named Effect Sizes:} Effect sizes must be specified as named lists matching the
#' target_params from the design object, enabling multi-parameter analysis.
#'
#' \strong{Extensible Design:} Easy to add new condition parameters (e.g., interim analyses)
#' without changing the function signature.
#'
#' @importFrom parallel detectCores makeCluster stopCluster parLapply clusterEvalQ clusterExport
#' @importFrom utils modifyList
#' @return A list of class "rctbayespower_sim_result" containing:
#' \itemize{
#'   \item design: The design object used for analysis
#'   \item conditions: The condition specifications used
#'   \item target_power_success: Target power level for success
#'   \item target_power_futility: Target power level for futility
#'   \item power_surface: Data frame with power results for all conditions
#'   \item optimal_combinations_success: Conditions achieving target success power
#'   \item optimal_combinations_futility: Conditions achieving target futility power
#'   \item sample_sizes: Unique sample sizes tested
#'   \item unique_effect_combinations: Unique effect size combinations tested
#'   \item detailed_results: Full simulation results for each condition
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Create an ANCOVA model and design
#' ancova_model <- build_model("ancova_cont_2arms")()
#' design <- build_design(
#'   build_model = ancova_model,
#'   target_params = "b_arms_treat",
#'   n_interim_analyses = 0,
#'   thresholds_success = 0.2,
#'   thresholds_futility = 0.0,
#'   p_sig_success = 0.975,
#'   p_sig_futility = 0.5
#' )
#'
#' # Create conditions grid
#' conditions <- expand_conditions(
#'   sample_sizes = c(100),
#'   b_arms_treat = c(0.5),
#' )
#'
#' # Run power analysis
#' result <- power_analysis(
#'   conditions = conditions,
#'   n_simulations = 10, # Low for example
#'   n_cores = 1
#' )
#' }
power_analysis <- function(conditions,
                           design_prior = NULL,
                           n_simulations = 500,
                           n_cores = 1,
                           n_progress_updates = 10,
                           verbose = FALSE,
                           brms_args = list(),
                           ...) {
  # time start
  start_time <- Sys.time()

  # Validate conditions, must be of class rctbayespower_conditions
  if (!inherits(conditions, "rctbayespower_conditions")) {
    stop("'conditions' must be a valid rctbayespower_conditions object")
  }

  # Validate n_simulations, must be a positive integer
  if (!is.numeric(n_simulations) || n_simulations <= 0) {
    stop("'n_simulations' must be a positive number")
  }

  # Validate n_cores, must be a positive integer
  if (!is.numeric(n_cores) || n_cores <= 0) {
    n_cores <- 1
    warning("Invalid n_cores value. Using n_cores = 1.")
  }

  # expand condition_arguments_list to match n_simulations
  condition_args_list <- rep(conditions$condition_arguments, each = n_simulations)

  # Extract design from conditions object
  design <- conditions$design

  # Validate design object
  if (!inherits(design, "rctbayespower_design")) {
    stop("'design' must be a valid rctbayespower_design object")
  }

  # Set up parallelization -----------------------------------------------------
  total_runs <- length(condition_args_list)

  # Optional logging
  if (verbose) {
    cat("\n=== Power Analysis ===\n")
    cat("Total simulations:", total_runs, "\n")
    cat("Total conditions to test:",
        attr(conditions, "n_conditions"),
        "\n")
    cat("Conditions:\n")
    print(conditions$conditions_grid)
    cat("\n")
    cat("Number of simulations per condition:", n_simulations, "\n")
    cat(
      "Number of total simulations :",
      n_simulations * attr(conditions, "n_conditions"),
      "\n"
    )
    if (n_cores > 1) {
      cat("Parallel cores:", n_cores, "\n")
    }
  }

  # Progress bar for sequential execution
  pb <- NULL
  if (n_cores == 1) {
    pb <- utils::txtProgressBar(min = 0,
                                max = total_runs,
                                style = 3)
  }

  # Execute simulations
  if (n_cores > 1) {
    # Set up cluster
    cl <- parallel::makeCluster(n_cores, type = "PSOCK")

    # Load required packages on workers
    parallel::clusterEvalQ(cl, {
      library(rctbayespower)
      library(brms)
      library(dplyr)
    })

    # Export required objects to cluster
    parallel::clusterExport(
      cl,
      varlist = c(
        "condition_args_list",
        "design",
        "brms_args",
        "simulate_single_run",
        "compute_measures_brmsfit"
      ),
      envir = environment()
    )

    # Run parallel computation - direct index access is faster
    results_raw_list <- parallel::parLapply(cl, seq_along(condition_args_list), function(i) {

      print(condition_args_list[[i]])

      # Simulate single run and compute measures
      df_measures <- simulate_single_run(
        condition_arguments = condition_args_list[[i]],
        id_sim = i,
        design = design,
        brms_args = brms_args
      )
      return(df_measures)
    })

    parallel::stopCluster(cl)
  } else {
    # Sequential execution with progress bar
    results_raw_list <- lapply(seq_along(condition_args_list), function(i) {
      args <- condition_args_list[[i]]
      # Simulate single run and compute measures
      df_measures <- simulate_single_run(
        condition_arguments = args,
        id_sim = i,
        design = design,
        brms_args = brms_args
      )
      if (!is.null(pb)) {
        utils::setTxtProgressBar(pb, i)
      }
      return(df_measures)
    })
    if (!is.null(pb)) {
      close(pb)
    }
  }

  # Combine results - bind_rows is faster than do.call(rbind, ...)
  results_df_raw <- do.call(rbind, results_raw_list)

  # Average across simulation runs
  results_df <- summarize_sims(results_df_raw, n_simulations)
  # Add condition IDs and arguments to results
  results_df <- dplyr::full_join(conditions$conditions_grid, results_df, by = "id_cond")
  # End time
  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
  # print elapsed time
  if (verbose) {
    cat("\nTotal analysis time:",
        round(as.numeric(elapsed_time), 2),
        "minutes\n")
  }

  # prepare return list
  return_list <- list(
    results_df = results_df,
    results_df_raw = results_df_raw,
    design = design,
    conditions = conditions,
    n_simulations = n_simulations,
    elapsed_time = elapsed_time
  )
  # add class for S3 methods
  class(return_list) <- "rctbayespower_sim_result"

  # return the result
  invisible(return_list)
}




#
#
# # Design Prior ---------------------------------------------------------------
# # Set design prior parameters to NULL initially
# design_prior_parsed <- NULL
# weight_fn <- NULL
# weight_type <- "none"
#
# # If multiple target parameters: do not use design prior
# if (length(design$target_params) > 1 && !is.null(design_prior)) {
#   warning(
#     "Design prior is not supported for multiple target parameters. Ignoring design prior."
#   )
# } else {
#   # Parse and validate design prior for integrated power
#   design_prior_parsed <- NULL
#   weight_fn <- NULL
#   weight_type <- "none"
#   if (!is.null(design_prior)) {
#     effect_names <- conditions$condtio
#     # Extract all unique effect sizes for prior parsing
#     all_effects_for_prior <- unique(conditions$conditions_grid[, design$target_params])
#     design_prior_parsed <- parse_design_prior(design_prior, all_effects_for_prior, verbose = TRUE)
#     weight_fn <- design_prior_parsed$weight_fn
#     weight_type <- design_prior_parsed$weight_type
#   }
#
