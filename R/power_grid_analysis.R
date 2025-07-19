#' Power Grid Analysis for Bayesian RCTs (New API)
#'
#' Comprehensive power analysis across multiple conditions using the new object-oriented
#' API. This function provides flexible power analysis by varying sample sizes, effect
#' sizes, interim analyses, and other parameters across a grid of conditions.
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
#' @param brms_args Arguments passed to brms for model fitting. Default includes 'algorithm' = "sampling", 'iter' = 500, 'warmup' = 250, 'chains' = 4, 'cores' = 1. User can override any of these or add additional arguments.
#' @param n_cores Number of parallel cores for condition execution (default: detectCores() - 1)
#' @param progress_updates Show progress every N conditions when running sequentially (default: 10)
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
#' @return A list of class "rctbayespower_grid" containing:
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
#' \donttest{
#' # Create an ANCOVA model and design
#' ancova_model <- build_model_ancova_cont()
#' design <- build_design(
#'   build_model = ancova_model,
#'   target_params = "b_grouptreat",
#'   n_interim_analyses = 0,
#'   thresholds_success = 0.2,
#'   thresholds_futility = 0.0,
#'   p_sig_success = 0.975,
#'   p_sig_futility = 0.5
#' )
#'
#' # Create conditions grid
#' conditions <- expand_conditions(
#'   sample_sizes = c(100, 200),
#'   effect_sizes_grid = list(
#'     list(b_grouptreat = 0.3),
#'     list(b_grouptreat = 0.5)
#'   ),
#'   n_interim_analyses = c(0, 1)
#' )
#'
#' # Run power grid analysis
#' result <- power_grid_analysis(
#'   conditions = conditions,
#'   n_simulations = 100, # Low for example
#'   n_cores = 1
#' )
#' }
power_grid_analysis <- function(conditions,
                                design_prior = NULL,
                                n_simulations = 500,
                                brms_args = list(),
                                n_cores = parallel::detectCores() - 1,
                                progress_updates = 10,
                                ...) {
  # Validate inputs
  if (!is.list(conditions) || length(conditions) == 0) {
    stop("'conditions' must be a non-empty list of condition specifications")
  }
  
  # Extract design from conditions and validate
  if (is.null(conditions$design)) {
    stop("'conditions' must contain a 'design' component")
  }
  
  design <- conditions$design
  if (!inherits(design, "rctbayespower_design")) {
    stop("'conditions$design' must be a valid rctbayespower_design object")
  }
  
  if (!is.numeric(n_simulations) || n_simulations <= 0) {
    stop("'n_simulations' must be a positive number")
  }
  
  if (!is.numeric(n_cores) || n_cores <= 0) {
    n_cores <- 1
    warning("Invalid n_cores value. Using n_cores = 1.")
  }
  
  # Extract condition_arguments from conditions structure
  if (is.null(conditions$condition_arguments)) {
    stop("'conditions' must contain a 'condition_arguments' component")
  }
  
  condition_arguments <- conditions$condition_arguments
  
  # Extract unique sample sizes and effect sizes for analysis summary
  sample_sizes <- unique(sapply(condition_arguments, function(x)
    x$sim_args$n_total))
  all_effect_sizes <- lapply(condition_arguments, function(x)
    x$sim_args$true_parameter_values)
  unique_effect_combinations <- unique(all_effect_sizes)
  
  # Extract target power levels from design object
  target_power_success <- 0.9 # Default values
  target_power_futility <- 0.95
  
  # Parse and validate design prior for integrated power
  design_prior_parsed <- NULL
  weight_fn <- NULL
  weight_type <- "none"
  if (!is.null(design_prior)) {
    # Extract all unique effect sizes for prior parsing
    all_effects_for_prior <- unique(unlist(lapply(all_effect_sizes, function(x) {
      # For now, use first target parameter for prior parsing
      if (length(design$target_params) > 0) {
        return(x[[design$target_params[1]]])
      }
      return(numeric(0))
    })))
    
    if (length(all_effects_for_prior) > 1) {
      design_prior_parsed <- parse_design_prior(design_prior, all_effects_for_prior, verbose = TRUE)
      weight_fn <- design_prior_parsed$weight_fn
      weight_type <- design_prior_parsed$weight_type
    }
  }
  
  # Log analysis start
  cat("\n=== Power Grid Analysis (New API) ===\n")
  cat("Design name:", attr(design, "design_name"), "\n")
  cat("Target parameters:",
      paste(design$target_params, collapse = ", "),
      "\n")
  cat("Total conditions to test:",
      length(condition_arguments),
      "\n")
  cat("Sample sizes:", paste(sample_sizes, collapse = ", "), "\n")
  cat("Number of simulations per condition:", n_simulations, "\n")
  if (n_cores > 1) {
    cat("Parallel cores:", n_cores, "\n")
  }
  cat("\n")
  
  # Parallel execution over all conditions
  start_time <- Sys.time()
  
  if (n_cores > 1) {
    # Set up parallel backend
    if (requireNamespace("parallel", quietly = TRUE)) {
      cat("Running conditions in parallel...\n")
      
      # Create cluster for parallel execution
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      
      # Export necessary objects to cluster
      parallel::clusterEvalQ(cl, library(rctbayespower))
      parallel::clusterExport(cl, c("design", "n_simulations", "brms_args"), envir = environment())
      
      # Run parallel computation
      results_list <- parallel::parLapply(cl, seq_along(condition_arguments), function(i) {
        condition_arg <- condition_arguments[[i]]
        
        # Execute single simulation run
        tryCatch({
          # Merge brms arguments: base defaults < brms_args < ... arguments
          merged_brms_args <- utils::modifyList(utils::modifyList(list(iter = n_simulations), brms_args), list(...))
          
          fitted_model <- simulate_single_run(
            condition_arguments = condition_arg,
            design = design,
            brms_args = merged_brms_args
          )
          
          # Extract posterior samples and compute power metrics
          posterior_samples <- brms::posterior_samples(fitted_model)
          
          # Compute success and futility probabilities for each target parameter
          success_probs <- sapply(design$target_params, function(param) {
            if (param %in% names(posterior_samples)) {
              mean(posterior_samples[[param]] > design$thresholds_success[which(design$target_params == param)])
            } else {
              NA_real_
            }
          })
          
          futility_probs <- sapply(design$target_params, function(param) {
            if (param %in% names(posterior_samples)) {
              mean(posterior_samples[[param]] < design$thresholds_futility[which(design$target_params == param)])
            } else {
              NA_real_
            }
          })
          
          # Power calculations (probability of correct decision)
          power_success <- mean(success_probs >= design$p_sig_success, na.rm = TRUE)
          power_futility <- mean(futility_probs >= design$p_sig_futility, na.rm = TRUE)
          
          # Mean probabilities
          mean_prob_success <- mean(success_probs, na.rm = TRUE)
          mean_prob_futility <- mean(futility_probs, na.rm = TRUE)
          
          # Convergence rate (simplified)
          convergence_rate <- 1.0 # Assume convergence for now
          
          list(
            condition_id = i,
            n_total = condition_arg$sim_args$n_total,
            effect_sizes = condition_arg$sim_args$true_parameter_values,
            n_interim_analyses = condition_arg$interim_args$n_interim_analyses %||% 0,
            power_success = power_success,
            power_futility = power_futility,
            mean_prob_success = mean_prob_success,
            mean_prob_futility = mean_prob_futility,
            convergence_rate = convergence_rate,
            error = NULL
          )
        }, error = function(e) {
          cat("  ERROR in condition", i, ":", as.character(e), "\n")
          list(
            condition_id = i,
            n_total = condition_arg$sim_args$n_total,
            effect_sizes = condition_arg$sim_args$true_parameter_values,
            n_interim_analyses = condition_arg$interim_args$n_interim_analyses %||% 0,
            power_success = NA_real_,
            power_futility = NA_real_,
            mean_prob_success = NA_real_,
            mean_prob_futility = NA_real_,
            convergence_rate = NA_real_,
            error = as.character(e)
          )
        })
      })
    } else {
      warning("parallel package not available, running sequentially")
      n_cores <- 1
    }
    
    if (n_cores == 1) {
      # Sequential execution
      cat("Running conditions sequentially...\n")
      results_list <- list()
      
      for (i in seq_along(condition_arguments)) {
        condition_arg <- condition_arguments[[i]]
        
        if (i %% progress_updates == 0 ||
            i == length(condition_arguments)) {
          cat("Processing condition",
              i,
              "of",
              length(condition_arguments),
              "\n")
        }
        
        # Execute single simulation run
        results_list[[i]] <- tryCatch({
          # Merge brms arguments: base defaults < brms_args < ... arguments
          merged_brms_args <- utils::modifyList(utils::modifyList(list(iter = n_simulations), brms_args), list(...))
          
          fitted_model <- simulate_single_run(
            condition_arguments = condition_arg,
            design = design,
            brms_args = merged_brms_args
          )
          
          # Extract posterior samples and compute power metrics
          posterior_samples <- brms::posterior_samples(fitted_model)
          
          # Compute success and futility probabilities for each target parameter
          success_probs <- sapply(design$target_params, function(param) {
            if (param %in% names(posterior_samples)) {
              mean(posterior_samples[[param]] > design$thresholds_success[which(design$target_params == param)])
            } else {
              NA_real_
            }
          })
          
          futility_probs <- sapply(design$target_params, function(param) {
            if (param %in% names(posterior_samples)) {
              mean(posterior_samples[[param]] < design$thresholds_futility[which(design$target_params == param)])
            } else {
              NA_real_
            }
          })
          
          # Power calculations (probability of correct decision)
          power_success <- mean(success_probs >= design$p_sig_success, na.rm = TRUE)
          power_futility <- mean(futility_probs >= design$p_sig_futility, na.rm = TRUE)
          
          # Mean probabilities
          mean_prob_success <- mean(success_probs, na.rm = TRUE)
          mean_prob_futility <- mean(futility_probs, na.rm = TRUE)
          
          # Convergence rate (simplified)
          convergence_rate <- 1.0  # Assume convergence for now
          
          list(
            condition_id = i,
            n_total = condition_arg$sim_args$n_total,
            effect_sizes = condition_arg$sim_args$true_parameter_values,
            n_interim_analyses = condition_arg$interim_args$n_interim_analyses %||% 0,
            power_success = power_success,
            power_futility = power_futility,
            mean_prob_success = mean_prob_success,
            mean_prob_futility = mean_prob_futility,
            convergence_rate = convergence_rate,
            error = NULL
          )
        }, error = function(e) {
          cat("  ERROR in condition", i, ":", as.character(e), "\n")
          list(
            condition_id = i,
            n_total = condition_arg$sim_args$n_total,
            effect_sizes = condition_arg$sim_args$true_parameter_values,
            n_interim_analyses = condition_arg$interim_args$n_interim_analyses %||% 0,
            power_success = NA_real_,
            power_futility = NA_real_,
            mean_prob_success = NA_real_,
            mean_prob_futility = NA_real_,
            convergence_rate = NA_real_,
            error = as.character(e)
          )
        })
      }
    } else {
      warning("parallel package not available, running sequentially")
      n_cores <- 1
    }
  }
  
  if (n_cores == 1) {
    # Sequential execution
    cat("Running conditions sequentially...\n")
    results_list <- list()
    
    for (i in seq_along(condition_arguments)) {
      condition_arg <- condition_arguments[[i]]
      
      if (i %% progress_updates == 0 ||
          i == length(condition_arguments)) {
        cat("Processing condition",
            i,
            "of",
            length(condition_arguments),
            "\n")
      }
      
      # Execute single simulation run
      results_list[[i]] <- tryCatch({
        # Merge brms arguments: base defaults < brms_args < ... arguments
        merged_brms_args <- utils::modifyList(utils::modifyList(list(iter = n_simulations), brms_args), list(...))
        
        fitted_model <- simulate_single_run(
          condition_arguments = condition_arg,
          design = design,
          brms_args = merged_brms_args
        )
        
        # Extract posterior samples and compute power metrics
        posterior_samples <- brms::posterior_samples(fitted_model)
        
        # Compute success and futility probabilities for each target parameter
        success_probs <- sapply(design$target_params, function(param) {
          if (param %in% names(posterior_samples)) {
            mean(posterior_samples[[param]] > design$thresholds_success[which(design$target_params == param)])
          } else {
            NA_real_
          }
        })
        
        futility_probs <- sapply(design$target_params, function(param) {
          if (param %in% names(posterior_samples)) {
            mean(posterior_samples[[param]] < design$thresholds_futility[which(design$target_params == param)])
          } else {
            NA_real_
          }
        })
        
        # Power calculations (probability of correct decision)
        power_success <- mean(success_probs >= design$p_sig_success, na.rm = TRUE)
        power_futility <- mean(futility_probs >= design$p_sig_futility, na.rm = TRUE)
        
        # Mean probabilities
        mean_prob_success <- mean(success_probs, na.rm = TRUE)
        mean_prob_futility <- mean(futility_probs, na.rm = TRUE)
        
        # Convergence rate (simplified)
        convergence_rate <- 1.0  # Assume convergence for now
        
        list(
          condition_id = i,
          n_total = condition_arg$sim_args$n_total,
          effect_sizes = condition_arg$sim_args$true_parameter_values,
          n_interim_analyses = condition_arg$interim_args$n_interim_analyses %||% 0,
          power_success = power_success,
          power_futility = power_futility,
          mean_prob_success = mean_prob_success,
          mean_prob_futility = mean_prob_futility,
          convergence_rate = convergence_rate,
          error = NULL
        )
      }, error = function(e) {
        cat("  ERROR in condition", i, ":", as.character(e), "\n")
        list(
          condition_id = i,
          n_total = condition_arg$sim_args$n_total,
          effect_sizes = condition_arg$sim_args$true_parameter_values,
          n_interim_analyses = condition_arg$interim_args$n_interim_analyses %||% 0,
          power_success = NA_real_,
          power_futility = NA_real_,
          mean_prob_success = NA_real_,
          mean_prob_futility = NA_real_,
          convergence_rate = NA_real_,
          error = as.character(e)
        )
      })
    }
  }
  
  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
  cat("\nTotal analysis time:", round(as.numeric(elapsed_time), 2), "minutes\n")
  
  # Create power surface data frame
  power_surface <- do.call(rbind, lapply(results_list, function(x) {
    # Flatten effect_sizes for the data frame
    effect_cols <- list()
    if (!is.null(x$effect_sizes)) {
      for (param_name in names(x$effect_sizes)) {
        effect_cols[[paste0("effect_", param_name)]] <- x$effect_sizes[[param_name]]
      }
    }
    
    basic_cols <- data.frame(
      condition_id = x$condition_id,
      n_total = x$n_total,
      n_interim_analyses = x$n_interim_analyses,
      power_success = x$power_success,
      power_futility = x$power_futility,
      mean_prob_success = x$mean_prob_success,
      mean_prob_futility = x$mean_prob_futility,
      convergence_rate = x$convergence_rate,
      stringsAsFactors = FALSE
    )
    
    if (length(effect_cols) > 0) {
      cbind(basic_cols, as.data.frame(effect_cols))
    } else {
      basic_cols
    }
  }))
  
  # Find optimal combinations
  optimal_success <- power_surface[!is.na(power_surface$power_success) &
                                     power_surface$power_success >= target_power_success, ]
  
  optimal_futility <- power_surface[!is.na(power_surface$power_futility) &
                                      power_surface$power_futility >= target_power_futility, ]
  
  # Create result object
  result <- list(
    # Analysis metadata
    design = design,
    conditions = conditions,
    target_power_success = target_power_success,
    target_power_futility = target_power_futility,
    
    # Thresholds from design
    threshold_success = design$thresholds_success,
    threshold_futility = design$thresholds_futility,
    
    # Analysis results
    power_surface = power_surface,
    optimal_combinations_success = optimal_success,
    optimal_combinations_futility = optimal_futility,
    
    # Summary information
    sample_sizes = sample_sizes,
    unique_effect_combinations = unique_effect_combinations,
    design_prior = design_prior,
    design_prior_type = weight_type,
    n_simulations = n_simulations,
    analysis_time_minutes = as.numeric(elapsed_time),
    n_cores = n_cores,
    
    # Detailed results
    detailed_results = results_list
  )
  
  class(result) <- "rctbayespower_grid"
  
  # Print summary
  cat("\n=== Power Grid Analysis Complete ===\n")
  cat("Total conditions analyzed:", length(conditions), "\n")
  
  success_count <- sum(
    !is.na(power_surface$power_success) &
      power_surface$power_success >= target_power_success
  )
  futility_count <- sum(
    !is.na(power_surface$power_futility) &
      power_surface$power_futility >= target_power_futility
  )
  
  cat(
    "Conditions achieving target success power (>=",
    target_power_success,
    "):",
    success_count,
    "\n"
  )
  cat(
    "Conditions achieving target futility power (>=",
    target_power_futility,
    "):",
    futility_count,
    "\n"
  )
  
  error_count <- sum(sapply(results_list, function(x)
    ! is.null(x$error)))
  if (error_count > 0) {
    cat("Conditions with errors:", error_count, "\n")
  }
  
  return(result)
}


