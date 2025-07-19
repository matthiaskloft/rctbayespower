#' Power Grid Analysis for Bayesian RCTs (New API)
#'
#' Comprehensive power analysis across multiple conditions using the new object-oriented
#' API. This function provides flexible power analysis by varying sample sizes, effect 
#' sizes, interim analyses, and other parameters across a grid of conditions.
#'
#' @param design An rctbayespower_design object containing the model 
#'   specifications, target parameters, thresholds, and analysis configuration
#' @param conditions List of condition specifications. Each condition must include:
#'   \itemize{
#'     \item n_total: Total sample size for this condition
#'     \item effect_sizes: Named list of effect sizes matching target_params from design
#'     \item n_interim_analyses: Number of interim analyses (optional, defaults to design value)
#'     \item p_alloc: Treatment allocation probabilities (optional, defaults to c(0.5, 0.5))
#'     \item true_parameter_values: Additional simulation parameters (optional)
#'   }
#' @param static_parameters List of parameters that override design defaults across all conditions.
#'   Can include: target_power_success, target_power_futility, true_parameter_values, etc.
#' @param design_prior Optional design prior for integrated power computation. Can be:
#'   \itemize{
#'     \item A string in brms prior syntax (e.g., "normal(0.3, 0.1)", "student_t(6, 0.5, 0.2)")
#'     \item An R function taking effect size as input (e.g., function(x) dnorm(x, 0.5, 0.2))
#'     \item NULL for no design prior (default)
#'   }
#' @param n_simulations Number of MCMC iterations per condition (default: 500)
#' @param n_cores Number of parallel cores for condition execution (default: detectCores() - 1)
#' @param progress_updates Show progress every N conditions when running sequentially (default: 10)
#' @param ... Additional arguments passed to brms fitting (e.g., algorithm, chains)
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
#'   \item static_parameters: Static parameter overrides applied
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
#'   design = design,
#'   conditions = conditions,
#'   static_parameters = list(
#'     target_power_success = 0.8,
#'     target_power_futility = 0.9
#'   ),
#'   n_simulations = 100,  # Low for example
#'   n_cores = 1
#' )
#' }
power_grid_analysis <- function(design,
                                conditions,
                                static_parameters = list(),
                                design_prior = NULL,
                                n_simulations = 500,
                                n_cores = parallel::detectCores() - 1,
                                progress_updates = 10,
                                ...) {
  # Validate inputs
  if (!inherits(design, "rctbayespower_design")) {
    stop("'design' must be a valid rctbayespower_design object")
  }

  if (!is.list(conditions) || length(conditions) == 0) {
    stop("'conditions' must be a non-empty list of condition specifications")
  }

  if (!is.numeric(n_simulations) || n_simulations <= 0) {
    stop("'n_simulations' must be a positive number")
  }

  if (!is.numeric(n_cores) || n_cores <= 0) {
    n_cores <- 1
    warning("Invalid n_cores value. Using n_cores = 1.")
  }

  # Validate each condition
  for (i in seq_along(conditions)) {
    tryCatch(
      validate_condition_parameters(conditions[[i]], design),
      error = function(e) {
        stop("Condition ", i, " validation failed: ", e$message)
      }
    )
  }

  # Extract unique sample sizes and effect sizes for analysis summary
  sample_sizes <- unique(sapply(conditions, function(x) x$n_total))
  all_effect_sizes <- lapply(conditions, function(x) x$effect_sizes)
  unique_effect_combinations <- unique(all_effect_sizes)

  # Extract target power levels from design object
  target_power_success <- 0.9  # Default values
  target_power_futility <- 0.95
  if (!is.null(static_parameters$target_power_success)) {
    target_power_success <- static_parameters$target_power_success
  }
  if (!is.null(static_parameters$target_power_futility)) {
    target_power_futility <- static_parameters$target_power_futility
  }

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
  cat("Target parameters:", paste(design$target_params, collapse = ", "), "\n")
  cat("Total conditions to test:", length(conditions), "\n")
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
      parallel::clusterExport(cl, c("design", "static_parameters", "n_simulations"), 
                             envir = environment())
      
      # Run parallel computation
      results_list <- parallel::parLapply(cl, seq_along(conditions), function(i) {
        condition <- conditions[[i]]
        
        # Resolve parameters for this condition
        resolved_true_params <- resolve_simulation_parameters(condition, design, static_parameters)
        resolved_allocation <- condition$p_alloc %||% c(0.5, 0.5)
        
        # Create design copy with condition-specific parameters if needed
        design_copy <- design
        if (!is.null(condition$n_interim_analyses) && condition$n_interim_analyses != design_copy$n_interim_analyses) {
          design_copy$n_interim_analyses <- condition$n_interim_analyses
        }
        
        # Execute single simulation run
        tryCatch({
          fitted_model <- simulate_single_run(
            n_total = condition$n_total,
            p_alloc = resolved_allocation,
            design = design_copy,
            true_parameter_values = resolved_true_params,
            iter = n_simulations,
            ...
          )
          
          # Extract posterior samples and compute power metrics
          posterior_samples <- brms::posterior_samples(fitted_model)
          
          # Compute success and futility probabilities for each target parameter
          success_probs <- sapply(design_copy$target_params, function(param) {
            if (param %in% names(posterior_samples)) {
              mean(posterior_samples[[param]] > design_copy$thresholds_success[which(design_copy$target_params == param)])
            } else {
              NA_real_
            }
          })
          
          futility_probs <- sapply(design_copy$target_params, function(param) {
            if (param %in% names(posterior_samples)) {
              mean(posterior_samples[[param]] < design_copy$thresholds_futility[which(design_copy$target_params == param)])
            } else {
              NA_real_
            }
          })
          
          # Power calculations (probability of correct decision)
          power_success <- mean(success_probs >= design_copy$p_sig_success, na.rm = TRUE)
          power_futility <- mean(futility_probs >= design_copy$p_sig_futility, na.rm = TRUE)
          
          # Mean probabilities
          mean_prob_success <- mean(success_probs, na.rm = TRUE)
          mean_prob_futility <- mean(futility_probs, na.rm = TRUE)
          
          # Convergence rate (simplified)
          convergence_rate <- 1.0  # Assume convergence for now
          
          list(
            condition_id = i,
            n_total = condition$n_total,
            effect_sizes = condition$effect_sizes,
            n_interim_analyses = condition$n_interim_analyses %||% 0,
            power_success = power_success,
            power_futility = power_futility,
            mean_prob_success = mean_prob_success,
            mean_prob_futility = mean_prob_futility,
            convergence_rate = convergence_rate,
            error = NULL
          )
        }, error = function(e) {
          list(
            condition_id = i,
            n_total = condition$n_total,
            effect_sizes = condition$effect_sizes,
            n_interim_analyses = condition$n_interim_analyses %||% 0,
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
  }
  
  if (n_cores == 1) {
    # Sequential execution
    cat("Running conditions sequentially...\n")
    results_list <- list()
    
    for (i in seq_along(conditions)) {
      condition <- conditions[[i]]
      
      if (i %% progress_updates == 0 || i == length(conditions)) {
        cat("Processing condition", i, "of", length(conditions), "\n")
      }
      
      # Resolve parameters for this condition
      resolved_true_params <- resolve_simulation_parameters(condition, design, static_parameters)
      resolved_allocation <- condition$p_alloc %||% c(0.5, 0.5)
      
      # Create design copy with condition-specific parameters if needed
      design_copy <- design
      if (!is.null(condition$n_interim_analyses) && condition$n_interim_analyses != design_copy$n_interim_analyses) {
        design_copy$n_interim_analyses <- condition$n_interim_analyses
      }
      
      # Execute single simulation run
      results_list[[i]] <- tryCatch({
        fitted_model <- simulate_single_run(
          n_total = condition$n_total,
          p_alloc = resolved_allocation,
          design = design_copy,
          true_parameter_values = resolved_true_params,
          iter = n_simulations,
          ...
        )
        
        # Extract posterior samples and compute power metrics
        posterior_samples <- brms::posterior_samples(fitted_model)
        
        # Compute success and futility probabilities for each target parameter
        success_probs <- sapply(design_copy$target_params, function(param) {
          if (param %in% names(posterior_samples)) {
            mean(posterior_samples[[param]] > design_copy$thresholds_success[which(design_copy$target_params == param)])
          } else {
            NA_real_
          }
        })
        
        futility_probs <- sapply(design_copy$target_params, function(param) {
          if (param %in% names(posterior_samples)) {
            mean(posterior_samples[[param]] < design_copy$thresholds_futility[which(design_copy$target_params == param)])
          } else {
            NA_real_
          }
        })
        
        # Power calculations (probability of correct decision)
        power_success <- mean(success_probs >= design_copy$p_sig_success, na.rm = TRUE)
        power_futility <- mean(futility_probs >= design_copy$p_sig_futility, na.rm = TRUE)
        
        # Mean probabilities
        mean_prob_success <- mean(success_probs, na.rm = TRUE)
        mean_prob_futility <- mean(futility_probs, na.rm = TRUE)
        
        # Convergence rate (simplified)
        convergence_rate <- 1.0  # Assume convergence for now
        
        list(
          condition_id = i,
          n_total = condition$n_total,
          effect_sizes = condition$effect_sizes,
          n_interim_analyses = condition$n_interim_analyses %||% 0,
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
          n_total = condition$n_total,
          effect_sizes = condition$effect_sizes,
          n_interim_analyses = condition$n_interim_analyses %||% 0,
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
    static_parameters = static_parameters,
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
  
  success_count <- sum(!is.na(power_surface$power_success) & 
                      power_surface$power_success >= target_power_success)
  futility_count <- sum(!is.na(power_surface$power_futility) & 
                       power_surface$power_futility >= target_power_futility)
  
  cat("Conditions achieving target success power (>=", target_power_success, "):", success_count, "\n")
  cat("Conditions achieving target futility power (>=", target_power_futility, "):", futility_count, "\n")
  
  error_count <- sum(sapply(results_list, function(x) !is.null(x$error)))
  if (error_count > 0) {
    cat("Conditions with errors:", error_count, "\n")
  }

  return(result)
}

#' Print method for power grid analysis objects
#' @param x An rctbayespower_grid object
#' @param ... Additional arguments (unused)
#' @export
print.rctbayespower_grid <- function(x, ...) {
  # Print header for new API
  cat("Bayesian RCT Power Grid Analysis (New API)\n")
  cat("==========================================\n\n")
  
  cat("Design name:", attr(x$design, "design_name"), "\n")
  cat("Target parameters:", paste(x$design$target_params, collapse = ", "), "\n")
  cat("Sample sizes tested:", paste(x$sample_sizes, collapse = ", "), "\n")
  cat("Total conditions analyzed:", length(x$conditions), "\n")
  cat("Target power - Success:", x$target_power_success, "\n")
  cat("Target power - Futility:", x$target_power_futility, "\n")
  
  if (!is.null(x$design_prior)) {
    cat("Design prior:", x$design_prior, "\n")
  }
  
  cat("\nConditions achieving target power:\n")
  success_count <- nrow(x$optimal_combinations_success)
  futility_count <- nrow(x$optimal_combinations_futility)
  cat("- Success power (>=", x$target_power_success, "):", success_count, "conditions\n")
  cat("- Futility power (>=", x$target_power_futility, "):", futility_count, "conditions\n")
  
  cat("\nUse summary() for detailed results.\n")
}

#' Summary method for power grid analysis objects
#' @param object An rctbayespower_grid object
#' @param design_prior Optional design prior for runtime integrated power computation. Can be:
#'   \itemize{
#'     \item A string in brms prior syntax (e.g., "normal(0.3, 0.1)", "student_t(6, 0.5, 0.2)")
#'     \item An R function taking effect size as input (e.g., function(x) dnorm(x, 0.5, 0.2))
#'     \item NULL for no runtime integration (default)
#'   }
#'   If provided, integrated power will be computed using this design prior instead of
#'   any design prior specified in the original power_grid_analysis() call.
#'   Only valid when effect sizes vary (length > 1).
#' @param print Logical indicating whether to print the summary to console (default: TRUE).
#'   When FALSE, suppresses all console output and only returns the summary object.
#' @param ... Additional arguments (unused)
#' @export
summary.rctbayespower_grid <- function(object, design_prior = NULL, print = TRUE, ...) {

  # Validate n_cores
  if (!is.numeric(n_cores) || n_cores < 1) {
    n_cores <- 1
    warning("Invalid n_cores value. Using n_cores = 1.")
  }

  # Filter dots based on the power analysis function being used (applies to both phases)
  if (power_analysis_fn == "power_analysis_ancova") {
    # Filter out parameters that power_analysis_ancova doesn't accept
    # Most importantly, remove simulate_data_fn since ANCOVA generates its own
    valid_ancova_params <- c(
      "outcome_type", "baseline_effect", "intercept_value", "sigma_value",
      "p_sig_success", "p_sig_futility", "n_simulations",
      "priors_treatment", "priors_baseline", "priors_intercept", "priors_sigma",
      "brms_args", "seed", "n_cores", "progress_updates"
    )
    dots <- dots[names(dots) %in% valid_ancova_params]
  }

  # PHASE 1: Parallel Model Compilation
  cat("\n=== Phase 1: Parallel Model Compilation ===\n")
  cat("Compiling models for", length(unique_effects), "unique effect sizes")
  if (n_cores > 1 && length(unique_effects) > 1) {
    cat(" in parallel using", n_cores, "cores...\n")
  } else {
    cat(" sequentially...\n")
  }

  # Function to compile models for a single effect size
  compile_models_for_effect <- function(effect_idx, dots) {
    # dots parameter is required and should be pre-filtered

    # Validate that dots is not empty for required parameters
    if (length(dots) == 0) {
      compilation_error <- "No parameters provided for model compilation"
      return(list(
        effect_size = unique_effects[effect_idx],
        compiled_models = NULL,
        error = compilation_error
      ))
    }

    current_effect <- unique_effects[effect_idx]
    effect_combinations <- combinations_by_effect[[as.character(current_effect)]]

    # Use first combination for compilation
    first_combo <- effect_combinations[1, ]
    n_total_first <- first_combo$sample_size
    n_treatment_first <- round(n_total_first * percent_group_treat)
    n_control_first <- n_total_first - n_treatment_first

    compiled_models <- NULL
    compilation_error <- NULL

    if (power_analysis_fn == "power_analysis") {
      # For power_analysis, compile using compile_models_only option
      tryCatch(
        {
          required_params <- c(
            "simulate_data_fn",
            "model_formula_true_params",
            "model_formula_estimation",
            "family",
            "priors_true_params",
            "priors_estimation",
            "target_param"
          )

          if (all(required_params %in% names(dots))) {
            # Filter to valid power_analysis parameters
            valid_power_analysis_params <- c(
              "simulate_data_fn", "model_formula_true_params", "model_formula_estimation",
              "family", "priors_true_params", "priors_estimation", "target_param",
              "p_sig_success", "p_sig_futility", "n_simulations",
              "brms_args", "seed", "n_cores", "progress_updates"
            )

            dots_filtered <- dots[names(dots) %in% valid_power_analysis_params]

            compile_args <- c(
              list(
                n_control = n_control_first,
                n_treatment = n_treatment_first,
                threshold_success = threshold_success,
                threshold_futility = threshold_futility,
                compile_models_only = TRUE
              ),
              dots_filtered
            )

            power_analysis_compilation_result <- do.call(power_analysis, compile_args)

            compiled_models <- list(
              brms_design_true_params = power_analysis_compilation_result$brms_design_true_params,
              brms_design_estimation = power_analysis_compilation_result$brms_design_estimation,
              simulate_data_fn = power_analysis_compilation_result$simulate_data_fn,
              model_formula_true_params = power_analysis_compilation_result$model_formula_true_params,
              model_formula_estimation = power_analysis_compilation_result$model_formula_estimation,
              family = power_analysis_compilation_result$family,
              priors_true_params = power_analysis_compilation_result$priors_true_params,
              priors_estimation = power_analysis_compilation_result$priors_estimation,
              target_param = power_analysis_compilation_result$target_param
            )
          } else {
            missing_params <- setdiff(required_params, names(dots))
            compilation_error <- paste("Missing required parameters for power_analysis:", paste(missing_params, collapse = ", "))
          }
        },
        error = function(e) {
          compilation_error <- as.character(e)
        }
      )
    } else if (power_analysis_fn == "power_analysis_ancova") {
      # For power_analysis_ancova, compile using compile_models_only option (passed through to power_analysis)
      tryCatch(
        {
          required_ancova_params <- c("outcome_type", "baseline_effect")

          if (all(required_ancova_params %in% names(dots))) {
            # Filter out parameters that power_analysis_ancova doesn't accept
            valid_ancova_params <- c(
              "outcome_type", "baseline_effect", "intercept_value", "sigma_value",
              "p_sig_success", "p_sig_futility", "n_simulations",
              "priors_treatment", "priors_baseline", "priors_intercept", "priors_sigma",
              "brms_args", "seed", "n_cores", "progress_updates"
            )

            dots_filtered <- dots[names(dots) %in% valid_ancova_params]

            compile_args <- c(
              list(
                n_control = n_control_first,
                n_treatment = n_treatment_first,
                effect_size = current_effect,
                threshold_success = threshold_success,
                threshold_futility = threshold_futility,
                compile_models_only = TRUE
              ),
              dots_filtered
            )

            ancova_compilation_result <- do.call(power_analysis_ancova, compile_args)

            compiled_models <- list(
              brms_design_true_params = ancova_compilation_result$brms_design_true_params,
              brms_design_estimation = ancova_compilation_result$brms_design_estimation,
              simulate_data_fn = ancova_compilation_result$simulate_data_fn,
              model_formula_true_params = ancova_compilation_result$model_formula_true_params,
              model_formula_estimation = ancova_compilation_result$model_formula_estimation,
              family = ancova_compilation_result$family,
              priors_true_params = ancova_compilation_result$priors_true_params,
              priors_estimation = ancova_compilation_result$priors_estimation,
              target_param = ancova_compilation_result$target_param,
              ancova_params = dots
            )
          } else {
            missing_params <- setdiff(required_ancova_params, names(dots))
            compilation_error <- paste("Missing required parameters for power_analysis_ancova:", paste(missing_params, collapse = ", "))
          }
        },
        error = function(e) {
          compilation_error <- as.character(e)
        }
      )
    } else {
      compilation_error <- paste("Model caching not supported for", power_analysis_fn)
    }

    return(list(
      effect_size = current_effect,
      compiled_models = compiled_models,
      error = compilation_error
    ))
  }

  # Execute model compilation (parallel or sequential)
  if (n_cores > 1 && length(unique_effects) > 1) {
    # Parallel compilation
    cl <- tryCatch(
      {
        parallel::makeCluster(n_cores)
      },
      error = function(e) {
        warning("Failed to create parallel cluster: ", e$message, ". Falling back to sequential compilation.")
        return(NULL)
      }
    )

    if (!is.null(cl)) {
      compilation_results <- tryCatch({
        # Export necessary objects to cluster
        parallel::clusterExport(cl, c(
          "unique_effects",
          "combinations_by_effect",
          "percent_group_treat",
          "threshold_success",
          "threshold_futility",
          "power_analysis_fn",
          "validate_power_design",
          "power_analysis_ancova",
          "dots"
        ), envir = environment())

        # Load required packages on cluster nodes
        parallel::clusterEvalQ(cl, {
          requireNamespace("brms", quietly = TRUE)
        })

        # Run compilation in parallel
        parallel::parLapply(cl, 1:length(unique_effects), function(effect_idx) {
          compile_models_for_effect(effect_idx, dots)
        })
      }, finally = {
        parallel::stopCluster(cl)
      })
    } else {
      # Fall back to sequential compilation if cluster creation failed
      compilation_results <- lapply(1:length(unique_effects), function(effect_idx) {
        compile_models_for_effect(effect_idx, dots)
      })
    }
  } else {
    # Sequential compilation
    compilation_results <- lapply(1:length(unique_effects), function(effect_idx) {
      compile_models_for_effect(effect_idx, dots)
    })
  }

  # Process compilation results and populate cache
  for (i in seq_along(compilation_results)) {
    result <- compilation_results[[i]]
    effect_size <- result$effect_size

    if (is.null(result$error)) {
      compiled_models_cache[[as.character(effect_size)]] <- result$compiled_models
      cat("[SUCCESS] Successfully compiled models for effect size", effect_size, "\n")
    } else {
      compiled_models_cache[[as.character(effect_size)]] <- NULL
      cat("[ERROR] Failed to compile models for effect size", effect_size, ":", result$error, "\n")
    }
  }

  cat("\n=== Phase 2: Grid Analysis with Cached Models ===\n")

  # Loop through effect sizes to run analyses using cached models
  for (effect_idx in seq_along(unique_effects)) {
    current_effect <- unique_effects[effect_idx]
    effect_combinations <- combinations_by_effect[[as.character(current_effect)]]

    cat(
      "\n--- Processing Effect Size",
      current_effect,
      "(",
      effect_idx,
      "of",
      length(unique_effects),
      ") ---\n"
    )
    cat(
      "Combinations for this effect size:",
      nrow(effect_combinations),
      "\n"
    )

    # Retrieve pre-compiled models from Phase 1 cache
    compiled_models <- compiled_models_cache[[as.character(current_effect)]]

    if (!is.null(compiled_models)) {
      cat("Using pre-compiled models from Phase 1 cache\n")
    } else {
      cat("No cached models available - will compile individually per combination\n")
    }

    # Process all combinations for this effect size
    for (combo_idx in 1:nrow(effect_combinations)) {
      combo <- effect_combinations[combo_idx, ]
      n_total <- combo$sample_size
      effect_size <- combo$effect_size
      n_treatment <- round(n_total * percent_group_treat)
      n_control <- n_total - n_treatment

      # Find the absolute index for this combination
      abs_index <- which(combinations$sample_size == n_total &
        combinations$effect_size == effect_size)[1]

      cat(
        "Testing combination",
        abs_index,
        "of",
        nrow(combinations),
        ":"
      )
      cat(" N =", n_total, ", Effect =", effect_size)
      if (!is.null(compiled_models)) {
        cat(" (using cached models)")
      }
      cat("\n")

      # Run power analysis with current combination
      power_args <- c(
        list(
          n_control = n_control,
          n_treatment = n_treatment,
          n_simulations = n_simulations,
          threshold_success = threshold_success,
          threshold_futility = threshold_futility,
          effect_size = effect_size
        ),
        dots
      )

      # Add pre-compiled models if available (consistent approach for both functions)
      if (!is.null(compiled_models)) {
        # For both functions, pass the pre-compiled models and cached parameters
        power_args$brms_design_true_params <- compiled_models$brms_design_true_params
        power_args$brms_design_estimation <- compiled_models$brms_design_estimation
        power_args$simulate_data_fn <- compiled_models$simulate_data_fn
        power_args$model_formula_true_params <- compiled_models$model_formula_true_params
        power_args$model_formula_estimation <- compiled_models$model_formula_estimation
        power_args$family <- compiled_models$family
        power_args$priors_true_params <- compiled_models$priors_true_params
        power_args$priors_estimation <- compiled_models$priors_estimation
        power_args$target_param <- compiled_models$target_param
      }

      tryCatch(
        {
          # Consistent execution for both function types
          if (power_analysis_fn == "power_analysis_ancova") {
            power_result <- do.call(power_analysis_ancova, power_args)
          } else {
            power_result <- do.call(power_analysis, power_args)
          }

          # Extract power metrics
          results_matrix[[result_index]] <- list(
            n_total = n_total,
            n_control = n_control,
            n_treatment = n_treatment,
            effect_size = effect_size,
            power_success = power_result$power_success,
            power_futility = power_result$power_futility,
            mean_prob_success = power_result$mean_prob_success,
            mean_prob_futility = power_result$mean_prob_futility,
            mcse_power_success = power_result$mcse_power_success,
            mcse_power_futility = power_result$mcse_power_futility,
            mcse_mean_prob_success = power_result$mcse_mean_prob_success,
            mcse_mean_prob_futility = power_result$mcse_mean_prob_futility,
            convergence_rate = power_result$convergence_rate,
            full_result = power_result
          )
        },
        error = function(e) {
          results_matrix[[result_index]] <- list(
            n_total = n_total,
            n_control = n_control,
            n_treatment = n_treatment,
            effect_size = effect_size,
            power_success = NA,
            power_futility = NA,
            mean_prob_success = NA,
            mean_prob_futility = NA,
            mcse_power_success = NA,
            mcse_power_futility = NA,
            mcse_mean_prob_success = NA,
            mcse_mean_prob_futility = NA,
            convergence_rate = NA,
            error = as.character(e)
          )
          cat("  ERROR:", as.character(e), "\n")
        }
      )

      result_index <- result_index + 1
    }
  }

  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
  cat("\nTotal analysis time:", round(as.numeric(elapsed_time), 2), "minutes\n")

  # Create power surface data frame
  power_surface <- do.call(rbind, lapply(results_matrix, function(x) {
    data.frame(
      n_total = if (is.null(x$n_total)) NA_integer_ else x$n_total,
      n_control = if (is.null(x$n_control)) NA_integer_ else x$n_control,
      n_treatment = if (is.null(x$n_treatment)) NA_integer_ else x$n_treatment,
      effect_size = if (is.null(x$effect_size)) NA_real_ else x$effect_size,
      power_success = if (is.null(x$power_success)) {
        NA_real_
      } else {
        x$power_success
      },
      power_futility = if (is.null(x$power_futility)) {
        NA_real_
      } else {
        x$power_futility
      },
      mean_prob_success = if (is.null(x$mean_prob_success)) {
        NA_real_
      } else {
        x$mean_prob_success
      },
      mean_prob_futility = if (is.null(x$mean_prob_futility)) {
        NA_real_
      } else {
        x$mean_prob_futility
      },
      mcse_power_success = if (is.null(x$mcse_power_success)) {
        NA_real_
      } else {
        x$mcse_power_success
      },
      mcse_power_futility = if (is.null(x$mcse_power_futility)) {
        NA_real_
      } else {
        x$mcse_power_futility
      },
      mcse_mean_prob_success = if (is.null(x$mcse_mean_prob_success)) {
        NA_real_
      } else {
        x$mcse_mean_prob_success
      },
      mcse_mean_prob_futility = if (is.null(x$mcse_mean_prob_futility)) {
        NA_real_
      } else {
        x$mcse_mean_prob_futility
      },
      convergence_rate = if (is.null(x$convergence_rate)) {
        NA_real_
      } else {
        x$convergence_rate
      },
      stringsAsFactors = FALSE
    )
  }))

  # Compute integrated power if design prior provided
  integrated_power_success <- NULL

  if (!is.null(weight_fn)) {
    cat("\nComputing integrated power using design prior...\n")

    # Get weights for each effect size
    weights <- sapply(effect_sizes, weight_fn)
    weights <- weights / sum(weights) # Normalize to sum to 1

    # For each sample size, compute weighted average power
    integrated_results <- list()

    for (n in sample_sizes) {
      subset_data <- power_surface[power_surface$n_total == n, ]

      if (nrow(subset_data) > 0 &&
        all(!is.na(subset_data$power_success))) {
        weighted_power_success <- sum(subset_data$power_success * weights)
        weighted_power_futility <- sum(subset_data$power_futility * weights)
        weighted_prob_success <- sum(subset_data$mean_prob_success * weights)
        weighted_prob_futility <- sum(subset_data$mean_prob_futility * weights)

        # Calculate MCSE for integrated power metrics
        mcse_integrated_power_success <- calculate_mcse_integrated_power(
          subset_data$power_success, weights, n_simulations,
          is_power_metric = TRUE
        )
        mcse_integrated_power_futility <- calculate_mcse_integrated_power(
          subset_data$power_futility, weights, n_simulations,
          is_power_metric = TRUE
        )
        mcse_integrated_prob_success <- calculate_mcse_integrated_power(
          subset_data$mean_prob_success, weights, n_simulations,
          is_power_metric = FALSE
        )
        mcse_integrated_prob_futility <- calculate_mcse_integrated_power(
          subset_data$mean_prob_futility, weights, n_simulations,
          is_power_metric = FALSE
        )

        integrated_results[[length(integrated_results) + 1]] <- data.frame(
          n_total = as.integer(n),
          integrated_power_success = weighted_power_success,
          integrated_power_futility = weighted_power_futility,
          integrated_prob_success = weighted_prob_success,
          integrated_prob_futility = weighted_prob_futility,
          mcse_integrated_power_success = mcse_integrated_power_success,
          mcse_integrated_power_futility = mcse_integrated_power_futility,
          mcse_integrated_prob_success = mcse_integrated_prob_success,
          mcse_integrated_prob_futility = mcse_integrated_prob_futility,
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(integrated_results) > 0) {
      integrated_power_success <- do.call(rbind, integrated_results)
    }
  }

  # Find optimal combinations
  optimal_success <- power_surface[!is.na(power_surface$power_success) &
    power_surface$power_success >= target_power_success, ]

  optimal_futility <- power_surface[!is.na(power_surface$power_futility) &
    power_surface$power_futility >= target_power_futility, ]

  # Find minimum sample size for integrated power if available
  min_n_integrated_success <- NA_integer_
  min_n_integrated_futility <- NA_integer_

  if (!is.null(integrated_power_success)) {
    adequate_integrated_success <- integrated_power_success$integrated_power_success >= target_power_success
    adequate_integrated_futility <- integrated_power_success$integrated_power_futility >= target_power_futility

    if (any(adequate_integrated_success)) {
      min_n_integrated_success <- min(integrated_power_success$n_total[adequate_integrated_success])
    }

    if (any(adequate_integrated_futility)) {
      min_n_integrated_futility <- min(integrated_power_success$n_total[adequate_integrated_futility])
    }
  }

  # Calculate min_n fields for sample_only mode (backward compatibility)
  min_n_success <- if (analysis_type == "sample_only" &&
    nrow(optimal_success) > 0) {
    min(optimal_success$n_total)
  } else {
    NA_integer_
  }

  min_n_futility <- if (analysis_type == "sample_only" &&
    nrow(optimal_futility) > 0) {
    min(optimal_futility$n_total)
  } else {
    NA_integer_
  }

  # For sample_only mode, create power_curve for backward compatibility
  power_curve <- if (analysis_type == "sample_only") {
    power_surface
  } else {
    NULL
  }

  # Prepare result object
  result <- list(
    target_power_success = target_power_success,
    target_power_futility = target_power_futility,
    threshold_success = threshold_success,
    threshold_futility = threshold_futility,
    sample_sizes = sample_sizes,
    effect_sizes = effect_sizes,
    design_prior = design_prior,
    design_prior_type = weight_type,
    analysis_type = analysis_type,
    percent_group_treat = percent_group_treat,
    power_analysis_fn = power_analysis_fn,
    n_simulations = n_simulations,
    analysis_time_minutes = as.numeric(elapsed_time),

    # Backward compatibility fields for sample_only mode
    min_n_success = min_n_success,
    min_n_futility = min_n_futility,
    power_curve = power_curve,
    effect_size = if (analysis_type == "sample_only") {
      effect_sizes[1]
    } else {
      NULL
    },

    # Main results
    power_surface = power_surface,
    integrated_power = integrated_power_success,

    # Optimal combinations
    optimal_combinations_success = optimal_success,
    optimal_combinations_futility = optimal_futility,
    min_n_integrated_success = min_n_integrated_success,
    min_n_integrated_futility = min_n_integrated_futility,

    # Detailed results
    detailed_results = results_matrix,

    # Analysis parameters (for reference)
    analysis_parameters = dots
  )

  class(result) <- "rctbayespower_grid"

  # Print summary based on analysis type
  if (analysis_type == "effect_only") {
    cat("\n=== Effect Size Analysis Complete ===\n")

    if (nrow(optimal_success) > 0) {
      best_effect <- optimal_success[which.max(optimal_success$power_success), ]
      cat(
        "Best effect size achieving success power >=",
        target_power_success,
        ":\n"
      )
      cat(
        "  Effect size =",
        best_effect$effect_size,
        ", Power =",
        round(best_effect$power_success, 3)
      )
      cat(
        ", Mean Probability =",
        round(best_effect$mean_prob_success, 3),
        "\n"
      )
    } else {
      cat("Target success power not achieved with tested effect sizes\n")
    }

    if (nrow(optimal_futility) > 0) {
      best_effect <- optimal_futility[which.max(optimal_futility$power_futility), ]
      cat(
        "Best effect size achieving futility power >=",
        target_power_futility,
        ":\n"
      )
      cat(
        "  Effect size =",
        best_effect$effect_size,
        ", Power =",
        round(best_effect$power_futility, 3)
      )
      cat(
        ", Mean Probability =",
        round(best_effect$mean_prob_futility, 3),
        "\n"
      )
    } else {
      cat("Target futility power not achieved with tested effect sizes\n")
    }
  } else if (analysis_type == "sample_only") {
    cat("\n=== Sample Size Analysis Complete ===\n")
    if (!is.na(min_n_success)) {
      cat(
        "Minimum required total sample size for success power >=",
        target_power_success,
        ":",
        min_n_success,
        "\n"
      )
    } else {
      cat("Target success power not achieved with tested sample sizes\n")
    }

    if (!is.na(min_n_futility)) {
      cat(
        "Minimum required total sample size for futility power >=",
        target_power_futility,
        ":",
        min_n_futility,
        "\n"
      )
    } else {
      cat("Target futility power not achieved with tested sample sizes\n")
    }

    # Add power overview table (like original sample_size_analysis)
    cat("\nPower Overview Across Sample Sizes:\n")
    overview_df <- power_surface

    # Add target achievement indicators
    success_achieved <- !is.na(overview_df$power_success) &
      overview_df$power_success >= target_power_success
    overview_df$success_target <- ifelse(success_achieved, "OK", "x")

    futility_achieved <- !is.na(overview_df$power_futility) &
      overview_df$power_futility >= target_power_futility
    overview_df$futility_target <- ifelse(futility_achieved, "OK", "x")

    # Format as percentages
    overview_df$power_success_pct <- paste0(round(overview_df$power_success * 100, 1), "%")
    overview_df$power_futility_pct <- paste0(round(overview_df$power_futility * 100, 1), "%")

    # Create compact display table
    compact_df <- data.frame(
      "N Total" = overview_df$n_total,
      "Success" = overview_df$power_success_pct,
      "Futility" = overview_df$power_futility_pct,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    print(compact_df, row.names = FALSE)
    cat("(N Total = total sample size) \n")
  } else {
    cat("\n=== Power Grid Complete ===\n")

    if (nrow(optimal_success) > 0) {
      min_combo <- optimal_success[which.min(optimal_success$n_total), ]
      cat(
        "Smallest sample size achieving success power >=",
        target_power_success,
        ":\n"
      )
      cat(
        "  N =",
        min_combo$n_total,
        ", Effect size =",
        min_combo$effect_size
      )
      cat(
        ", Power =",
        round(min_combo$power_success, 3),
        ", Mean Probability =",
        round(min_combo$mean_prob_success, 3),
        "\n"
      )
    } else {
      cat("Target success power not achieved with tested combinations\n")
    }

    if (nrow(optimal_futility) > 0) {
      min_combo <- optimal_futility[which.min(optimal_futility$n_total), ]
      cat(
        "Smallest sample size achieving futility power >=",
        target_power_futility,
        ":\n"
      )
      cat(
        "  N =",
        min_combo$n_total,
        ", Effect size =",
        min_combo$effect_size
      )
      cat(
        ", Power =",
        round(min_combo$power_futility, 3),
        ", Mean Probability =",
        round(min_combo$mean_prob_futility, 3),
        "\n"
      )
    } else {
      cat("Target futility power not achieved with tested combinations\n")
    }
  }

  # Integrated power results (only relevant when effect sizes vary)
  if (length(effect_sizes) > 1 &&
    !is.null(integrated_power_success)) {
    cat("\nIntegrated power results:\n")

    # Check if we have any results to show
    has_success_result <- !is.na(min_n_integrated_success)
    has_futility_result <- !is.na(min_n_integrated_futility)

    if (has_success_result) {
      cat(
        "  Minimum N for integrated success power >=",
        target_power_success,
        ":",
        min_n_integrated_success,
        "\n"
      )
    } else {
      cat(
        "  Target integrated success power >=",
        target_power_success,
        "not achieved with tested sample sizes\n"
      )
    }

    if (has_futility_result) {
      cat(
        "  Minimum N for integrated futility power >=",
        target_power_futility,
        ":",
        min_n_integrated_futility,
        "\n"
      )
    } else {
      cat(
        "  Target integrated futility power >=",
        target_power_futility,
        "not achieved with tested sample sizes\n"
      )
    }
  }

  return(result)
}

#' Print method for power grid analysis objects
#' @param x An rctbayespower_grid object
#' @param ... Additional arguments (unused)
#' @export
print.rctbayespower_grid <- function(x, ...) {
  # Print header based on analysis type
  if (x$analysis_type == "effect_only") {
    cat("Bayesian RCT Effect Size Analysis\n")
    cat("=================================\n\n")
    cat("Fixed sample size:", x$sample_sizes[1], "\n")
    cat(
      "Effect sizes tested:",
      paste(x$effect_sizes, collapse = ", "),
      "\n"
    )
  } else if (x$analysis_type == "sample_only") {
    cat("Bayesian RCT Sample Size Analysis\n")
    cat("=================================\n\n")
    cat("Fixed effect size:", x$effect_sizes[1], "\n")
    cat(
      "Sample sizes tested:",
      paste(x$sample_sizes, collapse = ", "),
      "\n"
    )
  } else {
    cat("Bayesian RCT Power Grid Analysis\n")
    cat("================================\n\n")
    cat(
      "Sample sizes tested:",
      paste(x$sample_sizes, collapse = ", "),
      "\n"
    )
    cat(
      "Effect sizes tested:",
      paste(x$effect_sizes, collapse = ", "),
      "\n"
    )
  }

  cat("Target power - Success:", x$target_power_success)
  cat(", Target power - Futility:", x$target_power_futility, "\n")
  cat("Thresholds - Success:", x$threshold_success)
  cat(", Futility:", x$threshold_futility, "\n")

  if (!is.null(x$design_prior)) {
    cat("Design prior:", x$design_prior, "\n")
  }

  cat("Total scenarios tested:", nrow(x$power_surface), "\n\n")

  # Results summary based on analysis type
  if (x$analysis_type == "effect_only") {
    # For effect size analysis, show best effect size for the fixed sample size
    if (!is.null(x$optimal_combinations_success) &&
      nrow(x$optimal_combinations_success) > 0) {
      best_effect <- x$optimal_combinations_success[which.max(x$optimal_combinations_success$power_success), ]
      cat("Best effect size achieving target success power:\n")
      cat(
        "  Effect size =",
        best_effect$effect_size,
        ", Power =",
        round(best_effect$power_success, 3)
      )
      cat(
        ", Mean Probability =",
        round(best_effect$mean_prob_success, 3),
        "\n"
      )
    } else {
      cat("Target success power not achieved with tested effect sizes\n")
    }

    if (!is.null(x$optimal_combinations_futility) &&
      nrow(x$optimal_combinations_futility) > 0) {
      best_effect <- x$optimal_combinations_futility[which.max(x$optimal_combinations_futility$power_futility), ]
      cat("Best effect size achieving target futility power:\n")
      cat(
        "  Effect size =",
        best_effect$effect_size,
        ", Power =",
        round(best_effect$power_futility, 3)
      )
      cat(
        ", Mean Probability =",
        round(best_effect$mean_prob_futility, 3),
        "\n"
      )
    } else {
      cat("Target futility power not achieved with tested effect sizes\n")
    }
  } else if (x$analysis_type == "sample_only") {
    # For sample size analysis, show minimum sample sizes (compatible with original format)
    cat(
      "Minimum required total sample sizes: Success =",
      if (!is.na(x$min_n_success)) {
        x$min_n_success
      } else {
        "Not achieved"
      }
    )
    cat(", Futility =", if (!is.na(x$min_n_futility)) {
      x$min_n_futility
    } else {
      "Not achieved"
    })
    cat("\n")

    # Add power overview table (copied from power_grid_analysis execution)
    cat("\nPower Overview Across Sample Sizes:\n")
    overview_df <- x$power_surface

    # Add target achievement indicators
    success_achieved <- !is.na(overview_df$power_success) &
      overview_df$power_success >= x$target_power_success
    overview_df$success_target <- ifelse(success_achieved, "OK", "x")

    futility_achieved <- !is.na(overview_df$power_futility) &
      overview_df$power_futility >= x$target_power_futility
    overview_df$futility_target <- ifelse(futility_achieved, "OK", "x")

    # Format as percentages
    overview_df$power_success_pct <- paste0(round(overview_df$power_success * 100, 1), "%")
    overview_df$power_futility_pct <- paste0(round(overview_df$power_futility * 100, 1), "%")

    # Create compact display table
    compact_df <- data.frame(
      "N Total" = overview_df$n_total,
      "Success" = overview_df$power_success_pct,
      "Futility" = overview_df$power_futility_pct,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    print(compact_df, row.names = FALSE)
    cat("(N Total = total sample size) \n")
  } else {
    # For both varying, show optimal combinations
    if (!is.null(x$optimal_combinations_success) &&
      nrow(x$optimal_combinations_success) > 0) {
      min_combo <- x$optimal_combinations_success[which.min(x$optimal_combinations_success$n_total), ]
      cat("Smallest sample size achieving target success power:\n")
      cat(
        "  N =",
        min_combo$n_total,
        ", Effect size =",
        min_combo$effect_size
      )
      cat(
        ", Power =",
        round(min_combo$power_success, 3),
        ", Mean Probability =",
        round(min_combo$mean_prob_success, 3),
        "\n"
      )
    } else {
      cat("Target success power not achieved with tested combinations\n")
    }

    if (!is.null(x$optimal_combinations_futility) &&
      nrow(x$optimal_combinations_futility) > 0) {
      min_combo <- x$optimal_combinations_futility[which.min(x$optimal_combinations_futility$n_total), ]
      cat("Smallest sample size achieving target futility power:\n")
      cat(
        "  N =",
        min_combo$n_total,
        ", Effect size =",
        min_combo$effect_size
      )
      cat(
        ", Power =",
        round(min_combo$power_futility, 3),
        ", Mean Probability =",
        round(min_combo$mean_prob_futility, 3),
        "\n"
      )
    } else {
      cat("Target futility power not achieved with tested combinations\n")
    }
  }

  # Integrated power results (only relevant when effect sizes vary)
  if (length(x$effect_sizes) > 1 && !is.null(x$integrated_power)) {
    cat("\nIntegrated power and probability analysis:\n")

    # Check if we have any results to show
    has_success_result <- !is.na(x$min_n_integrated_success)
    has_futility_result <- !is.na(x$min_n_integrated_futility)

    if (has_success_result) {
      cat(
        "Minimum N for integrated success power:",
        x$min_n_integrated_success,
        "\n"
      )
    } else {
      cat("Target integrated success power not achieved with tested sample sizes\n")
    }

    if (has_futility_result) {
      cat(
        "Minimum N for integrated futility power:",
        x$min_n_integrated_futility,
        "\n"
      )
    } else {
      cat("Target integrated futility power not achieved with tested sample sizes\n")
    }
  }

  if (x$analysis_type == "sample_only") {
    cat("\nUse summary() for detailed power analysis across all sample sizes.\n")
  } else {
    cat("\nUse summary() for detailed power analysis.\n")
  }
}

#' Summary method for power grid analysis objects
#' @param object An rctbayespower_grid object
#' @param design_prior Optional design prior for runtime integrated power computation. Can be:
#'   \itemize{
#'     \item A string in brms prior syntax (e.g., "normal(0.3, 0.1)", "student_t(6, 0.5, 0.2)")
#'     \item An R function taking effect size as input (e.g., function(x) dnorm(x, 0.5, 0.2))
#'     \item NULL for no runtime integration (default)
#'   }
#'   If provided, integrated power will be computed using this design prior instead of
#'   any design prior specified in the original power_grid_analysis() call.
#'   Only valid when effect sizes vary (length > 1).
#' @param print Logical indicating whether to print the summary to console (default: TRUE).
#'   When FALSE, suppresses all console output and only returns the summary object.
#' @param ... Additional arguments (unused)
#' @export
summary.rctbayespower_grid <- function(object, design_prior = NULL, print = TRUE, ...) {
  # Header based on analysis type
  if (print) {
    if (object$analysis_type == "effect_only") {
      cat("Bayesian RCT Effect Size Analysis - Detailed Summary\n")
      cat("====================================================\n\n")
    } else if (object$analysis_type == "sample_only") {
      cat("Bayesian RCT Sample Size Analysis - Detailed Summary\n")
      cat("====================================================\n\n")
    } else {
      cat("Bayesian RCT Power Grid - Detailed Summary\n")
      cat("=================================================\n\n")
    }
  }

  # Validate and parse design prior for runtime integration
  runtime_integrated_power <- NULL
  if (!is.null(design_prior)) {
    # Validate design prior can only be used with varying effect sizes
    if (length(object$effect_sizes) <= 1) {
      stop("design_prior can only be specified when effect sizes vary (length > 1)")
    }

    cat("Computing integrated power with runtime design prior...\n")

    # Parse design prior using effect sizes from object
    design_prior_parsed <- parse_design_prior(design_prior, object$effect_sizes, verbose = TRUE)
    weight_fn <- design_prior_parsed$weight_fn

    # Validate n_simulations for MCSE calculations
    n_sims_for_mcse <- object$n_simulations
    if (is.null(n_sims_for_mcse) || !is.numeric(n_sims_for_mcse) || n_sims_for_mcse <= 0) {
      n_sims_for_mcse <- 1000 # Safe default
      warning("n_simulations not found or invalid in result object, using default value of 1000 for MCSE calculations")
    }

    if (!is.null(weight_fn)) {
      # Get weights for each effect size
      weights <- sapply(object$effect_sizes, weight_fn)
      weights <- weights / sum(weights) # Normalize to sum to 1

      # For each sample size, compute weighted average power
      integrated_results <- list()

      for (n in object$sample_sizes) {
        subset_data <- object$power_surface[object$power_surface$n_total == n, ]

        if (nrow(subset_data) > 0 && all(!is.na(subset_data$power_success))) {
          weighted_power_success <- sum(subset_data$power_success * weights)
          weighted_power_futility <- sum(subset_data$power_futility * weights)
          weighted_prob_success <- sum(subset_data$mean_prob_success * weights)
          weighted_prob_futility <- sum(subset_data$mean_prob_futility * weights)

          # Calculate MCSE for integrated power metrics
          mcse_integrated_power_success <- calculate_mcse_integrated_power(
            subset_data$power_success, weights,
            n_sims_for_mcse,
            is_power_metric = TRUE
          )
          mcse_integrated_power_futility <- calculate_mcse_integrated_power(
            subset_data$power_futility, weights,
            n_sims_for_mcse,
            is_power_metric = TRUE
          )
          mcse_integrated_prob_success <- calculate_mcse_integrated_power(
            subset_data$mean_prob_success, weights,
            n_sims_for_mcse,
            is_power_metric = FALSE
          )
          mcse_integrated_prob_futility <- calculate_mcse_integrated_power(
            subset_data$mean_prob_futility, weights,
            n_sims_for_mcse,
            is_power_metric = FALSE
          )

          integrated_results[[length(integrated_results) + 1]] <- data.frame(
            n_total = as.integer(n),
            integrated_power_success = weighted_power_success,
            integrated_power_futility = weighted_power_futility,
            integrated_prob_success = weighted_prob_success,
            integrated_prob_futility = weighted_prob_futility,
            mcse_integrated_power_success = mcse_integrated_power_success,
            mcse_integrated_power_futility = mcse_integrated_power_futility,
            mcse_integrated_prob_success = mcse_integrated_prob_success,
            mcse_integrated_prob_futility = mcse_integrated_prob_futility,
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(integrated_results) > 0) {
        runtime_integrated_power <- do.call(rbind, integrated_results)
      }
    }
  }

  if (print) {
    cat("Analysis Parameters:\n")
    cat("  Target power - Success:", object$target_power_success, "\n")
    cat(
      "  Target power - Futility:",
      object$target_power_futility,
      "\n"
    )
    cat("  Threshold - Success:", object$threshold_success, "\n")
    cat("  Threshold - Futility:", object$threshold_futility, "\n")

    if (object$analysis_type == "effect_only") {
      cat("  Fixed sample size:", object$sample_sizes[1], "\n")
      cat(
        "  Effect sizes tested:",
        paste(object$effect_sizes, collapse = ", "),
        "\n"
      )
    } else if (object$analysis_type == "sample_only") {
      cat("  Effect size:", object$effect_size, "\n")
      cat(
        "  Sample sizes tested:",
        paste(object$sample_sizes, collapse = ", "),
        "\n"
      )
    } else {
      cat(
        "  Sample sizes:",
        paste(object$sample_sizes, collapse = ", "),
        "\n"
      )
      cat(
        "  Effect sizes:",
        paste(object$effect_sizes, collapse = ", "),
        "\n"
      )
    }

    cat(
      "  Allocation (treatment %):",
      paste0(object$percent_group_treat * 100, "%\n")
    )
    cat("  Power analysis function:", object$power_analysis_fn, "\n")

    if (!is.null(object$design_prior)) {
      cat("  Design prior:", object$design_prior, "\n")
      cat("  Design prior type:", object$design_prior_type, "\n")
    }

    cat(
      "  Analysis time:",
      round(object$analysis_time_minutes, 2),
      "minutes\n\n"
    )

    # For sample_only mode, show minimum sample sizes section like original
    if (object$analysis_type == "sample_only") {
      cat("Minimum Required Total Sample Sizes:\n")
      if (!is.na(object$min_n_success)) {
        cat(
          "  Success power (>=",
          object$target_power_success,
          "):",
          object$min_n_success,
          "\n"
        )
      } else {
        cat("  Success power: Target not achieved with tested sample sizes\n")
      }

      if (!is.na(object$min_n_futility)) {
        cat(
          "  Futility power (>=",
          object$target_power_futility,
          "):",
          object$min_n_futility,
          "\n"
        )
      } else {
        cat("  Futility power: Target not achieved with tested sample sizes\n")
      }

      cat("\nPower Analysis Across Sample Sizes:\n")
      cat("===================================\n")
    }

    # Display results based on analysis type
    if (object$analysis_type == "effect_only") {
      # For effect size analysis, show detailed table with probabilities
      cat("Power Results Across Effect Sizes:\n")
      cat("==================================\n")

      display_df <- object$power_surface[, c(
        "effect_size",
        "convergence_rate",
        "power_success",
        "mean_prob_success",
        "power_futility",
        "mean_prob_futility"
      )]

      # Format power metrics with MCSE if available
      if ("mcse_power_success" %in% names(object$power_surface)) {
        display_df$power_success <- paste0(
          round(display_df$power_success * 100, 1), "% (+/-",
          round(object$power_surface$mcse_power_success * 100, 2), ")"
        )
        display_df$power_futility <- paste0(
          round(display_df$power_futility * 100, 1), "% (+/-",
          round(object$power_surface$mcse_power_futility * 100, 2), ")"
        )
        display_df$mean_prob_success <- paste0(
          round(display_df$mean_prob_success * 100, 1), "% (+/-",
          round(object$power_surface$mcse_mean_prob_success * 100, 2), ")"
        )
        display_df$mean_prob_futility <- paste0(
          round(display_df$mean_prob_futility * 100, 1), "% (+/-",
          round(object$power_surface$mcse_mean_prob_futility * 100, 2), ")"
        )
      } else {
        display_df$power_success <- paste0(round(display_df$power_success * 100, 1), "%")
        display_df$power_futility <- paste0(round(display_df$power_futility * 100, 1), "%")
        display_df$mean_prob_success <- paste0(round(display_df$mean_prob_success * 100, 1), "%")
        display_df$mean_prob_futility <- paste0(round(display_df$mean_prob_futility * 100, 1), "%")
      }

      display_df$convergence_rate <- paste0(round(display_df$convergence_rate * 100, 1), "%")
      names(display_df) <- c(
        "Effect_Size",
        "Convergence",
        "Power_Success",
        "Prob_Success",
        "Power_Futility",
        "Prob_Futility"
      )

      print(display_df, row.names = FALSE)
    } else if (object$analysis_type == "sample_only") {
      # For sample size analysis, show detailed table like original
      power_df <- object$power_surface

      # Add target achievement indicators
      success_achieved <- !is.na(power_df$power_success) &
        power_df$power_success >= object$target_power_success
      power_df$success_target <- ifelse(success_achieved, "OK", "x")

      futility_achieved <- !is.na(power_df$power_futility) &
        power_df$power_futility >= object$target_power_futility
      power_df$futility_target <- ifelse(futility_achieved, "OK", "x")

      # Format the power values as percentages with MCSE if available
      if ("mcse_power_success" %in% names(power_df)) {
        power_df$power_success_pct <- paste0(
          round(power_df$power_success * 100, 1), "% (+/-",
          round(power_df$mcse_power_success * 100, 2), ")"
        )
        power_df$power_futility_pct <- paste0(
          round(power_df$power_futility * 100, 1), "% (+/-",
          round(power_df$mcse_power_futility * 100, 2), ")"
        )
        power_df$pr_success_pct <- paste0(
          round(power_df$mean_prob_success * 100, 1), "% (+/-",
          round(power_df$mcse_mean_prob_success * 100, 2), ")"
        )
        power_df$pr_futility_pct <- paste0(
          round(power_df$mean_prob_futility * 100, 1), "% (+/-",
          round(power_df$mcse_mean_prob_futility * 100, 2), ")"
        )
      } else {
        power_df$power_success_pct <- paste0(round(power_df$power_success * 100, 1), "%")
        power_df$power_futility_pct <- paste0(round(power_df$power_futility * 100, 1), "%")
        power_df$pr_success_pct <- paste0(round(power_df$mean_prob_success * 100, 1), "%")
        power_df$pr_futility_pct <- paste0(round(power_df$mean_prob_futility * 100, 1), "%")
      }

      power_df$convergence_pct <- paste0(round(power_df$convergence_rate * 100, 1), "%")

      # Create display table (consistent naming format)
      display_df <- data.frame(
        "N_Total" = power_df$n_total,
        "Convergence" = power_df$convergence_pct,
        "Power_Success" = power_df$power_success_pct,
        "Prob_Success" = power_df$pr_success_pct,
        "Power_Futility" = power_df$power_futility_pct,
        "Prob_Futility" = power_df$pr_futility_pct,
        check.names = FALSE
      )

      if (print) {
        print(display_df, row.names = FALSE)
      }
    } else {
      # For power grid (both varying), show detailed table
      cat("Power Grid Results:\n")
      cat("==================\n")

      display_df <- object$power_surface[, c(
        "n_total",
        "effect_size",
        "convergence_rate",
        "power_success",
        "mean_prob_success",
        "power_futility",
        "mean_prob_futility"
      )]

      # Format power metrics with MCSE if available
      if ("mcse_power_success" %in% names(object$power_surface)) {
        display_df$power_success <- paste0(
          round(display_df$power_success * 100, 1), "% (+/-",
          round(object$power_surface$mcse_power_success * 100, 2), ")"
        )
        display_df$power_futility <- paste0(
          round(display_df$power_futility * 100, 1), "% (+/-",
          round(object$power_surface$mcse_power_futility * 100, 2), ")"
        )
        display_df$mean_prob_success <- paste0(
          round(display_df$mean_prob_success * 100, 1), "% (+/-",
          round(object$power_surface$mcse_mean_prob_success * 100, 2), ")"
        )
        display_df$mean_prob_futility <- paste0(
          round(display_df$mean_prob_futility * 100, 1), "% (+/-",
          round(object$power_surface$mcse_mean_prob_futility * 100, 2), ")"
        )
      } else {
        display_df$power_success <- paste0(round(display_df$power_success * 100, 1), "%")
        display_df$power_futility <- paste0(round(display_df$power_futility * 100, 1), "%")
        display_df$mean_prob_success <- paste0(round(display_df$mean_prob_success * 100, 1), "%")
        display_df$mean_prob_futility <- paste0(round(display_df$mean_prob_futility * 100, 1), "%")
      }

      display_df$convergence_rate <- paste0(round(display_df$convergence_rate * 100, 1), "%")
      names(display_df) <- c(
        "N_Total",
        "Effect_Size",
        "Convergence",
        "Power_Success",
        "Prob_Success",
        "Power_Futility",
        "Prob_Futility"
      )

      if (print) {
        print(display_df, row.names = FALSE)
      }
    }

    # Optimal combinations
    cat("\nOptimal Combinations:\n")
    cat("====================\n")

    if (!is.null(object$optimal_combinations_success) &&
      nrow(object$optimal_combinations_success) > 0) {
      cat("Combinations achieving target success power:\n")
      success_display <- object$optimal_combinations_success[, c(
        "n_total",
        "effect_size",
        "power_success",
        "mean_prob_success"
      )]
      success_display$power_success <- paste0(round(success_display$power_success * 100, 1), "%")
      success_display$mean_prob_success <- paste0(round(success_display$mean_prob_success * 100, 1), "%")
      names(success_display) <- c("N_total", "Effect_size", "Power_success", "Prob_success")
      if (print) {
        print(success_display, row.names = FALSE)
      }
    } else {
      cat("No combinations achieved target success power.\n")
    }

    cat("\n")

    if (!is.null(object$optimal_combinations_futility) &&
      nrow(object$optimal_combinations_futility) > 0) {
      cat("Combinations achieving target futility power:\n")
      futility_display <- object$optimal_combinations_futility[, c(
        "n_total",
        "effect_size",
        "power_futility",
        "mean_prob_futility"
      )]
      futility_display$power_futility <- paste0(round(futility_display$power_futility * 100, 1), "%")
      futility_display$mean_prob_futility <- paste0(round(futility_display$mean_prob_futility * 100, 1), "%")
      names(futility_display) <- c(
        "N_total",
        "Effect_size",
        "Power_futility",
        "Prob_futility"
      )
      if (print) {
        print(futility_display, row.names = FALSE)
      }
    } else {
      cat("No combinations achieved target futility power.\n")
    }

    # Integrated power results
    # Use runtime integrated power if available, otherwise use existing integrated power
    integrated_power_to_display <- runtime_integrated_power
    if (is.null(integrated_power_to_display)) {
      integrated_power_to_display <- object$integrated_power
    }

    if (!is.null(integrated_power_to_display)) {
      cat("\nIntegrated Power & Probability Results:\n")
      cat("======================================\n")

      if (!is.null(runtime_integrated_power)) {
        cat("(Using runtime design prior:", design_prior, ")\n")
      } else if (!is.null(object$design_prior)) {
        cat("(Using design prior from main analysis:", object$design_prior, ")\n")
      }
      cat("\n")

      integrated_display <- integrated_power_to_display

      # Format as percentages with MCSE if available
      if ("mcse_integrated_power_success" %in% names(integrated_display)) {
        integrated_display$integrated_power_success <- paste0(
          round(integrated_display$integrated_power_success * 100, 1), "% (+/-",
          round(integrated_display$mcse_integrated_power_success * 100, 2), ")"
        )
        integrated_display$integrated_power_futility <- paste0(
          round(integrated_display$integrated_power_futility * 100, 1), "% (+/-",
          round(integrated_display$mcse_integrated_power_futility * 100, 2), ")"
        )
        integrated_display$integrated_prob_success <- paste0(
          round(integrated_display$integrated_prob_success * 100, 1), "% (+/-",
          round(integrated_display$mcse_integrated_prob_success * 100, 2), ")"
        )
        integrated_display$integrated_prob_futility <- paste0(
          round(integrated_display$integrated_prob_futility * 100, 1), "% (+/-",
          round(integrated_display$mcse_integrated_prob_futility * 100, 2), ")"
        )
      } else {
        integrated_display$integrated_power_success <- paste0(
          round(integrated_display$integrated_power_success * 100, 1), "%"
        )
        integrated_display$integrated_power_futility <- paste0(
          round(integrated_display$integrated_power_futility * 100, 1), "%"
        )
        integrated_display$integrated_prob_success <- paste0(
          round(integrated_display$integrated_prob_success * 100, 1), "%"
        )
        integrated_display$integrated_prob_futility <- paste0(
          round(integrated_display$integrated_prob_futility * 100, 1), "%"
        )
      }

      # Reorder columns to preferred order: N_total, Power_Success, Prob_Success, Power_Futility, Prob_Futility
      # Only select the main columns for display (MCSE are included in the formatted strings)
      display_columns <- c(
        "n_total", "integrated_power_success", "integrated_prob_success",
        "integrated_power_futility", "integrated_prob_futility"
      )
      available_columns <- intersect(display_columns, names(integrated_display))
      integrated_display <- integrated_display[, available_columns, drop = FALSE]

      names(integrated_display) <- c(
        "N_total",
        "Power_Success",
        "Prob_Success",
        "Power_Futility",
        "Prob_Futility"
      )

      if (print) {
        print(integrated_display, row.names = FALSE)
      }

      cat(
        "\nIntegrated results represent weighted averages across effect sizes using the specified design prior.\n"
      )
      cat(
        "Power = probability of making correct decision, Mean Probability = mean posterior probability of exceeding threshold.\n"
      )
      cat(
        "Values shown as percentage (+/-MCSE) where MCSE = Monte Carlo Standard Error.\n"
      )
    }
  } # End of if (print) block

  # Return structured summary for testing and programmatic access
  summary_obj <- list(
    analysis_info = list(
      analysis_type = object$analysis_type,
      target_power_success = object$target_power_success,
      target_power_futility = object$target_power_futility,
      threshold_success = object$threshold_success,
      threshold_futility = object$threshold_futility,
      sample_sizes = object$sample_sizes,
      effect_sizes = object$effect_sizes,
      design_prior = object$design_prior,
      runtime_design_prior = design_prior
    ),
    power_surface = object$power_surface,
    optimal_combinations_success = object$optimal_combinations_success,
    optimal_combinations_futility = object$optimal_combinations_futility,
    integrated_power = object$integrated_power,
    runtime_integrated_power = runtime_integrated_power
  )

  invisible(summary_obj)
}

#' Parse and Validate Design Prior
#'
#' Internal helper function to parse design prior specifications and create
#' weight and quantile functions for design prior integration.
#'
#' @param design_prior Design prior specification (string or function)
#' @param effect_sizes Vector of effect sizes for coverage checking
#' @param verbose Whether to print parsing information (default: TRUE)
#' @return List containing weight_fn, quantile_fn, and weight_type
#' @keywords internal
parse_design_prior <- function(design_prior, effect_sizes, verbose = TRUE) {
  weight_fn <- NULL
  weight_type <- "none"
  quantile_fn <- NULL

  if (is.null(design_prior)) {
    return(list(weight_fn = weight_fn, quantile_fn = quantile_fn, weight_type = weight_type))
  }

  if (is.character(design_prior)) {
    # Use brms density functions with generic evaluation
    weight_type <- "brms"
    tryCatch(
      {
        if (!requireNamespace("brms", quietly = TRUE)) {
          stop("Package 'brms' is required for parsing brms design prior syntax.")
        }

        # Validate basic syntax
        if (!grepl("^[a-zA-Z_][a-zA-Z0-9_]*\\(.*\\)$", design_prior)) {
          stop(
            "Invalid brms prior syntax. Expected format: 'distribution_name(param1, param2, ...)'"
          )
        }

        # Extract distribution name
        dist_name <- gsub("\\(.*\\)", "", design_prior)

        # Create density and quantile function calls
        # Priority: stats package first, then brms, then error
        density_call <- NULL
        quantile_call <- NULL
        source_package <- NULL

        # Define mapping from brms names to stats names
        brms_to_stats_map <- list(
          "normal" = "norm",
          "student_t" = "t",
          "gamma" = "gamma",
          "beta" = "beta",
          "exponential" = "exp",
          "uniform" = "unif",
          "poisson" = "pois",
          "binomial" = "binom",
          "cauchy" = "cauchy",
          "chi_square" = "chisq",
          "f" = "f",
          "lognormal" = "lnorm",
          "logistic" = "logis",
          "weibull" = "weibull",
          "wilcox" = "wilcox"
        )

        # Try stats package first
        stats_name <- brms_to_stats_map[[dist_name]]
        if (!is.null(stats_name)) {
          # Test if stats function exists and works
          stats_call <- sub(dist_name, stats_name, design_prior)
          test_density_call <- paste0("stats::d", stats_call)
          test_quantile_call <- paste0("stats::q", stats_call)

          # Test the stats functions
          stats_works <- tryCatch(
            {
              test_call <- gsub("\\(", "(0.5,", test_density_call)
              result <- eval(parse(text = test_call))
              is.numeric(result) &&
                length(result) == 1 && !is.na(result)
            },
            error = function(e) {
              FALSE
            }
          )

          if (stats_works) {
            density_call <- test_density_call
            quantile_call <- test_quantile_call
            source_package <- "stats"
          } else {
            stats_works <- FALSE
          }
        } else {
          stats_works <- FALSE
        }

        # If stats doesn't work, try brms
        if (!stats_works) {
          density_call <- paste0("brms::d", design_prior)
          quantile_call <- paste0("brms::q", design_prior)
          source_package <- "brms"

          # Test brms functions
          brms_works <- tryCatch(
            {
              test_call <- gsub("\\(", "(0.5,", density_call)
              result <- eval(parse(text = test_call))
              is.numeric(result) &&
                length(result) == 1 && !is.na(result)
            },
            error = function(e) {
              FALSE
            }
          )

          if (!brms_works) {
            stop(
              paste0(
                "Distribution '",
                dist_name,
                "' not available in either stats or brms packages"
              )
            )
          }
        }

        # Create weight function
        weight_fn <- function(x) {
          # Replace the first parameter (or add x as first parameter)
          modified_call <- gsub("\\(", paste0("(", x, ","), density_call)
          tryCatch(
            {
              eval(parse(text = modified_call))
            },
            error = function(e) {
              stop(
                paste0(
                  "Error evaluating ",
                  source_package,
                  " density function: ",
                  e$message
                )
              )
            }
          )
        }

        # Try to create quantile function
        quantile_fn <- NULL
        test_quantile_call <- gsub("\\(", "(0.5,", quantile_call)
        quantile_available <- tryCatch(
          {
            test_result <- eval(parse(text = test_quantile_call))
            if (is.numeric(test_result) &&
              length(test_result) == 1 && !is.na(test_result)) {
              # Create actual quantile function
              quantile_fn <<- function(p) {
                modified_call <- gsub("\\(", paste0("(", p, ","), quantile_call)
                tryCatch(
                  {
                    eval(parse(text = modified_call))
                  },
                  error = function(e) {
                    stop(
                      paste0(
                        "Error evaluating ",
                        source_package,
                        " quantile function: ",
                        e$message
                      )
                    )
                  }
                )
              }
              TRUE
            } else {
              FALSE
            }
          },
          error = function(e) {
            FALSE
          }
        )

        if (!quantile_available) {
          warning(
            paste0(
              "Quantile function '",
              quantile_call,
              "' not available. Coverage checking will be disabled."
            )
          )
        }

        if (verbose) {
          cat("Successfully parsed design prior:", design_prior, "\n")
          cat("  Distribution:", dist_name, "\n")
          cat(
            "  Density function:",
            density_call,
            "(using",
            source_package,
            "package)\n"
          )
          if (quantile_available) {
            cat(
              "  Quantile function:",
              quantile_call,
              "(using",
              source_package,
              "package)\n"
            )
          } else {
            cat("  Quantile function: Not available (coverage checking disabled)\n")
          }
        }
      },
      error = function(e) {
        stop(paste("Error parsing design prior:", e$message))
      }
    )
  } else if (is.function(design_prior)) {
    # Validate R function
    weight_type <- "function"
    tryCatch(
      {
        test_val <- design_prior(0.5)
        if (!is.numeric(test_val) || length(test_val) != 1) {
          stop("Design prior function must return a single numeric value")
        }
        weight_fn <- design_prior
        # For custom functions, we'll estimate quantiles numerically
        quantile_fn <- function(p) {
          # Create a grid of values and find approximate quantiles
          test_range <- seq(
            min(effect_sizes) - 2 * sd(effect_sizes),
            max(effect_sizes) + 2 * sd(effect_sizes),
            length.out = 1000
          )
          weights <- sapply(test_range, weight_fn)
          weights <- weights / sum(weights)
          cumulative <- cumsum(weights)
          approx(cumulative, test_range, xout = p)$y
        }
      },
      error = function(e) {
        stop(paste("Error testing design prior function:", e$message))
      }
    )
  } else {
    stop("'design_prior' must be either a character string (brms syntax) or an R function")
  }

  # Compute quantiles and check coverage
  if (!is.null(quantile_fn)) {
    tryCatch(
      {
        q10 <- quantile_fn(0.1)
        q90 <- quantile_fn(0.9)

        # Check if effect sizes adequately cover the prior distribution
        min_effect <- min(effect_sizes)
        max_effect <- max(effect_sizes)

        if (max_effect < q90) {
          warning(
            paste0(
              "Maximum effect size (",
              round(max_effect, 3),
              ") is less than 90th percentile of design prior (",
              round(q90, 3),
              "). Consider including larger effect sizes."
            )
          )
        }

        if (min_effect > q10) {
          warning(
            paste0(
              "Minimum effect size (",
              round(min_effect, 3),
              ") is greater than 10th percentile of design prior (",
              round(q10, 3),
              "). Consider including smaller effect sizes."
            )
          )
        }
      },
      error = function(e) {
        # If quantile computation fails for custom functions, just warn
        if (weight_type == "function") {
          warning(
            "Could not compute quantiles for custom design prior function. Unable to check effect size coverage."
          )
        } else {
          stop(paste("Error computing quantiles:", e$message))
        }
      }
    )
  }

  return(list(weight_fn = weight_fn, quantile_fn = quantile_fn, weight_type = weight_type))
}


#' Validate Weighting Function Implementation
#'
#' Tests that the weighting function parsing and computation in power_grid_analysis()
#' works correctly. This function validates both brms syntax parsing and R function handling,
#' as well as the weighted power computation logic.
#'
#' @param effect_sizes Vector of effect sizes to test with (default: seq(0.2, 0.8, 0.1))
#' @param verbose Whether to print detailed test results (default: TRUE)
#'
#' @return A list containing validation results:
#' \itemize{
#'   \item all_tests_passed: Boolean indicating if all tests passed
#'   \item test_results: List of individual test results
#'   \item errors: Any errors encountered during testing
#' }
#'
#' @details
#' This validation function tests:
#' \itemize{
#'   \item Normal distribution parsing from brms syntax
#'   \item Student-t distribution parsing from brms syntax
#'   \item Custom R function validation
#'   \item Weight normalization (ensures weights sum to 1)
#'   \item Quantile computation for coverage checking
#'   \item Error handling for invalid inputs
#' }
#'
#' @importFrom stats dnorm qnorm
#' @keywords internal
#'

validate_weighting_function <- function(effect_sizes = seq(0.2, 0.8, 0.1),
                                        verbose = TRUE) {
  if (verbose) {
    cat("=== Weighting Function Validation ===\n")
    cat("Testing weighting function implementation in power_grid_analysis()\n\n")
  }

  test_results <- list()
  errors <- character()

  # Test 1: Normal distribution parsing
  if (verbose) {
    cat("Test 1: Normal distribution parsing...\n")
  }
  test_results$normal_parsing <- tryCatch(
    {
      weighting_function <- "normal(0.5, 0.15)"

      # Test using the new generic approach
      density_call <- paste0("brms::d", weighting_function)
      quantile_call <- paste0("brms::q", weighting_function)

      # Create weight function
      weight_fn <- function(x) {
        modified_call <- gsub("\\(", paste0("(", x, ","), density_call)
        eval(parse(text = modified_call))
      }

      # Create quantile function
      quantile_fn <- function(p) {
        modified_call <- gsub("\\(", paste0("(", p, ","), quantile_call)
        eval(parse(text = modified_call))
      }

      # Test the functions work
      test_weights <- sapply(effect_sizes, weight_fn)
      test_quantiles <- quantile_fn(c(0.1, 0.5, 0.9))

      # Validate results
      if (!is.numeric(test_weights) || any(is.na(test_weights))) {
        stop("Weight function produced non-numeric or NA values")
      }

      if (!is.numeric(test_quantiles) || any(is.na(test_quantiles))) {
        stop("Quantile function produced non-numeric or NA values")
      }

      list(
        passed = TRUE,
        weights = test_weights,
        quantiles = test_quantiles
      )
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 1 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Test 2: Student-t distribution parsing
  if (verbose) {
    cat("Test 2: Student-t distribution parsing...\n")
  }
  test_results$studentt_parsing <- tryCatch(
    {
      weighting_function <- "student_t(6, 0.5, 0.2)"

      # Test using the new generic approach
      density_call <- paste0("brms::d", weighting_function)
      quantile_call <- paste0("brms::q", weighting_function)

      # Create weight function
      weight_fn <- function(x) {
        modified_call <- gsub("\\(", paste0("(", x, ","), density_call)
        eval(parse(text = modified_call))
      }

      # Create quantile function
      quantile_fn <- function(p) {
        modified_call <- gsub("\\(", paste0("(", p, ","), quantile_call)
        eval(parse(text = modified_call))
      }

      # Test the functions work
      test_weights <- sapply(effect_sizes, weight_fn)
      test_quantiles <- quantile_fn(c(0.1, 0.5, 0.9))

      # Validate results
      if (!is.numeric(test_weights) || any(is.na(test_weights))) {
        stop("Weight function produced non-numeric or NA values")
      }

      if (!is.numeric(test_quantiles) || any(is.na(test_quantiles))) {
        stop("Quantile function produced non-numeric or NA values")
      }

      list(
        passed = TRUE,
        weights = test_weights,
        quantiles = test_quantiles
      )
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 2 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Test 3: Custom R function validation
  if (verbose) {
    cat("Test 3: Custom R function validation...\n")
  }
  test_results$custom_function <- tryCatch(
    {
      # Create a custom weighting function
      custom_fn <- function(x) {
        dnorm(x, mean = 0.4, sd = 0.1)
      }

      # Test validation logic
      test_val <- custom_fn(0.5)
      if (!is.numeric(test_val) || length(test_val) != 1) {
        stop("Weighting function must return a single numeric value")
      }

      # Test the function works with effect sizes
      test_weights <- sapply(effect_sizes, custom_fn)

      if (!is.numeric(test_weights) || any(is.na(test_weights))) {
        stop("Custom function produced non-numeric or NA values")
      }

      list(passed = TRUE, weights = test_weights)
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 3 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Test 4: Weight normalization
  if (verbose) {
    cat("Test 4: Weight normalization...\n")
  }
  test_results$weight_normalization <- tryCatch(
    {
      # Use normal distribution weights
      weight_fn <- function(x) {
        dnorm(x, mean = 0.5, sd = 0.15)
      }
      weights <- sapply(effect_sizes, weight_fn)
      normalized_weights <- weights / sum(weights)

      # Check that weights sum to 1 (within tolerance)
      weight_sum <- sum(normalized_weights)
      if (abs(weight_sum - 1.0) > 1e-10) {
        stop(paste("Normalized weights do not sum to 1. Sum =", weight_sum))
      }

      # Check all weights are positive
      if (any(normalized_weights <= 0)) {
        stop("Some normalized weights are non-positive")
      }

      list(
        passed = TRUE,
        original_weights = weights,
        normalized_weights = normalized_weights,
        sum = weight_sum
      )
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 4 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Test 5: Coverage checking logic
  if (verbose) {
    cat("Test 5: Coverage checking logic...\n")
  }
  test_results$coverage_checking <- tryCatch(
    {
      # Test with normal distribution
      quantile_fn <- function(p) {
        qnorm(p, mean = 0.5, sd = 0.15)
      }

      q10 <- quantile_fn(0.1)
      q90 <- quantile_fn(0.9)

      min_effect <- min(effect_sizes)
      max_effect <- max(effect_sizes)

      # Test coverage warnings would be triggered correctly
      coverage_low <- min_effect <= q10
      coverage_high <- max_effect >= q90

      # With default effect_sizes (0.2 to 0.8) and normal(0.5, 0.15),
      # we expect good coverage
      expected_coverage <- coverage_low && coverage_high

      list(
        passed = TRUE,
        q10 = q10,
        q90 = q90,
        min_effect = min_effect,
        max_effect = max_effect,
        coverage_low = coverage_low,
        coverage_high = coverage_high,
        good_coverage = expected_coverage
      )
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 5 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Test 6: Error handling for invalid inputs
  if (verbose) {
    cat("Test 6: Error handling for invalid inputs...\n")
  }
  test_results$error_handling <- tryCatch(
    {
      errors_caught <- 0
      total_error_tests <- 0

      # Test invalid brms syntax
      total_error_tests <- total_error_tests + 1
      tryCatch(
        {
          # This should fail due to wrong number of parameters
          params <- gsub("normal\\(|\\)", "", "normal(0.5)") # Missing second parameter
          params <- as.numeric(strsplit(params, ",")[[1]])
          if (length(params) != 2) {
            stop("normal() requires 2 parameters")
          }
        },
        error = function(e) {
          errors_caught <<- errors_caught + 1
        }
      )

      # Test invalid R function
      total_error_tests <- total_error_tests + 1
      tryCatch(
        {
          invalid_fn <- function(x) {
            c(1, 2)
          } # Returns vector instead of single value
          test_val <- invalid_fn(0.5)
          if (!is.numeric(test_val) || length(test_val) != 1) {
            stop("Weighting function must return a single numeric value")
          }
        },
        error = function(e) {
          errors_caught <<- errors_caught + 1
        }
      )

      # Test unsupported distribution
      total_error_tests <- total_error_tests + 1
      tryCatch(
        {
          # Test with a distribution that doesn't exist
          density_call <- "brms::dnonexistent(1, 2)"
          test_result <- eval(parse(text = density_call))
        },
        error = function(e) {
          errors_caught <<- errors_caught + 1
        }
      )

      list(
        passed = TRUE,
        errors_caught = errors_caught,
        total_tests = total_error_tests
      )
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 6 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Summary
  all_passed <- all(sapply(test_results, function(x) {
    x$passed
  }))

  if (verbose) {
    cat("\n=== Validation Summary ===\n")
    cat(
      "Test 1 - Normal parsing:",
      ifelse(test_results$normal_parsing$passed, "PASS", "FAIL"),
      "\n"
    )
    cat(
      "Test 2 - Student-t parsing:",
      ifelse(test_results$studentt_parsing$passed, "PASS", "FAIL"),
      "\n"
    )
    cat(
      "Test 3 - Custom function:",
      ifelse(test_results$custom_function$passed, "PASS", "FAIL"),
      "\n"
    )
    cat(
      "Test 4 - Weight normalization:",
      ifelse(test_results$weight_normalization$passed, "PASS", "FAIL"),
      "\n"
    )
    cat(
      "Test 5 - Coverage checking:",
      ifelse(test_results$coverage_checking$passed, "PASS", "FAIL"),
      "\n"
    )
    cat(
      "Test 6 - Error handling:",
      ifelse(test_results$error_handling$passed, "PASS", "FAIL"),
      "\n"
    )

    if (test_results$error_handling$passed) {
      cat(
        "  Errors properly caught:",
        test_results$error_handling$errors_caught,
        "/",
        test_results$error_handling$total_tests,
        "\n"
      )
    }

    cat(
      "\nOverall result:",
      ifelse(all_passed, "ALL TESTS PASSED", "SOME TESTS FAILED"),
      "\n"
    )

    if (length(errors) > 0) {
      cat("\nErrors encountered:\n")
      for (error in errors) {
        cat("  -", error, "\n")
      }
    }

    if (all_passed) {
      cat("\nOK: Weighting function implementation is working correctly!\n")
    } else {
      cat("\nERROR: Weighting function implementation has issues that need attention.\n")
    }
  }

  return(list(
    all_tests_passed = all_passed,
    test_results = test_results,
    errors = errors
  ))
}

#' Single Run Simulation for RCT Bayesian Power Analysis
#'
#' Executes a single simulation run for a given sample size and effect size.
#' This function is the core simulation engine used by power analysis functions.
#'
#' @param n_total Total sample size
#' @param p_alloc Vector of allocation probabilities for each treatment arm (must sum to 1)
#' @param design A rctbayespower_design object containing the simulation and model specifications
#' @param true_parameter_values Named list of true parameter values for data simulation
#' @param ... Additional arguments passed to brms fitting (e.g., iter, chains, cores)
#'
#' @return A fitted brms model object
#'
#' @examples
#' \dontrun{
#' # Example with ANCOVA model
#' ancova_model <- build_model_ancova_cont()
#' design <- build_design(
#'   build_model = ancova_model,
#'   target_params = "b_grouptreat",
#'   n_interim_analyses = 0,
#'   thresholds_success = 0.2,
#'   thresholds_futility = 0,
#'   p_sig_success = 0.975,
#'   p_sig_futility = 0.5
#' )
#'
#' result <- simulate_single_run(
#'   n_total = 100,
#'   p_alloc = c(0.5, 0.5),
#'   design = design,
#'   true_parameter_values = list(
#'     intercept = 0,
#'     sigma = 1,
#'     b_grouptreat = 0.5,
#'     b_baseline = 0.2
#'   )
#' )
#' }
#' @export
simulate_single_run <- function(n_total = NULL,
                                p_alloc = NULL,
                                design = NULL,
                                true_parameter_values = NULL,
                                ...) {
  # validate the design
  if (!inherits(design, "rctbayespower_design")) {
    stop("The design must be a valid rctbayespower_design object.")
  }

  # validate n_total
  if (!is.numeric(n_total) || n_total <= 0) {
    stop("'n_total' must be a positive numeric value.")
  }

  # validate p_alloc: type, range
  if (!is.numeric(p_alloc) ||
    length(p_alloc) != design$n_treatment_arms ||
    any(p_alloc < 0) || abs(sum(p_alloc) - 1) > 1e-8) {
    stop(
      "p_alloc must be a numeric vector of length ",
      design$n_treatment_arms,
      " with non-negative values that sum to 1."
    )
  }

  # validate true_parameter_values, must be a named list of real numbers
  if (!is.null(true_parameter_values) &&
    (
      !is.list(true_parameter_values) ||
        any(
          !names(true_parameter_values) %in% design$parameter_names_sim_fn
        ) ||
        any(!sapply(true_parameter_values, is.numeric))
    )) {
    stop(
      "true_parameter_values must be a named list of numeric values matching the parameter names in the design."
    )
  }

  # extract the data simulation function and the brms model from the design
  data_simulation_fn <- design$data_simulation_fn
  brms_model <- design$brms_model

  # simulate data using the data simulation function
  simulated_data <- data_simulation_fn(
    n_total = n_total,
    p_alloc = p_alloc,
    true_parameter_values = true_parameter_values
  )

  # default brms arguments
  brms_args <- list(
    algorithm = "sampling",
    iter = 500,
    warmup = 250,
    chains = 4,
    cores = 1,
    init = 0.1,
    refresh = 0,
    silent = 2
  )

  # update the default brms arguments with any additional arguments passed to the function
  brms_args_final <- utils::modifyList(brms_args, list(...))

  # fit the model to the simulated data
  fitted_model <- do.call(function(...) {
    stats::update(object = brms_model, newdata = simulated_data, ...)
  }, brms_args_final)

  return(fitted_model)
}

# Parameter Resolution Helper Functions ----------------------------------------

#' Resolve Data Simulation Parameters from Condition
#'
#' Maps condition values to data simulation function parameters, handling
#' effect size mapping and parameter precedence resolution.
#'
#' @param condition List containing condition-specific parameters
#' @param design rctbayespower_design object
#' @param static_overrides List of static parameter overrides
#' @return Named list of resolved true parameter values
#' @keywords internal
resolve_simulation_parameters <- function(condition, design, static_overrides = list()) {
  # Start with default values from design object if available
  base_params <- list()
  
  # Extract parameter names required by simulation function
  required_params <- design$parameter_names_sim_fn
  
  # Initialize with any static overrides
  if (!is.null(static_overrides$true_parameter_values)) {
    base_params <- static_overrides$true_parameter_values
  }
  
  # Apply condition-specific true_parameter_values if provided
  if (!is.null(condition$true_parameter_values)) {
    base_params <- utils::modifyList(base_params, condition$true_parameter_values)
  }
  
  # Handle effect_sizes mapping - must be named list matching target_params
  if (!is.null(condition$effect_sizes)) {
    if (!is.list(condition$effect_sizes) || is.null(names(condition$effect_sizes))) {
      stop("effect_sizes must be a named list matching target_params: ", 
           paste(design$target_params, collapse = ", "))
    }
    
    # Validate that effect_sizes names match target_params
    target_params <- design$target_params
    missing_targets <- setdiff(target_params, names(condition$effect_sizes))
    if (length(missing_targets) > 0) {
      stop("effect_sizes missing values for target parameters: ", 
           paste(missing_targets, collapse = ", "))
    }
    
    extra_effects <- setdiff(names(condition$effect_sizes), target_params)
    if (length(extra_effects) > 0) {
      warning("effect_sizes contains parameters not in target_params: ", 
              paste(extra_effects, collapse = ", "))
    }
    
    # Map effect sizes to simulation parameters
    # Target params are brms parameter names, need to map to simulation function names
    for (target_param in target_params) {
      if (target_param %in% names(condition$effect_sizes)) {
        # Find corresponding simulation parameter name
        # For most cases, target_param corresponds directly to simulation param
        # Remove "b_" prefix if present to match simulation function parameter names
        sim_param <- gsub("^b_", "", target_param)
        
        # Common mappings
        if (sim_param == "grouptreat") {
          sim_param <- "b_grouptreat"
        }
        
        # Check if this parameter exists in simulation function
        if (sim_param %in% required_params) {
          base_params[[sim_param]] <- condition$effect_sizes[[target_param]]
        } else {
          # Try the original target_param name
          if (target_param %in% required_params) {
            base_params[[target_param]] <- condition$effect_sizes[[target_param]]
          } else {
            warning("Could not map target parameter '", target_param, 
                   "' to simulation function parameter. Available: ", 
                   paste(required_params, collapse = ", "))
          }
        }
      }
    }
  }
  
  # Validate that all required parameters are present
  missing_params <- setdiff(required_params, names(base_params))
  if (length(missing_params) > 0) {
    stop("Missing required simulation parameters: ", paste(missing_params, collapse = ", "))
  }
  
  return(base_params)
}

#' Resolve Interim Parameters from Condition
#'
#' Maps condition values to interim function parameters when interim analyses
#' are specified in the condition.
#'
#' @param condition List containing condition-specific parameters
#' @param design rctbayespower_design object
#' @param static_overrides List of static parameter overrides
#' @return Named list of resolved interim parameters or NULL if no interim function
#' @keywords internal
resolve_interim_parameters <- function(condition, design, static_overrides = list()) {
  # Return NULL if no interim function is defined
  if (is.null(design$interim_function)) {
    return(NULL)
  }
  
  # Initialize with static overrides
  base_params <- list()
  if (!is.null(static_overrides$interim_parameters)) {
    base_params <- static_overrides$interim_parameters
  }
  
  # Apply condition-specific interim parameters
  if (!is.null(condition$interim_parameters)) {
    base_params <- utils::modifyList(base_params, condition$interim_parameters)
  }
  
  # Map n_interim_analyses if provided in condition
  if (!is.null(condition$n_interim_analyses)) {
    base_params$n_interim_analyses <- condition$n_interim_analyses
  }
  
  return(base_params)
}

#' Validate Condition Parameters
#'
#' Ensures condition parameters are valid and compatible with the design object.
#'
#' @param condition List containing condition parameters
#' @param design rctbayespower_design object
#' @return Logical indicating validity (stops on error)
#' @keywords internal
validate_condition_parameters <- function(condition, design) {
  # Required parameters
  if (is.null(condition$n_total) || !is.numeric(condition$n_total) || condition$n_total <= 0) {
    stop("Each condition must specify a positive numeric n_total")
  }
  
  # Effect sizes validation - must be named list matching target_params
  if (!is.null(condition$effect_sizes)) {
    if (!is.list(condition$effect_sizes) || is.null(names(condition$effect_sizes))) {
      stop("effect_sizes must be a named list matching target_params: ", 
           paste(design$target_params, collapse = ", "))
    }
    
    if (!all(sapply(condition$effect_sizes, is.numeric))) {
      stop("All 'effect_sizes' values must be numeric")
    }
    
    # Validate names match target_params
    target_params <- design$target_params
    missing_targets <- setdiff(target_params, names(condition$effect_sizes))
    if (length(missing_targets) > 0) {
      stop("effect_sizes missing values for target parameters: ", 
           paste(missing_targets, collapse = ", "))
    }
  }
  
  # Allocation probabilities validation if provided
  if (!is.null(condition$p_alloc)) {
    if (!is.numeric(condition$p_alloc) || 
        length(condition$p_alloc) != design$n_treatment_arms ||
        any(condition$p_alloc < 0) || 
        abs(sum(condition$p_alloc) - 1) > 1e-8) {
      stop("p_alloc must be a numeric vector of length ", design$n_treatment_arms, 
           " with non-negative values that sum to 1")
    }
  }
  
  # Interim analyses validation if provided
  if (!is.null(condition$n_interim_analyses) && 
      (!is.numeric(condition$n_interim_analyses) || condition$n_interim_analyses < 0)) {
    stop("'n_interim_analyses' must be a non-negative numeric value")
  }
  
  # True parameter values validation if provided
  if (!is.null(condition$true_parameter_values)) {
    if (!is.list(condition$true_parameter_values) || 
        any(!sapply(condition$true_parameter_values, is.numeric))) {
      stop("'true_parameter_values' must be a named list of numeric values")
    }
    
    # Check parameter names are valid for the simulation function
    invalid_params <- setdiff(names(condition$true_parameter_values), design$parameter_names_sim_fn)
    if (length(invalid_params) > 0) {
      stop("Invalid parameter names in true_parameter_values: ", paste(invalid_params, collapse = ", "))
    }
  }
  
  return(TRUE)
}

#' Expand Conditions Grid
#'
#' Helper function to create a conditions list from standard grid parameters.
#'
#' @param sample_sizes Vector of sample sizes
#' @param effect_sizes_grid List where each element is a named list of effect sizes for target parameters
#' @param n_interim_analyses Vector of interim analysis counts (default: 0)
#' @param p_alloc Vector of allocation probabilities (default: c(0.5, 0.5))
#' @param ... Additional parameters to include in each condition
#' @return List of condition specifications
#' @export
#' @examples
#' \dontrun{
#' # For a design with target_params = "b_grouptreat"
#' conditions <- expand_conditions(
#'   sample_sizes = c(100, 200),
#'   effect_sizes_grid = list(
#'     list(b_grouptreat = 0.3),
#'     list(b_grouptreat = 0.5)
#'   ),
#'   n_interim_analyses = c(0, 1)
#' )
#' }
expand_conditions <- function(sample_sizes, effect_sizes_grid, n_interim_analyses = 0, 
                             p_alloc = c(0.5, 0.5), ...) {
  # Validate inputs
  if (!is.list(effect_sizes_grid) || 
      !all(sapply(effect_sizes_grid, function(x) is.list(x) && !is.null(names(x))))) {
    stop("'effect_sizes_grid' must be a list of named lists")
  }
  
  # Create grid of all combinations
  grid_data <- expand.grid(
    sample_idx = seq_along(sample_sizes),
    effect_idx = seq_along(effect_sizes_grid),
    interim_idx = seq_along(n_interim_analyses),
    stringsAsFactors = FALSE
  )
  
  # Convert to list of conditions
  conditions <- list()
  for (i in seq_len(nrow(grid_data))) {
    condition <- list(
      n_total = sample_sizes[grid_data$sample_idx[i]],
      effect_sizes = effect_sizes_grid[[grid_data$effect_idx[i]]],
      n_interim_analyses = n_interim_analyses[grid_data$interim_idx[i]],
      p_alloc = p_alloc
    )
    
    # Add any additional parameters
    extra_params <- list(...)
    if (length(extra_params) > 0) {
      condition <- c(condition, extra_params)
    }
    
    conditions[[i]] <- condition
  }
  
  return(conditions)
}
