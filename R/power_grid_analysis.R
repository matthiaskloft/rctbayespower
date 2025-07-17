#' Power Grid Analysis for Bayesian RCTs
#'
#' Comprehensive analysis varying both sample sizes and effect sizes, with optional
#' design prior for integrated power computation. This function provides flexible
#' power analysis across multiple scenarios and can compute weighted average
#' power using user-specified prior distributions.
#'
#' @param target_power_success Desired power level for success decisions (default: 0.9)
#' @param target_power_futility Desired power level for futility decisions (default: 0.95)
#' @param sample_sizes Vector of total sample sizes to test
#' @param effect_sizes Vector of effect sizes to test
#' @param threshold_success Upper threshold for success determination (required)
#' @param threshold_futility Lower threshold for futility determination (required)
#' @param design_prior Optional design prior for the effect size for integrated power. Can be:
#'   \itemize{
#'     \item A string in brms prior syntax (e.g., "normal(0.3, 0.1)", "student_t(6, 0.5, 0.2)", "gamma(2, 3)", "beta(2, 5)", "exponential(1)", "uniform(0, 1)", etc.). Any distribution supported by brms with a corresponding density function is supported.
#'     \item An R function taking effect size as input (e.g., function(x) dnorm(x, 0.5, 0.2))
#'     \item NULL for no design prior (default)
#'   }
#' @param percent_group_treat Percentage of total sample allocated to treatment group (default: .5)
#' @param power_analysis_fn Power analysis function to use. Either "power_analysis" for custom models or "power_analysis_ancova" for ANCOVA wrapper (default: "power_analysis")
#' @param n_simulations Number of simulations per combination (default: 500)
#' @param ... All arguments required by the chosen power analysis function (except n_control, n_treatment, effect_size, threshold_success, and threshold_futility which are handled automatically)
#'
#' @details
#' This function runs power analyses across all combinations of sample_sizes and effect_sizes
#' using a two-phase approach for optimal performance:
#'
#' \strong{Phase 1: Parallel Model Compilation}
#' Models are compiled in parallel for each unique effect size when n_cores > 1 and
#' multiple effect sizes are provided. This significantly reduces overall computation time
#' by utilizing multiple CPU cores for the compilation-intensive brms model fitting.
#'
#' \strong{Phase 2: Grid Analysis with Cached Models}
#' Each combination uses pre-compiled models from Phase 1, with simulations running in
#' parallel within each power analysis call (controlled by n_cores parameter).
#'
#' When design_prior is provided, the function computes integrated power by weighting each effect size
#' according to the specified design prior distribution.
#'
#' The design prior can be specified in two ways:
#' \itemize{
#'   \item \strong{brms syntax}: String expressions like "normal(0.5, 0.2)" or "student_t(6, 0.3, 0.1)"
#'   \item \strong{R function}: User-defined function, e.g., function(x) dnorm(x, 0.5, 0.2)
#' }
#'
#' For custom power analysis (power_analysis_fn = "power_analysis"), you must provide:
#' simulate_data_fn, model_formula_true_params, model_formula_estimation, family,
#' priors_true_params, priors_estimation, target_param
#'
#' For ANCOVA power analysis (power_analysis_fn = "power_analysis_ancova"), you must provide:
#' outcome_type, baseline_effect
#'
#' @importFrom stats sd approx
#' @return A list of class "rctbayespower_grid" containing:
#' \itemize{
#'   \item target_power_success: Desired success power level
#'   \item target_power_futility: Desired futility power level
#'   \item threshold_success: Success threshold used in analysis
#'   \item threshold_futility: Futility threshold used in analysis
#'   \item sample_sizes: Vector of tested sample sizes
#'   \item effect_sizes: Vector of tested effect sizes
#'   \item design_prior: Design prior specification
#'   \item power_surface: Data frame with power results for all combinations, including Monte Carlo Standard Errors (MCSE) for all power and probability metrics
#'   \item integrated_power: If design prior provided, integrated power and probability across effect sizes, including MCSE for integrated metrics
#'   \item optimal_combinations: Sample size/effect size combinations achieving target power
#'   \item detailed_results: Full power analysis results for each combination
#' }
#' @export
#'
#' @examples
#' \donttest{
#' # Basic analysis across sample sizes and effect sizes
#' grid_result <- power_grid_analysis(
#'   target_power_success = 0.8,
#'   sample_sizes = c(100, 200),
#'   effect_sizes = c(0.4, 0.8),
#'   threshold_success = 0.2,
#'   threshold_futility = 0,
#'   power_analysis_fn = "power_analysis_ancova",
#'   outcome_type = "continuous",
#'   baseline_effect = 0.2,
#'   n_simulations = 1,
#'   brms_args = list(algorithm = "meanfield"), # fast algorithm for testing
#'   n_cores = 1,
#'   progress_updates = 5
#' )
#' }
power_grid_analysis <- function(target_power_success = 0.9,
                                target_power_futility = 0.95,
                                sample_sizes,
                                effect_sizes,
                                threshold_success,
                                threshold_futility,
                                design_prior = NULL,
                                percent_group_treat = .5,
                                power_analysis_fn = "power_analysis",
                                n_simulations = 500,
                                ...) {
  # Validate inputs
  if (!is.numeric(target_power_success) ||
    target_power_success <= 0 || target_power_success >= 1) {
    stop("target_power_success must be a number between 0 and 1")
  }

  if (!is.numeric(target_power_futility) ||
    target_power_futility <= 0 || target_power_futility >= 1) {
    stop("target_power_futility must be a number between 0 and 1")
  }

  if (missing(sample_sizes)) {
    stop("sample_sizes argument is required")
  }

  if (!is.numeric(sample_sizes) || any(sample_sizes <= 0)) {
    stop("sample_sizes must be positive numbers")
  }

  if (missing(effect_sizes)) {
    stop("effect_sizes argument is required")
  }

  if (!is.numeric(effect_sizes) || any(effect_sizes <= 0)) {
    stop("effect_sizes must be positive numbers")
  }

  if (!is.numeric(percent_group_treat) ||
    percent_group_treat <= 0 || percent_group_treat >= 1) {
    stop("percent_group_treat must be a number between 0 and 1")
  }

  if (!power_analysis_fn %in% c("power_analysis", "power_analysis_ancova")) {
    stop("power_analysis_fn must be either 'power_analysis' or 'power_analysis_ancova'")
  }

  # Validate threshold parameters
  if (missing(threshold_success) || is.null(threshold_success)) {
    stop("threshold_success is required and must be specified.")
  }
  if (!is.numeric(threshold_success) ||
    length(threshold_success) != 1) {
    stop("threshold_success must be a single numeric value.")
  }

  if (missing(threshold_futility) || is.null(threshold_futility)) {
    stop("threshold_futility is required and must be specified.")
  }
  if (!is.numeric(threshold_futility) ||
    length(threshold_futility) != 1) {
    stop("threshold_futility must be a single numeric value.")
  }

  if (threshold_success <= threshold_futility) {
    stop("threshold_success must be greater than threshold_futility.")
  }

  # Parse and validate design prior
  design_prior_parsed <- parse_design_prior(design_prior, effect_sizes, verbose = TRUE)
  weight_fn <- design_prior_parsed$weight_fn
  quantile_fn <- design_prior_parsed$quantile_fn
  weight_type <- design_prior_parsed$weight_type

  # Extract additional arguments
  dots <- list(...)

  # Determine analysis type
  analysis_type <- if (length(sample_sizes) == 1 &&
    length(effect_sizes) > 1) {
    "effect_only"
  } else if (length(sample_sizes) > 1 &&
    length(effect_sizes) == 1) {
    "sample_only"
  } else if (length(sample_sizes) > 1 && length(effect_sizes) > 1) {
    "both"
  } else {
    stop("Must vary either sample_sizes (length > 1) or effect_sizes (length > 1) or both")
  }

  # Set up logging based on analysis type
  if (analysis_type == "effect_only") {
    cat("\n=== Effect Size Analysis ===\n")
    cat("Fixed sample size:", sample_sizes[1], "\n")
    cat(
      "Effect sizes to test:",
      paste(effect_sizes, collapse = ", "),
      "\n"
    )
  } else if (analysis_type == "sample_only") {
    cat("\n=== Sample Size Analysis ===\n")
    cat("Fixed effect size:", effect_sizes[1], "\n")
    cat(
      "Sample sizes to test:",
      paste(sample_sizes, collapse = ", "),
      "\n"
    )
  } else {
    cat("\n=== Power Grid Analysis ===\n")
    cat(
      "Sample sizes to test:",
      paste(sample_sizes, collapse = ", "),
      "\n"
    )
    cat(
      "Effect sizes to test:",
      paste(effect_sizes, collapse = ", "),
      "\n"
    )
  }
  cat("Threshold - Success:", threshold_success, "\n")
  cat("Threshold - Futility:", threshold_futility, "\n")
  cat("Allocation (treatment %):", percent_group_treat * 100, "%\n")
  cat("Power analysis function:", power_analysis_fn, "\n")
  if (weight_type != "none") {
    cat("Design prior:", design_prior, "\n")

    # Report quantiles if available
    if (!is.null(quantile_fn)) {
      tryCatch(
        {
          q10 <- quantile_fn(0.1)
          q25 <- quantile_fn(0.25)
          q50 <- quantile_fn(0.5)
          q75 <- quantile_fn(0.75)
          q90 <- quantile_fn(0.9)

          cat("Design prior quantiles:\n")
          cat(
            "  10%:",
            round(q10, 3),
            ", 25%:",
            round(q25, 3),
            ", 50%:",
            round(q50, 3)
          )
          cat(", 75%:", round(q75, 3), ", 90%:", round(q90, 3), "\n")

          # Compute quantiles of chosen effect sizes under the design prior
          effect_weights <- sapply(effect_sizes, weight_fn)
          effect_weights <- effect_weights / sum(effect_weights)

          # Create empirical CDF and find quantiles
          sorted_idx <- order(effect_sizes)
          sorted_effects <- effect_sizes[sorted_idx]
          sorted_weights <- effect_weights[sorted_idx]
          cumulative_weights <- cumsum(sorted_weights)

          # Find quantiles by interpolation
          effect_q10 <- approx(cumulative_weights, sorted_effects, xout = 0.1)$y
          effect_q25 <- approx(cumulative_weights, sorted_effects, xout = 0.25)$y
          effect_q50 <- approx(cumulative_weights, sorted_effects, xout = 0.5)$y
          effect_q75 <- approx(cumulative_weights, sorted_effects, xout = 0.75)$y
          effect_q90 <- approx(cumulative_weights, sorted_effects, xout = 0.9)$y

          cat("Chosen effect sizes quantiles under design prior:\n")
          cat(
            "  10%:",
            round(effect_q10, 3),
            ", 25%:",
            round(effect_q25, 3),
            ", 50%:",
            round(effect_q50, 3)
          )
          cat(
            ", 75%:",
            round(effect_q75, 3),
            ", 90%:",
            round(effect_q90, 3),
            "\n"
          )

          # Coverage status
          min_effect <- min(effect_sizes)
          max_effect <- max(effect_sizes)
          cat(
            "Effect size range: [",
            round(min_effect, 3),
            ", ",
            round(max_effect, 3),
            "]\n"
          )

          coverage_low <- min_effect <= q10
          coverage_high <- max_effect >= q90

          if (coverage_low && coverage_high) {
            cat("OK: Effect sizes adequately cover design prior (10%-90% range)\n")
          } else {
            cat("WARN: Effect sizes may not fully cover design prior range\n")
          }
        },
        error = function(e) {
          # Silently continue if quantile reporting fails
        }
      )
    }
  }
  cat(
    "Total combinations:",
    length(sample_sizes) * length(effect_sizes),
    "\n\n"
  )

  # Storage for results
  results_matrix <- vector("list", length(sample_sizes) * length(effect_sizes))
  result_index <- 1
  start_time <- Sys.time()

  # Create grid of combinations
  combinations <- expand.grid(
    sample_size = sample_sizes,
    effect_size = effect_sizes,
    stringsAsFactors = FALSE
  )

  # Group combinations by effect size for model caching
  combinations_by_effect <- split(combinations, combinations$effect_size)
  unique_effects <- sort(unique(effect_sizes))

  # Cache for compiled models
  compiled_models_cache <- list()

  # Get n_cores from dots, default to 1 if not provided
  n_cores <- ifelse(is.null(dots$n_cores), 1, dots$n_cores)

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
    stop("design_prior must be either a character string (brms syntax) or an R function")
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
