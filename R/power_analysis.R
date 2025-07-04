#' Flexible Bayesian Power Analysis for RCTs
#'
#' Conduct Bayesian power analysis for randomized controlled trials using user-specified
#' models and data generation functions. This function uses a design-based approach with
#' separate models for true parameter specification and estimation.
#'
#' @param n_control Number of participants in control group
#' @param n_treatment Number of participants in treatment group
#' @param simulate_data_fn User-defined function that takes (n_control, n_treatment) and returns a data frame
#' @param model_formula_true_params brms formula object for design model with true parameters
#' @param model_formula_estimation brms formula object for estimation model
#' @param family Distributional family for brms (e.g., gaussian(), bernoulli(), poisson())
#' @param priors_true_params Priors with true parameter values (as constants) for design model
#' @param priors_estimation Priors for estimation in power simulation
#' @param target_param Name of the parameter to track for power calculation (without "b_" prefix)
#' @param threshold_success Upper threshold for success determination
#' @param threshold_futility Lower threshold for futility determination (required)
#' @param p_sig_success Probability threshold for declaring success (default 0.975)
#' @param p_sig_futility Probability threshold for declaring futility (default 0.5)
#' @param n_simulations Number of simulation iterations
#' @param brms_args Arguments passed to brms for estimation models. Default includes algorithm="sampling", iter=1200, warmup=200, chains=2, cores=1. User can override any of these or add additional arguments.
#' @param seed Random seed for reproducibility
#' @param n_cores Number of cores for parallel processing. If n_cores > 1, simulations will run in parallel.
#' @param brms_design_true_params Optional pre-fitted brms model with true parameters. If provided, this model will be used instead of fitting a new design model.
#' @param brms_design_estimation Optional pre-fitted brms model template for estimation. If provided, this model will be used instead of fitting a new design model.
#' @param progress_updates Number of progress updates to show during parallel processing. Default is 10. Set to 0 to disable progress updates.
#'
#' @return A list containing power analysis results
#' @export
#' @importFrom parallel makeCluster stopCluster clusterExport clusterEvalQ parLapply detectCores
#'
#' @examples
#' \dontrun{
#' # Define data simulation function
#' simulate_data <- function(n_control, n_treatment) {
#'   data.frame(
#'     outcome = rnorm(n_control + n_treatment),
#'     baseline = rnorm(n_control + n_treatment),
#'     group = factor(rep(c(0, 1), times = c(n_control, n_treatment)),
#'       levels = c(0, 1), labels = c("ctrl", "treat")
#'     )
#'   )
#' }
#'
#' # Define model formulas and priors
#' model_formula_true <- brms::bf(outcome ~ baseline + group, center = FALSE)
#' model_formula_est <- brms::bf(outcome ~ baseline + group)
#' priors_true <- c(
#'   brms::set_prior("constant(.2)", class = "b", coef = "baseline"),
#'   brms::set_prior("constant(.5)", class = "b", coef = "grouptreat"),
#'   brms::set_prior("constant(0)", class = "Intercept"),
#'   brms::set_prior("constant(1)", class = "sigma")
#' )
#' priors_est <- c(
#'   brms::set_prior("student_t(3, 0, 2)", class = "b"),
#'   brms::set_prior("student_t(3, 0, 1)", class = "b", coef = "baseline"),
#'   brms::set_prior("student_t(3, 0, 2)", class = "Intercept"),
#'   brms::set_prior("student_t(3, 0, 1)", class = "sigma")
#' )
#'
#' # Run power analysis with default brms_args
#' power_result <- power_analysis(
#'   n_control = 50, n_treatment = 50,
#'   simulate_data_fn = simulate_data,
#'   model_formula_true_params = model_formula_true,
#'   model_formula_estimation = model_formula_est,
#'   family = gaussian(),
#'   priors_true_params = priors_true,
#'   priors_estimation = priors_est,
#'   target_param = "grouptreat",
#'   threshold_success = 0.2,
#'   threshold_futility = 0,
#'   p_sig_success = 0.95,
#'   p_sig_futility = 0.8,
#'   n_simulations = 100
#' )
#'
#' # Run power analysis with custom brms_args and parallel processing with progress
#' power_result_custom <- power_analysis(
#'   n_control = 50, n_treatment = 50,
#'   simulate_data_fn = simulate_data,
#'   model_formula_true_params = model_formula_true,
#'   model_formula_estimation = model_formula_est,
#'   family = gaussian(),
#'   priors_true_params = priors_true,
#'   priors_estimation = priors_est,
#'   target_param = "grouptreat",
#'   threshold_success = 0.2,
#'   threshold_futility = 0,
#'   p_sig_success = 0.95,
#'   p_sig_futility = 0.7,
#'   n_simulations = 100,
#'   brms_args = list(algorithm = "meanfield", iter = 800, chains = 4),
#'   n_cores = 4, # Use 4 cores for parallel processing
#'   progress_updates = 5 # Show 5 progress updates during execution
#' )
#'
#' # Alternative: Use pre-fitted models (e.g., from validate_power_design)
#' validation <- validate_power_design(
#'   n_control = 50, n_treatment = 50,
#'   simulate_data_fn = simulate_data,
#'   model_formula_true_params = model_formula_true,
#'   model_formula_estimation = model_formula_est,
#'   family = gaussian(),
#'   priors_true_params = priors_true,
#'   priors_estimation = priors_est,
#'   target_param = "grouptreat"
#' )
#'
#' power_result_prefitted <- power_analysis(
#'   n_control = 50, n_treatment = 50,
#'   simulate_data_fn = simulate_data,
#'   target_param = "grouptreat",
#'   threshold_success = 0.2,
#'   threshold_futility = 0,
#'   p_sig_success = 0.95,
#'   p_sig_futility = 0.7,
#'   n_simulations = 1000,
#'   n_cores = 6,
#'   progress_updates = 10, # Show progress every 100 simulations
#'   brms_design_true_params = validation$brms_design_true_params,
#'   brms_design_estimation = validation$brms_design_estimation
#' )
#' }
power_analysis <- function(n_control,
                           n_treatment,
                           simulate_data_fn,
                           model_formula_true_params = NULL,
                           model_formula_estimation = NULL,
                           family = NULL,
                           priors_true_params = NULL,
                           priors_estimation = NULL,
                           target_param,
                           threshold_success,
                           threshold_futility,
                           p_sig_success = 0.975,
                           p_sig_futility = 0.5,
                           n_simulations = 1000,
                           brms_args = list(
                             algorithm = "sampling",
                             iter = 1500,
                             warmup = 500,
                             chains = 2,
                             cores = 1,
                             init = .1,
                             control = list(adapt_delta = 0.9)
                           ),
                           seed = NULL,
                           n_cores = 1,
                           brms_design_true_params = NULL,
                           brms_design_estimation = NULL,
                           progress_updates = 10) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package 'brms' is required for this function.")
  }

  if (!requireNamespace("posterior", quietly = TRUE)) {
    stop("Package 'posterior' is required for this function.")
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for this function.")
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required for this function.")
  }

  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required for this function.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Check if pre-fitted models are provided
  using_pre_fitted_models <- !is.null(brms_design_true_params) &&
    !is.null(brms_design_estimation)

  # Validate inputs
  if (!is.function(simulate_data_fn)) {
    stop(
      "simulate_data_fn must be a function that takes (n_control, n_treatment) as arguments."
    )
  }

  # If using pre-fitted models, validate them and extract model information
  if (using_pre_fitted_models) {
    if (!inherits(brms_design_true_params, "brmsfit")) {
      stop("brms_design_true_params must be a fitted brms model (brmsfit object).")
    }
    if (!inherits(brms_design_estimation, "brmsfit")) {
      stop("brms_design_estimation must be a fitted brms model (brmsfit object).")
    }
    cat("Using pre-fitted design models...\n")

    # Extract model information from the brms objects
    model_formula_true_params <- brms_design_true_params$formula
    model_formula_estimation <- brms_design_estimation$formula
    family <- brms_design_estimation$family
  } else {
    # If not using pre-fitted models, validate that all required model specification parameters are provided
    if (is.null(model_formula_true_params)) {
      stop("model_formula_true_params is required when not using pre-fitted models.")
    }
    if (is.null(model_formula_estimation)) {
      stop("model_formula_estimation is required when not using pre-fitted models.")
    }
    if (is.null(family)) {
      stop("family is required when not using pre-fitted models.")
    }
    if (is.null(priors_true_params)) {
      stop("priors_true_params is required when not using pre-fitted models.")
    }
    if (is.null(priors_estimation)) {
      stop("priors_estimation is required when not using pre-fitted models.")
    }
  }

  # Validate required threshold parameters
  if (missing(threshold_success) || is.null(threshold_success)) {
    stop("threshold_success is required and must be specified.")
  }
  if (missing(threshold_futility) || is.null(threshold_futility)) {
    stop("threshold_futility is required and must be specified.")
  }

  # Validate threshold parameter types
  if (!is.numeric(threshold_success) || length(threshold_success) != 1) {
    stop("threshold_success must be a single numeric value.")
  }
  if (!is.numeric(threshold_futility) || length(threshold_futility) != 1) {
    stop("threshold_futility must be a single numeric value.")
  }

  # Storage for results
  power_results <- vector("list", n_simulations)
  successful_fits <- 0
  # if n_simulations = 1, set n_cores = 1
  n_cores <- ifelse(n_simulations == 1, 1, n_cores)

  # Progress tracking
  if (n_cores > 1) {
    cat(
      "Running",
      n_simulations,
      "power simulations in parallel using",
      n_cores,
      "cores...\n"
    )
  } else {
    cat(
      "Running",
      n_simulations,
      "power simulation(s) sequentially...\n"
    )
  }

  # Set up design models (either use pre-fitted or create new ones)
  if (!using_pre_fitted_models) {
    # Generate mock data to set up design models
    mock_data <- simulate_data_fn(n_control, n_treatment)

    # Fit design model with true parameters (for data generation)
    cat("Setting up design model with true parameters...\n")
    brms_design_true_params <- brms::brm(
      model_formula_true_params,
      data = mock_data,
      family = family,
      prior = priors_true_params,
      sample_prior = "only",
      algorithm = "fixed_param",
      iter = 2,
      warmup = 1,
      chains = 1,
      refresh = 0,
      silent = 1
    )

    # Fit design model with estimation priors (template for power simulation)
    cat("Setting up design model with estimation priors...\n")
    suppressWarnings(
      brms_design_estimation <- brms::brm(
        model_formula_estimation,
        data = mock_data,
        family = family,
        prior = priors_estimation,
        sample_prior = "no",
        algorithm = "sampling",
        iter = 2,
        warmup = 1,
        chains = 1,
        refresh = 0,
        silent = 1
      )
    )
  }


  # Extract true parameter values
  fixef <- brms::fixef(brms_design_true_params) |>
    as.data.frame() |>
    dplyr::select(Estimate) |>
    tibble::rownames_to_column("parameter") |>
    tidyr::pivot_wider(names_from = "parameter", values_from = "Estimate") |>
    unlist()

  ranef <- tryCatch(
    brms::ranef(brms_design_true_params),
    error = function(e) {
      # message("No random effects in the design model.")
      NULL
    }
  )

  true_params <- list(fixef = fixef, ranef = ranef)

  # Define single simulation function for parallel processing
  run_single_simulation <- function(sim_id) {
    tryCatch(
      {
        # Generate new data for this simulation
        data_sim_run <- simulate_data_fn(n_control, n_treatment)

        # Simulate outcome from design model with true parameters
        data_sim_run$outcome <- brms::posterior_predict(
          object = brms_design_true_params,
          newdata = data_sim_run,
          ndraws = 1
        )[1, ]

        # Fit model for this simulation run with estimation priors
        brms_args_default <- list(refresh = 0, silent = 2)

        # Merge user-specified arguments
        brms_args_final <- modifyList(brms_args_default, brms_args)

        # Update design model with new data - wrap in tryCatch for brms errors and warnings
        cat("Fitting estimation model for simulation", sim_id, "...\n")

        # Capture both errors and warnings
        warnings_captured <- NULL
        fit <- tryCatch(
          {
            withCallingHandlers(
              {
                do.call(function(...) {
                  stats::update(brms_design_estimation, newdata = data_sim_run, ...)
                }, brms_args_final)
              },
              warning = function(w) {
                warnings_captured <<- c(warnings_captured, conditionMessage(w))
                invokeRestart("muffleWarning")
              }
            )
          },
          error = function(e) {
            # If brms fitting fails, return a special error object
            structure(list(error = as.character(e), failed_step = "brms_fitting"),
              class = "brms_fit_error"
            )
          }
        )

        # Check if brms fitting failed
        if (inherits(fit, "brms_fit_error")) {
          return(list(
            simulation = sim_id,
            converged = FALSE,
            error_message = paste("brms fitting failed:", fit$error),
            error_type = "brms_fitting_error"
          ))
        }

        # Check if any warnings were captured during fitting
        if (!is.null(warnings_captured) && length(warnings_captured) > 0) {
          warning_summary <- paste(warnings_captured, collapse = "; ")
          return(list(
            simulation = sim_id,
            converged = FALSE,
            error_message = paste("brms fitting produced warnings:", warning_summary),
            error_type = "brms_fitting_warning"
          ))
        }

        # Extract target parameter samples - wrap in tryCatch for extraction errors
        param_name <- paste("b", target_param, sep = "_")
        effect_samples <- tryCatch(
          {
            posterior::as_draws_df(fit)[[param_name]]
          },
          error = function(e) {
            return(NULL)
          }
        )

        if (is.null(effect_samples)) {
          return(list(
            simulation = sim_id,
            converged = FALSE,
            error_message = paste(
              "Parameter",
              param_name,
              "not found in model. Available parameters:",
              paste(names(posterior::as_draws_df(fit)), collapse = ", ")
            ),
            error_type = "parameter_extraction_error"
          ))
        }

        # Note: We rely on brms warnings to catch convergence issues
        # rather than post-hoc R-hat checking, since warnings are more comprehensive

        # Calculate power metrics
        prob_above_success <- mean(effect_samples > threshold_success)
        prob_below_futility <- mean(effect_samples < threshold_futility)

        # Return results
        result <- list(
          simulation = sim_id,
          treatment_effect_mean = mean(effect_samples),
          treatment_effect_median = median(effect_samples),
          treatment_effect_sd = sd(effect_samples),
          prob_above_success = prob_above_success,
          prob_below_futility = prob_below_futility,
          success_decision = prob_above_success > p_sig_success,
          futility_decision = prob_below_futility > p_sig_futility,
          converged = TRUE
        )

        return(result)
      },
      error = function(e) {
        list(
          simulation = sim_id,
          converged = FALSE,
          error_message = as.character(e)
        )
      }
    )
  }

  # Run simulations (parallel or sequential)
  # Note: parallel is now an import dependency, so it should always be available

  if (n_cores > 1) {
    # Parallel execution with progress tracking
    cl <- parallel::makeCluster(n_cores)

    tryCatch({
      # Export necessary objects to cluster
      parallel::clusterExport(
        cl,
        c(
          "simulate_data_fn",
          "brms_design_true_params",
          "brms_design_estimation",
          "brms_args",
          "target_param",
          "threshold_success",
          "threshold_futility",
          "p_sig_success",
          "p_sig_futility",
          "n_control",
          "n_treatment"
        ),
        envir = environment()
      )

      # Load required libraries on cluster nodes
      parallel::clusterEvalQ(cl, {
        library(brms)
        library(posterior)
      })

      # Progress tracking with batched execution
      if (progress_updates > 0 &&
        n_simulations > progress_updates) {
        cat(
          "Running",
          n_simulations,
          "simulations in parallel using",
          n_cores,
          "cores...\n"
        )

        # Calculate batch size for progress updates
        batch_size <- max(1, floor(n_simulations / progress_updates))
        batches <- split(1:n_simulations, ceiling(seq_along(1:n_simulations) / batch_size))

        power_results <- vector("list", n_simulations)
        completed_sims <- 0
        start_time <- Sys.time()

        for (i in seq_along(batches)) {
          batch_indices <- batches[[i]]

          # Run batch in parallel
          batch_results <- parallel::parLapply(cl, batch_indices, run_single_simulation)

          # Store results
          for (j in seq_along(batch_indices)) {
            power_results[[batch_indices[j]]] <- batch_results[[j]]
          }

          completed_sims <- completed_sims + length(batch_indices)
          elapsed <- difftime(Sys.time(), start_time, units = "mins")

          # Show progress
          if (completed_sims < n_simulations) {
            estimated_total <- elapsed * n_simulations / completed_sims
            eta <- estimated_total - elapsed
            cat(
              sprintf(
                "Progress: %d/%d (%.1f%%) - Elapsed: %.1f min - ETC: %.1f min\n",
                completed_sims,
                n_simulations,
                100 * completed_sims / n_simulations,
                as.numeric(elapsed),
                as.numeric(eta)
              )
            )
          } else {
            cat(
              sprintf(
                "Completed: %d/%d (100%%) - Total time: %.1f min\n",
                completed_sims,
                n_simulations,
                as.numeric(elapsed)
              )
            )
          }
        }
      } else {
        # Run all simulations at once without progress tracking
        if (progress_updates == 0) {
          cat(
            "Running",
            n_simulations,
            "simulations in parallel using",
            n_cores,
            "cores (no progress updates)...\n"
          )
        }
        power_results <- parallel::parLapply(cl, 1:n_simulations, run_single_simulation)
      }
    }, finally = {
      parallel::stopCluster(cl)
    })

    # Count successful fits
    successful_fits <- sum(sapply(power_results, function(x) {
      x$converged
    }))
  } else {
    # Sequential execution
    power_results <- vector("list", n_simulations)
    successful_fits <- 0

    for (i in 1:n_simulations) {
      if (i %% 100 == 0) {
        cat("Simulation", i, "of", n_simulations, "\n")
      }

      power_results[[i]] <- run_single_simulation(i)

      if (power_results[[i]]$converged) {
        successful_fits <- successful_fits + 1
      } else {
        error_type <- if (!is.null(power_results[[i]]$error_type)) {
          paste0(" (", power_results[[i]]$error_type, ")")
        } else {
          ""
        }
        cat("Failed simulation", i, error_type, ":", power_results[[i]]$error_message, "\n")
      }
    }
  }

  # Process results
  successful_results <- power_results[sapply(power_results, function(x) {
    !is.null(x) && x$converged
  })]

  if (length(successful_results) == 0) {
    # Provide detailed error summary
    failed_results <- power_results[sapply(power_results, function(x) !is.null(x) && !x$converged)]
    error_summary <- table(sapply(failed_results, function(x) {
      if (is.null(x$error_type)) "unknown_error" else x$error_type
    }))
    cat("\nAll simulations failed. Summary by type:\n")
    for (i in seq_along(error_summary)) {
      error_type <- names(error_summary)[i]
      count <- error_summary[i]
      description <- switch(error_type,
        "brms_fitting_error" = "brms model fitting failures",
        "brms_fitting_warning" = "brms fitting warnings (convergence/other issues)",
        "parameter_extraction_error" = "parameter extraction failures",
        "convergence_warning" = "poor convergence (R-hat > 1.1)",
        "unknown_error" = "unknown errors",
        error_type # fallback to the error type name
      )
      cat("  ", description, ":", count, "\n")
    }
    stop("No simulations converged successfully. Check your model specification, data generation, and consider adjusting brms_args for better convergence.")
  }

  # Calculate summary statistics
  power_summary <- list(
    n_simulations = n_simulations,
    successful_fits = successful_fits,
    convergence_rate = successful_fits / n_simulations,

    # Power estimates
    power_success = mean(sapply(successful_results, function(x) {
      x$success_decision
    })),
    power_futility = mean(sapply(successful_results, function(x) {
      x$futility_decision
    })),
    mean_prob_success = mean(sapply(successful_results, function(x) {
      x$prob_above_success
    })),
    mean_prob_futility = mean(
      sapply(successful_results, function(x) {
        x$prob_below_futility
      })
    ),

    # Effect size estimates
    mean_effect_estimate = mean(
      sapply(successful_results, function(x) {
        x$treatment_effect_mean
      })
    ),
    median_effect_estimate = median(
      sapply(successful_results, function(x) {
        x$treatment_effect_median
      })
    ),
    sd_mean_effect_estimate = sd(
      sapply(successful_results, function(x) {
        x$treatment_effect_mean
      })
    ),
    sd_median_effect_estimate = sd(
      sapply(successful_results, function(x) {
        x$treatment_effect_median
      })
    ),

    # Study parameters
    study_parameters = list(
      n_control = n_control,
      n_treatment = n_treatment,
      target_param = target_param,
      threshold_success = threshold_success,
      threshold_futility = threshold_futility,
      p_sig_success = p_sig_success,
      p_sig_futility = p_sig_futility
    ),

    # Store design information
    true_parameters = true_params,
    model_formula_true_params = model_formula_true_params,
    model_formula_estimation = model_formula_estimation,
    family = family,

    # Raw results
    simulation_results = successful_results
  )

  class(power_summary) <- "rctbayespower"

  cat("\nPower Analysis Complete!\n")
  cat(
    "Successful fits:",
    successful_fits,
    "out of",
    n_simulations,
    "\n"
  )

  # Report error summary if there were failures
  if (successful_fits < n_simulations) {
    failed_results <- power_results[sapply(power_results, function(x) !is.null(x) && !x$converged)]
    if (length(failed_results) > 0) {
      error_summary <- table(sapply(failed_results, function(x) {
        if (is.null(x$error_type)) "unknown_error" else x$error_type
      }))
      cat("Failed simulations by type:\n")
      for (i in seq_along(error_summary)) {
        error_type <- names(error_summary)[i]
        count <- error_summary[i]
        description <- switch(error_type,
          "brms_fitting_error" = "brms model fitting failures",
          "brms_fitting_warning" = "brms fitting warnings (convergence/other issues)",
          "parameter_extraction_error" = "parameter extraction failures",
          "convergence_warning" = "poor convergence (R-hat > 1.1)",
          "unknown_error" = "unknown errors",
          error_type # fallback to the error type name
        )
        cat("  ", description, ":", count, "\n")
      }
    }
  }
  cat(
    "Mean effect estimate (median):",
    round(power_summary$median_effect_estimate, 3),
    "\n"
  )
  cat(
    "SD of effect estimate (median):",
    round(power_summary$sd_median_effect_estimate, 3),
    "\n"
  )
  cat(
    "Power - Success:",
    round(power_summary$power_success, 3),
    "\n"
  )
  cat(
    "Power - Futility:",
    round(power_summary$power_futility, 3),
    "\n"
  )
  cat(
    "Mean probability of success:",
    round(power_summary$mean_prob_success, 3),
    "\n"
  )
  cat(
    "Mean probability of futility:",
    round(power_summary$mean_prob_futility, 3),
    "\n"
  )
  cat("\n")

  return(power_summary)
}

#' Summary Method for Power Analysis Results
#'
#' Provides a structured summary of Bayesian power analysis results.
#'
#' @param object An object of class "rctbayespower" returned by power_analysis()
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing summary).
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a power analysis result
#' result <- power_analysis(...)
#' summary(result)
#' }
summary.rctbayespower <- function(object, ...) {
  cat("\n=== Bayesian Power Analysis Summary ===\n\n")

  # Study design parameters
  cat("Study Design:\n")
  cat("  Control group size:", object$study_parameters$n_control, "\n")
  cat("  Treatment group size:", object$study_parameters$n_treatment, "\n")
  cat("  Target parameter:", object$study_parameters$target_param, "\n")
  cat("  Success threshold:", object$study_parameters$threshold_success, "\n")
  cat("  Futility threshold:", object$study_parameters$threshold_futility, "\n")
  cat("  Success probability threshold:", object$study_parameters$p_sig_success, "\n\n")
  cat("  Futility probability threshold:", object$study_parameters$p_sig_futility, "\n\n")

  # Model information
  cat("Model Information:\n")

  # Extract family name properly
  family_name <- if (inherits(object$family, "brmsfamily")) {
    paste0(object$family$family, "(", object$family$link, ")")
  } else if (inherits(object$family, "family")) {
    paste0(object$family$family, "(", object$family$link, ")")
  } else {
    "Unknown"
  }
  cat("  Family:", family_name, "\n")

  if (!is.null(object$model_formula_estimation)) {
    formula_str <- if (inherits(object$model_formula_estimation, "brmsformula")) {
      as.character(object$model_formula_estimation$formula)[3]
    } else {
      deparse(object$model_formula_estimation)
    }
    cat("  Estimation formula:", formula_str, "\n")
  }
  if (!is.null(object$model_formula_true_params)) {
    formula_str <- if (inherits(object$model_formula_true_params, "brmsformula")) {
      as.character(object$model_formula_true_params$formula)[3]
    } else {
      deparse(object$model_formula_true_params)
    }
    cat("  Design formula:", formula_str, "\n")
  }
  cat("\n")

  # Simulation overview
  cat("Simulation Overview:\n")
  cat("  Total simulations:", object$n_simulations, "\n")
  cat("  Successful fits:", object$successful_fits, "\n")
  cat("  Convergence rate:", round(object$convergence_rate * 100, 1), "%\n\n")

  # Report failures if any
  if (object$successful_fits < object$n_simulations) {
    failed_count <- object$n_simulations - object$successful_fits
    cat("Failed Simulations:", failed_count, "\n")

    # Try to get failure summary from raw results if available
    if (!is.null(object$simulation_results)) {
      # Create a mock failed_results for summary (since we only store successful ones)
      cat("  (See detailed error output from power_analysis() for failure breakdown)\n")
    }
    cat("\n")
  }

  # Effect size estimates
  cat("Treatment Effect Estimates:\n")
  cat("  Mean effect estimate (median):", round(object$median_effect_estimate, 3), "\n")
  cat("  SD of effect estimates (median):", round(object$sd_median_effect_estimate, 3), "\n\n")

  # Power estimates
  cat("Power Analysis Results:\n")
  cat("  Power - Success:", round(object$power_success, 3), "\n")
  cat("  Power - Futility:", round(object$power_futility, 3), "\n\n")

  # Probability thresholds
  cat("Decision Probabilities:\n")
  cat("  Mean probability of success:", round(object$mean_prob_success, 3), "\n")
  cat("  Mean probability of futility:", round(object$mean_prob_futility, 3), "\n\n")

  # Footer
  cat("=== End Summary ===\n")

  invisible(object)
}

#' Validate Power Analysis Design
#'
#' Helper function to validate the design components before running a full power analysis.
#' Performs a single simulation run and checks for convergence and parameter extraction.
#'
#' @param n_control Number of participants in control group
#' @param n_treatment Number of participants in treatment group
#' @param simulate_data_fn User-defined function that takes (n_control, n_treatment) and returns a data frame
#' @param model_formula_true_params brms formula object for design model with true parameters
#' @param model_formula_estimation brms formula object for estimation model
#' @param family Distributional family for brms
#' @param priors_true_params Priors with true parameter values (as constants) for design model
#' @param priors_estimation Priors for estimation in power simulation
#' @param target_param Name of the parameter to track for power calculation (without "b_" prefix)
#' @param brms_args Arguments passed to brms for validation runs. Default includes algorithm="sampling", iter=700, warmup=200, chains=2, cores=1. User can override any of these or add additional arguments.
#'
#' @return A list containing validation results, model summaries, and compiled design models (brms_design_true_params and brms_design_estimation) that can be reused in power_analysis()
#' @export
#'
#' @examples
#' \dontrun{
#' # Validate design before running full power analysis
#' validation <- validate_power_design(
#'   n_control = 50, n_treatment = 50,
#'   simulate_data_fn = simulate_data,
#'   model_formula_true_params = model_formula_true,
#'   model_formula_estimation = model_formula_est,
#'   family = gaussian(),
#'   priors_true_params = priors_true,
#'   priors_estimation = priors_est,
#'   target_param = "grouptreat"
#' )
#'
#' # Use the compiled models from validation in power analysis
#' power_result <- power_analysis(
#'   n_control = 50, n_treatment = 50,
#'   simulate_data_fn = simulate_data,
#'   target_param = "grouptreat",
#'   threshold_success = 0.2,
#'   threshold_futility = 0,
#'   p_sig_success = 0.95,
#'   p_sig_futility = 0.5,
#'   n_simulations = 100,
#'   brms_design_true_params = validation$brms_design_true_params,
#'   brms_design_estimation = validation$brms_design_estimation
#' )
#' }
validate_power_design <- function(n_control,
                                  n_treatment,
                                  simulate_data_fn,
                                  model_formula_true_params,
                                  model_formula_estimation,
                                  family,
                                  priors_true_params,
                                  priors_estimation,
                                  target_param,
                                  brms_args = list(
                                    algorithm = "sampling",
                                    iter = 1500,
                                    warmup = 500,
                                    chains = 2,
                                    cores = 1,
                                    init = .1,
                                    control = list(adapt_delta = 0.9)
                                  )) {
  cat("Validating power analysis design...\n")

  # Check that all necessary arguments were specified
  cat("Checking required arguments...\n")

  if (missing(n_control) || is.null(n_control)) {
    stop("n_control is required and must be specified.")
  }
  if (missing(n_treatment) || is.null(n_treatment)) {
    stop("n_treatment is required and must be specified.")
  }
  if (missing(simulate_data_fn) || is.null(simulate_data_fn)) {
    stop("simulate_data_fn is required and must be specified.")
  }
  if (missing(model_formula_true_params) ||
    is.null(model_formula_true_params)) {
    stop("model_formula_true_params is required and must be specified.")
  }
  if (missing(model_formula_estimation) ||
    is.null(model_formula_estimation)) {
    stop("model_formula_estimation is required and must be specified.")
  }
  if (missing(family) || is.null(family)) {
    stop("family is required and must be specified.")
  }
  if (missing(priors_true_params) || is.null(priors_true_params)) {
    stop("priors_true_params is required and must be specified.")
  }
  if (missing(priors_estimation) || is.null(priors_estimation)) {
    stop("priors_estimation is required and must be specified.")
  }
  if (missing(target_param) || is.null(target_param)) {
    stop("target_param is required and must be specified.")
  }

  cat("✓ All required arguments provided\n")

  # Validate input types
  if (!is.function(simulate_data_fn)) {
    stop(
      "simulate_data_fn must be a function that takes (n_control, n_treatment) as arguments."
    )
  }

  if (!is.numeric(n_control) || n_control <= 0) {
    stop("n_control must be a positive number.")
  }

  if (!is.numeric(n_treatment) || n_treatment <= 0) {
    stop("n_treatment must be a positive number.")
  }

  if (!inherits(model_formula_true_params, "brmsformula") &&
    !inherits(model_formula_true_params, "formula")) {
    stop("model_formula_true_params must be a brms formula object or standard R formula.")
  }

  if (!inherits(model_formula_estimation, "brmsformula") &&
    !inherits(model_formula_estimation, "formula")) {
    stop("model_formula_estimation must be a brms formula object or standard R formula.")
  }

  if (!is.character(target_param) || length(target_param) != 1) {
    stop("target_param must be a single character string.")
  }

  # Check algorithm if provided in brms_args
  if (!is.null(brms_args$algorithm) &&
    !brms_args$algorithm %in% c("sampling", "meanfield", "fullrank", "pathfinder", "laplace")) {
    stop(
      "brms_args$algorithm must be one of: 'sampling', 'meanfield', 'fullrank', 'pathfinder', 'laplace'"
    )
  }

  cat("✓ All argument types valid\n")

  # Test data simulation function
  cat("Testing data simulation function...\n")
  tryCatch(
    {
      mock_data <- simulate_data_fn(n_control, n_treatment)
      cat("✓ Data simulation function works correctly\n")
      cat("  Generated data dimensions:", dim(mock_data), "\n")
      cat("  Column names:", paste(names(mock_data), collapse = ", "), "\n")
    },
    error = function(e) {
      stop("Data simulation function failed: ", as.character(e))
    }
  )

  # Test design model with true parameters
  cat("Testing design model with true parameters...\n")
  cat("  Using algorithm: fixed_param\n")
  cat("  Running brms::brm() with sample_prior = 'only'...\n")
  tryCatch(
    {
      brms_design_true <- brms::brm(
        model_formula_true_params,
        data = mock_data,
        family = family,
        prior = priors_true_params,
        sample_prior = "only",
        algorithm = "fixed_param",
        iter = 2,
        warmup = 1,
        chains = 1,
        refresh = 0,
        silent = 1
      )
      cat("✓ Design model with true parameters fitted successfully\n")

      # Show true parameter values
      cat("  True parameter values:\n \n")
      print(summary(brms_design_true))
    },
    error = function(e) {
      stop("Design model with true parameters failed: ", as.character(e))
    }
  )

  # Test design model with estimation priors
  cat("Testing design model with estimation priors...\n")
  cat("  Using algorithm: sampling\n")
  cat("  Running brms::brm() with sample_prior = 'only'...\n")

  tryCatch(
    {
      suppressWarnings(
        brms_design_est <- brms::brm(
          model_formula_estimation,
          data = mock_data,
          family = family,
          prior = priors_estimation,
          sample_prior = "no",
          algorithm = "sampling",
          iter = 2,
          warmup = 1,
          chains = 1,
          refresh = 0,
          silent = 1
        )
      )
      cat("✓ Design model with estimation priors fitted successfully\n")
    },
    error = function(e) {
      stop(
        "Design model with estimation priors failed: ",
        as.character(e)
      )
    }
  )

  # Test single simulation run
  cat("\n Testing single simulation run...\n")
  tryCatch(
    {
      # Generate new data
      cat("  Generating new simulation data...\n")
      data_sim <- simulate_data_fn(n_control, n_treatment)
      cat("  ✓ New data generated with dimensions:", dim(data_sim), "\n")

      # Simulate outcome from true model
      cat("  Simulating outcome from design model with true parameters...\n")
      data_sim$outcome <- brms::posterior_predict(
        object = brms_design_true,
        newdata = data_sim,
        ndraws = 1
      )[1, ]
      cat("  ✓ Outcome simulated\n")

      # Fit estimation model
      brms_args_default <- list(refresh = 100, silent = 1)
      brms_args_final <- modifyList(brms_args_default, brms_args)

      cat("  Fitting estimation model with brms arguments:\n")
      cat("    Algorithm:", brms_args_final$algorithm, "\n")
      cat(
        "    Iterations:",
        brms_args_final$iter,
        "(warmup:",
        brms_args_final$warmup,
        ")\n"
      )
      cat(
        "    Chains:",
        brms_args_final$chains,
        ", Cores:",
        brms_args_final$cores,
        "\n"
      )
      if (length(brms_args) > 0) {
        user_args <- setdiff(names(brms_args), names(brms_args_default))
        if (length(user_args) > 0) {
          cat(
            "    Additional user args:",
            paste(user_args, "=", brms_args[user_args], collapse = ", "),
            "\n"
          )
        }
      }

      fit <- do.call(function(...) {
        stats::update(brms_design_est, newdata = data_sim, ...)
      }, brms_args_final)

      print(fit)

      cat("\n✓ Single simulation run completed successfully\n")

      # Check parameter extraction
      param_name <- paste("b", target_param, sep = "_")
      effect_samples <- posterior::as_draws_df(fit)[[param_name]]

      if (is.null(effect_samples)) {
        available_params <- names(posterior::as_draws_df(fit))
        stop(
          paste(
            "Target parameter",
            param_name,
            "not found. Available parameters:",
            paste(available_params, collapse = ", ")
          )
        )
      }

      cat(
        "✓ Target parameter",
        param_name,
        "extracted successfully\n"
      )
      cat("  Parameter estimate:", round(mean(effect_samples), 4), "\n")
      cat("  Parameter SD:", round(sd(effect_samples), 4), "\n")

      # Check convergence
      rhat <- brms::rhat(fit)[param_name]
      ess_bulk <- brms::neff_ratio(fit)[param_name]

      cat("  Convergence diagnostics for target parameter:\n")
      cat("    R-hat:", round(rhat, 3), ifelse(rhat < 1.1, "✓", "⚠"), "\n")
      cat(
        "    ESS ratio:",
        round(ess_bulk, 3),
        ifelse(ess_bulk > 0.1, "✓", "⚠"),
        "\n"
      )

      validation_result <- list(
        validation_passed = TRUE,
        mock_data = mock_data,
        true_parameters = brms::fixef(brms_design_true),
        simulation_data = data_sim,
        fit_summary = summary(fit),
        convergence = list(rhat = rhat, ess_ratio = ess_bulk),
        brms_design_true_params = brms_design_true,
        brms_design_estimation = brms_design_est
      )
    },
    error = function(e) {
      stop("Single simulation run failed: ", as.character(e))
    }
  )

  cat("\n✓ All validation checks passed! Design is ready for power analysis.\n")

  return(validation_result)
}

#' Print method for rctbayespower objects
#'
#' @param x An object of class "rctbayespower" returned by power_analysis()
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @export
print.rctbayespower <- function(x, ...) {
  cat("Bayesian RCT Power Analysis Results\n")
  cat("===================================\n\n")

  cat("Study Parameters:\n")
  cat("  Sample size (control):", x$study_parameters$n_control, "\n")
  cat("  Sample size (treatment):", x$study_parameters$n_treatment, "\n")
  cat("  Target parameter:", x$study_parameters$target_param, "\n")
  cat("  Success threshold:", x$study_parameters$threshold_success, "\n")
  cat("  Futility threshold:", x$study_parameters$threshold_futility, "\n")
  cat("  Success probability threshold:", x$study_parameters$p_sig_success, "\n")

  if (!is.null(x$study_parameters$p_sig_futility)) {
    cat("  Futility probability threshold:", x$study_parameters$p_sig_futility, "\n")
  }
  cat("\n")

  cat("Simulation Results:\n")
  cat("  Total simulations:", x$n_simulations, "\n")
  cat("  Successful fits:", x$successful_fits, "\n")
  cat("  Convergence rate:", round(x$convergence_rate, 3), "\n\n")

  cat("Effect Size Estimates:\n")
  cat("  Mean effect estimate (median):", round(x$median_effect_estimate, 3), "\n")
  cat("  SD of effect estimate (median):", round(x$sd_median_effect_estimate, 3), "\n\n")

  cat("Power Results:\n")
  cat("  Power - Success:", round(x$power_success, 3), "\n")
  cat("  Power - Futility:", round(x$power_futility, 3), "\n")
  cat("  Mean probability of success:", round(x$mean_prob_success, 3), "\n")
  cat("  Mean probability of futility:", round(x$mean_prob_futility, 3), "\n")

  invisible(x)
}
