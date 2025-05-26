#' Null-coalescing operator
#' 
#' @param x First value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Generate Longitudinal RCT Data for Time-by-Group Model
#' 
#' @param conditions Named list with data generation parameters
#' @return data.frame with longitudinal RCT data
#' @keywords internal
#' @importFrom stats rnorm expand.grid
generate_rct_data <- function(conditions) {
  
  # Extract required parameters with validation
  n_subjects <- conditions[["n_subjects"]]
  n_timepoints <- conditions[["n_timepoints"]]
  time_effects <- conditions[["time_effects"]]
  group_time_effects <- conditions[["group_time_effects"]]
  
  if (is.null(n_subjects) || !is.numeric(n_subjects) || n_subjects < 1) {
    stop("'n_subjects' must be a positive integer", call. = FALSE)
  }
  
  if (is.null(n_timepoints) || !is.numeric(n_timepoints) || n_timepoints < 1) {
    stop("'n_timepoints' must be a positive integer", call. = FALSE)
  }
  
  # Extract optional parameters with defaults
  sigma <- conditions[["sigma"]] %||% 1
  baseline_mean <- conditions[["baseline_mean"]] %||% 0
  allocation_ratio <- conditions[["allocation_ratio"]] %||% 0.5
  
  # Additional validation
  if (sigma <= 0) {
    stop("'sigma' must be positive", call. = FALSE)
  }
  
  if (allocation_ratio <= 0 || allocation_ratio >= 1) {
    stop("'allocation_ratio' must be between 0 and 1", call. = FALSE)
  }
    # Validate inputs - baseline (t_0) is included in n_timepoints
  if (length(time_effects) != n_timepoints) {
    stop("'time_effects' length must equal 'n_timepoints' (including baseline)", call. = FALSE)
  }
  if (length(group_time_effects) != n_timepoints) {
    stop("'group_time_effects' length must equal 'n_timepoints' (including baseline)", call. = FALSE)
  }
  
  # Validate that effect vectors are numeric
  if (!is.numeric(time_effects)) {
    stop("'time_effects' must be numeric", call. = FALSE)
  }
  if (!is.numeric(group_time_effects)) {
    stop("'group_time_effects' must be numeric", call. = FALSE)
  }
  
  # Warn if baseline effects are non-zero
  if (time_effects[1] != 0) {
    warning("time_effects[1] (baseline t_0) should typically be 0", call. = FALSE)
  }
  if (group_time_effects[1] != 0) {
    warning("group_time_effects[1] (baseline t_0) should typically be 0", call. = FALSE)
  }
  
  # Generate subject-level data
  n_treatment <- round(n_subjects * allocation_ratio)
  n_control <- n_subjects - n_treatment
  
  subjects <- data.frame(
    subject_id = seq_len(n_subjects),
    group = c(rep(0L, n_control), rep(1L, n_treatment))
  )
  
  # Create longitudinal structure
  data_long <- expand.grid(
    subject_id = seq_len(n_subjects),
    timepoint = 0:(n_timepoints - 1L),
    stringsAsFactors = FALSE
  )
  
  # Merge with subject-level data
  data_long <- merge(data_long, subjects, by = "subject_id")
  
  # Generate time variables (t_0, t_1, ..., t_T)
  for (t in 0:(n_timepoints - 1)) {
    data_long[[paste0("t_", t)]] <- as.integer(data_long[["timepoint"]] == t)
  }
  
  # Calculate linear predictor
  linear_pred <- rep(baseline_mean, nrow(data_long))
  
  # Add time effects: t_0 + t_1 + ... + t_T
  for (t in 0:(n_timepoints - 1)) {
    linear_pred <- linear_pred + time_effects[t + 1] * data_long[[paste0("t_", t)]]
  }
  
  # Add group-by-time interactions: t_0*group + t_1*group + ... + t_T*group
  for (t in 0:(n_timepoints - 1)) {
    linear_pred <- linear_pred + 
      group_time_effects[t + 1] * data_long[[paste0("t_", t)]] * data_long[["group"]]
  }
  
  # Generate outcome
  data_long[["y"]] <- stats::rnorm(nrow(data_long), linear_pred, sigma)
  
  # Sort by subject and time
  data_long[order(data_long[["subject_id"]], data_long[["timepoint"]]), ]
}

#' Single Simulation for Bayesian RCT Power Analysis
#' 
#' @param sim_id Simulation identifier (integer)
#' @param conditions Named list with data generation parameters
#' @param t_endpoint Integer timepoint number for the target interaction effect (1 to n_timepoints-1)
#' @param threshold Numeric threshold value for probability of success
#' @param prior_spec Optional brms prior specification
#' @param success_threshold Numeric posterior probability threshold for declaring success
#' @param ... Additional arguments passed to brm()
#' @return Numeric probability of success for this simulation
#' @keywords internal
#' @importFrom brms brm bf prior student_t as_draws_df
#' @importFrom stats as.formula
single_sim_brms <- function(sim_id, conditions, t_endpoint, threshold, 
                            prior_spec = NULL, success_threshold = 0.8, ...) {
  tryCatch({
    # Generate data based on conditions and model structure
    data_sim <- generate_rct_data(conditions)
    
    # Build model formula dynamically based on n_timepoints
    n_timepoints <- conditions[["n_timepoints"]]
    
    # Create time terms: t_0 + t_1 + ... + t_(T-1)
    time_terms <- paste0("t_", 0:(n_timepoints - 1), collapse = " + ")
    
    # Create interaction terms: t_1:group + ... + t_(T-1):group (excluding t_0)
    if (n_timepoints > 1) {
      interaction_terms <- paste0("t_", 1:(n_timepoints - 1), ":group", collapse = " + ")
      formula_str <- paste("y ~", time_terms, "+", interaction_terms, "+ (1|subject_id)")
    } else {
      formula_str <- paste("y ~", time_terms, "+ (1|subject_id)")    }
    dynamic_formula <- brms::bf(stats::as.formula(formula_str))
    
    # Fit Bayesian model
    if (is.null(prior_spec)) {
      # Set default priors: student_t(3, 0, 2) for all parameters
      # Note: Use student_t() without brms:: prefix to avoid Stan compilation errors
      default_priors <- brms::prior(student_t(3, 0, 2), class = "b") +
        brms::prior(student_t(3, 0, 2), class = "Intercept") +
        brms::prior(student_t(3, 0, 2), class = "sd")
      fit <- brms::brm(dynamic_formula, data = data_sim, prior = default_priors, 
                       refresh = 0, silent = 2, ...)
    } else {
      fit <- brms::brm(dynamic_formula, data = data_sim, prior = prior_spec, 
                       refresh = 0, silent = 2, ...)
    }
    
    # Extract posterior samples for target parameter
    posterior_samples <- brms::as_draws_df(fit)
      # Construct parameter name from t_endpoint
    if (t_endpoint < 1 || t_endpoint >= n_timepoints) {
      stop("t_endpoint must be between 1 and ", n_timepoints - 1, " (post-baseline timepoints)", call. = FALSE)
    }
    
    param_name <- paste0("b_t_", t_endpoint, ":group")
    
    # Calculate probability of success (P(param > threshold))
    if (param_name %in% names(posterior_samples)) {
      prob_success <- mean(posterior_samples[[param_name]] > threshold)
    } else {
      # Try alternative parameter naming convention
      alt_param_name <- paste0("b_t_", t_endpoint, ".group")
      if (alt_param_name %in% names(posterior_samples)) {
        prob_success <- mean(posterior_samples[[alt_param_name]] > threshold)
      } else {
        warning("Parameter '", param_name, "' (or '", alt_param_name, "') not found in posterior samples for simulation ", 
                sim_id, ". Available parameters: ", paste(names(posterior_samples), collapse = ", "), call. = FALSE)
        return(NA_real_)
      }
    }
    
    return(prob_success)
    
  }, error = function(e) {
    warning("Simulation ", sim_id, " failed: ", e$message, call. = FALSE)
    return(NA_real_)
  })
}

#' Bayesian Power Analysis for RCT Models
#' 
#' Performs simulation-based power analysis for longitudinal RCT models using 
#' Bayesian mixed-effects modeling with brms.
#' 
#' @param conditions Named list of parameter combinations to test. Must include:
#'   \describe{
#'     \item{n_subjects}{Total number of subjects}
#'     \item{n_timepoints}{Number of time measurements (including baseline)}
#'     \item{time_effects}{Numeric vector of main time effects (length = n_timepoints, first element = baseline, typically 0)}
#'     \item{group_time_effects}{Numeric vector of group×time interaction effects (length = n_timepoints, first element = baseline, typically 0)}
#'     \item{allocation_ratio}{Proportion assigned to treatment group (default 0.5)}
#'     \item{sigma}{Residual standard deviation (default 1)}
#'     \item{baseline_mean}{Baseline outcome mean (default 0)}
#'   }
#' @param t_endpoint Integer timepoint number for the target interaction effect (1 to n_timepoints-1)
#' @param threshold Numeric threshold value for probability of success
#' @param n_sims Integer number of simulation repetitions (default 200)
#' @param n_cores Integer number of cores for parallel processing (default: detectCores() - 1)
#' @param prior_spec Optional brms prior specification. If NULL, uses student_t(3, 0, 2) priors
#' @param success_threshold Numeric posterior probability threshold for declaring success (default 0.8)
#' @param ... Additional arguments passed to \code{\link[brms]{brm}}
#' 
#' @return List containing:
#'   \describe{
#'     \item{power}{Estimated statistical power}
#'     \item{n_successful_sims}{Number of successful simulations}
#'     \item{mean_posterior_prob}{Mean posterior probability across simulations}
#'     \item{se_power}{Standard error of power estimate}
#'   }
#' 
#' @details The function generates longitudinal RCT data and fits Bayesian mixed-effects
#' models using the formula: y ~ t_0 + t_1 + ... + t_(T-1) + t_1:group + ... + t_(T-1):group + (1|subject_id)
#' Note that t_0 (baseline) has no group interaction. The t_endpoint parameter specifies which 
#' post-baseline timepoint's interaction effect to test (e.g., t_endpoint = 3 tests b_t_3:group).
#' 
#' @examples
#' \donttest{
#' # Basic example with 4 timepoints
#' conditions <- list(
#'   n_subjects = 100,
#'   n_timepoints = 4,
#'   time_effects = c(0, 0.2, 0.4, 0.6),
#'   group_time_effects = c(0, 0.1, 0.3, 0.5),
#'   allocation_ratio = 0.5,
#'   sigma = 1
#' )
#' 
#' result <- power_analysis_brms(
#'   conditions = conditions,
#'   t_endpoint = 3,
#'   threshold = 0.2,
#'   n_sims = 10
#' )
#' 
#' # Example with custom priors
#' custom_priors <- brms::prior(normal(0, 1), class = "b") +
#'                  brms::prior(normal(0, 1), class = "Intercept")
#' 
#' result_custom <- power_analysis_brms(
#'   conditions = conditions,
#'   t_endpoint = 3,
#'   threshold = 0.2,
#'   n_sims = 10,
#'   prior_spec = custom_priors
#' )
#' }
#' 
#' @importFrom brms brm bf prior student_t as_draws_df
#' @importFrom parallel makeCluster clusterEvalQ clusterExport parLapply stopCluster detectCores
#' @importFrom stats rnorm as.formula
#' @export
power_analysis_brms <- function(conditions, 
                                t_endpoint, 
                                threshold, 
                                n_sims = 200L, 
                                n_cores = parallel::detectCores() - 1L,
                                prior_spec = NULL,
                                success_threshold = 0.8,
                                verbose = TRUE,
                                ...) {
  
  # Input validation
  if (!is.list(conditions)) {
    stop("'conditions' must be a list", call. = FALSE)
  }
    required_params <- c("n_subjects", "n_timepoints", "time_effects", "group_time_effects")
  missing_params <- setdiff(required_params, names(conditions))
  if (length(missing_params) > 0) {
    stop("Missing required parameters in conditions: ", 
         paste(missing_params, collapse = ", "), call. = FALSE)
  }
  
  # Validate that required numeric parameters are actually numeric
  if (!is.numeric(conditions[["n_subjects"]]) || conditions[["n_subjects"]] < 1) {
    stop("'n_subjects' must be a positive number", call. = FALSE)
  }
  if (!is.numeric(conditions[["n_timepoints"]]) || conditions[["n_timepoints"]] < 1) {
    stop("'n_timepoints' must be a positive number", call. = FALSE)
  }
  if (!is.numeric(conditions[["time_effects"]])) {
    stop("'time_effects' must be numeric", call. = FALSE)
  }
  if (!is.numeric(conditions[["group_time_effects"]])) {
    stop("'group_time_effects' must be numeric", call. = FALSE)
  }
  
  if (!is.numeric(t_endpoint) || length(t_endpoint) != 1 || t_endpoint != round(t_endpoint)) {
    stop("'t_endpoint' must be a single integer", call. = FALSE)
  }
  
  n_timepoints <- conditions[["n_timepoints"]]
  if (t_endpoint < 1 || t_endpoint >= n_timepoints) {
    stop("'t_endpoint' must be between 1 and ", n_timepoints - 1, " (post-baseline timepoints)", call. = FALSE)
  }
  
  if (!is.numeric(threshold) || length(threshold) != 1) {
    stop("'threshold' must be a single numeric value", call. = FALSE)
  }
  
  if (!is.numeric(n_sims) || n_sims < 1) {
    stop("'n_sims' must be a positive integer", call. = FALSE)
  }
  
  if (!is.numeric(n_cores) || n_cores < 1) {
    stop("'n_cores' must be a positive integer", call. = FALSE)
  }
    if (!is.numeric(success_threshold) || success_threshold <= 0 || success_threshold > 1) {
    stop("'success_threshold' must be between 0 and 1", call. = FALSE)
  }
    # Print initial information
  if (verbose) {
    message("Starting Bayesian power analysis...")
    message("Configuration:")
    message("  - Total simulations: ", n_sims)
    message("  - Cores: ", if (n_cores > 1) min(n_cores, parallel::detectCores()) else 1)
    message("  - Target timepoint: t_", t_endpoint)
    message("  - Threshold: ", threshold)
    message("  - Success threshold: ", success_threshold)
    message("  - Sample size: ", conditions[["n_subjects"]], " subjects")
  }
  
  # Setup parallel cluster with error handling
  cl <- NULL
  start_time <- Sys.time()
  
  tryCatch({    if (n_cores > 1) {
      actual_cores <- min(n_cores, parallel::detectCores())
      if (verbose) message("Setting up parallel cluster with ", actual_cores, " cores...")
      
      cl <- parallel::makeCluster(actual_cores)
      
      # Load libraries on cluster nodes
      if (verbose) message("Loading libraries on cluster nodes...")
      parallel::clusterEvalQ(cl, {
        library(brms)
        library(stats)
      })
      
      # Export necessary objects to cluster
      if (verbose) message("Exporting functions and data to cluster nodes...")
      parallel::clusterExport(cl, c("conditions", "t_endpoint", 
                                    "threshold", "prior_spec", "success_threshold", 
                                    "generate_rct_data", "single_sim_brms", "%||%"), 
                              envir = environment())
      
      # Export additional arguments
      dots <- list(...)
      parallel::clusterExport(cl, "dots", envir = environment())
      
      if (verbose) {
        message("Running ", n_sims, " simulations in parallel...")
        message("Note: Progress updates are limited in parallel mode.")
      }
      
      # Run parallel simulations
      results <- parallel::parLapply(cl, seq_len(n_sims), function(sim_id) {
        do.call(single_sim_brms, c(list(sim_id = sim_id, 
                                        conditions = conditions, 
                                        t_endpoint = t_endpoint, 
                                        threshold = threshold, 
                                        prior_spec = prior_spec, 
                                        success_threshold = success_threshold), 
                                   dots))
      })
      
      if (verbose) message("Parallel simulations completed.")
      
    } else {
      if (verbose) message("Running ", n_sims, " simulations sequentially...")
      
      # Sequential processing with progress updates
      results <- vector("list", n_sims)
      successful_sims <- 0
      
      for (sim_id in seq_len(n_sims)) {
        if (verbose && (sim_id %% 10 == 0 || sim_id == 1)) {
          elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
          if (sim_id > 1) {
            eta <- elapsed / (sim_id - 1) * (n_sims - sim_id + 1)
            message("Simulation ", sim_id, "/", n_sims, " (", 
                   round((sim_id-1)/n_sims*100, 1), "%) - ",
                   "Elapsed: ", round(elapsed, 1), " min, ETA: ", round(eta, 1), " min")
          } else {
            message("Simulation ", sim_id, "/", n_sims, " - Starting...")
          }
        }
        
        results[[sim_id]] <- single_sim_brms(sim_id = sim_id, 
                                            conditions = conditions, 
                                            t_endpoint = t_endpoint, 
                                            threshold = threshold, 
                                            prior_spec = prior_spec, 
                                            success_threshold = success_threshold, 
                                            ...)
        
        # Count successful simulations for interim reporting
        if (!is.na(results[[sim_id]])) {
          successful_sims <- successful_sims + 1
        }
        
        # Provide interim power estimates every 25 simulations
        if (verbose && sim_id %% 25 == 0 && sim_id >= 25) {
          temp_probs <- unlist(results[1:sim_id])
          temp_probs <- temp_probs[!is.na(temp_probs)]
          if (length(temp_probs) > 0) {
            interim_power <- mean(temp_probs > success_threshold)
            message("  Interim power estimate (", sim_id, " sims): ", 
                   round(interim_power, 3), " (", length(temp_probs), " successful)")
          }
        }
      }
      
      if (verbose) message("Sequential simulations completed.")
    }
      }, finally = {
    # Always clean up cluster
    if (!is.null(cl)) {
      message("Cleaning up parallel cluster...")
      parallel::stopCluster(cl)
    }
  })
  
  # Calculate total runtime
  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  # Process results
  message("Processing results...")
  success_probs <- unlist(results)
  success_probs <- success_probs[!is.na(success_probs)]
  
  if (length(success_probs) == 0) {
    stop("All simulations failed", call. = FALSE)
  }
  
  # Calculate final statistics
  power_estimate <- mean(success_probs > success_threshold)
  n_successful <- length(success_probs)
  n_failed <- n_sims - n_successful
  mean_post_prob <- mean(success_probs)
  se_power <- sqrt(power_estimate * (1 - power_estimate) / n_successful)
  
  # Print comprehensive results summary
  message("\n=== POWER ANALYSIS RESULTS ===")
  message("Total runtime: ", round(total_time, 2), " minutes")
  message("Simulations completed: ", n_successful, "/", n_sims, 
         " (", round(n_successful/n_sims*100, 1), "%)")
  if (n_failed > 0) {
    message("Failed simulations: ", n_failed, " (", round(n_failed/n_sims*100, 1), "%)")
  }
  message("Mean posterior probability: ", round(mean_post_prob, 4))
  message("Estimated power: ", round(power_estimate, 4), 
         " ± ", round(1.96 * se_power, 4), " (95% CI)")
  message("Power interpretation: ", 
         if (power_estimate >= 0.8) "Adequate power (≥80%)" 
         else if (power_estimate >= 0.7) "Moderate power (70-79%)"
         else "Low power (<70%)")
  message("===============================\n")
  
  # Return power estimate (proportion of trials with posterior prob > success_threshold)
  list(
    power = power_estimate,
    n_successful_sims = n_successful,
    mean_posterior_prob = mean_post_prob,
    se_power = se_power,
    total_runtime_minutes = total_time,
    n_failed_sims = n_failed
  )
}

#' Power Simulation for RCT MMRM Models
#' 
#' Runs power analysis across multiple condition sets for longitudinal RCT designs.
#' 
#' @param conditions_list List of condition lists to test
#' @param t_endpoint Integer timepoint number for the target interaction effect (1 to n_timepoints-1)
#' @param threshold Numeric threshold value for probability of success  
#' @param n_sims Integer number of simulations per condition (default 200)
#' @param n_cores Integer number of cores for parallel processing
#' 
#' @return data.frame with power estimates for each condition
#' @export
power_sim_rct_mmrm <- function(conditions_list, 
                               t_endpoint, 
                               threshold, 
                               n_sims = 200L, 
                               n_cores = parallel::detectCores() - 1L) {
  
  # Input validation
  if (!is.list(conditions_list)) {
    stop("'conditions_list' must be a list", call. = FALSE)
  }
  
  if (length(conditions_list) == 0) {
    stop("'conditions_list' cannot be empty", call. = FALSE)
  }
  
  if (!is.numeric(t_endpoint) || length(t_endpoint) != 1 || t_endpoint != round(t_endpoint)) {
    stop("'t_endpoint' must be a single integer", call. = FALSE)
  }
  
  results <- vector("list", length(conditions_list))
  
  for (i in seq_along(conditions_list)) {
    conditions <- conditions_list[[i]]
    
    message("Running power analysis for condition ", i, " of ", length(conditions_list))
    
    power_result <- tryCatch({
      power_analysis_brms(
        conditions = conditions,
        t_endpoint = t_endpoint,
        threshold = threshold,
        n_sims = n_sims,
        n_cores = n_cores
      )
    }, error = function(e) {
      warning("Condition ", i, " failed: ", e$message, call. = FALSE)
      list(power = NA_real_, se_power = NA_real_, 
           mean_posterior_prob = NA_real_, n_successful_sims = 0L)
    })
    
    results[[i]] <- data.frame(
      condition_id = i,
      power = power_result[["power"]],
      se_power = power_result[["se_power"]],
      mean_posterior_prob = power_result[["mean_posterior_prob"]],
      n_successful_sims = power_result[["n_successful_sims"]],
      stringsAsFactors = FALSE
    )
  }
  
  # Combine results
  do.call(rbind, results)
}