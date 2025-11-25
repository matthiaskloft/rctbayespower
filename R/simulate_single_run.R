# =============================================================================
# CORE SIMULATION FUNCTION: simulate_single_run()
# =============================================================================
# Pipeline: Data simulation → Posterior estimation → Posterior extraction →
# Measures computation. Handles multiple design object formats (S7 vs list)
# for compatibility with parallel workers.

#' Single Run Simulation for RCT Bayesian Power Analysis
#'
#' Executes a single simulation run using pre-validated condition arguments.
#' This function is the core simulation engine used by power analysis functions.
#'
#' @param condition_arguments A single entry from the condition_arguments list
#'   created by [build_conditions()]. Contains 'sim_args' with data simulation
#'   parameters and 'decision_args' with decision thresholds and criteria.
#' @param id_iter Iteration identifier for tracking individual simulation runs
#' @param design A rctbp_design or rctbp_power_analysis object containing the
#'   simulation and model specifications
#'
#' @return A data frame with power analysis measures for the simulation
#'
#' @keywords internal
simulate_single_run <- function(condition_arguments,
                                id_iter,
                                design) {
  # No validations - lowest level function, arguments pre-validated upstream

  # =============================================================================
  # DESIGN OBJECT FORMAT HANDLING
  # =============================================================================
  # Design arrives in different formats depending on execution context:
  # 1. S7 rctbp_design object (direct calls)
  # 2. S7 rctbp_power_analysis object (with promoted model access)
  # 3. List format (parallel workers - S7 doesn't serialize)
  #
  # Rationale: Parallel workers receive serialized lists (see
  # prepare_design_for_workers in class_power_analysis.R). This branching
  # logic extracts fields using appropriate syntax for each format.
  if (inherits(design, "rctbayespower::rctbp_design") || inherits(design, "rctbp_design")) {
    # S7 design object
    data_simulation_fn <- design@model@data_simulation_fn
    backend <- design@model@backend
    estimation_model <- if (backend == "brms") design@model@brms_model else design@model@bayesflow_model
    backend_args <- design@model@backend_args
    target_params <- design@target_params
    p_sig_success <- design@p_sig_success
    p_sig_futility <- design@p_sig_futility
  } else if (inherits(design, "rctbayespower::rctbp_power_analysis") || inherits(design, "rctbp_power_analysis")) {
    # S7 power analysis object - use promoted model access
    data_simulation_fn <- design@model@data_simulation_fn
    backend <- design@model@backend
    estimation_model <- if (backend == "brms") design@model@brms_model else design@model@bayesflow_model
    backend_args <- design@model@backend_args
    target_params <- design@target_params
    p_sig_success <- design@p_sig_success
    p_sig_futility <- design@p_sig_futility
  } else if (is.list(design)) {
    # Regular list with design components (from parallel workers)
    data_simulation_fn <- design$model_data_simulation_fn
    backend <- design$model_backend %||% "brms"  # Default for backward compatibility
    estimation_model <- if (backend == "brms") {
      design$model_brms_model
    } else {
      design$model_bayesflow_model
    }
    backend_args <- design$model_backend_args %||% list()
    target_params <- design$target_params
    p_sig_success <- design$p_sig_success
    p_sig_futility <- design$p_sig_futility
  } else {
    cli::cli_abort(c(
      "Invalid design object",
      "i" = "This is an internal error - please report"
    ))
  }

  # Simulate data with error handling
  simulated_data <- tryCatch({
    do.call(data_simulation_fn, args = condition_arguments$sim_args)
  }, error = function(e) {
    n_total <- if(is.null(condition_arguments$sim_args$n_total)) "unknown" else condition_arguments$sim_args$n_total
    cli::cli_warn(c(
      "Data simulation failed",
      "x" = "n_total = {n_total}",
      "i" = "Error: {e$message}"
    ))
    return(NULL)
  })

  # Return error result if data simulation failed
  # Error results contain NA for all metrics but preserve iteration/condition IDs
  # for result aggregation and debugging
  if (is.null(simulated_data)) {
    return(data.frame(
      parameter = NA_character_,
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
      id_iter = id_iter,
      id_cond = condition_arguments$id_cond,
      converged = 0L,
      error = "Data simulation failed"
    ))
  }

  # Estimate posterior using backend-specific method
  estimation_result <- tryCatch({
    estimate_posterior(
      data = simulated_data,
      model = estimation_model,
      backend = backend,
      backend_args = backend_args
    )
  }, error = function(e) {
    n_total <- if (is.null(condition_arguments$sim_args$n_total))
      "unknown"
    else
      condition_arguments$sim_args$n_total
    cli::cli_warn(c(
      "Posterior estimation failed",
      "x" = "n_total = {n_total}",
      "i" = "Error: {e$message}"
    ))
    return(NULL)
  })

  # Check if estimation was successful
  if (is.null(estimation_result)) {
    return(data.frame(
      parameter = NA_character_,
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
      id_iter = id_iter,
      id_cond = condition_arguments$id_cond,
      converged = 0L,
      error = "Posterior estimation failed"
    ))
  }

  # Extract posterior as rvars
  posterior_rvars <- tryCatch({
    extract_posterior_rvars(
      estimation_result = estimation_result,
      backend = backend,
      target_params = target_params
    )
  }, error = function(e) {
    cli::cli_warn(c(
      "Posterior extraction failed",
      "i" = "Error: {e$message}"
    ))
    return(NULL)
  })

  if (is.null(posterior_rvars)) {
    return(data.frame(
      parameter = NA_character_,
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
      id_iter = id_iter,
      id_cond = condition_arguments$id_cond,
      converged = 0L,
      error = "Posterior extraction failed"
    ))
  }

  # Extract decision parameters from condition_arguments
  decision_args <- condition_arguments$decision_args

  # Compute measures (backend-agnostic)
  result <- tryCatch({
    df <- compute_measures(posterior_rvars,
                          target_params,
                          decision_args$thresholds_success,
                          decision_args$thresholds_futility,
                          p_sig_success,
                          p_sig_futility) |>
      dplyr::mutate(dplyr::across(-parameter, as.numeric))
    df |> dplyr::mutate(
      id_iter = id_iter,
      id_cond = condition_arguments$id_cond,
      id_analysis = 0L,  # 0 = single analysis (final only); 1+ = sequential interim numbers
      converged = 1L,
      error = NA_character_
    )
  }, error = function(e) {
    data.frame(
      parameter = NA_character_,
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
      id_iter = id_iter,
      id_cond = condition_arguments$id_cond,
      id_analysis = 0L,
      converged = 0L,
      error = as.character(e)
    )
  })

  return(result)
}
