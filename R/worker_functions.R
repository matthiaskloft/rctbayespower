# Worker Functions for Parallel Execution
# Process work units (id_cond, id_iter) with appropriate estimation strategy

#' Process Single Work Unit
#'
#' Processes one (id_cond, id_iter) work unit. Detects strategy from design
#' and calls appropriate estimation function.
#'
#' @param id_cond Condition identifier
#' @param id_iter Iteration identifier (within condition)
#' @param condition_args Condition arguments with sim_args
#' @param design Design object (S7 or list for parallel workers)
#'
#' @return Data frame with results for all analyses (1 row for single, n rows for sequential)
#' @keywords internal
worker_process_single <- function(id_cond, id_iter, condition_args, design) {

  # Extract design components (handle S7 and list)
  if (inherits(design, "rctbayespower::rctbp_design") || inherits(design, "rctbp_design")) {
    data_simulation_fn <- design@model@data_simulation_fn
    backend <- design@model@backend
    estimation_model <- if (backend == "brms") design@model@brms_model else design@model@bayesflow_model
    backend_args <- design@model@backend_args
    target_params <- design@target_params
    p_sig_success <- design@p_sig_success
    p_sig_futility <- design@p_sig_futility
  } else if (is.list(design)) {
    # Regular list for parallel workers
    data_simulation_fn <- design$model_data_simulation_fn
    backend <- design$model_backend
    estimation_model <- if (backend == "brms") {
      design$model_brms_model
    } else {
      design$model_bayesflow_model
    }
    backend_args <- design$model_backend_args
    target_params <- design$target_params
    p_sig_success <- design$p_sig_success
    p_sig_futility <- design$p_sig_futility
  } else {
    cli::cli_abort(c(
      "Invalid design object",
      "i" = "This is an internal error - please report"
    ))
  }

  # Extract decision parameters from condition_args
  decision_args <- condition_args$decision_args
  thresholds_success <- decision_args$thresholds_success
  thresholds_futility <- decision_args$thresholds_futility
  analysis_at <- decision_args$analysis_at
  interim_function <- decision_args$interim_function
  adaptive <- decision_args$adaptive %||% FALSE

  # =============================================================================
  # STRATEGY DETECTION (Single vs Sequential vs Adaptive)
  # =============================================================================
  # Determines analysis strategy based on decision parameters:
  #
  # "single" - No interim analyses (analysis_at = NULL or empty)
  #   - One analysis at n_total only
  #   - Most common scenario
  #
  # "sequential" - Has interim timepoints, non-adaptive
  #   - Fixed interim analyses at predefined sample sizes
  #   - Early stopping possible but parameters don't change
  #
  # "adaptive" - Has interim timepoints, adaptive = TRUE
  #   - Parameters can be modified between looks
  #   - Not yet implemented (planned feature)
  has_interims <- !is.null(analysis_at) && length(analysis_at) > 0
  strategy <- if (!has_interims) {
    "single"
  } else if (!adaptive) {
    "sequential"
  } else {
    "adaptive"
  }

  # Simulate full dataset (unless adaptive - which handles its own simulation)
  if (strategy %in% c("single", "sequential")) {
    full_data <- tryCatch({
      do.call(data_simulation_fn, condition_args$sim_args)
    }, error = function(e) {
      cli::cli_warn(c(
        "Data simulation failed",
        "x" = "Condition {id_cond}, iteration {id_iter}",
        "i" = "Error: {e$message}"
      ))
      return(NULL)
    })

    if (is.null(full_data)) {
      return(create_error_result(id_iter, id_cond, id_analysis = 0L, "Data simulation failed"))
    }
  }

  # Call appropriate estimation function
  result <- switch(strategy,
    single = {
      if (backend == "brms") {
        estimate_single_brms(
          data = full_data,
          model = estimation_model,
          backend_args = backend_args,
          target_params = target_params,
          thresholds_success = thresholds_success,
          thresholds_futility = thresholds_futility,
          p_sig_success = p_sig_success,
          p_sig_futility = p_sig_futility,
          id_iter = id_iter,
          id_cond = id_cond
        )
      } else {
        estimate_single_npe(
          data = full_data,
          model = estimation_model,
          backend_args = backend_args,
          target_params = target_params,
          thresholds_success = thresholds_success,
          thresholds_futility = thresholds_futility,
          p_sig_success = p_sig_success,
          p_sig_futility = p_sig_futility,
          id_iter = id_iter,
          id_cond = id_cond
        )
      }
    },
    sequential = {
      if (backend == "brms") {
        estimate_sequential_brms(
          full_data = full_data,
          model = estimation_model,
          backend_args = backend_args,
          target_params = target_params,
          thresholds_success = thresholds_success,
          thresholds_futility = thresholds_futility,
          p_sig_success = p_sig_success,
          p_sig_futility = p_sig_futility,
          analysis_at = analysis_at,
          interim_function = interim_function,
          id_iter = id_iter,
          id_cond = id_cond
        )
      } else {
        # For single simulation with NPE sequential, still pass as list
        estimate_sequential_npe(
          full_data_list = list(full_data),
          model = estimation_model,
          backend_args = backend_args,
          target_params = target_params,
          thresholds_success = thresholds_success,
          thresholds_futility = thresholds_futility,
          p_sig_success = p_sig_success,
          p_sig_futility = p_sig_futility,
          analysis_at = analysis_at,
          interim_function = interim_function,
          id_iter = id_iter,
          id_cond = id_cond
        )
      }
    },
    adaptive = {
      cli::cli_abort(c(
        "Adaptive strategy not yet implemented",
        "i" = "This feature is planned for future releases"
      ))
    }
  )

  return(result)
}


#' Process Batch of Work Units (NPE Batching)
#'
#' Processes a batch of (id_cond, id_iter) work units together. Only used when
#' backend = "npe" and batch_size > 1. Enables efficient batch processing through
#' neural network.
#'
#' @param work_units List of work unit specifications, each with id_cond, id_iter, condition_args
#' @param design Design object (S7 or list for parallel workers)
#'
#' @return Data frame with results for all work units and all analyses
#' @keywords internal
worker_process_batch <- function(work_units, design) {

  batch_size <- length(work_units)

  # Extract design components
  if (inherits(design, "rctbayespower::rctbp_design") || inherits(design, "rctbp_design")) {
    data_simulation_fn <- design@model@data_simulation_fn
    backend <- design@model@backend
    estimation_model <- design@model@bayesflow_model  # Must be NPE
    backend_args <- design@model@backend_args
    target_params <- design@target_params
    p_sig_success <- design@p_sig_success
    p_sig_futility <- design@p_sig_futility
  } else if (is.list(design)) {
    data_simulation_fn <- design$model_data_simulation_fn
    backend <- design$model_backend
    estimation_model <- design$model_bayesflow_model
    backend_args <- design$model_backend_args
    target_params <- design$target_params
    p_sig_success <- design$p_sig_success
    p_sig_futility <- design$p_sig_futility
  } else {
    cli::cli_abort(c(
      "Invalid design object",
      "i" = "This is an internal error - please report"
    ))
  }

  # Validate this is NPE
  if (backend != "npe") {
    cli::cli_abort(c(
      "{.fn worker_process_batch} should only be called for NPE backend",
      "x" = "Backend is {.val {backend}}",
      "i" = "This is an internal error - please report"
    ))
  }

  # Extract IDs
  id_cond_vec <- sapply(work_units, function(wu) wu$id_cond)
  id_iter_vec <- sapply(work_units, function(wu) wu$id_iter)

  # Extract decision parameters from first work unit (assume homogenous batch)
  # Note: For batching to work efficiently, all work units should have same decision_args
  decision_args <- work_units[[1]]$condition_args$decision_args
  thresholds_success <- decision_args$thresholds_success
  thresholds_futility <- decision_args$thresholds_futility
  analysis_at <- decision_args$analysis_at
  interim_function <- decision_args$interim_function
  adaptive <- decision_args$adaptive %||% FALSE

  if (adaptive) {
    cli::cli_abort(c(
      "Adaptive strategy with batching not yet implemented",
      "i" = "This feature is planned for future releases"
    ))
  }

  # Detect strategy
  has_interims <- !is.null(analysis_at) && length(analysis_at) > 0
  strategy <- if (!has_interims) "single" else "sequential"

  # Simulate full datasets for all work units
  full_data_list <- lapply(work_units, function(wu) {
    tryCatch({
      do.call(data_simulation_fn, wu$condition_args$sim_args)
    }, error = function(e) {
      cli::cli_warn(c(
        "Data simulation failed",
        "x" = "Condition {wu$id_cond}, iteration {wu$id_iter}",
        "i" = "Error: {e$message}"
      ))
      return(NULL)
    })
  })

  # Check for simulation failures
  failed_sims <- which(sapply(full_data_list, is.null))
  if (length(failed_sims) > 0) {
    # Create error results for failed simulations
    error_results <- lapply(failed_sims, function(i) {
      create_error_result(
        id_iter_vec[i],
        id_cond_vec[i],
        id_analysis = 0L,
        "Data simulation failed"
      )
    })

    # Remove failed from batch
    if (length(failed_sims) == batch_size) {
      # All failed
      return(dplyr::bind_rows(error_results))
    }

    # Continue with successful sims
    full_data_list <- full_data_list[-failed_sims]
    id_cond_vec <- id_cond_vec[-failed_sims]
    id_iter_vec <- id_iter_vec[-failed_sims]
  } else {
    error_results <- list()
  }

  # Call appropriate estimation function
  result <- switch(strategy,
    single = {
      estimate_single_npe(
        data = full_data_list,
        model = estimation_model,
        backend_args = backend_args,
        target_params = target_params,
        thresholds_success = thresholds_success,
        thresholds_futility = thresholds_futility,
        p_sig_success = p_sig_success,
        p_sig_futility = p_sig_futility,
        id_iter = id_iter_vec,
        id_cond = id_cond_vec
      )
    },
    sequential = {
      estimate_sequential_npe(
        full_data_list = full_data_list,
        model = estimation_model,
        backend_args = backend_args,
        target_params = target_params,
        thresholds_success = thresholds_success,
        thresholds_futility = thresholds_futility,
        p_sig_success = p_sig_success,
        p_sig_futility = p_sig_futility,
        analysis_at = analysis_at,
        interim_function = interim_function,
        id_iter = id_iter_vec,
        id_cond = id_cond_vec
      )
    }
  )

  # Combine with any error results
  if (length(error_results) > 0) {
    result <- dplyr::bind_rows(result, dplyr::bind_rows(error_results))
  }

  return(result)
}


#' Prepare Design Components for Parallel Workers
#'
#' Serializes design object into a list that can be passed to parallel workers.
#' Necessary because S7 objects may not serialize correctly across cluster boundaries.
#'
#' @param design S7 rctbp_design object
#'
#' @return Named list with all necessary design components
#' @keywords internal
prepare_design_for_workers <- function(design) {
  list(
    model_data_simulation_fn = design@model@data_simulation_fn,
    model_backend = design@model@backend,
    model_brms_model = design@model@brms_model,
    model_bayesflow_model = design@model@bayesflow_model,
    model_backend_args = design@model@backend_args,
    target_params = design@target_params,
    p_sig_success = design@p_sig_success,
    p_sig_futility = design@p_sig_futility
  )
}
