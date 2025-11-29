# Worker Functions for Parallel Execution
# Process work units (id_cond, id_iter) with appropriate estimation strategy

#' Process Single Work Unit
#'
#' Processes one (id_cond, id_iter) work unit. Detects strategy from design
#' and calls appropriate estimation function.
#'
#' @param id_cond Condition identifier
#' @param id_iter Iteration identifier (within condition)
#' @param condition_args Condition arguments with sim_args and decision_args
#' @param design Design object (S7 or list for parallel workers)
#'
#' @return Data frame with results for all analyses (1 row for single, n rows for sequential)
#' @keywords internal
worker_process_single <- function(id_cond, id_iter, condition_args, design) {

  # Extract design components (handle S7 and list)
  # NOTE: After merge, model properties are directly on design (not design@model@*)
  if (inherits(design, "rctbayespower::rctbp_design") || inherits(design, "rctbp_design")) {
    sim_fn <- design@sim_fn
    backend <- design@backend
    estimation_model <- design@inference_model
    backend_args <- if (backend == "brms") design@backend_args_brms else design@backend_args_bf
    target_params <- design@target_params
  } else if (is.list(design)) {
    # Regular list for parallel workers
    sim_fn <- design$sim_fn
    backend <- design$backend
    estimation_model <- design$inference_model
    backend_args <- design$backend_args
    target_params <- design$target_params
  } else {
    cli::cli_abort(c(
      "Invalid design object",
      "i" = "This is an internal error - please report"
    ))
  }

  # Extract decision parameters from condition_args
  # NOTE: thr_dec_eff, thr_dec_fut now come from decision_args (not design)
  decision_args <- condition_args$decision_args
  thr_fx_eff <- decision_args$thr_fx_eff
  thr_fx_fut <- decision_args$thr_fx_fut
  thr_dec_eff <- decision_args$thr_dec_eff
  thr_dec_fut <- decision_args$thr_dec_fut
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
      do.call(sim_fn, condition_args$sim_args)
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
          thr_fx_eff = thr_fx_eff,
          thr_fx_fut = thr_fx_fut,
          thr_dec_eff = thr_dec_eff,
          thr_dec_fut = thr_dec_fut,
          id_iter = id_iter,
          id_cond = id_cond
        )
      } else if (backend == "bf") {
        estimate_single_bf(
          data = full_data,
          model = estimation_model,
          backend_args = backend_args,
          target_params = target_params,
          thr_fx_eff = thr_fx_eff,
          thr_fx_fut = thr_fx_fut,
          thr_dec_eff = thr_dec_eff,
          thr_dec_fut = thr_dec_fut,
          id_iter = id_iter,
          id_cond = id_cond
        )
      } else {
        cli::cli_abort(c(
          "Unknown backend: {.val {backend}}",
          "i" = "Supported backends: {.val brms}, {.val bf}"
        ))
      }
    },
    sequential = {
      if (backend == "brms") {
        estimate_sequential_brms(
          full_data = full_data,
          model = estimation_model,
          backend_args = backend_args,
          target_params = target_params,
          thr_fx_eff = thr_fx_eff,
          thr_fx_fut = thr_fx_fut,
          thr_dec_eff = thr_dec_eff,
          thr_dec_fut = thr_dec_fut,
          analysis_at = analysis_at,
          interim_function = interim_function,
          id_iter = id_iter,
          id_cond = id_cond
        )
      } else if (backend == "bf") {
        # For single simulation with BayesFlow sequential, pass as list
        estimate_sequential_bf(
          full_data_list = list(full_data),
          model = estimation_model,
          backend_args = backend_args,
          target_params = target_params,
          thr_fx_eff = thr_fx_eff,
          thr_fx_fut = thr_fx_fut,
          thr_dec_eff = thr_dec_eff,
          thr_dec_fut = thr_dec_fut,
          analysis_at = analysis_at,
          interim_function = interim_function,
          id_iter = id_iter,
          id_cond = id_cond
        )
      } else {
        cli::cli_abort(c(
          "Unknown backend: {.val {backend}}",
          "i" = "Supported backends: {.val brms}, {.val bf}"
        ))
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


#' Process Batch of Work Units (BayesFlow Batching)
#'
#' Processes a batch of (id_cond, id_iter) work units together. Only used when
#' backend = "bf" (BayesFlow) and batch_size > 1. Enables efficient batch processing
#' through neural posterior estimation.
#'
#' @param work_units List of work unit specifications, each with id_cond, id_iter, condition_args
#' @param design Design object (S7 or list for parallel workers)
#'
#' @return Data frame with results for all work units and all analyses
#' @keywords internal
worker_process_batch <- function(work_units, design) {

  batch_size <- length(work_units)

  # Extract design components
  # NOTE: After merge, model properties are directly on design
  if (inherits(design, "rctbayespower::rctbp_design") || inherits(design, "rctbp_design")) {
    sim_fn <- design@sim_fn
    backend <- design@backend
    estimation_model <- design@inference_model
    backend_args <- design@backend_args_bf
    target_params <- design@target_params
  } else if (is.list(design)) {
    sim_fn <- design$sim_fn
    backend <- design$backend
    estimation_model <- design$inference_model
    backend_args <- design$backend_args  # Already selected in prepare_design_for_workers
    target_params <- design$target_params
  } else {
    cli::cli_abort(c(
      "Invalid design object",
      "i" = "This is an internal error - please report"
    ))
  }

  # Validate this is BayesFlow
  if (backend != "bf") {
    cli::cli_abort(c(
      "{.fn worker_process_batch} should only be called for BayesFlow backend",
      "x" = "Backend is {.val {backend}}",
      "i" = "This is an internal error - please report"
    ))
  }

  # Extract IDs
  id_cond_vec <- sapply(work_units, function(wu) wu$id_cond)
  id_iter_vec <- sapply(work_units, function(wu) wu$id_iter)

  # Extract decision parameters from first work unit (assume homogenous batch)
  # NOTE: thr_dec_eff, thr_dec_fut now come from decision_args
  decision_args <- work_units[[1]]$condition_args$decision_args
  thr_fx_eff <- decision_args$thr_fx_eff
  thr_fx_fut <- decision_args$thr_fx_fut
  thr_dec_eff <- decision_args$thr_dec_eff
  thr_dec_fut <- decision_args$thr_dec_fut
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
      do.call(sim_fn, wu$condition_args$sim_args)
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

  # Call appropriate estimation function (BayesFlow backend)
  result <- switch(strategy,
    single = {
      estimate_single_bf(
        data = full_data_list,
        model = estimation_model,
        backend_args = backend_args,
        target_params = target_params,
        thr_fx_eff = thr_fx_eff,
        thr_fx_fut = thr_fx_fut,
        thr_dec_eff = thr_dec_eff,
        thr_dec_fut = thr_dec_fut,
        id_iter = id_iter_vec,
        id_cond = id_cond_vec
      )
    },
    sequential = {
      estimate_sequential_bf(
        full_data_list = full_data_list,
        model = estimation_model,
        backend_args = backend_args,
        target_params = target_params,
        thr_fx_eff = thr_fx_eff,
        thr_fx_fut = thr_fx_fut,
        thr_dec_eff = thr_dec_eff,
        thr_dec_fut = thr_dec_fut,
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
  # Backend is already resolved at design creation time (never "auto")
  # NOTE: After merge, all properties are directly on design
  backend <- design@backend

  # Select appropriate backend_args based on backend
  backend_args <- if (backend == "brms") {
    design@backend_args_brms
  } else {
    design@backend_args_bf
  }

  list(
    # Model components (directly on design after merge)
    sim_fn = design@sim_fn,
    backend = backend,
    inference_model = design@inference_model,
    backend_args = backend_args,
    backend_args_brms = design@backend_args_brms,
    backend_args_bf = design@backend_args_bf,
    # Design parameters
    target_params = design@target_params
    # NOTE: p_sig_scs, p_sig_ftl, analysis_at, interim_function, adaptive
    # are now in conditions (via decision_args), not design
  )
}
