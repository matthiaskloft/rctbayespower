# Single Analysis Estimation Strategies
# Backend × Strategy = brms/npe × single

#' Estimate Single Analysis with brms Backend
#'
#' Performs a single posterior estimation with no interim analyses using brms.
#' This is the simplest estimation strategy.
#'
#' @param data Data frame with simulated observations
#' @param model brmsfit template model
#' @param backend_args List of brms-specific arguments
#' @param target_params Character vector of parameter names
#' @param thresholds_success Numeric vector of success thresholds
#' @param thresholds_futility Numeric vector of futility thresholds
#' @param p_sig_success Probability threshold for success
#' @param p_sig_futility Probability threshold for futility
#' @param id_iter Iteration identifier
#' @param id_cond Condition identifier
#'
#' @return Data frame with 1 row containing measures and IDs
#' @keywords internal
estimate_single_brms <- function(data, model, backend_args, target_params,
                                 thresholds_success, thresholds_futility,
                                 p_sig_success, p_sig_futility,
                                 id_iter, id_cond) {

  # Estimate posterior
  estimation_result <- tryCatch({
    estimate_posterior(
      data = data,
      model = model,
      backend = "brms",
      backend_args = backend_args
    )
  }, error = function(e) {
    warning("brms estimation failed for iter=", id_iter, ", cond=", id_cond, ": ", e$message)
    return(NULL)
  })

  if (is.null(estimation_result)) {
    return(create_error_result(id_iter, id_cond, id_analysis = 0L, "Estimation failed"))
  }

  # Extract posterior rvars
  posterior_rvars <- tryCatch({
    extract_posterior_rvars(
      estimation_result = estimation_result,
      backend = "brms",
      target_params = target_params
    )
  }, error = function(e) {
    warning("Posterior extraction failed: ", e$message)
    return(NULL)
  })

  if (is.null(posterior_rvars)) {
    return(create_error_result(id_iter, id_cond, id_analysis = 0L, "Extraction failed"))
  }

  # Compute measures
  result <- tryCatch({
    df <- compute_measures(posterior_rvars, target_params, thresholds_success,
                          thresholds_futility, p_sig_success, p_sig_futility) |>
      dplyr::mutate(dplyr::across(-parameter, as.numeric))
    df |> dplyr::mutate(
      id_iter = id_iter,
      id_cond = id_cond,
      id_analysis = 0L,  # Single analysis
      converged = 1L,
      error = NA_character_
    )
  }, error = function(e) {
    return(create_error_result(id_iter, id_cond, id_analysis = 0L, as.character(e)))
  })

  return(result)
}


#' Estimate Single Analysis with NPE Backend
#'
#' Performs a single posterior estimation with no interim analyses using NPE.
#' Can process either a single simulation or a batch.
#'
#' @param data Data frame (single sim) or list of data frames (batch)
#' @param model Keras/TensorFlow NPE model
#' @param backend_args List of NPE-specific arguments (n_posterior_samples, etc.)
#' @param target_params Character vector of parameter names
#' @param thresholds_success Numeric vector of success thresholds
#' @param thresholds_futility Numeric vector of futility thresholds
#' @param p_sig_success Probability threshold for success
#' @param p_sig_futility Probability threshold for futility
#' @param id_iter Integer or vector of iteration identifiers
#' @param id_cond Integer or vector of condition identifiers
#'
#' @return Data frame with 1 row per simulation containing measures and IDs
#' @keywords internal
estimate_single_npe <- function(data, model, backend_args, target_params,
                                thresholds_success, thresholds_futility,
                                p_sig_success, p_sig_futility,
                                id_iter, id_cond) {

  # Determine if batch or single
  is_batch <- is.list(data) && !is.data.frame(data)
  batch_size <- if (is_batch) length(data) else 1

  # Estimate posterior (handles batch internally)
  estimation_result <- tryCatch({
    estimate_posterior(
      data = data,
      model = model,
      backend = "npe",
      backend_args = backend_args
    )
  }, error = function(e) {
    warning("NPE estimation failed: ", e$message)
    return(NULL)
  })

  if (is.null(estimation_result)) {
    # Return error for all sims in batch
    return(dplyr::bind_rows(lapply(seq_len(batch_size), function(i) {
      create_error_result(
        id_iter = id_iter[i],
        id_cond = id_cond[i],
        id_analysis = 0L,
        error_msg = "NPE estimation failed"
      )
    })))
  }

  # Process each simulation in batch
  results_list <- lapply(seq_len(batch_size), function(i) {
    # Extract posterior rvars for this simulation
    posterior_rvars <- tryCatch({
      extract_posterior_rvars(
        estimation_result = estimation_result,
        backend = "npe",
        target_params = target_params,
        sim_index = if (is_batch) i else NULL
      )
    }, error = function(e) {
      warning("Posterior extraction failed for sim ", i, ": ", e$message)
      return(NULL)
    })

    if (is.null(posterior_rvars)) {
      return(create_error_result(id_iter[i], id_cond[i], id_analysis = 0L, "Extraction failed"))
    }

    # Compute measures
    result <- tryCatch({
      df <- compute_measures(posterior_rvars, target_params, thresholds_success,
                            thresholds_futility, p_sig_success, p_sig_futility) |>
        dplyr::mutate(dplyr::across(-parameter, as.numeric))
      df |> dplyr::mutate(
        id_iter = id_iter[i],
        id_cond = id_cond[i],
        id_analysis = 0L,
        converged = 1L,
        error = NA_character_
      )
    }, error = function(e) {
      return(create_error_result(id_iter[i], id_cond[i], id_analysis = 0L, as.character(e)))
    })

    return(result)
  })

  # Combine all results
  dplyr::bind_rows(results_list)
}


#' Create Error Result Row
#'
#' Helper function to create a standardized error result data frame.
#'
#' @param id_iter Iteration identifier
#' @param id_cond Condition identifier
#' @param id_analysis Analysis identifier
#' @param error_msg Error message
#'
#' @return Data frame with NA values and error information
#' @keywords internal
create_error_result <- function(id_iter, id_cond, id_analysis, error_msg) {
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
    id_cond = id_cond,
    id_analysis = id_analysis,
    converged = 0L,
    error = error_msg
  )
}
