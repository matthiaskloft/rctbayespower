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
#' @param p_sig_scs Probability threshold for success
#' @param p_sig_ftl Probability threshold for futility
#' @param id_iter Iteration identifier
#' @param id_cond Condition identifier
#'
#' @return Data frame with 1 row containing measures and IDs
#' @keywords internal
estimate_single_brms <- function(data, model, backend_args, target_params,
                                 thresholds_success, thresholds_futility,
                                 p_sig_scs, p_sig_ftl,
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
    cli::cli_warn(c(
      "brms estimation failed",
      "x" = "Condition {id_cond}, iteration {id_iter}",
      "i" = "Error: {e$message}"
    ))
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
    cli::cli_warn(c(
      "Posterior extraction failed",
      "i" = "Error: {e$message}"
    ))
    return(NULL)
  })

  if (is.null(posterior_rvars)) {
    return(create_error_result(id_iter, id_cond, id_analysis = 0L, "Extraction failed"))
  }

  # Resolve probability thresholds (info_frac = 1 for single-look designs)
  current_p_sig_scs <- resolve_threshold(p_sig_scs, 1)
  current_p_sig_ftl <- resolve_threshold(p_sig_ftl, 1)

  # Compute measures
  result <- tryCatch({
    df <- compute_measures(posterior_rvars, target_params, thresholds_success,
                          thresholds_futility, current_p_sig_scs, current_p_sig_ftl) |>
      dplyr::mutate(dplyr::across(-par_name, as.numeric))
    df |> dplyr::mutate(
      id_iter = id_iter,
      id_cond = id_cond,
      id_look = 0L,  # Single analysis
      converged = 1L,
      error_msg = NA_character_
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
#' @param p_sig_scs Probability threshold for success
#' @param p_sig_ftl Probability threshold for futility
#' @param id_iter Integer or vector of iteration identifiers
#' @param id_cond Integer or vector of condition identifiers
#'
#' @return Data frame with 1 row per simulation containing measures and IDs
#' @keywords internal
estimate_single_npe <- function(data, model, backend_args, target_params,
                                thresholds_success, thresholds_futility,
                                p_sig_scs, p_sig_ftl,
                                id_iter, id_cond) {

  # Batch detection: distinguish data.frame (single) from list of data.frames (batch)
  # Rationale: data.frame objects are also lists in R, so we explicitly exclude them.
  # Single sim: data.frame → is_batch = FALSE
  # Batch: list of data.frames → is_batch = TRUE
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
    cli::cli_warn(c(
      "NPE estimation failed",
      "i" = "Error: {e$message}"
    ))
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
      cli::cli_warn(c(
        "Posterior extraction failed",
        "x" = "Simulation {i}",
        "i" = "Error: {e$message}"
      ))
      return(NULL)
    })

    if (is.null(posterior_rvars)) {
      return(create_error_result(id_iter[i], id_cond[i], id_analysis = 0L, "Extraction failed"))
    }

    # Resolve probability thresholds (info_frac = 1 for single-look designs)
    current_p_sig_scs <- resolve_threshold(p_sig_scs, 1)
    current_p_sig_ftl <- resolve_threshold(p_sig_ftl, 1)

    # Compute measures
    result <- tryCatch({
      df <- compute_measures(posterior_rvars, target_params, thresholds_success,
                            thresholds_futility, current_p_sig_scs, current_p_sig_ftl) |>
        dplyr::mutate(dplyr::across(-par_name, as.numeric))
      df |> dplyr::mutate(
        id_iter = id_iter[i],
        id_cond = id_cond[i],
        id_look = 0L,
        converged = 1L,
        error_msg = NA_character_
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
    par_name = NA_character_,
    thr_scs = NA_real_,
    thr_ftl = NA_real_,
    pr_scs = NA_real_,
    pr_ftl = NA_real_,
    dec_scs = NA_real_,
    dec_ftl = NA_real_,
    post_med = NA_real_,
    post_mad = NA_real_,
    post_mn = NA_real_,
    post_sd = NA_real_,
    rhat = NA_real_,
    ess_bulk = NA_real_,
    ess_tail = NA_real_,
    id_iter = id_iter,
    id_cond = id_cond,
    id_look = id_analysis,
    converged = 0L,
    error_msg = error_msg
  )
}
