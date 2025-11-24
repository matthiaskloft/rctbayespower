# Sequential Analysis Estimation Strategies
# Backend × Strategy = brms/npe × sequential (with fixed interim analyses, no adaptation)

#' Estimate Sequential Analysis with brms Backend
#'
#' Performs sequential interim analyses with brms backend. Full dataset is simulated
#' once, then analyzed at multiple interim timepoints.
#'
#' @param full_data Complete simulated dataset (all n_total observations)
#' @param model brmsfit template model
#' @param backend_args List of brms-specific arguments
#' @param target_params Character vector of parameter names
#' @param thresholds_success Numeric vector of success thresholds
#' @param thresholds_futility Numeric vector of futility thresholds
#' @param p_sig_success Probability threshold for success
#' @param p_sig_futility Probability threshold for futility
#' @param analysis_at Vector of sample sizes for interim analyses
#' @param interim_function Function to make interim decisions
#' @param id_iter Iteration identifier
#' @param id_cond Condition identifier
#'
#' @return Data frame with n_analyses rows (one per interim + final)
#' @importFrom dplyr if_else
#' @keywords internal
estimate_sequential_brms <- function(full_data, model, backend_args, target_params,
                                     thresholds_success, thresholds_futility,
                                     p_sig_success, p_sig_futility,
                                     analysis_at, interim_function,
                                     id_iter, id_cond) {

  n_total <- nrow(full_data)
  analysis_schedule <- c(analysis_at, n_total)
  results_list <- list()
  stopped <- FALSE
  stop_reason <- NA_character_

  # Loop through each analysis timepoint sequentially
  for (id_analysis in seq_along(analysis_schedule)) {
    current_n <- analysis_schedule[id_analysis]
    is_final <- id_analysis == length(analysis_schedule)

    # Subset data to current analysis point
    analysis_data <- full_data[1:current_n, ]

    # Estimate posterior
    estimation_result <- tryCatch({
      estimate_posterior(
        data = analysis_data,
        model = model,
        backend = "brms",
        backend_args = backend_args
      )
    }, error = function(e) {
      warning("brms estimation failed at analysis ", id_analysis, ": ", e$message)
      return(NULL)
    })

    if (is.null(estimation_result)) {
      results_list[[id_analysis]] <- create_error_result(
        id_iter, id_cond, id_analysis,
        "Estimation failed"
      )
      next
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
      results_list[[id_analysis]] <- create_error_result(
        id_iter, id_cond, id_analysis,
        "Extraction failed"
      )
      next
    }

    # Compute measures
    measures <- tryCatch({
      compute_measures(posterior_rvars, target_params, thresholds_success,
                      thresholds_futility, p_sig_success, p_sig_futility) |>
        dplyr::mutate(dplyr::across(-parameter, as.numeric))
    }, error = function(e) {
      results_list[[id_analysis]] <- create_error_result(
        id_iter, id_cond, id_analysis,
        as.character(e)
      )
      return(NULL)
    })

    if (is.null(measures)) next

    # Make interim decision (if not final and not already stopped)
    interim_decision <- NULL
    if (!is_final && !stopped) {
      interim_decision <- tryCatch({
        interim_function(
          interim_summaries = measures,
          current_n = current_n,
          analysis_at = current_n,
          n_total = n_total
        )
      }, error = function(e) {
        warning("Interim function failed: ", e$message)
        list(decision = "continue", modified_params = NULL)
      })

      # Record stopping
      if (interim_decision$decision %in% c("stop_success", "stop_futility")) {
        stopped <- TRUE
        stop_reason <- interim_decision$decision
      }
    }

    # Add IDs and interim information
    measures <- measures |>
      dplyr::mutate(
        id_iter = id_iter,
        id_cond = id_cond,
        id_analysis = id_analysis,
        n_analyzed = current_n,
        stopped = stopped,
        stop_reason = if_else(stopped, stop_reason, NA_character_),
        interim_decision = if (!is.null(interim_decision)) list(interim_decision) else list(NULL),
        converged = 1L,
        error = NA_character_
      )

    results_list[[id_analysis]] <- measures
  }

  # Combine all analyses
  dplyr::bind_rows(results_list)
}


#' Estimate Sequential Analysis with NPE Backend (Batched)
#'
#' Performs sequential interim analyses with NPE backend using batch processing.
#' All simulations at the same interim step are processed together in a single
#' NPE forward pass for efficiency.
#'
#' @param full_data_list List of complete datasets (one per simulation in batch)
#' @param model Keras/TensorFlow NPE model
#' @param backend_args List of NPE-specific arguments
#' @param target_params Character vector of parameter names
#' @param thresholds_success Numeric vector of success thresholds
#' @param thresholds_futility Numeric vector of futility thresholds
#' @param p_sig_success Probability threshold for success
#' @param p_sig_futility Probability threshold for futility
#' @param analysis_at Vector of sample sizes for interim analyses
#' @param interim_function Function to make interim decisions
#' @param id_iter Vector of iteration identifiers (one per sim in batch)
#' @param id_cond Vector of condition identifiers (one per sim in batch)
#'
#' @return Data frame with (batch_size × n_analyses) rows
#' @importFrom dplyr if_else
#' @keywords internal
estimate_sequential_npe <- function(full_data_list, model, backend_args, target_params,
                                    thresholds_success, thresholds_futility,
                                    p_sig_success, p_sig_futility,
                                    analysis_at, interim_function,
                                    id_iter, id_cond) {

  batch_size <- length(full_data_list)
  n_total <- nrow(full_data_list[[1]])
  analysis_schedule <- c(analysis_at, n_total)
  n_analyses <- length(analysis_schedule)

  # Initialize tracking for each sim
  stopped <- rep(FALSE, batch_size)
  stop_reason <- rep(NA_character_, batch_size)

  # Storage for results: list of lists
  # results_by_sim[[sim_idx]][[analysis_idx]]
  results_by_sim <- vector("list", batch_size)
  for (i in seq_len(batch_size)) {
    results_by_sim[[i]] <- vector("list", n_analyses)
  }

  # Loop through each analysis timepoint
  for (id_analysis in seq_along(analysis_schedule)) {
    current_n <- analysis_schedule[id_analysis]
    is_final <- id_analysis == n_analyses

    # Subset all batch data to current analysis point
    analysis_data_batch <- lapply(full_data_list, function(fd) fd[1:current_n, ])

    # NPE batch estimation (single forward pass for all sims!)
    batch_posteriors <- tryCatch({
      estimate_posterior(
        data = analysis_data_batch,
        model = model,
        backend = "npe",
        backend_args = backend_args
      )
    }, error = function(e) {
      warning("NPE batch estimation failed at analysis ", id_analysis, ": ", e$message)
      return(NULL)
    })

    if (is.null(batch_posteriors)) {
      # Store error for all sims
      for (sim_idx in seq_len(batch_size)) {
        results_by_sim[[sim_idx]][[id_analysis]] <- create_error_result(
          id_iter[sim_idx], id_cond[sim_idx], id_analysis,
          "NPE batch estimation failed"
        )
      }
      next
    }

    # Process each simulation in the batch
    for (sim_idx in seq_len(batch_size)) {
      # Extract posterior rvars for this specific simulation
      posterior_rvars <- tryCatch({
        extract_posterior_rvars(
          estimation_result = batch_posteriors,
          backend = "npe",
          target_params = target_params,
          sim_index = sim_idx
        )
      }, error = function(e) {
        warning("Posterior extraction failed for sim ", sim_idx, ": ", e$message)
        return(NULL)
      })

      if (is.null(posterior_rvars)) {
        results_by_sim[[sim_idx]][[id_analysis]] <- create_error_result(
          id_iter[sim_idx], id_cond[sim_idx], id_analysis,
          "Extraction failed"
        )
        next
      }

      # Compute measures
      measures <- tryCatch({
        compute_measures(posterior_rvars, target_params, thresholds_success,
                        thresholds_futility, p_sig_success, p_sig_futility) |>
          dplyr::mutate(dplyr::across(-parameter, as.numeric))
      }, error = function(e) {
        results_by_sim[[sim_idx]][[id_analysis]] <- create_error_result(
          id_iter[sim_idx], id_cond[sim_idx], id_analysis,
          as.character(e)
        )
        return(NULL)
      })

      if (is.null(measures)) next

      # Make interim decision (if not final and not already stopped for this sim)
      interim_decision <- NULL
      if (!is_final && !stopped[sim_idx]) {
        interim_decision <- tryCatch({
          interim_function(
            interim_summaries = measures,
            current_n = current_n,
            analysis_at = current_n,
            n_total = n_total
          )
        }, error = function(e) {
          warning("Interim function failed: ", e$message)
          list(decision = "continue", modified_params = NULL)
        })

        # Record stopping for this sim
        if (interim_decision$decision %in% c("stop_success", "stop_futility")) {
          stopped[sim_idx] <- TRUE
          stop_reason[sim_idx] <- interim_decision$decision
        }
      }

      # Add IDs and interim information
      measures <- measures |>
        dplyr::mutate(
          id_iter = id_iter[sim_idx],
          id_cond = id_cond[sim_idx],
          id_analysis = id_analysis,
          n_analyzed = current_n,
          stopped = stopped[sim_idx],
          stop_reason = if_else(stopped[sim_idx], stop_reason[sim_idx], NA_character_),
          interim_decision = if (!is.null(interim_decision)) list(interim_decision) else list(NULL),
          converged = 1L,
          error = NA_character_
        )

      results_by_sim[[sim_idx]][[id_analysis]] <- measures
    }
  }

  # Flatten results: combine all sims and all analyses
  all_results <- lapply(results_by_sim, function(sim_results) {
    dplyr::bind_rows(sim_results)
  })

  dplyr::bind_rows(all_results)
}
