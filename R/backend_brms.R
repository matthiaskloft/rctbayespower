# =============================================================================
# BRMS BACKEND
# =============================================================================
# All brms-specific posterior estimation, extraction, and summarization.
# Provides complete brms pathway independent of other backends.
#
# Entry points:
#   - estimate_single_brms(): Single analysis estimation
#   - estimate_sequential_brms(): Sequential/interim analysis estimation
#
# Key functions:
#   - estimate_posterior_brms(): Fit brms model to data
#   - extract_posterior_rvars_brms(): Extract posterior as rvars
#   - summarize_post_brms(): Wrapper around compute_measures()
#
# Output: All functions produce results compatible with summarize_sims()

# =============================================================================
# POSTERIOR EXTRACTION
# =============================================================================

#' Extract Posterior Samples as rvars from brms Model
#'
#' Extracts posterior samples from a fitted brms model and converts them
#' to rvar format from the posterior package. This is the brms-specific
#' implementation of posterior extraction.
#'
#' @param brmsfit A fitted brmsfit object
#' @param target_params Character vector of parameter names to extract
#'
#' @return A named list of rvar objects, one per target parameter
#' @keywords internal
extract_posterior_rvars_brms <- function(brmsfit, target_params) {
  if (!inherits(brmsfit, "brmsfit")) {
    cli::cli_abort(c(
      "{.arg brmsfit} must be a fitted brms model object",
      "x" = "You supplied {.cls {class(brmsfit)}}",
      "i" = "Provide a fitted {.cls brmsfit} object"
    ))
  }

  # Extract all target parameters as rvars
  posterior_rvars <- brms::as_draws_rvars(brmsfit, variable = target_params)

  return(posterior_rvars)
}


# =============================================================================
# POSTERIOR ESTIMATION
# =============================================================================

#' Estimate Posterior Using brms Backend
#'
#' Fits a brms model to observed data by updating a pre-compiled template model.
#'
#' @param data Data frame containing the observed data
#' @param brms_model Pre-compiled brmsfit template model (chains = 0)
#' @param backend_args Named list of brms-specific arguments (chains, iter, cores, etc.)
#'
#' @return A fitted brmsfit object containing posterior samples
#' @keywords internal
estimate_posterior_brms <- function(data, brms_model, backend_args = list()) {
  if (!inherits(brms_model, "brmsfit")) {
    cli::cli_abort(c(
      "{.arg brms_model} must be a valid brmsfit object",
      "x" = "You supplied {.cls {class(brms_model)}}",
      "i" = "Provide a compiled {.cls brmsfit} template model"
    ))
  }

  # Merge with default brms args if not provided
  default_args <- list(
    object = brms_model,
    newdata = data
  )

  # User backend_args override defaults
  final_args <- modifyList(default_args, backend_args)

  # Fit model (suppress output to avoid console I/O bottleneck)
  fitted_model <- suppressWarnings(suppressMessages(
    do.call(stats::update, final_args)
  ))

  return(fitted_model)
}


# =============================================================================
# POSTERIOR SUMMARIZATION
# =============================================================================

#' Summarize Posterior - brms Backend
#'
#' Wrapper around [compute_measures()] for brms posterior output.
#' Provides consistent API with summarize_post_bf() (BayesFlow backend).
#'
#' @param posterior_rvars draws_rvars object from brms
#' @param target_params Character vector of parameter names
#' @param thresh_scs Numeric vector of success thresholds (ROPE)
#' @param thresh_ftl Numeric vector of futility thresholds (ROPE)
#' @param p_sig_scs Probability threshold for success
#' @param p_sig_ftl Probability threshold for futility
#'
#' @return Data frame with package output schema
#' @keywords internal
summarize_post_brms <- function(posterior_rvars, target_params,
                                 thresh_scs, thresh_ftl,
                                 p_sig_scs, p_sig_ftl) {
  # Delegate to existing compute_measures() function
  # which handles rvar operations
  compute_measures(
    posterior_rvars = posterior_rvars,
    target_params = target_params,
    thresh_scs = thresh_scs,
    thresh_ftl = thresh_ftl,
    p_sig_scs = p_sig_scs,
    p_sig_ftl = p_sig_ftl
  )
}


# =============================================================================
# SINGLE ANALYSIS ESTIMATION
# =============================================================================

#' Estimate Single Analysis with brms Backend
#'
#' Performs a single posterior estimation with no interim analyses using brms.
#' This is the simplest estimation strategy.
#'
#' @param data Data frame with simulated observations
#' @param model brmsfit template model
#' @param backend_args List of brms-specific arguments
#' @param target_params Character vector of parameter names
#' @param thresh_scs Numeric vector of success thresholds (ROPE)
#' @param thresh_ftl Numeric vector of futility thresholds (ROPE)
#' @param p_sig_scs Probability threshold for success
#' @param p_sig_ftl Probability threshold for futility
#' @param id_iter Iteration identifier
#' @param id_cond Condition identifier
#'
#' @return Data frame with 1 row containing measures and IDs
#' @keywords internal
estimate_single_brms <- function(data, model, backend_args, target_params,
                                 thresh_scs, thresh_ftl,
                                 p_sig_scs, p_sig_ftl,
                                 id_iter, id_cond) {

  # Estimate posterior
  estimation_result <- tryCatch({
    estimate_posterior_brms(
      data = data,
      brms_model = model,
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
    extract_posterior_rvars_brms(
      brmsfit = estimation_result,
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

  # Compute measures using brms summarization
  result <- tryCatch({
    df <- summarize_post_brms(
      posterior_rvars = posterior_rvars,
      target_params = target_params,
      thresh_scs = thresh_scs,
      thresh_ftl = thresh_ftl,
      p_sig_scs = current_p_sig_scs,
      p_sig_ftl = current_p_sig_ftl
    ) |>
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


# =============================================================================
# SEQUENTIAL ANALYSIS ESTIMATION
# =============================================================================

#' Estimate Sequential Analysis with brms Backend
#'
#' Performs sequential interim analyses with brms backend. Full dataset is simulated
#' once, then analyzed at multiple interim timepoints.
#'
#' @param full_data Complete simulated dataset (all n_total observations)
#' @param model brmsfit template model
#' @param backend_args List of brms-specific arguments
#' @param target_params Character vector of parameter names
#' @param thresh_scs Numeric vector of success thresholds (ROPE)
#' @param thresh_ftl Numeric vector of futility thresholds (ROPE)
#' @param p_sig_scs Probability threshold for success (numeric or pre-resolved vector)
#' @param p_sig_ftl Probability threshold for futility (numeric or pre-resolved vector)
#' @param analysis_at Vector of sample sizes for all analyses (including final at n_total)
#' @param interim_function Function to make interim decisions
#' @param id_iter Iteration identifier
#' @param id_cond Condition identifier
#'
#' @return Data frame with n_analyses rows (one per interim + final)
#' @importFrom dplyr if_else
#' @keywords internal
estimate_sequential_brms <- function(full_data, model, backend_args, target_params,
                                     thresh_scs, thresh_ftl,
                                     p_sig_scs, p_sig_ftl,
                                     analysis_at, interim_function,
                                     id_iter, id_cond) {

  n_total <- nrow(full_data)
  # analysis_at now includes the final analysis at n_total (last value = n_total)
  analysis_schedule <- analysis_at
  results_list <- list()
  stopped <- FALSE
  stop_reason <- NA_character_

  # Loop through each analysis timepoint sequentially
  for (id_analysis in seq_along(analysis_schedule)) {
    current_n <- analysis_schedule[id_analysis]
    is_final <- id_analysis == length(analysis_schedule)

    # Calculate information fraction for threshold resolution
    info_frac <- current_n / n_total

    # Resolve probability thresholds (handle function or numeric)
    current_p_sig_scs <- resolve_threshold(p_sig_scs, info_frac)
    current_p_sig_ftl <- resolve_threshold(p_sig_ftl, info_frac)

    # Subset data to current analysis point
    analysis_data <- full_data[1:current_n, ]

    # Estimate posterior
    estimation_result <- tryCatch({
      estimate_posterior_brms(
        data = analysis_data,
        brms_model = model,
        backend_args = backend_args
      )
    }, error = function(e) {
      cli::cli_warn(c(
        "brms estimation failed",
        "x" = "Analysis {id_analysis}",
        "i" = "Error: {e$message}"
      ))
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
      extract_posterior_rvars_brms(
        brmsfit = estimation_result,
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
      results_list[[id_analysis]] <- create_error_result(
        id_iter, id_cond, id_analysis,
        "Extraction failed"
      )
      next
    }

    # Compute measures (using resolved probability thresholds)
    measures <- tryCatch({
      summarize_post_brms(
        posterior_rvars = posterior_rvars,
        target_params = target_params,
        thresh_scs = thresh_scs,
        thresh_ftl = thresh_ftl,
        p_sig_scs = current_p_sig_scs,
        p_sig_ftl = current_p_sig_ftl
      ) |>
        dplyr::mutate(dplyr::across(-par_name, as.numeric))
    }, error = function(e) {
      results_list[[id_analysis]] <- create_error_result(
        id_iter, id_cond, id_analysis,
        as.character(e)
      )
      return(NULL)
    })

    if (is.null(measures)) next

    # =============================================================================
    # STOPPING MECHANISM (Sequential Trial Early Stopping)
    # =============================================================================
    # Once stopped=TRUE, no further interim decisions are made, BUT analysis
    # continues to n_total to collect full data for safety/consistency metrics.
    # This design allows reporting: "would have stopped at n=100, final at n=200"
    #
    # Rationale: Complete data collection provides better understanding of trial
    # behavior even when stopping rule would have terminated early.
    #
    # Default stopping rule: dec_scs = 1 (success) or dec_ftl = 1 (futility)
    # Custom interim_function can override this with more complex logic.
    interim_decision <- NULL
    if (!is_final && !stopped) {
      if (!is.null(interim_function)) {
        # Custom stopping logic via interim_function
        interim_decision <- tryCatch({
          interim_function(
            interim_summaries = measures,
            current_n = current_n,
            analysis_at = current_n,
            n_total = n_total
          )
        }, error = function(e) {
          cli::cli_warn(c(
            "Interim function failed",
            "i" = "Error: {e$message}"
          ))
          list(decision = "continue", modified_params = NULL)
        })
      } else {
        # Default stopping rule: stop when dec_scs = 1 or dec_ftl = 1
        # Check the first target parameter (or union if multiple)
        dec_scs_val <- measures$dec_scs[1]
        dec_ftl_val <- measures$dec_ftl[1]

        if (!is.na(dec_scs_val) && dec_scs_val == 1) {
          interim_decision <- list(decision = "stop_success", modified_params = NULL)
        } else if (!is.na(dec_ftl_val) && dec_ftl_val == 1) {
          interim_decision <- list(decision = "stop_futility", modified_params = NULL)
        } else {
          interim_decision <- list(decision = "continue", modified_params = NULL)
        }
      }

      # Record stopping decision (loop continues but no more decisions made)
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
        id_look = id_analysis,
        n_analyzed = current_n,
        stopped = stopped,
        stop_reason = dplyr::if_else(stopped, stop_reason, NA_character_),
        interim_decision = if (!is.null(interim_decision)) list(interim_decision) else list(NULL),
        converged = 1L,
        error_msg = NA_character_
      )

    results_list[[id_analysis]] <- measures
  }

  # Combine all analyses
  dplyr::bind_rows(results_list)
}
