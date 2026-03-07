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
#   - extract_posterior_draws_brms(): Extract posterior as draws_array
#   - summarize_post_brms(): Fast posterior summarization (direct matrix ops)
#
# Output: All functions produce results compatible with summarize_sims()

# =============================================================================
# POSTERIOR EXTRACTION
# =============================================================================

#' Extract Posterior Draws from brms Model
#'
#' Extracts posterior samples from a fitted brms model as a draws_array.
#' This preserves the chain structure needed for convergence diagnostics
#' (Rhat, ESS) while allowing fast vectorized operations.
#'
#' @param brmsfit A fitted brmsfit object
#' @param target_params Character vector of parameter names to extract
#'
#' @return A draws_array object (iterations x chains x variables)
#' @keywords internal
extract_posterior_draws_brms <- function(brmsfit, target_params) {
  if (!inherits(brmsfit, "brmsfit")) {
    cli::cli_abort(c(
      "{.arg brmsfit} must be a fitted brms model object",
      "x" = "You supplied {.cls {class(brmsfit)}}",
      "i" = "Provide a fitted {.cls brmsfit} object"
    ))
  }

  # Use draws_array to preserve chain structure for Rhat/ESS calculations

  brms::as_draws_array(brmsfit, variable = target_params)
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
# POSTERIOR SUMMARIZATION (Fast Path)
# =============================================================================

#' Summarize Posterior - brms Backend (Single Parameter)
#'
#' Fast computation of posterior summaries using direct draws operations.
#' Avoids rvar conversion for better performance.
#'
#' @param draws_arr draws_array object from [extract_posterior_draws_brms()]
#' @param target_param Single parameter name (character)
#' @param thr_fx_eff Efficacy threshold (numeric)
#' @param thr_fx_fut Futility threshold (numeric)
#' @param thr_dec_eff Probability threshold for efficacy
#' @param thr_dec_fut Probability threshold for futility
#'
#' @return Data frame with one row containing posterior summaries
#' @keywords internal
summarize_post_brms_single <- function(draws_arr, target_param,
                                        thr_fx_eff, thr_fx_fut,
                                        thr_dec_eff, thr_dec_fut) {
  # Extract draws for this parameter as vector (iterations x chains flattened)
  # draws_array is [iterations, chains, variables]
  draws_vec <- as.numeric(draws_arr[, , target_param])

  # Probability calculations (simple mean over all draws)
  pr_eff <- mean(draws_vec > thr_fx_eff)
  pr_fut <- mean(draws_vec < thr_fx_fut)

  # Convergence diagnostics - needs draws object with chain structure
  # extract_variable_matrix() returns (iterations x chains) matrix
  draws_for_diag <- posterior::extract_variable_matrix(draws_arr, target_param)
  rhat <- posterior::rhat(draws_for_diag)
  ess_bulk <- posterior::ess_bulk(draws_for_diag)
  ess_tail <- posterior::ess_tail(draws_for_diag)

  # Compute quantile profile
  quantiles <- stats::quantile(draws_vec, probs = QUANTILE_PROBS, names = FALSE)

  base_df <- data.frame(
    par_name = target_param,
    thr_fx_eff = thr_fx_eff,
    thr_fx_fut = thr_fx_fut,
    thr_dec_eff = thr_dec_eff,
    thr_dec_fut = thr_dec_fut,
    pr_eff = pr_eff,
    pr_fut = pr_fut,
    dec_eff = as.integer(pr_eff >= thr_dec_eff),
    dec_fut = as.integer(pr_fut >= thr_dec_fut),
    post_med = stats::median(draws_vec),
    post_mad = stats::mad(draws_vec),
    post_mn = mean(draws_vec),
    post_sd = stats::sd(draws_vec),
    stringsAsFactors = FALSE
  )
  quantile_cols <- do.call(data.frame, stats::setNames(as.list(quantiles), QUANTILE_COLS))
  tail_df <- data.frame(
    rhat = rhat,
    ess_bulk = ess_bulk,
    ess_tail = ess_tail,
    stringsAsFactors = FALSE
  )
  cbind(base_df, quantile_cols, tail_df)
}


#' Summarize Posterior - brms Backend (Multiple Parameters with Union)
#'
#' Fast computation of posterior summaries for multiple parameters.
#' Includes union calculation (AND logic across all parameters).
#'
#' @param draws_arr draws_array object from [extract_posterior_draws_brms()]
#' @param target_params Character vector of parameter names
#' @param thr_fx_eff Numeric vector of efficacy thresholds (one per param, recycled if shorter)
#' @param thr_fx_fut Numeric vector of futility thresholds (one per param, recycled if shorter)
#' @param thr_dec_eff Probability threshold for efficacy
#' @param thr_dec_fut Probability threshold for futility
#'
#' @return Data frame with one row per parameter plus union row (if multi-param)
#' @keywords internal
summarize_post_brms <- function(draws_arr, target_params,
                                 thr_fx_eff, thr_fx_fut,
                                 thr_dec_eff, thr_dec_fut) {
  # Process each parameter
  results_list <- lapply(seq_along(target_params), function(i) {
    param <- target_params[i]
    eff_thr <- if (length(thr_fx_eff) >= i) thr_fx_eff[i] else thr_fx_eff[1]
    fut_thr <- if (length(thr_fx_fut) >= i) thr_fx_fut[i] else thr_fx_fut[1]

    summarize_post_brms_single(draws_arr, param, eff_thr, fut_thr,
                               thr_dec_eff, thr_dec_fut)
  })

  # Compute union (AND logic across all params) if multiple parameters
  if (length(target_params) > 1) {
    # Get all draws as matrix (n_draws x n_params)
    n_draws <- prod(dim(draws_arr)[1:2])  # iterations x chains
    all_draws <- matrix(NA_real_, nrow = n_draws, ncol = length(target_params))
    for (i in seq_along(target_params)) {
      all_draws[, i] <- as.numeric(draws_arr[, , target_params[i]])
    }

    # Recycle thresholds if needed
    thr_eff <- if (length(thr_fx_eff) < length(target_params)) {
      rep(thr_fx_eff[1], length(target_params))
    } else {
      thr_fx_eff[seq_along(target_params)]
    }
    thr_fut <- if (length(thr_fx_fut) < length(target_params)) {
      rep(thr_fx_fut[1], length(target_params))
    } else {
      thr_fx_fut[seq_along(target_params)]
    }

    # Compute joint probabilities (AND logic: all params must meet threshold)
    exceeds_eff <- rowMeans(sweep(all_draws, 2, thr_eff, `>`)) == 1
    below_fut <- rowMeans(sweep(all_draws, 2, thr_fut, `<`)) == 1
    union_pr_eff <- mean(exceeds_eff)
    union_pr_fut <- mean(below_fut)

    union_base <- data.frame(
      par_name = "union",
      thr_fx_eff = NA_real_,
      thr_fx_fut = NA_real_,
      thr_dec_eff = thr_dec_eff,
      thr_dec_fut = thr_dec_fut,
      pr_eff = union_pr_eff,
      pr_fut = union_pr_fut,
      dec_eff = as.integer(union_pr_eff >= thr_dec_eff),
      dec_fut = as.integer(union_pr_fut >= thr_dec_fut),
      post_med = NA_real_,
      post_mad = NA_real_,
      post_mn = NA_real_,
      post_sd = NA_real_,
      stringsAsFactors = FALSE
    )
    union_quantiles <- do.call(data.frame,
      stats::setNames(as.list(rep(NA_real_, length(QUANTILE_COLS))), QUANTILE_COLS)
    )
    union_tail <- data.frame(
      rhat = NA_real_,
      ess_bulk = NA_real_,
      ess_tail = NA_real_,
      stringsAsFactors = FALSE
    )
    union_row <- cbind(union_base, union_quantiles, union_tail)
    results_list <- c(results_list, list(union_row))
  }

  do.call(rbind, results_list)
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
#' @param thr_fx_eff Numeric vector of efficacy thresholds (ROPE)
#' @param thr_fx_fut Numeric vector of futility thresholds (ROPE)
#' @param thr_dec_eff Probability threshold for efficacy
#' @param thr_dec_fut Probability threshold for futility
#' @param id_iter Iteration identifier
#' @param id_cond Condition identifier
#'
#' @return Data frame with 1 row containing measures and IDs
#' @keywords internal
estimate_single_brms <- function(data, model, backend_args, target_params,
                                 thr_fx_eff, thr_fx_fut,
                                 thr_dec_eff, thr_dec_fut,
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

  # Extract posterior draws (as draws_array for chain structure)
  posterior_draws <- tryCatch({
    extract_posterior_draws_brms(
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

  if (is.null(posterior_draws)) {
    return(create_error_result(id_iter, id_cond, id_analysis = 0L, "Extraction failed"))
  }

  # Resolve probability thresholds (info_frac = 1 for single-look designs)
  current_thr_dec_eff <- resolve_threshold(thr_dec_eff, 1)
  current_thr_dec_fut <- resolve_threshold(thr_dec_fut, 1)

  # Compute measures using fast summarization path
  result <- tryCatch({
    df <- summarize_post_brms(
      draws_arr = posterior_draws,
      target_params = target_params,
      thr_fx_eff = thr_fx_eff,
      thr_fx_fut = thr_fx_fut,
      thr_dec_eff = current_thr_dec_eff,
      thr_dec_fut = current_thr_dec_fut
    )
    # Add IDs and metadata
    df$id_iter <- id_iter
    df$id_cond <- id_cond
    df$id_look <- 0L  # Single analysis
    df$converged <- 1L
    df$error_msg <- NA_character_
    df
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
#' @param thr_fx_eff Numeric vector of efficacy thresholds (ROPE)
#' @param thr_fx_fut Numeric vector of futility thresholds (ROPE)
#' @param thr_dec_eff Probability threshold for efficacy (numeric or pre-resolved vector)
#' @param thr_dec_fut Probability threshold for futility (numeric or pre-resolved vector)
#' @param analysis_at Vector of sample sizes for all analyses (including final at n_total)
#' @param interim_function Function to make interim decisions
#' @param id_iter Iteration identifier
#' @param id_cond Condition identifier
#'
#' @return Data frame with n_analyses rows (one per interim + final)
#' @importFrom dplyr if_else
#' @keywords internal
estimate_sequential_brms <- function(full_data, model, backend_args, target_params,
                                     thr_fx_eff, thr_fx_fut,
                                     thr_dec_eff, thr_dec_fut,
                                     analysis_at, interim_function,
                                     id_iter, id_cond,
                                     followup_time = 0) {

  n_total <- nrow(full_data)
  # Pre-compute completion times once (invariant across analysis points)
  completion_times <- if ("enrollment_time" %in% names(full_data)) {
    sort(full_data$enrollment_time + followup_time)
  }
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
    current_thr_dec_eff <- resolve_threshold(thr_dec_eff, info_frac)
    current_thr_dec_fut <- resolve_threshold(thr_dec_fut, info_frac)

    # Subset data to current analysis point (accrual-aware when enrollment_time present)
    analysis_data <- subset_analysis_data(full_data, current_n, followup_time,
                                           completion_times)

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

    # Extract posterior draws (as draws_array for chain structure)
    posterior_draws <- tryCatch({
      extract_posterior_draws_brms(
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

    if (is.null(posterior_draws)) {
      results_list[[id_analysis]] <- create_error_result(
        id_iter, id_cond, id_analysis,
        "Extraction failed"
      )
      next
    }

    # Compute measures using fast summarization path
    measures <- tryCatch({
      summarize_post_brms(
        draws_arr = posterior_draws,
        target_params = target_params,
        thr_fx_eff = thr_fx_eff,
        thr_fx_fut = thr_fx_fut,
        thr_dec_eff = current_thr_dec_eff,
        thr_dec_fut = current_thr_dec_fut
      )
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
    # Default stopping rule: dec_eff = 1 (efficacy) or dec_fut = 1 (futility)
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
        # Default stopping rule: stop when dec_eff = 1 or dec_fut = 1
        # Check the first target parameter (or union if multiple)
        dec_eff_val <- measures$dec_eff[1]
        dec_fut_val <- measures$dec_fut[1]

        if (!is.na(dec_eff_val) && dec_eff_val == 1) {
          interim_decision <- list(decision = "stop_efficacy", modified_params = NULL)
        } else if (!is.na(dec_fut_val) && dec_fut_val == 1) {
          interim_decision <- list(decision = "stop_futility", modified_params = NULL)
        } else {
          interim_decision <- list(decision = "continue", modified_params = NULL)
        }
      }

      # Record stopping decision (loop continues but no more decisions made)
      if (interim_decision$decision %in% c("stop_efficacy", "stop_futility")) {
        stopped <- TRUE
        stop_reason <- interim_decision$decision
      }
    }

    # Add IDs and interim information (base R assignment)
    measures$id_iter <- id_iter
    measures$id_cond <- id_cond
    measures$id_look <- id_analysis
    measures$n_analyzed <- current_n
    measures$stopped <- stopped
    measures$stop_reason <- if (stopped) stop_reason else NA_character_
    measures$interim_decision <- if (!is.null(interim_decision)) {
      rep(list(interim_decision), nrow(measures))
    } else {
      rep(list(NULL), nrow(measures))
    }
    measures$converged <- 1L
    measures$error_msg <- NA_character_

    results_list[[id_analysis]] <- measures
  }

  # Combine all analyses
  do.call(rbind, results_list)
}
