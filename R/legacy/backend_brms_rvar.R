# =============================================================================
# LEGACY: RVAR-BASED BRMS POSTERIOR PROCESSING
# =============================================================================
# This file contains the original rvar-based posterior extraction and
# measure computation functions that were replaced with direct draws_array
# operations for better performance in December 2024.
#
# These functions are NOT loaded by the package - they are kept for reference
# and potential rollback if needed.
#
# Replacement functions (in R/backend_brms.R):
#   - extract_posterior_rvars_brms() -> extract_posterior_draws_brms()
#   - compute_measures() -> summarize_post_brms()
#
# Performance improvement: ~2-4x faster by avoiding rvar conversion overhead

# =============================================================================
# OLD: RVAR EXTRACTION
# =============================================================================

#' Extract Posterior as rvars (DEPRECATED)
#'
#' Original implementation that converted brms posteriors to rvar format.
#' Replaced by extract_posterior_draws_brms() which uses draws_array directly.
#'
#' @param brmsfit A fitted brmsfit object
#' @param target_params Character vector of parameter names to extract
#'
#' @return A draws_rvars object containing target parameters
#' @keywords internal
extract_posterior_rvars_brms <- function(brmsfit, target_params) {
  if (!inherits(brmsfit, "brmsfit")) {
    cli::cli_abort(c(
      "{.arg brmsfit} must be a fitted brms model object",
      "x" = "You supplied {.cls {class(brmsfit)}}",
      "i" = "Provide a fitted {.cls brmsfit} object"
    ))
  }

  posterior_rvars <- brms::as_draws_rvars(brmsfit, variable = target_params)
  return(posterior_rvars)
}


# =============================================================================
# OLD: COMPUTE MEASURES FROM RVARS
# =============================================================================

#' Compute Posterior Measures from rvars (DEPRECATED)
#'
#' Original implementation that computed posterior summaries using rvar operations.
#' Replaced by summarize_post_brms() which uses direct vector operations for
#' better performance.
#'
#' @param posterior_rvars A draws_rvars object containing target parameters
#' @param target_params Character vector of parameter names to process
#' @param thr_fx_eff Numeric vector of efficacy thresholds (ROPE boundaries)
#' @param thr_fx_fut Numeric vector of futility thresholds (ROPE boundaries)
#' @param thr_dec_eff Probability threshold for efficacy decision
#' @param thr_dec_fut Probability threshold for futility decision
#'
#' @return A data frame with one row per parameter (plus union if multi-param)
#'   containing posterior summaries and decisions
#' @keywords internal
compute_measures <- function(posterior_rvars, target_params,
                             thr_fx_eff, thr_fx_fut,
                             thr_dec_eff, thr_dec_fut) {

  # Input validation
  if (!inherits(posterior_rvars, "draws_rvars")) {
    cli::cli_abort(c(
      "{.arg posterior_rvars} must be a draws_rvars object",
      "x" = "Got {.cls {class(posterior_rvars)}}"
    ))
  }

  # Helper to compute measures for a single parameter
  compute_single_param <- function(param, eff_thr, fut_thr) {
    # Extract rvar for this parameter
    rvar_param <- posterior_rvars[[param]]

    # Compute probabilities using posterior package
    pr_eff <- posterior::Pr(rvar_param > eff_thr)
    pr_fut <- posterior::Pr(rvar_param < fut_thr)

    # Compute posterior summaries
    post_med <- stats::median(rvar_param)
    post_mad <- stats::mad(rvar_param)
    post_mn <- mean(rvar_param)
    post_sd <- stats::sd(rvar_param)

    # Convergence diagnostics
    draws_mat <- posterior::as_draws_matrix(posterior_rvars)
    rhat <- posterior::rhat(draws_mat[, param])
    ess_bulk <- posterior::ess_bulk(draws_mat[, param])
    ess_tail <- posterior::ess_tail(draws_mat[, param])

    data.frame(
      par_name = param,
      thr_fx_eff = eff_thr,
      thr_fx_fut = fut_thr,
      thr_dec_eff = thr_dec_eff,
      thr_dec_fut = thr_dec_fut,
      pr_eff = pr_eff,
      pr_fut = pr_fut,
      dec_eff = as.integer(pr_eff >= thr_dec_eff),
      dec_fut = as.integer(pr_fut >= thr_dec_fut),
      post_med = post_med,
      post_mad = post_mad,
      post_mn = post_mn,
      post_sd = post_sd,
      rhat = rhat,
      ess_bulk = ess_bulk,
      ess_tail = ess_tail,
      stringsAsFactors = FALSE
    )
  }

  # Process each parameter
  results_list <- lapply(seq_along(target_params), function(i) {
    param <- target_params[i]
    eff_thr <- if (length(thr_fx_eff) >= i) thr_fx_eff[i] else thr_fx_eff[1]
    fut_thr <- if (length(thr_fx_fut) >= i) thr_fx_fut[i] else thr_fx_fut[1]
    compute_single_param(param, eff_thr, fut_thr)
  })

  # Compute union if multiple parameters
  if (length(target_params) > 1) {
    # Extract all rvars
    rvars_list <- lapply(target_params, function(p) posterior_rvars[[p]])

    # Recycle thresholds
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

    # Compute joint probabilities using rvar operations
    # Union (AND logic): all parameters must exceed their thresholds
    exceeds_all_eff <- Reduce(`&`, lapply(seq_along(rvars_list), function(i) {
      rvars_list[[i]] > thr_eff[i]
    }))
    below_all_fut <- Reduce(`&`, lapply(seq_along(rvars_list), function(i) {
      rvars_list[[i]] < thr_fut[i]
    }))

    union_pr_eff <- posterior::Pr(exceeds_all_eff)
    union_pr_fut <- posterior::Pr(below_all_fut)

    union_row <- data.frame(
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
      rhat = NA_real_,
      ess_bulk = NA_real_,
      ess_tail = NA_real_,
      stringsAsFactors = FALSE
    )
    results_list <- c(results_list, list(union_row))
  }

  do.call(rbind, results_list)
}


# =============================================================================
# OLD: DPLYR-BASED SUMMARIZATION (from compute_measures.R)
# =============================================================================
# The following dplyr-based summarization code was also replaced with
# data.table implementations in R/compute_measures.R:
#
# - summarize_sims() - now uses data.table
# - summarize_sims_with_interim() - now uses data.table
# - resummarize_boundaries() - now uses data.table
# - compare_boundaries() - now uses lapply + data.table::rbindlist
#
# See git history for the original dplyr implementations.
