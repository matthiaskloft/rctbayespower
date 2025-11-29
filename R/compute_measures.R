#' Compute Power Measures from Posterior rvars (Backend-Agnostic)
#'
#' This function computes power analysis measures from posterior samples in rvar format.
#' It works with posteriors from any backend (brms, NPE, etc.) as long as they are
#' converted to rvar format first.
#'
#' @param posterior_rvars A draws_rvars object containing posterior samples for target parameters
#' @param target_params Character vector of parameter names to analyze
#' @param thresh_scs Numeric vector of success thresholds (ROPE, one per parameter)
#' @param thresh_ftl Numeric vector of futility thresholds (ROPE, one per parameter)
#' @param p_sig_scs Probability threshold for declaring success
#' @param p_sig_ftl Probability threshold for declaring futility
#'
#' @return A data frame containing power analysis measures
#' @importFrom stats median
#' @keywords internal
compute_measures <- function(posterior_rvars, target_params, thresh_scs,
                             thresh_ftl, p_sig_scs, p_sig_ftl) {

  # Compute measures for each parameter
  measures_list <- purrr::map(target_params, function(param) {
    # Extract rvar for this parameter
    param_rvar <- posterior_rvars[[param]]

    # extract thresholds
    if (length(target_params) > 1) {
      threshold_success <- thresh_scs[which(target_params == param)]
      # if the threshold is NA, use the first one
      if (is.na(threshold_success)) {
        threshold_success <- thresh_scs[1]
      }
      threshold_futility <- thresh_ftl[which(target_params == param)]
      # if the threshold is NA, use the first one
      if (is.na(threshold_futility)) {
        threshold_futility <- thresh_ftl[1]
      }
    } else {
      threshold_success <- thresh_scs
      threshold_futility <- thresh_ftl
    }
    # calculate the probability of success / futility
    success_prob <- posterior::Pr(param_rvar > threshold_success)
    futility_prob <- posterior::Pr(param_rvar < threshold_futility)
    # significance
    sig_success <- as.numeric(success_prob >= p_sig_scs, na.rm = TRUE)
    sig_futility <- as.numeric(futility_prob >= p_sig_ftl, na.rm = TRUE)
    # parameter estimates
    est_median <- stats::median(param_rvar)
    est_mad <- posterior::mad(param_rvar)
    est_mean <- mean(param_rvar)
    est_sd <- posterior::sd(param_rvar)
    # convergence metrics
    rhat <- posterior::rhat(param_rvar)
    ess_bulk <- posterior::ess_bulk(param_rvar)
    ess_tail <- posterior::ess_tail(param_rvar)

    # combine results into a list
    out_list <- list(
      par_name = param,
      thr_scs = threshold_success,
      thr_ftl = threshold_futility,
      p_sig_scs = p_sig_scs,
      p_sig_ftl = p_sig_ftl,
      pr_scs = success_prob,
      pr_ftl = futility_prob,
      dec_scs = sig_success,
      dec_ftl = sig_futility,
      post_med = est_median,
      post_mad = est_mad,
      post_mn = est_mean,
      post_sd = est_sd,
      rhat = rhat,
      ess_bulk = ess_bulk,
      ess_tail = ess_tail
    )

    return(out_list)
  })

  # compute combined probabilities and powers
  if (length(target_params) > 1) {
    # Convert rvars to matrix for combined calculations
    # Extract draws as matrix: each column is a parameter
    posterior_matrix_list <- lapply(target_params, function(param) {
      as.vector(posterior::draws_of(posterior_rvars[[param]]))
    })
    posterior_samples <- do.call(cbind, posterior_matrix_list)

    # extract thresholds and broadcast if necessary
    if (length(target_params) > length(thresh_scs)) {
      thresh_scs_combined <- rep(thresh_scs[1], length(target_params))
    } else {
      thresh_scs_combined <- thresh_scs
    }
    if (length(target_params) > length(thresh_ftl)) {
      thresh_ftl_combined <- rep(thresh_ftl[1], length(target_params))
    } else {
      thresh_ftl_combined <- thresh_ftl
    }

    # Combined probability algorithm (AND decision rule for multiple parameters)
    # All parameters must simultaneously exceed threshold (conjunctive logic)
    # Algorithm:
    #   1. Convert each parameter to binary: exceeds threshold? (1/0)
    #   2. Take minimum across parameters for each draw (AND operation)
    #   3. Average across draws to get probability
    # Example: P(param1 > 0.2 AND param2 > 0.3 AND param3 > 0.1)
    combined_success_prob <- mean(apply(ifelse(
      posterior_samples > thresh_scs_combined, 1, 0
    ), 1, min))
    combined_futility_prob <- mean(apply(
      ifelse(posterior_samples < thresh_ftl_combined, 1, 0),
      1,
      min
    ))

    # calculate combined significance
    combined_sig_success <- as.numeric(combined_success_prob >= p_sig_scs, na.rm = TRUE)
    combined_sig_futility <- as.numeric(combined_futility_prob >= p_sig_ftl, na.rm = TRUE)

    # combine results into a list
    measures_list_combined <- list(
      par_name = "union",
      thr_scs = NA,
      thr_ftl = NA,
      p_sig_scs = p_sig_scs,
      p_sig_ftl = p_sig_ftl,
      pr_scs = combined_success_prob,
      pr_ftl = combined_futility_prob,
      dec_scs = combined_sig_success,
      dec_ftl = combined_sig_futility,
      post_med = NA,
      post_mad = NA,
      post_mn = NA,
      post_sd = NA,
      rhat = NA,
      ess_bulk = NA,
      ess_tail = NA
    )
  }

  # remove success_samples and futility_samples from the list
  measures_list <- purrr::map(measures_list, function(x) {
    x$success_samples <- NULL
    x$futility_samples <- NULL
    return(x)
  })

  # make data.frames and rbind()
  measures_df <- do.call(rbind, measures_list)

  if (length(target_params) > 1) {
    # create a data frame for combined measures
    measures_df_combined <- do.call(cbind, measures_list_combined)
    # add combined measures
    measures_df <- as.data.frame(rbind(measures_df, measures_df_combined))
  } else {
    measures_df <- as.data.frame(measures_df)
  }

  return(measures_df)
}


#' Summarize Power Analysis Simulation Results
#'
#' This function aggregates raw simulation results across multiple runs to compute
#' summary statistics including power estimates, parameter estimates, convergence
#' metrics, and Monte Carlo standard errors.
#'
#' @param results_df_raw A data frame containing raw simulation results with columns:
#'   \itemize{
#'     \item `id_cond`: Condition identifier
#'     \item `par_name`: Parameter name
#'     \item `thr_scs`: Success threshold for the parameter
#'     \item `thr_ftl`: Futility threshold for the parameter
#'     \item `pr_scs`: Probability of success for each simulation
#'     \item `pr_ftl`: Probability of futility for each simulation
#'     \item `dec_scs`: Binary success decision indicator
#'     \item `dec_ftl`: Binary futility decision indicator
#'     \item `post_med`: Posterior median estimates
#'     \item `post_mad`: Posterior median absolute deviation
#'     \item `post_mn`: Posterior mean estimates
#'     \item `post_sd`: Posterior standard deviation
#'     \item `rhat`: R-hat convergence diagnostic
#'     \item `ess_bulk`: Bulk effective sample size
#'     \item `ess_tail`: Tail effective sample size
#'     \item `converged`: Convergence status indicator
#'     \item `error_msg`: Error messages (if any)
#'   }
#' @param n_sims Integer specifying the total number of simulations run
#'
#' @return A data frame with summarized results grouped by condition and parameter,
#'   containing mean estimates and Monte Carlo standard errors (MCSE) for all metrics:
#'   \itemize{
#'     \item Probability estimates: `pr_scs_mean`, `pr_ftl_mean` with `*_mcse`
#'     \item Power estimates: `pwr_scs_mean`, `pwr_ftl_mean` with `*_mcse`
#'     \item Parameter estimates: `post_med_mean`, `post_mn_mean`, `post_mad_mean`, `post_sd_mean`
#'     \item Convergence metrics: `rhat_mean`, `ess_bulk_mean`, `ess_tail_mean`, `conv_rate_mean`
#'   }
#'   Each metric includes corresponding `*_mcse` columns with Monte Carlo standard errors.
#'
#' @details
#' The function groups results by condition ID, parameter, and thresholds, then computes:
#' \itemize{
#'   \item Mean values across simulations for all continuous metrics
#'   \item Power as the proportion of simulations meeting significance criteria
#'   \item Convergence rate as the proportion of successfully converged simulations
#'   \item Monte Carlo standard errors for uncertainty quantification
#'   \item Concatenated error messages for debugging purposes
#' }
#'
#' @seealso [summarize_sims_with_interim()]
#' @keywords internal
summarize_sims <- function(results_df_raw, n_sims) {
  # Validate input
  if (!is.data.frame(results_df_raw) || nrow(results_df_raw) == 0) {
    cli::cli_abort(c(
      "{.arg results_df_raw} must be a non-empty data frame",
      "x" = "You supplied {.type {results_df_raw}} with {.val {nrow(results_df_raw)}} rows",
      "i" = "This is an internal error - please report"
    ))
  }

  # =============================================================================
  # INTERIM ANALYSIS DETECTION
  # =============================================================================
  # Detect if results contain interim analyses by checking:

  # 1. Required interim columns exist (id_look, n_analyzed, stopped, stop_reason)
  # 2. More than one unique analysis index exists (multiple looks per simulation)
  #
  # Single-look designs have id_look = 1 for all rows, so we only dispatch to
  # interim summarization when there are actually multiple analysis timepoints.
  has_interim_cols <- all(c("id_look", "n_analyzed", "stopped", "stop_reason") %in%
                            names(results_df_raw))
  has_multiple_analyses <- has_interim_cols &&
    length(unique(results_df_raw$id_look)) > 1

  if (has_multiple_analyses) {
    return(summarize_sims_with_interim(results_df_raw, n_sims))
  }

  # =============================================================================
  # STANDARD (SINGLE-LOOK) SUMMARIZATION
  # =============================================================================
  # Track rows before filtering (for error detection)
  n_total_rows <- nrow(results_df_raw)

  # remove rows with NA in id_cond or par_name (error results have NA par_name)
  results_df_raw <- results_df_raw |>
    dplyr::filter(!is.na(id_cond) & !is.na(par_name))

  # Warn if all rows were filtered (indicates all simulations failed)
  if (nrow(results_df_raw) == 0) {
    cli::cli_warn(c(
      "All {n_total_rows} simulation results were filtered out (likely all simulations failed)",
      "i" = "Check the raw results for error messages",
      "i" = "This may indicate a model or data compatibility issue"
    ))
    # Return empty data frame with expected columns
    return(data.frame(
      id_cond = integer(),
      par_name = character(),
      thr_scs = numeric(),
      thr_ftl = numeric(),
      p_sig_scs = numeric(),
      p_sig_ftl = numeric(),
      pwr_scs = numeric(),
      pwr_ftl = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  results_summarized <- results_df_raw |>
    dplyr::group_by(id_cond, par_name, thr_scs, thr_ftl, p_sig_scs, p_sig_ftl) |>
    dplyr::summarise(
      pr_scs_mean = mean(pr_scs, na.rm = TRUE),
      pr_scs_mcse = calculate_mcse_mean(pr_scs, n_sims),
      pr_ftl_mean = mean(pr_ftl, na.rm = TRUE),
      pr_ftl_mcse = calculate_mcse_mean(pr_ftl, n_sims),
      pwr_scs_mean = mean(dec_scs, na.rm = TRUE),
      pwr_scs_mcse = calculate_mcse_power(dec_scs, n_sims),
      pwr_ftl_mean = mean(dec_ftl, na.rm = TRUE),
      pwr_ftl_mcse = calculate_mcse_power(dec_ftl, n_sims),
      post_med_mean = mean(.data$post_med, na.rm = TRUE),
      post_med_mcse = calculate_mcse_mean(.data$post_med, n_sims),
      post_mad_mean = mean(.data$post_mad, na.rm = TRUE),
      post_mad_mcse = calculate_mcse_mean(.data$post_mad, n_sims),
      post_mn_mean = mean(.data$post_mn, na.rm = TRUE),
      post_mn_mcse = calculate_mcse_mean(.data$post_mn, n_sims),
      post_sd_mean = mean(.data$post_sd, na.rm = TRUE),
      post_sd_mcse = calculate_mcse_mean(.data$post_sd, n_sims),
      rhat_mean = mean(rhat, na.rm = TRUE),
      rhat_mcse = calculate_mcse_mean(rhat, n_sims),
      ess_bulk_mean = mean(ess_bulk, na.rm = TRUE),
      ess_bulk_mcse = calculate_mcse_mean(ess_bulk, n_sims),
      ess_tail_mean = mean(ess_tail, na.rm = TRUE),
      ess_tail_mcse = calculate_mcse_mean(ess_tail, n_sims),
      conv_rate_mean = mean(converged, na.rm = TRUE),
      conv_rate_mcse = calculate_mcse_power(converged, n_sims),
      .groups = "drop"
    ) |>
    # make shorter names
    dplyr::rename(
      pr_scs = pr_scs_mean,
      se_pr_scs = pr_scs_mcse,
      pr_ftl = pr_ftl_mean,
      se_pr_ftl = pr_ftl_mcse,
      pwr_scs = pwr_scs_mean,
      se_pwr_scs = pwr_scs_mcse,
      pwr_ftl = pwr_ftl_mean,
      se_pwr_ftl = pwr_ftl_mcse,
      post_med = post_med_mean,
      se_post_med = post_med_mcse,
      post_mad = post_mad_mean,
      se_post_mad = post_mad_mcse,
      post_mn = post_mn_mean,
      se_post_mn = post_mn_mcse,
      post_sd = post_sd_mean,
      se_post_sd = post_sd_mcse,
      rhat = rhat_mean,
      se_rhat = rhat_mcse,
      ess_bulk = ess_bulk_mean,
      se_ess_bulk = ess_bulk_mcse,
      ess_tail = ess_tail_mean,
      se_ess_tail = ess_tail_mcse,
      conv_rate = conv_rate_mean,
      se_conv_rate = conv_rate_mcse
    )

  return(results_summarized)
}


#' Summarize Power Analysis Results with Interim Analyses
#'
#' Aggregates raw simulation results for sequential designs with interim analyses.
#' Provides both per-look summaries and overall trial outcome statistics.
#'
#' @param results_df_raw A data frame containing raw simulation results including
#'   interim analysis columns (`id_look`, `n_analyzed`, `stopped`, `stop_reason`)
#' @param n_sims Integer specifying the total number of simulations run
#'
#' @return A list with two data frames:
#'   \describe{
#'     \item{by_look}{Summary statistics grouped by condition, parameter, and interim look}
#'     \item{overall}{Overall trial outcome statistics including stopping rates and sample size}
#'   }
#'
#' @details
#' This function is called automatically by [summarize_sims()] when interim analysis
#' results are detected. It computes:
#'
#' \strong{Per-Look Metrics (by_look):}
#' \itemize{
#'   \item Power metrics: `pr_scs`, `pr_ftl`, `pwr_scs`, `pwr_ftl` (with SEs)
#'   \item Posterior estimates: `post_med`, `post_mn`, `post_sd` (with SEs)
#'   \item Convergence: `rhat`, `ess_bulk`, `conv_rate`
#'   \item Stopping at this look: `prop_stp_look`, `prop_scs_look`, `prop_ftl_look`
#'   \item Cumulative stopping: `cumul_stp`
#' }
#'
#' \strong{Overall Metrics (overall):}
#' \itemize{
#'   \item `n_planned`: Maximum planned sample size
#'   \item `n_mn`, `se_n_mn`: Mean sample size with standard error
#'   \item `n_mdn`: Robust median (always an observed value, uses low median for ties)
#'   \item `n_mode`: Modal sample size (most frequent stopping point)
#'   \item `prop_at_mode`: Proportion of trials at modal N
#'   \item `prop_stp_early`: Proportion stopped before final look
#'   \item `prop_stp_scs`: Proportion stopped for success
#'   \item `prop_stp_ftl`: Proportion stopped for futility
#'   \item `prop_no_dec`: Proportion with no decision (= 1 - prop_stp_scs - prop_stp_ftl)
#' }
#'
#' @seealso [summarize_sims()]
#' @keywords internal
summarize_sims_with_interim <- function(results_df_raw, n_sims) {
  # Validate input
  if (!is.data.frame(results_df_raw) || nrow(results_df_raw) == 0) {
    cli::cli_abort(c(
      "{.arg results_df_raw} must be a non-empty data frame",
      "x" = "You supplied {.type {results_df_raw}} with {.val {nrow(results_df_raw)}} rows"
    ))
  }

  # Check required interim columns exist
 required_cols <- c("id_look", "n_analyzed", "stopped", "stop_reason")
  missing_cols <- setdiff(required_cols, names(results_df_raw))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required interim analysis columns",
      "x" = "Missing: {.val {missing_cols}}",
      "i" = "This function requires results from sequential estimation"
    ))
  }

  # Track rows before filtering (for error detection)
  n_total_rows <- nrow(results_df_raw)

  # Remove rows with NA in key identifiers (error results have NA par_name)
  results_df_raw <- results_df_raw |>
    dplyr::filter(!is.na(id_cond) & !is.na(par_name))

  # Warn if all rows were filtered (indicates all simulations failed)
  if (nrow(results_df_raw) == 0) {
    cli::cli_warn(c(
      "All {n_total_rows} simulation results were filtered out (likely all simulations failed)",
      "i" = "Check the raw results for error messages",
      "i" = "This may indicate a model or data compatibility issue"
    ))
    # Return list with empty data frames matching expected structure
    return(list(
      by_look = data.frame(
        id_cond = integer(),
        id_look = integer(),
        pwr_scs = numeric(),
        pwr_ftl = numeric(),
        stringsAsFactors = FALSE
      ),
      overall = data.frame(
        id_cond = integer(),
        n_planned = integer(),
        pwr_scs = numeric(),
        pwr_ftl = numeric(),
        stringsAsFactors = FALSE
      )
    ))
  }

  # =============================================================================
  # PER-LOOK SUMMARY
  # =============================================================================

  # First compute per-look stopping stats (condition × look level)
  # These will be joined to by_look (redundant across parameters but keeps it simple)
  n_total_sims <- results_df_raw |>
    dplyr::group_by(id_cond) |>
    dplyr::summarise(n_total_sims = dplyr::n_distinct(id_iter), .groups = "drop")

  # Count stops at each look
  stopping_by_look <- results_df_raw |>
    dplyr::filter(!is.na(stop_reason)) |>
    dplyr::group_by(id_cond, id_look, n_analyzed) |>
    dplyr::summarise(
      n_stp_look = dplyr::n_distinct(id_iter),
      n_scs_look = sum(stop_reason == "stop_success", na.rm = TRUE),
      n_ftl_look = sum(stop_reason == "stop_futility", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(n_total_sims, by = "id_cond") |>
    dplyr::mutate(
      prop_stp_look = n_stp_look / n_total_sims,
      prop_scs_look = n_scs_look / n_total_sims,
      prop_ftl_look = n_ftl_look / n_total_sims
    ) |>
    dplyr::group_by(id_cond) |>
    dplyr::arrange(id_look) |>
    dplyr::mutate(cumul_stp = cumsum(prop_stp_look)) |>
    dplyr::ungroup() |>
    dplyr::select(id_cond, id_look, prop_stp_look, prop_scs_look, prop_ftl_look, cumul_stp)

  # Group by condition, parameter, threshold, AND analysis look for power metrics
  by_look <- results_df_raw |>
    dplyr::group_by(id_cond, par_name, thr_scs, thr_ftl, p_sig_scs, p_sig_ftl, id_look, n_analyzed) |>
    dplyr::summarise(
      # Standard metrics
      pr_scs = mean(pr_scs, na.rm = TRUE),
      se_pr_scs = calculate_mcse_mean(pr_scs, n_sims),
      pr_ftl = mean(pr_ftl, na.rm = TRUE),
      se_pr_ftl = calculate_mcse_mean(pr_ftl, n_sims),
      pwr_scs = mean(dec_scs, na.rm = TRUE),
      se_pwr_scs = calculate_mcse_power(dec_scs, n_sims),
      pwr_ftl = mean(dec_ftl, na.rm = TRUE),
      se_pwr_ftl = calculate_mcse_power(dec_ftl, n_sims),
      post_med = mean(.data$post_med, na.rm = TRUE),
      se_post_med = calculate_mcse_mean(.data$post_med, n_sims),
      post_mn = mean(.data$post_mn, na.rm = TRUE),
      se_post_mn = calculate_mcse_mean(.data$post_mn, n_sims),
      post_sd = mean(.data$post_sd, na.rm = TRUE),
      se_post_sd = calculate_mcse_mean(.data$post_sd, n_sims),
      # Convergence
      rhat = mean(rhat, na.rm = TRUE),
      ess_bulk = mean(ess_bulk, na.rm = TRUE),
      conv_rate = mean(converged, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Join per-look stopping stats (redundant across parameters)
    dplyr::left_join(stopping_by_look, by = c("id_cond", "id_look")) |>
    # Fill NA stopping stats with 0 (looks where no trials stopped)
    dplyr::mutate(
      prop_stp_look = dplyr::if_else(is.na(prop_stp_look), 0, prop_stp_look),
      prop_scs_look = dplyr::if_else(is.na(prop_scs_look), 0, prop_scs_look),
      prop_ftl_look = dplyr::if_else(is.na(prop_ftl_look), 0, prop_ftl_look),
      cumul_stp = dplyr::if_else(is.na(cumul_stp), 0, cumul_stp)
    )

  # =============================================================================
  # OVERALL TRIAL SUMMARY (per condition)
  # =============================================================================
  # For overall metrics, we need to look at the final state of each simulation
  # Get final analysis for each simulation (last id_look per id_iter/id_cond)
  # Also need to track where stopping actually occurred

  # Get the analysis where stopping occurred (if any) for each simulation
  stopping_info <- results_df_raw |>
    dplyr::filter(!is.na(stop_reason)) |>
    dplyr::group_by(id_cond, id_iter) |>
    dplyr::slice_min(id_look, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(id_cond, id_iter, stop_n = n_analyzed, stop_reason)

  # Get all unique simulation runs
  all_sims <- results_df_raw |>
    dplyr::select(id_cond, id_iter) |>
    dplyr::distinct()

  # Join to get stopping info for each sim (NA if no stop)
  sim_outcomes <- all_sims |>
    dplyr::left_join(stopping_info, by = c("id_cond", "id_iter"))

  # Get planned n_total (max n_analyzed per condition)
  planned_n <- results_df_raw |>
    dplyr::group_by(id_cond) |>
    dplyr::summarise(n_planned = max(n_analyzed, na.rm = TRUE), .groups = "drop")

  sim_outcomes <- sim_outcomes |>
    dplyr::left_join(planned_n, by = "id_cond") |>
    dplyr::mutate(
      # If no stop, the effective N is n_planned
      effective_n = dplyr::if_else(is.na(stop_n), n_planned, stop_n),
      stopped_early = !is.na(stop_n)
    )


  # Helper to compute mode (most frequent value) and proportion at mode
  calc_mode <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    freq_table <- table(x)
    as.numeric(names(freq_table)[which.max(freq_table)])
  }

  calc_prop_at_mode <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    freq_table <- table(x)
    max(freq_table) / length(x)
  }

  # Robust median: returns actual observed value (low median for even n)
  # Unlike stats::median which averages two middle values, this always
  # returns a value that was actually observed in the data
  calc_robust_median <- function(x) {
    x <- sort(x[!is.na(x)])
    n <- length(x)
    if (n == 0) return(NA_real_)
    # For odd n: middle value; for even n: lower of two middle values
    x[(n + 1) %/% 2]
  }

  # Compute overall summaries by condition
  overall <- sim_outcomes |>
    dplyr::group_by(id_cond) |>
    dplyr::summarise(
      n_planned = dplyr::first(n_planned),
      n_mn = mean(effective_n, na.rm = TRUE),
      se_n_mn = stats::sd(effective_n, na.rm = TRUE) / sqrt(dplyr::n()),
      n_mdn = calc_robust_median(effective_n),
      n_mode = calc_mode(effective_n),
      prop_at_mode = calc_prop_at_mode(effective_n),
      prop_stp_early = mean(stopped_early, na.rm = TRUE),
      prop_stp_scs = mean(stop_reason == "stop_success", na.rm = TRUE),
      prop_stp_ftl = mean(stop_reason == "stop_futility", na.rm = TRUE),
      prop_no_dec = 1 - prop_stp_scs - prop_stp_ftl,
      .groups = "drop"
    )

  # Add power metrics from final look to overall
  final_look_id <- max(by_look$id_look)
  final_power <- by_look |>
    dplyr::filter(.data$id_look == final_look_id) |>
    dplyr::select("id_cond", "pwr_scs", "pwr_ftl", "se_pwr_scs", "se_pwr_ftl")

  # Merge and reorder columns logically

  # NOTE: We do NOT add n_total here because conditions_grid already has it.
  # Adding it here would cause duplicate columns (n_total.x, n_total.y) after

  # the join in power_analysis(), breaking column access. The n_total from
  # conditions_grid is the authoritative simulation parameter.
  overall <- overall |>
    dplyr::left_join(final_power, by = "id_cond") |>
    dplyr::select(
      "id_cond",
      # Sample size metrics (n_total comes from conditions_grid via join)
      "n_planned", "n_mn", "n_mdn", "n_mode", "se_n_mn", "prop_at_mode",
      # Power metrics
      "pwr_scs", "pwr_ftl", "se_pwr_scs", "se_pwr_ftl",
      # Stopping proportions
      "prop_stp_early", "prop_stp_scs", "prop_stp_ftl", "prop_no_dec"
    )

  return(list(
    by_look = by_look,
    overall = overall
  ))
}


# =============================================================================
# POST-HOC BOUNDARY RE-ANALYSIS
# =============================================================================

#' Re-summarize Results with Different Boundaries
#'
#' Apply different stopping boundaries to existing simulation results and return
#' a new power_analysis object with updated results. Useful for comparing boundary
#' configurations without re-running simulations.
#'
#' @param power_result An rctbp_power_analysis object with completed results
#' @param p_sig_scs New success boundary specification. Can be:
#'   \itemize{
#'     \item NULL to keep original thresholds
#'     \item A single numeric value (same threshold at all looks)
#'     \item A numeric vector (one threshold per look)
#'     \item A boundary function from [boundary_obf()], [boundary_linear()], etc.
#'   }
#' @param p_sig_ftl New futility boundary specification (same format as p_sig_scs)
#'
#' @return A new rctbp_power_analysis object with recomputed:
#'   \itemize{
#'     \item `results_raw`: Updated decisions and stopping based on new boundaries
#'     \item `results_interim`: Re-summarized by-look statistics
#'     \item `results_conditions`: Re-summarized overall statistics
#'   }
#'
#' @details
#' This function takes the raw posterior probabilities (`pr_scs`, `pr_ftl`) from
#' existing simulation results and recomputes the binary decisions (`dec_scs`,
#' `dec_ftl`) and stopping behavior using new probability thresholds.
#'
#' Since posterior probabilities are stored in results_raw, different threshold
#' configurations can be explored without re-running the computationally expensive
#' posterior estimation.
#'
#' @export
#' @seealso [compare_boundaries()], [boundary_obf()], [boundary_linear()]
#'
#' @examples
#' \dontrun{
#' # Run simulation once
#' result <- power_analysis(conditions, n_sims = 500, analysis_at = c(0.5, 0.75))
#'
#' # Re-analyze with O'Brien-Fleming-style boundaries
#' result_obf <- resummarize_boundaries(
#'   result,
#'   p_sig_scs = boundary_obf(0.975),
#'   p_sig_ftl = boundary_linear(0.70, 0.90)
#' )
#'
#' # Compare results
#' print(result)      # Original boundaries
#' print(result_obf)  # OBF boundaries
#' }
resummarize_boundaries <- function(power_result,
                                    p_sig_scs = NULL,
                                    p_sig_ftl = NULL) {

  # Validate input
 if (!inherits(power_result, "rctbayespower::rctbp_power_analysis") &&
      !inherits(power_result, "rctbp_power_analysis")) {
    cli::cli_abort(c(
      "{.arg power_result} must be an rctbp_power_analysis object",
      "x" = "Got object of class {.cls {class(power_result)}}"
    ))
  }

  results_raw <- power_result@results_raw
  n_sims <- power_result@n_sims

  # Check if this is a sequential design (has multiple looks)
  if (!"id_look" %in% names(results_raw) ||
      length(unique(results_raw$id_look)) <= 1) {
    cli::cli_abort(c(
      "Cannot re-analyze boundaries for single-look designs",
      "i" = "This function requires sequential designs with analysis_at specified"
    ))
  }

  # Get look structure for resolving functions
  look_info <- results_raw |>
    dplyr::select(id_look, n_analyzed) |>
    dplyr::distinct() |>
    dplyr::arrange(id_look)

  n_total <- max(look_info$n_analyzed)

  # Use original thresholds if not specified (now from conditions, not design)
  if (is.null(p_sig_scs)) {
    p_sig_scs <- get_original_threshold(power_result@conditions, "p_sig_scs")
  }
  if (is.null(p_sig_ftl)) {
    p_sig_ftl <- get_original_threshold(power_result@conditions, "p_sig_ftl")
  }

  # Resolve boundaries to per-look values
  scs_thresholds <- resolve_boundary_vector(p_sig_scs, look_info, n_total)
  ftl_thresholds <- resolve_boundary_vector(p_sig_ftl, look_info, n_total)

  # Create threshold lookup
  threshold_df <- look_info |>
    dplyr::mutate(
      p_sig_scs_new = scs_thresholds,
      p_sig_ftl_new = ftl_thresholds
    )

  # Recompute decisions with new thresholds
  results_recomputed <- results_raw |>
    dplyr::left_join(threshold_df, by = c("id_look", "n_analyzed")) |>
    dplyr::group_by(.data$id_cond, .data$id_iter, .data$par_name) |>
    dplyr::arrange(.data$id_look) |>
    dplyr::mutate(
      # Recompute binary decisions
      dec_scs = as.integer(.data$pr_scs >= .data$p_sig_scs_new),
      dec_ftl = as.integer(.data$pr_ftl >= .data$p_sig_ftl_new),
      # Update p_sig columns to show what was applied
      p_sig_scs = .data$p_sig_scs_new,
      p_sig_ftl = .data$p_sig_ftl_new,
      # Recompute stopping
      triggers_stop = (.data$dec_scs == 1) | (.data$dec_ftl == 1 & .data$dec_scs == 0),
      already_stopped = cumsum(dplyr::lag(.data$triggers_stop, default = FALSE)) > 0,
      is_stop_point = .data$triggers_stop & !.data$already_stopped,
      stopped = cumsum(.data$is_stop_point) > 0,
      stop_reason = dplyr::case_when(
        .data$is_stop_point & .data$dec_scs == 1 ~ "stop_success",
        .data$is_stop_point & .data$dec_ftl == 1 ~ "stop_futility",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"p_sig_scs_new", -"p_sig_ftl_new", -"triggers_stop",
                  -"already_stopped", -"is_stop_point")

  # Summarize with existing function
  summary_result <- summarize_sims_with_interim(results_recomputed, n_sims)

  # Create new power_analysis object with updated results
  new_result <- power_result
  new_result@results_raw <- results_recomputed
  new_result@results_interim <- summary_result$by_look
  new_result@results_conditions <- summary_result$overall

  new_result
}


#' Compare Multiple Boundary Configurations
#'
#' Evaluate the same simulation results under multiple stopping boundary
#' specifications. Returns a summary comparing operating characteristics
#' across all boundary configurations.
#'
#' @param power_result An rctbp_power_analysis object with completed results
#' @param boundaries Named list of boundary specifications. Each element should
#'   be a list with `success` and/or `futility` components specifying the
#'   boundary for that configuration. Components can be numeric values or
#'   boundary functions.
#'
#' @return A data frame with one row per boundary configuration per condition,
#'   containing operating characteristics:
#'   \itemize{
#'     \item `boundary`: Name of the boundary configuration
#'     \item `id_cond`: Condition identifier
#'     \item `n_planned`, `n_mn`, `n_mdn`: Sample size statistics
#'     \item `prop_stp_early`, `prop_stp_scs`, `prop_stp_ftl`: Stopping proportions
#'   }
#'
#' @export
#' @seealso [resummarize_boundaries()], [boundary_obf()], [boundary_linear()]
#'
#' @examples
#' \dontrun{
#' # Run simulation once
#' result <- power_analysis(conditions, n_sims = 500, analysis_at = c(0.5, 0.75))
#'
#' # Compare different boundary configurations
#' comparison <- compare_boundaries(result, list(
#'   "Fixed 0.975" = list(success = 0.975, futility = 0.90),
#'   "OBF-style" = list(success = boundary_obf(0.975), futility = 0.90),
#'   "Stringent" = list(success = 0.99, futility = 0.95),
#'   "Linear" = list(
#'     success = boundary_linear(0.999, 0.975),
#'     futility = boundary_linear(0.70, 0.90)
#'   )
#' ))
#'
#' print(comparison)
#' }
compare_boundaries <- function(power_result, boundaries) {

  # Validate input
  if (!is.list(boundaries) || length(boundaries) == 0) {
    cli::cli_abort(c(
      "{.arg boundaries} must be a non-empty named list",
      "i" = "Each element should have 'success' and/or 'futility' components"
    ))
  }

  if (is.null(names(boundaries)) || any(names(boundaries) == "")) {
    cli::cli_abort(c(
      "{.arg boundaries} must be a named list",
      "i" = "Provide names for each boundary configuration"
    ))
  }

  # Process each boundary configuration
  purrr::map_dfr(names(boundaries), function(name) {
    b <- boundaries[[name]]

    reanalyzed <- resummarize_boundaries(
      power_result,
      p_sig_scs = b$success,
      p_sig_ftl = b$futility
    )

    # Extract overall summary and add boundary name
    reanalyzed@results_conditions |>
      dplyr::mutate(boundary = name, .before = 1)
  })
}
