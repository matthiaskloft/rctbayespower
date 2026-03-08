# =============================================================================
# RESULT SUMMARIZATION
# =============================================================================
# Functions for aggregating simulation results from power analysis runs.
#
# Key functions:
#   - summarize_sims(): Aggregate single-look simulation results
#   - summarize_sims_with_interim(): Aggregate sequential analysis results
#   - resummarize_boundaries(): Re-analyze with different stopping boundaries
#   - compare_boundaries(): Compare multiple boundary configurations


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
#'     \item `thr_fx_eff`: Efficacy threshold for the parameter
#'     \item `thr_fx_fut`: Futility threshold for the parameter
#'     \item `pr_eff`: Probability of efficacy for each simulation
#'     \item `pr_fut`: Probability of futility for each simulation
#'     \item `dec_eff`: Binary efficacy decision indicator
#'     \item `dec_fut`: Binary futility decision indicator
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
#'     \item Probability estimates: `pr_eff_mean`, `pr_fut_mean` with `*_mcse`
#'     \item Power estimates: `pwr_eff_mean`, `pwr_fut_mean` with `*_mcse`
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

  # Work with base R data.frame to avoid data.table method dispatch issues
  # in special contexts (post-parallel, optimization callbacks)
  df <- as.data.frame(results_df_raw)

  # =============================================================================
  # INTERIM ANALYSIS DETECTION
  # =============================================================================
  has_interim_cols <- all(c("id_look", "n_analyzed", "stopped", "stop_reason") %in%
                            names(df))
  n_looks <- if (has_interim_cols) length(unique(df[["id_look"]])) else 1L
  has_multiple_analyses <- has_interim_cols && n_looks > 1

  if (has_multiple_analyses) {
    return(summarize_sims_with_interim(results_df_raw, n_sims))
  }

  # =============================================================================
  # STANDARD (SINGLE-LOOK) SUMMARIZATION
  # =============================================================================
  n_total_rows <- nrow(df)

  # Remove rows with NA in id_cond or par_name (error results have NA par_name)
  keep_idx <- which(!is.na(df[["id_cond"]]) & !is.na(df[["par_name"]]))
  df <- df[keep_idx, , drop = FALSE]

  # Warn if all rows were filtered (indicates all simulations failed)
  if (nrow(df) == 0) {
    cli::cli_warn(c(
      "All {n_total_rows} simulation results were filtered out (likely all simulations failed)",
      "i" = "Check the raw results for error messages",
      "i" = "This may indicate a model or data compatibility issue"
    ))
    return(data.frame(
      id_cond = integer(),
      par_name = character(),
      thr_fx_eff = numeric(),
      thr_fx_fut = numeric(),
      thr_dec_eff = numeric(),
      thr_dec_fut = numeric(),
      pwr_eff = numeric(),
      pwr_fut = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # Determine grouping columns
  by_cols <- intersect(
    c("id_cond", "par_name", "thr_fx_eff", "thr_fx_fut", "thr_dec_eff", "thr_dec_fut"),
    names(df)
  )

  # Safe column getter
  get_col <- function(data, col) {
    if (col %in% names(data)) data[[col]] else rep(NA_real_, nrow(data))
  }

  # Aggregation function for each group
  agg_fn <- function(grp) {
    base_agg <- data.frame(
      pr_eff = mean(get_col(grp, "pr_eff"), na.rm = TRUE),
      se_pr_eff = calculate_mcse_mean(get_col(grp, "pr_eff"), n_sims),
      pr_fut = mean(get_col(grp, "pr_fut"), na.rm = TRUE),
      se_pr_fut = calculate_mcse_mean(get_col(grp, "pr_fut"), n_sims),
      pwr_eff = mean(get_col(grp, "dec_eff"), na.rm = TRUE),
      se_pwr_eff = calculate_mcse_power(get_col(grp, "dec_eff"), n_sims),
      pwr_fut = mean(get_col(grp, "dec_fut"), na.rm = TRUE),
      se_pwr_fut = calculate_mcse_power(get_col(grp, "dec_fut"), n_sims),
      post_med = mean(get_col(grp, "post_med"), na.rm = TRUE),
      se_post_med = calculate_mcse_mean(get_col(grp, "post_med"), n_sims),
      post_mad = mean(get_col(grp, "post_mad"), na.rm = TRUE),
      se_post_mad = calculate_mcse_mean(get_col(grp, "post_mad"), n_sims),
      post_mn = mean(get_col(grp, "post_mn"), na.rm = TRUE),
      se_post_mn = calculate_mcse_mean(get_col(grp, "post_mn"), n_sims),
      post_sd = mean(get_col(grp, "post_sd"), na.rm = TRUE),
      se_post_sd = calculate_mcse_mean(get_col(grp, "post_sd"), n_sims),
      stringsAsFactors = FALSE
    )
    quantile_agg <- do.call(data.frame, c(
      unlist(lapply(QUANTILE_COLS, function(qc) {
        vals <- get_col(grp, qc)
        stats::setNames(list(
          mean(vals, na.rm = TRUE),
          calculate_mcse_mean(vals, n_sims)
        ), c(qc, paste0("se_", qc)))
      }), recursive = FALSE),
      list(stringsAsFactors = FALSE)
    ))
    tail_agg <- data.frame(
      rhat = mean(get_col(grp, "rhat"), na.rm = TRUE),
      se_rhat = calculate_mcse_mean(get_col(grp, "rhat"), n_sims),
      ess_bulk = mean(get_col(grp, "ess_bulk"), na.rm = TRUE),
      se_ess_bulk = calculate_mcse_mean(get_col(grp, "ess_bulk"), n_sims),
      ess_tail = mean(get_col(grp, "ess_tail"), na.rm = TRUE),
      se_ess_tail = calculate_mcse_mean(get_col(grp, "ess_tail"), n_sims),
      conv_rate = mean(get_col(grp, "converged"), na.rm = TRUE),
      se_conv_rate = calculate_mcse_power(get_col(grp, "converged"), n_sims),
      stringsAsFactors = FALSE
    )
    cbind(base_agg, quantile_agg, tail_agg)
  }

  # Split-apply-combine using base R
  # Create grouping key from by_cols
  group_key <- do.call(paste, c(df[by_cols], sep = "|||"))
  groups <- split(df, group_key, drop = TRUE)

  # Apply aggregation to each group
  results_list <- lapply(groups, agg_fn)

  # Bind results
  results <- do.call(rbind, results_list)
  rownames(results) <- NULL

  # Extract group keys back to columns
  first_rows <- lapply(groups, function(g) g[1, by_cols, drop = FALSE])
  key_df <- do.call(rbind, first_rows)
  rownames(key_df) <- NULL

  # Combine keys with results
  results <- cbind(key_df, results)

  as.data.frame(results)
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
#'   \item Power metrics: `pr_eff`, `pr_fut`, `pwr_eff`, `pwr_fut` (with SEs)
#'   \item Posterior estimates: `post_med`, `post_mn`, `post_sd` (with SEs)
#'   \item Convergence: `rhat`, `ess_bulk`, `conv_rate`
#'   \item Stopping at this look: `prop_stp_look`, `prop_eff_look`, `prop_fut_look`
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
#'   \item `prop_stp_eff`: Proportion stopped for efficacy
#'   \item `prop_stp_fut`: Proportion stopped for futility
#'   \item `prop_no_dec`: Proportion with no decision (= 1 - prop_stp_eff - prop_stp_fut)
#' }
#'
#' @seealso [summarize_sims()]
#' @importFrom stats ave
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

  # Work with base R data.frame to avoid data.table method dispatch issues
  # in special contexts (post-parallel, optimization callbacks)
  df <- as.data.frame(results_df_raw)

  # Remove rows with NA in key identifiers (error results have NA par_name)
  keep_idx <- which(!is.na(df[["id_cond"]]) & !is.na(df[["par_name"]]))
  df <- df[keep_idx, , drop = FALSE]

  # Warn if all rows were filtered (indicates all simulations failed)
  if (nrow(df) == 0) {
    cli::cli_warn(c(
      "All {n_total_rows} simulation results were filtered out (likely all simulations failed)",
      "i" = "Check the raw results for error messages",
      "i" = "This may indicate a model or data compatibility issue"
    ))
    return(list(
      by_look = data.frame(
        id_cond = integer(), id_look = integer(),
        pwr_eff = numeric(), pwr_fut = numeric(),
        stringsAsFactors = FALSE
      ),
      overall = data.frame(
        id_cond = integer(), n_planned = integer(),
        pwr_eff = numeric(), pwr_fut = numeric(),
        stringsAsFactors = FALSE
      )
    ))
  }

  # Safe column getter
  get_col <- function(data, col) {
    if (col %in% names(data)) data[[col]] else rep(NA_real_, nrow(data))
  }

  # Helper functions for aggregation
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

  calc_robust_median <- function(x) {
    x <- sort(x[!is.na(x)])
    n <- length(x)
    if (n == 0) return(NA_real_)
    x[(n + 1) %/% 2]
  }

  # =============================================================================
  # PER-LOOK SUMMARY (base R)
  # =============================================================================

  # Count total sims per condition
  n_total_sims_list <- tapply(df[["id_iter"]], df[["id_cond"]],
                               function(x) length(unique(x)), simplify = FALSE)
  n_total_sims_df <- data.frame(
    id_cond = as.integer(names(n_total_sims_list)),
    n_total_sims = unlist(n_total_sims_list),
    stringsAsFactors = FALSE
  )

  # Get rows where stopping occurred
  stopped_idx <- which(!is.na(df[["stop_reason"]]))
  stopped_df <- df[stopped_idx, , drop = FALSE]

  if (nrow(stopped_df) > 0) {
    # Compute stopping stats by condition and look
    stopping_by_cols <- intersect(c("id_cond", "id_look", "n_analyzed"), names(stopped_df))
    stopping_key <- do.call(paste, c(stopped_df[stopping_by_cols], sep = "|||"))
    stopping_groups <- split(stopped_df, stopping_key, drop = TRUE)

    stopping_list <- lapply(stopping_groups, function(grp) {
      data.frame(
        id_cond = grp$id_cond[1],
        id_look = grp$id_look[1],
        n_analyzed = grp$n_analyzed[1],
        n_stp_look = length(unique(grp$id_iter)),
        n_eff_look = sum(grp$stop_reason == "stop_efficacy", na.rm = TRUE),
        n_fut_look = sum(grp$stop_reason == "stop_futility", na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
    stopping_stats <- do.call(rbind, stopping_list)
    rownames(stopping_stats) <- NULL

    # Join with total sims and compute proportions
    stopping_stats <- merge(stopping_stats, n_total_sims_df, by = "id_cond", all.x = TRUE)
    stopping_stats$prop_stp_look <- stopping_stats$n_stp_look / stopping_stats$n_total_sims
    stopping_stats$prop_eff_look <- stopping_stats$n_eff_look / stopping_stats$n_total_sims
    stopping_stats$prop_fut_look <- stopping_stats$n_fut_look / stopping_stats$n_total_sims

    # Sort and compute cumulative stopping
    stopping_stats <- stopping_stats[order(stopping_stats$id_cond, stopping_stats$id_look), ]
    stopping_stats$cumul_stp <- ave(stopping_stats$prop_stp_look,
                                     stopping_stats$id_cond,
                                     FUN = cumsum)

    stopping_cols <- c("id_cond", "id_look", "prop_stp_look", "prop_eff_look",
                       "prop_fut_look", "cumul_stp")
    stopping_by_look <- stopping_stats[, stopping_cols, drop = FALSE]
  } else {
    stopping_by_look <- data.frame(
      id_cond = integer(), id_look = integer(),
      prop_stp_look = numeric(), prop_eff_look = numeric(),
      prop_fut_look = numeric(), cumul_stp = numeric(),
      stringsAsFactors = FALSE
    )
  }

  # =============================================================================
  # PER-LOOK POWER METRICS (base R)
  # =============================================================================
  by_look_by_cols <- intersect(
    c("id_cond", "par_name", "thr_fx_eff", "thr_fx_fut", "thr_dec_eff", "thr_dec_fut",
      "id_look", "n_analyzed"),
    names(df)
  )

  # Check once whether accrual data exists (consistent across all groups)
  has_accrual_data <- "calendar_time" %in% names(df) &&
    any(!is.na(df$calendar_time))

  by_look_key <- do.call(paste, c(df[by_look_by_cols], sep = "|||"))
  by_look_groups <- split(df, by_look_key, drop = TRUE)

  by_look_agg <- function(grp) {
    base_agg <- data.frame(
      pr_eff = mean(get_col(grp, "pr_eff"), na.rm = TRUE),
      se_pr_eff = calculate_mcse_mean(get_col(grp, "pr_eff"), n_sims),
      pr_fut = mean(get_col(grp, "pr_fut"), na.rm = TRUE),
      se_pr_fut = calculate_mcse_mean(get_col(grp, "pr_fut"), n_sims),
      pwr_eff = mean(get_col(grp, "dec_eff"), na.rm = TRUE),
      se_pwr_eff = calculate_mcse_power(get_col(grp, "dec_eff"), n_sims),
      pwr_fut = mean(get_col(grp, "dec_fut"), na.rm = TRUE),
      se_pwr_fut = calculate_mcse_power(get_col(grp, "dec_fut"), n_sims),
      post_med = mean(get_col(grp, "post_med"), na.rm = TRUE),
      se_post_med = calculate_mcse_mean(get_col(grp, "post_med"), n_sims),
      post_mn = mean(get_col(grp, "post_mn"), na.rm = TRUE),
      se_post_mn = calculate_mcse_mean(get_col(grp, "post_mn"), n_sims),
      post_sd = mean(get_col(grp, "post_sd"), na.rm = TRUE),
      se_post_sd = calculate_mcse_mean(get_col(grp, "post_sd"), n_sims),
      stringsAsFactors = FALSE
    )
    quantile_agg <- do.call(data.frame, c(
      unlist(lapply(QUANTILE_COLS, function(qc) {
        vals <- get_col(grp, qc)
        stats::setNames(list(
          mean(vals, na.rm = TRUE),
          calculate_mcse_mean(vals, n_sims)
        ), c(qc, paste0("se_", qc)))
      }), recursive = FALSE),
      list(stringsAsFactors = FALSE)
    ))
    tail_agg <- data.frame(
      rhat = mean(get_col(grp, "rhat"), na.rm = TRUE),
      ess_bulk = mean(get_col(grp, "ess_bulk"), na.rm = TRUE),
      conv_rate = mean(get_col(grp, "converged"), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    if (has_accrual_data) {
      accrual_agg <- data.frame(
        calendar_time_mn = mean(get_col(grp, "calendar_time"), na.rm = TRUE),
        calendar_time_mdn = stats::median(get_col(grp, "calendar_time"), na.rm = TRUE),
        n_enrolled_mn = mean(get_col(grp, "n_enrolled"), na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      cbind(base_agg, quantile_agg, tail_agg, accrual_agg)
    } else {
      cbind(base_agg, quantile_agg, tail_agg)
    }
  }

  by_look_list <- lapply(by_look_groups, by_look_agg)
  by_look_results <- do.call(rbind, by_look_list)
  rownames(by_look_results) <- NULL

  # Extract group keys
  by_look_keys <- lapply(by_look_groups, function(g) g[1, by_look_by_cols, drop = FALSE])
  by_look_keys_df <- do.call(rbind, by_look_keys)
  rownames(by_look_keys_df) <- NULL

  by_look_df <- cbind(by_look_keys_df, by_look_results)

  # Join per-look stopping stats
  by_look_df <- merge(by_look_df, stopping_by_look,
                      by = c("id_cond", "id_look"), all.x = TRUE)

  # Fill NA stopping stats with 0
  by_look_df$prop_stp_look[is.na(by_look_df$prop_stp_look)] <- 0
  by_look_df$prop_eff_look[is.na(by_look_df$prop_eff_look)] <- 0
  by_look_df$prop_fut_look[is.na(by_look_df$prop_fut_look)] <- 0
  by_look_df$cumul_stp[is.na(by_look_df$cumul_stp)] <- 0

  by_look <- by_look_df

  # =============================================================================
  # OVERALL TRIAL SUMMARY (base R)
  # =============================================================================
  # Get first stopping point per simulation (if any)
  overall_stopped_idx <- which(!is.na(df[["stop_reason"]]))
  overall_stopped_df <- df[overall_stopped_idx, , drop = FALSE]

  if (nrow(overall_stopped_df) > 0) {
    # Sort and get first stop per simulation
    overall_stopped_df <- overall_stopped_df[order(overall_stopped_df$id_cond,
                                                     overall_stopped_df$id_iter,
                                                     overall_stopped_df$id_look), ]
    sim_stop_key <- paste(overall_stopped_df$id_cond, overall_stopped_df$id_iter, sep = "|||")
    first_stop_idx <- !duplicated(sim_stop_key)
    first_stops <- overall_stopped_df[first_stop_idx, , drop = FALSE]

    stopping_info <- data.frame(
      id_cond = first_stops$id_cond,
      id_iter = first_stops$id_iter,
      stop_n = first_stops$n_analyzed,
      stop_reason = first_stops$stop_reason,
      stringsAsFactors = FALSE
    )
  } else {
    stopping_info <- data.frame(
      id_cond = integer(), id_iter = integer(),
      stop_n = integer(), stop_reason = character(),
      stringsAsFactors = FALSE
    )
  }

  # Get all unique simulation runs
  all_sims <- unique(df[, c("id_cond", "id_iter"), drop = FALSE])

  # Join to get stopping info for each sim (NA if no stop)
  sim_outcomes <- merge(all_sims, stopping_info, by = c("id_cond", "id_iter"), all.x = TRUE)

  # Get planned n_total (max n_analyzed per condition)
  planned_n_list <- tapply(df[["n_analyzed"]], df[["id_cond"]],
                            function(x) max(x, na.rm = TRUE), simplify = FALSE)
  planned_n_df <- data.frame(
    id_cond = as.integer(names(planned_n_list)),
    n_planned = unlist(planned_n_list),
    stringsAsFactors = FALSE
  )

  # Merge planned n with simulation outcomes
  sim_outcomes <- merge(sim_outcomes, planned_n_df, by = "id_cond", all.x = TRUE)

  # Compute effective_n and stopped_early
  sim_outcomes$effective_n <- ifelse(is.na(sim_outcomes$stop_n),
                                      sim_outcomes$n_planned,
                                      sim_outcomes$stop_n)
  sim_outcomes$stopped_early <- !is.na(sim_outcomes$stop_n)

  # Compute effective calendar time per simulation (accrual-aware)
  if (has_accrual_data) {
    # Deduplicate: one row per (id_cond, id_iter, id_look)
    dedup_key <- paste(df$id_cond, df$id_iter, df$id_look, sep = "|||")
    df_dedup <- df[!duplicated(dedup_key), , drop = FALSE]

    # Final-look calendar_time for ALL sims (fallback for non-stopped)
    final_look_id <- max(df_dedup$id_look)
    final_rows <- df_dedup[df_dedup$id_look == final_look_id, , drop = FALSE]
    cal_lookup <- data.frame(
      id_cond = final_rows$id_cond,
      id_iter = final_rows$id_iter,
      effective_calendar_time = final_rows$calendar_time,
      enrollment_duration = final_rows$enrollment_duration,
      stringsAsFactors = FALSE
    )

    # Override with stop-point calendar_time for stopped sims
    if (nrow(stopping_info) > 0 && "calendar_time" %in% names(first_stops)) {
      stop_cal <- data.frame(
        id_cond = first_stops$id_cond, id_iter = first_stops$id_iter,
        stop_calendar_time = first_stops$calendar_time,
        stringsAsFactors = FALSE
      )
      cal_lookup <- merge(cal_lookup, stop_cal,
                          by = c("id_cond", "id_iter"), all.x = TRUE)
      has_stop <- !is.na(cal_lookup$stop_calendar_time)
      cal_lookup$effective_calendar_time[has_stop] <- cal_lookup$stop_calendar_time[has_stop]
      cal_lookup$stop_calendar_time <- NULL
    }

    sim_outcomes <- merge(sim_outcomes, cal_lookup,
                          by = c("id_cond", "id_iter"), all.x = TRUE)
  }

  # Compute overall summaries by condition
  overall_groups <- split(sim_outcomes, sim_outcomes$id_cond, drop = TRUE)

  overall_list <- lapply(overall_groups, function(grp) {
    n_grp <- nrow(grp)
    base <- data.frame(
      n_planned = grp$n_planned[1],
      n_mn = mean(grp$effective_n, na.rm = TRUE),
      se_n_mn = stats::sd(grp$effective_n, na.rm = TRUE) / sqrt(n_grp),
      n_mdn = calc_robust_median(grp$effective_n),
      n_mode = calc_mode(grp$effective_n),
      prop_at_mode = calc_prop_at_mode(grp$effective_n),
      prop_stp_early = mean(grp$stopped_early, na.rm = TRUE),
      prop_stp_eff = sum(grp$stop_reason == "stop_efficacy", na.rm = TRUE) / n_grp,
      prop_stp_fut = sum(grp$stop_reason == "stop_futility", na.rm = TRUE) / n_grp,
      stringsAsFactors = FALSE
    )
    if (has_accrual_data) {
      accrual <- data.frame(
        trial_dur_mn = mean(grp$effective_calendar_time, na.rm = TRUE),
        trial_dur_mdn = calc_robust_median(grp$effective_calendar_time),
        enrollment_dur_mn = mean(grp$enrollment_duration, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      cbind(base, accrual)
    } else {
      base
    }
  })

  overall_results <- do.call(rbind, overall_list)
  overall_results$id_cond <- as.integer(names(overall_list))
  rownames(overall_results) <- NULL

  overall_df <- overall_results

  # Compute prop_no_dec
  overall_df$prop_no_dec <- 1 - overall_df$prop_stp_eff - overall_df$prop_stp_fut

  # Add power metrics from final look to overall
  final_look_id <- max(by_look_df$id_look)
  power_cols <- c("id_cond", "pwr_eff", "pwr_fut", "se_pwr_eff", "se_pwr_fut")
  power_cols <- intersect(power_cols, names(by_look_df))
  final_power_df <- by_look_df[by_look_df[["id_look"]] == final_look_id,
                                power_cols, drop = FALSE]

  # Merge and reorder columns logically
  overall_df <- merge(overall_df, final_power_df, by = "id_cond", all.x = TRUE)

  # Select and order columns (only those that exist)
  col_order <- c("id_cond", "n_planned", "n_mn", "n_mdn", "n_mode", "se_n_mn",
                 "prop_at_mode", "pwr_eff", "pwr_fut", "se_pwr_eff", "se_pwr_fut",
                 "prop_stp_early", "prop_stp_eff", "prop_stp_fut", "prop_no_dec")
  if ("trial_dur_mn" %in% names(overall_df)) {
    col_order <- c(col_order, "trial_dur_mn", "trial_dur_mdn", "enrollment_dur_mn")
  }
  col_order <- intersect(col_order, names(overall_df))
  overall <- overall_df[, col_order, drop = FALSE]

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
#' @param thr_dec_eff New efficacy boundary specification. Can be:
#'   \itemize{
#'     \item NULL to keep original thresholds
#'     \item A single numeric value (same threshold at all looks)
#'     \item A numeric vector (one threshold per look)
#'     \item A boundary function from [boundary_obf()], [boundary_linear()], etc.
#'   }
#' @param thr_dec_fut New futility boundary specification (same format as thr_dec_eff)
#'
#' @return A new rctbp_power_analysis object with recomputed:
#'   \itemize{
#'     \item `results_raw`: Updated decisions and stopping based on new boundaries
#'     \item `results_interim`: Re-summarized by-look statistics
#'     \item `results_conditions`: Re-summarized overall statistics
#'   }
#'
#' @details
#' This function takes the raw posterior probabilities (`pr_eff`, `pr_fut`) from
#' existing simulation results and recomputes the binary decisions (`dec_eff`,
#' `dec_fut`) and stopping behavior using new probability thresholds.
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
#'   thr_dec_eff = boundary_obf(0.975),
#'   thr_dec_fut = boundary_linear(0.70, 0.90)
#' )
#'
#' # Compare results
#' print(result)      # Original boundaries
#' print(result_obf)  # OBF boundaries
#' }
resummarize_boundaries <- function(power_result,
                                    thr_dec_eff = NULL,
                                    thr_dec_fut = NULL) {

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

  # Convert to data.table
  dt <- data.table::as.data.table(results_raw)

  # Get look structure for resolving functions
  look_cols <- intersect(c("id_look", "n_analyzed"), names(dt))
  if (length(look_cols) == 0) {
    cli::cli_abort(c(
      "Required columns for boundary analysis not found",
      "x" = "Need 'id_look' and 'n_analyzed' columns in results",
      "i" = "This function requires sequential analysis results with interim looks"
    ))
  }
  look_info_dt <- unique(dt[, look_cols, with = FALSE])
  data.table::setorderv(look_info_dt, "id_look")
  look_info <- as.data.frame(look_info_dt)

  n_total <- max(look_info$n_analyzed)

  # Use original thresholds if not specified (now from conditions, not design)
  if (is.null(thr_dec_eff)) {
    thr_dec_eff <- get_original_threshold(power_result@conditions, "thr_dec_eff")
  }
  if (is.null(thr_dec_fut)) {
    thr_dec_fut <- get_original_threshold(power_result@conditions, "thr_dec_fut")
  }

  # Resolve boundaries to per-look values
  eff_thresholds <- resolve_boundary_vector(thr_dec_eff, look_info, n_total)
  fut_thresholds <- resolve_boundary_vector(thr_dec_fut, look_info, n_total)

  # Create threshold lookup
  threshold_dt <- data.table::copy(look_info_dt)
  threshold_dt[, `:=`(
    thr_dec_eff_new = eff_thresholds,
    thr_dec_fut_new = fut_thresholds
  )]

  # Merge thresholds with results
  dt <- merge(dt, threshold_dt, by = c("id_look", "n_analyzed"), all.x = TRUE)

  # Sort for proper cumsum calculations within groups
  data.table::setorderv(dt, c("id_cond", "id_iter", "par_name", "id_look"))

  # Recompute decisions with new thresholds
  dt[, `:=`(
    # Recompute binary decisions
    dec_eff = as.integer(pr_eff >= thr_dec_eff_new),
    dec_fut = as.integer(pr_fut >= thr_dec_fut_new),
    # Update thr_dec columns to show what was applied
    thr_dec_eff = thr_dec_eff_new,
    thr_dec_fut = thr_dec_fut_new
  )]

  # Recompute stopping within each group
  group_by_cols <- intersect(c("id_cond", "id_iter", "par_name"), names(dt))
  dt[, `:=`(
    triggers_stop = (dec_eff == 1L) | (dec_fut == 1L & dec_eff == 0L)
  )]

  dt[, `:=`(
    already_stopped = cumsum(data.table::shift(triggers_stop, fill = FALSE)) > 0
  ), by = group_by_cols]

  dt[, `:=`(
    is_stop_point = triggers_stop & !already_stopped
  )]

  dt[, `:=`(
    stopped = cumsum(is_stop_point) > 0
  ), by = group_by_cols]

  dt[, stop_reason := data.table::fifelse(
    is_stop_point & dec_eff == 1L, "stop_efficacy",
    data.table::fifelse(is_stop_point & dec_fut == 1L, "stop_futility", NA_character_)
  )]

  # Remove temporary columns
  dt[, c("thr_dec_eff_new", "thr_dec_fut_new", "triggers_stop",
         "already_stopped", "is_stop_point") := NULL]

  results_recomputed <- as.data.frame(dt)

  # Summarize with existing function
  summary_result <- summarize_sims_with_interim(results_recomputed, n_sims)

  # Get condition grid columns from original results to preserve them
  # (e.g., n_total, b_arm_treat, analysis_at)
  orig_cond <- power_result@results_conditions
  grid_cols <- setdiff(names(orig_cond), names(summary_result$overall))
  if (length(grid_cols) > 0 && "id_cond" %in% names(orig_cond)) {
    grid_subset <- orig_cond[, c("id_cond", grid_cols), drop = FALSE]
    # Merge grid columns into overall results
    summary_result$overall <- merge(
      summary_result$overall,
      grid_subset,
      by = "id_cond",
      all.x = TRUE
    )
    # Also merge grid columns into by_look results (for plotting)
    summary_result$by_look <- merge(
      summary_result$by_look,
      grid_subset,
      by = "id_cond",
      all.x = TRUE
    )
  }

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
#'     \item `prop_stp_early`, `prop_stp_eff`, `prop_stp_fut`: Stopping proportions
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
  results_list <- lapply(names(boundaries), function(name) {
    b <- boundaries[[name]]

    reanalyzed <- resummarize_boundaries(
      power_result,
      thr_dec_eff = b$success,
      thr_dec_fut = b$futility
    )

    # Extract overall summary and add boundary name
    result_df <- reanalyzed@results_conditions
    result_df$boundary <- name
    result_df
  })

  # Bind all results and reorder columns
  result <- data.table::rbindlist(results_list, use.names = TRUE, fill = TRUE)
  # Move boundary column to first position
  data.table::setcolorder(result, c("boundary", setdiff(names(result), "boundary")))
  as.data.frame(result)
}
