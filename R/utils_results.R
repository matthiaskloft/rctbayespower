# =============================================================================
# SHARED RESULT UTILITIES
# =============================================================================
# Functions used across multiple backend files for creating standardized
# result data structures.

# Quantile probabilities and column names for posterior profile storage
QUANTILE_PROBS <- c(0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975)
QUANTILE_COLS <- paste0("post_q", sprintf("%03d", QUANTILE_PROBS * 1000))

#' Create Error Result Row
#'
#' Helper function to create a standardized error result data frame when
#' estimation or extraction fails. Used by all backend estimation functions.
#'
#' @param id_iter Iteration identifier
#' @param id_cond Condition identifier
#' @param id_analysis Analysis identifier (0 for single, 1+ for sequential)
#' @param error_msg Error message describing the failure
#'
#' @return Data frame with NA values and error information, matching the
#'   standard output schema for power analysis results
#' @keywords internal
create_error_result <- function(id_iter, id_cond, id_analysis, error_msg) {
  base_cols <- list(
    par_name = NA_character_,
    thr_fx_eff = NA_real_,
    thr_fx_fut = NA_real_,
    thr_dec_eff = NA_real_,
    thr_dec_fut = NA_real_,
    pr_eff = NA_real_,
    pr_fut = NA_real_,
    dec_eff = NA_real_,
    dec_fut = NA_real_,
    post_med = NA_real_,
    post_mad = NA_real_,
    post_mn = NA_real_,
    post_sd = NA_real_
  )
  quantile_cols <- stats::setNames(
    as.list(rep(NA_real_, length(QUANTILE_COLS))), QUANTILE_COLS
  )
  tail_cols <- list(
    rhat = NA_real_,
    ess_bulk = NA_real_,
    ess_tail = NA_real_,
    id_iter = id_iter,
    id_cond = id_cond,
    id_look = id_analysis,
    n_analyzed = NA_integer_,
    calendar_time = NA_real_,
    n_enrolled = NA_integer_,
    enrollment_duration = NA_real_,
    stopped = NA,
    stop_reason = NA_character_,
    converged = 0L,
    error_msg = error_msg
  )
  do.call(data.frame, c(base_cols, quantile_cols, tail_cols,
                         list(stringsAsFactors = FALSE)))
}
