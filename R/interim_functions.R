# =============================================================================
# INTERIM DECISION FUNCTIONS
# =============================================================================
# Factory functions for creating interim decision functions used in sequential
# trial designs. These functions return decision functions with the standard
# interface expected by the estimation code.
#
# Standard Interface:
#   function(interim_summaries, current_n, analysis_at, n_total)
#   Returns: list(decision = "continue"|"stop_success"|"stop_futility",
#                 modified_params = NULL or list of modified parameters)

#' Default Interim Function: Always Continue
#'
#' Creates an interim decision function that always returns "continue".
#' This is the default used when `analysis_at` is specified but no
#' `interim_function` is provided. Useful for sequential monitoring
#' without early stopping rules.
#'
#' @return A function with the standard interim function interface that
#'   always returns `list(decision = "continue", modified_params = NULL)`
#'
#' @details
#' This function is used internally as the default when users specify
#' `analysis_at` without an `interim_function`. It enables sequential
#' analysis (multiple looks at the data) without any stopping rules.
#'
#' The returned function accepts the standard interim function parameters:
#' \itemize{
#'   \item `interim_summaries`: Data frame from `compute_measures()`
#'   \item `current_n`: Current sample size at this analysis
#'   \item `analysis_at`: The scheduled analysis timepoint
#'   \item `n_total`: Maximum planned sample size
#' }
#'
#' @export
#' @seealso [interim_futility_only()], [interim_success_futility()]
#'
#' @examples
#' # Create a design with sequential monitoring but no early stopping
#' \dontrun{
#' design <- build_design(
#'   model = my_model,
#'   target_params = "b_arm2",
#'   p_sig_scs = 0.975,
#'   p_sig_ftl = 0.5,
#'   analysis_at = c(50, 100, 150),
#'   interim_function = interim_continue()  # Or omit - this is the default
#' )
#' }
interim_continue <- function() {
  function(interim_summaries, current_n, analysis_at, n_total) {
    list(
      decision = "continue",
      modified_params = NULL
    )
  }
}


#' Interim Function Factory: Futility Stopping Only
#'
#' Creates an interim decision function that stops for futility only.
#' The trial stops early if the posterior probability of futility exceeds
#' the specified threshold.
#'
#' @param futility_threshold Numeric threshold (0-1) for stopping for futility.
#'   If the maximum `pr_ftl` across target parameters exceeds this threshold,
#'   the trial stops for futility. Default is 0.90.
#'
#' @return A function with the standard interim function interface
#'
#' @details
#' At each interim analysis, this function checks if any target parameter
#' has a posterior probability of futility (`pr_ftl`) exceeding the threshold.
#' If so, it returns "stop_futility"; otherwise "continue".
#'
#' This is useful for trials where you want to stop early if the treatment
#' is clearly ineffective, but continue to the planned sample size for
#' efficacy assessment.
#'
#' @export
#' @seealso [interim_continue()], [interim_success_futility()]
#'
#' @examples
#' \dontrun{
#' # Stop if P(futility) > 0.95
#' design <- build_design(
#'   model = my_model,
#'   target_params = "b_arm2",
#'   p_sig_scs = 0.975,
#'   p_sig_ftl = 0.5,
#'   analysis_at = c(50, 100),
#'   interim_function = interim_futility_only(futility_threshold = 0.95)
#' )
#' }
interim_futility_only <- function(futility_threshold = 0.90) {
  # Validate threshold
  if (!is.numeric(futility_threshold) || length(futility_threshold) != 1 ||
      futility_threshold < 0 || futility_threshold > 1) {
    cli::cli_abort(c(
      "{.arg futility_threshold} must be a single numeric value between 0 and 1",
      "x" = "You supplied {.val {futility_threshold}}"
    ))
  }

  function(interim_summaries, current_n, analysis_at, n_total) {
    # Check if any parameter exceeds futility threshold
    max_pr_ftl <- max(interim_summaries$pr_ftl, na.rm = TRUE)

    if (!is.na(max_pr_ftl) && max_pr_ftl >= futility_threshold) {
      return(list(
        decision = "stop_futility",
        modified_params = NULL
      ))
    }

    list(
      decision = "continue",
      modified_params = NULL
    )
  }
}


#' Interim Function Factory: Success and Futility Stopping
#'
#' Creates an interim decision function that can stop for either success
#' or futility. The trial stops early if the posterior probability of
#' success or futility exceeds the respective threshold.
#'
#' @param success_threshold Numeric threshold (0-1) for stopping for success.
#'   If the maximum `pr_scs` across target parameters exceeds this threshold,
#'   the trial stops for success. Default is 0.99.
#' @param futility_threshold Numeric threshold (0-1) for stopping for futility.
#'   If the maximum `pr_ftl` across target parameters exceeds this threshold,
#'   the trial stops for futility. Default is 0.90.
#'
#' @return A function with the standard interim function interface
#'
#' @details
#' At each interim analysis, this function first checks for success
#' (early efficacy), then for futility. Success takes precedence if both
#' thresholds are exceeded (unlikely but possible with different parameters).
#'
#' This is the most common interim analysis setup for confirmatory trials.
#'
#' @export
#' @seealso [interim_continue()], [interim_futility_only()]
#'
#' @examples
#' \dontrun{
#' # Stop for overwhelming success or clear futility
#' design <- build_design(
#'   model = my_model,
#'   target_params = "b_arm2",
#'   p_sig_scs = 0.975,
#'   p_sig_ftl = 0.5,
#'   analysis_at = c(50, 100),
#'   interim_function = interim_success_futility(
#'     success_threshold = 0.995,
#'     futility_threshold = 0.90
#'   )
#' )
#' }
interim_success_futility <- function(success_threshold = 0.99,
                                     futility_threshold = 0.90) {
  # Validate thresholds
  if (!is.numeric(success_threshold) || length(success_threshold) != 1 ||
      success_threshold < 0 || success_threshold > 1) {
    cli::cli_abort(c(
      "{.arg success_threshold} must be a single numeric value between 0 and 1",
      "x" = "You supplied {.val {success_threshold}}"
    ))
  }
  if (!is.numeric(futility_threshold) || length(futility_threshold) != 1 ||
      futility_threshold < 0 || futility_threshold > 1) {
    cli::cli_abort(c(
      "{.arg futility_threshold} must be a single numeric value between 0 and 1",
      "x" = "You supplied {.val {futility_threshold}}"
    ))
  }

  function(interim_summaries, current_n, analysis_at, n_total) {
    max_pr_scs <- max(interim_summaries$pr_scs, na.rm = TRUE)
    max_pr_ftl <- max(interim_summaries$pr_ftl, na.rm = TRUE)

    # Check for conflicting decisions (both success AND futility triggered)
    # This indicates misconfigured thresholds
    success_met <- !is.na(max_pr_scs) && max_pr_scs >= success_threshold
    futility_met <- !is.na(max_pr_ftl) && max_pr_ftl >= futility_threshold

    if (success_met && futility_met) {
      cli::cli_abort(c(
        "Conflicting stopping decisions: both success and futility criteria met",
        "x" = "At n = {current_n}: P(success) = {round(max_pr_scs, 3)} >= {success_threshold}, P(futility) = {round(max_pr_ftl, 3)} >= {futility_threshold}",
        "i" = "This indicates misconfigured decision thresholds. Consider:",
        "*" = "Widening the gap between 'thresh_scs' and 'thresh_ftl' in conditions",
        "*" = "Increasing 'p_sig_scs' and/or 'p_sig_ftl' to require stronger evidence",
        "*" = "Reviewing priors to ensure posteriors aren't unreasonably wide"
      ))
    }

    # Check success
    if (success_met) {
      return(list(
        decision = "stop_success",
        modified_params = NULL
      ))
    }

    # Check futility
    if (futility_met) {
      return(list(
        decision = "stop_futility",
        modified_params = NULL
      ))
    }

    list(
      decision = "continue",
      modified_params = NULL
    )
  }
}
