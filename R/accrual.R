# =============================================================================
# ACCRUAL UTILITIES
# =============================================================================

#' Generate Enrollment Times
#'
#' Creates a sorted vector of enrollment times for `n_total` patients.
#'
#' @param n_total Integer. Total number of patients to enroll.
#' @param accrual_rate Numeric. Patients per time unit.
#' @param accrual_pattern Character or function. One of `"uniform"` (constant
#'   inter-arrival), `"poisson"` (exponential inter-arrival), `"ramp"` (linearly
#'   increasing rate), or a custom function `f(n_total, accrual_rate)` returning
#'   a sorted numeric vector.
#'
#' @return Sorted numeric vector of length `n_total` (time 0 = trial start).
#' @keywords internal
generate_enrollment_times <- function(n_total, accrual_rate,
                                       accrual_pattern = "uniform") {
  if (is.function(accrual_pattern)) {
    times <- accrual_pattern(n_total, accrual_rate)
    if (length(times) != n_total) {
      cli::cli_abort(c(
        "Custom {.arg accrual_pattern} must return {n_total} values",
        "x" = "Got {length(times)} values"
      ))
    }
    return(sort(times))
  }

  times <- switch(accrual_pattern,
    "uniform" = {
      seq(0, by = 1 / accrual_rate, length.out = n_total)
    },
    "poisson" = {
      inter_arrival <- stats::rexp(n_total, rate = accrual_rate)
      cumsum(inter_arrival)
    },
    "ramp" = {
      # Linear ramp 0 -> 2*accrual_rate; inverse CDF: t = D * sqrt(u)
      u <- seq(0, 1, length.out = n_total + 1)[-(n_total + 1)]
      expected_duration <- n_total / accrual_rate
      expected_duration * sqrt(u)
    },
    cli::cli_abort(c(
      "Unknown {.arg accrual_pattern}: {.val {accrual_pattern}}",
      "i" = "Use {.val uniform}, {.val poisson}, {.val ramp}, or a custom function"
    ))
  )

  sort(times)
}


#' Identify Patients with Completed Follow-up
#'
#' Returns a logical mask of patients whose follow-up is complete at a given
#' calendar time. These patients have analyzable data. Patients who are enrolled
#' but still in follow-up return FALSE.
#'
#' @param enrollment_times Numeric vector of enrollment times.
#' @param followup_time Numeric. Required follow-up duration after enrollment.
#' @param calendar_time Numeric. Calendar time at which to check.
#'
#' @return Logical vector. TRUE = enrolled AND follow-up complete.
#' @keywords internal
patients_with_data <- function(enrollment_times, followup_time, calendar_time) {
  (enrollment_times + followup_time) <= calendar_time
}


#' Identify Enrolled Patients
#'
#' Returns a logical mask of patients enrolled at a given calendar time,
#' regardless of whether follow-up is complete.
#'
#' @param enrollment_times Numeric vector of enrollment times.
#' @param calendar_time Numeric. Calendar time at which to check.
#'
#' @return Logical vector. TRUE = enrolled (may or may not have completed follow-up).
#' @keywords internal
patients_enrolled <- function(enrollment_times, calendar_time) {
  enrollment_times <= calendar_time
}


#' Convert Calendar Times to Available Sample Sizes
#'
#' For each calendar time, computes the number of analyzable patients
#' (completed follow-up), enrolled patients, and those still in follow-up.
#'
#' @param calendar_times Numeric vector of calendar times.
#' @param enrollment_times Numeric vector of enrollment times (sorted).
#' @param followup_time Numeric. Required follow-up duration.
#'
#' @return Data frame with columns `calendar_time`, `n_analyzable`,
#'   `n_enrolled`, `n_in_followup`.
#' @keywords internal
calendar_to_available_n <- function(calendar_times, enrollment_times,
                                     followup_time) {
  n_analyzable <- vapply(calendar_times, function(ct) {
    sum(patients_with_data(enrollment_times, followup_time, ct))
  }, integer(1))

  n_enrolled <- vapply(calendar_times, function(ct) {
    sum(patients_enrolled(enrollment_times, ct))
  }, integer(1))

  data.frame(
    calendar_time = calendar_times,
    n_analyzable = n_analyzable,
    n_enrolled = n_enrolled,
    n_in_followup = n_enrolled - n_analyzable
  )
}


#' Subset Data to an Analysis Point (Accrual-Aware)
#'
#' Returns the rows of `full_data` that are analyzable at the point when
#' `current_n` patients have completed follow-up. When `full_data` contains
#' an `enrollment_time` column, subsetting is calendar-time-aware; otherwise
#' it falls back to simple row-index subsetting (`full_data[1:current_n, ]`).
#'
#' @param full_data Data frame (possibly with `enrollment_time` column).
#' @param current_n Integer. Target number of analyzable patients.
#' @param followup_time Numeric. Required follow-up per patient.
#' @param completion_times Optional pre-sorted vector of completion times
#'   (`enrollment_time + followup_time`). Pass this when calling inside a loop
#'   to avoid redundant sorting.
#'
#' @return Data frame with approximately `current_n` rows and `enrollment_time`
#'   column removed.
#' @keywords internal
subset_analysis_data <- function(full_data, current_n, followup_time = 0,
                                  completion_times = NULL) {
  if ("enrollment_time" %in% names(full_data)) {
    if (is.null(completion_times)) {
      completion_times <- sort(full_data$enrollment_time + followup_time)
    }
    calendar_time <- completion_times[current_n]
    mask <- patients_with_data(full_data$enrollment_time, followup_time,
                               calendar_time)
    analysis_data <- full_data[mask, , drop = FALSE]
    analysis_data$enrollment_time <- NULL
    analysis_data
  } else {
    full_data[1:current_n, ]
  }
}


#' Validate Accrual Parameters
#'
#' Validates accrual-related parameters. Called during condition building.
#'
#' @param accrual_rate Numeric or NULL.
#' @param accrual_pattern Character, function, or NULL.
#' @param followup_time Numeric or NULL.
#' @param analysis_timing Character or NULL.
#' @param calendar_analysis_at Numeric vector or NULL.
#'
#' @return TRUE invisibly if valid; aborts on invalid input.
#' @keywords internal
validate_accrual_params <- function(accrual_rate = NULL,
                                     accrual_pattern = NULL,
                                     followup_time = NULL,
                                     analysis_timing = NULL,
                                     calendar_analysis_at = NULL) {
  if (!is.null(accrual_rate)) {
    if (!is.numeric(accrual_rate) || length(accrual_rate) != 1 ||
        accrual_rate <= 0) {
      cli::cli_abort(c(
        "{.arg accrual_rate} must be a positive number",
        "x" = "Got {.val {accrual_rate}}"
      ))
    }
  }

  if (!is.null(accrual_pattern)) {
    valid_patterns <- c("uniform", "poisson", "ramp")
    if (!is.function(accrual_pattern) && !accrual_pattern %in% valid_patterns) {
      cli::cli_abort(c(
        "{.arg accrual_pattern} must be a function or one of: {.val {valid_patterns}}",
        "x" = "Got {.val {accrual_pattern}}"
      ))
    }
  }

  if (!is.null(followup_time)) {
    if (!is.numeric(followup_time) || length(followup_time) != 1 ||
        followup_time < 0) {
      cli::cli_abort(c(
        "{.arg followup_time} must be a non-negative number",
        "x" = "Got {.val {followup_time}}"
      ))
    }
  }

  if (!is.null(analysis_timing)) {
    valid_timings <- c("sample_size", "calendar")
    if (!analysis_timing %in% valid_timings) {
      cli::cli_abort(c(
        "{.arg analysis_timing} must be one of: {.val {valid_timings}}",
        "x" = "Got {.val {analysis_timing}}"
      ))
    }
  }

  if (!is.null(calendar_analysis_at)) {
    if (!is.numeric(calendar_analysis_at) || any(calendar_analysis_at <= 0)) {
      cli::cli_abort(c(
        "{.arg calendar_analysis_at} must be positive numbers",
        "x" = "Got {.val {calendar_analysis_at}}"
      ))
    }
    if (is.unsorted(calendar_analysis_at, strictly = TRUE)) {
      cli::cli_abort(c(
        "{.arg calendar_analysis_at} must be strictly increasing",
        "x" = "Got {.val {calendar_analysis_at}}"
      ))
    }
  }

  if (identical(analysis_timing, "calendar")) {
    if (is.null(accrual_rate)) {
      cli::cli_abort(c(
        "{.arg accrual_rate} is required when {.code analysis_timing = \"calendar\"}",
        "i" = "Specify patients per time unit"
      ))
    }
    if (is.null(calendar_analysis_at)) {
      cli::cli_abort(c(
        "{.arg calendar_analysis_at} is required when {.code analysis_timing = \"calendar\"}",
        "i" = "Specify calendar times for analyses (e.g., {.code c(12, 18, 24)})"
      ))
    }
  }

  invisible(TRUE)
}
