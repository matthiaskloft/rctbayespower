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
#'   a numeric vector of length `n_total` (sorted internally).
#'
#' @return Sorted numeric vector of length `n_total` (time 0 = trial start).
#' @keywords internal
generate_enrollment_times <- function(n_total, accrual_rate,
                                       accrual_pattern = "uniform") {
  if (is.function(accrual_pattern)) {
    times <- accrual_pattern(n_total, accrual_rate)
    if (!is.numeric(times)) {
      cli::cli_abort(c(
        "Custom {.arg accrual_pattern} must return a numeric vector",
        "x" = "Got {.cls {class(times)}}"
      ))
    }
    if (length(times) != n_total) {
      cli::cli_abort(c(
        "Custom {.arg accrual_pattern} must return {n_total} values",
        "x" = "Got {length(times)} values"
      ))
    }
    if (anyNA(times)) {
      cli::cli_abort(c(
        "Custom {.arg accrual_pattern} must not return NA values",
        "x" = "Got {sum(is.na(times))} NA value{?s}"
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


#' Generate Dropout Times
#'
#' Generates random dropout times for each patient using an exponential
#' (constant hazard) model. A patient drops out if their dropout time
#' is less than the required follow-up time.
#'
#' @param n_total Integer. Total number of patients.
#' @param hazard_rate Numeric. Exponential hazard rate for dropout.
#'
#' @return Numeric vector of length `n_total` with dropout times (time after
#'   enrollment).
#' @keywords internal
generate_dropout_times <- function(n_total, hazard_rate) {
  stats::rexp(n_total, rate = hazard_rate)
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
#' @param analysis_timing Character. How `current_n` is interpreted:
#'   `"sample_size"` (default) or `"events"` (event-driven subsetting).
#'
#' @return Data frame with approximately `current_n` rows and `enrollment_time`
#'   column removed.
#' @keywords internal
subset_analysis_data <- function(full_data, current_n, followup_time = 0,
                                  completion_times = NULL,
                                  analysis_timing = "sample_size") {
  # Event-driven subsetting: find the calendar time when current_n events

  # have accumulated, then return all patients enrolled by that time.
  if (identical(analysis_timing, "events")) {
    return(subset_by_events(full_data, target_events = current_n,
                             followup_time = followup_time))
  }

  if ("enrollment_time" %in% names(full_data)) {
    has_dropout <- "dropout_time" %in% names(full_data)

    if (has_dropout) {
      # Dropout-aware: only patients who didn't drop out have completion times
      completed_mask <- full_data$dropout_time >= followup_time
      completed_times <- sort(full_data$enrollment_time[completed_mask] + followup_time)
      n_completers <- length(completed_times)
      target_not_met <- current_n > n_completers

      if (n_completers == 0L) {
        analysis_data <- full_data[integer(0), , drop = FALSE]
        analysis_data$enrollment_time <- NULL
        analysis_data$dropout_time <- NULL
        attr(analysis_data, "calendar_time") <- NA_real_
        attr(analysis_data, "n_enrolled") <- as.integer(nrow(full_data))
        attr(analysis_data, "n_dropped") <- as.integer(sum(!completed_mask))
        attr(analysis_data, "target_not_met") <- TRUE
        return(analysis_data)
      }

      effective_n <- if (target_not_met) n_completers else current_n
      calendar_time <- completed_times[effective_n]

      # Patients with data at this calendar time AND not dropped out
      time_mask <- patients_with_data(full_data$enrollment_time, followup_time,
                                       calendar_time)
      mask <- time_mask & completed_mask
      n_enrolled <- as.integer(sum(patients_enrolled(full_data$enrollment_time, calendar_time)))
      n_dropped <- as.integer(sum(time_mask & !completed_mask))

      analysis_data <- full_data[mask, , drop = FALSE]
      analysis_data$enrollment_time <- NULL
      analysis_data$dropout_time <- NULL
      attr(analysis_data, "calendar_time") <- calendar_time
      attr(analysis_data, "n_enrolled") <- n_enrolled
      attr(analysis_data, "n_dropped") <- n_dropped
      if (target_not_met) attr(analysis_data, "target_not_met") <- TRUE
      analysis_data
    } else {
      # No dropout: original logic
      if (is.null(completion_times)) {
        completion_times <- sort(full_data$enrollment_time + followup_time)
      }
      if (current_n > length(completion_times)) {
        cli::cli_abort(c(
          "Requested {current_n} analyzable patients but only {length(completion_times)} exist",
          "i" = "Check that {.arg analysis_at} values do not exceed {.arg n_total}"
        ))
      }
      calendar_time <- completion_times[current_n]
      mask <- patients_with_data(full_data$enrollment_time, followup_time,
                                  calendar_time)
      n_enrolled <- as.integer(sum(patients_enrolled(full_data$enrollment_time, calendar_time)))
      analysis_data <- full_data[mask, , drop = FALSE]
      analysis_data$enrollment_time <- NULL
      attr(analysis_data, "calendar_time") <- calendar_time
      attr(analysis_data, "n_enrolled") <- n_enrolled
      analysis_data
    }
  } else {
    full_data[1:current_n, ]
  }
}


#' Subset Data by Event Count (Event-Driven Designs)
#'
#' For survival/event-driven designs, finds the calendar time at which
#' `target_events` events have occurred and returns all patients enrolled
#' by that time.
#'
#' @param full_data Data frame with `enrollment_time`, `time` (event/censor time),
#'   and `censored` (0 = event, 1 = censored) columns.
#' @param target_events Number of events required for this analysis look.
#' @param followup_time Minimum follow-up time (currently unused for event-driven;
#'   events determine the analysis time).
#'
#' @return Subset of `full_data` with accrual attributes attached.
#' @keywords internal
subset_by_events <- function(full_data, target_events, followup_time = 0) {
  if (!all(c("enrollment_time", "time", "censored") %in% names(full_data))) {
    cli::cli_abort(c(
      "Event-driven subsetting requires columns: {.val enrollment_time}, {.val time}, {.val censored}",
      "i" = "Ensure your sim_fn returns survival data with these columns"
    ))
  }

  # Calendar time of each event = enrollment_time + event_time (for events only)
  event_mask <- full_data$censored == 0  # 0 = event in brms convention
  event_calendar_times <- sort(
    full_data$enrollment_time[event_mask] + full_data$time[event_mask]
  )

  n_total_events <- length(event_calendar_times)
  target_not_met <- target_events > n_total_events

  if (n_total_events == 0L) {
    analysis_data <- full_data[integer(0), , drop = FALSE]
    analysis_data$enrollment_time <- NULL
    attr(analysis_data, "calendar_time") <- NA_real_
    attr(analysis_data, "n_enrolled") <- as.integer(nrow(full_data))
    attr(analysis_data, "n_events") <- 0L
    attr(analysis_data, "target_not_met") <- TRUE
    return(analysis_data)
  }

  effective_events <- if (target_not_met) n_total_events else target_events
  calendar_time <- event_calendar_times[effective_events]

  # All patients enrolled by this calendar time
  enrolled_mask <- full_data$enrollment_time <= calendar_time
  analysis_data <- full_data[enrolled_mask, , drop = FALSE]

  # Update censoring: patients whose event hasn't occurred by calendar_time
  # are censored at (calendar_time - enrollment_time).
  # Small tolerance avoids floating-point rounding at event boundaries.
  follow_time <- calendar_time - analysis_data$enrollment_time
  still_censored <- analysis_data$time > follow_time + 1e-10
  analysis_data$time[still_censored] <- follow_time[still_censored]
  analysis_data$censored[still_censored] <- 1L

  n_enrolled <- as.integer(sum(enrolled_mask))
  n_events <- as.integer(sum(analysis_data$censored == 0))

  analysis_data$enrollment_time <- NULL
  attr(analysis_data, "calendar_time") <- calendar_time
  attr(analysis_data, "n_enrolled") <- n_enrolled
  attr(analysis_data, "n_events") <- n_events
  if (target_not_met) attr(analysis_data, "target_not_met") <- TRUE
  analysis_data
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
#' @param dropout rctbp_dropout object or NULL.
#'
#' @return TRUE invisibly if valid; aborts on invalid input.
#' @keywords internal
validate_accrual_params <- function(accrual_rate = NULL,
                                     accrual_pattern = NULL,
                                     followup_time = NULL,
                                     analysis_timing = NULL,
                                     calendar_analysis_at = NULL,
                                     dropout = NULL) {
  if (!is.null(accrual_rate)) {
    if (!is.numeric(accrual_rate) || length(accrual_rate) != 1L ||
        is.na(accrual_rate) || accrual_rate <= 0) {
      cli::cli_abort(c(
        "{.arg accrual_rate} must be a positive number",
        "x" = "Got {.val {accrual_rate}}"
      ))
    }
  }

  if (!is.null(accrual_pattern)) {
    valid_patterns <- c("uniform", "poisson", "ramp")
    if (!is.function(accrual_pattern) &&
        (!is.character(accrual_pattern) || length(accrual_pattern) != 1L ||
         is.na(accrual_pattern) || !accrual_pattern %in% valid_patterns)) {
      cli::cli_abort(c(
        "{.arg accrual_pattern} must be a function or one of: {.val {valid_patterns}}",
        "x" = "Got {.val {accrual_pattern}}"
      ))
    }
  }

  if (!is.null(followup_time)) {
    if (!is.numeric(followup_time) || length(followup_time) != 1L ||
        is.na(followup_time) || followup_time < 0) {
      cli::cli_abort(c(
        "{.arg followup_time} must be a non-negative number",
        "x" = "Got {.val {followup_time}}"
      ))
    }
  }

  if (!is.null(analysis_timing)) {
    valid_timings <- c("sample_size", "calendar", "events")
    if (!is.character(analysis_timing) || length(analysis_timing) != 1L ||
        is.na(analysis_timing) || !analysis_timing %in% valid_timings) {
      cli::cli_abort(c(
        "{.arg analysis_timing} must be one of: {.val {valid_timings}}",
        "x" = "Got {.val {analysis_timing}}"
      ))
    }
  }

  if (!is.null(calendar_analysis_at)) {
    if (!is.numeric(calendar_analysis_at) || length(calendar_analysis_at) == 0L ||
        anyNA(calendar_analysis_at) || any(calendar_analysis_at <= 0)) {
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

  # Validate dropout
  if (!is.null(dropout)) {
    if (!inherits(dropout, "rctbp_dropout")) {
      cli::cli_abort(c(
        "{.arg dropout} must be a {.cls rctbp_dropout} object",
        "x" = "Got {.cls {class(dropout)}}",
        "i" = "Use {.fn dropout} to create a dropout specification"
      ))
    }
    if (is.null(accrual_rate)) {
      cli::cli_abort(c(
        "{.arg accrual_rate} is required when {.arg dropout} is specified",
        "i" = "Dropout modeling requires calendar-time enrollment"
      ))
    }
    if (is.null(followup_time) || followup_time <= 0) {
      cli::cli_abort(c(
        "{.arg followup_time} must be positive when {.arg dropout} is specified",
        "i" = "Instant outcomes (followup_time = 0) cannot have dropout"
      ))
    }
  }

  invisible(TRUE)
}
