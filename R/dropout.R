# =============================================================================
# DROPOUT / LOSS-TO-FOLLOW-UP
# =============================================================================
# Dropout modeling for clinical trials. Creates dropout specification objects
# that integrate with accrual modeling to simulate patient attrition.
#
# Uses exponential (constant hazard) dropout model:
#   dropout_time ~ Exp(hazard_rate)
#   Patient completes if dropout_time >= followup_time

#' Create Dropout Specification
#'
#' Specifies expected patient dropout (loss-to-follow-up) for power analysis.
#' Returns a dropout specification object that converts to a hazard rate
#' given the follow-up time. Use in the `constant` or `crossed` arguments
#' of [build_conditions()].
#'
#' @param rate Numeric scalar or vector. For `type = "proportion"`: expected
#'   dropout proportion over `followup_time` (strictly between 0 and 1).
#'   For `type = "hazard"`: constant hazard rate (must be positive).
#' @param type Character. `"proportion"` (default) or `"hazard"`. Recycled
#'   to match `length(rate)`.
#'
#' @return For scalar `rate`: a single `rctbp_dropout` object (a function
#'   `f(followup_time) -> hazard_rate` with class `c("rctbp_dropout", "function")`).
#'   For vector `rate`: a list of `rctbp_dropout` objects (ready for grid crossing
#'   via `c()` or directly in `crossed`).
#'
#' @details
#' The dropout model uses an exponential (constant hazard) process:
#' \itemize{
#'   \item After enrollment, each patient gets `dropout_time ~ Exp(hazard)`
#'   \item Patient completes if `dropout_time >= followup_time`
#'   \item Dropped-out patients are excluded from analysis
#' }
#'
#' **Proportion parameterization** (`type = "proportion"`):
#' Converts to hazard via `hazard = -log(1 - rate) / followup_time`.
#' This means `rate` is the expected proportion of patients who drop out
#' over the full follow-up period.
#'
#' **Hazard parameterization** (`type = "hazard"`):
#' Uses `rate` directly as the constant hazard rate per time unit,
#' independent of follow-up time.
#'
#' **Requirements:**
#' \itemize{
#'   \item `dropout` requires `accrual_rate` (dropout without calendar time
#'     is meaningless)
#'   \item `dropout` requires `followup_time > 0` (instant outcomes can't
#'     have dropout)
#' }
#'
#' @examples
#' \dontrun{
#' # 20% expected dropout over follow-up period
#' build_conditions(
#'   design = my_design,
#'   constant = list(dropout = dropout(0.2), followup_time = 12,
#'                   accrual_rate = 5)
#' )
#'
#' # Cross different dropout rates
#' build_conditions(
#'   design = my_design,
#'   crossed = list(dropout = dropout(c(0.1, 0.2, 0.3)))
#' )
#'
#' # Hazard rate parameterization
#' build_conditions(
#'   design = my_design,
#'   constant = list(
#'     dropout = dropout(0.03, type = "hazard"),
#'     followup_time = 12, accrual_rate = 5
#'   )
#' )
#' }
#'
#' @export
dropout <- function(rate, type = "proportion") {
  if (!is.numeric(rate) || length(rate) == 0L || anyNA(rate)) {
    cli::cli_abort(c(
      "{.arg rate} must be a non-missing numeric value or vector",
      "x" = "Got {.val {rate}}"
    ))
  }

  # Recycle type to match rate length
  type <- rep_len(type, length(rate))

  # Validate each rate/type combination
  for (i in seq_along(rate)) {
    if (!type[i] %in% c("proportion", "hazard")) {
      cli::cli_abort(c(
        "{.arg type} must be {.val proportion} or {.val hazard}",
        "x" = "Got {.val {type[i]}}"
      ))
    }
    if (type[i] == "proportion" && (rate[i] <= 0 || rate[i] >= 1)) {
      cli::cli_abort(c(
        "Dropout {.arg rate} with {.code type = \"proportion\"} must be between 0 and 1 (exclusive)",
        "x" = "Got {.val {rate[i]}}"
      ))
    }
    if (type[i] == "hazard" && rate[i] <= 0) {
      cli::cli_abort(c(
        "Dropout {.arg rate} with {.code type = \"hazard\"} must be positive",
        "x" = "Got {.val {rate[i]}}"
      ))
    }
  }

  # Create individual dropout objects
  make_one <- function(r, t) {
    force(r)
    force(t)
    fn <- function(followup_time) {
      if (!is.numeric(followup_time) || length(followup_time) != 1L ||
          is.na(followup_time) || followup_time <= 0) {
        cli::cli_abort("{.arg followup_time} must be a positive number")
      }
      if (t == "proportion") {
        -log(1 - r) / followup_time
      } else {
        r
      }
    }
    structure(
      fn,
      class = c("rctbp_dropout", "function"),
      dropout_params = list(rate = r, type = t)
    )
  }

  if (length(rate) == 1L) {
    make_one(rate[1], type[1])
  } else {
    lapply(seq_along(rate), function(i) make_one(rate[i], type[i]))
  }
}


#' @export
print.rctbp_dropout <- function(x, ...) {
  cli::cat_line(format(x))
  invisible(x)
}


#' @export
format.rctbp_dropout <- function(x, ...) {
  params <- attr(x, "dropout_params")
  paste0("dropout(rate = ", params$rate, ", type = \"", params$type, "\")")
}
