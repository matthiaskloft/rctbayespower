# =============================================================================
# S3 METHOD WRAPPERS FOR S7 CLASSES
# =============================================================================
# These S3 generic wrappers enable standard R methods (print, plot, run) to
# work with S7 class objects. Without these wrappers, base R wouldn't recognize
# S7 methods when users call generic functions.
#
# How it works: When a user calls plot(power_analysis_object), R's S3 dispatch
# system looks for plot.rctbp_power_analysis. These wrappers delegate to the
# actual S7 method implementations.

#' @export
plot.rctbp_power_analysis <- function(x, ...) {
  S7::method(plot, rctbp_power_analysis)(x, ...)
}

#' @export
print.rctbp_power_analysis <- function(x, ...) {
  S7::method(print, rctbp_power_analysis)(x, ...)
}

#' @export
print.rctbp_conditions <- function(x, ...) {
  S7::method(print, rctbp_conditions)(x, ...)
}

#' @export
print.rctbp_design <- function(x, ...) {
  S7::method(print, rctbp_design)(x, ...)
}

#' @export
print.rctbp_model <- function(x, ...) {
  S7::method(print, rctbp_model)(x, ...)
}

#' @export
run.rctbp_power_analysis <- function(x, ...) {
  S7::method(run, rctbp_power_analysis)(x, ...)
}