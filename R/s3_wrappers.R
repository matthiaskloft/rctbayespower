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

#' @rdname print.rctbp_power_analysis
#' @export
print.rctbp_power_analysis <- function(x, ...) {
  S7::method(print, rctbp_power_analysis)(x, ...)
}

#' @rdname summary.rctbp_power_analysis
#' @export
summary.rctbp_power_analysis <- function(object, ...) {
  S7::method(summary, rctbp_power_analysis)(object, ...)
}

#' @rdname as.data.frame.rctbp_power_analysis
#' @export
as.data.frame.rctbp_power_analysis <- function(x, ...,
    what = c("conditions", "raw", "interim")) {
  S7::method(as.data.frame, rctbp_power_analysis)(x, ..., what = what)
}

#' @rdname print.rctbp_conditions
#' @export
print.rctbp_conditions <- function(x, ...) {
  S7::method(print, rctbp_conditions)(x, ...)
}

#' @rdname as.data.frame.rctbp_conditions
#' @export
as.data.frame.rctbp_conditions <- function(x, ...) {
  S7::method(as.data.frame, rctbp_conditions)(x, ...)
}

#' @rdname print.rctbp_design
#' @export
print.rctbp_design <- function(x, ...) {
  S7::method(print, rctbp_design)(x, ...)
}

#' @rdname print.rctbp_model
#' @export
print.rctbp_model <- function(x, ...) {
  S7::method(print, rctbp_model)(x, ...)
}

#' @rdname print.rctbp_sim_fn
#' @export
print.rctbp_sim_fn <- function(x, ...) {
  S7::method(print, rctbp_sim_fn)(x, ...)
}

#' @rdname run.rctbp_power_analysis
#' @export
run.rctbp_power_analysis <- function(x, ...) {
  S7::method(run, rctbp_power_analysis)(x, ...)
}

# =============================================================================
# GET_CODE GENERIC
# =============================================================================

#' @rdname get_code
#' @method get_code rctbp_design
#' @export
get_code.rctbp_design <- function(x, ...) {
  S7::method(get_code, rctbp_design)(x, ...)
}

#' @rdname get_code
#' @method get_code rctbp_conditions
#' @export
get_code.rctbp_conditions <- function(x, ...) {
  S7::method(get_code, rctbp_conditions)(x, ...)
}

#' @rdname get_code
#' @method get_code rctbp_power_analysis
#' @export
get_code.rctbp_power_analysis <- function(x, ...) {
  S7::method(get_code, rctbp_power_analysis)(x, ...)
}

# =============================================================================
# PARETO OPTIMIZATION CLASSES
# =============================================================================

#' @rdname print.rctbp_pareto_result
#' @export
print.rctbp_pareto_result <- function(x, ...) {
  S7::method(print, rctbp_pareto_result)(x, ...)
}

#' @rdname summary.rctbp_pareto_result
#' @export
summary.rctbp_pareto_result <- function(object, ...) {
  S7::method(summary, rctbp_pareto_result)(object, ...)
}

#' @rdname as.data.frame.rctbp_pareto_result
#' @export
as.data.frame.rctbp_pareto_result <- function(x, ...,
    what = c("pareto", "archive", "selected", "convergence")) {
  S7::method(as.data.frame, rctbp_pareto_result)(x, ..., what = what)
}
