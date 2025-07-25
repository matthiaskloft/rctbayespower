# S3 Method Wrappers for S7 Methods
# These provide S3 compatibility for the S7 methods defined in the package

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