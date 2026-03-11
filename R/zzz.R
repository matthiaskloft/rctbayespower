.onLoad <- function(libname, pkgname) {
  S7::methods_register()

  # Build AcqFunctionEIC R6 class only when mlr3mbo is available
  # (deferred to avoid hard dependency at parse time, which breaks roxygen)
  if (requireNamespace("mlr3mbo", quietly = TRUE)) {
    AcqFunctionEIC <<- .build_AcqFunctionEIC()
  }
}
