#' Verbosity Control System
#'
#' Provides three-level verbosity control (0 = quiet, 1 = normal, 2 = verbose)
#' for all package output.
#'
#' @name verbosity
#' @keywords internal
NULL

#' Get Current Verbosity Level
#'
#' Returns the current verbosity level setting for the package.
#'
#' @return Integer: 0 (quiet), 1 (normal), or 2 (verbose)
#' @export
#'
#' @examples
#' get_verbosity()
#'
get_verbosity <- function() {
  level <- getOption("rctbayespower.verbosity", default = 1)

  if (!level %in% c(0, 1, 2)) {
    cli::cli_warn(c(
      "Invalid verbosity level: {.val {level}}",
      "i" = "Using default {.val 1} (normal)"
    ))
    level <- 1
  }

  return(level)
}

#' Set Verbosity Level
#'
#' Sets the verbosity level for all package output.
#'
#' @param verbosity Integer: verbosity level
#'   * 0 = quiet (minimal output, errors only)
#'   * 1 = normal (standard output, default)
#'   * 2 = verbose (detailed output for debugging)
#'
#' @return Invisibly returns the previous verbosity level.
#' @export
#'
#' @examples
#' # Set to quiet mode
#' set_verbosity(0)
#'
#' # Set to verbose mode
#' set_verbosity(2)
#'
#' # Back to normal
#' set_verbosity(1)
#'
set_verbosity <- function(verbosity = c(0, 1, 2)) {
  if (!verbosity %in% c(0, 1, 2)) {
    cli::cli_abort(c(
      "{.arg verbosity} must be 0, 1, or 2",
      "x" = "You supplied {.val {verbosity}}"
    ))
  }

  old_level <- getOption("rctbayespower.verbosity", default = 1)
  options(rctbayespower.verbosity = verbosity)
  invisible(old_level)
}

#' Temporarily Change Verbosity Level
#'
#' Executes code with a temporary verbosity level.
#'
#' @param verbosity Integer: verbosity level (0, 1, or 2)
#' @param code Code to execute with the temporary verbosity
#'
#' @return Result of evaluating `code`
#' @export
#'
#' @examples
#' \dontrun{
#' # Run analysis in quiet mode temporarily
#' with_verbosity(0, {
#'   result <- power_analysis(conditions, n_sims = 100)
#' })
#' }
#'
with_verbosity <- function(verbosity = c(0, 1, 2), code) {
  if (!verbosity %in% c(0, 1, 2)) {
    cli::cli_abort(c(
      "{.arg verbosity} must be 0, 1, or 2",
      "x" = "You supplied {.val {verbosity}}"
    ))
  }

  old_level <- set_verbosity(verbosity)
  on.exit(options(rctbayespower.verbosity = old_level))
  force(code)
}

#' Check if Message Should Be Displayed
#'
#' Determines whether a message at a given level should be shown
#' based on current verbosity settings.
#'
#' @param required_level Integer: minimum verbosity level required to show message
#'   * 0 = always show (errors, critical messages)
#'   * 1 = show in normal and verbose modes (standard messages)
#'   * 2 = show only in verbose mode (debug messages)
#'
#' @return Logical: TRUE if message should be displayed, FALSE otherwise
#' @export
#'
#' @examples
#' # Check if verbose messages should be shown
#' if (should_show(2)) {
#'   message("This is a debug message")
#' }
#'
should_show <- function(required_level = 1) {
  current_level <- get_verbosity()
  return(current_level >= required_level)
}

#' Conditional Message Output
#'
#' Outputs a message only if current verbosity level permits.
#'
#' @param msg Character string: message to display
#' @param level Integer: minimum verbosity level required (0, 1, or 2)
#' @param .envir Environment for string interpolation
#'
#' @return NULL (outputs to console if verbosity permits)
#' @keywords internal
#'
verbosity_message <- function(msg, level = 1, .envir = parent.frame()) {
  if (should_show(level)) {
    cli::cli_alert_info(msg, .envir = .envir)
  }
  invisible(NULL)
}

