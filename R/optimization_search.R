# =============================================================================
# SIMPLEX SEARCH HELPERS
# =============================================================================
# Helper functions for specifying simplex-constrained search parameters.
# These create special objects that pareto_optimize() and its wrappers
# recognize and handle with appropriate transforms (ILR for k>2 dimensions,
# direct for k=2).

#' Specify Allocation Probability Search
#'
#' Creates a search specification for optimizing treatment allocation
#' probabilities. Instead of specifying bounds for each arm, you specify
#' the minimum acceptable proportion for ANY group.
#'
#' @param min Minimum proportion acceptable for any group (default 0.1).
#'   Must be in (0, 0.5). For k arms, requires `k * min < 1`.
#'
#' @return An object of class "rctbp_search_p_alloc" for use in
#'   [pareto_optimize()] or wrapper functions' `search` argument.
#'
#' @details
#' The allocation probabilities form a simplex (must sum to 1, all non-negative).
#' This function specifies a constrained region where no group gets less than
#' `min` proportion.
#'
#' **Internal handling:**
#' \itemize{
#'   \item 2-arm: Direct sampling of treatment proportion in `[min, 1-min]`
#'   \item k-arm (k>2): ILR transform on (k-1) dimensions, scaled to constraint
#' }
#'
#' **Output convention:**
#' The resulting `p_alloc` vector has k entries: `[p_ctrl, p_treat1, p_treat2, ...]`
#' where the first entry is the control arm probability.
#'
#' @seealso [pareto_optimize()], [search_looks()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Optimize allocation with at least 20% per group
#' result <- pareto_optimize(
#'   design = design,
#'   objectives = list(pwr_eff = "maximize", n_total = "minimize"),
#'   search = list(
#'     n_total = c(50, 500),
#'     p_alloc = search_p_alloc(min = 0.2)
#'   ),
#'   constant = list(...)
#' )
#' }
search_p_alloc <- function(min = 0.1) {
  # Validate min
  if (min <= 0) {
    cli::cli_abort(c(
      "{.arg min} must be positive",
      "x" = "Got {.val {min}}"
    ))
  }
  if (min >= 0.5) {
    cli::cli_abort(c(
      "{.arg min} must be less than 0.5",
      "x" = "Got {.val {min}}",
      "i" = "For 2 arms, min=0.5 would force equal allocation (no search needed)"
    ))
  }

  structure(
    list(min_prop = min),
    class = "rctbp_search_p_alloc"
  )
}

#' @rdname search_p_alloc
#' @param x An object of class `rctbp_search_p_alloc`.
#' @param ... Additional arguments (unused).
#' @export
print.rctbp_search_p_alloc <- function(x, ...) {
  cli::cat_line(sprintf("search_p_alloc(min = %g)", x$min_prop))
  invisible(x)
}


#' Specify Interim Look Timing Search
#'
#' Creates a search specification for optimizing the timing of interim
#' analyses. Instead of specifying bounds for each look, you specify the
#' number of looks and minimum spacing between consecutive looks.
#'
#' @param n Number of looks including final analysis (default 2).
#'   Must be >= 2 (at least one interim + final).
#' @param min_spacing Minimum proportion of total sample size between
#'   consecutive looks (default 0.2). Requires `n * min_spacing <= 1`.
#'
#' @return An object of class "rctbp_search_looks" for use in
#'   [pareto_optimize()] or wrapper functions' `search` argument.
#'
#' @details
#' The look timings must be ordered (0 < t1 < t2 < ... < 1) and the gaps
#' between consecutive looks (increments) form a simplex.
#'
#' **Internal handling:**
#' \itemize{
#'   \item n=2: Direct sampling of interim proportion in `[min_spacing, 1-min_spacing]`
#'   \item n>2: ILR transform on increments, scaled to satisfy spacing constraint
#' }
#'
#' **Output convention:**
#' The resulting `analysis_at` vector has n-1 proportions. The final look at 1.0
#' is automatically appended by [build_conditions()].
#'
#' @seealso [pareto_optimize()], [search_p_alloc()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Optimize timing of 3 looks with at least 20% spacing
#' result <- pareto_optimize(
#'   design = design,
#'   objectives = list(pwr_eff = "maximize", n_total = "minimize"),
#'   search = list(
#'     n_total = c(100, 400),
#'     analysis_at = search_looks(n = 3, min_spacing = 0.2)
#'   ),
#'   constant = list(...)
#' )
#' }
search_looks <- function(n = 2, min_spacing = 0.2) {
  # Validate n
  if (!is.numeric(n) || length(n) != 1 || n < 2 || n != floor(n)) {
    cli::cli_abort(c(
      "{.arg n} must be an integer >= 2",
      "x" = "Got {.val {n}}",
      "i" = "n=2 means one interim + final analysis"
    ))
  }

  # Validate min_spacing
  if (min_spacing <= 0) {
    cli::cli_abort(c(
      "{.arg min_spacing} must be positive",
      "x" = "Got {.val {min_spacing}}"
    ))
  }
  if (n * min_spacing > 1) {
    cli::cli_abort(c(
      "{.arg n} * {.arg min_spacing} must be <= 1",
      "x" = "Got {n} * {min_spacing} = {n * min_spacing}",
      "i" = "Reduce number of looks or minimum spacing"
    ))
  }

  structure(
    list(n_looks = as.integer(n), min_spacing = min_spacing),
    class = "rctbp_search_looks"
  )
}

#' @rdname search_looks
#' @param x An object of class `rctbp_search_looks`.
#' @param ... Additional arguments (unused).
#' @export
print.rctbp_search_looks <- function(x, ...) {
  cli::cat_line(sprintf("search_looks(n = %d, min_spacing = %g)", x$n_looks, x$min_spacing))
  invisible(x)
}
