# =============================================================================
# STOPPING BOUNDARY FUNCTIONS
# =============================================================================
# Factory functions for creating look-dependent probability thresholds.
# These functions return boundary functions that take information fraction
# (current_n / n_total) and return the threshold to use at that look.
#
# Usage in build_design():
#   build_design(..., p_sig_scs = boundary_obf(0.975))

#' O'Brien-Fleming-like Bayesian Boundary
#'
#' Creates a boundary function that is stringent at early looks and relaxes
#' toward the final analysis. Approximates O'Brien-Fleming spending function
#' behavior in a Bayesian context.
#'
#' @param base Threshold at final analysis (info_frac = 1). Default 0.975.
#'
#' @return A function that takes information fraction and returns threshold.
#'
#' @details
#' Formula: `threshold = 1 - (1 - base) * sqrt(info_frac)`
#'
#' Example values for `base = 0.975`:
#' \itemize{
#'   \item info_frac = 0.25 → threshold = 0.9875
#'   \item info_frac = 0.50 → threshold = 0.9823
#'   \item info_frac = 0.75 → threshold = 0.9783
#'
#'   \item info_frac = 1.00 → threshold = 0.9750
#' }
#'
#' This boundary is very conservative at early looks (hard to stop early for
#' success) but relaxes to the nominal threshold at the final analysis.
#'
#' @export
#' @seealso [boundary_pocock()], [boundary_linear()], [boundary_power()]
#'
#' @examples
#' # Create OBF-style boundary
#' obf <- boundary_obf(0.975)
#'
#' # Evaluate at different information fractions
#' obf(0.25)  # Early look: ~0.9875
#' obf(0.50)  # Midpoint: ~0.9823
#' obf(1.00)  # Final: 0.975
#'
#' # Use in design
#' \dontrun{
#' design <- build_design(
#'   model = my_model,
#'   target_params = "b_armtreat_1",
#'   p_sig_scs = boundary_obf(0.975),
#'   p_sig_ftl = 0.90,
#'   analysis_at = c(0.5, 0.75)
#' )
#' }
boundary_obf <- function(base = 0.975) {
  force(base)

  if (!is.numeric(base) || length(base) != 1 || base < 0 || base > 1) {
    cli::cli_abort(c(
      "{.arg base} must be a single numeric value between 0 and 1",
      "x" = "You supplied {.val {base}}"
    ))
  }

  function(info_frac) {
    1 - (1 - base) * sqrt(info_frac)
  }
}


#' Pocock-like Boundary (Constant)
#'
#' Creates a constant boundary function that returns the same threshold
#' at all analysis timepoints. Equivalent to the Pocock spending function
#' approach.
#'
#' @param threshold Fixed threshold for all looks. Default 0.99.
#'
#' @return A function that takes information fraction and returns threshold.
#'
#' @details
#' Note: Using the same threshold at all interim analyses can inflate the
#' overall Type I error rate compared to a single-look design. Consider using
#' a higher threshold (e.g., 0.99) or [boundary_obf()] for better error control.
#'
#' @export
#' @seealso [boundary_obf()], [boundary_linear()], [boundary_power()]
#'
#' @examples
#' # Create constant boundary
#' pocock <- boundary_pocock(0.99)
#'
#' # Returns same value regardless of info_frac
#' pocock(0.25)  # 0.99
#' pocock(0.50)  # 0.99
#' pocock(1.00)  # 0.99
boundary_pocock <- function(threshold = 0.99) {
  force(threshold)

  if (!is.numeric(threshold) || length(threshold) != 1 ||
      threshold < 0 || threshold > 1) {
    cli::cli_abort(c(
      "{.arg threshold} must be a single numeric value between 0 and 1",
      "x" = "You supplied {.val {threshold}}"
    ))
  }

  function(info_frac) {
    threshold
  }
}


#' Linear Boundary
#'
#' Creates a boundary function that interpolates linearly from a start value
#' at the first look to an end value at the final analysis.
#'
#' @param start Threshold at first analysis (info_frac approaching 0). Default 0.999.
#' @param end Threshold at final analysis (info_frac = 1). Default 0.975.
#'
#' @return A function that takes information fraction and returns threshold.
#'
#' @details
#' Formula: `threshold = start + (end - start) * info_frac`
#'
#' For success boundaries, typically `start > end` (stringent early, relaxed late).
#' For futility boundaries, typically `start < end` (lenient early, stringent late).
#'
#' @export
#' @seealso [boundary_obf()], [boundary_pocock()], [boundary_power()]
#'
#' @examples
#' # Success boundary: strict early, relaxed late
#' scs_boundary <- boundary_linear(start = 0.999, end = 0.975)
#' scs_boundary(0.5)  # 0.987
#'
#' # Futility boundary: lenient early, strict late
#' ftl_boundary <- boundary_linear(start = 0.70, end = 0.90)
#' ftl_boundary(0.5)  # 0.80
boundary_linear <- function(start = 0.999, end = 0.975) {
  force(start)
  force(end)

  if (!is.numeric(start) || length(start) != 1 || start < 0 || start > 1) {
    cli::cli_abort(c(
      "{.arg start} must be a single numeric value between 0 and 1",
      "x" = "You supplied {.val {start}}"
    ))
  }
  if (!is.numeric(end) || length(end) != 1 || end < 0 || end > 1) {
    cli::cli_abort(c(
      "{.arg end} must be a single numeric value between 0 and 1",
      "x" = "You supplied {.val {end}}"
    ))
  }

  function(info_frac) {
    start + (end - start) * info_frac
  }
}


#' Power Family Boundary
#'
#' Creates a boundary function using a power transformation. Higher values of
#' `rho` create more O'Brien-Fleming-like behavior (conservative early).
#'
#' @param base Threshold at final analysis (info_frac = 1). Default 0.975.
#' @param rho Shape parameter controlling the boundary curve. Default 2.
#'   \itemize{
#'     \item `rho = 2`: Approximates O'Brien-Fleming
#'     \item `rho = 1`: Linear interpolation
#'     \item `rho = 0.5`: More Pocock-like (less conservative early)
#'   }
#'
#' @return A function that takes information fraction and returns threshold.
#'
#' @details
#' Formula: `threshold = 1 - (1 - base) * info_frac^(rho/2)`
#'
#' The `rho` parameter controls how quickly the boundary relaxes:
#' \itemize{
#'   \item Higher rho: More conservative early, faster relaxation later
#'   \item Lower rho: Less conservative early, slower relaxation
#' }
#'
#' @export
#' @seealso [boundary_obf()], [boundary_pocock()], [boundary_linear()]
#'
#' @examples
#' # Compare different rho values at info_frac = 0.5
#' boundary_power(0.975, rho = 3)(0.5)   # More conservative
#' boundary_power(0.975, rho = 2)(0.5)   # OBF-like
#' boundary_power(0.975, rho = 1)(0.5)   # Linear
#' boundary_power(0.975, rho = 0.5)(0.5) # Less conservative
boundary_power <- function(base = 0.975, rho = 2) {
  force(base)
  force(rho)

  if (!is.numeric(base) || length(base) != 1 || base < 0 || base > 1) {
    cli::cli_abort(c(
      "{.arg base} must be a single numeric value between 0 and 1",
      "x" = "You supplied {.val {base}}"
    ))
  }
  if (!is.numeric(rho) || length(rho) != 1 || rho <= 0) {
    cli::cli_abort(c(
      "{.arg rho} must be a single positive numeric value",
      "x" = "You supplied {.val {rho}}"
    ))
  }

  function(info_frac) {
    1 - (1 - base) * info_frac^(rho / 2)
  }
}


#' Resolve Threshold Value
#'
#' Internal helper to resolve a threshold specification (numeric or function)
#' to a numeric value at a given information fraction.
#'
#' @param threshold Either a numeric value or a boundary function
#' @param info_frac Information fraction (current_n / n_total), between 0 and 1
#'
#' @return Numeric threshold value
#' @keywords internal
resolve_threshold <- function(threshold, info_frac) {
  if (is.function(threshold)) {
    threshold(info_frac)
  } else {
    threshold
  }
}


#' Resolve Boundary to Per-Look Vector
#'
#' Internal helper to resolve a boundary specification to a vector of
#' threshold values, one per analysis look.
#'
#' @param boundary Either NULL, a numeric value, a numeric vector, or a boundary function
#' @param look_info Data frame with id_look and n_analyzed columns
#' @param n_total Maximum planned sample size
#'
#' @return Numeric vector of thresholds, one per look
#' @keywords internal
resolve_boundary_vector <- function(boundary, look_info, n_total) {
  n_looks <- nrow(look_info)

  if (is.null(boundary)) {
    return(rep(NA_real_, n_looks))
  } else if (is.function(boundary)) {
    info_fracs <- look_info$n_analyzed / n_total
    sapply(info_fracs, boundary)
  } else if (length(boundary) == 1) {
    rep(boundary, n_looks)
  } else if (length(boundary) == n_looks) {
    boundary
  } else {
    cli::cli_abort(c(
      "Invalid boundary specification",
      "x" = "Got length {length(boundary)}, expected 1 or {n_looks}",
      "i" = "Provide: single value, vector of length {n_looks}, or boundary function"
    ))
  }
}
