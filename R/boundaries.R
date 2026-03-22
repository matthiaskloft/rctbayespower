# =============================================================================
# STOPPING BOUNDARY FUNCTIONS
# =============================================================================
# Factory functions for creating look-dependent probability thresholds.
# These functions return boundary functions that take information fraction
# (current_n / n_total) and return the threshold to use at that look.
#
# Usage in build_conditions():
#   build_conditions(..., thr_dec_eff = boundary_obf(0.975))

#' O'Brien-Fleming Boundary
#'
#' Creates a boundary function with O'Brien-Fleming shape (very conservative
#' early, relaxing toward final analysis). Specify either `alpha` for
#' frequentist Type I error control, or `threshold` for Bayesian decision rules.
#'
#' @param alpha One-sided significance level for frequentist error control.
#'   Must be between 0 and 0.5. Mutually exclusive with `threshold`.
#' @param threshold Final probability threshold for Bayesian decision rules.
#'   Must be between 0.5 and 1 (exclusive). Mutually exclusive with `alpha`.
#'
#' @return A function that takes a vector of information fractions and returns
#'   a vector of probability thresholds (one per look).
#'
#' @details
#' O'Brien-Fleming boundaries are very conservative at early looks (hard to
#' stop for success) but relax toward the final analysis. This preserves
#' most of the sample size and statistical power for the final look.
#'
#' \strong{Frequentist mode} (`alpha`): Uses the Lan-DeMets spending function
#' to control Type I error. The spending function is:
#' \deqn{\alpha(t) = 2 - 2\Phi(\Phi^{-1}(1 - \alpha/2) / \sqrt{t})}
#' When `gsDesign` is installed, computes exact boundaries using multivariate
#' normal integration.
#'
#' \strong{Bayesian mode} (`threshold`): Uses the OBF shape but scales it to
#' end at the specified probability threshold. Early looks are very stringent
#' (close to 1.0), relaxing to exactly `threshold` at the final analysis.
#'
#' @references
#' O'Brien, P. C. and Fleming, T. R. (1979). A multiple testing procedure for
#' clinical trials. Biometrics 35: 549-556.
#'
#' Lan, K. K. G. and DeMets, D. L. (1983). Discrete sequential boundaries for
#' clinical trials. Biometrika 70: 659-663.
#'
#' @export
#' @seealso [boundary_pocock()], [boundary_linear()], [boundary_power()]
#'
#' @examples
#' # Frequentist: control Type I error at 2.5%
#' obf_freq <- boundary_obf(alpha = 0.025)
#' obf_freq(c(0.5, 1.0))
#'
#' # Bayesian: OBF shape ending at 0.95 threshold
#' obf_bayes <- boundary_obf(threshold = 0.95)
#' obf_bayes(c(0.5, 1.0))
boundary_obf <- function(alpha = NULL, threshold = NULL) {
  # Validate: exactly one of alpha or threshold must be provided

  if (is.null(alpha) && is.null(threshold)) {
    cli::cli_abort(c(
      "Must specify either {.arg alpha} or {.arg threshold}",
      "i" = "Use {.arg alpha} for frequentist error control (e.g., 0.025)",
      "i" = "Use {.arg threshold} for Bayesian decision rules (e.g., 0.95)"
    ))
  }
  if (!is.null(alpha) && !is.null(threshold)) {
    cli::cli_abort(c(
      "Cannot specify both {.arg alpha} and {.arg threshold}",
      "i" = "Use {.arg alpha} for frequentist error control",
      "i" = "Use {.arg threshold} for Bayesian decision rules"
    ))
  }

  # Frequentist mode: alpha-spending
  if (!is.null(alpha)) {
    force(alpha)
    if (!is.numeric(alpha) || length(alpha) != 1 ||
        alpha <= 0 || alpha > 0.5) {
      cli::cli_abort(c(
        "{.arg alpha} must be a single numeric value between 0 and 0.5",
        "x" = "You supplied {.val {alpha}}"
      ))
    }

    f <- function(info_frac) {
      n_looks <- length(info_frac)

      # Try to use gsDesign for exact boundaries
      if (requireNamespace("gsDesign", quietly = TRUE)) {
        design <- gsDesign::gsDesign(
          k = n_looks,
          timing = info_frac,
          alpha = alpha,
          test.type = 1,
          sfu = gsDesign::sfLDOF
        )
        z_bounds <- design$upper$bound
        thresholds <- stats::pnorm(z_bounds)
      } else {
        # Fallback: spending function approximation
        z_alpha <- stats::qnorm(1 - alpha / 2)
        alpha_spent <- 2 - 2 * stats::pnorm(z_alpha / sqrt(info_frac))
        alpha_incremental <- c(alpha_spent[1], diff(alpha_spent))
        thresholds <- 1 - alpha_incremental / 2
      }
      thresholds
    }

    attr(f, "boundary_type") <- "obf"
    attr(f, "boundary_params") <- list(alpha = alpha)

  # Bayesian mode: threshold-based
  } else {
    force(threshold)
    if (!is.numeric(threshold) || length(threshold) != 1 ||
        threshold <= 0.5 || threshold >= 1) {
      cli::cli_abort(c(
        "{.arg threshold} must be a single numeric value between 0.5 and 1 (exclusive)",
        "x" = "You supplied {.val {threshold}}"
      ))
    }

    f <- function(info_frac) {
      # Use reference alpha = 0.025 for the OBF shape
      z_ref <- stats::qnorm(1 - 0.025 / 2)
      spent_pattern <- 2 - 2 * stats::pnorm(z_ref / sqrt(info_frac))
      spent_normalized <- spent_pattern / spent_pattern[length(spent_pattern)]
      budget <- 1 - threshold
      1 - spent_normalized * budget
    }

    attr(f, "boundary_type") <- "obf"
    attr(f, "boundary_params") <- list(threshold = threshold)
  }

  class(f) <- c("boundary_function", "function")
  f
}


#' Pocock Boundary
#'
#' Creates a Pocock-style boundary function with similar thresholds at all
#' interim analyses. Specify either `alpha` for frequentist Type I error
#' control, or `threshold` for Bayesian decision rules.
#'
#' @param alpha One-sided significance level for frequentist error control.
#'   Must be between 0 and 0.5. Mutually exclusive with `threshold`.
#' @param threshold Probability threshold for Bayesian decision rules.
#'   Must be between 0 and 1. Returns constant threshold at all looks.
#'   Mutually exclusive with `alpha`.
#'
#' @return A function that takes a vector of information fractions and returns
#'   a vector of thresholds (one per look).
#'
#' @details
#' Pocock boundaries use similar thresholds at all interim analyses,
#' making it easier to stop early compared to O'Brien-Fleming.
#'
#' \strong{Frequentist mode} (`alpha`): Uses the Lan-DeMets Pocock spending
#' function to control Type I error:
#' \deqn{\alpha(t) = \alpha \ln(1 + (e - 1) t)}
#' When `gsDesign` is installed, computes exact boundaries. Otherwise uses
#' a lookup table (accurate for equally spaced looks only).
#'
#' \strong{Bayesian mode} (`threshold`): Returns the same threshold at every
#' analysis. Note that using the same threshold at multiple looks will have
#' different operating characteristics than a single-look design.
#'
#' @references
#' Pocock, S. J. (1977). Group sequential methods in the design and analysis
#' of clinical trials. Biometrika 64: 191-199.
#'
#' Lan, K. K. G. and DeMets, D. L. (1983). Discrete sequential boundaries for
#' clinical trials. Biometrika 70: 659-663.
#'
#' @export
#' @seealso [boundary_obf()], [boundary_linear()], [boundary_constant()]
#'
#' @examples
#' # Frequentist: control Type I error at 2.5%
#' pocock_freq <- boundary_pocock(alpha = 0.025)
#' pocock_freq(c(0.5, 1.0))
#'
#' # Bayesian: constant 0.95 threshold at all looks
#' pocock_bayes <- boundary_pocock(threshold = 0.95)
#' pocock_bayes(c(0.5, 1.0))  # Returns c(0.95, 0.95)
boundary_pocock <- function(alpha = NULL, threshold = NULL) {
  # Validate: exactly one of alpha or threshold must be provided
  if (is.null(alpha) && is.null(threshold)) {
    cli::cli_abort(c(
      "Must specify either {.arg alpha} or {.arg threshold}",
      "i" = "Use {.arg alpha} for frequentist error control (e.g., 0.025)",
      "i" = "Use {.arg threshold} for Bayesian decision rules (e.g., 0.95)"
    ))
  }
  if (!is.null(alpha) && !is.null(threshold)) {
    cli::cli_abort(c(
      "Cannot specify both {.arg alpha} and {.arg threshold}",
      "i" = "Use {.arg alpha} for frequentist error control",
      "i" = "Use {.arg threshold} for Bayesian decision rules"
    ))
  }

  # Frequentist mode: alpha-spending
  if (!is.null(alpha)) {
    force(alpha)
    if (!is.numeric(alpha) || length(alpha) != 1 ||
        alpha <= 0 || alpha > 0.5) {
      cli::cli_abort(c(
        "{.arg alpha} must be a single numeric value between 0 and 0.5",
        "x" = "You supplied {.val {alpha}}"
      ))
    }

    f <- function(info_frac) {
      n_looks <- length(info_frac)

      if (requireNamespace("gsDesign", quietly = TRUE)) {
        design <- gsDesign::gsDesign(
          k = n_looks,
          timing = info_frac,
          alpha = alpha,
          test.type = 1,
          sfu = gsDesign::sfLDPocock
        )
        z_bounds <- design$upper$bound
        thresholds <- stats::pnorm(z_bounds)
      } else {
        # Fallback: lookup table for equally spaced looks
        pocock_z_table <- c(
          `1` = 1.960, `2` = 2.178, `3` = 2.289, `4` = 2.361,
          `5` = 2.413, `6` = 2.453, `7` = 2.485, `8` = 2.512,
          `9` = 2.535, `10` = 2.555
        )

        if (n_looks > 10) {
          cli::cli_warn(c(
            "Pocock boundary approximation for > 10 looks may be inaccurate",
            "i" = "Install {.pkg gsDesign} for exact boundary computation"
          ))
          z_crit <- 1.96 + 0.3 * log(n_looks)
        } else {
          z_crit <- pocock_z_table[as.character(n_looks)]
        }

        if (alpha != 0.025) {
          z_adjustment <- stats::qnorm(1 - alpha) - stats::qnorm(0.975)
          z_crit <- z_crit + z_adjustment
        }

        threshold_val <- stats::pnorm(z_crit)
        thresholds <- rep(threshold_val, n_looks)

        expected_spacing <- seq(1 / n_looks, 1, by = 1 / n_looks)
        if (!isTRUE(all.equal(info_frac, expected_spacing, tolerance = 0.01))) {
          cli::cli_warn(c(
            "Unequally spaced looks detected without {.pkg gsDesign}",
            "i" = "Boundaries are approximate; install {.pkg gsDesign} for exact values"
          ))
        }
      }
      thresholds
    }

    attr(f, "boundary_type") <- "pocock"
    attr(f, "boundary_params") <- list(alpha = alpha)

  # Bayesian mode: constant threshold
  } else {
    force(threshold)
    if (!is.numeric(threshold) || length(threshold) != 1 ||
        threshold < 0 || threshold > 1) {
      cli::cli_abort(c(
        "{.arg threshold} must be a single numeric value between 0 and 1",
        "x" = "You supplied {.val {threshold}}"
      ))
    }

    f <- function(info_frac) {
      rep(threshold, length(info_frac))
    }

    attr(f, "boundary_type") <- "pocock"
    attr(f, "boundary_params") <- list(threshold = threshold)
  }

  class(f) <- c("boundary_function", "function")
  f
}


#' Hwang-Shih-DeCani Boundary
#'
#' Creates a boundary function using the Hwang-Shih-DeCani (HSD) spending
#' function family. This is a flexible one-parameter family that can
#' approximate various boundary types. Requires the `gsDesign` package.
#'
#' @param alpha One-sided significance level. Default 0.025.
#' @param gamma Shape parameter controlling boundary behavior. Default -4.
#'   \itemize{
#'     \item gamma = -4: Approximates O'Brien-Fleming (conservative early)
#'     \item gamma = 1: Approximates Pocock (aggressive early)
#'     \item gamma = 0: Linear spending
#'     \item gamma < 0: More conservative early (like OBF)
#'     \item gamma > 0: More aggressive early (like Pocock)
#'   }
#'
#' @return A function that takes a vector of information fractions and returns
#'   a vector of probability thresholds (one per look).
#'
#' @details
#' The Hwang-Shih-DeCani spending function has the form:
#' \deqn{\alpha(t) = \alpha \frac{1 - e^{-\gamma t}}{1 - e^{-\gamma}}}
#'
#' for \eqn{\gamma \neq 0}, and \eqn{\alpha(t) = \alpha t} for \eqn{\gamma = 0}.
#'
#' This function requires the `gsDesign` package for exact boundary computation.
#'
#' @references
#' Hwang, I. K., Shih, W. J., and DeCani, J. S. (1990). Group sequential designs
#' using a family of type I error probability spending functions. Statistics in
#' Medicine 9: 1439-1445.
#'
#' @export
#' @seealso [boundary_obf()], [boundary_pocock()], [boundary_power()]
#'
#' @examples
#' \dontrun{
#' # Create HSD boundary (requires gsDesign)
#' hsd <- boundary_hsd(gamma = -4)  # OBF-like
#' hsd(c(0.5, 1.0))
#'
#' hsd_pocock <- boundary_hsd(gamma = 1)  # Pocock-like
#' hsd_pocock(c(0.5, 1.0))
#' }
boundary_hsd <- function(alpha = 0.025, gamma = -4) {
  force(alpha)
  force(gamma)

  if (!is.numeric(alpha) || length(alpha) != 1 ||
      alpha <= 0 || alpha > 0.5) {
    cli::cli_abort(c(
      "{.arg alpha} must be a single numeric value between 0 and 0.5",
      "x" = "You supplied {.val {alpha}}"
    ))
  }

  if (!is.numeric(gamma) || length(gamma) != 1 ||
      gamma < -40 || gamma > 40) {
    cli::cli_abort(c(
      "{.arg gamma} must be a single numeric value between -40 and 40",
      "x" = "You supplied {.val {gamma}}"
    ))
  }

  f <- function(info_frac) {
    n_looks <- length(info_frac)

    if (!requireNamespace("gsDesign", quietly = TRUE)) {
      cli::cli_abort(c(
        "{.fn boundary_hsd} requires the {.pkg gsDesign} package",
        "i" = "Install it with: {.code install.packages(\"gsDesign\")}"
      ))
    }

    # Use gsDesign's Hwang-Shih-DeCani spending function
    design <- gsDesign::gsDesign(
      k = n_looks,
      timing = info_frac,
      alpha = alpha,
      test.type = 1,
      sfu = gsDesign::sfHSD,
      sfupar = gamma
    )

    # Convert Z-boundaries to probability thresholds
    z_bounds <- design$upper$bound
    stats::pnorm(z_bounds)
  }

  # Add metadata for display
  attr(f, "boundary_type") <- "hsd"
  attr(f, "boundary_params") <- list(alpha = alpha, gamma = gamma)
  class(f) <- c("boundary_function", "function")
  f
}


#' Constant Boundary
#'
#' Creates a simple constant boundary function that returns the same threshold
#' at all analysis timepoints. Unlike [boundary_pocock()], this does not
#' adjust for the number of looks.
#'
#' @param threshold Fixed threshold for all looks. Default 0.95.
#'
#' @return A function that takes information fractions and returns thresholds.
#'
#' @details
#' This is a simple constant threshold without adjustment for multiple looks.
#' Using the same threshold at all interim analyses will inflate the overall
#' Type I error rate compared to a single-look design.
#'
#' For proper Type I error control, consider using [boundary_pocock()] which
#' calibrates the threshold based on the number of looks.
#'
#' @export
#' @seealso [boundary_pocock()], [boundary_obf()], [boundary_linear()]
#'
#' @examples
#' # Create constant boundary at 0.95
#' const <- boundary_constant(0.95)
#'
#' # Returns same value regardless of number of looks
#' const(c(0.5, 1.0))  # 0.95, 0.95
boundary_constant <- function(threshold = 0.95) {
  force(threshold)

  if (!is.numeric(threshold) || length(threshold) != 1 ||
      threshold < 0 || threshold > 1) {
    cli::cli_abort(c(
      "{.arg threshold} must be a single numeric value between 0 and 1",
      "x" = "You supplied {.val {threshold}}"
    ))
  }

  f <- function(info_frac) {
    rep(threshold, length(info_frac))
  }

  # Add metadata for display
  attr(f, "boundary_type") <- "constant"
  attr(f, "boundary_params") <- list(threshold = threshold)
  class(f) <- c("boundary_function", "function")
  f
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

  f <- function(info_frac) {
    start + (end - start) * info_frac
  }

  # Add metadata for display
  attr(f, "boundary_type") <- "linear"
  attr(f, "boundary_params") <- list(start = start, end = end)
  class(f) <- c("boundary_function", "function")
  f
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
#' This is a simple probability-scale shape function for Bayesian decision
#' rules. It does not provide frequentist Type I error control. For a
#' principled one-parameter family with alpha-spending support, see
#' the Wang-Tsiatis boundary [boundary_wang_tsiatis()], which
#' operates on the z-scale and subsumes O'Brien-Fleming and Pocock as
#' special cases.
#'
#' @export
#' @seealso [boundary_wang_tsiatis()], [boundary_obf()], [boundary_pocock()],
#'   [boundary_linear()]
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

  f <- function(info_frac) {
    1 - (1 - base) * info_frac^(rho / 2)
  }

  # Add metadata for display
  attr(f, "boundary_type") <- "power"
  attr(f, "boundary_params") <- list(base = base, rho = rho)
  class(f) <- c("boundary_function", "function")
  f
}


#' Wang-Tsiatis Boundary
#'
#' Creates a boundary function from the Wang-Tsiatis one-parameter family,
#' which smoothly interpolates between O'Brien-Fleming (`delta = 0`) and
#' Pocock (`delta = 0.5`) boundaries. Specify either `alpha` for frequentist
#' Type I error control, or `threshold` for Bayesian decision rules.
#'
#' @param delta Shape parameter controlling boundary aggressiveness.
#'   Must be between 0 and 0.5.
#'   \itemize{
#'     \item `delta = 0`: O'Brien-Fleming-like (very conservative early)
#'     \item `delta = 0.25`: Midpoint between OBF and Pocock
#'     \item `delta = 0.5`: Pocock-like (constant across looks)
#'   }
#' @param alpha One-sided significance level for frequentist error control.
#'   Must be between 0 and 0.5. Requires the `gsDesign` package at call time.
#'   Mutually exclusive with `threshold`.
#' @param threshold Probability threshold for Bayesian decision rules.
#'   Must be between 0 and 1 (exclusive). Mutually exclusive with `alpha`.
#'
#' @return A function that takes a vector of information fractions and returns
#'   a vector of probability thresholds (one per look).
#'
#' @details
#' The Wang-Tsiatis boundary family defines z-scale boundaries as:
#' \deqn{z_k = C \cdot t_k^{\Delta - 0.5}}
#' where \eqn{t_k} is the information fraction at look \eqn{k}, \eqn{\Delta}
#' is the shape parameter (`delta`), and \eqn{C} is a constant calibrated to
#' control the overall Type I error.
#'
#' \strong{Frequentist mode} (`alpha`): Requires the `gsDesign` package.
#' At `delta = 0`, uses the Lan-DeMets O'Brien-Fleming spending function
#' (`sfLDOF`) for exact results. At `delta = 0.5`, uses the Lan-DeMets Pocock
#' spending function (`sfLDPocock`). For intermediate values, calibrates the
#' constant \eqn{C} via numerical root-finding using `gsDesign::gsProbability()`
#' for multivariate normal integration.
#'
#' \strong{Bayesian mode} (`threshold`): Uses the closed-form probability-scale
#' formula:
#' \deqn{\text{threshold}_k = \Phi(\Phi^{-1}(\text{threshold}) \cdot t_k^{\Delta - 0.5})}
#' No external packages required. At `delta = 0.5`, this reduces to a constant
#' threshold (identical to [boundary_pocock(threshold = ...)]). At `delta = 0`,
#' produces an OBF-like shape that is qualitatively similar to
#' [boundary_obf(threshold = ...)] but not numerically identical (different
#' parameterizations).
#'
#' \strong{Comparison with `boundary_power()`}: `boundary_power()` is a simple
#' probability-scale shape function (`1 - (1-base) * t^(rho/2)`) without
#' frequentist alpha control. `boundary_wang_tsiatis()` is the principled
#' z-scale parameterization from the literature with both frequentist and
#' Bayesian modes.
#'
#' Note: `threshold` accepts values in (0, 1) exclusive, which differs from
#' [boundary_obf()] (which requires threshold > 0.5). Values near 0 or 1
#' produce degenerate boundaries.
#'
#' @references
#' Wang, S. K. and Tsiatis, A. A. (1987). Approximately optimal one-parameter
#' boundaries for group sequential trials. Biometrics 43: 193-199.
#'
#' Lan, K. K. G. and DeMets, D. L. (1983). Discrete sequential boundaries for
#' clinical trials. Biometrika 70: 659-663.
#'
#' Jennison, C. and Turnbull, B. W. (2000). Group Sequential Methods with
#' Applications to Clinical Trials. Chapman & Hall/CRC.
#'
#' @export
#' @seealso [boundary_obf()], [boundary_pocock()], [boundary_power()]
#'
#' @examples
#' # Bayesian: Wang-Tsiatis shape ending at 0.95 threshold
#' wt <- boundary_wang_tsiatis(delta = 0.25, threshold = 0.95)
#' wt(c(0.5, 1.0))
#'
#' # Pocock-like (delta = 0.5): constant threshold
#' wt_pocock <- boundary_wang_tsiatis(delta = 0.5, threshold = 0.95)
#' wt_pocock(c(0.25, 0.5, 0.75, 1.0))  # All 0.95
#'
#' \dontrun{
#' # Frequentist: control Type I error at 2.5% (requires gsDesign)
#' wt_freq <- boundary_wang_tsiatis(delta = 0.25, alpha = 0.025)
#' wt_freq(c(0.5, 1.0))
#' }
boundary_wang_tsiatis <- function(delta = 0.25, alpha = NULL, threshold = NULL) {
  # Validate delta
  force(delta)
  if (!is.numeric(delta) || length(delta) != 1 ||
      is.na(delta) || delta < 0 || delta > 0.5) {
    cli::cli_abort(c(
      "{.arg delta} must be a single numeric value between 0 and 0.5",
      "x" = "You supplied {.val {delta}}"
    ))
  }

  # Validate: exactly one of alpha or threshold must be provided
  if (is.null(alpha) && is.null(threshold)) {
    cli::cli_abort(c(
      "Must specify either {.arg alpha} or {.arg threshold}",
      "i" = "Use {.arg alpha} for frequentist error control (e.g., 0.025)",
      "i" = "Use {.arg threshold} for Bayesian decision rules (e.g., 0.95)"
    ))
  }
  if (!is.null(alpha) && !is.null(threshold)) {
    cli::cli_abort(c(
      "Cannot specify both {.arg alpha} and {.arg threshold}",
      "i" = "Use {.arg alpha} for frequentist error control",
      "i" = "Use {.arg threshold} for Bayesian decision rules"
    ))
  }

  # Frequentist mode: alpha-spending
  if (!is.null(alpha)) {
    force(alpha)
    if (!is.numeric(alpha) || length(alpha) != 1 ||
        alpha <= 0 || alpha > 0.5) {
      cli::cli_abort(c(
        "{.arg alpha} must be a single numeric value between 0 and 0.5",
        "x" = "You supplied {.val {alpha}}"
      ))
    }

    f <- function(info_frac) {
      n_looks <- length(info_frac)

      if (!requireNamespace("gsDesign", quietly = TRUE)) {
        cli::cli_abort(c(
          "{.fn boundary_wang_tsiatis} with {.arg alpha} requires the {.pkg gsDesign} package",
          "i" = "Install it with: {.code install.packages(\"gsDesign\")}"
        ))
      }

      # Single-look early return: no group sequential structure needed
      if (n_looks == 1) {
        return(stats::pnorm(stats::qnorm(1 - alpha)))
      }

      # Special cases for exact results via Lan-DeMets spending functions
      if (delta == 0) {
        design <- gsDesign::gsDesign(
          k = n_looks, timing = info_frac,
          alpha = alpha, test.type = 1,
          sfu = gsDesign::sfLDOF
        )
        z_bounds <- design$upper$bound
      } else if (delta == 0.5) {
        design <- gsDesign::gsDesign(
          k = n_looks, timing = info_frac,
          alpha = alpha, test.type = 1,
          sfu = gsDesign::sfLDPocock
        )
        z_bounds <- design$upper$bound
      } else {
        # General case: calibrate constant C in z_k = C * t_k^(delta - 0.5)
        # via root-finding on the rejection probability under H0
        objective <- function(C) {
          z_bounds <- C * info_frac^(delta - 0.5)
          prob <- gsDesign::gsProbability(
            k = n_looks, theta = 0, n.I = info_frac,
            a = rep(-20, n_looks), b = z_bounds
          )
          sum(prob$upper$prob) - alpha
        }
        z_single <- stats::qnorm(1 - alpha)
        root <- stats::uniroot(objective,
                               interval = c(z_single * 0.1, z_single * 3),
                               tol = .Machine$double.eps^0.5)
        z_bounds <- root$root * info_frac^(delta - 0.5)
      }
      stats::pnorm(z_bounds)
    }

    attr(f, "boundary_type") <- "wang_tsiatis"
    attr(f, "boundary_params") <- list(delta = delta, alpha = alpha)

  # Bayesian mode: threshold-based
  } else {
    force(threshold)
    if (!is.numeric(threshold) || length(threshold) != 1 ||
        threshold <= 0 || threshold >= 1) {
      cli::cli_abort(c(
        "{.arg threshold} must be a single numeric value between 0 and 1 (exclusive)",
        "x" = "You supplied {.val {threshold}}"
      ))
    }

    f <- function(info_frac) {
      z_final <- stats::qnorm(threshold)
      stats::pnorm(z_final * info_frac^(delta - 0.5))
    }

    attr(f, "boundary_type") <- "wang_tsiatis"
    attr(f, "boundary_params") <- list(delta = delta, threshold = threshold)
  }

  class(f) <- c("boundary_function", "function")
  f
}


# =============================================================================
# DEPRECATED: boundary_obf_threshold and boundary_pocock_threshold
# =============================================================================
# These functions are now integrated into boundary_obf() and boundary_pocock()
# via the 'threshold' parameter. Use:
#   boundary_obf(threshold = 0.95)     instead of boundary_obf_threshold(0.95)
#   boundary_pocock(threshold = 0.95)  instead of boundary_pocock_threshold(0.95)

#' @rdname boundary_obf
#' @export
boundary_obf_threshold <- function(threshold = 0.975) {
boundary_obf(threshold = threshold)
}

#' @rdname boundary_pocock
#' @export
boundary_pocock_threshold <- function(threshold = 0.975) {
  boundary_pocock(threshold = threshold)
}


#' Resolve Threshold Value
#'
#' Internal helper to resolve a threshold specification (numeric or function)
#' to a numeric value at a given information fraction.
#'
#' @param threshold Either a numeric value or a boundary function
#' @param info_frac Information fraction between 0 and 1. For sample-size designs:
#'   `current_n / n_total`. For event-driven designs: `events_observed / events_planned`.
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
    # Pass the full vector of info_fracs at once — boundary functions like
    # boundary_obf(alpha=) use gsDesign which needs the complete schedule
    # (including the final look at info_frac=1) to compute valid thresholds.
    info_fracs <- look_info$n_analyzed / n_total
    boundary(info_fracs)
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


#' Resolve boundary to a vector using pre-computed info_fracs
#'
#' Variant of [resolve_boundary_vector()] that takes an info_fracs vector
#' directly instead of a look_info data frame. Used by sequential backends
#' that pre-compute thresholds before the analysis loop.
#'
#' @param boundary A boundary function, numeric scalar, numeric vector, or NULL.
#' @param info_fracs Numeric vector of information fractions (e.g.,
#'   `analysis_schedule / n_total`).
#' @return Numeric vector of length `length(info_fracs)`.
#' @noRd
resolve_boundary_vector_from_fracs <- function(boundary, info_fracs) {
  if (!is.numeric(info_fracs)) {
    cli::cli_abort(c(
      "'info_fracs' must be a numeric vector",
      "x" = "Got {.cls {class(info_fracs)}}"
    ))
  }
  if (any(!is.finite(info_fracs))) {
    cli::cli_abort(c(
      "Non-finite values in information fractions",
      "x" = "info_fracs contains NA, NaN, or Inf",
      "i" = "Check that 'analysis_at' and 'n_total' are valid"
    ))
  }
  n_looks <- length(info_fracs)
  if (is.null(boundary)) {
    rep(NA_real_, n_looks)
  } else if (is.function(boundary)) {
    result <- boundary(info_fracs)
    if (length(result) != n_looks) {
      cli::cli_abort(c(
        "Boundary function returned wrong length",
        "x" = "Expected {n_looks} value{?s}, got {length(result)}",
        "i" = "Boundary functions must return one threshold per information fraction"
      ))
    }
    result
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


# =============================================================================
# DISPLAY FUNCTION
# =============================================================================

#' Show Available Boundary Functions
#'
#' Displays information about the available boundary functions for sequential
#' designs. These functions can be used with `thr_dec_eff` and `thr_dec_fut` in
#' [build_conditions()] to create look-dependent stopping thresholds.
#'
#' @return Invisibly returns a character vector of boundary function names.
#'
#' @examples
#' show_boundaries()
#'
#' @export
show_boundaries <- function() {
  cli::cli_h3("Available Boundary Functions")
  cli::cli_text("")

  has_gsdesign <- requireNamespace("gsDesign", quietly = TRUE)

  # -------------------------------------------------------------------------
  # Main boundary functions
  # -------------------------------------------------------------------------
  cli::cli_text("{.strong O'Brien-Fleming} (conservative early, relaxing late):")
  cli::cli_text("  boundary_obf(alpha = 0.025)     # Frequentist: control Type I error")
  cli::cli_text("  boundary_obf(threshold = 0.95) # Bayesian: ends at 0.95 threshold")
  if (!has_gsdesign) {
    cli::cli_text("  {.field Note: Install gsDesign for exact alpha-spending boundaries}")
  }
  cli::cli_text("")

  cli::cli_text("{.strong Pocock} (similar threshold at all looks):")
  cli::cli_text("  boundary_pocock(alpha = 0.025)     # Frequentist: calibrated for error")
  cli::cli_text("  boundary_pocock(threshold = 0.95) # Bayesian: constant 0.95 at all looks")
  if (!has_gsdesign) {
    cli::cli_text("  {.field Note: Install gsDesign for exact alpha-spending boundaries}")
  }
  cli::cli_text("")

  cli::cli_text("{.strong Hwang-Shih-DeCani} (flexible one-parameter family):")
  cli::cli_text("  boundary_hsd(alpha = 0.025, gamma = -4)")
  cli::cli_text("  {.emph gamma=-4: OBF-like, gamma=1: Pocock-like, gamma=0: linear}")
  if (!has_gsdesign) {
    cli::cli_text("  {.field Requires gsDesign package}")
  }
  cli::cli_text("")

  cli::cli_text("{.strong Wang-Tsiatis} (tunable between OBF and Pocock):")
  cli::cli_text("  boundary_wang_tsiatis(delta = 0.25, alpha = 0.025)")
  cli::cli_text("  boundary_wang_tsiatis(delta = 0.25, threshold = 0.95)")
  cli::cli_text("  {.emph delta=0: OBF-like, delta=0.5: Pocock-like}")
  if (!has_gsdesign) {
    cli::cli_text("  {.field Note: Install gsDesign for alpha mode}")
  }
  cli::cli_text("")

  cli::cli_text("{.strong Linear interpolation}:")
  cli::cli_text("  boundary_linear(start = 0.99, end = 0.95)")
  cli::cli_text("  {.emph Linear change from start to end threshold}")
  cli::cli_text("")

  cli::cli_text("{.strong Power family} (flexible shape):")
  cli::cli_text("  boundary_power(base = 0.95, rho = 2)")
  cli::cli_text("  {.emph rho=2: OBF-like, rho=1: linear, rho<1: less conservative}")
  cli::cli_text("")

  cli::cli_text("{.strong Constant} (simple fixed threshold):")
  cli::cli_text("  boundary_constant(threshold = 0.95)")
  cli::cli_text("  {.emph Same threshold at all looks}")
  cli::cli_text("")

  # -------------------------------------------------------------------------
  # Example
  # -------------------------------------------------------------------------
  cli::cli_text("{.emph Example usage in build_conditions():}")
  cli::cli_verbatim(
    "  constant = list(",
    "    # Bayesian: OBF shape ending at 0.95 threshold",
    "    thr_dec_eff = boundary_obf(threshold = 0.95),",
    "    # Linear futility: lenient early (0.3), stricter late (0.5)",
    "    thr_dec_fut = boundary_linear(0.30, 0.50)",
    "  )"
  )

  invisible(c(
    "boundary_obf", "boundary_pocock", "boundary_hsd",
    "boundary_wang_tsiatis", "boundary_linear", "boundary_power",
    "boundary_constant"
  ))
}
