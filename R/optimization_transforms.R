# =============================================================================
# OPTIMIZATION TRANSFORMS: Simplex and Parameter Transforms
# =============================================================================
# Transform functions for optimization parameter spaces:
# - ILR (Isometric Log-Ratio) transforms for simplex-constrained parameters
# - Constrained simplex mapping
# - Logit/invlogit transforms for probability-scale GP modeling
# - Feasibility probability computation
# - Simplex transform application (structured and flat formats)

# =============================================================================
# LOGIT / INVLOGIT TRANSFORMS
# =============================================================================

#' Logit Transform
#'
#' Transforms probability to unbounded scale for GP modeling.
#'
#' @param p Probability value(s) in (0, 1)
#' @param eps Small value to clip away from 0 and 1 (default 1e-9)
#'
#' @return Logit-transformed value(s)
#' @keywords internal
logit_transform <- function(p, eps = 1e-9) {
  p_clipped <- pmax(pmin(p, 1 - eps), eps)
  log(p_clipped / (1 - p_clipped))
}

#' Inverse Logit Transform
#'
#' Transforms unbounded value back to probability scale.
#'
#' @param x Logit-scale value(s)
#'
#' @return Probability value(s) in (0, 1)
#' @keywords internal
invlogit_transform <- function(x) {
  1 / (1 + exp(-x))
}

#' Compute Feasibility Probability
#'
#' Computes P(power >= target) from GP predictive distribution on logit scale.
#'
#' @param mu Predictive mean on logit(power) scale
#' @param sigma Predictive standard deviation on logit(power) scale
#' @param target Target power value (e.g., 0.80)
#' @param eps Small value to avoid log(0) (default 1e-9)
#'
#' @return Probability that power >= target
#'
#' @details
#' Uses the formula:
#' ```
#' P_feas = Phi((mu - logit(target)) / sigma)
#' ```
#' where Phi is the standard normal CDF.
#'
#' @keywords internal
compute_p_feas <- function(mu, sigma, target, eps = 1e-9) {
  logit_target <- logit_transform(target, eps)
  z <- (mu - logit_target) / pmax(sigma, eps)
  stats::pnorm(z)
}

# =============================================================================
# ILR TRANSFORMS: Isometric Log-Ratio for Simplex Parameters
# =============================================================================

#' Inverse Isometric Log-Ratio Transform
#'
#' Maps unconstrained R^(k-1) coordinates back to the k-simplex
#' using the Helmert sub-matrix basis.
#'
#' @param y Numeric vector of length k-1 (unconstrained)
#'
#' @return Numeric vector of length k on the standard simplex
#'
#' @details
#' The inverse ILR maps from R^(k-1) back to the k-simplex:
#' 1. Construct the ILR basis matrix V (k x k-1)
#' 2. Compute CLR coordinates: clr = V %*% y
#' 3. Apply softmax: p = exp(clr) / sum(exp(clr))
#'
#' @references
#' Egozcue, J.J., et al. (2003). Isometric logratio transformations for
#' compositional data analysis. Mathematical Geology, 35(3), 279-300.
#'
#' @keywords internal
ilr_inverse <- function(y) {
  k_minus_1 <- length(y)
  k <- k_minus_1 + 1

  # Construct ILR basis matrix V (k x k-1)
  # Using Helmert sub-matrix construction
  V <- matrix(0, nrow = k, ncol = k_minus_1)

  for (j in seq_len(k_minus_1)) {
    # Elements 1 to j get positive value
    V[1:j, j] <- 1 / j
    # Element j+1 gets negative value
    V[j + 1, j] <- -1
    # Normalize column
    V[, j] <- V[, j] * sqrt(j / (j + 1))
  }

  # Transform to CLR (centered log-ratio) space
  clr <- as.vector(V %*% y)

  # Softmax to get simplex (with numerical stability)
  exp_clr <- exp(clr - max(clr))
  exp_clr / sum(exp_clr)
}


#' Forward Isometric Log-Ratio Transform
#'
#' Transforms simplex values to unconstrained R^(k-1) space.
#' Inverse of [ilr_inverse()].
#'
#' @param p Numeric vector of length k (simplex: positive, sums to 1)
#'
#' @return Numeric vector of length k-1 (unconstrained)
#'
#' @keywords internal
ilr_forward <- function(p) {
  k <- length(p)
  k_minus_1 <- k - 1

  # Construct ILR basis matrix V (k x k-1)
  V <- matrix(0, nrow = k, ncol = k_minus_1)

  for (j in seq_len(k_minus_1)) {
    V[1:j, j] <- 1 / j
    V[j + 1, j] <- -1
    V[, j] <- V[, j] * sqrt(j / (j + 1))
  }

  # CLR transform: log(p) - mean(log(p))
  log_p <- log(p)
  clr <- log_p - mean(log_p)

  # ILR = V' %*% clr
  as.vector(t(V) %*% clr)
}


#' Constrained Simplex Transform
#'
#' Scales a standard simplex to satisfy minimum proportion constraints.
#'
#' @param q Numeric vector on standard simplex (from ilr_inverse)
#' @param min_prop Minimum proportion for each component
#' @param k Number of components
#'
#' @return Numeric vector on constrained simplex
#'
#' @details
#' Maps standard simplex q to constrained simplex p where all p_i >= min_prop:
#' p = min_prop + (1 - k * min_prop) * q
#'
#' @keywords internal
constrained_simplex <- function(q, min_prop, k) {
  min_prop + (1 - k * min_prop) * q
}


# =============================================================================
# SIMPLEX TRANSFORM APPLICATION
# =============================================================================

#' Apply Simplex Transforms to Parameters (Structured Return)
#'
#' Transforms optimizer parameter values (ILR coordinates, bounded proportions)
#' back to the original simplex-constrained space. Returns a structured list
#' with separate components for conditions, ILR values, and simplex values.
#'
#' See also [apply_simplex_transforms_flat()] in `pareto_optimize.R` which
#' returns a flat list (no nested structure).
#'
#' @param xs Named list of optimizer values from bbotk
#' @param search_specs Named list of simplex specifications from objectives
#'
#' @return List with:
#'   - `crossed`: Named list ready for build_conditions()
#'   - `ilr_values`: Named list of original ILR coordinates (for archive)
#'   - `simplex_values`: Named list of actual proportions (for archive)
#'
#' @details
#' For 2-arm/2-look cases: optimizer directly provides bounded proportions,
#' transform applies the allocation/timing format.
#'
#' For k-arm/k-look cases: optimizer provides k-1 ILR coordinates in separate
#' parameters (e.g., p_alloc_ilr_1, p_alloc_ilr_2). This function:
#' 1. Collects ILR coordinates into a vector
#' 2. Applies inverse ILR to get unconstrained simplex
#' 3. Applies min constraint via constrained_simplex()
#' 4. Formats for build_conditions()
#'
#' @keywords internal
apply_simplex_transforms <- function(xs, search_specs) {
  crossed <- xs
  ilr_values <- list()
  simplex_values <- list()

  for (param_name in names(search_specs)) {
    spec <- search_specs[[param_name]]

    if (spec$type == "p_alloc") {
      if (spec$n_arms == 2) {
        # 2-arm: direct proportion (already bounded [min, 1-min])
        p_treat <- xs[[param_name]]
        p_ctrl <- 1 - p_treat
        simplex_values[[param_name]] <- c(p_ctrl, p_treat)
        crossed[[param_name]] <- list(c(p_ctrl, p_treat))
      } else {
        # k-arm: collect ILR coordinates and transform
        ilr_coords <- numeric(spec$n_dims)
        for (i in seq_len(spec$n_dims)) {
          ilr_name <- paste0(param_name, "_ilr_", i)
          ilr_coords[i] <- xs[[ilr_name]]
          # Remove ILR params from crossed
          crossed[[ilr_name]] <- NULL
        }
        # Inverse ILR to unconstrained simplex
        q <- ilr_inverse(ilr_coords)
        # Apply min constraint
        p <- constrained_simplex(q, spec$min_prop, spec$n_arms)

        ilr_values[[param_name]] <- ilr_coords
        simplex_values[[param_name]] <- p
        crossed[[param_name]] <- list(p)
      }

    } else if (spec$type == "looks") {
      if (spec$n_looks == 2) {
        # 2-look: direct proportion (already bounded [min_spacing, 1-min_spacing])
        interim_prop <- xs[[param_name]]
        simplex_values[[param_name]] <- interim_prop
        crossed[[param_name]] <- c(interim_prop)
      } else {
        # k-look: collect ILR coordinates and transform
        ilr_coords <- numeric(spec$n_dims)
        for (i in seq_len(spec$n_dims)) {
          ilr_name <- paste0(param_name, "_ilr_", i)
          ilr_coords[i] <- xs[[ilr_name]]
          crossed[[ilr_name]] <- NULL
        }
        # Inverse ILR on increments (form k-simplex)
        increments <- ilr_inverse(ilr_coords)
        # Scale increments to satisfy min_spacing constraint
        k <- spec$n_looks
        min_spacing <- spec$min_spacing
        scaled_increments <- constrained_simplex(increments, min_spacing, k)
        # Cumulative sum gives look timings (exclude last which is 1.0)
        look_times <- cumsum(scaled_increments)[-k]

        ilr_values[[param_name]] <- ilr_coords
        simplex_values[[param_name]] <- look_times
        crossed[[param_name]] <- look_times
      }
    }
  }

  list(crossed = crossed,
       ilr_values = ilr_values,
       simplex_values = simplex_values)
}
