# =============================================================================
# OPTIMIZATION INTERNAL: mlr3mbo Integration
# =============================================================================
# Internal functions for Bayesian optimization using mlr3mbo/bbotk:
# - run_optimization(): Main orchestration
# - create_objective_fn(): Wrapper for power analysis as objective
# - setup_mbo_components(): Configure surrogate, acquisition, optimizer
# - Search type transforms for simplex-constrained parameters

# =============================================================================
# FIDELITY SCHEDULE: Progressive n_sims Escalation
# =============================================================================
# Multi-fidelity optimization where n_sims increases in discrete steps.
# Early evaluations use low n_sims (cheap exploration), later evaluations
# use high n_sims (precise refinement).

#' Build n_sims Schedule for Progressive Fidelity
#'
#' Creates a mapping from evaluation number to n_sims for multi-fidelity
#' optimization. Supports both constant fidelity (scalar n_sims) and
#' progressive fidelity (vector n_sims with evals_per_step).
#'
#' @param n_sims Scalar or vector of simulation counts. If a vector,
#'   n_sims increases in discrete steps (e.g., `c(100, 200, 500, 1000)`).
#' @param max_evals Total evaluations (used when n_sims is scalar).
#' @param evals_per_step Controls evaluations per fidelity level when n_sims is
#'   a vector. Can be:
#'   - **Scalar** (default 10): Same number of evaluations at each level.
#'     Total evals = `length(n_sims) * evals_per_step`.
#'   - **Vector**: Custom evaluations per level. Must have same length as n_sims.
#'     Example: `c(15, 10, 10, 15)` means 15 evals at level 1, 10 at level 2, etc.
#'     Total evals = `sum(evals_per_step)`.
#'
#' @return Data frame with columns:
#'   - `eval`: Evaluation number (1 to total_evals)
#'   - `n_sims`: Number of simulations for that evaluation
#'   - `fidelity_level`: Which fidelity level (1 to length(n_sims))
#'   - `is_progressive`: Whether this is progressive fidelity
#'
#' @details
#' **Constant fidelity** (scalar `n_sims`):
#' All evaluations use the same n_sims. Total evaluations = `max_evals`.
#'
#' **Progressive fidelity with scalar `evals_per_step`**:
#' Equal number of evaluations per level.
#' Example: `n_sims = c(100, 200, 500, 1000)`, `evals_per_step = 10` -> 40 total
#'
#' **Progressive fidelity with vector `evals_per_step`**:
#' Custom number of evaluations per level.
#' Example: `n_sims = c(100, 200, 500, 1000)`, `evals_per_step = c(15, 10, 10, 15)`
#' - Evals 1-15: n_sims = 100 (15 evals)
#' - Evals 16-25: n_sims = 200 (10 evals)
#' - Evals 26-35: n_sims = 500 (10 evals)
#' - Evals 36-50: n_sims = 1000 (15 evals)
#'
#' @keywords internal
build_fidelity_schedule <- function(n_sims, max_evals, evals_per_step = 10) {
  if (length(n_sims) == 1) {
    # Constant fidelity (backward compatible)
    data.frame(
      eval = seq_len(max_evals),
      n_sims = rep(n_sims, max_evals),
      fidelity_level = rep(1L, max_evals),
      is_progressive = FALSE
    )
  } else if (length(evals_per_step) > 1) {
    # Progressive fidelity with custom evals per level (vector evals_per_step)
    n_levels <- length(n_sims)
    total_evals <- sum(evals_per_step)
    
    # Build schedule from vector evals_per_step
    schedule <- rep(n_sims, times = evals_per_step)
    levels <- rep(seq_len(n_levels), times = evals_per_step)
    
    data.frame(
      eval = seq_len(total_evals),
      n_sims = schedule,
      fidelity_level = levels,
      is_progressive = TRUE
    )
  } else {
    # Progressive fidelity: uniform evals_per_step at each level
    n_levels <- length(n_sims)
    total_evals <- n_levels * evals_per_step
    schedule <- rep(n_sims, each = evals_per_step)
    levels <- rep(seq_len(n_levels), each = evals_per_step)
    data.frame(
      eval = seq_len(total_evals),
      n_sims = schedule,
      fidelity_level = levels,
      is_progressive = TRUE
    )
  }
}

# =============================================================================
# PROBABILISTIC CONSTRAINT HELPERS
# =============================================================================
# Functions for probabilistic constraint-based optimization:
# - Model logit(power) with GP surrogate
# - Compute P(power >= target) from predictive distribution
# - Minimize n subject to feasibility constraint P_feas >= alpha

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

#' Cost Function: Hard Constraint
#'
#' Returns n if P_feas >= alpha, otherwise returns a large penalty.
#' Used for strict "never undercut target" optimization.
#'
#' @param n Sample size
#' @param p_feas Feasibility probability P(power >= target)
#' @param alpha Required confidence level (e.g., 0.999)
#' @param M Penalty multiplier (default 1e6)
#'
#' @return Cost value (n if feasible, n + M*(1-p_feas) otherwise)
#' @keywords internal
cost_hard_constraint <- function(n, p_feas, alpha = 0.999, M = 1e6) {
  violation <- pmax(0, alpha - p_feas)
  n + M * violation
}

#' Cost Function: Soft Penalty
#'
#' Continuous cost function that penalizes low feasibility probability.
#'
#' @param n Sample size
#' @param p_feas Feasibility probability P(power >= target)
#' @param M Penalty multiplier (default 1e6)
#'
#' @return Cost value: n + M * max(0, 1 - p_feas)
#' @keywords internal
cost_soft_penalty <- function(n, p_feas, M = 1e6) {
  n + M * pmax(0, 1 - p_feas)
}

#' Cost Function: Risk-Aware Ratio
#'
#' Smooth penalty using n / p_feas^kappa formulation.
#' Low p_feas -> huge cost; p_feas near 1 -> cost approx n.
#'
#' @param n Sample size
#' @param p_feas Feasibility probability P(power >= target)
#' @param kappa Exponent controlling penalty steepness (default 2)
#' @param eps Small value to avoid division by zero (default 1e-9)
#'
#' @return Cost value: n / p_feas^kappa
#' @keywords internal
cost_risk_ratio <- function(n, p_feas, kappa = 2, eps = 1e-9) {
  n / pmax(p_feas, eps)^kappa
}

#' Cost Function: Logit Distance
#'
#' Symmetric cost function using logit-space distance from target.
#' Formula: `f(x) = 1 - invlogit(abs(logit(power) - logit(target)))`
#'
#' @param power Achieved power value in (0, 1)
#' @param target Target power value (e.g., 0.80)
#' @param eps Small value for numerical stability (default 1e-9)
#'
#' @return Cost value in \[0, 0.5\], maximum at target
#'
#' @details
#' Properties:
#' - At target: `abs(0) = 0`, `invlogit(0) = 0.5`, cost = 0.5 (maximum)
#' - Away from target: `abs(diff) > 0`, `invlogit > 0.5`, cost < 0.5
#' - Symmetric: same penalty for under/overshoot at same logit distance
#' - Smooth and differentiable everywhere in (0, 1)
#'
#' | Power | Target | Cost Value |
#' |-------|--------|------------|
#' | 0.80  | 0.80   | 0.500 (max)|
#' | 0.75  | 0.80   | 0.428      |
#' | 0.85  | 0.80   | 0.428      |
#' | 0.70  | 0.80   | 0.357      |
#' | 0.50  | 0.80   | 0.200      |
#'
#' @keywords internal
cost_logit_distance <- function(power, target, eps = 1e-9) {
  logit_power <- logit_transform(power, eps)
  logit_target <- logit_transform(target, eps)
  logit_diff <- abs(logit_power - logit_target)
  1 - invlogit_transform(logit_diff)
}

#' Standardize Input for GP
#'
#' Standardizes log-transformed sample size for GP modeling.
#'
#' @param n Sample size value(s)
#' @param n_bounds Bounds c(lower, upper) for n
#'
#' @return List with:
#'   - `value`: Standardized log(n) value
#'   - `mean`: Mean of log(n) range (for back-transform)
#'   - `sd`: SD of log(n) range (for back-transform)
#'
#' @keywords internal
standardize_log_n <- function(n, n_bounds) {
  log_n <- log(n)
  log_bounds <- log(n_bounds)
  mean_log <- mean(log_bounds)
  sd_log <- diff(log_bounds) / 4  # Approx SD covering 95% of range

  list(
    value = (log_n - mean_log) / sd_log,
    mean = mean_log,
    sd = sd_log
  )
}

#' Unstandardize Log-n
#'
#' Converts standardized log(n) back to original n scale.
#'
#' @param z Standardized value
#' @param mean_log Mean used for standardization
#' @param sd_log SD used for standardization
#'
#' @return Sample size on original scale
#' @keywords internal
unstandardize_log_n <- function(z, mean_log, sd_log) {
  exp(z * sd_log + mean_log)
}


#' Compute Observation Weight from n_sims
#'
#' Computes observation weights for surrogate model fitting based on
#' the precision of each evaluation (n_sims used).
#'
#' @param n_sims_used Vector of n_sims values used for each observation.
#' @param n_sims_max Maximum n_sims in the schedule (for normalization).
#'
#' @return Numeric vector of weights in (0, 1].
#'
#' @details
#' Weight = n_sims_used / n_sims_max
#'
#' This gives higher weight to observations with more simulations
#' (more precise estimates), helping the surrogate learn from
#' multi-fidelity data appropriately.
#'
#' @keywords internal
compute_fidelity_weights <- function(n_sims_used, n_sims_max) {
  n_sims_used / n_sims_max
}

# =============================================================================
# SEARCH TYPE REGISTRY: Parameter Transforms
# =============================================================================
# Handles transformation of optimizer values to condition-compatible formats.
# Key use cases:
# - p_alloc: Allocation probabilities (simplex constraint, sum to 1)
# - analysis_at: Interim look timings (ordered, final = 1)
#
# Convention for p_alloc:
# - Always k entries (k = n_arms): [p_ctrl, p_treat1, p_treat2, ...]
#
# For optimization, the search space is k-1 dimensional (ILR transform),
# but the output is always a full k-entry vector.

#' Get Search Parameter Transform
#'
#' Returns a transform function for converting optimizer values to the format
#' expected by [build_conditions()]. Handles simplex-constrained parameters
#' like allocation probabilities and interim look timings.
#'
#' @param type Transform type: "scalar" (default), "alloc_2arm", "alloc_3arm",
#'   "alloc_karm", "look_1interim", "look_2interim", or a custom function.
#' @param n_arms Number of arms (used for allocation transforms)
#' @param n_looks Number of looks (used for interim transforms)
#'
#' @return A function that transforms optimizer value(s) to condition format
#'
#' @details
#' **Allocation transforms** (`p_alloc`):
#'
#' The optimizer searches over treatment allocation probabilities. The transform
#' produces the full allocation vector with control probability derived.
#'
#' Convention: always k entries `[p_ctrl, p_treat1, p_treat2, ...]`.
#' The search space is k-1 dimensional (ILR transform), but the output
#' is always a full k-entry allocation vector.
#'
#' | n_arms | Search dims | Transform |
#' |--------|-------------|-----------|
#' | 2 | 1 scalar | `x -> list(c(1-x, x))` |
#' | 3 | 2 scalars | `x -> list(c(1-sum(x), x[1], x[2]))` |
#' | k | k-1 scalars | Stick-breaking to ensure valid simplex |
#'
#' **Interim look transforms** (`analysis_at`):
#'
#' The optimizer searches over interim timing(s). The transform produces
#' the full `analysis_at` vector with final look at 1.0 appended.
#'
#' | n_looks | Search dims | Transform |
#' |---------|-------------|-----------|
#' | 2 | 1 scalar | `x -> c(x, 1.0)` |
#' | 3 | 2 scalars | Stick-breaking on increments |
#'
#' @keywords internal
#' @seealso [infer_search_type()], [apply_search_transforms()]
get_search_transform <- function(type,
                                 n_arms = NULL,
                                 n_looks = NULL) {
  # Custom function: return as-is
  
  if (is.function(type)) {
    return(type)
  }
  
  # Built-in transforms
  transforms <- list(
    # ==========================================================================
    # SCALAR: No transformation (default)
    # ==========================================================================
    scalar = function(x)
      x,
    
    # ==========================================================================
    # ALLOCATION: 2-arm trial
    # ==========================================================================
    # Search: single scalar p_treat in (0, 1)
    # Output: list(c(p_ctrl, p_treat)) wrapped for build_conditions
    alloc_2arm = function(x) {
      list(c(1 - x, x))
    },
    
    # ==========================================================================
    # ALLOCATION: 3-arm trial (ILR transform)
    # ==========================================================================
    # Search: 2 unbounded scalars in R^2
    # Output: list(c(p_ctrl, p_treat1, p_treat2)) via inverse ILR
    alloc_3arm = function(x) {
      list(ilr_inverse(x))
    },
    
    # ==========================================================================
    # ALLOCATION: k-arm trial (general ILR transform)
    # ==========================================================================
    # Search: k-1 unbounded scalars in R^(k-1)
    # Output: list(c(p_ctrl, p_treat1, ..., p_treat_{k-1})) via inverse ILR
    alloc_karm = function(x) {
      list(ilr_inverse(x))
    },
    
    # ==========================================================================
    # INTERIM LOOKS: Single interim (2 looks total)
    # ==========================================================================
    # Search: single scalar for interim timing proportion
    # Output: c(interim_prop) - final 1.0 auto-appended by build_conditions
    look_1interim = function(x) {
      c(x)
    },
    
    # ==========================================================================
    # INTERIM LOOKS: Two interims (3 looks total)
    # ==========================================================================
    # Search: 2 unbounded scalars, ILR on increments to ensure ordering
    # Output: c(t1, t2) where 0 < t1 < t2 < 1 (final 1.0 auto-appended by build_conditions)
    look_2interim = function(x) {
      # ILR inverse on increments (which form a 3-simplex)
      increments <- ilr_inverse(x)
      # Cumulative sum gives look timings, exclude final (= 1.0)
      cumsum(increments)[-length(increments)]
    }
  )
  
  if (!type %in% names(transforms)) {
    cli::cli_abort(
      c(
        "Unknown search type: {.val {type}}",
        "i" = "Valid types: {.val {names(transforms)}}",
        "i" = "Or provide a custom transform function"
      )
    )
  }
  
  transforms[[type]]
}


# =============================================================================
# ILR TRANSFORMS: Isometric Log-Ratio
# =============================================================================
# ILR provides isometric (distance-preserving) mapping between simplex and R^(k-1).
# Used for optimizing over simplex-constrained parameters (allocations, increments).

#' Inverse Isometric Log-Ratio Transform
#'
#' Transforms unconstrained R^(k-1) values to a k-dimensional simplex.
#' Used for optimizing over simplex-constrained parameters like allocation
#' probabilities.
#'
#' @param y Numeric vector of length k-1 (unconstrained values)
#'
#' @return Numeric vector of length k (simplex: non-negative, sums to 1)
#'
#' @details
#' The ILR transform provides an isometric (distance-preserving) mapping
#' between the simplex and Euclidean space. This is preferred over simpler
#' transforms like stick-breaking because:
#' \itemize{
#'   \item Isometric: distances in R^(k-1) correspond to distances on simplex
#'   \item Symmetric: no arbitrary ordering of components
#'   \item Statistically principled for compositional data
#' }
#'
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
# PARAMETER TRANSFORM REGISTRY: Log/Logit/Identity Transforms
# =============================================================================
# Automatic transforms for different parameter types to improve GP fitting:
# - n_*, n_total: log transform (sample sizes)
# - thr_dec_*, pwr_*, pr_*: logit transform (probabilities)
# - Other: identity transform

#' Get Parameter Transform Specification
#'
#' Returns transform functions for a parameter based on naming patterns.
#' Used for automatic parameter transformations in optimization.
#'
#' @param param_name Parameter name (e.g., "n_total", "thr_dec_eff")
#' @param bounds Original bounds c(lower, upper)
#'
#' @return List with:
#'   - `type`: Transform type name ("log", "logit", "identity")
#'   - `forward_fn`: Function to transform to search space
#'   - `inverse_fn`: Function to transform back to original scale
#'   - `transform_bounds`: Function to transform bounds
#'
#' @details
#' Transform rules:
#' - Sample size parameters (`n_*`, `n_total`): log transform
#' - Probability parameters (`thr_dec_*`, `pwr_*`, `pr_*`): logit transform
#' - Other parameters: identity (no transform)
#'
#' Log transform improves GP fitting for sample sizes because the power-n
#' relationship is more linear on log scale.
#'
#' Logit transform improves GP fitting for probabilities because it maps
#' the bounded (0, 1) interval to unbounded R.
#'
#' @keywords internal
get_param_transform <- function(param_name, bounds) {
  # Sample size parameters: log transform
  if (grepl("^n_|^n$|_n$|n_total", param_name, ignore.case = TRUE)) {
    return(list(
      type = "log",
      forward_fn = function(x) log(x),
      inverse_fn = function(y) exp(y),
      transform_bounds = function(b) log(b)
    ))
  }

  # Probability/threshold parameters: logit transform
  # Match thr_dec_*, pwr_*, pr_*, but NOT p_alloc (handled separately)
  if (grepl("^thr_dec_|^pwr_|^pr_|^p_(?!alloc)", param_name, perl = TRUE)) {
    return(list(
      type = "logit",
      forward_fn = function(x) logit_transform(x),
      inverse_fn = function(y) invlogit_transform(y),
      transform_bounds = function(b) logit_transform(b)
    ))
  }

  # Default: identity transform (effect sizes, thresholds, etc.)
  list(
    type = "identity",
    forward_fn = function(x) x,
    inverse_fn = function(y) y,
    transform_bounds = function(b) b
  )
}


#' Apply Simplex Transforms to Optimizer Values
#'
#' Transforms optimizer values (which may include ILR coordinates for simplex
#' parameters) to the format expected by [build_conditions()].
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
        crossed[[param_name]] <- c(interim_prop)  # final 1.0 auto-appended by build_conditions
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
        crossed[[param_name]] <- look_times  # final 1.0 auto-appended by build_conditions
      }
    }
  }
  
  list(crossed = crossed,
       ilr_values = ilr_values,
       simplex_values = simplex_values)
}


#' Check Surrogate-Based Early Stopping Conditions
#'
#' Queries the surrogate model for predictions and checks if both stopping
#' conditions are met:
#' 1. Precision: Predictive SE at best point <= min_delta / 2
#' 2. Stability: Predicted mean hasn't changed meaningfully over patience iterations
#'
#' @param surr mlr3mbo surrogate model
#' @param archive bbotk archive object
#' @param search_params Character vector of search parameter names
#' @param pred_history List tracking prediction history (modified in place)
#' @param min_delta Minimum delta for meaningful change
#' @param patience Number of stable iterations required
#' @param target_val Target value for the objective
#' @param obj_name Name of the objective being optimized
#' @param verbosity Verbosity level
#'
#' @return List with:
#'   - `should_stop`: Logical, whether to stop
#'   - `pred_mean`: Current predicted mean at best point
#'   - `pred_se`: Current predictive SE at best point
#'   - `actual_power`: Actual power at best predicted point
#'   - `precision_ok`: Logical, whether SE is below threshold
#'   - `stability_ok`: Logical, whether predictions have stabilized
#'   - `stable_count`: Integer, consecutive stable iterations
#'   - `reason`: Character, reason for stopping (if any)
#'
#' @keywords internal
check_surrogate_stopping <- function(surr,
                                     archive,
                                     search_params,
                                     pred_history,
                                     min_delta,
                                     patience,
                                     target_val,
                                     obj_name,
                                     verbosity) {
  # Update surrogate with latest data
  # Suppress data.table warnings from mlr3mbo internals
  suppressWarnings(surr$update())
  
  # Get archive data
  archive_df <- as.data.frame(archive$data)
  if (nrow(archive_df) == 0) {
    return(list(
      should_stop = FALSE,
      pred_mean = NA,
      pred_se = NA,
      reason = NULL
    ))
  }
  
  # Get search param columns (handle ILR params)
  domain_ids <- archive$search_space$ids()
  # Use intersect to handle edge cases where columns might not exist
  domain_ids <- intersect(domain_ids, names(archive_df))
  xdt <- data.table::as.data.table(archive_df[, domain_ids, drop = FALSE])
  
  # Get surrogate predictions
  # Suppress data.table warnings from mlr3mbo internals
  pred <- suppressWarnings(surr$predict(xdt))
  pred_mean_vec <- pred$mean
  pred_se_vec <- if ("se" %in% names(pred))
    pred$se
  else
    sqrt(pred$var)
  
  # Find best predicted point (highest mean for maximization)
  best_idx <- which.max(pred_mean_vec)
  pred_mean <- pred_mean_vec[best_idx]
  pred_se <- pred_se_vec[best_idx]
  
  # Get actual power at best predicted point (for threshold validation)
  actual_col <- paste0("actual_", obj_name)
  if (actual_col %in% names(archive_df)) {
    actual_power <- archive_df[[actual_col]][best_idx]
  } else if (obj_name %in% names(archive_df)) {
    actual_power <- archive_df[[obj_name]][best_idx]
  } else {
    actual_power <- NA
  }
  
  # Condition 1: Precision - SE must be small enough
  precision_threshold <- min_delta / 2
  precision_ok <- !is.na(pred_se) && pred_se <= precision_threshold
  
  # Condition 2: Stability - predicted mean not moving meaningfully
  # "Meaningful" is calibrated to uncertainty: max(min_delta, 2 * current_se)
  effective_threshold <- max(min_delta, 2 * pred_se)
  
  # Update prediction history
  n_history <- length(pred_history$means)
  pred_history$means <- c(pred_history$means, pred_mean)
  pred_history$ses <- c(pred_history$ses, pred_se)
  
  # Check stability: has predicted mean been stable for patience iterations?
  stable_count <- 0
  if (n_history >= 1) {
    for (i in seq(from = n_history, to = max(1, n_history - patience + 1))) {
      change <- abs(pred_mean - pred_history$means[i])
      if (change < effective_threshold) {
        stable_count <- stable_count + 1
      } else {
        break
      }
    }
  }
  stability_ok <- stable_count >= patience
  
  # Validate against actual values: actual power should meet target
  actual_target_met <- !is.na(actual_power) &&
    actual_power >= target_val
  
  # Both conditions must hold + actual value validation
  should_stop <- precision_ok && stability_ok && actual_target_met
  
  reason <- NULL
  if (should_stop) {
    reason <- sprintf(
      "Surrogate converged: pred=%.3f (SE=%.4f < %.4f), stable for %d iters, actual=%.3f >= %.3f",
      pred_mean,
      pred_se,
      precision_threshold,
      stable_count,
      actual_power,
      target_val
    )
  }
  
  list(
    should_stop = should_stop,
    pred_mean = pred_mean,
    pred_se = pred_se,
    actual_power = actual_power,
    precision_ok = precision_ok,
    stability_ok = stability_ok,
    stable_count = stable_count,
    reason = reason
  )
}


#' Add Simplex Values to Archive
#'
#' Adds simplex values (and optionally ILR coordinates) to objective results
#' for archive storage.
#'
#' @param obj_values Named list of objective values to return
#' @param simplex_values Named list of simplex proportions
#' @param ilr_values Named list of ILR coordinates (for k>2 dimensions)
#' @param search_specs Named list of simplex specifications
#'
#' @return Modified obj_values with simplex/ILR columns added
#'
#' @keywords internal
add_simplex_to_archive <- function(obj_values,
                                   simplex_values,
                                   ilr_values,
                                   search_specs) {
  for (param_name in names(simplex_values)) {
    spec <- search_specs[[param_name]]
    simplex_val <- simplex_values[[param_name]]
    
    if (spec$n_dims > 1 && param_name %in% names(ilr_values)) {
      # Multi-dimensional: store as list columns
      obj_values[[paste0(param_name, "_ilr")]] <- list(ilr_values[[param_name]])
      obj_values[[param_name]] <- list(simplex_val)
    } else {
      # Single dimension: store as scalar or simple vector
      if (length(simplex_val) == 1) {
        obj_values[[param_name]] <- simplex_val
      } else {
        obj_values[[param_name]] <- list(simplex_val)
      }
    }
  }
  
  obj_values
}


#' Infer Search Parameter Type from Context
#'
#' Automatically determines the appropriate transform type for a search
#' parameter based on its name and the design properties.
#'
#' @param param_name Name of the search parameter
#' @param design rctbp_design object
#' @param bounds The search bounds for this parameter
#'
#' @return Character string: transform type name
#'
#' @details
#' Auto-detection rules:
#' \itemize{
#'   \item `p_alloc`: Uses `alloc_2arm`, `alloc_3arm`, or `alloc_karm`
#'     based on `design@n_arms`
#'   \item `interim_at`, `look_timing`: Uses `look_1interim` or `look_2interim`
#'     based on bounds dimensionality
#'   \item All others: `scalar` (no transform)
#' }
#'
#' @keywords internal
infer_search_type <- function(param_name, design, bounds) {
  n_arms <- design@n_arms
  
  # ============================================================================
  # p_alloc: Allocation probabilities
  # ============================================================================
  if (param_name == "p_alloc") {
    if (n_arms == 2) {
      return("alloc_2arm")
    } else if (n_arms == 3) {
      return("alloc_3arm")
    } else {
      return("alloc_karm")
    }
  }
  
  # ============================================================================
  # Interim look timing parameters
  # ============================================================================
  if (param_name %in% c("interim_at", "look_timing", "interim_prop")) {
    # Determine number of looks from bounds structure
    if (is.matrix(bounds)) {
      n_interims <- nrow(bounds)
    } else {
      n_interims <- 1
    }
    
    if (n_interims == 1) {
      return("look_1interim")
    } else if (n_interims == 2) {
      return("look_2interim")
    } else {
      cli::cli_warn(
        c(
          "Auto-detection for {n_interims} interim looks not supported",
          "i" = "Provide custom transform via {.arg search_types}"
        )
      )
      return("scalar")
    }
  }
  
  # ============================================================================
  # Default: scalar (no transform)
  # ============================================================================
  "scalar"
}


#' Apply Search Parameter Transforms
#'
#' Transforms optimizer values to the format expected by [build_conditions()].
#' Called within the objective function before building conditions.
#'
#' @param xs Named list of optimizer values (from paradox/bbotk)
#' @param search_types Named list mapping parameter names to transform types
#' @param design rctbp_design object (for n_arms, etc.)
#'
#' @return Named list with transformed values
#'
#' @keywords internal
apply_search_transforms <- function(xs, search_types, design) {
  result <- xs
  
  for (param_name in names(search_types)) {
    if (param_name %in% names(xs)) {
      type_spec <- search_types[[param_name]]
      
      # Get the transform function
      transform_fn <- get_search_transform(type = type_spec, n_arms = design@n_arms)
      
      # Apply transform
      result[[param_name]] <- transform_fn(xs[[param_name]])
    }
  }
  
  result
}


#' Derive Search Types from Specification
#'
#' For each search parameter, determines the appropriate transform type
#' either from explicit `search_types` or by auto-inference.
#'
#' @param search Named list of search bounds
#' @param search_types User-provided type specifications (can be partial)
#' @param design rctbp_design object
#'
#' @return Complete named list of search types for all parameters
#'
#' @keywords internal
derive_search_types <- function(search, search_types, design) {
  result <- list()
  
  for (param_name in names(search)) {
    if (param_name %in% names(search_types)) {
      # User explicitly specified
      result[[param_name]] <- search_types[[param_name]]
    } else {
      # Auto-infer from context
      result[[param_name]] <- infer_search_type(param_name = param_name,
                                                design = design,
                                                bounds = search[[param_name]])
    }
  }
  
  result
}



# =============================================================================
# MAIN INTERNAL: run_optimization()
# =============================================================================

#' Run Bayesian Optimization
#'
#' Internal function that executes the optimization using mlr3mbo.
#'
#' @param result rctbp_optimization_result object to populate
#' @param objectives rctbp_objectives specification
#' @param fidelity_schedule Data frame from build_fidelity_schedule() with
#'   columns: eval, n_sims, fidelity_level, is_progressive
#' @param max_evals Maximum evaluations (derived from fidelity_schedule)
#' @param patience Iterations without improvement before early stopping
#' @param min_delta Minimum power above target for early stopping
#' @param n_cores Parallel cores for power analysis
#' @param surrogate Surrogate model type
#' @param acq_function Acquisition function
#' @param penalty Penalty type for target optimization
#' @param penalty_alpha Confidence level for hard constraint penalty
#' @param penalty_kappa Exponent for ratio penalty
#' @param eic_kappa Conservatism parameter for EIC acquisition function (0 = neutral)
#' @param init_design Initial design method
#' @param init_design_size Initial design size
#' @param bf_args BayesFlow-specific arguments for power_analysis
#' @param brms_args brms-specific arguments for power_analysis
#' @param refresh How often to print progress (0 = never)
#' @param verbosity Output level
#'
#' @return Updated rctbp_optimization_result
#' @keywords internal
run_optimization <- function(result,
                             objectives,
                             fidelity_schedule,
                             max_evals,
                             patience,
                             min_delta,
                             sims_final_run,
                             trim_param_space,
                             profile_resolution,
                             max_final_evals,
                             n_cores,
                             surrogate,
                             acq_function,
                             penalty = "none",
                             penalty_alpha = 0.95,
                             penalty_kappa = 2,
                             eic_kappa = 0,
                             init_design,
                             init_design_size,
                             bf_args,
                             brms_args,
                             refresh,
                             verbosity) {
  start_time <- Sys.time()
  
  # Set verbosity
  old_verbosity <- set_verbosity(verbosity)
  on.exit(set_verbosity(old_verbosity), add = TRUE)
  
  # ===========================================================================
  # CONTROL mlr3/bbotk LOGGING
  # ===========================================================================
  # Suppress mlr3/bbotk logger output based on verbosity level
  # mlr3 uses lgr package for logging with named loggers
  if (requireNamespace("lgr", quietly = TRUE)) {
    # Get all mlr3-related loggers
    mlr3_logger <- lgr::get_logger("mlr3")
    bbotk_logger <- lgr::get_logger("bbotk")
    mlr3mbo_logger <- lgr::get_logger("mlr3mbo")
    
    old_mlr3_threshold <- mlr3_logger$threshold
    old_bbotk_threshold <- bbotk_logger$threshold
    old_mlr3mbo_threshold <- mlr3mbo_logger$threshold
    on.exit({
      mlr3_logger$set_threshold(old_mlr3_threshold)
      bbotk_logger$set_threshold(old_bbotk_threshold)
      mlr3mbo_logger$set_threshold(old_mlr3mbo_threshold)
    }, add = TRUE)
    
    if (verbosity <= 1) {
      # Suppress all mlr3/bbotk/mlr3mbo output
      mlr3_logger$set_threshold("off")
      bbotk_logger$set_threshold("off")
      mlr3mbo_logger$set_threshold("off")
    } else {
      # Verbose mode: show only errors (suppress warnings like data.table NSE issues)
      # Use "error" threshold to suppress data.table warnings from mlr3mbo internals
      mlr3_logger$set_threshold("error")
      bbotk_logger$set_threshold("error")
      mlr3mbo_logger$set_threshold("error")
    }
  }

  # ===========================================================================
  # SUPPRESS DATA.TABLE WARNINGS
  # ===========================================================================
  # Suppress data.table warnings that can occur from mlr3mbo/bbotk internals
  # when accessing archive columns. This is safe because we validate columns
  # explicitly in our code.
  old_dt_verbose <- getOption("datatable.verbose")
  old_dt_print <- getOption("datatable.print.class")
  options(datatable.verbose = FALSE, datatable.print.class = FALSE)
  on.exit({
    options(datatable.verbose = old_dt_verbose, datatable.print.class = old_dt_print)
  }, add = TRUE)

  # ===========================================================================
  # DISPLAY CONFIGURATION
  # ===========================================================================
  # Extract fidelity info
  is_progressive <- fidelity_schedule$is_progressive[1]
  n_sims_levels <- unique(fidelity_schedule$n_sims)
  n_sims_max <- max(n_sims_levels)
  evals_per_level <- table(fidelity_schedule$fidelity_level)
  
  if (should_show(1)) {
    cli::cli_h3("Bayesian Optimization Configuration")
    opt_type <- objectives@objective_info$optimization_type
    cli::cli_dl(
      c(
        "Type" = opt_type,
        "Search parameters" = paste(names(objectives@search), collapse = ", "),
        "Max evaluations" = max_evals,
        "Backend" = objectives@design@backend
      )
    )
    
    # Show fidelity info
    if (is_progressive) {
      n_levels <- length(n_sims_levels)
      evals_per <- evals_per_level[1]  # Assuming uniform
      fidelity_str <- paste(n_sims_levels, collapse = "\u2192")
      cli::cli_dl(c(
        "Progressive fidelity" = paste0(
          fidelity_str,
          " sims (",
          evals_per,
          " evals each, ",
          max_evals,
          " total)"
        )
      ))
    } else {
      cli::cli_dl(c("Simulations per eval" = n_sims_max))
    }
    
    # Show early stopping info for target optimization
    if (opt_type == "target" && !is.null(patience)) {
      cli::cli_dl(c(
        "Early stopping" = paste0("patience=", patience, ", min_delta=", min_delta)
      ))
    }
    cli::cli_text("")
  }
  
  # ===========================================================================
  # CREATE PARAMETER SPACE (paradox)
  # ===========================================================================
  domain <- create_parameter_space(objectives@search, objectives@search_specs)
  
  # ===========================================================================
  # CREATE CODOMAIN (objectives)
  # ===========================================================================
  codomain <- create_codomain(objectives)
  
  # ===========================================================================
  # CREATE OBJECTIVE FUNCTION (with fidelity schedule)
  # ===========================================================================
  obj_fn_result <- create_objective_fn(
    objectives = objectives,
    effective_search = objectives@search,
    fidelity_schedule = fidelity_schedule,
    max_evals = max_evals,
    patience = patience,
    min_delta = min_delta,
    penalty = penalty,
    penalty_alpha = penalty_alpha,
    penalty_kappa = penalty_kappa,
    n_cores = n_cores,
    bf_args = bf_args,
    brms_args = brms_args,
    refresh = refresh,
    verbosity = verbosity
  )
  # Extract the actual objective function (obj_fn_result also has get_best_pa())
  obj_fn <- obj_fn_result$fn
  
  # ===========================================================================
  # SET UP BBOTK INSTANCE
  # ===========================================================================
  opt_type <- objectives@objective_info$optimization_type
  
  if (opt_type == "multi") {
    # Multi-objective optimization
    objective <- bbotk::ObjectiveRFun$new(fun = obj_fn,
                                          domain = domain,
                                          codomain = codomain)
    
    instance <- bbotk::OptimInstanceBatchMultiCrit$new(objective = objective,
                                                       terminator = bbotk::trm("evals", n_evals = max_evals))
  } else {
    # Single-objective or target optimization
    objective <- bbotk::ObjectiveRFun$new(fun = obj_fn,
                                          domain = domain,
                                          codomain = codomain)
    
    instance <- bbotk::OptimInstanceBatchSingleCrit$new(objective = objective,
                                                        terminator = bbotk::trm("evals", n_evals = max_evals))
  }

  # ===========================================================================
  # PREPARE INITIAL DESIGN
  # ===========================================================================
  if (is.null(init_design_size)) {
    init_design_size <- 4 * length(objectives@search)
  }
  
  # Generate initial design
  if (should_show(1)) {
    cli::cli_alert_info("Generating initial design ({init_design_size} points)")
  }
  init_design_pts <- generate_initial_design(domain = domain,
                                             n = init_design_size,
                                             method = init_design)
  
  # ===========================================================================
  # RUN OPTIMIZATION WITH SURROGATE-BASED EARLY STOPPING
  # ===========================================================================
  if (should_show(1)) {
    cli::cli_alert_info("Running Bayesian optimization...")
    if (should_show(2)) {
      cli::cli_text("  Surrogate: {surrogate}, Acquisition: {acq_function}")
    }
  }
  
  # Suppress data.table auto-printing (bbotk uses data.table internally)
  if (verbosity < 2) {
    old_dt_opts <- options(
      datatable.print.nrows = 0,
      datatable.print.topn = 0,
      datatable.print.class = FALSE,
      datatable.print.keys = FALSE,
      datatable.print.trunc.cols = TRUE,
      datatable.verbose = FALSE
    )
    on.exit(options(old_dt_opts), add = TRUE)
  }
  
  # Get target info for surrogate-based stopping
  obj_info <- objectives@objective_info
  first_obj_name <- names(obj_info$objectives)[1]
  first_obj <- obj_info$objectives[[first_obj_name]]
  target_val <- if (first_obj$type == "target")
    first_obj$target_value
  else
    NA
  
  opt_error <- NULL
  early_stopped <- FALSE
  stop_reason <- NULL
  mbo_config <- NULL

  # Wrap optimization in withCallingHandlers to suppress data.table column warnings
  # These warnings can come from mlr3mbo/bbotk internals and are not actionable
  withCallingHandlers({
  tryCatch({
    # Evaluate initial design
    # Suppress data.table warnings from bbotk internals
    invisible(suppressWarnings(instance$eval_batch(init_design_pts)))

    # Set up MBO components (surrogate, acquisition function, optimizer)
    mbo_config <- setup_mbo_components(
      instance = instance,
      opt_type = opt_type,
      surrogate = surrogate,
      acq_function = acq_function,
      obj_info = obj_info,
      eic_kappa = eic_kappa,
      verbosity = verbosity
    )

    # Run the optimizer (uses mlr3mbo's built-in loop)
    # Suppress data.table warnings from mlr3mbo internals
    invisible(suppressWarnings(mbo_config$optimizer$optimize(instance)))

  }, error = function(e) {
    opt_error <<- e
  })
  }, warning = function(w) {
    # Suppress data.table "undefined columns selected" warnings from mlr3mbo internals
    if (grepl("nicht definierte Spalten|undefined columns", conditionMessage(w), ignore.case = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })

  # Re-throw any real error
  if (!is.null(opt_error)) {
    stop(opt_error)
  }
  
  # ===========================================================================
  # POST-OPTIMIZATION: Check surrogate stopping conditions
  
  # ===========================================================================
  # EXTRACT RESULTS
  # ===========================================================================
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  
  # Get archive as data frame
  archive_df <- as.data.frame(instance$archive$data)
  
  # Verbose diagnostic output
  if (verbosity >= 2) {
    cli::cli_alert_info("Archive: {nrow(archive_df)} rows, {ncol(archive_df)} cols")
  }
  
  # Post-process: For target optimization, the objective column contains
  # bonus-adjusted values (can exceed 1.0). Add actual metric columns.
  archive_df <- postprocess_archive(archive_df, objectives)
  
  # Get result(s)
  if (opt_type == "multi") {
    # Multi-objective: extract Pareto front
    pareto_df <- as.data.frame(instance$archive$best())
    pareto_df <- postprocess_archive(pareto_df, objectives)
    result_df <- pareto_df
    result@pareto_front <- pareto_df
    final_pa <- NULL
  } else {
    # Single-objective: use surrogate to find best point and run final confirmation
    final_result <- finalize_with_surrogate(
      mbo_config = mbo_config,
      instance = instance,
      objectives = objectives,
      fidelity_schedule = fidelity_schedule,
      sims_final_run = sims_final_run,
      trim_param_space = trim_param_space,
      profile_resolution = profile_resolution,
      max_final_evals = max_final_evals,
      n_cores = n_cores,
      bf_args = bf_args,
      brms_args = brms_args,
      verbosity = verbosity
    )
    result_df <- final_result$result_df
    final_pa <- final_result$power_analysis
    # Extract optimum results for storage in result object slots
    optimum_surrogate <- final_result$optimum_surrogate
    optimum_confident <- final_result$optimum_confident
  }
  
  # Build convergence trace
  convergence_df <- build_convergence_trace(archive_df, opt_type, objectives)
  
  # ===========================================================================
  # UPDATE RESULT OBJECT
  # ===========================================================================
  result@archive <- archive_df
  result@result <- result_df
  result@convergence <- convergence_df
  result@elapsed_time <- elapsed_time
  result@n_evals <- nrow(archive_df)
  result@early_stopped <- early_stopped
  # Use final confirmation PA if available, otherwise use best tracked during optimization
  result@best_power_analysis <- if (!is.null(final_pa))
    final_pa
  else
    obj_fn_result$get_best_pa()
  # Store mlr3mbo/bbotk objects for advanced access (NULL if setup failed)
  if (!is.null(mbo_config)) {
    mbo_config$instance <- instance
    result@mbo_objects <- mbo_config
  }
  # Store optimum results for single-objective optimization
  if (opt_type != "multi") {
    result@optimum_surrogate <- optimum_surrogate
    result@optimum_confident <- optimum_confident
  }
  
  # ===========================================================================
  # DISPLAY RESULTS
  # ===========================================================================
  if (should_show(1)) {
    if (early_stopped) {
      cli::cli_alert_success(
        "Early stopping after {nrow(archive_df)} evaluations ({format_duration(elapsed_time)})"
      )
    } else {
      cli::cli_alert_success("Optimization complete in {format_duration(elapsed_time)}")
    }
    
    # Multi-objective: show Pareto info (single-objective display handled by finalize_with_surrogate)
    if (opt_type == "multi" && !is.null(result@pareto_front)) {
      cli::cli_alert_info("Found {nrow(result@pareto_front)} Pareto-optimal solutions")
    }
    
    # Check if target was achieved (for target optimization)
    # Use confident optimum if available, otherwise surrogate optimum
    if (opt_type == "target" && nrow(result_df) > 0) {
      obj_info <- objectives@objective_info
      first_obj_name <- names(obj_info$objectives)[1]
      first_obj <- obj_info$objectives[[first_obj_name]]
      
      if (first_obj$type == "target" &&
          first_obj_name %in% names(result_df)) {
        target_val <- first_obj$target_value
        
        # Check confident optimum first (if exists), then surrogate optimum
        if (!is.null(optimum_confident) &&
            !is.null(optimum_confident$value)) {
          achieved_val <- optimum_confident$value
        } else {
          achieved_val <- result_df[[first_obj_name]][1]
        }
        
        target_achieved <- if (first_obj$direction == ">=") {
          achieved_val >= target_val
        } else {
          achieved_val <= target_val
        }
        
        if (!target_achieved) {
          cli::cli_alert_warning(
            "Target not achieved: {first_obj_name}={round(achieved_val, 3)} < target {target_val}"
          )
          # Suggest increasing evaluations or simulations
          cli::cli_alert_info("Consider increasing 'max_evals' or 'n_sims' to find the target")
        }
      }
    }
  }
  
  result
}

# =============================================================================
# HELPER: Create Parameter Space
# =============================================================================

#' Create Parameter Space from Search Specification
#'
#' Handles both standard bounds and simplex search specs. Optionally applies
#' automatic transforms (log for sample sizes, logit for probabilities).
#'
#' @param search Named list of parameter bounds or simplex specs
#' @param search_specs Named list of parsed simplex specifications (optional)
#' @param apply_transforms Logical, whether to apply automatic transforms (default FALSE)
#' @param transform_registry Named list of transform specs from get_param_transform()
#' @return paradox::ParamSet with optional transform_registry attribute
#' @keywords internal
create_parameter_space <- function(search, search_specs = list(),
                                    apply_transforms = FALSE,
                                    transform_registry = list()) {
  # Parameters that are always doubles (even if bounds look like integers)
  always_double <- c(
    "thr_dec_eff",
    "thr_dec_fut",
    "thr_fx_eff",
    "thr_fx_fut",
    # Decision thresholds
    "b_arm_treat",
    "b_covariate",
    "intercept",
    "sigma"          # Effect sizes
  )
  
  # Build parameter definitions
  param_list <- list()
  
  for (param_name in names(search)) {
    spec <- search[[param_name]]
    
    # Handle simplex search specs
    if (param_name %in% names(search_specs)) {
      simplex_spec <- search_specs[[param_name]]
      
      if (simplex_spec$type == "p_alloc") {
        # Allocation probability search
        if (simplex_spec$n_arms == 2) {
          # 2-arm: single bounded parameter [min, 1-min]
          param_list[[param_name]] <- paradox::p_dbl(lower = simplex_spec$min_prop,
                                                     upper = 1 - simplex_spec$min_prop)
        } else {
          # k-arm: k-1 unbounded parameters for ILR
          for (i in seq_len(simplex_spec$n_dims)) {
            ilr_name <- paste0(param_name, "_ilr_", i)
            param_list[[ilr_name]] <- paradox::p_dbl(lower = -5, upper = 5)
          }
        }
      } else if (simplex_spec$type == "looks") {
        # Interim look timing search
        if (simplex_spec$n_looks == 2) {
          # 2-look: single bounded parameter [min_spacing, 1-min_spacing]
          param_list[[param_name]] <- paradox::p_dbl(
            lower = simplex_spec$min_spacing,
            upper = 1 - simplex_spec$min_spacing
          )
        } else {
          # k-look: k-1 unbounded parameters for ILR on increments
          for (i in seq_len(simplex_spec$n_dims)) {
            ilr_name <- paste0(param_name, "_ilr_", i)
            param_list[[ilr_name]] <- paradox::p_dbl(lower = -5, upper = 5)
          }
        }
      }
      next
    }
    
    # Skip if this is a raw simplex spec object (not in search_specs)
    if (inherits(spec, "rctbp_search_p_alloc") ||
        inherits(spec, "rctbp_search_looks")) {
      next
    }
    
    # Standard bounds handling
    bounds <- spec

    # Check if we should apply transforms for this parameter
    if (apply_transforms && param_name %in% names(transform_registry)) {
      transform_spec <- transform_registry[[param_name]]

      # Transform bounds to search space
      transformed_bounds <- transform_spec$transform_bounds(bounds)

      # All transformed parameters are doubles (continuous)
      param_list[[param_name]] <- paradox::p_dbl(
        lower = transformed_bounds[1],
        upper = transformed_bounds[2]
      )
    } else {
      # Original behavior: determine if integer or double
      if (param_name %in% always_double) {
        is_integer <- FALSE
      } else {
        is_integer <- param_name == "n_total" ||
          (all(bounds == floor(bounds)) &&
             max(bounds) - min(bounds) >= 1)
      }

      if (is_integer) {
        param_list[[param_name]] <- paradox::p_int(lower = as.integer(bounds[1]), upper = as.integer(bounds[2]))
      } else {
        param_list[[param_name]] <- paradox::p_dbl(lower = bounds[1], upper = bounds[2])
      }
    }
  }

  # Create parameter set
  result <- do.call(paradox::ps, param_list)

  # Store transform registry as attribute for later use in objective function
  if (apply_transforms && length(transform_registry) > 0) {
    attr(result, "transform_registry") <- transform_registry
  }

  result
}

# =============================================================================
# HELPER: Create Codomain
# =============================================================================

#' Create Codomain from Objectives Specification
#'
#' @param objectives rctbp_objectives object
#' @return paradox::ParamSet for codomain
#' @keywords internal
create_codomain <- function(objectives) {
  obj_info <- objectives@objective_info
  
  # Build codomain for each objective
  codomain_list <- lapply(names(obj_info$objectives), function(obj_name) {
    obj <- obj_info$objectives[[obj_name]]
    
    # Determine tag based on optimization direction
    if (obj$type == "target") {
      # For target, we maximize (power + bonus for smaller n)
      paradox::p_dbl(tags = "maximize")
    } else if (obj$type == "maximize") {
      paradox::p_dbl(tags = "maximize")
    } else {
      paradox::p_dbl(tags = "minimize")
    }
  })
  names(codomain_list) <- names(obj_info$objectives)
  
  do.call(paradox::ps, codomain_list)
}

# =============================================================================
# HELPER: Create Objective Function
# =============================================================================

#' Create Objective Function Wrapper
#'
#' Creates a function that evaluates trial design power for given parameters.
#' Supports progressive fidelity where n_sims varies per evaluation.
#'
#' @param objectives rctbp_objectives object
#' @param effective_search Named list of search bounds for bonus computation
#' @param fidelity_schedule Data frame from build_fidelity_schedule() with
#'   columns: eval, n_sims, fidelity_level, is_progressive
#' @param max_evals Maximum number of evaluations (for progress bar)
#' @param patience Iterations without improvement before early stopping
#' @param min_delta Minimum power above target for early stopping
#' @param penalty Penalty type: "none", "hard", "soft", "ratio", or "logit_distance"
#' @param penalty_alpha Required confidence level for hard constraint (default 0.95)
#' @param penalty_kappa Exponent for ratio penalty (default 2)
#' @param transform_registry Named list of transform specs from get_param_transform()
#' @param n_cores Parallel cores
#' @param bf_args BayesFlow-specific arguments for power_analysis
#' @param brms_args brms-specific arguments for power_analysis
#' @param refresh How often to print progress (0 = never)
#' @param verbosity Output level
#'
#' @return List with:
#'   - `fn`: Function suitable for bbotk::ObjectiveRFun
#'   - `get_best_pa`: Function returning best power_analysis result
#' @keywords internal
create_objective_fn <- function(objectives,
                                effective_search,
                                fidelity_schedule,
                                max_evals,
                                patience,
                                min_delta,
                                penalty = "none",
                                penalty_alpha = 0.95,
                                penalty_kappa = 2,
                                transform_registry = list(),
                                n_cores,
                                bf_args,
                                brms_args,
                                refresh,
                                verbosity) {
  # Capture references
  design <- objectives@design
  search_params <- names(objectives@search)
  search_params_bounds <- effective_search
  search_specs <- objectives@search_specs   # Simplex transform specs
  constant <- objectives@constant
  obj_info <- objectives@objective_info

  # Fidelity schedule info
  is_progressive <- fidelity_schedule$is_progressive[1]
  n_sims_max <- max(fidelity_schedule$n_sims)

  # Evaluation counter for progress
  eval_count <- 0

  # Track best result for progress display
  best_value <- -Inf
  best_params <- NULL
  best_actual <- NULL
  best_pa_result <- NULL  # Full power_analysis result from best solution

  # Objective function
  obj_fn <- function(xs) {
    eval_count <<- eval_count + 1
    
    # Debug: confirm function is called
    if (verbosity >= 2) {
      cli::cli_alert_info("obj_fn called: eval {eval_count}, n_total={xs$n_total}")
    }
    
    # Look up n_sims from fidelity schedule
    # Clamp eval_count to schedule length (in case of overrun)
    schedule_idx <- min(eval_count, nrow(fidelity_schedule))
    current_n_sims <- fidelity_schedule$n_sims[schedule_idx]
    current_fidelity_level <- fidelity_schedule$fidelity_level[schedule_idx]
    
    # Detect fidelity level transition
    prev_level <- if (eval_count > 1)
      fidelity_schedule$fidelity_level[min(eval_count - 1, nrow(fidelity_schedule))]
    else
      0

    # Show fidelity level transition (for progressive fidelity)
    if (is_progressive &&
        verbosity >= 1 && current_fidelity_level != prev_level) {
      # Compute actual range from schedule (handles non-uniform evals_per_step)
      level_rows <- which(fidelity_schedule$fidelity_level == current_fidelity_level)
      level_start <- min(level_rows)
      level_end <- max(level_rows)
      n_levels <- length(unique(fidelity_schedule$n_sims))

      # Determine phase label
      phase_label <- if (current_fidelity_level == 1) {
        " (exploration)"
      } else if (current_fidelity_level == n_levels) {
        " (refinement)"
      } else {
        ""
      }
      cli::cli_alert_info(
        "[{level_start}-{level_end}/{max_evals}] n_sims={current_n_sims}{phase_label}"
      )
    }
    
    # Apply inverse transforms if any transform_registry exists
    # This converts optimizer values (on transformed scale) back to original scale
    xs_original <- xs
    if (length(transform_registry) > 0) {
      for (param_name in names(transform_registry)) {
        if (param_name %in% names(xs)) {
          transform_spec <- transform_registry[[param_name]]
          xs_original[[param_name]] <- transform_spec$inverse_fn(xs[[param_name]])
        }
      }
    }

    # Apply simplex transforms if any search_specs exist
    ilr_values <- list()
    simplex_values <- list()

    if (length(search_specs) > 0) {
      transform_result <- apply_simplex_transforms(xs_original, search_specs)
      crossed_list <- transform_result$crossed
      ilr_values <- transform_result$ilr_values
      simplex_values <- transform_result$simplex_values
    } else {
      # No simplex params: use xs_original directly
      crossed_list <- lapply(search_params, function(p)
        xs_original[[p]])
      names(crossed_list) <- search_params
    }
    
    # Create conditions object
    conditions <- tryCatch({
      build_conditions(design = design,
                       crossed = crossed_list,
                       constant = constant)
    }, error = function(e) {
      if (verbosity >= 2) {
        param_str <- paste(names(xs), "=", unlist(xs), collapse = ", ")
        cli::cli_warn(
          c(
            "Failed to build conditions at eval {eval_count}/{max_evals}",
            "x" = "{e$message}",
            "i" = "Parameters: {param_str}"
          )
        )
      }
      return(NULL)
    })
    
    if (is.null(conditions)) {
      result <- create_worst_result(obj_info)
      result$n_sims_used <- current_n_sims
      return(result)
    }
    
    # Run power analysis with current fidelity level's n_sims
    # Suppress data.table warnings that may come from internal summarization
    pa_result <- tryCatch({
      suppressWarnings(power_analysis(
        conditions = conditions,
        n_sims = current_n_sims,
        n_cores = n_cores,
        bf_args = bf_args,
        brms_args = brms_args,
        verbosity = 0,
        run = TRUE
      ))
    }, error = function(e) {
      if (verbosity >= 2) {
        param_str <- paste(names(xs), "=", unlist(xs), collapse = ", ")
        cli::cli_warn(
          c(
            "Power analysis failed at eval {eval_count}/{max_evals}",
            "x" = "{e$message}",
            "i" = "Parameters: {param_str}"
          )
        )
      }
      return(NULL)
    })
    
    if (is.null(pa_result) ||
        nrow(pa_result@results_conditions) == 0) {
      result <- create_worst_result(obj_info)
      result$n_sims_used <- current_n_sims
      return(result)
    }
    
    # Extract objective values from results
    results <- pa_result@results_conditions
    
    # Build return list for bbotk (may include bonus for optimization)
    obj_values <- list()
    # Also track actual values for display (without bonus)
    actual_values <- list()
    
    for (obj_name in names(obj_info$objectives)) {
      obj <- obj_info$objectives[[obj_name]]
      
      # Debug: show objective type
      if (verbosity >= 2) {
        cli::cli_alert_info(
          "Processing {obj_name}: type={obj$type}, in_results={obj_name %in% names(results)}"
        )
      }
      
      if (obj$type == "target") {
        if (obj_name %in% names(results)) {
          achieved <- results[[obj_name]][1]
          target_val <- obj$target_value
          actual_values[[obj_name]] <- achieved

          # =================================================================
          # PENALTY MODE: Probabilistic cost functions
          # =================================================================
          # When penalty != "none", use cost functions that:
          # - Minimize n (sample size) as primary objective
          # - Penalize not achieving target power
          #
          # P_feas approximation from observed power:
          #   - power >= target: P_feas = 1
          #   - power < target: P_feas = power / target (scaled)
          #
          # We return NEGATIVE cost since bbotk maximizes, but we want min n
          # =================================================================
          if (penalty != "none") {
            # Get sample size (assumed to be n_total in search params)
            n_val <- if ("n_total" %in% names(xs)) xs$n_total else 100

            # Approximate P_feas from observed power
            if (obj$direction == ">=") {
              p_feas <- if (achieved >= target_val) 1.0 else achieved / target_val
            } else {
              # For <= targets, invert logic
              p_feas <- if (achieved <= target_val) 1.0 else target_val / achieved
            }
            p_feas <- pmax(p_feas, 1e-9)  # Avoid division by zero

            # Compute cost based on penalty type
            if (penalty == "hard") {
              # Hard constraint: n + M * max(0, alpha - P_feas)
              M <- 1e6
              cost <- n_val + M * pmax(0, penalty_alpha - p_feas)
            } else if (penalty == "soft") {
              # Soft penalty: n + M * max(0, 1 - P_feas)
              M <- 1e6
              cost <- n_val + M * pmax(0, 1 - p_feas)
            } else if (penalty == "ratio") {
              # Risk-aware ratio: n / P_feas^kappa
              cost <- n_val / (p_feas^penalty_kappa)
            } else if (penalty == "logit_distance") {
              # Logit-distance: symmetric penalty around target
              # f(x) = 1 - invlogit(abs(logit(power) - logit(target)))
              # Maximum at target (0.5), decreases symmetrically
              cost <- cost_logit_distance(achieved, target_val)^2
              # Note: for logit_distance we return the cost directly (we maximize)
              obj_values[[obj_name]] <- cost

              if (verbosity >= 2) {
                cli::cli_alert_info(
                  "Logit-distance: power={round(achieved, 3)}, target={target_val}, cost={round(cost, 4)}"
                )
              }
              # Skip the negative cost logic below
              next
            } else {
              cost <- n_val
            }

            # Return NEGATIVE cost (we maximize, so -cost minimizes n)
            obj_values[[obj_name]] <- -cost

            if (verbosity >= 2) {
              cli::cli_alert_info(
                "Penalty mode ({penalty}): n={n_val}, power={round(achieved, 3)}, P_feas={round(p_feas, 3)}, cost={round(cost, 1)}"
              )
            }

          } else if (obj$direction == ">=") {
            # =================================================================
            # STANDARD MODE: Continuous cost function with sqrt overshoot
            # =================================================================
            # For >= targets (e.g., power >= 0.80):
            #   - Below target:  cost = (achieved / target)^2
            #     Ranges [0, 1) as achieved goes from 0 to target
            #   - At/above target: cost = 2 - sqrt((achieved - target) / (1 - target))
            #     Peaks at 2 when achieved=target, decreases toward 1 as achieved->1
            #
            # For >= targets (e.g., power >= 0.80)
            if (achieved < target_val) {
              # Below target: quadratic ramp from 0 to 1
              cost <- (achieved / target_val)^2
              if (verbosity >= 2) {
                cli::cli_alert_info(
                  "Below target: pwr={round(achieved, 3)}, cost=(pwr/target)^2={round(cost, 4)}"
                )
              }
            } else {
              # At or above target: peak at 2, sqrt decay with overshoot
              # Handle edge case when target = 1 (avoid division by zero)
              if (target_val >= 1) {
                cost <- 2
              } else {
                overshoot <- achieved - target_val
                cost <- 2 - sqrt(overshoot / (1 - target_val))
              }
              if (verbosity >= 2) {
                cli::cli_alert_info(
                  "At/above target: pwr={round(achieved, 3)}, cost=2-sqrt((pwr-target)/(1-target))={round(cost, 4)}"
                )
              }
            }
            obj_values[[obj_name]] <- cost

          } else {
            # For <= targets (e.g., type I error <= 0.05)
            if (achieved <= target_val) {
              # At or below target: peak at 2, sqrt decay with undershoot
              # Handle edge case when target = 0 (avoid division by zero)
              if (target_val <= 0) {
                cost <- 2
              } else {
                undershoot <- target_val - achieved
                cost <- 2 - sqrt(undershoot / target_val)
              }
              if (verbosity >= 2) {
                cli::cli_alert_info(
                  "At/below target: val={round(achieved, 3)}, cost=2-sqrt((target-val)/target)={round(cost, 4)}"
                )
              }
            } else {
              # Above target (bad): quadratic penalty
              cost <- (target_val / achieved)^2
              if (verbosity >= 2) {
                cli::cli_alert_info(
                  "Above target: val={round(achieved, 3)}, cost=(target/val)^2={round(cost, 4)}"
                )
              }
            }
            obj_values[[obj_name]] <- cost
          }
        } else {
          # Failed evaluation: worst case (cost = 0)
          obj_values[[obj_name]] <- 0
          actual_values[[obj_name]] <- NA
        }
        
      } else if (obj_name %in% names(results)) {
        obj_values[[obj_name]] <- results[[obj_name]][1]
        actual_values[[obj_name]] <- results[[obj_name]][1]
      } else if (obj_name %in% search_params) {
        obj_values[[obj_name]] <- xs[[obj_name]]
        actual_values[[obj_name]] <- xs[[obj_name]]
      } else {
        obj_values[[obj_name]] <- NA_real_
        actual_values[[obj_name]] <- NA_real_
      }
    }
    
    # Update best tracking
    first_obj <- names(obj_info$objectives)[1]
    first_obj_info <- obj_info$objectives[[first_obj]]
    current_value <- obj_values[[first_obj]]
    is_new_best <- FALSE
    
    # For target optimization: use consistent "new best" logic based on params
    if (first_obj_info$type == "target") {
      # Check if target is achieved
      actual_val <- actual_values[[first_obj]]
      target_val <- first_obj_info$target_value
      target_achieved <- if (first_obj_info$direction == ">=") {
        actual_val >= target_val
      } else {
        actual_val <= target_val
      }

      # Check if in optimal window (close to target, not overshooting)
      # This ensures "new best" is consistent with patience logic
      in_optimal_window <- target_achieved &&
        actual_val < target_val + min_delta
      
      if (in_optimal_window) {
        # "New best" = smaller n_total (if searching over n_total), else use objective value
        if ("n_total" %in% search_params) {
          current_n <- xs[["n_total"]]
          best_n <- if (!is.null(best_params)) best_params[["n_total"]] else Inf

          if (current_n < best_n) {
            best_value <<- current_value
            best_params <<- xs
            best_actual <<- actual_values
            best_pa_result <<- pa_result
            is_new_best <- TRUE
          }
        } else {
          # Fallback: use objective value comparison
          if (!is.na(current_value) && current_value > best_value) {
            best_value <<- current_value
            best_params <<- xs
            best_actual <<- actual_values
            best_pa_result <<- pa_result
            is_new_best <- TRUE
          }
        }
      }
    } else {
      # Non-target optimization: use objective value comparison
      if (!is.na(current_value) && current_value > best_value) {
        best_value <<- current_value
        best_params <<- xs
        best_actual <<- actual_values
        best_pa_result <<- pa_result
        is_new_best <- TRUE
      }
    }
    
    # Add actual values as extra columns (for archive)
    # Use "actual_" prefix instead of "_actual" suffix to avoid bbotk column conflicts
    for (name in names(actual_values)) {
      obj_values[[paste0("actual_", name)]] <- actual_values[[name]]
    }

    # Add logit-transformed power for EIC constraint surrogate
    # This allows the EIC acquisition function to model P(power >= target)
    # IMPORTANT: Always add the logit column to ensure consistent archive columns
    for (name in names(actual_values)) {
      if (grepl("^pwr_|^pr_", name)) {
        actual_val <- actual_values[[name]]
        logit_col <- paste0("logit_", name)
        if (!is.na(actual_val) && actual_val > 0 && actual_val < 1) {
          obj_values[[logit_col]] <- logit_transform(actual_val)
        } else {
          # Use NA for invalid values to maintain consistent columns
          obj_values[[logit_col]] <- NA_real_
        }
      }
    }

    # Add all MCSE values from results (se_* columns)
    # This includes: se_pwr_eff, se_pwr_fut, se_pr_eff, se_pr_fut, etc.
    se_cols <- grep("^se_", names(results), value = TRUE)
    for (se_col in se_cols) {
      obj_values[[se_col]] <- results[[se_col]][1]
    }
    
    # Add simplex values for archive storage (both ILR and proportions)
    if (length(simplex_values) > 0) {
      obj_values <- add_simplex_to_archive(obj_values, simplex_values, ilr_values, search_specs)
    }
    
    # Add n_sims_used for fidelity-based weighting
    obj_values$n_sims_used <- current_n_sims

    # Show progress at refresh intervals
    if (refresh > 0 &&
        verbosity >= 1 && eval_count %% refresh == 0) {
      # Format current evaluation (use 3 decimals for doubles, 0 for integers)
      param_str <- paste(search_params,
                         "=",
                         sapply(search_params, function(p) {
                           val <- xs[[p]]
                           if (p == "n_total" ||
                               (val == floor(val) && abs(val) >= 1)) {
                             format(round(val, 0), nsmall = 0)
                           } else {
                             format(round(val, 3), nsmall = 3)
                           }
                         }),
                         collapse = ", ")
      obj_str <- paste(names(actual_values), "=", round(unlist(actual_values), 3), collapse = ", ")

      # Add cost info when using penalty modes
      if (penalty != "none") {
        # obj_values contains negative cost (since bbotk maximizes)
        first_obj <- names(obj_info$objectives)[1]
        cost_val <- -obj_values[[first_obj]]
        obj_str <- paste0(obj_str, " | cost=", format(round(cost_val, 1), nsmall = 1))
      }

      cli::cli_alert_info("[{eval_count}/{max_evals}] {param_str} | {obj_str}")
    }
    
    # Always show new best results (if verbosity >= 1)
    if (is_new_best &&
        verbosity >= 1 && (refresh == 0 || eval_count %% refresh != 0)) {
      param_str <- paste(search_params,
                         "=",
                         sapply(search_params, function(p) {
                           val <- xs[[p]]
                           if (p == "n_total" ||
                               (val == floor(val) && abs(val) >= 1)) {
                             format(round(val, 0), nsmall = 0)
                           } else {
                             format(round(val, 3), nsmall = 3)
                           }
                         }),
                         collapse = ", ")
      obj_str <- paste(names(actual_values), "=", round(unlist(actual_values), 3), collapse = ", ")

      # Add cost info when using penalty modes
      if (penalty != "none") {
        first_obj <- names(obj_info$objectives)[1]
        cost_val <- -obj_values[[first_obj]]
        obj_str <- paste0(obj_str, " | cost=", format(round(cost_val, 1), nsmall = 1))
      }

      cli::cli_alert_success("[{eval_count}/{max_evals}] New best: {param_str} | {obj_str}")
    }
    
    # Note: Early stopping is now handled in the optimization loop via
    # surrogate-based stopping (check_surrogate_stopping), not here.
    
    # Clean up memory after each evaluation to prevent GPU OOM and RAM accumulation
    # Remove large objects explicitly before garbage collection
    rm(pa_result, conditions)
    
    # For BayesFlow backend, use comprehensive GPU + RAM cleanup
    # For brms backend, just run R garbage collection
    if (design@backend == "bf") {
      clear_gpu_memory(r_gc = TRUE)
    } else {
      gc(verbose = FALSE, full = TRUE)
    }
    
    # Debug: show what we are returning to bbotk
    if (verbosity >= 2) {
      cli::cli_alert_info(
        "Returning to bbotk: {paste(names(obj_values), '=', round(unlist(obj_values), 4), collapse = ', ')}"
      )
    }

    obj_values
  }

  # Return list with objective function and getters
  list(
    fn = obj_fn,
    get_best_pa = function()
      best_pa_result
  )
}

#' Create Worst Result for Failed Evaluations
#'
#' For target objectives, returns quadratic penalty assuming worst possible
#' outcome (achieved = 0 for >= targets, achieved = 1 for <= targets).
#'
#' @param obj_info Parsed objective info
#' @return Named list with worst values
#' @keywords internal
create_worst_result <- function(obj_info) {
  result <- list()
  for (obj_name in names(obj_info$objectives)) {
    obj <- obj_info$objectives[[obj_name]]
    if (obj$type == "target") {
      # Quadratic penalty assuming worst outcome
      target_val <- obj$target_value
      if (obj$direction == ">=") {
        # Worst case: achieved = 0
        result[[obj_name]] <- -(target_val - 0)^2
      } else {
        # Worst case: achieved = 1
        result[[obj_name]] <- -(1 - target_val)^2
      }
    } else if (obj$type == "maximize") {
      result[[obj_name]] <- -Inf
    } else {
      # Minimize type
      result[[obj_name]] <- Inf
    }
  }
  result
}

# =============================================================================
# HELPER: Generate Initial Design
# =============================================================================

#' Generate Initial Design Points
#'
#' @param domain paradox::ParamSet
#' @param n Number of points
#' @param method Design method ("lhs", "random", "sobol")
#'
#' @return data.table with initial design
#' @keywords internal
generate_initial_design <- function(domain, n, method) {
  design <- switch(
    method,
    "lhs" = paradox::generate_design_lhs(domain, n),
    "random" = paradox::generate_design_random(domain, n),
    "sobol" = paradox::generate_design_sobol(domain, n),
    paradox::generate_design_lhs(domain, n)  # Default
  )
  
  design$data
}

# =============================================================================
# HELPER: Find Surrogate Optimum (Method A)
# =============================================================================

#' Find Surrogate-Based Optimum (Internal)
#'
#' Searches over the parameter space using surrogate model predictions to find
#' the optimal point. Constrains search to observed quantile range to reduce
#' extrapolation risk.
#'
#' @param mbo_config MBO configuration with surrogate
#' @param instance bbotk optimization instance
#' @param archive_df Archive data frame
#' @param objectives rctbp_objectives object
#' @param n_candidates Number of LHS candidates to evaluate (default 10000)
#' @param ci_level Confidence level for interval (default 0.95)
#' @param trim Proportion to trim from each tail of observed distributions
#'   (default 0.25 = IQR). Set to 0 for full domain search.
#'
#' @return List with params, predicted_mean, predicted_se, ci_lower, ci_upper,
#'   domain_used
#' @keywords internal
find_surrogate_optimum_internal <- function(mbo_config,
                                            instance,
                                            archive_df,
                                            objectives,
                                            n_candidates = 10000,
                                            ci_level = 0.95,
                                            trim = 0.25,
                                            verbosity = 0) {
  surr <- mbo_config$surrogate
  # Suppress data.table warnings from mlr3mbo internals
  suppressWarnings(surr$update())
  
  search_params <- names(objectives@search)
  original_domain <- instance$search_space
  
  # Build constrained domain based on trim
  domain_used <- list()
  
  if (trim > 0 && trim < 0.5) {
    # Calculate quantiles for each search param
    param_list <- list()
    
    for (param_name in search_params) {
      if (!param_name %in% names(archive_df))
        next
      
      values <- archive_df[[param_name]]
      q_lower <- stats::quantile(values, trim, na.rm = TRUE)
      q_upper <- stats::quantile(values, 1 - trim, na.rm = TRUE)
      
      # Handle degenerate case (all same value)
      if (q_lower >= q_upper) {
        # Fall back to original bounds
        orig_param <- original_domain$params[[param_name]]
        q_lower <- orig_param$lower
        q_upper <- orig_param$upper
      }
      
      domain_used[[param_name]] <- c(q_lower, q_upper)
      
      # Create paradox parameter
      orig_param <- original_domain$params[[param_name]]
      if (inherits(orig_param, "ParamInt")) {
        param_list[[param_name]] <- paradox::p_int(lower = as.integer(ceiling(q_lower)),
                                                   upper = as.integer(floor(q_upper)))
      } else {
        param_list[[param_name]] <- paradox::p_dbl(lower = q_lower, upper = q_upper)
      }
    }
    
    # Show trimmed domain before LHS generation
    if (verbosity >= 1) {
      domain_info <- paste(sapply(names(domain_used), function(p) {
        bounds <- domain_used[[p]]
        sprintf("%s:[%.0f,%.0f]", p, bounds[1], bounds[2])
      }), collapse = ", ")
      cli::cli_alert_info("Trimmed search domain: {domain_info}")
    }
    
    constrained_domain <- do.call(paradox::ps, param_list)
  } else {
    # Use full original domain
    constrained_domain <- original_domain
    for (param_name in search_params) {
      orig_param <- original_domain$params[[param_name]]
      domain_used[[param_name]] <- c(orig_param$lower, orig_param$upper)
    }
  }
  
  
  # Calculate actual domain size (for integer params, limit to unique values)
  domain_size <- 1
  for (param_name in names(domain_used)) {
    bounds <- domain_used[[param_name]]
    # Check if original param is integer type
    orig_param <- original_domain$params[[param_name]]
    is_int <- inherits(orig_param, "ParamInt") ||
      (grepl("^n_|_n$|n_total", param_name, ignore.case = TRUE) &&
         all(bounds == floor(bounds)))
    
    if (is_int) {
      n_unique <- floor(bounds[2]) - ceiling(bounds[1]) + 1
      domain_size <- domain_size * max(1, n_unique)
    } else {
      # For continuous params, assume 1000 effective levels per dimension
      domain_size <- domain_size * 1000
    }
  }
  
  # Limit candidates to min(requested, domain_size)
  actual_candidates <- min(n_candidates, domain_size)
  
  if (verbosity >= 1) {
    cli::cli_alert_info("Evaluating {actual_candidates} candidates")
  }
  
  # Generate LHS candidates
  candidates <- paradox::generate_design_lhs(constrained_domain, n = actual_candidates)$data

  # Get surrogate predictions
  # Suppress data.table warnings from mlr3mbo internals
  pred <- suppressWarnings(surr$predict(candidates))
  
  # Determine optimization direction
  obj_info <- objectives@objective_info
  first_obj <- obj_info$objectives[[1]]
  
  # For target optimization, we maximize (higher composite = better)
  # For minimize objectives, we want lowest
  # For maximize objectives, we want highest
  if (first_obj$type == "target" || first_obj$type == "maximize") {
    best_idx <- which.max(pred$mean)
  } else {
    best_idx <- which.min(pred$mean)
  }
  
  # Extract best parameters (use intersect to handle edge cases)
  search_params_available <- intersect(search_params, names(candidates))
  best_params <- as.list(candidates[best_idx, search_params_available, drop = FALSE])
  
  # Calculate CI
  z <- stats::qnorm((1 + ci_level) / 2)
  predicted_mean <- pred$mean[best_idx]
  predicted_se <- pred$se[best_idx]
  ci_lower <- predicted_mean - z * predicted_se
  ci_upper <- predicted_mean + z * predicted_se
  
  list(
    params = best_params,
    predicted_mean = predicted_mean,
    predicted_se = predicted_se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    domain_used = domain_used
  )
}

# =============================================================================
# PROBABILISTIC CONSTRAINT OPTIMUM FINDING
# =============================================================================
# Find minimum n where P(power >= target) >= alpha using GP on logit(power)

#' Find Probabilistic Optimum from Archive
#'
#' Finds the minimum sample size where the GP-predicted probability of

#' achieving target power exceeds the required confidence level alpha.
#'
#' @param archive_df Archive data frame with n_total and power columns
#' @param objectives rctbp_objectives object
#' @param target Target power value (default: extracted from objectives)
#' @param alpha Required confidence level (default 0.95)
#' @param n_candidates Number of candidates to evaluate (default 1000)
#' @param use_log_n Whether to use log(n) for GP input (default TRUE)
#' @param verbosity Verbosity level
#'
#' @return List with:
#'   - `n_opt`: Optimal sample size (ceiling-rounded)
#'   - `p_feas`: Feasibility probability at n_opt
#'   - `predicted_power`: GP-predicted power at n_opt
#'   - `ci_lower`, `ci_upper`: Confidence interval for power
#'   - `gp_model`: Fitted GP model (for diagnostics)
#'
#' @details
#' This function implements the probabilistic constraint approach:
#' 1. Fits a GP on (log(n), logit(power)) from archive data
#' 2. Evaluates P_feas = P(power >= target) for a grid of n values
#' 3. Finds the minimum n where P_feas >= alpha
#'
#' The GP models logit(power) because:
#' - Power is bounded in (0, 1), logit transforms to unbounded
#' - GP works better on approximately Gaussian, unbounded targets
#'
#' @keywords internal
find_probabilistic_optimum_internal <- function(archive_df,
                                                 objectives,
                                                 target = NULL,
                                                 alpha = 0.95,
                                                 n_candidates = 1000,
                                                 use_log_n = TRUE,
                                                 verbosity = 1) {
  # Extract target from objectives if not provided
  if (is.null(target)) {
    obj_info <- objectives@objective_info
    first_obj <- obj_info$objectives[[1]]
    if (first_obj$type == "target") {
      target <- first_obj$target_value
    } else {
      cli::cli_abort(c(
        "Target value required for probabilistic optimum",
        "i" = "Provide target parameter or use target optimization"
      ))
    }
  }

  # Find power column in archive
  power_cols <- grep("^(pwr_|actual_pwr_)", names(archive_df), value = TRUE)
  if (length(power_cols) == 0) {
    cli::cli_abort(c(
      "No power column found in archive",
      "i" = "Archive must contain pwr_* or actual_pwr_* columns"
    ))
  }
  power_col <- power_cols[1]  # Use first power column

  # Find n_total column
  if (!"n_total" %in% names(archive_df)) {
    cli::cli_abort("n_total column not found in archive")
  }

  # Extract data
  n_values <- archive_df$n_total
  power_values <- archive_df[[power_col]]

  # Remove NA values
  valid_idx <- !is.na(n_values) & !is.na(power_values)
  n_values <- n_values[valid_idx]
  power_values <- power_values[valid_idx]

  if (length(n_values) < 5) {
    cli::cli_abort(c(
      "Insufficient data for GP fitting",
      "x" = "Need at least 5 valid observations, got {length(n_values)}"
    ))
  }

  # Transform inputs
  if (use_log_n) {
    x_values <- log(n_values)
  } else {
    x_values <- n_values
  }
  y_values <- logit_transform(power_values)

  # Fit GP using DiceKriging
  rlang::check_installed("DiceKriging", reason = "for GP surrogate model")

  # Prepare data for GP
  X <- matrix(x_values, ncol = 1)
  colnames(X) <- "x"

  # Fit GP with nugget for numerical stability
  gp_model <- tryCatch({
    DiceKriging::km(
      design = X,
      response = y_values,
      covtype = "matern3_2",
      nugget.estim = TRUE,
      control = list(trace = FALSE)
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "GP fitting failed",
      "x" = conditionMessage(e)
    ))
  })

  # Generate candidate n values
  n_bounds <- range(n_values)
  # Extend bounds slightly to ensure we cover the feasible region
  n_lower <- max(1, n_bounds[1] * 0.9)
  n_upper <- n_bounds[2] * 1.1

  n_candidates_vec <- seq(n_lower, n_upper, length.out = n_candidates)

  # Transform candidates for GP prediction
  if (use_log_n) {
    x_candidates <- log(n_candidates_vec)
  } else {
    x_candidates <- n_candidates_vec
  }
  X_new <- matrix(x_candidates, ncol = 1)
  colnames(X_new) <- "x"

  # Get GP predictions
  pred <- DiceKriging::predict(gp_model, newdata = X_new, type = "UK")
  mu <- pred$mean
  sigma <- sqrt(pred$sd^2)  # Predictive SD

  # Compute P_feas for each candidate
  p_feas <- compute_p_feas(mu, sigma, target)

  # Find minimum n where P_feas >= alpha
  feasible_idx <- which(p_feas >= alpha)

  if (length(feasible_idx) == 0) {
    # No feasible point found - return the point with highest P_feas
    best_idx <- which.max(p_feas)
    if (verbosity >= 1) {
      cli::cli_alert_warning(
        "No n found with P_feas >= {alpha}. Best P_feas = {round(max(p_feas), 3)} at n = {ceiling(n_candidates_vec[best_idx])}"
      )
    }
  } else {
    # Find minimum n among feasible points
    best_idx <- feasible_idx[which.min(n_candidates_vec[feasible_idx])]
  }

  n_opt <- ceiling(n_candidates_vec[best_idx])
  p_feas_opt <- p_feas[best_idx]

  # Compute power prediction at optimal n
  predicted_logit_power <- mu[best_idx]
  predicted_power <- invlogit_transform(predicted_logit_power)

  # Compute CI for power (on probability scale)
  z <- stats::qnorm(0.975)
  logit_ci_lower <- mu[best_idx] - z * sigma[best_idx]
  logit_ci_upper <- mu[best_idx] + z * sigma[best_idx]
  power_ci_lower <- invlogit_transform(logit_ci_lower)
  power_ci_upper <- invlogit_transform(logit_ci_upper)

  if (verbosity >= 1) {
    cli::cli_alert_success(
      "Probabilistic optimum: n = {n_opt}, P_feas = {round(p_feas_opt, 3)}, predicted power = {round(predicted_power, 3)} [{round(power_ci_lower, 3)}, {round(power_ci_upper, 3)}]"
    )
  }

  list(
    n_opt = n_opt,
    p_feas = p_feas_opt,
    predicted_power = predicted_power,
    ci_lower = power_ci_lower,
    ci_upper = power_ci_upper,
    target = target,
    alpha = alpha,
    gp_model = gp_model,
    # Full profile for plotting
    profile = data.frame(
      n = n_candidates_vec,
      p_feas = p_feas,
      mu = mu,
      sigma = sigma
    )
  )
}


# =============================================================================
# HELPER: Compute Profile Resolution
# =============================================================================

#' Compute Profile Resolution for Parameters
#'
#' Determines appropriate profiling resolution based on parameter type.
#' Sample size parameters (n_*) use integer steps, probability parameters
#' (thr_*, pwr_*, pr_*) use fine resolution.
#'
#' @param param_name Parameter name
#' @param bounds Numeric vector of length 2 with lower and upper bounds
#' @param resolution Resolution setting: "auto" or a positive number
#'
#' @return Numeric resolution value for this parameter
#' @keywords internal
compute_profile_resolution <- function(param_name, bounds, resolution = "auto") {
  if (!identical(resolution, "auto")) {
    return(resolution)
  }
  
  # Auto-detect based on parameter name patterns
  if (grepl("^n_|^n$|_n$|n_total", param_name, ignore.case = TRUE)) {
    # Sample size: integer resolution
    return(1)
  } else if (grepl("^thr_|^pwr_|^pr_|^p_|prob|threshold",
                   param_name,
                   ignore.case = TRUE)) {
    # Probability/threshold: fine resolution
    return(0.01)
  } else {
    # Other continuous: adaptive based on range
    range_val <- bounds[2] - bounds[1]
    return(range_val / 100)
  }
}

# =============================================================================
# HELPER: Profile Surrogate Model
# =============================================================================

#' Profile Surrogate Model Along Search Dimensions (Internal)
#'
#' Generates predictions along a grid of parameter values to visualize and
#' analyze the surrogate model's predictions across the search space.
#'
#' @param mbo_config MBO configuration with trained surrogate
#' @param objectives rctbp_objectives object
#' @param resolution Resolution setting: "auto" or positive number
#' @param ci_level Confidence level for intervals (default 0.95)
#'
#' @return Data frame with parameter values and predictions (predicted_mean,
#'   predicted_se, ci_lower, ci_upper)
#' @keywords internal
profile_surrogate_internal <- function(mbo_config,
                                       objectives,
                                       resolution = "auto",
                                       ci_level = 0.95) {
  surr <- mbo_config$surrogate
  # Suppress data.table warnings from mlr3mbo internals
  suppressWarnings(surr$update())
  
  search <- objectives@search
  search_params <- names(search)
  
  # Generate grid for each parameter
  grids <- lapply(search_params, function(p) {
    bounds <- search[[p]]
    res <- compute_profile_resolution(p, bounds, resolution)
    seq(bounds[1], bounds[2], by = res)
  })
  names(grids) <- search_params
  
  # Create full grid (Cartesian product for multi-dimensional)
  if (length(search_params) == 1) {
    profile_grid <- data.frame(grids[[1]])
    names(profile_grid) <- search_params[1]
  } else {
    profile_grid <- expand.grid(grids)
  }
  
  # Get surrogate predictions
  # Suppress data.table warnings from mlr3mbo internals
  xdt <- data.table::as.data.table(profile_grid)
  pred <- suppressWarnings(surr$predict(xdt))
  
  # Add predictions to grid
  z <- stats::qnorm((1 + ci_level) / 2)
  profile_grid$predicted_mean <- pred$mean
  profile_grid$predicted_se <- pred$se
  profile_grid$ci_lower <- pred$mean - z * pred$se
  profile_grid$ci_upper <- pred$mean + z * pred$se
  
  profile_grid
}

# =============================================================================
# HELPER: Find Confident Optimum from Profile
# =============================================================================

#' Find Confident Optimum from Surrogate Profile (Internal)
#'
#' Finds the smallest parameter value where the surrogate's lower CI excludes
#' the target value. This provides a statistically confident estimate that
#' the target is achieved, more conservative than the point estimate.
#'
#' @param profile_df Profile data frame from profile_surrogate_internal()
#' @param objectives rctbp_objectives object
#'
#' @return List with params, predicted_mean, predicted_se, ci_lower, ci_upper,
#'   or NULL if no point confidently achieves target
#' @keywords internal
find_confident_optimum_internal <- function(profile_df, objectives) {
  obj_info <- objectives@objective_info
  first_obj <- obj_info$objectives[[1]]
  search_params <- names(objectives@search)
  
  # Only meaningful for target optimization
  if (first_obj$type != "target") {
    return(NULL)
  }
  
  target_val <- first_obj$target_value
  direction <- first_obj$direction
  
  # Find rows where lower CI excludes target
  if (direction == ">=") {
    # Target is minimum threshold: confident when ci_lower >= target
    confident_mask <- profile_df$ci_lower >= target_val
  } else {
    # Target is maximum threshold: confident when ci_upper <= target
    confident_mask <- profile_df$ci_upper <= target_val
  }
  
  if (!any(confident_mask)) {
    # No point confidently achieves target
    return(NULL)
  }
  
  # Among confident points, find the one with smallest n_total (or first search param)
  confident_df <- profile_df[confident_mask, , drop = FALSE]
  
  # Determine which parameter to minimize (typically n_total)
  minimize_param <- if ("n_total" %in% search_params) {
    "n_total"
  } else {
    search_params[1]
  }
  
  best_idx <- which.min(confident_df[[minimize_param]])
  best_row <- confident_df[best_idx, , drop = FALSE]
  
  # Extract parameters (use intersect to handle edge cases)
  search_params <- intersect(search_params, names(best_row))
  best_params <- as.list(best_row[, search_params, drop = FALSE])
  
  list(
    params = best_params,
    predicted_mean = best_row$predicted_mean,
    predicted_se = best_row$predicted_se,
    ci_lower = best_row$ci_lower,
    ci_upper = best_row$ci_upper
  )
}

#' Finalize Optimization Using Surrogate Model
#'
#' Uses the fitted surrogate model to find the best point (smoothed prediction),
#' then runs final confirmation simulations. Iteratively steps upward from the
#' surrogate optimum to find the confident optimum (where CI excludes target).
#'
#' @param mbo_config MBO configuration from setup_mbo_components
#' @param instance bbotk optimization instance
#' @param objectives rctbp_objectives object
#' @param fidelity_schedule Data frame with fidelity schedule (uses max n_sims)
#' @param sims_final_run Number of simulations for final confirmation run
#' @param trim_param_space Trimming proportion for parameter space constraint (0-0.5)
#' @param profile_resolution Step size for stepping: "auto" or positive number
#' @param max_final_evals Maximum evaluations for final confirmation phase
#' @param n_cores Parallel cores
#' @param bf_args BayesFlow arguments
#' @param brms_args brms arguments
#' @param verbosity Output level
#'
#' @return List with result_df, power_analysis, optimum_surrogate, optimum_confident
#' @keywords internal
finalize_with_surrogate <- function(mbo_config,
                                    instance,
                                    objectives,
                                    fidelity_schedule,
                                    sims_final_run,
                                    trim_param_space,
                                    profile_resolution,
                                    max_final_evals,
                                    n_cores,
                                    bf_args,
                                    brms_args,
                                    verbosity) {
  # Use sims_final_run for final confirmation
  n_sims <- sims_final_run
  should_show <- function(level)
    verbosity >= level
  
  # Get archive and objective info
  archive_df <- as.data.frame(instance$archive$data)
  search_params <- names(objectives@search)
  obj_name <- names(objectives@objectives)[1]
  obj_info <- objectives@objective_info
  first_obj <- obj_info$objectives[[1]]
  
  # ===========================================================================
  # COMPUTE SURROGATE OPTIMUM (point estimate)
  # ===========================================================================
  
  if (should_show(1)) {
    cli::cli_alert_info("Finding surrogate optimum...")
  }
  
  surrogate_result <- find_surrogate_optimum_internal(
    mbo_config = mbo_config,
    instance = instance,
    archive_df = archive_df,
    objectives = objectives,
    n_candidates = 10000,
    ci_level = 0.95,
    trim = trim_param_space,
    verbosity = verbosity
  )
  
  # ===========================================================================
  # HELPER: Run Simulation at Point
  # ===========================================================================
  run_simulation_at <- function(params) {
    design <- objectives@design
    search_specs <- objectives@search_specs
    constant <- objectives@constant
    
    # Apply simplex transforms if any search_specs exist
    if (length(search_specs) > 0) {
      transform_result <- apply_simplex_transforms(params, search_specs)
      crossed_list <- transform_result$crossed
    } else {
      crossed_list <- lapply(search_params, function(p)
        params[[p]])
      names(crossed_list) <- search_params
    }
    
    # Debug: show crossed_list structure
    if (should_show(2)) {
      crossed_str <- paste(names(crossed_list), "=", sapply(crossed_list, function(x) paste(x, collapse=",")), collapse = "; ")
      cli::cli_alert_info("Building conditions with crossed: {crossed_str}")
    }

    conditions <- tryCatch({
      build_conditions(design = design,
                       crossed = crossed_list,
                       constant = constant)
    }, error = function(e) {
      if (should_show(2)) {
        cli::cli_warn("Failed to build conditions: {e$message}")
      }
      return(NULL)
    })

    if (is.null(conditions))
      return(NULL)

    # Run power analysis
    pa_result <- tryCatch({
      power_analysis(
        conditions = conditions,
        n_sims = n_sims,
        n_cores = n_cores,
        bf_args = bf_args,
        brms_args = brms_args,
        verbosity = 0,
        run = TRUE
      )
    }, error = function(e) {
      if (should_show(2)) {
        cli::cli_warn("Final simulation error: {e$message}")
      }
      return(NULL)
    })
    
    if (is.null(pa_result) ||
        nrow(pa_result@results_conditions) == 0) {
      return(NULL)
    }
    
    pa_result
  }
  
  # ===========================================================================
  # HELPER: Check if CI Excludes Target
  # ===========================================================================
  check_confident <- function(pa_result, params) {
    results <- pa_result@results_conditions[1, ]
    value <- results[[obj_name]]
    se_col <- paste0("se_", obj_name)
    se <- if (se_col %in% names(results))
      results[[se_col]]
    else
      0
    
    z <- stats::qnorm(0.975)  # 95% CI
    ci_lower <- value - z * se
    ci_upper <- value + z * se
    
    # Check if CI excludes target
    target_val <- first_obj$target_value
    direction <- first_obj$direction
    
    is_confident <- if (direction == ">=") {
      ci_lower >= target_val
    } else {
      ci_upper <= target_val
    }
    
    list(
      params = params,
      value = value,
      se = se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      is_confident = is_confident,
      power_analysis = pa_result
    )
  }
  
  # ===========================================================================
  # RUN SIMULATION AT SURROGATE OPTIMUM
  # ===========================================================================
  
  best_params <- surrogate_result$params
  eval_count <- 0
  confident_result <- NULL
  first_result <- NULL  # Store the surrogate optimum result

  # Debug: show what params we're trying to simulate
  if (should_show(2)) {
    param_str <- paste(names(best_params), "=", unlist(best_params), collapse = ", ")
    cli::cli_alert_info("Surrogate optimum params: {param_str}")
  }
  
  # Determine stepping parameter and step size
  # Use profile_resolution directly - user controls the step granularity
  step_param <- if ("n_total" %in% search_params)
    "n_total"
  else
    search_params[1]
  bounds <- objectives@search[[step_param]]
  step_size <- compute_profile_resolution(step_param, bounds, profile_resolution)
  
  current_params <- best_params
  
  # Helper to format and display result
  show_result <- function(eval_num, params, info, is_first = FALSE) {
    if (!should_show(1))
      return()
    param_val <- if (step_param == "n_total") {
      round(params[[step_param]])
    } else {
      round(params[[step_param]], 3)
    }
    val_str <- round(info$value, 3)
    ci_str <- paste0("[",
                     round(info$ci_lower, 3),
                     ", ",
                     round(info$ci_upper, 3),
                     "]")
    status <- if (info$is_confident) {
      "\u2713 confident"
    } else if (is_first) {
      "\u2717 CI contains target"
    } else {
      "\u2717"
    }
    cli::cli_alert_info(
      "[{eval_num}/{max_final_evals}] {step_param}={param_val}: {obj_name}={val_str} {ci_str} {status}"
    )
  }
  
  # ===========================================================================
  # FIRST EVALUATION: Determine direction
  # ===========================================================================
  eval_count <- 1
  pa_result <- run_simulation_at(current_params)
  
  if (is.null(pa_result)) {
    cli::cli_warn("First simulation failed")
    # Return bbotk's best as fallback
    result_df <- as.data.frame(instance$archive$best())
    result_df <- postprocess_archive(result_df, objectives)
    return(
      list(
        result_df = result_df,
        power_analysis = NULL,
        optimum_surrogate = surrogate_result,
        optimum_confident = NULL
      )
    )
  }
  
  first_result <- check_confident(pa_result, current_params)
  search_backward <- first_result$is_confident
  
  if (should_show(1)) {
    direction <- if (search_backward)
      "backward"
    else
      "forward"
    cli::cli_alert_info(
      "Running final evaluations (max {max_final_evals}, step={step_size}, {direction})..."
    )
  }
  show_result(eval_count, current_params, first_result, is_first = TRUE)
  
  if (search_backward) {
    # ===========================================================================
    # BACKWARD SEARCH: Find minimum n_total that's still confident
    # ===========================================================================
    confident_result <- first_result  # Start with current as best known confident
    last_confident <- first_result
    
    while (eval_count < max_final_evals) {
      # Step backward
      current_params[[step_param]] <- current_params[[step_param]] - step_size
      
      # Check lower bound
      if (current_params[[step_param]] < bounds[1]) {
        if (should_show(1)) {
          cli::cli_alert_info("Reached lower bound - using minimum confident value")
        }
        break
      }
      
      eval_count <- eval_count + 1
      pa_result <- run_simulation_at(current_params)
      
      if (is.null(pa_result)) {
        if (should_show(2))
          cli::cli_warn("Simulation {eval_count} failed, stopping backward search")
        break
      }
      
      result_info <- check_confident(pa_result, current_params)
      show_result(eval_count, current_params, result_info)
      
      if (result_info$is_confident) {
        # Still confident - update best and continue backward
        last_confident <- result_info
        confident_result <- result_info
      } else {
        # No longer confident - last_confident is the minimum
        if (should_show(1)) {
          cli::cli_alert_success("Found minimum confident value")
        }
        break
      }
    }
  } else {
    # ===========================================================================
    # FORWARD SEARCH: Find first n_total that's confident (existing behavior)
    # ===========================================================================
    while (eval_count < max_final_evals) {
      # Step forward
      current_params[[step_param]] <- current_params[[step_param]] + step_size
      
      # Check upper bound
      if (current_params[[step_param]] > bounds[2]) {
        if (should_show(1)) {
          cli::cli_alert_warning("Reached upper bound without finding confident optimum")
        }
        break
      }
      
      eval_count <- eval_count + 1
      pa_result <- run_simulation_at(current_params)
      
      if (is.null(pa_result)) {
        if (should_show(2))
          cli::cli_warn("Simulation {eval_count} failed, skipping")
        next
      }
      
      result_info <- check_confident(pa_result, current_params)
      show_result(eval_count, current_params, result_info)
      
      if (result_info$is_confident) {
        confident_result <- result_info
        break
      }
    }
  }
  
  # Check if we exhausted max_final_evals without finding confident optimum
  if (is.null(confident_result) &&
      eval_count >= max_final_evals && should_show(1)) {
    cli::cli_alert_warning("Reached max_final_evals ({max_final_evals}) without finding confident optimum")
  }
  
  # ===========================================================================
  # BUILD RESULTS
  # ===========================================================================
  
  # Use first_result (surrogate optimum) as the main result
  if (is.null(first_result)) {
    # Fallback to bbotk's best
    result_df <- as.data.frame(instance$archive$best())
    result_df <- postprocess_archive(result_df, objectives)
    return(
      list(
        result_df = result_df,
        power_analysis = NULL,
        optimum_surrogate = surrogate_result,
        optimum_confident = NULL
      )
    )
  }
  
  # Build result_df from surrogate optimum simulation
  final_results <- first_result$power_analysis@results_conditions[1, ]
  result_df <- data.frame(final_results)
  
  # Add surrogate uncertainty columns
  result_df$surrogate_pred <- surrogate_result$predicted_mean
  result_df$surrogate_se <- surrogate_result$predicted_se
  result_df$surrogate_ci_lower <- surrogate_result$ci_lower
  result_df$surrogate_ci_upper <- surrogate_result$ci_upper
  
  # Report final results
  if (should_show(1)) {
    surr_n <- round(best_params[[step_param]])
    surr_val <- round(first_result$value, 3)
    surr_ci <- paste0("[",
                      round(first_result$ci_lower, 3),
                      ", ",
                      round(first_result$ci_upper, 3),
                      "]")
    cli::cli_alert_success("Surrogate optimum: {step_param}={surr_n}, {obj_name}={surr_val} {surr_ci}")
    
    if (!is.null(confident_result)) {
      conf_n <- round(confident_result$params[[step_param]])
      conf_val <- round(confident_result$value, 3)
      conf_ci <- paste0(
        "[",
        round(confident_result$ci_lower, 3),
        ", ",
        round(confident_result$ci_upper, 3),
        "]"
      )
      cli::cli_alert_success("Confident optimum: {step_param}={conf_n}, {obj_name}={conf_val} {conf_ci}")
    } else {
      cli::cli_alert_warning("No confident optimum found within {max_final_evals} evaluations")
    }
  }
  
  # Format confident_result for storage (without power_analysis object)
  confident_for_storage <- if (!is.null(confident_result)) {
    list(
      params = confident_result$params,
      value = confident_result$value,
      se = confident_result$se,
      ci_lower = confident_result$ci_lower,
      ci_upper = confident_result$ci_upper,
      n_evals = eval_count
    )
  } else {
    NULL
  }
  
  list(
    result_df = result_df,
    power_analysis = first_result$power_analysis,
    optimum_surrogate = surrogate_result,
    optimum_confident = confident_for_storage
  )
}

# =============================================================================
# HELPER: Setup MBO Components
# =============================================================================

#' Setup mlr3mbo Components
#'
#' Configures surrogate model, acquisition function, and optimizer.
#'
#' @param instance bbotk optimization instance
#' @param opt_type Optimization type ("single", "target", "multi")
#' @param surrogate Surrogate model type ("gp" or "rf")
#' @param acq_function Acquisition function name ("auto", "ei", "cb", "pi", or "eic")
#' @param obj_info Objective information (for SE column name)
#' @param eic_kappa Conservatism parameter for EIC acquisition (default 0)
#' @param verbosity Verbosity level for logging
#'
#' @return List with optimizer configuration
#'
#' @keywords internal
setup_mbo_components <- function(instance,
                                 opt_type,
                                 surrogate,
                                 acq_function,
                                 obj_info = NULL,
                                 eic_kappa = 0,
                                 verbosity = 1) {
  # ===========================================================================
  # SURROGATE MODEL
  # ===========================================================================
  if (surrogate == "gp") {
    # Gaussian Process
    rlang::check_installed("mlr3learners", reason = "for GP surrogate model")
    rlang::check_installed("DiceKriging", reason = "for GP surrogate model")

    # Create GP learner with nugget estimation for numerical stability
    # nugget.estim=TRUE prevents Cholesky decomposition failures when
    # objective values have low variance (e.g., many points achieving target)
    gp_learner <- mlr3mbo::default_gp()
    gp_learner$param_set$values$nugget.estim <- TRUE
    surr <- mlr3mbo::srlrn(gp_learner, archive = instance$archive)
    
  } else if (surrogate == "rf") {
    # Random Forest surrogate
    rlang::check_installed("ranger", reason = "for RF surrogate model")
    
    # Create RF learner with default settings
    rf_learner <- mlr3mbo::default_rf()
    surr <- mlr3mbo::srlrn(rf_learner, archive = instance$archive)
    
  } else {
    # Use mlr3mbo's smart default
    surr <- mlr3mbo::default_surrogate(instance)
  }
  
  # Configure surrogate to catch errors gracefully
  surr$param_set$values$catch_errors <- TRUE
  
  # ===========================================================================
  # ACQUISITION FUNCTION
  # ===========================================================================
  if (acq_function == "eic" && opt_type == "target") {
    # EIC (Expected Improvement with Constraints) for target optimization
    # Requires target objective and uses constraint surrogate for P(feasible)

    # Find the first search parameter (objective to minimize, e.g., n_total)
    search_param_names <- instance$search_space$ids()
    objective_col <- search_param_names[1]  # Typically n_total

    # Find the first target objective (for constraint)
    target_obj_name <- NULL
    target_value <- 0.80  # Default
    if (!is.null(obj_info) && !is.null(obj_info$objectives)) {
      for (obj_name in names(obj_info$objectives)) {
        obj <- obj_info$objectives[[obj_name]]
        if (obj$type == "target") {
          target_obj_name <- obj_name
          target_value <- obj$target_value
          break
        }
      }
    }

    # Constraint column is logit-transformed power
    constraint_col <- if (!is.null(target_obj_name)) {
      paste0("logit_", target_obj_name)
    } else {
      "logit_pwr_eff"  # Fallback
    }

    # Create EIC surrogates (only constraint surrogate needed for input-objective)
    eic_surrogates <- setup_eic_surrogates(
      instance = instance,
      constraint_col = constraint_col,
      surrogate_type = surrogate,
      objective_col = objective_col,
      objective_is_input = TRUE  # n_total is an input parameter
    )

    # Create EIC acquisition function
    acq <- AcqFunctionEIC$new(
      surrogate_objective = eic_surrogates$surrogate_objective,
      surrogate_constraint = eic_surrogates$surrogate_constraint,
      target_value = target_value,
      kappa = eic_kappa,
      constraint_col = constraint_col,
      objective_col = objective_col,
      objective_is_input = TRUE
    )

    # Use the constraint surrogate as the main surrogate for the optimizer
    # (EIC handles its own surrogate updates internally)
    surr <- eic_surrogates$surrogate_constraint

  } else {
    # Standard acquisition functions
    acq <- if (acq_function == "auto" || acq_function == "ei") {
      mlr3mbo::acqf("ei")
    } else if (acq_function == "cb") {
      mlr3mbo::acqf("cb")
    } else if (acq_function == "pi") {
      mlr3mbo::acqf("pi")
    } else {
      mlr3mbo::acqf("ei")
    }
  }
  
  # ===========================================================================
  # ACQUISITION OPTIMIZER
  # ===========================================================================
  acq_opt <- mlr3mbo::acqo(
    optimizer = bbotk::opt("random_search", batch_size = 1000),
    terminator = bbotk::trm("evals", n_evals = 1000)
  )
  
  # Set logging level based on verbosity
  # Use "fatal" (most restrictive valid level) to suppress bbotk's internal messages
  # Valid levels: 'fatal', 'error', 'warn', 'info', 'debug', 'trace'
  if (verbosity < 2) {
    acq_opt$param_set$values$logging_level <- "fatal"
  } else {
    acq_opt$param_set$values$logging_level <- "info"
  }
  
  # ===========================================================================
  # LOOP FUNCTION
  # ===========================================================================
  loop_fn <- if (opt_type == "multi") {
    mlr3mbo::bayesopt_parego
  } else {
    mlr3mbo::bayesopt_ego
  }
  
  # ===========================================================================
  # CREATE OPTIMIZER
  # ===========================================================================
  optimizer <- bbotk::opt(
    "mbo",
    loop_function = loop_fn,
    surrogate = surr,
    acq_function = acq,
    acq_optimizer = acq_opt
  )
  
  list(
    surrogate = surr,
    acq_function = acq,
    acq_optimizer = acq_opt,
    loop_function = loop_fn,
    optimizer = optimizer
  )
}

# =============================================================================
# HELPER: Build Convergence Trace
# =============================================================================

#' Build Convergence Trace from Archive
#'
#' @param archive_df Archive data frame
#' @param opt_type Optimization type
#' @param objectives rctbp_objectives object
#'
#' @return Data frame with convergence trace
#' @keywords internal
build_convergence_trace <- function(archive_df, opt_type, objectives) {
  if (nrow(archive_df) == 0) {
    return(data.frame())
  }
  
  obj_names <- names(objectives@objectives)
  
  # Get the first objective column that exists in archive
  obj_col <- NULL
  for (on in obj_names) {
    if (on %in% names(archive_df)) {
      obj_col <- on
      break
    }
  }
  
  if (is.null(obj_col)) {
    return(data.frame())
  }
  
  # Determine if maximizing
  obj_type <- objectives@objective_info$objectives[[obj_col]]$type
  is_max <- obj_type == "maximize"
  
  # Build cumulative best
  values <- archive_df[[obj_col]]
  if (is_max) {
    cumulative_best <- cummax(values)
  } else {
    cumulative_best <- cummin(values)
  }
  
  data.frame(eval = seq_len(nrow(archive_df)),
             value = values,
             best_so_far = cumulative_best)
}

# =============================================================================
# HELPER: Post-process Archive
# =============================================================================

#' Post-process Archive to Show Actual Metrics
#'
#' For target optimization, the objective column contains bonus-adjusted values
#' that can exceed 1.0. This function adds actual metric columns by removing
#' the optimization bonus.
#'
#' @param df Data frame (archive or result)
#' @param objectives rctbp_objectives object
#'
#' @return Modified data frame with actual metric values
#' @keywords internal
postprocess_archive <- function(df, objectives) {
  if (nrow(df) == 0)
    return(df)
  
  obj_info <- objectives@objective_info
  
  for (obj_name in names(obj_info$objectives)) {
    obj <- obj_info$objectives[[obj_name]]
    
    # For target objectives, use the actual_ column if available
    # The main column contains the stepped objective (target + bonus)
    actual_col <- paste0("actual_", obj_name)
    
    if (obj$type == "target" && actual_col %in% names(df)) {
      # Replace objective value with actual metric value
      df[[obj_name]] <- df[[actual_col]]
    } else if (obj$type == "target" && obj_name %in% names(df)) {
      # Fallback: cap probability metrics at 1.0
      if (grepl("^(pr_|pwr_)", obj_name)) {
        df[[obj_name]] <- pmin(df[[obj_name]], 1.0)
      }
    }
  }
  
  # Remove actual_ columns (now redundant)
  actual_cols <- grep("^actual_", names(df), value = TRUE)
  if (length(actual_cols) > 0) {
    df <- df[, !names(df) %in% actual_cols, drop = FALSE]
  }
  
  df
}

# =============================================================================
# EXPORTED: User-Facing Optimum Finding Functions
# =============================================================================

#' Find Surrogate-Based Optimum
#'
#' Searches over the parameter space using the fitted surrogate model to find
#' the optimal point. By default, constrains search to the observed IQR to
#' reduce extrapolation risk.
#'
#' @param result An `rctbp_pareto_result` object from [pareto_optimize()]
#' @param n_candidates Number of LHS candidates to evaluate (default 10000)
#' @param ci_level Confidence level for interval (default 0.95)
#' @param trim Proportion to trim from each tail of observed distributions
#'   (default 0.25 = IQR). Set to 0 for full domain search, 0.1 for
#'   10th-90th percentile, etc.
#'
#' @return A list with:
#'   \describe{
#'     \item{params}{Named list of optimal parameter values}
#'     \item{predicted_mean}{Surrogate's predicted objective at optimum}
#'     \item{predicted_se}{Surrogate's prediction uncertainty}
#'     \item{ci_lower, ci_upper}{Confidence interval for prediction}
#'     \item{domain_used}{Named list of bounds used for each parameter}
#'   }
#'
#' @details
#' This function uses the surrogate model fitted during optimization to search
#' for the optimum over a constrained domain. Unlike the empirical optimum
#' (which only considers observed points), this can find optima between
#' observations.
#'
#' The `trim` parameter controls how much to constrain the search domain:
#' \itemize{
#'   \item `trim = 0.25` (default): Search within IQR (25th-75th percentile)
#'   \item `trim = 0.1`: Search within 10th-90th percentile
#'   \item `trim = 0`: Search full original domain (higher extrapolation risk)
#' }
#'
#' @seealso [pareto_optimize()]
#'
#' @export
#' @examples
#' \dontrun{
#' result <- optimize_power_n(design, n_range = c(50, 500), effect_size = 0.3, ...)
#'
#' # Find surrogate optimum in IQR (default)
#' surr_opt <- find_surrogate_optimum(result)
#' surr_opt$params$n_total
#'
#' # Search full domain
#' surr_opt_full <- find_surrogate_optimum(result, trim = 0)
#' }
find_surrogate_optimum <- function(result,
                                   n_candidates = 10000,
                                   ci_level = 0.95,
                                   trim = 0.25) {
  # Validate result

  if (!inherits(result, "rctbp_pareto_result") &&
      !inherits(result, "rctbayespower::rctbp_pareto_result")) {
    cli::cli_abort(
      c(
        "{.arg result} must be an rctbp_pareto_result object",
        "x" = "Got object of class {.cls {class(result)}}",
        "i" = "Use {.fn pareto_optimize} or a wrapper function to create a result object"
      )
    )
  }
  
  # Check mbo_objects exists
  if (is.null(result@mbo_objects) ||
      length(result@mbo_objects) == 0) {
    cli::cli_abort(
      c(
        "No MBO objects available in result",
        "x" = "result@mbo_objects is NULL or empty",
        "i" = "This may happen if optimization failed before surrogate was fitted"
      )
    )
  }
  
  # Validate trim
  if (!is.numeric(trim) || trim < 0 || trim >= 0.5) {
    cli::cli_abort(c("{.arg trim} must be a number in [0, 0.5)", "x" = "Got {.val {trim}}"))
  }
  
  # Call internal implementation
  find_surrogate_optimum_internal(
    mbo_config = result@mbo_objects,
    instance = result@mbo_objects$instance,
    archive_df = result@archive,
    objectives = result@objectives,
    n_candidates = n_candidates,
    ci_level = ci_level,
    trim = trim
  )
}

#' Find Probabilistic Optimum
#'
#' Finds the minimum sample size where the GP-predicted probability of
#' achieving target power exceeds a required confidence level.
#'
#' @param result An `rctbp_pareto_result` object from [pareto_optimize()]
#' @param alpha Required confidence level for P(power >= target) (default 0.95).
#'   Higher values (e.g., 0.99, 0.999) are more conservative.
#' @param target Target power value. If NULL (default), extracted from
#'   the optimization objectives.
#' @param n_candidates Number of candidate n values to evaluate (default 1000)
#' @param use_log_n Whether to use log(n) transform for GP input (default TRUE).
#'   Recommended for power functions which have diminishing returns.
#'
#' @return A list with:
#'   \describe{
#'     \item{n_opt}{Optimal sample size (ceiling-rounded to integer)}
#'     \item{p_feas}{Feasibility probability P(power >= target) at n_opt}
#'     \item{predicted_power}{GP-predicted power at n_opt}
#'     \item{ci_lower, ci_upper}{95% confidence interval for predicted power}
#'     \item{target}{Target power value used}
#'     \item{alpha}{Confidence level used}
#'     \item{profile}{Data frame with P_feas for all candidate n values}
#'   }
#'
#' @details
#' This function implements probabilistic constraint-based sample size
#' determination. Instead of finding n where predicted power = target,
#' it finds the minimum n where we are alpha-confident that power >= target.
#'
#' **Method**:
#' 1. Fits a GP on (log(n), logit(power)) from archive data
#' 2. For each candidate n, computes P_feas = P(power >= target) using
#'    the GP's predictive distribution
#' 3. Returns the minimum n where P_feas >= alpha
#'
#' **Why logit(power)?**
#' - Power is bounded in (0, 1); logit transforms to unbounded (-Inf, Inf)
#' - GPs work better on approximately Gaussian, unbounded targets
#' - Predictions back-transformed to probability scale
#'
#' **Choosing alpha**:
#' - alpha = 0.95: 95% confident power >= target (standard)
#' - alpha = 0.99: 99% confident (more conservative, larger n)
#' - alpha = 0.999: Very high confidence for critical studies
#'
#' @seealso [find_surrogate_optimum()], [pareto_optimize()]
#'
#' @export
#' @examples
#' \dontrun{
#' result <- optimize_power_n(design, n_range = c(50, 500), effect_size = 0.3, ...)
#'
#' # Find minimum n with 95% confidence of achieving target
#' prob_opt <- find_probabilistic_optimum(result, alpha = 0.95)
#' prob_opt$n_opt
#' prob_opt$p_feas
#'
#' # More conservative: 99% confidence
#' prob_opt_99 <- find_probabilistic_optimum(result, alpha = 0.99)
#'
#' # Plot P_feas profile
#' plot(prob_opt$profile$n, prob_opt$profile$p_feas, type = "l",
#'      xlab = "Sample Size", ylab = "P(power >= target)")
#' abline(h = 0.95, lty = 2, col = "red")
#' abline(v = prob_opt$n_opt, lty = 2, col = "blue")
#' }
find_probabilistic_optimum <- function(result,
                                        alpha = 0.95,
                                        target = NULL,
                                        n_candidates = 1000,
                                        use_log_n = TRUE) {
  # Validate result
  if (!inherits(result, "rctbp_pareto_result") &&
      !inherits(result, "rctbayespower::rctbp_pareto_result")) {
    cli::cli_abort(
      c(
        "{.arg result} must be an rctbp_pareto_result object",
        "x" = "Got object of class {.cls {class(result)}}",
        "i" = "Use {.fn pareto_optimize} or a wrapper function to create a result object"
      )
    )
  }

  # Validate alpha

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    cli::cli_abort(c(
      "{.arg alpha} must be a probability in (0, 1)",
      "x" = "Got {.val {alpha}}"
    ))
  }

  # Check archive exists
  if (is.null(result@archive) || nrow(result@archive) == 0) {
    cli::cli_abort(c(
      "No archive data available in result",
      "i" = "Optimization may have failed before collecting data"
    ))
  }

  # Call internal implementation
  find_probabilistic_optimum_internal(
    archive_df = result@archive,
    objectives = result@objectives,
    target = target,
    alpha = alpha,
    n_candidates = n_candidates,
    use_log_n = use_log_n,
    verbosity = 1
  )
}
