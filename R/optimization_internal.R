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
# - k entries (k = n_arms): All explicit [p_ctrl, p_treat1, p_treat2, ...]
# - k-1 entries: Control implicit [p_treat1, p_treat2, ...], ctrl = 1 - sum
#
# For optimization, we search over treatment probabilities and derive control.

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
#' Convention:
#' \itemize{
#'   \item k entries: `[p_ctrl, p_treat1, p_treat2, ...]` - all explicit
#'   \item k-1 entries: `[p_treat1, ...]` - control implicit (1 - sum)
#' }
#'
#' For optimization, output always has k entries (control + treatments).
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
get_search_transform <- function(type, n_arms = NULL, n_looks = NULL) {
  # Custom function: return as-is

if (is.function(type)) {
    return(type)
  }

  # Built-in transforms
  transforms <- list(
    # ==========================================================================
    # SCALAR: No transformation (default)
    # ==========================================================================
    scalar = function(x) x,

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
    cli::cli_abort(c(
      "Unknown search type: {.val {type}}",
      "i" = "Valid types: {.val {names(transforms)}}",
      "i" = "Or provide a custom transform function"
    ))
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


#' Apply Simplex Transforms to Optimizer Values
#'
#' Transforms optimizer values (which may include ILR coordinates for simplex
#' parameters) to the format expected by [build_conditions()].
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

  list(
    crossed = crossed,
    ilr_values = ilr_values,
    simplex_values = simplex_values
  )
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
#'   - `reason`: Character, reason for stopping (if any)
#'
#' @keywords internal
check_surrogate_stopping <- function(surr, archive, search_params, pred_history,
                                     min_delta, patience, target_val, obj_name,
                                     verbosity) {
  # Update surrogate with latest data
  surr$update()

  # Get archive data
  archive_df <- as.data.frame(archive$data)
  if (nrow(archive_df) == 0) {
    return(list(should_stop = FALSE, pred_mean = NA, pred_se = NA, reason = NULL))
  }

  # Get search param columns (handle ILR params)
  domain_ids <- archive$search_space$ids()
  xdt <- data.table::as.data.table(archive_df[, domain_ids, drop = FALSE])

  # Get surrogate predictions
  pred <- surr$predict(xdt)
  pred_mean_vec <- pred$mean
  pred_se_vec <- if ("se" %in% names(pred)) pred$se else sqrt(pred$var)

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
  actual_target_met <- !is.na(actual_power) && actual_power >= target_val

  # Both conditions must hold + actual value validation
  should_stop <- precision_ok && stability_ok && actual_target_met

  reason <- NULL
  if (should_stop) {
    reason <- sprintf(
      "Surrogate converged: pred=%.3f (SE=%.4f < %.4f), stable for %d iters, actual=%.3f >= %.3f",
      pred_mean, pred_se, precision_threshold, stable_count, actual_power, target_val
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
add_simplex_to_archive <- function(obj_values, simplex_values, ilr_values, search_specs) {
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
      cli::cli_warn(c(
        "Auto-detection for {n_interims} interim looks not supported",
        "i" = "Provide custom transform via {.arg search_types}"
      ))
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
      transform_fn <- get_search_transform(
        type = type_spec,
        n_arms = design@n_arms
      )

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
      result[[param_name]] <- infer_search_type(
        param_name = param_name,
        design = design,
        bounds = search[[param_name]]
      )
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
                             use_warmup,
                             patience,
                             min_delta,
                             n_cores,
                             surrogate,
                             acq_function,
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
      # Verbose mode: show warnings (useful for debugging)
      mlr3_logger$set_threshold("warn")
      bbotk_logger$set_threshold("warn")
      mlr3mbo_logger$set_threshold("warn")
    }
  }

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
    cli::cli_dl(c(
      "Type" = opt_type,
      "Search parameters" = paste(names(objectives@search), collapse = ", "),
      "Max evaluations" = max_evals,
      "Backend" = objectives@design@backend
    ))

    # Show fidelity info
    if (is_progressive) {
      n_levels <- length(n_sims_levels)
      evals_per <- evals_per_level[1]  # Assuming uniform
      fidelity_str <- paste(n_sims_levels, collapse = "\u2192")
      cli::cli_dl(c(
        "Progressive fidelity" = paste0(fidelity_str, " sims (", evals_per, " evals each, ", max_evals, " total)")
      ))
    } else {
      cli::cli_dl(c(
        "Simulations per eval" = n_sims_max
      ))
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
    use_warmup = use_warmup,
    patience = patience,
    min_delta = min_delta,
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
    objective <- bbotk::ObjectiveRFun$new(
      fun = obj_fn,
      domain = domain,
      codomain = codomain
    )

    instance <- bbotk::OptimInstanceBatchMultiCrit$new(
      objective = objective,
      terminator = bbotk::trm("evals", n_evals = max_evals)
    )
  } else {
    # Single-objective or target optimization
    objective <- bbotk::ObjectiveRFun$new(
      fun = obj_fn,
      domain = domain,
      codomain = codomain
    )

    instance <- bbotk::OptimInstanceBatchSingleCrit$new(
      objective = objective,
      terminator = bbotk::trm("evals", n_evals = max_evals)
    )
  }

  # Set archive reference for warmup recomputation
  obj_fn_result$set_archive_ref(instance$archive)

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
  init_design_pts <- generate_initial_design(
    domain = domain,
    n = init_design_size,
    method = init_design
  )

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
  target_val <- if (first_obj$type == "target") first_obj$target_value else NA

  opt_error <- NULL
  early_stopped <- FALSE
  stop_reason <- NULL
  mbo_config <- NULL

  tryCatch({
    # Evaluate initial design
    invisible(instance$eval_batch(init_design_pts))

    # Set up MBO components (surrogate, acquisition function, optimizer)
    mbo_config <- setup_mbo_components(
      instance = instance,
      opt_type = opt_type,
      surrogate = surrogate,
      acq_function = acq_function,
      verbosity = verbosity
    )

    # Run the optimizer (uses mlr3mbo's built-in loop)
    optimizer <- mbo_config$optimizer
    invisible(optimizer$optimize(instance))

  }, error = function(e) {
    opt_error <<- e
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

  # Debug: show raw archive before postprocessing
  if (verbosity >= 2) {
    cli::cli_alert_info("Raw archive columns: {paste(names(archive_df), collapse = ', ')}")
    # Show first few pwr_eff values
    if ("pwr_eff" %in% names(archive_df)) {
      sample_vals <- head(archive_df$pwr_eff, 5)
      cli::cli_alert_info("Raw pwr_eff values (first 5): {paste(round(sample_vals, 4), collapse = ', ')}")
    }
    if ("actual_pwr_eff" %in% names(archive_df)) {
      cli::cli_alert_info("actual_pwr_eff IS in archive")
      sample_actual <- head(archive_df$actual_pwr_eff, 5)
      cli::cli_alert_info("Raw actual_pwr_eff values (first 5): {paste(round(sample_actual, 4), collapse = ', ')}")
    } else {
      cli::cli_alert_warning("actual_pwr_eff NOT in archive!")
    }
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
      n_cores = n_cores,
      bf_args = bf_args,
      brms_args = brms_args,
      verbosity = verbosity
    )
    result_df <- final_result$result_df
    final_pa <- final_result$power_analysis
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
  result@best_power_analysis <- if (!is.null(final_pa)) final_pa else obj_fn_result$get_best_pa()
  # Store reference values from warmup phase (empty list if no warmup)
  result@reference_values <- obj_fn_result$get_reference_values()

  # ===========================================================================
  # DISPLAY RESULTS
  # ===========================================================================
  if (should_show(1)) {
    if (early_stopped) {
      cli::cli_alert_success("Early stopping after {nrow(archive_df)} evaluations ({format_duration(elapsed_time)})")
    } else {
      cli::cli_alert_success("Optimization complete in {format_duration(elapsed_time)}")
    }

    # Multi-objective: show Pareto info (single-objective display handled by finalize_with_surrogate)
    if (opt_type == "multi" && !is.null(result@pareto_front)) {
      cli::cli_alert_info("Found {nrow(result@pareto_front)} Pareto-optimal solutions")
    }

    # Check if target was achieved (for target optimization)
    if (opt_type == "target" && nrow(result_df) > 0) {
      obj_info <- objectives@objective_info
      first_obj_name <- names(obj_info$objectives)[1]
      first_obj <- obj_info$objectives[[first_obj_name]]

      if (first_obj$type == "target" && first_obj_name %in% names(result_df)) {
        target_val <- first_obj$target_value
        achieved_val <- result_df[[first_obj_name]][1]

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
#' Handles both standard bounds and simplex search specs.
#'
#' @param search Named list of parameter bounds or simplex specs
#' @param search_specs Named list of parsed simplex specifications (optional)
#' @return paradox::ParamSet
#' @keywords internal
create_parameter_space <- function(search, search_specs = list()) {
  # Parameters that are always doubles (even if bounds look like integers)
  always_double <- c(
    "thr_dec_eff", "thr_dec_fut", "thr_fx_eff", "thr_fx_fut",  # Decision thresholds
    "b_arm_treat", "b_covariate", "intercept", "sigma"          # Effect sizes
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
          param_list[[param_name]] <- paradox::p_dbl(
            lower = simplex_spec$min_prop,
            upper = 1 - simplex_spec$min_prop
          )
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

    # Determine if integer or double
    if (param_name %in% always_double) {
      is_integer <- FALSE
    } else {
      is_integer <- param_name == "n_total" ||
        (all(bounds == floor(bounds)) && max(bounds) - min(bounds) >= 1)
    }

    if (is_integer) {
      param_list[[param_name]] <- paradox::p_int(
        lower = as.integer(bounds[1]),
        upper = as.integer(bounds[2])
      )
    } else {
      param_list[[param_name]] <- paradox::p_dbl(
        lower = bounds[1],
        upper = bounds[2]
      )
    }
  }

  # Create parameter set
  do.call(paradox::ps, param_list)
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
                                use_warmup,
                                patience,
                                min_delta,
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
  secondary_specs <- objectives@secondary   # e.g., list(n_total = "minimize")

  # Fidelity schedule info
  is_progressive <- fidelity_schedule$is_progressive[1]
  n_sims_max <- max(fidelity_schedule$n_sims)


  # Warmup phase state (mutable)
  # After warmup: archive is updated to use consistent reference-based bonus
  warmup_active <- use_warmup && is_progressive
  reference_values <- list()  # Populated after warmup: list(n_total = 150, ...)

  # Archive reference (set via set_archive_ref after instance creation)
  # Used to update warmup points with consistent bonus after warmup ends
  archive_ref <- NULL

  # Track warmup evaluation data for archive recomputation
  # Each entry: list(row_idx = ..., xs = ..., actual_val = ..., target_achieved = ...)
  warmup_data <- list()

  # Evaluation counter for progress
  eval_count <- 0

  # Track best result for progress display
  best_value <- -Inf
  best_params <- NULL
  best_actual <- NULL
  best_pa_result <- NULL  # Full power_analysis result from best solution

  # Track best during warmup (any point achieving target, smallest n_total)
  # Less restrictive than best_params which requires optimal window
  warmup_best_params <- NULL
  warmup_best_n <- Inf


  # Compute bonus for secondary objectives (generalized beyond n_total)
  #
  # During warmup: inverse bonus (param_min/param for minimize)
  #   - Range: (0, 1] for minimize, [0, 1) for maximize
  #   - Bounds-independent, always rewards preferred direction
  #
  # After warmup: reference-based bonus (1 - param/ref for minimize)
  #   - Positive when better than reference, negative when worse
  #   - Creates gradient pushing toward smaller n (for minimize)
  #   - Not clamped: n >> ref gives large negative bonus
  #
  # Combined with power component (0.5 each) for total objective
  compute_secondary_bonus <- function(xs) {
    if (length(secondary_specs) == 0) return(0)

    total_bonus <- 0
    n_params <- 0

    for (param in names(secondary_specs)) {
      if (!param %in% search_params) next
      # Only handle standard bounds (not simplex specs)
      bounds <- search_params_bounds[[param]]
      if (!is.numeric(bounds) || length(bounds) != 2) next

      n_params <- n_params + 1
      direction <- secondary_specs[[param]]
      param_min <- min(bounds)
      param_max <- max(bounds)
      param_val <- xs[[param]]

      if (warmup_active) {
        # Inverse bonus (bounds-independent)
        if (direction == "minimize") {
          # param_min/param: 1 at min, approaches 0 as param increases
          total_bonus <- total_bonus + (param_min / param_val)
        } else {
          # param/param_max: 1 at max, approaches 0 as param decreases
          total_bonus <- total_bonus + (param_val / param_max)
        }
      } else {
        # Reference-based bonus (can be negative)
        ref_val <- reference_values[[param]]
        if (is.null(ref_val) || is.na(ref_val)) {
          # Fallback to inverse if no reference
          if (direction == "minimize") {
            total_bonus <- total_bonus + (param_min / param_val)
          } else {
            total_bonus <- total_bonus + (param_val / param_max)
          }
        } else {
          if (direction == "minimize") {
            # 1 - param/ref: positive when param < ref, negative when param > ref
            # e.g., ref=100: n=50 -> +0.5, n=100 -> 0, n=200 -> -1.0
            total_bonus <- total_bonus + (1 - param_val / ref_val)
          } else {
            # param/ref - 1: positive when param > ref, negative when param < ref
            total_bonus <- total_bonus + (param_val / ref_val - 1)
          }
        }
      }
    }

    if (n_params == 0) return(0)
    total_bonus / n_params  # Average across secondary objectives
  }

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
    prev_level <- if (eval_count > 1) fidelity_schedule$fidelity_level[min(eval_count - 1, nrow(fidelity_schedule))] else 0

    # Handle warmup → main phase transition
    if (warmup_active && current_fidelity_level > 1 && prev_level == 1) {
      # Warmup phase complete - extract reference values from best result
      warmup_active <<- FALSE

      # Set reference values from best warmup result
      # If best_params is not set (no points in optimal window), use the tracked
      # best_actual which has the smallest n_total among all target-achieving points
      for (param in names(secondary_specs)) {
        if (!is.null(best_params) && param %in% names(best_params)) {
          reference_values[[param]] <<- best_params[[param]]
        } else if (!is.null(warmup_best_params) && param %in% names(warmup_best_params)) {
          # Fallback to warmup best (any point achieving target with smallest n)
          reference_values[[param]] <<- warmup_best_params[[param]]
        }
      }

      if (verbosity >= 1) {
        if (length(reference_values) > 0) {
          # Filter to numeric values only
          numeric_refs <- reference_values[vapply(reference_values, is.numeric, logical(1))]
          if (length(numeric_refs) > 0) {
            ref_str <- paste(
              paste0(names(numeric_refs), "=", round(unlist(numeric_refs), 1)),
              collapse = ", "
            )
            cli::cli_alert_success("Warmup complete. Reference values: {ref_str}")
          } else {
            cli::cli_alert_warning("Warmup complete. No numeric reference values found.")
          }
        } else {
          cli::cli_alert_warning("Warmup complete. No reference values extracted (no points achieved target).")
        }
      }

      # Recompute warmup archive entries with reference-based bonus
      # This ensures the surrogate trains on consistent data
      recompute_warmup_archive()
    }

    # Show fidelity level transition (for progressive fidelity)
    if (is_progressive && verbosity >= 1 && current_fidelity_level != prev_level) {
      # Compute actual range from schedule (handles non-uniform evals_per_step)
      level_rows <- which(fidelity_schedule$fidelity_level == current_fidelity_level)
      level_start <- min(level_rows)
      level_end <- max(level_rows)
      n_levels <- length(unique(fidelity_schedule$n_sims))

      # Determine phase label
      phase_label <- if (warmup_active && current_fidelity_level == 1) {
        " (warmup)"
      } else if (current_fidelity_level == 1) {
        " (exploration)"
      } else if (current_fidelity_level == n_levels) {
        " (refinement)"
      } else {
        ""
      }
      cli::cli_alert_info("[{level_start}-{level_end}/{max_evals}] n_sims={current_n_sims}{phase_label}")
    }

    # Apply simplex transforms if any search_specs exist
    ilr_values <- list()
    simplex_values <- list()

    if (length(search_specs) > 0) {
      transform_result <- apply_simplex_transforms(xs, search_specs)
      crossed_list <- transform_result$crossed
      ilr_values <- transform_result$ilr_values
      simplex_values <- transform_result$simplex_values
    } else {
      # No simplex params: use xs directly
      crossed_list <- lapply(search_params, function(p) xs[[p]])
      names(crossed_list) <- search_params
    }

    # Create conditions object
    conditions <- tryCatch({
      build_conditions(
        design = design,
        crossed = crossed_list,
        constant = constant
      )
    }, error = function(e) {
      if (verbosity >= 2) cli::cli_warn("Failed to build conditions: {e$message}")
      return(NULL)
    })

    if (is.null(conditions)) {
      result <- create_worst_result(obj_info)
      result$n_sims_used <- current_n_sims
      return(result)
    }

    # Run power analysis with current fidelity level's n_sims
    pa_result <- tryCatch({
      power_analysis(
        conditions = conditions,
        n_sims = current_n_sims,
        n_cores = n_cores,
        bf_args = bf_args,
        brms_args = brms_args,
        verbosity = 0,
        run = TRUE
      )
    }, error = function(e) {
      if (verbosity >= 2) cli::cli_warn("Power analysis failed: {e$message}")
      return(NULL)
    })

    if (is.null(pa_result) || nrow(pa_result@results_conditions) == 0) {
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
        cli::cli_alert_info("Processing {obj_name}: type={obj$type}, in_results={obj_name %in% names(results)}")
      }

      if (obj$type == "target") {
        # Two-component objective with equal 0.5 weights:
        #   0.5 * power_component +    Quadratic approach, clamped at target
        #   0.5 * secondary_bonus      Only when target achieved
        #
        # Power component: (min(achieved, target) / target)^2
        #   - Quadratic ramp from 0 to 1 as power approaches target
        #   - Clamped at target (no benefit for overshooting)
        #
        # Secondary bonus (computed via compute_secondary_bonus()):
        #   - During warmup: inverse bonus (param_min/param for minimize)
        #   - After warmup: reference-based bonus (1 - param/ref for minimize)
        #   - Only added when target is achieved
        if (obj_name %in% names(results)) {
          achieved <- results[[obj_name]][1]
          target_val <- obj$target_value
          actual_values[[obj_name]] <- achieved

          if (obj$direction == ">=") {
            # For >= targets (e.g., power >= 0.80)
            # Quadratic approach clamped at target
            power_clamped <- min(achieved, target_val)
            power_component <- (power_clamped / target_val)^2
            target_achieved <- achieved >= target_val

            if (target_achieved) {
              secondary_bonus <- compute_secondary_bonus(xs)
              obj_values[[obj_name]] <- 0.5 * power_component + 0.5 * secondary_bonus
              if (verbosity >= 2) {
                cli::cli_alert_info("Target achieved: pwr={round(achieved, 3)}, bonus={round(secondary_bonus, 3)}, composite={round(obj_values[[obj_name]], 4)}")
              }
            } else {
              obj_values[[obj_name]] <- 0.5 * power_component
              if (verbosity >= 2) {
                cli::cli_alert_info("Target NOT achieved: pwr={round(achieved, 3)}, composite={round(obj_values[[obj_name]], 4)}")
              }
            }
          } else {
            # For <= targets (e.g., type I error <= 0.05)
            # Quadratic approach: lower is better, clamped at target
            error_clamped <- max(achieved, target_val)
            power_component <- (target_val / max(error_clamped, 1e-10))^2
            target_achieved <- achieved <= target_val

            if (target_achieved) {
              secondary_bonus <- compute_secondary_bonus(xs)
              obj_values[[obj_name]] <- 0.5 * power_component + 0.5 * secondary_bonus
            } else {
              obj_values[[obj_name]] <- 0.5 * power_component
            }
          }
        } else {
          obj_values[[obj_name]] <- 0  # Worst case
          actual_values[[obj_name]] <- 0
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

      # During warmup: track any point achieving target with smallest n_total
      # (less restrictive than optimal window, ensures we have a reference)
      if (warmup_active && target_achieved) {
        primary_param <- "n_total"  # Default to n_total for warmup reference
        if (primary_param %in% names(xs)) {
          current_n <- xs[[primary_param]]
          if (current_n < warmup_best_n) {
            warmup_best_n <<- current_n
            warmup_best_params <<- xs
          }
        }
      }

      # Check if in optimal window (close to target, not overshooting)
      # This ensures "new best" is consistent with patience logic
      in_optimal_window <- target_achieved && actual_val < target_val + min_delta

      if (in_optimal_window) {
        # "New best" = smaller primary minimize param (usually n_total)
        # Use first search param that's a secondary objective with "minimize"
        minimize_params <- names(secondary_specs)[vapply(secondary_specs, function(x) x == "minimize", logical(1))]
        primary_param <- intersect(search_params, minimize_params)[1]

        if (!is.null(primary_param) && !is.na(primary_param)) {
          current_param_val <- xs[[primary_param]]
          best_param_val <- if (!is.null(best_params)) best_params[[primary_param]] else Inf

          if (current_param_val < best_param_val) {
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

    # Add MCSE values for tracked metrics (se_<metric> pattern)
    for (obj_name in names(obj_info$objectives)) {
      se_col <- paste0("se_", obj_name)
      if (se_col %in% names(results)) {
        obj_values[[se_col]] <- results[[se_col]][1]
      }
    }

    # Add simplex values for archive storage (both ILR and proportions)
    if (length(simplex_values) > 0) {
      obj_values <- add_simplex_to_archive(
        obj_values, simplex_values, ilr_values, search_specs
      )
    }

    # Add n_sims_used for fidelity-based weighting
    obj_values$n_sims_used <- current_n_sims

    # Show progress at refresh intervals
    if (refresh > 0 && verbosity >= 1 && eval_count %% refresh == 0) {
      # Format current evaluation (use 3 decimals for doubles, 0 for integers)
      param_str <- paste(search_params, "=",
                         sapply(search_params, function(p) {
                           val <- xs[[p]]
                           if (p == "n_total" || (val == floor(val) && abs(val) >= 1)) {
                             format(round(val, 0), nsmall = 0)
                           } else {
                             format(round(val, 3), nsmall = 3)
                           }
                         }),
                         collapse = ", ")
      obj_str <- paste(names(actual_values), "=",
                       round(unlist(actual_values), 3),
                       collapse = ", ")

      cli::cli_alert_info("[{eval_count}/{max_evals}] {param_str} | {obj_str}")
    }

    # Always show new best results (if verbosity >= 1)
    if (is_new_best && verbosity >= 1 && (refresh == 0 || eval_count %% refresh != 0)) {
      param_str <- paste(search_params, "=",
                         sapply(search_params, function(p) {
                           val <- xs[[p]]
                           if (p == "n_total" || (val == floor(val) && abs(val) >= 1)) {
                             format(round(val, 0), nsmall = 0)
                           } else {
                             format(round(val, 3), nsmall = 3)
                           }
                         }),
                         collapse = ", ")
      obj_str <- paste(names(actual_values), "=",
                       round(unlist(actual_values), 3),
                       collapse = ", ")
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
      cli::cli_alert_info("Returning to bbotk: {paste(names(obj_values), '=', round(unlist(obj_values), 4), collapse = ', ')}")
    }

    # Store warmup data for archive recomputation after warmup ends
    # This allows us to recalculate composite values with reference-based bonus
    if (warmup_active) {
      first_obj <- names(obj_info$objectives)[1]
      first_obj_info <- obj_info$objectives[[first_obj]]
      actual_val <- if (first_obj %in% names(actual_values)) actual_values[[first_obj]] else NA
      target_achieved <- if (first_obj_info$type == "target" && !is.na(actual_val)) {
        if (first_obj_info$direction == ">=") {
          actual_val >= first_obj_info$target_value
        } else {
          actual_val <= first_obj_info$target_value
        }
      } else {
        FALSE
      }
      warmup_data[[length(warmup_data) + 1]] <<- list(
        row_idx = eval_count,
        xs = xs,
        actual_val = actual_val,
        target_achieved = target_achieved
      )
    }

    obj_values
  }

  # Function to recompute warmup archive values with reference-based bonus
  recompute_warmup_archive <- function() {
    if (is.null(archive_ref) || length(warmup_data) == 0 || length(reference_values) == 0) {
      return(invisible(NULL))
    }

    # Get the objective name and info
    first_obj <- names(obj_info$objectives)[1]
    first_obj_info <- obj_info$objectives[[first_obj]]
    if (first_obj_info$type != "target") {
      return(invisible(NULL))  # Only recompute for target optimization
    }

    target_val <- first_obj_info$target_value

    if (verbosity >= 2) {
      cli::cli_alert_info("Recomputing {length(warmup_data)} warmup archive entries with reference-based bonus...")
    }

    # Recompute each warmup point's composite value
    for (wd in warmup_data) {
      xs <- wd$xs
      actual_val <- wd$actual_val
      target_achieved <- wd$target_achieved
      row_idx <- wd$row_idx

      # Compute power component (same as before)
      power_clamped <- min(actual_val, target_val)
      power_component <- (power_clamped / target_val)^2

      # Compute reference-based bonus (NOT inverse bonus)
      if (target_achieved) {
        total_bonus <- 0
        n_params <- 0
        for (param in names(secondary_specs)) {
          if (!param %in% search_params) next
          bounds <- search_params_bounds[[param]]
          if (!is.numeric(bounds) || length(bounds) != 2) next

          n_params <- n_params + 1
          direction <- secondary_specs[[param]]
          param_val <- xs[[param]]
          ref_val <- reference_values[[param]]

          if (!is.null(ref_val) && !is.na(ref_val)) {
            if (direction == "minimize") {
              total_bonus <- total_bonus + (1 - param_val / ref_val)
            } else {
              total_bonus <- total_bonus + (param_val / ref_val - 1)
            }
          }
        }
        secondary_bonus <- if (n_params > 0) total_bonus / n_params else 0
        new_composite <- 0.5 * power_component + 0.5 * secondary_bonus
      } else {
        new_composite <- 0.5 * power_component
      }

      # Update archive (bbotk archive uses data.table)
      if (row_idx <= nrow(archive_ref$data)) {
        data.table::set(archive_ref$data, i = as.integer(row_idx), j = first_obj, value = new_composite)
      }
    }

    if (verbosity >= 1) {
      cli::cli_alert_success("Archive updated: {length(warmup_data)} warmup points recomputed with reference-based bonus")
    }

    invisible(NULL)
  }

  # Return list with objective function and getters
  list(
    fn = obj_fn,
    get_best_pa = function() best_pa_result,
    get_reference_values = function() reference_values,
    set_archive_ref = function(ref) {
      archive_ref <<- ref
    },
    recompute_warmup = recompute_warmup_archive
  )
}

#' Create Worst Result for Failed Evaluations
#'
#' @param obj_info Parsed objective info
#' @return Named list with worst values
#' @keywords internal
create_worst_result <- function(obj_info) {
  result <- list()
  for (obj_name in names(obj_info$objectives)) {
    obj <- obj_info$objectives[[obj_name]]
    if (obj$type == "target") {
      # Target objective is in [0, 1], worst is 0
      result[[obj_name]] <- 0
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
  design <- switch(method,
    "lhs" = paradox::generate_design_lhs(domain, n),
    "random" = paradox::generate_design_random(domain, n),
    "sobol" = paradox::generate_design_sobol(domain, n),
    paradox::generate_design_lhs(domain, n)  # Default
  )

  design$data
}

# =============================================================================
# HELPER: Finalize with Surrogate Prediction
# =============================================================================

#' Select Best Point via Surrogate Model
#'
#' Uses the fitted surrogate to find the best predicted point among
#' archive observations. Used as fallback when high-fidelity selection
#' is not available.
#'
#' @param mbo_config MBO configuration with surrogate
#' @param archive_df Archive data frame
#' @param search_params Parameter names to extract
#' @return Named list of best parameter values
#' @keywords internal
select_best_via_surrogate <- function(mbo_config, archive_df, search_params) {
  surr <- mbo_config$surrogate
  surr$update()

  xdt <- data.table::as.data.table(archive_df[, search_params, drop = FALSE])
  pred <- surr$predict(xdt)
  best_idx <- which.max(pred$mean)
  as.list(archive_df[best_idx, search_params, drop = FALSE])
}


#' Finalize Optimization Using Surrogate Model
#'
#' Uses the fitted surrogate model to find the best point (smoothed prediction),
#' then runs a final confirmation simulation at that point to get precise
#' metrics with standard errors.
#'
#' @param mbo_config MBO configuration from setup_mbo_components
#' @param instance bbotk optimization instance
#' @param objectives rctbp_objectives object
#' @param fidelity_schedule Data frame with fidelity schedule (uses max n_sims)
#' @param n_cores Parallel cores
#' @param bf_args BayesFlow arguments
#' @param brms_args brms arguments
#' @param verbosity Output level
#'
#' @return List with result_df and power_analysis
#' @keywords internal
finalize_with_surrogate <- function(mbo_config, instance, objectives,
                                    fidelity_schedule, n_cores, bf_args, brms_args,
                                    verbosity) {
  # Use max n_sims for final confirmation
  n_sims <- max(fidelity_schedule$n_sims)
  should_show <- function(level) verbosity >= level

  # Get archive and objective info
  archive_df <- as.data.frame(instance$archive$data)
  search_params <- names(objectives@search)
  obj_name <- names(objectives@objectives)[1]
  actual_col <- paste0("actual_", obj_name)

 # ===========================================================================
  # BEST PRACTICE: Select optimal point from HIGH-FIDELITY observations only
 # ===========================================================================
  # Research (arXiv:2410.00544) recommends measuring simple regret at highest
 # fidelity only. Low-fidelity observations are noisy and can mislead selection.

  is_progressive <- any(fidelity_schedule$is_progressive)

  if (is_progressive && "n_sims_used" %in% names(archive_df)) {
    # Filter to highest fidelity observations
    max_n_sims <- max(archive_df$n_sims_used)
    high_fidelity_df <- archive_df[archive_df$n_sims_used == max_n_sims, , drop = FALSE]

    if (nrow(high_fidelity_df) > 0) {
      # For target optimization: select point with highest composite objective
      # (composite = power component + secondary bonus, favors smaller n_total)
      # For non-target: select point with highest/lowest raw objective
      obj_info <- objectives@objective_info
      first_obj <- obj_info$objectives[[1]]

      if (first_obj$type == "target" && obj_name %in% names(high_fidelity_df)) {
        # Use composite objective (pwr_eff) for selection, not actual power
        best_hf_idx <- which.max(high_fidelity_df[[obj_name]])
        surrogate_pred <- if (actual_col %in% names(high_fidelity_df)) {
          high_fidelity_df[[actual_col]][best_hf_idx]
        } else {
          high_fidelity_df[[obj_name]][best_hf_idx]
        }
        if (should_show(2)) {
          composite_val <- high_fidelity_df[[obj_name]][best_hf_idx]
          cli::cli_alert_info("Selected by composite objective: {round(composite_val, 4)}")
        }
      } else if (actual_col %in% names(high_fidelity_df)) {
        # Non-target: use actual values
        best_hf_idx <- which.max(high_fidelity_df[[actual_col]])
        surrogate_pred <- high_fidelity_df[[actual_col]][best_hf_idx]
      } else if (obj_name %in% names(high_fidelity_df)) {
        best_hf_idx <- which.max(high_fidelity_df[[obj_name]])
        surrogate_pred <- high_fidelity_df[[obj_name]][best_hf_idx]
      } else {
        best_hf_idx <- 1
        surrogate_pred <- NA
      }

      best_params <- as.list(high_fidelity_df[best_hf_idx, search_params, drop = FALSE])

      if (should_show(1)) {
        n_hf <- nrow(high_fidelity_df)
        cli::cli_alert_info("Selecting best from {n_hf} high-fidelity (n_sims={max_n_sims}) evaluations")
      }
    } else {
      # Fallback to surrogate if no high-fidelity data
      best_params <- select_best_via_surrogate(mbo_config, archive_df, search_params)
      surrogate_pred <- NA
    }
  } else {
    # Non-progressive: use surrogate to select best
    best_params <- select_best_via_surrogate(mbo_config, archive_df, search_params)
    surrogate_pred <- NA
  }

  # Surrogate uncertainty not meaningful at training points (SE ≈ 0)
  surrogate_se <- NA
  surrogate_ci_lower <- NA
  surrogate_ci_upper <- NA

  if (should_show(1)) {
    cli::cli_alert_info("Running final evaluation...")
  }

  # Run final confirmation simulation at surrogate-selected best point
  design <- objectives@design
  search_specs <- objectives@search_specs
  constant <- objectives@constant

  # Apply simplex transforms if any search_specs exist
  if (length(search_specs) > 0) {
    transform_result <- apply_simplex_transforms(best_params, search_specs)
    crossed_list <- transform_result$crossed
  } else {
    crossed_list <- lapply(search_params, function(p) best_params[[p]])
    names(crossed_list) <- search_params
  }

  conditions <- tryCatch({
    build_conditions(
      design = design,
      crossed = crossed_list,
      constant = constant
    )
  }, error = function(e) {
    if (should_show(2)) cli::cli_warn("Failed to build conditions: {e$message}")
    return(NULL)
  })

  if (is.null(conditions)) {
    # Fallback to bbotk's best
    result_df <- as.data.frame(instance$archive$best())
    result_df <- postprocess_archive(result_df, objectives)
    return(list(result_df = result_df, power_analysis = NULL))
  }

  # Run power analysis with same n_sims as optimization
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
    if (should_show(2)) cli::cli_warn("Final simulation failed: {e$message}")
    return(NULL)
  })

  if (is.null(pa_result) || nrow(pa_result@results_conditions) == 0) {
    # Fallback to bbotk's best
    result_df <- as.data.frame(instance$archive$best())
    result_df <- postprocess_archive(result_df, objectives)
    return(list(result_df = result_df, power_analysis = NULL))
  }

  # Build result_df from final simulation
  final_results <- pa_result@results_conditions[1, ]
  result_df <- data.frame(final_results)

  # Add surrogate uncertainty columns
  result_df$surrogate_pred <- surrogate_pred
  result_df$surrogate_se <- surrogate_se
  result_df$surrogate_ci_lower <- surrogate_ci_lower
  result_df$surrogate_ci_upper <- surrogate_ci_upper

  # Report final result
  if (should_show(1)) {
    final_val <- round(result_df[[obj_name]], 3)
    n_total_round <- round(best_params$n_total)
    se_col <- paste0("se_", obj_name)

    if (se_col %in% names(result_df)) {
      final_se <- round(result_df[[se_col]], 3)
      cli::cli_alert_success(
        "Optimal: n_total={n_total_round}, {obj_name}={final_val} \u00b1 {final_se}"
      )
    } else {
      cli::cli_alert_success(
        "Optimal: n_total={n_total_round}, {obj_name}={final_val}"
      )
    }
  }

  list(result_df = result_df, power_analysis = pa_result)
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
#' @param surrogate Surrogate model type
#' @param acq_function Acquisition function name
#' @param verbosity Verbosity level for logging
#'
#' @return List with optimizer configuration
#'
#' @details
#' **Multi-fidelity note**: The archive stores `n_sims_used` for each evaluation
#' to enable precision-based observation weighting. Currently, weights are not
#' used during surrogate training (mlr3mbo limitation). The progressive fidelity
#' schedule still provides value through cost-efficient exploration at low
#' n_sims followed by precise refinement at high n_sims.
#'
#' @keywords internal
setup_mbo_components <- function(instance,
                                 opt_type,
                                 surrogate,
                                 acq_function,
                                 verbosity = 1) {

  # ===========================================================================
  # SURROGATE MODEL
  # ===========================================================================
  # NOTE: For multi-fidelity optimization, ideally we'd use n_sims_used from
  # the archive to weight observations (higher n_sims = more precise = higher weight).
  # mlr3mbo doesn't natively support observation weights in surrogate training.
  # Future enhancement: custom surrogate wrapper with weighted fitting.
  if (surrogate == "gp") {
    # Gaussian Process
    rlang::check_installed("mlr3learners", reason = "for GP surrogate model")
    rlang::check_installed("DiceKriging", reason = "for GP surrogate model")

    # Create GP learner with default settings
    gp_learner <- mlr3mbo::default_gp()
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
  acq <- if (acq_function == "auto" || acq_function == "ei") {
    mlr3mbo::acqf("ei")
  } else if (acq_function == "cb") {
    mlr3mbo::acqf("cb")
  } else if (acq_function == "pi") {
    mlr3mbo::acqf("pi")
  } else {
    mlr3mbo::acqf("ei")
  }

  # ===========================================================================
  # ACQUISITION OPTIMIZER
  # ===========================================================================
  acq_opt <- mlr3mbo::acqo(
    optimizer = bbotk::opt("random_search", batch_size = 1000),
    terminator = bbotk::trm("evals", n_evals = 1000)
  )

  # Set logging level based on verbosity
  if (verbosity < 2) {
    acq_opt$param_set$values$logging_level <- "warn"
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
  optimizer <- bbotk::opt("mbo",
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

  data.frame(
    eval = seq_len(nrow(archive_df)),
    value = values,
    best_so_far = cumulative_best
  )
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
  if (nrow(df) == 0) return(df)

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
