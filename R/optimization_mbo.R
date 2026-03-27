# =============================================================================
# OPTIMIZATION MBO: mlr3mbo/bbotk Setup
# =============================================================================
# Functions for configuring Bayesian optimization infrastructure:
# - Parameter space creation from search specifications
# - Initial design generation (LHS, Sobol, random)

# =============================================================================
# PARAMETER SPACE
# =============================================================================

#' Create Parameter Space from Search Specification
#'
#' Builds a [paradox::ParamSet] from the search bounds and simplex specs.
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
# INITIAL DESIGN
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
