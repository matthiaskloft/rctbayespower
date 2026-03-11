# =============================================================================
# CUSTOM ACQUISITION FUNCTION: Expected Improvement with Constraints (EIC)
# =============================================================================
# EIC = EI(objective) x P(constraint satisfied)
# Uses two surrogates: one for objective (minimize n), one for constraint (power)
#
# Reference: Schonlau, M., Welch, W. J., & Jones, D. R. (1998).
# Global versus local search in constrained optimization of computer models.
#
# NOTE: AcqFunctionEIC inherits from mlr3mbo::AcqFunction (R6), which is an
# optional dependency. The class is created in .onLoad to avoid loading mlr3mbo
# at parse time (which breaks roxygen when mlr3mbo is not installed).

#' Expected Improvement with Constraints (EIC) Acquisition Function
#'
#' Custom acquisition function that multiplies Expected Improvement by the
#' probability of feasibility. Optimized for sample size optimization where:
#' - Objective (n_total) is an input parameter, giving deterministic EI
#' - Constraint (power) is an output, modeled with GP on logit scale
#'
#' @details
#' The acquisition value is: `EIC(x) = EI(x) * P(power(x) >= target)`
#'
#' For sample size optimization (objective is input):
#' - `EI(x) = max(0, n_best - n)` where n_best is smallest feasible n observed
#'
#' P(feasible) is computed from the constraint surrogate on logit scale:
#' `P(power >= target) = Phi((mu_logit - logit(target)) / sigma_logit)`
#'
#' Optional conservatism via `kappa` parameter uses lower confidence bound:
#' `effective_mean = mu_logit - kappa * sigma_logit`
#'
#' @section Kappa values:
#' - `kappa = 0`: Uses posterior mean for P(feasible). Neutral, may recommend
#'   sample sizes with ~50% chance of meeting target.
#' - `kappa = 1.96`: Uses 97.5% lower confidence bound. Recommendations have
#'   high probability (97.5%) of meeting the target.
#' - `kappa = 2.58`: Uses 99.5% LCB. Very conservative.
#'
#' @section Fields:
#' \describe{
#'   \item{surrogate_constraint}{Surrogate for constraint (power on logit scale)}
#'   \item{target_value}{Target value on original scale (e.g., 0.80 for power)}
#'   \item{target_logit}{Target on logit scale}
#'   \item{kappa}{Conservatism parameter (0 = neutral, 1.96 = 97.5% LCB)}
#'   \item{y_best}{Current best feasible objective value (for EI computation)}
#'   \item{constraint_col}{Name of the constraint column in archive}
#'   \item{objective_col}{Name of the objective column (input for minimization)}
#'   \item{objective_is_input}{TRUE if objective is an input (e.g., n_total)}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{`initialize(surrogate_objective, surrogate_constraint, target_value, kappa, constraint_col, objective_col, objective_is_input)`}{Create EIC acquisition function}
#'   \item{`update(archive)`}{Update with current archive}
#' }
#'
#' @name AcqFunctionEIC
#' @export
AcqFunctionEIC <- NULL

#' Build the AcqFunctionEIC R6 class definition
#'
#' Called from `.onLoad` when mlr3mbo is available. Separated to keep the class
#' body in this file rather than in zzz.R.
#'
#' @return An R6ClassGenerator
#' @keywords internal
.build_AcqFunctionEIC <- function() {
  R6::R6Class(
    "AcqFunctionEIC",
    inherit = mlr3mbo::AcqFunction,

    public = list(
      surrogate_constraint = NULL,
      target_value = NULL,
      target_logit = NULL,
      kappa = NULL,
      y_best = NULL,
      constraint_col = NULL,
      objective_col = NULL,
      objective_is_input = NULL,

      initialize = function(surrogate_objective,
                            surrogate_constraint,
                            target_value = 0.80,
                            kappa = 0,
                            constraint_col = "logit_pwr_eff",
                            objective_col = "n_total",
                            objective_is_input = TRUE) {
        self$surrogate_constraint <- surrogate_constraint
        self$target_value <- target_value
        self$target_logit <- logit_transform(target_value)
        self$kappa <- kappa
        self$constraint_col <- constraint_col
        self$objective_col <- objective_col
        self$objective_is_input <- objective_is_input

        super$initialize(
          id = "eic",
          constants = paradox::ps(),
          surrogate = surrogate_objective,
          requires_predict_type_se = TRUE,
          direction = "minimize",
          label = "Expected Improvement with Constraints",
          man = NA_character_
        )
      },

      update = function(archive) {
        # Only update parent surrogate if not in input-objective mode
        if (!self$objective_is_input) {
          super$update(archive)
        }

        # Update constraint surrogate
        tryCatch({
          self$surrogate_constraint$update()
        }, error = function(e) {
          cli::cli_warn("Failed to update constraint surrogate: {e$message}")
        })

        # Find best feasible objective value
        data <- archive$data
        obj_col <- self$objective_col

        # Look for actual power column (try several naming patterns)
        power_col <- NULL
        for (pattern in c("actual_pwr_", "pwr_")) {
          power_cols <- grep(paste0("^", pattern), names(data), value = TRUE)
          if (length(power_cols) > 0) {
            power_col <- power_cols[1]
            break
          }
        }

        if (!is.null(power_col) && power_col %in% names(data)) {
          feasible_mask <- data[[power_col]] >= self$target_value
          if (any(feasible_mask, na.rm = TRUE)) {
            # Best feasible = minimum objective among feasible points
            self$y_best <- min(data[[obj_col]][feasible_mask], na.rm = TRUE)
          } else {
            # No feasible point yet - use worst objective + penalty
            self$y_best <- max(data[[obj_col]], na.rm = TRUE) + 1
          }
        } else {
          # Fallback: use minimum objective
          self$y_best <- min(data[[obj_col]], na.rm = TRUE)
        }

        invisible(self)
      },

      .eval_dt = function(xdt) {
        # Compute Expected Improvement based on objective type
        if (self$objective_is_input) {
          # Input-objective mode: n_total is known exactly (no uncertainty)
          # EI is deterministic: max(0, y_best - x)
          obj_values <- xdt[[self$objective_col]]
          improvement <- self$y_best - obj_values
          ei <- pmax(0, improvement)
        } else {
          # Output-objective mode: use surrogate prediction
          pred_obj <- self$surrogate$predict(xdt)
          mu_obj <- pred_obj$mean
          se_obj <- if ("se" %in% names(pred_obj)) pred_obj$se else sqrt(pred_obj$var)

          # Standard EI formula for minimization
          # EI = (y_best - mu) * Phi(z) + se * phi(z) where z = (y_best - mu) / se
          improvement <- self$y_best - mu_obj
          z_ei <- improvement / pmax(se_obj, 1e-8)
          ei <- improvement * stats::pnorm(z_ei) + se_obj * stats::dnorm(z_ei)
          ei[se_obj < 1e-8] <- pmax(0, improvement[se_obj < 1e-8])
        }

        # Predict constraint (on logit scale)
        pred_constr <- self$surrogate_constraint$predict(xdt)
        mu_logit <- pred_constr$mean
        se_logit <- if ("se" %in% names(pred_constr)) {
          pred_constr$se
        } else {
          sqrt(pred_constr$var)
        }
        se_logit <- pmax(se_logit, 1e-8)

        # Apply conservatism (use LCB if kappa > 0)
        # LCB means we're pessimistic about power -> more conservative
        effective_logit <- if (self$kappa > 0) {
          mu_logit - self$kappa * se_logit
        } else {
          mu_logit
        }

        # P(feasible) = P(logit_power >= logit_target)
        # = Phi((effective_logit - logit_target) / se_logit)
        z_constr <- (effective_logit - self$target_logit) / se_logit
        p_feasible <- stats::pnorm(z_constr)

        # Handle very certain predictions
        p_feasible[se_logit < 1e-8] <- as.numeric(
          mu_logit[se_logit < 1e-8] >= self$target_logit
        )

        # EIC = EI * P(feasible)
        eic <- ei * p_feasible

        data.table::data.table(acq_eic = eic)
      }
    )
  )
}


#' Setup EIC Surrogate Configuration
#'
#' Creates surrogate model(s) for EIC. For sample size optimization where the
#' objective is an input (n_total), only the constraint surrogate is needed.
#' For output-objective cases, both surrogates are created.
#'
#' @param instance bbotk optimization instance
#' @param constraint_col Column name for constraint on logit scale (e.g., "logit_pwr_eff")
#' @param surrogate_type "gp" or "rf"
#' @param objective_col Column name for objective (NULL for input-objective)
#' @param objective_is_input TRUE if objective is an input parameter
#'
#' @return List with:
#'   - `surrogate_objective`: Surrogate for objective (NULL if input-objective)
#'   - `surrogate_constraint`: Surrogate for constraint
#'
#' @keywords internal
setup_eic_surrogates <- function(instance,
                                  constraint_col,
                                  surrogate_type = "gp",
                                  objective_col = NULL,
                                  objective_is_input = TRUE) {
  # Create learner based on surrogate type
  if (surrogate_type == "gp") {
    # Use mlr3mbo's default GP configuration
    learner <- mlr3mbo::default_gp()
    # Enable nugget estimation for numerical stability
    if ("nugget.estim" %in% names(learner$param_set$values)) {
      learner$param_set$values$nugget.estim <- TRUE
    }
  } else {
    # Use mlr3mbo's default RF configuration
    learner <- mlr3mbo::default_rf()
  }

  # Create constraint surrogate (predicts logit-transformed power)
  surr_constr <- mlr3mbo::srlrn(learner$clone(deep = TRUE), archive = instance$archive)
  surr_constr$cols_y <- constraint_col

  # Create objective surrogate only if objective is an output
  surr_obj <- NULL
  if (!objective_is_input && !is.null(objective_col)) {
    surr_obj <- mlr3mbo::srlrn(learner$clone(deep = TRUE), archive = instance$archive)
    surr_obj$cols_y <- objective_col
  }

  list(
    surrogate_objective = surr_obj,
    surrogate_constraint = surr_constr
  )
}
