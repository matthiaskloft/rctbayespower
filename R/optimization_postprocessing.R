# =============================================================================
# OPTIMIZATION POSTPROCESSING: Surrogate and Probabilistic Optimum
# =============================================================================
# Post-hoc analysis functions for Pareto optimization results:
# - find_surrogate_optimum(): Query surrogate model for predicted optimum
# - find_probabilistic_optimum(): Find minimum n with P(power >= target) >= alpha

# =============================================================================
# SURROGATE OPTIMUM
# =============================================================================

#' Find Surrogate-Based Optimum from Pareto Result
#'
#' Searches over the parameter space using surrogate model predictions to find
#' the optimal point. Constrains search to observed quantile range to reduce
#' extrapolation risk.
#'
#' @param result An `rctbp_pareto_result` object from [pareto_optimize()]
#' @param n_candidates Number of LHS candidates to evaluate (default 10000)
#' @param ci_level Confidence level for interval (default 0.95)
#' @param trim Proportion to trim from each tail of observed distributions
#'   (default 0.25 = IQR). Set to 0 for full domain search.
#'
#' @return List with params, predicted_mean, predicted_se, ci_lower, ci_upper,
#'   domain_used
#'
#' @seealso [pareto_optimize()], [find_probabilistic_optimum()]
#'
#' @export
find_surrogate_optimum <- function(result,
                                   n_candidates = 10000,
                                   ci_level = 0.95,
                                   trim = 0.25) {
  if (!inherits(result, "rctbp_pareto_result") &&
      !inherits(result, "rctbayespower::rctbp_pareto_result")) {
    cli::cli_abort(c(
      "{.arg result} must be an rctbp_pareto_result object",
      "x" = "Got object of class {.cls {class(result)}}",
      "i" = "Use {.fn pareto_optimize} to create a result object"
    ))
  }

  if (is.null(result@mbo_objects) || length(result@mbo_objects) == 0) {
    cli::cli_abort(c(
      "No MBO objects available in result",
      "x" = "result@mbo_objects is NULL or empty",
      "i" = "This may happen if optimization failed before surrogate was fitted"
    ))
  }

  if (!is.numeric(trim) || trim < 0 || trim >= 0.5) {
    cli::cli_abort(c(
      "{.arg trim} must be a number in [0, 0.5)",
      "x" = "Got {.val {trim}}"
    ))
  }

  # Get surrogate and instance from stored MBO objects
  surr <- result@mbo_objects$surrogate
  instance <- result@mbo_objects$instance

  if (is.null(surr) || is.null(instance)) {
    cli::cli_abort(c(
      "Surrogate or instance not available in MBO objects",
      "i" = "Required: result@mbo_objects$surrogate and result@mbo_objects$instance"
    ))
  }

  # Update surrogate with latest data
  suppressWarnings(surr$update())

  archive_df <- result@archive
  objectives <- result@objectives
  obj_names <- names(objectives)

  # Generate candidate points within trimmed domain
  search_space <- instance$search_space
  param_names <- search_space$ids()

  # Compute trimmed bounds
  if (trim > 0) {
    trimmed_bounds <- lapply(param_names, function(pn) {
      if (pn %in% names(archive_df)) {
        vals <- archive_df[[pn]]
        stats::quantile(vals, c(trim, 1 - trim), na.rm = TRUE)
      } else {
        c(search_space$lower[[pn]], search_space$upper[[pn]])
      }
    })
    names(trimmed_bounds) <- param_names
  } else {
    trimmed_bounds <- lapply(param_names, function(pn) {
      c(search_space$lower[[pn]], search_space$upper[[pn]])
    })
    names(trimmed_bounds) <- param_names
  }

  # Generate candidates via LHS within trimmed bounds
  candidate_list <- lapply(param_names, function(pn) {
    bounds <- trimmed_bounds[[pn]]
    stats::runif(n_candidates, bounds[1], bounds[2])
  })
  names(candidate_list) <- param_names
  candidates <- data.table::as.data.table(candidate_list)

  # Get surrogate predictions
  pred <- surr$predict(candidates)

  # Find best predicted point for first objective
  if (length(obj_names) > 0) {
    is_max <- objectives[[obj_names[1]]] == "maximize"
    if (is_max) {
      best_idx <- which.max(pred$mean)
    } else {
      best_idx <- which.min(pred$mean)
    }
  } else {
    best_idx <- which.min(pred$mean)
  }

  # Extract results
  best_params <- as.list(candidates[best_idx, ])
  pred_mean <- pred$mean[best_idx]
  pred_se <- if ("se" %in% names(pred)) pred$se[best_idx] else NA_real_

  z <- stats::qnorm((1 + ci_level) / 2)

  list(
    params = best_params,
    predicted_mean = pred_mean,
    predicted_se = pred_se,
    ci_lower = pred_mean - z * pred_se,
    ci_upper = pred_mean + z * pred_se,
    domain_used = trimmed_bounds
  )
}


# =============================================================================
# PROBABILISTIC OPTIMUM
# =============================================================================

#' Find Probabilistic Optimum
#'
#' Finds the minimum sample size where the GP-predicted probability of
#' achieving target power exceeds a required confidence level.
#'
#' @param result An `rctbp_pareto_result` object from [pareto_optimize()]
#' @param alpha Required confidence level for P(power >= target) (default 0.95).
#'   Higher values (e.g., 0.99, 0.999) are more conservative.
#' @param target Target power value. If NULL (default), uses 0.80.
#' @param n_candidates Number of candidate n values to evaluate (default 1000)
#' @param use_log_n Whether to use log(n) transform for GP input (default TRUE).
#'   Recommended for power functions which have diminishing returns.
#'
#' @return A list with:
#'   \describe{
#'     \item{n_opt}{Optimal sample size (ceiling-rounded to integer)}
#'     \item{p_feas}{Feasibility probability P(power >= target) at n_opt}
#'     \item{predicted_power}{GP-predicted power at n_opt}
#'     \item{ci_lower, ci_upper}{Confidence interval for predicted power (level = alpha)}
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
#' @seealso [find_surrogate_optimum()], [pareto_optimize()]
#'
#' @export
find_probabilistic_optimum <- function(result,
                                        alpha = 0.95,
                                        target = NULL,
                                        n_candidates = 1000,
                                        use_log_n = TRUE) {
  if (!inherits(result, "rctbp_pareto_result") &&
      !inherits(result, "rctbayespower::rctbp_pareto_result")) {
    cli::cli_abort(c(
      "{.arg result} must be an rctbp_pareto_result object",
      "x" = "Got object of class {.cls {class(result)}}",
      "i" = "Use {.fn pareto_optimize} to create a result object"
    ))
  }

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    cli::cli_abort(c(
      "{.arg alpha} must be a probability in (0, 1)",
      "x" = "Got {.val {alpha}}"
    ))
  }

  if (is.null(result@archive) || nrow(result@archive) == 0) {
    cli::cli_abort(c(
      "No archive data available in result",
      "i" = "Optimization may have failed before collecting data"
    ))
  }

  # Default target
  if (is.null(target)) {
    target <- 0.80
  }

  # Find power column in archive
  archive_df <- result@archive
  power_cols <- grep("^(pwr_|actual_pwr_)", names(archive_df), value = TRUE)
  if (length(power_cols) == 0) {
    cli::cli_abort(c(
      "No power column found in archive",
      "i" = "Archive must contain pwr_* or actual_pwr_* columns"
    ))
  }
  power_col <- power_cols[1]

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

  X <- matrix(x_values, ncol = 1)
  colnames(X) <- "x"

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
  n_lower <- max(1, n_bounds[1] * 0.9)
  n_upper <- n_bounds[2] * 1.1

  n_candidates_vec <- seq(n_lower, n_upper, length.out = n_candidates)

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
  sigma <- pred$sd

  # Compute P_feas for each candidate
  p_feas <- compute_p_feas(mu, sigma, target)

  # Find minimum n where P_feas >= alpha
  feasible_idx <- which(p_feas >= alpha)

  if (length(feasible_idx) == 0) {
    best_idx <- which.max(p_feas)
    cli::cli_warn(
      "No n found with P_feas >= {alpha}. Best P_feas = {round(max(p_feas), 3)} at n = {ceiling(n_candidates_vec[best_idx])}"
    )
  } else {
    best_idx <- feasible_idx[which.min(n_candidates_vec[feasible_idx])]
  }

  n_opt <- ceiling(n_candidates_vec[best_idx])
  p_feas_opt <- p_feas[best_idx]

  # Compute power prediction at optimal n
  predicted_logit_power <- mu[best_idx]
  predicted_power <- invlogit_transform(predicted_logit_power)

  # Compute CI for power (on probability scale)
  z <- stats::qnorm((1 + alpha) / 2)
  logit_ci_lower <- mu[best_idx] - z * sigma[best_idx]
  logit_ci_upper <- mu[best_idx] + z * sigma[best_idx]
  power_ci_lower <- invlogit_transform(logit_ci_lower)
  power_ci_upper <- invlogit_transform(logit_ci_upper)

  cli::cli_alert_success(
    "Probabilistic optimum: n = {n_opt}, P_feas = {round(p_feas_opt, 3)}, predicted power = {round(predicted_power, 3)} [{round(power_ci_lower, 3)}, {round(power_ci_upper, 3)}]"
  )

  list(
    n_opt = n_opt,
    p_feas = p_feas_opt,
    predicted_power = predicted_power,
    ci_lower = power_ci_lower,
    ci_upper = power_ci_upper,
    target = target,
    alpha = alpha,
    gp_model = gp_model,
    profile = data.frame(
      n = n_candidates_vec,
      p_feas = p_feas,
      mu = mu,
      sigma = sigma
    )
  )
}
