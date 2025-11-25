#' Monte Carlo Standard Error (MCSE) Calculation
#'
#' Calculate Monte Carlo Standard Error for power metrics based on simulation results.
#' MCSE provides an estimate of the uncertainty in power estimates due to Monte Carlo sampling.
#'
#' @param successes Vector of success indicators (TRUE/FALSE or 1/0)
#' @param n_sims Total number of simulations
#' @return Monte Carlo Standard Error
#' @importFrom stats sd
#' @keywords internal
calculate_mcse_power <- function(successes, n_sims) {
  if (length(successes) == 0 || n_sims == 0) {
    return(NA_real_)
  }

  # Convert to numeric if needed
  if (is.logical(successes)) {
    successes <- as.numeric(successes)
  }

  # Calculate proportion
  p <- mean(successes, na.rm = TRUE)

  # MCSE for proportion = sqrt(p * (1 - p) / n)
  mcse <- sqrt(p * (1 - p) / n_sims)

  return(mcse)
}

#' Monte Carlo Standard Error for Continuous Metrics
#'
#' Calculate Monte Carlo Standard Error for continuous metrics like mean probabilities.
#'
#' @param values Vector of continuous values
#' @param n_sims Total number of simulations
#' @return Monte Carlo Standard Error
#' @keywords internal
calculate_mcse_mean <- function(values, n_sims) {
  if (length(values) == 0 || n_sims == 0) {
    return(NA_real_)
  }

  # Remove NA values
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    return(NA_real_)
  }

  # MCSE for mean = standard deviation / sqrt(n)
  mcse <- sd(values) / sqrt(length(values))

  return(mcse)
}

#' Monte Carlo Standard Error for Integrated Power Metrics
#'
#' Calculate Monte Carlo Standard Error for integrated power metrics that combine
#' results across multiple effect sizes or sample sizes using weighted averages.
#'
#' @param values Vector of power or probability values
#' @param weights Vector of weights for integration
#' @param n_sims Total number of simulations
#' @param is_power_metric Logical indicating if this is a power metric (TRUE) or probability metric (FALSE)
#' @return Monte Carlo Standard Error for integrated metric
#' @keywords internal
calculate_mcse_integrated_power <- function(values, weights, n_sims, is_power_metric = TRUE) {
  if (length(values) == 0 || length(weights) == 0 || n_sims == 0) {
    return(NA_real_)
  }

  if (length(values) != length(weights)) {
    cli::cli_abort(c(
      "{.arg values} and {.arg weights} must have the same length",
      "x" = "values has length {.val {length(values)}}, weights has length {.val {length(weights)}}",
      "i" = "Ensure both vectors have matching lengths"
    ))
  }

  # Remove NA values
  valid_idx <- !is.na(values) & !is.na(weights)
  values <- values[valid_idx]
  weights <- weights[valid_idx]

  if (length(values) == 0) {
    return(NA_real_)
  }

  # Normalize weights to sum to 1
  weights <- weights / sum(weights)

  # Calculate weighted average
  weighted_avg <- sum(values * weights)

  if (is_power_metric) {
    # For power metrics (proportions), use binomial-based MCSE
    # MCSE for weighted proportion approximation
    mcse <- sqrt(weighted_avg * (1 - weighted_avg) / n_sims)
  } else {
    # For probability metrics (continuous), use weighted variance approach
    # Approximate MCSE using weighted variance
    weighted_var <- sum(weights * (values - weighted_avg)^2)
    mcse <- sqrt(weighted_var / n_sims)
  }

  return(mcse)
}

