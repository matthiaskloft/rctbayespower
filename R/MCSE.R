#' Monte Carlo Standard Error (MCSE) Calculation
#'
#' Calculate Monte Carlo Standard Error for power metrics based on simulation results.
#' MCSE provides an estimate of the uncertainty in power estimates due to Monte Carlo sampling.
#'
#' @param successes Vector of success indicators (TRUE/FALSE or 1/0)
#' @param n_simulations Total number of simulations
#' @return Monte Carlo Standard Error
#' @keywords internal
calculate_mcse_power <- function(successes, n_simulations) {
  if (length(successes) == 0 || n_simulations == 0) {
    return(NA_real_)
  }

  # Convert to numeric if needed
  if (is.logical(successes)) {
    successes <- as.numeric(successes)
  }

  # Calculate proportion
  p <- mean(successes, na.rm = TRUE)

  # MCSE for proportion = sqrt(p * (1 - p) / n)
  mcse <- sqrt(p * (1 - p) / n_simulations)

  return(mcse)
}

#' Monte Carlo Standard Error for Continuous Metrics
#'
#' Calculate Monte Carlo Standard Error for continuous metrics like mean probabilities.
#'
#' @param values Vector of continuous values
#' @param n_simulations Total number of simulations
#' @return Monte Carlo Standard Error
#' @keywords internal
calculate_mcse_mean <- function(values, n_simulations) {
  if (length(values) == 0 || n_simulations == 0) {
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
