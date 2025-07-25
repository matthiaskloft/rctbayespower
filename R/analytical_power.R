#' Power Calculation for ANCOVA with Continuous Covariate and Two Groups
#'
#' Computes the analytical power to detect a group effect in an Analysis of Covariance
#' (ANCOVA) model with one continuous covariate and two groups. The model takes the form:
#' \code{y ~ covariate + group}, where the covariate is assumed to follow N(0,1).
#' This function is vectorized and can handle multiple values for n and/or d parameters.
#'
#' @param n Integer or vector of integers. Total sample size(s) across both groups.
#' @param d Numeric or vector of numerics. Cohen's d for the group effect (standardized mean difference).
#'   This represents the difference in group means divided by the pooled standard
#'   deviation, after adjusting for the covariate. For two-sided tests, the sign
#'   doesn't matter. For one-sided tests, positive d means group 1 > group 0.
#' @param beta_cov Numeric. True regression coefficient for the continuous covariate.
#'   Assumes the covariate follows a standard normal distribution N(0,1).
#' @param sigma Numeric. Residual standard deviation of the outcome after fitting
#'   the full model (including both covariate and group effects).
#' @param p_alloc Numeric vector of length 2. Allocation proportions for the two groups.
#'   Must sum to 1. Default is \code{c(0.5, 0.5)} for equal allocation.
#' @param alpha Numeric. Type I error rate (significance level). Default is 0.05.
#' @param alternative Character. Specifies the alternative hypothesis. Must be one of
#'   "two.sided" (default), "greater", or "less". For one-sided tests, "greater"
#'   tests if group 1 > group 0, "less" tests if group 1 < group 0.
#'
#' @return Numeric vector. The statistical power(s) to detect the specified group effect(s).
#'   If both n and d are vectors, returns a vector with length equal to the maximum of their lengths.
#'
#' @details
#' The function calculates power for testing the group effect in a linear model:
#' \itemize{
#'   \item The outcome y is modeled as: y = β₀ + β₁·covariate + β₂·group + ε
#'   \item The null hypothesis is H₀: β₂ = 0
#'   \item For two-sided tests: H₁: β₂ ≠ 0
#'   \item For one-sided tests: H₁: β₂ > 0 (if alternative = "greater") or β₂ < 0 (if alternative = "less")
#'   \item The covariate follows N(0,1)
#'   \item Group is coded as 0 or 1
#'   \item Residuals ε follow N(0, σ²)
#' }
#'
#' For two-sided tests, the function uses the F-test which is equivalent to the
#' square of a two-sided t-test. For one-sided tests, it uses the t-distribution
#' directly.
#'
#' @note
#' This function assumes:
#' \itemize{
#'   \item No interaction between group and covariate
#'   \item The covariate has the same variance in both groups
#'   \item The residual variance is homogeneous across groups
#' }
#'
#' @examples
#' # Two-sided test with equal allocation
#' analytical_power_ancova_cont_2arms(n = 100, d = 0.5, beta_cov = 0.4, sigma = 1)
#'
#' # Vectorized: multiple sample sizes with fixed effect size
#' analytical_power_ancova_cont_2arms(n = c(50, 100, 150), d = 0.5, beta_cov = 0.4, sigma = 1)
#'
#' # Vectorized: multiple effect sizes with fixed sample size
#' analytical_power_ancova_cont_2arms(n = 100, d = c(0.2, 0.5, 0.8), beta_cov = 0.4, sigma = 1)
#'
#' # Vectorized: both n and d (recycled to same length)
#' analytical_power_ancova_cont_2arms(n = c(80, 120), d = c(0.3, 0.7), beta_cov = 0.4, sigma = 1)
#'
#' # One-sided test (group 1 > group 0)
#' analytical_power_ancova_cont_2arms(n = 100, d = 0.5, beta_cov = 0.4, sigma = 1,
#'                                    alternative = "greater")
#'
#' # Unequal allocation (2:1 ratio)
#' analytical_power_ancova_cont_2arms(n = 150, d = 0.5, beta_cov = 0.4, sigma = 1,
#'                                    p_alloc = c(2/3, 1/3))
#'
#' @references
#' Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.).
#' Lawrence Erlbaum Associates.
#'
#' Shieh, G. (2020). Power analysis and sample size planning in ANCOVA designs.
#' Psychometrika, 85(1), 101-120.
#'
#' @export
analytical_power_ancova_cont_2arms <- function(n,
                                               d,
                                               beta_cov,
                                               sigma,
                                               p_alloc = c(0.5, 0.5),
                                               alpha = 0.05,
                                               alternative = c("two.sided", "greater", "less")) {
  # Match argument
  alternative <- match.arg(alternative)
  
  # Input validation for single-valued parameters
  if (!is.numeric(beta_cov) || length(beta_cov) != 1) {
    stop("beta_cov must be a single numeric value")
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    stop("sigma must be a single positive numeric value")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single numeric value between 0 and 1")
  }
  if (length(p_alloc) != 2 || !all(is.numeric(p_alloc)) ||
      any(p_alloc <= 0) || abs(sum(p_alloc) - 1) > 1e-10) {
    stop("p_alloc must be a numeric vector of length 2 with positive values that sum to 1")
  }
  
  # Input validation for vectorizable parameters
  if (!is.numeric(n) || any(n <= 0) || any(n != floor(n))) {
    stop("n must be positive integer(s)")
  }
  if (!is.numeric(d)) {
    stop("d must be numeric")
  }
  
  # Recycle n and d to the same length
  max_length <- max(length(n), length(d))
  n <- rep_len(n, max_length)
  d <- rep_len(d, max_length)
  
  # Check consistency of d and alternative for one-sided tests (vectorized)
  if (alternative == "greater" && any(d < 0)) {
    warning("Some d values are negative but alternative is 'greater'. Power will be very low for those values.")
  }
  if (alternative == "less" && any(d > 0)) {
    warning("Some d values are positive but alternative is 'less'. Power will be very low for those values.")
  }
  
  # Sample sizes per group (vectorized)
  n1 <- floor(n * p_alloc[1])
  n2 <- n - n1
  
  # Variance of group indicator (0/1 coding) (vectorized)
  var_group <- (n1 * n2) / n^2
  
  # Convert Cohen's d to regression coefficient (vectorized)
  beta_group <- d * sigma
  
  # Standard error of group coefficient (vectorized)
  se_group <- sigma / sqrt(n * var_group)
  
  # Degrees of freedom (vectorized)
  df <- n - 3   # n - number of parameters (intercept, covariate, group)
  
  if (alternative == "two.sided") {
    # For two-sided test, use F-test approach
    # Sum of squares for group effect (vectorized)
    SS_group <- n * var_group * beta_group^2
    
    # Non-centrality parameter (vectorized)
    lambda <- SS_group / sigma^2
    
    # Critical F-value (vectorized)
    F_crit <- qf(1 - alpha, df1 = 1, df2 = df)
    
    # Power (vectorized)
    power <- 1 - pf(F_crit,
                    df1 = 1,
                    df2 = df,
                    ncp = lambda)
    
  } else {
    # For one-sided tests, use t-test approach
    # Test statistic under alternative: t = beta_group / se_group (vectorized)
    t_alt <- beta_group / se_group
    
    # Critical t-value (vectorized)
    if (alternative == "greater") {
      t_crit <- qt(1 - alpha, df = df)
      # Power = P(T > t_crit | H1) (vectorized)
      power <- 1 - pt(t_crit, df = df, ncp = t_alt)
    } else {
      # alternative == "less"
      t_crit <- qt(alpha, df = df)
      # Power = P(T < t_crit | H1) (vectorized)
      power <- pt(t_crit, df = df, ncp = t_alt)
    }
  }
  
  return(power)
}



#' Compute Cohen's f² for Group Effect in Linear Model
#'
#' Calculates Cohen's f² for the group effect in a linear regression model of the form
#' \code{y ~ covariate + group}, assuming:
#' \itemize{
#'   \item \code{covariate} is standard normal: N(0, 1),
#'   \item \code{group} is binary: 0/1, with equal sample sizes,
#'   \item the true regression coefficients and residual SD are known.
#' }
#' The f² represents the proportion of variance uniquely explained by the \code{group}
#' variable, over and above the covariate. This function is vectorized for beta_group.
#'
#' @param beta_group Numeric or vector of numerics. True coefficient(s) for the group effect.
#' @param beta_cov Numeric. True coefficient for the covariate (assumes covariate ~ N(0,1)).
#' @param sigma Numeric. Residual standard deviation of the outcome.
#'
#' @return Numeric vector. Cohen's f² for the group effect(s).
#'
#' @examples
#' # Single value
#' f2_from_params_ancova_cont_2arms(beta_group = 0.5, beta_cov = 0.4, sigma = 1)
#'
#' # Multiple beta_group values
#' f2_from_params_ancova_cont_2arms(beta_group = c(0.2, 0.5, 0.8), beta_cov = 0.4, sigma = 1)
#'
#' @export
f2_from_params_ancova_cont_2arms <- function(beta_group, beta_cov, sigma) {
  # Input validation
  if (!is.numeric(beta_group)) {
    stop("beta_group must be numeric")
  }
  if (!is.numeric(beta_cov) || length(beta_cov) != 1) {
    stop("beta_cov must be a single numeric value")
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    stop("sigma must be a single positive numeric value")
  }
  
  # Total variance in y: Var(y) = Var(fitted) + Var(residuals)
  # Variance explained by covariate: Var(beta_cov * X) = beta_cov^2 (since X ~ N(0,1))
  R2_cov <- beta_cov^2 / (beta_cov^2 + sigma^2)
  
  # Cohen's d for group effect: standardized difference between groups (vectorized)
  # Since group coded 0/1, effect = beta_group; SD_y includes all variance
  d <- beta_group / sqrt(beta_cov^2 + sigma^2)
  
  # f² = d² / [4 * (1 - R²_cov)] (vectorized)
  f2 <- d^2 / (4 * (1 - R2_cov))
  
  return(f2)
}
