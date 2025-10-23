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
#' @param equal_groups Logical. If TRUE (default), assumes equal group sizes (n1 = n2).
#'   If FALSE, requires p_group to be specified.
#' @param p_group Numeric. Proportion in group 1 (only used if equal_groups = FALSE).
#' @param alpha Numeric. Type I error rate (significance level). Default is 0.05.
#' @param alternative Character. Specifies the alternative hypothesis. Must be one of
#'   "two.sided" (default), "greater", or "less". For one-sided tests, "greater"
#'   tests if group 1 > group 0, "less" tests if group 1 < group 0.
#' @param method Character. Specifies the lambda calculation method. Must be one of:
#'   \itemize{
#'     \item "cohen" (default): Uses Cohen's convention where \eqn{\lambda = f^{2}(u + v + 1)}
#'     \item "theory": Uses statistical theory where \eqn{\lambda = n \cdot f^{2}}
#'   }
#' @param covariate_method Character. Specifies how to handle the covariate. Must be one of:
#'   \itemize{
#'     \item "fixed" (default): Assumes fixed covariate values (conditional power)
#'     \item "expected": Integrates over covariate distribution (expected power, SAS-style)
#'   }
#' @param numint Integer. Number of integration points for covariate_method = "expected" (default = 2000).
#'
#' @return Numeric vector. The statistical power(s) to detect the specified group effect(s).
#'   If both n and d are vectors, returns a vector with length equal to the maximum of their lengths.
#'
#' @details
#' The function calculates power for testing the group effect in a linear model:
#' \itemize{
#'   \item The outcome y is modeled as: \eqn{y = \beta_0 + \beta_1 \cdot \text{covariate} + \beta_2 \cdot \text{group} + \epsilon}
#'   \item The null hypothesis is \eqn{H_0: \beta_2 = 0}
#'   \item For two-sided tests: \eqn{H_1: \beta_2 \neq 0}
#'   \item For one-sided tests: \eqn{H_1: \beta_2 > 0} (if alternative = "greater") or \eqn{H_1: \beta_2 < 0} (if alternative = "less")
#'   \item The covariate follows \eqn{N(0,1)}
#'   \item Group is coded as 0 or 1
#'   \item Residuals \eqn{\epsilon} follow \eqn{N(0, \sigma^{2})}
#' }
#'
#' Method options:
#' \itemize{
#'   \item "cohen": Uses Cohen's convention matching pwr.f2.test
#'   \item "theory": Uses classical statistical theory
#' }
#'
#' Covariate method options:
#' \itemize{
#'   \item "fixed": Conditional power given fixed covariate values
#'   \item "expected": Expected power integrating over covariate distribution (SAS approach)
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
#' \donttest{
#' # Two-sided test with Cohen's method and fixed covariate
#' analytical_power_ancova_cont_2arms(n = 100, d = 0.5, beta_cov = 0.4, sigma = 1)
#'
#' # Using statistical theory method
#' analytical_power_ancova_cont_2arms(n = 100, d = 0.5, beta_cov = 0.4, sigma = 1,
#'                                    method = "theory")
#'
#' # Using expected power with Cohen's lambda
#' analytical_power_ancova_cont_2arms(n = 100, d = 0.5, beta_cov = 0.4, sigma = 1,
#'                                    covariate_method = "expected")
#'
#' # Using expected power with theory lambda
#' analytical_power_ancova_cont_2arms(n = 100, d = 0.5, beta_cov = 0.4, sigma = 1,
#'                                    method = "theory", covariate_method = "expected")
#'
#' # Vectorized: multiple sample sizes
#' analytical_power_ancova_cont_2arms(n = c(50, 100, 150), d = 0.5, beta_cov = 0.4, sigma = 1)
#'
#' # One-sided test (group 1 > group 0)
#' analytical_power_ancova_cont_2arms(n = 100, d = 0.5, beta_cov = 0.4, sigma = 1,
#'                                    alternative = "greater")
#'
#' # Unequal groups (70/30 split)
#' analytical_power_ancova_cont_2arms(n = 150, d = 0.5, beta_cov = 0.4, sigma = 1,
#'                                    equal_groups = FALSE, p_group = 0.7)
#'}
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
                                               equal_groups = TRUE,
                                               p_group = 0.5,
                                               alpha = 0.05,
                                               alternative = c("two.sided", "greater", "less"),
                                               method = c("cohen", "theory"),
                                               covariate_method = c("fixed", "expected"),
                                               numint = 2000) {
  # Match arguments
  alternative <- match.arg(alternative)
  method <- match.arg(method)
  covariate_method <- match.arg(covariate_method)
  
  # Input validation for single-valued parameters
  if (!is.numeric(beta_cov) || length(beta_cov) != 1) {
    stop("beta_cov must be a single numeric value")
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    stop("sigma must be a single positive numeric value")
  }
  if (!is.numeric(alpha) ||
      length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single numeric value between 0 and 1")
  }
  if (!is.logical(equal_groups) || length(equal_groups) != 1) {
    stop("equal_groups must be a single logical value")
  }
  if (!equal_groups && (
    is.null(p_group) || !is.numeric(p_group) ||
    length(p_group) != 1 ||
    p_group <= 0 || p_group >= 1
  )) {
    stop("When equal_groups = FALSE, p_group must be a single numeric value between 0 and 1")
  }
  if (!is.numeric(numint) || length(numint) != 1 || numint < 10) {
    stop("numint must be a single numeric value >= 10")
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
    warning(
      "Some d values are negative but alternative is 'greater'. Power will be very low for those values."
    )
  }
  if (alternative == "less" && any(d > 0)) {
    warning(
      "Some d values are positive but alternative is 'less'. Power will be very low for those values."
    )
  }
  
  # Sample sizes per group (vectorized)
  if (equal_groups) {
    n1 <- floor(n / 2)
    n2 <- n - n1
  } else {
    n1 <- floor(n * p_group)
    n2 <- n - n1
  }
  
  # Variance of group indicator (0/1 coding) (vectorized)
  var_group <- (n1 * n2) / n^2
  
  # Convert Cohen's d to regression coefficient (vectorized)
  beta_group <- d * sigma
  
  # Calculate f^2 (vectorized)
  f2 <- (beta_group^2 * var_group) / sigma^2
  
  # For expected method, need to handle one-at-a-time
  if (covariate_method == "expected") {
    power <- numeric(max_length)
    for (i in 1:max_length) {
      power[i] <- power_ancova_expected_single(
        n = n[i],
        d = d[i],
        beta_cov = beta_cov,
        sigma = sigma,
        n1 = n1[i],
        n2 = n2[i],
        var_group = var_group[i],
        f2 = f2[i],
        alpha = alpha,
        alternative = alternative,
        lambda_method = method,
        numint = numint
      )
    }
    return(power)
  }
  
  # Fixed covariate method (vectorized)
  # Standard error of group coefficient (vectorized)
  se_group <- sigma / sqrt(n * var_group)
  
  # Degrees of freedom (vectorized)
  df <- n - 3   # n - number of parameters (intercept, covariate, group)
  
  if (alternative == "two.sided") {
    # For two-sided test, use F-test approach
    
    # Non-centrality parameter based on method
    if (method == "cohen") {
      # Cohen's convention: lambda = f^2 * (u + v + 1)
      u <- 1  # numerator df
      lambda <- f2 * (u + df + 1)
    } else {
      # method == "theory"
      # Statistical theory: lambda = n * f^2
      lambda <- n * f2
    }
    
    # Critical F-value (vectorized)
    F_crit <- stats::qf(1 - alpha, df1 = 1, df2 = df)
    
    # Power (vectorized)
    power <- 1 - stats::pf(F_crit,
                    df1 = 1,
                    df2 = df,
                    ncp = lambda)
    
  } else {
    # For one-sided tests, use t-test approach
    # Test statistic under alternative: t = beta_group / se_group (vectorized)
    t_alt <- beta_group / se_group
    
    # Critical t-value (vectorized)
    if (alternative == "greater") {
      t_crit <- stats::qt(1 - alpha, df = df)
      # Power = P(T > t_crit | H1) (vectorized)
      power <- 1 - stats::pt(t_crit, df = df, ncp = t_alt)
    } else {
      # alternative == "less"
      t_crit <- stats::qt(alpha, df = df)
      # Power = P(T < t_crit | H1) (vectorized)
      power <- stats::pt(t_crit, df = df, ncp = t_alt)
    }
  }
  
  return(power)
}

#' @keywords internal
#' @importFrom stats qf qt pf dt pt
# Helper function for expected power calculation (single value)
power_ancova_expected_single <- function(n,
                                          d,
                                          beta_cov,
                                          sigma,
                                          n1,
                                          n2,
                                          var_group,
                                          f2,
                                          alpha,
                                          alternative,
                                          lambda_method,
                                          numint) {
  # Setup similar to SAS
  dd <- 1e-5
  g <- 2  # number of groups
  n_cov <- 1  # number of covariates
  
  # Adjusted mean difference
  mean_diff <- d * sigma
  
  # Degrees of freedom
  df1 <- 1       # numerator df (testing one group parameter)
  df2 <- n - 3   # denominator df
  dfx <- df2 + 1
  
  # Integration weights (Simpson's rule coefficients)
  coevec <- c(1, rep(c(4, 2), numint / 2 - 1), 4, 1)
  
  if (alternative == "two.sided") {
    # Critical F-value
    f_crit <- stats::qf(1 - alpha, df1 = df1, df2 = df2)
    
    # For P=1 covariate case (matching SAS logic)
    b <- n_cov / dfx
    
    # Integration over t-distribution
    tl <- stats::qt(dd, dfx)
    tu <- stats::qt(1 - dd, dfx)
    intl <- (tu - tl) / numint
    tvec <- tl + intl * (0:numint)
    
    # Weights for integration
    wtpdf <- (intl / 3) * coevec * stats::dt(tvec, dfx)
    
    # Non-centrality parameter at each integration point
    if (lambda_method == "theory") {
      # Statistical theory: lambda = n * f^2
      ncp_vec <- n * f2 / (1 + b * tvec^2)
    } else {
      # lambda_method == "cohen"
      # Cohen's convention: lambda = f^2 * (u + v + 1)
      ncp_vec <- f2 * (df1 + df2 + 1) / (1 + b * tvec^2)
    }
    
    # Expected power calculation
    power <- sum(wtpdf * stats::pf(f_crit, df1, df2, ncp = ncp_vec, lower.tail = FALSE))
    
  } else {
    # For one-sided tests
    # Standard error accounting for covariate
    se_group <- sigma / sqrt(n * var_group)
    
    # Integration over t-distribution for covariate effect
    b <- n_cov / dfx
    tl <- stats::qt(dd, dfx)
    tu <- stats::qt(1 - dd, dfx)
    intl <- (tu - tl) / numint
    tvec <- tl + intl * (0:numint)
    wtpdf <- (intl / 3) * coevec * stats::dt(tvec, dfx)
    
    # Effective variance inflation due to covariate
    var_inflation <- 1 + b * tvec^2
    
    # Non-centrality for t-test at each integration point
    t_ncp <- mean_diff / (se_group * sqrt(var_inflation))
    
    if (alternative == "greater") {
      t_crit <- stats::qt(1 - alpha, df = df2)
      power_vec <- 1 - stats::pt(t_crit, df = df2, ncp = t_ncp)
    } else {
      t_crit <- stats::qt(alpha, df = df2)
      power_vec <- stats::pt(t_crit, df = df2, ncp = t_ncp)
    }
    
    # Integrate
    power <- sum(wtpdf * power_vec)
  }
  
  return(power)
}

#' Compare Power Calculation Methods
#'
#' Compares power calculations across different method combinations.
#'
#' @param n Integer. Total sample size.
#' @param d Numeric. Cohen's d.
#' @param beta_cov Numeric. Covariate coefficient.
#' @param sigma Numeric. Residual SD.
#' @param alpha Numeric. Significance level.
#'
#' @return Data frame comparing power across methods.
#'
#' @examples
#' \donttest{
#' compare_power_methods(n = 100, d = 0.5, beta_cov = 0.4, sigma = 1)
#' }
#' @importFrom pwr pwr.f2.test
#' @export
compare_power_methods <- function(n, d, beta_cov, sigma, alpha = 0.05) {
  # All combinations
  methods <- expand.grid(
    method = c("cohen", "theory"),
    covariate_method = c("fixed", "expected"),
    stringsAsFactors = FALSE
  )
  
  methods$power <- NA
  
  for (i in 1:nrow(methods)) {
    methods$power[i] <- analytical_power_ancova_cont_2arms(
      n = n,
      d = d,
      beta_cov = beta_cov,
      sigma = sigma,
      alpha = alpha,
      method = methods$method[i],
      covariate_method = methods$covariate_method[i]
    )
  }
  
  # Also compare with pwr package
  var_group <- 0.25  # equal groups
  beta_group <- d * sigma
  f2 <- (beta_group^2 * var_group) / sigma^2
  power_pwr <- pwr::pwr.f2.test(
    u = 1,
    v = n - 3,
    f2 = f2,
    sig.level = alpha
  )$power
  
  # Add pwr result
  methods <- rbind(
    methods,
    data.frame(
      method = "pwr.f2.test",
      covariate_method = "fixed",
      power = power_pwr
    )
  )
  
  # Add description
  methods$description <- paste(methods$method, methods$covariate_method, sep = " + ")
  
  return(methods[, c("description", "power", "method", "covariate_method")])
}

#' Compute Cohen's f-squared for Group Effect in Linear Model
#'
#' Calculates Cohen's f-squared for the group effect in a linear regression model of the form
#' \code{y ~ covariate + group}. Cohen's f-squared represents the proportion of variance
#' uniquely explained by the group variable relative to the unexplained variance.
#' This function is vectorized and can handle multiple values for the d parameter.
#'
#' @param d Numeric or vector of numerics. Cohen's d for the group effect (standardized mean difference).
#'   This represents the difference in group means divided by the pooled standard
#'   deviation, after adjusting for the covariate.
#' @param beta_cov Numeric. True coefficient for the covariate (assumes covariate ~ N(0,1)).
#' @param sigma Numeric. Residual standard deviation after fitting the full model.
#' @param equal_groups Logical. If TRUE (default), assumes equal group sizes (n1 = n2).
#'   If FALSE, requires p_group to be specified.
#' @param p_group Numeric. Proportion in group 1 (only used if equal_groups = FALSE).
#'
#' @return Numeric vector. Cohen's f-squared for the group effect(s).
#'
#' @details
#' Cohen's f-squared is calculated as the ratio of variance uniquely explained by the group
#' to the unexplained variance:
#' \deqn{f^{2} = \frac{R^{2}_{\text{full}} - R^{2}_{\text{reduced}}}{1 - R^{2}_{\text{full}}}}
#'
#' where \eqn{R^{2}_{\text{full}}} includes both predictors and \eqn{R^{2}_{\text{reduced}}} includes only the covariate.
#'
#' @examples
#' \donttest{
#' # Single value
#' f2_from_params_ancova_cont_2arms(d = 0.5, beta_cov = 0.4, sigma = 1)
#'
#' # Multiple d values (vectorized)
#' f2_from_params_ancova_cont_2arms(d = c(0.2, 0.5, 0.8), beta_cov = 0.4, sigma = 1)
#'
#' # Unequal groups (70/30 split)
#' f2_from_params_ancova_cont_2arms(d = 0.5, beta_cov = 0.4, sigma = 1,
#'                                   equal_groups = FALSE, p_group = 0.7)
#' }
#' @export
f2_from_params_ancova_cont_2arms <- function(d,
                                             beta_cov,
                                             sigma,
                                             equal_groups = TRUE,
                                             p_group = 0.5) {
  # Input validation
  if (!is.numeric(d)) {
    stop("d must be numeric")
  }
  if (!is.numeric(beta_cov) || length(beta_cov) != 1) {
    stop("beta_cov must be a single numeric value")
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    stop("sigma must be a single positive numeric value")
  }
  if (!is.logical(equal_groups) || length(equal_groups) != 1) {
    stop("equal_groups must be a single logical value")
  }
  if (!equal_groups && (
    is.null(p_group) || !is.numeric(p_group) ||
    length(p_group) != 1 || p_group <= 0 || p_group >= 1
  )) {
    stop("When equal_groups = FALSE, p_group must be a single numeric value between 0 and 1")
  }
  
  # Convert Cohen's d to regression coefficient (vectorized)
  beta_group <- d * sigma
  
  # Variance of group indicator
  if (equal_groups) {
    var_group <- 0.25  # p(1-p) with p = 0.5
  } else {
    var_group <- p_group * (1 - p_group)
  }
  
  # Direct calculation of Cohen's f^2 (vectorized for d/beta_group)
  # f^2 = (beta_group^2 * var_group) / sigma^2
  # Since beta_group = d * sigma, this becomes:
  # f^2 = (d^2 * sigma^2 * var_group) / sigma^2 = d^2 * var_group
  f2 <- d^2 * var_group
  
  return(f2)
}
