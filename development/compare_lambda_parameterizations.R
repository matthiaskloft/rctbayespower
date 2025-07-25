#' Compare Different Lambda Implementations
#'
#' Shows that different lambda formulas give the same power results
#' when used with their corresponding distributions.

compare_lambda_implementations <- function(n = 100, d = 0.5, sigma = 1, alpha = 0.05) {
  
  # Setup
  var_group <- 0.25  # for equal groups
  beta_group <- d * sigma
  f2 <- (beta_group^2 * var_group) / sigma^2
  
  # Degrees of freedom
  u <- 1        # numerator df
  v <- n - 3    # denominator df  
  
  # Critical F value
  F_crit <- qf(1 - alpha, df1 = u, df2 = v)
  
  # Method 1: Statistical theory (λ = n * f²)
  lambda_theory <- n * f2
  power_theory <- pf(F_crit, df1 = u, df2 = v, ncp = lambda_theory, lower.tail = FALSE)
  
  # Method 2: Cohen's convention (λ = f² * (u + v + 1))
  lambda_cohen <- f2 * (u + v + 1)
  power_cohen <- pf(F_crit, df1 = u, df2 = v, ncp = lambda_cohen, lower.tail = FALSE)
  
  # Method 3: Direct from sum of squares
  SS_group <- beta_group^2 * var_group * n
  lambda_ss <- SS_group / sigma^2
  power_ss <- pf(F_crit, df1 = u, df2 = v, ncp = lambda_ss, lower.tail = FALSE)
  
  # Method 4: Using pwr package
  power_pwr <- pwr::pwr.f2.test(u = u, v = v, f2 = f2, sig.level = alpha)$power
  
  # Results
  results <- data.frame(
    Method = c("Statistical Theory", "Cohen Convention", "Sum of Squares", "pwr.f2.test"),
    Lambda = c(lambda_theory, lambda_cohen, lambda_ss, lambda_cohen),
    Power = c(power_theory, power_cohen, power_ss, power_pwr),
    Formula = c("n * f²", "f² * (u + v + 1)", "SS_group / σ²", "f² * (u + v + 1)")
  )
  
  cat("\nParameter values:\n")
  cat(sprintf("n = %d, d = %.3f, f² = %.4f\n", n, d, f2))
  cat(sprintf("Degrees of freedom: u = %d, v = %d\n\n", u, v))
  
  print(results, digits = 6)
  
  # Show they're actually the same
  cat("\nNote: Statistical theory and Sum of Squares methods give the same lambda:\n")
  cat(sprintf("n * f² = %.4f\n", lambda_theory))
  cat(sprintf("SS/σ² = %.4f\n", lambda_ss))
  cat(sprintf("These are equal: %s\n", isTRUE(all.equal(lambda_theory, lambda_ss))))
  
  return(results)
}

# Run comparison
#compare_lambda_implementations(n = 100, d = 0.5, sigma = 1)