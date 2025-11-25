# Load package - try devtools first, then pkgload, then library
devtools::load_all()
#library(rctbayespower)

#-------------------------------------------------------------------------------
# 1. Uninformative priors for ANCOVA with continuous outcome

rho <- .3
d <- .5
sample_size <- seq(40, 280, 20)


#-------------------------------------------------------------------------------
# 1. Monte Carlo Estimate with uninformative priors for ANCOVA

# Create ANCOVA model with uninformative priors
model <- build_model_ancova_cont_2arms(
  b_arm_treat = d,
  b_covariate = rho,
  prior_treatment = brms::set_prior("normal(0, 1e3)", class = "b"),
  prior_covariate = brms::set_prior("normal(0, 1e3)", class = "b", coef = "covariate"),
  prior_intercept = brms::set_prior("constant(0)", class = "Intercept"),
  prior_sigma = brms::set_prior("normal(0, 2)", class = "sigma", lb = 0),
  link_sigma = "identity"
)

# Create design
design <- build_design(
  model = model,
  target_params = "b_arm2",
  n_interim_analyses = 0,
  thresholds_success = 0,
  thresholds_futility = 0,
  p_sig_success = 0.95,
  p_sig_futility = 0.5
)

# Create conditions
conditions <- build_conditions(
  design = design,
  condition_values = list(
    n_total = sample_size
  ),
  static_values = list())

# Run power analysis
n_cores <- parallel::detectCores() - 1
n_sims <- 1e3

power <- rctbayespower::power_analysis(
  conditions = conditions,
  n_sims = n_sims,
  n_cores = n_cores,
  brms_args = list(
    chains = 4,
    iter = 1000,
    warmup = 250,
    init = .5
  ),
  verbose = TRUE
)
power_mc <- power@summarized_results$power_success
power_mcse <- power@summarized_results$power_success_se
power_ci_95 <- paste0("[", 
                      round(power_mc - 1.96 * power_mcse, 3), ", ", 
                      round(power_mc + 1.96 * power_mcse, 3), "]")

#-------------------------------------------------------------------------------
# 5. Compare to analytical power

power_analytic <- analytical_power_ancova_cont_2arms(
  n = sample_size,
  d = d,
  beta_cov = rho,
  sigma = 1,
  alpha = .05,method = "theory", 
  covariate_method = "expected",
  alternative = "greater"
)

# Compare
tibble(
  "N total" = sample_size,
  "MC estimate" = power_mc,
  "95% CI" = power_ci_95 ,
  "Analytical estimate" =  power_analytic
) |>
  dplyr::mutate(
    `MC estimate` = round(`MC estimate`, 3),
    `Analytical estimate` = round(`Analytical estimate`, 3),
    Difference = round(`MC estimate` - `Analytical estimate`, 3)
  ) |>
  dplyr::arrange(`N total`)
