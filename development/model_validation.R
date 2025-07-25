# Load package - try devtools first, then pkgload, then library
devtools::load_all()
#library(rctbayespower)

#-------------------------------------------------------------------------------
# 1. Uninformative priors for ANCOVA with continuous outcome

rho <- .3
d <- .5
sample_size <- 100


#-------------------------------------------------------------------------------
# 1. Monte Carlo Estimate with uninformative priors for ANCOVA

# Create ANCOVA model with uninformative priors
model <- build_model_ancova_cont_2arms(
  b_arm_treat = d,
  b_covariate = rho,
  prior_treatment = brms::set_prior("normal(0, 1e3)", class = "b"),
  prior_covariate = brms::set_prior("normal(0, 1e3)", class = "b", coef = "covariate"),
  prior_intercept = brms::set_prior("normal(0, 1e3)", class = "Intercept"),
  prior_sigma = brms::set_prior("normal(0, 2)", class = "sigma", lb = 0)
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
n_sims <- 1e4

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

#-------------------------------------------------------------------------------
# 5. Compare to analytical power

power_analytic <- analytical_power_ancova_cont_2arms(
  n = sample_size,
  p_alloc = c(.5, .5),
  d = d,
  beta_cov = rho,
  sigma = 1,
  alpha = .05,
  alternative = "greater"
)


# Compare

round(c("MC estimate" = power_mc, "Analytical estimate" =  power_analytic),3)



