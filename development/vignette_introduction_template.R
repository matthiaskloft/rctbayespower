# tests
library(rctbayespower)


### How to Build a Custom Model ------------------------------------------------

n_control <- 50  # number of participants in the control arm
n_treatment <- 50  # number of participants in the treatment group


### 1) Data Simulation Function

# create a function that simulates data to fit the design brms object with
simulate_data <- function(n_control, n_treatment) {
  data.frame(
    outcome = rnorm(n_control + n_treatment),
    baseline = rnorm(n_control + n_treatment),
   arm= factor(
      rep(c(0, 1), times = c(n_control, n_treatment)),
      levels = c(0, 1),
      labels = c("ctrl", "treat")
    )
  )
}

# simulate the data for the design brms model
mock_data <- simulate_data(n_control, n_treatment)

### 2) Model Formulas

# define the model formula for the design brms model
model_formula_true_params <- bf(outcome ~ baseline + arm, center = FALSE)
# define the model formula for the estimation in the power simulation
model_formula_estimation <- bf(outcome ~ baseline + arm)


### 3) True Parameters via Priors

# define the distributional family
family <- gaussian()

# check default priors for the design brms model
default_prior(model_formula_true_params, mock_data, family = family)
# set the priors for the design brms model to constants of the true parameters
priors_true_params <- c(
  set_prior("constant(.2)", class = "b", coef = "baseline"),
  set_prior("constant(.5)", class = "b", coef = "grouparm"),
  set_prior("constant(0)", class = "b", coef = "Intercept"),
  set_prior("constant(1)", class = "sigma")
)


### 4) Priors for Estimation

# check default priors for the estimation model
default_prior(model_formula_estimation, mock_data, family = family)
# also specify the priors for the estimation of the model in the power simulation
priors_estimation <- c(
  set_prior("student_t(3, 0, 1)", class = "b", coef = "baseline"),
  set_prior("student_t(3, 0, 2)", class = "Intercept"),
  set_prior("student_t(3, 0, 1)", class = "sigma")
)

### 5) Validate Power Design ---------------------------------------------------

# define the target parameter in the brms model for the power simulation
target_param <- "grouparm"

algorithm <- "sampling"

threshold_significance <- 0.05  # significance level for the power simulation
threshold_success <- 0.2  # threshold for success in the power simulation
threshold_futility <- 0  # threshold for futility in the power simulation


val <- validate_power_design(
  n_control = n_control,
  n_treatment = n_treatment,
  model_formula_true_params = model_formula_true_params,
  model_formula_estimation = model_formula_estimation,
  family = family,
  priors_true_params = priors_true_params,
  priors_estimation = priors_estimation,
  target_param = "grouparm",
  simulate_data_fn = simulate_data,
  brms_args = list(
    algorithm = "meanfield",
    importance_resampling = TRUE,
    iter = 1e5,
    output_samples = 5e3
  )
)


### 6) Power Analysis with Validated Model -------------------------------------

n_cores <- parallel::detectCores() - 1  # use all but one core
n_simulations <- n_cores * 50


power_mcmc <-
  power_analysis(
    n_control = n_control,
    n_treatment = n_treatment,
    brms_design_true_params = val$brms_design_true_params,
    brms_design_estimation = val$brms_design_estimation,
    target_param = target_param,
    simulate_data_fn = simulate_data,
    threshold_success = threshold_success,
    threshold_futility = threshold_futility,
    p_sig_success = 0.95,
    p_sig_futility = 0.5,
    n_simulations = n_simulations,
    n_cores = n_cores,
    brms_args = list(
      algorithm = "sampling",
      iter = 1500,
      warmup = 500,
      chains = 2,
      cores = 1,
      init = .1,
      control = list(adapt_delta = 0.9)
    ),
  )
print(power_custom_model)
summary(power_custom_model)


# Let's also run the same power analysis with different algorithms to compare results
power_meanfield <-
  power_analysis(
    n_control = n_control,
    n_treatment = n_treatment,
    brms_design_true_params = val$brms_design_true_params,
    brms_design_estimation = val$brms_design_estimation,
    target_param = target_param,
    simulate_data_fn = simulate_data,
    threshold_success = threshold_success,
    threshold_futility = threshold_futility,
    p_sig_success = 0.95,
    p_sig_futility = 0.5,
    n_simulations = n_simulations,
    n_cores = n_cores,
    brms_args = list(
      algorithm = "meanfield",
      importance_resampling = TRUE,
      iter = 1e5,
      output_samples = 5e3
    )
  )
summary(power_meanfield)

power_fullrank <-
  power_analysis(
    n_control = n_control,
    n_treatment = n_treatment,
    brms_design_true_params = val$brms_design_true_params,
    brms_design_estimation = val$brms_design_estimation,
    target_param = target_param,
    simulate_data_fn = simulate_data,
    threshold_success = threshold_success,
    threshold_futility = threshold_futility,
    p_sig_success = 0.95,
    p_sig_futility = 0.5,
    n_simulations = n_simulations,
    n_cores = n_cores,
    brms_args = list(
      algorithm = "fullrank",
      importance_resampling = TRUE,
      iter = 1e5,
      output_samples = 5e3
    )
  )

summary(power_fullrank)

# Compare powers

power_comparison <- rbind(
  convergence = c(
    power_mcmc$convergence,
    power_meanfield$convergence,
    power_fullrank$convergence
  ),
  mean_effect_estimate = c(
    power_mcmc$mean_effect_estimate,
    power_meanfield$mean_effect_estimate,
    power_fullrank$mean_effect_estimate
  ),
  power_success = c(
    power_mcmc$power_success,
    power_meanfield$power_success,
    power_fullrank$power_success
  ),
  power_futility = c(
    power_mcmc$power_futility,
    power_meanfield$power_futility,
    power_fullrank$power_futility
  ),
  p_success = c(
    power_mcmc$mean_prob_success,
    power_meanfield$mean_prob_success,
    power_fullrank$mean_prob_success
  ),
  p_futility = c(
    power_mcmc$mean_prob_futility,
    power_meanfield$mean_prob_futility,
    power_fullrank$mean_prob_futility
  )
) |>  as.data.frame() |> round(3)
names(power_comparison) <- c("MCMC", "Meanfield", "Fullrank")

power_comparison


# Use the ancova wtapper for a pre-specified design with baseline covariate

power_ancova <-
  power_analysis_ancova(
    n_control = 500,
    n_treatment = 500,
    effect_size = 0,
    baseline_effect = 0.2,
    threshold_success = 0.3,
    threshold_futility = .05,
    p_sig_futility = 0.5,
    outcome_type = "continuous",
    n_simulations = n_cores * 20,
    n_cores = n_cores
  )

summary(power_ancova)


### Sample Size Analysis -------------------------------------------------------

n_cores <- parallel::detectCores() - 1  # use all but one core
n_simulations <- n_cores * 30

sample_size_ancova <-
  sample_size_analysis(
    sample_sizes = c(200, 240, 280),
    percent_group_treat = 0.5,
    target_power_success = .8,
    target_power_futility = .8,
    power_analysis_fn = "power_analysis_ancova",
    outcome_type = "continuous",
    effect_size = .6,
    baseline_effect = 0.5,
    threshold_success = 0.5,
    threshold_futility = .3,
    p_sig_success = 0.95,
    p_sig_futility = 0.95,
    n_simulations = n_simulations,
    n_cores = n_cores
    
  )
summary(sample_size_ancova)
plot_power_curve(sample_size_ancova)

### Power Curve Analyses -------------------------------------------------------

n_cores <- parallel::detectCores() - 1  # use all but one core
n_simulations <- n_cores * 30


# vary sample sizes
sample_size_ancova <-
  power_grid_analysis(
    sample_sizes = c(200, 260, 320),
    effect_sizes = .7,
    percent_group_treat = 0.5,
    target_power_success = .8,
    target_power_futility = .8,
    power_analysis_fn = "power_analysis_ancova",
    outcome_type = "continuous",
    baseline_effect = 0.1,
    threshold_success = 0.6,
    threshold_futility = .4,
    p_sig_success = 0.95,
    p_sig_futility = 0.95,
    n_simulations = n_simulations,
    n_cores = n_cores
  )
print(sample_size_ancova)
summary(sample_size_ancova)
plot(sample_size_ancova)


# vary effect sizes
effect_size_ancova <-
  power_grid_analysis(
    sample_sizes = c(300),
    effect_sizes = seq(0.5, .9, 0.1),
    design_prior = "normal(0.7,0.05)",
    percent_group_treat = 0.5,
    target_power_success = .8,
    target_power_futility = .8,
    power_analysis_fn = "power_analysis_ancova",
    outcome_type = "continuous",
    baseline_effect = 0.1,
    threshold_success = 0.6,
    threshold_futility = .4,
    p_sig_success = 0.95,
    p_sig_futility = 0.95,
    n_simulations = n_simulations,
    n_cores = n_cores
  )
print(effect_size_ancova)
summary(effect_size_ancova)
plot(effect_size_ancova, type="power_curve",show_integrated = T)


# vary sample sizes and effect sizes
power_grid_ancova <- power_grid_analysis(
  sample_sizes = c(200, 280),
  effect_sizes = seq(0.5, .9, 0.1),
  design_prior = "normal(0.7,0.05)",
  percent_group_treat = 0.5,
  target_power_success = .8,
  target_power_futility = .8,
  power_analysis_fn = "power_analysis_ancova",
  outcome_type = "continuous",
  baseline_effect = 0.1,
  threshold_success = 0.6,
  threshold_futility = .4,
  p_sig_success = 0.95,
  p_sig_futility = 0.95,
  n_simulations = n_simulations,
  n_cores = n_cores
)
print(power_grid_ancova)
summary(power_grid_ancova)
plot(power_grid_ancova, type = "heatmap")





