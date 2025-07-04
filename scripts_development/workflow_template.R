
### Template for the workfölow of the power simulation ###

library(brms)

### Everything from here on is specified before hand by the user ---------------

# define the number of participants in the control and treatment groups
n_control <- 50  # number of participants in the control group
n_treatment <- 50  # number of participants in the treatment group

# create a function that simulates data to fit the design brms object with
simulate_data <- function(n_control, n_treatment) {
  data.frame(
    outcome = rnorm(n_control + n_treatment),
    baseline = rnorm(n_control + n_treatment),
    group = factor(
      rep(c(0, 1), times = c(n_control, n_treatment)),
      levels = c(0, 1),
      labels = c("ctrl", "treat")
    )
  )
}
# simulate the data for the design brms model
mock_data <- simulate_data(n_control, n_treatment)

# define the model formula for the design brms model
model_formula_true_params <- bf(outcome ~ baseline + group, center= FALSE)
# define the model formula for the estimation in the power simulation
model_formula_estimation <- bf(outcome ~ baseline + group)


# define the distributional family
family <- gaussian()



# check default priors for the design brms model
default_prior(model_formula_true_params, mock_data, family = family)
# set the priors for the design brms model to constants of the true parameters
priors_true_params <- c(
  set_prior("constant(.2)", class = "b", coef = "baseline"),
  set_prior("constant(.5)", class = "b", coef = "grouptreat"),
  set_prior("constant(0)", class = "b", coef = "Intercept"),
  set_prior("constant(1)", class = "sigma")
)


# check default priors for the estimation model
default_prior(model_formula_estimation, mock_data, family = family)
# also specify the priors for the estimation of the model in the power simulation
priors_estimation <- c(
  set_prior("student_t(3, 0, 1)", class = "b", coef = "baseline"),
  set_prior("student_t(3, 0, 2)", class = "Intercept"),
  set_prior("student_t(3, 0, 1)", class = "sigma")
)


# define the target parameter in the brms model for the power simulation
target_param <- "grouptreat"

algorithm <- "sampling"

threshold_significance <- 0.05  # significance level for the power simulation
threshold_success <- 0.2  # threshold for success in the power simulation
threshold_futility <- 0  # threshold for futility in the power simulation


### End of user specifications -------------------------------------------------

# The package should have one function per outcome type (continuous, binary, count) 
# that automates these specifications. For custom models, users need to to do this
# manually.



### Everything from here on is handled by the package functions ---------------

# The following code should be handled by package functions that take the user 
# specifications as input and return the results of the power simulation


# fit the design brms model with the constant priors, i.e., the true parameter values
# we also need a helper function to check this design model before it is used for a power simulation


brms_design_true_params<- brms::brm(
  formula = model_formula_true_params,
  data = mock_data,
  family = family,
  prior = priors_true_params,
  sample_prior = "only",
  algorithm = "fixed_param",
  iter = 2,
  warmup = 1,
  chains = 1
)
brms_design_true_params


# initialize estimation model: fit the design brms model with the estimation priors, i.e., the priors that 
# will be used in the power simulation

brms_design_estimation<- brms::brm(
  model_formula_estimation,
  data = mock_data,
  family = family,
  prior = priors_estimation,
  sample_prior = "no",
  algorithm = "sampling",
  init = .1,
  iter = 2,
  warmup = 1,
  chains = 1
)
brms_design_estimation

# extract the true parameter values from the fitted design model via the estimates

# fixed effects
fixef <- 
  brms::fixef(brms_design_true_params) |> 
  as.data.frame() |> 
  dplyr::select(Estimate) |> 
  tibble::rownames_to_column("parameter") |> 
  tidyr::pivot_wider(names_from = "parameter", values_from = "Estimate") |> 
  unlist()
# random effects (if any)
ranef <- tryCatch(
  brms::ranef(brms_design_true_params), error = function(e) {
    message("No random effects in the design model.")
    NULL
  })
# combine the fixed and random effects into a list of true parameters
true_params <- list(
  fixef = fixef,
  ranef = ranef)
true_params


seed <- 123

### The following needs to be done in the simulation loop, i.e., for each simulation run

# simulate new data for the current simulation run via simulate_data()
data_sim_run <- simulate_data(n_control, n_treatment)

# simulate the outcome from the design model with the true parameter values
data_sim_run$outcome <- brms::posterior_predict(
  object = brms_design_true_params,
  newdata = data_sim_run,
  ndraws = 1
)[1,]

# fit model for the current simulation run with the estimation priors
brms_fit_sim_run <- stats::update(
  object = brms_design_estimation,
  newdata = data_sim_run,
  algorithm = "sampling",
  iter = 1200,
  warmup = 200,
  chains = 2,
  cores = 1,
  init = .1,
  seed = seed
  
)

# extract the estimates from the fitted model for the current simulation run
estimates_sim_run <- brms::fixef(brms_fit_sim_run, robust = TRUE) |> 
  as.data.frame() |> 
  dplyr::select(Estimate) |> 
  tibble::rownames_to_column("parameter") |> 
  tidyr::pivot_wider(names_from = "parameter", values_from = "Estimate") |> 
  unlist()


# Extract treatment effect
effect_samples <- posterior::as_draws_df(brms_fit_sim_run)[[paste("b", target_param, sep = "_")]]

# compute probability of success
p_success_sim_run <- mean(effect_samples > threshold_success)
# compute probability of futility
p_futility_sim_run <- mean(effect_samples < threshold_futility)
# compute significance = posterior probability above the threshold of success
significant_sim_run <- p_success_sim_run > threshold_significance






