
### Template for the workfölow of the power simulation ###

library(brms)


### 1. Build rctbayespower_model

# Needs to contain:
# - the data simulation function
# - the compiled brms model

# data simulation function
# create a function that simulates data to fit the design brms object with
simulate_data_ancova <- function(n_total,
                                 allocation_probs,
                                 intercept,
                                 sigma,
                                 b_group_treat,
                                 b_baseline) {
  data.frame(
    baseline = rnorm(n_total),
    group = factor(
      sample(
        x = c(0, 1),
        size = n_total,
        prob = allocation_probs,
        replace = TRUE
      ),
      levels = c(0, 1),
      labels = c("ctrl", "treat")
    ),
    outcome = rnorm(
      n_total,
      mean = intercept + b_group_treat + b_baseline * rnorm(n_total),
      sd = sigma
    )
  )
}

# simulate some data
n_total <- 100
allocation_probs <- c(0.5, 0.5)  # equal allocation
b_group_treat <- 0.5  # treatment effect
b_baseline <- 0.2  # baseline effect
mock_data_ancova <- simulate_data_ancova(
  n_total = n_total,
  allocation_probs = allocation_probs,
  b_group_treat = b_group_treat,
  b_baseline = b_baseline
)



# fit the brms model
brms_model_ancova <- brms::brm(
  formula = outcome ~ 1 + baseline + group,
  data = mock_data_ancova,
  family = gaussian(),
  prior = c(
    brms::set_prior("student_t(3, 0, 1)", class = "b", coef = "baseline"),
    brms::set_prior("student_t(3, 0, 1)", class = "b", coef = "grouptreat"),
    brms::set_prior("normal(0, 10)", class = "Intercept"),
    brms::set_prior("normal(0, 10)", class = "sigma")
  ),
  sample_prior = "only",
  algorithm = "fixed_param",
  iter = 500,
  warmup = 250,
  chains = 4,
  cores = 4
)

summary(brms_model_ancova)


# fit again with chain = 0 to create an empty fit for later use
brms_model_ancova_compiled <- update(brms_model_ancova, chains = 0)


# function that builds the class rctbayespower_model (implemented in the package)
rctbayespower_model <- function(
  data_simulation_fn,
  brms_model
) {
  
  # validate model
  if (!inherits(brms_model, "brmsfit")) {
    stop("The brms_model must be a valid brmsfit object.")
  }
  if (!is.function(data_simulation_fn)) {
    stop("The data_simulation_fn must be a valid function.")
  }
  
  
  # create the output list with the data simulation function and the brms model
  output_list <- list(
    data_simulation_fn = data_simulation_fn,
    brms_model = brms_model
  )
  
  # assign class to the output list
  class(output_list) <- "rctbayespower_model"
  
  return(output_list)
}


# create the rctbayespower_model object
rctbayespower_model_ancova <- rctbayespower_model(
  data_simulation_fn = simulate_data_ancova,
  brms_model = brms_model_ancova_compiled
)

rctbayespower_model_ancova

# for implemented default models there is a proprietary function that creates 
# the rctbayespower_model object (implemented in the package)
# example: ancova with baseline covariate and treatment effect
# we give users the option to specify priors for the model

rctbayespower_model_ancova <- function(prior_intercept = NULL,
                                       prior_sigma = NULL,
                                       prior_baseline = NULL,
                                       prior_treatment = NULL) {
  
  # create the data simulation function
  simulate_data_ancova <- function(n_total,
                                   allocation_probs,
                                   intercept,
                                   sigma,
                                   b_group_treat,
                                   b_baseline) {
    data.frame(
      baseline = rnorm(n_total),
      group = factor(
        sample(
          x = c(0, 1),
          size = n_total,
          prob = allocation_probs,
          replace = TRUE
        ),
        levels = c(0, 1),
        labels = c("ctrl", "treat")
      ),
      outcome = rnorm(
        n_total,
        mean = intercept + b_group_treat + b_baseline * rnorm(n_total),
        sd = sigma
      )
    )
  }
  # simulate some data
  mock_data_ancova <- simulate_data_ancova(
    n_total = 100,
    allocation_probs = c(0.5, 0.5),
    intercept = 0,
    sigma = 1,
    b_group_treat = 0.5,
    b_baseline = 0.2
  )
  
  # use user-specified priors if !is.null(prior_intercept) else use default priors
  # check that the priors are specified with brms::set_prior()
  if (is.null(prior_intercept)) {
    prior_intercept <- brms::set_prior("normal(0, 10)", class = "Intercept")
  } else if (!inherits(prior_intercept, "brmsprior")) {
    stop("The prior_intercept must be a valid brmsprior object.")
  }
  if (is.null(prior_sigma)) {
    prior_sigma <- brms::set_prior("normal(0, 10)", class = "sigma")
  } else if (!inherits(prior_sigma, "brmsprior")) {
    stop("The prior_sigma must be a valid brmsprior object.")
  }
  if (is.null(prior_baseline)) {
    prior_baseline <- brms::set_prior("student_t(3, 0, 1)", class = "b", coef = "baseline")
  } else if (!inherits(prior_baseline, "brmsprior")) {
    stop("The prior_baseline must be a valid brmsprior object.")
  }
  if (is.null(prior_treatment)) {
    prior_treatment <- brms::set_prior("student_t(3, 0, 1)", class = "b", coef = "grouptreat")
  } else if (!inherits(prior_treatment, "brmsprior")) {
    stop("The prior_treatment must be a valid brmsprior object.")
  }
  
  priors <- c(prior_baseline,
              prior_treatment,
              prior_intercept,
              prior_sigma)
  
  # fit the brms model
  brms_model_ancova <- brms::brm(
    formula = outcome ~ baseline + group,
    data = mock_data_ancova,
    family = gaussian(),
    prior = priors,
    chains = 0,
  )
  
  rctbayespower_model <-
    rctbayespower_model(data_simulation_fn = simulate_data_ancova, 
                        brms_model = brms_model_ancova_compiled)
  
  return(rctbayespower_model)
}

rctbayespower_model_ancova <- rctbayespower_model_ancova()


#------------------------------------------------------------------------------>

### 2. Build rctbayespower_design 
# Needs to contain:
# - the rctbayespower_model object
# - vector of target parameters
# - arguments that translate to further attributes
#
# - optional: allocation function for adaptive designs
















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






