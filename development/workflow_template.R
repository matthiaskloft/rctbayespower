

### Template for the workfölow of the power simulation ###

library(brms)
devtools::load_all(.)

#------------------------------------------------------------------------------>
# 1. Build rctbayespower_model
#------------------------------------------------------------------------------>
# Building function needs to contain:
# - the data simulation function
# - the compiled brms model

# data simulation function
# create a function that simulates data to fit the design brms object with
simulate_data_ancova <- function(n_total,
                                 allocation_probs,
                                 true_parameter_values =
                                   list(intercept, sigma, b_grouptreat, b_baseline)
)
{
  data.frame(
    baseline = stats::rnorm(n_total),
    group = factor(
      sample(
        x = c(0, 1),
        size = n_total,
        prob = allocation_probs,
        replace = TRUE
      ), levels = c(0, 1), labels = c("ctrl", "treat")
    ), outcome = stats::rnorm(
      n_total,
      mean = true_parameter_values$intercept + 
        true_parameter_values$b_grouptreat + 
        true_parameter_values$b_baseline * stats::rnorm(n_total),
      sd = true_parameter_values$sigma
    )
  )
}

# simulate some data
n_total <- 100
allocation_probs <- c(0.5, 0.5)  # equal allocation
b_grouptreat <- 0.5  # treatment effect
b_baseline <- 0.2  # baseline effect

mock_data_ancova <- simulate_data_ancova(
  n_total = n_total,
  allocation_probs = allocation_probs,
  true_parameter_values = list(
    intercept = 0,
    sigma = 1,
    b_grouptreat = b_grouptreat,
    b_baseline = b_baseline
  )
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
  iter = 1000,
  warmup = 250,
  chains = 1,
  cores = 1
)

# create the rctbayespower_model object
model_ancova_user <-
  rctbayespower_model(
    data_simulation_fn = simulate_data_ancova,
    brms_model = brms_model_ancova,
    n_endpoints = 1,
    endpoint_types = "continuous",
    n_treatment_arms = 2,
    n_repeated_measures = 0,
    model_name = "ancova"
  )

model_ancova_user


  
print(model_ancova_user)




model_ancova_continuous <- rctbayespower_model_ancova_continuous()

model_ancova_continuous

class(model_ancova_continuous)


#------------------------------------------------------------------------------>
# 2. Build rctbayespower_design
#------------------------------------------------------------------------------>

ancova_design <- rctbayespower_design(
  rctbayespower_model = model_ancova_continuous,
  target_params = "b_grouptreat",
  n_interim_analyses = 0,
  thresholds_success = c(0.2),
  thresholds_futility = c(0),
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)

print(ancova_design)





#------------------------------------------------------------------------------>
# 2. Simulate
#------------------------------------------------------------------------------>

# Function to simulatte a single repitition of a single condition

simulate_single_run <- function(n_total = NULL,
                                allocation_probs = NULL,
                                rctbayespower_design = NULL,
                                true_parameter_values = NULL,
                                ...) {
  # validate the rctbayespower_design
  if (!inherits(rctbayespower_design, "rctbayespower_design")) {
    stop("The rctbayespower_design must be a valid rctbayespower_design object.")
  }
  
  # validate n_total
  if (!is.numeric(n_total) || n_total <= 0) {
    stop("n_total must be a positive numeric value.")
  }
  
  # validate allocation_probs: type, range
  if (!is.numeric(allocation_probs) ||
      length(allocation_probs) != 2 ||
      any(allocation_probs < 0) || sum(allocation_probs) != 1) {
    stop(
      "allocation_probs must be a numeric vector of length 2 with non-negative values that sum to 1."
    )
  }
  # check that allocation_probs match the number of treatment arms in the design
  if (length(allocation_probs) != rctbayespower_design$n_treatment_arms) {
    stop(
      "allocation_probs must match the number of treatment arms in the rctbayespower_design."
    )
  }
  
  # validate true_parameter_values, must be a named list of real numbers
  if (!is.null(true_parameter_values) &&
      (
        !is.list(true_parameter_values) ||
        any(
          !names(true_parameter_values) %in% rctbayespower_design$parameter_names_sim_fn
        ) ||
        any(!sapply(true_parameter_values, is.numeric))
      )) {
    stop(
      "true_parameter_values must be a named list of numeric values matching the parameter names in the rctbayespower_design."
    )
  }
  
  
  
  # extract the data simulation function and the brms model from the design
  data_simulation_fn <- rctbayespower_design$data_simulation_fn
  brms_model <- rctbayespower_design$brms_model
  
  # simulate data using the data simulation function
  simulated_data <- data_simulation_fn(n_total = n_total,
                                       allocation_probs = allocation_probs,
                                       true_parameter_values)
  
  # default brms arguments
  brms_args <- list(
    algorithm = "sampling",
    iter = 500,
    warmup = 250,
    chains = 4,
    cores = 1,
    init = .1,
    refresh = 0,
    silent = 2
  )
  
  # update the default brms arguments with any additional arguments passed to the function
  brms_args_final <- modifyList(brms_args, list(...))
  
  # fit the model to the simulated data
  fitted_model <- do.call(function(...) {
    stats::update(object = brms_model, newdata = simulated_data, ...)
  }, brms_args_final)
  
  return(fitted_model)
}


# condition arguments
n_total <- 100  # total number of participants
allocation_probs <- c(0.5, 0.5)  # equal allocation
# true parameter values for the simulation
true_parameter_values <- list(
  intercept = 0,
  sigma = 1,
  b_grouptreat = 0.5,
  b_baseline = 0.2
)

# simulate a single run, measure time
system.time(
  fitted_model_single_run <- simulate_single_run(
    n_total = n_total,
    allocation_probs = allocation_probs,
    rctbayespower_design = ancova_design,
    true_parameter_values = true_parameter_values
  )
)

fitted_model_single_run <- simulate_single_run(
  n_total = n_total,
  allocation_probs = allocation_probs,
  rctbayespower_design = ancova_design,
  true_parameter_values = true_parameter_values
)
fitted_model_single_run

attr(ancova_design, "parameter_names_sim_fn")







