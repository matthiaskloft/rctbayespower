# devtools::load_all(.)
library(rctbayespower)

#------------------------------------------------------------------------------>
# 1. Build rctbayespower_model
#------------------------------------------------------------------------------>
# Building function needs:
# - the data simulation function
# - the compiled brms model

# data simulation function
# create a function that simulates data to fit the design brms object with
simulate_data_ancova <- function(n_total,
                                 p_alloc,
                                 true_parameter_values =
                                   list(intercept, sigma, b_arms_treat, b_covariate))
{
  data.frame(
    baseline = stats::rnorm(n_total),
   arm= factor(
      sample(
        x = c(0, 1),
        size = n_total,
        prob = p_alloc,
        replace = TRUE
      ),
      levels = c(0, 1),
      labels = c("ctrl", "treat")
    ),
    outcome = stats::rnorm(
      n_total,
      mean = true_parameter_values$intercept +
        true_parameter_values$b_arms_treat +
        true_parameter_values$b_covariate * stats::rnorm(n_total),
      sd = true_parameter_values$sigma
    )
  )
}

# simulate some data to fit the model
mock_data_ancova <- simulate_data_ancova(
  n_total = 100,
  p_alloc = c(0.5, 0.5),
  true_parameter_values = list(
    intercept = 0,
    sigma = 1,
    b_arms_treat = 0.5,
    b_covariate = 0.2
  )
)

# fit the brms model
brms_model_ancova <- brms::brm(
  formula = outcome ~ 1 + baseline + arm,
  data = mock_data_ancova,
  family = gaussian(),
  prior = c(
    brms::set_prior("student_t(3, 0, 1)", class = "b", coef = "baseline"),
    brms::set_prior("student_t(3, 0, 1)", class = "b", coef = "armtreat"),
    brms::set_prior("normal(0, 10)", class = "Intercept"),
    brms::set_prior("normal(0, 10)", class = "sigma")
  ),
  iter = 1000,
  warmup = 250,
  chains = 1,
  cores = 1,
  silent = 0
)

#create the rctbayespower_model object

model_ancova_user <-
  build_model(
    predefined_model = NULL,
    data_simulation_fn = simulate_data_ancova,
    brms_model = brms_model_ancova,
    n_endpoints = 1,
    endpoint_types = "continuous",
    n_arms = 2,
    n_repeated_measures = 0,
    model_name = "ancova"
  )

print(model_ancova_user)
class(model_ancova_user)



# or use the predefined model
model_ancova_predefined <- build_model(predefined_model = "ancova_cont_2arms")

class(model_ancova_predefined)


#------------------------------------------------------------------------------>
# 2. Build rctbayespower_design
#------------------------------------------------------------------------------>



ancova_design <- build_design(
  model = model_ancova_predefined,
  target_params = "b_arm2",
  n_interim_analyses = 0,
  thresholds_success = c(0.2),
  thresholds_futility = c(0),
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)

print(ancova_design)





#------------------------------------------------------------------------------>
# 3. Build conditions and simulate
#------------------------------------------------------------------------------>

# Build conditions using build_conditions function
conditions <- build_conditions(
  design = ancova_design,
  condition_values = list(n_total = c(100)),  # single condition for testing
  static_values = list(
    n_arms = 2,
    contrasts = NULL,
    p_alloc = list(c(0.5, 0.5)),
    intercept = 0,
    sigma = 1,
    b_arm_treat = 0.5,
    b_covariate = 0.2
  )
)

print(conditions)


# simulate a single run using the package function, measure time

fitted_model_single_run <- simulate_single_run(
  condition_arguments = conditions@condition_arguments[[1]],
  id_sim = 1,
  design = ancova_design,
  brms_args = list(silent=0)
)

print(fitted_model_single_run)

