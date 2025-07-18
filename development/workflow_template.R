

### Template for the workfölow of the power simulation ###

library(brms)

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
  chains = 2,
  cores = 2
)

 
### Function: rctbayespower_model
# builds the class rctbayespower_model (implemented in the package)
# Needs to contain:
# - the data simulation function
# - the compiled brms model

rctbayespower_model <- function(data_simulation_fn, brms_model, 
                                n_endpoints = NULL,
                                endpoint_types = NULL,
                                n_treatment_arms = NULL,
                                n_repeated_measures = NULL,
                                model_name = NULL) {
  
  # validate model
  if (!inherits(brms_model, "brmsfit")) {
    stop("The brms_model must be a valid brmsfit object.")
  }
  if (!is.function(data_simulation_fn)) {
    stop("The data_simulation_fn must be a valid function.")
  }
  
  # validate n_endpoints
  if (is.null(n_endpoints) || !is.numeric(n_endpoints) || n_endpoints <= 0) {
    stop("n_endpoints must be a positive numeric value.")
  }
  
  # validate endpoint_types, must have length n_endpoints and be valid types
  if (is.null(endpoint_types) || !is.character(endpoint_types) ||
      length(endpoint_types) != n_endpoints ||
      any(!endpoint_types %in% c("continuous", "binary", "count"))) {
    stop("endpoint_types must be a character vector of length n_endpoints with valid types.")
  }
  
  # validate n_treatment_arms
  if (is.null(n_treatment_arms) || !is.numeric(n_treatment_arms) || n_treatment_arms <= 0) {
    stop("n_treatment_arms must be a positive numeric value.")
  }
  
  # validate n_repeated_measures
  if (!is.null(n_repeated_measures) &&
      (!is.numeric(n_repeated_measures) || n_repeated_measures < 0)) {
    stop("n_repeated_measures must be a non-negative numeric value.")
  }
  
  # validate model_name
  if (!is.null(model_name) && !is.character(model_name)) {
    stop("model_name must be a character string or NULL.")
  }
  
  # check that data_simulation_fn has an argument called true_parameter_values
  if (!"true_parameter_values" %in% names(formals(data_simulation_fn))) {
    stop("data_simulation_fn must have an argument called 'true_parameter_values'.")
  }
  # check that true_parameter_values is a call to list()
  if (!is.call(formals(data_simulation_fn)$true_parameter_values) ||
      formals(data_simulation_fn)$true_parameter_values[[1]] != as.name("list")) {
    stop("data_simulation_fn's 'true_parameter_values' must be a call to list().")
  }
  
  # retrieve the names of the true_parameter_values by
  # parsing formals(data_simulation_fn)$true_parameter_values
    expr <- formals(simulate_data_ancova)$true_parameter_values
  # Ensure it's a call to list()
  if (is.call(expr) && expr[[1]] == as.name("list")) {
    parameter_names_sim_fn <- as.character(expr[-1])  # Drop the 'list' symbol, keep arguments
  } else {
    stop("data_simulation_fn's 'true_parameter_values' must be a call to list().")
  }
  
  # retriev parameter names from brms model
  parameter_names_brms <- stringr::str_subset(brms::variables(brms_model), pattern = "^b_")
  
  # strip brms model from posterior draws
  brms_model <- suppressMessages(update(brms_model, chains = 0, silent = 2))
  
  # create the output list with the data simulation function and the brms model
  output_list <- list(data_simulation_fn = data_simulation_fn, brms_model = brms_model)
  
  # assign class to the output list
  class(output_list) <- "rctbayespower_model"
  
  # add attributes to the output list
  attr(output_list, "model_name") <- model_name
  attr(output_list, "n_endpoints") <- n_endpoints
  attr(output_list, "endpoint_types") <- endpoint_types
  attr(output_list, "n_treatment_arms") <- n_treatment_arms
  attr(output_list, "n_repeated_measures") <- n_repeated_measures
  attr(output_list, "parameter_names_sim_fn") <- parameter_names_sim_fn
  attr(output_list, "parameter_names_brms") <- parameter_names_brms
  
  return(output_list)
}


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

# print S3 method for rctbayespower_model
# shows the simulated data function, the brms model, and the attributes
print.rctbayespower_model <- function(x, ...) {
  cat(paste0("'", attr(x, "class"),"'"), "object\n")
  cat("Model name:", attr(x, "model_name"), "\n")
  cat("Number of endpoints:", attr(x, "n_endpoints"), "\n")
  cat("Endpoint types:", paste(attr(x, "endpoint_types"), collapse = ", "), "\n")
  cat("Number of treatment arms:", attr(x, "n_treatment_arms"), "\n")
  cat("Number of repeated measures:", attr(x, "n_repeated_measures"), "\n")
  cat("Parameter names - simulation function:", 
      paste(attr(x, "parameter_names_sim_fn"), collapse = ", "), "\n")
  cat("Parameter names - brms model:", paste(attr(x, "parameter_names_brms"), collapse = ", "), "\n")
  cat("Data simulation function:\n")
  print(x$data_simulation_fn)
  cat("\nBrms model:\n")
  print(x$brms_model)
}
  
print(model_ancova_user)


# for implemented default models there is a proprietary function that creates
# the rctbayespower_model object (implemented in the package)
# example: ancova with continuous outcome, baseline covariate, and treatment effect
# we give users the option to specify priors for the model

rctbayespower_model_ancova_continuous <- function(prior_intercept = NULL,
                                          prior_sigma = NULL,
                                          prior_baseline = NULL,
                                          prior_treatment = NULL) {
  # create the data simulation function
  simulate_data_ancova <- function(n_total,
                                   allocation_probs,
                                   true_parameter_values = list(
                                     intercept, 
                                     b_grouptreat, 
                                     b_baseline,
                                     sigma)) {
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
  mock_data_ancova <- simulate_data_ancova(
    n_total = 100,
    allocation_probs = c(0.5, 0.5),
    list(
      intercept = 0,
      sigma = 1,
      b_grouptreat = 0.5,
      b_baseline = 0.2
    )
  )
  
  # priors
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
  
  # combine the priors into a single vector
  priors <- c(prior_baseline,
              prior_treatment,
              prior_intercept,
              prior_sigma)
  
  
  # fit the brms model
  cat("Compiling the brms model ...\n")
  
  # model for retrieving parameter names
  brms_model_ancova <- suppressMessages(
    brms::brm(
      formula = outcome ~ baseline + group,
      data = mock_data_ancova,
      family = gaussian(),
      prior = priors,
      chains = 1,
      iter = 500,
      refresh = 0,
      silent = 2
    )
  )
  cat("Model compilation done!\n")
  
  # build model
  rctbayespower_model <-
    rctbayespower_model(data_simulation_fn = simulate_data_ancova, 
                        brms_model = brms_model_ancova,
                        n_endpoints = 1,
                        endpoint_types = "continuous",
                        n_treatment_arms = 2,
                        n_repeated_measures = 0,
                        model_name = "ancova")
  
  return(rctbayespower_model)
}

model_ancova_continuous <- rctbayespower_model_ancova_continuous()

model_ancova_continuous

class(model_ancova_continuous)


#------------------------------------------------------------------------------>
# 2. Build rctbayespower_design
#------------------------------------------------------------------------------>

# Needs to contain:
# - the rctbayespower_model object
# - vector of target parameters
# - arguments that translate to further attributes
#   - n_endpoints
#   - n_treatment_arms
#   - n_repeated_measures
#   - n_interim_analyses
# - thresholds_success: vector matching target parameters
# - thresholds_futility: vector matching target parameters
# - p_sig_success: numeric probability
# - p_sig_futility: numeric probability
# - optional: allocation function for adaptive designs
# There are no defaults geiven by the function.
# All arguments need to be specified explicitly by the user !

rctbayespower_design <- function(rctbayespower_model = NULL,
                                 target_params = NULL,
                                 n_interim_analyses = NULL,
                                 thresholds_success = NULL,
                                 thresholds_futility = NULL,
                                 p_sig_success = NULL,
                                 p_sig_futility = NULL,
                                 allocation_function = NULL,
                                 design_name = NULL) {
  # validate the rctbayespower_model
  if (!inherits(rctbayespower_model, "rctbayespower_model")) {
    stop("The rctbayespower_model must be a valid rctbayespower_model object.")
  }
  
  # retrieve attributes from the rctbayespower_model
  n_endpoints <- attr(rctbayespower_model, "n_endpoints")
  endpoint_types <- attr(rctbayespower_model, "endpoint_types")
  n_treatment_arms <- attr(rctbayespower_model, "n_treatment_arms")
  n_repeated_measures <- attr(rctbayespower_model, "n_repeated_measures")
  parameter_names_sim_fn <- attr(rctbayespower_model, "parameter_names_sim_fn")
  parameter_names_brms <- attr(rctbayespower_model, "parameter_names_brms")
  
  
  # validate n_interim_analyses
  if (!is.null(n_interim_analyses) &&
      (!is.numeric(n_interim_analyses) || n_interim_analyses < 0)) {
    stop("n_interim_analyses must be a non-negative numeric value.")
  }
  
  # validate thresholds_success
  if (is.null(thresholds_success) ||
      !is.numeric(thresholds_success)) {
    stop("thresholds_success must be a non-null numeric vector.")
  }
  
  # validate thresholds_futility
  if (is.null(thresholds_futility) ||
      !is.numeric(thresholds_futility)) {
    stop("thresholds_futility must be a non-null numeric vector.")
  }
  
  # validate target_params
  if (is.null(target_params) || !is.character(target_params)) {
    stop("target_params must be a non-null character vector.")
  }
  
  # check that target_params is a subset of the parameter names in the model
  if (!all(target_params %in% parameter_names_brms)) {
    stop("target_params must be a subset of the parameter names in the rctbayespower_model.")
  }
  
  # check that thresholds_success and thresholds_futility have the same length as target_params
  if (length(thresholds_success) != length(target_params)) {
    stop("thresholds_success must have the same length as target_params.")
  }
  if (length(thresholds_futility) != length(target_params)) {
    stop("thresholds_futility must have the same length as target_params.")
  }
    
  # validate p_sig_success, must be a probability value
  if (is.null(p_sig_success) || !is.numeric(p_sig_success) ||
      p_sig_success < 0 || p_sig_success > 1) {
    stop("p_sig_success must be a numeric value between 0 and 1.")
  }
    
  # validate p_sig_futility, must be a probability value
  if (is.null(p_sig_futility) || !is.numeric(p_sig_futility) ||
      p_sig_futility < 0 || p_sig_futility > 1) {
    stop("p_sig_futility must be a numeric value between 0 and 1.")
  }
  
  # validate allocation_function
  if (!is.null(allocation_function) &&
      !is.function(allocation_function)) {
    stop("allocation_function must be a valid function or NULL.")
  }
  
  # validate design_name
  if (!is.null(design_name) && !is.character(design_name)) {
    stop("design_name must be a character string or NULL.")
  }
  
  
    # create the output list with the rctbayespower_model and the other attributes
    output_list <- list(
      data_simulation_fn = rctbayespower_model$data_simulation_fn,
      brms_model = rctbayespower_model$brms_model,
      allocation_function = allocation_function,
      n_endpoints = n_endpoints,
      endpoint_types = endpoint_types,
      n_treatment_arms = n_treatment_arms,
      n_repeated_measures = n_repeated_measures,
      parameter_names_sim_fn = parameter_names_sim_fn,
      parameter_names_brms = parameter_names_brms,
      target_params = target_params,
      n_interim_analyses = n_interim_analyses, thresholds_success = thresholds_success, thresholds_futility = thresholds_futility, p_sig_success = p_sig_success, p_sig_futility = p_sig_futility
    )

    # overwrite class
    class(output_list) <- "rctbayespower_design"
    
    # add attributes not inherited from the rctbayespower_model
    attr(output_list, "design_name") <- attr(rctbayespower_model, "design_name")
    attr(output_list, "target_params") <- attr(rctbayespower_model, "target_params")
    attr(output_list, "n_interim_analyses") <- attr(rctbayespower_model, "n_interim_analyses")
    attr(output_list, "thresholds_success") <- attr(rctbayespower_model, "thresholds_success")
    attr(output_list, "thresholds_futility") <- attr(rctbayespower_model, "thresholds_futility")
    attr(output_list, "p_sig_success") <- attr(rctbayespower_model, "p_sig_success")
    attr(output_list, "p_sig_futility") <- attr(rctbayespower_model, "p_sig_futility")
    attr(output_list, "allocation_function") <- attr(rctbayespower_model, "allocation_function")

    # attributes from the rctbayespower_model
    attr(output_list, "n_endpoints") <- n_endpoints
    attr(output_list, "endpoint_types") <- endpoint_types
    attr(output_list, "n_treatment_arms") <- n_treatment_arms
    attr(output_list, "n_repeated_measures") <- n_repeated_measures
    attr(output_list, "model_name") <- attr(rctbayespower_model, "model_name")
    attr(output_list, "parameter_names_sim_fn") <- attr(rctbayespower_model, "parameter_names_sim_fn")
    attr(output_list, "parameter_names_brms") <- parameter_names_brms
    
    
    
    # return the output list
  return(output_list)
  
}

ancova_design <- rctbayespower_design(
  rctbayespower_model = model_ancova_continuous,
  target_params = "b_grouptreat",
  n_interim_analyses = 0,
  thresholds_success = c(0.2),
  thresholds_futility = c(0),
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)

# print S3 method for rctbayespower_design
print.rctbayespower_design <- function(x, ...) {
  cat("\nObject of Class: 'rctbayespower_design'\n")
  # model
  cat("\n=== Model Specifications ===\n\n")
  cat("Number of endpoints:", x$n_endpoints, "\n")
  cat("Endpoint types:", paste(x$endpoint_types, collapse = ", "), "\n")
  cat("Number of treatment arms:", x$n_treatment_arms, "\n")
  cat("Number of repeated measures:", x$n_repeated_measures, "\n")
  cat("Parameter names - simulation function:", 
      paste(x$parameter_names_sim_fn, collapse = ", "), "\n")
  cat("Parameter names - brms model:", paste(x$parameter_names_brms, collapse = ", "), "\n")
  # design
  cat("\n\n=== Design Specifications ===\n\n")
  cat("Design name:", attr(x, "design_name"), "\n")
  cat("Target parameters:", paste(x$target_params, collapse = ", "), "\n")
  cat("Number of interim analyses:", x$n_interim_analyses, "\n")
  cat("Thresholds for success:", paste(x$thresholds_success, collapse = ", "), "\n")
  cat("Thresholds for futility:", paste(x$thresholds_futility, collapse = ", "), "\n")
  cat("Probability of success significance:", x$p_sig_success, "\n")
  cat("Probability of futility significance:", x$p_sig_futility, "\n")
  cat("\n\n=== Data Simulation Function ===\n\n")
  print(x$data_simulation_fn)
  cat("\n\n=== Brms Model ===\n\n")
  print(x$brms_model)
  cat("\n\n === Allocation Function ===\n\n")
  print(x$allocation_function)

}

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







