### Function: rctbayespower_model
# builds the class rctbayespower_model (implemented in the package)
# Needs to contain:
# - the data simulation function
# - the compiled brms model

rctbayespower_model <- function(data_simulation_fn,
                                brms_model,
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
  if (is.null(n_endpoints) ||
    !is.numeric(n_endpoints) || n_endpoints <= 0) {
    stop("n_endpoints must be a positive numeric value.")
  }

  # validate endpoint_types, must have length n_endpoints and be valid types
  if (is.null(endpoint_types) || !is.character(endpoint_types) ||
    length(endpoint_types) != n_endpoints ||
    any(!endpoint_types %in% c("continuous", "binary", "count"))) {
    stop("endpoint_types must be a character vector of length n_endpoints with valid types.")
  }

  # validate n_treatment_arms
  if (is.null(n_treatment_arms) ||
    !is.numeric(n_treatment_arms) || n_treatment_arms <= 0) {
    stop("n_treatment_arms must be a positive numeric value.")
  }

  # validate n_repeated_measures
  if (!is.null(n_repeated_measures) &&
    (!is.numeric(n_repeated_measures) ||
      n_repeated_measures < 0)) {
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
    parameter_names_sim_fn <- as.character(expr[-1]) # Drop the 'list' symbol, keep arguments
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





# print S3 method for rctbayespower_model
# shows the simulated data function, the brms model, and the attributes
print.rctbayespower_model <- function(x, ...) {
  cat(paste0("'", attr(x, "class"), "'"), "object\n")
  cat("Model name:", attr(x, "model_name"), "\n")
  cat("Number of endpoints:", attr(x, "n_endpoints"), "\n")
  cat("Endpoint types:", paste(attr(x, "endpoint_types"), collapse = ", "), "\n")
  cat("Number of treatment arms:", attr(x, "n_treatment_arms"), "\n")
  cat(
    "Number of repeated measures:",
    attr(x, "n_repeated_measures"),
    "\n"
  )
  cat(
    "Parameter names - simulation function:",
    paste(attr(x, "parameter_names_sim_fn"), collapse = ", "),
    "\n"
  )
  cat(
    "Parameter names - brms model:",
    paste(attr(x, "parameter_names_brms"), collapse = ", "),
    "\n"
  )
  cat("Data simulation function:\n")
  print(x$data_simulation_fn)
  cat("\nBrms model:\n")
  print(x$brms_model)
}



### Predefined Models --------------------------------------------------------->

# ANCOVA, baseline covariate, continuous outcome, 1 treatment effect,
# 0 repeated measures
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
                                   true_parameter_values = list(intercept, b_grouptreat, b_baseline, sigma)) {
    data.frame(
      baseline = stats::rnorm(n_total),
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
      outcome = stats::rnorm(
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
  priors <- c(
    prior_baseline,
    prior_treatment,
    prior_intercept,
    prior_sigma
  )


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
    rctbayespower_model(
      data_simulation_fn = simulate_data_ancova,
      brms_model = brms_model_ancova,
      n_endpoints = 1,
      endpoint_types = "continuous",
      n_treatment_arms = 2,
      n_repeated_measures = 0,
      model_name = "ancova"
    )

  return(rctbayespower_model)
}
