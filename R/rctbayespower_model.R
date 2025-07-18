#' Create an rctbayespower_model Object
#'
#' Constructs an rctbayespower_model object that encapsulates a data simulation
#' function, a compiled brms model, and associated metadata for power analysis.
#' This object serves as the foundation for Bayesian power analysis in RCTs.
#'
#' @param data_simulation_fn A function that simulates data for the RCT. Must take
#'   parameters n_total, allocation_probs, and true_parameter_values (as a list).
#'   The true_parameter_values parameter must be defined as a call to list() in
#'   the function's formals.
#' @param brms_model A fitted brmsfit object that serves as the template model.
#'   This should be compiled without posterior draws (chains = 0) for efficiency.
#' @param n_endpoints Number of endpoints in the study (must be positive integer)
#' @param endpoint_types Character vector specifying the type of each endpoint.
#'   Valid types are "continuous", "binary", "count". Length must match n_endpoints.
#' @param n_treatment_arms Number of treatment arms in the study (must be positive integer)
#' @param n_repeated_measures Number of repeated measures per participant.
#'   Use NULL or 0 for single time point studies.
#' @param model_name Optional character string providing a descriptive name for the model
#' @param pre_defined_model Optional character string specifying a predefined model
#'   to use instead of creating a custom model. Currently supported values:
#'   \itemize{
#'     \item "ancova_cont" - ANCOVA model for continuous outcomes with baseline covariate
#'   }
#'   When specified, other parameters are ignored and the predefined model is returned.
#'
#' @details
#' The rctbayespower_model class encapsulates all components needed for power
#' analysis simulation:
#' 
#' \strong{Predefined Models:} For convenience, users can specify pre_defined_model
#' to use ready-made model configurations. This is the recommended approach for
#' standard analyses. When using predefined models, other parameters are ignored.
#' 
#' \strong{Custom Models:} For advanced users, custom models can be created by
#' providing all required parameters:
#' 
#' \strong{Data Simulation Function:} Must accept n_total (total sample size),
#' allocation_probs (vector of allocation probabilities), and true_parameter_values
#' (named list of parameter values). The function should return a data.frame with
#' simulated baseline data ready for outcome generation.
#' 
#' \strong{BRMS Model:} A compiled brms model that will be used as a template.
#' The model should be fitted with minimal chains (e.g., chains = 0) to serve
#' as a compilation template only.
#' 
#' \strong{Validation:} The function validates that the data simulation function
#' has the required parameter structure and that the brms model is properly fitted.
#'
#' @return An object of class "rctbayespower_model" containing:
#' \describe{
#'   \item{data_simulation_fn}{The provided data simulation function}
#'   \item{brms_model}{The compiled brms model template}
#' }
#' The object has the following attributes:
#' \describe{
#'   \item{model_name}{Descriptive name for the model}
#'   \item{n_endpoints}{Number of study endpoints}
#'   \item{endpoint_types}{Types of endpoints}
#'   \item{n_treatment_arms}{Number of treatment arms}
#'   \item{n_repeated_measures}{Number of repeated measures}
#'   \item{parameter_names_sim_fn}{Parameter names extracted from simulation function}
#'   \item{parameter_names_brms}{Parameter names from brms model}
#' }
#'
#' @export
#' @importFrom stringr str_subset
#' @seealso [rctbayespower_design()], [model_ancova_continuous()]
#'
#' @examples
#' \donttest{
#' # Method 1: Use predefined model (recommended)
#' ancova_model <- rctbayespower_model(pre_defined_model = "ancova_cont")
#' 
#' # Method 2: Create custom model
#' # Define a simple data simulation function
#' simulate_ancova_data <- function(n_total, allocation_probs, 
#'                                  true_parameter_values = list(intercept, b_grouptreat, b_baseline, sigma)) {
#'   data.frame(
#'     baseline = rnorm(n_total),
#'     group = factor(
#'       sample(c(0, 1), size = n_total, prob = allocation_probs, replace = TRUE),
#'       levels = c(0, 1), labels = c("ctrl", "treat")
#'     )
#'   )
#' }
#' 
#' # Create mock data and fit template model
#' mock_data <- simulate_ancova_data(100, c(0.5, 0.5), 
#'                                   list(intercept = 0, b_grouptreat = 0.5, 
#'                                        b_baseline = 0.2, sigma = 1))
#' mock_data$outcome <- rnorm(100)  # Add outcome for model fitting
#' 
#' template_model <- brms::brm(
#'   outcome ~ baseline + group,
#'   data = mock_data,
#'   family = gaussian(),
#'   chains = 0,  # Compile only
#'   silent = 2
#' )
#' 
#' # Create rctbayespower_model object
#' my_model <- rctbayespower_model(
#'   data_simulation_fn = simulate_ancova_data,
#'   brms_model = template_model,
#'   n_endpoints = 1,
#'   endpoint_types = "continuous",
#'   n_treatment_arms = 2,
#'   n_repeated_measures = 0,
#'   model_name = "ANCOVA Model"
#' )
#' }
rctbayespower_model <- function(data_simulation_fn,
                                brms_model,
                                n_endpoints = NULL,
                                endpoint_types = NULL,
                                n_treatment_arms = NULL,
                                n_repeated_measures = NULL,
                                model_name = NULL,
                                pre_defined_model = NULL) {
  # validate pre_defined_model
  # needs to be one of the predefined models:
  # c("ancova_cont")
  if (!is.null(pre_defined_model) &&
    !pre_defined_model %in% c("ancova_cont")) {
    stop("pre_defined_model must be one of the predefined models: 'ancova_cont'.")
  }
  # if pre_defined_model is specified, use the predefined model
  if (!is.null(pre_defined_model)) {
    if (pre_defined_model == "ancova_cont") {
      return(model_ancova_continuous())
    }
  }
  
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
  expr <- formals(data_simulation_fn)$true_parameter_values
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





#' Print Method for rctbayespower_model Objects
#'
#' Displays a summary of an rctbayespower_model object, including model specifications,
#' parameter information, and function details.
#'
#' @param x An object of class "rctbayespower_model"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @export
#' @method print rctbayespower_model
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

#' Create Predefined ANCOVA Model for Continuous Outcomes
#'
#' Creates a ready-to-use rctbayespower_model object for ANCOVA (Analysis of
#' Covariance) with a continuous outcome, baseline covariate, and treatment effect.
#' This is a convenience function that sets up the complete model structure with
#' sensible defaults.
#'
#' @param prior_intercept Prior for the intercept parameter. If NULL (default),
#'   uses normal(0, 10). Must be a brmsprior object created with brms::set_prior().
#' @param prior_sigma Prior for the residual standard deviation. If NULL (default),
#'   uses normal(0, 10). Must be a brmsprior object created with brms::set_prior().
#' @param prior_baseline Prior for the baseline covariate effect. If NULL (default),
#'   uses student_t(3, 0, 1). Must be a brmsprior object created with brms::set_prior().
#' @param prior_treatment Prior for the treatment effect. If NULL (default),
#'   uses student_t(3, 0, 1). Must be a brmsprior object created with brms::set_prior().
#'
#' @details
#' This function creates a complete ANCOVA model with the following structure:
#' 
#' \strong{Model Formula:} outcome ~ baseline + group
#' 
#' \strong{Data Structure:} The generated data includes:
#' \itemize{
#'   \item baseline: Standardized normal baseline covariate
#'   \item group: Factor with levels "ctrl" and "treat"
#'   \item outcome: Continuous outcome generated from the linear model
#' }
#' 
#' \strong{Parameters:} The model includes parameters for intercept, baseline effect,
#' treatment effect (b_grouptreat), and residual standard deviation (sigma).
#' 
#' \strong{Model Compilation:} The function compiles the brms model during creation,
#' which may take some time but enables efficient power analysis later.
#'
#' @return An object of class "rctbayespower_model" ready for use with
#'   [rctbayespower_design()] and power analysis functions.
#'
#' @export
#' @importFrom brms set_prior brm
#' @importFrom stats gaussian
#' @seealso [rctbayespower_model()], [rctbayespower_design()]
#'
#' @examples
#' \donttest{
#' # Create ANCOVA model with default priors
#' ancova_model <- model_ancova_continuous()
#' 
#' # Create ANCOVA model with custom priors
#' custom_model <- model_ancova_continuous(
#'   prior_treatment = brms::set_prior("normal(0.5, 0.2)", class = "b", coef = "grouptreat"),
#'   prior_baseline = brms::set_prior("normal(0.3, 0.1)", class = "b", coef = "baseline")
#' )
#' }
model_ancova_continuous <- function(prior_intercept = NULL,
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

