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
                                 interim_function = NULL,
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
  
  # validate interim_function
  if (!is.null(interim_function) &&
      !is.function(interim_function)) {
    stop("interim_function must be a valid function or NULL.")
  }
  
  # validate design_name
  if (!is.null(design_name) && !is.character(design_name)) {
    stop("design_name must be a character string or NULL.")
  }
  
  # if interim_function is not NULL, check that it has the required arguments
  if (!is.null(interim_function)) {
    # check that interim_function has an argument called interim_parameters
    if (!"interim_parameters" %in% names(formals(interim_function))) {
      stop("interim_function must have an argument called 'interim_parameters'.")
    }
    # check that interim_parameters is a call to list()
    if (!is.call(formals(interim_function)$interim_parameters) ||
        formals(interim_function)$interim_parameters[[1]] != as.name("list")) {
      stop("interim_function's 'interim_parameters' must be a call to list().")
    }
    
    # retrieve the names of the interim_parameters by
    # parsing formals(interim_function)$interim_parameters
    expr <- formals(simulate_data_ancova)$interim_parameters
    # Ensure it's a call to list()
    if (is.call(expr) && expr[[1]] == as.name("list")) {
      parameter_names_interim_fn <- as.character(expr[-1])  # Drop the 'list' symbol, keep arguments
    } else {
      stop("interim_function's 'interim_parameters' must be a call to list().")
    }
  } else {
    # if interim_function is NULL, set parameter_names_interim_fn to NULL
    parameter_names_interim_fn <- NULL
  }
  
  # create the output list with the rctbayespower_model and the other attributes
  output_list <- list(
    data_simulation_fn = rctbayespower_model$data_simulation_fn,
    brms_model = rctbayespower_model$brms_model,
    interim_function = interim_function,
    n_endpoints = n_endpoints,
    endpoint_types = endpoint_types,
    n_treatment_arms = n_treatment_arms,
    n_repeated_measures = n_repeated_measures,
    parameter_names_sim_fn = parameter_names_sim_fn,
    parameter_names_brms = parameter_names_brms,
    parameter_names_interim_fn = parameter_names_interim_fn,
    target_params = target_params,
    n_interim_analyses = n_interim_analyses, 
    thresholds_success = thresholds_success, 
    thresholds_futility = thresholds_futility, 
    p_sig_success = p_sig_success, 
    p_sig_futility = p_sig_futility
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
  attr(output_list, "interim_function") <- attr(rctbayespower_model, "interim_function")
  attr(output_list, "parameter_names_interim_fn") <- attr(rctbayespower_model, "parameter_names_interim_fn")
  
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
  cat("Parameter names - interim function:", paste(x$parameter_names_interim_fn, collapse = ", "), "\n")
  cat("\n\n=== Data Simulation Function ===\n\n")
  print(x$data_simulation_fn)
  cat("\n\n=== Brms Model ===\n\n")
  print(x$brms_model)
  cat("\n\n === Allocation Function ===\n\n")
  print(x$interim_function)
}