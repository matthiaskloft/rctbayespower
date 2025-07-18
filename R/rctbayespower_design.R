#' Create an rctbayespower_design Object
#'
#' Constructs an rctbayespower_design object that combines an rctbayespower_model
#' with analysis configuration parameters for Bayesian power analysis. This object
#' encapsulates all information needed to conduct power simulations.
#'
#' @param rctbayespower_model An object of class "rctbayespower_model" created by
#'   \code{\link{rctbayespower_model}} or predefined model functions
#' @param target_params Character vector specifying which model parameters to
#'   analyze for power. Must be valid parameter names from the brms model
#'   (e.g., "b_grouptreat" for treatment effect)
#' @param n_interim_analyses Number of interim analyses planned during the study.
#'   Use 0 for studies with only final analysis. Must be non-negative integer.
#' @param thresholds_success Numeric vector of success thresholds for each target
#'   parameter. Length must match target_params. These represent the minimum
#'   clinically meaningful effect sizes.
#' @param thresholds_futility Numeric vector of futility thresholds for each target
#'   parameter. Length must match target_params. These represent effect sizes
#'   below which the treatment is considered ineffective.
#' @param p_sig_success Probability threshold for declaring success. The posterior
#'   probability that the effect exceeds the success threshold must be greater
#'   than this value to declare success (typically 0.975 or 0.95).
#' @param p_sig_futility Probability threshold for declaring futility. The posterior
#'   probability that the effect is below the futility threshold must be greater
#'   than this value to declare futility (typically 0.5).
#' @param interim_function Optional function for adaptive interim analyses. If provided,
#'   must accept an interim_parameters argument defined as a call to list().
#'   Currently not fully implemented.
#' @param design_name Optional character string providing a descriptive name for the design
#'
#' @details
#' The rctbayespower_design class combines model specifications with analysis
#' decision criteria:
#' 
#' \strong{Model Integration:} Inherits the data simulation function, compiled brms
#' model, and metadata from the rctbayespower_model object.
#' 
#' \strong{Decision Thresholds:} Success and futility thresholds define the
#' regions of practical equivalence (ROPE) for decision making. Effects above
#' the success threshold are considered clinically meaningful, while effects
#' below the futility threshold suggest treatment ineffectiveness.
#' 
#' \strong{Probability Thresholds:} The p_sig_success and p_sig_futility parameters
#' control the certainty required for decisions. Higher values require stronger
#' evidence.
#' 
#' \strong{Validation:} All parameters are validated for consistency with the
#' underlying model structure and each other.
#'
#' @return An object of class "rctbayespower_design" containing all elements from
#'   the rctbayespower_model plus the analysis configuration. Key components include:
#' \describe{
#'   \item{data_simulation_fn}{Data simulation function from the model}
#'   \item{brms_model}{Compiled brms model template}
#'   \item{target_params}{Target parameters for analysis}
#'   \item{thresholds_success}{Success thresholds}
#'   \item{thresholds_futility}{Futility thresholds}
#'   \item{p_sig_success}{Success probability threshold}
#'   \item{p_sig_futility}{Futility probability threshold}
#' }
#'
#' @export
#' @seealso [rctbayespower_model()], [simulate_single_run()]
#'
#' @examples
#' \donttest{
#' # Create an ANCOVA model
#' ancova_model <- model_ancova_continuous()
#' 
#' # Create a design for analyzing treatment effect
#' my_design <- rctbayespower_design(
#'   rctbayespower_model = ancova_model,
#'   target_params = "b_grouptreat",
#'   n_interim_analyses = 0,
#'   thresholds_success = 0.2,
#'   thresholds_futility = 0,
#'   p_sig_success = 0.975,
#'   p_sig_futility = 0.5,
#'   design_name = "ANCOVA Treatment Effect Analysis"
#' )
#' }
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


#' Print Method for rctbayespower_design Objects
#'
#' Displays a comprehensive summary of an rctbayespower_design object, showing
#' both model specifications and analysis configuration in an organized format.
#'
#' @param x An object of class "rctbayespower_design"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @export
#' @method print rctbayespower_design
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