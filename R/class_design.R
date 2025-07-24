# S7 Class Definition for RCT Bayesian Power Design
#' @importFrom S7 new_class class_character class_numeric class_integer class_function class_any new_union
rctbp_design <- S7::new_class(
  "rctbp_design",
  properties = list(
    model = S7::class_any,  # Inherits from rctbp_model
    interim_function = S7::class_function | NULL,
    parameter_names_interim_fn =  S7::new_property(
      getter = function(self) {
        # Extract parameter names from the interim function if it exists
        if (!is.null(self@interim_function)) {
          names(formals(self@interim_function))
        } else {
          NULL
        }
      }),
    target_params = S7::class_character,
    n_interim_analyses = S7::class_numeric,
    thresholds_success = S7::class_numeric,
    thresholds_futility = S7::class_numeric,
    p_sig_success = S7::class_numeric,
    p_sig_futility = S7::class_numeric,
    design_name = S7::class_character | NULL
  ),
  validator = function(self) {
    # validate model
    if (!inherits(self@model, "rctbayespower::rctbp_model")) {
      return("'model' must be a valid rctbp_model object.")
    }
    # Design-specific validations only (parent validation is automatic via inheritance)

    # Check for required parameters
    if (is.null(self@target_params) || length(self@target_params) == 0) {
      return("'target_params' cannot be NULL or empty.")
    }
    if (is.null(self@thresholds_success) || length(self@thresholds_success) == 0) {
      return("'thresholds_success' cannot be NULL or empty.")
    }
    if (is.null(self@thresholds_futility) || length(self@thresholds_futility) == 0) {
      return("'thresholds_futility' cannot be NULL or empty.")
    }
    if (is.null(self@p_sig_success)) {
      return("'p_sig_success' cannot be NULL.")
    }
    if (is.null(self@p_sig_futility)) {
      return("'p_sig_futility' cannot be NULL.")
    }

    # Validate n_interim_analyses
    if (length(self@n_interim_analyses) != 1 || self@n_interim_analyses < 0) {
      return("'n_interim_analyses' must be a non-negative numeric value.")
    }

    # Check that target_params is a subset of the parameter names in the model
    if (!all(self@target_params %in% self@model@parameter_names_brms)) {
      return(paste(
        "'target_params' must be a subset of the parameter names in the model:",
        paste(self@model@parameter_names_brms, collapse = ", ")
      ))
    }

    # Check that thresholds have the same length as target_params
    if (length(self@thresholds_success) != length(self@target_params)) {
      return("'thresholds_success' must have the same length as 'target_params'.")
    }
    if (length(self@thresholds_futility) != length(self@target_params)) {
      return("'thresholds_futility' must have the same length as 'target_params'.")
    }

    # Validate probability values
    if (length(self@p_sig_success) != 1 ||
        self@p_sig_success < 0 || self@p_sig_success > 1) {
      return("'p_sig_success' must be a numeric value between 0 and 1.")
    }
    if (length(self@p_sig_futility) != 1 ||
        self@p_sig_futility < 0 || self@p_sig_futility > 1) {
      return("'p_sig_futility' must be a numeric value between 0 and 1.")
    }

    # Check parameter name uniqueness across simulation and interim functions
    if (!is.null(self@parameter_names_interim_fn)) {
      all_parameter_names <- c(self@model@parameter_names_sim_fn, self@parameter_names_interim_fn)
      if (length(unique(all_parameter_names)) != length(all_parameter_names)) {
        return("Parameter names must be unique across the simulation and interim functions.")
      }
    }

    # If all validations pass, return NULL
    NULL
  }
)

#' Constructor for rctbp_design Objects
#'
#' Creates an S7 rctbp_design object that combines a rctbp_model with analysis
#' configuration parameters for Bayesian power analysis. This constructor function
#' provides a user-friendly interface to the S7 class constructor.
#'
#' @param model An S7 object of class "rctbp_model" created by [build_model()]
#' @param target_params Character vector specifying which model parameters to
#'   analyze for power. Must be valid parameter names from the brms model
#'   (e.g., "b_arms_treat" for treatment effect). Required.
#' @param thresholds_success Numeric vector of success thresholds for each target
#'   parameter. Length must match target_params. These represent the minimum
#'   clinically meaningful effect sizes. Required.
#' @param thresholds_futility Numeric vector of futility thresholds for each target
#'   parameter. Length must match target_params. These represent effect sizes
#'   below which the treatment is considered ineffective. Required.
#' @param p_sig_success Probability threshold for declaring success. The posterior
#'   probability that the effect exceeds the success threshold must be greater
#'   than this value to declare success (typically 0.975 or 0.95). Required.
#' @param p_sig_futility Probability threshold for declaring futility. The posterior
#'   probability that the effect is below the futility threshold must be greater
#'   than this value to declare futility (typically 0.5). Required.
#' @param n_interim_analyses Number of interim analyses planned during the study.
#'   Defaults to 0 for studies with only final analysis. Must be non-negative integer.
#' @param interim_function Optional function for adaptive interim analyses. If provided,
#'   must accept an interim_parameters argument defined as a call to list().
#'   Currently not fully implemented. Defaults to NULL.
#' @param design_name Optional character string providing a descriptive name for the
#'   design. Defaults to NULL.
#'
#' @details
#' The rctbp_design class combines model specifications with analysis
#' decision criteria:
#'
#'
#' \strong{Model Integration:} Inherits the data simulation function, compiled brms
#' model, and metadata from the model object.
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
#' @return An S7 object of class "rctbp_design" containing all elements from
#'   the model plus the analysis configuration. Key components include:
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
#' @seealso [build_model()], [simulate_single_run()]
#'
#' @examples
#' \dontrun{
#' # Create an ANCOVA model
#' ancova_model <- build_model_ancova_cont()
#'
#' # Create a design for analyzing treatment effect
#' my_design <- build_design(
#'   model = ancova_model,
#'   target_params = "b_arms_treat",
#'   n_interim_analyses = 0,
#'   thresholds_success = 0.2,
#'   thresholds_futility = 0,
#'   p_sig_success = 0.975,
#'   p_sig_futility = 0.5,
#'   design_name = "ANCOVA Treatment Effect Analysis"
#' )
#' }
build_design <- function(model,
                         target_params,
                         thresholds_success,
                         thresholds_futility,
                         p_sig_success,
                         p_sig_futility,
                         n_interim_analyses = 0,
                         interim_function = NULL,
                         design_name = NULL) {



  # Use S7 constructor directly - all validation happens in the class validator
  rctbp_design(
    model = model,
    interim_function = interim_function,
    target_params = target_params,
    n_interim_analyses = n_interim_analyses,
    thresholds_success = thresholds_success,
    thresholds_futility = thresholds_futility,
    p_sig_success = p_sig_success,
    p_sig_futility = p_sig_futility,
    design_name = design_name
  )
}


# S7 Method for Print (uses existing base print generic)
#' @importFrom S7 method

#' Print Method for rctbp_design Objects
#'
#' Displays a comprehensive summary of a rctbp_design object, showing
#' both model specifications and analysis configuration in an organized format.
#'
#' @param x An S7 object of class "rctbp_design"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @export
S7::method(print, rctbp_design) <- function(x, ...) {
  cat("\nS7 Object of class: 'rctbp_design'\n")
  cat("--------------------------------------------------\n")
  # model
  cat("\n=== Model Specifications ===\n\n")
  cat("Number of endpoints:", x@model@n_endpoints, "\n")
  cat("Endpoint types:",
      paste(x@model@endpoint_types, collapse = ", "),
      "\n")
  cat("Number of arms:", x@model@n_arms, "\n")
  cat("Number of repeated measures:",
      if (is.null(x@model@n_repeated_measures))
        "NULL"
      else
        x@model@n_repeated_measures,
      "\n")
  cat(
    "Parameter names - simulation function:",
    paste(x@model@parameter_names_sim_fn, collapse = ", "),
    "\n"
  )
  cat("Parameter names - brms model:",
      paste(x@model@parameter_names_brms, collapse = ", "),
      "\n")

  # design
  cat("\n=== Design Specifications ===\n\n")
  cat("Design name:", if (is.null(x@design_name))
    "NULL"
    else
      x@design_name, "\n")
  cat("Target parameters:",
      paste(x@target_params, collapse = ", "),
      "\n")
  cat("Number of interim analyses:", x@n_interim_analyses, "\n")
  cat("Thresholds for success:",
      paste(x@thresholds_success, collapse = ", "),
      "\n")
  cat("Thresholds for futility:",
      paste(x@thresholds_futility, collapse = ", "),
      "\n")
  cat("Probability of success significance:", x@p_sig_success, "\n")
  cat("Probability of futility significance:",
      x@p_sig_futility,
      "\n")
  cat(
    "Parameter names - interim function:",
    if (is.null(x@parameter_names_interim_fn))
      "NULL"
    else
      paste(x@parameter_names_interim_fn, collapse = ", "),
    "\n"
  )

  cat("\n=== Brms Model ===\n\n")
  print(x@model@brms_model)

  if (!is.null(x@interim_function)) {
    cat("\n=== Interim Function ===\n\n")
    print(x@interim_function)
  }

  invisible(x)
}
