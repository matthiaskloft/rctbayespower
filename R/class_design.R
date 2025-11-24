# S7 Class Definition for RCT Bayesian Power Design
#' @importFrom S7 new_class class_character class_numeric class_integer class_function class_any new_union
rctbp_design <- S7::new_class(
  "rctbp_design",
  properties = list(
    model = S7::class_any,
    # Inherits from rctbp_model
    target_params = S7::class_character,
    p_sig_success = S7::class_numeric,
    p_sig_futility = S7::class_numeric,
    design_name = S7::class_character | NULL
  ),
  validator = function(self) {
    # validate model
    if (!inherits(self@model, "rctbayespower::rctbp_model")) {
      return("'model' must be a valid rctbp_model object.")
    }

    # Check for required parameters
    if (is.null(self@target_params) ||
        length(self@target_params) == 0) {
      return("'target_params' cannot be NULL or empty.")
    }
    if (is.null(self@p_sig_success)) {
      return("'p_sig_success' cannot be NULL.")
    }
    if (is.null(self@p_sig_futility)) {
      return("'p_sig_futility' cannot be NULL.")
    }

    # Check that target_params is a subset of the parameter names in the model
    if (!all(self@target_params %in% self@model@parameter_names_brms)) {
      return(paste(
        "'target_params' must be a subset of the parameter names in the model:",
        paste(self@model@parameter_names_brms, collapse = ", ")
      ))
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
#' @param p_sig_success Probability threshold for declaring success. The posterior
#'   probability that the effect exceeds the success threshold must be greater
#'   than this value to declare success (typically 0.975 or 0.95). Required.
#' @param p_sig_futility Probability threshold for declaring futility. The posterior
#'   probability that the effect is below the futility threshold must be greater
#'   than this value to declare futility (typically 0.5). Required.
#' @param design_name Optional character string providing a descriptive name for the
#'   design. Defaults to NULL.
#'
#' @details
#' The rctbp_design class combines model specifications with global analysis
#' configuration parameters. It defines WHICH parameters to analyze and WHAT
#' probability thresholds to use for decision-making.
#'
#' \strong{Important:} The actual success and futility thresholds (e.g., 0.2 for
#' treatment effect) are NOT stored in the design object. They are specified
#' per-condition when creating the conditions object using build_conditions().
#' The design only stores the probability thresholds (p_sig_success, p_sig_futility)
#' that determine how certain we need to be about exceeding those thresholds.
#'
#' \strong{Model Integration:} Links to the rctbp_model object containing data
#' simulation function and compiled brms model.
#'
#' \strong{Target Parameters:} Specifies which model parameters to analyze for power
#' (e.g., "b_arms_treat" for treatment effect).
#'
#' \strong{Probability Thresholds:} The p_sig_success and p_sig_futility parameters
#' control the certainty required for success/futility decisions. Higher values require
#' stronger evidence.
#'
#' \strong{Validation:} All parameters are validated for consistency with the
#' underlying model structure.
#'
#' @return An S7 object of class "rctbp_design" containing:
#' \describe{
#'   \item{model}{The rctbp_model object}
#'   \item{target_params}{Target parameters for analysis}
#'   \item{p_sig_success}{Success probability threshold}
#'   \item{p_sig_futility}{Futility probability threshold}
#'   \item{design_name}{Optional descriptive name}
#' }
#'
#' @export
#' @seealso [build_model()], [simulate_single_run()]
#'
#' @examples
#' \dontrun{
#' # Create an ANCOVA model
#' ancova_model <- build_model("ancova_cont_2arms")
#'
#' # Create a design for analyzing treatment effect
#' my_design <- build_design(
#'   model = ancova_model,
#'   target_params = "b_arms_treat",
#'   p_sig_success = 0.975,
#'   p_sig_futility = 0.5,
#'   design_name = "ANCOVA Treatment Effect Analysis"
#' )
#' }
build_design <- function(model,
                         target_params,
                         p_sig_success,
                         p_sig_futility,
                         design_name = NULL) {
  # Use S7 constructor directly - all validation happens in the class validator
  rctbp_design(
    model = model,
    target_params = target_params,
    p_sig_success = p_sig_success,
    p_sig_futility = p_sig_futility,
    design_name = design_name
  )
}


# S7 Method for Print (uses existing base print generic)

#' Print Method for rctbp_design Objects
#'
#' Displays a comprehensive summary of a rctbp_design object, showing
#' both model specifications and analysis configuration in an organized format.
#'
#' @param x An S7 object of class "rctbp_design"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object. Used for side effects (printing).
#' @importFrom S7 method
#' @name print.rctbp_design
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
  cat(
    "Parameter names - brms model:",
    paste(x@model@parameter_names_brms, collapse = ", "),
    "\n"
  )

  # design
  cat("\n=== Design Specifications ===\n\n")
  cat("Design name:", if (is.null(x@design_name))
    "NULL"
    else
      x@design_name, "\n")
  cat("Target parameters:",
      paste(x@target_params, collapse = ", "),
      "\n")
  cat("Probability threshold for success:", x@p_sig_success, "\n")
  cat("Probability threshold for futility:", x@p_sig_futility, "\n")
  cat("\nNote: Decision criteria (thresholds, interim schedules) are specified\n")
  cat("      per-condition in the conditions object.\n")

  cat("\n=== 'brms' Model ===\n\n")
  print(x@model@brms_model)

  invisible(x)
}
