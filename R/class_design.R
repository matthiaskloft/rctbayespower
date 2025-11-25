# =============================================================================
# S7 CLASS DEFINITION: rctbp_design
# =============================================================================
# Links a rctbp_model with global analysis configuration. Stores WHICH
# parameters to analyze and WHAT probability thresholds define success/futility.
# Note: Effect size thresholds (e.g., treatment effect > 0.2) are specified
# per-condition in build_conditions(), not here.

#' @importFrom S7 new_class class_character class_numeric class_integer class_function class_any new_union new_property
rctbp_design <- S7::new_class(
  "rctbp_design",
  properties = list(
    # Core design properties
    model = S7::class_any,                  # rctbp_model object
    target_params = S7::class_character,    # Parameters to analyze (must match model@parameter_names_brms)
    p_sig_scs = S7::class_numeric,          # Probability threshold for success decision
    p_sig_ftl = S7::class_numeric,          # Probability threshold for futility decision
    design_name = S7::class_character | NULL,

    # Interim analysis properties (design-level defaults, can be overridden per-condition)
    analysis_at = S7::new_property(
      class = S7::class_numeric | NULL,
      default = NULL
    ),
    interim_function = S7::new_property(
      class = S7::class_function | NULL,
      default = NULL
    ),
    adaptive = S7::new_property(
      class = S7::class_logical,
      default = FALSE
    )
  ),
  # Validator ensures target_params are valid for the model and probability
  # thresholds are between 0 and 1
  validator = function(self) {
    # Validate model object type
    if (!inherits(self@model, "rctbayespower::rctbp_model")) {
      return("'model' must be a valid rctbp_model object.")
    }

    # Check required parameters are provided
    if (is.null(self@target_params) ||
        length(self@target_params) == 0) {
      return("'target_params' cannot be NULL or empty.")
    }
    if (is.null(self@p_sig_scs)) {
      return("'p_sig_scs' cannot be NULL.")
    }
    if (is.null(self@p_sig_ftl)) {
      return("'p_sig_ftl' cannot be NULL.")
    }

    # Validate target_params exist in brms model
    # This ensures we can extract posteriors for these parameters
    if (!all(self@target_params %in% self@model@parameter_names_brms)) {
      return(paste(
        "'target_params' must be a subset of the parameter names in the model:",
        paste(self@model@parameter_names_brms, collapse = ", ")
      ))
    }

    # Validate probability thresholds are valid probabilities [0, 1]
    if (length(self@p_sig_scs) != 1 ||
        self@p_sig_scs < 0 || self@p_sig_scs > 1) {
      return("'p_sig_scs' must be a numeric value between 0 and 1.")
    }
    if (length(self@p_sig_ftl) != 1 ||
        self@p_sig_ftl < 0 || self@p_sig_ftl > 1) {
      return("'p_sig_ftl' must be a numeric value between 0 and 1.")
    }

    # Validate interim analysis properties
    if (!is.null(self@analysis_at)) {
      # analysis_at must be monotonically increasing
      if (length(self@analysis_at) > 1 &&
          !all(diff(self@analysis_at) > 0)) {
        return("'analysis_at' must be monotonically increasing (each value greater than previous)")
      }
      # analysis_at must be positive integers
      if (any(self@analysis_at <= 0) || any(self@analysis_at != round(self@analysis_at))) {
        return("'analysis_at' must contain positive integers (sample sizes for interim analyses)")
      }
    }

    # Validate interim_function and analysis_at compliance
    # Rule: interim_function requires analysis_at (can't decide without knowing when)
    if (!is.null(self@interim_function) && is.null(self@analysis_at)) {
      return("'interim_function' requires 'analysis_at' to specify when interim analyses occur")
    }
    # Note: analysis_at without interim_function is allowed - a default "continue" function
    # will be used (see interim_continue()), enabling sequential monitoring without stopping

    # Validate adaptive requires interim analysis setup
    if (isTRUE(self@adaptive) && is.null(self@analysis_at)) {
      return("'adaptive = TRUE' requires 'analysis_at' to specify interim timepoints")
    }

    # Validate adaptive is logical
    if (!is.logical(self@adaptive) || length(self@adaptive) != 1) {
      return("'adaptive' must be a single logical value (TRUE or FALSE)")
    }

    NULL  # All validations passed
  }
)

# =============================================================================
# CONSTRUCTOR FUNCTION: build_design()
# =============================================================================
# User-friendly interface to create rctbp_design objects with validation.

#' Constructor for rctbp_design Objects
#'
#' Creates an S7 rctbp_design object that combines a rctbp_model with analysis
#' configuration parameters for Bayesian power analysis. This constructor function
#' provides a user-friendly interface to the S7 class constructor.
#'
#' @param model An S7 object of class "rctbp_model" created by [build_model()]
#' @param target_params Character vector specifying which model parameters to
#'   analyze for power. Must be valid parameter names from the brms model.
#'   Use `model@parameter_names_brms` to discover available names. Required.
#' @param p_sig_scs Probability threshold for declaring success. The posterior
#'   probability that the effect exceeds the success threshold must be greater
#'   than this value to declare success (typically 0.975 or 0.95). Required.
#' @param p_sig_ftl Probability threshold for declaring futility. The posterior
#'   probability that the effect is below the futility threshold must be greater
#'   than this value to declare futility (typically 0.5). Required.
#' @param design_name Optional character string providing a descriptive name for the
#'   design. Defaults to NULL.
#' @param analysis_at Optional numeric vector of sample sizes at which to perform
#'   interim analyses. Must be monotonically increasing positive integers. The final
#'   analysis at `n_total` is added automatically. Set to NULL (default) for
#'   single-look designs with no interim analyses.
#' @param interim_function Optional function to make interim decisions. Must accept
#'   parameters: `interim_summaries`, `current_n`, `analysis_at`, `n_total`.
#'   Should return a list with `decision` ("continue", "stop_success", "stop_futility")
#'   and optionally `modified_params` (for adaptive designs). See
#'   `interim_futility_only()` and `interim_success_futility()` for helper factories.
#' @param adaptive Logical indicating whether the design allows parameter modification
#'   between interim looks (default FALSE). When TRUE, the `interim_function` can return
#'   modified simulation parameters (e.g., updated allocation ratios).
#'
#' @details
#' The rctbp_design class combines model specifications with global analysis
#' configuration parameters. It defines WHICH parameters to analyze and WHAT
#' probability thresholds to use for decision-making.
#'
#' \strong{Important:} The actual success and futility thresholds (e.g., 0.2 for
#' treatment effect) are NOT stored in the design object. They are specified
#' per-condition when creating the conditions object using build_conditions().
#' The design only stores the probability thresholds (p_sig_scs, p_sig_ftl)
#' that determine how certain we need to be about exceeding those thresholds.
#'
#' \strong{Model Integration:} Links to the rctbp_model object containing data
#' simulation function and compiled brms model.
#'
#' \strong{Target Parameters:} Specifies which model parameters to analyze for power.
#' Parameter names are model-dependent. Use `model@parameter_names_brms` to discover
#' available parameters for your specific model.
#'
#' \strong{Probability Thresholds:} The p_sig_scs and p_sig_ftl parameters
#' control the certainty required for success/futility decisions. Higher values require
#' stronger evidence.
#'
#' \strong{Interim Analysis:} The `analysis_at`, `interim_function`, and `adaptive` parameters
#' specify design-level defaults for group sequential designs. These can be overridden
#' per-condition in [build_conditions()] via `condition_values` or `static_values`.
#'
#' \strong{Validation:} All parameters are validated for consistency with the
#' underlying model structure.
#'
#' @return An S7 object of class "rctbp_design" containing:
#' \describe{
#'   \item{model}{The rctbp_model object}
#'   \item{target_params}{Target parameters for analysis}
#'   \item{p_sig_scs}{Success probability threshold}
#'   \item{p_sig_ftl}{Futility probability threshold}
#'   \item{design_name}{Optional descriptive name}
#'   \item{analysis_at}{Interim analysis sample sizes (NULL for single-look)}
#'   \item{interim_function}{Interim decision function (NULL if none)}
#'   \item{adaptive}{Whether design allows parameter modification}
#' }
#'
#' @export
#' @seealso [build_model()], [build_conditions()]
#'
#' @examples
#' \dontrun{
#' # Create an ANCOVA model
#' ancova_model <- build_model("ancova_cont_2arms")
#'
#' # Simple design (no interim analysis)
#' simple_design <- build_design(
#'   model = ancova_model,
#'   target_params = "b_armtreat_1",
#'   p_sig_scs = 0.975,
#'   p_sig_ftl = 0.5
#' )
#'
#' # Design with interim analysis
#' sequential_design <- build_design(
#'   model = ancova_model,
#'   target_params = "b_armtreat_1",
#'   p_sig_scs = 0.975,
#'   p_sig_ftl = 0.5,
#'   analysis_at = c(50, 100),
#'   interim_function = interim_futility_only(futility_threshold = 0.90)
#' )
#' }
build_design <- function(model,
                         target_params,
                         p_sig_scs,
                         p_sig_ftl,
                         design_name = NULL,
                         analysis_at = NULL,
                         interim_function = NULL,
                         adaptive = FALSE) {
  # Use S7 constructor directly - all validation happens in the class validator
  rctbp_design(
    model = model,
    target_params = target_params,
    p_sig_scs = p_sig_scs,
    p_sig_ftl = p_sig_ftl,
    design_name = design_name,
    analysis_at = analysis_at,
    interim_function = interim_function,
    adaptive = adaptive
  )
}

# =============================================================================
# S7 METHOD: print()
# =============================================================================
# Formats rctbp_design objects showing both model specifications and
# analysis configuration parameters.

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
  report <- build_report.rctbp_design(x)
  render_report(report)
  invisible(x)
}
