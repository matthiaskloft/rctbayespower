#' Single Run Simulation for RCT Bayesian Power Analysis
#'
#' Executes a single simulation run using pre-validated condition arguments.
#' This function is the core simulation engine used by power analysis functions.
#'
#' @param condition_arguments A single entry from the condition_arguments list
#'   created by [build_conditions()]. Contains 'sim_args' with 'n_total', 'p_alloc',
#'   and 'true_parameter_values', plus optional 'interim_args'.
#' @param design A rctbayespower_design object containing the simulation and model specifications
#' @param brms_args Arguments passed to brms for model fitting. Default includes 'algorithm' = "sampling", 'iter' = 500, 'warmup' = 250, 'chains' = 4, 'cores' = 1. User can override any of these or add additional arguments.
#'
#' @return A fitted brms model object on success, NULL on failure
#'
#' @examples
#' \dontrun{
#' # Create model, design, and conditions
#' ancova_model <- build_model_ancova_cont()
#' design <- build_design(
#'   model = ancova_model,
#'   target_params = "b_armtreat",
#'   n_interim_analyses = 0,
#'   thresholds_success = 0.2,
#'   thresholds_futility = 0,
#'   "p_sig_success" = 0.975,
#'   "p_sig_futility" = 0.5
#' )
#'
#' conditions <- build_conditions(
#'   design = design,
#'   condition_values = list(n_total = c(100, 200)),
#'   static_values = list(
#'     p_alloc = list(c(0.5, 0.5)),
#'     true_parameter_values = list(
#'       intercept = 0,
#'       sigma = 1,
#'       b_armtreat = 0.5,
#'       b_baseline = 0.2
#'     )
#'   )
#' )
#'
#' # Simulate single condition with default brms settings
#' result <- simulate_single_run(
#'   condition_arguments = conditions$condition_arguments[[1]],
#'   design = conditions$design
#' )
#'
#' # Or with custom brms arguments
#' result_custom <- simulate_single_run(
#'   condition_arguments = conditions$condition_arguments[[1]],
#'   design = conditions$design,
#'   brms_args = list(algorithm = "meanfield", iter = 1000)
#' )
#' }
#' @export
simulate_single_run <- function(condition_arguments,
                                design,
                                brms_args = list()) {
  # no validations since this is the lowest level function

  # Simulate data with error handling
  simulated_data <- tryCatch(
    {
      do.call(design$data_simulation_fn, args = condition_arguments$sim_args)
    },
    error = function(e) {
      n_total <- condition_arguments$sim_args$n_total %||% "unknown"
      warning(
        "Data simulation failed for 'n_total'=",
        n_total,
        ": ",
        e$message
      )
      return(NULL)
    }
  )

  if (is.null(simulated_data)) {
    return(NULL)
  }

  # default brms arguments
  brms_args_default <- list(
    algorithm = "sampling",
    iter = 750,
    warmup = 250,
    chains = 4,
    cores = 1,
    init = 0.1,
    refresh = 0,
    silent = 2
  )

  # Merge arguments: defaults < brms_args
  brms_args_final <- utils::modifyList(brms_args_default, brms_args)

  # warn if cores > 1
  if (brms_args_final$cores > 1) {
    warning("Do not use multiple cores for brms when running simulations in parallel!")
  }

  # Fit the model to the simulated data with error handling
  fitted_model <- tryCatch(
    {
      do.call(function(...) {
        stats::update(object = design$brms_model, newdata = simulated_data, ...)
      }, brms_args_final)
    },
    error = function(e) {
      n_total <- condition_arguments$sim_args$n_total %||% "unknown"
      warning(
        "Model fitting failed for 'n_total'=",
        n_total,
        ": ",
        e$message
      )
      return(NULL)
    }
  )

  return(fitted_model) # Either brmsfit object or NULL
}
