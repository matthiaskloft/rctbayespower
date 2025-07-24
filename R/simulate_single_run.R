#' Single Run Simulation for RCT Bayesian Power Analysis
#'
#' Executes a single simulation run using pre-validated condition arguments.
#' This function is the core simulation engine used by power analysis functions.
#'
#' @param condition_arguments A single entry from the condition_arguments list
#'   created by [build_conditions()]. Contains 'sim_args' with 'n_total', 'p_alloc',
#'   and 'true_parameter_values', plus optional 'interim_args'.
#' @param id_sim Simulation identifier for tracking individual simulation runs
#' @param design A rctbp_design object containing the simulation and model specifications
#' @param brms_args Arguments passed to brms for model fitting. Default includes 'algorithm' = "sampling", 'iter' = 500, 'warmup' = 250, 'chains' = 4, 'cores' = 1. User can override any of these or add additional arguments.
#'
#' @return A fitted brms model object on success, NULL on failure
#'
#' @examples
#' \dontrun{
#' # Create model, design, and conditions
#' ancova_model <- build_model("ancova_cont_2arms")()
#' design <- build_design(
#'   model = ancova_model,
#'   target_params = "b_arms_treat",
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
#'       b_arms_treat = 0.5,
#'       b_covariate = 0.2
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
                                id_sim,
                                design,
                                brms_args = list()) {
  # no validations since this is the lowest level function

  # Handle both S7 design objects and regular list design components (for parallel workers)
  if (inherits(design, "rctbayespower::rctbp_design") || inherits(design, "rctbp_design")) {
    # S7 design object
    data_simulation_fn <- design@model@data_simulation_fn
    brms_model <- design@model@brms_model
  } else if (is.list(design)) {
    # Regular list with design components (from parallel workers)
    data_simulation_fn <- design$model_data_simulation_fn
    brms_model <- design$model_brms_model
  } else {
    stop("Invalid design object")
  }

  # Simulate data with error handling
  simulated_data <- tryCatch({
    do.call(data_simulation_fn, args = condition_arguments$sim_args)
  }, error = function(e) {
    n_total <- if(is.null(condition_arguments$sim_args$n_total)) "unknown" else condition_arguments$sim_args$n_total
    warning("Data simulation failed for 'n_total'=",
            n_total,
            ": ",
            e$message)
    return(NULL)
  })

  if (is.null(simulated_data)) {
    return(data.frame(
      parameter = NA_character_,
      threshold_success = NA_real_,
      threshold_futility = NA_real_,
      success_prob = NA_real_,
      futility_prob = NA_real_,
      power_success = NA_real_,
      power_futility = NA_real_,
      median = NA_real_,
      mad = NA_real_,
      mean = NA_real_,
      sd = NA_real_,
      rhat = NA_real_,
      ess_bulk = NA_real_,
      ess_tail = NA_real_,
      id_sim = id_sim,
      id_cond = condition_arguments$id_cond,
      converged = 0L,
      error = "Data simulation failed"
    ))
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
  fitted_model <- tryCatch({
    do.call(function(...) {
      stats::update(object = brms_model, newdata = simulated_data, ...)
    }, brms_args_final)
  }, error = function(e) {
    n_total <- if(is.null(condition_arguments$sim_args$n_total)) "unknown" else condition_arguments$sim_args$n_total
    warning("Model fitting failed for 'n_total'=",
            n_total,
            ": ",
            e$message)
    return(NULL)
  })

  # Check if model fitting was successful
  if (is.null(fitted_model)) {
    return(data.frame(
      parameter = NA_character_,
      threshold_success = NA_real_,
      threshold_futility = NA_real_,
      success_prob = NA_real_,
      futility_prob = NA_real_,
      power_success = NA_real_,
      power_futility = NA_real_,
      est_median = NA_real_,
      est_mad = NA_real_,
      est_mean = NA_real_,
      est_sd = NA_real_,
      rhat = NA_real_,
      ess_bulk = NA_real_,
      ess_tail = NA_real_,
      id_sim = id_sim,
      id_cond = condition_arguments$id_cond,
      converged = 0L,
      error = "Model fitting failed"
    ))
  }

  # compute measures
  result <- tryCatch({
    df <- compute_measures_brmsfit(fitted_model, design) |>
      dplyr::mutate(dplyr::across(-parameter, as.numeric))
    df |> dplyr::mutate(
      id_sim = id_sim,
      id_cond = condition_arguments$id_cond,
      converged = 1L,
      error = NA_character_
    )
    # error handling for compute_measures_brmsfit
  }, error = function(e) {
    data.frame(
      parameter = NA_character_,
      threshold_success = NA_real_,
      threshold_futility = NA_real_,
      success_prob = NA_real_,
      futility_prob = NA_real_,
      power_success = NA_real_,
      power_futility = NA_real_,
      est_median = NA_real_,
      est_mad = NA_real_,
      est_mean = NA_real_,
      est_sd = NA_real_,
      rhat = NA_real_,
      ess_bulk = NA_real_,
      ess_tail = NA_real_,
      id_sim = id_sim,
      id_cond = condition_arguments$id_cond,
      converged = 0L,
      error = as.character(e)
    )
  })

  return(result)
}
