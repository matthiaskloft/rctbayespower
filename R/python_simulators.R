# =============================================================================
# PYTHON SIMULATOR INTEGRATION
# =============================================================================
# Functions to load and use Python simulation functions from inst/python/
# for efficient data generation with BayesFlow backend.
#
# The Python simulators mirror the R batch simulation functions but are more
# efficient when used with the BayesFlow backend as they avoid R/Python
# data conversion overhead.

# Package-level cache for Python simulator module
.py_sim_cache <- new.env(parent = emptyenv())


#' Load Python Simulators Module
#'
#' Loads the Python simulators from inst/python/simulators/ and caches
#' the module for reuse. These simulators are optimized for use with
#' the BayesFlow backend.
#'
#' @return Python module with simulator functions
#'
#' @details
#' The module provides:
#' - `simulate_ancova_2arms()`: 2-arm ANCOVA batch simulation
#' - `simulate_ancova_3arms()`: 3-arm ANCOVA batch simulation
#' - `ANCOVASimulator2Arms`: Class for 2-arm simulation with prior sampling
#' - `ANCOVASimulator3Arms`: Class for 3-arm simulation with prior sampling
#'
#' @export
#'
#' @examples
#' \dontrun{
#' py_sims <- load_python_simulators()
#' data <- py_sims$simulate_ancova_2arms(
#'   n_sims = 64L,
#'   n_total = 100L,
#'   b_arm_treat = 0.5,
#'   b_covariate = 0.3
#' )
#' dim(data$outcome)  # [64, 100]
#' }
load_python_simulators <- function() {
  # Return cached if available
  if (exists("simulators", envir = .py_sim_cache) &&
      !is.null(.py_sim_cache$simulators)) {
    return(.py_sim_cache$simulators)
  }

  # Ensure Python is available
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg reticulate} is required for Python simulators",
      "i" = "Install with: {.code install.packages('reticulate')}"
    ))
  }

  # Initialize BayesFlow Python environment (sets up numpy etc.)
  init_bf_python()

  # Find simulator module path
  sim_path <- system.file("python", package = "rctbayespower")
  if (sim_path == "") {
    cli::cli_abort(c(
      "Python simulators not found in package",
      "i" = "Expected location: inst/python/simulators/"
    ))
  }

  # Import simulators module
  .py_sim_cache$simulators <- tryCatch({
    reticulate::import_from_path("simulators", path = sim_path)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to load Python simulators",
      "x" = conditionMessage(e),
      "i" = "Ensure numpy is installed: {.code reticulate::py_require('numpy')}"
    ))
  })

  .py_sim_cache$simulators
}


#' Create Python Simulation Function for Model
#'
#' Creates an R wrapper function that calls the Python simulator.
#' This function can be used as `data_simulation_fn` in `rctbp_model`
#' for efficient simulation with BayesFlow backend.
#'
#' @param model_type Character: "ancova_cont_2arms" or "ancova_cont_3arms"
#' @param ... Default parameter values passed to the simulator
#'
#' @return Function compatible with rctbp_model@sim_fn
#'
#' @details
#' The returned function accepts the same parameters as the R simulation
#' functions but calls Python internally for efficiency. This is particularly
#' useful for BayesFlow power analysis where many simulations are needed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create Python-backed simulation function
#' py_sim_fn <- create_python_sim_fn(
#'   "ancova_cont_2arms",
#'   p_alloc = 0.5,
#'   intercept = 0,
#'   b_covariate = 0.3,
#'   sigma = 1
#' )
#'
#' # Use in model (for BayesFlow backend)
#' model <- rctbp_model(
#'   data_simulation_fn = py_sim_fn,
#'   bayesflow_model = bf_model,
#'   backend = "bf"
#' )
#' }
create_python_sim_fn <- function(model_type, ...) {
  defaults <- list(...)

  if (model_type == "ancova_cont_2arms") {
    # Return wrapper function for 2-arm ANCOVA
    function(n_sims = 1L, n_total, p_alloc = c(.5, .5), intercept = 0,
             b_arm_treat = NULL, b_covariate = NULL, sigma = 1) {

      py_sims <- load_python_simulators()

      # Python expects p_alloc as scalar (treatment probability)
      # R uses c(p_ctrl, p_treat), so extract treatment probability
      p_treat <- if (length(p_alloc) == 2) p_alloc[2] else p_alloc

      params <- list(
        n_sims = as.integer(n_sims),
        n_total = as.integer(n_total),
        p_alloc = p_treat,
        intercept = intercept,
        b_arm_treat = b_arm_treat,
        b_covariate = b_covariate,
        sigma = sigma
      )

      do.call(py_sims$simulate_ancova_2arms, params)
    }

  } else if (model_type == "ancova_cont_3arms") {
    # Return wrapper function for 3-arm ANCOVA
    function(n_sims = 1L, n_total, p_alloc = c(1/3, 1/3, 1/3), intercept = 0,
             b_arm_treat = NULL, b_covariate = NULL, sigma = 1) {

      py_sims <- load_python_simulators()

      params <- list(
        n_sims = as.integer(n_sims),
        n_total = as.integer(n_total),
        p_alloc = p_alloc,
        intercept = intercept,
        b_arm_treat = b_arm_treat,
        b_covariate = b_covariate,
        sigma = sigma
      )

      do.call(py_sims$simulate_ancova_3arms, params)
    }

  } else {
    cli::cli_abort(c(
      "Unknown model type: {.val {model_type}}",
      "i" = "Supported types: 'ancova_cont_2arms', 'ancova_cont_3arms'"
    ))
  }
}


#' Get Python Simulator Class
#'
#' Returns a Python simulator class that can be used directly with
#' BayesFlow for training or inference.
#'
#' @param model_type Character: "ancova_cont_2arms" or "ancova_cont_3arms"
#' @param ... Parameters to initialize the simulator class
#'
#' @return Python simulator class instance
#'
#' @details
#' The simulator class provides:
#' - `__call__(n_sims, n_total, ...)`: Generate simulated data
#' - `sample_prior(n_sims)`: Sample parameters from prior distribution
#'
#' This is useful for BayesFlow training workflows where the simulator
#' needs to be passed directly to BayesFlow functions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get simulator class for BayesFlow training
#' simulator <- get_python_simulator(
#'   "ancova_cont_2arms",
#'   b_arm_treat = 0.5,
#'   b_covariate = 0.3
#' )
#'
#' # Use with BayesFlow
#' import bayesflow as bf
#' workflow <- bf$make_simulator(simulator)
#' }
get_python_simulator <- function(model_type, ...) {
  py_sims <- load_python_simulators()

  params <- list(...)

  if (model_type == "ancova_cont_2arms") {
    do.call(py_sims$ANCOVASimulator2Arms, params)
  } else if (model_type == "ancova_cont_3arms") {
    do.call(py_sims$ANCOVASimulator3Arms, params)
  } else {
    cli::cli_abort(c(
      "Unknown model type: {.val {model_type}}",
      "i" = "Supported types: 'ancova_cont_2arms', 'ancova_cont_3arms'"
    ))
  }
}


#' Check if Python Simulators are Available
#'
#' Checks if the Python simulator infrastructure is available.
#' This includes checking for reticulate, Python, numpy, and the
#' simulator module itself.
#'
#' @param silent If TRUE, return FALSE instead of error (default FALSE)
#'
#' @return Logical indicating availability
#' @export
#'
#' @examples
#' if (check_python_sims_available(silent = TRUE)) {
#'   message("Python simulators available!")
#' }
check_python_sims_available <- function(silent = FALSE) {
  # Check reticulate
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    if (silent) return(FALSE)
    cli::cli_abort("Package 'reticulate' is required")
  }

  # Check Python
  if (!reticulate::py_available(initialize = TRUE)) {
    if (silent) return(FALSE)
    cli::cli_abort("Python is not available")
  }

  # Check numpy
  if (!reticulate::py_module_available("numpy")) {
    if (silent) return(FALSE)
    cli::cli_abort("Python package 'numpy' is required")
  }

  # Check simulator module exists
  sim_path <- system.file("python", "simulators", "__init__.py",
                          package = "rctbayespower")
  if (sim_path == "") {
    if (silent) return(FALSE)
    cli::cli_abort("Python simulators not found in package")
  }

  TRUE
}
