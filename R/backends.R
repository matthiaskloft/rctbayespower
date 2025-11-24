# Backend Abstraction Layer for rctbayespower
# Provides unified interface for multiple posterior estimation backends

#' Extract Posterior Samples as rvars from brms Model
#'
#' Extracts posterior samples from a fitted brms model and converts them
#' to rvar format from the posterior package. This is the brms-specific
#' implementation of posterior extraction.
#'
#' @param brmsfit A fitted brmsfit object
#' @param target_params Character vector of parameter names to extract
#'
#' @return A named list of rvar objects, one per target parameter
#' @keywords internal
extract_posterior_rvars_brms <- function(brmsfit, target_params) {
  if (!inherits(brmsfit, "brmsfit")) {
    stop("'brmsfit' must be a fitted brms model object")
  }

  # Extract all target parameters as rvars
  posterior_rvars <- brms::as_draws_rvars(brmsfit, variable = target_params)

  return(posterior_rvars)
}


#' Extract Posterior Samples as rvars from NPE Output
#'
#' Converts neural posterior estimation (NPE/Bayesflow) output to rvar format
#' from the posterior package. This is the NPE-specific implementation of
#' posterior extraction.
#'
#' @param npe_output Array of posterior samples from NPE model.
#'   Expected format: `[n_posterior_samples, n_parameters]` for single simulation
#'   or `[batch_size, n_posterior_samples, n_parameters]` for batch.
#' @param target_params Character vector of parameter names corresponding to
#'   columns in the posterior samples array.
#' @param sim_index Integer index of simulation to extract from batch. If NULL,
#'   assumes npe_output is for a single simulation.
#'
#' @return A named list of rvar objects, one per target parameter
#' @keywords internal
extract_posterior_rvars_npe <- function(npe_output, target_params, sim_index = NULL) {
  # Handle batch vs single simulation
  if (!is.null(sim_index)) {
    # Extract single simulation from batch
    # npe_output is [batch_size, n_samples, n_params]
    posterior_samples <- npe_output[sim_index, , , drop = FALSE]
    # Result is [1, n_samples, n_params], squeeze to [n_samples, n_params]
    posterior_samples <- posterior_samples[1, , ]
  } else {
    # Single simulation
    # npe_output is [n_samples, n_params]
    posterior_samples <- npe_output
  }

  # Convert to rvar format
  # posterior_samples should be [n_samples, n_params]
  n_params <- length(target_params)

  if (ncol(posterior_samples) != n_params) {
    stop("Number of parameters in 'npe_output' (",
         ncol(posterior_samples),
         ") does not match length of 'target_params' (",
         n_params, ")")
  }

  # Create named list of rvars
  posterior_rvars <- list()
  for (i in seq_along(target_params)) {
    param_name <- target_params[i]
    # Extract column i and convert to rvar
    param_samples <- posterior_samples[, i]
    posterior_rvars[[param_name]] <- posterior::rvar(param_samples)
  }

  # Convert to draws_rvars format for consistency with brms
  posterior_rvars <- posterior::as_draws_rvars(posterior_rvars)

  return(posterior_rvars)
}


#' Estimate Posterior Using brms Backend
#'
#' Fits a brms model to observed data by updating a pre-compiled template model.
#'
#' @param data Data frame containing the observed data
#' @param brms_model Pre-compiled brmsfit template model (chains = 0)
#' @param backend_args Named list of brms-specific arguments (chains, iter, cores, etc.)
#'
#' @return A fitted brmsfit object containing posterior samples
#' @keywords internal
estimate_posterior_brms <- function(data, brms_model, backend_args = list()) {
  if (!inherits(brms_model, "brmsfit")) {
    stop("'brms_model' must be a valid brmsfit object")
  }

  # Merge with default brms args if not provided
  default_args <- list(
    object = brms_model,
    newdata = data
  )

  # User backend_args override defaults
  final_args <- modifyList(default_args, backend_args)

  # Fit model
  fitted_model <- do.call(stats::update, final_args)

  return(fitted_model)
}


#' Estimate Posterior Using NPE Backend
#'
#' Generates posterior samples using a trained neural posterior estimation model.
#' This function handles both single simulations and batches.
#'
#' @param data Data frame or list of data frames. For batch processing, provide
#'   a list where each element is one simulation's data.
#' @param npe_model Trained keras/tensorflow neural network model
#' @param backend_args Named list of NPE-specific arguments including:
#'   \itemize{
#'     \item n_posterior_samples: Number of posterior samples to generate (default: 4000)
#'     \item batch_size: Size of batches for processing (used elsewhere, not here)
#'   }
#'
#' @return Array of posterior samples. For single simulation: `[n_samples, n_params]`.
#'   For batch: `[batch_size, n_samples, n_params]`.
#' @keywords internal
estimate_posterior_npe <- function(data, npe_model, backend_args = list()) {
  # Extract configuration
  n_samples <- backend_args$n_posterior_samples %||% 4000

  # Check if data is a batch (list) or single simulation (data.frame)
  is_batch <- is.list(data) && !is.data.frame(data)

  if (is_batch) {
    # Batch processing
    batch_size <- length(data)

    # Stack data for neural network input
    # This needs to be implemented based on NPE model's expected input format
    stacked_data <- stack_data_for_npe(data)

    # Forward pass through neural network
    # NPE model should output parameters of posterior distribution
    posterior_params <- npe_model$predict(stacked_data)

    # Sample from the learned posterior
    # This depends on the NPE architecture (e.g., mixture density network, normalizing flow)
    posterior_samples <- sample_from_npe_posterior(
      posterior_params,
      n_samples = n_samples,
      batch_size = batch_size
    )

    # Return: [batch_size, n_samples, n_params]
    return(posterior_samples)
  } else {
    # Single simulation
    # Prepare data for neural network
    prepared_data <- prepare_data_for_npe(data)

    # Forward pass
    posterior_params <- npe_model$predict(prepared_data)

    # Sample from posterior
    posterior_samples <- sample_from_npe_posterior(
      posterior_params,
      n_samples = n_samples,
      batch_size = 1
    )

    # Return: [n_samples, n_params]
    return(posterior_samples[1, , ])
  }
}


#' Stack Data for NPE Model Input
#'
#' Converts a list of simulation data frames into a format suitable for
#' batch processing by a neural network. The exact format depends on the
#' NPE model architecture.
#'
#' @param data_list List of data frames, one per simulation
#'
#' @return Array or tensor suitable for keras model input
#' @keywords internal
stack_data_for_npe <- function(data_list) {
  # This is a placeholder - actual implementation depends on NPE model architecture
  # Common approaches:
  # 1. Convert each data frame to a fixed-size summary statistic vector
  # 2. Pad/truncate to fixed size and stack
  # 3. Use sequence model with variable length

  stop("stack_data_for_npe() must be implemented based on specific NPE model architecture")
}


#' Prepare Single Simulation Data for NPE
#'
#' Converts a single simulation's data frame into format suitable for NPE model.
#'
#' @param data Data frame for one simulation
#'
#' @return Array or tensor suitable for keras model input (with batch dimension)
#' @keywords internal
prepare_data_for_npe <- function(data) {
  # This is a placeholder - actual implementation depends on NPE model architecture
  # Should add batch dimension even for single simulation

  stop("prepare_data_for_npe() must be implemented based on specific NPE model architecture")
}


#' Sample from NPE Posterior Distribution
#'
#' Generates posterior samples from the parameters learned by the NPE model.
#' The sampling strategy depends on the type of NPE architecture used.
#'
#' @param posterior_params Output from NPE model forward pass. Format depends
#'   on architecture (e.g., mean/variance for Gaussian, flow parameters, etc.)
#' @param n_samples Number of posterior samples to generate per simulation
#' @param batch_size Number of simulations in the batch
#'
#' @return Array of shape `[batch_size, n_samples, n_params]`
#' @keywords internal
sample_from_npe_posterior <- function(posterior_params, n_samples, batch_size) {
  # This is a placeholder - actual implementation depends on NPE architecture
  # Examples:
  # - Mixture Density Network: Sample from mixture of Gaussians
  # - Normalizing Flow: Apply inverse flow transformation to base samples
  # - MAF/IAF: Autoregressive sampling

  stop("sample_from_npe_posterior() must be implemented based on specific NPE model architecture")
}


#' Generic Posterior Estimation Dispatcher
#'
#' Routes to appropriate backend-specific estimation function based on backend type.
#'
#' @param data Data frame or list of data frames containing observed data
#' @param model Either a brmsfit object or keras model depending on backend
#' @param backend Character string: "brms" or "npe"
#' @param backend_args Named list of backend-specific arguments
#'
#' @return Backend-specific fitted model or posterior samples
#' @keywords internal
estimate_posterior <- function(data, model, backend, backend_args = list()) {
  switch(backend,
    brms = estimate_posterior_brms(data, model, backend_args),
    npe = estimate_posterior_npe(data, model, backend_args),
    stop("Unknown backend: '", backend, "'. Supported backends: 'brms', 'npe'")
  )
}


#' Generic Posterior Extraction Dispatcher
#'
#' Routes to appropriate backend-specific rvar extraction function.
#'
#' @param estimation_result Output from estimate_posterior() - format depends on backend
#' @param backend Character string: "brms" or "npe"
#' @param target_params Character vector of parameter names to extract
#' @param sim_index For NPE batch results, which simulation to extract (NULL for single sim)
#'
#' @return Named list of rvar objects (as draws_rvars format)
#' @keywords internal
extract_posterior_rvars <- function(estimation_result, backend, target_params, sim_index = NULL) {
  switch(backend,
    brms = extract_posterior_rvars_brms(estimation_result, target_params),
    npe = extract_posterior_rvars_npe(estimation_result, target_params, sim_index),
    stop("Unknown backend: '", backend, "'. Supported backends: 'brms', 'npe'")
  )
}
