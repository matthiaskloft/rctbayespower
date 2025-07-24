#' # Global variables for R CMD check
#' utils::globalVariables(
#'   c(
#'     "parameter",
#'     "threshold_success",
#'     "threshold_futility",
#'     "id_cond",
#'     "success_prob",
#'     "futility_prob",
#'     "sig_success",
#'     "sig_futility",
#'     "est_median",
#'     "est_mad",
#'     "est_mean",
#'     "est_sd",
#'     "rhat",
#'     "ess_bulk",
#'     "ess_tail",
#'     "error"
#'   )
#' )
#'
#' #' Power Grid Analysis for Bayesian RCTs with Robust Parallel Backend
#' #'
#' #' Comprehensive power analysis across multiple conditions using a cluster-based
#' #' parallel backend for maximal speed and stability.
#' #'
#' #' @param conditions A conditions object created by [build_conditions()] containing:
#' #'   \itemize{
#' #'     \item design: An rctbp_design object with model specifications
#' #'     \item condition_arguments: List of prepared condition arguments for simulation
#' #'   }
#' #' @param design_prior Optional design prior for integrated power computation. See original API docs.
#' #' @param n_sims Number of MCMC iterations per condition (default: 500)
#' #' @param n_cores Number of parallel cores for execution (default: 1)
#' #' @param n_progress_updates Show progress every N conditions when running sequentially (default: 10)
#' #' @param brms_args Arguments passed to brms for model fitting.
#' #' @param ... Additional arguments passed to brms (override defaults and brms_args).
#' #' @return An object of class "rctbayespower_sim_result" with raw and aggregated results.
#' #' @importFrom parallel detectCores makeCluster stopCluster parLapply clusterEvalQ clusterExport
#' #' @importFrom utils modifyList
#' #' @export
#' power_analysis <- function(
#'     conditions,
#'     design_prior = NULL,
#'     n_sims = 500,
#'     n_cores = 1,
#'     n_progress_updates = 10,
#'     brms_args = list(),
#'     ...
#' ) {
#'   # time start
#'   start_time <- Sys.time()
#'
#'   if (!inherits(conditions, "rctbayespower_conditions")) {
#'     stop("'conditions' must be a valid rctbayespower_conditions object")
#'   }
#'   if (!is.numeric(n_sims) || n_sims <= 0) {
#'     stop("'n_sims' must be a positive number")
#'   }
#'   if (!is.numeric(n_cores) || n_cores <= 0) {
#'     warning("Invalid n_cores value. Using n_cores = 1.")
#'     n_cores <- 1
#'   }
#'
#'   # Expand conditions for each simulation
#'   condition_args_list <- rep(conditions$condition_arguments, each = n_sims)
#'   design <- conditions$design
#'   if (!inherits(design, "rctbayespower::rctbp_design")) {
#'     stop("'design' must be a valid rctbp_design object")
#'   }
#'
#'   # Parse design_prior if provided (same as original) ...
#'
#'   # Set up cluster
#'   if (n_cores > 1) {
#'     cl <- parallel::makeCluster(n_cores)
#'     parallel::clusterEvalQ(cl, {
#'       library(rctbayespower)
#'       library(brms)
#'       library(dplyr)
#'     })
#'     parallel::clusterExport(
#'       cl,
#'       varlist = c(
#'         "condition_args_list", "design", "brms_args",
#'         "simulate_single_run", "compute_measures_brmsfit"
#'       ),
#'       envir = environment()
#'     )
#'   }
#'
#'   # Progress setup
#'   total_runs <- length(condition_args_list)
#'   pb <- NULL
#'   if (n_cores == 1) {
#'     pb <- utils::txtProgressBar(min = 0, max = total_runs, style = 3)
#'   }
#'
#'   # Run simulations
#'   if (n_cores > 1) {
#'     results_raw_list <- parallel::parLapply(cl, seq_along(condition_args_list), function(i) {
#'       args <- condition_args_list[[i]]
#'       tryCatch({
#'         fit <- simulate_single_run(condition_arguments = args,
#'                                    design = design,
#'                                    brms_args = brms_args)
#'         df <- compute_measures_brmsfit(fit, design) |> mutate(across(-parameter, as.numeric))
#'         df |> mutate(
#'           id_sim = i,
#'           id_cond = args$id_cond,
#'           converged = 1L,
#'           error = NA_character_
#'         )
#'       }, error = function(e) {
#'         data.frame(
#'           parameter = NA_character_, threshold_success = NA_real_, threshold_futility = NA_real_,
#'           success_prob = NA_real_, futility_prob = NA_real_, sig_success = NA_real_,
#'           sig_futility = NA_real_, est_median = NA_real_, est_mad = NA_real_,
#'           est_mean = NA_real_, est_sd = NA_real_, rhat = NA_real_, ess_bulk = NA_real_,
#'           ess_tail = NA_real_, id_sim = i, id_cond = args$id_cond,
#'           converged = 0L, error = as.character(e)
#'         )
#'       })
#'     })
#'     parallel::stopCluster(cl)
#'   } else {
#'     results_raw_list <- lapply(seq_along(condition_args_list), function(i) {
#'       args <- condition_args_list[[i]]
#'       res <- tryCatch({
#'         fit <- simulate_single_run(condition_arguments = args,
#'                                    design = design,
#'                                    brms_args = brms_args)
#'         df <- compute_measures_brmsfit(fit, design) |> mutate(across(-parameter, as.numeric))
#'         df |> mutate(
#'           id_sim = i,
#'           id_cond = args$id_cond,
#'           converged = 1L,
#'           error = NA_character_
#'         )
#'       }, error = function(e) {
#'         data.frame(
#'           parameter = NA_character_, threshold_success = NA_real_, threshold_futility = NA_real_,
#'           success_prob = NA_real_, futility_prob = NA_real_, sig_success = NA_real_,
#'           sig_futility = NA_real_, est_median = NA_real_, est_mad = NA_real_,
#'           est_mean = NA_real_, est_sd = NA_real_, rhat = NA_real_, ess_bulk = NA_real_,
#'           ess_tail = NA_real_, id_sim = i, id_cond = args$id_cond,
#'           converged = 0L, error = as.character(e)
#'         )
#'       })
#'       utils::setTxtProgressBar(pb, i)
#'       res
#'     })
#'     close(pb)
#'   }
#'
#'   # Combine raw results
#'   results_df_raw <- dplyr::bind_rows(results_raw_list)
#'
#'   # Aggregate across simulations
#'   results_df <- results_df_raw |>
#'     dplyr::group_by(id_cond, parameter, threshold_success, threshold_futility) |>
#'     dplyr::summarise(
#'       success_prob = mean(success_prob, na.rm = TRUE),
#'       futility_prob = mean(futility_prob, na.rm = TRUE),
#'       sig_success = mean(sig_success, na.rm = TRUE),
#'       sig_futility = mean(sig_futility, na.rm = TRUE),
#'       est_median = mean(est_median, na.rm = TRUE),
#'       est_mad = mean(est_mad, na.rm = TRUE),
#'       est_mean = mean(est_mean, na.rm = TRUE),
#'       est_sd = mean(est_sd, na.rm = TRUE),
#'       rhat = mean(rhat, na.rm = TRUE),
#'       ess_bulk = mean(ess_bulk, na.rm = TRUE),
#'       ess_tail = mean(ess_tail, na.rm = TRUE),
#'       convergence_rate = sum(converged) / n_sims,
#'       error = paste(unique(error), collapse = "; ")
#'     ) |>
#'     dplyr::ungroup()
#'
#'   results_df <- dplyr::full_join(conditions$conditions_grid, results_df)
#'
#'   elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
#'
#'   return_list <- list(
#'     results_df = results_df,
#'     results_df_raw = results_df_raw,
#'     design = design,
#'     conditions = conditions,
#'     elapsed_time = elapsed_time
#'   )
#'   class(return_list) <- "rctbayespower_sim_result"
#'
#'   return(return_list)
#' }
