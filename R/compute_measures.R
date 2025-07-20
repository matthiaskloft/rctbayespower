compute_measures_brmsfit <- function(brmsfit, design) {
  target_params <- design$target_params
  
  # Compute measures
  measures_list <- purrr::map(target_params, function(param) {
    # extract posterior samples and compute power metrics
    posterior_samples <- brms::as_draws_rvars(brmsfit, variable = param)
    
    # extract thresholds
    if (length(target_params) > 1) {
      threshold_success <- design$thresholds_success[which(target_params == param)]
      # if the threshold is NA, use the first one
      if(is.na(threshold_success)) {
        threshold_success <- design$thresholds_success[1]
      }
      threshold_futility <- design$thresholds_futility[which(target_params == param)]
      # if the threshold is NA, use the first one
      if(is.na(threshold_futility)) {
        threshold_futility <- design$thresholds_futility[1]
      }
    } else {
      threshold_success <- design$thresholds_success
      threshold_futility <- design$thresholds_futility
    }
    # calculate the probability of success / futility
    success_prob <- posterior::Pr(posterior_samples[[param]] > threshold_success)
    futility_prob <- posterior::Pr(posterior_samples[[param]] < threshold_futility)
    # significance
    sig_success <- as.numeric(success_prob >= design$p_sig_success, na.rm = TRUE)
    sig_futility <- as.numeric(futility_prob >= design$p_sig_futility, na.rm = TRUE)
    # parameter estimates
    est_median <- median(posterior_samples[[param]])
    est_mad <- posterior::mad(posterior_samples[[param]])
    est_mean <- mean(posterior_samples[[param]])
    est_sd <- posterior::sd(posterior_samples[[param]])
    # convergence metrics
    rhat <- posterior::rhat(posterior_samples[[param]])
    ess_bulk <- posterior::ess_bulk(posterior_samples[[param]])
    ess_tail <- posterior::ess_tail(posterior_samples[[param]])
    
    # combine results into a list
    out_list <- list(
      parameter = param,
      threshold_success = threshold_success,
      threshold_futility = threshold_futility,
      success_prob = success_prob,
      futility_prob = futility_prob,
      sig_success = sig_success,
      sig_futility = sig_futility,
      est_median = est_median,
      est_mad = est_mad,
      est_mean = est_mean,
      est_sd = est_sd,
      rhat = rhat,
      ess_bulk = ess_bulk,
      ess_tail = ess_tail
    )
    
    return(out_list)
  })
  
  # compute combined probabilities and powers
  if (length(target_params) > 1) {
    
    # extract posterior samples and compute power metrics
    posterior_samples <- brms::as_draws_matrix(brmsfit, variable = target_params)
    # extract thresholds and broadcast if necessary
    if(length(target_params)>length(design$thresholds_success)){
      thresholds_success <- rep(design$thresholds_success[1], length(target_params))
    } else {
      thresholds_success <- design$thresholds_success
    }
    if(length(target_params)>length(design$thresholds_futility)){
      thresholds_futility <- rep(design$thresholds_futility[1], length(target_params))
    } else {
      thresholds_futility <- design$thresholds_futility
    }
    
    # calculate combined probabilities
    combined_success_prob <- mean(apply(ifelse(posterior_samples>thresholds_success, 1 , 0),1,min))
    combined_futility_prob <- mean(apply(ifelse(posterior_samples<thresholds_futility, 1 , 0),1,min))
    
    # calculate combined significance
    combined_sig_success <- as.numeric(combined_success_prob >= design$p_sig_success, na.rm = TRUE)
    combined_sig_futility <- as.numeric(combined_futility_prob >= design$p_sig_futility, na.rm = TRUE)
    
    # combine results into a list
    measures_list_combined <- list(
      parameter = "union",
      threshold_success = NA,
      threshold_futility = NA,
      success_prob = combined_success_prob,
      futility_prob = combined_futility_prob,
      sig_success = combined_sig_success,
      sig_futility = combined_sig_success,
      est_median = NA,
      est_mad = NA,
      est_mean = NA,
      est_sd = NA,
      rhat = NA,
      ess_bulk = NA,
      ess_tail = NA
    )
  }
  
  # remove success_samples and futility_samples from the list
  measures_list <- purrr::map(measures_list, function(x) {
    x$success_samples <- NULL
    x$futility_samples <- NULL
    return(x)
  })
  
  # make data.frames and rbind()
  measures_df <- do.call(rbind, measures_list)
  
  if(length(target_params) > 1) {
    # create a data frame for combined measures
    measures_df_combined <- do.call(cbind, measures_list_combined)
    # add combined measures
    measures_df <- as.data.frame(rbind(measures_df, measures_df_combined))
  } else {
    measures_df <- as.data.frame(measures_df)
  }
  
  return(measures_df)
}




















# # Create power surface data frame
# power_surface <- do.call(rbind, lapply(results_list, function(x) {
#   # Flatten effect_sizes for the data frame
#   effect_cols <- list()
#   if (!is.null(x$effect_sizes)) {
#     for (param_name in names(x$effect_sizes)) {
#       effect_cols[[paste0("effect_", param_name)]] <- x$effect_sizes[[param_name]]
#     }
#   }
#
#   basic_cols <- data.frame(
#     condition_id = x$condition_id,
#     n_total = x$n_total,
#     n_interim_analyses = x$n_interim_analyses,
#     sig_success = x$sig_success,
#     sig_futility = x$sig_futility,
#     mean_prob_success = x$mean_prob_success,
#     mean_prob_futility = x$mean_prob_futility,
#     convergence_rate = x$convergence_rate,
#     stringsAsFactors = FALSE
#   )
#
#   if (length(effect_cols) > 0) {
#     cbind(basic_cols, as.data.frame(effect_cols))
#   } else {
#     basic_cols
#   }
# }))
#
# # Create result object
# result <- list(
#   # Analysis metadata
#   design = design,
#   conditions = conditions,
#
#   # Thresholds from design
#   threshold_success = design$thresholds_success,
#   threshold_futility = design$thresholds_futility,
#
#   # Analysis results
#   power_surface = power_surface,
#
#   # Summary information
#   design_prior = design_prior,
#   design_prior_type = weight_type,
#   n_simulations = n_simulations,
#   analysis_time_minutes = as.numeric(elapsed_time),
#   n_cores = n_cores,
#
#   # Detailed results
#   detailed_results = results_list
# )
#
# class(result) <- "rctbayespower_grid"
#
# # Print summary
# cat("\n=== Power Grid Analysis Complete ===\n")
# cat("Total conditions analyzed:", length(conditions), "\n")
#
#
# error_count <- sum(sapply(results_list, function(x)
#   ! is.null(x$error)))
# if (error_count > 0) {
#   cat("Conditions with errors:", error_count, "\n")
# }