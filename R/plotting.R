#' Plot Power Analysis Results
#'
#' Create visualizations for Bayesian power analysis results, including
#' power curves, sample size plots, and effect size distributions.
#'
#' @param x An object of class 'rctbayespower', 'rctbayespower_samplesize', 
#'   or 'rctbayespower_curve'
#' @param type Type of plot ("power_curve", "sample_size", "effect_distribution")
#' @param power_metric Which power metric to plot ("rope", "direction", "significant")
#' @param ... Additional arguments passed to ggplot2 functions
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' # Plot power curve
#' power_curve_result <- bayesian_power_curve(50, 50, seq(0, 1, 0.1))
#' plot_power_curve(power_curve_result)
#' 
#' # Plot sample size analysis
#' sample_size_result <- sample_size_analysis(0.5, 0.8)
#' plot_power_curve(sample_size_result, type = "sample_size")
#' }
plot_power_curve <- function(x, 
                            type = "auto",
                            power_metric = "rope",
                            ...) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }
  
  # Auto-detect plot type based on object class
  if (type == "auto") {
    if (inherits(x, "rctbayespower_curve")) {
      type <- "power_curve"
    } else if (inherits(x, "rctbayespower_samplesize")) {
      type <- "sample_size"
    } else if (inherits(x, "rctbayespower")) {
      type <- "effect_distribution"
    } else {
      stop("Cannot auto-detect plot type. Please specify 'type' argument.")
    }
  }
  
  # Validate power metric
  power_column <- switch(power_metric,
                        "rope" = "power_rope",
                        "direction" = "power_direction", 
                        "significant" = "power_significant",
                        stop("power_metric must be 'rope', 'direction', or 'significant'"))
  
  if (type == "power_curve") {
    # Power curve plot
    plot_data <- x$power_curve
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = effect_size, y = .data[[power_column]])) +
      ggplot2::geom_line(size = 1.2, color = "steelblue") +
      ggplot2::geom_point(size = 3, color = "steelblue") +
      ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", alpha = 0.7) +
      ggplot2::labs(
        title = "Bayesian Power Curve",
        subtitle = paste("Sample size: Control =", x$n_control, ", Treatment =", x$n_treatment),
        x = "Effect Size",
        y = paste("Power (", stringr::str_to_title(power_metric), ")", sep = ""),
        caption = "Dashed line indicates 80% power"
      ) +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 10)
      )
    
  } else if (type == "sample_size") {
    # Sample size plot
    plot_data <- x$power_curve
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = n_per_group, y = .data[[power_column]])) +
      ggplot2::geom_line(size = 1.2, color = "darkgreen") +
      ggplot2::geom_point(size = 3, color = "darkgreen") +
      ggplot2::geom_hline(yintercept = x$target_power, linetype = "dashed", color = "red", alpha = 0.7) +
      ggplot2::labs(
        title = "Sample Size Analysis",
        subtitle = paste("Effect size =", x$effect_size, ", Target power =", x$target_power),
        x = "Sample Size (per group)",
        y = paste("Power (", stringr::str_to_title(power_metric), ")", sep = ""),
        caption = paste("Dashed line indicates target power of", x$target_power)
      ) +
      ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 10)
      )
    
    # Add vertical line for minimum sample size if available
    min_n_col <- switch(power_metric,
                       "rope" = "min_n_rope",
                       "direction" = "min_n_direction",
                       "min_n_rope")  # default fallback
    
    if (!is.na(x[[min_n_col]])) {
      p <- p + ggplot2::geom_vline(xintercept = x[[min_n_col]], 
                                   linetype = "dotted", color = "blue", alpha = 0.7)
    }
    
  } else if (type == "effect_distribution") {
    # Effect size distribution from single power analysis
    if (is.null(x$simulation_results)) {
      stop("No simulation results available for effect distribution plot.")
    }
    
    effect_estimates <- sapply(x$simulation_results, function(sim) sim$treatment_effect_mean)
    
    plot_data <- data.frame(effect_estimate = effect_estimates)
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = effect_estimate)) +
      ggplot2::geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
      ggplot2::geom_vline(xintercept = x$study_parameters$true_effect_size, 
                         linetype = "dashed", color = "red", size = 1) +
      ggplot2::geom_vline(xintercept = x$mean_effect_estimate, 
                         linetype = "solid", color = "blue", size = 1) +
      ggplot2::labs(
        title = "Distribution of Treatment Effect Estimates",
        subtitle = paste("Sample sizes: Control =", x$study_parameters$n_control, 
                        ", Treatment =", x$study_parameters$n_treatment),
        x = "Treatment Effect Estimate",
        y = "Frequency",
        caption = "Red line: True effect size, Blue line: Mean estimate"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 10)
      )
    
  } else {
    stop("Unknown plot type. Use 'power_curve', 'sample_size', or 'effect_distribution'.")
  }
  
  return(p)
}

#' Print method for rctbayespower objects
#' @param x An rctbayespower object
#' @param ... Additional arguments (unused)
#' @export
print.rctbayespower <- function(x, ...) {
  cat("Bayesian RCT Power Analysis Results\n")
  cat("===================================\n\n")
  
  cat("Study Parameters:\n")
  cat("  Sample size (control):", x$study_parameters$n_control, "\n")
  cat("  Sample size (treatment):", x$study_parameters$n_treatment, "\n")
  cat("  True effect size:", x$study_parameters$true_effect_size, "\n")
  cat("  Outcome type:", x$study_parameters$outcome_type, "\n")
  cat("  ROPE limits:", paste(x$study_parameters$rope_limits, collapse = " to "), "\n")
  cat("  Probability threshold:", x$study_parameters$prob_threshold, "\n\n")
  
  cat("Simulation Results:\n")
  cat("  Total simulations:", x$n_simulations, "\n")
  cat("  Successful fits:", x$successful_fits, "\n")
  cat("  Convergence rate:", round(x$convergence_rate, 3), "\n\n")
  
  cat("Power Estimates:\n")
  cat("  Power (ROPE):", round(x$power_rope, 3), "\n")
  cat("  Power (Direction):", round(x$power_direction, 3), "\n")
  cat("  Power (Significant):", round(x$power_significant, 3), "\n\n")
  
  cat("Effect Size Estimates:\n")
  cat("  Mean estimate:", round(x$mean_effect_estimate, 3), "\n")
  cat("  SD of estimates:", round(x$sd_effect_estimate, 3), "\n")
}

#' Print method for sample size analysis objects
#' @param x An rctbayespower_samplesize object
#' @param ... Additional arguments (unused)
#' @export
print.rctbayespower_samplesize <- function(x, ...) {
  cat("Bayesian RCT Sample Size Analysis\n")
  cat("=================================\n\n")
  
  cat("Analysis Parameters:\n")
  cat("  Effect size:", x$effect_size, "\n")
  cat("  Target power:", x$target_power, "\n")
  cat("  Outcome type:", x$outcome_type, "\n")
  cat("  Allocation ratio:", x$allocation_ratio, "\n\n")
  
  cat("Sample Size Recommendations:\n")
  if (!is.na(x$min_n_rope)) {
    cat("  Minimum sample size (ROPE):", x$min_n_rope, "per group\n")
  } else {
    cat("  Minimum sample size (ROPE): Not achieved with tested sizes\n")
  }
  
  if (!is.na(x$min_n_direction)) {
    cat("  Minimum sample size (Direction):", x$min_n_direction, "per group\n")
  } else {
    cat("  Minimum sample size (Direction): Not achieved with tested sizes\n")
  }
  
  cat("\nPower Curve Summary:\n")
  print(x$power_curve)
}

#' Print method for power curve objects  
#' @param x An rctbayespower_curve object
#' @param ... Additional arguments (unused)
#' @export
print.rctbayespower_curve <- function(x, ...) {
  cat("Bayesian RCT Power Curve Analysis\n")
  cat("=================================\n\n")
  
  cat("Study Parameters:\n")
  cat("  Sample size (control):", x$n_control, "\n")
  cat("  Sample size (treatment):", x$n_treatment, "\n")
  cat("  Outcome type:", x$outcome_type, "\n\n")
  
  cat("Power Curve Summary:\n")
  print(x$power_curve)
}
