# =============================================================================
# HEATMAP PLOT
# =============================================================================
# 2D heatmap visualization when both sample size and effect size vary.

#' Create Heatmap Plot
#'
#' Internal function to create heatmap visualizations using ggplot2.
#'
#' @param plot_data Data frame with power analysis results
#' @param design Design object
#' @param conditions Conditions object with threshold values (thr_dec_eff, thr_dec_fut)
#' @param analysis_type Must be "both" for heatmap
#' @param effect_col Column name for effect size (from condition_values)
#' @param metric Filter: "power", "prob", or "both"
#' @param decision Filter: "success", "futility", or "both"
#' @param show_target Whether to show target power contour lines
#' @param target_power Target power level for contour (default from conditions)
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot2 object
#' @keywords internal
#' @importFrom rlang .data
create_heatmap_plot <- function(plot_data,
                                design,
                                conditions,
                                analysis_type,
                                effect_col,
                                metric,
                                decision,
                                show_target,
                                target_power,
                                ...) {
  if (analysis_type != "both") {
    cli::cli_abort(c(
      "Heatmap plot requires both sample sizes and effect sizes to vary",
      "x" = "Your analysis only varies one dimension",
      "i" = "Use {.code type = 'power_curve'} instead"
    ))
  }

  # Check that effect_col exists
  if (is.null(effect_col) || !effect_col %in% names(plot_data)) {
    cli::cli_abort(c(
      "Effect size column not found in data",
      "x" = "Expected column {.val {effect_col}} not found",
      "i" = "Check that effect size was included in condition_values"
    ))
  }

  # Check for sufficient data points
  if (nrow(plot_data) < 4) {
    cli::cli_abort(c(
      "Heatmap requires at least 4 data points (2x2 grid)",
      "x" = "You have {.val {nrow(plot_data)}} data points",
      "i" = "Use {.code type = 'power_curve'} instead"
    ))
  }

  # Pivot data to long format
  plot_data_long <- pivot_plot_data_long(plot_data, decision, metric)

  # Remove rows with NA values
  plot_data_long <- dplyr::filter(plot_data_long, !is.na(.data$value))

  if (nrow(plot_data_long) == 0) {
    cli::cli_abort(c(
      "No valid data points for heatmap",
      "x" = "All power analysis results contain NA values",
      "i" = "Check your analysis for errors or convergence issues"
    ))
  }

  # Set default target power from conditions if not specified
  if (is.null(target_power)) {
    target_power <- get_original_threshold(conditions, "thr_dec_eff")
    if (is.null(target_power)) target_power <- 0.8  # Fallback default
  }

  # Build ggplot heatmap
  p <- ggplot2::ggplot(
    plot_data_long,
    ggplot2::aes(
      x = .data[[effect_col]],
      y = .data$n_total,
      fill = .data$value
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      low = "lightblue",
      high = "darkblue",
      labels = scales::percent_format(),
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      x = paste("Effect Size (", effect_col, ")", sep = ""),
      y = "Total Sample Size",
      fill = "Value",
      title = "Power Analysis Heatmap",
      subtitle = paste(
        "Targets - Efficacy:",
        get_threshold_display(conditions, "thr_dec_eff"),
        "| Futility:",
        get_threshold_display(conditions, "thr_dec_fut")
      )
    ) +
    rctbp_theme() +
    ggplot2::theme(legend.position = "right")

  # Add contour line at target power level (for power facets only)
  if (show_target && (metric == "power" || metric == "both")) {
    power_data <- dplyr::filter(plot_data_long, .data$measure == "Power")
    if (nrow(power_data) > 0) {
      p <- p +
        ggplot2::geom_contour(
          data = power_data,
          ggplot2::aes(z = .data$value),
          breaks = target_power,
          color = "white",
          linewidth = 1,
          linetype = "dashed"
        )
    }
  }

  # Determine faceting based on what's being shown
  n_outcomes <- length(unique(plot_data_long$outcome))
  n_measures <- length(unique(plot_data_long$measure))

  if (n_outcomes > 1 && n_measures > 1) {
    p <- p + ggplot2::facet_grid(
      rows = ggplot2::vars(.data$outcome),
      cols = ggplot2::vars(.data$measure)
    )
  } else if (n_outcomes > 1) {
    p <- p + ggplot2::facet_wrap(~ outcome)
  } else if (n_measures > 1) {
    p <- p + ggplot2::facet_wrap(~ measure)
  }

  p
}
