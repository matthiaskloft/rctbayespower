# =============================================================================
# COMPARISON PLOT
# =============================================================================
# Compare power vs posterior probability visualization.

#' Create Comparison Plot
#'
#' Internal function to create power vs posterior probability comparison plots.
#'
#' @param plot_data Data frame with power analysis results
#' @param design Design object with p_sig_scs and p_sig_ftl
#' @param analysis_type One of "sample_only" or "effect_only"
#' @param effect_col Column name for effect size (from condition_values)
#' @param metric Ignored (always shows both power and probability)
#' @param decision Filter: "success", "futility", or "both"
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot2 object
#' @keywords internal
#' @importFrom rlang .data
create_comparison_plot <- function(plot_data,
                                   design,
                                   analysis_type,
                                   effect_col,
                                   metric,
                                   decision,
                                   ...) {
  if (analysis_type == "both") {
    cli::cli_abort(c(
      "Comparison plot requires single varying dimension",
      "x" = "Your analysis varies multiple dimensions",
      "i" = "Use {.code type = 'heatmap'} for multi-dimensional analysis"
    ))
  }

  # Determine x variable and label
  if (analysis_type == "sample_only") {
    x_var <- "n_total"
    x_label <- "Total Sample Size"
    title_base <- "Power vs Posterior Probability (Sample Size Analysis)"
  } else if (analysis_type == "effect_only") {
    x_var <- effect_col
    x_label <- paste("Effect Size (", effect_col, ")", sep = "")
    title_base <- "Power vs Posterior Probability (Effect Size Analysis)"
  } else {
    cli::cli_abort(c(
      "Unknown analysis type for comparison plot",
      "x" = "Analysis type: {.val {analysis_type}}"
    ))
  }

  # Force metric to "both" for comparison plot (always shows both power and probability)
  plot_data_long <- pivot_plot_data_long(plot_data, decision, metric = "both")

  # Build ggplot
  p <- ggplot2::ggplot(
    plot_data_long,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data$value,
      color = .data$outcome,
      linetype = .data$measure,
      shape = .data$measure,
      group = interaction(.data$outcome, .data$measure)
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, 1)
    ) +
    ggplot2::scale_color_manual(values = rctbp_colors()) +
    ggplot2::scale_linetype_manual(
      values = c("Power" = "solid", "Probability" = "dashed")
    ) +
    ggplot2::scale_shape_manual(
      values = c("Power" = 16, "Probability" = 18)
    ) +
    ggplot2::labs(
      x = x_label,
      y = "Value",
      title = title_base,
      subtitle = "Solid: Power | Dashed: Posterior Probability",
      color = "Outcome",
      linetype = "Measure",
      shape = "Measure"
    ) +
    rctbp_theme()

  p
}
