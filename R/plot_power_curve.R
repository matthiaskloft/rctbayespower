# =============================================================================
# POWER CURVE PLOT
# =============================================================================
# Power curve visualization for single-dimension or faceted analysis.

#' Create Power Curve Plot
#'
#' Internal function to create power curve visualizations using ggplot2.
#'
#' @param plot_data Data frame with power analysis results
#' @param design Design object with p_sig_scs and p_sig_ftl
#' @param analysis_type One of "sample_only", "effect_only", or "both"
#' @param effect_col Column name for effect size (from condition_values)
#' @param metric Filter: "success", "futility", or "both"
#' @param values Filter: "power", "post_prob", or "both"
#' @param show_target Whether to show target power lines
#' @param show_mcse Whether to show MCSE uncertainty ribbons
#' @param facet_by Faceting: "effect_size", "sample_size", or "look"
#' @param has_looks Whether data has multiple interim looks
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot2 object
#' @keywords internal
#' @importFrom rlang .data
create_power_curve_plot <- function(plot_data,
                                    design,
                                    analysis_type,
                                    effect_col,
                                    metric,
                                    values,
                                    show_target,
                                    show_mcse,
                                    facet_by,
                                    has_looks,
                                    ...) {
  # Determine x variable, label, and facet variable
  if (analysis_type == "sample_only") {
    x_var <- "n_total"
    x_label <- "Total Sample Size"
    title_base <- "Sample Size Analysis"
    facet_var <- NULL
  } else if (analysis_type == "effect_only") {
    x_var <- effect_col
    x_label <- paste("Effect Size (", effect_col, ")", sep = "")
    title_base <- "Effect Size Analysis"
    facet_var <- NULL
  } else if (analysis_type == "both") {
    if (facet_by == "look" && has_looks) {
      x_var <- "n_total"
      x_label <- "Total Sample Size"
      facet_var <- "id_look"
      title_base <- "Power by Interim Analysis"
    } else if (facet_by == "effect_size") {
      x_var <- "n_total"
      x_label <- "Total Sample Size"
      facet_var <- effect_col
      title_base <- "Power Curves by Effect Size"
    } else {
      x_var <- effect_col
      x_label <- paste("Effect Size (", effect_col, ")", sep = "")
      facet_var <- "n_total"
      title_base <- "Power Curves by Sample Size"
    }
  } else {
    cli::cli_abort(c(
      "Unknown analysis type",
      "x" = "Analysis type: {.val {analysis_type}}",
      "i" = "This is an internal error - please report"
    ))
  }

  # For sequential with facet_by = "look", also facet by look
  if (facet_by == "look" && has_looks && analysis_type != "both") {
    facet_var <- "id_look"
    title_base <- paste(title_base, "by Interim Analysis")
  }

  # Pivot data to long format (with SE if show_mcse)
  plot_data_long <- pivot_plot_data_long(plot_data, metric, values, include_se = show_mcse)

  # Determine y-axis label
  y_label <- switch(
    values,
    "power" = "Power",
    "post_prob" = "Posterior Probability",
    "both" = "Power / Posterior Probability"
  )

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
  )

  # Add MCSE ribbons if requested
  if (show_mcse && "se" %in% names(plot_data_long)) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = pmax(0, .data$value - 1.96 * .data$se),
          ymax = pmin(1, .data$value + 1.96 * .data$se),
          fill = .data$outcome
        ),
        alpha = 0.2,
        linetype = 0,
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(values = rctbp_colors())
  }

  p <- p +
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
      y = y_label,
      title = paste("Bayesian RCT", title_base),
      color = "Outcome",
      linetype = "Measure",
      shape = "Measure"
    ) +
    rctbp_theme()

  # Add target lines if requested
  if (show_target && (values == "power" || values == "both")) {
    target_data <- data.frame(
      outcome = c("Success", "Futility"),
      yintercept = c(design@p_sig_scs, design@p_sig_ftl)
    )

    # Filter targets based on metric
    if (metric == "success") {
      target_data <- dplyr::filter(target_data, .data$outcome == "Success")
    } else if (metric == "futility") {
      target_data <- dplyr::filter(target_data, .data$outcome == "Futility")
    }

    p <- p +
      ggplot2::geom_hline(
        data = target_data,
        ggplot2::aes(yintercept = .data$yintercept, color = .data$outcome),
        linetype = "dashed",
        linewidth = 0.5,
        alpha = 0.6,
        show.legend = FALSE
      )
  }

  # Add faceting if needed
  if (!is.null(facet_var)) {
    # Create a labeled version of the facet variable
    if (facet_var == "n_total") {
      facet_label_prefix <- "N"
    } else if (facet_var == "id_look") {
      facet_label_prefix <- "Look"
    } else {
      facet_label_prefix <- "Effect"
    }

    plot_data_long <- plot_data_long |>
      dplyr::mutate(
        facet_label = paste0(facet_label_prefix, " = ", .data[[facet_var]])
      )
    p <- p %+% plot_data_long +
      ggplot2::facet_wrap(~ facet_label)
  }

  p
}
