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
#' @param metric Filter: "power", "prob", or "both"
#' @param decision Filter: "success", "futility", or "both"
#' @param show_target Whether to show target power lines
#' @param show_mcse Whether to show MCSE uncertainty ribbons
#' @param facet_by Faceting: "metric", "decision", "effect_size", or "sample_size"
#' @param has_looks Whether data has multiple interim looks
#' @param group_by Variable to use for line coloring: "decision", "metric",
#'   "effect_size", or "sample_size"
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
                                    decision,
                                    show_target,
                                    show_mcse,
                                    facet_by,
                                    has_looks,
                                    group_by = "effect_size",
                                    ...) {
  # =============================================================================
  # DETERMINE PLOT STRUCTURE (x-axis, faceting, title)
  # =============================================================================
  # Default x-axis is sample size
  x_var <- "n_total"
  x_label <- "Total Sample Size"
  facet_var <- NULL
  title_base <- "Power Analysis"

  # Handle facet_by options
  # "decision" -> facet by outcome (Success/Futility)
  # "metric" -> facet by measure (Power/Probability)
  if (facet_by == "decision") {
    facet_var <- "outcome"
    title_base <- "Power by Decision"
  } else if (facet_by == "metric") {
    facet_var <- "measure"
    title_base <- "Power by Metric"
  } else if (facet_by == "effect_size" && !is.null(effect_col)) {
    facet_var <- effect_col
    title_base <- "Power by Effect Size"
  } else if (facet_by == "sample_size") {
    x_var <- if (!is.null(effect_col)) effect_col else "n_total"
    x_label <- if (!is.null(effect_col)) paste("Effect Size (", effect_col, ")") else "Total Sample Size"
    facet_var <- "n_total"
    title_base <- "Power by Sample Size"
  }

  # Adjust title based on analysis type
  if (analysis_type == "sample_only") {
    title_base <- paste(title_base, "- Sample Size Analysis")
  } else if (analysis_type == "effect_only") {
    x_var <- effect_col
    x_label <- paste("Effect Size (", effect_col, ")")
    title_base <- paste(title_base, "- Effect Size Analysis")
  }

  # Pivot data to long format (with SE if show_mcse)
  # Note: pivot_plot_data_long expects (decision, metric) in new naming
  plot_data_long <- pivot_plot_data_long(plot_data, decision, metric, include_se = show_mcse)

  # Determine y-axis label
  y_label <- switch(
    metric,
    "power" = "Power",
    "prob" = "Posterior Probability",
    "both" = "Power / Posterior Probability"
  )

  # =============================================================================
  # PREPARE GROUPING AND COLORING

  # =============================================================================
  # Resolve group_by to actual column name
  # API names map to internal column names:
  # - "decision" -> "outcome" (Success/Futility)
  # - "metric" -> "measure" (Power/Probability)
  color_var <- switch(
    group_by,
    "decision" = "outcome",
    "metric" = "measure",
    "effect_size" = effect_col,
    "sample_size" = "n_total",
    "outcome"  # default fallback
  )

  # Check if color variable exists in data
  if (!color_var %in% names(plot_data_long)) {
    cli::cli_warn(c(
      "Cannot group by {.val {group_by}} - column not found in data",
      "i" = "Falling back to grouping by decision"
    ))
    color_var <- "outcome"
    group_by <- "decision"
  }

  # Create color group column (factor for proper legend ordering)
  if (color_var == "outcome") {
    plot_data_long$color_group <- factor(
      plot_data_long$outcome,
      levels = c("Success", "Futility")
    )
    color_label <- "Decision"
  } else if (color_var == "measure") {
    plot_data_long$color_group <- factor(
      plot_data_long$measure,
      levels = c("Power", "Probability")
    )
    color_label <- "Metric"
  } else if (color_var == "n_total") {
    plot_data_long$color_group <- factor(
      paste0("N=", plot_data_long$n_total),
      levels = paste0("N=", sort(unique(plot_data_long$n_total)))
    )
    color_label <- "Sample Size"
  } else {
    # Effect size column
    plot_data_long$color_group <- factor(
      paste0(effect_col, "=", plot_data_long[[color_var]]),
      levels = paste0(effect_col, "=", sort(unique(plot_data_long[[color_var]])))
    )
    color_label <- "Effect Size"
  }

  # Create facet label if faceting
  if (!is.null(facet_var)) {
    if (facet_var == "outcome") {
      # Facet by Success/Futility - use outcome directly
      plot_data_long$facet_label <- factor(
        plot_data_long$outcome,
        levels = c("Success", "Futility")
      )
    } else if (facet_var == "measure") {
      # Facet by Power/Probability - use measure directly
      plot_data_long$facet_label <- factor(
        plot_data_long$measure,
        levels = c("Power", "Probability")
      )
    } else if (facet_var == "n_total") {
      plot_data_long$facet_label <- factor(
        paste0("N = ", plot_data_long$n_total),
        levels = paste0("N = ", sort(unique(plot_data_long$n_total)))
      )
    } else {
      # Effect size column
      plot_data_long$facet_label <- factor(
        paste0(facet_var, " = ", plot_data_long[[facet_var]]),
        levels = paste0(facet_var, " = ", sort(unique(plot_data_long[[facet_var]])))
      )
    }
  }

  # Build group interaction for proper line connectivity
  # Must include: outcome, measure, and any facet/color variables to avoid cross-connections
  group_vars <- c("outcome", "measure")
  if (!is.null(facet_var) && facet_var %in% names(plot_data_long)) {
    group_vars <- c(group_vars, facet_var)
  }
  if (color_var != "outcome" && color_var %in% names(plot_data_long)) {
    group_vars <- c(group_vars, color_var)
  }

  # Create group column
  if (length(group_vars) == 2) {
    plot_data_long$line_group <- interaction(
      plot_data_long[[group_vars[1]]],
      plot_data_long[[group_vars[2]]]
    )
  } else {
    plot_data_long$line_group <- interaction(
      plot_data_long[, group_vars, drop = FALSE]
    )
  }

  # =============================================================================
  # BUILD GGPLOT
  # =============================================================================
  p <- ggplot2::ggplot(
    plot_data_long,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data$value,
      color = .data$color_group,
      linetype = .data$measure,
      shape = .data$measure,
      group = .data$line_group
    )
  )

  # Add MCSE ribbons if requested
  if (show_mcse && "se" %in% names(plot_data_long)) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = pmax(0, .data$value - 1.96 * .data$se),
          ymax = pmin(1, .data$value + 1.96 * .data$se),
          fill = .data$color_group
        ),
        alpha = 0.2,
        linetype = 0,
        show.legend = FALSE
      )
  }

  p <- p +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, 1)
    ) +
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
      color = color_label,
      linetype = "Measure",
      shape = "Measure"
    ) +
    rctbp_theme()

  # Add color scale based on group_by
  if (group_by == "outcome") {
    p <- p + ggplot2::scale_color_manual(values = rctbp_colors())
    if (show_mcse && "se" %in% names(plot_data_long)) {
      p <- p + ggplot2::scale_fill_manual(values = rctbp_colors())
    }
  } else {
    # Use a color palette for non-outcome groupings
    n_groups <- length(unique(plot_data_long$color_group))
    if (n_groups <= 8) {
      p <- p + ggplot2::scale_color_brewer(palette = "Set1")
      if (show_mcse && "se" %in% names(plot_data_long)) {
        p <- p + ggplot2::scale_fill_brewer(palette = "Set1")
      }
    } else {
      p <- p + ggplot2::scale_color_viridis_d()
      if (show_mcse && "se" %in% names(plot_data_long)) {
        p <- p + ggplot2::scale_fill_viridis_d()
      }
    }
  }

  # Add target lines if requested (only for decision-based coloring)
  # Note: Skip target lines when boundary functions are used (thresholds vary by look)
  if (show_target && (metric == "power" || metric == "both") && group_by == "decision") {
    # Only add target lines for numeric thresholds, not boundary functions
    p_scs_numeric <- is.numeric(design@p_sig_scs)
    p_ftl_numeric <- is.numeric(design@p_sig_ftl)

    if (p_scs_numeric || p_ftl_numeric) {
      outcomes <- character()
      yintercepts <- numeric()

      if (p_scs_numeric && (decision == "success" || decision == "both")) {
        outcomes <- c(outcomes, "Success")
        yintercepts <- c(yintercepts, design@p_sig_scs)
      }
      if (p_ftl_numeric && (decision == "futility" || decision == "both")) {
        outcomes <- c(outcomes, "Futility")
        yintercepts <- c(yintercepts, design@p_sig_ftl)
      }

      if (length(outcomes) > 0) {
        target_data <- data.frame(
          outcome = outcomes,
          yintercept = yintercepts,
          stringsAsFactors = FALSE
        )
        target_data$line_color <- ifelse(
          target_data$outcome == "Success",
          rctbp_colors()["Success"],
          rctbp_colors()["Futility"]
        )

        # Add each target line separately to get correct colors
        for (i in seq_len(nrow(target_data))) {
          p <- p +
            ggplot2::geom_hline(
              yintercept = target_data$yintercept[i],
              color = target_data$line_color[i],
              linetype = "dashed",
              linewidth = 0.5,
              alpha = 0.6
            )
        }
      }
    }
  }

  # Add faceting if needed
  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(~ facet_label)
  }

  p
}
