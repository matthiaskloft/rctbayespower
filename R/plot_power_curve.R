# =============================================================================
# POWER CURVE PLOT
# =============================================================================
# Power curve visualization for single-dimension or faceted analysis.

#' Create Power Curve Plot
#'
#' Internal function to create power curve visualizations using ggplot2.
#'
#' @param plot_data Data frame with power analysis results
#' @param design Design object
#' @param conditions Conditions object with threshold values (thr_dec_eff, thr_dec_fut)
#' @param analysis_type One of "sample_only", "effect_only", or "both"
#' @param effect_col Column name for effect size (from condition_values)
#' @param metric Filter: "power", "prob", or "both"
#' @param decision Filter: "success", "futility", or "both"
#' @param show_target Whether to show target power lines
#' @param show_mcse Whether to show MCSE uncertainty ribbons
#' @param facet_by Faceting variable(s). Single value or vector of two for
#'   2D grid faceting. Options: "metric", "decision", "effect_size", "sample_size"
#' @param has_looks Whether data has multiple interim looks
#' @param group_by Variable to use for line coloring: "decision", "metric",
#'   "effect_size", or "sample_size"
#' @param target_power Optional numeric value (0-1) for target power line.
#'   If NULL, uses thresholds from conditions when group_by = "decision".
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot2 object
#' @keywords internal
#' @importFrom rlang .data
create_power_curve_plot <- function(plot_data,
                                    design,
                                    conditions,
                                    analysis_type,
                                    effect_col,
                                    metric,
                                    decision,
                                    show_target,
                                    show_mcse,
                                    facet_by,
                                    has_looks,
                                    group_by = "effect_size",
                                    target_power = NULL,
                                    ...) {
  # =============================================================================
  # DETERMINE PLOT STRUCTURE (x-axis, faceting, title)
  # =============================================================================
  # Default x-axis is sample size
  x_var <- "n_total"
  x_label <- "Total Sample Size"
  title_base <- "Power Analysis"


  # Helper to resolve facet_by name to internal column name
 resolve_facet_var <- function(fb) {
    switch(
      fb,
      "decision" = "outcome",
      "metric" = "measure",
      "effect_size" = effect_col,
      "sample_size" = "n_total",
      NULL
    )
  }

  # Resolve facet variables (supports 1 or 2 elements)
  facet_vars <- vapply(facet_by, resolve_facet_var, character(1))
  use_facet_grid <- length(facet_vars) == 2

  # Handle x-axis adjustment for sample_size faceting
  if ("sample_size" %in% facet_by) {
    x_var <- if (!is.null(effect_col)) effect_col else "n_total"
    x_label <- if (!is.null(effect_col)) paste("Effect Size (", effect_col, ")") else "Total Sample Size"
  }

  # Adjust title based on faceting
  if (use_facet_grid) {
    title_base <- paste("Power by", paste(facet_by, collapse = " & "))
  } else if (facet_by[1] == "decision") {
    title_base <- "Power by Decision"
  } else if (facet_by[1] == "metric") {
    title_base <- "Power by Metric"
  } else if (facet_by[1] == "effect_size") {
    title_base <- "Power by Effect Size"
  } else if (facet_by[1] == "sample_size") {
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

  # Helper to create facet label column for a given facet variable
 create_facet_label <- function(data, fvar, effect_col_name) {
    if (fvar == "outcome") {
      factor(data$outcome, levels = c("Success", "Futility"))
    } else if (fvar == "measure") {
      factor(data$measure, levels = c("Power", "Probability"))
    } else if (fvar == "n_total") {
      factor(
        paste0("N = ", data$n_total),
        levels = paste0("N = ", sort(unique(data$n_total)))
      )
    } else {
      # Effect size column
      factor(
        paste0(fvar, " = ", data[[fvar]]),
        levels = paste0(fvar, " = ", sort(unique(data[[fvar]])))
      )
    }
  }

  # Create facet label columns
  if (use_facet_grid) {
    # Two facet variables: create row and column labels
    plot_data_long$facet_row <- create_facet_label(plot_data_long, facet_vars[1], effect_col)
    plot_data_long$facet_col <- create_facet_label(plot_data_long, facet_vars[2], effect_col)
  } else if (length(facet_vars) == 1 && !is.na(facet_vars[1])) {
    # Single facet variable
    plot_data_long$facet_label <- create_facet_label(plot_data_long, facet_vars[1], effect_col)
  }

  # Build group interaction for proper line connectivity
  # Must include: outcome, measure, and any facet/color variables to avoid cross-connections
  group_vars <- c("outcome", "measure")
  # Add all facet variables that exist in the data
  for (fv in facet_vars) {
    if (!is.na(fv) && fv %in% names(plot_data_long)) {
      group_vars <- c(group_vars, fv)
    }
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
  ) +
    # Boundary lines at 0% and 100% (slightly distinct from grid)
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = 1, color = "gray50", linewidth = 0.4)

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

  # Add target lines if requested

  # Case 1: User specified explicit target_power - show single gray target line
  if (show_target && !is.null(target_power) && (metric == "power" || metric == "both")) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = target_power,
        color = "gray40",
        linetype = "dashed",
        linewidth = 0.7,
        alpha = 0.8
      )
  }

  # Case 2: No explicit target_power and group_by == "decision" - show thresholds from conditions
  # Note: Skip target lines when boundary functions are used (thresholds vary by look)
  if (show_target && is.null(target_power) && (metric == "power" || metric == "both") && group_by == "decision") {
    # Get threshold values from conditions
    thr_dec_eff <- get_original_threshold(conditions, "thr_dec_eff")
    thr_dec_fut <- get_original_threshold(conditions, "thr_dec_fut")

    # Only add target lines for numeric thresholds, not boundary functions
    p_eff_numeric <- is.numeric(thr_dec_eff) && length(thr_dec_eff) == 1
    p_fut_numeric <- is.numeric(thr_dec_fut) && length(thr_dec_fut) == 1

    if (p_eff_numeric || p_fut_numeric) {
      outcomes <- character()
      yintercepts <- numeric()

      if (p_eff_numeric && (decision == "success" || decision == "both")) {
        outcomes <- c(outcomes, "Efficacy")
        yintercepts <- c(yintercepts, thr_dec_eff)
      }
      if (p_fut_numeric && (decision == "futility" || decision == "both")) {
        outcomes <- c(outcomes, "Futility")
        yintercepts <- c(yintercepts, thr_dec_fut)
      }

      if (length(outcomes) > 0) {
        target_data <- data.frame(
          outcome = outcomes,
          yintercept = yintercepts,
          stringsAsFactors = FALSE
        )
        target_data$line_color <- ifelse(
          target_data$outcome == "Efficacy",
          rctbp_colors()["Efficacy"],
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
  if (use_facet_grid) {
    # 2D faceting with facet_grid (rows ~ cols)
    p <- p + ggplot2::facet_grid(facet_row ~ facet_col)
  } else if (length(facet_vars) == 1 && !is.na(facet_vars[1])) {
    # 1D faceting with facet_wrap
    p <- p + ggplot2::facet_wrap(~ facet_label)
  }

  p
}
