#' Plot Power Grid Analysis Results
#'
#' Create comprehensive visualizations for power grid analysis results from power_grid_analysis().
#' Supports different plot types based on analysis type (sample_only, effect_only, or both varying).
#'
#' @param x An object of class 'rctbayespower_grid' returned by power_grid_analysis()
#' @param type Type of plot to create:
#'   \itemize{
#'     \item "auto" - Automatically detect best plot type based on analysis (default)
#'     \item "power_curve" - Power curve across single varying dimension
#'     \item "heatmap" - 2D heatmap when both sample sizes and effect sizes vary
#'     \item "integrated" - Integrated power results when design prior is used
#'     \item "comparison" - Compare power vs posterior probabilities
#'   }
#' @param metric Which power metric to display:
#'   \itemize{
#'     \item "success" - Success power and probability
#'     \item "futility" - Futility power and probability
#'     \item "both" - Both success and futility power and probabilities (default)
#'   }
#' @param values Which values to display:
#'   \itemize{
#'     \item "both" - Both power and posterior probabilities (default)
#'     \item "power" - Power only
#'     \item "post_prob" - Posterior probabilities only
#'   }
#' @param show_target Whether to show target power lines (default: TRUE)
#' @param show_integrated Whether to include integrated power when available (default: TRUE)
#' @param facet_by For power_curve plots when both sample sizes and effect sizes vary:
#'   \itemize{
#'     \item "effect_size" - Facet by effect size, vary sample size on x-axis (default)
#'     \item "sample_size" - Facet by sample size, vary effect size on x-axis
#'   }
#' @param ... Additional arguments passed to ggplot2 functions
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' # Sample size analysis plot
#' grid_result <- power_grid_analysis(
#'   sample_sizes = seq(20, 100, 20),
#'   effect_sizes = 0.5,
#'   threshold_success = 0.2,
#'   threshold_futility = 0,
#'   power_analysis_fn = "power_analysis_ancova",
#'   outcome_type = "continuous",
#'   baseline_effect = 0.2
#' )
#' plot(grid_result) # Auto-detects power curve
#'
#' # Effect size analysis plot
#' grid_result <- power_grid_analysis(
#'   sample_sizes = 60,
#'   effect_sizes = seq(0.2, 0.8, 0.1),
#'   threshold_success = 0.2,
#'   threshold_futility = 0,
#'   power_analysis_fn = "power_analysis_ancova",
#'   outcome_type = "continuous",
#'   baseline_effect = 0.2
#' )
#' plot(grid_result) # Shows both success and futility, both power and probabilities
#'
#' # Full grid heatmap
#' grid_result <- power_grid_analysis(
#'   sample_sizes = seq(20, 100, 20),
#'   effect_sizes = seq(0.2, 0.8, 0.2),
#'   threshold_success = 0.2,
#'   threshold_futility = 0,
#'   power_analysis_fn = "power_analysis_ancova",
#'   outcome_type = "continuous",
#'   baseline_effect = 0.2
#' )
#' plot(grid_result, type = "heatmap")
#'
#' # Power curves faceted by effect size (when both vary)
#' plot(grid_result, type = "power_curve", facet_by = "effect_size")
#'
#' # Power curves faceted by sample size (when both vary)
#' plot(grid_result, type = "power_curve", facet_by = "sample_size")
#'
#' # Integrated power plot with design prior
#' grid_result <- power_grid_analysis(
#'   sample_sizes = seq(20, 100, 20),
#'   effect_sizes = seq(0.2, 0.8, 0.1),
#'   design_prior = "normal(0.5, 0.15)",
#'   threshold_success = 0.2,
#'   threshold_futility = 0,
#'   power_analysis_fn = "power_analysis_ancova",
#'   outcome_type = "continuous",
#'   baseline_effect = 0.2
#' )
#' plot(grid_result, type = "integrated")
#' }
plot.rctbayespower_grid <- function(x,
                                    type = "auto",
                                    metric = "both",
                                    values = "both",
                                    show_target = TRUE,
                                    show_integrated = TRUE,
                                    facet_by = "effect_size",
                                    ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }

  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Package 'scales' is required for plotting.")
  }

  # Check for valid data
  if (is.null(x$power_surface) || nrow(x$power_surface) == 0) {
    stop("No power analysis results to plot. Check that power_grid_analysis() completed successfully.")
  }

  # Check for missing essential columns
  required_cols <- c(
    "n_total", "effect_size", "power_success", "power_futility",
    "mean_prob_success", "mean_prob_futility"
  )
  missing_cols <- setdiff(required_cols, names(x$power_surface))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in power_surface: ", paste(missing_cols, collapse = ", "))
  }

  # Auto-detect plot type based on analysis type
  if (type == "auto") {
    if (x$analysis_type == "sample_only") {
      type <- "power_curve"
    } else if (x$analysis_type == "effect_only") {
      type <- "power_curve"
    } else if (x$analysis_type == "both") {
      type <- "heatmap" # Default to heatmap, but power_curve is now also available
    } else {
      stop("Cannot auto-detect plot type for analysis type: ", x$analysis_type)
    }
  }

  # Validate parameters
  if (!metric %in% c("success", "futility", "both")) {
    stop("metric must be 'success', 'futility', or 'both'")
  }

  if (!values %in% c("power", "post_prob", "both")) {
    stop("values must be 'power', 'post_prob', or 'both'")
  }

  if (!facet_by %in% c("effect_size", "sample_size")) {
    stop("facet_by must be 'effect_size' or 'sample_size'")
  }

  # Create plot based on type
  if (type == "power_curve") {
    create_power_curve_plot(x, metric, values, show_target, show_integrated, facet_by, ...)
  } else if (type == "heatmap") {
    create_heatmap_plot(x, metric, values, show_target, ...)
  } else if (type == "integrated") {
    create_integrated_plot(x, metric, values, show_target, ...)
  } else if (type == "comparison") {
    create_comparison_plot(x, metric, values, ...)
  } else {
    stop("Unknown plot type: ", type, ". Use 'power_curve', 'heatmap', 'integrated', or 'comparison'.")
  }
}

#' Create Power Curve Plot
#' @noRd
create_power_curve_plot <- function(x, metric, values, show_target, show_integrated, facet_by, ...) {
  plot_data <- x$power_surface

  if (x$analysis_type == "sample_only") {
    x_var <- "n_total"
    x_label <- "Total Sample Size"
    title_base <- "Sample Size Analysis"
    subtitle <- paste("Fixed effect size:", x$effect_size)
    facet_var <- NULL
  } else if (x$analysis_type == "effect_only") {
    x_var <- "effect_size"
    x_label <- "Effect Size"
    title_base <- "Effect Size Analysis"
    subtitle <- paste("Fixed sample size:", x$sample_sizes[1])
    facet_var <- NULL
  } else if (x$analysis_type == "both") {
    # Handle faceting for both varying
    if (facet_by == "effect_size") {
      x_var <- "n_total"
      x_label <- "Total Sample Size"
      facet_var <- "effect_size"
      facet_label <- "Effect Size"
      title_base <- "Power Curves by Effect Size"
      subtitle <- paste("Sample sizes:", paste(range(x$sample_sizes), collapse = "-"))
    } else {
      x_var <- "effect_size"
      x_label <- "Effect Size"
      facet_var <- "n_total"
      facet_label <- "Sample Size"
      title_base <- "Power Curves by Sample Size"
      subtitle <- paste("Effect sizes:", paste(range(x$effect_sizes), collapse = "-"))
    }
  } else {
    stop("Unknown analysis type")
  }

  # Base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = x_var))

  # Success metrics
  if (metric == "success" || metric == "both") {
    # Add power if requested
    if (values == "power" || values == "both") {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(y = power_success, color = "Success Power"),
          size = 1.2, linetype = "solid"
        ) +
        ggplot2::geom_point(ggplot2::aes(y = power_success, color = "Success Power"),
          size = 3, shape = 16
        )

      if (show_target) {
        p <- p + ggplot2::geom_hline(
          yintercept = x$target_power_success,
          linetype = "dashed", color = "steelblue", alpha = 0.7
        )
      }
    }

    # Add posterior probability if requested
    if (values == "post_prob" || values == "both") {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(y = mean_prob_success, color = "Success Probability"),
          size = 1.2, linetype = "dotted"
        ) +
        ggplot2::geom_point(ggplot2::aes(y = mean_prob_success, color = "Success Probability"),
          size = 3, shape = 17
        )
    }
  }

  # Futility metrics
  if (metric == "futility" || metric == "both") {
    # Add power if requested
    if (values == "power" || values == "both") {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(y = power_futility, color = "Futility Power"),
          size = 1.2, linetype = "solid"
        ) +
        ggplot2::geom_point(ggplot2::aes(y = power_futility, color = "Futility Power"),
          size = 3, shape = 16
        )

      if (show_target) {
        p <- p + ggplot2::geom_hline(
          yintercept = x$target_power_futility,
          linetype = "dashed", color = "darkred", alpha = 0.7
        )
      }
    }

    # Add posterior probability if requested
    if (values == "post_prob" || values == "both") {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(y = mean_prob_futility, color = "Futility Probability"),
          size = 1.2, linetype = "dotted"
        ) +
        ggplot2::geom_point(ggplot2::aes(y = mean_prob_futility, color = "Futility Probability"),
          size = 3, shape = 17
        )
    }
  }

  # Add integrated power if available and requested
  # Integrated power is meaningful when we have multiple effect sizes and sample sizes on x-axis
  if (show_integrated && !is.null(x$integrated_power) && length(x$effect_sizes) > 1 && x_var == "n_total") {
    if (metric == "success" || metric == "both") {
      # Add horizontal reference lines for integrated power values
      for (i in 1:nrow(x$integrated_power)) {
        power_val <- x$integrated_power$integrated_power_success[i]
        p <- p +
          ggplot2::geom_hline(
            yintercept = power_val,
            color = "cyan",
            linetype = "longdash",
            size = 1.2,
            alpha = 0.7
          )
      }
    }

    if (metric == "futility" || metric == "both") {
      # Add horizontal reference lines for integrated power values
      for (i in 1:nrow(x$integrated_power)) {
        power_val <- x$integrated_power$integrated_power_futility[i]
        p <- p +
          ggplot2::geom_hline(
            yintercept = power_val,
            color = "magenta",
            linetype = "longdash",
            size = 1.2,
            alpha = 0.7
          )
      }
    }
  }

  # Color scheme
  colors <- c(
    "Success Power" = "steelblue", "Success Probability" = "lightblue",
    "Futility Power" = "darkred", "Futility Probability" = "pink",
    "Integrated Success" = "cyan", "Integrated Futility" = "magenta"
  )

  # Dynamic y-axis label based on values parameter
  y_label <- switch(values,
    "power" = "Power",
    "post_prob" = "Posterior Probability",
    "both" = "Power / Posterior Probability"
  )

  # Dynamic caption
  caption_parts <- c()
  if (show_target) caption_parts <- c(caption_parts, "Gray dashed lines: target power levels")
  if (values == "both") caption_parts <- c(caption_parts, "Solid lines: power, Dotted lines: posterior probability")
  if (show_integrated && !is.null(x$integrated_power) && length(x$effect_sizes) > 1 && x_var == "n_total") {
    caption_parts <- c(caption_parts, "Colored horizontal lines: integrated power (weighted by design prior)")
  }
  caption_text <- if (length(caption_parts) > 0) paste(caption_parts, collapse = "; ") else NULL

  # Add faceting if needed
  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_var)),
      labeller = ggplot2::labeller(.default = function(x) paste(facet_label, "=", x))
    )
  }

  # Set x-axis to use discrete values from study design
  if (x_var == "n_total") {
    p <- p + ggplot2::scale_x_continuous(breaks = x$sample_sizes, labels = x$sample_sizes)
  } else if (x_var == "effect_size") {
    p <- p + ggplot2::scale_x_continuous(breaks = x$effect_sizes, labels = x$effect_sizes)
  }

  p <- p +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    ggplot2::labs(
      title = paste("Bayesian RCT", title_base),
      subtitle = subtitle,
      x = x_label,
      y = y_label,
      color = "Metric",
      caption = caption_text
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )

  return(p)
}

#' Create Heatmap Plot
#' @noRd
create_heatmap_plot <- function(x, metric, values, show_target, ...) {
  if (x$analysis_type != "both") {
    stop("Heatmap plot requires both sample sizes and effect sizes to vary")
  }

  plot_data <- x$power_surface

  # Check for sufficient data points
  if (nrow(plot_data) < 4) {
    stop("Heatmap requires at least 4 data points (2x2 grid). Consider using type='power_curve' instead.")
  }

  # Remove rows with all NA values for key metrics
  valid_rows <- !(is.na(plot_data$power_success) & is.na(plot_data$power_futility) &
    is.na(plot_data$mean_prob_success) & is.na(plot_data$mean_prob_futility))
  plot_data <- plot_data[valid_rows, ]

  if (nrow(plot_data) == 0) {
    stop("No valid data points for heatmap. All power analysis results contain NA values.")
  }

  # Determine what to plot based on values parameter
  if (values == "power") {
    fill_var_success <- "power_success"
    fill_var_futility <- "power_futility"
    fill_name <- "Power"
    plot_suffix <- "Power"
  } else if (values == "post_prob") {
    fill_var_success <- "mean_prob_success"
    fill_var_futility <- "mean_prob_futility"
    fill_name <- "Posterior Probability"
    plot_suffix <- "Posterior Probability"
  } else {
    # For "both", default to power but add note in subtitle
    fill_var_success <- "power_success"
    fill_var_futility <- "power_futility"
    fill_name <- "Power"
    plot_suffix <- "Power"
  }

  if (metric == "success") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "effect_size", y = "n_total", fill = fill_var_success)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(name = fill_name, labels = scales::percent_format()) +
      ggplot2::labs(
        title = paste("Success", plot_suffix, "Heatmap"),
        subtitle = if (values == "both") paste("Showing power | Target power:", x$target_power_success) else paste("Target:", x$target_power_success),
        x = "Effect Size",
        y = "Total Sample Size"
      )
  } else if (metric == "futility") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "effect_size", y = "n_total", fill = fill_var_futility)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(name = fill_name, labels = scales::percent_format()) +
      ggplot2::labs(
        title = paste("Futility", plot_suffix, "Heatmap"),
        subtitle = if (values == "both") paste("Showing power | Target power:", x$target_power_futility) else paste("Target:", x$target_power_futility),
        x = "Effect Size",
        y = "Total Sample Size"
      )
  } else {
    # Both - create faceted plot
    if (values == "both") {
      # Create 4-panel plot: success power, success prob, futility power, futility prob
      plot_data_long <- rbind(
        data.frame(plot_data[, c("n_total", "effect_size")],
          value = plot_data$power_success,
          type = "Success Power"
        ),
        data.frame(plot_data[, c("n_total", "effect_size")],
          value = plot_data$mean_prob_success,
          type = "Success Probability"
        ),
        data.frame(plot_data[, c("n_total", "effect_size")],
          value = plot_data$power_futility,
          type = "Futility Power"
        ),
        data.frame(plot_data[, c("n_total", "effect_size")],
          value = plot_data$mean_prob_futility,
          type = "Futility Probability"
        )
      )
      fill_name <- "Value"
    } else {
      # Standard 2-panel plot
      plot_data_long <- rbind(
        data.frame(plot_data[, c("n_total", "effect_size")],
          value = plot_data[[fill_var_success]],
          type = paste("Success", plot_suffix)
        ),
        data.frame(plot_data[, c("n_total", "effect_size")],
          value = plot_data[[fill_var_futility]],
          type = paste("Futility", plot_suffix)
        )
      )
    }

    p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = effect_size, y = n_total, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::facet_wrap(~type) +
      ggplot2::scale_fill_viridis_c(name = fill_name, labels = scales::percent_format()) +
      ggplot2::labs(
        title = if (values == "both") "Power and Probability Analysis Heatmap" else paste(plot_suffix, "Analysis Heatmap"),
        subtitle = paste(
          "Target powers - Success:", x$target_power_success,
          ", Futility:", x$target_power_futility
        ),
        x = "Effect Size",
        y = "Total Sample Size"
      )
  }

  # Add contour lines at target power if requested - skip for now to avoid aesthetic warnings
  # Will be re-enabled once the core plotting works smoothly
  if (FALSE && show_target && values != "post_prob") {
    # Contour functionality temporarily disabled to prevent aesthetic warnings
    # The core heatmap functionality works fine without contours
  }

  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )

  return(p)
}

#' Create Integrated Power Plot
#' @noRd
create_integrated_plot <- function(x, metric, values, show_target, ...) {
  if (is.null(x$integrated_power)) {
    stop("Integrated power plot requires design prior to be specified in power_grid_analysis()")
  }

  plot_data <- x$integrated_power

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = n_total))

  if (metric == "success" || metric == "both") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = integrated_power_success, color = "Integrated Success Power"),
        size = 1.5, linetype = "solid"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = integrated_power_success, color = "Integrated Success Power"),
        size = 3, shape = 17
      )

    if (show_target) {
      p <- p + ggplot2::geom_hline(
        yintercept = x$target_power_success,
        linetype = "dashed", color = "steelblue", alpha = 0.7
      )
    }
  }

  if (metric == "futility" || metric == "both") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = integrated_power_futility, color = "Integrated Futility Power"),
        size = 1.5, linetype = "solid"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = integrated_power_futility, color = "Integrated Futility Power"),
        size = 3, shape = 17
      )

    if (show_target) {
      p <- p + ggplot2::geom_hline(
        yintercept = x$target_power_futility,
        linetype = "dashed", color = "darkred", alpha = 0.7
      )
    }
  }

  colors <- c("Integrated Success Power" = "lightblue", "Integrated Futility Power" = "pink")

  p <- p +
    ggplot2::scale_x_continuous(breaks = x$sample_sizes, labels = x$sample_sizes) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    ggplot2::labs(
      title = "Integrated Power Analysis",
      subtitle = paste("Design prior:", x$design_prior),
      x = "Total Sample Size",
      y = "Integrated Power",
      color = "Metric",
      caption = "Power weighted by design prior across effect sizes"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )

  return(p)
}

#' Create Comparison Plot (Power vs Posterior Probability)
#' @noRd
create_comparison_plot <- function(x, metric, values, ...) {
  plot_data <- x$power_surface

  if (x$analysis_type == "sample_only") {
    x_var <- "n_total"
    x_label <- "Total Sample Size"
    title_base <- "Power vs Posterior Probability (Sample Size Analysis)"
  } else if (x$analysis_type == "effect_only") {
    x_var <- "effect_size"
    x_label <- "Effect Size"
    title_base <- "Power vs Posterior Probability (Effect Size Analysis)"
  } else {
    stop("Comparison plot requires single varying dimension (sample_only or effect_only)")
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = x_var))

  if (metric == "success" || metric == "both") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = power_success, color = "Success Power"),
        size = 1.2, linetype = "solid"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = power_success, color = "Success Power"),
        size = 3, shape = 16
      ) +
      ggplot2::geom_line(ggplot2::aes(y = mean_prob_success, color = "Success Probability"),
        size = 1.2, linetype = "dashed"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = mean_prob_success, color = "Success Probability"),
        size = 3, shape = 17
      )
  }

  if (metric == "futility" || metric == "both") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = power_futility, color = "Futility Power"),
        size = 1.2, linetype = "solid"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = power_futility, color = "Futility Power"),
        size = 3, shape = 16
      ) +
      ggplot2::geom_line(ggplot2::aes(y = mean_prob_futility, color = "Futility Probability"),
        size = 1.2, linetype = "dashed"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = mean_prob_futility, color = "Futility Probability"),
        size = 3, shape = 17
      )
  }

  colors <- c(
    "Success Power" = "steelblue", "Success Probability" = "lightblue",
    "Futility Power" = "darkred", "Futility Probability" = "pink"
  )

  # Set x-axis to use discrete values from study design
  if (x_var == "n_total") {
    p <- p + ggplot2::scale_x_continuous(breaks = x$sample_sizes, labels = x$sample_sizes)
  } else if (x_var == "effect_size") {
    p <- p + ggplot2::scale_x_continuous(breaks = x$effect_sizes, labels = x$effect_sizes)
  }

  p <- p +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    ggplot2::labs(
      title = title_base,
      x = x_label,
      y = "Value",
      color = "Metric",
      caption = "Solid lines: Power, Dashed lines: Posterior Probability"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )

  return(p)
}
