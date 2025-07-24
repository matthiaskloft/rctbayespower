#' Plot Power Analysis Results
#'
#' Create comprehensive visualizations for power analysis results from rctbp_power_analysis objects.
#' Supports different plot types based on analysis type (sample_only, effect_only, or both varying).
#'
#' @param x An rctbp_power_analysis object that has been run with results
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
#' @param design_prior Optional design prior for runtime integrated power computation. Can be:
#'   \itemize{
#'     \item A string in brms prior syntax (e.g., "normal(0.3, 0.1)", "student_t(6, 0.5, 0.2)")
#'     \item An R function taking effect size as input (e.g., function(x) dnorm(x, 0.5, 0.2))
#'     \item NULL for no runtime integration (default)
#'   }
#'   If provided, integrated power will be computed using this design prior instead of
#'   any design prior specified in the original rctbp_power_analysis object.
#'   Only valid when effect sizes vary (length > 1).
#' @param ... Additional arguments passed to ggplot2 functions
#'
#' @return A ggplot2 object
#' @export
#' @importFrom stats as.formula
#' @importFrom rlang .data
#'
# S7 Plot Method for rctbp_power_analysis Objects
#' @importFrom S7 method
S7::method(plot, rctbp_power_analysis) <- function(x,
                                                   type = "auto",
                                                   metric = "both",
                                                   values = "both",
                                                   show_target = TRUE,
                                                   show_integrated = TRUE,
                                                   facet_by = "effect_size",
                                                   design_prior = NULL,
                                                   ...) {
  
  # Check if analysis has been run
  if (is.null(x@summarized_results)) {
    stop("No simulation results found. Please run the analysis first using run(power_config).")
  }
  
  # Call the internal plotting function
  create_power_plot(x, type, metric, values, show_target, show_integrated, facet_by, design_prior, ...)
}

# Internal plotting function (shared by both S7 and S3 methods)
create_power_plot <- function(x,
                             type = "auto",
                             metric = "both", 
                             values = "both",
                             show_target = TRUE,
                             show_integrated = TRUE,
                             facet_by = "effect_size",
                             design_prior = NULL,
                             ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }

  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Package 'scales' is required for plotting.")
  }

  # Check for valid data
  if (is.null(x@summarized_results) || nrow(x@summarized_results) == 0) {
    stop("No power analysis results to plot. Check that the rctbp_power_analysis object was run successfully.")
  }

  # Use S7 object structure directly
  plot_data <- x@summarized_results
  design <- x@design
  conditions <- x@conditions

  # Check for missing essential columns
  required_cols <- c(
    "power_success", "power_futility",
    "prob_success", "prob_futility"
  )
  missing_cols <- setdiff(required_cols, names(plot_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in results_df: ", paste(missing_cols, collapse = ", "))
  }

  # Determine analysis type from data dimensions
  unique_n_total <- if ("n_total" %in% names(plot_data)) length(unique(plot_data$n_total)) else 1
  # Use design target params to find effect size columns
  target_param <- design@target_params[1]  # Use first target parameter
  unique_effects <- if (target_param %in% names(plot_data)) length(unique(plot_data[[target_param]])) else 1

  # Determine analysis type
  if (unique_n_total > 1 && unique_effects > 1) {
    analysis_type <- "both"
  } else if (unique_n_total > 1 && unique_effects == 1) {
    analysis_type <- "sample_only"
  } else if (unique_n_total == 1 && unique_effects > 1) {
    analysis_type <- "effect_only"
  } else {
    analysis_type <- "single"
  }

  # Auto-detect plot type based on analysis type
  if (type == "auto") {
    if (analysis_type == "sample_only") {
      type <- "power_curve"
    } else if (analysis_type == "effect_only") {
      type <- "power_curve"
    } else if (analysis_type == "both") {
      type <- "heatmap"
    } else {
      stop("Cannot auto-detect plot type for analysis type: ", analysis_type)
    }
  }

  # Validate parameters
  if (!metric %in% c("success", "futility", "both")) {
    stop("'metric' must be 'success', 'futility', or 'both'")
  }

  if (!values %in% c("power", "post_prob", "both")) {
    stop("'values' must be 'power', 'post_prob', or 'both'")
  }

  if (!facet_by %in% c("effect_size", "sample_size")) {
    stop("'facet_by' must be 'effect_size' or 'sample_size'")
  }

  # Create plot based on type
  if (type == "power_curve") {
    create_power_curve_plot(plot_data, design, analysis_type, target_param, metric, values, show_target, show_integrated, facet_by, ...)
  } else if (type == "heatmap") {
    create_heatmap_plot(plot_data, design, analysis_type, target_param, metric, values, show_target, ...)
  } else if (type == "integrated") {
    create_integrated_plot(plot_data, design, analysis_type, target_param, metric, values, show_target, ...)
  } else if (type == "comparison") {
    create_comparison_plot(plot_data, design, analysis_type, target_param, metric, values, ...)
  } else {
    stop("Unknown plot type: ", type, ". Use 'power_curve', 'heatmap', 'integrated', or 'comparison'.")
  }
}

#' Create Power Curve Plot
#' @noRd
create_power_curve_plot <- function(plot_data, design, analysis_type, target_param, metric, values, show_target, show_integrated, facet_by, ...) {
  
  if (analysis_type == "sample_only") {
    x_var <- "n_total"
    x_label <- "Total Sample Size"
    title_base <- "Sample Size Analysis"
    subtitle <- paste("Fixed effect size:", unique(plot_data[[target_param]])[1])
    facet_var <- NULL
  } else if (analysis_type == "effect_only") {
    x_var <- target_param
    x_label <- "Effect Size"
    title_base <- "Effect Size Analysis"
    subtitle <- paste("Fixed sample size:", unique(plot_data$n_total)[1])
    facet_var <- NULL
  } else if (analysis_type == "both") {
    # Handle faceting for both varying
    if (facet_by == "effect_size") {
      x_var <- "n_total"
      x_label <- "Total Sample Size"
      facet_var <- target_param
      facet_label <- "Effect Size"
      title_base <- "Power Curves by Effect Size"
      subtitle <- paste("Sample sizes:", paste(range(plot_data$n_total), collapse = "-"))
    } else {
      x_var <- target_param
      x_label <- "Effect Size"
      facet_var <- "n_total"
      facet_label <- "Sample Size"
      title_base <- "Power Curves by Sample Size"
      subtitle <- paste("Effect sizes:", paste(range(plot_data[[target_param]]), collapse = "-"))
    }
  } else {
    stop("Unknown analysis type")
  }

  # Base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_var]]))

  # Success metrics
  if (metric == "success" || metric == "both") {
    # Add power if requested
    if (values == "power" || values == "both") {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(y = .data$power_success, color = "Success Power"),
          size = 1.2, linetype = "solid"
        ) +
        ggplot2::geom_point(ggplot2::aes(y = .data$power_success, color = "Success Power"),
          size = 3, shape = 16
        )

      if (show_target) {
        p <- p + ggplot2::geom_hline(
          yintercept = design@p_sig_success,
          linetype = "dashed", color = "steelblue", alpha = 0.7
        )
      }
    }

    # Add posterior probability if requested
    if (values == "post_prob" || values == "both") {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(y = .data$prob_success, color = "Success Probability"),
          size = 1.2, linetype = "dotted"
        ) +
        ggplot2::geom_point(ggplot2::aes(y = .data$prob_success, color = "Success Probability"),
          size = 3, shape = 17
        )
    }
  }

  # Futility metrics
  if (metric == "futility" || metric == "both") {
    # Add power if requested
    if (values == "power" || values == "both") {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(y = .data$power_futility, color = "Futility Power"),
          size = 1.2, linetype = "solid"
        ) +
        ggplot2::geom_point(ggplot2::aes(y = .data$power_futility, color = "Futility Power"),
          size = 3, shape = 16
        )

      if (show_target) {
        p <- p + ggplot2::geom_hline(
          yintercept = design@p_sig_futility,
          linetype = "dashed", color = "darkred", alpha = 0.7
        )
      }
    }

    # Add posterior probability if requested
    if (values == "post_prob" || values == "both") {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(y = .data$prob_futility, color = "Futility Probability"),
          size = 1.2, linetype = "dotted"
        ) +
        ggplot2::geom_point(ggplot2::aes(y = .data$prob_futility, color = "Futility Probability"),
          size = 3, shape = 17
        )
    }
  }

  # Note: Integrated power functionality will be handled separately if needed

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

  # Set x-axis to use discrete values from data
  if (x_var == "n_total") {
    unique_n <- sort(unique(plot_data$n_total))
    p <- p + ggplot2::scale_x_continuous(breaks = unique_n, labels = unique_n)
  } else {
    # Handle effect size axis
    unique_effects <- sort(unique(plot_data[[x_var]]))
    p <- p + ggplot2::scale_x_continuous(breaks = unique_effects, labels = unique_effects)
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
create_heatmap_plot <- function(plot_data, design, analysis_type, target_param, metric, values, show_target, ...) {
  if (analysis_type != "both") {
    stop("Heatmap plot requires both sample sizes and effect sizes to vary")
  }

  # Check for sufficient data points
  if (nrow(plot_data) < 4) {
    stop("Heatmap requires at least 4 data points (2x2 grid). Consider using type='power_curve' instead.")
  }

  # Remove rows with all NA values for key metrics
  valid_rows <- !(is.na(plot_data$power_success) & is.na(plot_data$power_futility) &
    is.na(plot_data$prob_success) & is.na(plot_data$prob_futility))
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
    fill_var_success <- "prob_success"
    fill_var_futility <- "prob_futility"
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
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[target_param]], y = .data$n_total, fill = .data[[fill_var_success]])) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(name = fill_name, labels = scales::percent_format(), low = "lightblue", high = "darkblue") +
      ggplot2::labs(
        title = paste("Success", plot_suffix, "Heatmap"),
        subtitle = if (values == "both") paste("Showing power | Target power:", design@p_sig_success) else paste("Target:", design@p_sig_success),
        x = "Effect Size",
        y = "Total Sample Size"
      )
  } else if (metric == "futility") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[target_param]], y = .data$n_total, fill = .data[[fill_var_futility]])) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(name = fill_name, labels = scales::percent_format(), low = "lightblue", high = "darkblue") +
      ggplot2::labs(
        title = paste("Futility", plot_suffix, "Heatmap"),
        subtitle = if (values == "both") paste("Showing power | Target power:", design@p_sig_futility) else paste("Target:", design@p_sig_futility),
        x = "Effect Size",
        y = "Total Sample Size"
      )
  } else {
    # Both - create faceted plot
    if (values == "both") {
      # Create 4-panel plot: success power, success prob, futility power, futility prob
      plot_data_long <- rbind(
        data.frame(plot_data[, c("n_total", target_param)],
          value = plot_data$power_success,
          type = "Success Power"
        ),
        data.frame(plot_data[, c("n_total", target_param)],
          value = plot_data$prob_success,
          type = "Success Probability"
        ),
        data.frame(plot_data[, c("n_total", target_param)],
          value = plot_data$power_futility,
          type = "Futility Power"
        ),
        data.frame(plot_data[, c("n_total", target_param)],
          value = plot_data$prob_futility,
          type = "Futility Probability"
        )
      )
      # Rename the effect size column to a standard name for plotting
      names(plot_data_long)[names(plot_data_long) == target_param] <- "effect_size"
      fill_name <- "Value"
    } else {
      # Standard 2-panel plot
      plot_data_long <- rbind(
        data.frame(plot_data[, c("n_total", target_param)],
          value = plot_data[[fill_var_success]],
          type = paste("Success", plot_suffix)
        ),
        data.frame(plot_data[, c("n_total", target_param)],
          value = plot_data[[fill_var_futility]],
          type = paste("Futility", plot_suffix)
        )
      )
      # Rename the effect size column to a standard name for plotting
      names(plot_data_long)[names(plot_data_long) == target_param] <- "effect_size"
    }

    p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = .data$effect_size, y = .data$n_total, fill = .data$value)) +
      ggplot2::geom_tile() +
      ggplot2::facet_wrap(~type) +
      ggplot2::scale_fill_gradient(name = fill_name, labels = scales::percent_format(), low = "lightblue", high = "darkblue") +
      ggplot2::labs(
        title = if (values == "both") "Power and Probability Analysis Heatmap" else paste(plot_suffix, "Analysis Heatmap"),
        subtitle = paste(
          "Target powers - Success:", design@p_sig_success,
          ", Futility:", design@p_sig_futility
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
create_integrated_plot <- function(plot_data, design, analysis_type, target_param, metric, values, show_target, ...) {
  # For now, integrated power functionality is not implemented in the new API
  stop("Integrated power plots are not yet implemented in the new API. Please use type='power_curve' or type='heatmap' instead.")

}

#' Create Comparison Plot (Power vs Posterior Probability)
#' @noRd
create_comparison_plot <- function(plot_data, design, analysis_type, target_param, metric, values, ...) {
  
  if (analysis_type == "sample_only") {
    x_var <- "n_total"
    x_label <- "Total Sample Size"
    title_base <- "Power vs Posterior Probability (Sample Size Analysis)"
  } else if (analysis_type == "effect_only") {
    x_var <- target_param
    x_label <- "Effect Size"
    title_base <- "Power vs Posterior Probability (Effect Size Analysis)"
  } else {
    stop("Comparison plot requires single varying dimension (sample_only or effect_only)")
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_var]]))

  if (metric == "success" || metric == "both") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = .data$power_success, color = "Success Power"),
        size = 1.2, linetype = "solid"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = .data$power_success, color = "Success Power"),
        size = 3, shape = 16
      ) +
      ggplot2::geom_line(ggplot2::aes(y = .data$prob_success, color = "Success Probability"),
        size = 1.2, linetype = "dashed"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = .data$prob_success, color = "Success Probability"),
        size = 3, shape = 17
      )
  }

  if (metric == "futility" || metric == "both") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = .data$power_futility, color = "Futility Power"),
        size = 1.2, linetype = "solid"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = .data$power_futility, color = "Futility Power"),
        size = 3, shape = 16
      ) +
      ggplot2::geom_line(ggplot2::aes(y = .data$prob_futility, color = "Futility Probability"),
        size = 1.2, linetype = "dashed"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = .data$prob_futility, color = "Futility Probability"),
        size = 3, shape = 17
      )
  }

  colors <- c(
    "Success Power" = "steelblue", "Success Probability" = "lightblue",
    "Futility Power" = "darkred", "Futility Probability" = "pink"
  )

  # Set x-axis to use discrete values from data
  if (x_var == "n_total") {
    unique_n <- sort(unique(plot_data$n_total))
    p <- p + ggplot2::scale_x_continuous(breaks = unique_n, labels = unique_n)
  } else {
    # Handle effect size axis
    unique_effects <- sort(unique(plot_data[[x_var]]))
    p <- p + ggplot2::scale_x_continuous(breaks = unique_effects, labels = unique_effects)
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
