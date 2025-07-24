#' Plot Power Analysis Results
#'
#' Create comprehensive visualizations for power analysis results from power_analysis().
#' Supports different plot types based on analysis type (sample_only, effect_only, or both varying).
#'
#' @param x An object of class 'rctbayespower_sim_result' returned by power_analysis()
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
#'   any design prior specified in the original power_analysis() call.
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
  if (is.null(x@sim_results)) {
    stop("No simulation results found. Please run the analysis first using run(power_config).")
  }
  
  # Extract results from S7 object
  sim_results <- x@sim_results
  
  # Call the internal plotting function
  create_power_plot(sim_results, type, metric, values, show_target, show_integrated, facet_by, design_prior, ...)
}

# Legacy S3 plot method (for backward compatibility)
#' @export
plot.rctbayespower_sim_result <- function(x,
                                    type = "auto",
                                    metric = "both",
                                    values = "both",
                                    show_target = TRUE,
                                    show_integrated = TRUE,
                                    facet_by = "effect_size",
                                    design_prior = NULL,
                                    ...) {
  
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

  # Check for valid data - adapt to new API
  if (is.null(x$results_df) || nrow(x$results_df) == 0) {
    stop("No power analysis results to plot. Check that power_analysis() completed successfully.")
  }

  # Map new API to old structure for plotting
  power_surface <- x$results_df
  design <- x$design
  conditions <- x$conditions

  # Check for missing essential columns - updated for new S7 API
  required_cols <- c(
    "power_success", "power_futility",
    "prob_success", "prob_futility"
  )
  missing_cols <- setdiff(required_cols, names(power_surface))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in results_df: ", paste(missing_cols, collapse = ", "))
  }

  # Map column names to plotting expectations (prob_* -> mean_prob_*)
  if ("prob_success" %in% names(power_surface)) {
    power_surface$mean_prob_success <- power_surface$prob_success
  }
  if ("prob_futility" %in% names(power_surface)) {
    power_surface$mean_prob_futility <- power_surface$prob_futility
  }

  # Validate and compute design prior integration for plotting
  if (!is.null(design_prior)) {
    # Validate design prior can only be used with varying effect sizes
    if (length(x$effect_sizes) <= 1) {
      stop("'design_prior' can only be specified when effect sizes vary (length > 1)")
    }

    # Parse design prior using effect sizes from object
    design_prior_parsed <- parse_design_prior(design_prior, x$effect_sizes, verbose = FALSE)
    weight_fn <- design_prior_parsed$weight_fn

    if (!is.null(weight_fn)) {
      # Get weights for each effect size
      weights <- sapply(x$effect_sizes, weight_fn)
      weights <- weights / sum(weights) # Normalize to sum to 1

      # For each sample size, compute weighted average power
      integrated_results <- list()

      for (n in x$sample_sizes) {
        subset_data <- x$power_surface[x$power_surface$n_total == n, ]

        if (nrow(subset_data) > 0 && all(!is.na(subset_data$power_success))) {
          weighted_power_success <- sum(subset_data$power_success * weights)
          weighted_power_futility <- sum(subset_data$power_futility * weights)
          weighted_prob_success <- sum(subset_data$mean_prob_success * weights)
          weighted_prob_futility <- sum(subset_data$mean_prob_futility * weights)

          integrated_results[[length(integrated_results) + 1]] <- data.frame(
            n_total = as.integer(n),
            integrated_power_success = weighted_power_success,
            integrated_power_futility = weighted_power_futility,
            integrated_prob_success = weighted_prob_success,
            integrated_prob_futility = weighted_prob_futility,
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(integrated_results) > 0) {
        # Update the object's integrated power with runtime computation
        x$integrated_power <- do.call(rbind, integrated_results)
        x$design_prior <- design_prior
      }
    }
  }

  # Extract unique values to determine analysis type
  unique_n_total <- if ("n_total" %in% names(power_surface)) length(unique(power_surface$n_total)) else 1
  target_param_cols <- intersect(names(power_surface), c("b_arms_treat", "effect_size"))
  unique_effects <- if (length(target_param_cols) > 0) length(unique(power_surface[[target_param_cols[1]]])) else 1

  # Determine analysis type from data
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

  # Create a compatible object structure for existing plot functions
  plot_obj <- list(
    power_surface = power_surface,
    analysis_type = analysis_type,
    target_power_success = design$p_sig_success,
    target_power_futility = design$p_sig_futility,
    sample_sizes = unique_n_total,
    effect_sizes = unique_effects,
    integrated_power = x$integrated_power
  )

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

  # Create plot based on type using compatible object
  if (type == "power_curve") {
    create_power_curve_plot(plot_obj, metric, values, show_target, show_integrated, facet_by, ...)
  } else if (type == "heatmap") {
    create_heatmap_plot(plot_obj, metric, values, show_target, ...)
  } else if (type == "integrated") {
    create_integrated_plot(plot_obj, metric, values, show_target, ...)
  } else if (type == "comparison") {
    create_comparison_plot(plot_obj, metric, values, ...)
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
    subtitle <- paste("Fixed effect size:", x$effect_sizes[1])
    facet_var <- NULL
  } else if (x$analysis_type == "effect_only") {
    # Use the actual target parameter name from the data
    target_param_cols <- intersect(names(plot_data), c("b_arms_treat", "effect_size"))
    x_var <- if (length(target_param_cols) > 0) target_param_cols[1] else "effect_size"
    x_label <- "Effect Size"
    title_base <- "Effect Size Analysis"
    subtitle <- paste("Fixed sample size:", x$sample_sizes[1])
    facet_var <- NULL
  } else if (x$analysis_type == "both") {
    # Handle faceting for both varying
    if (facet_by == "effect_size") {
      x_var <- "n_total"
      x_label <- "Total Sample Size"
      # Use the actual target parameter name from the data
      target_param_cols <- intersect(names(plot_data), c("b_arms_treat", "effect_size"))
      facet_var <- if (length(target_param_cols) > 0) target_param_cols[1] else "effect_size"
      facet_label <- "Effect Size"
      title_base <- "Power Curves by Effect Size"
      subtitle <- paste("Sample sizes:", paste(range(x$sample_sizes), collapse = "-"))
    } else {
      # Use the actual target parameter name from the data
      target_param_cols <- intersect(names(plot_data), c("b_arms_treat", "effect_size"))
      x_var <- if (length(target_param_cols) > 0) target_param_cols[1] else "effect_size"
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
        ggplot2::geom_line(ggplot2::aes(y = .data$power_success, color = "Success Power"),
          size = 1.2, linetype = "solid"
        ) +
        ggplot2::geom_point(ggplot2::aes(y = .data$power_success, color = "Success Power"),
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
        ggplot2::geom_line(ggplot2::aes(y = .data$mean_prob_success, color = "Success Probability"),
          size = 1.2, linetype = "dotted"
        ) +
        ggplot2::geom_point(ggplot2::aes(y = .data$mean_prob_success, color = "Success Probability"),
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
          yintercept = x$target_power_futility,
          linetype = "dashed", color = "darkred", alpha = 0.7
        )
      }
    }

    # Add posterior probability if requested
    if (values == "post_prob" || values == "both") {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(y = .data$mean_prob_futility, color = "Futility Probability"),
          size = 1.2, linetype = "dotted"
        ) +
        ggplot2::geom_point(ggplot2::aes(y = .data$mean_prob_futility, color = "Futility Probability"),
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
  } else if (x_var != "n_total") {
    # Handle effect size axis - use the actual effect size values from plot object
    if (!is.null(x$effect_sizes)) {
      p <- p + ggplot2::scale_x_continuous(breaks = x$effect_sizes, labels = x$effect_sizes)
    }
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
    # Use the actual target parameter name from the data
    target_param_cols <- intersect(names(x$power_surface), c("b_arms_treat", "effect_size"))
    x_var <- if (length(target_param_cols) > 0) target_param_cols[1] else "effect_size"
    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = x_var, y = "n_total", fill = fill_var_success)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(name = fill_name, labels = scales::percent_format()) +
      ggplot2::labs(
        title = paste("Success", plot_suffix, "Heatmap"),
        subtitle = if (values == "both") paste("Showing power | Target power:", x$target_power_success) else paste("Target:", x$target_power_success),
        x = "Effect Size",
        y = "Total Sample Size"
      )
  } else if (metric == "futility") {
    # Use the actual target parameter name from the data
    target_param_cols <- intersect(names(x$power_surface), c("b_arms_treat", "effect_size"))
    x_var <- if (length(target_param_cols) > 0) target_param_cols[1] else "effect_size"
    p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = x_var, y = "n_total", fill = fill_var_futility)) +
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
    # Use the actual target parameter name from the data
    target_param_cols <- intersect(names(x$power_surface), c("b_arms_treat", "effect_size"))
    x_var <- if (length(target_param_cols) > 0) target_param_cols[1] else "effect_size"

    if (values == "both") {
      # Create 4-panel plot: success power, success prob, futility power, futility prob
      plot_data_long <- rbind(
        data.frame(plot_data[, c("n_total", x_var)],
          value = plot_data$power_success,
          type = "Success Power"
        ),
        data.frame(plot_data[, c("n_total", x_var)],
          value = plot_data$mean_prob_success,
          type = "Success Probability"
        ),
        data.frame(plot_data[, c("n_total", x_var)],
          value = plot_data$power_futility,
          type = "Futility Power"
        ),
        data.frame(plot_data[, c("n_total", x_var)],
          value = plot_data$mean_prob_futility,
          type = "Futility Probability"
        )
      )
      # Rename the effect size column to a standard name for plotting
      names(plot_data_long)[names(plot_data_long) == x_var] <- "effect_size"
      fill_name <- "Value"
    } else {
      # Standard 2-panel plot
      plot_data_long <- rbind(
        data.frame(plot_data[, c("n_total", x_var)],
          value = plot_data[[fill_var_success]],
          type = paste("Success", plot_suffix)
        ),
        data.frame(plot_data[, c("n_total", x_var)],
          value = plot_data[[fill_var_futility]],
          type = paste("Futility", plot_suffix)
        )
      )
      # Rename the effect size column to a standard name for plotting
      names(plot_data_long)[names(plot_data_long) == x_var] <- "effect_size"
    }

    p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = .data$effect_size, y = .data$n_total, fill = .data$value)) +
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
    stop("Integrated power plot requires design prior to be specified in power_analysis()")
  }

  plot_data <- x$integrated_power

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$n_total))

  if (metric == "success" || metric == "both") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = .data$integrated_power_success, color = "Integrated Success Power"),
        size = 1.5, linetype = "solid"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = .data$integrated_power_success, color = "Integrated Success Power"),
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
      ggplot2::geom_line(ggplot2::aes(y = .data$integrated_power_futility, color = "Integrated Futility Power"),
        size = 1.5, linetype = "solid"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = .data$integrated_power_futility, color = "Integrated Futility Power"),
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
    # Use the actual target parameter name from the data
    target_param_cols <- intersect(names(x$power_surface), c("b_arms_treat", "effect_size"))
    x_var <- if (length(target_param_cols) > 0) target_param_cols[1] else "effect_size"
    x_label <- "Effect Size"
    title_base <- "Power vs Posterior Probability (Effect Size Analysis)"
  } else {
    stop("Comparison plot requires single varying dimension (sample_only or effect_only)")
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = x_var))

  if (metric == "success" || metric == "both") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = .data$power_success, color = "Success Power"),
        size = 1.2, linetype = "solid"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = .data$power_success, color = "Success Power"),
        size = 3, shape = 16
      ) +
      ggplot2::geom_line(ggplot2::aes(y = .data$mean_prob_success, color = "Success Probability"),
        size = 1.2, linetype = "dashed"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = .data$mean_prob_success, color = "Success Probability"),
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
      ggplot2::geom_line(ggplot2::aes(y = .data$mean_prob_futility, color = "Futility Probability"),
        size = 1.2, linetype = "dashed"
      ) +
      ggplot2::geom_point(ggplot2::aes(y = .data$mean_prob_futility, color = "Futility Probability"),
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
  } else if (x_var != "n_total") {
    # Handle effect size axis - use the actual effect size values from plot object
    if (!is.null(x$effect_sizes)) {
      p <- p + ggplot2::scale_x_continuous(breaks = x$effect_sizes, labels = x$effect_sizes)
    }
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
