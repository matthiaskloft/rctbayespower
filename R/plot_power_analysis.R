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
#' @param ... Additional arguments passed to plotly functions
#'
#' @return A plotly object for all plot types (power curves, heatmaps, and comparison plots)
#' @export
#' @importFrom stats as.formula
#' @importFrom rlang .data
#' @importFrom plotly plot_ly add_trace layout subplot
#'
# S7 Plot Method for rctbp_power_analysis Objects
#' @importFrom S7 method
#' @name plot.rctbp_power_analysis
#' @export
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
    cli::cli_abort(c(
      "No simulation results found",
      "x" = "The power analysis has not been run yet",
      "i" = "Run the analysis first using {.fn run}"
    ))
  }
  
  # Call the internal plotting function
  create_power_plot(x,
                    type,
                    metric,
                    values,
                    show_target,
                    show_integrated,
                    facet_by,
                    design_prior,
                    ...)
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
  # if (!requireNamespace("ggplot2", quietly = TRUE)) {
  #   stop("Package 'ggplot2' is required for plotting.")
  # }
  # 
  # if (!requireNamespace("scales", quietly = TRUE)) {
  #   stop("Package 'scales' is required for plotting.")
  # }
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg plotly} is required for interactive plotting",
      "i" = "Install it with {.code install.packages('plotly')}"
    ))
  }
  
  # Check for valid data
  if (is.null(x@summarized_results) ||
      nrow(x@summarized_results) == 0) {
    cli::cli_abort(c(
      "No power analysis results to plot",
      "x" = "The summarized results are empty or missing",
      "i" = "Check that the power analysis object was run successfully"
    ))
  }
  
  # Use S7 object structure directly
  plot_data <- x@summarized_results
  design <- x@design
  conditions <- x@conditions
  
  # Check for missing essential columns
  required_cols <- c("power_success",
                     "power_futility",
                     "prob_success",
                     "prob_futility")
  missing_cols <- setdiff(required_cols, names(plot_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in results",
      "x" = "Missing: {.val {missing_cols}}",
      "i" = "Check that the power analysis completed successfully"
    ))
  }
  
  # Determine analysis type from data dimensions
  unique_n_total <- if ("n_total" %in% names(plot_data))
    length(unique(plot_data$n_total))
  else
    1
  # Use design target params to find effect size columns
  target_param <- design@target_params[1]  # Use first target parameter
  unique_effects <- if (target_param %in% names(plot_data$parameter)) {
    effect_name <- unique(plot_data$parameter)[1] |> unlist()
    plot_data |>
      dplyr::filter(parameter == effect_name) |>
      dplyr::group_by(parameter) |>
      # count rows
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::distinct(n) |> unlist()
  }
  else{
    1
  }
  
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
      cli::cli_abort(c(
        "Cannot auto-detect plot type",
        "x" = "Analysis type: {.val {analysis_type}}",
        "i" = "Specify {.arg type} explicitly"
      ))
    }
  }
  
  # Validate parameters
  if (!metric %in% c("success", "futility", "both")) {
    cli::cli_abort(c(
      "{.arg metric} must be {.val success}, {.val futility}, or {.val both}",
      "x" = "You supplied {.val {metric}}"
    ))
  }

  if (!values %in% c("power", "post_prob", "both")) {
    cli::cli_abort(c(
      "{.arg values} must be {.val power}, {.val post_prob}, or {.val both}",
      "x" = "You supplied {.val {values}}"
    ))
  }

  if (!facet_by %in% c("effect_size", "sample_size")) {
    cli::cli_abort(c(
      "{.arg facet_by} must be {.val effect_size} or {.val sample_size}",
      "x" = "You supplied {.val {facet_by}}"
    ))
  }
  
  # Create plot based on type
  if (type == "power_curve") {
    create_power_curve_plot(
      plot_data,
      design,
      analysis_type,
      target_param,
      metric,
      values,
      show_target,
      show_integrated,
      facet_by,
      ...
    )
  } else if (type == "heatmap") {
    create_heatmap_plot(plot_data,
                        design,
                        analysis_type,
                        target_param,
                        metric,
                        values,
                        show_target,
                        ...)
  } else if (type == "integrated") {
    create_integrated_plot(plot_data,
                           design,
                           analysis_type,
                           target_param,
                           metric,
                           values,
                           show_target,
                           ...)
  } else if (type == "comparison") {
    create_comparison_plot(plot_data,
                           design,
                           analysis_type,
                           target_param,
                           metric,
                           values,
                           ...)
  } else {
    cli::cli_abort(c(
      "Unknown plot type: {.val {type}}",
      "i" = "Use {.val power_curve}, {.val heatmap}, {.val integrated}, or {.val comparison}"
    ))
  }
}

#' Create Power Curve Plot
#' @noRd
create_power_curve_plot <- function(plot_data,
                                    design,
                                    analysis_type,
                                    target_param,
                                    metric,
                                    values,
                                    show_target,
                                    show_integrated,
                                    facet_by,
                                    ...) {
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
    cli::cli_abort(c(
      "Unknown analysis type",
      "x" = "Analysis type: {.val {analysis_type}}",
      "i" = "This is an internal error - please report"
    ))
  }
  
  # Initialize plotly figure
  p <- plotly::plot_ly(plot_data)
  
  # Success metrics
  if (metric == "success" || metric == "both") {
    # Add power if requested
    if (values == "power" || values == "both") {
      p <- p |>
        plotly::add_trace(
          x = ~ get(x_var),
          y = ~ power_success,
          type = "scatter",
          mode = "lines+markers",
          name = "Success Power",
          line = list(color = "steelblue", width = 2),
          marker = list(color = "steelblue", size = 6)
        )
      
      if (show_target) {
        p <- p |>
          plotly::add_trace(
            x = c(min(plot_data[[x_var]]), max(plot_data[[x_var]])),
            y = c(design@p_sig_success, design@p_sig_success),
            type = "scatter",
            mode = "lines",
            name = "Success Target",
            line = list(
              color = "steelblue",
              width = 1,
              dash = "dash"
            ),
            showlegend = FALSE
          )
      }
    }
    
    # Add posterior probability if requested
    if (values == "post_prob" || values == "both") {
      p <- p |>
        plotly::add_trace(
          x = ~ get(x_var),
          y = ~ prob_success,
          type = "scatter",
          mode = "lines+markers",
          name = "Success Probability",
          line = list(
            color = "lightblue",
            width = 2,
            dash = "dot"
          ),
          marker = list(
            color = "lightblue",
            size = 6,
            symbol = "diamond"
          )
        )
    }
  }
  
  # Futility metrics
  if (metric == "futility" || metric == "both") {
    # Add power if requested
    if (values == "power" || values == "both") {
      p <- p |>
        plotly::add_trace(
          x = ~ get(x_var),
          y = ~ power_futility,
          type = "scatter",
          mode = "lines+markers",
          name = "Futility Power",
          line = list(color = "darkred", width = 2),
          marker = list(color = "darkred", size = 6)
        )
      
      if (show_target) {
        p <- p |>
          plotly::add_trace(
            x = c(min(plot_data[[x_var]]), max(plot_data[[x_var]])),
            y = c(design@p_sig_futility, design@p_sig_futility),
            type = "scatter",
            mode = "lines",
            name = "Futility Target",
            line = list(
              color = "darkred",
              width = 1,
              dash = "dash"
            ),
            showlegend = FALSE
          )
      }
    }
    
    # Add posterior probability if requested
    if (values == "post_prob" || values == "both") {
      p <- p |>
        plotly::add_trace(
          x = ~ get(x_var),
          y = ~ prob_futility,
          type = "scatter",
          mode = "lines+markers",
          name = "Futility Probability",
          line = list(
            color = "pink",
            width = 2,
            dash = "dot"
          ),
          marker = list(
            color = "pink",
            size = 6,
            symbol = "diamond"
          )
        )
    }
  }
  
  # Dynamic y-axis label based on values parameter
  y_label <- switch(values,
                    "power" = "Power",
                    "post_prob" = "Posterior Probability",
                    "both" = "Power / Posterior Probability")
  
  # Handle faceting for plotly (create subplots if needed)
  if (!is.null(facet_var)) {
    # For faceted plots, we need to restructure the approach
    # This is a simplified version - full subplot implementation would be more complex
    subtitle <- paste(subtitle, "| Faceting:", facet_label)
  }
  
  # Apply layout to plotly figure
  p <- p |>
    plotly::layout(
      title = list(
        text = paste("Bayesian RCT", title_base),
        font = list(size = 16, family = "Arial, sans-serif")
      ),
      xaxis = list(
        title = x_label,
        titlefont = list(size = 12),
        tickfont = list(size = 10)
      ),
      yaxis = list(
        title = y_label,
        titlefont = list(size = 12),
        tickfont = list(size = 10),
        range = c(0, 1),
        tickformat = ".0%"
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.1
      ),
      hovermode = "x unified",
      annotations = list(
        list(
          text = subtitle,
          x = 0.5,
          y = 1.02,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 12)
        )
      )
    )
  
  return(p)
}

#' Create Heatmap Plot
#' @noRd
create_heatmap_plot <- function(plot_data,
                                design,
                                analysis_type,
                                target_param,
                                metric,
                                values,
                                show_target,
                                ...) {
  if (analysis_type != "both") {
    cli::cli_abort(c(
      "Heatmap plot requires both sample sizes and effect sizes to vary",
      "x" = "Your analysis only varies one dimension",
      "i" = "Use {.code type = 'power_curve'} instead"
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
  
  # Remove rows with all NA values for key metrics
  valid_rows <- !(
    is.na(plot_data$power_success) & is.na(plot_data$power_futility) &
      is.na(plot_data$prob_success) & is.na(plot_data$prob_futility)
  )
  plot_data <- plot_data[valid_rows, ]
  
  if (nrow(plot_data) == 0) {
    cli::cli_abort(c(
      "No valid data points for heatmap",
      "x" = "All power analysis results contain NA values",
      "i" = "Check your analysis for errors or convergence issues"
    ))
  }
  
  # Get unique values for axes
  x_vals <- sort(unique(plot_data[[target_param]]))
  y_vals <- sort(unique(plot_data$n_total))
  
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
    # Create matrix for heatmap
    z_matrix <- matrix(NA, nrow = length(y_vals), ncol = length(x_vals))
    for (i in seq_along(y_vals)) {
      for (j in seq_along(x_vals)) {
        row_match <- which(plot_data$n_total == y_vals[i] &
                             plot_data[[target_param]] == x_vals[j])
        if (length(row_match) > 0) {
          z_matrix[i, j] <- plot_data[[fill_var_success]][row_match[1]]
        }
      }
    }
    
    p <- plotly::plot_ly(
      x = x_vals,
      y = y_vals,
      z = z_matrix,
      type = "heatmap",
      colorscale = list(c(0, "lightblue"), c(1, "darkblue")),
      hovertemplate = paste(
        "Effect Size: %{x}<br>",
        "Sample Size: %{y}<br>",
        fill_name,
        ": %{z:.1%}<extra></extra>"
      )
    ) |>
      plotly::layout(
        title = list(
          text = paste("Success", plot_suffix, "Heatmap"),
          font = list(size = 16, family = "Arial, sans-serif")
        ),
        xaxis = list(
          title = "Effect Size",
          titlefont = list(size = 12),
          tickfont = list(size = 10)
        ),
        yaxis = list(
          title = "Total Sample Size",
          titlefont = list(size = 12),
          tickfont = list(size = 10)
        ),
        annotations = list(
          list(
            text = if (values == "both")
              paste("Showing power | Target power:", design@p_sig_success)
            else
              paste("Target:", design@p_sig_success),
            x = 0.5,
            y = 1.02,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE,
            font = list(size = 12)
          )
        )
      )
    
  } else if (metric == "futility") {
    # Create matrix for heatmap
    z_matrix <- matrix(NA, nrow = length(y_vals), ncol = length(x_vals))
    for (i in seq_along(y_vals)) {
      for (j in seq_along(x_vals)) {
        row_match <- which(plot_data$n_total == y_vals[i] &
                             plot_data[[target_param]] == x_vals[j])
        if (length(row_match) > 0) {
          z_matrix[i, j] <- plot_data[[fill_var_futility]][row_match[1]]
        }
      }
    }
    
    p <- plotly::plot_ly(
      x = x_vals,
      y = y_vals,
      z = z_matrix,
      type = "heatmap",
      colorscale = list(c(0, "lightblue"), c(1, "darkblue")),
      hovertemplate = paste(
        "Effect Size: %{x}<br>",
        "Sample Size: %{y}<br>",
        fill_name,
        ": %{z:.1%}<extra></extra>"
      )
    ) |>
      plotly::layout(
        title = list(
          text = paste("Futility", plot_suffix, "Heatmap"),
          font = list(size = 16, family = "Arial, sans-serif")
        ),
        xaxis = list(
          title = "Effect Size",
          titlefont = list(size = 12),
          tickfont = list(size = 10)
        ),
        yaxis = list(
          title = "Total Sample Size",
          titlefont = list(size = 12),
          tickfont = list(size = 10)
        ),
        annotations = list(
          list(
            text = if (values == "both")
              paste("Showing power | Target power:", design@p_sig_futility)
            else
              paste("Target:", design@p_sig_futility),
            x = 0.5,
            y = 1.02,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE,
            font = list(size = 12)
          )
        )
      )
    
  } else {
    # Both - create subplot heatmaps
    if (values == "both") {
      # Create matrices for all 4 plots
      z_success_power <- matrix(NA, nrow = length(y_vals), ncol = length(x_vals))
      z_success_prob <- matrix(NA, nrow = length(y_vals), ncol = length(x_vals))
      z_futility_power <- matrix(NA, nrow = length(y_vals), ncol = length(x_vals))
      z_futility_prob <- matrix(NA, nrow = length(y_vals), ncol = length(x_vals))
      
      for (i in seq_along(y_vals)) {
        for (j in seq_along(x_vals)) {
          row_match <- which(plot_data$n_total == y_vals[i] &
                               plot_data[[target_param]] == x_vals[j])
          if (length(row_match) > 0) {
            z_success_power[i, j] <- plot_data$power_success[row_match[1]]
            z_success_prob[i, j] <- plot_data$prob_success[row_match[1]]
            z_futility_power[i, j] <- plot_data$power_futility[row_match[1]]
            z_futility_prob[i, j] <- plot_data$prob_futility[row_match[1]]
          }
        }
      }
      
      # Create subplot structure
      p1 <- plotly::plot_ly(
        x = x_vals,
        y = y_vals,
        z = z_success_power,
        type = "heatmap",
        colorscale = list(c(0, "lightblue"), c(1, "darkblue")),
        hovertemplate = "Effect Size: %{x}<br>Sample Size: %{y}<br>Success Power: %{z:.1%}<extra></extra>",
        showscale = FALSE
      )
      p2 <- plotly::plot_ly(
        x = x_vals,
        y = y_vals,
        z = z_success_prob,
        type = "heatmap",
        colorscale = list(c(0, "lightblue"), c(1, "darkblue")),
        hovertemplate = "Effect Size: %{x}<br>Sample Size: %{y}<br>Success Probability: %{z:.1%}<extra></extra>",
        showscale = FALSE
      )
      p3 <- plotly::plot_ly(
        x = x_vals,
        y = y_vals,
        z = z_futility_power,
        type = "heatmap",
        colorscale = list(c(0, "lightblue"), c(1, "darkblue")),
        hovertemplate = "Effect Size: %{x}<br>Sample Size: %{y}<br>Futility Power: %{z:.1%}<extra></extra>",
        showscale = FALSE
      )
      p4 <- plotly::plot_ly(
        x = x_vals,
        y = y_vals,
        z = z_futility_prob,
        type = "heatmap",
        colorscale = list(c(0, "lightblue"), c(1, "darkblue")),
        hovertemplate = "Effect Size: %{x}<br>Sample Size: %{y}<br>Futility Probability: %{z:.1%}<extra></extra>"
      )
      
      p <- plotly::subplot(
        p1,
        p2,
        p3,
        p4,
        nrows = 2,
        subplot_titles = c(
          "Success Power",
          "Success Probability",
          "Futility Power",
          "Futility Probability"
        ),
        shareX = TRUE,
        shareY = TRUE
      ) |>
        plotly::layout(
          title = list(
            text = "Power and Probability Analysis Heatmap",
            font = list(size = 16, family = "Arial, sans-serif")
          ),
          annotations = list(
            list(
              text = paste(
                "Target powers - Success:",
                design@p_sig_success,
                ", Futility:",
                design@p_sig_futility
              ),
              x = 0.5,
              y = -0.05,
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "top",
              showarrow = FALSE,
              font = list(size = 12)
            )
          )
        )
      
    } else {
      # Standard 2-panel plot
      z_success <- matrix(NA, nrow = length(y_vals), ncol = length(x_vals))
      z_futility <- matrix(NA, nrow = length(y_vals), ncol = length(x_vals))
      
      for (i in seq_along(y_vals)) {
        for (j in seq_along(x_vals)) {
          row_match <- which(plot_data$n_total == y_vals[i] &
                               plot_data[[target_param]] == x_vals[j])
          if (length(row_match) > 0) {
            z_success[i, j] <- plot_data[[fill_var_success]][row_match[1]]
            z_futility[i, j] <- plot_data[[fill_var_futility]][row_match[1]]
          }
        }
      }
      
      p1 <- plotly::plot_ly(
        x = x_vals,
        y = y_vals,
        z = z_success,
        type = "heatmap",
        colorscale = list(c(0, "lightblue"), c(1, "darkblue")),
        hovertemplate = paste(
          "Effect Size: %{x}<br>Sample Size: %{y}<br>Success",
          plot_suffix,
          ": %{z:.1%}<extra></extra>"
        ),
        showscale = FALSE
      )
      p2 <- plotly::plot_ly(
        x = x_vals,
        y = y_vals,
        z = z_futility,
        type = "heatmap",
        colorscale = list(c(0, "lightblue"), c(1, "darkblue")),
        hovertemplate = paste(
          "Effect Size: %{x}<br>Sample Size: %{y}<br>Futility",
          plot_suffix,
          ": %{z:.1%}<extra></extra>"
        )
      )
      
      p <- plotly::subplot(
        p1,
        p2,
        nrows = 1,
        subplot_titles = c(
          paste("Success", plot_suffix),
          paste("Futility", plot_suffix)
        ),
        shareX = TRUE,
        shareY = TRUE
      ) |>
        plotly::layout(title = list(
          text = paste(plot_suffix, "Analysis Heatmap"),
          font = list(size = 16, family = "Arial, sans-serif")
        ),
        annotations = list(
          list(
            text = paste(
              "Target powers - Success:",
              design@p_sig_success,
              ", Futility:",
              design@p_sig_futility
            ),
            x = 0.5,
            y = -0.05,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 12)
          )
        ))
    }
  }
  
  return(p)
}

#' Create Integrated Power Plot
#' @noRd
create_integrated_plot <- function(plot_data,
                                   design,
                                   analysis_type,
                                   target_param,
                                   metric,
                                   values,
                                   show_target,
                                   ...) {
  # For now, integrated power functionality is not implemented in the new API
  cli::cli_abort(c(
    "Integrated power plots are not yet implemented",
    "i" = "Use {.code type = 'power_curve'} or {.code type = 'heatmap'} instead"
  ))
  
}

#' Create Comparison Plot (Power vs Posterior Probability)
#' @noRd
create_comparison_plot <- function(plot_data,
                                   design,
                                   analysis_type,
                                   target_param,
                                   metric,
                                   values,
                                   ...) {
  if (analysis_type == "sample_only") {
    x_var <- "n_total"
    x_label <- "Total Sample Size"
    title_base <- "Power vs Posterior Probability (Sample Size Analysis)"
  } else if (analysis_type == "effect_only") {
    x_var <- target_param
    x_label <- "Effect Size"
    title_base <- "Power vs Posterior Probability (Effect Size Analysis)"
  } else {
    cli::cli_abort(c(
      "Comparison plot requires single varying dimension",
      "x" = "Your analysis varies multiple dimensions",
      "i" = "Use {.code type = 'heatmap'} for multi-dimensional analysis"
    ))
  }
  
  # Initialize plotly figure
  p <- plotly::plot_ly(plot_data)
  
  if (metric == "success" || metric == "both") {
    p <- p |>
      plotly::add_trace(
        x = ~ get(x_var),
        y = ~ power_success,
        type = "scatter",
        mode = "lines+markers",
        name = "Success Power",
        line = list(color = "steelblue", width = 2),
        marker = list(color = "steelblue", size = 6)
      ) |>
      plotly::add_trace(
        x = ~ get(x_var),
        y = ~ prob_success,
        type = "scatter",
        mode = "lines+markers",
        name = "Success Probability",
        line = list(
          color = "lightblue",
          width = 2,
          dash = "dash"
        ),
        marker = list(
          color = "lightblue",
          size = 6,
          symbol = "diamond"
        )
      )
  }
  
  if (metric == "futility" || metric == "both") {
    p <- p |>
      plotly::add_trace(
        x = ~ get(x_var),
        y = ~ power_futility,
        type = "scatter",
        mode = "lines+markers",
        name = "Futility Power",
        line = list(color = "darkred", width = 2),
        marker = list(color = "darkred", size = 6)
      ) |>
      plotly::add_trace(
        x = ~ get(x_var),
        y = ~ prob_futility,
        type = "scatter",
        mode = "lines+markers",
        name = "Futility Probability",
        line = list(
          color = "pink",
          width = 2,
          dash = "dash"
        ),
        marker = list(
          color = "pink",
          size = 6,
          symbol = "diamond"
        )
      )
  }
  
  # Apply layout to plotly figure
  p <- p |>
    plotly::layout(
      title = list(
        text = title_base,
        font = list(size = 16, family = "Arial, sans-serif")
      ),
      xaxis = list(
        title = x_label,
        titlefont = list(size = 12),
        tickfont = list(size = 10)
      ),
      yaxis = list(
        title = "Value",
        titlefont = list(size = 12),
        tickfont = list(size = 10),
        range = c(0, 1),
        tickformat = ".0%"
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.1
      ),
      hovermode = "x unified",
      annotations = list(
        list(
          text = "Solid lines: Power, Dashed lines: Posterior Probability",
          x = 0.5,
          y = -0.2,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10)
        )
      )
    )
  
  return(p)
}
