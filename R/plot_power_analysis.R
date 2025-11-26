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
#' @param show_mcse Whether to show Monte Carlo standard error ribbons for uncertainty
#'   visualization (default: FALSE). Only applies to power_curve plots.
#' @param facet_by For power_curve plots when both sample sizes and effect sizes vary:
#'   \itemize{
#'     \item "effect_size" - Facet by effect size, vary sample size on x-axis (default)
#'     \item "sample_size" - Facet by sample size, vary effect size on x-axis
#'     \item "look" - For sequential designs, facet by interim analysis look
#'   }
#' @param target_power Optional numeric value (0-1) for drawing contour lines on heatmaps
#'   at the specified target power level. If NULL (default), uses the design's p_sig_scs.
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A plotly object for all plot types (power curves, heatmaps, and comparison plots)
#' @export
#' @importFrom stats as.formula
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_tile geom_hline geom_ribbon %+%
#' @importFrom ggplot2 geom_contour
#' @importFrom ggplot2 scale_y_continuous scale_x_continuous scale_fill_gradient
#' @importFrom ggplot2 scale_color_manual scale_linetype_manual scale_shape_manual scale_fill_manual
#' @importFrom ggplot2 labs theme_minimal theme element_text facet_wrap facet_grid vars
#' @importFrom plotly ggplotly
#' @importFrom scales percent_format percent
#'
#' @name plot.rctbp_power_analysis
#' @export
S7::method(plot, rctbp_power_analysis) <- function(x,
                                                   type = "auto",
                                                   metric = "both",
                                                   values = "both",
                                                   show_target = TRUE,
                                                   show_mcse = FALSE,
                                                   facet_by = "effect_size",
                                                   target_power = NULL,
                                                   ...) {
  # Check if analysis has been run
  if (nrow(x@results_conditions) == 0) {
    cli::cli_abort(c(
      "No simulation results found",
      "x" = "The power analysis has not been run yet",
      "i" = "Run the analysis first using {.fn run}"
    ))
  }

  # Call the internal plotting dispatcher
  create_power_plot(x,
                    type,
                    metric,
                    values,
                    show_target,
                    show_mcse,
                    facet_by,
                    target_power,
                    ...)
}

#' Internal plotting dispatcher
#'
#' Routes to appropriate plot function based on type and analysis dimensions.
#'
#' @inheritParams plot.rctbp_power_analysis
#' @keywords internal
create_power_plot <- function(x,
                              type = "auto",
                              metric = "both",
                              values = "both",
                              show_target = TRUE,
                              show_mcse = FALSE,
                              facet_by = "effect_size",
                              target_power = NULL,
                              ...) {
  # Check for valid data
  if (nrow(x@results_conditions) == 0) {
    cli::cli_abort(c(
      "No power analysis results to plot",
      "x" = "The summarized results are empty or missing",
      "i" = "Check that the power analysis object was run successfully"
    ))
  }

  # Use S7 object structure directly
  # For sequential: use results_interim (per-look data), for single-look: use results_conditions
  plot_data <- if (x@has_interim) x@results_interim else x@results_conditions
  design <- x@design
  conditions <- x@conditions

  # Check for missing essential columns
  required_cols <- c("pwr_scs", "pwr_ftl", "pr_scs", "pr_ftl")
  missing_cols <- setdiff(required_cols, names(plot_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in results",
      "x" = "Missing: {.val {missing_cols}}",
      "i" = "Check that the power analysis completed successfully"
    ))
  }

  # =============================================================================
  # DETECT EFFECT SIZE COLUMN FROM CONDITIONS
  # =============================================================================
  # The effect size column name comes from condition_values, not target_params.
  # target_params contains brms parameter names (e.g., "b_armtreat_1"),
  # but condition_values uses user-specified names (e.g., "b_arm_treat").
  condition_value_names <- names(conditions@condition_values)
  effect_cols <- setdiff(condition_value_names, "n_total")

  # Use first effect column if available, otherwise NULL
  effect_col <- if (length(effect_cols) > 0) effect_cols[1] else NULL

  # =============================================================================
  # DETERMINE ANALYSIS TYPE FROM DATA DIMENSIONS
  # =============================================================================
  unique_n_total <- if ("n_total" %in% names(plot_data)) {
    length(unique(plot_data$n_total))
  } else {
    1
  }

  unique_effects <- if (!is.null(effect_col) && effect_col %in% names(plot_data)) {
    length(unique(plot_data[[effect_col]]))
  } else {
    1
  }

  # Check for sequential analysis (multiple looks)
  has_looks <- "id_look" %in% names(plot_data) && length(unique(plot_data$id_look)) > 1

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

  # =============================================================================
  # AUTO-DETECT PLOT TYPE
  # =============================================================================
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

  # =============================================================================
  # VALIDATE PARAMETERS
  # =============================================================================
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

  if (!facet_by %in% c("effect_size", "sample_size", "look")) {
    cli::cli_abort(c(
      "{.arg facet_by} must be {.val effect_size}, {.val sample_size}, or {.val look}",
      "x" = "You supplied {.val {facet_by}}"
    ))
  }

  # Warn if facet_by = "look" but no interim analyses
  if (facet_by == "look" && !has_looks) {
    cli::cli_warn(c(
      "Cannot facet by look - no interim analyses detected",
      "i" = "Using default faceting instead"
    ))
    facet_by <- "effect_size"
  }

  # =============================================================================
  # DISPATCH TO PLOT FUNCTION
  # =============================================================================
  if (type == "power_curve") {
    p <- create_power_curve_plot(
      plot_data,
      design,
      analysis_type,
      effect_col,
      metric,
      values,
      show_target,
      show_mcse,
      facet_by,
      has_looks,
      ...
    )
  } else if (type == "heatmap") {
    p <- create_heatmap_plot(
      plot_data,
      design,
      analysis_type,
      effect_col,
      metric,
      values,
      show_target,
      target_power,
      ...
    )
  } else if (type == "comparison") {
    p <- create_comparison_plot(
      plot_data,
      design,
      analysis_type,
      effect_col,
      metric,
      values,
      ...
    )
  } else {
    cli::cli_abort(c(
      "Unknown plot type: {.val {type}}",
      "i" = "Use {.val power_curve}, {.val heatmap}, or {.val comparison}"
    ))
  }

  # Convert to interactive plotly
  to_interactive(p, legend_position = if (type == "heatmap") "right" else "bottom")
}
