# =============================================================================
# PLOT METHODS FOR PARETO OPTIMIZATION RESULTS
# =============================================================================
# Visualization functions for rctbp_pareto_result objects:
# - Pareto front (default)
# - Convergence trace
# - Search space exploration

# =============================================================================
# S7 METHOD: plot() for rctbp_pareto_result
# =============================================================================

#' Plot Pareto Optimization Results
#'
#' Creates visualizations for Pareto optimization results.
#'
#' @param x An rctbp_pareto_result object
#' @param type Plot type: "pareto" (default), "convergence", "search", or "all"
#' @param ... Additional arguments passed to specific plot functions
#'
#' @return A ggplot2 object or list of plots
#'
#' @name plot.rctbp_pareto_result
#' @export
#' @examples
#' \dontrun{
#' result <- optimize_power_n(design, n_range = c(50, 500), effect_size = 0.3, ...)
#' plot(result)  # Pareto front
#' plot(result, type = "convergence")  # Convergence trace
#' plot(result, type = "search")  # Search space exploration
#' }
S7::method(plot, rctbp_pareto_result) <- function(x, type = "pareto", ...) {
  if (nrow(x@archive) == 0) {
    cli::cli_abort("No results to plot. Run optimization first.")
  }

  type <- match.arg(type, c("pareto", "convergence", "search", "all"))

  if (type == "all") {
    plots <- list(
      pareto = plot_pareto_front(x, ...),
      convergence = plot_pareto_convergence(x, ...),
      search = plot_pareto_search(x, ...)
    )
    return(plots)
  }

  switch(type,
    "pareto" = plot_pareto_front(x, ...),
    "convergence" = plot_pareto_convergence(x, ...),
    "search" = plot_pareto_search(x, ...)
  )
}


# =============================================================================
# PLOT: Pareto Front
# =============================================================================

#' Plot Pareto Front for Pareto Optimization
#'
#' @param result rctbp_pareto_result object
#' @param highlight_selected Logical, highlight the selected knee point
#' @param ... Additional ggplot arguments
#'
#' @return ggplot2 object
#' @keywords internal
plot_pareto_front <- function(result, highlight_selected = TRUE, ...) {
  pareto_df <- result@pareto_front
  archive_df <- result@archive
  objectives <- result@objectives

  if (nrow(pareto_df) == 0) {
    cli::cli_warn("No Pareto-optimal solutions found")
    return(NULL)
  }

  obj_names <- names(objectives)
  if (length(obj_names) < 2) {
    cli::cli_warn("Need at least 2 objectives for Pareto plot")
    return(NULL)
  }

  x_obj <- obj_names[1]
  y_obj <- obj_names[2]

  # Check columns exist
  if (!x_obj %in% names(pareto_df) || !y_obj %in% names(pareto_df)) {
    cli::cli_warn("Objective columns not found in results")
    return(NULL)
  }

  # Axis labels based on optimization type
  axis_labels <- get_pareto_axis_labels(result@optimization_type, x_obj, y_obj)

  # Base plot with all evaluations
  p <- ggplot2::ggplot(archive_df, ggplot2::aes(
    x = .data[[x_obj]],
    y = .data[[y_obj]]
  )) +
    ggplot2::geom_point(alpha = 0.3, color = "gray50")

  # Add Pareto front
  pareto_sorted <- pareto_df[order(pareto_df[[x_obj]]), ]
  p <- p +
    ggplot2::geom_line(
      data = pareto_sorted,
      color = "#0066CC",
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = pareto_sorted,
      color = "#0066CC",
      size = 3
    )

  # Highlight selected design
  if (highlight_selected && nrow(result@selected_design) > 0) {
    selected <- result@selected_design
    if (x_obj %in% names(selected) && y_obj %in% names(selected)) {
      p <- p +
        ggplot2::geom_point(
          data = selected,
          color = "#CC0000",
          size = 5,
          shape = 18
        ) +
        ggplot2::annotate(
          "text",
          x = selected[[x_obj]],
          y = selected[[y_obj]],
          label = "Selected",
          vjust = -1,
          color = "#CC0000",
          fontface = "bold"
        )
    }
  }

  # Title based on optimization type
  title <- switch(result@optimization_type,
    "power_n" = "Power vs Sample Size Trade-off",
    "power_effect" = "Power vs Effect Size Trade-off",
    "effect_n" = "Effect Size vs Sample Size Trade-off",
    "Pareto Front"
  )

  # Styling
  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = paste(nrow(pareto_df), "Pareto-optimal solutions"),
      x = axis_labels$x,
      y = axis_labels$y
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )

  p
}


#' Get Axis Labels for Pareto Plot
#'
#' @param opt_type Optimization type
#' @param x_obj X objective name
#' @param y_obj Y objective name
#'
#' @return List with x and y labels
#' @keywords internal
get_pareto_axis_labels <- function(opt_type, x_obj, y_obj) {
  labels <- list(x = x_obj, y = y_obj)

  # Friendly names
  friendly <- c(
    "pwr_eff" = "Power (Efficacy)",
    "pwr_fut" = "Power (Futility)",
    "n_total" = "Sample Size",
    "b_arm_treat" = "Effect Size"
  )

  if (x_obj %in% names(friendly)) labels$x <- friendly[[x_obj]]
  if (y_obj %in% names(friendly)) labels$y <- friendly[[y_obj]]

  labels
}


# =============================================================================
# PLOT: Convergence
# =============================================================================

#' Plot Convergence for Pareto Optimization
#'
#' @param result rctbp_pareto_result object
#' @param show_all Logical, show all evaluations
#' @param ... Additional ggplot arguments
#'
#' @return ggplot2 object
#' @keywords internal
plot_pareto_convergence <- function(result, show_all = TRUE, ...) {
  if (nrow(result@convergence) == 0) {
    cli::cli_warn("No convergence data available")
    return(NULL)
  }

  conv_df <- result@convergence
  obj_names <- names(result@objectives)
  y_label <- if (length(obj_names) > 0) obj_names[1] else "Objective"

  # Base plot
  p <- ggplot2::ggplot(conv_df, ggplot2::aes(x = .data$eval))

  if (show_all) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$value),
      alpha = 0.4,
      color = "gray50"
    )
  }

  p <- p + ggplot2::geom_line(
    ggplot2::aes(y = .data$best_so_far),
    color = "#0066CC",
    linewidth = 1.2
  )

  p <- p +
    ggplot2::labs(
      title = "Optimization Convergence",
      subtitle = paste("Best after", max(conv_df$eval), "evaluations:",
                       round(conv_df$best_so_far[nrow(conv_df)], 3)),
      x = "Evaluation",
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )

  p
}


# =============================================================================
# PLOT: Search Space
# =============================================================================

#' Plot Search Space for Pareto Optimization
#'
#' @param result rctbp_pareto_result object
#' @param ... Additional ggplot arguments
#'
#' @return ggplot2 object
#' @keywords internal
plot_pareto_search <- function(result, ...) {
  archive_df <- result@archive
  search_params <- names(result@search)

  if (length(search_params) == 0) {
    cli::cli_warn("No search parameters to plot")
    return(NULL)
  }

  # Add evaluation order if not present
  if (!"eval" %in% names(archive_df)) {
    archive_df$eval <- seq_len(nrow(archive_df))
  }

  obj_names <- names(result@objectives)

  if (length(search_params) == 1) {
    param <- search_params[1]
    obj_col <- if (length(obj_names) > 0 && obj_names[1] %in% names(archive_df)) {
      obj_names[1]
    } else {
      "eval"
    }

    p <- ggplot2::ggplot(archive_df, ggplot2::aes(
      x = .data[[param]],
      y = .data[[obj_col]],
      color = .data$eval
    )) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_color_viridis_c(name = "Evaluation") +
      ggplot2::labs(
        title = "Search Space Exploration",
        x = param,
        y = obj_col
      )

  } else {
    x_param <- search_params[1]
    y_param <- search_params[2]

    p <- ggplot2::ggplot(archive_df, ggplot2::aes(
      x = .data[[x_param]],
      y = .data[[y_param]],
      color = .data$eval
    )) +
      ggplot2::geom_point(size = 2, alpha = 0.7) +
      ggplot2::scale_color_viridis_c(name = "Evaluation") +
      ggplot2::labs(
        title = "Search Space Exploration",
        x = x_param,
        y = y_param
      )

    # Highlight selected
    if (nrow(result@selected_design) > 0) {
      selected <- result@selected_design
      if (x_param %in% names(selected) && y_param %in% names(selected)) {
        p <- p +
          ggplot2::geom_point(
            data = selected,
            color = "#CC0000",
            size = 5,
            shape = 18
          )
      }
    }
  }

  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  p
}


# =============================================================================
# S3 WRAPPER FOR PLOT
# =============================================================================

#' @export
#' @rdname plot.rctbp_pareto_result
plot.rctbp_pareto_result <- function(x, type = "pareto", ...) {
  S7::method(plot, rctbp_pareto_result)(x, type = type, ...)
}
