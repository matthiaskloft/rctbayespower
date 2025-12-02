# =============================================================================
# PLOT METHODS FOR OPTIMIZATION RESULTS
# =============================================================================
# Visualization functions for rctbp_optimization_result objects:
# - Convergence plot
# - Pareto front (multi-objective)
# - Search space exploration

# =============================================================================
# S7 METHOD: plot() for rctbp_optimization_result
# =============================================================================

#' Plot Optimization Results
#'
#' Creates visualizations for Bayesian optimization results.
#'
#' @param x An rctbp_optimization_result object
#' @param type Plot type: "convergence" (default), "pareto", "search", or "all"
#' @param ... Additional arguments passed to specific plot functions
#'
#' @return A ggplot2 object or list of plots
#'
#' @name plot.rctbp_optimization_result
#' @export
#' @examples
#' \dontrun{
#' result <- optimization(objectives, n_sims = 200, max_evals = 30)
#' plot(result)  # Convergence plot
#' plot(result, type = "pareto")  # Pareto front (multi-objective)
#' plot(result, type = "search")  # Search space exploration
#' }
S7::method(plot, rctbp_optimization_result) <- function(x, type = "convergence", ...) {
  if (nrow(x@archive) == 0) {
    cli::cli_abort("No results to plot. Run optimization first.")
  }

  type <- match.arg(type, c("convergence", "pareto", "search", "all"))

  if (type == "all") {
    plots <- list(
      convergence = plot_optimization_convergence(x, ...),
      search = plot_optimization_search(x, ...)
    )
    if (x@optimization_type == "multi" && !is.null(x@pareto_front)) {
      plots$pareto <- plot_optimization_pareto(x, ...)
    }
    return(plots)
  }

  switch(type,
    "convergence" = plot_optimization_convergence(x, ...),
    "pareto" = plot_optimization_pareto(x, ...),
    "search" = plot_optimization_search(x, ...)
  )
}

# =============================================================================
# PLOT: Convergence
# =============================================================================

#' Plot Optimization Convergence
#'
#' Shows how the objective value improves over evaluations.
#'
#' @param result rctbp_optimization_result object
#' @param show_all Logical, show all evaluations or just cumulative best
#' @param ... Additional ggplot arguments
#'
#' @return ggplot2 object
#' @export
plot_optimization_convergence <- function(result, show_all = TRUE, ...) {
  if (nrow(result@convergence) == 0) {
    cli::cli_warn("No convergence data available")
    return(NULL)
  }

  conv_df <- result@convergence

  # Base plot
  p <- ggplot2::ggplot(conv_df, ggplot2::aes(x = .data$eval))

  if (show_all) {
    # Show all evaluations as points
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data$value),
      alpha = 0.4,
      color = "gray50"
    )
  }

  # Cumulative best line
  p <- p + ggplot2::geom_line(
    ggplot2::aes(y = .data$best_so_far),
    color = "#0066CC",
    linewidth = 1.2
  )

  # Styling
  obj_names <- names(result@objectives@objectives)
  y_label <- if (length(obj_names) > 0) obj_names[1] else "Objective"

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
# PLOT: Pareto Front
# =============================================================================

#' Plot Pareto Front
#'
#' Visualizes the Pareto-optimal solutions for multi-objective optimization.
#'
#' @param result rctbp_optimization_result object
#' @param highlight_best Logical, highlight the "knee" point
#' @param ... Additional ggplot arguments
#'
#' @return ggplot2 object
#' @export
plot_optimization_pareto <- function(result, highlight_best = TRUE, ...) {
  if (result@optimization_type != "multi" || is.null(result@pareto_front)) {
    cli::cli_warn("Pareto front only available for multi-objective optimization")
    return(NULL)
  }

  pareto_df <- result@pareto_front
  archive_df <- result@archive

  # Get objective names
  obj_names <- names(result@objectives@objectives)
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

  # Highlight "knee" point (closest to ideal)
  if (highlight_best && nrow(pareto_df) > 0) {
    # Simple utopia distance (normalized)
    obj1_range <- range(pareto_df[[x_obj]])
    obj2_range <- range(pareto_df[[y_obj]])

    obj1_norm <- (pareto_df[[x_obj]] - obj1_range[1]) / diff(obj1_range)
    obj2_norm <- (pareto_df[[y_obj]] - obj2_range[1]) / diff(obj2_range)

    # For both minimize, closest to (0,0) in normalized space
    utopia_dist <- sqrt(obj1_norm^2 + obj2_norm^2)
    knee_idx <- which.min(utopia_dist)
    knee_point <- pareto_df[knee_idx, , drop = FALSE]

    p <- p +
      ggplot2::geom_point(
        data = knee_point,
        color = "#CC0000",
        size = 5,
        shape = 18
      ) +
      ggplot2::annotate(
        "text",
        x = knee_point[[x_obj]],
        y = knee_point[[y_obj]],
        label = "Knee",
        vjust = -1,
        color = "#CC0000",
        fontface = "bold"
      )
  }

  # Styling
  p <- p +
    ggplot2::labs(
      title = "Pareto Front",
      subtitle = paste(nrow(pareto_df), "Pareto-optimal solutions"),
      x = x_obj,
      y = y_obj
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )

  p
}

# =============================================================================
# PLOT: Search Space Exploration
# =============================================================================

#' Plot Search Space Exploration
#'
#' Visualizes how the optimizer explored the search space.
#'
#' @param result rctbp_optimization_result object
#' @param color_by Column to color points by (default: evaluation order)
#' @param ... Additional ggplot arguments
#'
#' @return ggplot2 object
#' @export
plot_optimization_search <- function(result, color_by = "eval", ...) {
  archive_df <- result@archive
  search_params <- names(result@objectives@search)

  if (length(search_params) == 0) {
    cli::cli_warn("No search parameters to plot")
    return(NULL)
  }

  # Add evaluation order if not present
  if (!"eval" %in% names(archive_df)) {
    archive_df$eval <- seq_len(nrow(archive_df))
  }

  if (length(search_params) == 1) {
    # 1D: show evaluations over parameter
    param <- search_params[1]
    obj_names <- names(result@objectives@objectives)
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

  } else if (length(search_params) == 2) {
    # 2D: scatter plot of search space
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

    # Highlight best
    if (nrow(result@result) > 0) {
      best <- result@result[1, , drop = FALSE]
      if (x_param %in% names(best) && y_param %in% names(best)) {
        p <- p +
          ggplot2::geom_point(
            data = best,
            color = "#CC0000",
            size = 5,
            shape = 18
          )
      }
    }

  } else {
    # >2D: pairs plot or faceted
    # Use first two params for simplicity
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
        title = paste("Search Space Exploration (", x_param, "vs", y_param, ")"),
        subtitle = paste("Additional params:", paste(search_params[-(1:2)], collapse = ", ")),
        x = x_param,
        y = y_param
      )
  }

  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )

  p
}

# =============================================================================
# S3 WRAPPER FOR PLOT
# =============================================================================

#' @export
#' @rdname plot.rctbp_optimization_result
plot.rctbp_optimization_result <- function(x, type = "convergence", ...) {
  S7::method(plot, rctbp_optimization_result)(x, type = type, ...)
}
