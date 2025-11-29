# =============================================================================
# PLOT HELPERS
# =============================================================================
# Shared utilities for power analysis plotting functions.

#' Pivot plot data to long format for ggplot2
#'
#' Transforms wide-format power analysis results to long format suitable for
#' ggplot2 visualization. Handles both value columns and optional standard errors.
#'
#' @param plot_data Data frame with power analysis results
#' @param decision Filter: "success", "futility", or "both"
#' @param metric Filter: "power", "prob", or "both"
#' @param include_se Whether to include standard error columns
#'
#' @return Data frame in long format with columns: measure, outcome, value, and optionally se
#' @keywords internal
pivot_plot_data_long <- function(plot_data, decision, metric, include_se = FALSE) {
  # Determine columns to pivot
  value_cols <- c("pwr_eff", "pwr_fut", "pr_eff", "pr_fut")

  # Pivot power and probability columns to long format
  plot_data_long <- plot_data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = c("measure", "outcome"),
      names_pattern = "(pwr|pr)_(eff|fut)",
      values_to = "value"
    ) |>
    dplyr::mutate(
      measure = dplyr::case_match(
        .data$measure,
        "pwr" ~ "Power",
        "pr" ~ "Probability"
      ),
      outcome = dplyr::case_match(
        .data$outcome,
        "eff" ~ "Efficacy",
        "fut" ~ "Futility"
      )
    )

  # Add standard errors if requested and available
  if (include_se) {
    se_cols <- c("se_pwr_eff", "se_pwr_fut", "se_pr_eff", "se_pr_fut")
    available_se_cols <- se_cols[se_cols %in% names(plot_data)]

    if (length(available_se_cols) == 0) {
      cli::cli_warn(c(
        "Cannot show MCSE ribbons - no standard error columns found in data",
        "i" = "Expected columns: {.val {se_cols}}"
      ))
    } else if (length(available_se_cols) < length(se_cols)) {
      cli::cli_warn(c(
        "Some MCSE columns missing - ribbons may be incomplete",
        "i" = "Found: {.val {available_se_cols}}",
        "i" = "Missing: {.val {setdiff(se_cols, available_se_cols)}}"
      ))
    }

    if (length(available_se_cols) > 0) {
      # Pivot SE columns similarly
      se_data <- plot_data |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(available_se_cols),
          names_to = c("se_measure", "se_outcome"),
          names_pattern = "se_(pwr|pr)_(eff|fut)",
          values_to = "se"
        ) |>
        dplyr::mutate(
          se_measure = dplyr::case_match(.data$se_measure, "pwr" ~ "Power", "pr" ~ "Probability"),
          se_outcome = dplyr::case_match(.data$se_outcome, "eff" ~ "Efficacy", "fut" ~ "Futility")
        ) |>
        dplyr::select(-dplyr::any_of(value_cols))

      # Join SE data - need to match on all common columns plus measure/outcome
      join_cols <- intersect(names(plot_data_long), names(se_data))
      join_cols <- setdiff(join_cols, c("se", "se_measure", "se_outcome"))

      plot_data_long <- plot_data_long |>
        dplyr::left_join(
          se_data |> dplyr::rename(measure = "se_measure", outcome = "se_outcome"),
          by = c(join_cols, "measure", "outcome")
        )
    }
  }

  # Filter based on decision parameter
  if (decision == "success") {
    plot_data_long <- dplyr::filter(plot_data_long, .data$outcome == "Efficacy")
  } else if (decision == "futility") {
    plot_data_long <- dplyr::filter(plot_data_long, .data$outcome == "Futility")
  }

  # Filter based on metric parameter
  if (metric == "power") {
    plot_data_long <- dplyr::filter(plot_data_long, .data$measure == "Power")
  } else if (metric == "prob") {
    plot_data_long <- dplyr::filter(plot_data_long, .data$measure == "Probability")
  }

  plot_data_long
}

#' Standard color palette for power analysis plots
#'
#' Returns named vector of colors for efficacy/futility outcomes.
#'
#' @return Named character vector of colors
#' @keywords internal
rctbp_colors <- function() {
  c(
    "Efficacy" = "steelblue",
    "Futility" = "darkred"
  )
}

#' Standard ggplot2 theme for power analysis plots
#'
#' Returns a minimal theme with standard formatting for power analysis plots.
#'
#' @return A ggplot2 theme object
#' @keywords internal
rctbp_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      # Facet separation
      panel.spacing = ggplot2::unit(0.8, "lines"),
      panel.border = ggplot2::element_rect(
        color = "gray80",
        fill = NA,
        linewidth = 0.5
      ),
      strip.background = ggplot2::element_rect(
        fill = "gray95",
        color = "gray80",
        linewidth = 0.5
      ),
      strip.text = ggplot2::element_text(face = "bold", size = 10)
    )
}

#' Convert ggplot2 object to interactive plotly
#'
#' Wrapper around ggplotly with standard legend positioning.
#'
#' @param p A ggplot2 object
#' @param legend_position Legend position: "bottom" (default) or "right"
#'
#' @return A plotly object
#' @keywords internal
to_interactive <- function(p, legend_position = "bottom") {
  pl <- plotly::ggplotly(p)

  if (legend_position == "bottom") {
    pl <- pl |>
      plotly::layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)
      )
  }

  pl
}
