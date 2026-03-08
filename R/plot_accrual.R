# =============================================================================
# ACCRUAL PLOT
# =============================================================================
# Enrollment curve visualization showing enrolled vs analyzable patients
# over calendar time, with analysis timepoint markers.

#' Create Accrual Plot
#'
#' Visualizes enrollment and data availability over calendar time for
#' sequential designs with accrual modeling. Shows enrolled vs analyzable
#' patient counts at each analysis look.
#'
#' @param plot_data Data frame with per-look results (from results_interim)
#' @param conditions An rctbp_conditions object
#' @param ... Additional arguments (currently unused)
#'
#' @return A ggplot2 object
#' @keywords internal
create_accrual_plot <- function(plot_data, conditions, ...) {
  # Validate required columns
  required_cols <- c("id_cond", "id_look", "n_analyzed",
                     "calendar_time_mn", "n_enrolled_mn")
  missing_cols <- setdiff(required_cols, names(plot_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Accrual plot requires columns: {.val {required_cols}}",
      "x" = "Missing: {.val {missing_cols}}",
      "i" = "Ensure accrual modeling is enabled via {.arg accrual_rate} in {.fn build_conditions}"
    ))
  }

  # Deduplicate across par_name (same look data repeated per parameter)
  dedup_key <- paste(plot_data$id_cond, plot_data$id_look, sep = "|||")
  look_data <- plot_data[!duplicated(dedup_key), required_cols, drop = FALSE]

  # Add n_total from conditions grid for labeling
  if ("n_total" %in% names(conditions@grid)) {
    look_data <- merge(look_data, conditions@grid[, c("id_cond", "n_total"), drop = FALSE],
                       by = "id_cond", all.x = TRUE)
  }

  # Create condition label for faceting
  if ("n_total" %in% names(look_data)) {
    look_data$condition <- paste0("n=", look_data$n_total, " (id:", look_data$id_cond, ")")
  } else {
    look_data$condition <- paste0("Condition ", look_data$id_cond)
  }

  # Pivot to long format: enrolled vs analyzable
  enrolled_df <- data.frame(
    id_cond = look_data$id_cond,
    id_look = look_data$id_look,
    condition = look_data$condition,
    calendar_time = look_data$calendar_time_mn,
    count = look_data$n_enrolled_mn,
    status = "Enrolled",
    stringsAsFactors = FALSE
  )

  analyzed_df <- data.frame(
    id_cond = look_data$id_cond,
    id_look = look_data$id_look,
    condition = look_data$condition,
    calendar_time = look_data$calendar_time_mn,
    count = look_data$n_analyzed,
    status = "Analyzable",
    stringsAsFactors = FALSE
  )

  long_data <- rbind(enrolled_df, analyzed_df)
  long_data$status <- factor(long_data$status, levels = c("Enrolled", "Analyzable"))

  n_conditions <- length(unique(long_data$id_cond))

  accrual_colors <- c("Enrolled" = "steelblue", "Analyzable" = "forestgreen")

  p <- ggplot2::ggplot(long_data,
                        ggplot2::aes(x = .data$calendar_time,
                                     y = .data$count,
                                     color = .data$status)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_color_manual(values = accrual_colors, name = NULL) +
    ggplot2::labs(
      title = "Enrollment & Data Availability Over Calendar Time",
      x = "Calendar Time",
      y = "Number of Patients"
    ) +
    rctbp_theme()

  # Facet by condition if multiple

  if (n_conditions > 1) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$condition))
  }

  # Add vertical dashed lines at analysis timepoints (per condition)
  analysis_times <- look_data[, c("condition", "calendar_time_mn"), drop = FALSE]
  names(analysis_times)[2] <- "xintercept"
  p <- p + ggplot2::geom_vline(
    data = analysis_times,
    ggplot2::aes(xintercept = .data$xintercept),
    inherit.aes = FALSE,
    linetype = "dashed", color = "gray50", alpha = 0.5
  )

  p
}
