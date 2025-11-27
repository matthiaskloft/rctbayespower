#' Report Renderers for Different Output Modes
#'
#' Render structured report data in CLI or Markdown format.
#'
#' @name report_renderers
#' @keywords internal
#' @importFrom stats setNames
NULL


# =============================================================================
# TABLE FORMATTING HELPERS
# =============================================================================

#' Format a Data Frame as a CLI Table
#'
#' Creates a nicely formatted table for CLI output with proper alignment.
#'
#' @param df Data frame to format
#' @param max_rows Maximum rows to show (default 20)
#' @return Character vector of formatted lines
#' @keywords internal
format_table_cli <- function(df, max_rows = 20) {
 if (nrow(df) == 0) {
    return("(no data)")
  }

  # Convert to data frame if tibble
 df <- as.data.frame(df)

  # Truncate if too many rows
  truncated <- FALSE
  if (nrow(df) > max_rows) {
    df <- df[1:max_rows, , drop = FALSE]
    truncated <- TRUE
  }

  # Format numeric columns
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      # Detect if likely a proportion (0-1 range)
      vals <- df[[col]][!is.na(df[[col]])]
      if (length(vals) > 0 && all(vals >= 0 & vals <= 1)) {
        # Format as percentage
        df[[col]] <- sprintf("%.1f%%", df[[col]] * 100)
      } else if (all(abs(vals) < 100) && any(vals != floor(vals))) {
        # Small decimals - show 2 decimal places
        df[[col]] <- sprintf("%.2f", df[[col]])
      } else {
        # Integers or large numbers - no decimals
        df[[col]] <- sprintf("%.0f", df[[col]])
      }
    } else if (is.list(df[[col]])) {
      # Handle list columns (like par_name)
      df[[col]] <- sapply(df[[col]], function(x) {
        if (is.null(x)) "NULL" else paste(x, collapse = ", ")
      })
    } else {
      df[[col]] <- as.character(df[[col]])
    }
    df[[col]][is.na(df[[col]])] <- "-"
  }

  # Calculate column widths
  col_widths <- sapply(names(df), function(col) {
    max(nchar(col), max(nchar(df[[col]]), na.rm = TRUE))
  })

  # Create header
  header <- paste(
    mapply(function(name, width) sprintf(paste0("%-", width, "s"), name),
           names(df), col_widths),
    collapse = "  "
  )

  # Create separator
  separator <- paste(sapply(col_widths, function(w) strrep("\u2500", w)), collapse = "  ")

  # Create rows
  rows <- apply(df, 1, function(row) {
    paste(
      mapply(function(val, width) sprintf(paste0("%", width, "s"), val),
             row, col_widths),
      collapse = "  "
    )
  })

  # Combine
  result <- c(header, separator, rows)

  if (truncated) {
    result <- c(result, paste0("... (", nrow(df), " of ", max_rows, "+ rows shown)"))
  }

  result
}


#' Format a Data Frame as a Markdown Table
#'
#' Creates a markdown-formatted table with proper alignment.
#'
#' @param df Data frame to format
#' @param max_rows Maximum rows to show (default 20)
#' @return Character vector of formatted lines
#' @keywords internal
format_table_markdown <- function(df, max_rows = 20) {
  if (nrow(df) == 0) {
    return("_(no data)_")
  }

  # Convert to data frame if tibble
  df <- as.data.frame(df)

  # Truncate if too many rows
  truncated <- FALSE
  original_nrow <- nrow(df)
  if (nrow(df) > max_rows) {
    df <- df[1:max_rows, , drop = FALSE]
    truncated <- TRUE
  }

  # Detect numeric columns for alignment
  is_numeric <- sapply(df, is.numeric)

  # Format values
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      vals <- df[[col]][!is.na(df[[col]])]
      if (length(vals) > 0 && all(vals >= 0 & vals <= 1)) {
        df[[col]] <- sprintf("%.1f%%", df[[col]] * 100)
      } else if (all(abs(vals) < 100) && any(vals != floor(vals))) {
        df[[col]] <- sprintf("%.2f", df[[col]])
      } else {
        df[[col]] <- sprintf("%.0f", df[[col]])
      }
    } else if (is.list(df[[col]])) {
      df[[col]] <- sapply(df[[col]], function(x) {
        if (is.null(x)) "NULL" else paste(x, collapse = ", ")
      })
    } else {
      df[[col]] <- as.character(df[[col]])
    }
    df[[col]][is.na(df[[col]])] <- "-"
  }

  # Calculate column widths
  col_widths <- sapply(names(df), function(col) {
    max(nchar(col), max(nchar(df[[col]]), na.rm = TRUE))
  })

  # Create header row
  header <- paste0("| ", paste(
    mapply(function(name, width) sprintf(paste0("%-", width, "s"), name),
           names(df), col_widths),
    collapse = " | "
  ), " |")

  # Create alignment row
  align_row <- paste0("| ", paste(
    mapply(function(numeric, width) {
      if (numeric) {
        paste0(strrep("-", width - 1), ":")  # Right align for numbers
      } else {
        strrep("-", width)  # Left align for text
      }
    }, is_numeric, col_widths),
    collapse = " | "
  ), " |")

  # Create data rows
  rows <- apply(df, 1, function(row) {
    paste0("| ", paste(
      mapply(function(val, width) sprintf(paste0("%", width, "s"), val),
             row, col_widths),
      collapse = " | "
    ), " |")
  })

  result <- c(header, align_row, rows)

  if (truncated) {
    result <- c(result, "", paste0("_... (showing ", max_rows, " of ", original_nrow, " rows)_"))
  }

  result
}

#' Render Report in CLI Mode
#'
#' Renders a structured report using cli package functions.
#'
#' @param report List with report data from build_report.*()
#' @return NULL (outputs to console)
#' @keywords internal
#'
render_cli <- function(report) {
  # Title
  cli::cli_h1(report$title)

  # Status
  if (!is.null(report$status)) {
    if (report$status == "COMPLETED") {
      cli::cli_alert_success("Analysis completed")
    } else {
      cli::cli_alert_info("Analysis not yet run")
    }
    cli::cli_text("")
  }

  # Render sections
  for (section in report$sections) {
    if (is.null(section)) next

    cli::cli_h2(section$name)

    # Items as definition list
    if (!is.null(section$items)) {
      items_vec <- sapply(names(section$items), function(key) {
        paste0("{.strong ", key, "}: ", section$items[[key]])
      })
      names(items_vec) <- rep("*", length(items_vec))
      cli::cli_bullets(items_vec)
    }

    # Power ranges
    if (!is.null(section$power_ranges)) {
      cli::cli_text("")
      cli::cli_text("{.emph Power ranges:}")
      for (pr in section$power_ranges) {
        cli::cli_bullets(c("*" = paste0("{.field ", pr$name, "}: ", pr$range)))
      }
    }

    # Actions
    if (!is.null(section$actions)) {
      for (action in section$actions) {
        cli::cli_bullets(c("*" = action))
      }
    }

    # Optimal condition
    if (!is.null(section$optimal_condition)) {
      oc <- section$optimal_condition
      if (isTRUE(oc$found)) {
        bullets <- c(
          "*" = paste0("{.strong Achieved power}: ", oc$achieved_pwr),
          "*" = paste0("{.strong Sample size (n_total)}: ", oc$n_total),
          "*" = paste0("{.strong Condition ID}: ", oc$condition_id),
          "*" = paste0("{.strong Parameters}: ", oc$params)
        )
        # Add target power if in target mode
        if (!is.null(oc$target_pwr)) {
          bullets <- c("*" = paste0("{.strong Target power}: ", oc$target_pwr), bullets)
        }
        cli::cli_bullets(bullets)

        # Add interim stats if available
        if (!is.null(oc$interim)) {
          cli::cli_text("")
          cli::cli_text("{.emph Early stopping (this condition):}")
          cli::cli_bullets(c(
            "*" = paste0("{.field Mean N}: ", oc$interim$n_mn),
            "*" = paste0("{.field Median N}: ", oc$interim$n_mdn),
            "*" = paste0("{.field Mode N}: ", oc$interim$n_mode, " (", oc$interim$prop_at_mode, " of trials)"),
            "*" = paste0("{.field Stopped early}: ", oc$interim$prop_stp_early),
            "*" = paste0("{.field Stopped for success}: ", oc$interim$prop_stp_scs),
            "*" = paste0("{.field Stopped for futility}: ", oc$interim$prop_stp_ftl),
            "*" = paste0("{.field No decision}: ", oc$interim$prop_no_dec)
          ))
        }
      } else {
        cli::cli_alert_warning("No condition achieves the target power of {oc$target_pwr}")
        cli::cli_bullets(c(
          "*" = paste0("{.strong Closest power}: ", oc$closest_pwr),
          "*" = paste0("{.strong Sample size (n_total)}: ", oc$closest_n),
          "*" = paste0("{.strong Condition ID}: ", oc$closest_id),
          "*" = paste0("{.strong Parameters}: ", oc$params)
        ))

        # Add interim stats for closest if available
        if (!is.null(oc$interim)) {
          cli::cli_text("")
          cli::cli_text("{.emph Early stopping (closest condition):}")
          cli::cli_bullets(c(
            "*" = paste0("{.field Mean N}: ", oc$interim$n_mn),
            "*" = paste0("{.field Median N}: ", oc$interim$n_mdn),
            "*" = paste0("{.field Mode N}: ", oc$interim$n_mode, " (", oc$interim$prop_at_mode, " of trials)"),
            "*" = paste0("{.field Stopped early}: ", oc$interim$prop_stp_early),
            "*" = paste0("{.field Stopped for success}: ", oc$interim$prop_stp_scs),
            "*" = paste0("{.field Stopped for futility}: ", oc$interim$prop_stp_ftl),
            "*" = paste0("{.field No decision}: ", oc$interim$prop_no_dec)
          ))
        }
      }
    }

    # BRMS args
    if (!is.null(section$brms_args)) {
      cli::cli_text("{.strong BRMS arguments:}")
      for (arg_name in names(section$brms_args)) {
        cli::cli_bullets(c("*" = paste0("{.code ", arg_name, "}: ", section$brms_args[[arg_name]])))
      }
    }

    # Note
    if (!is.null(section$note)) {
      cli::cli_text("")
      cli::cli_alert_info(section$note)
    }

    # Grid (data frame)
    if (!is.null(section$grid)) {
      cli::cli_text("")
      table_lines <- format_table_cli(section$grid)
      cat(paste(table_lines, collapse = "\n"), "\n")
    }

    # BRMS model
    if (!is.null(section$brms_model)) {
      cli::cli_text("")
      print(section$brms_model)
    }

    # Bayesflow model
    if (!is.null(section$bayesflow_model)) {
      cli::cli_text("")
      print(section$bayesflow_model)
    }

    # Backend args
    if (!is.null(section$backend_args)) {
      cli::cli_text("")
      print(section$backend_args)
    }

    cli::cli_text("")  # Blank line between sections
  }

  invisible(NULL)
}

#' Render Report in Markdown Mode
#'
#' Renders a structured report in markdown format.
#'
#' @param report List with report data from build_report.*()
#' @param heading_level Integer specifying the starting heading level (default 2)
#' @return NULL (outputs to console)
#' @keywords internal
#'
render_markdown <- function(report, heading_level = 2L) {
  output <- character()

  # Helper to create heading with the appropriate level
  make_heading <- function(text, level_offset = 0) {
    level <- min(heading_level + level_offset, 6)  # Cap at h6
    paste0(strrep("#", level), " ", text)
  }

  # Title (primary heading)
  output <- c(output, make_heading(report$title, 0), "")

  # Status
  if (!is.null(report$status)) {
    status_symbol <- ifelse(report$status == "COMPLETED", "\u2714", "\u2139")  # ✔ or ℹ
    status_text <- paste0("**STATUS**: [", report$status, "] ",
                         ifelse(report$status == "COMPLETED", "Analysis completed", "Analysis not yet run"))
    output <- c(output, paste0(status_symbol, " ", status_text), "")
  }

  output <- c(output, "---", "")

  # Render sections
  for (section in report$sections) {
    if (is.null(section)) next

    output <- c(output, make_heading(section$name, 1), "")

    # Items as bullet list
    if (!is.null(section$items)) {
      for (key in names(section$items)) {
        output <- c(output, paste0("- **", key, "**: ", section$items[[key]]))
      }
      output <- c(output, "")
    }

    # Power ranges
    if (!is.null(section$power_ranges)) {
      output <- c(output, "_Power ranges:_", "")
      for (pr in section$power_ranges) {
        output <- c(output, paste0("- **", pr$name, "**: ", pr$range))
      }
      output <- c(output, "")
    }

    # Actions
    if (!is.null(section$actions)) {
      for (action in section$actions) {
        output <- c(output, paste0("- ", action))
      }
      output <- c(output, "")
    }

    # Optimal condition
    if (!is.null(section$optimal_condition)) {
      oc <- section$optimal_condition
      if (isTRUE(oc$found)) {
        # Add target power only if in target mode
        if (!is.null(oc$target_pwr)) {
          output <- c(output, paste0("- **Target power**: ", oc$target_pwr))
        }
        output <- c(output,
          paste0("- **Achieved power**: ", oc$achieved_pwr),
          paste0("- **Sample size (n_total)**: ", oc$n_total),
          paste0("- **Condition ID**: ", oc$condition_id),
          paste0("- **Parameters**: ", oc$params)
        )

        # Add interim stats if available
        if (!is.null(oc$interim)) {
          output <- c(output,
            "",
            "_Early stopping (this condition):_",
            paste0("- Mean N: ", oc$interim$n_mn),
            paste0("- Median N: ", oc$interim$n_mdn),
            paste0("- Mode N: ", oc$interim$n_mode, " (", oc$interim$prop_at_mode, " of trials)"),
            paste0("- Stopped early: ", oc$interim$prop_stp_early),
            paste0("- Stopped for success: ", oc$interim$prop_stp_scs),
            paste0("- Stopped for futility: ", oc$interim$prop_stp_ftl),
            paste0("- No decision: ", oc$interim$prop_no_dec)
          )
        }
        output <- c(output, "")
      } else {
        output <- c(output,
          paste0("\u26A0\uFE0F No condition achieves the target power of ", oc$target_pwr),
          "",
          paste0("- **Closest power**: ", oc$closest_pwr),
          paste0("- **Sample size (n_total)**: ", oc$closest_n),
          paste0("- **Condition ID**: ", oc$closest_id),
          paste0("- **Parameters**: ", oc$params)
        )

        # Add interim stats for closest if available
        if (!is.null(oc$interim)) {
          output <- c(output,
            "",
            "_Early stopping (closest condition):_",
            paste0("- Mean N: ", oc$interim$n_mn),
            paste0("- Median N: ", oc$interim$n_mdn),
            paste0("- Mode N: ", oc$interim$n_mode, " (", oc$interim$prop_at_mode, " of trials)"),
            paste0("- Stopped early: ", oc$interim$prop_stp_early),
            paste0("- Stopped for success: ", oc$interim$prop_stp_scs),
            paste0("- Stopped for futility: ", oc$interim$prop_stp_ftl),
            paste0("- No decision: ", oc$interim$prop_no_dec)
          )
        }
        output <- c(output, "")
      }
    }

    # BRMS args
    if (!is.null(section$brms_args)) {
      output <- c(output, "**BRMS arguments:**", "")
      for (arg_name in names(section$brms_args)) {
        output <- c(output, paste0("- `", arg_name, "`: ", section$brms_args[[arg_name]]))
      }
      output <- c(output, "")
    }

    # Note
    if (!is.null(section$note)) {
      output <- c(output, "> **Note**: ", paste0("> ", strsplit(section$note, "\n")[[1]]), "")
    }

    # Grid (data frame)
    if (!is.null(section$grid)) {
      table_lines <- format_table_markdown(section$grid)
      output <- c(output, table_lines, "")
    }

    # BRMS model
    if (!is.null(section$brms_model)) {
      output <- c(output, "```")
      model_output <- utils::capture.output(print(section$brms_model))
      output <- c(output, model_output)
      output <- c(output, "```", "")
    }

    # Bayesflow model
    if (!is.null(section$bayesflow_model)) {
      output <- c(output, "```")
      model_output <- utils::capture.output(print(section$bayesflow_model))
      output <- c(output, model_output)
      output <- c(output, "```", "")
    }

    # Backend args
    if (!is.null(section$backend_args)) {
      output <- c(output, "```")
      args_output <- utils::capture.output(print(section$backend_args))
      output <- c(output, args_output)
      output <- c(output, "```", "")
    }
  }

  # Output everything as one block
  cat(paste(output, collapse = "\n"), "\n")
  invisible(NULL)
}

#' Render Report Based on Output Format
#'
#' Routes to appropriate renderer based on specified format or global setting.
#'
#' @param report List with report data from build_report.*()
#' @param format Output format: "cli" or "markdown". If NULL (default), uses
#'   the global output mode setting from [get_output_mode()].
#' @param heading_level Integer specifying the starting heading level for
#'   markdown output (default 2). Ignored for CLI format.
#' @return NULL (outputs to console)
#' @keywords internal
#'
render_report <- function(report, format = NULL, heading_level = 2L) {
  # Use explicit format if provided, otherwise fall back to global setting
  if (is.null(format)) {
    format <- get_output_mode()
  }

  if (format == "cli") {
    render_cli(report)
  } else {
    render_markdown(report, heading_level = heading_level)
  }

  invisible(NULL)
}
