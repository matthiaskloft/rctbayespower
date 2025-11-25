#' Report Renderers for Different Output Modes
#'
#' Render structured report data in CLI or Markdown format.
#'
#' @name report_renderers
#' @keywords internal
#' @importFrom stats setNames
NULL

#' Render Report in CLI Mode
#'
#' Renders a structured report using cli package functions.
#'
#' @param report List with report data from build_report.*()
#' @return NULL (outputs to console)
#' @keywords internal
#'
render_cli <- function(report) {
  # Title and status
  if (!is.null(report$status)) {
    status_msg <- paste0("STATUS: [", report$status, "] ",
                         ifelse(report$status == "COMPLETED", "Analysis completed", "Analysis not yet run"))
    if (report$status == "COMPLETED") {
      cli::cli_alert_success(status_msg)
    } else {
      cli::cli_alert_info(status_msg)
    }
  }

  cli::cli_h2(report$title)
  cli::cli_rule()

  # Render sections
  for (section in report$sections) {
    if (is.null(section)) next

    cli::cli_h3(section$name)

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
      print(section$grid)
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
#' @return NULL (outputs to console)
#' @keywords internal
#'
render_markdown <- function(report) {
  output <- character()

  # Title
  output <- c(output, paste0("## ", report$title), "")

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

    output <- c(output, paste0("### ", section$name), "")

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
      output <- c(output, "```")
      grid_output <- utils::capture.output(print(section$grid))
      output <- c(output, grid_output)
      output <- c(output, "```", "")
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

#' Render Report Based on Current Output Mode
#'
#' Routes to appropriate renderer based on output mode setting.
#'
#' @param report List with report data from build_report.*()
#' @return NULL (outputs to console)
#' @keywords internal
#'
render_report <- function(report) {
  mode <- get_output_mode()

  if (mode == "cli") {
    render_cli(report)
  } else {
    render_markdown(report)
  }

  invisible(NULL)
}
