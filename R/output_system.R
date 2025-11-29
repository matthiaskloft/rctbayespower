#' Output Mode System for rctbayespower
#'
#' Provides dual-mode output rendering (CLI or Markdown) for all package output.
#'
#' @name output_system
#' @keywords internal
NULL

#' Get Current Output Mode
#'
#' Returns the current output mode setting for the package.
#'
#' @return Character string: "cli" or "markdown"
#' @export
#'
#' @examples
#' get_output_mode()
#'
get_output_mode <- function() {
  mode <- getOption("rctbayespower.output_mode", default = "cli")

  if (!mode %in% c("cli", "markdown")) {
    cli::cli_warn(c(
      "Invalid output mode: {.val {mode}}",
      "i" = "Using default {.val cli}"
    ))
    mode <- "cli"
  }

  return(mode)
}

#' Set Output Mode
#'
#' Sets the output mode for all package output.
#'
#' @param mode Character string: "cli" for styled console output (default)
#'   or "markdown" for markdown-formatted output that can be exported.
#'
#' @return Invisibly returns the previous mode setting.
#' @export
#'
#' @examples
#' # Set to markdown mode
#' set_output_mode("markdown")
#'
#' # Set back to CLI mode
#' set_output_mode("cli")
#'
set_output_mode <- function(mode = c("cli", "markdown")) {
  mode <- match.arg(mode)
  old_mode <- getOption("rctbayespower.output_mode", default = "cli")
  options(rctbayespower.output_mode = mode)
  invisible(old_mode)
}

#' Temporarily Change Output Mode
#'
#' Executes code with a temporary output mode setting.
#'
#' @param mode Character string: "cli" or "markdown"
#' @param code Code to execute with the temporary mode
#'
#' @return Result of evaluating `code`
#' @export
#'
#' @examples
#' \dontrun{
#' # Print in markdown mode temporarily
#' with_output_mode("markdown", {
#'   print(my_power_analysis)
#' })
#' }
#'
with_output_mode <- function(mode = c("cli", "markdown"), code) {
  mode <- match.arg(mode)
  old_mode <- set_output_mode(mode)
  on.exit(options(rctbayespower.output_mode = old_mode))
  force(code)
}

#' Export Report to File
#'
#' Exports a report for an rctbayespower object to a markdown file.
#'
#' @param object An S7 object (rctbp_model, rctbp_design, rctbp_conditions, or rctbp_power_analysis)
#' @param file Character string: path to output file
#' @param format Character string: output format (currently only "markdown" supported)
#'
#' @return Invisibly returns the file path
#' @export
#'
#' @examples
#' \dontrun{
#' design <- build_design(model_name = "ancova_cont_2arms",
#'                        target_params = "b_arm2",
#'                        p_sig_scs = 0.975, p_sig_ftl = 0.5)
#' export_report(design, "design_report.md")
#' }
#'
export_report <- function(object, file, format = "markdown") {
  if (format != "markdown") {
    cli::cli_abort(c(
      "Unsupported format: {.val {format}}",
      "i" = "Currently only {.val markdown} is supported"
    ))
  }

  # Capture output in markdown mode
  output <- with_output_mode("markdown", {
    utils::capture.output(print(object))
  })

  # Write to file
  writeLines(output, file)

  cli::cli_alert_success("Report exported to {.file {file}}")
  invisible(file)
}

#' Convert CLI Semantic Element to Markdown
#'
#' Internal helper to route output based on current mode.
#'
#' @param cli_fn Function to call in CLI mode
#' @param md_text Text to output in markdown mode
#'
#' @return NULL (outputs to console)
#' @keywords internal
#'
cli_or_md <- function(cli_fn, md_text) {
  if (get_output_mode() == "cli") {
    cli_fn()
  } else {
    cat(md_text, "\n", sep = "")
  }
  invisible(NULL)
}

#' Markdown Helper Functions
#'
#' Convert semantic output elements to markdown format.
#'
#' @name markdown_helpers
#' @keywords internal
NULL

#' @rdname markdown_helpers
md_h1 <- function(text) {
  paste0("# ", text, "\n")
}

#' @rdname markdown_helpers
md_h2 <- function(text) {
  paste0("## ", text, "\n")
}

#' @rdname markdown_helpers
md_h3 <- function(text) {
  paste0("### ", text, "\n")
}

#' @rdname markdown_helpers
md_rule <- function() {
  "---\n"
}

#' @rdname markdown_helpers
md_alert <- function(text, type = c("info", "success", "warning", "danger")) {
  type <- match.arg(type)
  symbol <- switch(type,
    info = "\u2139",      # ℹ
    success = "\u2714",   # ✔
    warning = "!",
    danger = "\u2716"     # ✖
  )
  paste0(symbol, " ", text, "\n")
}

#' @rdname markdown_helpers
md_bullet <- function(items, indent = 0) {
  prefix <- paste0(rep("  ", indent), collapse = "")
  paste0(prefix, "- ", items, "\n", collapse = "")
}

#' @rdname markdown_helpers
md_code <- function(text) {
  paste0("`", text, "`")
}

#' @rdname markdown_helpers
md_code_block <- function(text, lang = "") {
  paste0("```", lang, "\n", text, "\n```\n")
}

#' Build Output as Character Vector
#'
#' Helper to collect output sections before rendering.
#'
#' @return Character vector that can be collapsed and rendered
#' @keywords internal
#'
new_output_builder <- function() {
  structure(character(), class = "output_builder")
}

#' @keywords internal
add_line <- function(builder, ...) {
  c(builder, paste0(...))
}

#' @keywords internal
add_blank <- function(builder) {
  c(builder, "")
}

#' @keywords internal
render_output <- function(builder) {
  cat(paste(builder, collapse = "\n"), "\n")
  invisible(NULL)
}
