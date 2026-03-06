# =============================================================================
# GET_CODE: Reconstruct Reproducible R Code from Pipeline Objects
# =============================================================================
# Walks the object chain (power_analysis -> conditions -> design) and
# reconstructs the full call chain as copy-pasteable R code.

#' Get Reproducible Code for a Pipeline Object
#'
#' Reconstructs the R code that produced a pipeline object. Walks the
#' chain from the current object back through its parents
#' (`power_analysis` -> `conditions` -> `design`) and returns the full
#' call chain as a formatted, copy-pasteable R script.
#'
#' @param x A pipeline object: `rctbp_design`, `rctbp_conditions`, or
#'   `rctbp_power_analysis`.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns a character string containing the reconstructed
#'   R code. The code is also printed to the console.
#'
#' @details
#' Each pipeline constructor (`build_design()`, `build_conditions()`,
#' `power_analysis()`) stores the original call via `match.call()`.
#' `get_code()` retrieves these stored calls and formats them as a
#' self-contained script.
#'
#' Only arguments explicitly supplied by the user appear in the output --
#' default values are omitted for clarity. The variable names in the
#' output follow the convention: `design`, `conditions`, `result`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' design <- build_design(
#'   predefined_model = "ancova_cont_2arms",
#'   target_params = "b_arm2"
#' )
#' conditions <- build_conditions(
#'   design = design,
#'   crossed = list(n_total = c(100, 200), b_arm_treat = c(0.3, 0.5)),
#'   constant = list(
#'     p_alloc = list(c(0.5, 0.5)),
#'     intercept = 0, b_covariate = 0.3, sigma = 1,
#'     thr_dec_eff = 0.975, thr_dec_fut = 0.5,
#'     thr_fx_eff = 0.2, thr_fx_fut = 0
#'   )
#' )
#' result <- power_analysis(conditions, n_sims = 100, n_cores = 4)
#'
#' # Print the full reproducible script
#' get_code(result)
#'
#' # Works at any level of the pipeline
#' get_code(conditions)
#' get_code(design)
#' }
get_code <- S7::new_generic("get_code", "x")

# =============================================================================
# METHOD: get_code(rctbp_design)
# =============================================================================

S7::method(get_code, rctbp_design) <- function(x, ...) {
  code <- format_call(x@.call, "design", "build_design")
  cat(code, sep = "\n")
  invisible(paste(code, collapse = "\n"))
}

# =============================================================================
# METHOD: get_code(rctbp_conditions)
# =============================================================================

S7::method(get_code, rctbp_conditions) <- function(x, ...) {
  lines <- character()

  # Design step
  design_call <- x@design@.call
  lines <- c(lines, format_call(design_call, "design", "build_design"))
  lines <- c(lines, "")

  # Conditions step -- replace the design argument with the variable name
  cond_call <- x@.call
  cond_call <- replace_arg_with_symbol(cond_call, "design", "design")
  lines <- c(lines, format_call(cond_call, "conditions", "build_conditions"))

  cat(lines, sep = "\n")
  invisible(paste(lines, collapse = "\n"))
}

# =============================================================================
# METHOD: get_code(rctbp_power_analysis)
# =============================================================================

S7::method(get_code, rctbp_power_analysis) <- function(x, ...) {
  lines <- character()

  # Design step
  design_call <- x@conditions@design@.call
  lines <- c(lines, format_call(design_call, "design", "build_design"))
  lines <- c(lines, "")

  # Conditions step
  cond_call <- x@conditions@.call
  cond_call <- replace_arg_with_symbol(cond_call, "design", "design")
  lines <- c(lines, format_call(cond_call, "conditions", "build_conditions"))
  lines <- c(lines, "")

  # Power analysis step
  pa_call <- x@.call
  pa_call <- replace_arg_with_symbol(pa_call, "conditions", "conditions")
  lines <- c(lines, format_call(pa_call, "result", "power_analysis"))

  cat(lines, sep = "\n")
  invisible(paste(lines, collapse = "\n"))
}

# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' Format a Stored Call as an Assignment Expression
#'
#' @param call_obj A call object from `match.call()`
#' @param var_name Variable name for the left side of the assignment
#' @param fn_name Expected function name (used as fallback)
#'
#' @return Character vector of formatted code lines
#' @keywords internal
format_call <- function(call_obj, var_name, fn_name) {
  if (is.null(call_obj)) {
    return(paste0(
      "# ", var_name, " <- ", fn_name,
      "(...) # (call not recorded -- object was created directly)"
    ))
  }

  # Deparse the call -- deparse can return multiple lines for long calls
  deparsed <- deparse(call_obj, width.cutoff = 60L)

  # Prepend variable assignment
  deparsed[1] <- paste0(var_name, " <- ", deparsed[1])

  deparsed
}

#' Replace a Call Argument with a Symbol
#'
#' Replaces a named argument in a call object with a symbol reference
#' (e.g., replace `design = <actual object>` with `design = design`).
#'
#' @param call_obj A call object
#' @param arg_name The argument name to replace
#' @param symbol_name The symbol name to substitute
#'
#' @return Modified call object
#' @keywords internal
replace_arg_with_symbol <- function(call_obj, arg_name, symbol_name) {
  if (is.null(call_obj)) return(call_obj)
  if (arg_name %in% names(call_obj)) {
    call_obj[[arg_name]] <- as.symbol(symbol_name)
  }
  call_obj
}
