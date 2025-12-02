# =============================================================================
# S7 CLASS DEFINITION: rctbp_sim_fn
# =============================================================================
# Callable simulation function with test arguments and output schema.
# Inherits from S7::class_function, making instances directly callable.
#
# Key Design:
# - Function IS the S7 object (not a wrapper)
# - do.call(sim_fn, args) works directly
# - Schema derived from stored test output (no re-simulation)
# - Validates output structure on creation

# =============================================================================
# OUTPUT SCHEMA DERIVATION
# =============================================================================

#' Derive Output Schema from Test Data
#'
#' Discovers column names and required transformations from simulation output.
#' Handles both R data.frame and Python batch dict formats.
#'
#' @param test_data Test output from simulation function
#'
#' @return Named list of field specifications, each with:
#'   \itemize{
#'     \item source: Original column/field name
#'     \item transform: Transform to apply (NULL, "factor_to_numeric", "logical_to_int")
#'   }
#'
#' @details
#' Detection uses structural heuristics (not hardcoded field names):
#' \itemize{
#'   \item Batch dict: list (not data.frame) containing matrices or "n_total" key
#'   \item R data.frame: standard data.frame output
#' }
#'
#' \strong{WORKAROUND}: Current BayesFlow models expect `group` (integer) instead
#' of `arm` (factor). The schema transforms `arm` -> `group` as a compatibility
#' workaround. Future BayesFlow models should be retrained to match sim_fn output.
#'
#' @keywords internal
derive_output_schema_from_data <- function(test_data) {
  # Detect batch dict by structure, not field names:
  # - Is a list but not data.frame

  # - Contains matrices OR has "n_total" metadata key
  is_batch_dict <- is.list(test_data) && !is.data.frame(test_data) &&
    (any(vapply(test_data, is.matrix, logical(1))) || "n_total" %in% names(test_data))

  schema <- list()

  if (is_batch_dict) {
    # Python batch dict: keys are field names, values are matrices
    for (field_name in names(test_data)) {
      if (field_name %in% c("n_total", "p_alloc")) next
      val <- test_data[[field_name]]
      if (!is.matrix(val) && !is.numeric(val)) next

      # WORKAROUND: arm -> group for BayesFlow compatibility
      output_name <- if (field_name == "arm") "group" else field_name
      schema[[output_name]] <- list(source = field_name, transform = NULL)
    }
  } else {
    # R data.frame: columns are fields
    for (col_name in names(test_data)) {
      col_data <- test_data[[col_name]]

      transform <- NULL
      if (is.factor(col_data)) {
        transform <- "factor_to_numeric"
      } else if (is.logical(col_data)) {
        transform <- "logical_to_int"
      } else if (!is.numeric(col_data)) {
        next
      }

      # WORKAROUND: arm -> group for BayesFlow compatibility
      output_name <- if (col_name == "arm") "group" else col_name
      schema[[output_name]] <- list(source = col_name, transform = transform)
    }
  }

  schema
}

# =============================================================================
# OUTPUT VALIDATION
# =============================================================================

#' Validate Simulation Output Structure
#'
#' Validates that simulation output is either a valid R data.frame or
#' Python batch dict format.
#'
#' @param data Output from simulation function
#'
#' @return Invisible TRUE if valid, otherwise aborts with informative error
#'
#' @keywords internal
validate_sim_output <- function(data) {
  # Detect batch dict by structure, not field names:
  # - Is a list but not data.frame
  # - Contains matrices OR has "n_total" metadata key
  is_batch_dict <- is.list(data) && !is.data.frame(data) &&
    (any(vapply(data, is.matrix, logical(1))) || "n_total" %in% names(data))

  if (is_batch_dict) {
    # Python batch dict validation
    if (length(names(data)) == 0) {
      cli::cli_abort("sim_fn must return a dict with fields")
    }
    # Check at least one data field exists (exclude metadata)
    data_fields <- setdiff(names(data), c("n_total", "p_alloc"))
    if (length(data_fields) == 0) {
      cli::cli_abort("sim_fn must return at least one data field")
    }
  } else if (is.data.frame(data)) {
    # R data.frame validation
    if (ncol(data) == 0) {
      cli::cli_abort("sim_fn must return a data.frame with columns")
    }
    if (nrow(data) == 0) {
      cli::cli_abort("sim_fn must return a data.frame with rows")
    }
  } else {
    cli::cli_abort(c(
      "sim_fn must return a data.frame or batch dict",
      "i" = "Got: {.cls {class(data)}}"
    ))
  }

  invisible(TRUE)
}

# =============================================================================
# S7 CLASS: rctbp_sim_fn
# =============================================================================

#' Callable Simulation Function Class
#'
#' S7 class that wraps a simulation function with test arguments and schema.
#' Inherits from `S7::class_function`, making instances directly callable.
#'
#' @details
#' The function remains callable - `do.call(sim_fn, args)` works directly.
#' Schema is derived from stored test output (no re-simulation needed).
#'
#' Properties:
#' \describe{
#'   \item{test_args}{List of arguments for test/validation run}
#'   \item{test_output}{Stored output from test run (for schema derivation)}
#'   \item{output_schema}{Computed: field schema derived from test_output}
#'   \item{sim_fn_params}{Computed: parameter names from function formals}
#' }
#'
#' @keywords internal
rctbp_sim_fn <- S7::new_class(
  "rctbp_sim_fn",
  parent = S7::class_function,

  properties = list(
    # Test arguments for validation/discovery
    test_args = S7::class_list,

    # Stored: test output from validation run (avoids re-simulation)
    test_output = S7::new_property(
      class = S7::class_any,
      default = NULL
    ),

    # Computed: output column schema derived from stored test output
    output_schema = S7::new_property(
      class = S7::class_list,
      getter = function(self) {
        if (is.null(self@test_output)) {
          # Fallback: run test sim if test_output not set (shouldn't happen)
          test_data <- do.call(self, self@test_args)
        } else {
          test_data <- self@test_output
        }
        derive_output_schema_from_data(test_data)
      }
    ),

    # Computed: parameter names from function formals
    sim_fn_params = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        names(formals(self))
      }
    )
  ),

  validator = function(self) {
    fn_par_names <- names(formals(self))
    test_arg_names <- names(self@test_args)

    # Validate mandatory function parameters exist
    required <- c("n_total")
    missing_fn_params <- setdiff(required, fn_par_names)
    if (length(missing_fn_params) > 0) {
      cli::cli_abort(c(
        "Simulation function missing required parameters",
        "x" = "Missing: {.arg {missing_fn_params}}",
        "i" = "sim_fn must accept at least {.arg n_total}"
      ))
    }

    # Validate test_args keys match function formals
    invalid_test_args <- setdiff(test_arg_names, fn_par_names)
    if (length(invalid_test_args) > 0) {
      cli::cli_abort(c(
        "{.arg test_args} contains invalid parameter names",
        "x" = "Invalid: {.arg {invalid_test_args}}",
        "i" = "Valid parameters: {.arg {fn_par_names}}"
      ))
    }

    # Validate test_args has required parameters
    missing_test_args <- setdiff(required, test_arg_names)
    if (length(missing_test_args) > 0) {
      cli::cli_abort(c(
        "{.arg test_args} missing required parameters",
        "x" = "Missing: {.arg {missing_test_args}}"
      ))
    }

    # Validate output structure (test_output set by constructor helper)
    if (!is.null(self@test_output)) {
      validate_sim_output(self@test_output)
    }

    NULL
  }
)

# =============================================================================
# CONSTRUCTOR HELPER
# =============================================================================

#' Build Simulation Function Object
#'
#' Wraps a simulation function with test arguments for validation and schema
#' discovery. Runs a test simulation to validate output structure and cache
#' schema derivation.
#'
#' @param fn Simulation function (must accept `n_total` parameter)
#' @param test_args List of arguments for test run (must include `n_total`)
#'
#' @return An `rctbp_sim_fn` object that is:
#'   \itemize{
#'     \item Directly callable: `do.call(sim_fn, args)` works
#'     \item Has `@test_args`: Test arguments used for validation
#'     \item Has `@output_schema`: Discovered column schema for batch preparation
#'     \item Has `@sim_fn_params`: Parameter names from function formals
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Custom simulation function
#' my_sim <- function(n_total, effect_size, noise_sd) {
#'   arm <- factor(sample(c("ctrl", "treat"), n_total, replace = TRUE))
#'   data.frame(
#'     outcome = rnorm(n_total, effect_size * (as.integer(arm) - 1), noise_sd),
#'     covariate = rnorm(n_total),
#'     arm = arm
#'   )
#' }
#'
#' # Wrap with test arguments
#' sim_fn <- build_sim_fn(
#'   fn = my_sim,
#'   test_args = list(n_total = 10, effect_size = 0.5, noise_sd = 1)
#' )
#'
#' # Use directly
#' data <- do.call(sim_fn, list(n_total = 100, effect_size = 0.3, noise_sd = 1))
#'
#' # Access schema
#' sim_fn@output_schema
#' }
build_sim_fn <- function(fn, test_args = list(n_total = 10L)) {
  # Validate fn is a function

  if (!is.function(fn)) {
    cli::cli_abort(c(
      "{.arg fn} must be a function",
      "x" = "You supplied {.type {fn}}"
    ))
  }

  # Validate test_args is a list
  if (!is.list(test_args)) {
    cli::cli_abort(c(
      "{.arg test_args} must be a list",
      "x" = "You supplied {.type {test_args}}"
    ))
  }

  # Run test simulation upfront
  test_output <- tryCatch(
    do.call(fn, test_args),
    error = function(e) {
      cli::cli_abort(c(
        "Test simulation failed",
        "x" = e$message,
        "i" = "Check {.arg test_args} are valid for your sim_fn"
      ))
    }
  )

  # Create S7 object with pre-computed test_output
  rctbp_sim_fn(fn, test_args = test_args, test_output = test_output)
}

# =============================================================================
# S7 METHOD: print()
# =============================================================================

#' Print Method for rctbp_sim_fn Objects
#'
#' Displays a summary of the simulation function object.
#'
#' @param x An `rctbp_sim_fn` object
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object
#' @name print.rctbp_sim_fn
#' @keywords internal
#' @export
S7::method(print, rctbp_sim_fn) <- function(x, ...) {
  cli::cli_h3("Simulation Function (rctbp_sim_fn)")

  # Parameters
  params <- x@sim_fn_params
  cli::cli_text("Parameters: {.field {params}}")

  # Test args
  test_arg_names <- names(x@test_args)
  cli::cli_text("Test args: {.field {test_arg_names}}")

  # Schema summary
  schema <- x@output_schema
  schema_names <- names(schema)
  cli::cli_text("Output schema: {.field {schema_names}}")

  invisible(x)
}
