#' #' Build Conditions for Power Analysis
#' #'
#' #' Creates a structured set of conditions and argument lists for power analysis
#' #' simulations. This function takes varying condition parameters and static
#' #' parameters, validates them against the design requirements, and creates
#' #' all necessary argument combinations for simulation runs.
#' #'
#' #' @param design An rctbayespower_design object that defines the study design
#' #' @param condition_values A named list where each element contains vectors of
#' #'   parameter values to vary across conditions. All combinations will be created.
#' #' @param static_values A named list of parameter values that remain constant
#' #'   across all conditions
#' #'
#' #' @return An rctbayespower_conditions object containing:
#' #'   \item{conditions_grid}{A data.frame with all parameter combinations}
#' #'   \item{condition_arguments}{A list of argument lists for each condition,
#' #'     separated into simulation and interim analysis arguments}
#' #'   \item{design}{The original rctbayespower_design object}
#' #'
#' #' @details The function performs several validation steps:
#' #' \itemize{
#' #'   \item Checks that condition_values and static_values don't have overlapping names
#' #'   \item Validates that all required parameters are provided
#' #'   \item Ensures p_alloc is properly specified as a list
#' #'   \item Creates expanded grid of all condition combinations
#' #' }
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Create conditions for sample size and effect size analysis
#' #' conditions <- build_conditions(
#' #'   design = my_design,
#' #'   condition_values = list(
#' #'     n_total = c(100, 200, 300),
#' #'     effect_size = c(0.2, 0.5, 0.8)
#' #'   ),
#' #'   static_values = list(
#' #'     p_alloc = list(c(0.5, 0.5)),
#' #'     baseline_effect = 0.1
#' #'   )
#' #' )
#' #'
#' #' # Print the conditions
#' #' print(conditions)
#' #' }
#' #'
#' #' @export
#' build_conditions <- function(design, condition_values, static_values) {
#'   # validate design
#'   if (!inherits(design, "rctbayespower_design")) {
#'     stop("'design' must be a valid rctbayespower_design object.")
#'   }
#' 
#'   # validate inputs
#'   if (!is.list(condition_values)) {
#'     stop("'condition_values' must be a list.")
#'   }
#'   if (!is.list(static_values)) {
#'     stop("'static_values' must be a list.")
#'   }
#' 
#'   # gather provided parameter names
#'   params_given <- c(names(condition_values), names(static_values))
#' 
#'   # check for overlapping names between condition_values and static_values
#'   params_overlap <- intersect(names(condition_values), names(static_values))
#'   if (length(params_overlap) > 0) {
#'     stop(
#'       paste0(
#'         "Redundant parameter(s) found in both 'condition_values' and 'static_values': ",
#'         paste(params_overlap, collapse = ", ")
#'       )
#'     )
#'   }
#'   # check for duplicated parameter names overall (within or across lists)
#'   params_redundant <- unique(params_given[duplicated(params_given)])
#'   if (length(params_redundant) > 0) {
#'     stop(
#'       paste0(
#'         "Duplicated parameter names detected (possibly within the same list): ",
#'         paste(params_redundant, collapse = ", ")
#'       )
#'     )
#'   }
#' 
#'   # required parameters
#'   params_needed <- required_fn_args(design, print = FALSE)
#' 
#'   # check for missing param values
#'   if (!all(params_needed$params_all %in% params_given)) {
#'     stop(paste(
#'       "The following parameters are missing and must be specified:",
#'       paste(
#'         setdiff(params_needed$params_all, params_given),
#'         collapse = ", "
#'       )
#'     ))
#'   }
#' 
#'   # merge inputs
#'   all_values <- c(condition_values, static_values)
#' 
#'   # check p_alloc
#'   if ("p_alloc" %in% names(condition_values) &&
#'       !is.list(all_values[["p_alloc"]])) {
#'     condition_values$p_alloc <- list(condition_values$p_alloc)
#'   }
#'   if ("p_alloc" %in% names(static_values) &&
#'       !is.list(all_values[["p_alloc"]])) {
#'     static_values$p_alloc <- list(static_values$p_alloc)
#'   }
#' 
#'   # expansion of conditions ----------------------------------------------------
#' 
#'   # create condition grid (data frame of combinations)
#'   df_grid <- do.call(tidyr::expand_grid, condition_values)
#'   # add id per condition
#'   df_grid <- tibble::rowid_to_column(df_grid, var = "id_cond")
#' 
#'   # Convert each row into a list of named values
#'   condition_arguments_flat <- apply(df_grid, 1, as.list)
#' 
#'   # Combine simulation and interim arguments per condition
#'   condition_arguments <- lapply(condition_arguments_flat, function(condition) {
#'     # --- Simulation arguments ---
#'     sim_args <- list()
#'     for (param in params_needed$params_sim) {
#'       if (param %in% names(condition)) {
#'         sim_args[[param]] <- condition[[param]]
#'       } else if (param %in% names(static_values)) {
#'         sim_args[[param]] <- static_values[[param]]
#'       } else {
#'         stop(
#'           sprintf(
#'             "Parameter '%s' is missing in condition_values or static_values.",
#'             param
#'           )
#'         )
#'       }
#'     }
#'     # --- Interim arguments ---
#'     interim_args <- NULL
#'     if (!is.null(params_needed$params_interim)) {
#'       interim_args <- list()
#'       for (param in params_needed$params_interim) {
#'         if (param %in% names(condition)) {
#'           interim_args[[param]] <- condition[[param]]
#'         } else if (param %in% names(static_values)) {
#'           interim_args[[param]] <- static_values[[param]]
#'         } else {
#'           stop(
#'             sprintf(
#'               "Parameter '%s' is missing in condition_values or static_values.",
#'               param
#'             )
#'           )
#'         }
#'       }
#'     }
#'     # Return both sets of args
#'     list(
#'       id_cond = condition$id_cond,
#'       sim_args = sim_args,
#'       interim_args = interim_args
#'     )
#'   })
#' 
#' 
#'   # list to return
#'   return_list <- list(
#'     conditions_grid = df_grid,
#'     condition_arguments = condition_arguments,
#'     design = design,
#'     condition_values = condition_values,
#'     static_values = static_values
#'   )
#' 
#'   # assign class
#'   class(return_list) <- "rctbayespower_conditions"
#'   # add attribute: number of conditions
#'   attr(return_list, "n_conditions") <- nrow(df_grid)
#'   # add attribute: number of varying parameters
#'   attr(return_list, "n_params") <- length(condition_values)
#'   # add attribute: number of static parameters
#'   attr(return_list, "n_static_params") <- length(static_values)
#' 
#'   # return
#'   return_list
#' }
#' 
#' 
#' #' Print Method for rctbayespower_conditions Objects
#' #'
#' #' Prints a formatted summary of condition grids created by [build_conditions()].
#' #' Shows the condition grid with all parameter combinations and provides
#' #' summary information about the number of conditions and parameters.
#' #'
#' #' @param x An rctbayespower_conditions object created by [build_conditions()]
#' #' @param ... Additional arguments passed to [print()]
#' #'
#' #' @return Invisibly returns the input object
#' #'
#' #' @examples
#' #' \dontrun{
#' #' conditions <- build_conditions(design, condition_values, static_values)
#' #' print(conditions) # or just: conditions
#' #' }
#' #'
#' #' @export
#' print.rctbayespower_conditions <- function(x, ...) {
#'   cat("\nObject of class: 'rctbayespower_conditions'\n")
#'   cat("--------------------------------------------------\n\n")
#' 
#'   # Print basic info
#'   n_conditions <- nrow(x$conditions_grid)
#'   n_params <- ncol(x$conditions_grid)
#' 
#'   cat("Number of conditions:", n_conditions, "\n")
#'   cat("Number of varying parameters:", n_params, "\n\n")
#' 
#'   # Print the conditions grid
#'   cat("Condition Grid:\n")
#'   print(x$conditions_grid, ...)
#' 
#'   invisible(x)
#' }
