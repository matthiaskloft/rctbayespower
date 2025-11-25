#' Report Builders for S7 Objects
#'
#' Build structured report data for rendering in different output modes.
#'
#' @name report_builders
#' @keywords internal
NULL

#' Build Report for rctbp_model
#'
#' Creates structured report data for a model object.
#'
#' @param x rctbp_model object
#' @return List with report sections
#' @keywords internal
#'
build_report.rctbp_model <- function(x) {
  list(
    title = "S7 Object: rctbp_model",
    sections = list(
      list(
        name = "Model Information",
        items = list(
          "Model name" = x@model_name,
          "Backend" = x@backend,
          "Predefined model" = if (is.null(x@predefined_model)) "None" else x@predefined_model,
          "Number of endpoints" = x@n_endpoints,
          "Endpoint types" = paste(x@endpoint_types, collapse = ", "),
          "Number of arms" = x@n_arms,
          "Number of repeated measures" = if (is.null(x@n_repeated_measures)) "NULL" else x@n_repeated_measures,
          "Parameter names - simulation function" = paste(x@parameter_names_sim_fn, collapse = ", ")
        )
      ),
      if (x@backend == "brms") {
        list(
          name = "BRMS Model",
          items = list(
            "Parameter names - brms model" = paste(x@parameter_names_brms, collapse = ", ")
          ),
          brms_model = x@brms_model
        )
      } else if (x@backend == "npe") {
        list(
          name = "Bayesflow/NPE Model",
          bayesflow_model = x@bayesflow_model
        )
      },
      if (length(x@backend_args) > 0) {
        list(
          name = "Backend Arguments",
          backend_args = x@backend_args
        )
      }
    )
  )
}

#' Build Report for rctbp_design
#'
#' Creates structured report data for a design object.
#'
#' @param x rctbp_design object
#' @return List with report sections
#' @keywords internal
#'
build_report.rctbp_design <- function(x) {
  list(
    title = "S7 Object: rctbp_design",
    sections = list(
      list(
        name = "Model Specifications",
        items = list(
          "Number of endpoints" = x@model@n_endpoints,
          "Endpoint types" = paste(x@model@endpoint_types, collapse = ", "),
          "Number of arms" = x@model@n_arms,
          "Number of repeated measures" = if (is.null(x@model@n_repeated_measures)) "NULL" else x@model@n_repeated_measures,
          "Parameter names - simulation function" = paste(x@model@parameter_names_sim_fn, collapse = ", "),
          "Parameter names - brms model" = paste(x@model@parameter_names_brms, collapse = ", ")
        )
      ),
      list(
        name = "Design Specifications",
        items = list(
          "Design name" = if (is.null(x@design_name)) "NULL" else x@design_name,
          "Target parameters" = paste(x@target_params, collapse = ", "),
          "Probability threshold for success" = x@p_sig_success,
          "Probability threshold for futility" = x@p_sig_futility
        ),
        note = "Decision criteria (thresholds, interim schedules) are specified\nper-condition in the conditions object."
      ),
      list(
        name = "brms Model",
        brms_model = x@model@brms_model
      )
    )
  )
}

#' Build Report for rctbp_conditions
#'
#' Creates structured report data for a conditions object.
#'
#' @param x rctbp_conditions object
#' @return List with report sections
#' @keywords internal
#'
build_report.rctbp_conditions <- function(x) {
  n_conditions <- nrow(x@conditions_grid)
  n_params <- ncol(x@conditions_grid) - 1  # Subtract 1 for id_cond column
  n_static_params <- length(x@static_values)

  list(
    title = "S7 Object: rctbp_conditions",
    sections = list(
      list(
        name = "Summary",
        items = list(
          "Number of conditions" = n_conditions,
          "Number of varying parameters" = n_params,
          "Number of static parameters" = n_static_params
        )
      ),
      list(
        name = "Condition Grid",
        grid = x@conditions_grid
      )
    )
  )
}

#' Build Report for rctbp_power_analysis
#'
#' Creates structured report data for a power analysis object.
#'
#' @param x rctbp_power_analysis object
#' @return List with report sections
#' @keywords internal
#'
build_report.rctbp_power_analysis <- function(x) {
  design <- x@conditions@design
  has_results <- nrow(x@summarized_results) > 0 || nrow(x@raw_results) > 0

  report <- list(
    title = "S7 Object: rctbp_power_analysis",
    sections = list(
      list(
        name = "Design Summary",
        items = list(
          "Target parameters" = paste(design@target_params, collapse = ", "),
          "Success probability threshold" = design@p_sig_success,
          "Futility probability threshold" = design@p_sig_futility
        )
      )
    )
  )

  if (has_results) {
    # Completed analysis
    results_df <- x@summarized_results
    n_conditions <- nrow(x@conditions@conditions_grid)

    # Power ranges
    power_ranges <- NULL
    power_cols <- intersect(names(results_df), c("power_success", "power_futility"))
    if (length(power_cols) > 0) {
      power_ranges <- lapply(power_cols, function(col) {
        power_range <- range(results_df[[col]], na.rm = TRUE)
        list(
          name = gsub("power_", "", col),
          range = paste0(round(power_range[1] * 100, 1), "% - ", round(power_range[2] * 100, 1), "%")
        )
      })
    }

    report$status <- "COMPLETED"
    report$sections <- c(report$sections, list(
      list(
        name = "Results Summary",
        items = list(
          "Analysis runtime" = if (!is.null(x@elapsed_time)) paste0(round(x@elapsed_time, 2), " minutes") else "Not available",
          "Conditions analyzed" = n_conditions,
          "Simulations per condition" = x@n_sims,
          "Total simulations" = n_conditions * x@n_sims
        ),
        power_ranges = power_ranges
      ),
      list(
        name = "Available Actions",
        actions = c(
          "plot() - Create visualizations",
          "power_config@summarized_results - Access summarized results",
          "power_config@raw_results - Access raw simulation results"
        )
      )
    ))
  } else {
    # Pending analysis
    n_conditions <- nrow(x@conditions@conditions_grid)
    total_sims <- n_conditions * x@n_sims

    report$status <- "PENDING"
    report$sections <- c(report$sections, list(
      list(
        name = "Analysis Configuration",
        items = list(
          "Number of simulations per condition" = x@n_sims,
          "Number of cores for parallel execution" = x@n_cores,
          "Verbose output" = x@verbose,
          "Design prior" = if (is.null(x@design_prior)) {
            "None"
          } else if (is.function(x@design_prior)) {
            "Custom function"
          } else {
            as.character(x@design_prior)
          }
        ),
        brms_args = if (length(x@brms_args) > 0) x@brms_args else NULL
      ),
      list(
        name = "Analysis Preview",
        items = list(
          "Total conditions" = n_conditions,
          "Total simulations" = total_sims
        )
      ),
      list(
        name = "Available Actions",
        actions = c(
          "run() - Execute the analysis",
          "power_config@conditions - View condition details"
        )
      )
    ))
  }

  return(report)
}
