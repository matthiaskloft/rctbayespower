#' Report Builders for S7 Objects
#'
#' Build structured report data for rendering in different output modes.
#'
#' @name report_builders
#' @keywords internal
NULL

#' Format Boundary Specification for Display
#'
#' Formats a probability threshold or boundary function into a display string.
#'
#' @param threshold Either a numeric value or a boundary function
#' @return Character string describing the threshold
#' @keywords internal
format_boundary <- function(threshold) {
  if (is.function(threshold)) {
    # Check for boundary function metadata
    boundary_type <- attr(threshold, "boundary_type")
    boundary_params <- attr(threshold, "boundary_params")

    if (!is.null(boundary_type) && !is.null(boundary_params)) {
      # Format based on boundary type
      switch(boundary_type,
        "obf" = paste0("O'Brien-Fleming (", boundary_params$base, ")"),
        "pocock" = paste0("Pocock (", boundary_params$threshold, ")"),
        "linear" = paste0("Linear (", boundary_params$start, " \u2192 ", boundary_params$end, ")"),
        "power" = paste0("Power (base=", boundary_params$base, ", rho=", boundary_params$rho, ")"),
        # Fallback for unknown types
        paste0("Function (", boundary_type, ")")
      )
    } else {
      # Generic function without metadata - evaluate at sample points
      sample_vals <- c(0.5, 1.0)
      sample_results <- sapply(sample_vals, threshold)
      paste0("Function (at 50%: ", round(sample_results[1], 4),
             ", at 100%: ", round(sample_results[2], 4), ")")
    }
  } else {
    as.character(threshold)
  }
}

#' Format Value Range
#'
#' Formats a numeric vector as a range string "min-max" with optional percentage.
#'
#' @param vals Numeric vector
#' @param pct Logical; if TRUE, format as percentages (default FALSE)
#' @param digits Number of decimal digits (default 1)
#' @return Character string representing the range
#' @keywords internal
fmt_range <- function(vals, pct = FALSE, digits = 1) {
  rng <- range(vals, na.rm = TRUE)
  if (pct) {
    paste0(round(rng[1] * 100, digits), "%-", round(rng[2] * 100, digits), "%")
  } else {
    paste0(round(rng[1], digits), "-", round(rng[2], digits))
  }
}

#' Format Parameters Compactly
#'
#' Formats a named list of parameters as "name=value, name=value" string.
#'
#' @param params Named list of parameters
#' @return Character string with formatted parameters
#' @keywords internal
fmt_params <- function(params) {
  paste(
    names(params),
    sapply(params, function(v) if (is.numeric(v)) round(v, 3) else as.character(v)),
    sep = "=",
    collapse = ", "
  )
}

#' Get Threshold Display from Conditions
#'
#' Extracts a threshold parameter value from conditions and formats for display.
#' Checks constant, crossed, and grid in order.
#'
#' @param conditions An rctbp_conditions object
#' @param param_name Name of the threshold parameter (e.g., "thr_dec_eff")
#' @return Character string describing the threshold value(s)
#' @keywords internal
get_threshold_display <- function(conditions, param_name) {
  # Check constant first
  if (param_name %in% names(conditions@constant)) {
    val <- conditions@constant[[param_name]]
    return(format_boundary(val))
  }

  # Check crossed
  if (param_name %in% names(conditions@crossed)) {
    vals <- conditions@crossed[[param_name]]
    if (is.function(vals)) {
      return(format_boundary(vals))
    } else if (is.numeric(vals)) {
      if (length(vals) == 1) {
        return(as.character(vals))
      } else {
        return(paste0(min(vals), "-", max(vals), " (varies)"))
      }
    }
  }

  # Check grid
  if (param_name %in% names(conditions@grid)) {
    vals <- unique(conditions@grid[[param_name]])
    if (length(vals) == 1) {
      return(format_boundary(vals))
    } else {
      return(paste0(min(vals), "-", max(vals), " (varies)"))
    }
  }

  # Not found
  "(not specified)"
}

#' Get Original Threshold Value from Conditions
#'
#' Extracts the original threshold parameter value from conditions.
#' Unlike get_threshold_display(), this returns the actual value, not a display string.
#'
#' @param conditions An rctbp_conditions object
#' @param param_name Name of the threshold parameter (e.g., "thr_dec_eff")
#' @return The original threshold value (numeric, function, or NULL if not found)
#' @keywords internal
get_original_threshold <- function(conditions, param_name) {
  # Check constant first
  if (param_name %in% names(conditions@constant)) {
    return(conditions@constant[[param_name]])
  }

  # Check crossed
  if (param_name %in% names(conditions@crossed)) {
    return(conditions@crossed[[param_name]])
  }

  # Check grid - return the first unique value
  if (param_name %in% names(conditions@grid)) {
    vals <- unique(conditions@grid[[param_name]])
    if (length(vals) == 1) {
      return(vals)
    } else {
      # Multiple values - return as vector
      return(vals)
    }
  }

  # Not found
  NULL
}

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
          "Parameter names - simulation" = paste(x@par_names_sim, collapse = ", ")
        )
      ),
      list(
        name = if (x@backend == "brms") "BRMS Model" else "BayesFlow Model",
        items = list(
          "Parameter names - inference" = paste(x@par_names_inference, collapse = ", ")
        ),
        brms_model = if (x@backend == "brms") x@inference_model else NULL,
        bayesflow_model = if (x@backend == "bf") x@inference_model else NULL
      ),
      if (x@backend == "brms" && length(x@backend_args_brms) > 0) {
        list(
          name = "brms Arguments",
          backend_args = x@backend_args_brms
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
  # NOTE: After API merge, model properties are directly on design (not x@model@*)
  # NOTE: Decision parameters (thr_dec_eff, thr_dec_fut, analysis_at, etc.) are now in conditions
  list(
    title = "S7 Object: rctbp_design",
    sections = list(
      list(
        name = "Model Specifications",
        items = list(
          "Model name" = if (is.null(x@model_name)) "Custom" else x@model_name,
          "Backend" = x@backend,
          "Number of endpoints" = x@n_endpoints,
          "Endpoint types" = paste(x@endpoint_types, collapse = ", "),
          "Number of arms" = x@n_arms,
          "Parameter names - simulation" = paste(x@par_names_sim, collapse = ", "),
          "Parameter names - inference" = paste(x@par_names_inference, collapse = ", ")
        )
      ),
      list(
        name = "Design Specifications",
        items = list(
          "Design name" = if (is.null(x@design_name)) "(not set)" else x@design_name,
          "Target parameters" = paste(x@target_params, collapse = ", ")
        ),
        note = "Decision thresholds and analysis schedule are specified in build_conditions()."
      ),
      if (x@backend == "brms") {
        list(
          name = "brms Model",
          brms_model = x@inference_model
        )
      } else if (x@backend == "bf") {
        list(
          name = "BayesFlow Model",
          bayesflow_model = x@inference_model
        )
      }
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
  n_conditions <- nrow(x@grid)
  n_constant <- length(x@constant)

  # Count regular params and link() groups in crossed
  is_link <- function(obj) inherits(obj, "rctbp_link")
  link_indices <- vapply(x@crossed, is_link, logical(1))
  n_link_groups <- sum(link_indices)
  n_regular_crossed <- sum(!link_indices)

  # Count total unique params from link groups
  n_linked_params <- sum(vapply(x@crossed[link_indices], length, integer(1)))

  target_pwr_display <- if (is.null(x@target_pwr)) {
    "Not set (will show highest power)"
  } else {
    paste0(round(x@target_pwr * 100, 1), "%")
  }

  # Build summary items
  summary_items <- list(
    "Number of conditions" = n_conditions,
    "Crossed parameters" = n_regular_crossed,
    "Constant parameters" = n_constant,
    "Target power for optimal condition" = target_pwr_display
  )

  # Add link() info only if present

  if (n_link_groups > 0) {
    summary_items <- c(
      summary_items[1:2],  # conditions, crossed
      list("Linked groups (via link())" = paste0(n_link_groups, " (", n_linked_params, " params)")),
      summary_items[3:4]   # constant, target_pwr
    )
  }

  list(
    title = "S7 Object: rctbp_conditions",
    sections = list(
      list(
        name = "Summary",
        items = summary_items
      ),
      list(
        name = "Condition Grid",
        grid = x@grid
      )
    )
  )
}

#' Build Report for rctbp_power_analysis
#'
#' Creates structured report data for a power analysis object.
#'
#' @param x rctbp_power_analysis object
#' @param target_pwr Target power for optimal condition (default NULL shows highest)
#' @return List with report sections
#' @keywords internal
#'
build_report.rctbp_power_analysis <- function(x, target_pwr = NULL) {
  design <- x@conditions@design
  has_results <- nrow(x@results_conditions) > 0 || nrow(x@results_raw) > 0

  target_pwr_display <- if (is.null(target_pwr)) {
    "Not set (showing highest power)"
  } else {
    paste0(round(target_pwr * 100, 1), "%")
  }

  # Get decision thresholds from conditions (in crossed or constant)
  conditions <- x@conditions
  thr_dec_eff_display <- get_threshold_display(conditions, "thr_dec_eff")
  thr_dec_fut_display <- get_threshold_display(conditions, "thr_dec_fut")

  report <- list(
    title = "Power Analysis Summary",
    sections = list(
      list(
        name = "Design Summary",
        items = list(
          "Target parameters" = paste(design@target_params, collapse = ", "),
          "Efficacy probability threshold" = thr_dec_eff_display,
          "Futility probability threshold" = thr_dec_fut_display,
          "Target power" = target_pwr_display
        )
      )
    )
  )

  if (has_results) {
    # Completed analysis
    n_conditions <- nrow(x@conditions@grid)

    # Check for interim analysis results (via S7 property)
    has_interim <- x@has_interim
    # For sequential: power metrics in results_interim, overall stats in results_conditions
    # For single-look: power metrics in results_conditions
    results_df <- if (has_interim) x@results_interim else x@results_conditions
    interim_overall <- if (has_interim) x@results_conditions else NULL

    # Power ranges
    power_ranges <- NULL
    power_cols <- intersect(names(results_df), c("pwr_eff", "pwr_fut"))
    if (length(power_cols) > 0) {
      power_ranges <- lapply(power_cols, function(col) {
        power_range <- range(results_df[[col]], na.rm = TRUE)
        list(
          name = gsub("pwr_", "", col),
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
          "Total simulations" = n_conditions * x@n_sims,
          "Design type" = if (has_interim) "Sequential (with interim analyses)" else "Single-look"
        ),
        power_ranges = power_ranges
      )
    ))

    # Find optimal condition for target power
    optimal <- find_optimal_condition(
      results_summ = results_df,
      conditions_grid = x@conditions@grid,
      target_pwr = target_pwr,
      interim_overall = interim_overall,
      power_col = "pwr_eff"
    )

    # Helper to format condition parameters
    format_params <- function(params) {
      paste(
        names(params),
        sapply(params, function(v) {
          if (is.numeric(v)) round(v, 3) else as.character(v)
        }),
        sep = " = ",
        collapse = ", "
      )
    }

    # Helper to format interim stats (matches column names from get_interim_stats)
    format_interim <- function(interim) {
      if (is.null(interim)) return(NULL)
      list(
        n_mn = round(interim$n_mn, 0),
        n_mdn = round(interim$n_mdn, 0),
        n_mode = round(interim$n_mode, 0),
        prop_at_mode = paste0(round(interim$prop_at_mode * 100, 1), "%"),
        prop_stp_early = paste0(round(interim$prop_stp_early * 100, 1), "%"),
        prop_stp_eff = paste0(round(interim$prop_stp_eff * 100, 1), "%"),
        prop_stp_fut = paste0(round(interim$prop_stp_fut * 100, 1), "%"),
        prop_no_dec = paste0(round(interim$prop_no_dec * 100, 1), "%")
      )
    }

    if (optimal$found || optimal$mode == "highest") {
      # Found optimal or showing highest power
      param_str <- format_params(optimal$condition_params)
      interim_fmt <- format_interim(optimal$interim)

      section_name <- if (optimal$mode == "highest") {
        "Highest Power Condition"
      } else {
        "Optimal Condition"
      }

      note_text <- if (optimal$mode == "highest") {
        "Condition with highest achieved power. Set target with: print(x, target_pwr = 0.8)"
      } else {
        "Smallest sample size achieving target power. Override with: print(x, target_pwr = 0.9)"
      }

      report$sections <- c(report$sections, list(
        list(
          name = section_name,
          optimal_condition = list(
            found = TRUE,
            mode = optimal$mode,
            target_pwr = if (!is.null(target_pwr)) paste0(round(target_pwr * 100, 1), "%") else NULL,
            achieved_pwr = paste0(round(optimal$achieved_pwr * 100, 1), "%"),
            n_total = optimal$n_total,
            condition_id = optimal$condition_id,
            params = param_str,
            interim = interim_fmt
          ),
          note = note_text
        )
      ))
    } else if (!is.null(optimal$closest)) {
      # No condition meets target - show closest
      param_str <- format_params(optimal$closest$condition_params)
      interim_fmt <- format_interim(optimal$closest$interim)

      report$sections <- c(report$sections, list(
        list(
          name = "Optimal Condition",
          optimal_condition = list(
            found = FALSE,
            mode = "target",
            target_pwr = paste0(round(target_pwr * 100, 1), "%"),
            closest_pwr = paste0(round(optimal$closest$achieved_pwr * 100, 1), "%"),
            closest_n = optimal$closest$n_total,
            closest_id = optimal$closest$condition_id,
            params = param_str,
            interim = interim_fmt
          ),
          note = "No condition achieves target power. Showing closest. Try larger sample sizes."
        )
      ))
    }

    report$sections <- c(report$sections, list(
      list(
        name = "Available Actions",
        actions = c(
          "plot() - Create visualizations",
          "power_config@results_conditions - Access condition-level results",
          "power_config@results_interim - Access per-look results (sequential only)",
          "power_config@results_raw - Access raw simulation results"
        )
      )
    ))
  } else {
    # Pending analysis
    n_conditions <- nrow(x@conditions@grid)
    total_sims <- n_conditions * x@n_sims

    report$status <- "PENDING"
    report$sections <- c(report$sections, list(
      list(
        name = "Analysis Configuration",
        items = list(
          "Number of simulations per condition" = x@n_sims,
          "Number of cores for parallel execution" = x@n_cores,
          "Verbosity level" = x@verbosity,
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

#' Find Optimal Condition Based on Target Power
#'
#' Identifies the optimal condition based on target power. If target_pwr is NULL,
#' returns the condition with highest power. If target_pwr is specified, finds
#' the smallest sample size that achieves at least that power.
#'
#' @param results_summ A data.frame with summarized power analysis results
#' @param conditions_grid A data.frame with condition parameter combinations
#' @param target_pwr Target power level (0 to 1), or NULL for highest power
#' @param interim_overall Optional data.frame with interim analysis stats
#' @param power_col Column name for power values (default "pwr_eff")
#'
#' @return A list with:
#'   \item{found}{Logical indicating if an optimal condition was found}
#'   \item{mode}{"highest" if target_pwr is NULL, "target" otherwise}
#'   \item{target_pwr}{The target power used (NULL if mode = "highest")}
#'   \item{n_total}{Sample size of optimal condition}
#'   \item{achieved_pwr}{Achieved power of optimal condition}
#'   \item{condition_id}{ID of optimal condition}
#'   \item{condition_params}{Named list of condition parameters}
#'   \item{interim}{Interim analysis stats for this condition (if available)}
#'   \item{closest}{If no condition meets target, info on closest condition}
#'
#' @keywords internal
#'
find_optimal_condition <- function(results_summ, conditions_grid, target_pwr,
                                    interim_overall = NULL,
                                    power_col = "pwr_eff") {
  # Default return for cases where no optimal found
  not_found <- list(
    found = FALSE,
    mode = if (is.null(target_pwr)) "highest" else "target",
    target_pwr = target_pwr,
    n_total = NA_real_,
    achieved_pwr = NA_real_,
    condition_id = NA_integer_,
    condition_params = NULL,
    interim = NULL,
    closest = NULL
  )

  # Validate inputs
  if (nrow(results_summ) == 0 || nrow(conditions_grid) == 0) {
    return(not_found)
  }

  if (!power_col %in% names(results_summ)) {
    return(not_found)
  }

  # Check if n_total is in conditions_grid
  if (!"n_total" %in% names(conditions_grid)) {
    return(not_found)
  }

  # Identify condition ID column in results_summ
  if (!"id_cond" %in% names(results_summ)) {
    return(not_found)
  }
  id_col <- "id_cond"

  # Check if power column has any non-NA values
  # aggregate() with formula interface drops NA values, causing "no rows" error if all NA
  if (all(is.na(results_summ[[power_col]]))) {
    return(not_found)
  }

  # Join results with conditions grid
  # Get unique power per condition (handle multi-parameter cases)
  results_by_cond <- stats::aggregate(
    stats::as.formula(paste(power_col, "~", id_col)),
    data = results_summ,
    FUN = max  # Take max power if multiple parameters
  )
  names(results_by_cond) <- c("id_cond", "power")

  # Merge with conditions grid
  merged <- merge(
    conditions_grid,
    results_by_cond,
    by = "id_cond",
    all.x = FALSE
  )

  if (nrow(merged) == 0) {
    return(not_found)
  }

  # Helper to extract interim stats for a condition
 get_interim_stats <- function(cond_id) {
    if (is.null(interim_overall) || nrow(interim_overall) == 0) {
      return(NULL)
    }
    # Find row for this condition
    if (!"id_cond" %in% names(interim_overall)) {
      return(NULL)
    }
    row <- interim_overall[interim_overall[["id_cond"]] == cond_id, , drop = FALSE]
    if (nrow(row) == 0) {
      return(NULL)
    }
    list(
      n_mn = row$n_mn[1],
      n_mdn = row$n_mdn[1],
      n_mode = row$n_mode[1],
      prop_at_mode = row$prop_at_mode[1],
      prop_stp_early = row$prop_stp_early[1],
      prop_stp_eff = row$prop_stp_eff[1],
      prop_stp_fut = row$prop_stp_fut[1],
      prop_no_dec = row$prop_no_dec[1]
    )
  }

  # Helper to build result
  build_result <- function(row, mode, target, closest_info = NULL) {
    param_cols <- setdiff(names(row), c("id_cond", "power"))
    condition_params <- as.list(row[1, param_cols, drop = FALSE])

    list(
      found = is.null(closest_info),
      mode = mode,
      target_pwr = target,
      n_total = row$n_total[1],
      achieved_pwr = row$power[1],
      condition_id = row$id_cond[1],
      condition_params = condition_params,
      interim = get_interim_stats(row$id_cond[1]),
      closest = closest_info
    )
  }

  # Mode 1: NULL target_pwr - return highest power condition
  if (is.null(target_pwr)) {
    best_idx <- which.max(merged$power)
    optimal <- merged[best_idx, , drop = FALSE]
    return(build_result(optimal, mode = "highest", target = NULL))
  }

  # Mode 2: Specific target_pwr - find smallest n_total meeting target
  meets_target <- merged[merged$power >= target_pwr, , drop = FALSE]

  if (nrow(meets_target) > 0) {
    # Find minimum n_total among those meeting target
    min_n <- min(meets_target$n_total, na.rm = TRUE)
    candidates <- meets_target[meets_target$n_total == min_n, , drop = FALSE]

    # If multiple, pick highest power
    best_idx <- which.max(candidates$power)
    optimal <- candidates[best_idx, , drop = FALSE]

    return(build_result(optimal, mode = "target", target = target_pwr))
  } else {
    # No condition meets target; find closest (highest power)
    best_idx <- which.max(merged$power)
    closest <- merged[best_idx, , drop = FALSE]

    param_cols <- setdiff(names(closest), c("id_cond", "power"))
    closest_params <- as.list(closest[1, param_cols, drop = FALSE])

    closest_info <- list(
      n_total = closest$n_total[1],
      achieved_pwr = closest$power[1],
      condition_id = closest$id_cond[1],
      condition_params = closest_params,
      interim = get_interim_stats(closest$id_cond[1])
    )

    return(list(
      found = FALSE,
      mode = "target",
      target_pwr = target_pwr,
      n_total = NA_real_,
      achieved_pwr = NA_real_,
      condition_id = NA_integer_,
      condition_params = NULL,
      interim = NULL,
      closest = closest_info
    ))
  }
}


# =============================================================================
# TOPIC-SPECIFIC REPORTS
# =============================================================================

#' Report on Power Metrics per Condition
#'
#' Displays power analysis metrics for each simulation condition, including
#' success/futility rates, posterior estimates, and convergence diagnostics.
#'
#' @param x An rctbp_power_analysis object with results
#' @param format Output format: "cli" for styled console output (default)
#'   or "markdown" for markdown-formatted output suitable for Quarto/RMarkdown.
#' @param heading_level Integer specifying the starting heading level for
#'   markdown output (default 2). Use this to integrate reports into documents
#'   where you need headings to start at a different level (e.g., 3 for `###`).
#'
#' @return Invisibly returns the input object. Prints report as side effect.
#'
#' @details
#' The report includes a table with:
#' \itemize{
#'   \item Condition identifiers and sample sizes
#'   \item Power metrics: `pwr_eff` (efficacy rate), `pwr_fut` (futility rate)
#'   \item Posterior estimates: `post_mn`, `post_sd`
#'   \item Convergence: `rhat`, `ess_bulk`
#' }
#'
#' For sequential designs, power metrics are taken from the final analysis look.
#'
#' @seealso [report()], [report_stopping()], [report_stopping_by_look()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Power metrics report
#' report_power(result)
#'
#' # Markdown format for Quarto integration
#' report_power(result, format = "markdown")
#' }
report_power <- function(x, format = c("cli", "markdown"), heading_level = 2L) {
  format <- match.arg(format)
  heading_level <- as.integer(heading_level)

  # Check for S7 class
  if (!inherits(x, "rctbp_power_analysis") &&
      !inherits(x, "rctbayespower::rctbp_power_analysis")) {
    cli::cli_abort("{.arg x} must be an rctbp_power_analysis object")
  }

  has_results <- nrow(x@results_conditions) > 0
  if (!has_results) {
    cli::cli_abort(c(
      "Analysis has not been run",
      "i" = "Use {.code run(x)} first"
    ))
  }

  # Get power metrics
  if (x@has_interim) {
    # For sequential: use final look from results_interim
    results_interim <- x@results_interim
    final_look <- max(results_interim$id_look)
    power_df <- results_interim[results_interim$id_look == final_look, , drop = FALSE]
  } else {
    # For single-look: use results_conditions directly
    power_df <- x@results_conditions
  }

  # Select relevant columns
  cols <- c("id_cond", "n_total", "par_name", "pwr_eff", "se_pwr_eff",
            "pwr_fut", "se_pwr_fut", "post_mn", "post_sd", "rhat", "ess_bulk")
  cols_available <- intersect(cols, names(power_df))
  power_table <- power_df[, cols_available, drop = FALSE]

  # Sort by power (descending)
  if ("pwr_eff" %in% names(power_table)) {
    power_table <- power_table[order(-power_table$pwr_eff), ]
  }

  # Build report
  report <- list(
    title = "Power Metrics by Condition",
    sections = list(
      list(
        name = "Power Results",
        grid = power_table
      )
    )
  )

  render_report(report, format = format, heading_level = heading_level)
  invisible(x)
}


#' Report on Early Stopping Metrics per Condition
#'
#' Displays early stopping statistics aggregated per condition. This report
#' is only available for sequential designs with interim analyses.
#'
#' @param x An rctbp_power_analysis object with sequential design results
#' @param format Output format: "cli" for styled console output (default)
#'   or "markdown" for markdown-formatted output suitable for Quarto/RMarkdown.
#' @param heading_level Integer specifying the starting heading level for
#'   markdown output (default 2). Use this to integrate reports into documents
#'   where you need headings to start at a different level (e.g., 3 for `###`).
#'
#' @return Invisibly returns the input object. Prints report as side effect.
#'
#' @details
#' The report includes a table with per-condition statistics:
#' \itemize{
#'   \item Sample sizes: `n_total`, `n_planned`, `n_mn`, `n_mdn`, `n_mode`
#'   \item Stopping proportions: `prop_stp_early`, `prop_stp_eff`, `prop_stp_fut`
#'   \item Modal stopping: `prop_at_mode` (proportion stopped at modal N)
#'   \item No decision rate: `prop_no_dec`
#' }
#'
#' @seealso [report()], [report_power()], [report_stopping_by_look()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Early stopping summary per condition
#' report_stopping(result)
#'
#' # Markdown format for Quarto integration
#' report_stopping(result, format = "markdown")
#' }
report_stopping <- function(x, format = c("cli", "markdown"), heading_level = 2L) {
  format <- match.arg(format)
  heading_level <- as.integer(heading_level)

  # Check for S7 class
  if (!inherits(x, "rctbp_power_analysis") &&
      !inherits(x, "rctbayespower::rctbp_power_analysis")) {
    cli::cli_abort("{.arg x} must be an rctbp_power_analysis object")
  }

  if (!x@has_interim) {
    cli::cli_abort(c(
      "Early stopping report requires a sequential design",
      "i" = "This analysis does not have interim analyses"
    ))
  }

  has_results <- nrow(x@results_conditions) > 0
  if (!has_results) {
    cli::cli_abort(c(
      "Analysis has not been run",
      "i" = "Use {.code run(x)} first"
    ))
  }

  # Use results_conditions which has overall stopping stats
  stopping_df <- x@results_conditions

  # Select relevant columns
  cols <- c("id_cond", "n_total", "n_planned", "n_mn", "se_n_mn", "n_mdn",
            "n_mode", "prop_at_mode", "prop_stp_early", "prop_stp_eff",
            "prop_stp_fut", "prop_no_dec")
  cols_available <- intersect(cols, names(stopping_df))
  stopping_table <- stopping_df[, cols_available, drop = FALSE]

  # Build report
  report <- list(
    title = "Early Stopping by Condition",
    sections = list(
      list(
        name = "Stopping Statistics",
        grid = stopping_table
      )
    )
  )

  render_report(report, format = format, heading_level = heading_level)
  invisible(x)
}


#' Report on Early Stopping per Look and Condition
#'
#' Displays detailed early stopping statistics broken down by analysis look
#' and condition. This report is only available for sequential designs.
#'
#' @param x An rctbp_power_analysis object with sequential design results
#' @param format Output format: "cli" for styled console output (default)
#'   or "markdown" for markdown-formatted output suitable for Quarto/RMarkdown.
#' @param heading_level Integer specifying the starting heading level for
#'   markdown output (default 2). Use this to integrate reports into documents
#'   where you need headings to start at a different level (e.g., 3 for `###`).
#'
#' @return Invisibly returns the input object. Prints report as side effect.
#'
#' @details
#' The report includes a table with per-look × per-condition statistics:
#' \itemize{
#'   \item Look identifiers: `id_cond`, `id_look`, `n_analyzed`
#'   \item Power at this look: `pwr_eff`, `pwr_fut`
#'   \item Stopping at this look: `prop_stp_look`, `prop_eff_look`, `prop_fut_look`
#'   \item Cumulative stopping: `cumul_stp`
#' }
#'
#' @seealso [report()], [report_power()], [report_stopping()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Per-look stopping breakdown
#' report_stopping_by_look(result)
#'
#' # Markdown format for Quarto integration
#' report_stopping_by_look(result, format = "markdown")
#' }
report_stopping_by_look <- function(x, format = c("cli", "markdown"),
                                     heading_level = 2L) {
  format <- match.arg(format)
  heading_level <- as.integer(heading_level)

  # Check for S7 class
  if (!inherits(x, "rctbp_power_analysis") &&
      !inherits(x, "rctbayespower::rctbp_power_analysis")) {
    cli::cli_abort("{.arg x} must be an rctbp_power_analysis object")
  }

  if (!x@has_interim) {
    cli::cli_abort(c(
      "Per-look report requires a sequential design",
      "i" = "This analysis does not have interim analyses"
    ))
  }

  has_results <- nrow(x@results_interim) > 0
  if (!has_results) {
    cli::cli_abort(c(
      "Analysis has not been run",
      "i" = "Use {.code run(x)} first"
    ))
  }

  # Use results_interim which has per-look data
  look_df <- x@results_interim
  n_looks <- length(unique(look_df$id_look))

  # Select relevant columns
  cols <- c("id_cond", "id_look", "n_analyzed", "pwr_eff", "pwr_fut",
            "prop_stp_look", "prop_eff_look", "prop_fut_look", "cumul_stp")
  cols_available <- intersect(cols, names(look_df))
  look_table <- look_df[, cols_available, drop = FALSE]

  # Sort by condition then look
  look_table <- look_table[order(look_table$id_cond, look_table$id_look), ]

  # Build report
  report <- list(
    title = paste0("Early Stopping by Look (", n_looks, " looks)"),
    sections = list(
      list(
        name = "Per-Look Statistics",
        grid = look_table
      )
    )
  )

  render_report(report, format = format, heading_level = heading_level)
  invisible(x)
}


# Legacy aliases for backward compatibility
#' @rdname report_stopping
#' @export
report_early_stopping <- report_stopping

#' @rdname report_power
#' @export
report_conditions <- report_power


#' Generate Topic-Specific Reports
#'
#' Unified interface for generating detailed reports on specific aspects
#' of power analysis results. Multiple topics can be specified to generate
#' concatenated reports.
#'
#' @param x An rctbp_power_analysis object
#' @param topic Character vector specifying report topic(s). Valid values:
#'   \describe{
#'     \item{"power"}{Power metrics per condition}
#'     \item{"stopping"}{Early stopping summary per condition (sequential only)}
#'     \item{"stopping_by_look"}{Early stopping per look × condition (sequential only)}
#'   }
#'   Multiple topics can be specified to generate concatenated reports.
#' @param format Output format: "cli" for styled console output (default)
#'   or "markdown" for markdown-formatted output suitable for Quarto/RMarkdown.
#' @param heading_level Integer specifying the starting heading level for
#'   markdown output (default 2). Use this to integrate reports into documents
#'   where you need headings to start at a different level (e.g., 3 for `###`).
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object.
#'
#' @seealso [report_power()], [report_stopping()], [report_stopping_by_look()],
#'   [summary.rctbp_power_analysis()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Power metrics per condition
#' report(result, topic = "power")
#'
#' # Early stopping summary per condition (sequential only)
#' report(result, topic = "stopping")
#'
#' # Per-look stopping breakdown (sequential only)
#' report(result, topic = "stopping_by_look")
#'
#' # Multiple topics - generates concatenated reports
#' report(result, topic = c("power", "stopping", "stopping_by_look"))
#'
#' # Markdown format for Quarto integration
#' report(result, topic = "power", format = "markdown")
#'
#' # Start headings at level 3 (###) for embedding in a document section
#' report(result, topic = "stopping", format = "markdown", heading_level = 3)
#' }
report <- function(x, topic = "power",
                   format = c("cli", "markdown"), heading_level = 2L, ...) {
  # Validate topics
  valid_topics <- c("power", "stopping", "stopping_by_look")
  invalid <- setdiff(topic, valid_topics)
  if (length(invalid) > 0) {
    cli::cli_abort(c(
      "Invalid topic{?s}: {.val {invalid}}",
      "i" = "Valid topics are: {.val {valid_topics}}"
    ))
  }

  format <- match.arg(format)
  heading_level <- as.integer(heading_level)

  if (heading_level < 1 || heading_level > 6) {
    cli::cli_abort("{.arg heading_level} must be between 1 and 6")
  }

  # Generate each report in sequence
  for (t in topic) {
    switch(t,
      "power" = report_power(x, format = format, heading_level = heading_level),
      "stopping" = report_stopping(x, format = format, heading_level = heading_level),
      "stopping_by_look" = report_stopping_by_look(x, format = format,
                                                    heading_level = heading_level)
    )
  }

  invisible(x)
}
