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
          "Probability threshold for success" = x@p_sig_scs,
          "Probability threshold for futility" = x@p_sig_ftl
        ),
        note = "Effect size thresholds are specified per-condition in build_conditions()."
      ),
      list(
        name = "Interim Analysis (Design Defaults)",
        items = list(
          "Analysis timepoints" = if (is.null(x@analysis_at)) "None (single-look)" else paste(x@analysis_at, collapse = ", "),
          "Stopping rules" = if (is.null(x@interim_function)) {
            if (is.null(x@analysis_at)) "N/A (single-look)" else "Default (dec_scs=1 or dec_ftl=1)"
          } else {
            "Custom (interim_function specified)"
          },
          "Adaptive design" = x@adaptive
        ),
        note = if (!is.null(x@analysis_at)) {
          "These defaults apply to all conditions unless overridden."
        } else NULL
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

  target_pwr_display <- if (is.null(x@target_pwr)) {
    "Not set (will show highest power)"
  } else {
    paste0(round(x@target_pwr * 100, 1), "%")
  }

  list(
    title = "S7 Object: rctbp_conditions",
    sections = list(
      list(
        name = "Summary",
        items = list(
          "Number of conditions" = n_conditions,
          "Number of varying parameters" = n_params,
          "Number of static parameters" = n_static_params,
          "Target power for optimal condition" = target_pwr_display
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

  report <- list(
    title = "S7 Object: rctbp_power_analysis",
    sections = list(
      list(
        name = "Design Summary",
        items = list(
          "Target parameters" = paste(design@target_params, collapse = ", "),
          "Success probability threshold" = design@p_sig_scs,
          "Futility probability threshold" = design@p_sig_ftl,
          "Target power" = target_pwr_display
        )
      )
    )
  )

  if (has_results) {
    # Completed analysis
    n_conditions <- nrow(x@conditions@conditions_grid)

    # Check for interim analysis results (via S7 property)
    has_interim <- x@has_interim
    # For sequential: power metrics in results_interim, overall stats in results_conditions
    # For single-look: power metrics in results_conditions
    results_df <- if (has_interim) x@results_interim else x@results_conditions
    interim_overall <- if (has_interim) x@results_conditions else NULL

    # Power ranges
    power_ranges <- NULL
    power_cols <- intersect(names(results_df), c("pwr_scs", "pwr_ftl"))
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

    # Add interim analysis section if applicable
    if (has_interim && !is.null(interim_overall)) {
      # Compute aggregate interim statistics across conditions
      avg_n_mn <- mean(interim_overall$n_mn, na.rm = TRUE)
      avg_planned_n <- mean(interim_overall$n_planned, na.rm = TRUE)
      avg_prop_stp <- mean(interim_overall$prop_stp_early, na.rm = TRUE)
      avg_prop_scs <- mean(interim_overall$prop_stp_scs, na.rm = TRUE)
      avg_prop_ftl <- mean(interim_overall$prop_stp_ftl, na.rm = TRUE)
      avg_prop_no_dec <- mean(interim_overall$prop_no_dec, na.rm = TRUE)

      # Number of analysis looks
      n_looks <- length(unique(results_df$id_look))

      report$sections <- c(report$sections, list(
        list(
          name = "Interim Analysis Summary",
          items = list(
            "Number of analysis looks" = n_looks,
            "Trials stopped early (avg)" = paste0(round(avg_prop_stp * 100, 1), "%"),
            "Stopped for success (avg)" = paste0(round(avg_prop_scs * 100, 1), "%"),
            "Stopped for futility (avg)" = paste0(round(avg_prop_ftl * 100, 1), "%"),
            "No decision (avg)" = paste0(round(avg_prop_no_dec * 100, 1), "%"),
            "Mean sample size (avg)" = round(avg_n_mn, 0),
            "Planned sample size (avg)" = round(avg_planned_n, 0)
          ),
          note = "Averages computed across all conditions. Use x@results_conditions for per-condition details."
        )
      ))
    }

    # Find optimal condition for target power
    optimal <- find_optimal_condition(
      results_summ = results_df,
      conditions_grid = x@conditions@conditions_grid,
      target_pwr = target_pwr,
      interim_overall = interim_overall,
      power_col = "pwr_scs"
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
        prop_stp_scs = paste0(round(interim$prop_stp_scs * 100, 1), "%"),
        prop_stp_ftl = paste0(round(interim$prop_stp_ftl * 100, 1), "%"),
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
#' @param power_col Column name for power values (default "pwr_scs")
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
                                    power_col = "pwr_scs") {
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
      prop_stp_scs = row$prop_stp_scs[1],
      prop_stp_ftl = row$prop_stp_ftl[1],
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
