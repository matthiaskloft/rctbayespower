# =============================================================================
# PARETO OPTIMIZATION: Core Function
# =============================================================================
# Core Pareto optimization function for trial design optimization.
# Supports any two objectives with maximize/minimize directions.
# Uses ParEGO via mlr3mbo for multi-objective Bayesian optimization.

#' Pareto Optimization for Trial Designs
#'
#' Core function for multi-objective Bayesian optimization of trial designs.
#' Finds the Pareto front trading off two objectives (e.g., power vs sample size).
#'
#' @param design An [build_design()] object specifying the trial design.
#' @param objectives Named list of exactly 2 objectives. Each element should be
#'   `"maximize"` or `"minimize"`. Names must be either power metrics
#'   (e.g., `pwr_eff`, `pwr_fut`) or search parameters.
#'   Example: `list(pwr_eff = "maximize", n_total = "minimize")`.
#' @param search Named list of parameter bounds to search over. Each element is a
#'   numeric vector `c(lower, upper)`. Can include:
#'   - Standard parameters: `n_total`, `b_arm_treat`, threshold parameters
#'   - Simplex specs: `p_alloc = search_p_alloc(min = 0.2)`,
#'     `analysis_at = search_looks(n = 3)`
#' @param constant Named list of fixed parameters for all evaluations.
#' @param constraint Optional constraint specification. A list with:
#'   - `metric`: Name of metric to constrain (e.g., `"pwr_eff"`)
#'   - `threshold`: Minimum value required (e.g., `0.80` for 80% power)
#'   Designs not meeting the constraint are excluded from the Pareto front.
#' @param n_sims Number of simulations per evaluation (scalar).
#' @param max_evals Maximum number of evaluations.
#' @param n_cores Number of parallel cores for power analysis.
#' @param knee_method Method for selecting from Pareto front:
#'   - `"utopia"` (default): Closest to ideal point in normalized space
#'   - `"min_cost"`: Minimize second objective
#'   - `"linear"`: Maximum perpendicular distance from endpoints line
#' @param surrogate Surrogate model type: `"gp"` (Gaussian Process, default) or
#'   `"rf"` (Random Forest).
#' @param init_design Initial design method: `"lhs"` (Latin Hypercube, default),
#'   `"sobol"`, or `"random"`.
#' @param init_design_size Number of initial design points (default: 4 * n_params).
#' @param bf_args Named list of BayesFlow-specific arguments.
#' @param brms_args Named list of brms-specific arguments.
#' @param verbosity Output level: 0 (silent), 1 (progress), 2 (debug).
#'
#' @return An `rctbp_pareto_result` object containing:
#'   - `pareto_front`: Data frame of Pareto-optimal solutions
#'   - `selected_design`: Knee point selected by `knee_method`
#'   - `archive`: All evaluated designs
#'   - `convergence`: Optimization progress trace
#'
#' @seealso [plot.rctbp_pareto_result]
#'
#' @export
#' @examples
#' \dontrun{
#' design <- build_design(predefined_model = "ancova_cont_2arms", target_params = "b_arm2")
#'
#' result <- pareto_optimize(
#'   design = design,
#'   objectives = list(pwr_eff = "maximize", n_total = "minimize"),
#'   search = list(n_total = c(50, 500)),
#'   constant = list(
#'     b_arm_treat = 0.3,
#'     thr_dec_eff = 0.975, thr_dec_fut = 0.5,
#'     thr_fx_eff = 0.2, thr_fx_fut = 0,
#'     p_alloc = list(c(0.5, 0.5)),
#'     intercept = 0, b_covariate = 0.3, sigma = 1
#'   ),
#'   n_sims = 500,
#'   max_evals = 30,
#'   n_cores = 4
#' )
#'
#' plot(result)
#' result@pareto_front
#' }
pareto_optimize <- function(design,
                            objectives,
                            search,
                            constant = list(),
                            constraint = NULL,
                            n_sims = 1000,
                            max_evals = 50,
                            n_cores = 1,
                            knee_method = "utopia",
                            surrogate = "gp",
                            init_design = "lhs",
                            init_design_size = NULL,
                            bf_args = list(),
                            brms_args = list(),
                            verbosity = 1) {
  # ===========================================================================
  # INPUT VALIDATION
  # ===========================================================================
  if (!inherits(design, "rctbp_design") &&
      !inherits(design, "rctbayespower::rctbp_design")) {
    cli::cli_abort(c(
      "{.arg design} must be an rctbp_design object",
      "x" = "Got: {.cls {class(design)}}"
    ))
  }

  # Validate objectives (exactly 2)
  if (!is.list(objectives) || length(objectives) != 2) {
    cli::cli_abort(c(
      "{.arg objectives} must be a list of exactly 2 objectives",
      "x" = "Got {length(objectives)} objective{?s}"
    ))
  }

  valid_directions <- c("maximize", "minimize", "max", "min")
  for (obj_name in names(objectives)) {
    direction <- objectives[[obj_name]]
    if (!direction %in% valid_directions) {
      cli::cli_abort(c(
        "Invalid objective direction for {.val {obj_name}}",
        "x" = "Got: {.val {direction}}",
        "i" = "Must be 'maximize' or 'minimize'"
      ))
    }
  }

  # Normalize directions
  objectives <- lapply(objectives, function(d) {
    if (d %in% c("max", "maximize")) "maximize" else "minimize"
  })

  # Validate knee_method
  valid_knee_methods <- c("utopia", "min_cost", "linear")
  if (!knee_method %in% valid_knee_methods) {
    cli::cli_abort(c(
      "Invalid {.arg knee_method}",
      "x" = "Got: {.val {knee_method}}",
      "i" = "Must be one of: {.val {valid_knee_methods}}"
    ))
  }

  # ===========================================================================
  # SETUP
  # ===========================================================================
  start_time <- Sys.time()

  # Set verbosity
  old_verbosity <- set_verbosity(verbosity)
  on.exit(set_verbosity(old_verbosity), add = TRUE)

  # Parse simplex search specs
  search_specs <- parse_simplex_specs(search, design)

  # ===========================================================================
  # DISPLAY CONFIGURATION
  # ===========================================================================
  if (should_show(1)) {
    cli::cli_h3("Pareto Optimization")
    obj_str <- paste(
      vapply(names(objectives), function(nm) {
        paste0(nm, " (", objectives[[nm]], ")")
      }, character(1)),
      collapse = ", "
    )
    cli::cli_dl(c(
      "Objectives" = obj_str,
      "Search parameters" = paste(names(search), collapse = ", "),
      "Max evaluations" = max_evals,
      "Backend" = design@backend
    ))

    if (!is.null(constraint)) {
      cli::cli_dl(c(
        "Constraint" = paste0(constraint$metric, " >= ", constraint$threshold)
      ))
    }
    cli::cli_text("")
  }

  # ===========================================================================
  # CREATE PARAMETER SPACE
  # ===========================================================================
  domain <- create_parameter_space(search, search_specs)

  # ===========================================================================
  # CREATE CODOMAIN (2 objectives)
  # ===========================================================================
  codomain <- create_pareto_codomain(objectives)

  # ===========================================================================
  # CREATE OBJECTIVE FUNCTION
  # ===========================================================================
  obj_fn_result <- create_pareto_objective_fn(
    design = design,
    objectives = objectives,
    search = search,
    search_specs = search_specs,
    constant = constant,
    constraint = constraint,
    n_sims = n_sims,
    n_cores = n_cores,
    bf_args = bf_args,
    brms_args = brms_args,
    verbosity = verbosity
  )
  obj_fn <- obj_fn_result$fn

  # ===========================================================================
  # SET UP BBOTK MULTI-OBJECTIVE INSTANCE
  # ===========================================================================
  objective <- bbotk::ObjectiveRFun$new(
    fun = obj_fn,
    domain = domain,
    codomain = codomain
  )

  instance <- bbotk::OptimInstanceBatchMultiCrit$new(
    objective = objective,
    terminator = bbotk::trm("evals", n_evals = max_evals)
  )

  # ===========================================================================
  # PREPARE INITIAL DESIGN
  # ===========================================================================
  if (is.null(init_design_size)) {
    init_design_size <- 4 * length(search)
  }

  if (should_show(1)) {
    cli::cli_alert_info("Generating initial design ({init_design_size} points)")
  }
  init_design_pts <- generate_initial_design(
    domain = domain,
    n = init_design_size,
    method = init_design
  )

  # ===========================================================================
  # SETUP MBO COMPONENTS (ParEGO for multi-objective)
  # ===========================================================================
  mbo_components <- setup_pareto_mbo(instance, surrogate, verbosity)

  # ===========================================================================
  # RUN OPTIMIZATION
  # ===========================================================================
  if (should_show(1)) {
    cli::cli_alert_info("Running Pareto optimization...")
  }

  # Suppress mlr3/bbotk logging
  suppress_mbo_logging(verbosity)

  # Evaluate initial design
  instance$eval_batch(init_design_pts)

  # Run MBO loop
  mbo_components$optimizer$optimize(instance)

  # ===========================================================================
  # EXTRACT RESULTS
  # ===========================================================================
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))

  # Get archive
  archive_df <- as.data.frame(instance$archive$data)

  # Extract Pareto front
  pareto_df <- extract_pareto_front(instance, objectives)

  # Apply constraint filter if specified
  if (!is.null(constraint) && nrow(pareto_df) > 0) {
    constraint_col <- constraint$metric
    if (constraint_col %in% names(pareto_df)) {
      pareto_df <- pareto_df[pareto_df[[constraint_col]] >= constraint$threshold, ]
    }
  }

  # Select knee point
  if (nrow(pareto_df) > 0) {
    selected <- select_knee_point(
      pareto_df = pareto_df,
      objectives = objectives,
      method = knee_method
    )
  } else {
    selected <- data.frame()
  }

  # Build convergence trace
  convergence_df <- build_pareto_convergence(archive_df, objectives)

  # ===========================================================================
  # CREATE RESULT OBJECT
  # ===========================================================================
  result <- rctbp_pareto_result(
    design = design,
    pareto_front = pareto_df,
    archive = archive_df,
    selected_design = selected,
    convergence = convergence_df,
    optimization_type = "custom",
    objectives = objectives,
    search = search,
    n_sims = n_sims,
    n_evals = nrow(archive_df),
    elapsed_time = elapsed_time,
    mbo_objects = list(
      instance = instance,
      surrogate = mbo_components$surrogate,
      acq_function = mbo_components$acq_function,
      acq_optimizer = mbo_components$acq_optimizer,
      optimizer = mbo_components$optimizer
    )
  )

  # ===========================================================================
  # DISPLAY RESULTS
  # ===========================================================================
  if (should_show(1)) {
    cli::cli_alert_success(
      "Optimization complete in {round(elapsed_time, 2)} min"
    )
    cli::cli_alert_info(
      "Found {nrow(pareto_df)} Pareto-optimal solution{?s}"
    )
  }

  result
}


# =============================================================================
# KNEE POINT SELECTION
# =============================================================================

#' Select Knee Point from Pareto Front
#'
#' @param pareto_df Data frame of Pareto-optimal solutions
#' @param objectives Named list of objectives with directions
#' @param method Selection method: "utopia", "min_cost", or "linear"
#'
#' @return Data frame with single selected row
#' @keywords internal
select_knee_point <- function(pareto_df, objectives, method = "utopia") {
  if (nrow(pareto_df) == 0) {
    return(data.frame())
  }

  if (nrow(pareto_df) == 1) {
    return(pareto_df)
  }

  obj_names <- names(objectives)
  obj1 <- obj_names[1]
  obj2 <- obj_names[2]

  switch(method,
    "utopia" = find_knee_utopia(pareto_df, obj1, obj2, objectives),
    "min_cost" = find_knee_min_cost(pareto_df, obj2),
    "linear" = find_knee_linear(pareto_df, obj1, obj2, objectives),
    find_knee_utopia(pareto_df, obj1, obj2, objectives)
  )
}


#' Find Knee Point Using Utopia Distance
#'
#' Selects the Pareto point closest to the ideal point in normalized space.
#'
#' @param pareto_df Data frame of Pareto-optimal solutions
#' @param obj1 Name of first objective
#' @param obj2 Name of second objective
#' @param objectives Named list with directions
#'
#' @return Data frame with single selected row
#' @keywords internal
find_knee_utopia <- function(pareto_df, obj1, obj2, objectives) {
  if (!all(c(obj1, obj2) %in% names(pareto_df))) {
    return(pareto_df[1, , drop = FALSE])
  }

  vals1 <- pareto_df[[obj1]]
  vals2 <- pareto_df[[obj2]]

  # Normalize to [0, 1]
  range1 <- range(vals1, na.rm = TRUE)
  range2 <- range(vals2, na.rm = TRUE)

  if (diff(range1) == 0) {
    norm1 <- rep(0.5, length(vals1))
  } else {
    norm1 <- (vals1 - range1[1]) / diff(range1)
  }

  if (diff(range2) == 0) {
    norm2 <- rep(0.5, length(vals2))
  } else {
    norm2 <- (vals2 - range2[1]) / diff(range2)
  }

  # Flip if maximizing (ideal is at 1 for maximize, 0 for minimize)
  if (objectives[[obj1]] == "maximize") {
    norm1 <- 1 - norm1
  }
  if (objectives[[obj2]] == "maximize") {
    norm2 <- 1 - norm2
  }

  # Distance to origin (ideal point after flip)
  distances <- sqrt(norm1^2 + norm2^2)

  knee_idx <- which.min(distances)
  pareto_df[knee_idx, , drop = FALSE]
}


#' Find Knee Point by Minimizing Second Objective
#'
#' @param pareto_df Data frame of Pareto-optimal solutions
#' @param obj2 Name of second objective (to minimize)
#'
#' @return Data frame with single selected row
#' @keywords internal
find_knee_min_cost <- function(pareto_df, obj2) {
  if (!obj2 %in% names(pareto_df)) {
    return(pareto_df[1, , drop = FALSE])
  }

  knee_idx <- which.min(pareto_df[[obj2]])
  pareto_df[knee_idx, , drop = FALSE]
}


#' Find Knee Point Using Linear Method
#'
#' Selects the point with maximum perpendicular distance from the line
#' connecting the endpoints of the Pareto front.
#'
#' @param pareto_df Data frame of Pareto-optimal solutions
#' @param obj1 Name of first objective
#' @param obj2 Name of second objective
#' @param objectives Named list with directions
#'
#' @return Data frame with single selected row
#' @keywords internal
find_knee_linear <- function(pareto_df, obj1, obj2, objectives) {
  if (!all(c(obj1, obj2) %in% names(pareto_df))) {
    return(pareto_df[1, , drop = FALSE])
  }

  if (nrow(pareto_df) <= 2) {
    return(pareto_df[1, , drop = FALSE])
  }

  vals1 <- pareto_df[[obj1]]
  vals2 <- pareto_df[[obj2]]

  # Normalize to [0, 1]
  range1 <- range(vals1, na.rm = TRUE)
  range2 <- range(vals2, na.rm = TRUE)

  if (diff(range1) == 0 || diff(range2) == 0) {
    return(pareto_df[1, , drop = FALSE])
  }

  norm1 <- (vals1 - range1[1]) / diff(range1)
  norm2 <- (vals2 - range2[1]) / diff(range2)

  # Find endpoints
  idx_min1 <- which.min(norm1)
  idx_max1 <- which.max(norm1)

  # Line from endpoint to endpoint
  x1 <- norm1[idx_min1]
  y1 <- norm2[idx_min1]
  x2 <- norm1[idx_max1]
  y2 <- norm2[idx_max1]

  # Perpendicular distance from each point to line
  # Line: (y2-y1)*x - (x2-x1)*y + (x2-x1)*y1 - (y2-y1)*x1 = 0
  a <- y2 - y1
  b <- -(x2 - x1)
  c <- (x2 - x1) * y1 - (y2 - y1) * x1

  distances <- abs(a * norm1 + b * norm2 + c) / sqrt(a^2 + b^2)

  knee_idx <- which.max(distances)
  pareto_df[knee_idx, , drop = FALSE]
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' Create Codomain for Pareto Optimization
#'
#' @param objectives Named list with directions
#' @return paradox::ParamSet
#' @keywords internal
create_pareto_codomain <- function(objectives) {
  codomain_list <- lapply(names(objectives), function(obj_name) {
    direction <- objectives[[obj_name]]
    if (direction == "maximize") {
      paradox::p_dbl(tags = "maximize")
    } else {
      paradox::p_dbl(tags = "minimize")
    }
  })
  names(codomain_list) <- names(objectives)
  do.call(paradox::ps, codomain_list)
}


#' Create Objective Function for Pareto Optimization
#'
#' @param design rctbp_design object
#' @param objectives Named list of objectives
#' @param search Named list of search bounds
#' @param search_specs Parsed simplex specs
#' @param constant Fixed parameters
#' @param constraint Optional constraint
#' @param n_sims Number of simulations per evaluation
#' @param n_cores Number of cores
#' @param bf_args BayesFlow arguments
#' @param brms_args brms arguments
#' @param verbosity Verbosity level
#'
#' @return List with fn (objective function)
#' @keywords internal
create_pareto_objective_fn <- function(design,
                                       objectives,
                                       search,
                                       search_specs,
                                       constant,
                                       constraint,
                                       n_sims,
                                       n_cores,
                                       bf_args,
                                       brms_args,
                                       verbosity) {
  # Capture references
  search_params <- names(search)
  obj_names <- names(objectives)

  # Objective function
  obj_fn <- function(xs) {
    # Apply simplex transforms
    params <- apply_simplex_transforms_flat(xs, search_specs)

    # Merge with constant parameters
    all_params <- c(params, constant)

    # Build conditions
    conditions <- tryCatch({
      build_conditions(
        design = design,
        crossed = list(),
        constant = all_params
      )
    }, error = function(e) {
      if (verbosity >= 2) {
        cli::cli_alert_warning("Conditions error: {e$message}")
      }
      return(NULL)
    })

    if (is.null(conditions)) {
      # Return worst values for invalid params
      obj_values <- lapply(objectives, function(dir) {
        if (dir == "maximize") -Inf else Inf
      })
      names(obj_values) <- obj_names
      return(obj_values)
    }

    # Run power analysis
    pa_result <- tryCatch({
      power_analysis(
        conditions = conditions,
        n_sims = n_sims,
        n_cores = n_cores,
        bf_args = bf_args,
        brms_args = brms_args,
        verbosity = 0
      )
    }, error = function(e) {
      if (verbosity >= 2) {
        cli::cli_alert_warning("Power analysis error: {e$message}")
      }
      return(NULL)
    })

    if (is.null(pa_result)) {
      obj_values <- lapply(objectives, function(dir) {
        if (dir == "maximize") -Inf else Inf
      })
      names(obj_values) <- obj_names
      return(obj_values)
    }

    # Extract objective values
    results_df <- pa_result@results_conditions
    obj_values <- list()

    for (obj_name in obj_names) {
      if (obj_name %in% names(results_df)) {
        # Objective is a result metric (e.g., pwr_eff)
        obj_values[[obj_name]] <- results_df[[obj_name]][1]
      } else if (obj_name %in% names(params)) {
        # Objective is a search parameter (e.g., n_total)
        obj_values[[obj_name]] <- params[[obj_name]]
      } else if (obj_name %in% names(all_params)) {
        # Objective is in constant params
        obj_values[[obj_name]] <- all_params[[obj_name]]
      } else {
        # Fallback
        if (objectives[[obj_name]] == "maximize") {
          obj_values[[obj_name]] <- -Inf
        } else {
          obj_values[[obj_name]] <- Inf
        }
      }
    }

    obj_values
  }

  list(fn = obj_fn)
}


#' Extract Pareto Front from Instance
#'
#' @param instance bbotk OptimInstanceBatchMultiCrit
#' @param objectives Named list of objectives
#'
#' @return Data frame of Pareto-optimal solutions
#' @keywords internal
extract_pareto_front <- function(instance, objectives) {
  # Get best (Pareto-optimal) solutions from archive
  pareto_data <- instance$archive$best()

  if (is.null(pareto_data) || nrow(pareto_data) == 0) {
    return(data.frame())
  }

  as.data.frame(pareto_data)
}


#' Setup MBO Components for Pareto Optimization
#'
#' @param instance bbotk instance
#' @param surrogate Surrogate type
#' @param verbosity Verbosity level
#'
#' @return List with surrogate, acq_function, acq_optimizer, optimizer
#' @keywords internal
setup_pareto_mbo <- function(instance, surrogate, verbosity) {
  # Surrogate model
  if (surrogate == "gp") {
    rlang::check_installed("mlr3learners", reason = "for GP surrogate model")
    rlang::check_installed("DiceKriging", reason = "for GP surrogate model")

    gp_learner <- mlr3mbo::default_gp()
    gp_learner$param_set$values$nugget.estim <- TRUE
    surr <- mlr3mbo::srlrn(gp_learner, archive = instance$archive)
  } else if (surrogate == "rf") {
    rlang::check_installed("ranger", reason = "for RF surrogate model")
    rf_learner <- mlr3mbo::default_rf()
    surr <- mlr3mbo::srlrn(rf_learner, archive = instance$archive)
  } else {
    surr <- mlr3mbo::default_surrogate(instance)
  }

  surr$param_set$values$catch_errors <- TRUE

  # Acquisition function (EI for ParEGO)
  acq <- mlr3mbo::acqf("ei")

  # Acquisition optimizer
  acq_opt <- mlr3mbo::acqo(
    optimizer = bbotk::opt("random_search", batch_size = 1000),
    terminator = bbotk::trm("evals", n_evals = 1000)
  )

  if (verbosity < 2) {
    acq_opt$param_set$values$logging_level <- "fatal"
  }

  # ParEGO for multi-objective
  loop_fn <- mlr3mbo::bayesopt_parego

  optimizer <- bbotk::opt(
    "mbo",
    loop_function = loop_fn,
    surrogate = surr,
    acq_function = acq,
    acq_optimizer = acq_opt
  )

  list(
    surrogate = surr,
    acq_function = acq,
    acq_optimizer = acq_opt,
    optimizer = optimizer
  )
}


#' Suppress MBO Logging
#'
#' @param verbosity Verbosity level
#' @keywords internal
suppress_mbo_logging <- function(verbosity) {
  if (requireNamespace("lgr", quietly = TRUE)) {
    mlr3_logger <- lgr::get_logger("mlr3")
    bbotk_logger <- lgr::get_logger("bbotk")
    mlr3mbo_logger <- lgr::get_logger("mlr3mbo")

    if (verbosity <= 1) {
      mlr3_logger$set_threshold("off")
      bbotk_logger$set_threshold("off")
      mlr3mbo_logger$set_threshold("off")
    } else {
      mlr3_logger$set_threshold("error")
      bbotk_logger$set_threshold("error")
      mlr3mbo_logger$set_threshold("error")
    }
  }

  options(datatable.verbose = FALSE, datatable.print.class = FALSE)
}


#' Build Convergence Trace for Pareto Optimization
#'
#' @param archive_df Archive data frame
#' @param objectives Named list of objectives
#'
#' @return Data frame with convergence trace
#' @keywords internal
build_pareto_convergence <- function(archive_df, objectives) {
  if (nrow(archive_df) == 0) {
    return(data.frame())
  }

  obj_names <- names(objectives)

  # Track hypervolume or first objective
  obj_col <- obj_names[1]
  if (!obj_col %in% names(archive_df)) {
    return(data.frame())
  }

  values <- archive_df[[obj_col]]
  is_max <- objectives[[obj_col]] == "maximize"

  if (is_max) {
    cumulative_best <- cummax(values)
  } else {
    cumulative_best <- cummin(values)
  }

  data.frame(
    eval = seq_len(nrow(archive_df)),
    value = values,
    best_so_far = cumulative_best
  )
}


#' Parse Simplex Search Specifications
#'
#' @param search Named list of search bounds
#' @param design rctbp_design object
#'
#' @return Named list of parsed simplex specs
#' @keywords internal
parse_simplex_specs <- function(search, design) {
  specs <- list()

  for (param_name in names(search)) {
    spec <- search[[param_name]]

    if (inherits(spec, "rctbp_search_p_alloc")) {
      # Allocation probability search
      n_arms <- design@n_arms

      specs[[param_name]] <- list(
        type = "p_alloc",
        min_prop = spec$min_prop,
        n_arms = n_arms,
        n_dims = n_arms - 1
      )

    } else if (inherits(spec, "rctbp_search_looks")) {
      # Interim look timing search
      specs[[param_name]] <- list(
        type = "looks",
        n_looks = spec$n_looks,
        min_spacing = spec$min_spacing,
        n_dims = spec$n_looks - 1
      )
    }
  }

  specs
}


#' Apply Simplex Transforms to Parameters (Flat Return)
#'
#' Returns a flat named list with transformed parameters, suitable for merging
#' with `c(params, constant)`. See also [apply_simplex_transforms()] in
#' `optimization_transforms.R` which returns a structured list with `$crossed`,
#' `$ilr_values`, and `$simplex_values`.
#'
#' @param xs Named list of raw parameter values
#' @param search_specs Parsed simplex specifications
#'
#' @return Named list with transformed parameters (flat, no nested structure)
#' @keywords internal
apply_simplex_transforms_flat <- function(xs, search_specs) {
  params <- as.list(xs)

  for (param_name in names(search_specs)) {
    spec <- search_specs[[param_name]]

    if (spec$type == "p_alloc") {
      if (spec$n_arms == 2) {
        # 2-arm: direct transform
        treat_prob <- params[[param_name]]
        params[[param_name]] <- list(c(1 - treat_prob, treat_prob))
        # Remove the raw param
      } else {
        # k-arm: ILR inverse
        ilr_vals <- numeric(spec$n_dims)
        for (i in seq_len(spec$n_dims)) {
          ilr_name <- paste0(param_name, "_ilr_", i)
          ilr_vals[i] <- params[[ilr_name]]
          params[[ilr_name]] <- NULL
        }
        q <- ilr_inverse(ilr_vals)
        p <- constrained_simplex(q, spec$min_prop, spec$n_arms)
        params[[param_name]] <- list(p)
      }

    } else if (spec$type == "looks") {
      if (spec$n_looks == 2) {
        # 2-look: direct transform
        first_look <- params[[param_name]]
        params[[param_name]] <- c(first_look, 1.0)
      } else {
        # k-look: ILR inverse on increments
        ilr_vals <- numeric(spec$n_dims)
        for (i in seq_len(spec$n_dims)) {
          ilr_name <- paste0(param_name, "_ilr_", i)
          ilr_vals[i] <- params[[ilr_name]]
          params[[ilr_name]] <- NULL
        }
        increments <- ilr_inverse(ilr_vals)
        increments <- constrained_simplex(increments, spec$min_spacing, spec$n_looks)
        params[[param_name]] <- cumsum(increments)
      }
    }
  }

  params
}
