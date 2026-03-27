# =============================================================================
# OPTIMIZATION SINGLE: Single-Objective Sample Size BO
# =============================================================================
# Bayesian optimization for finding minimum sample size that achieves target
# power. Three surrogate strategies: gp_power (custom GP loop), gp_score
# (bbotk EGO on feasibility score), rf (random forest on feasibility score).

# =============================================================================
# FEASIBILITY SCORE
# =============================================================================

#' Compute Feasibility Score
#'
#' Computes the feasibility score for a sample size evaluation. Score is 0 for
#' infeasible points (power < target) and monotonically decreasing from n_min
#' to n_max for feasible points.
#'
#' @param power Observed power value
#' @param n Sample size
#' @param n_range Numeric vector c(n_min, n_max)
#' @param target_power Target power level
#' @param scale Score scale: "log" or "raw"
#' @param shape Score shape: "linear", "quadratic", or "root"
#'
#' @return Numeric score in \[0, 1\]
#' @keywords internal
compute_feasibility_score <- function(power, n, n_range, target_power,
                                      scale = "log", shape = "linear") {
  if (power < target_power) {
    return(0)
  }

  dist <- compute_normalized_distance(n, n_range, scale)

  switch(shape,
    "linear" = dist,
    "quadratic" = dist^2,
    "root" = sqrt(dist),
    dist
  )
}

#' Compute Normalized Distance
#'
#' Measures how close n is to n_min vs n_max, using either log or raw scale.
#'
#' @param n Sample size
#' @param n_range Numeric vector c(n_min, n_max)
#' @param scale "log" or "raw"
#'
#' @return Numeric value in \[0, 1\] where 1 = at n_min, 0 = at n_max
#' @keywords internal
compute_normalized_distance <- function(n, n_range, scale = "log") {
  n_min <- n_range[1]
  n_max <- n_range[2]

  if (scale == "log") {
    (log(n_max) - log(n)) / (log(n_max) - log(n_min))
  } else {
    (n_max - n) / (n_max - n_min)
  }
}

# =============================================================================
# INITIAL DESIGN
# =============================================================================

#' Generate Initial Design for Single-Objective BO
#'
#' Creates n_init points on log(n) scale within n_range.
#'
#' @param n_range Numeric vector c(n_min, n_max)
#' @param n_init Number of initial points
#'
#' @return Numeric vector of sample sizes (integer-valued)
#' @keywords internal
generate_single_bo_initial <- function(n_range, n_init) {
  log_min <- log(n_range[1])
  log_max <- log(n_range[2])
  log_points <- seq(log_min, log_max, length.out = n_init)
  unique(round(exp(log_points)))
}

# =============================================================================
# SINGLE-OBJECTIVE BO LOOP
# =============================================================================

#' Run Single-Objective Bayesian Optimization
#'
#' @param design rctbp_design object
#' @param target_power Target power level
#' @param n_range Numeric vector c(n_min, n_max)
#' @param constant Named list of fixed parameters
#' @param n_sims Number of simulations per evaluation
#' @param n_cores Number of parallel cores
#' @param surrogate Surrogate type: "gp_power", "gp_score", "rf"
#' @param score_shape Score shape: "linear", "quadratic", "root"
#' @param score_scale Score scale: "log", "raw"
#' @param n_init Number of initial design points
#' @param max_evals Maximum evaluations
#' @param patience Early stopping patience
#' @param seed Random seed
#' @param verbose Whether to show progress
#' @param bf_args BayesFlow arguments
#' @param brms_args brms arguments
#'
#' @return rctbp_sample_size_result object
#' @keywords internal
run_single_bo <- function(design, target_power, n_range, constant, n_sims,
                          n_cores, surrogate, score_shape, score_scale,
                          n_init, max_evals, patience, seed, verbose,
                          bf_args, brms_args) {

  start_time <- Sys.time()

  if (!is.null(seed)) set.seed(seed)

  # Dispatch to appropriate surrogate strategy
  if (surrogate == "gp_power") {
    result <- run_gp_power_loop(
      design = design, target_power = target_power, n_range = n_range,
      constant = constant, n_sims = n_sims, n_cores = n_cores,
      score_shape = score_shape, score_scale = score_scale,
      n_init = n_init, max_evals = max_evals, patience = patience,
      verbose = verbose, bf_args = bf_args, brms_args = brms_args
    )
  } else {
    result <- run_bbotk_score_loop(
      design = design, target_power = target_power, n_range = n_range,
      constant = constant, n_sims = n_sims, n_cores = n_cores,
      surrogate = surrogate, score_shape = score_shape,
      score_scale = score_scale, n_init = n_init, max_evals = max_evals,
      patience = patience, verbose = verbose,
      bf_args = bf_args, brms_args = brms_args
    )
  }

  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  result@elapsed_time <- elapsed_time

  result
}


# =============================================================================
# GP_POWER: Custom GP Loop on logit(power)
# =============================================================================

#' Run GP Power Custom Loop
#'
#' Fits GP to logit(power) on log(n) scale. Uses P_feas acquisition rule
#' to select next n. Does not use bbotk.
#'
#' @keywords internal
run_gp_power_loop <- function(design, target_power, n_range, constant,
                              n_sims, n_cores, score_shape, score_scale,
                              n_init, max_evals, patience, verbose,
                              bf_args, brms_args) {

  rlang::check_installed("DiceKriging", reason = "for GP surrogate in gp_power mode")

  # Generate initial design
  initial_ns <- generate_single_bo_initial(n_range, n_init)

  # Evaluate initial points
  archive <- data.frame(
    n_total = integer(0), power = numeric(0),
    score = numeric(0), n_sims = integer(0)
  )

  for (n in initial_ns) {
    power <- evaluate_power_at_n(
      design, n, constant, n_sims, n_cores, bf_args, brms_args, verbose
    )
    score <- compute_feasibility_score(
      power, n, n_range, target_power, score_scale, score_shape
    )
    archive <- rbind(archive, data.frame(
      n_total = n, power = power, score = score, n_sims = n_sims
    ))
  }

  # Best score tracking for patience

  best_score <- max(archive$score)
  patience_counter <- 0
  gp_model <- NULL

  # BO loop
  n_evaluated <- nrow(archive)
  while (n_evaluated < max_evals) {
    # Fit GP to logit(power) on log(n)
    x_vals <- log(archive$n_total)
    y_vals <- logit_transform(archive$power)

    X <- matrix(x_vals, ncol = 1)
    colnames(X) <- "x"

    gp_model <- tryCatch({
      DiceKriging::km(
        design = X, response = y_vals,
        covtype = "matern3_2", nugget.estim = TRUE,
        control = list(trace = FALSE)
      )
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_warning("GP fit failed: {e$message}")
      }
      NULL
    })

    if (is.null(gp_model)) {
      cli::cli_alert_warning(
        "GP surrogate fitting failed after {n_evaluated} evaluations; stopping early"
      )
      break
    }

    # Query GP at dense grid
    n_grid <- seq(n_range[1], n_range[2], length.out = 500)
    X_new <- matrix(log(n_grid), ncol = 1)
    colnames(X_new) <- "x"

    pred <- DiceKriging::predict(gp_model, newdata = X_new, type = "UK")
    p_feas <- compute_p_feas(pred$mean, pred$sd, target_power)

    # Acquisition: minimum n where P_feas >= 0.95
    alpha_threshold <- 0.95
    feasible_idx <- which(p_feas >= alpha_threshold)

    if (length(feasible_idx) > 0) {
      # Smallest n meeting threshold
      selected_idx <- feasible_idx[which.min(n_grid[feasible_idx])]
    } else {
      # Fallback: highest P_feas
      selected_idx <- which.max(p_feas)
    }

    next_n <- round(n_grid[selected_idx])

    # Avoid re-evaluating exact same n (nudge if duplicate)
    if (next_n %in% archive$n_total) {
      # Try adjacent values
      candidates <- setdiff(
        round(seq(max(n_range[1], next_n - 5), min(n_range[2], next_n + 5))),
        archive$n_total
      )
      if (length(candidates) > 0) {
        next_n <- candidates[which.min(abs(candidates - next_n))]
      } else {
        break
      }
    }

    # Evaluate
    power <- evaluate_power_at_n(
      design, next_n, constant, n_sims, n_cores, bf_args, brms_args, verbose
    )
    score <- compute_feasibility_score(
      power, next_n, n_range, target_power, score_scale, score_shape
    )
    archive <- rbind(archive, data.frame(
      n_total = next_n, power = power, score = score, n_sims = n_sims
    ))
    n_evaluated <- n_evaluated + 1

    # Check patience (starts after initial design)
    if (score > best_score) {
      best_score <- score
      patience_counter <- 0
    } else {
      patience_counter <- patience_counter + 1
    }

    if (patience_counter >= patience) {
      if (verbose) {
        cli::cli_alert_info("Early stopping: no improvement for {patience} evaluations")
      }
      break
    }

    if (verbose) {
      cli::cli_alert_info(
        "Eval {n_evaluated}/{max_evals}: n={next_n}, power={round(power, 3)}, score={round(score, 3)}"
      )
    }
  }

  # Extract result
  extract_single_result(
    archive = archive, design = design, target_power = target_power,
    surrogate_fit = gp_model, surrogate_type = "gp_power",
    score_config = list(scale = score_scale, shape = score_shape),
    n_sims = n_sims
  )
}


# =============================================================================
# GP_SCORE / RF: bbotk EGO Loop on Feasibility Score
# =============================================================================

#' Run bbotk EGO Loop on Feasibility Score
#'
#' Uses standard bbotk EGO with GP or RF surrogate fitted to feasibility
#' score values directly.
#'
#' @keywords internal
run_bbotk_score_loop <- function(design, target_power, n_range, constant,
                                 n_sims, n_cores, surrogate, score_shape,
                                 score_scale, n_init, max_evals, patience,
                                 verbose, bf_args, brms_args) {

  rlang::check_installed("mlr3mbo", reason = "for Bayesian optimization")
  rlang::check_installed("bbotk", reason = "for Bayesian optimization")
  rlang::check_installed("paradox", reason = "for parameter spaces")

  if (surrogate == "gp_score") {
    rlang::check_installed("DiceKriging", reason = "for GP surrogate")
    rlang::check_installed("mlr3learners", reason = "for GP surrogate")
  } else if (surrogate == "rf") {
    rlang::check_installed("ranger", reason = "for RF surrogate")
  }

  # Create parameter space (single parameter: n_total)
  domain <- paradox::ps(
    n_total = paradox::p_int(
      lower = as.integer(n_range[1]),
      upper = as.integer(n_range[2])
    )
  )

  # Codomain: maximize score
  codomain <- paradox::ps(
    score = paradox::p_dbl(tags = "maximize")
  )

  # Evaluation counter for tracking
  eval_count <- 0

  # Archive for our own tracking (bbotk also has one)
  our_archive <- data.frame(
    n_total = integer(0), power = numeric(0),
    score = numeric(0), n_sims = integer(0)
  )

  # Objective function
  obj_fn <- function(xs) {
    eval_count <<- eval_count + 1
    n <- xs$n_total

    power <- evaluate_power_at_n(
      design, n, constant, n_sims, n_cores, bf_args, brms_args, verbose
    )
    score <- compute_feasibility_score(
      power, n, n_range, target_power, score_scale, score_shape
    )

    our_archive <<- rbind(our_archive, data.frame(
      n_total = n, power = power, score = score, n_sims = n_sims
    ))

    if (verbose && eval_count > n_init) {
      cli::cli_alert_info(
        "Eval {eval_count}/{max_evals}: n={n}, power={round(power, 3)}, score={round(score, 3)}"
      )
    }

    list(score = score)
  }

  # bbotk setup
  objective <- bbotk::ObjectiveRFun$new(
    fun = obj_fn,
    domain = domain,
    codomain = codomain
  )

  # Combine evals limit with stagnation-based early stopping (patience)
  terminator <- bbotk::trm("combo",
    list(
      bbotk::trm("evals", n_evals = max_evals),
      bbotk::trm("stagnation", iters = patience, threshold = 0)
    ),
    any = TRUE
  )

  instance <- bbotk::OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = terminator
  )

  # Suppress logging
  suppress_mbo_logging(if (verbose) 1 else 0)

  # Surrogate model
  if (surrogate == "gp_score") {
    learner <- mlr3mbo::default_gp()
    learner$param_set$values$nugget.estim <- TRUE
    surr <- mlr3mbo::srlrn(learner, archive = instance$archive)
  } else {
    learner <- mlr3mbo::default_rf()
    surr <- mlr3mbo::srlrn(learner, archive = instance$archive)
  }
  surr$param_set$values$catch_errors <- TRUE

  # Acquisition function (EI)
  acq <- mlr3mbo::acqf("ei")

  # Acquisition optimizer
  acq_opt <- mlr3mbo::acqo(
    optimizer = bbotk::opt("random_search", batch_size = 1000),
    terminator = bbotk::trm("evals", n_evals = 1000)
  )
  acq_opt$param_set$values$logging_level <- "fatal"

  # Single-objective BO
  optimizer <- bbotk::opt(
    "mbo",
    loop_function = mlr3mbo::bayesopt_ego,
    surrogate = surr,
    acq_function = acq,
    acq_optimizer = acq_opt
  )

  # Generate and evaluate initial design
  init_pts <- paradox::generate_design_lhs(domain, n_init)$data
  instance$eval_batch(init_pts)

  # Run BO loop
  optimizer$optimize(instance)

  if (verbose && nrow(our_archive) < max_evals) {
    cli::cli_alert_info(
      "Early stopping after {nrow(our_archive)} evaluations (patience = {patience})"
    )
  }

  # Extract result
  extract_single_result(
    archive = our_archive, design = design, target_power = target_power,
    surrogate_fit = surr, surrogate_type = surrogate,
    score_config = list(scale = score_scale, shape = score_shape),
    n_sims = n_sims
  )
}


# =============================================================================
# HELPER: Evaluate Power at a Single n
# =============================================================================

#' Evaluate Power at a Single Sample Size
#'
#' @keywords internal
evaluate_power_at_n <- function(design, n, constant, n_sims, n_cores,
                                bf_args, brms_args, verbose) {
  all_params <- c(list(n_total = n), constant)

  conditions <- tryCatch({
    build_conditions(
      design = design,
      crossed = list(),
      constant = all_params
    )
  }, error = function(e) {
    if (verbose) {
      cli::cli_alert_warning("Conditions error at n={n}: {e$message}")
    }
    return(NULL)
  })

  if (is.null(conditions)) return(0)

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
    if (verbose) {
      cli::cli_alert_warning("Power analysis error at n={n}: {e$message}")
    }
    return(NULL)
  })

  if (is.null(pa_result)) return(0)

  # Extract power (pwr_eff)
  results_df <- pa_result@results_conditions
  if ("pwr_eff" %in% names(results_df)) {
    results_df$pwr_eff[1]
  } else {
    power_cols <- grep("^pwr_", names(results_df), value = TRUE)
    if (length(power_cols) > 0) {
      results_df[[power_cols[1]]][1]
    } else {
      0
    }
  }
}


# =============================================================================
# HELPER: Extract Single-Objective Result
# =============================================================================

#' Extract Single-Objective Result from Archive
#'
#' @keywords internal
extract_single_result <- function(archive, design, target_power,
                                  surrogate_fit, surrogate_type,
                                  score_config, n_sims) {
  # Find feasible points
  feasible <- archive[archive$power >= target_power, ]

  if (nrow(feasible) > 0) {
    # Smallest feasible n
    best_idx <- which.min(feasible$n_total)
    n_optimal <- as.integer(feasible$n_total[best_idx])
    power_optimal <- feasible$power[best_idx]
    is_feasible <- TRUE
  } else {
    # No feasible: return point with highest power
    best_idx <- which.max(archive$power)
    n_optimal <- as.integer(archive$n_total[best_idx])
    power_optimal <- archive$power[best_idx]
    is_feasible <- FALSE
    cli::cli_alert_warning(
      "No feasible solution found. Best power = {round(power_optimal, 3)} at n = {n_optimal}"
    )
  }

  # Build convergence trace
  best_score_so_far <- cummax(archive$score)
  convergence <- data.frame(
    eval = seq_len(nrow(archive)),
    n_total = archive$n_total,
    power = archive$power,
    score = archive$score,
    best_score = best_score_so_far
  )

  rctbp_sample_size_result(
    design = design,
    n_optimal = n_optimal,
    power_optimal = power_optimal,
    target_power = target_power,
    feasible = is_feasible,
    convergence = convergence,
    archive = archive,
    surrogate_fit = surrogate_fit,
    surrogate_type = surrogate_type,
    score_config = score_config,
    n_sims = n_sims,
    n_evals = nrow(archive),
    elapsed_time = NA_real_
  )
}
