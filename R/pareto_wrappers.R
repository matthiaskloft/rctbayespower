# =============================================================================
# PARETO OPTIMIZATION: Wrapper Functions
# =============================================================================
# User-friendly wrapper functions for common optimization scenarios:
# - optimize_power_n(): Power vs Sample Size (fixed effect)
# - optimize_power_effect(): Power vs Effect Size (fixed n)
# - optimize_effect_n(): Effect Size vs Sample Size (fixed power)

#' Optimize Power vs Sample Size
#'
#' Finds the Pareto front of power vs sample size for a fixed treatment effect.
#' This is the most common use case: determining how much power is achievable
#' at different sample sizes.
#'
#' @param design An [build_design()] object specifying the trial design.
#' @param power_metric Power metric to maximize: `"pwr_eff"` (default),
#'   `"pwr_fut"`, or other power columns from results.
#' @param n_range Numeric vector `c(lower, upper)` for sample size search bounds.
#' @param effect_size Fixed treatment effect size (e.g., standardized mean difference).
#'   This is the `b_arm_treat` parameter for ANCOVA models.
#' @param constant Named list of other fixed parameters. Should include all
#'   parameters required by the model except `n_total` and `b_arm_treat`.
#' @param search Optional named list of additional search parameters beyond
#'   `n_total` (e.g., threshold parameters).
#' @param n_sims Number of simulations per evaluation. Vector for progressive
#'   fidelity (e.g., `c(500, 2000)`).
#' @param evals_per_step Evaluations per fidelity level.
#' @param max_evals Maximum evaluations (when `n_sims` is scalar).
#' @param n_cores Number of parallel cores.
#' @param knee_method Knee selection method: `"utopia"`, `"min_cost"`, `"linear"`.
#' @param surrogate Surrogate model: `"gp"` or `"rf"`.
#' @param bf_args BayesFlow-specific arguments.
#' @param brms_args brms-specific arguments.
#' @param verbosity Output level (0, 1, 2).
#'
#' @return An `rctbp_pareto_result` object with `optimization_type = "power_n"`.
#'
#' @seealso [pareto_optimize()], [optimize_power_effect()], [optimize_effect_n()]
#'
#' @export
#' @examples
#' \dontrun{
#' design <- build_design(predefined_model = "ancova_cont_2arms", target_params = "b_arm2")
#'
#' result <- optimize_power_n(
#'   design = design,
#'   power_metric = "pwr_eff",
#'   n_range = c(50, 500),
#'   effect_size = 0.3,
#'   constant = list(
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
#' }
optimize_power_n <- function(design,
                             power_metric = "pwr_eff",
                             n_range = c(50, 500),
                             effect_size,
                             constant = list(),
                             search = list(),
                             n_sims = 1000,
                             evals_per_step = 10,
                             max_evals = 50,
                             n_cores = 1,
                             knee_method = "utopia",
                             surrogate = "gp",
                             bf_args = list(),
                             brms_args = list(),
                             verbosity = 1) {
  # Validate effect_size

  if (missing(effect_size)) {
    cli::cli_abort(c(
      "{.arg effect_size} is required",
      "i" = "Specify the fixed treatment effect size (e.g., 0.3)"
    ))
  }

  # Add effect_size to constant
  constant$b_arm_treat <- effect_size

  # Build search space (n_total + any additional)
  full_search <- c(list(n_total = n_range), search)

  # Build objectives
  objectives <- stats::setNames(
    list("maximize", "minimize"),
    c(power_metric, "n_total")
  )

  # Run optimization
  result <- pareto_optimize(
    design = design,
    objectives = objectives,
    search = full_search,
    constant = constant,
    n_sims = n_sims,
    evals_per_step = evals_per_step,
    max_evals = max_evals,
    n_cores = n_cores,
    knee_method = knee_method,
    surrogate = surrogate,
    bf_args = bf_args,
    brms_args = brms_args,
    verbosity = verbosity
  )

  # Update optimization type
  result@optimization_type <- "power_n"

  result
}


#' Optimize Power vs Effect Size
#'
#' Finds the Pareto front of power vs effect size for a fixed sample size.
#' Useful for understanding the minimum detectable effect (MDE) at a given
#' sample size.
#'
#' @param design An [build_design()] object specifying the trial design.
#' @param power_metric Power metric to maximize: `"pwr_eff"` (default),
#'   `"pwr_fut"`, or other power columns from results.
#' @param effect_range Numeric vector `c(lower, upper)` for effect size search bounds.
#' @param n_total Fixed sample size.
#' @param constant Named list of other fixed parameters.
#' @param search Optional named list of additional search parameters.
#' @param n_sims Number of simulations per evaluation.
#' @param evals_per_step Evaluations per fidelity level.
#' @param max_evals Maximum evaluations (when `n_sims` is scalar).
#' @param n_cores Number of parallel cores.
#' @param knee_method Knee selection method: `"utopia"`, `"min_cost"`, `"linear"`.
#' @param surrogate Surrogate model: `"gp"` or `"rf"`.
#' @param bf_args BayesFlow-specific arguments.
#' @param brms_args brms-specific arguments.
#' @param verbosity Output level (0, 1, 2).
#'
#' @return An `rctbp_pareto_result` object with `optimization_type = "power_effect"`.
#'
#' @details
#' The second objective minimizes effect size. This is useful because larger
#' effects are easier to detect - finding the smallest effect that achieves
#' good power identifies the minimum detectable effect.
#'
#' @seealso [pareto_optimize()], [optimize_power_n()], [optimize_effect_n()]
#'
#' @export
#' @examples
#' \dontrun{
#' design <- build_design(predefined_model = "ancova_cont_2arms", target_params = "b_arm2")
#'
#' result <- optimize_power_effect(
#'   design = design,
#'   power_metric = "pwr_eff",
#'   effect_range = c(0.1, 0.8),
#'   n_total = 200,
#'   constant = list(
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
#' }
optimize_power_effect <- function(design,
                                  power_metric = "pwr_eff",
                                  effect_range = c(0.1, 0.8),
                                  n_total,
                                  constant = list(),
                                  search = list(),
                                  n_sims = 1000,
                                  evals_per_step = 10,
                                  max_evals = 50,
                                  n_cores = 1,
                                  knee_method = "utopia",
                                  surrogate = "gp",
                                  bf_args = list(),
                                  brms_args = list(),
                                  verbosity = 1) {
  # Validate n_total
  if (missing(n_total)) {
    cli::cli_abort(c(
      "{.arg n_total} is required",
      "i" = "Specify the fixed sample size"
    ))
  }

  # Add n_total to constant
  constant$n_total <- n_total

  # Build search space
  full_search <- c(list(b_arm_treat = effect_range), search)

  # Build objectives (maximize power, minimize effect size = find MDE)
  objectives <- stats::setNames(
    list("maximize", "minimize"),
    c(power_metric, "b_arm_treat")
  )

  # Run optimization
  result <- pareto_optimize(
    design = design,
    objectives = objectives,
    search = full_search,
    constant = constant,
    n_sims = n_sims,
    evals_per_step = evals_per_step,
    max_evals = max_evals,
    n_cores = n_cores,
    knee_method = knee_method,
    surrogate = surrogate,
    bf_args = bf_args,
    brms_args = brms_args,
    verbosity = verbosity
  )

  # Update optimization type
  result@optimization_type <- "power_effect"

  result
}


#' Optimize Effect Size vs Sample Size
#'
#' Finds the Pareto front of effect size vs sample size for designs that
#' achieve a target power level. Useful for understanding the trade-off
#' between the effect you can detect and the sample size required.
#'
#' @param design An [build_design()] object specifying the trial design.
#' @param power_target Target power level to achieve (e.g., 0.80 for 80% power).
#' @param power_metric Power metric to constrain: `"pwr_eff"` (default),
#'   `"pwr_fut"`, or other power columns from results.
#' @param n_range Numeric vector `c(lower, upper)` for sample size search bounds.
#' @param effect_range Numeric vector `c(lower, upper)` for effect size search bounds.
#' @param constant Named list of other fixed parameters.
#' @param search Optional named list of additional search parameters.
#' @param n_sims Number of simulations per evaluation.
#' @param evals_per_step Evaluations per fidelity level.
#' @param max_evals Maximum evaluations (when `n_sims` is scalar).
#' @param n_cores Number of parallel cores.
#' @param knee_method Knee selection method: `"utopia"`, `"min_cost"`, `"linear"`.
#' @param surrogate Surrogate model: `"gp"` or `"rf"`.
#' @param bf_args BayesFlow-specific arguments.
#' @param brms_args brms-specific arguments.
#' @param verbosity Output level (0, 1, 2).
#'
#' @return An `rctbp_pareto_result` object with `optimization_type = "effect_n"`.
#'
#' @details
#' This function finds (effect_size, n_total) combinations that achieve the
#' target power. The Pareto front represents the trade-off: smaller effects
#' require larger samples. Both objectives are minimized (smaller effect to
#' detect = better, smaller sample = cheaper).
#'
#' Designs that do not meet the power constraint are excluded from the Pareto front.
#'
#' @seealso [pareto_optimize()], [optimize_power_n()], [optimize_power_effect()]
#'
#' @export
#' @examples
#' \dontrun{
#' design <- build_design(predefined_model = "ancova_cont_2arms", target_params = "b_arm2")
#'
#' result <- optimize_effect_n(
#'   design = design,
#'   power_target = 0.80,
#'   power_metric = "pwr_eff",
#'   n_range = c(50, 500),
#'   effect_range = c(0.1, 0.8),
#'   constant = list(
#'     thr_dec_eff = 0.975, thr_dec_fut = 0.5,
#'     thr_fx_eff = 0.2, thr_fx_fut = 0,
#'     p_alloc = list(c(0.5, 0.5)),
#'     intercept = 0, b_covariate = 0.3, sigma = 1
#'   ),
#'   n_sims = 500,
#'   max_evals = 50,
#'   n_cores = 4
#' )
#'
#' plot(result)
#' }
optimize_effect_n <- function(design,
                              power_target = 0.80,
                              power_metric = "pwr_eff",
                              n_range = c(50, 500),
                              effect_range = c(0.1, 0.8),
                              constant = list(),
                              search = list(),
                              n_sims = 1000,
                              evals_per_step = 10,
                              max_evals = 50,
                              n_cores = 1,
                              knee_method = "utopia",
                              surrogate = "gp",
                              bf_args = list(),
                              brms_args = list(),
                              verbosity = 1) {
  # Build search space
  full_search <- c(
    list(n_total = n_range, b_arm_treat = effect_range),
    search
  )

  # Build objectives (minimize both)
  objectives <- list(
    b_arm_treat = "minimize",
    n_total = "minimize"
  )

  # Constraint: power >= target
  constraint <- list(
    metric = power_metric,
    threshold = power_target
  )

  # Run optimization
  result <- pareto_optimize(
    design = design,
    objectives = objectives,
    search = full_search,
    constant = constant,
    constraint = constraint,
    n_sims = n_sims,
    evals_per_step = evals_per_step,
    max_evals = max_evals,
    n_cores = n_cores,
    knee_method = knee_method,
    surrogate = surrogate,
    bf_args = bf_args,
    brms_args = brms_args,
    verbosity = verbosity
  )

  # Update optimization type
  result@optimization_type <- "effect_n"

  result
}
