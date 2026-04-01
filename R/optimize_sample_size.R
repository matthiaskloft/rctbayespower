# =============================================================================
# OPTIMIZE SAMPLE SIZE: Unified Entry Point
# =============================================================================
# Single discoverable API for sample size optimization.
# Dispatches to single-objective BO or Pareto optimization based on
# the `objective` parameter.

#' Optimize Sample Size for Trial Design
#'
#' Unified entry point for sample size optimization. Finds either the minimum
#' sample size achieving a target power (single-objective) or the Pareto front
#' of power vs sample size (multi-objective).
#'
#' @param design An [build_design()] object specifying the trial design.
#' @param objective Optimization objective: `"single"` (find minimum n for
#'   target power) or `"pareto"` (find power-vs-n trade-off curve).
#' @param n_range Numeric vector `c(lower, upper)` for sample size search bounds.
#' @param constant Named list of fixed parameters. Must include all parameters
#'   required by the model except `n_total`. For example,
#'   `list(b_arm_treat = 0.3, thr_dec_eff = 0.975, ...)`.
#' @param n_sims Number of simulations per evaluation.
#' @param n_cores Number of parallel cores for power analysis.
#'
#' @param target_power Target power level (single-objective only, default 0.80).
#' @param surrogate Surrogate model strategy (single-objective only):
#'   - `"gp_power"` (default): Custom GP loop fitting logit(power). Theoretically
#'     strongest — exploits smooth monotonic structure.
#'   - `"gp_score"`: Standard bbotk EGO on feasibility score. Simplest pipeline.
#'   - `"rf"`: Random forest on feasibility score. Handles discontinuities natively.
#' @param score_shape Score decay shape (single-objective only): `"linear"` (default),
#'   `"quadratic"`, or `"root"`.
#' @param score_scale Score normalization scale (single-objective only):
#'   `"log"` (default, proportional cost) or `"raw"` (absolute cost).
#' @param n_init Number of initial design points (default 5).
#' @param max_evals Maximum total evaluations including initial design points (default 30).
#' @param patience Early stopping patience — stop if best score hasn't improved
#'   for this many consecutive evaluations after the initial design (default 5).
#'
#' @param power_metric Power metric for Pareto objective (Pareto only, default `"pwr_eff"`).
#' @param knee_method Knee point selection method (Pareto only):
#'   `"utopia"` (default), `"min_cost"`, or `"linear"`.
#'
#' @param seed Random seed for reproducibility.
#' @param verbose Whether to show progress messages (default TRUE).
#' @param bf_args Named list of BayesFlow-specific arguments.
#' @param brms_args Named list of brms-specific arguments.
#'
#' @return
#' - `objective = "single"`: An `rctbp_sample_size_result` object with `n_optimal`,
#'   `power_optimal`, `feasible` status, `archive`, and `convergence` trace.
#' - `objective = "pareto"`: An `rctbp_pareto_result` object with `pareto_front`,
#'   `selected_design`, and `archive`.
#'
#' @details
#' **Single-objective mode** uses Bayesian optimization with a feasibility score
#' function. The score is 0 when power < target and monotonically decreasing
#' from n_min to n_max when power >= target, pushing the optimizer toward the
#' smallest feasible sample size.
#'
#' **Pareto mode** searches only over `n_total`. For multi-dimensional Pareto
#' search (e.g., n_total + p_alloc), use [pareto_optimize()] directly.
#'
#' @seealso [pareto_optimize()], [build_design()], [power_analysis()]
#'
#' @export
#' @examples
#' \dontrun{
#' design <- build_design(
#'   predefined_model = "ancova_cont_2arms",
#'   target_params = "b_arm2"
#' )
#'
#' # Single-objective: find minimum n for 80% power
#' result <- optimize_sample_size(
#'   design,
#'   objective = "single",
#'   n_range = c(50, 300),
#'   constant = list(
#'     b_arm_treat = 0.3,
#'     thr_dec_eff = 0.975, thr_dec_fut = 0.5,
#'     thr_fx_eff = 0.2, thr_fx_fut = 0,
#'     p_alloc = list(c(0.5, 0.5)),
#'     intercept = 0, b_covariate = 0.3, sigma = 1
#'   ),
#'   target_power = 0.80,
#'   n_sims = 500,
#'   max_evals = 20
#' )
#' result
#'
#' # Pareto: power vs sample size trade-off
#' result_pareto <- optimize_sample_size(
#'   design,
#'   objective = "pareto",
#'   n_range = c(50, 300),
#'   constant = list(
#'     b_arm_treat = 0.3,
#'     thr_dec_eff = 0.975, thr_dec_fut = 0.5,
#'     thr_fx_eff = 0.2, thr_fx_fut = 0,
#'     p_alloc = list(c(0.5, 0.5)),
#'     intercept = 0, b_covariate = 0.3, sigma = 1
#'   ),
#'   n_sims = 500,
#'   max_evals = 30
#' )
#' plot(result_pareto)
#' }
optimize_sample_size <- function(design,
                                  objective = c("single", "pareto"),
                                  n_range = c(50, 500),
                                  constant = list(),
                                  n_sims = 1000,
                                  n_cores = 1,
                                  # Single-objective args
                                  target_power = 0.80,
                                  surrogate = c("gp_power", "gp_score", "rf"),
                                  score_shape = c("linear", "quadratic", "root"),
                                  score_scale = c("log", "raw"),
                                  n_init = 5,
                                  max_evals = 30,
                                  patience = 5,
                                  # Pareto args
                                  power_metric = "pwr_eff",
                                  knee_method = c("utopia", "min_cost", "linear"),
                                  # Common
                                  seed = NULL,
                                  verbose = TRUE,
                                  bf_args = list(),
                                  brms_args = list()) {

  # ==========================================================================
  # INPUT VALIDATION
  # ==========================================================================
  objective <- match.arg(objective)

  if (!inherits(design, "rctbp_design") &&
      !inherits(design, "rctbayespower::rctbp_design")) {
    cli::cli_abort(c(
      "{.arg design} must be an rctbp_design object",
      "x" = "Got: {.cls {class(design)}}"
    ))
  }

  if (!is.numeric(n_range) || length(n_range) != 2) {
    cli::cli_abort(c(
      "{.arg n_range} must be a numeric vector of length 2",
      "x" = "Got: {.val {n_range}}"
    ))
  }

  if (n_range[1] >= n_range[2]) {
    cli::cli_abort(c(
      "{.arg n_range} lower bound must be less than upper bound",
      "x" = "Got: [{.val {n_range[1]}}, {.val {n_range[2]}}]"
    ))
  }

  if (n_range[1] < 1) {
    cli::cli_abort(c(
      "{.arg n_range} lower bound must be >= 1",
      "x" = "Got: {n_range[1]}"
    ))
  }

  if (!is.list(constant)) {
    cli::cli_abort(c(
      "{.arg constant} must be a named list",
      "x" = "Got: {.cls {class(constant)}}"
    ))
  }

  if (length(constant) > 0 && is.null(names(constant))) {
    cli::cli_abort(c(
      "{.arg constant} must be a named list",
      "x" = "Got an unnamed list"
    ))
  }

  # ==========================================================================
  # DISPATCH
  # ==========================================================================
  if (objective == "single") {
    surrogate <- match.arg(surrogate)
    score_shape <- match.arg(score_shape)
    score_scale <- match.arg(score_scale)

    if (!is.numeric(target_power) || target_power <= 0 || target_power >= 1) {
      cli::cli_abort(c(
        "{.arg target_power} must be a probability in (0, 1)",
        "x" = "Got: {.val {target_power}}"
      ))
    }

    if (verbose) {
      cli::cli_h3("Sample Size Optimization")
      cli::cli_dl(c(
        "Objective" = "Find minimum n for power >= {target_power}",
        "Search range" = "[{n_range[1]}, {n_range[2]}]",
        "Surrogate" = surrogate,
        "Max evaluations" = max_evals,
        "Backend" = design@backend
      ))
      cli::cli_text("")
    }

    run_single_bo(
      design = design, target_power = target_power, n_range = n_range,
      constant = constant, n_sims = n_sims, n_cores = n_cores,
      surrogate = surrogate, score_shape = score_shape,
      score_scale = score_scale, n_init = n_init, max_evals = max_evals,
      patience = patience, seed = seed, verbose = verbose,
      bf_args = bf_args, brms_args = brms_args
    )

  } else {
    knee_method <- match.arg(knee_method)

    if (!is.null(seed)) {
      cli::cli_warn(
        "{.arg seed} is not supported for Pareto mode and will be ignored."
      )
    }

    # Pareto mode: construct objectives and search, delegate to pareto_optimize()
    objectives <- stats::setNames(
      list("maximize", "minimize"),
      c(power_metric, "n_total")
    )

    search <- list(n_total = n_range)

    pareto_optimize(
      design = design,
      objectives = objectives,
      search = search,
      constant = constant,
      n_sims = n_sims,
      max_evals = max_evals,
      n_cores = n_cores,
      knee_method = knee_method,
      init_design_size = n_init,
      bf_args = bf_args,
      brms_args = brms_args,
      verbosity = if (verbose) 1 else 0
    )
  }
}
