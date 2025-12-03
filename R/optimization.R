# =============================================================================
# OPTIMIZATION FUNCTIONS
# =============================================================================
# User-facing functions for Bayesian optimization of trial designs:
# - search_p_alloc(): Specify allocation probability search with constraints
# - search_looks(): Specify interim look timing search with constraints
# - build_objectives(): Create optimization problem specification
# - optimization(): Execute optimization

# =============================================================================
# SIMPLEX SEARCH HELPERS
# =============================================================================
# Helper functions for specifying simplex-constrained search parameters.
# These create special objects that build_objectives() recognizes and handles
# with appropriate transforms (ILR for k>2 dimensions, direct for k=2).

#' Specify Allocation Probability Search
#'
#' Creates a search specification for optimizing treatment allocation
#' probabilities. Instead of specifying bounds for each arm, you specify

#' the minimum acceptable proportion for ANY group.
#'
#' @param min Minimum proportion acceptable for any group (default 0.1).
#'   Must be in (0, 0.5). For k arms, requires `k * min < 1`.
#'
#' @return An object of class "rctbp_search_p_alloc" for use in
#'   [build_objectives()] `search` argument.
#'
#' @details
#' The allocation probabilities form a simplex (must sum to 1, all non-negative).
#' This function specifies a constrained region where no group gets less than
#' `min` proportion.
#'
#' **Internal handling:**
#' \itemize{
#'   \item 2-arm: Direct sampling of treatment proportion in `[min, 1-min]`
#'   \item k-arm (k>2): ILR transform on (k-1) dimensions, scaled to constraint
#' }
#'
#' **Output convention:**
#' The resulting `p_alloc` vector has k entries: `[p_ctrl, p_treat1, p_treat2, ...]`
#' where the first entry is the control arm probability.
#'
#' @seealso [build_objectives()], [search_looks()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Optimize allocation with at least 20% per group
#' obj <- build_objectives(
#'   design = design,
#'   search = list(
#'     n_total = c(50, 500),
#'     p_alloc = search_p_alloc(min = 0.2)
#'   ),
#'   objectives = list(pwr_eff = target(0.80)),
#'   constant = list(...)
#' )
#' }
search_p_alloc <- function(min = 0.1) {
  # Validate min

if (min <= 0) {
    cli::cli_abort(c(
      "{.arg min} must be positive",
      "x" = "Got {.val {min}}"
    ))
  }
  if (min >= 0.5) {
    cli::cli_abort(c(
      "{.arg min} must be less than 0.5",
      "x" = "Got {.val {min}}",
      "i" = "For 2 arms, min=0.5 would force equal allocation (no search needed)"
    ))
  }

  structure(
    list(min_prop = min),
    class = "rctbp_search_p_alloc"
  )
}

#' @export
print.rctbp_search_p_alloc <- function(x, ...) {
  cat(sprintf("search_p_alloc(min = %g)\n", x$min_prop))
  invisible(x)
}


#' Specify Interim Look Timing Search
#'
#' Creates a search specification for optimizing the timing of interim
#' analyses. Instead of specifying bounds for each look, you specify the
#' number of looks and minimum spacing between consecutive looks.
#'
#' @param n Number of looks including final analysis (default 2).
#'   Must be >= 2 (at least one interim + final).
#' @param min_spacing Minimum proportion of total sample size between
#'   consecutive looks (default 0.2). Requires `n * min_spacing <= 1`.
#'
#' @return An object of class "rctbp_search_looks" for use in
#'   [build_objectives()] `search` argument.
#'
#' @details
#' The look timings must be ordered (0 < t1 < t2 < ... < 1) and the gaps
#' between consecutive looks (increments) form a simplex.
#'
#' **Internal handling:**
#' \itemize{
#'   \item n=2: Direct sampling of interim proportion in `[min_spacing, 1-min_spacing]`
#'   \item n>2: ILR transform on increments, scaled to satisfy spacing constraint
#' }
#'
#' **Output convention:**
#' The resulting `analysis_at` vector has n-1 proportions. The final look at 1.0
#' is automatically appended by [build_conditions()].
#'
#' @seealso [build_objectives()], [search_p_alloc()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Optimize timing of 3 looks with at least 20% spacing
#' obj <- build_objectives(
#'   design = design,
#'   search = list(
#'     n_total = c(100, 400),
#'     analysis_at = search_looks(n = 3, min_spacing = 0.2)
#'   ),
#'   objectives = list(pwr_eff = target(0.80)),
#'   constant = list(...)
#' )
#' }
search_looks <- function(n = 2, min_spacing = 0.2) {
  # Validate n
  if (!is.numeric(n) || length(n) != 1 || n < 2 || n != floor(n)) {
    cli::cli_abort(c(
      "{.arg n} must be an integer >= 2",
      "x" = "Got {.val {n}}",
      "i" = "n=2 means one interim + final analysis"
    ))
  }

  # Validate min_spacing
  if (min_spacing <= 0) {
    cli::cli_abort(c(
      "{.arg min_spacing} must be positive",
      "x" = "Got {.val {min_spacing}}"
    ))
  }
  if (n * min_spacing > 1) {
    cli::cli_abort(c(
      "{.arg n} * {.arg min_spacing} must be <= 1",
      "x" = "Got {n} * {min_spacing} = {n * min_spacing}",
      "i" = "Reduce number of looks or minimum spacing"
    ))
  }

  structure(
    list(n_looks = as.integer(n), min_spacing = min_spacing),
    class = "rctbp_search_looks"
  )
}

#' @export
print.rctbp_search_looks <- function(x, ...) {
  cat(sprintf("search_looks(n = %d, min_spacing = %g)\n", x$n_looks, x$min_spacing))
  invisible(x)
}


# =============================================================================
# CONSTRUCTOR: build_objectives()
# =============================================================================

#' Build Optimization Objectives
#'
#' Creates an optimization problem specification for finding optimal trial
#' designs. This parallels [build_conditions()] but for continuous optimization
#' rather than grid evaluation.
#'
#' @param design An rctbp_design object from [build_design()]
#' @param search Named list of parameter bounds for optimization.
#'   Each parameter must have a 2-element numeric vector `c(lower, upper)`.
#'   Example: `list(n_total = c(50, 500))`
#' @param objectives Named list specifying what to optimize. Values can be:
#'   \itemize{
#'     \item `"maximize"` or `"max"`: Maximize this objective
#'     \item `"minimize"` or `"min"`: Minimize this objective
#'     \item `target(value)`: Find designs achieving this target
#'   }
#'   Valid objective names: `pwr_eff`, `pwr_fut`, `n_total`, `expected_n`
#' @param constant Named list of fixed parameters for all evaluations.
#'   Must include all parameters required by [build_conditions()] that are
#'   not in `search`.
#' @param secondary Named list specifying secondary objectives for target
#'   optimization. These parameters are minimized/maximized after the primary
#'   target is achieved. If NULL (default), auto-infers from search parameters:
#'   `n_total`, `thr_dec_eff`, `thr_dec_fut`, `thr_fx_eff`, `thr_fx_fut`
#'   default to "minimize". Override with explicit list, e.g.,
#'   `list(n_total = "minimize")` to only consider n_total.
#' @param cost_fn Optional user-defined cost function for custom objectives.
#'   Must accept a data frame row and return a numeric scalar.
#'
#' @return An S7 object of class "rctbp_objectives"
#'
#' @details
#' The optimization problem is specified by:
#' \itemize{
#'   \item \strong{search}: Continuous parameter ranges to explore
#'   \item \strong{objectives}: Goals to optimize (power, sample size, etc.)
#'   \item \strong{constant}: Fixed design parameters
#' }
#'
#' \strong{Objective Types:}
#' \itemize{
#'   \item Single maximize/minimize: Standard Bayesian optimization

#'   \item Target: Finds minimum n_total (or search param) achieving target
#'   \item Multiple objectives: Returns Pareto frontier
#' }
#'
#' @seealso [optimization()], [target()], [power_analysis()]
#'
#' @export
#' @examples
#' \dontrun{
#' design <- build_design(
#'   model_name = "ancova_cont_2arms",
#'   target_params = "b_arm2"
#' )
#'
#' # Find minimum sample size for 80% power
#' obj <- build_objectives(
#'   design = design,
#'   search = list(n_total = c(50, 500)),
#'   objectives = list(pwr_eff = target(0.80)),
#'   constant = list(
#'     b_arm_treat = 0.3,
#'     thr_dec_eff = 0.975, thr_dec_fut = 0.5,
#'     thr_fx_eff = 0.2, thr_fx_fut = 0,
#'     p_alloc = list(c(0.5, 0.5)),
#'     intercept = 0, b_covariate = 0.3, sigma = 1
#'   )
#' )
#'
#' # Multi-objective: power vs sample size tradeoff
#' obj <- build_objectives(
#'   design = design,
#'   search = list(n_total = c(50, 500), b_arm_treat = c(0.2, 0.6)),
#'   objectives = list(pwr_eff = "maximize", n_total = "minimize"),
#'   constant = list(...)
#' )
#' }
build_objectives <- function(design,
                             search,
                             objectives,
                             constant = list(),
                             secondary = NULL,
                             cost_fn = NULL) {
  # ===========================================================================
  # INPUT VALIDATION
  # ===========================================================================

  # Validate design
  if (!inherits(design, "rctbp_design") &&
      !inherits(design, "rctbayespower::rctbp_design")) {
    cli::cli_abort(c(
      "{.arg design} must be a valid rctbp_design object",
      "x" = "Got object of class {.cls {class(design)}}",
      "i" = "Use {.fn build_design} to create a valid design object"
    ))
  }

  # Validate search
  if (!is.list(search) || length(search) == 0) {
    cli::cli_abort(c(
      "{.arg search} must be a non-empty named list",
      "i" = "Example: {.code list(n_total = c(50, 500))}"
    ))
  }
  if (is.null(names(search)) || any(names(search) == "")) {
    cli::cli_abort(c(
      "All elements in {.arg search} must be named",
      "i" = "Example: {.code list(n_total = c(50, 500))}"
    ))
  }

  # ===========================================================================
  # PROCESS SIMPLEX SEARCH SPECS
  # ===========================================================================
  # Detect and validate simplex search specs (search_p_alloc, search_looks)
  # Store parsed specs and validate constraints against design

  search_specs <- list()
  n_arms <- design@n_arms

  for (param_name in names(search)) {
    spec <- search[[param_name]]

    # Handle search_p_alloc spec
    if (inherits(spec, "rctbp_search_p_alloc")) {
      # Validate constraint against number of arms
      if (n_arms * spec$min_prop >= 1) {
        cli::cli_abort(c(
          "Invalid {.fn search_p_alloc} constraint for {n_arms}-arm design",
          "x" = "{.arg min} = {spec$min_prop} but {n_arms} * {spec$min_prop} = {n_arms * spec$min_prop} >= 1",
          "i" = "For {n_arms} arms, {.arg min} must be < {round(1/n_arms, 3)}"
        ))
      }

      search_specs[[param_name]] <- list(
        type = "p_alloc",
        min_prop = spec$min_prop,
        n_arms = n_arms,
        n_dims = if (n_arms == 2) 1L else as.integer(n_arms - 1)
      )
      next
    }

    # Handle search_looks spec
    if (inherits(spec, "rctbp_search_looks")) {
      search_specs[[param_name]] <- list(
        type = "looks",
        n_looks = spec$n_looks,
        min_spacing = spec$min_spacing,
        n_dims = if (spec$n_looks == 2) 1L else as.integer(spec$n_looks - 1)
      )
      next
    }

    # Standard bounds validation
    if (!is.numeric(spec) || length(spec) != 2) {
      cli::cli_abort(c(
        "{.arg search${param_name}} must be a numeric vector of length 2 or a simplex search spec",
        "x" = "Got {.type {spec}}",
        "i" = "Use {.code c(lower, upper)} for bounds, or {.fn search_p_alloc}/{.fn search_looks} for simplex"
      ))
    }
    if (spec[1] >= spec[2]) {
      cli::cli_abort(c(
        "{.arg search${param_name}} lower bound must be less than upper bound",
        "x" = "Got [{spec[1]}, {spec[2]}]"
      ))
    }
  }

  # Validate objectives
  if (!is.list(objectives) || length(objectives) == 0) {
    cli::cli_abort(c(
      "{.arg objectives} must be a non-empty named list",
      "i" = "Example: {.code list(pwr_eff = target(0.80))}"
    ))
  }
  if (is.null(names(objectives)) || any(names(objectives) == "")) {
    cli::cli_abort(c(
      "All elements in {.arg objectives} must be named",
      "i" = "Valid names: pwr_eff, pwr_fut, n_total, expected_n"
    ))
  }

  # Validate each objective
  valid_obj_types <- c("maximize", "minimize", "max", "min")
  for (obj_name in names(objectives)) {
    obj_val <- objectives[[obj_name]]
    if (!inherits(obj_val, "rctbp_target") &&
        !(is.character(obj_val) && obj_val %in% valid_obj_types)) {
      cli::cli_abort(c(
        "{.arg objectives${obj_name}} must be 'maximize', 'minimize', or target()",
        "x" = "Got {.type {obj_val}}: {.val {obj_val}}"
      ))
    }
  }

  # Validate constant
  if (!is.list(constant)) {
    cli::cli_abort(c(
      "{.arg constant} must be a list",
      "x" = "Got {.type {constant}}"
    ))
  }

  # Check for overlapping parameters
  overlap <- intersect(names(search), names(constant))
  if (length(overlap) > 0) {
    cli::cli_abort(c(
      "Parameters cannot appear in both {.arg search} and {.arg constant}",
      "x" = "Overlapping parameters: {.val {overlap}}"
    ))
  }

  # ===========================================================================
  # VALIDATE REQUIRED PARAMETERS
  # ===========================================================================
  # Get required parameters from design
  params_needed <- show_condition_args(design, print = FALSE)
  params_given <- c(names(search), names(constant))

  # Parameters with defaults (same as in build_conditions)
  params_with_defaults <- c("analysis_at", "interim_function", "adaptive")
  params_required <- setdiff(params_needed$params_all, params_with_defaults)

  missing_params <- setdiff(params_required, params_given)
  if (length(missing_params) > 0) {
    cli::cli_abort(c(
      "Missing required parameters",
      "x" = "The following parameters must be specified: {.val {missing_params}}",
      "i" = "Add these to {.arg search} or {.arg constant}"
    ))
  }

  # ===========================================================================
  # PARSE OBJECTIVE INFORMATION
  # ===========================================================================
  objective_info <- parse_objectives(objectives)

  # ===========================================================================
  # SECONDARY OBJECTIVES: Auto-infer or validate user-provided
  # ===========================================================================
  # Default: minimize n_total and threshold parameters when present in search
  minimize_defaults <- c("n_total", "thr_dec_eff", "thr_dec_fut",
                         "thr_fx_eff", "thr_fx_fut")

  if (is.null(secondary)) {
    # Auto-infer from search params
    secondary <- list()
    for (param in intersect(names(search), minimize_defaults)) {
      # Only include if it's a standard bounds spec (not simplex)
      if (is.numeric(search[[param]]) && length(search[[param]]) == 2) {
        secondary[[param]] <- "minimize"
      }
    }
  } else {
    # Validate user-provided secondary
    for (param in names(secondary)) {
      if (!param %in% names(search)) {
        cli::cli_warn(c(
          "Secondary param {.val {param}} not in search, ignored",
          "i" = "Secondary objectives must be search parameters"
        ))
      }
      if (!secondary[[param]] %in% c("minimize", "maximize")) {
        cli::cli_abort(c(
          "Secondary direction must be 'minimize' or 'maximize'",
          "x" = "Got {.val {secondary[[param]]}} for {.val {param}}"
        ))
      }
    }
    # Filter to only search params with standard bounds
    secondary <- secondary[names(secondary) %in% names(search)]
    secondary <- secondary[vapply(names(secondary), function(p) {
      is.numeric(search[[p]]) && length(search[[p]]) == 2
    }, logical(1))]
  }

  # ===========================================================================
  # CREATE S7 OBJECT
  # ===========================================================================
  rctbp_objectives(
    design = design,
    search = search,
    search_specs = search_specs,
    objectives = objectives,
    constant = constant,
    secondary = secondary,
    cost_fn = cost_fn,
    objective_info = objective_info
  )
}

# =============================================================================
# INTERNAL: Parse Objectives
# =============================================================================
# Analyzes objectives specification and determines optimization type

#' Parse Objectives Specification
#'
#' @param objectives Named list of objectives
#' @return List with parsed information
#' @keywords internal
parse_objectives <- function(objectives) {
  n_objectives <- length(objectives)
  has_target <- any(vapply(objectives, inherits, logical(1), "rctbp_target"))

  # Determine optimization type
  opt_type <- if (has_target) {
    "target"
  } else if (n_objectives > 1) {
    "multi"
  } else {
    "single"
  }

  # Parse each objective
  parsed <- lapply(names(objectives), function(obj_name) {
    obj_val <- objectives[[obj_name]]
    if (inherits(obj_val, "rctbp_target")) {
      list(
        name = obj_name,
        type = "target",
        target_value = obj_val$value,
        direction = obj_val$direction
      )
    } else {
      # Normalize direction
      dir <- if (obj_val %in% c("maximize", "max")) "maximize" else "minimize"
      list(
        name = obj_name,
        type = dir,
        target_value = NULL,
        direction = NULL
      )
    }
  })
  names(parsed) <- names(objectives)

  list(
    optimization_type = opt_type,
    n_objectives = n_objectives,
    has_target = has_target,
    objectives = parsed
  )
}

# =============================================================================
# MAIN FUNCTION: optimization()
# =============================================================================

#' Execute Bayesian Optimization
#'
#' Runs Bayesian optimization to find optimal trial designs based on the
#' objectives specification. Uses mlr3mbo for the optimization backend.
#'
#' @param objectives An rctbp_objectives object from [build_objectives()]
#' @param n_sims Number of simulations per evaluation. Can be:
#'   - **Scalar** (default 200): Constant fidelity, all evaluations use same n_sims
#'   - **Vector** (e.g., `c(100, 200, 500, 1000)`): Progressive fidelity where
#'     n_sims increases in discrete steps. Control timing with `evals_per_step`.
#'     Early cheap exploration, late precise refinement.
#' @param evals_per_step Controls evaluations per fidelity level when using
#'   progressive fidelity (vector `n_sims`). Ignored when `n_sims` is scalar. Can be:
#'   - **Scalar** (default 10): Same number of evaluations at each level.
#'     Total evals = `length(n_sims) * evals_per_step`.
#'   - **Vector**: Custom evaluations per level. Must have same length as `n_sims`.
#'     Total evals = `sum(evals_per_step)`.
#'     Example: `evals_per_step = c(15, 10, 10, 15)` with `n_sims = c(100, 200, 500, 1000)`
#'     means 15 evals at 100, 10 at 200, 10 at 500, 15 at 1000 (50 total).
#' @param use_warmup Logical (default TRUE). When TRUE and using progressive
#'   fidelity (`length(n_sims) > 1`), the first fidelity level is treated as a
#'   warmup phase to estimate reference values for secondary objectives. During
#'   warmup, inverse bonus (`min/value`) is used. After warmup, reference-based
#'   bonus (`1 - value/reference`) is used. Warmup archive points are recomputed
#'   with the new bonus and seed the main phase GP. Requires secondary objectives
#'   (auto-inferred or explicit via [build_objectives()]).
#' @param warmup_n_sims Deprecated. Use vector `n_sims` instead for multi-fidelity.
#' @param warmup_evals Deprecated. Use vector `n_sims` with `evals_per_step` instead.
#' @param warmup_margin Deprecated. No longer used.
#' @param max_evals Maximum TOTAL number of objective function evaluations (default 50),
#'   including initial design points. **Ignored when `n_sims` is a vector** (total
#'   evals derived from `length(n_sims) * evals_per_step`).
#' @param n_cores Number of CPU cores for parallel power analysis (default 1).
#' @param surrogate Surrogate model type: "rf" (Random Forest, default) or "gp" (Gaussian Process).
#' @param acq_function Acquisition function: "auto" (default), "ei" (Expected Improvement),
#'   "cb" (Confidence Bound), or "pi" (Probability of Improvement).
#' @param init_design Initial design method: "lhs" (Latin Hypercube, default),
#'   "random", or "sobol". Ignored if warmup provides data.
#' @param init_design_size Number of initial evaluations. If NULL, uses 4*d where
#'   d is the number of search parameters. When warmup is enabled, this controls
#'   how many of the best warmup points are selected for re-evaluation with full n_sims.
#' @param patience Number of evaluations without improvement before early stopping
#'   (default 30). Only applies to target optimization. Once target is achieved
#'   in the optimal window, ALL subsequent evaluations count toward patience.
#'   The counter resets only when finding smaller parameters that still achieve
#'   target in the optimal window. Set to NULL to disable.
#' @param min_delta Defines the optimal window as `[target, target + min_delta)`
#'   (default 0.001). Early stopping is triggered when the target is first
#'   achieved within this window and `patience` evaluations pass without finding
#'   smaller parameters that also achieve target in the window.
#' @param optimum Optimum selection method for final result: "surrogate" (default)
#'   uses surrogate model search in constrained domain; "empirical" uses
#'   confidence interval-based selection from archive points.
#' @param sims_final_run Number of simulations for final confirmation run
#'   (default 1000). Higher values provide more precise final estimates but
#'   increase computation time.
#' @param trim_param_space Trimming proportion for surrogate search domain
#'   (default 0.25). Constrains search to trimmed quantile range of archive
#'   parameters. Use 0.25 for IQR, 0 for no constraint, 0.5 for maximum constraint.
#' @param bf_args Named list of BayesFlow-specific arguments passed to
#'   [power_analysis()]. Options: `n_posterior_samples`, `batch_size`.
#'   Note: Set Python environment before optimization via [bf_status()].
#' @param brms_args Named list of brms-specific arguments passed to
#'   [power_analysis()]. See [brms::brm()] for available options.
#' @param refresh How often to print progress updates (default 5).
#'   Set to 0 to disable progress output, or a positive integer to print
#'   every N evaluations. A CLI progress bar is shown when verbosity >= 1.
#' @param run Logical, whether to run immediately (default TRUE).
#'   Set to FALSE to return configuration without executing.
#' @param verbosity Output level: 0 (quiet), 1 (normal), 2 (verbose).
#'
#' @return An S7 object of class "rctbp_optimization_result"
#'
#' @details
#' \strong{Optimization Types (Auto-detected):}
#' \itemize{
#'   \item Single-objective: Uses EGO (Efficient Global Optimization)
#'   \item Target-constrained: Finds minimum search parameter achieving target
#'   \item Multi-objective: Uses ParEGO for Pareto frontier
#' }
#'
#' \strong{Surrogate Models:}
#' \itemize{
#'   \item GP (Gaussian Process): Better for smooth functions, handles noise
#'   \item RF (Random Forest): Better for non-smooth, handles categorical
#' }
#'
#' @seealso [build_objectives()], [target()], [power_analysis()]
#'
#' @export
#' @examples
#' \dontrun{
#' design <- build_design(
#'   model_name = "ancova_cont_2arms",
#'   target_params = "b_arm2"
#' )
#'
#' obj <- build_objectives(
#'   design = design,
#'   search = list(n_total = c(50, 500)),
#'   objectives = list(pwr_eff = target(0.80)),
#'   constant = list(
#'     b_arm_treat = 0.3,
#'     thr_dec_eff = 0.975, thr_dec_fut = 0.5,
#'     thr_fx_eff = 0.2, thr_fx_fut = 0,
#'     p_alloc = list(c(0.5, 0.5)),
#'     intercept = 0, b_covariate = 0.3, sigma = 1
#'   )
#' )
#'
#' result <- optimization(obj, n_sims = 200, max_evals = 30)
#' print(result)
#' plot(result)
#' }
optimization <- function(objectives,
                         n_sims = 200,
                         evals_per_step = 10,
                         use_warmup = TRUE,
                         warmup_n_sims = NULL,
                         warmup_evals = NULL,
                         warmup_margin = NULL,
                         max_evals = 50,
                         n_cores = 1,
                         surrogate = c("rf", "gp"),
                         acq_function = "auto",
                         init_design = c("lhs", "random", "sobol"),
                         init_design_size = NULL,
                         patience = 30,
                         min_delta = 0.001,
                         optimum = c("surrogate", "empirical"),
                         sims_final_run = 1000,
                         trim_param_space = 0.25,
                         bf_args = list(),
                         brms_args = list(),
                         refresh = 5,
                         run = TRUE,
                         verbosity = 1) {

  # ===========================================================================

  # INPUT VALIDATION
  # ===========================================================================
  surrogate <- match.arg(surrogate)
  init_design <- match.arg(init_design)
  optimum <- match.arg(optimum)

  # Validate sims_final_run
  if (!is.numeric(sims_final_run) || sims_final_run < 1) {
    cli::cli_abort(c(
      "{.arg sims_final_run} must be a positive integer",
      "x" = "Got {.val {sims_final_run}}"
    ))
  }

  # Validate trim_param_space
  if (!is.numeric(trim_param_space) || length(trim_param_space) != 1 ||
      trim_param_space < 0 || trim_param_space > 0.5) {
    cli::cli_abort(c(
      "{.arg trim_param_space} must be a number between 0 and 0.5",
      "x" = "Got {.val {trim_param_space}}",
      "i" = "Use 0.25 (default) for IQR constraint, 0 for no constraint, 0.5 for maximum constraint"
    ))
  }

  if (!inherits(objectives, "rctbp_objectives") &&
      !inherits(objectives, "rctbayespower::rctbp_objectives")) {
    cli::cli_abort(c(
      "{.arg objectives} must be an rctbp_objectives object",
      "x" = "Got object of class {.cls {class(objectives)}}",
      "i" = "Use {.fn build_objectives} to create an objectives object"
    ))
  }

  # Validate n_sims (scalar or vector)
  if (!is.numeric(n_sims) || any(n_sims < 1)) {
    cli::cli_abort(c(
      "{.arg n_sims} must be positive integer(s)",
      "x" = "Got {.val {n_sims}}"
    ))
  }

  # Determine if using progressive fidelity
  is_progressive <- length(n_sims) > 1

  if (is_progressive) {
    # Vector n_sims: validate increasing order
    if (!all(diff(n_sims) > 0)) {
      cli::cli_abort(c(
        "{.arg n_sims} vector must be strictly increasing",
        "x" = "Got {.val {n_sims}}",
        "i" = "Example: {.code c(100, 200, 500, 1000)}"
      ))
    }

    # Validate evals_per_step (scalar or vector)
    if (length(evals_per_step) > 1) {
      # Vector evals_per_step: custom evals per level
      if (length(evals_per_step) != length(n_sims)) {
        cli::cli_abort(c(
          "{.arg evals_per_step} vector must have same length as {.arg n_sims}",
          "x" = "n_sims has {length(n_sims)} levels, evals_per_step has {length(evals_per_step)}",
          "i" = "Example: {.code n_sims = c(100, 200, 500), evals_per_step = c(15, 10, 10)}"
        ))
      }
      if (any(evals_per_step < 1)) {
        cli::cli_abort(c(
          "{.arg evals_per_step} values must be positive integers",
          "x" = "Got {.val {evals_per_step}}"
        ))
      }
    } else {
      # Scalar evals_per_step: uniform evals per level
      if (!is.numeric(evals_per_step) || evals_per_step < 1) {
        cli::cli_abort(c(
          "{.arg evals_per_step} must be a positive integer",
          "x" = "Got {.val {evals_per_step}}"
        ))
      }
    }
  }

  if (!is.numeric(max_evals) || max_evals < 1) {
    cli::cli_abort(c(
      "{.arg max_evals} must be a positive integer",
      "x" = "Got {.val {max_evals}}"
    ))
  }

  if (!is.null(patience) && (!is.numeric(patience) || patience < 1)) {
    cli::cli_abort(c(
      "{.arg patience} must be a positive integer or NULL",
      "x" = "Got {.val {patience}}"
    ))
  }

  if (!is.numeric(min_delta) || min_delta < 0) {
    cli::cli_abort(c(
      "{.arg min_delta} must be a non-negative number",
      "x" = "Got {.val {min_delta}}"
    ))
  }

  # Deprecation warnings for warmup parameters
  if (!is.null(warmup_n_sims)) {
    cli::cli_warn(c(
      "{.arg warmup_n_sims} is deprecated",
      "i" = "Use vector {.arg n_sims} for multi-fidelity optimization instead",
      "i" = "Example: {.code n_sims = c(100, 200, 500, 1000)}"
    ))
  }
  if (!is.null(warmup_evals)) {
    cli::cli_warn(c(
      "{.arg warmup_evals} is deprecated",
      "i" = "Use {.arg evals_per_step} with vector {.arg n_sims} instead"
    ))
  }
  if (!is.null(warmup_margin)) {
    cli::cli_warn(c(
      "{.arg warmup_margin} is deprecated and ignored"
    ))
  }

  # ===========================================================================
  # WARMUP PHASE CONFIGURATION
  # ===========================================================================
  # Warmup requires: (1) progressive fidelity, (2) secondary objectives
  has_secondary <- length(objectives@secondary) > 0
  can_warmup <- use_warmup && is_progressive && has_secondary

  if (use_warmup && !can_warmup) {
    if (!is_progressive) {
      cli::cli_alert_info(
        "Warmup disabled: requires progressive fidelity (vector {.arg n_sims})"
      )
    } else if (!has_secondary) {
      cli::cli_alert_info(
        "Warmup disabled: no secondary objectives detected"
      )
    }
  }

  if (can_warmup && verbosity >= 1) {
    secondary_str <- paste(
      paste0(names(objectives@secondary), " (", unlist(objectives@secondary), ")"),
      collapse = ", "
    )
    cli::cli_alert_info(
      "Warmup enabled: first {n_sims[1]}-sim phase will estimate reference for: {secondary_str}"
    )
  }

  # ===========================================================================
  # BUILD FIDELITY SCHEDULE
  # ===========================================================================
  # Compute effective max_evals and build n_sims schedule
  opt_type <- objectives@objective_info$optimization_type

  fidelity_schedule <- build_fidelity_schedule(
    n_sims = n_sims,
    max_evals = max_evals,
    evals_per_step = evals_per_step
  )

  # For progressive fidelity, max_evals is derived from schedule
  effective_max_evals <- nrow(fidelity_schedule)

  # ===========================================================================
  # CHECK DEPENDENCIES
  # ===========================================================================
  rlang::check_installed(
    c("mlr3mbo", "bbotk", "paradox"),
    reason = "for Bayesian optimization"
  )

  # ===========================================================================
  # CREATE RESULT OBJECT
  # ===========================================================================
  # For result, store max n_sims (final fidelity level)
  n_sims_max <- max(n_sims)

  result <- rctbp_optimization_result(
    objectives = objectives,
    n_sims = n_sims_max,
    n_evals = effective_max_evals,
    backend_used = objectives@design@backend,
    optimization_type = objectives@objective_info$optimization_type
  )

  # ===========================================================================
  # RUN OPTIMIZATION
  # ===========================================================================
  if (run) {
    result <- run_optimization(
      result = result,
      objectives = objectives,
      fidelity_schedule = fidelity_schedule,
      max_evals = effective_max_evals,
      use_warmup = can_warmup,
      patience = patience,
      min_delta = min_delta,
      optimum = optimum,
      sims_final_run = sims_final_run,
      trim_param_space = trim_param_space,
      n_cores = n_cores,
      surrogate = surrogate,
      acq_function = acq_function,
      init_design = init_design,
      init_design_size = init_design_size,
      bf_args = bf_args,
      brms_args = brms_args,
      refresh = refresh,
      verbosity = verbosity
    )
  }

  result
}

# =============================================================================
# HELPER: Show Optimization Arguments
# =============================================================================

#' Show Available Optimization Arguments
#'
#' Displays the available arguments and their defaults for [optimization()].
#'
#' @return Invisibly returns NULL
#' @export
#'
#' @examples
#' show_optimization_args()
show_optimization_args <- function() {

  cli::cli_h1("Optimization Arguments")

  cli::cli_h2("Searchable Parameters (for 'search' argument)")
  cli::cli_text("Any simulation or decision parameter can be searched:")
  cli::cli_bullets(c(
    " " = "{.strong Sample size:}",
    "*" = "n_total: Total sample size (integer bounds)",
    " " = "{.strong Effect sizes:}",
    "*" = "b_arm_treat, b_covariate, intercept, sigma, ...",
    " " = "{.strong Decision thresholds:}",
    "*" = "thr_dec_eff: Probability threshold for efficacy (e.g., c(0.90, 0.99))",
    "*" = "thr_dec_fut: Probability threshold for futility (e.g., c(0.30, 0.70))",
    "*" = "thr_fx_eff: Effect size threshold for efficacy (e.g., c(0, 0.3))",
    "*" = "thr_fx_fut: Effect size threshold for futility (e.g., c(-0.1, 0.1))",
    " " = "{.strong Allocation:}",
    "*" = "p_alloc: Allocation probabilities (use search_types for transform)"
  ))
  cli::cli_text("")

  cli::cli_h2("Core Parameters")
  cli::cli_bullets(c(
    "*" = "n_sims: Simulations per evaluation",
    " " = "  Scalar (default 200): constant fidelity",
    " " = "  Vector (e.g., c(100, 200, 500, 1000)): progressive fidelity",
    "*" = "evals_per_step: Evals per fidelity level (for vector n_sims)",
    " " = "  Scalar (default 10): uniform evals per level",
    " " = "  Vector: custom evals per level (same length as n_sims)",
    "*" = "max_evals: Maximum evaluations (default: 50, ignored if n_sims is vector)",
    "*" = "n_cores: Parallel cores (default: 1)",
    "*" = "refresh: Print progress every N evaluations (default: 5, 0 = quiet)"
  ))

  cli::cli_h2("Progressive Fidelity (Multi-Fidelity Optimization)")
  cli::cli_bullets(c(
    " " = "Use vector n_sims for cost-efficient optimization:",
    "*" = "Scalar evals_per_step: Equal evals per level",
    " " = "  n_sims = c(100, 200, 500, 1000), evals_per_step = 10 \u2192 40 total",
    "*" = "Vector evals_per_step: Custom evals per level",
    " " = "  n_sims = c(100, 200, 500, 1000), evals_per_step = c(15, 10, 10, 15)",
    " " = "  \u2192 15 at 100, 10 at 200, 10 at 500, 15 at 1000 (50 total)"
  ))

  cli::cli_h2("Early Stopping (target optimization only)")
  cli::cli_bullets(c(
    "*" = "patience: Iterations without improvement before stopping (default: 30)",
    "*" = "min_delta: Minimum power above target to consider achieved (default: 0.001)",
    " " = "  Stops when: actual_power >= target + min_delta AND n_total stagnates"
  ))

  cli::cli_h2("Surrogate Model")
  cli::cli_bullets(c(
    "*" = "surrogate: 'rf' (Random Forest, default) or 'gp' (Gaussian Process)",
    " " = "  rf: Robust, handles noise well, faster fitting",
    " " = "  gp: Better uncertainty quantification, smoother predictions"
  ))

  cli::cli_h2("Acquisition Function")
  cli::cli_bullets(c(
    "*" = "acq_function: 'auto', 'ei', 'cb', or 'pi'",
    " " = "  auto: EI for single/target, scalarized EI for multi",
    " " = "  ei: Expected Improvement",
    " " = "  cb: Confidence Bound (lower for minimize)",
    " " = "  pi: Probability of Improvement"
  ))

  cli::cli_h2("Initial Design")
  cli::cli_bullets(c(
    "*" = "init_design: 'lhs', 'random', or 'sobol'",
    "*" = "init_design_size: Number of initial points (default: 4*d)"
  ))

  cli::cli_h2("Backend Arguments")
  cli::cli_bullets(c(
    "*" = "bf_args: BayesFlow-specific arguments (list)",
    " " = "  n_posterior_samples: Posterior samples (default: 1000)",
    " " = "  batch_size: Sims per GPU batch (default: n_sims)",
    " " = "  (Reduce batch_size for limited GPU memory, e.g., 128 for 6GB)",
    " " = "  (Set Python env before running via bf_status())",
    "*" = "brms_args: brms-specific arguments (list)",
    " " = "  See brms::brm() for available options"
  ))

  invisible(NULL)
}

# =============================================================================
# PREDEFINED OBJECTIVE FUNCTIONS
# =============================================================================

#' Show Available Objective Functions
#'
#' Displays predefined objective function templates for common optimization
#' use cases with example configurations.
#'
#' @return Invisibly returns NULL
#' @export
#'
#' @examples
#' show_objective_fns()
show_objective_fns <- function() {
  cli::cli_h1("Optimization Objective Functions")

  cli::cli_h2("1. Minimum Sample Size for Target Power")
  cli::cli_text("Find the smallest n_total that achieves target power")
  cli::cli_code(
    'objectives = list(pwr_eff = target(0.80))',
    'search = list(n_total = c(50, 500))'
  )
  cli::cli_bullets(c(
    " " = "Maximizes power, prefers smaller n when target achieved",
    " " = "Best for: Sample size determination"
  ))
  cli::cli_text("")

  cli::cli_h2("2. Maximum Power (fixed sample size)")
  cli::cli_text("Find design parameters maximizing power")
  cli::cli_code(
    'objectives = list(pwr_eff = "maximize")',
    'search = list(b_arm_treat = c(0.1, 0.8))',
    'constant = list(n_total = 200, ...)'
  )
  cli::cli_bullets(c(
    " " = "Explores effect sizes or other parameters",
    " " = "Best for: Understanding power landscape"
  ))
  cli::cli_text("")

  cli::cli_h2("3. Multi-Objective: Power vs Sample Size")
  cli::cli_text("Find Pareto frontier of power-cost tradeoffs")
  cli::cli_code(
    'objectives = list(pwr_eff = "maximize", n_total = "minimize")',
    'search = list(n_total = c(50, 500), b_arm_treat = c(0.2, 0.6))'
  )
  cli::cli_bullets(c(
    " " = "Returns set of Pareto-optimal designs",
    " " = "Best for: Exploring tradeoffs"
  ))
  cli::cli_text("")

  cli::cli_h2("4. Optimize Decision Thresholds")
  cli::cli_text("Find optimal thresholds for target power")
  cli::cli_code(
    'objectives = list(pwr_eff = target(0.80))',
    'search = list(',
    '  thr_dec_eff = c(0.90, 0.99),  # Efficacy probability threshold',
    '  thr_fx_eff = c(0, 0.3)        # Efficacy effect size threshold',
    ')',
    'constant = list(n_total = 200, ...)'
  )
  cli::cli_bullets(c(
    " " = "Searches decision rule parameter space",
    " " = "Best for: Calibrating decision thresholds"
  ))
  cli::cli_text("")

  cli::cli_h2("5. Joint Sample Size and Threshold Optimization")
  cli::cli_text("Find n_total and thresholds achieving target power")
  cli::cli_code(
    'objectives = list(pwr_eff = target(0.80))',
    'search = list(',
    '  n_total = c(50, 300),',
    '  thr_dec_eff = c(0.90, 0.99)',
    ')'
  )
  cli::cli_bullets(c(
    " " = "Jointly optimizes sample size and decision rule",
    " " = "Best for: Flexible design optimization"
  ))
  cli::cli_text("")

  cli::cli_h2("Available Metrics")
  cli::cli_bullets(c(
    "*" = "pwr_eff: Power for efficacy (P(declare efficacy | H1))",
    "*" = "pwr_fut: Power for futility (P(stop for futility | H0))",
    "*" = "pr_eff: Probability of efficacy decision",
    "*" = "pr_fut: Probability of futility decision",
    "*" = "n_total: Total sample size"
  ))
  cli::cli_text("")

  cli::cli_h2("Objective Types")
  cli::cli_bullets(c(
    "*" = '"maximize" or "max": Maximize the metric',
    "*" = '"minimize" or "min": Minimize the metric',
    "*" = "target(value): Find design achieving target value"
  ))
  cli::cli_text("")

  cli::cli_h2("Important Notes")
  cli::cli_bullets(c(
    "!" = "Power (pwr_eff) is the proportion of sims where pr_eff >= thr_dec_eff",
    "!" = "If pr_eff is low, pwr_eff will be 0 even for large n",
    "!" = "Check your thresholds: thr_dec_eff, thr_fx_eff"
  ))

  invisible(NULL)
}

#' Objective Specification: Minimize N for Target Power
#'
#' Creates objective specification for finding minimum sample size
#' that achieves a target power level.
#'
#' @param target_power Target power level (0-1), default 0.80
#' @param power_metric Which power metric to use: "pwr_eff" or "pwr_fut"
#' @param n_range Numeric vector c(min, max) for n_total search range
#'
#' @return List with `search` and `objectives` for [build_objectives()]
#' @export
#'
#' @examples
#' \dontrun{
#' spec <- obj_min_n_for_power(0.80, n_range = c(50, 500))
#' obj <- build_objectives(
#'   design = design,
#'   search = spec$search,
#'   objectives = spec$objectives,
#'   constant = list(...)
#' )
#' }
obj_min_n_for_power <- function(target_power = 0.80,
                                 power_metric = "pwr_eff",
                                 n_range = c(50, 500)) {
  if (target_power <= 0 || target_power >= 1) {
    cli::cli_abort("target_power must be between 0 and 1")
  }

  objectives <- list()
  objectives[[power_metric]] <- target(target_power)

  list(
    search = list(n_total = n_range),
    objectives = objectives,
    description = paste0("Minimize n_total for ", power_metric, " >= ", target_power)
  )
}

#' Objective Specification: Maximize Power
#'
#' Creates objective specification for maximizing power.
#'
#' @param power_metric Which power metric: "pwr_eff" or "pwr_fut"
#'
#' @return List with `objectives` for [build_objectives()]
#' @export
#'
#' @examples
#' \dontrun{
#' spec <- obj_max_power()
#' obj <- build_objectives(
#'   design = design,
#'   search = list(n_total = c(50, 500)),
#'   objectives = spec$objectives,
#'   constant = list(...)
#' )
#' }
obj_max_power <- function(power_metric = "pwr_eff") {
  objectives <- list()
  objectives[[power_metric]] <- "maximize"

  list(
    objectives = objectives,
    description = paste0("Maximize ", power_metric)
  )
}

#' Objective Specification: Pareto Power vs Sample Size
#'
#' Creates multi-objective specification for finding Pareto frontier
#' of power vs sample size tradeoffs.
#'
#' @param power_metric Which power metric: "pwr_eff" or "pwr_fut"
#' @param n_range Numeric vector c(min, max) for n_total search range
#'
#' @return List with `search` and `objectives` for [build_objectives()]
#' @export
#'
#' @examples
#' \dontrun{
#' spec <- obj_pareto_power_n(n_range = c(50, 500))
#' obj <- build_objectives(
#'   design = design,
#'   search = spec$search,
#'   objectives = spec$objectives,
#'   constant = list(...)
#' )
#' }
obj_pareto_power_n <- function(power_metric = "pwr_eff",
                                n_range = c(50, 500)) {
  objectives <- list()
  objectives[[power_metric]] <- "maximize"
  objectives[["n_total"]] <- "minimize"

  list(
    search = list(n_total = n_range),
    objectives = objectives,
    description = paste0("Pareto frontier: ", power_metric, " vs n_total")
  )
}
