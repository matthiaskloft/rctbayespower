# Bayesian Optimization for rctbayespower

## Overview

Implement Bayesian optimization (BO) for automated optimal trial design discovery. This enables users to find designs that maximize power while minimizing sample size (or other objectives) without exhaustive grid search.

**Backend Support**: Works with both brms and BayesFlow backends. BayesFlow recommended for BO (milliseconds vs. minutes per evaluation).

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| API Style | S7 constructor | `rctbp_optimization()` + `optimize()` - consistent with package patterns |
| MVP Scope | Multi-objective from start | Core use case is power vs. sample size trade-off |
| Backends | Both brms + BayesFlow | Flexibility; warn users about brms speed |
| Priority Use Cases | Sample size, Pareto, Sequential | Based on user requirements |

## Use Cases (Priority Order)

### 1. Sample Size Optimization (Primary)
Find minimum `n_total` to achieve target power (e.g., 80%):
```r
# Create optimization specification
optim <- rctbp_optimization(
  design = design,
  search = list(n_total = c(50, 500)),
  objectives = list(pwr_scs = target(0.80)),  # Find n where pwr >= 0.80
  constant = list(b_arm_treat = 0.3, ...)
)

# Run optimization
result <- optimize(optim, n_sims = 500, n_evals = 30)
# Returns: optimal n_total ≈ 187
```

### 2. Multi-Objective: Power vs. Sample Size (Core)
Explore Pareto frontier of power vs. sample size trade-off:
```r
optim <- rctbp_optimization(
  design = design,
  search = list(n_total = c(50, 500), b_arm_treat = c(0.2, 0.6)),
  objectives = list(pwr_scs = "maximize", n_total = "minimize"),
  constant = list(...)
)

result <- optimize(optim, n_sims = 300, n_evals = 50)
result@pareto_front  # View trade-off frontier
plot(result)         # Visualize Pareto frontier
```

### 3. Sequential Design Optimization
Optimize interim analysis timing and sample size:
```r
optim <- rctbp_optimization(
  design = design,
  search = list(
    n_total = c(100, 400),
    interim_fraction = c(0.3, 0.7)  # When to do interim (proportion)
  ),
  objectives = list(
    expected_n = "minimize",  # Average sample size (accounting for early stopping)
    pwr_scs = "maximize"
  ),
  constant = list(b_arm_treat = 0.4, ...)
)

result <- optimize(optim, n_sims = 300, n_evals = 50)
```

### 4. Cost-Constrained Optimization (Phase 2)
Optimize power subject to budget constraint:
```r
optim <- rctbp_optimization(
  design = design,
  search = list(n_total = c(50, 500), p_alloc_treat = c(0.3, 0.7)),
  objectives = list(pwr_scs = "maximize"),
  cost_fn = function(params) {
    n_ctrl <- params$n_total * (1 - params$p_alloc_treat)
    n_treat <- params$n_total * params$p_alloc_treat
    n_ctrl * 100 + n_treat * 500  # Treatment arm more expensive
  },
  constraints = list(cost <= 50000),
  constant = list(...)
)
```

## Architecture

### Dependencies

```
Suggests:
  mlr3mbo (>= 0.2.0),
  bbotk (>= 0.7.0),
  paradox (>= 0.11.0)
```

All dependencies are lazy-loaded via `rlang::check_installed()`.

### New Files

| File | Purpose |
|------|---------|
| `R/optimization.R` | Main entry point `find_optimal_design()` + helpers |
| `R/class_optimization.R` | S7 classes for optimization specification and results |
| `R/optimization_objectives.R` | Objective function wrappers for mlr3mbo |

### Class Structure

```r
# Optimization problem specification
rctbp_optimization <- S7::new_class(
  "rctbp_optimization",
  properties = list(
    design = rctbp_design,
    search_space = S7::class_list,        # Named list: param = c(lower, upper) or param = c(val1, val2, ...)
    objectives = S7::class_list,          # Named list: pwr_scs = "maximize", n_total = "minimize", or target(0.80)
    constant = S7::class_list,            # Fixed parameters for power_analysis
    constraints = S7::class_list | NULL,  # Optional constraints (Phase 2)
    cost_fn = S7::class_function | NULL,  # Optional cost function (Phase 2)
    # BO configuration
    surrogate = S7::class_character,      # "gp" (default) or "rf"
    acq_function = S7::class_character,   # "ei", "aei", "cb", "parego", "smsego"
    initial_design = S7::class_character  # "lhs" (default), "sobol", "random"
  ),
  validator = function(self) {
    # Validate search_space parameters exist in design
    # Validate objectives are valid (pwr_scs, pwr_ftl, n_total, expected_n, cost)
    # Validate constant has all required non-search params
    NULL
  }
)

# Optimization results
rctbp_optimization_result <- S7::new_class(
  "rctbp_optimization_result",
  properties = list(
    optimization = rctbp_optimization,    # Reference to problem spec
    archive = S7::class_any,              # bbotk archive (all evaluations as data.table)
    result = S7::class_data.frame,        # Optimal design(s) - single row or Pareto set
    pareto_front = S7::class_data.frame | NULL,  # For multi-objective: non-dominated solutions
    convergence = S7::class_data.frame,   # Optimization trace: batch_nr, best_so_far
    n_sims = S7::class_integer,           # Simulations per evaluation
    n_evals = S7::class_integer,          # Total BO iterations
    elapsed_time = S7::class_numeric,     # Total runtime
    backend_used = S7::class_character    # "brms" or "bf"
  )
)

# Helper: target specification for constrained optimization
target <- function(value, direction = ">=") {
  structure(list(value = value, direction = direction), class = "rctbp_target")
}
```

### Integration with mlr3mbo

The core pattern wraps `power_analysis()` as a bbotk objective:

```r
# Internal: Create objective function for mlr3mbo
create_bo_objective <- function(design, search_space, constant, n_sims, n_cores) {

  # Define paradox search space
  domain <- do.call(paradox::ps, lapply(search_space, function(x) {
    if (is.integer(x)) paradox::p_int(lower = x[1], upper = x[2])
    else paradox::p_dbl(lower = x[1], upper = x[2])
  }))

  # Objective function
  objective_fn <- function(xs) {
    # Merge search params with constants
    all_params <- c(xs, constant)

    # Build conditions (single point)
    conditions <- build_conditions(
      design = design,
      crossed = lapply(xs, identity),  # Single values
      constant = constant
    )

    # Run power analysis
    result <- power_analysis(conditions, n_sims = n_sims, n_cores = n_cores)

    # Return objectives
    list(
      pwr_scs = result@results_conditions$pwr_scs[1],
      n_total = xs$n_total  # Pass through for multi-objective
    )
  }

  # Create bbotk objective
  bbotk::ObjectiveRFun$new(
    fun = objective_fn,
    domain = domain,
    codomain = paradox::ps(
      pwr_scs = paradox::p_dbl(tags = "maximize"),
      n_total = paradox::p_dbl(tags = "minimize")
    ),
    properties = "noisy"  # Power is estimated via simulation
  )
}
```

### Main Entry Points

#### 1. S7 Constructor (Primary API)

```r
#' Create Bayesian Optimization Specification
#'
#' @param design An rctbp_design object
#' @param search Named list of parameters to optimize with bounds
#' @param objectives Named list specifying optimization direction or targets
#' @param constant Named list of fixed parameters
#' @param surrogate Surrogate model: "gp" (default) or "rf"
#' @param acq_function Acquisition function: "ei", "aei", "parego", "smsego"
#' @param initial_design Initial design type: "lhs" (default), "sobol", "random"
#'
#' @return An rctbp_optimization object
rctbp_optimization <- function(
  design,
  search,
  objectives = list(pwr_scs = "maximize"),
  constant = list(),
  constraints = NULL,
  cost_fn = NULL,
  surrogate = "gp",
  acq_function = "auto",  # Auto-select based on single vs. multi-objective
  initial_design = "lhs"
) {
  # Validation and construction via S7
}
```

#### 2. optimize() Method

```r
#' Run Bayesian Optimization
#'
#' @param object An rctbp_optimization object
#' @param n_sims Number of simulations per evaluation (default: 200)
#' @param n_evals Maximum BO iterations (default: 50)
#' @param n_cores Number of cores for power_analysis
#' @param initial_design_size Initial random evaluations (default: 4 * n_params)
#' @param seed Random seed for reproducibility
#' @param verbose Verbosity level (0, 1, 2)
#'
#' @return An rctbp_optimization_result object
S7::method(optimize, rctbp_optimization) <- function(
  object,
  n_sims = 200,
  n_evals = 50,
  n_cores = 1,
  initial_design_size = NULL,
  seed = NULL,
  verbose = 1
) {
  # Check dependencies
  rlang::check_installed(c("mlr3mbo", "bbotk", "paradox"))

  # Warn if using brms backend
  if (object@design@backend == "brms") {
    cli::cli_warn(c(
      "Using brms backend for optimization",
      "i" = "This will be slow (~minutes per evaluation)",
      "i" = "Consider using BayesFlow backend for faster optimization"
    ))
  }

  # ... implementation
}
```

#### 3. Convenience Wrapper (Optional)

```r
#' Quick Optimization Entry Point
#'
#' Combines rctbp_optimization() + optimize() for simple cases
find_optimal_design <- function(design, search, objectives, constant, ...) {
  optim <- rctbp_optimization(design, search, objectives, constant)
  optimize(optim, ...)
}
```

## Technical Considerations

### 1. Noisy Objective Function
Power is estimated via Monte Carlo simulation, making evaluations noisy. Handle with:
- `properties = "noisy"` on bbotk Objective
- GP surrogate with nugget: `srlrn(default_gp(noisy = TRUE))`
- Augmented Expected Improvement: `acqf("aei")`
- Higher `n_sims` for more stable estimates (trade-off with speed)

### 2. Integer Parameters
Sample size `n_total` must be integer. Options:
- Use `p_int()` in paradox (native integer support)
- Round continuous values in objective function

### 3. Computational Cost
Even with BayesFlow, each evaluation takes time. Mitigation:
- Default `n_sims = 200` (lower than typical power analysis)
- Batch evaluations when possible
- Warmstart from previous runs
- Progress reporting via cli

### 4. Initial Design
Space-filling initial design improves BO efficiency:
- Default: Latin Hypercube Sampling (LHS)
- Size: 4 × number of parameters (mlr3mbo default)
- Option to provide pre-evaluated points

### 5. Multi-Objective Optimization
For power vs. sample size trade-offs:
- ParEGO: Scalarizes objectives with random weights
- SMS-EGO: Uses hypervolume improvement
- Return Pareto frontier for user selection

## Workflow Examples

### Basic: Find Minimum Sample Size for 80% Power

```r
library(rctbayespower)

# Create design
design <- build_design(
  model_name = "ancova_cont_2arms",
  backend = "bf",  # Use BayesFlow for speed
  target_params = "b_arm2"
)

# Find optimal sample size
result <- find_optimal_design(
  design = design,
  search = list(n_total = c(50, 500)),
  objectives = list(pwr_scs = list(target = 0.80, direction = ">=")),
  constant = list(
    b_arm_treat = 0.3,
    p_alloc = list(c(0.5, 0.5)),
    intercept = 0, b_covariate = 0.3, sigma = 1,
    p_sig_scs = 0.975, p_sig_ftl = 0.5,
    thresh_scs = 0.2, thresh_ftl = 0
  ),
  n_sims = 500,
  n_evals = 30
)

# View result
print(result)
# Optimal design: n_total = 187, pwr_scs = 0.81

# Plot convergence
plot(result, type = "convergence")

# Plot surrogate surface
plot(result, type = "surface")
```

### Advanced: Multi-Objective Power vs. Sample Size

```r
result <- find_optimal_design(
  design = design,
  search = list(
    n_total = c(50, 400),
    b_arm_treat = c(0.2, 0.6)
  ),
  objectives = list(
    pwr_scs = "maximize",
    n_total = "minimize"
  ),
  constant = list(...),
  n_sims = 300,
  n_evals = 50,
  acq_function = "parego"  # ParEGO for multi-objective
)

# View Pareto frontier
result@pareto_front

# Plot trade-off
plot(result, type = "pareto")
```

### With Cost Function

```r
# Define cost function
trial_cost <- function(n_total, p_alloc) {
  n_ctrl <- n_total * p_alloc[1]
  n_treat <- n_total * p_alloc[2]
  # Recruitment + per-patient costs
  5000 + n_ctrl * 200 + n_treat * 800
}

result <- find_optimal_design(
  design = design,
  search = list(
    n_total = c(50, 300),
    p_alloc_treat = c(0.3, 0.7)  # Proportion in treatment arm
  ),
  objectives = list(
    pwr_scs = "maximize",
    cost = "minimize"  # Uses cost_fn
  ),
  cost_fn = trial_cost,
  constant = list(...)
)
```

## Implementation Phases

### Phase 1: Core + Multi-Objective (MVP)
- [ ] `R/class_optimization.R`: S7 class definitions (`rctbp_optimization`, `rctbp_optimization_result`)
- [ ] `R/optimization.R`: `optimize()` method implementation
- [ ] `R/optimization_objectives.R`: bbotk objective wrapper with power_analysis integration
- [ ] Single-objective support (EGO with EI/AEI)
- [ ] Multi-objective support (ParEGO for power vs. n_total)
- [ ] Pareto frontier extraction
- [ ] Basic print/summary/plot methods
- [ ] `target()` helper for constrained optimization
- [ ] Unit tests with mock BayesFlow mode

**Deliverables:**
- Find minimum n_total for target power
- Explore power vs. sample size Pareto frontier

### Phase 2: Sequential Design Optimization
- [ ] `expected_n` objective (average sample size accounting for early stopping)
- [ ] `interim_fraction` as searchable parameter
- [ ] Validation for sequential analysis constraints
- [ ] Extended archive storage for interim metrics

**Deliverables:**
- Optimize interim timing + sample size jointly

### Phase 3: Cost & Constraints
- [ ] `cost_fn` integration (user-defined cost functions)
- [ ] `cost` as optimizable objective
- [ ] Constraint handling (penalty method or native bbotk)
- [ ] Warmstart from previous optimization runs

### Phase 4: Polish & Documentation
- [ ] Vignette: "Optimal Trial Design with Bayesian Optimization"
- [ ] roxygen2 documentation for all exports
- [ ] `show_optimization_args()` discovery helper
- [ ] Performance benchmarks (BayesFlow vs. brms)

## Files to Create/Modify

| File | Action | Purpose |
|------|--------|---------|
| `R/class_optimization.R` | Create | S7 class definitions |
| `R/optimization.R` | Create | `optimize()` method, `find_optimal_design()` wrapper |
| `R/optimization_objectives.R` | Create | bbotk objective wrapper |
| `R/plot_optimization.R` | Create | Plot methods for results |
| `R/zzz.R` | Modify | Register S7 methods for `optimize()` |
| `DESCRIPTION` | Modify | Add mlr3mbo, bbotk, paradox to Suggests |
| `NAMESPACE` | Auto | Via roxygen2 |

## Critical Files to Read Before Implementation

| File | Reason |
|------|--------|
| `R/class_design.R` | Understand design properties, backend detection |
| `R/class_conditions.R` | Understand build_conditions() interface |
| `R/class_power_analysis.R` | Understand power_analysis() + run() pattern |
| `R/backend_bf.R` | BayesFlow integration for fast inference |
| `R/compute_measures.R` | How power metrics are calculated |
| `R/S7_helpers.R` | S7 utility patterns used in package |

## References

### mlr3 Ecosystem
- [mlr3mbo: Flexible Bayesian Optimization](https://mlr3mbo.mlr-org.com/dev/index.html)
- [mlr3book: Advanced Tuning Methods and Black Box Optimization](https://mlr3book.mlr-org.com/chapters/chapter5/advanced_tuning_methods_and_black_box_optimization.html)
- [bbotk: Black-Box Optimization Toolkit (GitHub)](https://github.com/mlr-org/bbotk)
- [paradox: Universal Parameter Space Description](https://paradox.mlr-org.com/)

### BO for Clinical Trial Design
- Richter et al. (2022). "Improving adaptive seamless designs through Bayesian optimization." *Biometrical Journal*. [PubMed](https://pubmed.ncbi.nlm.nih.gov/35212423/) | [arXiv](https://arxiv.org/abs/2105.09223)

### Bayesian Clinical Trial Design
- Kunzmann et al. (2021). "A Review of Bayesian Perspectives on Sample Size Derivation for Confirmatory Trials." [PMC7612172](https://pmc.ncbi.nlm.nih.gov/articles/PMC7612172/)
- Giovagnoli (2021). "The Bayesian Design of Adaptive Clinical Trials." [PMC7826635](https://pmc.ncbi.nlm.nih.gov/articles/PMC7826635/)
- Eggleston et al. (2021). "BayesCTDesign: An R Package for Bayesian Trial Design Using Historical Control Data." *J Stat Softw*. [PMC8715862](https://pmc.ncbi.nlm.nih.gov/articles/PMC8715862/)

### R Resources
- [CRAN Task View: Clinical Trial Design, Monitoring, and Analysis](https://cran.r-project.org/web/views/ClinicalTrials.html)
- [bpp: Bayesian Predictive Power](https://cran.r-project.org/web/packages/bpp/index.html)
