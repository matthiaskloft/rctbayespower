# Pareto Optimization for rctbayespower

## Overview

Pareto-based Bayesian optimization for automated optimal trial design discovery. Finds the Pareto front of designs trading off two objectives (e.g., power vs sample size) without exhaustive grid search.

**Backend Support**: Both brms and BayesFlow. BayesFlow recommended (ms vs minutes per evaluation).

**Status**: Core implementation complete with Pareto front extraction and knee point selection.

## Architecture

### Class Structure

**`rctbp_pareto_result`** - Results from Pareto optimization:

| Property | Type | Description |
|----------|------|-------------|
| `design` | rctbp_design | Reference to trial design |
| `pareto_front` | data.frame | Pareto-optimal solutions |
| `archive` | data.frame | All evaluations with parameters and objectives |
| `selected_design` | data.frame | Knee point (auto-selected) |
| `convergence` | data.frame | Optimization trace |
| `optimization_type` | character | "power_n", "power_effect", "effect_n", or "custom" |
| `objectives` | list | Objectives specification |
| `search` | list | Search parameter bounds |
| `n_sims` | numeric | Maximum n_sims used |
| `n_evals` | numeric | Total evaluations |
| `elapsed_time` | numeric | Runtime in minutes |
| `mbo_objects` | list | mlr3mbo/bbotk objects for advanced access |

## Core Functions

### `pareto_optimize()`

Core Pareto optimization function:

```r
pareto_optimize(
  design,
  objectives,           # list(pwr_eff = "maximize", n_total = "minimize")
  search,               # list(n_total = c(50, 500), ...)
  constant = list(),
  constraint = NULL,    # Optional: list(metric = "pwr_eff", threshold = 0.80)
  n_sims = 1000,
  evals_per_step = 10,
  max_evals = 50,
  n_cores = 1,
  knee_method = "utopia",
  surrogate = "gp"
)
```

**Parameters:**

| Argument | Description |
|----------|-------------|
| `design` | rctbp_design from `build_design()` |
| `objectives` | Named list of exactly 2 objectives with directions |
| `search` | Named list of bounds or simplex specs |
| `constant` | Fixed parameters for all evaluations |
| `constraint` | Optional power threshold constraint |
| `n_sims` | Simulations per eval (scalar or vector for progressive fidelity) |
| `knee_method` | "utopia" (default), "min_cost", or "linear" |
| `surrogate` | "gp" (Gaussian Process) or "rf" (Random Forest) |

### Wrapper Functions

Three specialized wrappers for common use cases:

#### `optimize_power_n()` - Power vs Sample Size

```r
optimize_power_n(
  design,
  power_metric = "pwr_eff",
  n_range = c(50, 500),
  effect_size,              # Required: fixed effect size
  constant = list(),
  ...
)
```

Returns Pareto front of (power, n_total) for a fixed treatment effect.

#### `optimize_power_effect()` - Power vs Effect Size

```r
optimize_power_effect(
  design,
  power_metric = "pwr_eff",
  effect_range = c(0.1, 0.8),
  n_total,                  # Required: fixed sample size
  constant = list(),
  ...
)
```

Returns Pareto front of (power, effect_size) for a fixed sample size. Useful for finding minimum detectable effect (MDE).

#### `optimize_effect_n()` - Effect Size vs Sample Size

```r
optimize_effect_n(
  design,
  power_target = 0.80,
  power_metric = "pwr_eff",
  n_range = c(50, 500),
  effect_range = c(0.1, 0.8),
  constant = list(),
  ...
)
```

Returns Pareto front of (effect_size, n_total) pairs that achieve target power. Constraint filters designs not meeting the power threshold.

## Knee Point Selection

Three methods for selecting from the Pareto front:

1. **Utopia** (default): Closest to ideal point in normalized space
   - Normalize both objectives to [0, 1]
   - Flip directions (maximize → 1-x)
   - Find point with minimum Euclidean distance to origin

2. **Min Cost**: Simply minimize the second objective
   - Useful when you want the smallest n_total regardless of power tradeoff

3. **Linear**: Maximum perpendicular distance from endpoints line
   - Also known as the "L-method"
   - Finds the point where the Pareto curve bends most

## Progressive Fidelity

Vector `n_sims` enables multi-fidelity optimization:

```r
optimize_power_n(
  ...,
  n_sims = c(500, 2000),     # Two fidelity levels
  evals_per_step = c(30, 20) # 30 evals at 500, 20 at 2000
)
```

Early evaluations use low fidelity (cheap exploration), later evaluations use high fidelity (precise refinement).

## Simplex Search Helpers

For constrained search over allocation probabilities or look timings:

```r
# Allocation search with minimum 20% per group
search = list(
  n_total = c(50, 500),
  p_alloc = search_p_alloc(min = 0.2)
)

# Interim look timing with minimum 20% spacing
search = list(
  n_total = c(100, 400),
  analysis_at = search_looks(n = 3, min_spacing = 0.2)
)
```

Internally uses ILR (Isometric Log-Ratio) transform to map simplex constraints to unconstrained space.

## Visualization

```r
result <- optimize_power_n(...)

# Pareto front plot (default)
plot(result)

# Convergence trace
plot(result, type = "convergence")

# Search space exploration
plot(result, type = "search")

# All plots
plot(result, type = "all")
```

## Files

| File | Purpose |
|------|---------|
| `R/class_pareto_result.R` | `rctbp_pareto_result` S7 class |
| `R/pareto_optimize.R` | Core `pareto_optimize()` + knee selection |
| `R/pareto_wrappers.R` | Three wrapper functions |
| `R/optimization.R` | Simplex search helpers |
| `R/optimization_internal.R` | mlr3mbo/bbotk integration, ILR transforms |
| `R/plot_optimization.R` | Plot methods |
