# Bayesian Optimization for rctbayespower

## Overview

Bayesian optimization (BO) for automated optimal trial design discovery. Finds designs that maximize power while minimizing sample size (or other objectives) without exhaustive grid search.

**Backend Support**: Both brms and BayesFlow. BayesFlow recommended (ms vs minutes per evaluation).

**Status**: Core implementation complete with warmup phase and generalized secondary objectives.

## Architecture

### Parallel to Existing Pattern

```
build_conditions() → rctbp_conditions  → power_analysis()   # Grid evaluation
build_objectives() → rctbp_objectives  → optimization()     # Search optimization
```

### Class Structure

**`rctbp_objectives`** - Problem specification (what to optimize):

| Property | Type | Description |
|----------|------|-------------|
| `design` | rctbp_design | Reference to trial design |
| `search` | list | Parameter bounds: `list(n_total = c(50, 500))` |
| `search_specs` | list | Internal: parsed simplex specs for p_alloc, looks |
| `objectives` | list | What to optimize: `list(pwr_eff = target(0.80))` |
| `constant` | list | Fixed parameters for all evaluations |
| `secondary` | list | Secondary objectives: `list(n_total = "minimize")` |
| `cost_fn` | function\|NULL | Optional user-defined cost function |
| `objective_info` | list | Internal: parsed objective information |

**`rctbp_optimization_result`** - Results:

| Property | Type | Description |
|----------|------|-------------|
| `objectives` | rctbp_objectives | Reference to problem spec |
| `archive` | data.frame | All evaluations with parameters and objectives |
| `result` | data.frame | Optimal design(s) |
| `pareto_front` | data.frame\|NULL | For multi-objective (NULL if single) |
| `convergence` | data.frame | Optimization trace |
| `best_power_analysis` | rctbp_power_analysis | Full result from best solution |
| `reference_values` | list | Reference values from warmup phase |
| `n_sims` | numeric | Maximum n_sims used |
| `n_evals` | numeric | Total evaluations |
| `elapsed_time` | numeric | Runtime in seconds |
| `early_stopped` | logical | Whether early stopping triggered |
| `backend_used` | character | "brms" or "bf" |
| `optimization_type` | character | "single", "target", or "multi" |

## Core Functions

### `build_objectives()`

Creates optimization problem specification:

```r
build_objectives(
  design,
  search,
  objectives,
  constant = list(),
  secondary = NULL,
  cost_fn = NULL
)
```

**Parameters:**

| Argument | Description |
|----------|-------------|
| `design` | rctbp_design from `build_design()` |
| `search` | Named list of bounds: `list(n_total = c(50, 500))` |
| `objectives` | Named list: `"maximize"`, `"minimize"`, or `target(value)` |
| `constant` | Fixed parameters for all evaluations |
| `secondary` | Secondary objectives (auto-inferred if NULL) |
| `cost_fn` | Optional custom cost function |

**Secondary Objectives Auto-Inference:**

When `secondary = NULL`, automatically infers from search parameters:
- `n_total` → "minimize"
- `thr_dec_eff`, `thr_dec_fut` → "minimize"
- `thr_fx_eff`, `thr_fx_fut` → "minimize"

Override with explicit list: `secondary = list(n_total = "minimize")` to only minimize n_total.

### `optimization()`

Executes Bayesian optimization:

```r
optimization(
  objectives,
  n_sims = 200,
  evals_per_step = 10,
  use_warmup = TRUE,
  max_evals = 50,
  n_cores = 1,
  surrogate = c("rf", "gp"),
  acq_function = "auto",
  init_design = c("lhs", "random", "sobol"),
  init_design_size = NULL,
  patience = 30,
  min_delta = 0.001,
  bf_args = list(),
  brms_args = list(),
  refresh = 5,
  run = TRUE,
  verbosity = 1
)
```

**Key Parameters:**

| Argument | Default | Description |
|----------|---------|-------------|
| `n_sims` | 200 | Scalar or vector for progressive fidelity |
| `evals_per_step` | 10 | Evals per fidelity level (scalar or vector) |
| `use_warmup` | TRUE | Enable warmup phase for reference estimation |
| `max_evals` | 50 | Max evaluations (ignored for vector n_sims) |
| `surrogate` | "rf" | Surrogate model: "rf" or "gp" |
| `patience` | 30 | Early stopping patience |
| `min_delta` | 0.001 | Optimal window for early stopping |
| `weight_evals` | FALSE | Weight surrogate training by MCSE (1/se²) |

### `target()`

Helper for target-constrained optimization:

```r
target(value, direction = ">=")
```

Examples:
- `target(0.80)` - Find minimum sample size where power >= 80%
- `target(0.05, "<=")` - Find design where type I error <= 5%

## Optimization Types (Auto-Detected)

| Objectives | Backend | Output |
|------------|---------|--------|
| Single (e.g., `pwr_eff = "max"`) | SingleCrit + EGO | Single optimal |
| Target (e.g., `pwr_eff = target(0.80)`) | SingleCrit + 2-component | Single optimal |
| Multiple (e.g., `pwr_eff = "max", n_total = "min"`) | MultiCrit + ParEGO | Pareto frontier |

## Target Optimization: Two-Component Objective

For target optimization (e.g., `pwr_eff = target(0.80)`), uses a two-component objective function with equal weights:

```
objective = 0.5 * power_component + 0.5 * secondary_bonus  (if target achieved)
objective = 0.5 * power_component                          (if target not achieved)
```

### Components

| Component | Formula | Range | Description |
|-----------|---------|-------|-------------|
| `power_component` | `(min(power, target) / target)²` | [0, 1] | Quadratic ramp, clamped at target |
| `secondary_bonus` | see below | unbounded | Only added when target achieved |

### Power Component (Quadratic, Clamped)

The power component provides a smooth quadratic gradient toward the target, with no benefit for overshooting:

| power | target | power_component |
|-------|--------|-----------------|
| 0.40 | 0.80 | (0.40/0.80)² = 0.25 |
| 0.60 | 0.80 | (0.60/0.80)² = 0.56 |
| 0.70 | 0.80 | (0.70/0.80)² = 0.77 |
| 0.80 | 0.80 | 1.00 (clamped) |
| 0.90 | 0.80 | 1.00 (clamped) |

### Secondary Bonus Computation

The secondary bonus encourages finding designs with preferred secondary parameter values (e.g., smaller n_total) **only after** the target is achieved.

**Two-phase approach with warmup:**

| Phase | Bonus Type | Formula (minimize) | Range | Description |
|-------|------------|-------------------|-------|-------------|
| Warmup | Inverse | `param_min / param` | (0, 1] | Bounds-independent |
| Main | Reference-based | `1 - param / reference` | unbounded | Can be negative |

**Reference-based bonus examples** (n_ref = 100):

| n_total | secondary_bonus | Total objective (target achieved) |
|---------|-----------------|-----------------------------------|
| 50 | 1 - 50/100 = +0.50 | 0.5 × 1.0 + 0.5 × 0.50 = 0.75 |
| 75 | 1 - 75/100 = +0.25 | 0.5 × 1.0 + 0.5 × 0.25 = 0.625 |
| 100 | 1 - 100/100 = 0.00 | 0.5 × 1.0 + 0.5 × 0.00 = 0.50 |
| 150 | 1 - 150/100 = -0.50 | 0.5 × 1.0 + 0.5 × (-0.50) = 0.25 |
| 500 | 1 - 500/100 = -4.00 | 0.5 × 1.0 + 0.5 × (-4.00) = -1.50 |

**Key insight**: The unbounded negative bonus for n >> reference creates a strong gradient pushing the optimizer away from high sample sizes.

**Multiple secondary objectives** are combined with equal weight:
```r
total_bonus <- mean(bonus_n_total, bonus_thr_dec_eff, ...)
```

## Warmup Phase

### Overview

When using progressive fidelity (`length(n_sims) > 1`) with `use_warmup = TRUE`:

1. **Warmup phase** (first fidelity level): Uses inverse bonus to explore and estimate reference values
2. **Main phase** (remaining levels): Uses reference-based bonus centered on warmup estimates

### Enabling Warmup

Warmup requires:
1. Progressive fidelity: `n_sims` must be a vector
2. Secondary objectives: At least one secondary objective (auto-inferred or explicit)

```r
# Warmup enabled: progressive fidelity + secondary objectives
result <- optimization(
  obj,
  n_sims = c(500, 2000),        # Vector = progressive fidelity
  evals_per_step = c(30, 20),   # 30 warmup evals, 20 main evals
  use_warmup = TRUE             # First level = warmup phase
)

# Warmup disabled: scalar n_sims
result <- optimization(obj, n_sims = 500, use_warmup = TRUE)  # Ignored
```

### Reference Values

After warmup completes:
1. Reference values are extracted from the best warmup result
2. Stored in `result@reference_values`
3. Main phase uses `1 - param/reference` for bonus computation

### Console Output

```
ℹ Warmup enabled: first 500-sim phase will estimate reference for: n_total (minimize)
ℹ [1-30/50] n_sims=500 (warmup)
ℹ [10/50] n_total = 180 | pwr_eff = 0.72
✓ [20/50] New best: n_total = 105 | pwr_eff = 0.81
✓ Warmup complete. Reference values: n_total=105
ℹ [31-50/50] n_sims=2000 (refinement)
✓ [40/50] New best: n_total = 98 | pwr_eff = 0.80
```

## Progressive Fidelity (Multi-Fidelity Optimization)

### Basic Usage

```r
# Scalar evals_per_step: equal evals per level (40 total)
optimization(obj, n_sims = c(100, 200, 500, 1000), evals_per_step = 10)

# Vector evals_per_step: custom evals per level (50 total)
optimization(obj, n_sims = c(100, 200, 500, 1000), evals_per_step = c(15, 10, 10, 15))
```

### Fidelity Schedule

The fidelity schedule determines n_sims for each evaluation:

| Eval | Fidelity Level | n_sims | Phase |
|------|----------------|--------|-------|
| 1-15 | 1 | 100 | Warmup (if enabled) |
| 16-25 | 2 | 200 | Main |
| 26-35 | 3 | 500 | Main |
| 36-50 | 4 | 1000 | Refinement |

### High-Fidelity Selection (Research-Based)

**Reference**: [arXiv:2410.00544](https://arxiv.org/abs/2410.00544) - "Best Practices for Multi-Fidelity Bayesian Optimization"

**Critical insight**: Final selection uses **high-fidelity observations only**.

Low-fidelity observations are noisy and can mislead. Example from testing:
- Low-fidelity (n_sims=300) reported pwr_eff=0.91 at n_total=366
- Surrogate predicted 0.914 (trained on noisy data)
- Final confirmation (n_sims=2000) showed 0.848 ± 0.008

**Implementation** (`finalize_with_surrogate()`):
```r
max_n_sims <- max(archive_df$n_sims_used)
high_fidelity_df <- archive_df[archive_df$n_sims_used == max_n_sims, ]
best_idx <- which.max(high_fidelity_df[[obj_name]])
```

### MCSE-based Weighted Surrogate Training

Enable `weight_evals = TRUE` to weight surrogate training observations by their Monte Carlo Standard Error (MCSE). More precise observations (lower MCSE) receive higher weight.

```r
result <- optimization(
  obj,
  n_sims = c(100, 500, 2000),
  evals_per_step = c(20, 15, 15),
  weight_evals = TRUE  # Weight by inverse-variance (1/se^2)
)
```

**How it works:**
1. Each power simulation produces an MCSE (stored as `se_pwr_eff` in archive)
2. `compute_mcse_weights()` computes inverse-variance weights: `weight = 1/se^2`
3. `wrap_surrogate_with_weights()` injects weights into the TaskRegr
4. GP/RF learners use weighted training for more accurate predictions

**Why MCSE is better than n_sims-based weighting:**
- MCSE directly measures precision of each estimate
- Same n_sims can have different MCSE depending on effect size (power near 0.5 has higher variance)
- Statistically standard: inverse-variance weighting is the textbook approach

**Implementation** (`optimization_internal.R`):
```r
# Weight computation
compute_mcse_weights <- function(se_values) {
  weights <- 1 / (se_values^2)
  weights / max(weights)  # Normalize to [0, 1]
}

# Applied in setup_mbo_components() when weight_evals = TRUE
surr <- wrap_surrogate_with_weights(surr, "se_pwr_eff")
```

## Early Stopping

For target optimization, early stopping triggers when:
1. Target is achieved within optimal window `[target, target + min_delta)`
2. `patience` consecutive evaluations pass without finding smaller secondary parameters

```r
optimization(
  obj,
  patience = 30,      # Stop after 30 evals without improvement
  min_delta = 0.001   # Optimal window: [0.80, 0.801) for target(0.80)
)
```

## Memory Management

Critical for BayesFlow backend with limited GPU memory:

```r
# In objective function (optimization_internal.R)
rm(pa_result, conditions)
if (design@backend == "bf") {
  clear_gpu_memory(r_gc = TRUE)  # Python gc + CUDA cache + R gc
} else {
  gc(verbose = FALSE, full = TRUE)
}
```

For GPU OOM issues:
```r
# Reduce batch_size
optimization(obj, bf_args = list(batch_size = 64))

# Configure PyTorch memory allocator
Sys.setenv(PYTORCH_CUDA_ALLOC_CONF = "expandable_segments:True")
```

## Simplex Search Parameters

### Allocation Probabilities

Search for optimal treatment allocation using `search_p_alloc()`:

```r
obj <- build_objectives(
  design = design,
  search = list(
    n_total = c(50, 500),
    p_alloc = search_p_alloc(min = 0.2)  # Each arm gets >= 20%
  ),
  objectives = list(pwr_eff = target(0.80)),
  constant = list(...)
)
```

**Internal handling:**
- 2-arm: Direct sampling of treatment proportion in `[min, 1-min]`
- k-arm (k>2): ILR transform on (k-1) dimensions, scaled to constraint

### Interim Look Timing

Search for optimal interim analysis timing using `search_looks()`:

```r
obj <- build_objectives(
  design = design,
  search = list(
    n_total = c(100, 400),
    analysis_at = search_looks(n = 3, min_spacing = 0.2)
  ),
  objectives = list(expected_n = "minimize", pwr_eff = target(0.80)),
  constant = list(...)
)
```

## Use Cases

### 1. Sample Size for Target Power (with Warmup)

```r
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2"
)

obj <- build_objectives(
  design = design,
  search = list(n_total = c(50, 500)),
  objectives = list(pwr_eff = target(0.80)),
  constant = list(
    b_arm_treat = 0.3,
    thr_dec_eff = 0.975, thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0,
    p_alloc = list(c(0.5, 0.5)),
    intercept = 0, b_covariate = 0.3, sigma = 1
  )
)

result <- optimization(
  obj,
  n_sims = c(500, 2000),
  evals_per_step = c(30, 20),
  use_warmup = TRUE
)

# Access results
result@result            # Optimal design
result@reference_values  # e.g., list(n_total = 105)
result@archive           # All evaluations
```

### 2. Multi-Parameter Search with Multiple Secondary Objectives

```r
obj <- build_objectives(
  design = design,
  search = list(
    n_total = c(50, 500),
    thr_dec_eff = c(0.90, 0.99)
  ),
  objectives = list(pwr_eff = target(0.80)),
  secondary = list(n_total = "minimize", thr_dec_eff = "minimize"),
  constant = list(...)
)
```

### 3. Power vs. Sample Size Pareto

```r
obj <- build_objectives(
  design = design,
  search = list(n_total = c(50, 500), b_arm_treat = c(0.2, 0.6)),
  objectives = list(pwr_eff = "maximize", n_total = "minimize"),
  constant = list(...)
)

result <- optimization(obj, n_sims = 300, max_evals = 50)
result@pareto_front
```

## Files

| File | Purpose |
|------|---------|
| `R/class_objectives.R` | `rctbp_objectives`, `rctbp_optimization_result` S7 classes, print methods |
| `R/optimization.R` | `build_objectives()`, `optimization()`, `target()`, `search_p_alloc()`, `search_looks()` |
| `R/optimization_internal.R` | `run_optimization()`, `create_objective_fn()`, mlr3mbo/bbotk integration |
| `R/plot_optimization.R` | Plot methods (convergence, pareto, search space) |

## Dependencies

```
Suggests:
  mlr3mbo (>= 0.2.0),
  bbotk (>= 0.7.0),
  paradox (>= 0.11.0)
```

Lazy-loaded via `rlang::check_installed()`.

## Implementation Status

### Completed

- [x] S7 classes (`rctbp_objectives`, `rctbp_optimization_result`)
- [x] `build_objectives()` with validation
- [x] `optimization()` with auto-detection
- [x] SingleCrit (EGO) + MultiCrit (ParEGO) support
- [x] `target()` helper with three-component objective
- [x] Progressive fidelity (multi-fidelity n_sims)
- [x] Warmup phase with reference-based bonus
- [x] Secondary objectives (generalized beyond n_total)
- [x] High-fidelity final selection
- [x] GPU/RAM memory management
- [x] Basic print/plot methods
- [x] `show_optimization_args()` helper
- [x] Simplex search (`search_p_alloc()`, `search_looks()`)
- [x] Early stopping with patience

### Planned

- [ ] Unit tests with mock BayesFlow
- [ ] `expected_n` objective for sequential designs
- [ ] `cost_fn` integration
- [ ] Constraint handling
- [ ] Vignette documentation

## Technical Details

### Bonus Comparison: Inverse vs Reference-Based

For `n_total` with bounds [50, 500] and reference 100:

| n_total | Inverse (50/n) | Reference (1-n/100) |
|---------|----------------|---------------------|
| 50 | 1.00 | 0.50 |
| 75 | 0.67 | 0.25 |
| 100 | 0.50 | 0.00 |
| 150 | 0.33 | -0.50 → 0 (clamped) |
| 500 | 0.10 | -4.00 → 0 (clamped) |

**Key insight**: Inverse bonus is bounds-independent (always rewards smaller values). Reference-based bonus provides stronger gradient near the reference point.

### Surrogate Model Considerations

- **GP (Gaussian Process)**: Better for smooth functions, handles noise
- **RF (Random Forest)**: Better for non-smooth, handles categorical

Default is RF for robustness. GP recommended for smoother power surfaces.

### Surrogate Uncertainty at Training Points

**Technical note**: Surrogate SE ≈ 0 at training points (archive observations) because interpolating surrogates (GP, RF) have zero uncertainty at observed data. This SE is **not meaningful** for reporting - only useful for acquisition functions at unobserved points.

## References

- [mlr3mbo](https://mlr3mbo.mlr-org.com/dev/index.html)
- [Richter et al. (2022) - BO for clinical trials](https://pubmed.ncbi.nlm.nih.gov/35212423/)
- [arXiv:2410.00544 - Multi-Fidelity BO Best Practices](https://arxiv.org/abs/2410.00544)
