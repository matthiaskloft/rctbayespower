# Package Architecture

**Last Updated:** 2025-11-24

## Overview

`rctbayespower` uses a **pipeline architecture** where users progressively construct objects:

```
Model → Design → Conditions → Power Analysis → Results
```

## Design Principles

1. **Progressive Enhancement**: Each object inherits from and extends the previous
2. **Validation at Construction**: S7 validators ensure correctness at each stage
3. **Explicit Over Implicit**: Users specify what they want rather than relying on defaults
4. **Reproducibility**: All specifications stored in objects for full reproducibility
5. **Parallelization-Friendly**: Objects designed to serialize well for parallel execution

## Class Hierarchy

```
rctbp_model
    ↓ (contained by)
rctbp_design
    ↓ (contained by)
rctbp_conditions
    ↓ (contained by)
rctbp_power_analysis
```

## File Organization

| Directory/File | Purpose |
|----------------|---------|
| `R/class_model.R` | Model class definition |
| `R/class_design.R` | Design class definition |
| `R/class_conditions.R` | Conditions class definition |
| `R/class_power_analysis.R` | Power analysis + run() method |
| `R/class_interim.R` | Interim analysis class (placeholder) |
| `R/models_ancova.R` | ANCOVA model builders |
| `R/simulate_single_run.R` | Core simulation engine |
| `R/compute_measures.R` | Metric computation and aggregation |
| `R/plot_power_analysis.R` | Plotting methods |
| `R/MCSE.R` | Monte Carlo standard errors |
| `R/design_prior.R` | Design prior handling |
| `R/required_fn_args.R` | Parameter extraction |
| `R/backends.R` | Backend abstraction (brms/NPE dispatcher) |
| `R/estimation_single.R` | Single posterior estimation |
| `R/estimation_sequential.R` | Sequential estimation for interim |
| `R/worker_functions.R` | Parallel worker functions |
| `R/s3_wrappers.R` | S3 method wrappers for compatibility |
| `R/S7_helpers.R` | S7 utility functions |
| `R/output_system.R` | Output formatting system |
| `R/report_builders.R` | Report building functions |
| `R/report_renderers.R` | Report rendering functions |
| `R/verbosity.R` | Verbosity controls |
| `data-raw/build_predefined_models.R` | Creates internal model data |

## Key Components

### 1. Data Simulation Functions

- Embedded in `rctbp_model` objects
- Generate trial data for given parameters
- **Required signature**: Must accept `n_total`, `p_alloc`, plus model-specific parameters

Example:
```r
simulate_data_ancova(
  n_total,           # Total sample size
  p_alloc = c(0.5, 0.5),  # Allocation ratios
  intercept = 0,     # Baseline mean
  b_arm_treat = 0.3, # Treatment effect
  b_covariate = 0.2, # Covariate effect
  sigma = 1          # Residual SD
)
```

### 2. BRMS Model Templates

- Pre-compiled and stored in `rctbp_model@brms_model`
- Compiled once with `chains=0` to avoid recompilation during simulations
- **Critical for performance**: Compilation takes 30-60 seconds; pre-compiling once saves hours on large studies

### 3. Power Metrics

Computed by `compute_measures_brmsfit()`:

| Metric | Description |
|--------|-------------|
| `pr_scs` | P(parameter > thr_scs) |
| `pr_ftl` | P(parameter < thr_ftl) |
| `dec_scs` | 1 if pr_scs >= p_sig_scs, else 0 (raw) |
| `dec_ftl` | 1 if pr_ftl >= p_sig_ftl, else 0 (raw) |
| `median`, `mad` | Posterior median and MAD |
| `mean`, `sd` | Posterior mean and SD |
| `rhat` | Convergence diagnostic |
| `ess_bulk`, `ess_tail` | Effective sample sizes |

**Union Measures**: When multiple parameters are analyzed, also computes combined metrics where ALL parameters must meet criteria.

### 4. Result Aggregation

`summarize_sims()` averages metrics across simulations:
- Groups by condition (id_cond) and parameter
- Computes mean of each metric
- Calculates Monte Carlo standard errors (MCSE)

## Parallelization Strategy

**Challenge**: S7 objects don't serialize well to parallel workers.

**Solution**: Extract needed information into plain lists before parallelization:

```r
design_components <- list(
  target_params = design@target_params,
  p_sig_scs = design@p_sig_scs,
  p_sig_ftl = design@p_sig_ftl,
  model_data_simulation_fn = model@data_simulation_fn,
  model_brms_model = model@brms_model
)
```

Worker functions (`simulate_single_run()`, `compute_measures_brmsfit()`) handle both:
- S7 objects (for sequential execution)
- Plain lists (for parallel execution)

## Architecture Decision: Model/Design Separation

**Decision**: Keep `rctbp_model` and `rctbp_design` as separate classes.

**Rationale**:
1. **Model compilation is expensive** (2+ minutes) - separation allows reusing compiled models
2. **Clean separation of concerns** - Model = WHAT; Design = HOW to analyze
3. **Future flexibility** - Same model with different analysis strategies
4. **Single Responsibility Principle** - Each class has one job

See `archive/model_design_merge_analysis.md` for full analysis.
