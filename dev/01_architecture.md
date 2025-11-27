# Package Architecture

**Last Updated:** 2025-11-27

## Overview

`rctbayespower` uses a **pipeline architecture** where users progressively construct objects:

```
Model → Design → Conditions → Power Analysis → Results
```

The package supports **dual backends** for posterior estimation:
- **brms/Stan**: Traditional MCMC-based Bayesian inference
- **BayesFlow**: Neural posterior estimation for fast amortized inference

## Design Principles

1. **Progressive Enhancement**: Each object inherits from and extends the previous
2. **Validation at Construction**: S7 validators ensure correctness at each stage
3. **Explicit Over Implicit**: Users specify what they want rather than relying on defaults
4. **Reproducibility**: All specifications stored in objects for full reproducibility
5. **Parallelization-Friendly**: Objects designed to serialize well for parallel execution
6. **Backend Agnostic**: Core logic works with either brms or BayesFlow

## Class Hierarchy

```
rctbp_model (dual backend support)
    ↓ (contained by)
rctbp_design
    ↓ (contained by)
rctbp_conditions
    ↓ (contained by)
rctbp_power_analysis
```

## File Organization

### Core Class Files

| File | Purpose |
|------|---------|
| `R/class_model.R` | Model class (dual backend: brms + BayesFlow) |
| `R/class_design.R` | Design class definition |
| `R/class_conditions.R` | Conditions class definition |
| `R/class_power_analysis.R` | Power analysis + run() method |
| `R/class_interim.R` | Interim analysis class (placeholder) |

### Backend Files

| File | Purpose |
|------|---------|
| `R/backend_brms.R` | brms-specific estimation functions |
| `R/backend_bf.R` | BayesFlow estimation + reticulate integration |
| `R/model_cache.R` | Model download and caching system |
| `R/utils_results.R` | Shared result utilities (create_error_result) |
| `R/worker_functions.R` | Parallel worker dispatch (backend routing) |

### Model & Simulation Files

| File | Purpose |
|------|---------|
| `R/models_ancova.R` | ANCOVA model builders + batch simulation |
| `R/compute_measures.R` | Metric computation and aggregation |
| `R/design_prior.R` | Design prior handling |
| `R/required_fn_args.R` | Parameter extraction |

### Output & Visualization Files

| File | Purpose |
|------|---------|
| `R/plot_power_analysis.R` | Plotting methods |
| `R/output_system.R` | Output formatting system |
| `R/report_builders.R` | Report building functions |
| `R/report_renderers.R` | Report rendering functions |
| `R/verbosity.R` | Verbosity controls |

### Utility Files

| File | Purpose |
|------|---------|
| `R/MCSE.R` | Monte Carlo standard errors |
| `R/s3_wrappers.R` | S3 method wrappers for compatibility |
| `R/S7_helpers.R` | S7 utility functions |

### Deprecated Files (Empty, Kept for Reference)

| File | Replaced By |
|------|-------------|
| `R/backends.R` | `R/backend_brms.R`, `R/backend_bf.R` |
| `R/estimation_single.R` | `R/backend_*.R` |
| `R/estimation_sequential.R` | `R/backend_*.R` |
| `R/simulate_single_run.R` | `R/worker_functions.R` |

## Backend Architecture

### Dual Backend Design

The `rctbp_model` class supports **both backends simultaneously**:

```r
rctbp_model
├── brms_model          # brmsfit template (for brms backend)
├── bayesflow_model     # Keras model (for BayesFlow backend)
├── backend             # "brms", "bf", or "auto"
├── active_backend      # Computed: resolves "auto" to actual backend
├── backend_args_brms   # brms-specific config (chains, iter, etc.)
└── backend_args_bf     # BayesFlow config (batch_size, n_posterior_samples)
```

**Backend Selection**:
- `"brms"`: Force brms backend
- `"bf"`: Force BayesFlow backend
- `"auto"`: Prefer BayesFlow if available (faster), else brms

### Backend Files

**`R/backend_brms.R`** (~200 lines):
- `estimate_single_brms()`: Single posterior estimation
- `estimate_sequential_brms()`: Sequential interim analysis
- `extract_posterior_rvars_brms()`: Convert brmsfit to rvars
- `summarize_post_brms()`: Compute decision metrics

**`R/backend_bf.R`** (~700 lines):
- `check_bf_available()`: Verify Python/BayesFlow available
- `init_bf_python()`: Initialize Python modules (cached)
- `load_bf_model_python()`: Load .keras or .pkl models
- `detect_bf_model_type()`: Identify workflow/approximator/keras
- `sample_bf_model()`: Dispatch to appropriate sampling method
- `estimate_batch_bf()`: Core batch estimation via reticulate
- `estimate_single_bf()`: Single/batch posterior estimation
- `estimate_sequential_bf()`: Sequential interim analysis
- `compute_summaries_batch_ancova()`: Extract summary statistics
- `summarize_post_bf()`: Fast vectorized summarization
- `mock_bf_samples()`: Mock samples for testing

**`R/model_cache.R`** (~330 lines):
- `get_model_cache_dir()`: Cross-platform cache directory
- `download_model()`: Download from GitHub releases
- `load_brms_model()`: Load cached brms model
- `load_bf_model()`: Load cached BayesFlow model
- `list_models()`: Show available models
- `clear_model_cache()`: Remove cached models

### Worker Dispatch

**`R/worker_functions.R`** handles backend routing:

```r
worker_process_single(id_cond, id_iter, condition_args, design)
├── Extract backend from design@model@active_backend
├── if (backend == "brms")
│   └── estimate_single_brms(...)
├── else if (backend == "bf")
│   └── estimate_single_bf(...)
└── Return results

worker_process_batch(work_units, design)  # BayesFlow batch processing
├── Simulate data for all work units
├── estimate_single_bf(data_list, ...)  # Single forward pass
└── Return combined results
```

## Key Components

### 1. Data Simulation Functions

- Embedded in `rctbp_model` objects
- Generate trial data for given parameters
- **Required signature**: Must accept `n_total`, `p_alloc`, plus model-specific parameters
- **Batch versions**: `simulate_data_ancova_cont_2arms_batch()` for BayesFlow

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

### 2. Backend-Specific Models

**brms Models**:
- Pre-compiled and stored in `rctbp_model@brms_model`
- Compiled once with `chains=0` to avoid recompilation during simulations
- **Critical for performance**: Compilation takes 30-60 seconds

**BayesFlow Models**:
- Stored in `rctbp_model@bayesflow_model`
- Loaded via Python/reticulate
- Supports: BasicWorkflow, Approximator, raw Keras models
- **Much faster**: Single forward pass for batch of simulations

### 3. Power Metrics

Computed by backend-specific summarization functions:

| Metric | Description |
|--------|-------------|
| `pr_scs` | P(parameter > thr_scs) |
| `pr_ftl` | P(parameter < thr_ftl) |
| `dec_scs` | 1 if pr_scs >= p_sig_scs, else 0 |
| `dec_ftl` | 1 if pr_ftl >= p_sig_ftl, else 0 |
| `post_med`, `post_mad` | Posterior median and MAD |
| `post_mn`, `post_sd` | Posterior mean and SD |
| `rhat` | Convergence diagnostic (brms only) |
| `ess_bulk`, `ess_tail` | Effective sample sizes (brms only) |

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
# In prepare_design_for_workers()
design_components <- list(
  model_data_simulation_fn = design@model@data_simulation_fn,
  model_backend = design@model@active_backend,  # Resolved, not "auto"
  model_brms_model = design@model@brms_model,
  model_bayesflow_model = design@model@bayesflow_model,
  model_backend_args = backend_args,
  target_params = design@target_params,
  p_sig_scs = design@p_sig_scs,
  p_sig_ftl = design@p_sig_ftl
)
```

Worker functions handle both:
- S7 objects (for sequential execution)
- Plain lists (for parallel execution)

## BayesFlow Integration

### Summary Statistics (8-dimensional for 2-arm ANCOVA)

```r
summaries <- c(
  mean_outcome_ctrl,      # 1
  mean_outcome_treat,     # 2
  sd_outcome_ctrl,        # 3
  sd_outcome_treat,       # 4
  n_ctrl,                 # 5
  n_treat,                # 6
  cor_outcome_covariate,  # 7
  mean_covariate          # 8
)
```

### Model Types Supported

| Type | Detection | Sampling Method |
|------|-----------|-----------------|
| BasicWorkflow | has `sample()` + `fit_online()` | `workflow$sample(conditions, num_samples)` |
| Approximator | has `sample()` + `fit()` | `approximator$sample(data, num_samples)` |
| Raw Keras | has `predict()` + `compile()` | `model$predict()` + Gaussian sampling |

### Mock Mode for Testing

Set `RCTBP_MOCK_BF=TRUE` to use mock samples without Python:

```r
Sys.setenv(RCTBP_MOCK_BF = "TRUE")
# BayesFlow calls return mock samples based on data
```

## Architecture Decision: Model/Design Separation

**Decision**: Keep `rctbp_model` and `rctbp_design` as separate classes.

**Rationale**:
1. **Model compilation is expensive** (2+ minutes) - separation allows reusing compiled models
2. **Clean separation of concerns** - Model = WHAT; Design = HOW to analyze
3. **Future flexibility** - Same model with different analysis strategies
4. **Single Responsibility Principle** - Each class has one job
5. **Dual backend support** - Model holds both backends; design selects which to use

See `archive/model_design_merge_analysis.md` for full analysis.
