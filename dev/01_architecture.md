# Package Architecture

**Last Updated:** 2025-11-29

## Overview

`rctbayespower` uses a **pipeline architecture** where users progressively construct objects:

```
Design → Conditions → Power Analysis → Results
```

The `rctbp_design` class now contains all model properties (simulation function, inference model, backend selection). The previous `rctbp_model` class is deprecated.

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
rctbp_design (contains merged model properties)
    ↓ (contained by)
rctbp_conditions
    ↓ (contained by)
rctbp_power_analysis
```

Note: `rctbp_model` is deprecated. All model properties (sim_fn, inference_model, backend) are now part of `rctbp_design`.

## File Organization

### Core Class Files

| File | Purpose |
|------|---------|
| `R/class_design.R` | Design class with merged model properties (dual backend) |
| `R/class_conditions.R` | Conditions class definition |
| `R/class_power_analysis.R` | Power analysis + run() method |
| `R/class_model.R` | **DEPRECATED** - Legacy model class for backward compatibility |

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

### Archived/Legacy Files

Legacy code is archived in `R/legacy/` for reference. These files are NOT loaded by the package.

## Backend Architecture

### Dual Backend Design

The `rctbp_design` class (with merged model properties) supports **both backends**:

```r
rctbp_design
├── sim_fn              # Data simulation function
├── inference_model     # brmsfit template (for brms backend)
├── bf_model            # Keras/BayesFlow model (for bf backend)
├── backend             # "brms" or "bf"
├── target_params       # Parameters to analyze
├── par_names_inference # Parameter names in inference model
└── par_names_sim       # Parameter names in simulation function
```

**Backend Selection** (specified via `build_design(backend = ...)`):
- `"brms"`: Use brms/Stan backend (default, always available)
- `"bf"`: Use BayesFlow backend (requires Python + BayesFlow)

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
├── Extract backend from design@backend
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

- Embedded in `rctbp_design` objects (via `sim_fn` property)
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
- Pre-compiled and stored in `rctbp_design@inference_model`
- Compiled once with `chains=0` to avoid recompilation during simulations
- **Critical for performance**: Compilation takes 30-60 seconds

**BayesFlow Models**:
- Stored in `rctbp_design@bf_model`
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
  sim_fn = design@sim_fn,
  backend = design@backend,
  inference_model = design@inference_model,
  bf_model = design@bf_model,
  target_params = design@target_params
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

## Architecture Decision: Model/Design Merger

**Decision** (2025-11): Merge `rctbp_model` properties into `rctbp_design`.

**Rationale**:
1. **Simpler API**: Users only need `build_design()` instead of `build_model()` + `build_design()`
2. **Predefined models**: Most users use predefined models via `model_name` parameter
3. **Reduced complexity**: Fewer S7 classes to understand and maintain
4. **Model caching**: Pre-compiled models are cached on disk, not in objects

**Migration**:
- Old: `model <- get_model("ancova_cont_2arms"); design <- build_design(model, ...)`
- New: `design <- build_design(model_name = "ancova_cont_2arms", ...)`

The deprecated `rctbp_model` class is kept in `R/class_model.R` for backward compatibility during development.
