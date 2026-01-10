# Reference: File Organization and Package Status

This file contains reference material moved from CLAUDE.md to keep that file concise per [Boris Cherny's best practices](https://howborisusesclaudecode.com/).

## Current Status (2025-11-29)

**Core Package State**: Functional with dual-backend support. API refactoring completed (model merged into design).

### Implemented

| Feature | Status |
|---------|--------|
| Core workflow (`build_design` → `build_conditions` → `power_analysis`) | Complete |
| S7 class system (`rctbp_design`, `rctbp_conditions`, `rctbp_power_analysis`) | Complete |
| API refactoring: model merged into design | Complete |
| ANCOVA continuous models (2-arm and 3-arm) | Complete |
| Design prior integration | Complete |
| Parallelization with model caching | Complete |
| Plotting (power curves, heatmaps) | Complete |
| Documentation (vignettes, manual pages) | Complete |
| Group sequential / interim analysis (non-adaptive) | Complete |
| Look-dependent stopping boundaries | Complete |
| Boundary re-analysis functions | Complete |
| Dual backend support (brms + BayesFlow) | Complete |
| BayesFlow reticulate integration | Complete |
| Mock mode for testing without Python | Complete |
| Bayesian optimization for design search | Complete |
| trial_type parameter | Complete |

### Backend System

| Component | File | Status |
|-----------|------|--------|
| brms backend | `R/backend_brms.R` | Complete |
| BayesFlow backend | `R/backend_bf.R` | Complete (reticulate calls) |
| Model caching | `R/model_cache.R` | Complete |
| Merged design class | `R/class_design.R` | Complete (model properties merged) |
| Legacy model class | `R/class_model.R` | Deprecated (backward compat) |
| Worker dispatch | `R/worker_functions.R` | Complete |
| Batch simulation | `R/models_ancova.R` | Complete |
| Shared utilities | `R/utils_results.R` | Complete |

### Planned / In Progress

| Feature | Status | Plan |
|---------|--------|------|
| Adaptive interim analysis (parameter modification) | Planning | `20_interim_analysis_plan.md` |
| Train BayesFlow models | Next step | `11_bayesflow_integration_roadmap.md` |
| Binary/survival outcomes | Not started | `21_adaptive_trials_roadmap.md` |
| Test suite | 0% | `06_testing.md` |

---

## File Organization

### Core Classes (`R/class_*.R`)

| File | Purpose |
|------|---------|
| `R/class_design.R` | Merged design class (includes model properties, dual backend) |
| `R/class_model.R` | Legacy model class (backward compatibility) |
| `R/class_conditions.R` | Conditions class definition |
| `R/class_power_analysis.R` | Power analysis + run() + print/summary methods |
| `R/class_pareto_result.R` | Pareto optimization result class (`rctbp_pareto_result`) |
| `R/models_ancova.R` | ANCOVA model builders + batch simulation |

### Backend System

| File | Purpose |
|------|---------|
| `R/backend_brms.R` | brms-specific estimation (single + sequential) |
| `R/backend_bf.R` | BayesFlow estimation + pre-allocated batch prep |
| `R/model_cache.R` | Model download and caching system |
| `R/utils_results.R` | Shared error result utilities |
| `R/worker_functions.R` | Parallel worker dispatch + S7 serialization |

**Key BayesFlow functions** (in `R/backend_bf.R`):
- `get_batch_field_map(model_type)` - Field mapping registry for batch prep
- `prepare_data_list_as_batch_bf()` - Pre-allocated O(n) batch preparation

### Boundaries

| File | Purpose |
|------|---------|
| `R/boundaries.R` | Stopping boundary functions (OBF, Pocock, linear, power) |

### Optimization

| File | Purpose |
|------|---------|
| `R/pareto_optimize.R` | Core `pareto_optimize()` + knee point selection |
| `R/pareto_wrappers.R` | Wrapper functions: `optimize_power_n()`, `optimize_power_effect()`, `optimize_effect_n()` |
| `R/optimization.R` | Simplex search helpers: `search_p_alloc()`, `search_looks()` |
| `R/optimization_internal.R` | mlr3mbo/bbotk integration, surrogate setup, ILR transforms |
| `R/plot_optimization.R` | Optimization result plots (pareto, convergence, search) |

### Plotting (`R/plot_*.R`)

| File | Purpose |
|------|---------|
| `R/plot_power_analysis.R` | Main S7 plot method + dispatcher |
| `R/plot_power_curve.R` | Power curve visualization |
| `R/plot_heatmap.R` | 2D heatmap visualization |
| `R/plot_comparison.R` | Power vs probability comparison |
| `R/plot_helpers.R` | Shared utilities (pivot, colors, theme) |

### Reporting (`R/report_*.R`)

| File | Purpose |
|------|---------|
| `R/report_builders.R` | Build structured report data + topic reports |
| `R/report_renderers.R` | CLI/Markdown table rendering |

### Discovery/Helper Functions

| Function | File | Purpose |
|----------|------|---------|
| `show_predefined_models()` | `R/class_design.R` | List available predefined models |
| `show_target_params(model_name)` | `R/required_fn_args.R` | Show available target parameters |
| `show_condition_args(design)` | `R/required_fn_args.R` | Show required arguments for build_conditions() |
| `show_boundaries()` | `R/boundaries.R` | List available boundary functions |

### Utilities

| File | Purpose |
|------|---------|
| `R/compute_measures.R` | Posterior measure computation from rvars |
| `R/verbosity.R` | Three-level verbosity control (0, 1, 2) |
| `R/output_system.R` | CLI/Markdown dual-mode output system |
| `R/MCSE.R` | Monte Carlo standard error calculations |
| `R/required_fn_args.R` | Helper functions for parameter discovery |
| `R/S7_helpers.R` | S7 utility functions |
| `R/s3_wrappers.R` | S3 method wrappers for S7 classes |

### Python Integration

| File | Purpose |
|------|---------|
| `R/setup_python.R` | Environment setup helpers (GPU detection, install) |
| `R/python_simulators.R` | Python simulator loading (reticulate) |

### Package Infrastructure

| File | Purpose |
|------|---------|
| `R/zzz.R` | .onLoad hook (S7 method registration) |
| `R/rctbayespower-package.R` | Package documentation + globalVariables |

### Legacy Files

| File/Directory | Status |
|------|--------|
| `R/class_model.R` | Deprecated - legacy model class kept for backward compat |
| `R/legacy/` | Archive of old API implementations (NOT loaded by package) |

---

## Deprecation Policy

**Package not yet released**: Deprecated functions are removed rather than issuing deprecation warnings. Legacy code is archived in `R/legacy/` for reference.

**When package is released**: Add `lifecycle::deprecate_warn()` calls to any remaining legacy functions before removal.

---

## Extended Workflow Examples

### Quick Workflow Example

```r
# Build design with predefined model (defaults to brms backend)
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2"
)

# Discover available parameter names
design@par_names_inference    # Parameters available for target_params
design@par_names_sim          # Simulation function parameters
show_condition_args(design)   # Show required arguments for build_conditions()

# Build conditions
conditions <- build_conditions(
  design = design,
  crossed = list(n_total = c(100, 200), b_arm_treat = c(0.3, 0.5)),
  constant = list(
    p_alloc = list(c(0.5, 0.5)),
    intercept = 0, b_covariate = 0.3, sigma = 1,
    thr_dec_eff = 0.975, thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0
  )
)

result <- power_analysis(conditions = conditions, n_sims = 100, n_cores = 4)
plot(result)
```

### BayesFlow Backend

```r
# First time setup
setup_bf_python()

# Each session: Initialize before BayesFlow operations
init_bf()

# Use BayesFlow backend
design <- build_design(
  model_name = "ancova_cont_2arms",
  backend = "bf",
  target_params = "b_arm2"
)

# Diagnostic functions
check_bf_status()
verify_bf_installation()
get_bf_env_info()
```

### Sequential Design with Boundaries

```r
design <- build_design(model_name = "ancova_cont_2arms", target_params = "b_arm2")

conditions <- build_conditions(
  design = design,
  crossed = list(
    link(
      n_total = c(100, 200),
      analysis_at = list(c(50, 100), c(100, 200))
    )
  ),
  constant = list(
    b_arm_treat = 0.5, p_alloc = list(c(0.5, 0.5)),
    intercept = 0, b_covariate = 0.3, sigma = 1,
    thr_fx_eff = 0.2, thr_fx_fut = 0,
    thr_dec_eff = boundary_obf(threshold = 0.95),
    thr_dec_fut = boundary_linear(0.30, 0.50)
  )
)

# Available boundary functions:
# boundary_obf(), boundary_pocock(), boundary_hsd(),
# boundary_linear(), boundary_power(), boundary_constant()
```

### Trial Types

| Type | Description | Required |
|------|-------------|----------|
| `fixed` (default) | Single final analysis | None |
| `group_sequential` | Multiple looks with stopping rules | `analysis_at` |
| `adaptive` | Parameter modification between looks | `analysis_at`, specific params |

### Pareto Optimization

```r
# Power vs Sample Size
result <- optimize_power_n(
  design = design, power_metric = "pwr_eff",
  n_range = c(50, 500), effect_size = 0.3,
  constant = list(...), n_sims = 500, n_cores = 4
)

# Access results
result@pareto_front      # Pareto-optimal points
result@selected_design   # Knee point (auto-selected)
plot(result)
```

---

## CLI Efficiency Best Practices

For **reports and multi-line output**: Collect ALL content first, output with a **single cat() call**.

```r
# Build ALL sections, then output once
all_output <- character()
all_output <- c(all_output, "\n-- Section 1 --", "")
for (item in items) {
  all_output <- c(all_output, paste0("  - ", item$name))
}
cat(paste(all_output, collapse = "\n"), "\n")
```

For **status messages**: Individual CLI calls are fine.

```r
cli::cli_alert_info("Starting analysis...")
cli::cli_alert_success("Complete")
```
