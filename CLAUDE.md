# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`rctbayespower` is an R package for conducting Bayesian power analysis for randomized controlled trials (RCTs). The package supports dual backends:
- **brms/Stan**: Traditional MCMC-based Bayesian inference
- **BayesFlow**: Neural posterior estimation for fast amortized inference

The package provides tools for estimating power curves, determining optimal sample sizes, and incorporating prior knowledge about treatment effects using region of practical equivalence (ROPE) for decision making.

## Developer Documentation

Documentation is organized in `dev/` as numbered topic files:

| File | Topic |
|------|-------|
| [`01_architecture.md`](dev/01_architecture.md) | Package architecture, class hierarchy, backend system |
| [`02_s7_classes.md`](dev/02_s7_classes.md) | S7 class definitions, properties, patterns |
| [`03_workflow.md`](dev/03_workflow.md) | User workflow, API reference, debugging |
| [`04_development_guidelines.md`](dev/04_development_guidelines.md) | Code style, roxygen, R CMD check |
| [`05_testing.md`](dev/05_testing.md) | Testing strategy, parallel test setup |
| [`06_interim_analysis_plan.md`](dev/06_interim_analysis_plan.md) | Group sequential designs, boundary functions |
| [`07_backend_abstraction_plan.md`](dev/07_backend_abstraction_plan.md) | **[IMPLEMENTED]** Backend abstraction design |
| [`08_adaptive_trials_roadmap.md`](dev/08_adaptive_trials_roadmap.md) | **[PLANNED]** Binary, survival, adaptive designs |
| [`09_bayesian_adaptive_designs_reference.md`](dev/09_bayesian_adaptive_designs_reference.md) | Reference: Bayesian adaptive trial designs |
| [`10_bayesflow_integration_roadmap.md`](dev/10_bayesflow_integration_roadmap.md) | BayesFlow integration status and next steps |
| [`11_code_consistency_review.md`](dev/11_code_consistency_review.md) | Code consistency patterns, naming conventions |
| [`12_bayesian_optimization_plan.md`](dev/12_bayesian_optimization_plan.md) | **[IMPLEMENTED]** Bayesian optimization for design search |
| [`13_claude_code_settings.md`](dev/13_claude_code_settings.md) | Claude Code settings, hooks, and security configuration |

Archived files in `dev/archive/`.

## Current Status (2025-11-29)

**Core Package State**: Functional with dual-backend support. API refactoring completed (model merged into design).

### Implemented

| Feature | Status |
|---------|--------|
| Core workflow (`build_design` → `build_conditions` → `power_analysis`) | ✅ |
| S7 class system (`rctbp_design`, `rctbp_conditions`, `rctbp_power_analysis`) | ✅ |
| **API refactoring: model merged into design** | ✅ |
| ANCOVA continuous models (2-arm and 3-arm) | ✅ |
| Design prior integration | ✅ |
| Parallelization with model caching | ✅ |
| Plotting (power curves, heatmaps) | ✅ |
| Documentation (vignettes, manual pages) | ✅ |
| Group sequential / interim analysis (non-adaptive) | ✅ |
| Look-dependent stopping boundaries | ✅ |
| Boundary re-analysis functions | ✅ |
| **Dual backend support (brms + BayesFlow)** | ✅ |
| **BayesFlow reticulate integration** | ✅ |
| **Mock mode for testing without Python** | ✅ |
| **Bayesian optimization for design search** | ✅ |
| **trial_type parameter** | ✅ |

### Backend System

| Component | File | Status |
|-----------|------|--------|
| brms backend | `R/backend_brms.R` | ✅ Complete |
| BayesFlow backend | `R/backend_bf.R` | ✅ Complete (reticulate calls) |
| Model caching | `R/model_cache.R` | ✅ Complete |
| Merged design class | `R/class_design.R` | ✅ Complete (model properties merged) |
| Legacy model class | `R/class_model.R` | ✅ Deprecated (backward compat) |
| Worker dispatch | `R/worker_functions.R` | ✅ Complete |
| Batch simulation | `R/models_ancova.R` | ✅ Complete |
| Shared utilities | `R/utils_results.R` | ✅ Complete |

### Planned / In Progress

| Feature | Status | Plan |
|---------|--------|------|
| Adaptive interim analysis (parameter modification) | Planning | `06_interim_analysis_plan.md` |
| Train BayesFlow models | Next step | `10_bayesflow_integration_roadmap.md` |
| Binary/survival outcomes | Not started | `08_adaptive_trials_roadmap.md` |
| Test suite | 0% | `05_testing.md` |

### Quick Workflow Example

```r
# Build design with predefined model (defaults to brms backend)
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2"  # Use design@par_names_inference to discover
  # trial_type = "fixed" is default; use "group_sequential" or "adaptive" for interim analyses
)

# Discover available parameter names
design@par_names_inference    # Parameters available for target_params
design@par_names_sim          # Simulation function parameters
design@backend                # "brms" or "bf"
show_condition_args(design)   # Show required arguments for build_conditions()

# Build conditions with three parameter types:
# - crossed: Cartesian product (all combinations)
# - linked: vary 1-to-1 with a crossed parameter
# - constant: same value for all conditions
conditions <- build_conditions(
  design = design,
  crossed = list(n_total = c(100, 200), b_arm_treat = c(0.3, 0.5)),
  constant = list(
    # Simulation parameters
    p_alloc = list(c(0.5, 0.5)),
    intercept = 0,
    b_covariate = 0.3,
    sigma = 1,
    # Decision parameters
    thr_dec_eff = 0.975,    # Probability threshold for efficacy
    thr_dec_fut = 0.5,      # Probability threshold for futility
    thr_fx_eff = 0.2,       # Effect size threshold for efficacy
    thr_fx_fut = 0          # Effect size threshold for futility
  )
)
result <- power_analysis(conditions = conditions, n_sims = 100, n_cores = 4)
plot(result)
result@results_conditions
```

### BayesFlow Backend (When Available)

```r
# =============================================================================
# FIRST TIME SETUP (run once per machine)
# =============================================================================
setup_bf_python()  # Auto-detects CUDA, creates venv, installs packages

# Or with specific options:
setup_bf_python(cuda_version = "12.4")  # Specific CUDA version
setup_bf_python(cuda_version = "cpu")   # CPU-only

# =============================================================================
# EACH SESSION: Initialize at the start of your script (REQUIRED)
# =============================================================================
# IMPORTANT: Call init_bf() BEFORE any BayesFlow operations.
# All BayesFlow functions require this - they will error if not initialized.
# This sets KERAS_BACKEND, activates the venv, and validates dependencies.
init_bf()  # Uses default "r-rctbayespower" environment

# Or use a custom environment name:
init_bf("my-custom-env")

# Output on success:
# ✔ BayesFlow 2.0.1 initialized: GPU (NVIDIA RTX 4090), env: r-rctbayespower

# If not initialized, BayesFlow functions will error:
# ✖ BayesFlow not initialized
# ℹ Call init_bf() at the start of your script

# If environment not found:
# ✖ Could not activate virtual environment "r-rctbayespower"
# ℹ Run setup_bf_python() to create the environment

# =============================================================================
# THEN USE BAYESFLOW BACKEND
# =============================================================================
design <- build_design(
  model_name = "ancova_cont_2arms",
  backend = "bf",
  target_params = "b_arm2"
)

# BayesFlow-specific options
result <- power_analysis(
  conditions,
  n_sims = 100,
  bf_args = list(n_posterior_samples = 2000)  # batch_size defaults to n_sims
)

# =============================================================================
# DIAGNOSTIC FUNCTIONS
# =============================================================================
check_bf_status()              # Show full environment status
verify_bf_installation()       # Check all packages with versions
detect_cuda_version()          # Just check CUDA
get_bf_env_info()              # Get device (CPU/GPU) and environment info
check_bf_available(silent = TRUE)  # Quick availability check

# Testing without Python (mock mode)
Sys.setenv(RCTBP_MOCK_BF = "TRUE")
# BayesFlow calls will return mock samples

# GPU Memory Management
# For limited GPU memory, reduce batch_size:
result <- power_analysis(conditions, bf_args = list(batch_size = 64))

# Configure PyTorch memory allocator (before running):
Sys.setenv(PYTORCH_CUDA_ALLOC_CONF = "expandable_segments:True")

# Manual cleanup between analyses if needed:
clear_gpu_memory()  # Clears Python gc + CUDA cache + R gc
```

**Note**: Parameter names vary by model. Use `design@par_names_inference` to discover available `target_params` and `design@par_names_sim` for simulation arguments.

### Sequential Design with Look-Dependent Boundaries

```r
# Create design
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2"
)

# Create conditions with sequential analysis and boundary functions
# Use link() to co-vary analysis_at with n_total
conditions <- build_conditions(
  design = design,
  crossed = list(
    link(
      n_total = c(100, 200),
      # analysis_at accepts:
      # - Proportions (0, 1]: e.g., c(0.5, 1) means 50% and 100% of n_total
      # - Absolute integers: e.g., c(50, 100) for n_total=100
      # n_total is auto-appended if not the last element
      analysis_at = list(c(50, 100), c(100, 200))  # Absolute sample sizes
      # Or equivalently: list(c(0.5), c(0.5))  # Proportions (n_total auto-appended)
    )
  ),
  constant = list(
    b_arm_treat = 0.5,
    p_alloc = list(c(0.5, 0.5)),
    intercept = 0, b_covariate = 0.3, sigma = 1,
    thr_fx_eff = 0.2, thr_fx_fut = 0,
    # Sequential analysis settings (Bayesian: use threshold parameter)
    thr_dec_eff = boundary_obf(threshold = 0.95), # OBF shape ending at 0.95
    thr_dec_fut = boundary_linear(0.30, 0.50)     # Linear from 0.3 to 0.5
  )
)

# Available boundary functions (specify either alpha OR threshold):
#
# - boundary_obf(alpha=0.025)      Frequentist: OBF alpha-spending
# - boundary_obf(threshold=0.95)   Bayesian: OBF shape ending at threshold
# - boundary_pocock(alpha=0.025)   Frequentist: Pocock alpha-spending
# - boundary_pocock(threshold=0.95) Bayesian: constant threshold at all looks
# - boundary_hsd(alpha, gamma)     Hwang-Shih-DeCani family (requires gsDesign)
# - boundary_linear(start, end)    Linear interpolation between thresholds
# - boundary_power(base, rho)      Power family (rho=2: OBF-like shape)
# - boundary_constant(threshold)   Simple fixed threshold

# Post-hoc boundary comparison (no re-simulation needed)
result <- power_analysis(conditions, n_sims = 500)
comparison <- compare_boundaries(result, list(
  "Fixed 0.95" = list(success = 0.95, futility = 0.50),
  "OBF-shape" = list(success = boundary_obf(threshold = 0.95), futility = 0.50),
  "Stringent" = list(success = 0.99, futility = 0.70)
))

# Re-analyze with new boundaries (returns modified power_analysis object)
result_obf <- resummarize_boundaries(result,
  thr_dec_eff = boundary_obf(threshold = 0.95),
  thr_dec_fut = boundary_linear(0.30, 0.50)
)
```

### Trial Types

The `trial_type` parameter in `build_design()` determines the fundamental structure of your trial:

| Type | Description | Required in `build_conditions()` |
|------|-------------|----------------------------------|
| `fixed` (default) | Single final analysis | None |
| `group_sequential` | Multiple looks with stopping rules | `analysis_at` |
| `adaptive` | Parameter modification between looks (RAR, SSR) | `analysis_at`, specific params |

```r
# Fixed trial (default - simplest)
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2"
  # trial_type = "fixed" is default
)

# Group sequential trial
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2",
  trial_type = "group_sequential"
)
# Requires analysis_at in build_conditions()

# Adaptive trial (future - currently errors)
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2",
  trial_type = "adaptive"
)
# Will support RAR, SSR when implemented
```

**Note:** The `adaptive` boolean in `build_conditions()` is deprecated. Use `trial_type` in `build_design()` instead.

### Pareto Optimization for Design Search

The package provides Pareto-based Bayesian optimization with three specialized wrappers:

```r
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2"
)

# =============================================================================
# WRAPPER 1: Power vs Sample Size (fixed effect)
# =============================================================================
# Find Pareto front of power vs n_total for a fixed treatment effect
result <- optimize_power_n(
  design = design,
  power_metric = "pwr_eff",
  n_range = c(50, 500),
  effect_size = 0.3,              # Fixed effect size
  constant = list(
    thr_dec_eff = 0.975, thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0,
    p_alloc = list(c(0.5, 0.5)),
    intercept = 0, b_covariate = 0.3, sigma = 1
  ),
  n_sims = c(500, 2000),          # Progressive fidelity
  evals_per_step = c(30, 20),
  n_cores = 4
)

# Access results
result@pareto_front      # All Pareto-optimal points
result@selected_design   # Knee point (auto-selected)
result@archive           # All evaluations
plot(result)             # Pareto front visualization

# =============================================================================
# WRAPPER 2: Power vs Effect Size (fixed sample size)
# =============================================================================
# Find minimum detectable effect at a given sample size
result <- optimize_power_effect(
  design = design,
  power_metric = "pwr_eff",
  effect_range = c(0.1, 0.8),
  n_total = 200,                  # Fixed sample size
  constant = list(...),
  n_sims = 500,
  n_cores = 4
)

# =============================================================================
# WRAPPER 3: Effect Size vs Sample Size (fixed power target)
# =============================================================================
# Find (effect, n) pairs that achieve target power
result <- optimize_effect_n(
  design = design,
  power_target = 0.80,            # Target power level
  power_metric = "pwr_eff",
  n_range = c(50, 500),
  effect_range = c(0.1, 0.8),
  constant = list(...),
  n_sims = 500,
  n_cores = 4
)

# =============================================================================
# GENERIC CORE FUNCTION
# =============================================================================
# For custom two-objective optimization
result <- pareto_optimize(
  design = design,
  objectives = list(pwr_eff = "maximize", n_total = "minimize"),
  search = list(n_total = c(50, 500), thr_dec_eff = c(0.90, 0.99)),
  constant = list(...),
  n_sims = 500,
  n_cores = 4
)
```

**Key optimization features:**
- **Pareto front**: Returns set of non-dominated designs trading off objectives
- **Knee point selection**: Automatic selection via utopia distance (normalized)
- **Progressive fidelity**: Vector `n_sims` for cost-efficient optimization
- **Simplex search**: Use `search_p_alloc()` and `search_looks()` for allocation/timing
- **Constraint support**: Filter Pareto front by minimum power threshold

## File Organization

### Core Classes (`R/class_*.R`)

| File | Purpose |
|------|---------|
| `R/class_design.R` | Merged design class (includes model properties, dual backend) |
| `R/class_model.R` | Legacy model class (backward compatibility, see note below) |
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
- `get_batch_field_map(model_type)` - Field mapping registry for batch prep ("ancova", "binary", "survival")
- `prepare_data_list_as_batch_bf()` - Pre-allocated O(n) batch preparation (vs O(n²) rbind pattern)

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
| `show_target_params(model_name)` | `R/required_fn_args.R` | Show available target parameters for a model |
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

### Deprecation Policy

**Package not yet released**: Since the package has not been released to CRAN yet, deprecated functions are removed rather than issuing deprecation warnings. Legacy code is archived in `R/legacy/` for reference.

**When package is released**: Add `lifecycle::deprecate_warn()` calls to any remaining legacy functions before removal.

## Development Practices

### Code Patterns (Verified 2025-11-28)

See [`dev/11_code_consistency_review.md`](dev/11_code_consistency_review.md) for full details.

#### File Structure
```r
# =============================================================================
# FILE TITLE IN CAPS
# =============================================================================
# Brief description of file purpose.

# Major section code...

# =============================================================================
# NEXT MAJOR SECTION
# =============================================================================
```

#### Naming Conventions

| Element | Convention | Examples |
|---------|------------|----------|
| S7 classes | `rctbp_*` prefix | `rctbp_design`, `rctbp_conditions` |
| Power columns | `pwr_*` prefix | `pwr_eff`, `pwr_fut` |
| Probability columns | `pr_*` prefix | `pr_eff`, `pr_fut` |
| Decision columns | `dec_*` prefix | `dec_eff`, `dec_fut` |
| Effect threshold params | `thr_fx_*` | `thr_fx_eff`, `thr_fx_fut` |
| Decision threshold params | `thr_dec_*` | `thr_dec_eff`, `thr_dec_fut` |
| Standard error cols | `se_*` prefix | `se_pwr_eff`, `se_pr_fut` |
| Efficacy suffix | `_eff` | `pwr_eff`, `pr_eff`, `thr_fx_eff` |
| Futility suffix | `_fut` | `pwr_fut`, `pr_fut`, `thr_fx_fut` |
| Functions | snake_case | `build_design`, `power_analysis` |
| Parameters | snake_case | `n_total`, `thr_dec_eff` |

#### Error Handling Pattern
```r
cli::cli_abort(c(
  "Main error message",
  "x" = "What went wrong: {.val {bad_value}}",
  "i" = "Helpful hint or suggestion"
))
```

#### S7 Class Pattern
```r
rctbp_classname <- S7::new_class(
  "rctbp_classname",
  properties = list(
    prop1 = S7::new_property(S7::class_numeric, default = 0),
    prop2 = S7::class_character | NULL
  ),
  validator = function(self) {
    if (invalid) cli::cli_abort(c("message", "x" = "...", "i" = "..."))
    NULL
  }
)
```

### File Organization

- **All R source files must be in `/R` root** - no subfolders allowed in `/R` directory
- Save new development documents into `dev/`

### Documentation Guidelines
- **Always update documentations directly in the .R file's roxygen documentation**
- **CRITICAL**: After updating roxygen comments, run `devtools::document()` to regenerate .Rd files
- **Never manually edit .Rd files** in the `man/` directory - they are auto-generated
- Only edit the README.Rmd file, the README.md file will get build from it later.

### Documentation Best Practices
- **Concise Example Guidelines**:
  - Examples in the documentation of a function need to be as short and concise as possible
  - Only one variant of the function should be demonstrated if the function has extended run time

### R CMD Check Best Practices

#### 1. Avoiding Documentation Mismatches
**Problem**: R CMD check fails when default parameter values in code don't match roxygen documentation.

**Solution**: Always ensure roxygen `@param` documentation matches the actual default values in function signatures.

**Example Fix**:
```r
# BAD: Code has default = 0.975 but docs say 0.95
#' @param thr_dec_eff Probability threshold for efficacy (default 0.95)
my_function <- function(thr_dec_eff = 0.975) { ... }

# GOOD: Documentation matches code
#' @param thr_dec_eff Probability threshold for efficacy (default 0.975)
my_function <- function(thr_dec_eff = 0.975) { ... }
```

### Documentation Conventions
- Don't use \code{\link{function_name}} in roxygen docs. Use [functionname()] instead.

### Code Writing Guidelines

#### Required Practices
- **Roxygen2 documentation** for all exported functions
- **Explicit namespacing**: Use `package::function()` (e.g., `dplyr::mutate()`)
- **CLI messaging**: Use `cli` package (`cli_alert_info`, `cli_abort`, `cli_inform`)
- **Pipe operator**: Base R `|>` (not magrittr `%>%`)
- **Parameter validation**: Extensive validation with informative errors at function start

#### Output and Messaging
- Always wrap argument names in quotes when displaying them in printed output such as in stop(), warning(), message(), or documentation. This improves clarity and avoids confusion with actual values or natural language.

#### CLI Efficiency Best Practices

**IMPORTANT**: For **reports and multi-line output**, collect ALL content first and output with a **single cat() call** to create one unified output block. For **status messages**, individual CLI calls are fine.

**Problem** (Reports/Multi-line Output Only):
- Calling CLI functions (even `cli::cli_bullets()`) creates separate console messages for each element
- Multiple `message()` or `cat()` calls create separate output blocks
- This fragments output in rendered documents and looks unprofessional

**Solution**: Build complete output as a character vector, collapse to a single string, and output once with `cat()`.

**Example - Report Functions (Multi-Section Output)**:

```r
# ❌ INCORRECT - Multiple output calls create separate blocks
message(paste(section1_lines, collapse = "\n"))  # Block 1
message(paste(section2_lines, collapse = "\n"))  # Block 2
message(paste(section3_lines, collapse = "\n"))  # Block 3

# ❌ ALSO INCORRECT - CLI functions create many separate messages
cli::cli_h2("Section Header")
for (item in items) {
  cli::cli_bullets(c("*" = paste0(item$name, ": ", item$value)))
}

# ✅ CORRECT - Build ALL sections, then output once
all_output <- character()

# Section 1
all_output <- c(all_output, "\n── Section 1 ──", "")
for (item in section1_items) {
  all_output <- c(all_output, paste0("  • ", item$name))
  all_output <- c(all_output, paste0("    ", item$description))
  all_output <- c(all_output, "")
}

# Section 2
all_output <- c(all_output, "\n── Section 2 ──", "")
for (item in section2_items) {
  all_output <- c(all_output, paste0("  • ", item$name))
  all_output <- c(all_output, paste0("    ", item$value))
}

# Output everything as ONE block
cat(paste(all_output, collapse = "\n"), "\n")
```

**Example - Status Messages (Individual Calls OK)**:

```r
# ✅ CORRECT - Status messages can be individual calls
cli::cli_alert_info("Starting {analysis_type} interpretation...")
# ... do work ...
cli::cli_alert_success("Analysis complete")
cli::cli_alert_info("Tokens used: {tokens$total}")
```

**Key Principles**:
1. **Reports/Print Methods**: Build helper functions that return text, collect all sections, single `cat()` call
2. **Status Messages**: Individual CLI calls are fine and encouraged for clarity
3. **Rule of thumb**: If output has multiple sections or will be rendered in docs, use single-call pattern
4. Use `cat()` for stdout (like reports), `message()` for stderr (like warnings)

**When to Apply Single-Call Pattern**:
- Report generation functions (`build_report.{class}()`)
- Parameter display functions (`show_interpret_args()`)
- Print methods for S3 classes (`print.chat_session()`, `print.variable_labels()`)
- Any function with multiple sections or many lines
- Functions whose output will be rendered in vignettes or documentation

**When Individual CLI Calls are OK**:
- Status messages (`cli::cli_alert_info()`, `cli::cli_alert_success()`)
- Progress updates during processing
- Interactive prompts
- Warnings (`cli::cli_warn()`)
- Error messages (`cli::cli_abort()`)
- Single informational messages
