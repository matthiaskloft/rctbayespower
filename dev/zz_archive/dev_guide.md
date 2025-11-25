# rctbayespower Developer Guide

**Last Updated:** 2025-11-24
**Purpose:** Comprehensive guide for package development, architecture, and planned features

---

## Table of Contents

1. [Package Architecture](#1-package-architecture)
2. [S7 Class System](#2-s7-class-system)
3. [User Workflow](#3-user-workflow)
4. [Development Guidelines](#4-development-guidelines)
5. [Feature Roadmap](#5-feature-roadmap)
6. [Architecture Decisions](#6-architecture-decisions)
7. [Testing & CI](#7-testing--ci)
8. [Quick Reference](#8-quick-reference)

---

## 1. Package Architecture

### Core Philosophy

`rctbayespower` is built around a **pipeline architecture** where users progressively construct objects:

```
Model → Design → Conditions → Power Analysis → Results
```

**Design Principles:**
- **Progressive Enhancement**: Each object inherits from and extends the previous
- **Validation at Construction**: S7 validators ensure correctness at each stage
- **Explicit Over Implicit**: Users specify what they want rather than relying on defaults
- **Reproducibility**: All specifications stored in objects for full reproducibility
- **Parallelization-Friendly**: Objects designed to serialize well for parallel execution

### File Organization

| Directory | Purpose |
|-----------|---------|
| `R/class_*.R` | S7 class definitions (model, design, conditions, power_analysis) |
| `R/models_*.R` | Model builders (e.g., `models_ancova.R`) |
| `R/simulate_*.R` | Simulation engine |
| `R/compute_*.R` | Metric computation |
| `R/plot_*.R` | Visualization |
| `R/MCSE.R` | Monte Carlo standard errors |
| `data-raw/` | Scripts to build internal data |
| `dev/` | Development documentation (this guide) |

### Key Components

#### 1. Data Simulation Functions
- Embedded in `rctbp_model` objects
- Generate trial data for given parameters
- Required signature: Must accept `n_total`, `p_alloc`, plus model-specific parameters

#### 2. BRMS Model Templates
- Pre-compiled and stored in `rctbp_model@brms_model`
- Compiled once with `chains=0` to avoid recompilation during simulations
- **Critical for performance**: Compilation takes 30-60 seconds

#### 3. Power Metrics (compute_measures.R)
- `success_prob`: P(parameter > threshold_success)
- `futility_prob`: P(parameter < threshold_futility)
- `power_success`: 1 if success_prob >= p_sig_success, else 0
- `power_futility`: 1 if futility_prob >= p_sig_futility, else 0
- Union measures when multiple parameters analyzed

---

## 2. S7 Class System

### Why S7?

- **Property Access**: Clean `@` syntax (e.g., `model@data_simulation_fn`)
- **Validation**: Built-in validators ensure object integrity
- **Composition**: Easy nesting (design contains model, conditions contain design)
- **Modern R**: S7 is the recommended OOP system for new packages

### Class Hierarchy

```
rctbp_model
    ↓ (contained by)
rctbp_design
    ↓ (contained by)
rctbp_conditions
    ↓ (contained by)
rctbp_power_analysis
```

### Class Definitions

#### `rctbp_model` (class_model.R)
**Purpose**: Encapsulates everything needed to simulate data and fit a Bayesian model.

| Property | Type | Description |
|----------|------|-------------|
| `data_simulation_fn` | function | Generates trial data |
| `brms_model` | brmsfit | Pre-compiled template |
| `predefined_model` | character/NULL | Name if predefined |
| `model_name` | character | Descriptive name |
| `n_endpoints` | numeric | Number of outcomes |
| `endpoint_types` | character | Types (continuous, binary) |
| `n_arms` | numeric | Treatment arms |
| `parameter_names_sim_fn` | computed | Auto-extracted |
| `parameter_names_brms` | computed | Auto-extracted |

#### `rctbp_design` (class_design.R)
**Purpose**: Combines a model with analysis decision criteria.

| Property | Type | Description |
|----------|------|-------------|
| `model` | rctbp_model | The statistical model |
| `target_params` | character | Parameters to analyze |
| `p_sig_success` | numeric | Probability threshold for success |
| `p_sig_futility` | numeric | Probability threshold for futility |
| `design_name` | character/NULL | Optional name |

#### `rctbp_conditions` (class_conditions.R)
**Purpose**: Generates parameter combinations for power analysis grid.

| Property | Type | Description |
|----------|------|-------------|
| `design` | rctbp_design | The analysis design |
| `conditions_grid` | data.frame | All condition combinations |
| `condition_arguments` | list | Argument lists per condition |
| `condition_values` | list | User-specified varying params |
| `static_values` | list | User-specified constant params |

#### `rctbp_power_analysis` (class_power_analysis.R)
**Purpose**: Configures and executes power analysis simulations.

| Property | Type | Description |
|----------|------|-------------|
| `conditions` | rctbp_conditions | Conditions to evaluate |
| `n_sims` | numeric | Simulations per condition |
| `n_cores` | numeric | Parallel cores |
| `brms_args` | list | Arguments passed to brms |
| `design_prior` | various | Optional design prior |
| `summarized_results` | data.frame | Aggregated results |
| `raw_results` | data.frame | Individual simulation results |
| `elapsed_time` | numeric | Runtime in minutes |

### S7 Package Integration

In `R/zzz.R`:
```r
.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
```

### Common S7 Patterns

```r
# Property access
model@data_simulation_fn
design@model@brms_model

# Method definition
S7::method(print, rctbp_model) <- function(x, ...) { ... }

# Validators
validator = function(self) {
  if (nchar(self@foo) == 0) "@foo must be non-empty"
  NULL  # Return NULL if valid
}
```

---

## 3. User Workflow

### Standard 4-Step Workflow

```r
# Step 1: Define the model
model <- build_model(predefined_model = "ancova_cont_2arms")

# Step 2: Specify analysis design
design <- build_design(
  model = model,
  target_params = "b_arms_treat",
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)

# Step 3: Create conditions grid
conditions <- build_conditions(
  design = design,
  condition_values = list(
    n_total = c(100, 150, 200),
    b_arms_treat = c(0.2, 0.3, 0.5)
  ),
  static_values = list(
    p_alloc = list(c(0.5, 0.5)),
    thresholds_success = 0.2,
    thresholds_futility = 0,
    intercept = 0,
    b_covariate = 0.3,
    sigma = 1
  )
)

# Step 4: Run power analysis
result <- power_analysis(
  conditions = conditions,
  n_sims = 100,
  n_cores = 4,
  run = TRUE
)

# Visualize
plot(result)

# Access results
result@summarized_results
result@raw_results
```

### Alternative: Build-then-Run

```r
# Create configuration without running
power_config <- power_analysis(conditions, n_sims = 100, run = FALSE)
print(power_config)

# Execute when ready
power_config <- run(power_config)
```

### Available Functions

| Function | Purpose | Returns |
|----------|---------|---------|
| `build_model()` | Create/retrieve model | `rctbp_model` |
| `build_design()` | Configure analysis | `rctbp_design` |
| `build_conditions()` | Generate grid | `rctbp_conditions` |
| `power_analysis()` | Configure/run analysis | `rctbp_power_analysis` |
| `run()` | Execute analysis | `rctbp_power_analysis` |
| `plot()` | Visualize results | plotly object |
| `list_predefined_models()` | List available models | character |
| `required_fn_args()` | Show required params | character |

---

## 4. Development Guidelines

### Code Style Requirements

- **Roxygen2**: Document all exported functions
- **Explicit namespacing**: Use `package::function()` (e.g., `dplyr::mutate()`)
- **CLI messaging**: Use `cli` package (`cli_alert_info`, `cli_abort`)
- **Pipe operator**: Base R `|>` (not magrittr `%>%`)
- **Parameter validation**: Extensive validation at function start

### Documentation Standards

**Critical**: Always update documentation in `.R` files, then run `devtools::document()`.
**Never** manually edit `.Rd` files in `man/`.

```r
#' Title in Sentence Case
#'
#' Description of what the function does.
#'
#' @param x Description of parameter x
#' @return Description of return value
#' @export
#' @examples
#' function_name(1, 2)
function_name <- function(x) { ... }
```

**Link syntax**: Use `[function_name()]` not `\code{\link{function_name}}`

### R CMD Check Requirements

- **0 ERRORs, 0 WARNINGs, 0 NOTEs**
- Ensure default values in code match roxygen documentation
- Declare global variables to avoid "no visible binding" notes

### CLI Output Best Practices

**For reports/multi-line output**: Build ALL content first, output with single `cat()`:

```r
# BAD: Multiple outputs
cli::cli_h2("Section 1")
cli::cli_bullets(c("*" = "Item 1"))

# GOOD: Single output
all_output <- character()
all_output <- c(all_output, "\n-- Section 1 --", "  * Item 1")
cat(paste(all_output, collapse = "\n"), "\n")
```

**For status messages**: Individual CLI calls are fine:
```r
cli::cli_alert_info("Starting analysis...")
cli::cli_alert_success("Complete")
```

### Development Workflow

```r
devtools::load_all()     # Load package (Ctrl+Shift+L)
devtools::document()     # Generate docs (Ctrl+Shift+D)
devtools::test()         # Run tests (Ctrl+Shift+T)
devtools::check()        # R CMD check (Ctrl+Shift+E)
```

---

## 5. Feature Roadmap

### Current Status (November 2025)

**Implemented:**
- Core 4-step workflow (model → design → conditions → power_analysis)
- ANCOVA continuous outcome model
- Design prior integration
- Parallelization with model caching
- Comprehensive plotting

**Not Implemented:**
- Interim analysis / group sequential designs
- Binary outcomes
- Survival outcomes
- Sample size re-estimation
- Adaptive randomization
- Dose finding

### Priority 1: Interim Analysis

**Status**: Detailed implementation plan exists (`interim_analysis_implementation_plan.md`)
**Estimated Time**: 16-23 hours

**Key Features:**
- `analysis_at` property in `rctbp_design` for interim timepoints
- `adaptive` flag for parameter modification between looks
- Custom interim decision functions
- Continue simulation to `n_total` even after early stopping
- Rich results: stopping probabilities, expected sample size

**Target API:**
```r
design <- build_design(
  model = model,
  target_params = "b_arms_treat",
  p_sig_success = 0.975,
  p_sig_futility = 0.5,
  analysis_at = c(100, 150),  # Interim at 100, 150; final at n_total
  adaptive = FALSE,
  interim_function = interim_futility_only(0.90)
)
```

### Priority 2: Binary & Survival Outcomes

**Status**: Not started
**Estimated Time**: 12-16 hours each

Required for:
- Most Phase II/III trials (response rates, mortality)
- Oncology trials (PFS, OS)
- Cardiovascular trials (MACE)

### Priority 3: Backend Abstraction (NPE Support)

**Status**: Design plan exists (`backend_abstraction_refactoring_plan.md`)
**Estimated Time**: 20-25 hours

**Key Insight**: Convert all posterior samples to `rvars` (posterior package) for backend-agnostic computation.

**Design:**
- `backend` property: "brms" (default) or "npe"
- `estimation_model`: Holds either brmsfit or keras model
- NPE processes multiple simulations in single forward pass
- All downstream computations identical regardless of backend

---

## 6. Architecture Decisions

### Model/Design Separation (Keep Separate)

**Decision**: Do NOT merge `rctbp_model` and `rctbp_design` classes.

**Rationale:**
1. **Model compilation is expensive** (2+ minutes)
   - Separation allows reusing compiled models across designs
   - Merging would require recompilation for each design

2. **Clean separation of concerns**
   - Model = WHAT can be simulated (data generation, Stan model)
   - Design = HOW to analyze it (which params, thresholds)

3. **Future flexibility**
   - Same model, different analysis strategies
   - Analyze different parameters from same model

4. **Architecture principle**: Single Responsibility Principle

**Alternative (implemented)**: Allow string model names in `build_design()`:
```r
design <- build_design(model = "ancova_cont_2arms", ...)
```

### Parallelization Strategy

**Challenge**: S7 objects don't serialize well to parallel workers.

**Solution**: Extract needed information into plain lists before parallelization:

```r
design_components <- list(
  target_params = design@target_params,
  model_data_simulation_fn = model@data_simulation_fn,
  model_brms_model = model@brms_model,
  ...
)
```

Worker functions handle both S7 objects (sequential) and plain lists (parallel).

---

## 7. Testing & CI

### Current Status

**Test suite**: Placeholder files only (0% implemented)

### Parallel Testing Support

Enable in `.Renviron`:
```
PARALLEL_TESTS=true
```

Configuration:
- Uses `available cores - 2` for test execution
- Minimum 1 core safeguard
- testthat >= 3.0.0 required

### Testing Strategy (Recommended)

1. **Unit tests**: Each class constructor, validators
2. **Integration tests**: Full workflow
3. **Edge cases**: Single condition, missing params, convergence failures
4. **Backward compatibility**: Old code still works

---

## 8. Quick Reference

### Predefined Models

| Model Name | Description |
|------------|-------------|
| `ancova_cont_2arms` | 2-arm ANCOVA, continuous outcome |

### Required Arguments by Model

Use `required_fn_args(design)` to see required simulation parameters.

For ANCOVA 2-arm:
- `n_total`: Total sample size
- `p_alloc`: Allocation ratios (e.g., `list(c(0.5, 0.5))`)
- `intercept`: Baseline mean
- `b_arms_treat`: Treatment effect
- `b_covariate`: Covariate effect
- `sigma`: Residual SD
- `thresholds_success`: ROPE boundary for success
- `thresholds_futility`: ROPE boundary for futility

### Common Issues & Solutions

| Issue | Solution |
|-------|----------|
| "Pre-defined model not found" | Use `predefined_model = "ancova_cont_2arms"` |
| S7 property access | Use `@` not `$` (e.g., `design@model`) |
| Parallel crashes | Set `n_cores = 1` to debug |
| Documentation mismatch | Run `devtools::document()` after changes |

### Key Files to Know

| File | Purpose |
|------|---------|
| `R/class_model.R` | Model class definition |
| `R/class_design.R` | Design class definition |
| `R/class_conditions.R` | Conditions class definition |
| `R/class_power_analysis.R` | Power analysis + run() method |
| `R/simulate_single_run.R` | Core simulation engine |
| `R/compute_measures.R` | Metric computation |
| `data-raw/build_predefined_models.R` | Creates internal model data |

---

## Archived Documentation Files

The following files in `dev/` are superseded by this guide:

| File | Status | Notes |
|------|--------|-------|
| `package_architecture.md` | Integrated | Full architecture details |
| `S7_guide.md` | Integrated | S7 patterns |
| `r_package_development_guidelines.md` | Integrated | Dev guidelines |
| `workflow_summary.md` | Integrated | Workflow examples |
| `development_status.md` | Integrated | Status tracking |
| `interim_analysis_implementation_plan.md` | **Active** | Detailed implementation plan |
| `adaptive_trials_*.md` | **Active** | Capability analysis & roadmap |
| `backend_abstraction_refactoring_plan.md` | **Active** | NPE integration plan |
| `model_design_merge_*.md` | Integrated | Architecture decision |
| `QUICK_REFERENCE.md` | Integrated | Common fixes |
| `roxygen_*.md` | **Archived** | Historical fixes (completed) |
| `dev_todos.md` | **Active** | Task tracking |
| `parallel_testing_setup.md` | Integrated | Testing config |

---

## Version History

| Date | Changes |
|------|---------|
| 2025-11-24 | Initial synthesized guide created |
