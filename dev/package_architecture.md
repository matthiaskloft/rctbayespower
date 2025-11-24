# Package Architecture: rctbayespower

**Last Updated:** 2025-10-15
**Purpose:** This document provides a comprehensive overview of the package architecture, function relationships, and intended user workflow for co-developers.

---

## Table of Contents

1. [Overview](#overview)
2. [Core Philosophy](#core-philosophy)
3. [Object-Oriented System (S7 Classes)](#object-oriented-system-s7-classes)
4. [User Workflow](#user-workflow)
5. [Function Hierarchy](#function-hierarchy)
6. [Key Components](#key-components)
7. [Parallelization Strategy](#parallelization-strategy)
8. [Extension Points](#extension-points)

---

## Overview

`rctbayespower` is built around a **pipeline architecture** where users progressively construct objects that encapsulate increasing levels of information:

```
Model → Design → Conditions → Power Analysis → Results
```

Each stage builds upon the previous one, maintaining all prior information through composition while adding new specifications.

---

## Core Philosophy

### Design Principles

1. **Progressive Enhancement**: Each object inherits from and extends the previous one
2. **Validation at Construction**: S7 validators ensure correctness at each stage
3. **Explicit Over Implicit**: Users specify what they want rather than relying on defaults
4. **Reproducibility**: All specifications are stored in objects for full reproducibility
5. **Parallelization-Friendly**: Objects are designed to serialize well for parallel execution

### Why S7?

The package uses S7 (not S3 or S4) for its class system because:

- **Property Access**: Clean `@` syntax for accessing properties (`model@data_simulation_fn`)
- **Validation**: Built-in validators ensure object integrity
- **Composition**: Easy to nest objects (design contains model, conditions contain design)
- **Modern R**: S7 is the recommended OOP system for new packages

---

## Object-Oriented System (S7 Classes)

### Class Hierarchy

The package defines four core S7 classes that form a compositional hierarchy:

```
rctbp_model
    ↓ (contained by)
rctbp_design
    ↓ (contained by)
rctbp_conditions
    ↓ (contained by)
rctbp_power_analysis
```

### 1. `rctbp_model` (class_model.R)

**Purpose**: Encapsulates everything needed to simulate data and fit a Bayesian model.

**Properties**:
- `data_simulation_fn`: Function to generate trial data
- `brms_model`: Pre-compiled brmsfit object (template)
- `predefined_model`: Name of predefined model (if applicable)
- `model_name`: Descriptive name
- `n_endpoints`: Number of outcome variables
- `endpoint_types`: Types of outcomes (continuous, binary, count)
- `n_arms`: Number of treatment arms
- `n_repeated_measures`: Number of time points (NULL for single timepoint)
- `parameter_names_sim_fn`: Auto-extracted from simulation function
- `parameter_names_brms`: Auto-extracted from brms model

**Constructor**: `build_model(predefined_model)` or direct construction for custom models

**Key Feature**: The brms model is pre-compiled (chains=0) to avoid recompilation during simulations.

**File**: `R/class_model.R`

---

### 2. `rctbp_design` (class_design.R)

**Purpose**: Combines a model with analysis decision criteria (thresholds and probability cutoffs).

**Properties**:
- `model`: An rctbp_model object
- `target_params`: Which parameters to analyze (e.g., "b_arms_treat")
- `thresholds_success`: ROPE boundaries for clinically meaningful effects
- `thresholds_futility`: ROPE boundaries for ineffective treatments
- `p_sig_success`: Probability threshold for declaring success (e.g., 0.975)
- `p_sig_futility`: Probability threshold for declaring futility (e.g., 0.5)
- `n_interim_analyses`: Number of planned interim looks
- `interim_function`: Optional function for adaptive designs
- `design_name`: Descriptive name

**Constructor**: `build_design(model, target_params, thresholds_success, ...)`

**Key Feature**: Validates that target_params exist in the model's parameter names.

**File**: `R/class_design.R`

---

### 3. `rctbp_conditions` (class_conditions.R)

**Purpose**: Generates all parameter combinations for a power analysis grid.

**Properties**:
- `design`: An rctbp_design object
- `conditions_grid`: Data frame of all condition combinations
- `condition_arguments`: List of argument lists for each condition
- `condition_values`: User-specified varying parameters
- `static_values`: User-specified constant parameters
- `model`: Property that accesses design@model (convenience)

**Constructor**: `build_conditions(design, condition_values, static_values)`

**Key Feature**:
- Creates expanded grid from `condition_values` (e.g., varying sample sizes and effect sizes)
- Merges with `static_values` to create complete argument lists
- Separates arguments into `sim_args` and `interim_args` based on function signatures

**File**: `R/class_conditions.R`

---

### 4. `rctbp_power_analysis` (class_power_analysis.R)

**Purpose**: Configures and executes power analysis simulations.

**Properties**:
- `conditions`: An rctbp_conditions object
- `n_sims`: Number of simulations per condition
- `n_cores`: Number of parallel cores to use
- `verbose`: Whether to show progress
- `brms_args`: Arguments passed to brms::brm() during fitting
- `design_prior`: Optional prior for design parameters
- `design`: Property that accesses conditions@design (convenience)
- `model`: Property that accesses conditions@model (convenience)
- `summarized_results`: Data frame of aggregated results (filled after run)
- `raw_results`: Data frame of all individual simulation results (filled after run)
- `elapsed_time`: Runtime in minutes (filled after run)

**Constructor**: `power_analysis(conditions, n_sims, n_cores, ...)` with `run=TRUE` by default

**Execution**: `run(power_object)` - S7 generic method

**Key Features**:
- Handles parallelization automatically
- Preserves all simulation details in raw_results
- Computes aggregated power curves in summarized_results

**File**: `R/class_power_analysis.R`

---

## User Workflow

### Standard Workflow (4 Steps)

```r
# Step 1: Define the model
model <- build_model(predefined_model = "ancova_cont_2arms")

# Step 2: Specify analysis design
design <- build_design(
  model = model,
  target_params = "b_arms_treat",
  thresholds_success = 0.2,
  thresholds_futility = 0,
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
    intercept = 0,
    b_covariate = 0.3,
    sigma = 1
  )
)

# Step 4: Run power analysis
power_config <- power_analysis(
  conditions = conditions,
  n_sims = 100,
  n_cores = 4,
  run = TRUE  # Executes immediately
)

# Visualize results
plot(power_config)

# Access results
power_config@summarized_results  # Power curves
power_config@raw_results         # Individual simulations
```

### Alternative: Build-then-Run Workflow

```r
# Create configuration without running
power_config <- power_analysis(
  conditions = conditions,
  n_sims = 100,
  n_cores = 4,
  run = FALSE  # Don't execute yet
)

# Inspect configuration
print(power_config)

# Execute when ready
power_config <- run(power_config)
```

---

## Function Hierarchy

### Top-Level User Functions

These are the main entry points for users:

| Function | Purpose | Returns | File |
|----------|---------|---------|------|
| `build_model()` | Create or retrieve model | `rctbp_model` | class_model.R |
| `build_design()` | Configure analysis criteria | `rctbp_design` | class_design.R |
| `build_conditions()` | Generate condition grid | `rctbp_conditions` | class_conditions.R |
| `power_analysis()` | Configure/run power analysis | `rctbp_power_analysis` | class_power_analysis.R |
| `run()` | Execute power analysis | `rctbp_power_analysis` (with results) | class_power_analysis.R |
| `plot()` | Visualize results | plotly object | plot_power_analysis.R |

### Predefined Model Builders

Convenience functions for common models:

| Function | Purpose | File |
|----------|---------|------|
| `build_model_ancova()` | General ANCOVA model | models_ancova.R |
| `build_model_ancova_cont_2arms()` | 2-arm ANCOVA with defaults | models_ancova.R |
| `build_model_ancova_cont_3arms()` | 3-arm ANCOVA with defaults | models_ancova.R |

### Utility Functions

| Function | Purpose | File |
|----------|---------|------|
| `list_predefined_models()` | List available models | class_model.R |
| `get_predefined_model()` | Retrieve specific model | class_model.R |
| `required_fn_args()` | Extract required parameters | required_fn_args.R |

### Core Simulation Engine

| Function | Purpose | File |
|----------|---------|------|
| `simulate_single_run()` | Execute one simulation | simulate_single_run.R |
| `compute_measures_brmsfit()` | Extract power metrics from fit | compute_measures.R |
| `summarize_sims()` | Aggregate simulation results | compute_measures.R |

### Statistical Functions

| Function | Purpose | File |
|----------|---------|------|
| `calculate_mcse_power()` | Monte Carlo SE for power | MCSE.R |
| `calculate_mcse_mean()` | Monte Carlo SE for means | MCSE.R |
| `calculate_mcse_integrated_power()` | Monte Carlo SE for integrated power | MCSE.R |

---

## Key Components

### 1. Data Simulation Functions

**Location**: Embedded in `rctbp_model` objects
**Purpose**: Generate trial data for a given set of parameters
**Required Signature**: Must accept `n_total` and `p_alloc` plus model-specific parameters

Example from ANCOVA model:
```r
simulate_data_ancova(
  n_total,           # Total sample size
  n_arms = 2,        # Number of arms
  p_alloc = c(0.5, 0.5),  # Allocation ratios
  intercept = 0,     # Baseline mean
  b_arm_treat = 0.3, # Treatment effect
  b_covariate = 0.2, # Covariate effect
  sigma = 1          # Residual SD
)
```

**Output**: Data frame with columns needed for brms model (e.g., `arm`, `covariate`, `outcome`)

---

### 2. BRMS Model Templates

**Location**: Pre-compiled and stored in `rctbp_model@brms_model`
**Purpose**: Provide compiled Stan code to avoid recompilation during simulations
**Creation**: Compiled once during model building with `chains=0`

**Why Pre-compile?**
- Compilation takes 30-60 seconds
- Running 1000 simulations would require 1000 compilations
- Pre-compilation reduces this to 1 compilation + 1000 fast updates

---

### 3. Condition Arguments

**Location**: Generated by `build_conditions()`
**Structure**: List of lists, one per condition

```r
condition_arguments[[1]]
# $id_cond
# [1] 1
#
# $sim_args
# $sim_args$n_total
# [1] 100
# $sim_args$b_arms_treat
# [1] 0.3
# ...
#
# $interim_args
# NULL (or interim-specific arguments)
```

**Purpose**: Pre-organized argument lists ready for `do.call()` during simulation

---

### 4. Power Metrics

**Computed by**: `compute_measures_brmsfit()`
**Location**: `R/compute_measures.R`

For each parameter, computes:

| Metric | Description |
|--------|-------------|
| `success_prob` | P(parameter > threshold_success) |
| `futility_prob` | P(parameter < threshold_futility) |
| `power_success` | 1 if success_prob ≥ p_sig_success, else 0 |
| `power_futility` | 1 if futility_prob ≥ p_sig_futility, else 0 |
| `median`, `mad` | Posterior median and MAD |
| `mean`, `sd` | Posterior mean and SD |
| `rhat` | Convergence diagnostic |
| `ess_bulk`, `ess_tail` | Effective sample sizes |

**Union Measures**: When multiple parameters are analyzed, also computes combined metrics where ALL parameters must meet criteria.

---

### 5. Result Aggregation

**Function**: `summarize_sims()`
**Purpose**: Average metrics across simulations to estimate power

**Process**:
1. Group by condition (id_cond) and parameter
2. Compute mean of each metric across simulations
3. Calculate Monte Carlo standard errors (MCSE)
4. Return data frame with power estimates and uncertainties

**Output Columns**:
- Power estimates: `power_success`, `power_futility`
- Probability estimates: `prob_success`, `prob_futility`
- Parameter estimates: `median`, `mean`, `mad`, `sd`
- Convergence: `rhat`, `ess_bulk`, `ess_tail`, `conv_rate`
- Standard errors: `*_se` for each metric

---

## Parallelization Strategy

### Design Considerations

**Challenge**: S7 objects don't serialize well to parallel workers.

**Solution**: Extract needed information into plain lists before parallelization.

### Implementation (in run.rctbp_power_analysis)

```r
# 1. Extract design components into plain list
design_components <- list(
  target_params = design@target_params,
  thresholds_success = design@thresholds_success,
  thresholds_futility = design@thresholds_futility,
  p_sig_success = design@p_sig_success,
  p_sig_futility = design@p_sig_futility,
  model_data_simulation_fn = model@data_simulation_fn,
  model_brms_model = model@brms_model
)

# 2. Create cluster
cl <- parallel::makeCluster(n_cores, type = "PSOCK")

# 3. Load packages on workers
parallel::clusterEvalQ(cl, {
  library(brms)
  library(dplyr)
  library(rctbayespower)
  # ... other dependencies
})

# 4. Export objects to workers
parallel::clusterExport(cl,
  varlist = c("condition_args_list", "design_components", "brms_args"),
  envir = environment()
)

# 5. Run simulations in parallel
results <- pbapply::pblapply(cl = cl, seq_along(condition_args_list),
  function(i) {
    simulate_single_run(
      condition_arguments = condition_args_list[[i]],
      id_sim = i,
      design = design_components,  # Plain list, not S7 object
      brms_args = brms_args
    )
  }
)

# 6. Stop cluster
parallel::stopCluster(cl)
```

### Worker-Side Handling

`simulate_single_run()` and `compute_measures_brmsfit()` can handle both:
- S7 objects (for non-parallel execution)
- Plain lists (for parallel execution)

```r
# In simulate_single_run():
if (inherits(design, "rctbp_design")) {
  # S7 object
  data_simulation_fn <- design@model@data_simulation_fn
} else if (is.list(design)) {
  # Plain list from parallel worker
  data_simulation_fn <- design$model_data_simulation_fn
}
```

---

## Extension Points

### Adding New Models

To add a new model type:

1. **Create simulation function**: Must accept `n_total`, `p_alloc`, and model-specific parameters
2. **Define brms formula**: Specify the Bayesian model structure
3. **Set priors**: Use `brms::set_prior()` to define parameter priors
4. **Compile template**: Fit model with `chains=0` to create template
5. **Create rctbp_model**: Use `rctbp_model()` constructor
6. **Add wrapper function**: Create `build_model_*()` convenience function

**Example**: See `build_model_ancova()` in `R/models_ancova.R`

---

### Adding New Outcome Types

Current support: Continuous outcomes (Gaussian family)

To add binary or count outcomes:

1. Update `simulate_data_*` functions to generate appropriate outcomes
2. Modify brms formula to use appropriate family:
   - Binary: `family = bernoulli(link = "logit")`
   - Count: `family = poisson(link = "log")`
3. Update validators in `rctbp_model` if needed
4. Test with `build_conditions()` and `power_analysis()`

---

### Adding New Visualizations

Current plots: Power curves, heatmaps, comparison plots

To add new plot types:

1. Add new type to `plot.rctbp_power_analysis()` type parameter
2. Create `create_*_plot()` helper function in `plot_power_analysis.R`
3. Use plotly for interactive visualizations
4. Access data via `power_config@summarized_results`

**Example structure**:
```r
create_new_plot_type <- function(plot_data, design, ...) {
  # Extract relevant columns
  # Create plotly visualization
  # Return plotly object
}
```

---

### Adding Interim Analysis Support

Currently: Framework exists but not fully implemented

To implement:

1. Define interim decision rules in `interim_function`
2. Update `simulate_single_run()` to call interim function at specified times
3. Add early stopping logic based on interim results
4. Track stopped simulations in results

**Framework already exists**:
- `n_interim_analyses` in rctbp_design
- `interim_function` parameter
- `interim_args` in condition_arguments

---

## File Organization

### Core Classes
- `R/class_model.R` - Model class and functions
- `R/class_design.R` - Design class and functions
- `R/class_conditions.R` - Conditions class and functions
- `R/class_power_analysis.R` - Power analysis class and run() method

### Model Definitions
- `R/models_ancova.R` - ANCOVA model builders
- `data-raw/build_predefined_models.R` - Script to create internal model data

### Simulation Engine
- `R/simulate_single_run.R` - Core simulation function
- `R/compute_measures.R` - Metric computation and aggregation

### Utilities
- `R/required_fn_args.R` - Parameter extraction
- `R/MCSE.R` - Monte Carlo standard errors
- `R/design_prior.R` - Design prior handling
- `R/analytical_power.R` - Analytical power calculations (legacy/reference)

### Visualization
- `R/plot_power_analysis.R` - Plotting methods and helpers

### S3/S7 Infrastructure
- `R/s3_wrappers.R` - S3 method wrappers for compatibility
- `R/S7_helpers.R` - S7 utility functions

### Legacy Code
- `R/legacy/` - Old API (kept for reference, not exported)

---

## Development Workflow

### Making Changes

1. **Modify class definitions**: Edit class_*.R files
2. **Update validators**: Add checks to S7 class validators
3. **Test interactively**: Use `devtools::load_all()` to test changes
4. **Update documentation**: Modify roxygen comments in source files
5. **Regenerate docs**: Run `devtools::document()`
6. **Run checks**: Use `devtools::check()` to validate package

### Testing Strategy

**Current State**: Test files exist but contain only TODO comments

**Recommended Tests**:
- Unit tests for each class constructor
- Validation tests for each validator
- Integration tests for full workflow
- Edge cases (single condition, missing parameters, convergence failures)

---

## Best Practices

### For Package Users

1. **Use predefined models** when possible (faster, validated)
2. **Start small** with n_sims=10 to test workflow before full runs
3. **Monitor convergence** via `rhat` and `ess_*` columns
4. **Check conv_rate** to ensure simulations succeeded
5. **Use parallel processing** for production runs (n_cores > 1)

### For Package Developers

1. **Maintain S7 validators** for all new properties
2. **Support both S7 and list inputs** in functions used by parallel workers
3. **Document parameter requirements** clearly
4. **Test with edge cases** (n=10, single arm, extreme effect sizes)
5. **Use descriptive error messages** that guide users to fix issues
6. **Follow CLAUDE.md guidelines** for documentation and code style

---

## FAQ

### Why not use S3/S4?

S7 provides modern OOP with property validation and clean syntax. S3 lacks validation; S4 is verbose.

### Why pre-compile BRMS models?

Compilation takes 30-60 seconds. Pre-compiling once saves hours on large simulation studies.

### Why separate condition_values and static_values?

Makes it clear what's being varied (for power curves) vs. what's held constant. Also improves performance by not repeating static values in every row.

### How do I debug parallel execution?

Set `n_cores = 1` to run sequentially, which provides better error messages. Check `error` column in raw_results for simulation failures.

### Can I use custom priors?

Yes, specify `brms_args = list(prior = your_prior)` in `power_analysis()`. For design priors (on effect sizes), use the `design_prior` parameter.

---

## Contact & Contribution

For questions or contributions, see the main README and CLAUDE.md files.

**Document Maintained By**: Package development team
**Review Frequency**: After major API changes or new feature additions
