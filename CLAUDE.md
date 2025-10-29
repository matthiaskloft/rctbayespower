# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`rctbayespower` is an R package for conducting Bayesian power analysis for randomized controlled trials (RCTs) using `brms` and Stan. The package provides tools for estimating power curves, determining optimal sample sizes, and incorporating prior knowledge about treatment effects using region of practical equivalence (ROPE) for decision making.

## Current Status (Updated - 2025-10-29)

**Core Package State: Functional and stable core with documentation lag. Interim analysis feature in planning phase.**

**✅ IMPLEMENTED FEATURES - Object-Oriented API:**

**Core Functions (100% Complete & Working):**
- **`build_model()`** - Create model specifications for power analysis
- **`build_design()`** - Create experimental design configurations
- **`build_conditions()`** - Generate analysis conditions from design parameters
- **`simulate_single_run()`** - Execute single simulation run for power analysis
- **`power_analysis()` / `run()`** - Main power analysis execution using S7 objects

**Class System (S7 Architecture):**
- **`rctbp_model`** - S7 class for model specifications with data simulation and brms model
- **`rctbp_design`** - S7 class combining model with analysis decision criteria
- **`rctbp_conditions`** - S7 class for condition grids and argument management
- **`rctbp_power_analysis`** - S7 class for power analysis configuration and results

**Pre-built Models:**
- **`build_model("ancova_cont_2arms")()`** - ANCOVA model for continuous outcomes ✅
  - **To Do**: `build_model_ancova()` as generalized version with wrappers for specific defaults

**Advanced Features (100% Complete):**
- **Design Prior Integration** - Supports brms syntax with comprehensive fallback hierarchy
- **Integrated Power Computation** - Weighted power across effect sizes using priors
- **Sophisticated Parallelization** - Robust parallel processing with parameter preservation
- **Comprehensive Plotting** - Multiple visualization types with auto-detection
- **Model Caching** - Significant performance improvements for grid analyses

**S7 Methods (100% Complete):**
- `plot()` method for rctbp_power_analysis objects - Visualization of power analysis results
- `print()` methods for all rctbp_* object types - Formatted console output
- `run()` generic for executing analysis objects

**⚠️ IN PROGRESS FEATURES:**

**Interim Analysis Support (Planning Phase - 2025-10-29):**
- **Status**: Detailed implementation plan completed in `development/interim_analysis_implementation_plan.md`
- **Scope**: Sequential trial designs with early stopping for success/futility
- **Key Features Planned**:
  - `analysis_at` property in rctbp_design for interim timepoints
  - `adaptive` flag for parameter modification between looks
  - Custom interim decision functions receiving posterior summaries
  - Continue simulation to n_total even after early stopping (for metrics)
  - Rich results: stopping probabilities, expected sample size, decision consistency
- **Implementation Timeline**: 16-23 hours estimated
- **Next Steps**: Begin Phase 1 (Update rctbp_design class)

**⚠️ INCOMPLETE FEATURES:**

**Documentation Inconsistencies (Critical):**
- **Vignettes**: Still reference non-existent `power_analysis_ancova()` function - need complete rewriting
- **Manual pages**: Some still reference old function names
- **All vignettes need updating** for new API

**Test Suite (0% Complete):**
- **Status**: All test files contain only TODO comments
- **Impact**: No automated testing coverage
- **Files**: 4 test files with placeholder content only

**Missing Outcome Types:**
- **Binary outcomes** - Referenced in old docs but never implemented
- **Count outcomes** - Referenced in old docs but never implemented

**Current Production-Ready Workflow:**
```r
# 1. Create model → 2. Create design → 3. Create conditions → 4. Run analysis
model_ancova <- build_model("ancova_cont_2arms")()
design <- build_design(
  model = model_ancova,
  target_params = "b_arms_treat",
  thresholds_success = 0.2,
  thresholds_futility = 0,
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)
conditions <- build_conditions(
  design = design,
  condition_values = list(n_total = c(100, 200)),
  static_values = list(
    p_alloc = list(c(0.5, 0.5)),
    true_parameter_values = list(b_arms_treat = 0.5, ...)
  )
)
power_config <- power_analysis(
  conditions = conditions,
  n_cores = 4,
  n_sims = 100,
  run = TRUE  # Set to FALSE to create config without running
)
# Access results
print(power_config)
plot(power_config)
power_config@summarized_results
power_config@raw_results
```

## Development Practices and R CMD Check Guidelines

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
#' @param p_sig_success Probability threshold for success (default 0.95)
my_function <- function(p_sig_success = 0.975) { ... }

# GOOD: Documentation matches code
#' @param p_sig_success Probability threshold for success (default 0.975) 
my_function <- function(p_sig_success = 0.975) { ... }
```

**Recent fixes applied**: Updated documentation for `p_sig_success` in `power_analysis.R`, and class system fixes throughout package.

### Documentation Conventions
- Don't use \code{\link{function_name}} in roxygen docs. Use [functionname()] instead.

### Code Writing Guidelines
- **Output and Messaging Best Practices**:
  - Always wrap argument names in quotes when displaying them in printed output such as in stop(), warning(), message(), or documentation. This improves clarity and avoids confusion with actual values or natural language.
