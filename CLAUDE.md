# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`rctbayespower` is an R package for conducting Bayesian power analysis for randomized controlled trials (RCTs) using `brms` and Stan. The package provides tools for estimating power curves, determining optimal sample sizes, and incorporating prior knowledge about treatment effects using region of practical equivalence (ROPE) for decision making.

## Current Status (Updated - 2025-07-21)

**Core Package State: Functional and stable core with documentation lag**

**✅ IMPLEMENTED FEATURES - Object-Oriented API:**

**Core Functions (100% Complete & Working):**
- **`power_analysis()`** - Main Bayesian power analysis function using new object-oriented API
- **`build_model()`** - Create model specifications for power analysis
- **`build_design()`** - Create experimental design configurations
- **`build_conditions()`** - Generate analysis conditions from design parameters
- **`simulate_single_run()`** - Execute single simulation run for power analysis

**Pre-built Models:**
- **`build_model("ancova_cont_2arms")()`** - ANCOVA model for continuous outcomes ✅
  - **To Do**: `build_model_ancova()` as generalized version with wrappers for specific defaults

**Advanced Features (100% Complete):**
- **Design Prior Integration** - Supports brms syntax with comprehensive fallback hierarchy
- **Integrated Power Computation** - Weighted power across effect sizes using priors
- **Sophisticated Parallelization** - Robust parallel processing with parameter preservation
- **Comprehensive Plotting** - Multiple visualization types with auto-detection
- **Model Caching** - Significant performance improvements for grid analyses

**S3 Methods (100% Complete):**
- `plot.rctbayespower_sim_result()` - Visualization of power analysis results
- `print.rctbayespower_*()` methods for all object types

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
design <- build_design(model, target_params, thresholds, p_sig)
conditions <- build_conditions(design, condition_values, static_values)
result <- power_analysis(conditions, n_cores, n_simulations)
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
