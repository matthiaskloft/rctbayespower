# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`rctbayespower` is an R package for conducting Bayesian power analysis for randomized controlled trials (RCTs) using `brms` and Stan. The package provides tools for estimating power curves, determining optimal sample sizes, and incorporating prior knowledge about treatment effects using region of practical equivalence (ROPE) for decision making.

## Current Status (Updated)

**Recent Major Improvements Completed:**
- ✅ **Model Caching**: Implemented sophisticated model caching in `power_grid_analysis()` that groups combinations by effect size and reuses compiled brms models for significant performance gains
- ✅ **Design Prior Parsing**: Enhanced design prior parsing with comprehensive fallback hierarchy (stats package → brms package → error) and support for all standard distributions
- ✅ **Integrated Power Output**: Fixed missing output when target power levels aren't achieved with tested sample sizes
- ✅ **Plotting Function Fixes**: Resolved aesthetic inheritance warnings in `plot_power_grid_analysis()` and improved data validation
- ✅ **Parallelization Fixes**: Corrected parameter preservation issues for ANCOVA functions when using cached models
- ✅ **Grid Analysis Functions**: New comprehensive `power_grid_analysis()` function that replaces and extends previous sample size and effect size analysis functions

**Current Package State:**
- All core functions working with enhanced performance and reliability
- Model caching provides significant speedup for grid analyses
- Comprehensive plotting system with multiple visualization options
- Robust parallelization with proper parameter handling
- Package structure and dependencies stable and optimized

## Core Architecture

The package follows standard R package structure with enhanced performance optimizations:

### **Primary Grid Analysis Function** 
- **`power_grid_analysis()`**: Unified function for all power analysis scenarios:
  - Sample size analysis (varying sample sizes, fixed effect size)
  - Effect size analysis (varying effect sizes, fixed sample size) 
  - Full grid analysis (varying both sample sizes and effect sizes)
  - **Model Caching**: Groups combinations by effect size and reuses compiled models
  - **Design Prior Integration**: Supports weighted power computation using prior distributions
  - **Integrated Power**: Computes weighted average power across effect sizes

### **Core Power Analysis Pipeline**
- **`power_analysis()`**: Flexible Bayesian power analysis with custom models
- **`power_analysis_ancova()`**: Convenience wrapper for standard RCT designs with baseline covariates
  - Enhanced with `compile_models_only` option for model caching
  - Proper parameter preservation for parallelization
- **`validate_power_design()`**: Pre-validation of analysis designs with model compilation

### **Visualization System**
- **`plot.rctbayespower_grid()`**: Comprehensive plotting for grid analysis results
  - Auto-detection of optimal plot types
  - Power curves, heatmaps, integrated power plots, comparison plots
  - Support for faceting and multiple metrics
  - Robust data validation and dependency checking

### **Design Prior System**
- **Enhanced Parser**: Supports brms syntax with comprehensive fallback to stats package
- **Validation**: Automatic quantile computation and coverage checking
- **Integration**: Seamless integration with grid analysis for weighted power computation

All functions support three outcome types (continuous, binary, count) with flexible covariate inclusion and custom prior specifications. A workflow template (`workflow_template.R`) provides a starting point for custom power analyses.

## Common Development Commands

### Package Building and Checking
```r
# Generate documentation from roxygen2 comments
devtools::document()

# Install package locally with dependencies
devtools::install(".", dependencies = TRUE)

# Run R CMD check
devtools::check()

# Build package tarball
devtools::build()

# Run tests
devtools::test()
```

### Testing
```r
# Run all tests
testthat::test_dir("tests/testthat")

# Run specific test file
testthat::test_file("tests/testthat/test-power_analysis.R")
```

### Documentation Building
```r
# Build vignettes
devtools::build_vignettes()
```

## Dependencies

Key dependencies that must be available:
- **brms** (≥ 2.20.0): Bayesian modeling interface to Stan
- **rstanarm**: Alternative Bayesian modeling
- **bayestestR**: Bayesian inference tools (ROPE analysis)
- **posterior**: Stan posterior analysis
- **ggplot2, dplyr, tidyr, purrr**: Data manipulation and visualization

## Key Implementation Details

### Enhanced Bayesian Workflow
The package implements a sophisticated simulation-based approach with performance optimizations:

**Standard Power Analysis Pipeline:**
1. Generates RCT data using custom `simulate_data_fn` or built-in functions
2. Fits Bayesian model with `brms` using design parameters
3. Extracts treatment effect posterior distributions
4. Calculates power metrics based on posterior probabilities

**Model Caching System (New):**
- Groups analysis combinations by effect size to enable model reuse
- Compiles brms models once per effect size, reuses for different sample sizes
- Provides dramatic performance improvements for grid analyses
- Supports both `power_analysis` and `power_analysis_ancova` workflows

### ANCOVA Convenience Function (Enhanced)
The `power_analysis_ancova()` function provides a simplified interface with new capabilities:
- Automatically sets up baseline covariate models
- Supports all three outcome types with appropriate link functions
- Includes sensible default priors for treatment and baseline effects
- **New**: `compile_models_only` option for model caching workflows
- **Enhanced**: Proper parameter preservation for parallel processing
- Allows custom threshold specifications for success/futility decisions

### Design Prior Integration
**Comprehensive Parser (New):**
- Supports brms syntax: `"normal(0.3, 0.1)"`, `"student_t(6, 0.5, 0.2)"`, etc.
- Automatic fallback hierarchy: stats package → brms package → error
- Coverage validation with quantile computation and warnings
- Custom R function support for complex prior specifications

**Integrated Power Computation:**
- Weighted power computation across effect sizes using design priors
- Automatic normalization and validation of prior weights
- Informative output when target power levels aren't achieved

### Algorithm Selection
Functions support multiple algorithms via `algorithm` parameter:
- "sampling": Full MCMC (default, most accurate)
- "meanfield"/"fullrank": Variational inference (faster) 
- "pathfinder": Path-finder variational inference
- "laplace": Laplace approximation (fastest)

### Outcome Type Handling
- **Continuous**: Cohen's d effect sizes, Gaussian models
- **Binary**: Log odds ratios, Bernoulli models with logit link
- **Count**: Log rate ratios, Poisson models with log link

Each outcome type has specific data generation and model fitting procedures in the respective functions.

### Parallelization System (Enhanced)
- Robust parallel processing with proper parameter preservation
- Progress tracking with configurable update intervals
- Error handling and convergence monitoring
- Automatic core detection and optimization

### Visualization System (New)
**Comprehensive Plotting Functions:**
- Auto-detection of optimal plot types based on analysis structure
- Power curves, heatmaps, integrated power plots, comparison plots
- Support for faceting by effect size or sample size
- Configurable metrics display (power vs posterior probabilities)
- Robust data validation and dependency checking

### Validation and Testing
- **`validate_power_design()`**: Pre-validates analysis setups
- **`validate_weighting_function()`**: Tests design prior implementations
- Comprehensive error checking and informative error messages
- Built-in convergence diagnostics and quality checks

### Workflow Template
The `workflow_template.R` file provides a complete example workflow for custom power analyses:
- Shows step-by-step setup from data simulation to model specification
- Demonstrates prior specification with constant values for design models
- Includes examples for checking default priors and setting custom priors
- Shows model caching and performance optimization techniques
- Serves as a template for researchers developing their own power analysis pipelines