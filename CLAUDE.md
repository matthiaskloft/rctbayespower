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
- Core functions (`power_analysis`, `power_analysis_ancova`, `power_grid_analysis`) working with enhanced performance
- Model caching provides significant speedup for grid analyses
- Comprehensive plotting system with multiple visualization options
- Robust parallelization with proper parameter handling
- Package structure and dependencies stable and optimized

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

**Recent fixes applied**: Updated documentation for `p_sig_success` in `power_analysis.R` and `power_analysis_ancova.R`, and `target_power_futility` in `power_grid_analysis.R`.

### Documentation Conventions
- Don't use \code{\link{function_name}} in roxygen docs. Use [functionname()] instead.