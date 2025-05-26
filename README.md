# rctbayespower

<!-- badges: start -->
[![R-CMD-check](https://github.com/matthiaskloft/rctbayespower/workflows/R-CMD-check/badge.svg)](https://github.com/matthiaskloft/rctbayespower/actions)
[![Codecov test coverage](https://codecov.io/gh/matthiaskloft/rctbayespower/branch/main/graph/badge.svg)](https://codecov.io/gh/matthiaskloft/rctbayespower?branch=main)
[![CRAN status](https://www.r-pkg.org/badges/version/rctbayespower)](https://CRAN.R-project.org/package=rctbayespower)
<!-- badges: end -->

## Overview

`rctbayespower` provides tools for conducting Bayesian power analysis for randomized controlled trials (RCTs) using `brms` and Stan. The package allows researchers to:

- Estimate power curves for different effect sizes
- Determine optimal sample sizes using Bayesian methods
- Incorporate prior knowledge about treatment effects
- Use region of practical equivalence (ROPE) for decision making
- Handle various outcome types (continuous, binary, count)
- Include covariates in power calculations

## Installation

You can install the development version of `rctbayespower` from GitHub:

```r
# install.packages("devtools")
devtools::install_github("matthiaskloft/rctbayespower")
```

## Quick Start

```r
library(rctbayespower)

# Basic power analysis for continuous outcome
power_result <- power_analysis(
  n_control = 50,
  n_treatment = 50,
  effect_size = 0.5,  # Cohen's d
  outcome_type = "continuous",
  n_simulations = 1000
)

print(power_result)

# Sample size analysis
sample_size_result <- sample_size_analysis(
  effect_size = 0.5,
  target_power = 0.8,
  outcome_type = "continuous",
  sample_sizes = seq(20, 100, by = 10)
)

# Plot results
plot_power_curve(sample_size_result, type = "sample_size")
```

## Key Features

### Multiple Outcome Types

- **Continuous outcomes**: Using Cohen's d effect sizes
- **Binary outcomes**: Using log odds ratios
- **Count outcomes**: Using log rate ratios

### Flexible Power Metrics

- **ROPE Power**: Probability effect is outside region of practical equivalence
- **Directional Power**: Probability effect is in expected direction  
- **Significance Power**: Traditional frequentist-like power

### Advanced Features

- Custom prior specifications
- Covariate inclusion
- Power curves and sample size curves
- Effect size bias analysis
- Comprehensive plotting functions

## Main Functions

| Function | Purpose |
|----------|---------|
| `power_analysis()` | Main power analysis function |
| `sample_size_analysis()` | Determine optimal sample size |
| `bayesian_power_curve()` | Generate power curves |
| `effect_size_analysis()` | Analyze effect size estimation |
| `simulate_rct_data()` | Generate simulated RCT data |
| `plot_power_curve()` | Visualization functions |

## Example Workflows

### Sample Size Planning

```r
# Determine sample size for desired power
result <- sample_size_analysis(
  effect_size = 0.4,
  target_power = 0.8,
  outcome_type = "continuous",
  sample_sizes = seq(30, 150, by = 20),
  n_simulations = 500
)

plot_power_curve(result, type = "sample_size")
```

### Power Curve Analysis

```r
# Generate power curve across effect sizes
curve_result <- bayesian_power_curve(
  n_control = 75,
  n_treatment = 75,
  effect_sizes = seq(0, 1, by = 0.1),
  outcome_type = "continuous",
  n_simulations = 500
)

plot_power_curve(curve_result)
```

### Binary Outcomes

```r
# Power analysis for binary endpoint
binary_power <- power_analysis(
  n_control = 100,
  n_treatment = 100,
  effect_size = 0.5,  # log odds ratio
  outcome_type = "binary",
  baseline_prob = 0.3,  # control group success rate
  n_simulations = 1000
)
```

### Including Covariates

```r
# Define covariates
covariates <- list(
  age = list(type = "continuous", mean = 45, sd = 10),
  sex = list(type = "binary", prob = 0.5)
)

# Power analysis with covariates
power_with_covs <- power_analysis(
  n_control = 60,
  n_treatment = 60,
  effect_size = 0.4,
  outcome_type = "continuous",
  covariates = covariates,
  n_simulations = 1000
)
```

## Documentation

- [Getting Started Vignette](vignettes/introduction.Rmd): Basic usage and concepts
- [Advanced Techniques](vignettes/advanced-techniques.Rmd): Complex designs and custom analyses  
- [Case Studies](vignettes/case-studies.Rmd): Real-world applications across domains

## Dependencies

The package requires:

- R (≥ 4.1.0)
- brms (≥ 2.20.0)
- rstanarm
- bayestestR
- posterior
- ggplot2, dplyr, tidyr, purrr

For full functionality, ensure you have a working Stan installation.

## Package Status

✅ **Complete R Package Framework** - Ready for use and distribution

### Package Contents

- **Core Functions**: Complete Bayesian power analysis workflow
- **Documentation**: Comprehensive help files for all functions  
- **Vignettes**: Three detailed guides covering basic usage, advanced techniques, and real-world case studies
- **Tests**: Unit tests for core functionality
- **Examples**: Simulated datasets for testing and learning

### Files Included

- `R/`: All core analysis functions
- `man/`: Complete function documentation
- `tests/`: Unit tests with testthat
- `vignettes/`: Three comprehensive vignettes
- `data-raw/`: Scripts for creating example datasets
- `build_package.R`: Package building instructions
- `validate_package.R`: Quick functionality testing
- `INSTALLATION_GUIDE.md`: Complete setup and usage guide

## Installation and Setup

### Quick Installation
```r
# Install dependencies first
install.packages(c("brms", "bayestestR", "ggplot2", "dplyr"))

# Install rctbayespower
devtools::install_local("path/to/rctbayespower")
```

### Detailed Setup
See `INSTALLATION_GUIDE.md` for complete installation instructions, including system dependencies and troubleshooting.

### Quick Validation
```r
# Test core functionality
source("validate_package.R")
```

## Contributing

Contributions are welcome! Please see our [contributing guidelines](CONTRIBUTING.md) for details.

## Citation

If you use `rctbayespower` in your research, please cite:

```
Author, A. et al. (2025). rctbayespower: Bayesian Power Analysis for 
Randomized Controlled Trials. R package version X.X.X.
```

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

- The Stan Development Team for Stan and related R packages
- The brms package authors for making Bayesian modeling accessible
- Contributors to bayestestR for Bayesian inference tools
