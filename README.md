
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rctbayespower

<!-- badges: start -->

<!-- Note: Badges are placeholders for when the package is published -->

<!-- badges: end -->

## Disclaimer

This is an experimental package written with the help of LLMs and is not
yet thouroughly tested yet. Hence I do not recommend using it for
production code or critical analyses.

## Overview

`rctbayespower` provides tools for conducting Bayesian power analysis
for randomized controlled trials (RCTs) using `brms` and Stan. The
package allows researchers to:

- Estimate power curves for different effect sizes
- Determine optimal sample sizes using Bayesian methods
- Incorporate prior knowledge about treatment effects
- Use region of practical equivalence (ROPE) for decision making
- Handle various outcome types (continuous, binary, count)
- Include covariates in power calculations

## Installation

You can install the development version of `rctbayespower` from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("matthiaskloft/rctbayespower")
```

## Quick Start

``` r
library(rctbayespower)

# Basic power analysis using ANCOVA wrapper
power_result <- power_analysis_ancova(
  n_control = 50,
  n_treatment = 50,
  outcome_type = "continuous",
  effect_size = 0.5,  # Cohen's d
  baseline_effect = 0.2,
  threshold_success = 0.2,
  threshold_futility = 0,
  n_simulations = 1000
)

print(power_result)

# Grid analysis for sample size planning
grid_result <- power_grid_analysis(
  sample_sizes = seq(20, 100, by = 20),
  effect_sizes = 0.5,
  threshold_success = 0.2,
  threshold_futility = 0,
  power_analysis_fn = "power_analysis_ancova",
  outcome_type = "continuous",
  baseline_effect = 0.2,
  n_simulations = 500
)

# Plot results
plot(grid_result)
```

## Key Features

### Multiple Outcome Types

- **Continuous outcomes**: Using Cohen’s d effect sizes
- **Binary outcomes**: Using log odds ratios
- **Count outcomes**: Using log rate ratios

### Flexible Power Metrics

- **ROPE Power**: Probability effect is outside region of practical
  equivalence
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
|----|----|
| `power_analysis()` | Main power analysis function |
| `power_analysis_ancova()` | Convenience wrapper for ANCOVA designs |
| `power_grid_analysis()` | Grid analysis varying sample sizes and/or effect sizes |
| `plot.rctbayespower_grid()` | Visualization functions for grid analysis |
| `validate_power_design()` | Pre-validate analysis designs |

## Example Workflows

### Sample Size Planning

``` r
# Determine sample size for desired power
result <- power_grid_analysis(
  sample_sizes = seq(30, 150, by = 20),
  effect_sizes = 0.4,
  threshold_success = 0.2,
  threshold_futility = 0,
  power_analysis_fn = "power_analysis_ancova",
  outcome_type = "continuous",
  baseline_effect = 0.2,
  n_simulations = 500
)

plot(result)
```

### Power Curve Analysis

``` r
# Generate power curve across effect sizes
curve_result <- power_grid_analysis(
  sample_sizes = 75,
  effect_sizes = seq(0, 1, by = 0.1),
  threshold_success = 0.2,
  threshold_futility = 0,
  power_analysis_fn = "power_analysis_ancova",
  outcome_type = "continuous",
  baseline_effect = 0.2,
  n_simulations = 500
)

plot(curve_result)
```

### Binary Outcomes

``` r
# Power analysis for binary endpoint
binary_power <- power_analysis_ancova(
  n_control = 100,
  n_treatment = 100,
  outcome_type = "binary",
  effect_size = 0.5,  # log odds ratio
  baseline_effect = 0.2,
  threshold_success = 0.3,
  threshold_futility = 0,
  n_simulations = 1000
)
```

### Including Covariates

``` r
# The power_analysis_ancova function includes baseline covariates by default
# For more complex covariate structures, use the flexible power_analysis function
# with custom data simulation and model formulas (see workflow_template.R)

# Basic analysis with baseline covariate
power_with_baseline <- power_analysis_ancova(
  n_control = 60,
  n_treatment = 60,
  outcome_type = "continuous",
  effect_size = 0.4,
  baseline_effect = 0.3,  # Effect of baseline covariate
  threshold_success = 0.2,
  threshold_futility = 0,
  n_simulations = 1000
)
```

## Documentation

- [Getting Started Vignette](vignettes/introduction.Rmd): Basic usage
  and concepts
- [Advanced Techniques](vignettes/advanced-techniques.Rmd): Complex
  designs and custom analyses  
- [Case Studies](vignettes/case-studies.Rmd): Real-world applications
  across domains

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
- **Vignettes**: Three detailed guides covering basic usage, advanced
  techniques, and real-world case studies
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

``` r
# Install dependencies first
install.packages(c("brms", "bayestestR", "ggplot2", "dplyr"))

# Install rctbayespower
devtools::install_local("path/to/rctbayespower")
```

### Detailed Setup

See `INSTALLATION_GUIDE.md` for complete installation instructions,
including system dependencies and troubleshooting.

### Quick Validation

``` r
# Test core functionality
source("validate_package.R")
```

## Contributing

Contributions are welcome! Please see our [contributing
guidelines](CONTRIBUTING.md) for details.

## Citation

If you use `rctbayespower` in your research, please cite:

    Author, A. et al. (2025). rctbayespower: Bayesian Power Analysis for 
    Randomized Controlled Trials. R package version X.X.X.

## License

This project is licensed under the MIT License - see the
[LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

- The Stan Development Team for Stan and related R packages
- The brms package authors for making Bayesian modeling accessible
- Contributors to bayestestR for Bayesian inference tools
