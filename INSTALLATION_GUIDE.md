# rctbayespower: Complete Installation and Usage Guide

## Overview
The `rctbayespower` package provides comprehensive tools for Bayesian power analysis in randomized controlled trials (RCTs). This guide covers installation, setup, and basic usage.

## Installation Steps

### 1. Prerequisites
Ensure you have R >= 4.1.0 installed. For Bayesian modeling, you'll also need:

```r
# Install required system dependencies (may require admin privileges)
# For Windows: Install Rtools from https://cran.r-project.org/bin/windows/Rtools/

# Install package development tools
install.packages(c("devtools", "roxygen2", "testthat"))
```

### 2. Install Dependencies
```r
# Core dependencies
install.packages(c(
  "brms",        # Bayesian modeling
  "rstanarm",    # Alternative Bayesian models
  "bayestestR",  # Bayesian statistics
  "posterior",   # Posterior analysis
  "ggplot2",     # Plotting
  "dplyr",       # Data manipulation
  "tidyr",       # Data reshaping
  "purrr",       # Functional programming
  "magrittr",    # Pipe operators
  "tibble",      # Modern data frames
  "rlang"        # R language tools
))

# Suggested packages
install.packages(c(
  "knitr",       # Document generation
  "rmarkdown",   # R Markdown
  "testthat",    # Testing
  "covr"         # Coverage testing
))
```

### 3. Install rctbayespower
```r
# From source (development version)
devtools::install_local("path/to/rctbayespower")

# Or if published to GitHub
# devtools::install_github("matthiaskloft/rctbayespower")
```

## Quick Start

### Basic Power Analysis
```r
library(rctbayespower)

# Simple continuous outcome power analysis
result <- power_analysis(
  n_control = 50,
  n_treatment = 50,
  effect_size = 0.5,
  outcome_type = "continuous",
  n_simulations = 100  # Use more for real analysis (e.g., 1000)
)

# View results
print(result)
```

### Sample Size Determination
```r
# Find sample size for 80% power
sample_size_result <- sample_size_analysis(
  effect_size = 0.5,
  target_power = 0.8,
  outcome_type = "continuous",
  sample_sizes = seq(20, 100, by = 10),
  n_simulations = 200
)

print(sample_size_result)
```

### Power Curves
```r
# Generate power curve across effect sizes
power_curve_result <- bayesian_power_curve(
  n_control = 50,
  n_treatment = 50,
  effect_sizes = seq(0, 1, by = 0.1),
  outcome_type = "continuous",
  n_simulations = 200
)

# Plot results
plot_power_curve(power_curve_result)
```

### Data Simulation
```r
# Simulate trial data for testing
data <- simulate_rct_data(
  n_control = 100,
  n_treatment = 100,
  effect_size = 0.4,
  outcome_type = "continuous",
  baseline_mean = 0,
  baseline_sd = 1
)

head(data)
```

## Advanced Usage

### Binary Outcomes
```r
binary_power <- power_analysis(
  n_control = 200,
  n_treatment = 200,
  effect_size = 0.3,
  outcome_type = "binary",
  baseline_prob = 0.2,
  n_simulations = 500
)
```

### Count Outcomes
```r
count_power <- power_analysis(
  n_control = 150,
  n_treatment = 150,
  effect_size = 0.4,
  outcome_type = "count",
  baseline_rate = 3,
  n_simulations = 500
)
```

### Including Covariates
```r
covariates <- list(
  age = list(type = "continuous", mean = 50, sd = 10),
  sex = list(type = "binary", prob = 0.5),
  baseline_score = list(type = "continuous", mean = 20, sd = 5)
)

covariate_power <- power_analysis(
  n_control = 75,
  n_treatment = 75,
  effect_size = 0.5,
  outcome_type = "continuous",
  covariates = covariates,
  n_simulations = 300
)
```

### Custom Priors
```r
custom_power <- power_analysis(
  n_control = 60,
  n_treatment = 60,
  effect_size = 0.4,
  outcome_type = "continuous",
  prior_specification = "normal(0, 0.5)",  # Skeptical prior
  n_simulations = 400
)
```

## Package Testing

Run the validation script to test core functionality:
```r
source("validate_package.R")
```

For complete testing with brms models (requires Stan):
```r
# Run all tests
testthat::test_package("rctbayespower")

# Check package
devtools::check()
```

## Performance Tips

1. **Start Small**: Use fewer simulations (e.g., 100) for initial testing
2. **Parallel Processing**: Enable parallel processing for large analyses:
   ```r
   power_analysis(..., parallel = TRUE, cores = 4)
   ```
3. **Simulation Count**: Use 1000+ simulations for final analyses
4. **Stan Setup**: Ensure Stan/brms is properly configured for your system

## Troubleshooting

### Common Issues

1. **Stan/brms Installation**: 
   - Ensure Rtools is installed on Windows
   - Try `install.packages("brms", dependencies = TRUE)`

2. **Memory Issues**: 
   - Reduce `n_simulations` 
   - Use `parallel = FALSE` if memory is limited

3. **Slow Performance**:
   - Start with smaller sample sizes for testing
   - Use parallel processing when possible

### Getting Help

1. Check vignettes: `browseVignettes("rctbayespower")`
2. Function documentation: `?power_analysis`
3. Package website: [GitHub Repository URL]

## Citation

If you use this package in your research, please cite:

```
[Citation information - to be added when package is published]
```
