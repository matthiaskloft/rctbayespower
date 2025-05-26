# rctbayespower 0.1.0

## Initial Release

### New Features

* **Core Functions**:
  - `power_analysis()`: Main function for Bayesian power analysis
  - `sample_size_analysis()`: Determine optimal sample sizes
  - `bayesian_power_curve()`: Generate power curves across effect sizes
  - `effect_size_analysis()`: Analyze effect size estimation accuracy
  - `simulate_rct_data()`: Generate simulated RCT data

* **Outcome Types**:
  - Continuous outcomes with Cohen's d effect sizes
  - Binary outcomes with log odds ratio effect sizes  
  - Count outcomes with log rate ratio effect sizes

* **Power Metrics**:
  - ROPE Power: Probability effect is outside region of practical equivalence
  - Directional Power: Probability effect is in expected direction
  - Significance Power: Traditional frequentist-like power

* **Advanced Features**:
  - Custom prior specifications using `brms` syntax
  - Covariate inclusion in power calculations
  - Flexible ROPE (Region of Practical Equivalence) limits
  - Comprehensive plotting functions with `ggplot2`

* **Visualization**:
  - `plot_power_curve()`: Create power curves, sample size plots, and effect distributions
  - Automatic plot type detection based on analysis object
  - Publication-ready plots with customizable themes

* **Documentation**:
  - Three comprehensive vignettes covering basic usage, advanced techniques, and real-world case studies
  - Extensive function documentation with examples
  - Complete README with quick start guide

### Dependencies

* Requires R >= 4.1.0
* Integrates with `brms` for Bayesian model fitting
* Uses `bayestestR` for Bayesian inference metrics
* Plotting capabilities via `ggplot2`
* Data manipulation with `dplyr`, `tidyr`, `purrr`

### Notes

* This is the initial release of the package
* All functions are considered stable for basic use cases
* Extensive testing across different scenarios included in vignettes
* Package follows standard R package development practices
