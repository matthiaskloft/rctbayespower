# rctbayespower 0.1.0

## Initial Release

### New Features

* **Core Functions**:
  - `power_analysis()`: Main function for Bayesian power analysis using object-oriented API
  - `build_model()`: Create model specifications for power analysis
  - `build_design()`: Create experimental design configurations  
  - `build_conditions()`: Generate analysis conditions from design parameters
  - `simulate_single_run()`: Execute single simulation run for power analysis

* **Pre-built Models**:
  - `build_model_ancova_cont()`: ANCOVA model for continuous outcomes with baseline covariates

* **Outcome Types**:
  - Continuous outcomes with Cohen's d effect sizes (via ANCOVA models)

* **Power Metrics**:
  - ROPE Power: Probability effect is outside region of practical equivalence
  - Directional Power: Probability effect is in expected direction
  - Significance Power: Traditional frequentist-like power

* **Advanced Features**:
  - Design prior integration for weighted power computation using `brms` syntax
  - Model caching system for performance optimization
  - Comprehensive parallelization support
  - Flexible ROPE (Region of Practical Equivalence) limits
  - Object-oriented API for consistent parameter management

* **Visualization**:
  - `plot.rctbayespower_sim_result()`: Comprehensive plotting system with multiple visualization types
  - Automatic plot type detection based on analysis conditions
  - Power curves, heatmaps, integrated plots, and comparison plots
  - Publication-ready plots with customizable themes

* **Documentation**:
  - Three comprehensive vignettes covering basic usage, prior specification, and algorithm performance
  - Extensive function documentation with examples
  - Complete README with quick start guide

### Dependencies

* Requires R >= 4.1.0
* Integrates with `brms` for Bayesian model fitting
* Plotting capabilities via `ggplot2`
* Data manipulation with `dplyr`, `tidyr`, `purrr`
* Parallelization support via `parallel` package

### API Design

This package uses an object-oriented approach requiring users to:
1. Build model specifications using `build_model()` or pre-built models
2. Create experimental designs with `build_design()`
3. Generate analysis conditions with `build_conditions()`
4. Execute power analysis with `power_analysis()`

### Notes

* This is the initial release of the package
* Core functionality is stable and production-ready
* Model caching provides significant performance improvements for grid analyses
* Package follows standard R package development practices