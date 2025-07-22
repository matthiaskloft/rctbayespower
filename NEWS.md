# rctbayespower 0.1.0 (Development Version)

## Development Status

**⚠️ This package is currently in active development and not yet released.**

### New Features

* **Core Functions**:
  - `power_analysis()`: Main function for Bayesian power analysis using object-oriented API
  - `build_model()`: Create model specifications for power analysis
  - `build_design()`: Create experimental design configurations  
  - `build_conditions()`: Generate analysis conditions from design parameters
  - `simulate_single_run()`: Execute single simulation run for power analysis

* **Pre-built Models**:
  - `build_model("ancova_cont_2arms")`: ANCOVA model for continuous outcomes with baseline covariates

* **Outcome Types**:
  - Continuous outcomes with Cohen's d effect sizes (via ANCOVA models)

* **Power Metrics**:
  - Directional Power: Probability effect is in expected direction
  - Significance Power: Traditional frequentist-like power

* **Advanced Features**:
  - Comprehensive parallelization support
  - Object-oriented API for consistent parameter management


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



