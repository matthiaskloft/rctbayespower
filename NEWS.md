# rctbayespower 0.0.0.9000 (Development Version)

## Development Status

**⚠️ This package is currently in active development and not yet released.**

### New Features

* **Core Functions**:
  - `power_analysis()`: Run a power analysis

* **Pre-built Models**:
  - ANCOVA model for continuous outcomes with baseline covariate
  - `build_model("ancova_cont_2arms")`
  - `build_model("ancova_cont_3arms")`

* **Power Metrics**:
  - Probability of success and futility
  - Significance Power: Traditional frequentist-like power for success and futility

* **Advanced Features**:
  - Comprehensive parallelization support
  - Object-oriented API for consistent parameter management

### API Design

This package uses an object-oriented approach requiring users to:
1. Build model specifications using `build_model()` or pre-built models
2. Create study designs with `build_design()`
3. Generate analysis conditions with `build_conditions()`
4. Run power analysis using `power_analysis()`


### Dependencies

* Requires R >= 4.1.0
* Integrates with `brms` for Bayesian model fitting
* Plotting capabilities via `plotly`
* Data manipulation with `dplyr`, `tidyr`, `purrr`
* Parallelization support via `parallel` package






