# rctbayespower 0.0.0.9000 (Development Version)

## Development Status

**⚠️ This package is currently in active development and not yet released.**

### New Features

* **Core Functions**:
  - `power_analysis()`: Run a power analysis

* **Pre-built Models**:
  - ANCOVA model for continuous outcomes with baseline covariate
  - `build_design(model_name = "ancova_cont_2arms")`
  - `build_design(model_name = "ancova_cont_3arms")`

* **Power Metrics**:
  - Probability of success and futility
  - Significance Power: Traditional frequentist-like power for success and futility

* **Advanced Features**:
  - Comprehensive parallelization support
  - Object-oriented API for consistent parameter management
  - Dual backend support (brms + BayesFlow)

### API Design

This package uses an object-oriented approach requiring users to:
1. Create designs with `build_design(model_name = ...)` (includes model)
2. Generate analysis conditions with `build_conditions()`
3. Run power analysis using `power_analysis()`

### Breaking Changes (v0.0.0.9000)

* **API Refactoring**: Model merged into design
  - `build_model()` is deprecated, use `build_design(model_name = ...)` instead
  - `get_model()` is deprecated, use `build_design(model_name = ...)` instead
  - Model properties now accessed directly on design (e.g., `design@backend` not `design@model@backend`)

* **Parameter Renaming**:
  - `thresholds_success` → `thresh_scs`
  - `thresholds_futility` → `thresh_ftl`
  - `parameter_names_brms` → `par_names_inference`
  - `parameter_names_sim_fn` → `par_names_sim`


### Dependencies

* Requires R >= 4.1.0
* Integrates with `brms` for Bayesian model fitting
* Plotting capabilities via `plotly`
* Data manipulation with `dplyr`, `tidyr`, `purrr`
* Parallelization support via `parallel` package






