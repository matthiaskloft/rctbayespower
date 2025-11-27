# S7 Class System

**Last Updated:** 2025-11-27

## Why S7?

- **Property Access**: Clean `@` syntax (e.g., `model@data_simulation_fn`)
- **Validation**: Built-in validators ensure object integrity
- **Composition**: Easy nesting (design contains model, conditions contain design)
- **Modern R**: S7 is the recommended OOP system for new packages

## Class Definitions

### `rctbp_model` (R/class_model.R)

**Purpose**: Encapsulates everything needed to simulate data and fit a Bayesian model. Supports dual backends (brms + BayesFlow).

| Property | Type | Description |
|----------|------|-------------|
| `data_simulation_fn` | function | Generates trial data |
| `brms_model` | brmsfit/NULL | Pre-compiled brms template |
| `bayesflow_model` | any/NULL | BayesFlow model (Keras/Workflow/Approximator) |
| `backend` | character | "brms", "bf", or "auto" (default: "auto") |
| `backend_args_brms` | list | brms config (chains, iter, etc.) |
| `backend_args_bf` | list | BayesFlow config (batch_size, n_posterior_samples) |
| `predefined_model` | character/NULL | Name if predefined |
| `model_name` | character | Descriptive name |
| `n_endpoints` | numeric | Number of outcomes |
| `endpoint_types` | character | Types (continuous, binary) |
| `n_arms` | numeric | Treatment arms |
| `n_repeated_measures` | numeric/NULL | Time points |
| `active_backend` | **computed** | Resolves "auto" → "bf" or "brms" |
| `backend_args` | **computed** | Merged args for active backend |
| `parameter_names_sim_fn` | **computed** | Auto-extracted from sim function |
| `parameter_names_brms` | **computed** | Auto-extracted from brms model |

**Backend Selection Logic** (in `active_backend` getter):
```r
if (backend != "auto") return(backend)
if (!is.null(bayesflow_model)) return("bf")
if (!is.null(brms_model)) return("brms")
cli::cli_abort("No backend available")
```

**Constructor**: `build_model(predefined_model)` or direct construction for custom models

**Helper Functions**:
- `add_bf_backend(model, bf_model)`: Add BayesFlow backend to existing model
- `add_brms_backend(model, brms_model)`: Add brms backend to existing model

### `rctbp_design` (R/class_design.R)

**Purpose**: Combines a model with analysis decision criteria.

| Property | Type | Description |
|----------|------|-------------|
| `model` | rctbp_model | The statistical model |
| `target_params` | character | Parameters to analyze |
| `p_sig_scs` | numeric/function | Probability threshold for success (default 0.975) |
| `p_sig_ftl` | numeric/function | Probability threshold for futility (default 0.5) |
| `analysis_at` | numeric/NULL | Interim analysis points (fractions of n_total) |
| `design_name` | character/NULL | Optional name |

**Constructor**: `build_design(model, target_params, ...)`

**Validator**: Checks that `target_params` exist in the model's parameter names.

**Look-Dependent Boundaries**: `p_sig_scs` and `p_sig_ftl` can be functions for sequential designs:
```r
design <- build_design(
  model = model,
  target_params = "b_armtreat_1",
  p_sig_scs = boundary_obf(0.975),     # O'Brien-Fleming style
  p_sig_ftl = boundary_linear(0.7, 0.9), # Linear interpolation
  analysis_at = c(0.5, 0.75)           # Interim at 50%, 75%
)
```

### `rctbp_conditions` (R/class_conditions.R)

**Purpose**: Generates parameter combinations for power analysis grid.

| Property | Type | Description |
|----------|------|-------------|
| `design` | rctbp_design | The analysis design |
| `conditions_grid` | data.frame | All condition combinations |
| `condition_arguments` | list | Argument lists per condition |
| `condition_values` | list | User-specified varying params |
| `static_values` | list | User-specified constant params |

**Constructor**: `build_conditions(design, condition_values, static_values)`

**Key Feature**: Separates arguments into `sim_args` and `decision_args` based on function signatures.

### `rctbp_power_analysis` (R/class_power_analysis.R)

**Purpose**: Configures and executes power analysis simulations.

| Property | Type | Description |
|----------|------|-------------|
| `conditions` | rctbp_conditions | Conditions to evaluate |
| `n_sims` | numeric | Simulations per condition (default: 1) |
| `n_cores` | numeric | Parallel cores (default: 1) |
| `verbose` | logical | Show progress (default: TRUE) |
| `brms_args` | list/NULL | Arguments passed to brms |
| `design_prior` | character/function/NULL | Optional design prior |
| `design` | **computed** | Convenience access to conditions@design |
| `results_summ` | data.frame | Aggregated results (filled after run) |
| `results_raw` | data.frame | Individual simulation results (filled after run) |
| `elapsed_time` | numeric | Runtime in minutes (filled after run) |

**Constructor**: `power_analysis(conditions, n_sims, n_cores, ..., run = TRUE)`

**Execution**: `run(power_object)` - S7 generic method

## S7 Package Integration

In `R/zzz.R`:
```r
.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
```

## Common S7 Patterns

### Defining Classes

```r
MyClass <- S7::new_class(
  name = "MyClass",
  parent = S7::S7_object,
  properties = list(
    foo = S7::class_character,
    bar = S7::class_double
  ),
  validator = function(self) {
    if (nchar(self@foo) == 0) return("@foo must be non-empty")
    NULL  # Return NULL if valid
  }
)
```

### Computed Properties

```r
properties = list(
  data = S7::class_list,
  count = S7::new_property(
    getter = function(self) length(self@data)
  )
)
```

### Settable Properties with Validation

```r
properties = list(
  backend = S7::new_property(
    class = S7::class_character,
    default = "auto",
    setter = function(self, value) {
      if (!value %in% c("brms", "bf", "auto")) {
        cli::cli_abort("{.arg backend} must be 'brms', 'bf', or 'auto'")
      }
      self@backend <- value
      self
    }
  )
)
```

### Defining Methods

```r
# S7 generic
my_generic <- S7::new_generic("my_generic", dispatch_args = "x")

# Method for specific class
S7::method(my_generic, MyClass) <- function(x, ...) {
  paste0("Hello, ", x@foo)
}

# Method for existing generic (like print)
S7::method(print, MyClass) <- function(x, ...) {
  cat("MyClass object with foo =", x@foo, "\n")
}
```

### Property Access

```r
# Access via @
model@data_simulation_fn
design@model@brms_model
model@active_backend  # Computed property

# NOT $ (that's S3/list syntax)
# model$data_simulation_fn  # WRONG
```

### Checking Class

```r
# Use S7_inherits or inherits with class name
if (S7::S7_inherits(obj, rctbp_model)) { ... }
if (inherits(obj, "rctbp_model")) { ... }
```

## Migration from S3

1. Convert `list + class attribute` to `new_class()` with properties
2. Replace `UseMethod()` with `new_generic()` + `method()`
3. Change `$` access to `@` access
4. Add validators for input checking

## Resources

- Official S7 docs: https://rconsortium.github.io/S7/
- Vignettes: `vignette("classes-objects")`, `vignette("generics-methods")`
