# S7 Class System

**Last Updated:** 2025-11-24

## Why S7?

- **Property Access**: Clean `@` syntax (e.g., `model@data_simulation_fn`)
- **Validation**: Built-in validators ensure object integrity
- **Composition**: Easy nesting (design contains model, conditions contain design)
- **Modern R**: S7 is the recommended OOP system for new packages

## Class Definitions

### `rctbp_model` (R/class_model.R)

**Purpose**: Encapsulates everything needed to simulate data and fit a Bayesian model.

| Property | Type | Description |
|----------|------|-------------|
| `data_simulation_fn` | function | Generates trial data |
| `brms_model` | brmsfit/NULL | Pre-compiled brms template (for brms backend) |
| `bayesflow_model` | any/NULL | Neural posterior model (for NPE backend, placeholder) |
| `backend_args` | list | Backend-specific arguments (default: empty list) |
| `predefined_model` | character/NULL | Name if predefined |
| `model_name` | character | Descriptive name |
| `n_endpoints` | numeric | Number of outcomes |
| `endpoint_types` | character | Types (continuous, binary) |
| `n_arms` | numeric | Treatment arms |
| `n_repeated_measures` | numeric/NULL | Time points |
| `backend` | computed | "brms" or "npe" based on which model is set |
| `parameter_names_sim_fn` | computed | Auto-extracted from sim function |
| `parameter_names_brms` | computed | Auto-extracted from brms model |

**Constructor**: `build_model(predefined_model)` or direct construction for custom models

### `rctbp_design` (R/class_design.R)

**Purpose**: Combines a model with analysis decision criteria.

| Property | Type | Description |
|----------|------|-------------|
| `model` | rctbp_model | The statistical model |
| `target_params` | character | Parameters to analyze |
| `p_sig_success` | numeric | Probability threshold for success (default 0.975) |
| `p_sig_futility` | numeric | Probability threshold for futility (default 0.5) |
| `design_name` | character/NULL | Optional name |

**Constructor**: `build_design(model, target_params, ...)`

**Validator**: Checks that `target_params` exist in the model's parameter names.

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
| `design` | computed | Convenience access to conditions@design |
| `summarized_results` | data.frame | Aggregated results (filled after run) |
| `raw_results` | data.frame | Individual simulation results (filled after run) |
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
