# S7 Class System

**Last Updated:** 2025-11-29

## Why S7?

- **Property Access**: Clean `@` syntax (e.g., `design@sim_fn`)
- **Validation**: Built-in validators ensure object integrity
- **Composition**: Easy nesting (conditions contain design)
- **Modern R**: S7 is the recommended OOP system for new packages

## Class Definitions

### `rctbp_design` (R/class_design.R) - Primary Class

**Purpose**: The main user-facing class. Contains model properties (simulation function, inference model, backend) merged with analysis decision criteria.

| Property | Type | Description |
|----------|------|-------------|
| `model_name` | character/NULL | Predefined model name (e.g., "ancova_cont_2arms") |
| `sim_fn` | function | Data simulation function |
| `inference_model` | brmsfit/NULL | Pre-compiled brms template |
| `bf_model` | any/NULL | BayesFlow model (Keras/Workflow) |
| `backend` | character | "brms" (default) or "bf" |
| `target_params` | character | Parameters to analyze |
| `par_names_inference` | **computed** | Parameters in inference model |
| `par_names_sim` | **computed** | Parameters in simulation function |

**Constructor**: `build_design(model_name, target_params, backend, ...)`

**Validator**: Checks that `target_params` exist in the model's parameter names.

**Example**:
```r
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2",
  backend = "brms"  # or "bf"
)

# Discover available parameters
design@par_names_inference  # For target_params
design@par_names_sim        # For simulation args
```

### `rctbp_model` (R/class_model.R) - DEPRECATED

> **Note**: This class is deprecated as of 2025-11. All model properties have been merged into `rctbp_design`. This file is kept only for backward compatibility.

**Old API** (deprecated):
```r
# Old way (deprecated)
model <- get_model("ancova_cont_2arms")
design <- build_design(model = model, target_params = "b_arm2")

# New way
design <- build_design(model_name = "ancova_cont_2arms", target_params = "b_arm2")
```

### `rctbp_conditions` (R/class_conditions.R)

**Purpose**: Generates parameter combinations for power analysis grid.

| Property | Type | Description |
|----------|------|-------------|
| `design` | rctbp_design | The analysis design |
| `conditions_grid` | data.frame | All condition combinations |
| `condition_arguments` | list | Argument lists per condition |
| `crossed` | list | Varying params (Cartesian product) |
| `linked` | list | Linked params (via `link()`) |
| `constant` | list | Constant params for all conditions |

**Constructor**: `build_conditions(design, crossed, linked, constant)`

**Key Features**:
- Use `crossed` for parameters that vary across conditions (creates Cartesian product)
- Use `link()` within crossed to co-vary parameters 1-to-1
- Use `constant` for parameters that stay the same across all conditions
- Separates arguments into `sim_args` and `decision_args` based on function signatures

**Example**:
```r
conditions <- build_conditions(
  design = design,
  crossed = list(
    n_total = c(100, 200),
    b_arm_treat = c(0.3, 0.5)
  ),
  constant = list(
    p_alloc = list(c(0.5, 0.5)),
    intercept = 0, b_covariate = 0.3, sigma = 1,
    p_sig_scs = 0.975, p_sig_ftl = 0.5,
    thresh_scs = 0.2, thresh_ftl = 0
  )
)
```

### `rctbp_power_analysis` (R/class_power_analysis.R)

**Purpose**: Configures and executes power analysis simulations.

| Property | Type | Description |
|----------|------|-------------|
| `conditions` | rctbp_conditions | Conditions to evaluate |
| `n_sims` | numeric | Simulations per condition (default: 1) |
| `n_cores` | numeric | Parallel cores (default: 1) |
| `verbosity` | numeric | Verbosity level: 0, 1, or 2 (default: 1) |
| `brms_args` | list/NULL | Arguments passed to brms |
| `bf_args` | list/NULL | Arguments passed to BayesFlow |
| `design_prior` | character/function/NULL | Optional design prior |
| `design` | **computed** | Convenience access to conditions@design |
| `results_conditions` | data.frame | Aggregated results (filled after run) |
| `results_interim` | data.frame | Per-look interim results (sequential designs) |
| `results_raw` | data.frame | Individual simulation results (filled after run) |
| `elapsed_time` | difftime | Runtime (filled after run) |

**Constructor**: `power_analysis(conditions, n_sims, n_cores, ..., run = TRUE)`

**Execution**: `run(power_object)` - S7 generic method

**Returns**: Modified S7 object with results populated in properties

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
design@sim_fn
design@inference_model
design@par_names_inference  # Computed property
result@results_conditions   # After run()

# NOT $ (that's S3/list syntax)
# design$sim_fn  # WRONG
```

### Checking Class

```r
# Use S7_inherits or inherits with class name
if (S7::S7_inherits(obj, rctbp_design)) { ... }
if (inherits(obj, "rctbp_design")) { ... }
```

## Migration from S3

1. Convert `list + class attribute` to `new_class()` with properties
2. Replace `UseMethod()` with `new_generic()` + `method()`
3. Change `$` access to `@` access
4. Add validators for input checking

## Resources

- Official S7 docs: https://rconsortium.github.io/S7/
- Vignettes: `vignette("classes-objects")`, `vignette("generics-methods")`
