# Testing & CI

**Last Updated:** 2026-03-06

## Current Status

**Test suite**: ~350 tests across 12 files. Covers S7 classes, boundaries, compute measures, output system, verbosity, backend (mock mode), and utilities. No integration tests yet (require brms compilation or BayesFlow).

## Test Files

| File | Tests | What it covers |
|------|-------|----------------|
| `helper-mock-objects.R` | — | Mock factories: brmsfit, sim_fn, design, conditions, raw_results |
| `helper-cli.R` | — | `expect_cli_abort`, `expect_cli_warn`, `capture_cli`, `expect_output_contains` |
| `test-backend_bf.R` | 34 | BayesFlow backend (mock mode) |
| `test-boundaries.R` | 63 | All 6 boundary functions + resolve helpers |
| `test-class_conditions.R` | 56 | `link()`, `build_conditions` (full coverage) |
| `test-class_design.R` | 40 | S7 validators, properties, predefined models |
| `test-class_sim_fn.R` | 38 | `build_sim_fn`, schema derivation, validation |
| `test-compute_measures.R` | 38 | `summarize_sims`, interim summaries, MCSE |
| `test-output_system.R` | 18 | Output mode switching, markdown helpers |
| `test-required_fn_args.R` | 25 | Arg discovery, `show_condition_args` |
| `test-s3_methods.R` | 7 | S3 print method dispatch + content checks |
| `test-s7_helpers.R` | 3 | `update_S7_with_dots` |
| `test-utilities.R` | 4 | `format_duration`, `get_cpu_info` |
| `test-verbosity.R` | 25 | Three-level verbosity control |

## Not Yet Covered

- `R/worker_functions.R` (parallel dispatch — needs real brms)
- `R/backend_brms.R` (needs brms compilation)
- `R/class_power_analysis.R` (needs full simulation run)
- `R/pareto_optimize.R` and optimization wrappers
- `R/plot_*.R` (plot output)
- `R/model_cache.R`
- `R/setup_python.R`

## Mock Object System

All tests use lightweight mocks defined in `helper-mock-objects.R` that bypass expensive operations:

- **`mock_brmsfit()`** — Minimal list with `class = "brmsfit"`, no compilation
- **`mock_sim_fn()`** — Function with `NULL` defaults (matches real ANCOVA pattern so `get_args_without_defaults()` treats them as "required")
- **`mock_rctbp_sim_fn()`** — Full `rctbp_sim_fn` via `build_sim_fn()` with cached test output
- **`mock_design()`** — Direct `rctbp_design()` constructor call, bypasses `build_design()`
- **`mock_conditions()`** — Direct `rctbp_conditions()` constructor, bypasses `build_conditions()`
- **`mock_raw_results()`** — Deterministic data.frame with evenly-spaced values (no randomness)

## Test Infrastructure (helper-cli.R)

| Helper | Purpose |
|--------|---------|
| `expect_cli_abort(expr)` | Expects `rlang_error` class (from `cli::cli_abort()`) |
| `expect_cli_warn(expr)` | Expects `rlang_warning` class (from `cli::cli_warn()`) |
| `capture_cli(expr)` | Captures both stdout and stderr (CLI outputs to messages) |
| `expect_output_contains(expr, expected)` | Checks captured output contains strings |
| `expect_markdown_output(object, expected)` | Tests markdown output mode |

## Key Testing Gotchas

### S7 Class Names Require Namespace

```r
# WRONG — inherits() returns FALSE for bare S7 class names
expect_s3_class(obj, "rctbp_design")

# CORRECT — S7 registers classes with namespace prefix
expect_s3_class(obj, "rctbayespower::rctbp_design")
```

### CLI Output Goes to stderr

```r
# WRONG — capture.output(type = "output") misses CLI output
output <- capture.output(print(obj), type = "output")

# CORRECT — use capture_cli() which captures both channels
output <- capture_cli(print(obj))
```

### Mock sim_fn Defaults Must Be NULL

```r
# WRONG — real defaults make params disappear from params_sim
function(n_total, p_alloc, b_arm_treat = 0, sigma = 1) { ... }

# CORRECT — NULL defaults are treated as "required but configurable"
function(n_total, p_alloc, b_arm_treat = NULL, sigma = NULL) { ... }
```

### Error/Warning Classes

```r
# WRONG — outdated class names
expect_error(expr, class = "rlib_error_3_0")
expect_warning(expr, class = "rlib_warning_3_0")

# CORRECT — current rlang class names
expect_error(expr, class = "rlang_error")
expect_warning(expr, class = "rlang_warning")
```

## Parallel Testing Support

### Enabling Parallel Tests

Set in `.Renviron`:
```
PARALLEL_TESTS=true
```

### Configuration

- Uses `available cores - 2` for test execution
- Minimum 1 core safeguard
- Requires `testthat` >= 3.0.0

### How It Works

The test runner (`tests/testthat.R`) checks `PARALLEL_TESTS`:
- **Parallel mode**: Uses `testthat::ParallelProgressReporter`
- **Sequential mode**: Standard `testthat::test_check()`

## Running Tests

### During Development

```r
# Run all tests
devtools::test()

# Run specific file
testthat::test_file("tests/testthat/test-boundaries.R")

# Run with parallel (if configured)
devtools::test()
```

### During R CMD check

```bash
R CMD check .
```

## Windows-Specific Issues

- **CLI reporter crash**: `devtools::test()` sometimes crashes with paste overflow in `rule_left`. Workaround: use `reporter = "summary"` or `reporter = "check"`.
- **Transient Rscript segfaults**: Long `-e` inline strings can segfault. Workaround: write to temp `.R` script file and run that instead.
