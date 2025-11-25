# Testing & CI

**Last Updated:** 2025-11-24

## Current Status

**Test suite**: Placeholder files only (0% implemented)

Test files exist in `tests/testthat/` but contain only TODO comments.

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

## Recommended Testing Strategy

### Unit Tests (per component)

1. **Class constructors**
   - Valid inputs create objects
   - Invalid inputs produce clear errors
   - Validators catch edge cases

2. **Class validators**
   - Required fields must be present
   - Types must match specifications
   - Cross-field consistency checks

3. **Simulation functions**
   - Output has correct structure
   - Parameters affect output correctly
   - Edge cases (n=1, extreme values)

4. **Compute functions**
   - Metrics calculated correctly
   - Handle convergence failures
   - Union measures computed properly

### Integration Tests

1. **Full workflow**
   - build_model → build_design → build_conditions → power_analysis
   - Verify results have expected structure
   - Check print and plot methods work

2. **Parallelization**
   - Same results with n_cores=1 and n_cores>1
   - No crashes with parallel execution

3. **Edge cases**
   - Single condition
   - Missing parameters (expect errors)
   - Convergence failures (expect warnings)

### Test File Structure

```
tests/
├── testthat.R
└── testthat/
    ├── test-class_model.R
    ├── test-class_design.R
    ├── test-class_conditions.R
    ├── test-class_power_analysis.R
    ├── test-simulate_single_run.R
    └── test-integration.R
```

## Test Patterns

### Testing S7 Classes

```r
test_that("rctbp_model validates inputs", {
  # Valid creation
  model <- build_model(predefined_model = "ancova_cont_2arms")
  expect_s3_class(model, "rctbp_model")

  # Invalid model name

  expect_error(
    build_model(predefined_model = "nonexistent"),
    "not found"
  )
})
```

### Testing with Long-Running Operations

```r
test_that("power_analysis produces valid results", {
  skip_on_cran()  # Skip slow tests on CRAN

  # Minimal simulation for speed
  result <- power_analysis(conditions, n_sims = 5, n_cores = 1)

  expect_true(nrow(result@summarized_results) > 0)
  expect_true(all(c("power_success", "power_futility") %in%
                  names(result@summarized_results)))
})
```

### Testing Parallel Execution

```r
test_that("parallel produces same results as sequential", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 2)

  set.seed(123)
  result_seq <- power_analysis(conditions, n_sims = 10, n_cores = 1)

  set.seed(123)
  result_par <- power_analysis(conditions, n_sims = 10, n_cores = 2)

  expect_equal(
    result_seq@summarized_results$power_success,
    result_par@summarized_results$power_success,
    tolerance = 0.1  # Some variance expected
  )
})
```

## Running Tests

### During Development

```r
# Run all tests
devtools::test()

# Run specific file
testthat::test_file("tests/testthat/test-class_model.R")

# Run with parallel (if configured)
devtools::test()
```

### During R CMD check

```bash
R CMD check .
```

## Troubleshooting

### Tests Fail in Parallel but Pass Sequential

Check for:
- Shared state between tests
- Race conditions
- Resource contention

**Fix**: Set `PARALLEL_TESTS=false` to debug, then isolate tests.

### Tests Too Slow

- Use `skip_on_cran()` for slow tests
- Minimize `n_sims` in tests (5-10 is usually enough)
- Use model caching where possible
