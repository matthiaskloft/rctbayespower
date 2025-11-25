# Parallel Testing Setup

## Overview
The package now supports parallel test execution using testthat 3.x's built-in parallel testing capabilities.

## Configuration

### Enabling Parallel Testing
Set the `PARALLEL_TESTS` environment variable to `true` in `.Renviron`:

```
PARALLEL_TESTS=true
```

### Disabling Parallel Testing
Set the `PARALLEL_TESTS` environment variable to `false` or remove the line:

```
PARALLEL_TESTS=false
```

## Implementation Details

### Core Count
When parallel testing is enabled, the system automatically:
- Detects the number of available CPU cores using `parallel::detectCores()`
- Uses `available cores - 2` for test execution
- Ensures at least 1 core is used (minimum safeguard)

**Example**: On a 16-core system, tests will run on 14 cores.

### Test Runner Configuration
The test runner (`tests/testthat.R`) checks the `PARALLEL_TESTS` environment variable and:
- **Parallel mode**: Uses `testthat::ParallelProgressReporter` with calculated core count
- **Sequential mode**: Uses standard `testthat::test_check()` execution

## Dependencies
- `testthat` (>= 3.0.0) - Required for parallel testing support
- `withr` - Used for setting local options (added to Suggests)

## Usage

### During Development
```r
# Run tests (respects .Renviron setting)
testthat::test_local('tests/testthat')

# Or use devtools
devtools::test()
```

### During R CMD check
```bash
R CMD check .
```

The parallel testing configuration is automatically applied based on `.Renviron` settings.

## Performance Considerations
- Parallel testing is most beneficial when:
  - You have many test files
  - Individual tests have significant runtime
  - Tests are independent (no shared state)
- For small test suites, sequential mode may be faster due to parallelization overhead

## Troubleshooting
- If tests fail in parallel mode but pass in sequential mode, check for:
  - Shared state between tests
  - Race conditions
  - Resource contention issues
- Set `PARALLEL_TESTS=false` to debug such issues
