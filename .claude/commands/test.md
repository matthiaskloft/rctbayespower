# Run Tests

Execute the test suite and report results.

## Steps

1. Run `devtools::test()` to execute all tests
2. Parse output for failures and errors
3. For each failure:
   - Report file:line reference
   - Read the failing test
   - Analyze expected vs actual result
   - Suggest fix (in test or in code)
4. Report summary: X passed, Y failed, Z skipped

## Running Specific Tests

```r
# Run tests in a specific file
devtools::test(filter = "filename")

# Run tests matching a pattern
devtools::test(filter = "pattern")
```

## Common Failure Patterns

| Pattern | Likely Cause |
|---------|--------------|
| `expect_equal` fail | Return value changed |
| `expect_error` fail | Error message changed or no longer thrown |
| `expect_s7_class` fail | Class not properly created/exported |
| Timeout | Long-running brms model (use mock mode) |

## Success Criteria

- All tests pass
- No skipped tests (unless intentional)
