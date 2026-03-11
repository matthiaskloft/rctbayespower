# Pre-Ship Review Checklist

## Code Quality

- [ ] Functions follow snake_case naming
- [ ] S7 classes prefixed with `rctbp_*`
- [ ] Column names follow convention: `pwr_*`, `pr_*`, `dec_*`, `se_*`
- [ ] Explicit namespacing: `package::function()`
- [ ] No `%>%` pipe — use `|>`
- [ ] Parameter validation at function start
- [ ] No dead code, commented-out blocks, or unused variables
- [ ] No over-engineering (abstractions used only once, premature optimization)

## Documentation

- [ ] All exported functions have roxygen docs
- [ ] `@param` entries match function signature
- [ ] No `\code{\link{}}` — use `[fn()]` syntax
- [ ] Default values not duplicated in `@param` and signature
- [ ] `devtools::document()` runs cleanly

## Testing

- [ ] New functions have tests
- [ ] Edge cases covered (NULL, empty, boundary values)
- [ ] S7 validator tests for invalid inputs
- [ ] `skip_on_cran()` for slow tests
- [ ] No flaky tests introduced
- [ ] Test file named `test-{component}.R`

## Package Integrity

- [ ] `devtools::test()` — all tests pass
- [ ] `devtools::check()` — no errors, warnings, or notes
- [ ] No new global variable warnings (add to `R/globals.R` if needed)

## Integration

- [ ] Changes work with both brms and BayesFlow backends (if applicable)
- [ ] Pipeline methods (print, summary, plot) work on new objects
- [ ] Existing public API not broken

## Dev Docs

- [ ] `dev/99_dev_todos.md` updated with any out-of-scope items
- [ ] Relevant `dev/*.md` docs updated if architecture changed
