---
name: r-cmd-check
description: Fix rctbayespower R CMD check issues. Use when R CMD check has errors, warnings, or notes. Includes globalVariables location, roxygen fixes, and common patterns.
---

# Fixing R CMD Check Issues

## globalVariables

NSE variables causing "no visible binding" notes go in:

**File:** `R/rctbayespower-package.R`

```r
utils::globalVariables(c(
  "n_total", "pwr_eff", "pr_fut",
  "new_var"  # Add new variables here
))
```

## Roxygen Defaults Mismatch

**Problem:** `@param x (default Y)` doesn't match `function(x = Z)`

```r
# Wrong - mismatch
#' @param threshold Threshold (default 0.95)
my_fn <- function(threshold = 0.975) { }

# Correct - synced
#' @param threshold Threshold (default 0.975)
my_fn <- function(threshold = 0.975) { }
```

## Roxygen Link Syntax

```r
# Wrong (deprecated)
\code{\link{power_analysis}}

# Correct (modern)
[power_analysis()]
```

## After Making Fixes

```r
devtools::document()  # Regenerate .Rd files
devtools::check()     # Verify fixes
```

## Common Issues Quick Reference

| Issue | Fix |
|-------|-----|
| "no visible binding" | Add to `globalVariables()` in `R/rctbayespower-package.R` |
| "Codoc mismatches" | Sync roxygen `@param` defaults with function signature |
| "S3 method not exported" | Add `@export` to roxygen |
| Unexpected files | Add to `.Rbuildignore` |

## Full Reference

For all guidelines: `dev/04_development_guidelines.md`
