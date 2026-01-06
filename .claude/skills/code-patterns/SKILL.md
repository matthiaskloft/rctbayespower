---
name: code-patterns
description: Essential code patterns for rctbayespower. Use when writing new code, adding functions, or creating R files. Includes naming conventions, file headers, error handling, and S7 class templates.
---

# rctbayespower Code Patterns

## Naming Conventions

| Element | Pattern | Example |
|---------|---------|---------|
| S7 classes | `rctbp_*` | `rctbp_design` |
| Power columns | `pwr_*` | `pwr_eff` |
| Probability columns | `pr_*` | `pr_fut` |
| Decision columns | `dec_*` | `dec_eff` |
| Effect thresholds | `thr_fx_*` | `thr_fx_eff` |
| Decision thresholds | `thr_dec_*` | `thr_dec_eff` |
| Standard errors | `se_*` | `se_pwr_eff` |
| Functions/params | snake_case | `build_design` |

## File Header

All R files must start with:

```r
# =============================================================================
# FILE TITLE IN CAPS
# =============================================================================
# Brief description.

# Code...

# =============================================================================
# NEXT SECTION
# =============================================================================
```

## Error Handling

Use `cli::cli_abort()` with structured messages:

```r
cli::cli_abort(c(
  "Main error message",
  "x" = "What went wrong: {.val {value}}",
  "i" = "Helpful suggestion"
))
```

## S7 Class Template

```r
rctbp_name <- S7::new_class(
  "rctbp_name",
  properties = list(
    prop = S7::class_numeric
  ),
  validator = function(self) {
    if (invalid) cli::cli_abort(c("msg", "x" = "...", "i" = "..."))
    NULL
  }
)
```

## Code Style

- Pipe: Base R `|>` (not magrittr `%>%`)
- Namespacing: `package::function()` (explicit)
- Assignment: `<-` (not `=`)
- Booleans: `TRUE`/`FALSE` (not `T`/`F`)

## Full Reference

For complete patterns: `dev/11_code_consistency_review.md`
