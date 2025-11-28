# Code Consistency Review

**Last Updated:** 2025-11-28

This document summarizes the code consistency review across all R files in the package.

## Summary

The codebase demonstrates **strong consistency** across all major dimensions. The package follows established R package development conventions with some project-specific patterns that are consistently applied.

## Consistent Patterns Found

### 1. File Structure

All R files follow a consistent structure:

```r
# =============================================================================
# FILE TITLE IN CAPS
# =============================================================================
# Brief description of file purpose.

# Section code...

# =============================================================================
# NEXT SECTION
# =============================================================================
```

- File-level documentation block at top
- Major sections separated by `# ===...===` headers
- Functions grouped logically (class definition → validators → constructors → methods)

### 2. S7 Class System

All S7 classes follow this pattern:

```r
rctbp_classname <- S7::new_class(
  "rctbp_classname",
  properties = list(
    # Properties with types/defaults
    prop1 = S7::new_property(S7::class_numeric, default = 0),
    prop2 = S7::class_character | NULL,
    # Computed properties
    computed = S7::new_property(getter = function(self) ...)
  ),
  validator = function(self) {
    # Validation with cli::cli_abort()
    if (invalid) cli::cli_abort(c("message", "x" = "...", "i" = "..."))
    NULL  # Return NULL if valid
  }
)
```

### 3. Documentation (Roxygen2)

All exported functions include:
- `#' Title in Sentence Case`
- `#' Description paragraph`
- `#' @param name Description (default value)`
- `#' @return Description`
- `#' @export`
- `#' @examples` or `#' @examples \dontrun{}`

Internal functions include:
- Same structure minus `@export`
- `#' @keywords internal`

Cross-references use modern syntax: `[function_name()]` (not `\code{\link{}}`)

### 4. Naming Conventions

| Element | Convention | Examples |
|---------|------------|----------|
| S7 classes | `rctbp_*` prefix | `rctbp_model`, `rctbp_design` |
| Power columns | `pwr_*` prefix | `pwr_scs`, `pwr_ftl` |
| Probability columns | `pr_*` prefix | `pr_scs`, `pr_ftl` |
| Decision columns | `dec_*` prefix | `dec_scs`, `dec_ftl` |
| Standard error columns | `se_*` prefix | `se_pwr_scs`, `se_pr_ftl` |
| Success suffix | `_scs` | `pwr_scs`, `pr_scs` |
| Futility suffix | `_ftl` | `pwr_ftl`, `pr_ftl` |
| Functions | snake_case | `build_model`, `power_analysis` |
| Parameters | snake_case | `n_total`, `p_sig_scs` |
| Internal helpers | snake_case | `create_error_result` |

### 5. Error Handling

All errors use `cli::cli_abort()` with structured messages:

```r
cli::cli_abort(c(
  "Main error message",
  "x" = "What went wrong",
  "i" = "Helpful hint or suggestion"
))
```

Warnings use `cli::cli_warn()` with the same structure.

### 6. Code Style

| Pattern | Standard |
|---------|----------|
| Pipe operator | Base R `\|>` (never magrittr `%>%`) |
| Package calls | `package::function()` (explicit namespacing) |
| Assignment | `<-` (not `=`) |
| Booleans | `TRUE`/`FALSE` (not `T`/`F`) |
| S7 checks | Both `rctbp_*` and `rctbayespower::rctbp_*` |

### 7. Validation Pattern

Functions validate inputs at the start:

```r
function_name <- function(x, y) {
  # Validate x
  if (!is.numeric(x) || x <= 0) {
    cli::cli_abort(c(
      "{.arg x} must be a positive number",
      "x" = "You supplied {.val {x}}",
      "i" = "Provide a numeric value > 0"
    ))
  }

  # Main logic...
}
```

## File Organization

### Core Classes (`R/class_*.R`)

| File | Purpose | Lines |
|------|---------|-------|
| `class_model.R` | Model with dual backend support | ~500 |
| `class_design.R` | Design configuration | ~350 |
| `class_conditions.R` | Simulation conditions | ~400 |
| `class_power_analysis.R` | Power analysis + run() | ~1500 |
| `class_interim.R` | Interim analysis (deprecated) | ~10 |

### Backend System

| File | Purpose |
|------|---------|
| `backend_brms.R` | brms estimation functions |
| `backend_bf.R` | BayesFlow estimation (reticulate) |
| `worker_functions.R` | Parallel worker dispatch |
| `model_cache.R` | Model caching system |
| `utils_results.R` | Error result helpers |

### Models

| File | Purpose |
|------|---------|
| `models_ancova.R` | ANCOVA model builders + batch simulation |

### Boundaries

| File | Purpose |
|------|---------|
| `boundaries.R` | Stopping boundary functions (OBF, Pocock, linear, power) |

### Plotting (`R/plot_*.R`)

| File | Purpose |
|------|---------|
| `plot_power_analysis.R` | Main S7 plot method + dispatcher |
| `plot_power_curve.R` | Power curve visualization |
| `plot_heatmap.R` | 2D heatmap visualization |
| `plot_comparison.R` | Power vs probability comparison |
| `plot_helpers.R` | Shared utilities (pivot, colors, theme) |

### Reporting (`R/report_*.R`)

| File | Purpose |
|------|---------|
| `report_builders.R` | Build structured report data |
| `report_renderers.R` | CLI/Markdown rendering |

### Utilities

| File | Purpose |
|------|---------|
| `compute_measures.R` | Posterior measure computation |
| `verbosity.R` | Three-level verbosity control (0, 1, 2) |
| `output_system.R` | CLI/Markdown dual-mode output |
| `MCSE.R` | Monte Carlo standard error calculations |
| `required_fn_args.R` | Parameter requirement analysis |
| `S7_helpers.R` | S7 utility functions |
| `s3_wrappers.R` | S3 method wrappers for S7 classes |

### Python Integration

| File | Purpose |
|------|---------|
| `python_simulators.R` | Python simulator loading (reticulate) |

### Package Infrastructure

| File | Purpose |
|------|---------|
| `zzz.R` | .onLoad hook (S7 registration) |
| `rctbayespower-package.R` | Package docs + globalVariables |

## Minor Inconsistencies

### 1. Verbosity Property Naming

- Current: `verbosity` property (0, 1, 2 levels)
- Some older code may reference `verbose` (boolean)
- **Recommendation**: Already standardized to `verbosity`

### 2. Section Comment Styles

Most files use:
```r
# =============================================================================
# SECTION TITLE
# =============================================================================
```

A few internal sections use:
```r
# --- Subsection ---
```

This is acceptable as it provides visual hierarchy.

### 3. Report Function Output

Per CLAUDE.md guidelines, multi-section reports should use single `cat()` calls.
- `report_renderers.R` correctly implements this
- Print methods in `class_power_analysis.R` use `cli::` calls (acceptable for status output)

## Recommendations

1. **Maintain current patterns** - The codebase is consistent and well-structured
2. **Continue using `verbosity`** (0, 1, 2) over boolean `verbose`
3. **Keep section header style** - `# ===...===` for major sections
4. **Follow validation pattern** - cli::cli_abort() with structured messages
5. **Document new functions** - Follow established roxygen2 template

## Verification Checklist

When adding new code, verify:

- [ ] File has header comment block
- [ ] Major sections use `# ===...===` separators
- [ ] Functions have complete roxygen2 documentation
- [ ] Errors use `cli::cli_abort()` with `"x"` and `"i"` bullets
- [ ] Base R `|>` pipe used (not `%>%`)
- [ ] Package functions explicitly namespaced
- [ ] Parameter validation at function start
- [ ] Naming follows conventions (snake_case, `_scs`/`_ftl` suffixes)
