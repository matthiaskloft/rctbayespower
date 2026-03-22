# Plan: as.data.frame() methods for core classes

**Created**: 2026-03-22
**Author**: Claude (brainstorm with user)

## Status

| Phase | Status | Date | Notes |
|-------|--------|------|-------|
| Spec | DONE | 2026-03-22 | |
| Plan | DONE | 2026-03-22 | |
| Phase 1: Methods + Tests | MERGED | 2026-03-22 | PR #28 |
| Ship | MERGED | 2026-03-22 | |

## Spec

### Summary

**Motivation**: Users frequently need to extract tabular data from rctbayespower result objects for downstream analysis, plotting (ggplot2), or export. Currently they must know the internal property names (`pa@results_conditions`, `pr@pareto_front`, etc.). An `as.data.frame()` method provides the standard R idiom for this conversion.

**Outcome**: Users can call `as.data.frame()` on `rctbp_power_analysis`, `rctbp_conditions`, and `rctbp_pareto_result` objects to get a data.frame. A `what` argument selects sub-tables where multiple exist.

### Requirements

1. `as.data.frame(pa)` returns `results_conditions` by default
2. `as.data.frame(pa, what = "raw")` returns `results_raw`
3. `as.data.frame(pa, what = "interim")` returns `results_interim`; errors via `cli::cli_abort()` if `!x@has_interim`
4. `as.data.frame(cond)` returns `conditions@grid`
5. `as.data.frame(pr)` returns `pareto_front` by default
6. `as.data.frame(pr, what = "archive")` returns the full evaluation archive
7. `as.data.frame(pr, what = "selected")` returns `pr@selected_design`
8. `as.data.frame(pr, what = "convergence")` returns convergence trace (may be empty `data.frame()` for pre-run objects)
9. Invalid `what` values produce `cli::cli_abort()` with valid options listed
10. All methods return plain `data.frame` (not tibble), consistent with package internals
11. Data is returned as-is from the stored properties — no column merging or transformation

### Design Decisions

| Decision | Options | Chosen | Rationale |
|----------|---------|--------|-----------|
| Classes in scope | All 4 core classes / 3 without design | 3 classes (no `rctbp_design`) | `rctbp_design` is scalar metadata, not naturally tabular |
| Default for power_analysis | results_conditions / results_raw / require `what` | results_conditions | Most common use case: aggregated power per condition |
| Default for pareto_result | pareto_front / archive / selected_design | pareto_front | Most useful for analysis; `summary()` already returns this |
| Sub-table argument name | `which` / `what` / `type` | `what` | Avoids collision with base `which()` |
| Invalid `what` handling | Error / warning+fallback | Error via `cli::cli_abort()` | Fail fast, consistent with package validation patterns |
| Interim not available | Error / empty data.frame | Error via `cli::cli_abort()` | Clear signal; fixed designs have no interim results |
| Pareto default keyword | `"pareto_front"` / `"pareto"` / `"front"` | `"pareto"` | Consistent with short one-word keywords used elsewhere |
| Return type | data.frame / tibble | data.frame | Consistent with package internals |

### Scope

#### In Scope
- `as.data.frame()` S3 methods for `rctbp_power_analysis`, `rctbp_conditions`, `rctbp_pareto_result`
- `what` argument for selecting sub-tables
- Input validation with informative error messages via `cli::cli_abort()`
- S3 wrapper functions in `s3_wrappers.R` (required for S7/S3 dispatch)
- Unit tests for all methods and error paths
- Roxygen documentation

#### Out of Scope
- `as.data.frame()` for `rctbp_design` (scalar metadata)
- `as_tibble()` methods (can be added later if requested)
- Column transformations or merging beyond what's already stored
- Vignette updates (methods are discoverable via `?as.data.frame`)

### Architecture Overview

Two-part registration required (per package convention in `s3_wrappers.R`):

1. **S7 method definition** in respective `class_*.R` files (with `#' @name` and `#' @export` roxygen):
```r
#' @name as.data.frame.rctbp_power_analysis
#' @export
S7::method(as.data.frame, rctbp_power_analysis) <- function(x, ...,
    what = c("conditions", "raw", "interim")) {
  what <- match.arg(what)
  ...
}
```

2. **S3 wrapper** in `R/s3_wrappers.R` with `#' @export` — must explicitly declare and forward `what` (not rely on `...` alone), per `plot.rctbp_pareto_result` precedent:
```r
#' @rdname as.data.frame.rctbp_power_analysis
#' @export
as.data.frame.rctbp_power_analysis <- function(x, ...,
    what = c("conditions", "raw", "interim")) {
  S7::method(as.data.frame, rctbp_power_analysis)(x, ..., what = what)
}
```

**`what` value → property mapping:**

| Class | `what` value | Property |
|-------|-------------|----------|
| `rctbp_power_analysis` | `"conditions"` (default) | `x@results_conditions` |
| `rctbp_power_analysis` | `"raw"` | `x@results_raw` |
| `rctbp_power_analysis` | `"interim"` | `x@results_interim` (guarded by `x@has_interim`) |
| `rctbp_pareto_result` | `"pareto"` (default) | `x@pareto_front` |
| `rctbp_pareto_result` | `"archive"` | `x@archive` |
| `rctbp_pareto_result` | `"selected"` | `x@selected_design` |
| `rctbp_pareto_result` | `"convergence"` | `x@convergence` |
| `rctbp_conditions` | _(no argument)_ | `x@grid` |

**File placement:**
- S7 method definitions: `R/class_power_analysis.R`, `R/class_conditions.R`, `R/class_pareto_result.R`
- S3 wrappers: `R/s3_wrappers.R`
- Tests: `tests/testthat/test-as-data-frame.R`

### Constraints

- Declare `what` default as a character vector of all valid options (e.g., `what = c("conditions", "raw", "interim")`) so `match.arg(what)` works correctly; first element is the default
- Use `cli::cli_abort()` for error messages (package convention — `rlang::abort()` is not used)
- Use `x@has_interim` computed property for interim availability check (DRY)
- `rctbp_conditions` validator guarantees `nrow(grid) >= 1` — no empty guard needed
- `rctbp_power_analysis` results properties have no default; tests must construct objects with explicit `data.frame()` values (see `mock_power_analysis()` pattern)
- `rctbp_pareto_result` properties default to `data.frame()` — returning empty frames is valid

### Open Questions

- None — all decisions resolved during brainstorm.

## Implementation Plan

### Phase 1: Methods + Tests

Small enough for a single phase — all three methods, wrappers, and tests.

**Files to modify:**
- `R/class_power_analysis.R` — add S7 `as.data.frame` method with `what = c("conditions", "raw", "interim")`; include `#' @name` and `#' @export` roxygen
- `R/class_conditions.R` — add S7 `as.data.frame` method (returns `x@grid`); include `#' @name` and `#' @export` roxygen
- `R/class_pareto_result.R` — add S7 `as.data.frame` method with `what = c("pareto", "archive", "selected", "convergence")`; include `#' @name` and `#' @export` roxygen
- `R/s3_wrappers.R` — add 3 S3 wrapper functions with `#' @export`; wrappers for `rctbp_power_analysis` and `rctbp_pareto_result` must explicitly declare and forward `what` (not rely on `...` alone)
- `tests/testthat/helper-mock-objects.R` — add `mock_pareto_result()` helper

**Files to create:**
- `tests/testthat/test-as-data-frame.R` — tests for all methods

**Tests:**
- `as.data.frame(pa)` returns `results_conditions` (default)
- `as.data.frame(pa, what = "raw")` returns `results_raw`
- `as.data.frame(pa, what = "interim")` returns `results_interim` (accrual variant)
- `as.data.frame(pa, what = "interim")` errors for fixed design (`!has_interim`)
- `as.data.frame(pa, what = "invalid")` errors with valid options listed
- `as.data.frame(cond)` returns `conditions@grid`
- `as.data.frame(pr)` returns `pareto_front` (default)
- `as.data.frame(pr, what = "archive")` returns `archive`
- `as.data.frame(pr, what = "selected")` returns `selected_design`
- `as.data.frame(pr, what = "convergence")` returns `convergence`
- `as.data.frame(pr, what = "invalid")` errors
- All returns are `data.frame` class (not tibble)
- Return values are identical to direct property access

**Steps:**
1. Add `mock_pareto_result()` to `tests/testthat/helper-mock-objects.R` — minimal `rctbp_pareto_result` with non-empty `pareto_front`, `archive`, `selected_design`, and empty `convergence`
2. Add S7 method for `rctbp_conditions` in `class_conditions.R` (simplest — no `what` arg); include `#' @name as.data.frame.rctbp_conditions` and `#' @export` roxygen
3. Add S7 method for `rctbp_power_analysis` in `class_power_analysis.R` with `what = c("conditions", "raw", "interim")`, `match.arg(what)`, and `has_interim` guard via `cli::cli_abort()`; include roxygen block
4. Add S7 method for `rctbp_pareto_result` in `class_pareto_result.R` with `what = c("pareto", "archive", "selected", "convergence")` and `match.arg(what)`; include roxygen block
5. Add 3 S3 wrappers in `s3_wrappers.R` — wrappers with `what` must explicitly declare and forward it (e.g., `function(x, ..., what = c(...)) { S7::method(...)(x, ..., what = what) }`)
6. Run `devtools::document()` to update NAMESPACE with `S3method(as.data.frame, ...)` entries
7. Write tests in `test-as-data-frame.R` using `mock_power_analysis()`, `mock_conditions()`, and `mock_pareto_result()` helpers
8. Run `devtools::test()` and `devtools::check()`

## Verification & Validation

- **Automated**: Unit tests for each method, each `what` value, error paths (invalid `what`, no interim); R CMD check
- **Manual**: REPL verification with example power analysis objects
- **Numerical**: Confirm `as.data.frame(pa)` is identical to `pa@results_conditions`

## Dependencies

- None — uses existing class infrastructure and base R generic

## Notes

_Living section — updated during implementation._

## Review Feedback

Reviewed by code-architect agent (2026-03-22). 10 findings: 3 blockers, 4 warnings, 3 suggestions. All addressed:

- **F1 (BLOCKER)**: Replaced `rlang::arg_match()` with `match.arg()` — `rlang::arg_match()` not used anywhere in codebase
- **F2 (BLOCKER)**: Scoped "empty results" testing to objects constructed with explicit `data.frame()` args; noted `rctbp_power_analysis` has no defaults for result properties
- **F3 (BLOCKER)**: Added S3 wrapper requirement in `s3_wrappers.R` with `#' @export` — required for S7/S3 dispatch per package pattern
- **F4 (WARNING)**: Added explicit `what` value → property mapping table
- **F5 (WARNING)**: Changed default from `"pareto_front"` to `"pareto"` (user chose this)
- **F6 (WARNING)**: Specified use of `x@has_interim` computed property for interim guard
- **F7 (WARNING)**: Replaced all `rlang::abort()` references with `cli::cli_abort()`
- **F8 (SUGGESTION)**: Noted `rctbp_conditions` validator guarantees non-empty grid
- **F9 (SUGGESTION)**: Documented that empty `data.frame()` is valid for `convergence` on pre-run objects
- **F10 (SUGGESTION)**: Added explicit file placement for methods, wrappers, and tests

### Plan Review (iteration 1)

Reviewed by code-architect agent (2026-03-22). 6 findings: 2 blockers, 3 warnings, 1 suggestion. All addressed:

- **F1 (BLOCKER)**: `match.arg(what)` with scalar default won't validate choices; changed to declare `what` as character vector of all valid options (first element is default)
- **F2 (BLOCKER)**: `mock_pareto_result()` helper missing from test helpers; added as explicit step 1
- **F3 (WARNING)**: S3 wrappers must explicitly declare and forward `what`; updated Architecture Overview and step 5 to match `plot.rctbp_pareto_result` precedent
- **F4 (WARNING)**: Roxygen `#' @name` and `#' @export` blocks needed on S7 method definitions; added to steps 2-4
- **F5 (WARNING)**: Noted as duplicate of F3 (same underlying issue)
- **F6 (SUGGESTION)**: `mock_pareto_result()` now an explicit step

Plan reviewed in 1 iteration.
