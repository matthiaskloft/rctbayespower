# Plan: boundary_wang_tsiatis()

**Created**: 2026-03-22
**Author**: Claude (brainstorm with user)

## Status

| Phase | Status | Date | Notes |
|-------|--------|------|-------|
| Spec | DONE | 2026-03-22 | All blockers resolved |
| Plan | DONE | 2026-03-22 | Single phase |
| Phase 1: Function, tests, docs | MERGED | 2026-03-23 | PR #29 |
| Ship | MERGED | 2026-03-23 | |

## Spec

### Summary

**Motivation**: The Wang-Tsiatis boundary family is a one-parameter generalization that subsumes O'Brien-Fleming and Pocock as special cases. Users who want to tune the aggressiveness of early stopping between these extremes currently have no principled option with proper alpha-spending support. `boundary_power()` approximates the shape in Bayesian mode but lacks frequentist alpha control.

**Outcome**: Users can call `boundary_wang_tsiatis(delta, alpha = ...)` for frequentist alpha-spending or `boundary_wang_tsiatis(delta, threshold = ...)` for Bayesian threshold mode. The `delta` parameter smoothly interpolates between OBF-like (delta = 0) and Pocock-like (delta = 0.5) behavior.

### Requirements

1. `boundary_wang_tsiatis(delta, alpha = ...)` returns a boundary function computing exact Wang-Tsiatis z-boundaries `z_k = C * t_k^(delta - 0.5)` where the constant C is calibrated to control overall type I error at `alpha`. Requires gsDesign for multivariate normal integration. Errors hard inside the closure if gsDesign is absent (same pattern as `boundary_hsd`). Factory construction succeeds without gsDesign.
2. `boundary_wang_tsiatis(delta, threshold = ...)` returns a boundary function using the probability-scale Wang-Tsiatis formula: `threshold_k = pnorm(qnorm(threshold) * t_k^(delta - 0.5))`. No gsDesign required.
3. Exactly one of `alpha` or `threshold` must be provided (same pattern as `boundary_obf`/`boundary_pocock`).
4. `delta` must be a single numeric value in `[0, 0.5]`. Default: 0.25 (midpoint between OBF and Pocock).
5. `delta = 0` produces OBF-like boundaries (very conservative early, relaxing late). In threshold mode, produces the same qualitative shape as `boundary_obf(threshold = ...)` but via a different formula — not numerically identical.
6. `delta = 0.5` produces Pocock-like boundaries (constant across looks). In threshold mode, produces constant thresholds equal to the target threshold, matching `boundary_pocock(threshold = ...)` exactly.
7. `alpha` must be between 0 and 0.5 (exclusive of 0, inclusive of 0.5). `threshold` must be between 0 and 1 (exclusive of both), consistent with `boundary_pocock`.
8. Returns a function with class `c("boundary_function", "function")` and attributes `boundary_type = "wang_tsiatis"`, `boundary_params = list(delta = ..., alpha/threshold = ...)`.
9. Added to `show_boundaries()` output and its invisible return vector.
10. Added to `_pkgdown.yml` in the "Sequential Analysis and Boundaries" section.
11. Documented with `@seealso` links to `boundary_obf()`, `boundary_pocock()`, `boundary_power()`. The `@details` section explicitly distinguishes `boundary_wang_tsiatis` (z-scale parameterization, alpha-spending) from `boundary_power()` (probability-scale shape, Bayesian only).

### Design Decisions

| Decision | Options | Chosen | Rationale |
|----------|---------|--------|-----------|
| New function vs extend boundary_power | New function / replace boundary_power / extend boundary_power | New function, keep boundary_power() | boundary_power() is a simple Bayesian-only shape tool. Wang-Tsiatis is the proper frequentist+Bayesian generalization. No breaking changes. |
| Dual mode | alpha+threshold / alpha-only / threshold-only | Both alpha + threshold | Follows the OBF/Pocock pattern. Consistent API across all major boundary families. |
| gsDesign dependency | Required for both / required for alpha only / optional with fallback | Required for alpha, not for threshold | Alpha mode needs exact boundaries. Threshold mode has a closed-form formula. gsDesign check is inside closure (call time), not factory (construction time), matching `boundary_hsd`. |
| Deprecate OBF/Pocock | Keep all / soft deprecate / hard deprecate | Keep all, no deprecation | OBF and Pocock are well-known names in clinical trials. Keep for discoverability. Document the equivalence in @details. |
| Parameter name | delta / Delta / shape | delta | Standard Wang-Tsiatis convention in the literature. Lowercase consistent with R naming. |
| Default delta | 0.25 / 0.5 / none (required) | 0.25 | Midpoint between OBF (0) and Pocock (0.5). Provides useful default behavior. |
| Delta convention | Original WT (0=OBF, 0.5=Pocock) / reversed | Original Wang & Tsiatis (1987) | delta=0 → OBF, delta=0.5 → Pocock. Matches the formula `z_k = C * t^(delta-0.5)` directly: delta=0 gives `C/sqrt(t)` (OBF shape), delta=0.5 gives `C` (constant, Pocock). |
| Delta range | [0, ∞) / [0, 0.5] | [0, 0.5] | Values >0.5 produce boundaries that increase with info fraction (harder to cross over time), which is unusual for efficacy. Restricting to [0, 0.5] covers all standard use cases. |
| Numerical matching with boundary_obf | Exact match / approximate match | No exact match required | WT threshold mode uses `pnorm(qnorm(threshold) * t^(delta-0.5))`. boundary_obf uses a spending-function normalization formula. Same qualitative shape at delta=0, not numerically identical. This is by design — they are different parameterizations. |
| Threshold mode formula | Power-family `1-(1-t)*x^delta` / z-scale `pnorm(qnorm(t)*x^(delta-0.5))` | z-scale formula | The z-scale formula directly corresponds to the WT boundary definition. It correctly recovers constant threshold at delta=0.5 (since `x^0 = 1`). The power-family formula doesn't correspond to WT on the z-scale. |
| Alpha mode: gsDesign approach | sfPower spending function / direct z-boundary calibration via root-finding | Direct z-boundary calibration | WT defines boundaries on the z-scale, not via a spending function. Use root-finding to find constant C such that `P(reject at any look | H0) = alpha`, using gsDesign's `gsBound1` or equivalent for multivariate normal integration. Special-case delta=0 → `sfLDOF` and delta=0.5 → `sfLDPocock` for exact results at boundary values. |

### Scope

#### In Scope
- `boundary_wang_tsiatis(delta, alpha, threshold)` factory function in `R/boundaries.R`
- Alpha mode via gsDesign z-boundary calibration
- Threshold mode via analytic z-scale WT formula on probability scale
- Input validation with informative error messages
- `show_boundaries()` update (output + invisible return vector)
- `_pkgdown.yml` update
- Unit tests for both modes, delta edge cases (0, 0.25, 0.5), validation errors, metadata
- Roxygen documentation with `@details`, `@references`, `@examples`
- NAMESPACE export

#### Out of Scope
- Deprecating `boundary_obf()` or `boundary_pocock()` — kept as-is
- Deprecating or modifying `boundary_power()` — kept as-is
- Changing the boundary function class system or metadata attributes
- Futility-specific boundary variants
- Adding Wang-Tsiatis to validation articles (separate TODO)

### Architecture Overview

Single factory function in `R/boundaries.R`, following the exact pattern of existing boundary functions.

**Delta convention** (Wang & Tsiatis 1987): The z-scale boundary is `z_k = C * t_k^(delta - 0.5)` where `t_k` is the information fraction at look `k`.

| delta | z-boundary shape | Equivalent |
|-------|-----------------|------------|
| 0 | `C / sqrt(t)` | O'Brien-Fleming |
| 0.25 | `C * t^(-0.25)` | Midpoint |
| 0.5 | `C` (constant) | Pocock |

**Threshold mode** (no gsDesign):
```r
# Convert z-scale WT to probability scale
# z_k = C * t_k^(delta - 0.5), calibrated so z at t=1 equals qnorm(threshold)
# threshold_k = pnorm(qnorm(threshold) * info_frac_k^(delta - 0.5))
f <- function(info_frac) {
  z_final <- stats::qnorm(threshold)
  stats::pnorm(z_final * info_frac^(delta - 0.5))
}
```

At delta=0.5: `pnorm(qnorm(threshold) * t^0) = pnorm(qnorm(threshold)) = threshold` (constant). At delta=0: `pnorm(qnorm(threshold) / sqrt(t))` — stringent early, relaxing to threshold at t=1.

**Alpha mode** (requires gsDesign):
```r
f <- function(info_frac) {
  n_looks <- length(info_frac)

  if (!requireNamespace("gsDesign", quietly = TRUE)) {
    cli::cli_abort(...)
  }

  # Special-case exact gsDesign spending functions at boundary delta values
  if (delta == 0) {
    design <- gsDesign::gsDesign(k = n_looks, timing = info_frac,
                                  alpha = alpha, test.type = 1,
                                  sfu = gsDesign::sfLDOF)
  } else if (delta == 0.5) {
    design <- gsDesign::gsDesign(k = n_looks, timing = info_frac,
                                  alpha = alpha, test.type = 1,
                                  sfu = gsDesign::sfLDPocock)
  } else {
    # General case: calibrate C via root-finding
    # Find C such that P(reject at any look | H0) = alpha
    # z_k = C * info_frac_k^(delta - 0.5)
    # Use gsDesign internals or mvtnorm for multivariate normal integration
    ...
  }

  stats::pnorm(z_bounds)
}
```

The general-case alpha mode requires finding constant C via numerical root-finding with multivariate normal integration. Implementation will use `gsDesign::gsBound1` or `mvtnorm::pmvnorm` to compute the crossing probability for a given set of z-boundaries, then `stats::uniroot` to find C.

### Constraints

- Must not break existing boundary functions or their tests
- Alpha mode must produce exact results at delta=0 (matching `boundary_obf(alpha=...)`) and delta=0.5 (matching `boundary_pocock(alpha=...)`) by delegating to the corresponding gsDesign spending functions
- Threshold mode at delta=0.5 must produce exactly constant thresholds (mathematical identity, not numerical approximation)
- Follows existing validation patterns (`cli::cli_abort`)
- Tests for `show_boundaries()` output must use `capture_cli()` from `tests/testthat/helper-cli.R`, not `capture.output()`

### Open Questions

- None — all resolved during brainstorm and review.

## Implementation Plan

### Phase 1: boundary_wang_tsiatis() — function, tests, docs

**Files to create:**
- (none)

**Files to modify:**
- `R/boundaries.R` — add `boundary_wang_tsiatis()` factory function and update `show_boundaries()`
- `tests/testthat/test-boundaries.R` — add test section for `boundary_wang_tsiatis()`
- `_pkgdown.yml` — add `boundary_wang_tsiatis` to "Sequential Analysis and Boundaries" section

**Steps:**

1. **Add `boundary_wang_tsiatis()` to `R/boundaries.R`** (before the deprecated section, after `boundary_power()`):
   - Roxygen: `@title`, `@description`, `@param delta`, `@param alpha`, `@param threshold`, `@return`, `@details` (z-scale formula, distinguish from `boundary_power()`, note that threshold validation range (0,1) differs from `boundary_obf` which requires (0.5,1)), `@references` (Wang & Tsiatis 1987, Lan & DeMets 1983, Jennison & Turnbull 2000), `@seealso`, `@examples`, `@export`
   - Call `force(delta)` immediately after validation, before either mode branch
   - Input validation:
     - `delta`: single numeric in [0, 0.5]
     - Exactly one of `alpha` or `threshold` (same pattern as `boundary_obf`)
     - `alpha`: single numeric in (0, 0.5]
     - `threshold`: single numeric in (0, 1) exclusive — intentionally stricter than `boundary_pocock` which accepts [0, 1] inclusive, because threshold=0 and threshold=1 produce degenerate boundaries via `qnorm()`
   - **Threshold mode** (no gsDesign):
     ```r
     force(threshold)
     # ... validation ...
     f <- function(info_frac) {
       z_final <- stats::qnorm(threshold)
       stats::pnorm(z_final * info_frac^(delta - 0.5))
     }
     ```
   - **Alpha mode** (requires gsDesign at call time, not at factory time):
     ```r
     force(alpha)
     # ... validation ...
     f <- function(info_frac) {
       n_looks <- length(info_frac)

       if (!requireNamespace("gsDesign", quietly = TRUE)) {
         cli::cli_abort(...)
       }

       # Single-look early return: no group sequential structure needed
       if (n_looks == 1) {
         return(stats::pnorm(stats::qnorm(1 - alpha)))
       }

       # Special cases for exact results via Lan-DeMets spending functions
       if (delta == 0) {
         design <- gsDesign::gsDesign(k = n_looks, timing = info_frac,
                                     alpha = alpha, test.type = 1,
                                     sfu = gsDesign::sfLDOF)
         z_bounds <- design$upper$bound
       } else if (delta == 0.5) {
         design <- gsDesign::gsDesign(k = n_looks, timing = info_frac,
                                     alpha = alpha, test.type = 1,
                                     sfu = gsDesign::sfLDPocock)
         z_bounds <- design$upper$bound
       } else {
         # General case: calibrate C via root-finding
         # z_k = C * info_frac_k^(delta - 0.5)
         # gsProbability() uses n.I as proportional information levels;
         # passing info fractions directly is correct — gsDesign computes
         # the correlation matrix from increments diff(c(0, n.I)).
         objective <- function(C) {
           z_bounds <- C * info_frac^(delta - 0.5)
           prob <- gsDesign::gsProbability(
             k = n_looks, theta = 0, n.I = info_frac,
             a = rep(-20, n_looks), b = z_bounds
           )
           # upper$prob is a k x length(theta) matrix of first-passage probs;
           # sum gives total rejection probability under H0 (theta=0)
           sum(prob$upper$prob) - alpha
         }
         # Interval: lower bound near 0 for large alpha (where C → 0),
         # upper bound generous for small alpha with many looks
         z_single <- stats::qnorm(1 - alpha)
         root <- stats::uniroot(objective,
                                interval = c(z_single * 0.1, z_single * 3),
                                tol = .Machine$double.eps^0.5)
         z_bounds <- root$root * info_frac^(delta - 0.5)
       }
       stats::pnorm(z_bounds)
     }
     ```
   - Set attributes: `boundary_type = "wang_tsiatis"`, `boundary_params = list(delta = ..., alpha/threshold = ...)`
   - Set class: `c("boundary_function", "function")`

2. **Update `show_boundaries()`** in `R/boundaries.R`:
   - Add Wang-Tsiatis entry between HSD and Linear interpolation (after line 733):
     ```
     Wang-Tsiatis (tunable between OBF and Pocock):
       boundary_wang_tsiatis(delta = 0.25, alpha = 0.025)
       boundary_wang_tsiatis(delta = 0.25, threshold = 0.95)
       delta=0: OBF-like, delta=0.5: Pocock-like
     ```
   - Add gsDesign note (same conditional pattern as OBF/Pocock)
   - Update invisible return vector to: `c("boundary_obf", "boundary_pocock", "boundary_hsd", "boundary_wang_tsiatis", "boundary_linear", "boundary_power", "boundary_constant")`

3. **Fix existing `show_boundaries()` test** in `tests/testthat/test-boundaries.R`:
   - Replace `capture.output(out <- show_boundaries())` (line 239) with `capture_cli()` pattern per CLAUDE.md anti-pattern rule

4. **Add unit tests** to `tests/testthat/test-boundaries.R`:
   - **Threshold mode — metadata**: class, boundary_type, boundary_params
   - **Threshold mode — delta=0.5 produces constant**: `expect_equal(f(c(0.25, 0.5, 0.75, 1.0)), rep(threshold, 4))` (exact, not approximate)
   - **Threshold mode — delta=0 produces OBF-like shape**: decreasing thresholds, final = threshold, first > last
   - **Threshold mode — delta=0.25 midpoint shape**: verify monotonically decreasing, between OBF-like and constant
   - **Threshold mode — final look equals threshold for all delta**: at info_frac=1, threshold_k = pnorm(qnorm(threshold)*1) = threshold
   - **Threshold mode — single look**: f(c(1.0)) returns threshold
   - **Alpha mode — metadata**: class, boundary_type, boundary_params (skip_if_not_installed("gsDesign"))
   - **Alpha mode — delta=0 matches boundary_obf(alpha=...)**: compare thresholds within tolerance (skip_if_not_installed("gsDesign"))
   - **Alpha mode — delta=0.5 matches boundary_pocock(alpha=...)**: compare thresholds within tolerance (skip_if_not_installed("gsDesign"))
   - **Alpha mode — general delta=0.25**: valid probabilities, decreasing shape (skip_if_not_installed("gsDesign"))
   - **Alpha mode — single look**: f(c(1.0)) returns pnorm(qnorm(1 - alpha)) (skip_if_not_installed("gsDesign"))
   - **Alpha mode — requires gsDesign at call time**: factory succeeds, but mock missing gsDesign → cli_abort at call time
   - **Validation — delta out of range**: delta = -0.1, delta = 0.6, delta = "a", delta = c(0.1, 0.2)
   - **Validation — both alpha and threshold**: expect_cli_abort
   - **Validation — neither alpha nor threshold**: expect_cli_abort
   - **Validation — alpha out of range**: alpha = 0, alpha = 0.6
   - **Validation — threshold out of range**: threshold = 0, threshold = 1

5. **Update `_pkgdown.yml`**: Add `boundary_wang_tsiatis` after `boundary_hsd` (line 101), before `show_boundaries` (line 102)

6. **Run `devtools::document()`** to regenerate NAMESPACE export and man page

7. **Run `devtools::check()`** to verify no R CMD check issues

**Depends on:** None

## Verification & Validation

- **Automated**: Unit tests for both modes, delta edge cases (0, 0.25, 0.5), validation errors, metadata attributes, show_boundaries() output; R CMD check
- **Manual**: REPL verification comparing alpha-mode boundaries at delta=0 vs `boundary_obf(alpha=...)` and delta=0.5 vs `boundary_pocock(alpha=...)`
- **Numerical**: Confirm threshold mode at delta=0.5 returns exact constant; confirm alpha-mode at delta=0 and delta=0.5 match existing implementations exactly

## Dependencies

- `gsDesign` package (Suggests, not Imports) — already used by `boundary_obf`, `boundary_pocock`, `boundary_hsd`. Used for both special-case spending functions (sfLDOF, sfLDPocock) and general-case root-finding via `gsProbability()`
- `mvtnorm` NOT needed — `gsDesign::gsProbability()` handles multivariate normal integration internally

## Notes

_Living section — updated during implementation._

## Review Feedback

### Spec Review (2026-03-22)

Review by code-architect agent. 10 findings (3 blockers, 4 warnings, 3 suggestions). All blockers resolved:

**Resolved blockers:**
1. **Delta convention contradiction** — Fixed: consistently use original Wang & Tsiatis (1987) convention. delta=0 → OBF, delta=0.5 → Pocock. All requirements, architecture, and constraints updated to match.
2. **Threshold formula mismatch with boundary_obf** — Fixed: adopted z-scale formula `pnorm(qnorm(threshold) * t^(delta-0.5))` instead of power-family formula. Dropped exact numerical matching constraint with `boundary_obf` — they use different formulas with the same qualitative shape.
3. **gsDesign sfPower mapping unresolved** — Fixed: abandoned sfPower approach. Alpha mode uses direct z-boundary calibration via root-finding. Special-cases delta=0 (sfLDOF) and delta=0.5 (sfLDPocock) for exact results.

**Addressed warnings:**
4. show_boundaries() testing → added constraint to use `capture_cli()`
5. gsDesign error placement → clarified: error inside closure at call time, factory construction succeeds without gsDesign
6. _pkgdown.yml update → added to scope and requirements
7. Threshold validation range → specified as (0, 1) exclusive

**Incorporated suggestions:**
8. Delta upper bound → restricted to [0, 0.5] with validation error
9. boundary_power() distinction → added to requirement 11 (`@details` must distinguish the two)
10. show_boundaries() invisible return vector → added to requirement 9

### Plan Review (2026-03-22)

Review by code-architect agent. 11 findings (3 blockers, 5 warnings, 3 suggestions). All blockers resolved. Plan reviewed in 1 iteration.

**Resolved blockers:**
1. **`gsProbability(n.I=info_frac)` semantics** — Added code comment documenting that gsDesign uses n.I as proportional information levels; passing info fractions directly is correct.
2. **`sum(prob$upper$prob)` matrix structure** — Added code comment clarifying that upper$prob is a k×len(theta) matrix of first-passage probabilities; sum gives total rejection probability.
3. **`uniroot` interval [0.5, 5] breaks for large alpha** — Fixed: changed interval to `c(z_single * 0.1, z_single * 3)` where `z_single = qnorm(1 - alpha)`, anchored to the single-look z-critical value. This handles alpha near 0.5 (C→0) and small alpha (C large).

**Addressed warnings:**
4. **Threshold validation range mismatch with boundary_pocock** — Kept (0,1) exclusive as intentional; documented in plan step 1 that boundary_pocock uses [0,1] inclusive but WT uses stricter range because qnorm(0) and qnorm(1) are degenerate.
5. **Existing show_boundaries() test uses capture.output()** — Added Step 3 to fix existing test to use `capture_cli()`.
6. **Single-look k=1 edge case** — Added `if (n_looks == 1) return(pnorm(qnorm(1 - alpha)))` early return in alpha mode, and single-look test cases for both modes.
7. **force() calls missing** — Added `force(delta)`, `force(alpha)`, `force(threshold)` to plan pseudocode.
8. **k=1 in special-case branches** — Covered by the single-look early return (fires before special-case branches).

**Incorporated suggestions:**
9. **boundary_obf threshold lower bound difference** — Added to @details documentation note in Step 1.
10. **Invisible return vector order** — Specified exact vector in Step 2: `c("boundary_obf", "boundary_pocock", "boundary_hsd", "boundary_wang_tsiatis", "boundary_linear", "boundary_power", "boundary_constant")`.
11. **_pkgdown.yml insertion point** — Clarified: after line 101 (boundary_hsd), before line 102 (show_boundaries).
