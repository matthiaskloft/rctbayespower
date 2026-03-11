# Plan: fix-duplicate-simplex-transforms

**Created**: 2026-03-10
**Author**: Claude

## Status

| Phase | Status | Date | Notes |
|-------|--------|------|-------|
| Plan | DONE | 2026-03-10 | |
| Phase 1: Rename & Test | DONE | 2026-03-10 | |
| Ship | DONE | 2026-03-10 | PR #19 |

## Summary

**Motivation**: `apply_simplex_transforms()` is defined in both `R/optimization_internal.R` (line 641) and `R/pareto_optimize.R` (line 839) with **incompatible return types**. R loads source files alphabetically, so the `pareto_optimize.R` version (flat list) silently overwrites the `optimization_internal.R` version (3-element list). This means the single-objective optimization pipeline silently fails — callers at lines 1742 and 2820 in `optimization_internal.R` access `$crossed`, `$ilr_values`, `$simplex_values` on a flat list and get `NULL`.

**Outcome**: Each optimization pipeline has its own correctly-named function. The single-objective pipeline works correctly again. Tests verify both return structures.

## Design Decisions

| Decision | Options | Chosen | Rationale |
|----------|---------|--------|-----------|
| Resolution strategy | (A) Rename pareto version, (B) Unified function with flag, (C) Shared helper + wrappers | A: Rename | Minimal diff, no coupling between pipelines. The two functions have genuinely different semantics (archive-ready vs merge-ready). |
| Rename target | `_pareto` suffix vs `_flat` suffix | `_flat` | Describes the structural difference (flat return type), not just the file location. More informative to future readers. |

## Scope

### In Scope
- Rename the `pareto_optimize.R` definition to `apply_simplex_transforms_flat()`
- Update its single caller in `pareto_optimize.R` (~line 568)
- Add cross-reference comments in both files
- Add tests verifying return structures of both functions
- Verify single-objective optimization path is restored

### Out of Scope
- `dev/zz_scripts/optimization_internal_backup.R` contains a third copy (~line 392). Not loaded by R, no runtime impact. Will not modify.
- Refactoring the optimization files more broadly
- Adding new optimization features

## Implementation Plan

### Phase 1: Rename & Test

**Files to modify:**
- `R/pareto_optimize.R` — rename function definition + update caller + add cross-reference comment
- `R/optimization_internal.R` — add cross-reference comment only (no code changes)
- `tests/testthat/test-pareto_optimize.R` — add return-structure tests

**Steps:**
1. In `R/pareto_optimize.R` line 839: rename `apply_simplex_transforms` → `apply_simplex_transforms_flat`
2. In `R/pareto_optimize.R` line 568: update call to `apply_simplex_transforms_flat()`
3. Add comment above `pareto_optimize.R` definition: `# See also: apply_simplex_transforms() in optimization_internal.R (returns structured list)`
4. Add comment above `optimization_internal.R` definition (~line 641): `# See also: apply_simplex_transforms_flat() in pareto_optimize.R (returns flat list)`
5. Add test: `apply_simplex_transforms()` with a 2-arm p_alloc spec returns list with `$crossed`, `$ilr_values`, `$simplex_values`
6. Add test: `apply_simplex_transforms_flat()` with the same spec returns a flat named list (no `$crossed` element, p_alloc is directly in the list)
7. Run `devtools::check()`

## Verification & Validation

- **Automated**: R CMD check passes; new tests pass for both function variants
- **Manual (one-time)**: After `devtools::load_all()`, confirm `ls(getNamespace("rctbayespower"))` contains both `apply_simplex_transforms` and `apply_simplex_transforms_flat` as distinct entries
- **Important note**: R CMD check does NOT warn about internal function name collisions (neither function is exported). Only behavioral tests can verify this fix. This is why the return-structure tests are essential.

## Dependencies

- None. This is a self-contained bug fix.

## Notes

_Living section — updated during implementation._

## Review Feedback

**Reviewer**: plan-reviewer agent (2026-03-10)
**Verdict**: REVISE → incorporated

Key feedback incorporated:
1. Clarified that the `optimization_internal.R` version is the one being shadowed/broken, not the one being renamed
2. Added requirement to test BOTH function variants (not just the renamed one)
3. Acknowledged `dev/zz_scripts/` backup file as out of scope
4. Changed rename from `_pareto` to `_flat` for semantic clarity
5. Noted that R CMD check won't catch this class of bug — only behavioral tests can
6. Added cross-reference comments to prevent future re-introduction
