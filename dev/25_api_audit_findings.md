# API Audit Findings

**Created:** 2026-02-21
**Status:** Open
**Relates to:** [`24_api_improvement_plan.md`](24_api_improvement_plan.md), [`05_code_consistency_review.md`](05_code_consistency_review.md)

---

## 1. Critical Bug: Column Name Mismatch in `interim_functions.R`

**Severity: Critical (runtime error)**

The interim decision functions access columns `pr_scs` and `pr_ftl` from `interim_summaries`, but the actual columns produced by `summarize_post_brms_single()` and `summarize_post_bf()` are `pr_eff` and `pr_fut`.

**Broken code:**

```r
# interim_functions.R:109
max_pr_ftl <- max(interim_summaries$pr_ftl, na.rm = TRUE)

# interim_functions.R:185-186
max_pr_scs <- max(interim_summaries$pr_scs, na.rm = TRUE)
max_pr_ftl <- max(interim_summaries$pr_ftl, na.rm = TRUE)
```

**Actual column names (backend_brms.R:131-132, backend_bf.R:935-936):**

```r
pr_eff = pr_eff,
pr_fut = pr_fut,
```

**Impact:** `$pr_scs` / `$pr_ftl` return `NULL` in R, `max()` returns `-Inf`, and the trial **never stops early** -- a silent logic failure. Affects `interim_futility_only()` and `interim_success_futility()`.

**Fix:** Replace `pr_scs` -> `pr_eff` and `pr_ftl` -> `pr_fut` in `interim_functions.R`.

---

## 2. Stale Naming in Roxygen Documentation

**Severity: High (misleading docs)**

The codebase went through a naming refactor (`_scs`/`_ftl` -> `_eff`/`_fut`), but several roxygen blocks still reference the old names.

| Location | Old Names Used | Correct Names |
|----------|---------------|---------------|
| `R/class_design.R:270` | `p_sig_scs`, `p_sig_ftl`, `thresh_scs`, `thresh_ftl` | `thr_dec_eff`, `thr_dec_fut`, `thr_fx_eff`, `thr_fx_fut` |
| `R/output_system.R:97` | `p_sig_scs = 0.975, p_sig_ftl = 0.5` | These params don't exist in `build_design()` |
| `R/compute_measures.R:747` | `prop_stp_scs`, `prop_stp_ftl` | `prop_stp_eff`, `prop_stp_fut` |

---

## 3. Broken Roxygen Examples in `interim_functions.R`

**Severity: High (broken docs)**

All three exported interim functions (`interim_continue`, `interim_futility_only`, `interim_success_futility`) have roxygen examples that reference parameters that don't exist in `build_design()`:

```r
# Current (wrong):
design <- build_design(
  model = my_model,          # 'model' param doesn't exist
  target_params = "b_arm2",
  p_sig_scs = 0.975,         # doesn't exist in build_design()
  p_sig_ftl = 0.5,           # doesn't exist in build_design()
  analysis_at = c(50, 100),  # doesn't exist in build_design()
  interim_function = ...     # doesn't exist in build_design()
)
```

**Fix:** Rewrite examples to show the correct 2-step flow (`build_design()` then `build_conditions()` with `thr_dec_eff`/`thr_dec_fut`).

Lines: `interim_functions.R:42-49`, `interim_functions.R:87-95`, `interim_functions.R:152-164`.

---

## 4. Stale Comments with Old Naming (Code Hygiene)

**Severity: Low**

Internal comments reference old naming conventions:

| File:Line | Old Name | Should Be |
|-----------|----------|-----------|
| `R/class_conditions.R:403` | `p_sig_scs`, `p_sig_ftl` | `thr_dec_eff`, `thr_dec_fut` |
| `R/class_conditions.R:519` | `p_sig_scs`, `p_sig_ftl` | `thr_dec_eff`, `thr_dec_fut` |
| `R/class_conditions.R:597` | `p_sig_scs`, `p_sig_ftl` | `thr_dec_eff`, `thr_dec_fut` |
| `R/worker_functions.R:377` | `p_sig_scs`, `p_sig_ftl` | `thr_dec_eff`, `thr_dec_fut` |
| `R/backend_brms.R:450` | `dec_scs`, `dec_ftl` | `dec_eff`, `dec_fut` |
| `R/interim_functions.R:198-199` (error msg) | `'thresh_scs'`, `'p_sig_scs'` | `'thr_fx_eff'`, `'thr_dec_eff'` |

---

## 5. Dev Docs Reference Wrong Column Names

**Severity: High (outdated dev docs)**

### `dev/03_workflow.md` (Results Structure section)

Claims `results_summ` contains:
- `power_success`, `power_futility`, `prob_success`, `prob_futility`

Actual columns: `pwr_eff`, `pwr_fut`, `pr_eff`, `pr_fut`

### `dev/01_architecture.md` (Power Metrics section)

Uses old column names: `pr_scs`, `pr_ftl`, `dec_scs`, `dec_ftl`

Actual columns: `pr_eff`, `pr_fut`, `dec_eff`, `dec_fut`

### `dev/03_workflow.md` (Debugging Tips)

Line 242 says: `Check model@active_backend after setting` -- property doesn't exist (it's `design@backend`).

---

## 6. Missing Validations in `build_conditions()`

**Severity: Medium (silent data bugs / late errors)**

| What | Risk | Suggested Fix |
|------|------|---------------|
| `p_alloc` doesn't sum to 1 | Silently generates invalid simulated data | Validate `abs(sum(p_alloc) - 1) < 1e-10` |
| `analysis_at` not sorted | Undefined behavior in sequential analysis | Validate `all(diff(analysis_at) > 0)` |
| `analysis_at` has duplicates | Duplicate looks create nonsense results | Validate `!anyDuplicated(analysis_at)` |
| `thr_fx_eff > thr_fx_fut` not checked | Inverted ROPE makes every posterior "both effective AND futile" | Warn or error when `thr_fx_eff <= thr_fx_fut` |
| `n_total` not in `crossed`/`constant` | Cryptic error deep in simulation | Early check that `n_total` is provided |
| `thr_dec_eff`/`thr_dec_fut` not provided | NAs in results, not caught early | Require these or provide clear error |
| `thr_fx_eff`/`thr_fx_fut` not provided | NAs in results, not caught early | Require these or provide clear error |

---

## 7. `model_name` Parameter vs Property Ambiguity

**Severity: Medium (API confusion)**

In `build_design()`:
- The **parameter** `model_name` means "which predefined model to load" (e.g., `"ancova_cont_2arms"`)
- The **property** `@model_name` stores a human-readable display name (e.g., `"2-arm ANCOVA"`)
- The **property** `@predefined_model` stores the actual model registry key

A user who inspects `design@model_name` after `build_design(model_name = "ancova_cont_2arms")` won't get back `"ancova_cont_2arms"`.

**Possible fixes:**
- Rename the parameter to `predefined_model` (matches the property)
- Or rename the property `@model_name` to `@display_name`

---

## 8. `backend_args_brms` vs `brms_args` Overlap

**Severity: Medium (API confusion)**

Two places to configure brms settings:
- `build_design(backend_args_brms = ...)` -- stored in the design object
- `power_analysis(brms_args = ...)` -- stored in the power analysis object

It's unclear which takes precedence, whether they merge or override, and which the user should prefer. This should be documented or one should be removed.

---

## 9. Incomplete `linked` Deprecation

**Severity: Low (incomplete migration)**

`build_conditions()` accepts `linked =` for backward compatibility, but the code says:

```r
# This is complex, so we'll just warn and not attempt conversion
```

Old code using `linked` gets a deprecation warning but the parameter is silently ignored -- no data is migrated. Either complete the migration logic or error explicitly.

---

## 10. `rctbp_conditions` Stores Insufficient Call Metadata

**Severity: Low (blocks future `get_code()` feature)**

The `crossed` property stores the original user input (including `link()` objects), but `constant` is not stored as a property on the class. This blocks the planned `get_code()` feature (see `dev/24_api_improvement_plan.md` section 3.1) because the full call chain can't be reconstructed.

**Fix:** Add a `constant` property to `rctbp_conditions` storing the original constant args.

---

## Priority Summary

| # | Issue | Severity | Effort | Files |
|---|-------|----------|--------|-------|
| 1 | `pr_scs`/`pr_ftl` bug in interim functions | **Critical** | Low | `interim_functions.R` |
| 2 | Stale roxygen with old param names | **High** | Low | `class_design.R`, `output_system.R`, `compute_measures.R` |
| 3 | Broken roxygen examples in interim functions | **High** | Low | `interim_functions.R` |
| 4 | Dev docs with wrong column names | **High** | Low | `03_workflow.md`, `01_architecture.md` |
| 5 | Stale comments with old naming | Low | Low | 5 files (see #4 above) |
| 6 | Missing validations in `build_conditions()` | **Medium** | Medium | `class_conditions.R` |
| 7 | `model_name` param/property ambiguity | **Medium** | Medium | `class_design.R` |
| 8 | `backend_args_brms` vs `brms_args` overlap | **Medium** | Low | Docs or `class_power_analysis.R` |
| 9 | Incomplete `linked` deprecation | Low | Low | `class_conditions.R` |
| 10 | Missing `constant` property on conditions | Low | Low | `class_conditions.R` |

**Recommended order:** Fix #1 first (critical bug), then #2-4 (high-impact docs), then #6-8 (medium).
