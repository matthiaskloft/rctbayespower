# Test Coverage Gaps Plan

> Status: APPROVED (4 review rounds, converged)
> Created: 2026-03-11
> Modules: `plot_*.R`, `model_cache.R`, `setup_python.R`
> Deferred: `plot_optimization.R` (pending optimization module redesign), `pareto_optimize.R`

---

## Pre-requisite: Fix Pre-existing Code Bugs

Two bugs discovered during plan review must be fixed before writing plot tests:

### Bug 1: Dead code in `plot_power_curve.R:277`
`group_by == "outcome"` is unreachable — dispatcher validates against `c("metric", "decision", "effect_size", "sample_size")`. Should be `group_by == "decision"` so `rctbp_colors()` is applied when grouping by decision.

### Bug 2: Factor level mismatch in `plot_power_curve.R:141,168`
`color_group` uses `c("Success", "Futility")` but `pivot_plot_data_long()` produces `"Efficacy"` and `"Futility"`. Should be `c("Efficacy", "Futility")`.

---

## Phase 0: mock_power_analysis() Helper

Add to `tests/testthat/helper-mock-objects.R`.

### Design

`mock_power_analysis(variant = c("1d", "2d", "accrual", "accrual_no_cols"))`

- `n_cores = 1L` (avoids `detectCores()` validator)
- `results_raw = data.frame()` (S7 property has no default)
- Builds own `rctbp_conditions` directly (NOT reusing `mock_conditions()`)
- `conditions@constant = list(thr_dec_eff = 0.975, thr_dec_fut = 0.5)` — scalars for `get_original_threshold()`

### Variants

| Variant | Grid columns | results_conditions | results_interim | Use case |
|---------|-------------|-------------------|-----------------|----------|
| `"1d"` | `id_cond`, `n_total` (3 values) | 3 rows | `data.frame()` | power_curve, comparison (sample_only) |
| `"2d"` | `id_cond`, `n_total` (3), `b_arm_treat` (2) | 6 rows incl. `b_arm_treat` col | `data.frame()` | heatmap, group_by="effect_size", comparison error |
| `"accrual"` | `id_cond`, `n_total` (2) | 2 rows + `calendar_time_mn`, `n_enrolled_mn` | non-empty (2+ rows with `id_look`, `id_cond`, `n_analyzed`, `calendar_time_mn`, `n_enrolled_mn`) | accrual happy path |
| `"accrual_no_cols"` | `id_cond`, `n_total` (2) | 2 rows | non-empty (2+ rows WITHOUT `calendar_time_mn`) | accrual "missing columns" error |

### results_conditions columns (all variants)

`id_cond`, `n_total`, `pwr_eff`, `pwr_fut`, `pr_eff`, `pr_fut`, `post_median`, `post_q025`, `post_q975`, `par_name`

### Smoke test

```r
test_that("mock_power_analysis variants construct without error", {
  expect_no_error(mock_power_analysis("1d"))
  expect_no_error(mock_power_analysis("2d"))
  expect_no_error(mock_power_analysis("accrual"))
  expect_no_error(mock_power_analysis("accrual_no_cols"))
})
```

---

## Phase 1: Plot Helpers (`plot_helpers.R`)

Test file: `test-plot.R` (delete old commented-out `test-plot_power_grid_analysis.R`)

Convention: ALL `plot()` calls use `interactive = FALSE`. Documented at file top.

Internal functions tested via `:::`:

- `rctbp_colors()`: keys are `"Efficacy"` and `"Futility"` (capitalized)
- `rctbp_theme()`: returns `ggplot2::theme` object
- `pivot_plot_data_long()`:
  - Happy path: output has `measure` and `outcome` columns (NOT `metric`/`value`)
  - Warning path: `include_se = TRUE` with no SE columns → `expect_cli_warn()`; also check returned df lacks `se` column
- `to_interactive()`: `skip_if_not_installed("plotly")`, `inherits(result, "plotly")`

---

## Phase 2: Dispatcher + Power Curve

All through public `plot()` S7 method:

- `plot(mock_1d, type = "power_curve", interactive = FALSE)` → ggplot
- `plot(mock_1d, type = "auto", interactive = FALSE)` → auto-detects power_curve for 1D
- `plot(mock_2d, type = "auto", interactive = FALSE)` → auto-detects heatmap for 2D
- `plot(mock_1d, type = "invalid", interactive = FALSE)` → `expect_cli_abort()`
- `group_by = "decision"` with 1d mock → works
- `group_by = "effect_size"` with **2d mock** → works, colors by effect (requires `b_arm_treat` in grid AND results_conditions)
- `show_target = TRUE` with 1d mock AND `group_by = "decision"` explicitly → target lines drawn
- Faceting: `facet_by = "sample_size"` with 2d mock → faceted plot

---

## Phase 3: Heatmap (`plot_heatmap.R`)

- `plot(mock_2d, type = "heatmap", interactive = FALSE)` → ggplot with `geom_tile` layer
- `plot(mock_1d, type = "heatmap", interactive = FALSE)` → `expect_cli_abort()` (analysis_type != "both")

---

## Phase 4: Comparison (`plot_comparison.R`)

- `plot(mock_1d, type = "comparison", interactive = FALSE)` → ggplot (analysis_type = "sample_only")
  - 1d mock has 3 distinct n_total → analysis_type = "sample_only" ✓
  - This is where `rctbp_colors()` usage is testable (line 73)
- `plot(mock_2d, type = "comparison", interactive = FALSE)` → `expect_cli_abort()` (analysis_type = "both")

---

## Phase 5: Accrual (`plot_accrual.R`)

Three test scenarios:

1. `plot(mock_1d, type = "accrual", interactive = FALSE)` → `expect_cli_abort()` matching "sequential design" (`has_interim = FALSE`)
2. `plot(mock_accrual_no_cols, type = "accrual", interactive = FALSE)` → `expect_cli_abort()` matching "accrual modeling" (dispatcher guard at plot_power_analysis.R:298)
3. `plot(mock_accrual, type = "accrual", interactive = FALSE)` → ggplot

---

## Phase 6: model_cache.R

### Cache dir mock pattern

```r
tmp_dir <- withr::local_tempdir()
local_mocked_bindings(user_cache_dir = function(...) tmp_dir, .package = "rappdirs")
```

`rappdirs` is in DESCRIPTION Imports (verified).

### Download mock pattern

```r
download_calls <- list()
local_mocked_bindings(
  download_model = function(model_name, dest_path, model_type) {
    download_calls[[length(download_calls) + 1L]] <<- list(model_name, dest_path, model_type)
  },
  .package = "rctbayespower"
)
```

### Tests

- `get_model_cache_dir()`: creates dir, correct path; subdir variant
- `list_models()`: empty → not cached; plant `saveRDS(list(), file.path(tmp_dir, "brms", "ancova_cont_2arms.rds"))` → cached (only checks `file.exists()`)
  - Must `dir.create(file.path(tmp_dir, "brms"), recursive = TRUE)` before planting
- `clear_model_cache()`: plant files, clear, verify empty
- `get_cache_size()`: plant files, check size > 0
- `load_brms_model()` cache-hit: plant `saveRDS(mock_brmsfit(), file.path(tmp_dir, "brms", "ancova_cont_2arms.rds"))`, assert `length(download_calls) == 0`
- `load_bf_model()`: skip (Python-dependent)
- `download_model()`: mock `utils::download.file` with `.package = "utils"` and side-effect capture, verify correct GitHub release URL

---

## Phase 7: setup_python.R

### Testable without Python

- `setup_bf_python(python_version = "2.7")` → error
- `setup_bf_python(cuda_version = "invalid")` → error (validation is in `setup_bf_python`, NOT `install_bf_dependencies`)
- `detect_cuda_version()` without nvidia-smi → returns `"cpu"` (wrap in `suppressMessages()`)

### Testable with Python (`skip_if_not`)

- `verify_bf_installation()` return structure
- `check_bf_status()` CLI output

### Accepted gaps

- CUDA version mapping logic (embedded, requires `system2` mock)
- `requireNamespace` mocking for base primitives
- Python environment creation/management

### Realistic coverage: ~30-40%

---

## Execution Order

1. Fix pre-existing bugs (plot_power_curve.R lines 141, 168, 277)
2. `mock_power_analysis()` helper + smoke tests
3. Phase 1: plot helpers
4. Phase 2: dispatcher + power_curve
5. Phase 3-5: heatmap, comparison, accrual
6. Phase 6: model_cache.R
7. Phase 7: setup_python.R

---

## Status

| Step | Status |
|------|--------|
| Plan | DONE |
| Pre-req: Fix bugs | MERGED |
| Phase 0: mock_power_analysis | MERGED |
| Phase 1: Plot helpers | MERGED |
| Phase 2: Dispatcher + power curve | MERGED |
| Phase 3: Heatmap | MERGED |
| Phase 4: Comparison | MERGED |
| Phase 5: Accrual | MERGED |
| Phase 6: model_cache | MERGED |
| Phase 7: setup_python | MERGED |
| Ship | MERGED |

---

## Review History

| Round | Issues | Status |
|-------|--------|--------|
| 1 | 12 (3 critical) | Fixed in v2 |
| 2 | 6 (3 critical) | Fixed in v3 |
| 3 | 7 (3 critical) | Fixed in v4 |
| 4 | 3 minor spec gaps + 2 pre-existing bugs | Documented, plan approved |
