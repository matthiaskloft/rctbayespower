# Plan: v1 Feature Specification

**Created**: 2026-03-11
**Author**: Claude + Matze
**Review rounds**: 1 (plan-reviewer) + feature discussion

## Status

| Phase | Status | Date | Notes |
|-------|--------|------|-------|
| Plan | REVIEW | 2026-03-11 | Feature decisions finalized, awaiting final approval |
| Phase 1: Critical Bug Fixes | TODO | | API audit items #1-#3, #5 (interim_functions.R) |
| Phase 2: Missing Validations | TODO | | Genuinely missing items from API audit #6 |
| Phase 3: Test Coverage | TODO | | Plot, model_cache, setup_python |
| Phase 4: New v1 Features | TODO | | survival_2arms, as.data.frame, wang_tsiatis, model_name fix |
| Phase 5: Optimization Redesign (experimental) | TODO | | Separate plan exists; not a release blocker |
| Phase 6: Documentation & Polish | TODO | | Roxygen, BF interface spec, package docs, API cleanup |
| Phase 7: Validation Articles | TODO | | Pkgdown articles, one per model |
| Phase 8: R CMD check Clean | TODO | | 0 errors, 0 warnings, 0 notes |
| Ship v1 | TODO | | Tag + CRAN submission |

## Summary

**What is v1?** The first public release of `rctbayespower` — a Bayesian power analysis package for RCTs with ROPE-based decision making, group sequential boundaries, and brms/Stan estimation. v1 targets **psychological intervention trials** using ANCOVA designs with continuous, binary, proportion, or survival outcomes.

**Guiding principle:** Ship what works well. Defer what's experimental, blocked, or untested.

---

## Scope Decision: What's IN vs OUT

### IN: v1 Core (already working)

| Feature | Current State | v1 Work Needed |
|---------|--------------|----------------|
| **Pipeline** (Design → Conditions → Power Analysis → Results) | Working | None |
| **brms backend** (MCMC via Stan) | Working | None |
| **ancova_cont_2arms** predefined model | Working, tested | None |
| **ancova_cont_3arms** predefined model | Working, tested | None |
| **ancova_bin_2arms** predefined model | Working, tested | None |
| **Fixed designs** | Working | None |
| **Group sequential designs** | Working (except interim bug) | Fix interim_functions.R bug |
| **Boundary functions** (OBF, Pocock, HSD, constant, linear, power) | Working | None |
| **Sample accrual** (uniform, poisson, ramp, custom) | Working | None |
| **Dropout modeling** | Working | None |
| **Calendar-time analysis** | Working | None |
| **Event-driven analysis** | Working | None |
| **ROPE-based decisions** (pr_eff/pr_fut, dec_eff/dec_fut) | Working | None |
| **Interim functions** (continue, futility_only, success_futility) | Bug in column names | Fix pr_scs→pr_eff, pr_ftl→pr_fut |
| **Plotting** (power_curve, heatmap, comparison, accrual) | Working (2 bugs) | Fix dead code + factor mismatch |
| **Reporting** (power, stopping, convergence) | Working | None |
| **get_code() reproducibility** | Working, 32 tests | None |
| **Analytical power** (ancova_cont_2arms) | Working | None |
| **Verbosity controls** | Working | None |
| **Model caching** | Working, untested | Add tests |
| **resummarize_boundaries()** | Working | None |
| **build_sim_fn() custom models** | Working | None |
| **link() for co-varying params** | Working | None |

### IN: New v1 Features (need implementation)

| Feature | Effort | Notes |
|---------|--------|-------|
| **`survival_2arms` predefined model** | Medium | Promote builder to registry entry, add tests |
| **`as.data.frame()` methods** for core classes | Low | Thin S3 wrappers |
| **`boundary_wang_tsiatis()`** | Low | Follows existing boundary pattern |
| **Fix `model_name` param/property ambiguity** | Low-Medium | Rename before first public release |
| **Package-level documentation** (`?rctbayespower`) | Low | Roxygen block in `rctbayespower-package.R` |
| **BayesFlow model interface spec** | Medium | Dev doc for `rctbp-bf-training` repo |
| **Validation articles** (pkgdown) | Medium-High | One per model, doubles as user guide |

### IN: v1 Bug Fixes & Quality (required before release)

| Item | Source | Effort |
|------|--------|--------|
| Fix `pr_scs`/`pr_ftl` column name bug in interim_functions.R | API audit #1 | Low |
| Fix stale error messages in interim_functions.R (`thresh_scs`→`thr_fx_eff`, etc.) | API audit #5 | Low |
| Fix stale roxygen with old param names | API audit #2 | Low |
| Fix broken roxygen examples in interim_functions.R | API audit #3 | Low |
| Fix plot_power_curve.R dead code (`group_by == "outcome"`) | Test coverage plan | Low |
| Fix plot_power_curve.R factor mismatch (Success→Efficacy) | Test coverage plan | Low |
| Add genuinely missing `build_conditions()` validations (see Phase 2) | API audit #6 | Medium |
| Test coverage: plot functions | Test coverage plan | Medium |
| Test coverage: model_cache.R | Test coverage plan | Low |
| Test coverage: setup_python.R | Test coverage plan | Low |
| Fix dev docs with wrong column names (01_architecture, 03_workflow) | API audit #4 | Low |
| Remove deprecated aliases and dead wrappers (see API Cleanup) | Pre-release cleanup | Low |
| R CMD check: 0 errors, 0 warnings, 0 notes | CRAN requirement | Medium |

### IN: v1 Optimization (experimental — not a release blocker)

Ships with v1 but labeled experimental. Redesign if time allows, otherwise current implementation stays.

| Item | Status |
|------|--------|
| `optimize_sample_size()` unified API | Plan done, implementation TODO |
| Single-objective BO with feasibility score | Plan done, implementation TODO |
| Pareto multi-objective (refactored) | Plan done, implementation TODO |
| `rctbp_sample_size_result` class | Plan done, implementation TODO |
| `find_surrogate_optimum()` / `find_probabilistic_optimum()` (Pareto post-hoc) | Kept per optimization plan |
| Dead code removal (~2,000 LOC) | Plan done, implementation TODO |

See `dev/plans/optimization-redesign.md`.

### OUT: Deferred to post-v1

| Feature | Reason |
|---------|--------|
| **Adaptive designs** (RAR, SSR, Thompson) | Infrastructure exists but no working interim modification functions. |
| **New `trial_type` values** | Existing `trial_type` property stays as-is. No new values in v1. |
| ~~**Beta proportion model**~~ | Implemented — promoted to v1 registry model. |
| **Count/Poisson models** | Not yet implemented. |
| **Probit binary model** | Not yet implemented. |
| **Pipe-friendly aliases** (`add_conditions`, `run_power`) | Shiny GUI will cover low-barrier entry. |
| **`quick_power()` / `quick_sample_size()`** | Shiny GUI will cover low-barrier entry. |
| **`extract()` generic** | `as.data.frame()` covers the primary need. |
| **`decision_criteria()` helper** | Pairs with `reanalyze()`, ship together post-v1. |
| **`reanalyze()`** | Pairs with `decision_criteria()`, ship together post-v1. |
| **Endpoint-specific constructors** (`design_continuous`, etc.) | Not enough models yet to justify. |
| **`available_plots()`** | Part of future plotting system review. |
| **`compare_designs()` infrastructure** | Not yet implemented. |
| **Sensitivity analysis** | Not yet implemented. |
| **GPU support** | BayesFlow-dependent. |
| **Platform/basket/umbrella trials** | Long-term roadmap. |
| **Prior borrowing** (power/commensurate/MAP) | Long-term roadmap. |
| **Varying brms priors** | High effort, niche value. |
| **Shiny GUI** | Post-v1, planned. |

---

## v1 Predefined Models

| Registry Name | Outcome | Arms | Builder Function | Status |
|---------------|---------|------|------------------|--------|
| `ancova_cont_2arms` | Continuous | 2 | `build_model_ancova_cont_2arms()` | Ready |
| `ancova_cont_3arms` | Continuous | 3 | `build_model_ancova_cont_3arms()` | Ready |
| `ancova_bin_2arms` | Binary (logit) | 2 | `build_model_ancova_bin_2arms()` | Ready |
| `ancova_prop_2arms` | Proportion (Beta) | 2 | `build_model_ancova_prop_2arms()` | Ready (just merged) |
| `survival_2arms` | Time-to-event | 2 | `build_model_survival_2arms()` | Needs registry entry + tests |

**Custom models:** Users can always build arbitrary models via `build_sim_fn()` + `build_design(sim_fn = ..., inference_model = ...)`.

---

## v1 Design Types

| Type | API | Status |
|------|-----|--------|
| Fixed | `build_design()` with no `analysis_at` | Ready |
| Group Sequential | `build_conditions(analysis_at = c(...))` | Ready (after interim bug fix) |

**Not in v1:** New `trial_type` values, adaptive designs with mid-trial modifications.

---

## v1 Boundary Functions

| Function | Type | Status |
|----------|------|--------|
| `boundary_obf()` | O'Brien-Fleming | Ready |
| `boundary_pocock()` | Pocock | Ready |
| `boundary_hsd()` | Hwang-Shih-DeCani | Ready (requires gsDesign) |
| `boundary_constant()` | Constant threshold | Ready |
| `boundary_linear()` | Linear interpolation | Ready |
| `boundary_power()` | Power family | Ready |
| `boundary_wang_tsiatis()` | Wang-Tsiatis (generalized) | **New — needs implementation** |

---

## v1 Exported API Surface

### Core Pipeline (5 functions)

```
build_design()          — Create design from predefined or custom model
build_conditions()      — Create parameter grid
power_analysis()        — Configure power analysis
run()                   — Execute power analysis
build_sim_fn()          — Wrap custom simulation function
```

### Discovery (5 functions)

```
show_predefined_models()  — List registry models
show_condition_args()     — Required/optional condition params
show_target_params()      — Available target parameters
show_boundaries()         — List available boundary functions
required_fn_args()        — Required simulation params
```

### Boundaries (9 functions)

```
boundary_obf()            — O'Brien-Fleming
boundary_pocock()         — Pocock
boundary_hsd()            — Hwang-Shih-DeCani
boundary_constant()       — Constant
boundary_linear()         — Linear
boundary_power()          — Power family
boundary_wang_tsiatis()   — Wang-Tsiatis (generalized, NEW)
compare_boundaries()      — Visual comparison
resummarize_boundaries()  — Post-hoc re-analysis
```

### Conditions Helpers (2 functions)

```
dropout()                 — Dropout specification constructor
link()                    — Co-varying parameter link
```

### Interim Functions (3 functions)

```
interim_continue()          — Always continue
interim_futility_only()     — Stop for futility
interim_success_futility()  — Stop for either
```

### Optimization (experimental)

```
optimize_sample_size()         — Unified sample size optimization (single/pareto)
search_p_alloc()               — Allocation probability search
search_looks()                 — Interim look timing search
find_surrogate_optimum()       — Pareto post-hoc: surrogate-based selection
find_probabilistic_optimum()   — Pareto post-hoc: probabilistic selection
```

### Data Access (NEW)

```
as.data.frame.rctbp_power_analysis()  — Results summary as data frame
as.data.frame.rctbp_conditions()      — Conditions grid as data frame
as.data.frame.rctbp_design()          — Design summary as data frame
as.data.frame.rctbp_pareto_result()   — Pareto front as data frame
```

### Visualization (2 S3 methods + 1 function)

```
plot.rctbp_power_analysis()   — Plot power results
plot.rctbp_pareto_result()    — Plot optimization results
compare_power_methods()       — Compare backends/methods
```

### Reporting (7 functions)

```
report()                  — Generic report dispatcher
report_power()            — Power summary
report_stopping()         — Early stopping summary
report_stopping_by_look() — Per-look stopping
report_convergence()      — MCMC diagnostics
export_report()           — Export to file
get_code()                — Reconstruct R code
```

### BayesFlow (experimental — may not work without Python/BF setup)

```
check_bf_status()         — Detailed BF diagnostics
get_bf_env_info()         — BF environment information
setup_bf_python()         — Setup Python env
install_bf_dependencies() — Install BF packages
verify_bf_installation()  — Verify BF install
```

**Note:** `check_bf_available()` and `init_bf()` are internal. Used by `build_design(backend = "bf")` automatically.

### Utilities (8 functions)

```
set_verbosity() / get_verbosity() / with_verbosity()
set_output_mode() / get_output_mode() / with_output_mode()
analytical_power_ancova_cont_2arms()
f2_from_params_ancova_cont_2arms()
```

### Cache (3 functions)

```
clear_model_cache()       — Clear cache
get_cache_size()          — Check cache size
get_model_cache_dir()     — Cache location
```

### API Cleanup: Remove Before v1

Since v1 is the first public release, there are no external users to protect. Remove dead wrappers to keep the API surface clean:

| Function | Action | Rationale |
|----------|--------|-----------|
| `list_predefined_models()` | Remove | Replaced by `show_predefined_models()` |
| `build_model()` / `get_model()` | Remove | Replaced by `build_design(model_name = ...)` |
| `rctbp_model` class | Remove | Merged into `rctbp_design` |
| `boundary_obf_threshold()` / `boundary_pocock_threshold()` | Remove | Replaced by `boundary_obf(threshold = ...)` |
| `optimize_power_n()` / `optimize_power_effect()` / `optimize_effect_n()` | Remove | Replaced by `optimize_sample_size()` |
| `report_early_stopping()` | Remove | Alias for `report_stopping()` |
| `report_conditions()` | Remove | Alias for `report_power()` |

---

## v1 API Audit Fixes

These are from `dev/25_api_audit_findings.md` and must be resolved before v1:

| # | Issue | Severity | Action |
|---|-------|----------|--------|
| 1 | `pr_scs`/`pr_ftl` bug in interim_functions.R | Critical | Fix: replace with `pr_eff`/`pr_fut` |
| 2 | Stale roxygen with old param names | High | Fix: update to `thr_dec_eff` etc. |
| 3 | Broken roxygen examples in interim_functions.R | High | Fix: rewrite examples |
| 4 | Dev docs with wrong column names | High | Fix: update 01_architecture, 03_workflow |
| 5 | Stale comments/error messages with old naming | Low | Fix in Phase 1 (interim_functions.R) and Phase 6 (other files) |
| 6 | Missing validations in build_conditions() | Medium | Fix: see Phase 2 (only genuinely missing items) |
| 7 | model_name param/property ambiguity | Medium | Fix in Phase 4 (first release = right time) |
| 8 | backend_args_brms vs brms_args overlap | Medium | **Defer** — document precedence in v1, fix post-v1 (backend system review) |
| 9 | Incomplete linked deprecation | Low | Fix: error instead of silently ignoring |
| 10 | Missing constant property on conditions | Low | **Defer** — get_code() works via .call attribute |

---

## Implementation Order

The phases below have dependencies. Each phase should be shipped as a separate PR.

### Phase 1: Critical Bug Fixes (no dependencies)

Fix the bugs that cause silent failures in running code:
- `interim_functions.R`: `pr_scs` → `pr_eff`, `pr_ftl` → `pr_fut`
- `interim_functions.R`: stale error messages referencing `'thresh_scs'`/`'p_sig_scs'` → `'thr_fx_eff'`/`'thr_dec_eff'`
- `plot_power_curve.R`: `"outcome"` → `"decision"` dead code fix
- `plot_power_curve.R`: `"Success"` → `"Efficacy"` factor mismatch
- `build_conditions()`: incomplete `linked` deprecation → error explicitly

**Tests:** Add regression tests for each bug fix.

### Phase 2: Missing Validations (no dependencies)

**Pre-step:** Audit `build_conditions()` to confirm which validations already exist. Known status:
- `p_alloc` sum check: **Already implemented** (class_conditions.R:536-558) — verify tests exist
- ROPE inversion warning: **Already implemented** (class_conditions.R:598-608) — verify tests exist
- `analysis_at` sorted and no duplicates: **Missing** — implement
- `thr_dec_eff`/`thr_dec_fut`/`thr_fx_eff`/`thr_fx_fut` required early-check: **Missing** — implement
- `n_total` provided early-check: **Status unclear** — audit, implement if missing

**Tests:** Add validation test cases for new validations; verify existing ones have coverage.

### Phase 3: Test Coverage (depends on Phase 1 bug fixes)

From `dev/plans/test_coverage_gaps.md`:
- Build `mock_power_analysis()` helper first (prerequisite for plot tests)
- Test plot dispatcher, power_curve, heatmap, comparison, accrual
- Test model_cache.R (with temp dirs)
- Test setup_python.R (mock reticulate)

### Phase 4: New v1 Features (depends on Phases 1-2)

New features added during v1 scoping:
- **`survival_2arms` registry entry**: Add to model registry, create sim_fn case, end-to-end tests
- **`as.data.frame()` methods**: Thin S3 wrappers for power_analysis, conditions, design, pareto_result
- **`boundary_wang_tsiatis()`**: Generalized boundary family (delta parameter), follows existing pattern
- **Fix `model_name` ambiguity**: Rename param or property, update all references (API audit #7)

### Phase 5: Optimization Redesign (independent, experimental)

From `dev/plans/optimization-redesign.md`. Not a release blocker — ships as experimental.
- Phase 1: File reorg + dead code removal
- Phase 2: Unified API + single-objective BO
- Phase 3: Tests + validation
- Phase 4: Cleanup + documentation

**Checkpoint:** After completion, run `devtools::check()`. Must not introduce new errors/warnings.

### Phase 6: Documentation & Polish (depends on Phases 1-4)

- Fix all stale roxygen (API audit #2, #3)
- Fix stale comments in remaining files (API audit #5, excluding interim_functions.R already fixed in Phase 1)
- Fix dev docs (API audit #4)
- Remove deprecated wrappers (see API Cleanup table)
- Document `backend_args_brms` vs `brms_args` precedence
- Package-level documentation (`?rctbayespower`)
- BayesFlow model interface spec (`dev/bf_model_interface_spec.md`)
- Audit `@export` tags: ensure all v1 API functions are exported, remove exports for removed functions
- Review all `@examples` in exported functions
- Ensure `show_predefined_models()` includes all 4 v1 models

### Phase 7: Validation Articles (depends on Phase 4)

Pkgdown articles, not installed with package. One per predefined model:
- `ancova_cont_2arms` — continuous 2-arm (fixed + group sequential + accrual)
- `ancova_cont_3arms` — continuous 3-arm
- `ancova_bin_2arms` — binary 2-arm
- `ancova_prop_2arms` — proportion 2-arm (Beta regression)
- `survival_2arms` — survival 2-arm
- Include posterior quality checks (coverage, calibration) using external packages
- Set up pkgdown articles directory and `_pkgdown.yml` configuration

### Phase 8: R CMD check Clean (depends on Phases 6-7)

- Resolve all errors, warnings, notes
- Verify on R 4.1+ (minimum supported version)
- Verify on macOS, Windows, Linux (CI matrix)
- LICENSE and CRAN-required metadata check

---

## What v1 Does NOT Promise

To set expectations clearly:

1. **No BayesFlow inference.** The brms backend is the only production backend. BayesFlow functions are exported for developers/testing but marked experimental. A model interface spec is provided so the training repo can target v1 compatibility.
2. **No adaptive trial designs.** Group sequential with stopping rules: yes. Mid-trial parameter modifications (RAR, SSR): no.
3. **No GUI.** CLI/scripting only. Shiny GUI planned for post-v1.
4. **Optimization is experimental.** Works but may change. Not a release blocker.

---

## Success Criteria

v1 is ready to ship when:

- [ ] All Phase 1-4, 6-8 items complete (Phase 5 optional)
- [ ] R CMD check passes with 0 errors, 0 warnings, 0 notes
- [ ] All 5 predefined models work end-to-end (fixed + group sequential)
- [ ] All existing tests pass after each phase (no regressions)
- [ ] Test suite passes on R 4.1+ across macOS, Windows, Linux
- [ ] get_code() round-trips for all predefined model workflows
- [ ] No known silent failures in core pipeline
- [ ] Exported API surface documented (all @export functions have roxygen)
- [ ] No deprecated wrappers remain in NAMESPACE (clean API surface for first release)
- [ ] Validation articles published for all 5 predefined models
- [ ] BayesFlow model interface spec documented
