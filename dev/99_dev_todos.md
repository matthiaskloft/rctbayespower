# Development TODOs

> Last reviewed: 2026-03-11
> Organized by priority tier. Cross-references to dev docs in brackets.

---

# CRITICAL — Blocks BayesFlow Backend

### BayesFlow Model Retraining [13] — IN PROGRESS (outsourced to dedicated repo)

The pre-trained model (`dev/bf_models/ancova_cont_2arms.keras`) **cannot load** with any released BayesFlow 2.0.x — `CouplingFlow` fails on 1D inference (`Dense(units=0)`). The BF backend silently falls back to brms. Training outsourced to `rctbp-bf-training` repo.

- [ ] Create training script using `FlowMatching` (handles 1D natively)
- [ ] Train on ~100k samples, validate posterior coverage
- [ ] Save trained model and upload to GitHub release tag `bf-models-v0.0.0.9000`
- [ ] Smoke test R↔Python round-trip
- [ ] Remove `arm → group` workaround in `R/class_sim_fn.R:37-39` (retrain model to match sim_fn output directly)
- [ ] Document BayesFlow training parameter names per registry model in a dev doc (e.g., which sim_fn params map to BF inference targets, summary stats, etc. for each predefined model)

### BayesFlow Python Environment [11] — blocked on retraining

- [ ] Set up reproducible Python environment with BayesFlow 2.0
- [ ] Test R↔Python round-trip with trained model
- [ ] Add automatic brms fallback message when BF unavailable (currently silent)

---

# HIGH — v1 Release Blockers

### ~~Binary Outcome Models~~ (DONE)

Implemented as `build_model_ancova_bin()` with `ancova_bin_2arms` predefined model. Log-odds parameterization — `compute_measures()` works unchanged (parameter-agnostic ROPE decisions). Probability-scale output columns deferred.

- [x] `build_model_ancova_bin()` — logistic regression sim_fn + brms formula
- [x] Binary data generation function (`create_ancova_bin_sim_fn()`, batch fn)
- [x] Registry: `ancova_bin_2arms` predefined model
- [x] Tests for binary outcome pipeline
- [ ] Adapt `compute_measures()` for proportions / odds ratios — DEFERRED (log-odds sufficient for v1)
- [ ] BayesFlow training for binary model (after BF retraining blocker resolved)

### ~~Duplicate `apply_simplex_transforms()` Definition~~ (DONE)

Fixed in PR #19 — duplicate removed from `pareto_optimize.R`, canonical implementation in `optimization_internal.R` retained.

### Test Coverage Gaps [06]

~960 tests exist, but several modules have **zero coverage**:

- [ ] ~~`R/pareto_optimize.R` and optimization wrappers~~ — deferred pending optimization redesign
- [ ] `R/plot_*.R` (plotting functions) — not tested
- [ ] `R/model_cache.R` — not tested
- [ ] `R/setup_python.R` — not tested

### ~~Bayesian OBF Boundary Vector Fix~~ (DONE)

Fixed: `resolve_boundary_vector_from_fracs()` pre-resolves thresholds before the sequential loop in both brms and BF backends. All boundary functions (OBF, Pocock alpha, HSD) now receive the full info_frac vector.

- [x] Pre-resolve all thresholds as a vector at start of sequential loop (not per-look)
- [x] Applies to all boundary functions (OBF threshold/alpha, Pocock alpha, HSD)

### Optimization Module Redesign (experimental for v1)

Complete refactor/redesign of the optimization module (`R/pareto_optimize.R`, `R/optimization_internal.R`, and related wrappers). Ships as experimental in v1 — not a release blocker. See `dev/plans/optimization-redesign.md`.

- [x] Design new optimization API and architecture
- [ ] Implement refactored optimization module
- [ ] Write tests after redesign is stable

### ~~v1 Feature Spec — Planning~~ (DONE)

Documented as `dev/plans/v1-feature-spec.md`. Scope finalized in conversation 2026-03-11.

- [x] Enumerate v1 features (models, backends, design types, analysis types)
- [x] Decide what is in-scope vs deferred to post-v1
- [x] Document as `dev/plans/v1-feature-spec.md`

### ~~Survival Model → Predefined Registry~~ (DONE)

Refactored `build_model_survival_2arms()` into registry pattern: `build_model_survival_exp()` (generic builder), `build_model_survival_exp_2arms()` (2-arm wrapper), `create_survival_exp_sim_fn()` (sim_fn factory). Registered as `survival_exp_2arms` predefined model.

- [x] Add `survival_exp_2arms` to model registry in `load_predefined_model_components()`
- [x] Add `create_sim_fn_for_model()` case for survival
- [x] `show_predefined_models()` includes `survival_exp_2arms`
- [x] Comprehensive tests (sim_fn, builder, registry)
- [ ] Verify `get_code()` round-trip for survival workflows — DEFERRED

### `as.data.frame()` Methods for Core Classes

Thin S3 wrappers returning the relevant data frame from each core class. Low effort, high usability impact.

- [ ] `as.data.frame.rctbp_power_analysis()` → `results_summ`
- [ ] `as.data.frame.rctbp_conditions()` → conditions grid
- [ ] `as.data.frame.rctbp_design()` → design summary
- [ ] `as.data.frame.rctbp_pareto_result()` → pareto front
- [ ] Tests for each method

### `boundary_wang_tsiatis()`

Generalized Wang-Tsiatis boundary family. Subsumes OBF (`delta = 0.5`) and Pocock (`delta = 0`) as special cases. Follows existing boundary function pattern.

- [ ] Implement `boundary_wang_tsiatis(delta)` in `R/boundaries.R`
- [ ] Add to `show_boundaries()` output
- [ ] Tests
- [ ] Roxygen documentation

### BayesFlow Model Interface Spec

Dev doc specifying the exact contract between `rctbayespower` and the `rctbp-bf-training` repo. Unblocks model training.

- [ ] Document input format (summary statistics per model)
- [ ] Document output format (posterior samples, shape, naming)
- [ ] Document file format (.keras/.pkl) and naming conventions
- [ ] Document parameter mapping (sim_fn params → BF inference targets)
- [ ] Document per-model specs for all 4 v1 registry models
- [ ] Save as `dev/bf_model_interface_spec.md`

### Validation Articles (pkgdown)

End-to-end validation articles published to pkgdown site (not installed with package). Double as user guides and validation evidence. One per model, covering fixed + group sequential + accrual.

- [ ] Plan validation article structure
- [ ] Article: `ancova_cont_2arms` (continuous 2-arm)
- [ ] Article: `ancova_cont_3arms` (continuous 3-arm)
- [ ] Article: `ancova_bin_2arms` (binary 2-arm)
- [ ] Article: `ancova_prop_2arms` (proportion 2-arm, Beta regression)
- [ ] Article: `survival_2arms` (survival 2-arm)
- [ ] Include posterior quality checks (coverage, calibration) using external packages
- [ ] Set up pkgdown articles directory

### Fix `model_name` Param/Property Ambiguity

`build_design(model_name = ...)` parameter and `design@model_name` property return different things. First public release is the right time to fix this. Part of the predefined model system review.

- [ ] Decide: rename param to `predefined_model` or rename property `@model_name` to `@display_name`
- [ ] Update all references
- [ ] Update roxygen docs
- [ ] Tests

### Package-Level Documentation

`?rctbayespower` help page — overview, links to key functions, basic usage example.

- [ ] Create `R/rctbayespower-package.R` with roxygen block
- [ ] Include overview, key functions, basic workflow example

### Validation Strategy — Planning

Plan a comprehensive validation strategy covering all features and all models. Ensure correctness of the full pipeline (design → conditions → power analysis → results) across:

- [ ] All predefined models (`ancova_cont_2arms`, `ancova_bin_2arms`, future models)
- [ ] Both backends (brms, BayesFlow)
- [ ] Fixed and group-sequential designs
- [ ] Accrual, dropout, and event-driven subsetting
- [ ] All boundary functions (OBF, Pocock, custom)
- [ ] Decision criteria (ROPE-based effectiveness, futility)
- [ ] Edge cases (single condition, single look, all-stop, all-continue)

### Validation Strategy — Implementation

Implement the validation strategy from the plan above. Write and run validation tests/checks for each feature × model combination.

- [ ] Implement validation suite per the plan
- [ ] Document validation results

### Automation Recommendations — Implementation

Implement automation additions from `dev/plans/automation_recommendations.md`.

- [ ] Phase 1: Install context7 MCP server
- [ ] Phase 2: Create `/rctbp-validation` skill (orchestrator) + `integration-tester` agent (worker)
- [ ] Phase 3: Related test hook (optional)

---

# MEDIUM — Post-v1 Features

### API Improvements [24]

- [ ] Pipe-friendly aliases: `add_conditions()`, `run_power()` (§3.6) — deferred, Shiny GUI covers low-barrier entry
- [ ] `quick_power()` / `quick_sample_size()` shortcut functions (§3.3) — deferred, Shiny covers this
- [ ] `extract()` generic for result components (§3.9) — deferred, `as.data.frame()` covers primary need
- [ ] `decision_criteria()` helper + integration (§3.14) — deferred, pairs with `reanalyze()`
- [ ] `reanalyze()` — cached simulation re-analysis, generalizes `resummarize_boundaries()` (§3.15)

### `trial_type` Parameter [23]

Replace scattered boolean flags with declarative `trial_type` in `build_design()`:

| Type | Status |
|------|--------|
| `"fixed"` | Done (current default) |
| `"group_sequential"` | Done |
| `"adaptive"` | Infrastructure exists, needs testing |
| `"platform"` | Future |
| `"dose_finding"` | Future |

- [ ] Implement `trial_type` parameter in `build_design()`
- [ ] Deprecate `adaptive = TRUE/FALSE` in `build_conditions()`

### New Outcome Models [21, 23, 24 Phase D]

- [x] Beta proportion ANCOVA — `ancova_prop_2arms` (done: PR #24, predefined model in registry)
- [ ] Count outcomes — `build_model_count_2arms()` (Poisson, negative binomial)
- [ ] Survival outcomes — registry refactor planned (`survival_exp_2arms`), see `dev/plans/survival_registry_refactor.md`
- [ ] Probit binary ANCOVA — `ancova_bin_probit_2arms`. Binary outcomes with probit link (`family = bernoulli(link = "probit")`). Effects on probit scale. Alternative to logit for latent-normal interpretation.
- [x] Binary ANCOVA (logit) — `build_model_ancova_bin()` + `ancova_bin_2arms`
- [ ] Endpoint-specific convenience constructors: `design_continuous()`, `design_binary()`, etc.

### Sequential / Adaptive Extensions [20, 21, 23]

- [ ] Sample size re-estimation: `interim_ssr()` (conditional power, blinded variance)
- [ ] Response-adaptive randomization: `interim_rar()`, `interim_thompson()`
- [ ] ~~`boundary_wang_tsiatis()`~~ — promoted to HIGH (v1)
- [ ] New boundaries: `boundary_hsd()` (already implemented)
- [ ] Per-`n_total` `analysis_at` specification (named list keyed by `n_total`)
- [ ] Interim-specific plot types (stopping probability by look, power trajectory)

### Varying brms Priors

Priors are baked into pre-compiled Stan model at design time. Varying them requires recompilation per condition.

- [ ] Parallelization of model compilation
- [ ] Avoid expanding compiled models into conditions × n_sims list

### Posterior Quality Diagnostics [11]

Needed for validation articles (v1). Compute from `results_raw` using existing packages (`SBC`, `bayesplot`, `posterior`) where possible. Consider lightweight helpers if recurring patterns emerge during validation.

- [ ] Coverage analysis — empirical coverage of credible intervals (computed from results_raw, true values known from conditions)
- [ ] Calibration — posterior mean vs true parameter (straightforward from results_raw)
- [ ] SBC — use `SBC` package or compute rank statistics from results_raw
- [ ] Posterior Predictive Checks — use `bayesplot` on brmsfit objects
- [ ] Decide: utility helpers in package vs validation-article-only code

### Backend System Review

Review the backend argument system and broader backend architecture. Currently `backend_args_brms` lives on the design and `brms_args` on the power analysis — unclear precedence. Also review BayesFlow fallback behavior, backend detection, and how backend args flow through the pipeline.

- [ ] Resolve `backend_args_brms` vs `brms_args` overlap (API audit #8) — pick one or define clear precedence
- [ ] Audit backend arg flow: build_design → build_conditions → power_analysis → worker
- [ ] Review BayesFlow silent fallback — should it warn more prominently?
- [ ] Review `backend_args_bf` structure and defaults
- [ ] Consider whether backend args belong on design or power_analysis (or both with merge rules)

### Predefined Model System Review

Review and rework the predefined model registry, loading, and naming system. The model registry is hardcoded in `class_design.R` — audit for extensibility, naming consistency, and correct integration with both backends. Includes fixing the `model_name` param/property ambiguity (API audit #7).

- [ ] Fix `model_name` param vs `@model_name` vs `@display_name` vs `@predefined_model` ambiguity
- [ ] Audit model registry structure (currently hardcoded list in `load_predefined_model_components()`)
- [ ] Review `get_model_builder()` / `create_sim_fn_for_model()` dispatch pattern
- [ ] Consider whether registry should be a standalone file (e.g., `R/model_registry.R`)
- [ ] Ensure all 5 v1 registry models (cont_2arms, cont_3arms, bin_2arms, prop_2arms, survival_2arms) follow same pattern
- [ ] Review `show_predefined_models()` output for accuracy and usefulness
- [ ] Document how to add new predefined models (for contributors)

### Labeling/Naming System Review

Audit `design_name`, `display_name`, and `model_name` properties across core classes for consistency and usability. The `model_name` param/property ambiguity (API audit #7) is part of this. Consider whether `design_name` should propagate to conditions and power_analysis print output.

- [ ] Audit `design_name` vs `display_name` vs `model_name` vs `predefined_model` properties
- [ ] Resolve API audit #7 (model_name param/property ambiguity)
- [ ] Ensure labels propagate through pipeline (design → conditions → results → plots)
- [ ] Consider label support on conditions (e.g., naming condition sets)

### Plotting System Review

Complete review of the plotting system (`plot_power_analysis.R`, `plot_power_curve.R`, `plot_heatmap.R`, `plot_comparison.R`, `plot_accrual.R`, `plot_optimization.R`, `plot_helpers.R`). Audit plot types for consistency, discoverability, and completeness. Consider `available_plots()`, interim-specific plots, and whether the auto-detection logic is robust.

- [ ] Audit all plot types for consistency and quality
- [ ] Review auto-detection logic in dispatcher
- [ ] Consider `available_plots()` discovery function
- [ ] Consider interim-specific plots (stopping probability by look, power trajectory, expected N)
- [ ] Consider forest-plot style visualization for multi-condition comparisons
- [ ] Ensure plot themes and color palettes are consistent
- [ ] Review interactive (plotly) vs static (ggplot2) output paths

### Reporting & Summary Infrastructure Review

Review and rework the reporting and summary system (`report_builders.R`, `report_renderers.R`, `s3_wrappers.R`). Audit print/summary/report methods for consistency, coverage, and usefulness. Consider whether `extract()`, structured report config, and `as.data.frame()` methods fit into a unified data access pattern.

- [ ] Audit all print/summary/report methods for consistency
- [ ] Identify gaps (classes without useful print/summary)
- [ ] Design coherent data access pattern (print vs summary vs report vs as.data.frame vs extract)
- [ ] Implement improvements

### CRAN Preparation [40]

Deferred until all v1 features are implemented.

- [ ] R CMD check: 0 ERRORs, 0 WARNINGs, 0 NOTEs
- [ ] Vignettes for core workflows (optional for CRAN — may emerge from validation strategy as executable end-to-end examples)
- [ ] Package-level documentation (`?rctbayespower`)
- [ ] LICENSE and CRAN-required metadata
- [ ] Determine which pre-defined models to include in v1

---

# LOW — Long-term / Post-v2

### GPU Support [12]

BayesFlow backend defaults to CPU-only PyTorch. Five planned sprints:

- [ ] Phase 1: GPU detection (`check_gpu_available()`)
- [ ] Phase 2: Backend selection enhancement (`init_bf_python()` with device param)
- [ ] Phase 3: Configuration system (`configure_bf()`, `get_bf_config()`)
- [ ] Phase 4: Integration with `power_analysis()` output
- [ ] Phase 5: Documentation (GPU setup vignette) + tests

### Advanced Trial Designs [21, 22]

- [ ] Platform / basket / umbrella trials (dynamic arm management, shared control)
- [ ] Dose finding (CRM, BOIN)
- [ ] Enrichment / biomarker-driven subgroup selection
- [ ] Bayesian seamless Phase II/III

### Prior Borrowing [22]

- [ ] Power prior (weighted historical data)
- [ ] Commensurate prior (adaptive borrowing)
- [ ] Robust MAP prior (meta-analytic framework)

### Reporting & Comparison [24, Phase C/E]

- [ ] `compare_designs()` infrastructure (§3.13)
- [ ] `report_config()` structured report configuration — HTML/PDF/Word (§3.16)
- [ ] Performance score for Bayesian designs (efficiency, precision, accuracy) (§3.11)
- [ ] Conditional power computation given interim data (§3.13)
- [ ] Multi-parameter decision rules / co-primary endpoints (§3.18)

### Sensitivity Analysis (deferred post-v1)

`sensitivity_analysis()` — vary decision thresholds without re-simulation.

- Varying `thr_dec_eff`/`thr_dec_fut` is exact (builds on `resummarize_boundaries()`)
- Varying `thr_fx_eff`/`thr_fx_fut` (ROPE) only approximate from stored quantiles
- Prior sensitivity requires re-simulation entirely
- Would need `rctbp_sensitivity_analysis` S7 class with `plot()`/`summary()`

### Shiny GUI

Interactive Shiny app for the package — covers low-barrier entry for non-programmers and exploratory use. Replaces the need for convenience wrappers like `quick_power()` and pipe-friendly aliases.

- [ ] Design the Shiny app scope (which workflows to support)
- [ ] Implement as companion package or built-in `launch_app()`
- [ ] Support all predefined models, fixed + group sequential designs
- [ ] Interactive plotting and result export

### Competitor-Identified Gaps [40]

- [ ] Mixed-model / repeated measures (longitudinal therapy trials)
- [ ] Cohen's d / standardized effect sizes
- [ ] Factorial designs (treatment × moderator)
- [ ] Cluster randomization (therapist/site)

### BayesFlow Extensions [11]

- [ ] Train 3-arm ANCOVA model
- [ ] Variable sample size support in BF models
- [ ] Benchmark BF vs brms performance

---

# DONE (Archive)

<details>
<summary>Completed items (click to expand)</summary>

### class 'model'
- ~~print() should show default values for the data_simulation_fn~~ (done: "Simulation Function Parameters" section)
- ~~allocation probs need to be disambiguated~~ (done: length validation, p_alloc data flow fix, floating-point tolerance, docs)

### class 'conditions'
- ~~implement check that all required params are specified~~ (done)
- ~~no differentiation between condition_values and static_values~~ (done: grid shows only varying cols)
- ~~automatic detection of static values and varying parameters~~ (done: warns when crossed param has 1 level)
- ~~n_total always included in grid~~ (done: single-value columns excluded)

### Power Analysis
- ~~save full quantile profile~~ (done: 9 quantile columns post_q025-post_q975)
- ~~report for convergence~~ (done: `report_convergence()`, `report(x, "convergence")`)

### Across Package
- ~~use cli for outputs~~ (done: all active code uses cli)

### Sample Accrual [25]
- ~~Phase 1: enrollment time generation, calendar-time subsetting~~ (done)
- ~~Phase 2: reporting & accrual plot type~~ (done)
- ~~Phase 3: dropout / loss-to-follow-up~~ (done)
- ~~Phase 4: survival/event-driven integration~~ (done)
- ~~BayesFlow batch processing with variable completer counts~~ (fixed: split-by-size batching)
- ~~effective_n for non-stopped target_not_met sims~~ (fixed: per-sim n_analyzed_final)
- ~~Threshold resolution before dropout-aware subsetting~~ (fixed: info_frac after subset)

### Next Development Tasks
- ~~Integration tests~~ (done: 5 tests in `test-integration.R`)
- ~~get_code() reproducibility~~ (done: 32 tests in `test-get_code.R`)
- ~~BayesFlow model training~~ (outsourced to dedicated repo)
- ~~Sample accrual MVP~~ (done: Phase 1 + 2 merged)

### Infrastructure
- ~~Test coverage: `test-power_grid_analysis.R`~~ (done: 77 tests)

### v1 Blockers (resolved)
- ~~Binary outcome models~~ (done: PR #20/#21, `ancova_bin_2arms` predefined model)
- ~~Proportional outcome models~~ (done: PR #24, `ancova_prop_2arms` predefined model)
- ~~Duplicate `apply_simplex_transforms()`~~ (done: PR #19, canonical in `optimization_internal.R`)
- ~~OBF boundary vector pre-resolution~~ (done: PR #17)

</details>
