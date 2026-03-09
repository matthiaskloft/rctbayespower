## class 'model'

* dots for brms arguments like link functions for family
  -> implemented link\_sigma as argument for ancova
* ~~print() should show default values for the data\_simulation\_fn as well as required arguments to specify~~ (done: "Simulation Function Parameters" section in print output)

* allocation probs need to be disambiguated (in progress)


## class 'conditions'

* ~~implement check that all required params are specified~~ (done)
* ~~no differentiation between condition\_values and static\_values~~ (done: grid shows only varying cols; fixed params listed below)
* ~~automatic detection of static values and varying parameters~~ (done: warns when crossed param has 1 level)
* ~~n\_total is always included in grid, even when unique(n\_total) == 1~~ (done: single-value columns excluded from grid display)
* inclusion of brms priors as varying parameters (deferred: requires architectural changes — priors baked into pre-compiled Stan model at design time, varying them needs recompilation per condition)

  * parallelization of model compilation
  * compiled models in list are beeing expanded into conditions\*n\_sims list
  
  
# Power Analysis
* ~~save full quantile profile for target parameter posterior to enable post-hoc exploration of alternative thresholds~~ (done: 9 quantile columns post\_q025-post\_q975 in results\_raw and aggregated in results\_conditions)

* ~~reports: report for convergence~~ (done: `report_convergence()` function, convergence sections in `print()`/`summary()`, `report(x, "convergence")` dispatch)

# Across Package

* ~~use cli "package" for outputs~~ (done: all active code uses cli for errors/warnings/output; remaining cat() calls are legitimate markdown rendering and internal diagnostics)




# Bayesian Optimization

* after NPE implementation

* New class: rctbp_bayesian_optimization
  * is build on rctbp_conditions
  * creates ranges from min/max of condition values
  * takes a cost function as argument
* use package 'ml33mbo' as backend
  * Put mlr3mbo, bbotk, and paradox in the Suggests field of your DESCRIPTION file.
  * Write a high-level wrapper function (e.g., find_optimal_design()).
  * Inside that function, use rlang::check_installed("mlr3mbo") (a modern, cleaner version of requireNamespace) to prompt the user to install the missing packages if they try to run it.
  
  
# Major Package Development Decisions

- Consolidate API
  - minimal viable product for first release
  - placeholders for future extensions
  - naming conventions:
    - classes
    - arguments
    - pre-defined models
    
- Strategy for rolling out first package version
  - Repo managment
  - which pre-defined models
  
  
  
  
# Sample Accrual (see [25\_sample\_accrual\_plan.md](25_sample_accrual_plan.md))

* ~~Phase 1 (MVP): enrollment time generation, calendar-time subsetting, trial duration metrics~~ (done)
* ~~Phase 2: reporting & accrual plot type~~ (done)
* ~~Phase 3: dropout / loss-to-follow-up~~ (done)
* Phase 4: survival/event-driven integration (dual routing for `accrual_rate`)

## Dropout: Known Limitations / Follow-up Work

* **BayesFlow batch processing with variable completer counts**: `prepare_data_list_as_batch_bf()` assumes all simulations at a given look have the same row count (pre-allocates fixed-width matrices at line 1263). With stochastic dropout, different simulations can have different completer counts at the same look, causing matrix dimension mismatches. Fix requires splitting batches by data size or padding. See CodeRabbit review on PR #11.
* ~~**`effective_n` for non-stopped target\_not\_met sims**~~: Fixed — now uses per-sim `n_analyzed_final` from final look instead of `n_planned`, with defensive fallback.
* **Threshold resolution before dropout-aware subsetting** (in progress) (brms backend): `resolve_threshold()` for `thr_dec_eff/fut` uses the scheduled `current_n` information fraction before `subset_analysis_data()` reduces the actual analyzed count. With dropout, this overstates the information fraction. Fix: move threshold resolution after subsetting, using actual completer count.



# Sensitivity Analysis (deferred post-v1)

`sensitivity_analysis()` — systematically vary decision thresholds to assess robustness without re-simulation. Reviewed and deferred: not essential for v1.

**Key findings from review:**
- Varying `thr_dec_eff`/`thr_dec_fut` is exact and trivial — builds on `resummarize_boundaries()`/`compare_boundaries()`
- Varying `thr_fx_eff`/`thr_fx_fut` (ROPE) requires full posterior, only approximate from stored quantiles — not reliable enough for clinical use
- Prior sensitivity requires re-simulation entirely — separate feature
- Sequential designs need output showing both power AND expected sample size, not just power
- Would need new `rctbp_sensitivity_analysis` S7 class with `plot()`/`summary()` methods


# Next Development Tasks (prioritized)

1. ~~**Integration tests**~~ (done) — 5 integration tests in `test-integration.R` exercising the full pipeline with real brms fitting: single-core, multi-core (S7 serialization), crossed conditions, group sequential + resummarize_boundaries, print/summary. See `06_testing.md`.

2. ~~**`get_code()` reproducibility**~~ (done) — `get_code()` generic with methods for all 3 pipeline classes. Stores `match.call()` in `.call` property, walks the chain to reconstruct full `build_design()` → `build_conditions()` → `power_analysis()` call. 32 tests in `test-get_code.R`. See `24_api_improvement_plan.md` §3.1.

3. ~~**BayesFlow model training**~~ (outsourced) — Moved to dedicated repo/package. See `11_bayesflow_integration_roadmap.md`.

4. ~~**Sample accrual MVP**~~ (done) — Phase 1 + Phase 2 merged. Enrollment times, calendar-time subsetting, trial duration metrics, reporting & accrual plot. See `25_sample_accrual_plan.md`.


# Upcoming Features (collected from dev docs)

## New Outcome Models (see [21_adaptive_trials_roadmap.md](21_adaptive_trials_roadmap.md), [23_api_roadmap.md](23_api_roadmap.md))

* Binary outcomes — `build_model_binary_2arms()` (Priority 1)
* Count outcomes — `build_model_count_2arms()`
* Survival outcomes — `build_model_survival_2arms()` (ties into Phase 4 accrual)

## Sequential / Adaptive Extensions (see [20_interim_analysis_plan.md](20_interim_analysis_plan.md), [23_api_roadmap.md](23_api_roadmap.md))

* New interim strategies: `interim_rar()`, `interim_ssr()`, `interim_thompson()`
* New boundaries: `boundary_hsd()`, `boundary_wang_tsiatis()`
* Per-`n_total` `analysis_at` specification (named list keyed by `n_total`)
* Interim-specific plot types (deferred)

## API Convenience (see [24_api_improvement_plan.md](24_api_improvement_plan.md))

* `quick_power()` / `quick_sample_size()` shortcut functions (§3.3)

## Infrastructure

* GPU support for BayesFlow backend (see [12_gpu_support_plan.md](12_gpu_support_plan.md)) — detection, backend selection, configuration
* Test coverage: `test-power_grid_analysis.R` is a placeholder with 7 untested areas

## Long-term (see [21_adaptive_trials_roadmap.md](21_adaptive_trials_roadmap.md))

* Response-adaptive randomization
* Dose finding
* Platform / basket / umbrella trial designs
