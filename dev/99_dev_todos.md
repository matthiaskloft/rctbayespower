## class 'model'

* dots for brms arguments like link functions for family
  -> implemented link\_sigma as argument for ancova
* ~~print() should show default values for the data\_simulation\_fn as well as required arguments to specify~~ (done: "Simulation Function Parameters" section in print output)

* ~~allocation probs need to be disambiguated~~ (done: p_alloc documented in show_condition_args, validation error improved with format guidance)


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
* ~~Phase 4: survival/event-driven integration~~ (done: dual routing, survival sim_fn, event-driven subsetting, n_events metric)

## Dropout: Known Limitations / Follow-up Work

* ~~**BayesFlow batch processing with variable completer counts**~~: Fixed — split-by-size batching groups sims by completer count, runs separate forward passes per group, reassembles in original order. No Python changes needed. Assertion added to `prepare_data_list_as_batch_bf()` to catch mixed row counts.
* ~~**`effective_n` for non-stopped target\_not\_met sims**~~: Fixed — now uses per-sim `n_analyzed_final` from final look instead of `n_planned`, with defensive fallback.
* ~~**Threshold resolution before dropout-aware subsetting** (brms backend)~~: Fixed — brms backend now computes `info_frac = nrow(analysis_data) / n_total` after `subset_analysis_data()`. BF backend keeps scheduled info_frac (deterministic, shared across batch). Zero-completer guard added. Also fixed `resolve_boundary_vector()` to pass full info_frac vector (was calling per-element, breaking gsDesign).

## Out-of-scope issues discovered during threshold fix

* **Bayesian OBF boundary single-scalar behavior**: `boundary_obf(threshold=)` self-normalizes by the last element of the info_frac vector. When called with a single scalar (as in `resolve_threshold()` per-look), it always returns `threshold` regardless of info_frac. The OBF shape only manifests with vector context. This is a design limitation of the Bayesian mode boundary functions — they need the full look schedule to compute the shape. Consider pre-resolving all thresholds as a vector at the start of the sequential loop rather than per-look.



# Next Development Tasks (prioritized)

1. ~~**Integration tests**~~ (done) — 5 integration tests in `test-integration.R` exercising the full pipeline with real brms fitting: single-core, multi-core (S7 serialization), crossed conditions, group sequential + resummarize_boundaries, print/summary. See `06_testing.md`.

2. ~~**`get_code()` reproducibility**~~ (done) — `get_code()` generic with methods for all 3 pipeline classes. Stores `match.call()` in `.call` property, walks the chain to reconstruct full `build_design()` → `build_conditions()` → `power_analysis()` call. 32 tests in `test-get_code.R`. See `24_api_improvement_plan.md` §3.1.

3. ~~**BayesFlow model training**~~ (outsourced) — Moved to dedicated repo/package. See `11_bayesflow_integration_roadmap.md`.

4. ~~**Sample accrual MVP**~~ (done) — Phase 1 + Phase 2 merged. Enrollment times, calendar-time subsetting, trial duration metrics, reporting & accrual plot. See `25_sample_accrual_plan.md`.
