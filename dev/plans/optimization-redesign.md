# Plan: Optimization Module Redesign

**Created**: 2026-03-11
**Author**: Claude + Matze

## Status

| Phase | Status | Date | Notes |
|-------|--------|------|-------|
| Plan | DONE | 2026-03-11 | |
| Phase 1: File Reorg + Dead Code Removal | DONE | 2026-03-27 | Split `optimization_internal.R`, delete dead code, remove fidelity |
| Phase 2: Unified API + Single-Objective BO | DONE | 2026-03-27 | `optimize_sample_size()`, result class, feasibility score BO |
| Phase 3: Tests + Validation | DONE | 2026-03-27 | Comprehensive test suite for all optimization paths |
| Phase 4: Cleanup + Documentation | DONE | 2026-03-27 | Update dev docs, archive superseded docs |
| Ship | DONE | 2026-03-27 | |

## Summary

**Motivation**: The optimization module is the package's largest subsystem (~5,700 LOC across 6 files) but has significant architectural debt:
- `optimization_internal.R` is 3,643 lines containing two parallel optimization architectures — only one is used
- `run_optimization()` (single-objective with penalties, fidelity, early stopping) is never called by any user-facing function
- Duplicate simplex transform functions with different return structures
- `inherits(design, "rctbp_design")` S7 bug (missing namespace qualification)
- 1.9% test coverage (109 lines of tests)
- No unified entry point for sample size determination
- Progressive fidelity infrastructure adds complexity without proven benefit

**Outcome**: A clean `optimize_sample_size(objective = "single"/"pareto")` API that unifies sample size optimization. Single-objective BO uses a feasibility score function with configurable surrogate (gp_power/gp_score/rf), shape (linear/quadratic/root), and scale (log/raw). Well-organized files with clear boundaries. Dead code and progressive fidelity removed. Proper test coverage.

## Design Decisions

| Decision | Options | Chosen | Rationale |
|----------|---------|--------|-----------|
| Entry point | Separate functions vs unified | `optimize_sample_size(objective=)` | Single discoverable API; `objective` switches backend |
| `optimize_power_n()` | Keep / deprecate / remove | **Remove** | Pre-release package, no deprecation warnings needed |
| `optimize_power_effect()`, `optimize_effect_n()` | Keep / remove | **Remove** | Focus this release on sample size only; can re-add later |
| `effect_size` parameter | Dedicated param / `constant` list | **`constant = list(...)`** | Model-agnostic; user specifies `b_arm_treat`, `log_hazard_ratio`, etc. explicitly |
| Result classes | Single class / separate | Separate: `rctbp_sample_size_result` (single) + `rctbp_pareto_result` (pareto) | Different structures — document return type varies by `objective` |
| Dead code | Keep / delete | Delete `run_optimization()` and all unused infrastructure | Never called; single-objective rebuilt from scratch |
| Progressive fidelity | Keep / remove | **Remove from both paths** | Unproven complexity; document for future re-addition |
| `find_surrogate_optimum()` / `find_probabilistic_optimum()` | Keep / defer / drop | Keep — Pareto-only, move to dedicated file | Useful post-hoc analysis for Pareto results; single-objective equivalent deferred |
| Dependencies (mlr3mbo) | Imports / Suggests | Keep as Suggests | Optimization is optional functionality |
| Surrogate strategy | Single / configurable | **Configurable**: `gp_power`, `gp_score`, `rf` | Benchmark to find best; prune later |
| Score function | Fixed / configurable | **Configurable**: shape × scale | Need real-world experience to know best combination |
| Pruning strategy | Pick best now / keep all | Keep all methods, prune after benchmarking | Need vignette with systematic comparison |

## Scope

### In Scope
- Unified `optimize_sample_size(objective = "single"/"pareto")` API
- New `rctbp_sample_size_result` S7 class for single-objective results
- Single-objective BO with feasibility score function (designed from scratch)
- Three surrogate strategies: `gp_power`, `gp_score`, `rf`
- Configurable score: shape (linear/quadratic/root) × scale (log/raw)
- File reorganization: split `optimization_internal.R` into focused modules
- Delete dead code (`run_optimization()`, unused penalty/fidelity/early-stopping infrastructure)
- Remove progressive fidelity from both single-objective and Pareto paths
- Fix S7 `inherits()` bug
- Consolidate duplicate simplex transforms
- Remove `optimize_power_n()`, `optimize_power_effect()`, `optimize_effect_n()`
- Test coverage for all optimization code paths
- `plot()` for single-objective: dual panel (surrogate curve + convergence), with `type` options

### Out of Scope
- New optimization algorithms (e.g., constrained EIC from dev/31 — future work)
- BayesFlow-specific optimization paths (blocked on BF retraining)
- Effect size optimization (`optimize_power_effect`, `optimize_effect_n` — future)
- Progressive fidelity (removed; documented for future re-addition)
- CRAN preparation for optimization module

### Future Work (documented but not implemented)
- Benchmarking vignette: systematic comparison of surrogate × shape × scale combinations
- Progressive fidelity: `n_sims = c(500, 2000)` for multi-fidelity optimization
- Effect size optimization: re-add `optimize_power_effect()` etc. when needed
- Constrained EIC acquisition function (from dev/31)
- Method pruning after benchmark results

## Single-Objective BO Design

### Problem

Find minimum `n_total` such that `power(n_total) >= target_power`.

**Characteristics:**
- Input: `n_total` (integer, bounded by `n_range`)
- Output: power (continuous, [0, 1])
- Relationship: monotonically increasing (more n → more power)
- Evaluation: expensive (simulation-based)

### Feasibility Score Function

The objective function is a **feasibility score** that the BO maximizes:

```
score(n) = 0                              if power(n) < target_power
score(n) = shape(normalized_distance(n))  if power(n) >= target_power
```

Where:
- `normalized_distance(n)` measures how close n is to n_min vs n_max
- Score is **monotonically decreasing** from n_min to n_max for feasible points
- Score = 1 at n_min (the search range lower bound)
- Score → 0 as n → n_max
- Score = 0 when power < target (infeasible, step discontinuity)

The BO pushes toward the smallest feasible n — the leftmost point where the score jumps from 0 to a positive value. This is the correct objective: find the feasibility boundary and return the smallest n above it.

### Scale Options

The `score_scale` parameter controls how n is normalized:

**`"log"` (default):**
```
normalized_distance(n) = (log(n_max) - log(n)) / (log(n_max) - log(n_min))
```
Proportional cost — doubling n is equally expensive at any scale. Better for wide ranges.

**`"raw"`:**
```
normalized_distance(n) = (n_max - n) / (n_max - n_min)
```
Absolute cost — each additional participant costs the same. Better for narrow ranges or budget-constrained designs.

### Shape Options

The `score_shape` parameter controls the decay curve from n_min to n_max:

| Shape | Formula | Behavior |
|-------|---------|----------|
| `"linear"` | `x` | Uniform preference across feasible range |
| `"quadratic"` | `x^2` | Stronger preference for small n, sharper peak |
| `"root"` | `sqrt(x)` | Gentler preference, more tolerant of overshoot |

Where `x = normalized_distance(n)`.

### Surrogate Strategies

Three options for the BO surrogate model:

| Surrogate | Fits to | Handles step? | Sample efficiency |
|-----------|---------|---------------|-------------------|
| `"gp_power"` (default) | Raw power (smooth) | Score computed analytically from GP prediction | High — GP sees smooth function |
| `"gp_score"` | Peaked score directly | GP smears discontinuity | Medium — struggles near threshold |
| `"rf"` | Peaked score directly | Trees handle discontinuity natively | Lower — but robust |

**`gp_power`** (custom loop, not bbotk EGO): Fit GP to **logit(power)** values (unbounded, smooth, monotonic — ideal for GP; avoids boundary artifacts from raw [0,1] scale). Does NOT use bbotk's EGO loop. Instead, runs a custom optimization loop:
1. Evaluate initial design, store (n, power) pairs; transform: `logit_power = logit(power)`
2. Fit GP to logit(power) values on log(n) scale
3. Query GP posterior (mu, sigma on logit scale) at dense grid of n values; compute `P_feas(n) = P(power >= target) = Phi((mu_logit - logit(target)) / sigma_logit)` using `compute_p_feas()`
4. **Acquisition rule**: select the minimum n where `P_feas(n) >= alpha` (alpha = 0.95, fixed). If no n meets this threshold, select the n with the highest P_feas.
5. Evaluate `power_analysis()` at selected n; add to GP; repeat
6. After loop, extract smallest n where **observed** power >= target (use archive, not GP prediction)

This exploits the smooth monotonic structure that GP handles well on the logit scale. The feasibility score is computed post-hoc from archived power values for result display only. `power_optimal` stores the observed (simulation-based) power at `n_optimal`, not the GP prediction. More complex to implement but theoretically the strongest approach.

**`gp_score`** (standard bbotk EGO): Fit GP directly to the feasibility score values via bbotk's `bayesopt_ego`. Simplest pipeline — standard EI maximization. But GP will smooth over the step discontinuity at the power threshold.

**`rf`** (standard bbotk EGO): Random forest (via `ranger`) fitted to feasibility score values via bbotk's `bayesopt_ego`. Same pipeline as `gp_score` but trees handle the discontinuity natively. Less sample-efficient than GP.

### Optimization Loop

1. **Initial design**: `n_init = 5` points via Latin Hypercube Sampling on log(n) scale
2. **Evaluate**: Run `power_analysis()` at each initial point, compute feasibility score
3. **BO loop** (up to `max_evals = 30`):
   a. Fit surrogate to evaluated points
   b. Optimize acquisition function (EI) to select next n
   c. Evaluate `power_analysis()` at selected n
   d. Compute feasibility score
   e. Check patience: stop if best score hasn't improved for `patience = 5` consecutive evals (patience counting starts after initial design is evaluated, not during it)
4. **Extract result**: Among all feasible points (power >= target), return the one with smallest n
5. **No feasible solution**: Warn, return the point with highest power as `n_optimal` with `feasible = FALSE`

### Result Class: `rctbp_sample_size_result`

| Property | Type | Description |
|----------|------|-------------|
| `design` | `S7::class_any` | Reference design (use `class_any` for R6 surrogate compatibility) |
| `n_optimal` | `S7::class_numeric` | Recommended sample size (stored as integer-valued numeric; rounded via `as.integer()` during result extraction, validated: `n_optimal == round(n_optimal)`) |
| `power_optimal` | `S7::class_numeric` | Estimated power at n_optimal |
| `target_power` | `S7::class_numeric` | User's target |
| `feasible` | `S7::class_logical` | Whether target was met |
| `convergence` | `S7::class_data.frame` | Columns: `eval`, `n_total`, `power`, `score`, `best_score` |
| `archive` | `S7::class_data.frame` | All evaluated points: `n_total`, `power`, `score`, `n_sims` |
| `surrogate_fit` | `S7::class_any` | Fitted surrogate model (R6, not serializable); `class_any` accepts NULL, no union needed; default NULL |
| `surrogate_type` | `S7::class_character` | `"gp_power"`, `"gp_score"`, or `"rf"` |
| `score_config` | `S7::class_list` | `list(scale = "log", shape = "linear")` — metadata only, does not recompute archived scores if modified |
| `n_sims` | `S7::class_numeric` | Sims per evaluation |
| `n_evals` | `S7::class_numeric` | Total evaluations |
| `elapsed_time` | `S7::class_numeric` | Runtime in minutes |

**Validator**: Use namespace-qualified `inherits(self@design, "rctbayespower::rctbp_design")`.

**`print()`**: Summary showing n_optimal, power_optimal, target, feasible status, n_evals, elapsed_time.

**`summary()`**: Returns archive data.frame.

### Unified API Signature

```r
optimize_sample_size(
  design,
  objective = c("single", "pareto"),
  n_range = c(50, 500),
  constant = list(),        # Fixed params, e.g., list(b_arm_treat = 0.3)
  n_sims = 1000,
  n_cores = 1,
  # --- Single-objective args ---
  target_power = 0.80,
  surrogate = c("gp_power", "gp_score", "rf"),
  score_shape = c("linear", "quadratic", "root"),
  score_scale = c("log", "raw"),
  n_init = 5,
  max_evals = 30,
  patience = 5,
  # --- Pareto args ---
  power_metric = "pwr_eff",
  knee_method = c("utopia", "min_cost", "linear"),
  # --- Common ---
  seed = NULL,
  verbose = TRUE
)
```

**Return type** (documented in roxygen `@return`):
- `objective = "single"` → `rctbp_sample_size_result`
- `objective = "pareto"` → `rctbp_pareto_result`

Pareto-specific args are ignored when `objective = "single"` and vice versa.

## Implementation Plan

### Phase 1: File Reorg + Dead Code Removal

**Goal**: Split `optimization_internal.R` (3,643 lines) into focused modules. Delete dead code and progressive fidelity. Consolidate duplicates. Fix S7 bug. Remove old wrapper functions.

**Pre-step**: Run `grep -rn` on every candidate "dead code" function to confirm zero live callers before deleting. The reviewer flagged `build_fidelity_schedule()` as potentially called by `pareto_optimize()` — verify.

**Files to create:**
- `R/optimization_transforms.R` — ILR forward/inverse, `constrained_simplex()`, `apply_simplex_transforms()`, `apply_simplex_transforms_flat()`, logit/log transforms
- `R/optimization_mbo.R` — mlr3mbo/bbotk setup: `create_parameter_space()`, `create_codomain()`, surrogate config, acquisition function config, logging control
- `R/optimization_objective.R` — objective function creation, archive management, `add_simplex_to_archive()`
- `R/optimization_postprocessing.R` — `find_surrogate_optimum()`, `find_probabilistic_optimum()` (Pareto-only; accept `rctbp_pareto_result`, not the new single-objective class)

**Files to delete:**
- `R/optimization_internal.R` — after extracting live code
- `R/pareto_wrappers.R` — `optimize_power_n()`, `optimize_power_effect()`, `optimize_effect_n()` all removed
- `R/acq_function_eic.R` — custom EIC acquisition function from dev/31, dead code (exported but never called)

**Files to modify:**
- `R/pareto_optimize.R` — remove `apply_simplex_transforms_flat()` (moved), fix `inherits()` S7 bug (line 99), remove progressive fidelity support, update internal references
- `R/optimization.R` → rename to `R/optimization_search.R` via `git mv`
- `R/class_pareto_result.R` — ensure consistent namespace-qualified `inherits()` pattern; update `optimization_type` validator to only allow `"pareto"` and `"custom"` (remove `"power_n"`, `"power_effect"`, `"effect_n"` since wrappers are deleted); update `type_labels` in `print()` method to include `"pareto"` label; `optimize_sample_size(objective = "pareto")` sets `optimization_type = "pareto"`; fix `mbo_objects` property from `class_list | NULL` to `class_any` for consistency with new class design
- `R/rctbayespower-package.R` — remove globalVariables for deleted functions if any

**Dead code to delete** (from `optimization_internal.R` — verify each with grep first):
- `run_optimization()` — full single-objective orchestration, never called
- `cost_hard_constraint()`, `cost_soft_penalty()`, `cost_risk_ratio()`, `cost_logit_distance()` — penalty functions
- `build_fidelity_schedule()` — fidelity infrastructure (verify: may be called by `pareto_optimize()`)
- `check_surrogate_stopping()` — early stopping
- `compute_fidelity_weights()` — fidelity weighting
- `standardize_log_n()`, `unstandardize_log_n()` — normalization
- `infer_search_type()`, `apply_search_transforms()`, `derive_search_types()` — verify callers; may be dead
- Any other functions only reachable from `run_optimization()`

**CAUTION**: `logit_transform()`, `invlogit_transform()`, and `compute_p_feas()` are interleaved with the dead cost functions in the file (lines ~113-154) but are **live code** — used by `find_probabilistic_optimum()` and needed by the new `gp_power` loop. These must be EXTRACTED to `optimization_transforms.R`, NOT deleted.

**Live code to extract** (from `optimization_internal.R`):
- ILR transforms: `ilr_inverse()`, `ilr_forward()`, `constrained_simplex()` → `optimization_transforms.R`
- Simplex transforms: `apply_simplex_transforms()` → `optimization_transforms.R`
- Parameter transforms: `get_param_transform()`, `get_search_transform()` → `optimization_transforms.R` (if live callers exist)
- `logit_transform()`, `invlogit_transform()`, `compute_p_feas()` → `optimization_transforms.R`
- MBO setup: `create_parameter_space()`, `create_codomain()` → `optimization_mbo.R`
- Objective: `create_objective_fn()` → `optimization_objective.R`
- Archive: `add_simplex_to_archive()` → `optimization_objective.R`
- Post-hoc: `find_surrogate_optimum()`, `find_probabilistic_optimum()` → `optimization_postprocessing.R`

**Steps:**
1. Run `grep -rn` on every function in `optimization_internal.R` to map callers → extract vs delete
2. Run `devtools::document()` to ensure clean starting state
3. Create the 4 new files with proper file headers (per code-patterns)
4. Move live functions, preserving roxygen docs
5. Consolidate `apply_simplex_transforms_flat()` (from `pareto_optimize.R`) into `optimization_transforms.R`
6. Fix `inherits(design, "rctbp_design")` → `inherits(design, "rctbayespower::rctbp_design")` everywhere
7. Remove progressive fidelity from `pareto_optimize()` (scalar `n_sims` only)
8. Delete `R/pareto_wrappers.R` (all three wrapper functions removed)
9. Delete `R/optimization_internal.R`
10. Rename `R/optimization.R` → `R/optimization_search.R` via `git mv`
11. Run `devtools::document()` to regenerate NAMESPACE
12. Run `devtools::check()` — must pass with no new errors/warnings

**Tests:**
- Existing `test-pareto_optimize.R` tests must still pass (they test transform functions)
- Add regression test: call `pareto_optimize()` with a mock design to verify `inherits()` fix works end-to-end
- Verify moved `apply_simplex_transforms_flat()` produces identical output

---

### Phase 2: Unified API + Single-Objective BO

**Goal**: Create `optimize_sample_size()` with both Pareto and single-objective modes. Implement feasibility score BO from scratch. Both modes fully functional.

**Files to create:**
- `R/optimize_sample_size.R` — unified entry point function
- `R/class_sample_size_result.R` — `rctbp_sample_size_result` S7 class + print/summary
- `R/optimization_single.R` — single-objective BO: feasibility score, surrogate setup, optimization loop, result extraction

**Files to modify:**
- `R/optimization_transforms.R` — add feasibility score computation functions
- `R/rctbayespower-package.R` — add globalVariables if needed

**Steps:**
1. Define `rctbp_sample_size_result` S7 class (properties as specified in design section above)
   - Validator: namespace-qualified `inherits()` for design
   - `surrogate_fit` uses `S7::class_any` (R6 objects, not serializable)
   - `n_optimal` uses `S7::class_numeric` (cast to integer during extraction)
2. Add `print()` and `summary()` S7 methods
3. Implement feasibility score functions in `optimization_transforms.R`:
   - `compute_peaked_score(power, n, n_range, target_power, scale, shape)` → numeric score
   - `compute_normalized_distance(n, n_range, scale)` → numeric in [0, 1]
4. Implement `R/optimization_single.R`:
   - `create_single_objective_fn(design, target_power, n_range, score_scale, score_shape, surrogate_type, constant, n_sims, n_cores)` — wraps `build_conditions()` + `power_analysis()`, returns feasibility score
   - `generate_single_bo_initial(n_range, n_init)` — LHS on log(n), 5 points (named to avoid collision with `generate_initial_design()` in `optimization_mbo.R`)
   - `run_single_bo(objective_fn, n_range, surrogate_type, n_init, max_evals, patience, seed, verbose)` — sets up bbotk, runs EGO loop with patience-based early stopping (patience counting starts after initial design, i.e., after eval `n_init`), extracts result
   - `extract_single_result(archive, design, target_power, ...)` — finds best feasible point or warns and returns best
5. Create `optimize_sample_size()` in `R/optimize_sample_size.R`:
   - Input validation: `n_range[1] < n_range[2]` (degenerate range → NaN in score), valid `objective`, valid `constant` list, etc.
   - `rlang::check_installed()` for mlr3mbo/bbotk/paradox (for `gp_score`/`rf`); `rlang::check_installed("DiceKriging")` for `gp_power` (custom GP loop uses `DiceKriging::km()` + `predict()`)
   - Dispatch: `objective = "pareto"` → `pareto_optimize()`, `objective = "single"` → `run_single_bo()`
   - Pareto mode constructs `objectives = list(pwr_eff = "maximize", n_total = "minimize")` (using `power_metric`) and `search = list(n_total = n_range)` from the unified API params, then calls `pareto_optimize(design, objectives, search, constant, ...)`. This is the same construction pattern as the deleted `optimize_power_n()` but without `effect_size` (user passes effect params via `constant`).
   - Note: `optimize_sample_size(objective = "pareto")` only searches over `n_total`. Users who want multi-dimensional Pareto search (e.g., n_total + p_alloc) must call `pareto_optimize()` directly. Document this in roxygen `@details`.
   - Document return type varies by `objective` in roxygen `@return`
6. Run `devtools::document()` and `devtools::check()`

**Tests:**
- `test-optimize_sample_size.R`: input validation, both dispatch paths
- `test-optimization_single.R`:
  - `compute_peaked_score()`: each shape × scale combination, boundary values
  - Score = 0 when power < target
  - Score = 1 at n_min when feasible
  - Score → 0 at n_max
  - `generate_initial_design()`: correct count, within bounds, log-spaced
  - Integration: small BO run (n_sims=50, max_evals=5) with mock design
  - Edge: no feasible solution → warning + `feasible = FALSE`
  - Edge: target met at n_min → returns n_min immediately
  - Edge: `n_range[1] == n_range[2]` (degenerate range) → normalized_distance produces 0/0 = NaN; must validate and error early

---

### Phase 3: Tests + Validation

**Goal**: Comprehensive test coverage for all optimization code paths.

**Files to create:**
- `tests/testthat/test-optimization_transforms.R` — transform function tests
- `tests/testthat/test-optimization_postprocessing.R` — surrogate/probabilistic optimum tests

**Files to modify:**
- `tests/testthat/test-pareto_optimize.R` — expand existing tests, add Pareto-specific edge cases

**Test categories:**

1. **Transform tests** (`test-optimization_transforms.R`):
   - ILR forward/inverse roundtrip for 2, 3, k-arm
   - Constrained simplex with various min_prop values
   - `apply_simplex_transforms()` structured output
   - `apply_simplex_transforms_flat()` flat output
   - `get_param_transform()` auto-detection (n_* → log, thr_dec_* → logit, etc.)
   - Peaked score: each shape × scale, threshold boundary, edge values
   - Edge: single-element simplex, boundary values

2. **Pareto tests** (expand `test-pareto_optimize.R`):
   - Knee point selection: all 3 methods (utopia, min_cost, linear)
   - Pareto front extraction from mock archive
   - `create_pareto_codomain()` with different objective specs
   - Edge: single-point Pareto front, dominated-only archive

3. **Post-processing tests** (`test-optimization_postprocessing.R`):
   - `find_surrogate_optimum()` with mock result
   - `find_probabilistic_optimum()` with mock result
   - Edge: empty archive, no feasible points

**Note**: Integration tests that run mlr3mbo are expensive. Use `testthat::skip_if_not_installed("mlr3mbo")` and keep n_sims/evals very small.

---

### Phase 4: Cleanup + Documentation

**Goal**: Plotting for single-objective results, update dev docs, archive superseded docs.

**Files to modify:**
- `R/plot_optimization.R` — add `plot.rctbp_sample_size_result()`:
  - Default: dual panel (surrogate prediction curve + convergence trace)
  - `type = "surrogate"`: predicted power vs n + evaluated points + target line. Y-axis is always power (for `gp_score`/`rf`, invert score→power if possible, otherwise show score). GP surrogates show confidence band; RF shows no band (no native uncertainty).
  - `type = "convergence"`: best score over evaluations
  - `type = "all"`: both panels
- `dev/30_bayesian_optimization_plan.md` — update to reflect new architecture
- `dev/31_BO_implementation_guide.md` — archive: add header noting it's superseded by this plan
- `dev/99_dev_todos.md` — mark optimization redesign as DONE

**Steps:**
1. Add `plot.rctbp_sample_size_result()` with type dispatch
2. Update `dev/30_bayesian_optimization_plan.md` architecture section and file map
3. Archive `dev/31_BO_implementation_guide.md` with superseded header
4. Final `devtools::check()` — 0 errors, 0 warnings, 0 new notes
5. Update `dev/99_dev_todos.md`

**Tests:**
- Plot method: returns ggplot object, correct type dispatch, handles all surrogate types

## Verification & Validation

- **Automated**: All tests pass, R CMD check clean, no regressions in existing test suite (~960 tests)
- **Manual smoke tests**:
  - `optimize_sample_size(design, objective = "pareto", n_range = c(50, 300), constant = list(b_arm_treat = 0.3))` produces Pareto front with reasonable power-vs-n trade-off
  - `optimize_sample_size(design, objective = "single", n_range = c(50, 300), constant = list(b_arm_treat = 0.3), target_power = 0.80)` finds n close to analytic solution for simple designs
  - `plot()` on both result types produces sensible visualizations
  - `find_surrogate_optimum()` and `find_probabilistic_optimum()` work on Pareto results
- **Numerical validation**:
  - For a two-sample t-test equivalent design, compare `optimize_sample_size(objective = "single", target_power = 0.80)` result against `pwr::pwr.t.test(power = 0.80, d = effect_size)$n` — should be within ±10% given simulation noise
  - Pareto front should be monotonically non-decreasing (more n → more power)
  - Knee point should be on the Pareto front
  - Feasibility score is 0 for all infeasible points, = 1 at n_min when feasible, monotonically decays to 0 at n_max
- **Regression**: transform tests from `test-pareto_optimize.R` must pass unchanged after file reorganization

## Dependencies

- `mlr3mbo (>= 0.2.0)`, `bbotk (>= 0.7.0)`, `paradox (>= 0.11.0)` — remain in Suggests
- `DiceKriging` — GP surrogate, Suggests
- `ranger` — RF surrogate, Suggests
- `lgr` — logging control, Suggests
- No new hard dependencies
- No `lifecycle` dependency needed (pre-release: remove, don't deprecate)

## File Map (Before → After)

| Before | After | Action |
|--------|-------|--------|
| `R/optimization_internal.R` (3,643 LOC) | **DELETED** | Split into 4 files, dead code removed |
| `R/pareto_wrappers.R` (353 LOC) | **DELETED** | All wrappers removed (pre-release) |
| — | `R/optimization_transforms.R` (~350 LOC) | NEW: ILR, simplex, param transforms, feasibility score |
| — | `R/optimization_mbo.R` (~300 LOC) | NEW: mlr3mbo/bbotk setup |
| — | `R/optimization_objective.R` (~200 LOC) | NEW: objective fn creation, archive mgmt |
| — | `R/optimization_postprocessing.R` (~200 LOC) | NEW: surrogate/probabilistic optimum |
| — | `R/optimize_sample_size.R` (~250 LOC) | NEW: unified entry point |
| — | `R/class_sample_size_result.R` (~150 LOC) | NEW: single-objective result class |
| — | `R/optimization_single.R` (~350 LOC) | NEW: feasibility score BO logic |
| `R/pareto_optimize.R` (890 LOC) | `R/pareto_optimize.R` (~500 LOC) | SLIMMED: remove duplicates, fix S7 bug, remove fidelity |
| `R/optimization.R` (160 LOC) | `R/optimization_search.R` (160 LOC) | RENAMED via `git mv` |
| `R/class_pareto_result.R` (176 LOC) | `R/class_pareto_result.R` (176 LOC) | MINOR: consistent `inherits()` |
| `R/plot_optimization.R` (347 LOC) | `R/plot_optimization.R` (~500 LOC) | ADD: single-objective plot method |

**Estimated net change**: ~5,700 LOC → ~3,150 LOC (remove ~3,300 LOC of dead code + wrappers + fidelity, add ~800 LOC of new code)

## Future Work

Documented here for re-addition after benchmarking:

### Progressive Fidelity
Multi-fidelity optimization using `n_sims = c(500, 2000)`. Early evaluations use cheap simulations, later evaluations use precise ones. Was partially implemented in `pareto_optimize()` via `build_fidelity_schedule()`. Removed in this redesign to reduce complexity. To re-add:
- Restore `build_fidelity_schedule()` from git history (commit before this redesign)
- Modify BO loop to accept vector `n_sims` and switch fidelity at specified eval counts
- Apply to both single-objective and Pareto paths

### Effect Size Optimization
Re-add `optimize_power_effect()` and `optimize_effect_n()` as separate functions (not through `optimize_sample_size()`). These optimize over effect size, not sample size.

### Constrained EIC Acquisition
Expected Improvement with Constraints — uses GP posterior for probabilistic feasibility. Described in `dev/31_BO_implementation_guide.md`. More principled than feasibility score for constrained problems but more complex.

### Benchmarking Vignette
Systematic comparison of surrogate × shape × scale combinations for single-objective BO. Essential for informed method pruning. Should cover:
- Multiple design types (continuous ANCOVA, binary, sequential)
- Various true n_opt locations within n_range
- Convergence speed and accuracy metrics
- Computational cost comparison

## Notes

_Living section — updated during implementation._

- `dev/31_BO_implementation_guide.md` describes a superseded `bo_sample_size()` API. Archive after this redesign.
- `R/acq_function_eic.R` — confirmed to exist, contains custom EIC from dev/31. Added to Phase 1 delete list.
- When implementing Phase 1, run `git mv` for the rename and `devtools::document()` immediately after to keep NAMESPACE clean.

## Review Feedback

### Plan Reviewer Findings (addressed)

| # | Severity | Issue | Resolution |
|---|----------|-------|------------|
| 1 | CRITICAL | `lifecycle` not in DESCRIPTION | Resolved: no deprecations pre-release, just remove |
| 2 | CRITICAL | Pareto param mismatch with `...` | Resolved: explicit params in unified API signature |
| 3 | CRITICAL | `build_fidelity_schedule()` may not be dead | Resolved: verify with grep; removing fidelity entirely |
| 4 | CRITICAL | New class validator must use namespace `inherits()` | Addressed: explicit in class design section |
| 5 | IMPORTANT | `effect_size` hardwired to `b_arm_treat` | Resolved: removed; use `constant = list(...)` |
| 6 | IMPORTANT | `infer_search_type()` etc. may be dead | Addressed: pre-step grep audit before deleting |
| 7 | IMPORTANT | Phase 1 ships broken `objective = "single"` | Resolved: merged Phases 1+3, both modes ship together |
| 8 | IMPORTANT | `surrogate_fit` should be `class_any` not `class_list` | Addressed: `S7::class_any` in class design |
| 9 | IMPORTANT | Convergence schema differs single/pareto | Addressed: columns defined in result class section |
| 10 | IMPORTANT | Rename with `git mv`, document first | Addressed: explicit in Phase 1 steps |
| 11 | MINOR | `n_optimal` should be `class_numeric` | Addressed: `S7::class_numeric` with `as.integer()` in extraction |
| 12 | MINOR | Return type differs by `objective` | Addressed: documented in API signature section |
| 13 | MINOR | Phase 2 "no new tests" wrong for inherits fix | Addressed: regression test added to Phase 1 |
| 14 | MINOR | Progressive fidelity reuse unclear | Resolved: removed entirely from both paths |

### Second Review Findings (addressed)

| # | Severity | Issue | Resolution |
|---|----------|-------|------------|
| 1 | CRITICAL | `find_surrogate_optimum()`/`find_probabilistic_optimum()` incompatible with new result class | Resolved: scoped to Pareto-only; single-objective equivalent deferred |
| 2 | CRITICAL | "Peaked score" name misleading — score is monotonically decreasing, not interior peak | Resolved: renamed to "feasibility score", clarified monotonic behavior |
| 3 | IMPORTANT | `rctbp_pareto_result` validator dual-form `inherits()` needs cleanup | Addressed: Phase 1 explicitly fixes to namespace-qualified only |
| 4 | IMPORTANT | `acq_function_eic.R` exists but not in delete list | Resolved: added to Phase 1 delete list |
| 5 | IMPORTANT | `generate_initial_design()` name collision with existing function | Resolved: renamed to `generate_single_bo_initial()` |
| 6 | IMPORTANT | `gp_power` EI on composed function under-specified | Resolved: detailed implementation approach in surrogate strategy section |
| 7 | IMPORTANT | Patience could trigger during initial design | Resolved: patience counting starts after initial design |
| 8 | IMPORTANT | `optimization_type` validator allows deleted wrapper types | Resolved: update validator to only allow `"pareto"`, `"custom"` |
| 9 | MINOR | `score_config` mutable but archived scores don't update | Addressed: documented as metadata-only |
| 10 | MINOR | Plot spec ambiguous across surrogate types | Resolved: per-surrogate-type behavior specified |
| 11 | MINOR | `n_optimal` integer casting unclear | Resolved: `as.integer()` in extraction, validated `== round()` |

### Third Review Findings (addressed)

| # | Severity | Issue | Resolution |
|---|----------|-------|------------|
| 1 | IMPORTANT | `gp_power` "transform-then-acquire" conflicts with bbotk internals | Resolved: `gp_power` uses custom loop (not bbotk EGO), fits GP to raw power, finds crossing point directly |
| 2 | IMPORTANT | `optimization_type` validator + print labels for `"pareto"` value | Resolved: explicit in Phase 1 file modification notes |
| 3 | MINOR | Degenerate `n_range` validation only in tests, not implementation | Resolved: added to Phase 2 Step 5 input validation |
| 4 | MINOR | `evals_per_step` meaningless after fidelity removal | Resolved: removed from unified API signature |

### Fourth Review Findings (addressed)

| # | Severity | Issue | Resolution |
|---|----------|-------|------------|
| 1 | IMPORTANT | GP should fit logit(power), not raw power — boundary artifacts and P_feas math requires logit scale | Resolved: corrected to logit(power), added logit transform details |
| 2 | IMPORTANT | `gp_power` acquisition rule ambiguous — "P_feas maximized at smallest n" underspecified | Resolved: precise rule: minimum n where P_feas >= 0.95; fallback to highest P_feas |
| 3 | MINOR | `S7::class_any \| NULL` redundant — `class_any` already accepts NULL | Resolved: changed to `S7::class_any` throughout |

### Fifth Review Findings (addressed) — APPROVED

| # | Severity | Issue | Resolution |
|---|----------|-------|------------|
| 1 | IMPORTANT | `gp_power` GP fitting package not named; no `check_installed` guard | Resolved: `DiceKriging::km()` named explicitly, `check_installed("DiceKriging")` added |
| 2 | IMPORTANT | `mbo_objects` in `rctbp_pareto_result` still uses `class_list \| NULL` anti-pattern | Resolved: added to Phase 1 modification list — fix to `class_any` |
| 3 | IMPORTANT | Pareto dispatch construction not specified; `search` not exposed | Resolved: construction pattern documented; complex searches use `pareto_optimize()` directly |
| 4 | IMPORTANT | `logit_transform`/`invlogit_transform`/`compute_p_feas` interleaved with dead code, risk of accidental deletion | Resolved: explicit CAUTION note in dead code section |
