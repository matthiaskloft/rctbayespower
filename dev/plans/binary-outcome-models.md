# Plan: Binary Outcome Models

**Created**: 2026-03-10

## Status

| Phase | Status | Date | Notes |
|-------|--------|------|-------|
| Plan | DONE | 2026-03-10 | |
| Phase 1: Rename continuous | DONE | 2026-03-11 | `build_model_ancova()` -> `build_model_ancova_cont()` |
| Phase 2: Binary builder | DONE | 2026-03-11 | `build_model_ancova_bin()` + sim fn |
| Phase 3: Registry + batch sim | DONE | 2026-03-11 | Register `ancova_bin_2arms`, add batch fn |
| Phase 4: Tests | DONE | 2026-03-11 | 20 tests, all passing |
| Phase 5: Docs + TODO cleanup | DONE | 2026-03-11 | Dev TODOs updated |
| Ship | MERGED | 2026-03-11 | PR #20 https://github.com/matthiaskloft/rctbayespower/pull/20 |

## Summary

**Motivation**: Binary outcomes (response rates, remission, mortality) are the most common endpoint type in Phase II/III trials. Currently the package only supports continuous ANCOVA models. This blocks multiple real-world use cases (oncology ORR, vaccine efficacy, etc.).

**Outcome**: Users can run `build_design(predefined_model = "ancova_bin_2arms")` and get full power analysis for binary endpoints -- including fixed designs, group sequential with stopping rules, and accrual/dropout support. All existing infrastructure (compute_measures, backends, sequential analysis) works unchanged because the pipeline operates on the log-odds scale, which is parameter-agnostic.

## Design Decisions

| Decision | Options | Chosen | Rationale |
|----------|---------|--------|-----------|
| Architecture | (A) `family` param on shared function, (B) Separate `_cont` / `_bin` functions | **B: Separate functions** | No conditional branching; each function self-contained |
| Naming | `build_model_ancova_cont()` / `build_model_ancova_bin()` | -- | Symmetric pair; `_cont` parallels `_bin`; `build_model_ancova()` removed entirely |
| Sim fn | Each builder creates its own closure | -- | Binary has no `sigma` in formals; `get_args_without_defaults()` works correctly |
| Link function | (A) logit only, (B) configurable | **A: logit only** | Standard for clinical trials |
| Default priors (binary intercept) | (A) `normal(0, 10)`, (B) `normal(0, 5)` | **B: `normal(0, 5)`** | Log-odds scale: covers response rates ~0.7% to ~99.3% |
| File location | Extend `R/models_ancova.R` | -- | Same model family |
| Registry name | `ancova_bin_2arms` | -- | Follows `{model}_{outcome}_{arms}` pattern |
| `b_arm_treat` default | `NULL` (required in wrapper) | -- | User must specify effect size; inner sim fn uses `rep(0, n_arms-1)` matching continuous pattern |
| `model_name` display | `"Continuous ANCOVA"` / `"Binary ANCOVA"` | -- | Symmetric pair in print output |

## Scope

### In Scope
- Rename `build_model_ancova()` -> `build_model_ancova_cont()` (remove old name entirely)
- Update `model_name` from `"ANCOVA"` to `"Continuous ANCOVA"` in existing function
- Fix `b_covariate` signature: add `= NULL` default in `build_model_ancova_cont()` for symmetry with `_bin`
- New `build_model_ancova_bin()` -- general binary ANCOVA builder (defaults to 2 arms)
- Binary sim fn closure (no `sigma`)
- `create_ancova_bin_sim_fn()` for `rctbp_sim_fn` wrapper
- Registration of `ancova_bin_2arms` in predefined model registry
- `simulate_data_ancova_bin_2arms_batch()` for BayesFlow batch format
- Update all references across the codebase (see "Complete File Touch List" below)
- Tests + TODO cleanup

### Out of Scope (deferred)
- 3-arm binary model (`ancova_bin_3arms`) -- follow-up PR
- BayesFlow neural network training for binary model
- Probability-scale convenience helpers (e.g., `prob_ctrl`/`prob_treat` -> log-odds)
- Probability-scale output columns (response rates, risk differences) -- `dev/99_dev_todos.md` item "Adapt `compute_measures()`" deferred; log-odds sufficient for v1
- Endpoint-specific design constructors (`design_binary()`)
- Probit or other link functions

## Implementation Plan

### Phase 1: Rename Continuous

**Files to modify:**
- `R/models_ancova.R` -- rename function, update `model_name`, fix `b_covariate` default, update `do.call` in wrappers
- `R/class_model.R` -- update `get_predefined_model()` registry AND `build_predefined_from_brmsfit_legacy()` (set `model_name = "Continuous ANCOVA"`)
- `R/models_survival.R` line 52 -- update stale `@seealso [build_model_ancova()]`
- `R/class_design.R` -- update roxygen cross-references
- `data-raw/build_predefined_models.R` -- verify only (already uses `_cont_2arms`/`_cont_3arms`; do NOT add binary model -- saves to `sysdata.rda`)
- Any test files that call `build_model_ancova()` directly

**Steps:**

1. **Rename `build_model_ancova()` -> `build_model_ancova_cont()`**:
   - Rename function, remove old name entirely (no deprecated alias)
   - Update roxygen: title -> "Create General Continuous ANCOVA Model", `@seealso` references
   - Add `@importFrom stats rbinom plogis` in anticipation of Phase 2 (or add during Phase 2)
   - Change `model_name = "ANCOVA"` -> `model_name = "Continuous ANCOVA"` in `rctbp_model()` call
   - Fix `b_covariate` signature: change bare `b_covariate,` to `b_covariate = NULL,` for symmetry with `_bin`

2. **Update `_cont_2arms` and `_cont_3arms` wrappers**:
   - Change `do.call(build_model_ancova, final_args)` -> `do.call(build_model_ancova_cont, final_args)` at lines 478 and 540

3. **Run `devtools::document()`** to regenerate NAMESPACE (removes stale `build_model_ancova` export, adds `build_model_ancova_cont` export)

4. **Update all cross-file references**:
   - `R/class_model.R`: update `build_predefined_from_brmsfit_legacy()` to use `model_name = "Continuous ANCOVA"`. The `get_predefined_model()` legacy registry (lines 182-191) does NOT need `ancova_bin_2arms` added -- it is a legacy path; only `load_predefined_model_components()` in `class_design.R` matters for new models. Do NOT add `ancova_bin_2arms` to `build_predefined_from_brmsfit_legacy()` either -- binary has no legacy path (the `else` branch will `cli_abort` for unknown models, which is correct).
   - `R/class_design.R`: verify `get_model_builder()` calls, update roxygen cross-references
   - `R/models_survival.R`: `@seealso [build_model_ancova()]` -> `[build_model_ancova_cont()]`
   - `data-raw/build_predefined_models.R`: update calls (do NOT add binary model -- this file saves to `sysdata.rda`)
   - Update any existing tests that call `build_model_ancova()` directly
   - Grep entire codebase: `grep -rn "build_model_ancova[^_]" --include="*.R"` to catch remaining references

5. **Run full existing test suite** -- wrappers must work before moving to Phase 2

### Phase 2: Binary Builder

**Files to modify:**
- `R/models_ancova.R`

**Steps:**

1. **Create `build_model_ancova_bin()`** -- mirrors `build_model_ancova_cont()` structure with these differences:

   **Parameters** (no `sigma`, `prior_sigma`, `link_sigma`):
   ```r
   build_model_ancova_bin <- function(
     prior_intercept = NULL,
     prior_covariate = NULL,
     prior_treatment = NULL,
     n_arms = 2,
     contrasts = "contr.treatment",
     p_alloc = c(0.5, 0.5),
     intercept = NULL,
     b_arm_treat = NULL,
     b_covariate = NULL
   )
   ```

   **Roxygen**: `@importFrom stats rbinom plogis` (in addition to existing stats imports)

   **Validation**: Same as continuous for `n_arms`, `p_alloc`, `b_arm_treat`, `b_covariate`, `contrasts`. No `sigma` validation. Must validate `b_covariate` and `b_arm_treat` are non-NULL in the sim fn closure (since defaults are NULL, `df$covariate * NULL` would silently produce `numeric(0)`).

   **Sim fn closure** -- use `create_ancova_sim_fn()` pattern from `class_design.R` lines 636-641 as reference (NOT `build_model_ancova()` which has a pre-existing bug in the matrix-contrasts path). Do NOT use `dplyr::mutate()` -- use direct assignment for performance:
   ```r
   simulate_data_ancova_bin <- local({
     # ... capture defaults ...
     function(n_total, n_arms, contrasts, p_alloc, intercept, b_arm_treat, b_covariate) {
       # ... validation (same as continuous minus sigma) ...
       # MUST validate b_arm_treat and b_covariate are non-NULL

       # Build contrast matrix (use create_ancova_sim_fn pattern, NOT build_model_ancova pattern)
       if (is.character(contrasts)) {
         contrasts_fn <- get(contrasts)
         contrast_matrix <- contrasts_fn(n_arms)
       } else {
         contrast_matrix <- contrasts
       }

       # Arm assignment + covariate (identical to continuous)
       df <- data.frame(
         covariate = stats::rnorm(n_total),
         arm = factor(sample(...), levels = ..., labels = c("ctrl", "treat_1", ...))
       )
       stats::contrasts(df$arm) <- contrast_matrix

       # Binary outcome generation (differs from continuous)
       eta <- as.vector(
         intercept +
         stats::model.matrix(~ arm, data = df)[, -1, drop = FALSE] %*% b_arm_treat +
         df$covariate * b_covariate
       )
       df$outcome <- stats::rbinom(n_total, size = 1, prob = stats::plogis(eta))
       return(df)
     }
   })
   ```
   Note: `as.vector()` after `%*%` prevents `rbinom()` receiving a matrix `prob`.

   **Mock data**: Call binary sim fn with `n_total = 20, n_arms, contrasts, p_alloc = equal, intercept = 0, b_arm_treat = rep(0, n_arms - 1), b_covariate = 0`

   **Priors**:
   - `prior_intercept`: default `normal(0, 5)` (log-odds scale)
   - `prior_covariate`: default `student_t(3, 0, 1)` (same as continuous)
   - `prior_treatment`: default `student_t(3, 0, 1)` (same as continuous)
   - No `prior_sigma`

   **brms compilation**:
   ```r
   brms::brm(
     formula = outcome ~ 1 + covariate + arm,
     data = mock_data,
     family = brms::bernoulli(link = "logit"),
     prior = priors,
     chains = 1, iter = 500, refresh = 0, silent = 2
   )
   ```

   **`rctbp_model()` construction**:
   ```r
   rctbp_model(
     sim_fn = simulate_data_ancova_bin,
     inference_model = brms_model,
     model_name = "Binary ANCOVA",
     n_endpoints = 1L,
     endpoint_types = "binary",
     n_arms = as.integer(n_arms),
     n_repeated_measures = 0L
   )
   ```

   **Set predefined model** (guarded -- only for known arm counts):
   ```r
   if (n_arms == 2L) model@predefined_model <- "ancova_bin_2arms"
   ```

2. **Roxygen documentation**: Document log-odds interpretation of `intercept` and `b_arm_treat`. Example showing `qlogis(0.3)` for 30% baseline rate.

### Phase 3: Registry + Batch Sim

**Files to modify:**
- `R/class_design.R` -- 4 registry touch points (note: `show_predefined_models()` uses a SEPARATE hardcoded vector from the registry list)
- `R/model_cache.R` -- add `"ancova_bin_2arms"` to `load_brms_model()` and `list_models()` ONLY (NOT `load_bf_model()` -- no BF model exists, would cause download failure)
- `R/models_ancova.R` -- add `create_ancova_bin_sim_fn()` + batch fn

**Steps:**

1. **Register `ancova_bin_2arms` in `class_design.R`**:
   - `load_predefined_model_components()` (~line 463):
     ```r
     ancova_bin_2arms = list(
       description = "2-arm binary ANCOVA",
       n_endpoints = 1L,
       endpoint_types = "binary",
       n_arms = 2L,
       n_repeated_measures = 0L
     )
     ```
   - `get_model_builder()` (~line 590): `ancova_bin_2arms = build_model_ancova_bin`
   - `create_sim_fn_for_model()` (~line 604): add `"ancova_bin_2arms"` branch -> `create_ancova_bin_sim_fn(n_arms = 2)`
   - `show_predefined_models()` (~line 724): add `"ancova_bin_2arms"` to the **hardcoded character vector** (this is separate from the registry list in `load_predefined_model_components()`)

2. **Update `model_cache.R`**:
   - Add `"ancova_bin_2arms"` to `available_models` in `load_brms_model()` (~line 118)
   - Do **NOT** add to `load_bf_model()` (~line 170) -- no BF model trained yet; adding it would cause a download attempt for a nonexistent `.keras` file
   - `list_models()` (~line 220): the current `models` vector is shared across brms and BF sections. Split into `brms_models` and `bf_models` vectors so `"ancova_bin_2arms"` only appears in brms output, not BF output. E.g.:
     ```r
     brms_models <- c("ancova_cont_2arms", "ancova_cont_3arms", "ancova_bin_2arms")
     bf_models <- c("ancova_cont_2arms", "ancova_cont_3arms")
     ```
   - Binary model is compile-on-demand: `load_predefined_model_components()` compiles via `get_model_builder()` when no cache file exists

3. **Create `create_ancova_bin_sim_fn(n_arms)`**:
   - Mirrors `create_ancova_sim_fn()` without `sigma`
   - Use `create_ancova_sim_fn()` as reference pattern (clean contrast handling), NOT `build_model_ancova_cont()` closure
   - Inner function defaults: `b_arm_treat = rep(0, n_arms - 1)`, `b_covariate = 0`, `intercept = 0`
   - Must include full contrast setup block
   - `test_args`: `list(n_total = 20L, n_arms = n_arms, p_alloc = rep(1, n_arms) / n_arms, intercept = 0, b_arm_treat = rep(0, n_arms - 1), b_covariate = 0)`
   - Wraps with `build_sim_fn(fn = fn, test_args = test_args)`

4. **Create `simulate_data_ancova_bin_2arms_batch()`**:
   - Mirrors `simulate_data_ancova_cont_2arms_batch()` without `sigma`
   - Vectorized: `eta_mat = intercept + covariate_mat * b_covariate + group_mat * b_arm_treat`
   - Outcome: `matrix(stats::rbinom(total_elements, 1, stats::plogis(eta_mat)), nrow = n_sims, ncol = n_total)`
   - Returns: `list(outcome, covariate, group, N, p_alloc)`

### Phase 4: Tests

**Files to create:**
- `tests/testthat/test-models_ancova_bin.R`

**Tests:**

1. **Sim fn validation** (~6 tests):
   - Bad inputs: negative n_total, wrong p_alloc length, non-numeric b_arm_treat
   - `sigma` not in binary sim fn formals
   - NULL `b_arm_treat` / `b_covariate` rejected at call time

2. **Sim fn output** (~5 tests):
   - Correct columns: `covariate`, `arm`, `outcome`
   - Outcome is strictly 0 or 1
   - Arm factor levels: `c("ctrl", "treat_1")`
   - Correct n_total rows

3. **Sim fn statistical properties** (~4 tests, `set.seed` + large n):
   - Null effect (`b_arm_treat = 0`): rates approximately equal across arms
   - Large positive `b_arm_treat`: treatment arm has higher rate
   - `intercept = qlogis(0.3)`: observed control rate ~ 0.3

4. **brms compilation** (~4 tests):
   - Compiles without error
   - Family is bernoulli
   - Parameters discovered via `brms::variables()` include intercept, covariate, arm terms
   - No sigma parameter

5. **Predefined model** (~5 tests):
   - `build_design(predefined_model = "ancova_bin_2arms")` works
   - `show_predefined_models()` includes `"ancova_bin_2arms"`
   - `show_predefined_models("bin")` returns it (case-sensitive grep)
   - Design `endpoint_types` is `"binary"`
   - `show_condition_args()` lists `b_arm_treat` and `b_covariate` (not `sigma`) as required params

6. **Integration** (~3 tests):
   - Full pipeline with `n_sims = 50`, `intercept = qlogis(0.3)`
   - Null effect: power near alpha
   - Sequential design with `analysis_at` + `boundary_obf()`

7. **Batch sim fn** (~4 tests):
   - Correct dimensions
   - Values in {0, 1}
   - Statistical equivalence to standard sim fn

8. **Rename verification** (~2 tests):
   - `build_model_ancova_cont_2arms()` unchanged behavior
   - `build_model_ancova_cont()` produces correct gaussian model

### Phase 5: Documentation + TODO Cleanup

**Files to modify:**
- `dev/99_dev_todos.md`

**Steps:**
1. Mark "Binary ANCOVA" as DONE, update function name reference (`build_model_binary_ancova_2arms()` -> `build_model_ancova_bin()`)
2. Update "Adapt `compute_measures()`" -> "DEFERRED -- log-odds sufficient for v1"

## Verification & Validation

- **Automated**: All tests pass, R CMD check clean
- **Manual smoke test**: REPL power analysis -- `n = 200, intercept = qlogis(0.3), b_arm_treat = qlogis(0.5) - qlogis(0.3)`. Note: comparison with `pwr::pwr.2p.test()` is order-of-magnitude only (different effect parameterizations -- arcsine vs log-odds). For exact validation, use large-n simulation check (n -> inf, power -> 1).
- **Numerical checks**:
  - `b_arm_treat = 0` -> power ~ alpha
  - Large `b_arm_treat` + large `n_total` -> power -> 1.0
  - Batch and standard sim fns: statistically equivalent
- **Phase 1 gate**: Run full existing test suite + R CMD check after rename, before starting Phase 2

## Dependencies

- `compute_measures.R`: **no changes** -- parameter-agnostic ROPE decisions
- Backend brms/BF code: **no changes** -- generic draws extraction
- Sequential analysis: **no changes** -- works on any brmsfit
- Accrual/dropout: **no changes** -- operates on sim fn output rows

## Complete File Touch List

### Phase 1 (Rename)
| File | Change |
|------|--------|
| `R/models_ancova.R` | Rename function, update `model_name`, fix `b_covariate` default, update `do.call` in `_cont_2arms`/`_cont_3arms` |
| `R/class_model.R` | Update `get_predefined_model()` builder list AND `build_predefined_from_brmsfit_legacy()` (`model_name = "Continuous ANCOVA"`) |
| `R/models_survival.R` | `@seealso` reference |
| `R/class_design.R` | Update roxygen cross-references |
| `data-raw/build_predefined_models.R` | Already uses `_cont_2arms`/`_cont_3arms` -- verify no changes needed (do NOT add binary model -- saves to `sysdata.rda`) |
| `NAMESPACE` | Auto-regenerated by `devtools::document()` |
| `tests/testthat/*` | Update any direct calls to `build_model_ancova()` |

### Phase 2 (Binary Builder)
| File | Change |
|------|--------|
| `R/models_ancova.R` | Add `build_model_ancova_bin()`, binary sim fn closure, `@importFrom stats rbinom plogis` |

### Phase 3 (Registry)
| File | Change |
|------|--------|
| `R/class_design.R` | 4 registry touch points (including hardcoded `show_predefined_models()` vector) |
| `R/model_cache.R` | Add `"ancova_bin_2arms"` to `load_brms_model()` only (NOT `load_bf_model()`); split `list_models()` shared vector into `brms_models`/`bf_models` |
| `R/models_ancova.R` | Add `create_ancova_bin_sim_fn()` + batch fn |

### Phase 4 (Tests)
| File | Change |
|------|--------|
| `tests/testthat/test-models_ancova_bin.R` | New test file |

### Phase 5 (Docs)
| File | Change |
|------|--------|
| `dev/99_dev_todos.md` | Mark done, update function name |

## Key Implementation Details

### Function hierarchy

```
build_model_ancova_cont(...)                  # general continuous (renamed from build_model_ancova)
|-- build_model_ancova_cont_2arms(...)        # 2-arm convenience (existing)
+-- build_model_ancova_cont_3arms(...)        # 3-arm convenience (existing)

build_model_ancova_bin(...)                   # general binary (new)
+-- (future: build_model_ancova_bin_3arms)
```

### Implementation warnings

- **Contrast pattern**: Use `create_ancova_sim_fn()` (class_design.R lines 636-641) as reference for contrast handling. The `build_model_ancova()` closure has a pre-existing bug in the matrix-contrasts path (`get(matrix)` errors). Do NOT copy that pattern.
- **No dplyr in closures**: The continuous sim fn uses `dplyr::mutate()` inside the closure (performance anti-pattern for code that runs thousands of times). The binary sim fn should use direct `df$outcome <- ...` assignment instead.
- **NULL validation**: The binary closure MUST validate `b_covariate` and `b_arm_treat` are non-NULL before arithmetic. With `NULL` defaults captured from the outer function, `df$covariate * NULL` silently produces `numeric(0)`.

### Parameter flow for binary models

```
User specifies: intercept = qlogis(0.3)  # 30% control rate
                b_arm_treat = log(2)     # odds ratio = 2
                thr_fx_eff = 0            # any positive log-OR
                thr_dec_eff = 0.975       # 97.5% posterior probability

Sim fn:          logit(p) = intercept + X * b_arm_treat + cov * b_covariate
                 outcome = rbinom(n, 1, plogis(eta))

brms estimates:  b_armtreat_1 (log-odds)

Backend:         pr_eff = mean(draws > thr_fx_eff)
compute_measures: pwr_eff = mean(pr_eff >= thr_dec_eff)
```

No special binary handling needed downstream.

### User-facing example

```r
design <- build_design(
  predefined_model = "ancova_bin_2arms",
  target_params = "b_armtreat_1"
)

conditions <- build_conditions(
  design = design,
  crossed = list(
    n_total = c(100, 200, 300),
    b_arm_treat = c(log(1.5), log(2.0))  # OR = 1.5 and 2.0
  ),
  constant = list(
    intercept = qlogis(0.3),  # 30% control response rate
    b_covariate = 0.3,
    thr_dec_eff = 0.975,
    thr_dec_fut = 0.5,
    thr_fx_eff = 0,
    thr_fx_fut = 0
  )
)

results <- power_analysis(conditions, n_sims = 1000)
```

## Notes

_Living section -- updated during implementation._

## Review Feedback

### Review 1 (2026-03-10) -- Architecture

**Verdict: REVISE** -- resolved by switching from `family` branching to separate functions.

### Review 2 (2026-03-11) -- Completeness

**Verdict: REVISE** -- issues incorporated:

1. [CRITICAL] `class_model.R` fifth registry: Added to Phase 1
2. [CRITICAL] `model_cache.R` hardcoded `available_models`: Added to Phase 3
3. [IMPORTANT] `do.call(build_model_ancova, ...)` in wrappers: Phase 1 Step 2 with test gate
4. [IMPORTANT] `models_survival.R` stale `@seealso`: Phase 1 file list
5. [IMPORTANT] `b_covariate` signature asymmetry: Phase 1 fix
6. [IMPORTANT] Contrast setup in binary closure: Explicit in pseudocode
7. [MINOR] `model_name` symmetry: `"Continuous ANCOVA"` / `"Binary ANCOVA"`
8. [MINOR] `predefined_model` guard: `if (n_arms == 2L)` in Phase 2
9. [MINOR] `data-raw/build_predefined_models.R`: Phase 1 file list
10. [MINOR] Dev TODO function name: Phase 5 updates reference

### Review 3 (2026-03-11) -- Final

**Verdict: REVISE** -- issues incorporated:

1. [CRITICAL] NAMESPACE: Added explicit `devtools::document()` step (Phase 1 Step 3) and NAMESPACE to file touch list
2. [CRITICAL] `build_predefined_from_brmsfit_legacy()`: Added explicitly to Phase 1 Step 4 and file touch list
3. [CRITICAL] `data-raw/build_predefined_models.R` saves to `sysdata.rda`: Corrected -- do NOT add binary model there. Removed from Phase 3.
4. [CRITICAL] `load_bf_model()` would cause download failure: Corrected -- only add to `load_brms_model()` + `list_models()`, NOT `load_bf_model()`
5. [IMPORTANT] Contrast bug in existing closure: Added "Implementation warnings" section -- use `create_ancova_sim_fn()` as reference
6. [IMPORTANT] NULL validation for `b_covariate`/`b_arm_treat`: Added to validation section and implementation warnings
7. [IMPORTANT] `@importFrom stats rbinom plogis`: Added to Phase 2 roxygen
8. [IMPORTANT] `show_predefined_models()` is a separate hardcoded vector: Noted explicitly in Phase 3 Step 1
9. [MINOR] No `dplyr::mutate` in binary closure: Added to implementation warnings
10. [MINOR] `pwr::pwr.2p.test()` comparison is approximate: Corrected smoke test description
11. [MINOR] `show_condition_args()` test: Added to Phase 4 test group 5
12. [MINOR] `brms::variables()` for dynamic param discovery: Updated test group 4

### Review 4 (2026-03-11) -- Final

**Verdict: REVISE** -- issues incorporated:

1. [IMPORTANT] `list_models()` shared vector: Split into `brms_models`/`bf_models` so binary only appears in brms output
2. [IMPORTANT] Legacy path boundary: Confirmed `get_predefined_model()` does NOT need `ancova_bin_2arms`; documented in Phase 1 Step 4
3. [MINOR] `data-raw/build_predefined_models.R` already correct: Changed to verify-only in file touch list
