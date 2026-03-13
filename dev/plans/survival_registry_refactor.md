# Plan: Register Survival Model in Predefined Model Registry

> Status: IMPLEMENTED
> Created: 2026-03-11
> Scope: Refactor `build_model_survival_2arms()` to follow the ANCOVA registry pattern

---

## Goal

Make the survival model (`survival_exp_2arms`) a first-class predefined model accessible via `build_design(model_name = "survival_exp_2arms")`, following the same pattern as `ancova_cont_2arms` and `ancova_bin_2arms`.

---

## Current State

**Survival model** (`R/models_survival.R`):
- `build_model_survival_2arms()` — monolithic function that creates sim_fn, compiles brms model, and returns a design in one call
- Sim_fn uses baked-in defaults via `local()` closure (old pattern)
- Params: `baseline_hazard`, `hazard_ratio`, `accrual_rate`, `followup_time`, `p_alloc`
- brms formula: `time | cens(censored) ~ arm`, family `weibull()`
- Output columns: `time`, `censored`, `arm`, `enrollment_time`
- **Note**: The existing function is already partially broken — it calls `build_design()` without passing `n_endpoints`, `endpoint_types`, or `n_arms`, so the existing integration test is `skip()`-ed.

**ANCOVA pattern** (the target):
1. `build_model_ancova_bin()` — generic builder, compiles brms model, returns `rctbp_model` (legacy class)
2. `build_model_ancova_bin_2arms()` — thin wrapper supplying 2-arm defaults, sets `@predefined_model`
3. `create_ancova_bin_sim_fn()` — returns `rctbp_sim_fn` object with `test_args` and NULL-default effect params (`@keywords internal`, not exported)
4. Registry in `class_design.R`: `load_predefined_model_components()` calls `builder_fn()`, extracts `@inference_model` (brmsfit), pairs with `create_sim_fn_for_model()`, and passes both to `build_design()`
5. `model_cache.R`: model name in `brms_models` vector in `list_models()` AND in `available_models` in `load_brms_model()`

---

## Key Differences from ANCOVA

| Aspect | ANCOVA | Survival |
|--------|--------|----------|
| Family | `gaussian()` / `bernoulli()` | `weibull()` |
| Formula | `outcome ~ arm + covariate` | `time \| cens(censored) ~ arm` |
| Effect params | `intercept`, `b_arm_treat`, `b_covariate`, `sigma` | `baseline_hazard`, `hazard_ratio` |
| Accrual built-in | No (optional via `accrual_rate` in conditions) | Yes (sim_fn generates enrollment times) |
| Dual-route params | Not needed | `accrual_rate`, `followup_time` (already in `DUAL_ROUTE_PARAMS`) |
| Event-driven analysis | N/A | `analysis_timing = "events"` + `subset_by_events()` |
| Censoring column | N/A | `censored` (brms convention: 0=event, 1=censored) |
| Target param | `b_arm2` / `b_armtreat_1` | `b_armtreat_1` (log hazard ratio) |
| `accrual_pattern` | N/A | Hardcoded `"uniform"` (deliberate simplification; non-uniform accrual can be added later) |

---

## Pre-implementation Verification

Before starting, verify in an R session:
```r
# Confirm brms::variables() works on weibull model (chains=1, matching ANCOVA pattern)
m <- suppressMessages(suppressWarnings(brms::brm(
  brms::bf(time | cens(censored) ~ arm),
  data = data.frame(time=c(1,2,3,4), censored=c(0,0,1,0),
                    arm=factor(c("ctrl","ctrl","treat_1","treat_1"))),
  family = brms::weibull(),
  prior = brms::set_prior("normal(0,5)", class="Intercept") +
    brms::set_prior("normal(0,2)", class="b"),
  chains = 1, iter = 500, refresh = 0, silent = 2
)))
brms::variables(m)
# Expect: includes "b_armtreat_1" (and "shape", "Intercept" — shape is filtered by build_design)
```

If `brms::variables()` does NOT return `b_armtreat_1`, the registry approach needs adaptation. Note: ANCOVA builders also use `chains = 1, iter = 500` (not `chains = 0`) for this reason.

---

## Plan

### Phase 0: Add `"survival"` to Validator Allowlists

**BLOCKER — must be done first.**

**`R/class_design.R:152-153`** — the `rctbp_design` S7 validator:
```r
any(!self@endpoint_types %in% c("continuous", "binary", "count"))
# Change to:
any(!self@endpoint_types %in% c("continuous", "binary", "count", "survival"))
```

**`R/class_model.R:118-119`** — the `rctbp_model` S7 validator (same allowlist). This IS active: the generic builder in Phase 1 creates `rctbp_model` objects (same as ANCOVA builders do). Must add `"survival"` here too.

Update the roxygen `@param endpoint_types` docs in both classes to list `"survival"` as valid.

Verify no downstream code branches on `endpoint_types` values with `grep -r "endpoint_types" R/` — it is metadata-only (stored/displayed, never used in conditionals).

### Phase 1: Create Generic Builder (`build_model_survival_exp`)

**File: `R/models_survival.R`** | **Export: `@export`**

Create `build_model_survival_exp()` following the `build_model_ancova_bin()` pattern. Returns `rctbp_model` (NOT `rctbp_design` — the registry extracts `@inference_model` from the model object).

```r
build_model_survival_exp <- function(
  prior_intercept = NULL,    # Prior for Intercept (log baseline scale)
  prior_treatment = NULL,    # Prior for treatment coefficient
  n_arms = 2,
  p_alloc = c(0.5, 0.5)
) {
  # Default priors: normal(0, 5) for Intercept, normal(0, 2) for b
  if (is.null(prior_intercept)) {
    prior_intercept <- brms::set_prior("normal(0, 5)", class = "Intercept")
  }
  if (is.null(prior_treatment)) {
    prior_treatment <- brms::set_prior("normal(0, 2)", class = "b")
  }
  priors <- prior_intercept + prior_treatment

  # Minimal sample data for compilation
  sample_data <- data.frame(
    time = c(1, 2, 3, 4),
    censored = c(0, 0, 1, 0),
    arm = factor(c("ctrl", "ctrl", "treat_1", "treat_1"),
                 levels = c("ctrl", "treat_1"))
  )

  # Compile brms model (suppress compilation messages)
  # Use chains = 1, iter = 500 (NOT chains = 0) — matches ANCOVA builders.
  # chains = 0 may cause brms::variables() to return empty, breaking
  # par_names_inference extraction in build_design().
  compiled <- suppressMessages(suppressWarnings(
    brms::brm(
      formula = brms::bf(time | cens(censored) ~ arm),
      data = sample_data,
      family = brms::weibull(),
      prior = priors,
      chains = 1,
      iter = 500,
      refresh = 0,
      silent = 2
    )
  ))

  # Minimal sim_fn placeholder — must pass rctbp_model validator
  # (requires n_total and p_alloc in formals)
  # The registry ignores this and uses create_sim_fn_for_model() instead.
  placeholder_sim_fn <- function(n_total, p_alloc = NULL, ...) {
    cli::cli_abort("This is a placeholder sim_fn. Use build_design(model_name = ...) instead.")
  }

  # Return rctbp_model (registry extracts @inference_model from this)
  rctbp_model(
    sim_fn = placeholder_sim_fn,
    inference_model = compiled,
    model_name = "Exponential Survival",
    n_endpoints = 1L,
    endpoint_types = "survival",
    n_arms = as.integer(n_arms),
    n_repeated_measures = 0L
  )
}
```

Key decisions:
- **No `contrasts` parameter**: Unlike ANCOVA, the brms formula `time | cens(censored) ~ arm` compiles identically regardless of contrast coding. Including it would be misleading.
- **No sim params in builder**: `baseline_hazard`, `hazard_ratio`, etc. belong in the sim_fn factory, not the model builder.
- **`chains = 1, iter = 500`**: Matches ANCOVA builders. The current survival code uses `chains = 0`, but ANCOVA builders use `chains = 1` to ensure `brms::variables()` works on the compiled model. Follow the ANCOVA pattern.
- **Placeholder sim_fn with named formals**: The `rctbp_model` validator at `class_model.R:133-138` requires `formals()` to include `"n_total"` and `"p_alloc"`. A plain `function(...) NULL` fails this check. The placeholder errors on use to prevent accidental invocation.

### Phase 2: Create 2-Arm Wrapper (`build_model_survival_exp_2arms`)

**File: `R/models_survival.R`** | **Export: `@export`**

Thin wrapper following `build_model_ancova_bin_2arms()`:

```r
build_model_survival_exp_2arms <- function(...) {
  dots <- list(...)
  default_args <- list(
    prior_intercept = NULL,
    prior_treatment = NULL,
    n_arms = 2,
    p_alloc = c(0.5, 0.5)
  )
  final_args <- modifyList(default_args, dots)
  model <- do.call(build_model_survival_exp, final_args)
  model@predefined_model <- "survival_exp_2arms"
  invisible(model)
}
```

### Phase 3: Create Sim_fn Factory (`create_survival_exp_sim_fn`)

**File: `R/models_survival.R`** | **Not exported** (`@keywords internal`)

Follow `create_ancova_bin_sim_fn()` pattern. Effect params (`baseline_hazard`, `hazard_ratio`) get `NULL` defaults so `get_args_without_defaults()` treats them as required/configurable. `p_alloc` gets a non-NULL default since it's handled specially by `build_conditions()`.

`accrual_pattern` is deliberately hardcoded to `"uniform"` inside the function body. This matches the current `build_model_survival_2arms()` behavior and is not a regression.

```r
create_survival_exp_sim_fn <- function(n_arms) {
  default_p_alloc <- rep(1, n_arms) / n_arms

  fn <- function(n_total,
                 p_alloc = default_p_alloc,
                 baseline_hazard = NULL,     # NULL = required
                 hazard_ratio = NULL,        # NULL = required
                 accrual_rate = NULL,        # NULL = required (dual-routed)
                 followup_time = NULL) {     # NULL = required (dual-routed)
    # Validate inputs
    if (is.null(baseline_hazard) || baseline_hazard <= 0) {
      cli::cli_abort("{.arg baseline_hazard} must be a positive number")
    }
    if (is.null(hazard_ratio) || hazard_ratio <= 0) {
      cli::cli_abort("{.arg hazard_ratio} must be a positive number")
    }
    if (is.null(accrual_rate) || accrual_rate <= 0) {
      cli::cli_abort("{.arg accrual_rate} must be a positive number")
    }
    if (is.null(followup_time) || followup_time < 0) {
      cli::cli_abort("{.arg followup_time} must be non-negative")
    }

    # Arm assignment
    arm_idx <- sample(0:1, n_total, replace = TRUE, prob = p_alloc)
    arm <- factor(arm_idx, levels = 0:1, labels = c("ctrl", "treat_1"))

    # Enrollment times (staggered entry, uniform accrual)
    enrollment_time <- generate_enrollment_times(
      n_total, accrual_rate, accrual_pattern = "uniform"
    )

    # Exponential event times
    hazard <- ifelse(arm_idx == 0, baseline_hazard,
                      baseline_hazard * hazard_ratio)
    event_time <- stats::rexp(n_total, rate = hazard)

    # Administrative censoring from staggered enrollment
    max_calendar_time <- max(enrollment_time) + followup_time
    admin_censor_time <- max_calendar_time - enrollment_time

    # Observed time and censoring indicator
    time <- pmin(event_time, admin_censor_time)
    censored <- as.integer(event_time > admin_censor_time)

    data.frame(
      time = time,
      censored = censored,
      arm = arm,
      enrollment_time = enrollment_time
    )
  }

  build_sim_fn(
    fn = fn,
    test_args = list(
      n_total = 20L,
      p_alloc = rep(1, n_arms) / n_arms,
      baseline_hazard = 0.1,
      hazard_ratio = 0.7,
      accrual_rate = 10,
      followup_time = 12
    )
  )
}
```

**Dual-route semantics**: `accrual_rate` and `followup_time` are in `DUAL_ROUTE_PARAMS`. The dual-route logic at `class_conditions.R:585-596` copies them from `sim_args` to `analysis_args`. This is:
- **Redundant** when params are in `constant` (they reach `analysis_args` via normal param separation)
- **Load-bearing** when params are in `crossed` (dual-route correctly copies the per-iteration sim value into `analysis_args`)

Both paths work correctly; the dual-route mechanism is needed for the `crossed` case.

### Phase 4: Register in `class_design.R`

**4 touch points:**

1. **`load_predefined_model_components()` registry**:
```r
survival_exp_2arms = list(
  description = "2-arm exponential survival",
  n_endpoints = 1L,
  endpoint_types = "survival",
  n_arms = 2L,
  n_repeated_measures = 0L
)
```

2. **`get_model_builder()`**:
```r
survival_exp_2arms = build_model_survival_exp_2arms
```

3. **`create_sim_fn_for_model()`**:
```r
} else if (model_name == "survival_exp_2arms") {
  create_survival_exp_sim_fn(n_arms = 2)
}
```

4. **`show_predefined_models()`**:
```r
models <- c("ancova_cont_2arms", "ancova_cont_3arms", "ancova_bin_2arms", "survival_exp_2arms")
```

### Phase 5: Update `model_cache.R`

**Two touch points:**

1. Add `"survival_exp_2arms"` to the `available_models` vector inside `load_brms_model()`. Without this, direct calls to `load_brms_model("survival_exp_2arms")` hard-error.
2. Add `"survival_exp_2arms"` to the `brms_models` vector inside `list_models()`.

**Note**: `load_brms_model()` can also download from GitHub releases. No release asset exists yet for `survival_exp_2arms` — this is fine for now (first use always compiles locally via the registry path). The GitHub release upload is deferred until the model is stable.

### Phase 6: Remove Old Function, Migrate Tests

Since the package is pre-release (deprecations are removals per project policy), **delete** `build_model_survival_2arms()` entirely.

**Test migration**: Rename `tests/testthat/test-survival.R` to `tests/testthat/test-models-survival.R` (consistent with `test-models-ancova.R`). Replace `make_survival_sim_fn()` with a helper that calls `create_survival_exp_sim_fn(n_arms = 2)` and extracts the callable. Rewrite the integration test to use `build_design(model_name = "survival_exp_2arms")`.

**Run `devtools::document()`** after deletion — this removes `man/build_model_survival_2arms.Rd` and updates `NAMESPACE`.

### Phase 7: Tests

**File: `tests/testthat/test-models-survival.R`**

Preserved from existing file (adapted to new sim_fn factory):
- Sim_fn returns correct data structure (columns, types, levels)
- All events observed with `followup_time = Inf` (no censoring)
- Censoring increases with shorter followup
- `hazard_ratio` produces survival difference
- Enrollment times sorted and non-negative
- Allocation respects `p_alloc`

New tests:
1. `build_model_survival_exp()` returns valid `rctbp_model` with `endpoint_types = "survival"` and correct family
2. `build_model_survival_exp_2arms()` sets `@predefined_model` to `"survival_exp_2arms"`
3. `create_survival_exp_sim_fn()` returns `rctbp_sim_fn` with correct `test_args`; calling it works (S7 `class_function` inheritance)
4. `censored` column follows brms convention (0=event, 1=censored)
5. Registry round-trip: `build_design(model_name = "survival_exp_2arms")` works (`skip_on_cran()` + `skip("manual")` for Stan compilation)
6. `show_predefined_models()` includes `"survival_exp_2arms"`
7. `show_condition_args()` shows `baseline_hazard`, `hazard_ratio`, `accrual_rate`, `followup_time` as required
8. `show_target_params("survival_exp_2arms")` returns `"b_armtreat_1"` (confirms `brms::variables()` works on weibull chains=0; confirm `shape` is excluded) — requires compilation, so `skip_on_cran()` + `skip("manual")`
9. `build_conditions()` with dual-route params works correctly
10. End-to-end: `build_design()` → `build_conditions()` → sim_fn produces valid data
11. `followup_time = 0` edge case: sim_fn runs (degenerate but valid at sim level). Note: last-enrolled patient gets `time = 0` which brms `weibull()` may reject — test only the sim_fn, not brms fitting. Document that `followup_time = 0` is not recommended for actual power analysis.
12. `analysis_timing` default: with `analysis_timing = "sample_size"` (default) and `analysis_at = 50`, subsetting returns exactly 50 rows (documents expected behavior for users who forget `analysis_timing = "events"`)

### Phase 8: Update Documentation

- Run `devtools::document()` to regenerate man pages and NAMESPACE
- Update roxygen `@seealso` on new functions to reference `build_design()` and `show_predefined_models()`
- Update `dev/07_reference.md`: change survival row to "Complete (registry model)", update function names
- Update `dev/99_dev_todos.md`: add survival registry refactor and mark done
- Update `show_predefined_models()` output in any vignettes

---

## Naming Decision

**`survival_exp_2arms`** (not `survival_2arms`) because:
- The model assumes **exponential** event times (constant hazard)
- Leaves room for `survival_weibull_2arms` (shape parameter) or `survival_cox_2arms` later
- brms family is `weibull()` but shape=1 is exponential — the sim_fn generates exponential data
- Consistent with `ancova_cont_*` / `ancova_bin_*` pattern (model type + variant)

---

## Export Status

| Function | Export | Rationale |
|----------|--------|-----------|
| `build_model_survival_exp()` | `@export` | Public API (matches `build_model_ancova_cont()`) |
| `build_model_survival_exp_2arms()` | `@export` | Public API (matches `build_model_ancova_cont_2arms()`) |
| `create_survival_exp_sim_fn()` | `@keywords internal` | Internal factory (matches `create_ancova_sim_fn()`) |

---

## Risks & Edge Cases

1. **`endpoint_types = "survival"`** — BLOCKER: must add to validator allowlist in BOTH `R/class_design.R:152-153` AND `R/class_model.R:118-123`. Handled in Phase 0.

2. **`analysis_timing = "events"`** — the user must set this in `build_conditions()` constant args. The registry doesn't auto-set it. If omitted, `subset_analysis_data()` treats `analysis_at` values as sample sizes (silent wrong results). Future improvement: consider a `required_analysis_args` metadata field in registry entries. For now, document clearly in roxygen and test the default behavior.

3. **brms `weibull()` family** — the model compiles with `weibull()` even though we simulate exponential data. Weibull with shape=1 is exponential; brms estimates the shape parameter. The compiled model has a `shape` parameter that ANCOVA models don't — `build_design()` filters this out when populating `par_names_inference` (only `b_*` class parameters). No impact on `compute_measures()`.

4. **No batch sim_fn** — ANCOVA models have `simulate_data_ancova_cont_2arms_batch()` for BayesFlow. Not needed yet (BF retraining in progress). Can be added later.

5. **`followup_time = 0`** — degenerate: the last-enrolled patient gets `time = 0`. brms `weibull()` family requires strictly positive times, so this will fail at the fitting stage. The sim_fn validation allows `>= 0`, which is correct (the sim_fn shouldn't know about brms constraints). Document in roxygen that `followup_time` should be positive for meaningful results.

6. **`accrual_pattern` not user-configurable** — hardcoded to `"uniform"`. Matches current behavior, not a regression.

7. **Placeholder sim_fn in model builder** — Uses `function(n_total, p_alloc = NULL, ...) { abort(...) }` to pass `rctbp_model` validator. The registry ignores this and uses `create_sim_fn_for_model()` instead.

8. **GitHub release asset** — `load_brms_model("survival_exp_2arms")` will attempt download on cache miss when `force_download = TRUE`, but no release asset exists. First use always compiles locally via the registry. Deferred until model is stable.

9. **`brms::variables()` on weibull model** — must verify pre-implementation (see verification section above). Using `chains = 1, iter = 500` (matching ANCOVA builders) should ensure variable names are available. If it still fails, investigate brms version compatibility.

---

## Files to Modify

| File | Changes |
|------|---------|
| `R/class_design.R:152-153` | Add `"survival"` to `endpoint_types` validator |
| `R/class_design.R` (4 registry functions) | Add `survival_exp_2arms` entries |
| `R/class_model.R:118-119` | Add `"survival"` to `endpoint_types` validator |
| `R/models_survival.R` | Replace monolithic function with builder + wrapper + factory |
| `R/model_cache.R` (`load_brms_model` + `list_models`) | Add `"survival_exp_2arms"` to both vectors |
| `tests/testthat/test-survival.R` → `test-models-survival.R` | Rename, rewrite, add new tests |
| `man/build_model_survival_2arms.Rd` | Auto-deleted by `devtools::document()` |

---

## Estimated Scope

| Phase | LOC (est.) | Complexity |
|-------|-----------|------------|
| 0. Validator fixes | ~5 | Trivial |
| 1. Generic builder | ~50 | Low |
| 2. 2-arm wrapper | ~15 | Low |
| 3. Sim_fn factory | ~65 | Medium |
| 4. Registry entries | ~15 | Low |
| 5. Model cache (2 vectors) | ~2 | Trivial |
| 6. Remove old + migrate tests | ~-130 (net deletion) | Low |
| 7. New tests | ~130-170 | Medium |
| 8. Documentation | ~25 | Low |
| **Total** | **~300-350** (net ~130-170 new) | **Medium** |
