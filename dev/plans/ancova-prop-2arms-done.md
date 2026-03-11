# Plan: Logit-Normal Proportional ANCOVA (`ancova_prop_2arms`)

**Created**: 2026-03-11
**Author**: Claude

## Status

| Phase | Status | Date | Notes |
|-------|--------|------|-------|
| Plan | DONE | 2026-03-11 | Approved 2026-03-11 |
| Phase 1: Core model implementation | MERGED | 2026-03-11 | PR #24 — sim_fn, brms builder, registry, tests |
| Ship | MERGED | 2026-03-11 | Squash-merged via PR #24 |

## Summary

**Motivation**: The package currently supports continuous (`ancova_cont`) and binary (`ancova_bin`) outcomes. Proportional outcomes (values in [0,1], e.g., adherence rates, percentage improvement, symptom severity scales) are common in RCTs but have no dedicated model. Users must manually set up logit-transformations.

**Outcome**: Users can run power analysis for proportional outcomes by specifying `model = "ancova_prop_2arms"` in `build_design()`, with parameters interpreted on the log-odds scale. The model uses a logit-normal distribution: outcomes are proportions in (0,1) whose logit-transform is normally distributed.

## Design Decisions

| Decision | Options | Chosen | Rationale |
|----------|---------|--------|-----------|
| Data generation process | (A) Logitnormal: logit(Y) ~ Normal(eta, sigma^2) | A | Correctly specified model -- logit(Y) is exactly normal, so gaussian ANCOVA on logit scale is exact. Simpler than Beta. |
| | (B) Beta: Y ~ Beta(mu*phi, (1-mu)*phi), then logit-transform | -- | Model would be misspecified (logit of Beta is not normal). More complex parameterization. |
| Outcome column content | (A) Logit-transformed values (on real line) | A | Direct input to gaussian brms model. No additional transformation needed at analysis time. |
| | (B) Raw proportions (0,1), transform at brms stage | -- | Would require formula-level transform or pre-processing step. Breaks pattern. |
| brms family | gaussian() | -- | Outcome is on logit scale (unbounded real), gaussian is correct. Same as continuous ANCOVA. |
| sigma parameter | Include (like continuous, unlike binary) | -- | Logitnormal has a scale parameter controlling variability on the logit scale. Essential for the model. |
| Default priors | See "Prior Specification" section below | -- | Priors must be justified on the bounded proportion scale, not just on the logit scale. The implied prior on proportions must remain unimodal. |
| endpoint_types metadata | "proportion" | -- | Distinct from "continuous" and "binary". Allows future proportion-specific output (e.g., back-transformed CIs). |
| Separate builder vs wrapper around continuous | Separate `build_model_ancova_prop()` | -- | Clearer error messages ("log-odds scale"), different default priors, different `endpoint_types`. Follows binary model precedent. |

### Prior Specification

Priors must be justified on the bounded proportion scale. The implied prior on (0,1) must be **unimodal** -- no bimodal or U-shaped distributions that concentrate mass near 0 and 1.

**Sigma prior** -- this is the most sensitive choice. Numerically verified bimodality threshold (second derivative of logitnormal density at y=0.5, mu=0):

| sigma (logit scale) | Implied proportion spread at 50% baseline (95% CI) | Shape on (0,1) | f''(0.5) |
|---------------------|-----------------------------------------------------|-----------------|----------|
| 0.5 | (0.27, 0.73) | tight unimodal | -33.9 |
| 1.0 | (0.12, 0.88) | broad unimodal | -12.8 |
| 1.3 | (0.07, 0.93) | flat unimodal | -1.8 |
| 1.4 | (0.06, 0.94) | borderline unimodal | -0.2 |
| 1.5 | (0.05, 0.95) | **bimodal** (U-shaped) | +1.0 |
| 2.0 | (0.02, 0.98) | strongly bimodal | +1.8 |

**Bimodality threshold: sigma ≈ 1.45** (at mu=0, the worst case; non-central mu shifts the threshold higher).

**Default**: `normal(0, 0.5)` with `lb = 0` (half-normal, SD=0.5). Prior mass below bimodality threshold:
- P(sigma < 1.0) = 95.5%
- P(sigma < 1.45) = 99.6%

This ensures the implied prior on proportions is unimodal for virtually all prior draws. Much tighter than the continuous model's `normal(0, 10)`, justified by the bounded scale.

**Intercept prior**: `normal(0, 2.5)`. On the proportion scale:
- +/-1 SD (logit +/-2.5): proportion in (0.076, 0.924)
- +/-2 SD (logit +/-5.0): proportion in (0.007, 0.993)

Covers essentially all plausible baseline rates while placing most mass on moderate proportions. Tighter than binary model's `normal(0, 5)` to avoid unnecessary diffuseness.

**Treatment/covariate priors**: `student_t(3, 0, 0.5)`. Scaled to match the sigma prior scale (SD = 0.5), so that standardized effects (b/sigma) are comparable to the continuous model's `student_t(3, 0, 1)` at sigma=1.

Justification: the continuous model uses `student_t(3, 0, 1)` with typical sigma ~ 1, giving standardized effects ~ student_t(3, 0, 1). The proportional model's sigma prior (half-normal, SD=0.5) has median ~ 0.34. Using `student_t(3, 0, 0.5)` gives standardized effects ~ student_t(3, 0, 1.5) -- comparable to the continuous model. Using the unscaled `student_t(3, 0, 1)` would give standardized effects ~ student_t(3, 0, 3) -- 3x too wide.

Coverage: 80% of |b| < 0.82 log-odds, 95% of |b| < 1.59 log-odds.

## Scope

### In Scope
- `build_model_ancova_prop()` -- core builder function with logit-scale parameters
- `build_model_ancova_prop_2arms()` -- convenience wrapper with 2-arm defaults
- `create_ancova_prop_sim_fn()` -- sim_fn factory returning `rctbp_sim_fn`
- `simulate_data_ancova_prop_2arms_batch()` -- vectorized batch simulator for BayesFlow (`@keywords internal`)
- Registry updates (4 locations in `class_design.R`) + validator whitelist update (1 location)
- Test file `test-models_ancova_prop.R`
- Exports: `build_model_ancova_prop()` and `build_model_ancova_prop_2arms()` are `@export`. `create_ancova_prop_sim_fn()` and `simulate_data_ancova_prop_2arms_batch()` are `@keywords internal`.

### Out of Scope
- 3-arm proportional model (`ancova_prop_3arms`) -- can follow later
- Probability-scale output columns in `compute_measures()` -- deferred
- BayesFlow training for proportional model -- blocked on BF retraining
- Back-transformed summary statistics (e.g., median proportion)
- Beta-regression alternative (`brms::Beta()` family)

## Implementation Plan

### Phase 1: Core Model Implementation

**Files to modify:**

1. **`R/models_ancova.R`** -- Add after binary ANCOVA section (after line ~920):

   - `build_model_ancova_prop()` -- core builder (~250 lines, mirrors continuous structure with binary-style priors)
     - Parameters: `prior_intercept`, `prior_sigma`, `prior_covariate`, `prior_treatment`, `n_arms`, `contrasts`, `p_alloc`, `intercept`, `b_arm_treat`, `b_covariate`, `sigma`
     - All effect parameters (`intercept`, `b_arm_treat`, `b_covariate`, `sigma`) default to `NULL` in the core builder (required)
     - Sim_fn data generation (inline closure):
       ```
       eta = intercept + model.matrix(~arm)[,-1] %*% b_arm_treat + covariate * b_covariate
       outcome = eta + rnorm(n_total, 0, sigma)   # outcome is on logit scale
       ```
     - brms: `outcome ~ 1 + covariate + arm`, `family = gaussian()` (no `link_sigma` parameter -- not needed on logit scale, unlike the continuous model which supports it)
     - Priors including sigma
     - `model_name = "Proportional ANCOVA"`, `endpoint_types = "proportion"`

   - `build_model_ancova_prop_2arms()` -- wrapper (~20 lines, uses `function(...)` with `modifyList` pattern matching `build_model_ancova_bin_2arms`)
     - `default_args`: `prior_intercept=NULL, prior_sigma=NULL, prior_covariate=NULL, prior_treatment=NULL, n_arms=2, contrasts="contr.treatment", p_alloc=c(0.5,0.5), intercept=0, sigma=1, b_arm_treat=NULL, b_covariate=NULL`
     - `intercept=0` (logit scale) corresponds to 50% baseline proportion -- a reasonable default unlike binary where baseline rate is domain-specific
     - Calls `do.call(build_model_ancova_prop, final_args)`
     - Sets `@predefined_model = "ancova_prop_2arms"`
     - Returns `rctbp_model` object (same class as `build_model_ancova_bin()` / `build_model_ancova_cont()`)

   - `create_ancova_prop_sim_fn(n_arms)` -- sim_fn factory (~55 lines)
     - Closure defaults (matching binary/continuous pattern):
       - `default_n_arms <- n_arms` (concrete)
       - `default_contrasts <- "contr.treatment"` (concrete)
       - `default_p_alloc <- rep(1, n_arms) / n_arms` (concrete)
       - `default_intercept <- NULL` (required)
       - `default_b_arm_treat <- NULL` (required)
       - `default_b_covariate <- NULL` (required)
       - `default_sigma <- NULL` (required)
     - Function params use closure vars: `intercept = default_intercept`, `sigma = default_sigma`, etc. (not inline `NULL`)
     - `get_args_without_defaults()` returns: `intercept`, `b_arm_treat`, `b_covariate`, `sigma` (the 4 `NULL`-defaulted params). `n_arms`, `contrasts`, `p_alloc` have concrete defaults and are NOT required.
     - `test_args`: `list(n_total=20L, n_arms=n_arms, contrasts="contr.treatment", p_alloc=rep(1,n_arms)/n_arms, intercept=0, b_arm_treat=rep(0,n_arms-1), b_covariate=0, sigma=1)`
     - Returns `rctbp_sim_fn` via `build_sim_fn()`

   - `simulate_data_ancova_prop_2arms_batch()` -- batch simulator (~75 lines, `@keywords internal`)
     - Parameters: `n_sims, n_total, p_alloc=0.5, intercept=0, b_arm_treat=0, b_covariate=0, sigma=1`
     - Same structure as `simulate_data_ancova_cont_2arms_batch()` -- outcome is on logit scale
     - Returns: `list(outcome, covariate, group, N, p_alloc)` -- same fields as continuous batch function (sigma is a parameter, not a return field)
     - Note: batch function uses concrete defaults (not `NULL`) unlike the sim_fn factory -- this is intentional and matches the existing continuous/binary batch function pattern (internal BayesFlow interface)

2. **`R/class_design.R`** -- 4 registry updates + 1 validator fix (these are separate hardcoded data structures that must be kept in sync):
   - **Validator whitelist** (line ~153): add `"proportion"` to allowed `endpoint_types`: `c("continuous", "binary", "count", "proportion")`
   - `show_predefined_models()`: add `"ancova_prop_2arms"` to models vector
   - `load_predefined_model_components()`: add registry entry with all 5 fields:
     ```r
     ancova_prop_2arms = list(
       description = "2-arm proportional ANCOVA",
       n_endpoints = 1L,
       endpoint_types = "proportion",
       n_arms = 2L,
       n_repeated_measures = 0L
     )
     ```
   - `get_model_builder()`: add `ancova_prop_2arms = build_model_ancova_prop_2arms`
   - `create_sim_fn_for_model()`: add `else if (model_name == "ancova_prop_2arms") create_ancova_prop_sim_fn(n_arms = 2)`

**Files to create:**

3. **`tests/testthat/test-models_ancova_prop.R`** -- Test file (~220 lines), mirroring `test-models_ancova_bin.R`:

   - **Sim_fn validation**: rejects invalid n_total, n_arms, NULL b_arm_treat, NULL sigma
   - **Required parameters**: `get_args_without_defaults()` returns `intercept`, `b_arm_treat`, `b_covariate`, `sigma` (confirms `NULL` defaults are detected correctly)
   - **Output structure**: correct columns (covariate, arm, outcome), outcome is numeric and unbounded (on logit scale)
   - **Statistical properties**:
     - Null effect (b_arm_treat=0) -> similar means across arms
     - Positive b_arm_treat -> higher mean logit-outcome in treatment
     - sigma controls variability: `sd(outcome)` with sigma=0.3 << `sd(outcome)` with sigma=2
     - With small sigma and central intercept, back-transformed outcomes cluster near expected proportion (not trivially checking (0,1))
   - **brms compilation** (skip_on_ci): compiles, gaussian family, sigma IS present in `brms::variables()` (positive assertion — inverse of binary test which asserts absence), discovers fixed effects (b_Intercept, b_covariate, b_arm*)
   - **Predefined model**: listed in `show_predefined_models()`, filter "prop" works
   - **S7 object**: `create_ancova_prop_sim_fn()` returns `rctbp_sim_fn`
   - **Batch function**: correct dimensions, logit-scale outcomes, sigma wired correctly
   - **Wrapper required params**: calling `build_model_ancova_prop_2arms()` without `b_arm_treat` and `b_covariate` errors clearly
   - **Back-transform sanity**: `plogis(outcome)` values are all strictly in (0, 1) and finite

**Steps:**
1. Add section header and `build_model_ancova_prop()` to `R/models_ancova.R`
2. Add `build_model_ancova_prop_2arms()` wrapper
3. Add `create_ancova_prop_sim_fn()` factory
4. Add `simulate_data_ancova_prop_2arms_batch()` batch function
5. Update `endpoint_types` validator whitelist in `R/class_design.R` (add `"proportion"`)
6. Update 4 registry locations in `R/class_design.R`
7. Write tests in `tests/testthat/test-models_ancova_prop.R`
8. Run `devtools::document()` to update NAMESPACE
9. Run `devtools::test(filter = "ancova_prop")` to verify
10. Run `devtools::check()` for full package check

## Verification & Validation

- **Automated**: All tests in `test-models_ancova_prop.R` pass; R CMD check with 0 errors/warnings
- **Manual**: REPL smoke test:
  ```r
  model <- build_model_ancova_prop_2arms(
    intercept = qlogis(0.3),  # baseline 30% proportion
    b_arm_treat = 0.5,        # treatment effect on log-odds scale
    b_covariate = 0.2,
    sigma = 0.5
  )
  # Verify sim_fn produces logit-scale outcomes
  data <- model@sim_fn(
    n_total = 100,
    intercept = qlogis(0.3),
    b_arm_treat = 0.5,
    b_covariate = 0.2,
    sigma = 0.5
  )
  stopifnot(all(is.finite(data$outcome)))  # logit-scale, unbounded
  # Back-transformed outcomes should cluster near expected proportions
  props <- plogis(data$outcome)
  stopifnot(mean(props) > 0.1 && mean(props) < 0.9)  # reasonable range
  ```
- **Numerical**: With b_arm_treat=0 and large N, mean outcome difference between arms should be ~0. With large b_arm_treat (e.g., 2), treatment arm mean should be clearly higher on logit scale.
- **Pipeline test**: Full `build_design() |> build_conditions() |> power_analysis()` pipeline works with the proportional model (integration test).
- **`get_code()` compatibility**: Verify that `get_code()` correctly reconstructs the proportional model via `@predefined_model` when used through the `build_design()` path (which populates `@.call`). Direct `build_model_ancova_prop_2arms()` calls do not populate `@.call` -- this is consistent with all other model builders.

## Dependencies

- No new package dependencies -- uses existing brms, stats, S7 infrastructure
- Pattern follows binary model implementation (PR #20/#21) and continuous model structure (for sigma handling)

## Notes

_Living section -- updated during implementation._

- The model is mathematically equivalent to continuous ANCOVA -- the only differences are documentation, default priors, metadata, and parameter interpretation. This is by design: "Minimal new infrastructure -- reuses existing ANCOVA machinery."
- `compute_measures()` works unchanged since it operates on the treatment coefficient posterior, which is on the log-odds scale (same as binary model).
- Future enhancement: back-transformed proportion-scale summary statistics could be added to `compute_measures()` when `endpoint_types == "proportion"`.

## Review Feedback

**Reviewed by**: plan-reviewer agent (2026-03-11)

**Addressed issues:**
1. [CRITICAL] Sigma prior `normal(0, 10)` replaced with `normal(0, 0.5)` (half-normal, lb=0, SD=0.5). Bimodality threshold numerically verified at sigma ≈ 1.45. Prior puts 99.6% of mass below threshold. Prior table added with second-derivative verification.
2. [CRITICAL] Explicitly stated `NULL` defaults for `intercept`, `b_arm_treat`, `b_covariate`, `sigma` in sim_fn closure. Added `get_args_without_defaults()` test.
3. [IMPORTANT] Core builder `build_model_ancova_prop()` explicitly uses `sigma = NULL` (required). Wrapper sets `sigma = 1`.
4. [IMPORTANT] Added `get_args_without_defaults()` test to test plan.
5. [IMPORTANT] Smoke test now supplies all required parameters explicitly.
6. [IMPORTANT] Registry entry shows all 5 fields. Note added about hardcoded structures needing manual sync.
7. [MINOR] Replaced trivial `plogis()` boundary test with distributional test (clustering near expected proportion).
8. [MINOR] Batch function explicitly marked `@keywords internal`.
9. [MINOR] Intercept prior tightened to `normal(0, 2.5)` with clearer justification.

**Review 2** (2026-03-11):
10. [CRITICAL] `endpoint_types = "proportion"` added to S7 validator whitelist in `class_design.R` (line ~153). Without this, model construction would fail at runtime.
11. [IMPORTANT] Wrapper uses `function(...)` with `modifyList` pattern (matching `build_model_ancova_bin_2arms`), not named parameters.
12. [IMPORTANT] Return type explicitly stated: `rctbp_model` (same as continuous/binary builders).
13. [IMPORTANT] Batch function return fields explicitly listed: `list(outcome, covariate, group, N, p_alloc)`.
14. [IMPORTANT] Sim_fn closure params use `default_X` variables (not inline `NULL`), matching binary pattern.
15. [MINOR] `get_code()` verification scoped to `build_design()` path only.
16. [MINOR] Added tests: wrapper required-param error, `plogis(outcome)` strictly in (0,1).
17. [MINOR] Steps renumbered: validator whitelist update is now step 5 (separate from registry updates in step 6).

**Review 3** (2026-03-11):
18. [IMPORTANT] Sim_fn factory now lists all 7 closure defaults explicitly (n_arms, contrasts, p_alloc with concrete values; intercept, b_arm_treat, b_covariate, sigma with NULL). `contrasts` and `n_arms` were previously omitted. `test_args` now includes `contrasts`.
19. [IMPORTANT] Wrapper `default_args` now includes `prior_*=NULL` and `contrasts="contr.treatment"` — matching binary/continuous wrapper pattern.
20. [MINOR] brms compilation test explicitly asserts sigma IS present (positive assertion, inverse of binary test).
