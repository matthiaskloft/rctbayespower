# API Improvement Plan

**Created:** 2026-02-21
**Updated:** 2026-02-22
**Status:** Planning
**Relates to:** [`23_api_roadmap.md`](23_api_roadmap.md), [`41_mediana_comparison.md`](41_mediana_comparison.md) (competitor deep-dive)

---

## 1. Overview

This document captures concrete API improvements for `rctbayespower` based on analysis of mature clinical trial design packages and general API design best practices. All recommendations are evaluated against our existing architecture and prioritized by impact and effort.

**Guiding principles:**
- Preserve our Bayesian/ROPE-based identity and S7 class system
- Improve discoverability and composability without breaking existing API
- Lower the barrier to entry for simple use cases while keeping power-user flexibility
- Additions should be thin wrappers or new functions, not rewrites

---

## 2. Current API Strengths (Preserve These)

| Strength | Why It Works |
|----------|-------------|
| snake_case naming | Modern R convention, consistent with tidyverse/S7 ecosystem |
| Flat S7 class hierarchy | Composition over inheritance; simpler maintenance |
| `rctbp_*` prefix convention | Clear namespace, avoids collisions |
| Varied verb prefixes (`build_*`, `show_*`, `check_*`) | More descriptive than a single generic verb |
| Immutable pipeline (`design` -> `conditions` -> `power_analysis`) | Explicit data flow, easy debugging |
| Custom models via `build_sim_fn()` | Flexibility beyond predefined endpoint types |
| `link()` for co-varying parameters | Elegant solution to a common pain point |
| `resummarize_boundaries()` | Avoids costly re-simulation |
| Dual backend architecture | Unique competitive advantage |

---

## 3. Improvement Areas

### 3.1 Reproducibility: `get_code()`

**Problem:** Users cannot regenerate the R code that produced a result object. This hurts preregistration, sharing, and debugging.

**Proposal:** Store `match.call()` metadata in each pipeline object and add a `get_code()` generic that reconstructs the full call chain.

```r
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2"
)
conditions <- build_conditions(design, crossed = list(n_total = c(100, 200)), ...)
result <- power_analysis(conditions, n_sims = 1000, n_cores = 4)

get_code(result)
# Prints the complete build_design() -> build_conditions() -> power_analysis() chain
# with all non-default arguments included
```

**Implementation notes:**
- Each constructor calls `match.call()` and stores it as a private property (e.g., `.call`)
- `get_code()` walks the chain: `result` -> `conditions` -> `design`
- Only non-default arguments are included in output
- Requires parameter type tracking (see 3.7)

**Priority:** High
**Effort:** Medium

---

### 3.2 Pipe-Friendly API Layer

**Problem:** The 3-step workflow requires intermediate variable assignment. It doesn't compose via `|>`, which is increasingly expected in modern R.

**Proposal:** Add thin aliases that enable piping without changing core functions:

```r
build_design(model_name = "ancova_cont_2arms", target_params = "b_arm2") |>
  add_conditions(
    crossed = list(n_total = c(100, 200)),
    constant = list(thr_dec_eff = 0.975)
  ) |>
  run_power(n_sims = 1000, n_cores = 4) |>
  summary()
```

| Alias | Wraps | Notes |
|-------|-------|-------|
| `add_conditions()` | `build_conditions()` | `build_conditions()` already takes `design` as first arg |
| `run_power()` | `power_analysis()` | Takes `conditions` as first arg, `run = TRUE` default |

**Implementation:** Pure aliases — `add_conditions <- build_conditions` plus documentation. No core logic changes.

**Priority:** Medium-High
**Effort:** Low

---

### 3.3 Quick Power Shortcuts

**Problem:** Even for simple fixed-design power calculations, users must go through a 3-step pipeline. This is a high barrier for quick exploratory work.

**Proposal:** Add convenience functions that wrap the full pipeline:

```r
# Quick power for a common design
quick_power(
  model = "ancova_cont_2arms",
  n_total = c(100, 150, 200),
  effect = 0.5,
  n_sims = 500
)

# Quick sample size search
quick_sample_size(
  model = "ancova_cont_2arms",
  effect = 0.5,
  target_power = 0.8,
  n_sims = 500
)
```

Internally, these construct the full `build_design()` -> `build_conditions()` -> `power_analysis()` chain with sensible defaults. Power users still use the explicit pipeline.

**Priority:** Medium-High
**Effort:** Medium

---

### 3.4 Endpoint-Specific Design Constructors

**Problem:** `build_design(model_name = "ancova_cont_2arms")` requires knowing model registry names. New users don't know what models exist or what parameters each expects.

**Proposal:** Add endpoint-type shortcut functions that wrap `build_design()`:

```r
# Convenience layer
design_continuous(arms = 2, target_params = "b_arm2")
design_binary(arms = 2, target_params = "b_arm2")        # Phase 2
design_survival(arms = 2, target_params = "b_arm2")      # Phase 3

# All equivalent to:
build_design(model_name = "ancova_cont_2arms", target_params = "b_arm2")
```

Each function has endpoint-aware parameter validation (e.g., `design_binary()` rejects `stDev`). `build_design()` remains the power-user entry point for custom models.

**Dependency:** These constructors require the underlying predefined models to exist. See 3.17 for the predefined endpoint model library that provides the actual simulation functions and brms formulas behind these convenience wrappers. The constructors in 3.4 are the discoverability layer; 3.17 is the implementation layer. `design_survival()` will use accrual parameters natively (see [`25_sample_accrual_plan.md`](25_sample_accrual_plan.md)).

**Priority:** Medium
**Effort:** Low per function (thin wrappers), but blocked on 3.17 for non-continuous endpoints

---

### 3.5 `as.data.frame()` Methods for All Core Classes

**Problem:** Not all S7 classes have clean data.frame conversion, making integration with tidyverse/ggplot workflows awkward.

**Proposal:** Register `as.data.frame()` S3 methods for every core class:

| Class | `as.data.frame()` returns |
|-------|---------------------------|
| `rctbp_design` | 1-row data.frame of design metadata |
| `rctbp_conditions` | The condition grid (`@grid`) |
| `rctbp_power_analysis` | The summary results (`@results_summ`) |
| `rctbp_pareto_result` | The Pareto front (`@pareto_front`) |

**Priority:** Medium
**Effort:** Low

---

### 3.6 Parameter Extraction Generic

**Problem:** Accessing results requires knowing S7 property names (`result@results_summ`, `conditions@grid`). This is less discoverable than named extraction.

**Proposal:** Add an `extract()` generic with tab-completable named components:

```r
extract(result, "power")        # result@results_summ
extract(result, "raw")          # result@results_raw
extract(result, "interim")      # result@results_interim
extract(conditions, "grid")     # conditions@grid
extract(design, "sim_params")   # design@par_names_sim
```

Falls back to `@` access but provides a more discoverable interface. Returns an error with valid component names if the user requests a non-existent component.

**Priority:** Medium
**Effort:** Low-Medium

---

### 3.7 Parameter Type Tracking

**Problem:** We can't distinguish user-provided parameters from defaults. This blocks `get_code()` (3.1) and smart `print()` output.

**Proposal:** Tag each parameter as `"user_defined"`, `"generated"`, or `"default"`:

```r
# Internal implementation sketch
design <- build_design(model_name = "ancova_cont_2arms", target_params = "b_arm2")
# Internally:
#   model_name = "ancova_cont_2arms"  -> user_defined
#   backend = "brms"                  -> default
#   trial_type = "fixed"              -> default
#   target_params = "b_arm2"          -> user_defined
```

**Benefits:**
- `print()` can show only user-modified parameters (compact output)
- `get_code()` can omit default parameters (cleaner reproducibility)
- Validation can distinguish "user explicitly set this" from "using default"

**Implementation:** Store a companion list or attribute alongside properties. Could use a simple `list(.param_types = list(model_name = "user", backend = "default", ...))`.

**Priority:** Medium
**Effort:** Medium

---

### 3.8 Design Comparison Infrastructure

**Problem:** No systematic way to compare multiple designs. Users must manually build and juxtapose results.

**Proposal:** Add `compare_designs()` with companion plot and summary methods:

```r
# Vary one parameter across a base design
comparison <- compare_designs(
  base = build_design(model_name = "ancova_cont_2arms", target_params = "b_arm2"),
  vary = list(backend = c("brms", "bf"))
)
plot(comparison)     # Side-by-side visualization
summary(comparison)  # Tabular comparison

# Or compare pre-built designs
comparison <- compare_designs(design1, design2, design3)
```

**Implementation:** New `rctbp_design_set` class with `plot()` and `summary()` methods. Tracks which parameters vary across the set.

**Priority:** Medium
**Effort:** Medium

---

### 3.9 Configurable Output Formatting

**Problem:** Our output system controls mode (`cli`/`markdown`) and verbosity, but not numeric formatting. Users preparing publications need control over decimal places, probability formatting, and zero trimming.

**Proposal:** Extend the output system with formatting options:

```r
# Global settings
set_output_format(
  digits = 3,
  probability_digits = 4,
  trim_zeros = TRUE,
  line_width = 80
)

# Per-call override
summary(result, digits = 4)
```

**Implementation:** Extend existing `output_system.R` with a formatting options registry. `report_*()` functions read from this registry.

**Priority:** Medium
**Effort:** Low

---

### 3.10 Plot Type Discovery

**Problem:** Our `plot()` dispatches based on result structure, but users don't know what plot types are available for a given object.

**Proposal:** Add `available_plots()` and named `type` argument:

```r
available_plots(result)
# [1] "power_curve"  "heatmap"  "comparison"  "boundary"  "stopping"

plot(result, type = "power_curve")
plot(result, type = "heatmap")
plot(result, type = "boundary")   # Only for sequential designs
```

Use named strings (not integers) for readability.

**Priority:** Low-Medium
**Effort:** Low (reorganize existing plot dispatch)

---

### 3.11 Design Performance Score

**Problem:** No standardized metric for objectively comparing design quality. Our Pareto optimization handles multi-objective trade-offs but doesn't produce a single quality index.

**Proposal:** Add `performance_score()` with Bayesian-specific sub-scores:

```r
performance_score(result)
# Returns data.frame:
#   efficiency: 0.82   (expected N / max N)
#   precision:  0.91   (average posterior width relative to prior)
#   accuracy:   0.88   (rate of correct ROPE-based decisions)
#   overall:    0.87   (weighted composite)
```

**Priority:** Low
**Effort:** Medium-High (careful statistical definition needed)

---

### 3.12 Expanded Analytical Power Functions

**Problem:** We have `analytical_power_ancova_cont_2arms()` but lack closed-form power for other endpoint types. These are useful as fast cross-checks against simulation results.

**Proposal:** Add analytical power functions as new endpoint types are implemented:

```r
analytical_power_binary_2arms(n, p1, p2, alpha = 0.025)
analytical_power_survival_2arms(n_events, hazard_ratio, alpha = 0.025)
analytical_power_count_2arms(n, lambda1, lambda2, alpha = 0.025)
```

**Priority:** Low
**Effort:** Medium per function

---

### 3.13 Conditional Power Computation

**Problem:** No function to compute conditional power given observed interim data. This limits the package to trial planning; extending to trial monitoring would increase value.

**Proposal:**

```r
conditional_power(
  result,             # Power analysis result (for reference design)
  interim_data,       # Observed data at interim look
  remaining_n = 100   # Planned remaining sample size
)
```

**Note:** Conditional power projections benefit from sample accrual parameters (`accrual_rate`, `followup_time`) to estimate remaining enrollment time. See [`25_sample_accrual_plan.md`](25_sample_accrual_plan.md).

**Priority:** Low (Phase 3+ feature)
**Effort:** High (new statistical functionality)

---

### 3.14 Decision Criteria Separation

**Problem:** Decision thresholds (`thr_dec_eff`, `thr_fx_eff`, `thr_dec_fut`, `thr_fx_fut`) are mixed into the conditions grid alongside data-generation parameters (`n_total`, `b_arm_treat`, `sigma`). This conflates two conceptually distinct concerns — "how to simulate data" and "how to make decisions about the data" — making it harder to reason about each independently.

The existing `resummarize_boundaries()` function already demonstrates the value of treating decision criteria as separable from the simulation specification: it allows boundary function changes without re-running simulations. This principle should be generalized.

**Proposal:** Introduce a lightweight `decision_criteria()` helper that groups all decision-related parameters:

```r
criteria <- decision_criteria(
  thr_dec_eff = 0.975,
  thr_dec_fut = boundary_obf(),
  thr_fx_eff = 0.2,
  thr_fx_fut = 0
)

conditions <- build_conditions(
  design = design,
  crossed = list(n_total = c(50, 100, 200)),
  constant = list(intercept = 0, sigma = 1, b_arm_treat = 0.3),
  decision = criteria
)

# Decision criteria can also vary across conditions:
conditions <- build_conditions(
  design = design,
  crossed = list(
    n_total = c(100, 200),
    decision = list(
      decision_criteria(thr_dec_eff = 0.95, thr_fx_eff = 0.1),
      decision_criteria(thr_dec_eff = 0.975, thr_fx_eff = 0.2)
    )
  ),
  constant = list(intercept = 0, sigma = 1, b_arm_treat = 0.3)
)
```

**Benefits:**
- Clearer separation of simulation parameters from decision parameters
- Enables swapping decision strategies without touching simulation specs
- Self-documenting: the intent of each parameter group is explicit
- Generalizes the `resummarize_boundaries()` pattern to all decision criteria

**Backward compatibility:** The existing flat-list approach in `crossed`/`constant` continues to work. `decision_criteria()` is additive sugar.

**Priority:** Medium
**Effort:** Medium

---

### 3.15 Cached Simulation Re-Analysis

**Problem:** Changing any decision threshold (not just boundaries) requires a full re-simulation. For large studies with `n_sims = 10000` and expensive MCMC, this wastes significant computation. The `resummarize_boundaries()` function already handles the specific case of changing boundary functions; this should be generalized.

**Proposal:** Store raw posterior summaries (or full posterior samples, configurable) from each simulation run and provide a `reanalyze()` function that re-applies decision criteria without re-simulation:

```r
# Original run
result <- power_analysis(conditions, n_sims = 1000, run = TRUE,
                         store_posteriors = TRUE)  # Default: store summaries only

# Fast re-analysis with different ROPE thresholds
result2 <- reanalyze(result,
  thr_dec_eff = 0.99,   # Stricter decision threshold
  thr_fx_eff = 0.3      # Wider ROPE
)

# Re-analysis with different boundary functions
result3 <- reanalyze(result,
  thr_dec_eff = boundary_pocock(),  # Switch from OBF to Pocock
  thr_dec_fut = boundary_linear()
)
```

**Implementation notes:**
- Currently, `results_raw` stores per-simulation decision outcomes. Extend it to optionally store the posterior probability vectors that feed into the decision logic.
- `reanalyze()` re-runs only the decision step of `compute_measures()`, skipping data generation and model fitting.
- `resummarize_boundaries()` becomes a special case of `reanalyze()` and could be implemented as a wrapper for backward compatibility.
- Memory trade-off: storing posterior summaries per simulation adds ~50-100 bytes per sim per condition. For 10,000 sims x 20 conditions, this is ~10-20 MB. Full posterior samples would be much larger and should be opt-in.

**Priority:** Medium
**Effort:** Medium

---

### 3.16 Structured Report Configuration

**Problem:** The current `report()` system generates topic-specific console output, and `export_report()` exports to files. However, there is no way to configure report structure — section ordering, custom labels for conditions, header metadata for regulatory context, or multi-format export (HTML, PDF, Word).

Structured, reproducible reports are essential for regulatory submissions and preregistration. Mature trial design frameworks treat report configuration as a first-class concern separate from result computation.

**Proposal:** Add a report configuration layer:

```r
config <- report_config(
  title = "Phase III Power Analysis: Study XYZ-001",
  author = "Study Statistician",
  date = Sys.Date(),
  sections = c("design", "conditions", "power", "early_stopping", "diagnostics"),
  organize_by = "n_total",
  custom_labels = list(
    n_total = c("Small (N=50)", "Medium (N=100)", "Large (N=200)"),
    b_arm_treat = c("Conservative", "Expected", "Optimistic")
  ),
  format = "html"  # or "pdf", "docx"
)

export_report(result, config = config, filename = "power_report")
```

**Key design decisions:**
- `report_config()` returns a simple S7 class (or named list) — no `+` operator composition needed
- `sections` determines which report topics appear, in what order
- `organize_by` controls how results are grouped (analogous to table/section organization in report templates)
- `custom_labels` maps parameter values to human-readable labels for tables and headers
- `format = "docx"` would require `officer`/`flextable` as suggested dependencies

**Benefits:**
- Reproducible report structure that can be versioned alongside analysis code
- Regulatory-ready Word/PDF output for submission packages
- Custom labels eliminate cryptic parameter names in reports
- Ties into `get_code()` (3.1): report configuration is part of the reproducible analysis chain

**Priority:** Medium
**Effort:** Medium-High (format backends add complexity)

---

### 3.17 Predefined Endpoint Model Library

**Problem:** The package currently ships only with ANCOVA models for continuous endpoints (`ancova_cont_2arms`, `ancova_cont_3arms`). Binary, count, survival, and multivariate endpoints require users to write custom `build_sim_fn()` implementations and provide their own brms inference models. This is a significant barrier — most trial design packages provide a library of endpoint types out of the box.

This goes beyond 3.4 (endpoint-specific constructors), which proposes convenience wrappers. Here the need is for actual model implementations: simulation functions + brms formulas + compiled Stan models for each endpoint type.

**Proposal:** Expand the predefined model registry with the most common additional endpoint types:

| Model name | Endpoint | Simulation | Inference |
|-----------|----------|------------|-----------|
| `logistic_2arms` | Binary (proportion) | Bernoulli draw per arm | `brms::brm(y ~ arm, family = bernoulli())` |
| `logistic_3arms` | Binary, 3 arms | Bernoulli, 3 arms | `brms::brm(y ~ arm, family = bernoulli())` |
| `ancova_binary_2arms` | Binary + baseline covariate | Correlated binary outcome | `brms::brm(y ~ arm + baseline, family = bernoulli())` |
| `poisson_2arms` | Count | Poisson draw per arm | `brms::brm(y ~ arm, family = poisson())` |
| `negbinom_2arms` | Count (overdispersed) | NegBin draw per arm | `brms::brm(y ~ arm, family = negbinomial())` |

**Implementation per model:**
1. Write `build_model_<name>()` function (simulation fn + brms formula)
2. Pre-compile and cache the brms/Stan model
3. Add to `show_predefined_models()` registry
4. Add `analytical_power_<name>()` companion where closed-form solutions exist
5. Create corresponding BayesFlow architecture if applicable

**Priority:** High
**Effort:** High (each model requires sim fn + brms formula + compiled model + tests + documentation)

**Phasing:**
- Phase 1: `logistic_2arms` (most requested beyond continuous)
- Phase 2: `poisson_2arms`, `negbinom_2arms`
- Phase 3: `ancova_binary_2arms`, multivariate combinations

---

### 3.18 Multi-Parameter Decision Rules

**Problem:** The current decision framework evaluates a single `target_params` value against ROPE thresholds. However, many trials have co-primary endpoints or multiple treatment arms where the success decision depends on the joint posterior over multiple parameters.

Common multi-parameter decision scenarios:
- **Co-primary endpoints**: Both endpoints must show efficacy (conjunctive)
- **At-least-one success**: Any endpoint showing efficacy suffices (disjunctive)
- **Hierarchical testing**: Primary must succeed before evaluating secondary
- **Dose selection**: Best among multiple treatment arms must exceed control

**Proposal:** Extend `target_params` and the decision logic to support multi-parameter rules:

```r
# Co-primary endpoints (conjunctive: both must succeed)
design <- build_design(
  predefined_model = "ancova_cont_2arms_bivariate",  # Future model
  target_params = c("b_arm2_endpoint1", "b_arm2_endpoint2"),
  decision_rule = "conjunctive"  # "disjunctive", "hierarchical", or custom fn
)

# Custom decision function
design <- build_design(
  predefined_model = "ancova_cont_3arms",
  target_params = c("b_arm2", "b_arm3"),
  decision_rule = function(posteriors, thresholds) {
    # At least one arm must exceed ROPE
    any(posteriors > thresholds$thr_fx_eff)
  }
)
```

**Power metrics become richer:**
- Conjunctive power: P(all target_params succeed)
- Disjunctive power: P(at least one target_param succeeds)
- Per-parameter power: P(each target_param succeeds individually)
- Weighted power: Weighted combination of per-parameter powers

**Implementation notes:**
- `compute_measures()` already processes target_params in a loop; extend to joint evaluation
- Store per-parameter decisions in `results_raw` alongside the joint decision
- `report_power()` should show both per-parameter and joint power when multi-parameter rules are used

**Priority:** Medium
**Effort:** High

---

### 3.19 Named Specification Labels

**Problem:** Pipeline objects are identified by class and position, not by user-assigned labels. When comparing multiple analyses or generating reports, there's no built-in way to attach descriptive names to designs, conditions, or analyses.

**Proposal:** Add optional `label` parameters to key constructors:

```r
design <- build_design(
  predefined_model = "ancova_cont_2arms",
  target_params = "b_arm2",
  label = "Two-arm continuous ANCOVA"
)

conditions <- build_conditions(
  design = design,
  crossed = list(n_total = c(100, 200)),
  constant = list(...),
  label = "Sample size exploration"
)

result <- power_analysis(
  conditions = conditions,
  n_sims = 1000,
  label = "Main analysis (1000 sims)"
)
```

**Where labels appear:**
- `print()` and `summary()` output headers
- Plot titles and legends (especially in `compare_designs()`)
- Report section headers
- `get_code()` output as comments

**Implementation:** Single `label` property on each core S7 class, defaulting to `NULL`. All display functions check for label and use it if present.

**Priority:** Low
**Effort:** Low

---

## 4. Documentation Improvements

### 4.1 Vignette Structure

Organize vignettes by workflow phase and complexity:

1. **Getting Started** — Fixed trial, single continuous endpoint
2. **Sequential Designs** — Boundaries, stopping rules, `resummarize_boundaries()`
3. **Adaptive Designs** — RAR, SSR, interim functions
4. **Bayesian Optimization** — Pareto, sample size search, effect size search
5. **BayesFlow Backend** — Setup, GPU acceleration, performance tips
6. **Custom Models** — `build_sim_fn()`, custom inference models

### 4.2 Interactive Companion (Long-Term)

Consider a Shiny app that lets users:
- Select model type via dropdown
- Configure conditions with sliders
- View power curves in real-time
- Export configurations as R code (ties into `get_code()`)

---

## 5. Implementation Roadmap

### Phase A: Quick Wins (days)

| # | Item | Section | Effort |
|---|------|---------|--------|
| 1 | `as.data.frame()` methods for all core classes | 3.5 | Low |
| 2 | Pipe-friendly aliases (`add_conditions`, `run_power`) | 3.2 | Low |
| 3 | `available_plots()` function | 3.10 | Low |
| 4 | `set_output_format()` extension | 3.9 | Low |
| 5 | Named specification labels (`label` on core classes) | 3.19 | Low |

### Phase B: Core Improvements (weeks)

| # | Item | Section | Effort |
|---|------|---------|--------|
| 6 | Parameter type tracking infrastructure | 3.7 | Medium |
| 7 | `get_code()` reproducibility (depends on #6) | 3.1 | Medium |
| 8 | `extract()` generic | 3.6 | Low-Medium |
| 9 | `quick_power()` / `quick_sample_size()` shortcuts | 3.3 | Medium |
| 10 | `decision_criteria()` helper + `build_conditions()` integration | 3.14 | Medium |
| 11 | Cached simulation re-analysis via `reanalyze()` | 3.15 | Medium |

### Phase C: Discoverability & Reporting (weeks)

| # | Item | Section | Effort |
|---|------|---------|--------|
| 12 | Endpoint-specific constructors (`design_continuous`, etc.) | 3.4 | Low each |
| 13 | Design comparison infrastructure (`compare_designs()`) | 3.8 | Medium |
| 14 | Structured report configuration (`report_config()`) | 3.16 | Medium-High |

### Phase D: Endpoint Expansion (months)

| # | Item | Section | Effort |
|---|------|---------|--------|
| 15 | Predefined binary endpoint models (`logistic_2arms`) | 3.17 | High |
| 16 | Predefined count endpoint models (`poisson_2arms`, `negbinom_2arms`) | 3.17 | High |
| 17 | Predefined binary ANCOVA model (`ancova_binary_2arms`) | 3.17 | High |
| 18 | Expanded analytical power functions | 3.12 | Medium each |

### Phase E: Advanced Features (months)

| # | Item | Section | Effort |
|---|------|---------|--------|
| 19 | Multi-parameter decision rules (co-primary endpoints) | 3.18 | High |
| 20 | Performance score for Bayesian designs | 3.11 | Medium-High |
| 21 | Conditional power computation | 3.13 | High |

---

## 6. Non-Goals

Things we explicitly will **not** change:

| Current Pattern | Why We Keep It |
|----------------|----------------|
| snake_case naming | Modern R standard, consistent with S7/tidyverse |
| `rctbp_*` class prefix | Clear namespace, industry standard for package prefixes |
| Flat S7 class hierarchy | Simpler than deep inheritance trees |
| Varied verb prefixes | More descriptive than a single generic verb for all functions |
| Simulation-based power as primary | More flexible than analytical-only approaches |
| ROPE-based decision framework | Core differentiator of the package |
| `build_design()` as power-user entry | Must remain; convenience functions are additive |
| Constructor-based (not `+` operator) model building | Our pipeline wraps previous step; `+` composition suits independent-component frameworks (like ggplot2 layers) but not our progressive pipeline where each step enriches the previous |
| Single-paradigm (Bayesian) inference | No frequentist tests or p-value-based decisions; ROPE-based posteriors are our differentiator |

---

## 7. Cross-References

- API evolution roadmap: [`23_api_roadmap.md`](23_api_roadmap.md)
- Sample accrual plan: [`25_sample_accrual_plan.md`](25_sample_accrual_plan.md)
- Architecture: [`01_architecture.md`](01_architecture.md)
- Naming conventions: [`05_code_consistency_review.md`](05_code_consistency_review.md)
- Workflow: [`03_workflow.md`](03_workflow.md)
- Competitor analysis: [`40_competitor_analysis.md`](40_competitor_analysis.md)
- Competitor deep-dive (CSE framework): [`41_mediana_comparison.md`](41_mediana_comparison.md)
