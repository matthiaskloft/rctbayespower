# API Improvement Plan

**Created:** 2026-02-21
**Status:** Planning
**Relates to:** [`23_api_roadmap.md`](23_api_roadmap.md)

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

**Priority:** Medium
**Effort:** Low per function (thin wrappers)

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

**Priority:** Low (Phase 3+ feature)
**Effort:** High (new statistical functionality)

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

| # | Item | Effort |
|---|------|--------|
| 1 | `as.data.frame()` methods for all core classes | Low |
| 2 | Pipe-friendly aliases (`add_conditions`, `run_power`) | Low |
| 3 | `available_plots()` function | Low |
| 4 | `set_output_format()` extension | Low |

### Phase B: Core Improvements (weeks)

| # | Item | Effort |
|---|------|--------|
| 5 | Parameter type tracking infrastructure | Medium |
| 6 | `get_code()` reproducibility (depends on #5) | Medium |
| 7 | `extract()` generic | Low-Medium |
| 8 | `quick_power()` / `quick_sample_size()` shortcuts | Medium |

### Phase C: Discoverability Layer (weeks)

| # | Item | Effort |
|---|------|--------|
| 9 | Endpoint-specific constructors (`design_continuous`, etc.) | Low each |
| 10 | Design comparison infrastructure (`compare_designs()`) | Medium |

### Phase D: Advanced Features (months)

| # | Item | Effort |
|---|------|--------|
| 11 | Performance score for Bayesian designs | Medium-High |
| 12 | Expanded analytical power functions | Medium each |
| 13 | Conditional power computation | High |

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

---

## 7. Cross-References

- API evolution roadmap: [`23_api_roadmap.md`](23_api_roadmap.md)
- Architecture: [`01_architecture.md`](01_architecture.md)
- Naming conventions: [`05_code_consistency_review.md`](05_code_consistency_review.md)
- Workflow: [`03_workflow.md`](03_workflow.md)
- Competitor analysis: [`40_competitor_analysis.md`](40_competitor_analysis.md)
