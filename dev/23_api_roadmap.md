# API Evolution Roadmap

**Created:** 2025-01-06
**Status:** Active Planning Document

## 1. Executive Summary

This document defines the strategic API evolution for `rctbayespower` to support comprehensive Bayesian power analysis for adaptive RCTs.

**Core Design Decision:** `trial_type` as the primary architectural selector in `build_design()`

**Implementation Phases:**
- Phase 1: Foundation (trial_type infrastructure) ← Current
- Phase 2: Core Adaptations (binary outcomes, RAR, SSR)
- Phase 3: Extended Features (survival, historical borrowing)
- Phase 4: Advanced (platform trials, dose-finding)

---

## 2. Major Design Decision: `trial_type`

### Rationale

- Replaces scattered boolean flags (`adaptive`) with single declarative selector
- Enables validation: each type has specific required/optional parameters
- Progressive disclosure: simple trials stay simple, complex trials declare complexity

### Trial Types (MVP)

| Type | Description | Required Params |
|------|-------------|-----------------|
| `fixed` | Single final analysis (default) | None |
| `group_sequential` | Multiple looks, stopping rules only | `analysis_at` |
| `adaptive` | Parameter modification between looks | `analysis_at`, adaptive-specific |

### Future Trial Types

| Type | Description | Target Phase |
|------|-------------|--------------|
| `platform` | Multi-arm with dynamic arm management | Phase 4 |
| `dose_finding` | Phase I dose escalation (CRM, BOIN) | Phase 4 |
| `enrichment` | Biomarker-driven subgroup selection | Phase 4 |

### Breaking Changes

- `adaptive = TRUE/FALSE` in `build_conditions()` → **REMOVED**
- Use `trial_type = "adaptive"` in `build_design()` instead

---

## 3. Feature-to-API Mapping

### Level 1: Extend Existing Patterns (Low Effort)

| Feature | API Addition | Location |
|---------|--------------|----------|
| Binary outcomes | New predefined models (`binary_2arms`) | model_registry |
| Count outcomes | New predefined models (`count_2arms`) | model_registry |
| RAR | `interim_rar()` | interim_functions.R |
| SSR | `interim_ssr()` | interim_functions.R |
| Thompson sampling | `interim_thompson()` | interim_functions.R |
| New boundaries | `boundary_hsd()`, `boundary_wang_tsiatis()` | boundaries.R |

### Level 2: New Properties/Parameters (Medium Effort)

| Feature | API Addition | Location |
|---------|--------------|----------|
| Sample accrual | `accrual_rate`, `followup_time`, `analysis_timing`, `calendar_analysis_at` | build_conditions() |
| Survival/events | `analysis_timing = "events"` | build_conditions() |
| Multi-endpoint | `endpoint_roles`, `decision_gate` | build_design() |
| Historical borrowing | `borrowing_method`, `borrowing_weight` | build_design() |
| Subgroups | `subgroup_var`, `subgroup_analysis` | build_design() |

### Level 3: New Components (High Effort)

| Feature | API Addition | Notes |
|---------|--------------|-------|
| Platform trials | `rctbp_arm_config` class | Dynamic arm management |
| Dose-finding | New model type | CRM/BOIN framework |
| Predictive probability | New compute function | Simulation-based |

---

## 4. Implementation Phases

### Phase 1: Foundation (Current)

- [x] Sequential designs with stopping rules
- [x] Boundary functions (OBF, Pocock, linear, power)
- [x] Boundary re-analysis without re-simulation
- [x] Bayesian optimization for design search
- [ ] **`trial_type` parameter** ← CURRENT FOCUS
- [ ] Remove `adaptive` boolean

### Phase 2: Core Adaptations

- [ ] Binary outcome models
- [ ] `interim_rar()` - Response-adaptive randomization
- [ ] `interim_ssr()` - Sample size re-estimation
- [ ] Allocation tracking in results
- [ ] Sample accrual: enrollment timing, calendar-time interims (see [`25_sample_accrual_plan.md`](25_sample_accrual_plan.md))

### Phase 3: Extended Features

- [ ] Survival / event-driven analysis (depends on sample accrual from Phase 2)
- [ ] Dropout / loss-to-follow-up modeling (extends sample accrual)
- [ ] Conditional power computation
- [ ] Historical data borrowing
- [ ] Multi-endpoint structure

### Phase 4: Advanced

- [ ] Platform trial framework
- [ ] Dose-finding designs
- [ ] Predictive probability engine
- [ ] Subgroup/enrichment designs

---

## 5. Design Principles

1. **Declarative trial_type**: Users declare intent, system validates compatibility
2. **Progressive disclosure**: `fixed` needs nothing; `adaptive` requires more
3. **Consistent patterns**: Same structure works for all outcome types
4. **Composability**: Features combine cleanly (boundaries + RAR + stopping)

---

## 6. API Examples by Trial Type

### Fixed Trial (Default)

```r
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2"
  # trial_type = "fixed" is default
)

conditions <- build_conditions(
  design = design,
  crossed = list(n_total = c(100, 200)),
  constant = list(
    thr_dec_eff = 0.975, thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0
    # No analysis_at needed
  )
)
```

### Group Sequential Trial

```r
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2",
  trial_type = "group_sequential"
)

conditions <- build_conditions(
  design = design,
  crossed = list(n_total = c(100, 200)),
  constant = list(
    analysis_at = c(0.5, 1.0),  # REQUIRED
    thr_dec_eff = boundary_obf(threshold = 0.95),
    thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0
  )
)
```

### Adaptive Trial (Future)

```r
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2",
  trial_type = "adaptive"
)

conditions <- build_conditions(
  design = design,
  crossed = list(n_total = c(100, 200)),
  constant = list(
    analysis_at = c(0.5, 1.0),
    interim_function = interim_rar(min_alloc = 0.2, max_alloc = 0.8),
    thr_dec_eff = 0.975, thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0
  )
)
```

### Group Sequential with Sample Accrual (Phase 2)

```r
design <- build_design(
  predefined_model = "ancova_cont_2arms",
  target_params = "b_arm2",
  trial_type = "group_sequential"
)

conditions <- build_conditions(
  design = design,
  crossed = list(n_total = 200),
  constant = list(
    thr_dec_eff = boundary_obf(0.975), thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0,
    accrual_rate = 15,                    # 15 patients/month
    followup_time = 3,                    # 3-month outcome
    analysis_timing = "calendar",         # Calendar-time interims
    calendar_analysis_at = c(12, 18, 24)  # Analyze at months 12, 18, 24
  )
)
```

---

## 7. Cross-References

- Interim/sequential details: `20_interim_analysis_plan.md`
- Outcome type roadmap: `21_adaptive_trials_roadmap.md`
- Bayesian design reference: `22_bayesian_adaptive_designs_reference.md`
- BayesFlow integration: `11_bayesflow_integration_roadmap.md`
- Sample accrual plan: `25_sample_accrual_plan.md`
