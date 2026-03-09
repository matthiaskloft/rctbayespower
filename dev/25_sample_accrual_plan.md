# Sample Accrual Integration Plan

**Created:** 2026-03-02
**Status:** Planning
**Relates to:** [`23_api_roadmap.md`](23_api_roadmap.md) (Phase 2), [`21_adaptive_trials_roadmap.md`](21_adaptive_trials_roadmap.md) (survival outcomes), [`24_api_improvement_plan.md`](24_api_improvement_plan.md)

---

## 1. Problem

rctbayespower has no concept of calendar time or patient enrollment rates. Data simulation generates all `n_total` rows at once, and interim analyses subset by row index (`full_data[1:current_n, ]`). This means the package cannot answer:

- How long will the trial take?
- At an interim analysis at month 12, how many patients have completed follow-up?
- How does enrollment speed affect trial feasibility vs power trade-offs?

---

## 2. Design Decisions

### 2.1 No design-level flag — self-detection only

Every downstream consumer can detect accrual from parameter presence or column existence. No workflow is blocked without a `build_design()` flag:

| Consumer | Self-detection |
|---|---|
| `build_conditions()` | `!is.null(accrual_rate)` in params |
| `show_condition_args()` | Accrual shown as optional; survival sim_fns declare `accrual_rate` as formal (auto-detected by `get_args_without_defaults()`) |
| `worker_functions.R` | `!is.null(analysis_args$accrual_rate)` |
| `estimate_sequential_*()` | `"enrollment_time" %in% names(full_data)` |
| `compute_measures.R` | Check for accrual columns in results |

Future features also unblocked: survival sim_fns declare `accrual_rate` in formals. SSR/RAR receive condition_args with accrual params. Event-driven analysis uses `analysis_timing = "events"`.

### 2.2 Accrual parameters in `build_conditions()`, not `build_design()`

Accrual parameters are operational/simulation parameters users may want to vary across conditions. They follow the same pattern as `analysis_at`.

### 2.3 Enrollment times as post-processing

For non-survival endpoints, enrollment timing is orthogonal to outcome generation. Enrollment times are added after `do.call(sim_fn, ...)` as a column, keeping all existing sim_fn implementations unchanged.

For survival endpoints (future Phase 4), the sim_fn accepts `accrual_rate` directly because enrollment time affects censoring. In that case, `accrual_rate` routes to both `sim_args` and `analysis_args`.

### 2.4 No new S7 class

Accrual parameters fit into `crossed`/`constant` naturally.

### 2.5 Full backward compatibility

When `accrual_rate = NULL` (default), every code path is identical to current behavior.

---

## 3. New Parameters

Specified in `build_conditions()` via `crossed` or `constant`:

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `accrual_rate` | numeric | `NULL` | Patients per time unit. `NULL` = instant enrollment |
| `accrual_pattern` | character/function | `"uniform"` | `"uniform"`, `"poisson"`, `"ramp"`, or custom function |
| `followup_time` | numeric | `0` | Time from enrollment to outcome measurement |
| `analysis_timing` | character | `"sample_size"` | `"sample_size"` (current) or `"calendar"` |
| `calendar_analysis_at` | numeric vector | `NULL` | Calendar times for analyses (required when `analysis_timing = "calendar"`) |

---

## 4. Phased Implementation

### Phase 1 (MVP): Enrollment Times + Calendar-Time Subsetting

#### 4.1 New file: `R/accrual.R`

Core utility functions:

- **`generate_enrollment_times(n_total, accrual_rate, accrual_pattern)`** — Returns sorted numeric vector of enrollment times
  - `"uniform"`: constant inter-arrival time `1/accrual_rate`
  - `"poisson"`: exponential inter-arrival times (Poisson process)
  - `"ramp"`: rate starts slow, reaches full rate by mid-enrollment
  - Custom function: `f(n_total, accrual_rate) -> sorted numeric vector`

- **`patients_with_data(enrollment_times, followup_time, calendar_time)`** — Returns logical mask of patients with completed follow-up at given calendar time

- **`calendar_to_available_n(calendar_times, enrollment_times, followup_time)`** — Converts calendar times to available sample sizes

#### 4.2 Modify: `R/class_conditions.R`

- Add accrual parameters to recognized decision parameter defaults
- Validation:
  - `accrual_rate > 0` when provided
  - `followup_time >= 0`
  - `calendar_analysis_at` required when `analysis_timing = "calendar"`
  - `calendar_analysis_at` positive and increasing
- When `analysis_timing = "calendar"`: compute approximate `analysis_at` from expected enrollment curve for boundary pre-resolution

#### 4.3 Modify: `R/required_fn_args.R`

- Add accrual parameters to `show_condition_args()` under "Accrual (optional)" section

#### 4.4 Modify: `R/worker_functions.R`

- In `worker_process_single()`: after simulation, if `accrual_rate` present, generate enrollment times and add as `enrollment_time` column
- Same for `worker_process_batch()` (BayesFlow)
- Pass accrual parameters through to estimation functions

#### 4.5 Modify: `R/backend_brms.R`

- In `estimate_sequential_brms()`: change `full_data[1:current_n, ]` to accrual-aware subsetting:
  - `analysis_timing = "calendar"`: subset where `enrollment_time + followup_time <= calendar_time`
  - `analysis_timing = "sample_size"` with followup: compute calendar time when nth patient completes, then subset
  - Drop `enrollment_time` column before model fitting
- Add `n_enrolled` and `calendar_time` to per-look results

#### 4.6 Modify: `R/backend_bf.R`

- Same subsetting changes in `estimate_sequential_bf()` batch path

#### 4.7 Modify: `R/compute_measures.R`

- In `summarize_sims_with_interim()`: add accrual metrics:
  - `trial_duration_mn`, `trial_duration_mdn`: calendar time to final analysis
  - `enrollment_duration`: time to enroll all patients
  - Per-look: `n_enrolled` vs `n_analyzed` distinction

### Phase 2: Reporting & Plotting

- Display trial duration in print output when accrual is active
- New plot type `type = "accrual"`: enrollment curve with analysis timepoint markers
- Enhance interim plots with enrolled-vs-analyzed distinction

### Phase 3: Dropout/Loss-to-Follow-Up ✅ IMPLEMENTED

- `dropout()` constructor: `dropout(rate, type = "proportion"|"hazard")`
- Exponential (constant hazard) dropout model
- Per-patient dropout times generated after enrollment
- Dropped-out patients excluded from analysis data in `subset_analysis_data()`
- `n_dropped` metric carried through both backends
- Aggregated dropout metrics: `n_dropped_mn`, `n_dropped_mdn`, `dropout_pct`
- Display in print/summary output
- Requires `accrual_rate` and `followup_time > 0`
- Graceful handling when dropout is high (target_not_met attribute)

### Phase 4: Survival/Event-Driven Integration

- Connects with survival models (API roadmap Phase 3)
- Survival sim_fns accept `accrual_rate` directly (enrollment time affects censoring)
- **Dual routing:** `build_conditions()` routes `accrual_rate` to both `sim_args` AND `analysis_args` when param appears in sim_fn formals
- `analysis_timing = "events"`: analyze when event count reaches threshold

---

## 5. API Examples

### Fixed trial with accrual (operational planning)
```r
conditions <- build_conditions(
  design = design,
  crossed = list(n_total = c(100, 200)),
  constant = list(
    b_arm_treat = 0.3, intercept = 0, b_covariate = 0.2, sigma = 1,
    thr_dec_eff = 0.975, thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0,
    accrual_rate = 10,    # 10 patients/month
    followup_time = 6     # 6-month outcome
  )
)
# Power unchanged; result reports trial_duration:
# n=100: ~10mo enrollment + 6mo follow-up = ~16mo
# n=200: ~20mo enrollment + 6mo follow-up = ~26mo
```

### Group sequential with calendar-time interims
```r
conditions <- build_conditions(
  design = design,  # trial_type = "group_sequential"
  crossed = list(n_total = 200),
  constant = list(
    b_arm_treat = 0.3, intercept = 0, b_covariate = 0.2, sigma = 1,
    thr_dec_eff = boundary_obf(0.975), thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0,
    accrual_rate = 15,
    followup_time = 3,
    analysis_timing = "calendar",
    calendar_analysis_at = c(12, 18, 24)
  )
)
# Month 12: ~180 enrolled, ~135 with completed follow-up
# Month 18: 200 enrolled, ~200 with data
```

### Varying accrual rate (feasibility comparison)
```r
conditions <- build_conditions(
  design = design,
  crossed = list(
    n_total = 200,
    accrual_rate = c(5, 10, 20)
  ),
  constant = list(
    b_arm_treat = 0.3, intercept = 0, b_covariate = 0.2, sigma = 1,
    thr_dec_eff = 0.975, thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0,
    followup_time = 6,
    analysis_at = c(100, 200)
  )
)
# Same power, different durations: 46 vs 26 vs 16 months
```

---

## 6. Files to Modify/Create

| File | Action | Changes |
|------|--------|---------|
| `R/accrual.R` | **Create** | `generate_enrollment_times()`, `patients_with_data()`, `calendar_to_available_n()` |
| `R/class_conditions.R` | Modify | Accrual parameter defaults, validation, calendar-to-sample-size conversion |
| `R/required_fn_args.R` | Modify | Add accrual params to `show_condition_args()` |
| `R/worker_functions.R` | Modify | Post-process sim data with enrollment times; pass accrual params |
| `R/backend_brms.R` | Modify | Accrual-aware subsetting in `estimate_sequential_brms()` |
| `R/backend_bf.R` | Modify | Accrual-aware subsetting in `estimate_sequential_bf()` |
| `R/compute_measures.R` | Modify | Trial duration metrics in `summarize_sims_with_interim()` |

---

## 7. Verification

1. Unit tests for `generate_enrollment_times()`: verify uniform/poisson/ramp distributions
2. Unit tests for `patients_with_data()`: edge cases (followup=0, large followup)
3. Integration: fixed trial + accrual gives same power as without (outcomes unchanged)
4. Integration: group sequential + calendar timing produces correct n_analyzed < n_enrolled
5. Backward compatibility: all existing tests pass unchanged
6. Both backends: key tests with `backend = "bf"`
7. R CMD check: no new warnings or notes

---

## 8. Cross-References

- API roadmap Phase 2: [`23_api_roadmap.md`](23_api_roadmap.md)
- Survival accrual parameters: [`21_adaptive_trials_roadmap.md`](21_adaptive_trials_roadmap.md) (lines 323-408)
- Endpoint constructors: [`24_api_improvement_plan.md`](24_api_improvement_plan.md) (Section 3.4)
- Interim analysis infrastructure: [`20_interim_analysis_plan.md`](20_interim_analysis_plan.md)
