# Adaptive Trials Implementation Roadmap
**Date:** 2025-01-06
**Purpose:** Actionable roadmap to enable adaptive trial simulations

## Current State Summary

The `rctbayespower` package currently supports:
- ✅ Fixed sample size Bayesian power analysis
- ✅ Continuous outcomes (ANCOVA models)
- ✅ ROPE-based decisions
- ✅ Efficient parallel processing
- ✅ **Group sequential designs** (interim stopping)
- ✅ **Look-dependent boundaries** (OBF, Pocock, linear, power)
- ✅ **Boundary re-analysis** without re-simulation
- ✅ Bayesian optimization for design search

The package currently LACKS:
- ❌ Binary, count outcomes
- ❌ Adaptive parameter modification (RAR, SSR) - infrastructure exists
- ❌ Dose finding
- ❌ Platform/basket/umbrella designs

Recently completed:
- ✅ **Sample accrual / calendar time** (Phases 1-4 complete, see [`25_sample_accrual_plan.md`](25_sample_accrual_plan.md))
- ✅ **Dropout / loss-to-follow-up**
- ✅ **Survival event-driven subsetting** (`subset_by_events()`)
- ✅ **Survival simulation function** (`R/models_survival.R`)

## Practical Use Cases We Should Target

Based on industry analysis, these are the most valuable adaptive trial scenarios to support:

### Use Case 1: Oncology Trial with Interim Futility
**Scenario:** Phase II/III cancer trial with binary outcome (overall response rate)
- N=200 patients planned
- Interim analysis at n=100
- Stop for futility if P(ORR < 30%) > 0.90
- Primary endpoint: ORR > 40%

**Required features:**
- Binary outcome model ❌
- Interim analysis ✅
- Futility stopping rules ✅

### Use Case 2: Rare Disease with Sample Size Re-estimation
**Scenario:** Rare disease trial with continuous outcome
- Initial n=50 planned
- Interim at n=25 to re-estimate variance
- Increase sample size up to n=100 if needed for power
- ROPE: [-0.3, 0.3] for standardized effect

**Required features:**
- Continuous outcome model ✅
- Interim analysis ✅
- Sample size re-estimation ❌
- Conditional power calculation ❌

### Use Case 3: COVID-19 Platform Trial
**Scenario:** Multiple treatments vs. control with 28-day mortality
- Start with 3 treatments + control
- Drop treatments with P(benefit) < 0.10 at interim
- Add new treatments as they become available
- Binary outcome (28-day mortality)

**Required features:**
- Binary outcome ❌
- Multiple arms ✅ (3-arm model exists)
- Arm dropping ❌
- Dynamic arm addition ❌

### Use Case 4: Dose-Finding in Pediatrics
**Scenario:** Find optimal dose balancing efficacy and safety
- 4 dose levels
- Efficacy: continuous biomarker
- Safety: binary toxicity outcome
- Adaptive dose allocation based on efficacy/safety trade-off

**Required features:**
- Multiple dose levels ❌
- Mixed outcomes (continuous + binary) ❌
- Dose-response modeling ❌
- Adaptive allocation ❌

### Use Case 5: Time-to-Event with Early Efficacy
**Scenario:** Cardiovascular trial with time to MACE
- N=500 planned
- Interim analyses at 150, 300 events
- Stop early if HR < 0.7 with high probability
- Primary: Time to first MACE

**Required features:**
- Sample accrual / calendar time ❌ (prerequisite, see [`25_sample_accrual_plan.md`](25_sample_accrual_plan.md))
- Survival outcomes ❌
- Event-driven interims ❌
- Hazard ratio calculations ❌

## Implementation Priorities

### 🔴 Priority 1: Enable Group Sequential Designs (Most Common)
**Timeline: 4 weeks** → **STATUS: Mostly Complete**

#### Week 1-2: Core Interim Analysis
- [x] Implement interim analysis infrastructure per existing plan
- [x] Add analysis_at property to rctbp_design
- [x] Refactor simulate_single_run() for sequential data
- [x] Create interim decision functions
- [x] Test with continuous ANCOVA model

#### Week 3: Binary Outcomes
- [ ] Create build_model_binary_2arms()
- [ ] Add logistic regression via brms
- [ ] Binary data generation
- [ ] Adapt compute_measures for proportions
- [ ] Test binary model with interim analysis

#### Week 4: Integration and Testing
- [x] Full workflow testing
- [x] Documentation and examples
- [x] Performance optimization
- [x] Create vignette for group sequential

**Deliverable:** ~~Can simulate standard group sequential trials with continuous or binary outcomes~~ **ACHIEVED for continuous outcomes** - binary outcomes remain TODO

### 🟡 Priority 2: Enable Advanced Adaptations
**Timeline: 3 weeks**

#### Week 5: Sample Size Re-estimation
- [ ] Add conditional power calculations
- [ ] Implement promising zone design
- [ ] Blinded variance re-estimation
- [ ] Modify n_total during simulation
- [ ] Test SSR with both outcome types

#### Week 6: Survival Outcomes
- [ ] Create build_model_survival_2arms()
- [ ] Exponential/Weibull data generation
- [ ] Censoring mechanisms
- [ ] Cox regression via brms
- [ ] Hazard ratio calculations

#### Week 7: Response-Adaptive Randomization
- [ ] Extend adaptive parameter modification
- [ ] Thompson sampling implementation
- [ ] Doubly adaptive biased coin
- [ ] Drift protection mechanisms
- [ ] Test RAR with all outcome types

**Deliverable:** Can simulate trials with SSR, survival outcomes, and adaptive randomization

### 🟢 Priority 3: Platform Trial Features
**Timeline: 4 weeks**

#### Week 8-9: Multi-Arm Infrastructure
- [ ] Dynamic arm management
- [ ] Shared control allocation
- [ ] Arm-specific stopping rules
- [ ] Between-arm comparisons

#### Week 10-11: Platform Trial Logic
- [ ] Perpetual trial framework
- [ ] Staggered entry of arms
- [ ] Common data model updates
- [ ] Borrowing across periods

**Deliverable:** Can simulate platform trials with multiple arms entering/leaving

## Minimum Viable Product Definition

To be useful for real adaptive trial planning, the package MUST have:

1. **Interim analysis** ✅ **COMPLETE**
   - At least futility and efficacy stopping
   - ~~Works with all outcome types~~ Works with continuous outcomes

2. **Binary outcomes** ❌ **TODO**
   - Most Phase II/III trials use binary endpoints
   - Response rates, mortality, success/failure

3. **Sample size re-estimation** ❌ **TODO** (after Priority 2)
   - Address uncertainty in planning assumptions
   - Required by many regulatory agencies

4. **Basic reporting** ✅ **COMPLETE**
   - Probability of early stopping
   - Expected sample size
   - Operating characteristics

## Code Examples for Target Functionality

### Example 1: Binary Outcome with Futility Stopping
```r
# After implementation, this should work:
model_binary <- build_model("binary_2arms")

design <- build_design(
  model = model_binary,
  target_params = "b_treatment",
  p_sig_scs = 0.975,
  p_sig_ftl = 0.90,
  analysis_at = c(50, 100),  # Interim at 50, 100
  interim_function = interim_futility_only(0.90)
)

conditions <- build_conditions(
  design = design,
  condition_values = list(
    n_total = 150,
    true_response_rate_control = c(0.3),
    true_response_rate_treatment = c(0.3, 0.45, 0.6)
  )
)

results <- power_analysis(conditions, n_sims = 1000)

# Should show:
# - Power curve across effect sizes
# - Probability of stopping for futility
# - Expected sample size
```

### Example 2: Sample Size Re-estimation
```r
design <- build_design(
  model = model_ancova,
  target_params = "b_arm2",
  p_sig_scs = 0.975,
  analysis_at = 50,  # Check variance at n=50
  interim_function = ssr_variance_based(
    max_n = 200,
    target_power = 0.80
  )
)

# Should automatically:
# - Re-estimate variance at interim
# - Calculate new sample size for target power
# - Continue to new sample size
# - Report actual vs. planned sample size
```

### Example 3: Response-Adaptive Randomization
```r
design <- build_design(
  model = model_binary,
  target_params = "b_treatment",
  analysis_at = seq(20, 180, by = 20),
  adaptive = TRUE,
  interim_function = rar_thompson_sampling()
)

# Should show:
# - Allocation probabilities over time
# - More patients on better arm
# - Power comparison vs. fixed randomization
```

## API Design for Planned Features

This section specifies the API design for features that are planned but not yet implemented.

### Binary Outcomes

#### Model Registry Entry
```r
# In models_ancova.R or new models_binary.R
predefined_models$binary_2arms <- list(
  model_type = "binary",
  backend = "brms",
  formula = "y ~ 0 + intercept + arm",
  family = "bernoulli(link='logit')",
  fn_sim = sim_binary_2arms,
  fn_design_prior = design_prior_binary_2arms,
  par_names_inference = c("b_intercept", "b_arm2"),
  par_names_sim = c("n_total", "p_alloc", "true_prob_control", "true_prob_treatment")
)
```

#### Simulation Function Schema
```r
#' @title Simulate Binary 2-Arm Data
#' @param n_total Total sample size
#' @param p_alloc Allocation probabilities (length 2)
#' @param true_prob_control True response probability in control
#' @param true_prob_treatment True response probability in treatment
#' @param ... Additional parameters
#' @return data.frame with columns: y (binary 0/1), arm (factor)
sim_binary_2arms <- function(n_total, p_alloc, true_prob_control,
                              true_prob_treatment, ...) {
  # Generate arm assignments
  n_per_arm <- rmultinom(1, n_total, p_alloc)[, 1]
  arm <- rep(c("arm1", "arm2"), times = n_per_arm)

  # Generate binary outcomes
  y <- numeric(n_total)
  y[arm == "arm1"] <- rbinom(n_per_arm[1], 1, true_prob_control)
  y[arm == "arm2"] <- rbinom(n_per_arm[2], 1, true_prob_treatment)

  data.frame(
    y = y,
    arm = factor(arm, levels = c("arm1", "arm2")),
    intercept = 1
  )
}
```

#### Decision Parameters
For binary outcomes, ROPE thresholds can be specified on:
- **Probability scale**: `thr_fx_eff = 0.10` means 10% absolute risk difference
- **Odds ratio scale**: `thr_fx_eff_or = 1.5` means OR > 1.5
- **Log-odds scale**: `thr_fx_eff = 0.405` means log(OR) > 0.405 (OR > 1.5)

```r
# Example usage
conditions <- build_conditions(
  design = design,
  crossed = list(n_total = c(100, 200)),
  constant = list(
    true_prob_control = 0.30,
    true_prob_treatment = 0.45,
    p_alloc = list(c(0.5, 0.5)),
    # Decision on probability difference scale
    thr_dec_eff = 0.975,
    thr_dec_fut = 0.5,
    thr_fx_eff = 0.10,   # 10% absolute difference
    thr_fx_fut = 0
  )
)
```

### Survival Outcomes

> **Dependency:** Survival outcomes require sample accrual infrastructure (enrollment times, calendar-time subsetting). See [`25_sample_accrual_plan.md`](25_sample_accrual_plan.md) Phase 4 for the dual-routing design where `accrual_rate` routes to both `sim_args` and `analysis_args`.

#### Model Registry Entry
```r
predefined_models$survival_2arms <- list(
  model_type = "survival",
  backend = "brms",
  formula = "y | cens(censored) ~ 0 + intercept + arm",
  family = "weibull",  # or "exponential", "cox"
  fn_sim = sim_survival_2arms,
  fn_design_prior = design_prior_survival_2arms,
  par_names_inference = c("b_intercept", "b_arm2"),
  par_names_sim = c("n_total", "p_alloc", "lambda_control", "lambda_treatment",
                     "shape", "accrual_rate", "followup_time")
)
```

#### Event-Driven Analysis Timing
For survival outcomes, interim analyses are often event-driven rather than calendar-driven:

```r
# Specify interim analyses by number of events
conditions <- build_conditions(
  design = design,
  crossed = list(
    link(
      n_total = c(200, 400),
      analysis_at = list(c(100, 200), c(200, 400))  # Number of events
    )
  ),
  constant = list(
    analysis_timing = "events",  # "events" or "calendar"
    lambda_control = 0.05,       # Control hazard rate
    lambda_treatment = 0.03,     # Treatment hazard rate
    shape = 1,                   # Weibull shape (1 = exponential)
    accrual_rate = 10,           # Patients per month
    followup_time = 12,          # Maximum followup (months)
    p_alloc = list(c(0.5, 0.5)),
    thr_dec_eff = 0.975,
    thr_dec_fut = 0.5,
    thr_fx_eff = log(0.7),       # log(HR) < log(0.7) → HR < 0.7
    thr_fx_fut = log(1.0)        # log(HR) > 0 → HR > 1 (futility)
  )
)
```

#### Simulation Function Schema
```r
#' @title Simulate Survival 2-Arm Data
#' @param n_total Total sample size
#' @param p_alloc Allocation probabilities (length 2)
#' @param lambda_control Hazard rate in control
#' @param lambda_treatment Hazard rate in treatment
#' @param shape Weibull shape parameter (1 = exponential)
#' @param accrual_rate Patient accrual per time unit
#' @param followup_time Maximum followup time
#' @param ... Additional parameters
#' @return data.frame with columns: time, event (0/1), arm
sim_survival_2arms <- function(n_total, p_alloc, lambda_control, lambda_treatment,
                                shape = 1, accrual_rate = Inf, followup_time = Inf, ...) {
  # Generate arm assignments
  n_per_arm <- rmultinom(1, n_total, p_alloc)[, 1]
  arm <- rep(c("arm1", "arm2"), times = n_per_arm)

  # Generate entry times (if accrual_rate < Inf)
  if (is.finite(accrual_rate)) {
    entry_times <- cumsum(rexp(n_total, rate = accrual_rate))
  } else {
    entry_times <- rep(0, n_total)
  }

  # Generate event times from Weibull distribution
  lambda <- ifelse(arm == "arm1", lambda_control, lambda_treatment)
  event_times <- rweibull(n_total, shape = shape, scale = 1 / lambda)

  # Apply censoring
  obs_times <- pmin(event_times, followup_time - entry_times)
  event <- as.integer(event_times <= (followup_time - entry_times))

  data.frame(
    time = obs_times,
    event = event,
    arm = factor(arm, levels = c("arm1", "arm2")),
    intercept = 1
  )
}
```

### Sample Size Re-estimation (SSR)

SSR modifies `n_total` during simulation based on interim data. The function signature:

```r
#' @title Sample Size Re-estimation at Interim
#' @param interim_data Data observed up to interim look
#' @param current_n Current sample size
#' @param max_n Maximum allowable sample size
#' @param target_power Desired conditional power (default 0.80)
#' @param method "variance" (re-estimate variance), "conditional_power", or "promising_zone"
#' @param ... Additional method-specific parameters
#' @return List with:
#'   - new_n: Updated total sample size
#'   - reason: Character string explaining decision
#'   - details: List of intermediate calculations
interim_ssr <- function(interim_data, current_n, max_n,
                        target_power = 0.80,
                        method = c("variance", "conditional_power", "promising_zone"),
                        ...) {
  method <- match.arg(method)

  if (method == "variance") {
    # Re-estimate variance from interim data
    sigma_hat <- sd(interim_data$y)
    # Calculate new n for target power (simplified)
    new_n <- calculate_n_for_power(sigma_hat, target_power, ...)
    new_n <- min(new_n, max_n)

    return(list(
      new_n = new_n,
      reason = sprintf("Variance re-estimation: sigma_hat=%.2f", sigma_hat),
      details = list(sigma_hat = sigma_hat, target_power = target_power)
    ))
  }

  # Other methods...
}
```

#### Usage in Conditions
```r
# Enable SSR in conditions
conditions <- build_conditions(
  design = design,
  crossed = list(
    n_total = c(100, 200)  # Initial planned sample size
  ),
  constant = list(
    # Standard simulation/decision params
    b_arm_treat = 0.3,
    p_alloc = list(c(0.5, 0.5)),
    intercept = 0, b_covariate = 0.3, sigma = 1,
    thr_dec_eff = 0.975, thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0,

    # SSR parameters
    analysis_at = list(c(0.5)),  # Re-estimate at 50% of n_total
    interim_ssr = interim_ssr,   # Function to call
    ssr_max_n = 300,             # Maximum allowable n
    ssr_target_power = 0.80,
    ssr_method = "variance"
  )
)
```

#### New Result Columns
SSR adds these columns to `results_conditions`:
- `n_actual_mean`: Mean realized sample size (may differ from `n_total`)
- `n_actual_median`: Median realized sample size
- `n_actual_sd`: Standard deviation of realized sample size
- `pr_increase_n`: Probability that sample size was increased
- `pr_max_n`: Probability that maximum sample size was reached

### Response-Adaptive Randomization (RAR)

RAR modifies `p_alloc` during simulation based on interim results. The function signature:

```r
#' @title Response-Adaptive Randomization at Interim
#' @param interim_results Posterior samples or summaries from interim analysis
#' @param current_alloc Current allocation probabilities
#' @param method "thompson" (Thompson sampling), "bayes_optimal", "play_winner", "DBCD"
#' @param tuning Tuning parameter controlling degree of adaptation (0 = none, 1 = full)
#' @param min_alloc Minimum allocation probability per arm (e.g., 0.1)
#' @param ... Additional method-specific parameters
#' @return List with:
#'   - new_alloc: Updated allocation probabilities
#'   - reason: Character string explaining decision
#'   - details: List of intermediate calculations
interim_rar <- function(interim_results, current_alloc,
                        method = c("thompson", "bayes_optimal", "play_winner", "DBCD"),
                        tuning = 1, min_alloc = 0.1, ...) {
  method <- match.arg(method)

  if (method == "thompson") {
    # Thompson sampling: allocate proportional to P(arm is best)
    # interim_results should contain posterior samples for each arm
    pr_best <- colMeans(apply(interim_results, 1, function(x) x == max(x)))

    # Apply tuning and minimum allocation constraint
    new_alloc <- tuning * pr_best + (1 - tuning) * current_alloc
    new_alloc <- pmax(new_alloc, min_alloc)
    new_alloc <- new_alloc / sum(new_alloc)  # Renormalize

    return(list(
      new_alloc = new_alloc,
      reason = sprintf("Thompson sampling: P(best) = [%s]",
                      paste(round(pr_best, 2), collapse=", ")),
      details = list(pr_best = pr_best, tuning = tuning)
    ))
  }

  # Other methods...
}
```

#### Usage in Conditions
```r
# Enable RAR in conditions
conditions <- build_conditions(
  design = design,
  crossed = list(
    n_total = c(200, 400)
  ),
  constant = list(
    # Standard params
    b_arm_treat = 0.3,
    p_alloc = list(c(0.5, 0.5)),  # Initial allocation (equal)
    intercept = 0, b_covariate = 0.3, sigma = 1,
    thr_dec_eff = 0.975, thr_dec_fut = 0.5,
    thr_fx_eff = 0.2, thr_fx_fut = 0,

    # RAR parameters
    analysis_at = list(seq(0.25, 0.75, by = 0.25)),  # Adapt at 25%, 50%, 75%
    interim_rar = interim_rar,
    rar_method = "thompson",
    rar_tuning = 0.7,    # 70% adaptive, 30% fixed
    rar_min_alloc = 0.2  # At least 20% per arm
  )
)
```

#### New Result Columns
RAR adds these columns to `results_simulations`:
- `alloc_look1`, `alloc_look2`, ...: Allocation probabilities at each look (list column)
- `alloc_final`: Final allocation probabilities (list column)

And to `results_conditions`:
- `alloc_mean`: Mean allocation across simulations (list column)
- `alloc_sd`: Standard deviation of allocation (list column)
- `drift_control`: Mean drift in control allocation from initial
- `drift_treatment`: Mean drift in treatment allocation from initial

## Success Metrics

The package will be considered ready for adaptive trials when it can:

1. **Reproduce published trial designs**
   - Match operating characteristics from literature
   - Validate against other software (EAST, rpact)

2. **Answer key planning questions**
   - "What sample size with 2 interim looks?"
   - "Probability of stopping early?"
   - "Expected sample size savings?"
   - "Type I error with early stopping?"
   - "How long will the trial take?" (requires sample accrual)
   - "At month 12, how many patients have completed follow-up?" (requires sample accrual)

3. **Handle realistic scenarios**
   - Sample accrual / calendar time (see [`25_sample_accrual_plan.md`](25_sample_accrual_plan.md))
   - Missing data
   - Delayed outcomes
   - Multiple endpoints
   - Subgroup analyses

## Testing Strategy

### Unit Tests (per component)
- Data generation for each outcome type
- Model fitting validation
- Interim decision logic
- Adaptation mechanisms

### Integration Tests (workflows)
- Complete trial simulation
- Edge cases (stop at first look, never stop)
- Performance benchmarks
- Reproducibility checks

### Validation Tests (against known results)
- Compare to published trials
- Match analytical solutions where available
- Cross-validate with other packages

## Documentation Requirements

### Vignettes Needed
1. "Introduction to Adaptive Trials with rctbayespower"
2. "Group Sequential Designs"
3. "Sample Size Re-estimation"
4. "Response-Adaptive Randomization"
5. "Platform Trials" (if implemented)

### Examples for Each Model Type
- Basic usage
- With interim analysis
- With adaptations
- Interpretation of results

## Risk Mitigation

### Technical Risks
- **Performance degradation with interim analyses**
  - Mitigation: Profile code, optimize bottlenecks
  - Consider caching strategies

- **Numerical instability in adaptive algorithms**
  - Mitigation: Extensive testing
  - Bounds on adaptation parameters

### Scientific Risks
- **Type I error inflation**
  - Mitigation: Careful implementation of stopping rules
  - Validation against known methods
  - Clear documentation of assumptions

## Conclusion

With 7 weeks of focused development following this roadmap, `rctbayespower` can become a capable adaptive trial simulation package covering:
- ✅ Group sequential designs (most common)
- ✅ Binary, continuous, and survival outcomes
- ✅ Sample size re-estimation
- ✅ Response-adaptive randomization

This would cover approximately 80% of real-world adaptive trial needs and position the package as a valuable tool for trial planning.