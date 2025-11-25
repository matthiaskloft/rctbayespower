# Adaptive Trials Implementation Roadmap
**Date:** November 24, 2025
**Purpose:** Actionable roadmap to enable adaptive trial simulations

## Current State Summary

The `rctbayespower` package currently supports:
- ✅ Fixed sample size Bayesian power analysis
- ✅ Continuous outcomes only (ANCOVA models)
- ✅ ROPE-based decisions
- ✅ Efficient parallel processing

The package currently LACKS:
- ❌ Any interim analysis capability (plan exists, not implemented)
- ❌ Binary, survival, count outcomes
- ❌ Sample size re-estimation
- ❌ Adaptive randomization
- ❌ Dose finding
- ❌ Platform/basket/umbrella designs

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
- Interim analysis ❌
- Futility stopping rules ❌

### Use Case 2: Rare Disease with Sample Size Re-estimation
**Scenario:** Rare disease trial with continuous outcome
- Initial n=50 planned
- Interim at n=25 to re-estimate variance
- Increase sample size up to n=100 if needed for power
- ROPE: [-0.3, 0.3] for standardized effect

**Required features:**
- Continuous outcome model ✅
- Interim analysis ❌
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
- Survival outcomes ❌
- Event-driven interims ❌
- Hazard ratio calculations ❌

## Implementation Priorities

### 🔴 Priority 1: Enable Group Sequential Designs (Most Common)
**Timeline: 4 weeks**

#### Week 1-2: Core Interim Analysis
- [ ] Implement interim analysis infrastructure per existing plan
- [ ] Add analysis_at property to rctbp_design
- [ ] Refactor simulate_single_run() for sequential data
- [ ] Create interim decision functions
- [ ] Test with continuous ANCOVA model

#### Week 3: Binary Outcomes
- [ ] Create build_model_binary_2arms()
- [ ] Add logistic regression via brms
- [ ] Binary data generation
- [ ] Adapt compute_measures for proportions
- [ ] Test binary model with interim analysis

#### Week 4: Integration and Testing
- [ ] Full workflow testing
- [ ] Documentation and examples
- [ ] Performance optimization
- [ ] Create vignette for group sequential

**Deliverable:** Can simulate standard group sequential trials with continuous or binary outcomes

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

1. **Interim analysis** ✅ (after Priority 1)
   - At least futility and efficacy stopping
   - Works with all outcome types

2. **Binary outcomes** ✅ (after Priority 1)
   - Most Phase II/III trials use binary endpoints
   - Response rates, mortality, success/failure

3. **Sample size re-estimation** ✅ (after Priority 2)
   - Address uncertainty in planning assumptions
   - Required by many regulatory agencies

4. **Basic reporting** ✅ (after Priority 1)
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
  p_sig_success = 0.975,
  p_sig_futility = 0.90,
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
  p_sig_success = 0.975,
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

3. **Handle realistic scenarios**
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