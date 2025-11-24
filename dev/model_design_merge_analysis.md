# Analysis: Should We Merge rctbp_model and rctbp_design Classes?

**Date:** November 24, 2025
**Status:** Architecture Analysis

## Executive Summary

After analyzing the current class structure, I **recommend AGAINST merging** the model and design classes, despite the design class being minimal. The separation provides important architectural benefits that outweigh the simplicity of merging.

## Current State Analysis

### rctbp_design Class (Minimal - 5 properties)
```r
properties:
  - model: rctbp_model object
  - target_params: character (which params to analyze)
  - p_sig_success: numeric (probability threshold)
  - p_sig_futility: numeric (probability threshold)
  - design_name: character | NULL (optional)
```

### rctbp_model Class (Complex - 11+ properties)
```r
properties:
  - data_simulation_fn: function
  - brms_model: brmsfit | NULL
  - bayesflow_model: any | NULL
  - backend_args: list
  - predefined_model: character | NULL
  - model_name: character
  - n_endpoints: numeric
  - endpoint_types: character
  - n_arms: numeric
  - n_repeated_measures: numeric | NULL
  - backend: computed property
  - parameter_names_sim_fn: computed property
  - parameter_names_brms: computed property
```

### Usage Pattern Analysis

1. **Always Used Together**: Every design contains exactly one model
2. **Never Standalone**: Models are never used without a design
3. **Deep Access Pattern**: Code frequently accesses `design@model@property`
4. **No Model Reuse**: No evidence of sharing models across designs

## Arguments FOR Merging

### 1. Simplification
- Eliminate one level of nesting (`design@property` vs `design@model@property`)
- Reduce number of classes from 4 to 3
- Simpler mental model for users

### 2. Current Usage Patterns
- Models are never reused across designs
- No standalone model operations
- Always created and used together

### 3. Reduced Boilerplate
```r
# Current
model <- build_model("ancova_cont_2arms")
design <- build_design(model, target_params = "b_arm2", ...)

# After merge
design <- build_design("ancova_cont_2arms", target_params = "b_arm2", ...)
```

## Arguments AGAINST Merging (STRONGER)

### 1. Separation of Concerns ✅

**Model**: WHAT can be simulated and estimated
- Data generation logic
- Statistical model specification
- Parameter structure
- Backend configuration

**Design**: HOW to analyze for power
- Which parameters to target
- Decision thresholds
- Analysis configuration

This separation is **conceptually clean** and follows good OOP principles.

### 2. Future Flexibility ✅

Current separation enables future scenarios:

```r
# Scenario 1: Same model, different analysis strategies
model_ancova <- build_model("ancova_cont_2arms")

design_conservative <- build_design(
  model = model_ancova,
  target_params = "b_arm2",
  p_sig_success = 0.995  # Very conservative
)

design_standard <- build_design(
  model = model_ancova,
  target_params = "b_arm2",
  p_sig_success = 0.975  # Standard
)

# Scenario 2: Analyzing different parameters from same model
design_treatment <- build_design(
  model = model_ancova,
  target_params = "b_arm2"  # Treatment effect
)

design_covariate <- build_design(
  model = model_ancova,
  target_params = "b_covariate"  # Covariate effect
)
```

### 3. Model Compilation Cost ✅

Models contain compiled Stan/brms objects that are expensive to create:
- Compilation takes minutes
- Models are ~50-100MB serialized
- Reusing models saves significant time

Merging would prevent model reuse optimization.

### 4. Cleaner Extension Points ✅

For planned interim analysis features:
```r
# Clean to add to design
design <- build_design(
  model = model,
  target_params = "b_arm2",
  analysis_at = c(50, 100),  # Interim analyses
  interim_function = my_stopping_rule  # Design-specific
)
```

These are analysis decisions, not model properties.

### 5. Backend Abstraction ✅

Models encapsulate backend complexity:
- brms vs NPE/Bayesflow
- Backend-specific arguments
- Model compilation

Designs shouldn't know about backends.

### 6. Testing and Validation ✅

Separate classes allow:
- Independent testing of model validity
- Independent testing of design configuration
- Clear error messages about which component failed

## Impact Analysis of Merging

### Code Changes Required

**High Impact Files** (20+ references each):
- `simulate_single_run.R`: 12 `design@model@` references
- `worker_functions.R`: 22 `design@model@` references
- `class_design.R`: 18 `design@model@` references
- `class_power_analysis.R`: 8 `design@model@` references

**Total Refactoring**: ~60+ code locations

### API Breaking Changes

```r
# Current API (would break)
model <- build_model("ancova_cont_2arms")
design <- build_design(model = model, ...)

# New merged API
design <- build_design_with_model("ancova_cont_2arms", ...)
```

### Loss of Functionality

1. **No model precompilation and reuse**
2. **No model caching across designs**
3. **Harder to implement model libraries/repositories**
4. **Less clear error attribution**

## Alternative Improvements (Instead of Merging)

### Option 1: Convenience Functions
```r
# Keep classes separate but add convenience
build_analysis <- function(model_name, target_params, ...) {
  model <- build_model(model_name)
  build_design(model, target_params, ...)
}
```

### Option 2: Property Forwarding
```r
# Add computed properties to design for common access
rctbp_design <- S7::new_class(
  properties = list(
    # ... existing ...
    data_simulation_fn = S7::new_property(
      getter = function(self) self@model@data_simulation_fn
    )
  )
)
# Now can use: design@data_simulation_fn instead of design@model@data_simulation_fn
```

### Option 3: Simplified Constructors
```r
# Allow string model names in build_design
build_design <- function(model, ...) {
  if (is.character(model)) {
    model <- build_model(model)
  }
  # ... continue
}
```

## Recommendation

### DON'T MERGE - Keep Classes Separate

**Reasons:**
1. **Conceptual clarity** > Minor simplification
2. **Future flexibility** for model reuse is valuable
3. **Compilation cost** makes model reuse important
4. **Clean extension points** for interim analysis
5. **Backend abstraction** belongs in model, not design

### DO: Add Convenience Features

1. **Implement Option 3**: Allow `build_design(model = "ancova_cont_2arms", ...)`
2. **Consider property forwarding** for most common accesses
3. **Document the architecture** clearly for users

### Architecture Principle

The current separation follows the **Single Responsibility Principle**:
- **Model**: Responsible for HOW to generate data and fit statistical models
- **Design**: Responsible for WHAT questions to ask about the model
- **Conditions**: Responsible for WHICH parameter values to evaluate
- **Power Analysis**: Responsible for orchestrating the simulation

This is good architecture worth preserving.

## Code Example: Why Separation Matters

```r
# With separation (current): Clean and reusable
cancer_model <- build_model("survival_2arms")  # Expensive: 2 min compile

# Multiple analyses of same model
os_analysis <- build_design(cancer_model, target_params = "hazard_ratio")
pfs_analysis <- build_design(cancer_model, target_params = "median_survival")
subgroup_analysis <- build_design(cancer_model, target_params = "biomarker_interaction")

# With merging: Wasteful and unclear
os_analysis <- build_merged("survival_2arms", target = "hazard_ratio")  # 2 min
pfs_analysis <- build_merged("survival_2arms", target = "median_survival")  # 2 min again!
subgroup_analysis <- build_merged("survival_2arms", target = "biomarker_interaction")  # 2 min again!
# Total: 6 minutes vs 2 minutes
```

## Final Verdict

**Keep the classes separate.** The minimal nature of rctbp_design is a feature, not a bug - it represents a clean abstraction of analysis configuration separate from model specification. The separation provides real value for compilation efficiency, future extensions, and architectural clarity.