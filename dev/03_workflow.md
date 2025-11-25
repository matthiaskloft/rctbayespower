# User Workflow & API Reference

**Last Updated:** 2025-11-24

## Standard 4-Step Workflow

```r
# Step 1: Define the model
model <- build_model(predefined_model = "ancova_cont_2arms")

# Discover available parameter names (model-dependent)
model@parameter_names_brms    # For target_params (e.g., "b_armtreat_1")
model@parameter_names_sim_fn  # For simulation args (e.g., "b_arm_treat")

# Step 2: Specify analysis design
design <- build_design(
  model = model,
  target_params = "b_armtreat_1",  # Must match model@parameter_names_brms
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)

# Step 3: Create conditions grid
conditions <- build_conditions(
  design = design,
  condition_values = list(
    n_total = c(100, 150, 200),
    b_arm_treat = c(0.2, 0.3, 0.5)  # Must match model@parameter_names_sim_fn
  ),
  static_values = list(
    p_alloc = list(c(0.5, 0.5)),
    thresholds_success = 0.2,
    thresholds_futility = 0,
    intercept = 0,
    b_covariate = 0.3,
    sigma = 1
  )
)

# Step 4: Run power analysis
result <- power_analysis(
  conditions = conditions,
  n_sims = 100,
  n_cores = 4,
  run = TRUE
)

# Visualize and access results
plot(result)
result@summarized_results
result@raw_results
```

## Alternative: Build-then-Run

```r
# Create configuration without running
power_config <- power_analysis(
  conditions = conditions,
  n_sims = 100,
  n_cores = 4,
  run = FALSE  # Don't execute yet
)

# Inspect configuration
print(power_config)

# Execute when ready
power_config <- run(power_config)
```

## API Reference

### Top-Level Functions

| Function | Purpose | Returns |
|----------|---------|---------|
| `build_model()` | Create or retrieve model | `rctbp_model` |
| `build_design()` | Configure analysis criteria | `rctbp_design` |
| `build_conditions()` | Generate condition grid | `rctbp_conditions` |
| `power_analysis()` | Configure/run power analysis | `rctbp_power_analysis` |
| `run()` | Execute power analysis | `rctbp_power_analysis` (with results) |
| `plot()` | Visualize results | plotly object |

### Utility Functions

| Function | Purpose |
|----------|---------|
| `list_predefined_models()` | List available predefined models |
| `get_predefined_model()` | Retrieve specific predefined model |
| `required_fn_args()` | Extract required parameters for a design |

### Predefined Models

| Model Name | Description |
|------------|-------------|
| `ancova_cont_2arms` | 2-arm ANCOVA with continuous outcome |
| `ancova_cont_3arms` | 3-arm ANCOVA with continuous outcome |

### Required Parameters for ANCOVA Models

Use `required_fn_args(design)` to see required parameters. Use `model@parameter_names_sim_fn` to discover actual names.

**Simulation Function Parameters** (for condition_values/static_values):

| Parameter | Description |
|-----------|-------------|
| `n_total` | Total sample size |
| `p_alloc` | Allocation ratios, e.g., `list(c(0.5, 0.5))` |
| `intercept` | Baseline mean |
| `b_arm_treat` | Treatment effect coefficient(s) |
| `b_covariate` | Covariate effect |
| `sigma` | Residual standard deviation |

**Decision Parameters** (for condition_values/static_values):

| Parameter | Description |
|-----------|-------------|
| `thresholds_success` | ROPE boundary for success decision |
| `thresholds_futility` | ROPE boundary for futility decision |

**Note**: The `target_params` in `build_design()` must match brms parameter names (use `model@parameter_names_brms`), which differ from simulation function parameter names.

### Condition Values vs Static Values

**`condition_values`**: Parameters that vary across conditions (creates grid)
```r
condition_values = list(
  n_total = c(100, 200),     # 2 sample sizes
  b_arm_treat = c(0.2, 0.5)  # 2 effect sizes
)
# Creates 2 × 2 = 4 conditions
```

**`static_values`**: Parameters held constant across all conditions
```r
static_values = list(
  p_alloc = list(c(0.5, 0.5)),
  sigma = 1
)
```

## Results Structure

### `summarized_results` (Aggregated)

| Column | Description |
|--------|-------------|
| `id_cond` | Condition identifier |
| `parameter` | Parameter name |
| `power_success` | Estimated power for success |
| `power_futility` | Estimated power for futility |
| `prob_success` | Mean P(effect > threshold) |
| `prob_futility` | Mean P(effect < threshold) |
| `median`, `mean` | Posterior point estimates |
| `rhat`, `ess_bulk` | Convergence diagnostics |
| `*_se` | Monte Carlo standard errors |

### `raw_results` (Individual Simulations)

One row per simulation × condition × parameter, with all metrics from each individual run.

## Debugging Tips

| Problem | Solution |
|---------|----------|
| Parallel crashes | Set `n_cores = 1` to run sequentially |
| "Pre-defined model not found" | Use `predefined_model = "ancova_cont_2arms"` |
| S7 property access error | Use `@` not `$` |
| Missing parameters | Check `required_fn_args(design)` |
| Convergence issues | Check `conv_rate` and `rhat` columns |
