# BayesFlow Model Interface Specification

**Version:** 1.0
**Date:** 2026-04-02
**Status:** Active — contract for model development and retraining

---

## 1. Purpose

This document defines the exact contract between `rctbayespower` (R, the consumer) and `bayesflow-rct` (Python, the trainer) for BayesFlow neural posterior estimation models. It specifies input/output formats, file conventions, and parameter mappings for all v1 registry models.

**Audience:** Developer implementing or retraining a BF model for the registry.

---

## 2. General Contract

### 2.1 File Format & Naming

| Property | Value |
|----------|-------|
| Format | `.keras` (Keras 3 native via `keras.saving.save_model()`) |
| Filename | `{registry_name}.keras` (e.g., `ancova_cont_2arms.keras`) |
| Cache dir | `~/.cache/rctbayespower/bf/` |
| Release tag | `bf-models-v{version}` (currently `v0.0.0.9000`) |
| Download URL | `https://github.com/matthiaskloft/rctbayespower/releases/download/{tag}/{name}.keras` |
| Metadata sidecar | `{name}_metadata.json` (optional, via `save_workflow_with_metadata()`) |

**Source:** `model_cache.R:67-76`

### 2.2 Model Architecture Requirements

- Must be `bf.ContinuousApproximator` (provides `.sample()`, `.adapter`, `.fit_online()`)
- For **1D inference targets** (e.g., `b_group` scalar): must use `FlowMatching`, not `CouplingFlow`
  - CouplingFlow splits the vector into two halves: `floor(1/2) = 0` → `Dense(units=0)` → crash
  - See `dev/13_bf_model_retraining.md` for details
- For **≥2D inference targets**: either `FlowMatching` or `CouplingFlow` is viable

R detects model type via duck-typing in `detect_bf_model_type()` (`backend_bf.R:1355-1375`):
- `"workflow"`: has `sample()` + `fit_online()`
- `"approximator"`: has `sample()` + `fit()`
- `"keras"`: has `predict()` + `compile()`

### 2.3 Adapter Contract

Every trained model's adapter must contain:

1. **Rename transform**: `{param_name} → "inference_variables"` — this is how R discovers inference target names via `get_bf_parameter_names()` (`class_design.R:211-223`)
2. **Canonical output keys** after adapter forward pass:
   - `inference_variables`: shape `(B, D)` where D = number of inference parameters
   - `summary_variables`: shape `(B, N_obs, F)` — set-based observation features
   - `inference_conditions`: shape `(B, C)` — scalar context variables

### 2.4 Data Pipeline

```
R sim_fn(params)
    → data.frame { outcome, arm (factor), covariate }

output_schema (arm → group via factor_to_numeric)
    → workaround in derive_output_schema_from_data() [class_sim_fn.R:59-61]

estimate_batch_bf() builds conditions dict:
    {outcome: (B,N), covariate: (B,N), group: (B,N),
     N: int, p_alloc: float, prior_df: int, prior_scale: float}
    → converted to NumPy float32 [backend_bf.R:1614-1621]
    ⚠ CURRENT GAP: R does NOT pass prior_df/prior_scale yet — must be fixed

Python adapter forward pass:
    1. Broadcast scalar context to (B,1)
    2. as_set([outcome, covariate, group]) → (B, N_obs, 1) each
    3. Standardize outcome, covariate
    4. Transform: sqrt(N), log1p(prior_df)
    5. Drop nuisance params (b_covariate)
    6. Rename b_group → inference_variables
    7. Concatenate [outcome, covariate, group] → summary_variables (B, N_obs, 3)
    8. Concatenate [N, p_alloc, prior_df, prior_scale] → inference_conditions (B, 4)
    9. Convert float64 → float32

BayesFlow inference:
    summary_network(summary_variables) → (B, summary_dim)
    inference_network.sample(conditions, num_samples)
    → {param_name: (B, num_samples, D)}

R post-processing:
    squeeze → (B, num_samples) for D=1
    extract target_params column → R matrix (B, num_samples)
```

### 2.5 Inference Protocol

R calls:
```r
# backend_bf.R:1624-1625
samples_py <- bf_model$sample(conditions = data_cond, num_samples = n_samples)
```

**Input (`data_cond`):** Python dict with all batch fields as NumPy float32 arrays.

**Output:** Python dict `{param_name: ndarray(B, num_samples, D)}`.

R squeezes trailing dim for D=1 and extracts the column matching `target_params`.

### 2.6 Parameter Naming

| Context | Name | Meaning |
|---------|------|---------|
| BayesFlow model | `b_group` | Treatment effect (batch format, int 0/1 encoding) |
| brms model | `b_arm2` | Treatment effect (factor level encoding, `contr.treatment`) |
| R sim_fn | `b_arm_treat` | Treatment effect (user-facing parameter name) |

All three are numerically equivalent for 2-arm models. The naming difference is intentional — each convention matches its context.

### 2.7 arm → group Workaround

R's `sim_fn` outputs `arm` as a factor (`ctrl`, `treat_1`, ...). BayesFlow expects `group` as integer (0/1).

The workaround lives in two places:
1. `derive_output_schema_from_data()` (`class_sim_fn.R:59-61`): maps `arm` → `group` in the output schema
2. `get_batch_field_map("ancova")` (`backend_bf.R:1026-1060`): accepts either `group` or `arm` as source, applies `factor_to_numeric` transform

**Future:** Retrain models to accept sim_fn output directly, or formalize `group` as the canonical BF convention.

### 2.8 Validation Requirements

- Must pass condition-grid validation via `run_condition_grid_validation()` from `bayesflow-hpo`
- Key metrics: NRMSE, posterior contraction, calibration error
- Training prior ranges define the model's domain of validity — document them per model

---

## 3. Per-Model Specifications

### 3.1 ancova_cont_2arms

| Property | Value |
|----------|-------|
| **Status** | Blocked — needs retrain with FlowMatching (CouplingFlow 1D bug) |
| **Registry entry** | `class_design.R:464-470` |
| **R sim_fn** | `create_ancova_sim_fn(n_arms = 2)` → `class_design.R:652-707` |
| **Python model** | `bayesflow-rct/src/bayesflow_rct/models/ancova/` |
| **Inference target** | `b_group` — shape `(B, 1)` |
| **Inference network** | `FlowMatching` (required for 1D) |

**R sim_fn signature:**
```r
function(n_total, n_arms = 2, contrasts = "contr.treatment",
         p_alloc = c(0.5, 0.5), intercept = 0,
         b_arm_treat = 0, b_covariate = 0, sigma = 1)
# Returns: data.frame(covariate, arm, outcome)
```

**Batch format (R → Python):**

| Field | Shape | Type | Description |
|-------|-------|------|-------------|
| `outcome` | `(B, N)` | float32 | Continuous outcome |
| `covariate` | `(B, N)` | float32 | Baseline covariate |
| `group` | `(B, N)` | float32 | Binary treatment indicator (0/1) |
| `N` | scalar | int | Total sample size |
| `p_alloc` | scalar | float | Treatment allocation probability |
| `prior_df` | scalar | int | Prior degrees of freedom for `b_group` |
| `prior_scale` | scalar | float | Prior scale for `b_group` |

**Adapter spec** (`adapter.py:30-47`):
```python
{
    "set_keys": ["outcome", "covariate", "group"],
    "param_keys": ["b_group"],
    "context_keys": ["N", "p_alloc", "prior_df", "prior_scale"],
    "standardize_keys": ["outcome", "covariate"],
    "prior_standardize": {"b_group": (None, "prior_scale")},
    "context_transforms": {
        "N": (np.sqrt, np.square),
        "prior_df": (np.log1p, np.expm1),
    },
}
```

**Inference conditions** (after transforms): `[sqrt(N), p_alloc, log1p(prior_df), prior_scale]` → shape `(B, 4)`

**Training priors** (`simulator.py` + `config.py`):

| Parameter | Distribution | Range/Params |
|-----------|-------------|--------------|
| `N` | LogUniform (int) | [20, 1000] |
| `p_alloc` | Uniform | [0.5, 0.9] |
| `prior_df` | LogUniform (shifted) | [0, 30] (sampled as `loguniform_int(1, 31) - 1`) |
| `prior_scale` | Gamma | shape=2.0, scale=1.0 |
| `b_group` | Student-t(df, scale) | df=`prior_df`, scale=`prior_scale` (Normal if df invalid) |
| `b_covariate` | Normal | mean=0, sd=2.0 |
| `sigma` | Fixed | 1.0 |

**Known issues:**
1. Current `.keras` file uses CouplingFlow → cannot load (Dense units=0 crash)
2. R `estimate_batch_bf()` does not yet pass `prior_df`/`prior_scale` in `data_cond`

---

### 3.2 ancova_cont_3arms [Planned]

| Property | Value |
|----------|-------|
| **Status** | Not yet implemented |
| **Registry entry** | `class_design.R:471-477` |
| **R sim_fn** | `create_ancova_sim_fn(n_arms = 3)` |
| **Inference target** | `b_group` — shape `(B, 2)` (two treatment effects vs control) |
| **Inference network** | `FlowMatching` or `CouplingFlow` (dim ≥ 2, CouplingFlow viable) |

**Differences from 2-arm:**
- `b_arm_treat` has 2 elements (one per treatment arm vs control)
- `group` encoding: 0/1/2 (ordinal) — needs design decision on one-hot vs ordinal
- May need per-arm allocation probabilities in conditions

**Open design questions:**
- Group encoding scheme (ordinal integer vs one-hot)
- Whether `p_alloc` becomes a vector `(p_ctrl, p_treat1, p_treat2)`

---

### 3.3 ancova_bin_2arms [Planned]

| Property | Value |
|----------|-------|
| **Status** | Not yet implemented |
| **Registry entry** | `class_design.R:478-484` |
| **R sim_fn** | `create_ancova_bin_sim_fn(n_arms = 2)` |
| **Inference target** | `b_group` on **log-odds scale** — shape `(B, 1)` |
| **Inference network** | `FlowMatching` (1D target) |

**Differences from continuous:**
- Binary outcome (0/1) instead of continuous
- Logistic regression link: `logit(P(outcome=1)) = intercept + b_covariate*x + b_group*group`
- Standardization may differ for binary outcome (mean-centering not meaningful)
- Same batch structure otherwise

---

### 3.4 ancova_prop_2arms [Planned]

| Property | Value |
|----------|-------|
| **Status** | Not yet implemented |
| **Registry entry** | `class_design.R:485-491` |
| **R sim_fn** | `create_ancova_prop_sim_fn(n_arms = 2)` |
| **Inference target** | `b_group` — shape `(B, 1)` |
| **Inference network** | `FlowMatching` (1D target) |

**Differences from continuous:**
- Outcome is a proportion (0, 1), e.g., logit-normal or Beta regression
- Functionally similar to continuous ANCOVA for BF (same adapter structure, different prior ranges)
- May need different standardization for bounded outcomes

---

### 3.5 survival_exp_2arms [Planned]

| Property | Value |
|----------|-------|
| **Status** | Not yet implemented |
| **Registry entry** | `class_design.R:492-498` |
| **R sim_fn** | `create_survival_exp_sim_fn(n_arms = 2)` |
| **Inference target** | `log_hazard_ratio` or `hazard_ratio` — shape `(B, 1)` |
| **Inference network** | `FlowMatching` (1D target) |

**Different batch field map** (`get_batch_field_map("survival")`, `backend_bf.R:1043-1048`):

| Field | Shape | Source aliases | Transform |
|-------|-------|---------------|-----------|
| `time` | `(B, N)` | `time`, `survival_time`, `t` | — |
| `event` | `(B, N)` | `event`, `status`, `delta` | `logical_to_int` |
| `covariate` | `(B, N)` | `covariate` | — |
| `group` | `(B, N)` | `group`, `arm` | `factor_to_numeric` |

**Key differences from ANCOVA models:**
- `estimate_batch_bf()` is currently hardcoded for ANCOVA field structure — needs generalization
- R sim_fn may output `censored` but field map expects `event` — naming convention gap
- Summary network input: 4 features per observation (time, event, covariate, group) vs 3 for ANCOVA

---

## 4. Contract Gaps & Action Items

| # | Gap | Resolution | Priority |
|---|-----|------------|----------|
| 1 | R does not pass `prior_df`/`prior_scale` to Python | Update `estimate_batch_bf()` in `backend_bf.R:1614-1621` to include both fields | **High** — blocks retrained model usage |
| 2 | `estimate_batch_bf()` hardcoded for ANCOVA fields | Generalize to use `get_batch_field_map()` dynamically | Medium — blocks survival model |
| 3 | Model cache whitelist | Update for new models as they are trained | Low — per-model |
| 4 | Validation naming mismatch | `b_arm_treat` in condition grid vs `b_group` in batch data | Low — cosmetic |
| 5 | Survival `censored` vs `event` | Standardize naming in sim_fn or field map | Medium — blocks survival model |
| 6 | Old model only had 2 conditions | Retrained model will have 4 conditions (`N, p_alloc, prior_df, prior_scale`) | Resolved by retrain |

---

## 5. Version History

| Date | Change |
|------|--------|
| 2026-04-02 | Initial spec created. Covers all 5 v1 registry models. |

---

## 6. Key Source Files

| File | What it contains |
|------|-----------------|
| `rctbayespower/R/backend_bf.R:1614-1621` | R → Python data conversion (the `prior_df`/`prior_scale` gap) |
| `rctbayespower/R/backend_bf.R:1026-1060` | Batch field maps per model type |
| `rctbayespower/R/backend_bf.R:1355-1375` | `detect_bf_model_type()` — duck-typing dispatch |
| `rctbayespower/R/class_design.R:211-223` | `get_bf_parameter_names()` — reads Rename transform |
| `rctbayespower/R/class_design.R:463-498` | Model registry (5 models) |
| `rctbayespower/R/class_design.R:628-707` | `create_sim_fn_for_model()` + `create_ancova_sim_fn()` |
| `rctbayespower/R/class_sim_fn.R:37-61` | arm → group workaround |
| `rctbayespower/R/model_cache.R:67-76` | Download URL pattern |
| `rctbayespower/dev/13_bf_model_retraining.md` | Existing retraining guide (CouplingFlow bug details) |
| `bayesflow-rct/src/.../ancova/adapter.py` | Full adapter pipeline (declarative spec + fluent API) |
| `bayesflow-rct/src/.../ancova/simulator.py` | Simulator: prior, likelihood, meta functions |
| `bayesflow-rct/src/.../ancova/config.py` | `ANCOVAConfig` dataclass (~28 fields) + `build_networks()` |
| `bayesflow-rct/src/.../ancova/metadata.py` | Model saving with metadata |
