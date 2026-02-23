# BayesFlow Model Retraining Guide

**Status:** Required before BF backend is usable
**Priority:** High — BF backend silently falls back to brms until resolved
**Date documented:** 2026-02-21

---

## Problem Summary

The pre-trained model at `dev/bf_models/ancova_cont_2arms.keras` cannot be
loaded with any released BayesFlow 2.0.x version (tested: 2.0.4, 2.0.8).

### Root Cause

The saved model uses a 1D `CouplingFlow` (one inference parameter: `b_group`).
All released BayesFlow 2.0.x versions fail with:

```
ValueError: Received an invalid value for `units`, expected a positive integer.
Received: units=0  (or units=None)
```

**Why:** `CouplingFlow` splits the inference vector into two halves for the
affine coupling layers. For a 1D input: `floor(1/2) = 0` → `Dense(units=0)` →
crash. The model was apparently saved with a dev/pre-release version of
BayesFlow where `SingleCoupling` built `output_projector` lazily in `build()`
(so it had real dimensions from the first forward pass). All PyPI releases
moved this to `__init__()` where the dimension isn't known yet.

### How This Manifests at Runtime

```
ℹ Loading cached BayesFlow model: "ancova_cont_2arms"
# BF load fails silently inside tryCatch
Warnung: BayesFlow backend unavailable, using brms
```

The design falls back to brms with `par_names_inference = c("b_Intercept",
"b_covariate", "b_arm2")`. Any attempt to use the BF-native parameter name
`"b_group"` then fails validation.

### Parameter Naming (relevant for retrain)

The trained model uses `b_group` as the inference variable name (confirmed from
the Keras adapter config: `Rename(from_key="b_group", to_key="inference_variables")`).
The brms model uses `b_arm2`. These are intentionally different — the BF model
is trained on batch-format data where the treatment indicator column is `group`.

---

## Architecture of the Saved Model (for reference)

From the `.keras` config JSON:

```
ContinuousApproximator
├── adapter: Adapter
│   ├── Broadcast(N → outcome shape)
│   ├── Broadcast(p_alloc → outcome shape)
│   ├── FilterTransform: Standardize(outcome, covariate)
│   ├── MapTransform: AsSet(outcome, covariate, group)
│   ├── MapTransform: Sqrt(N)
│   ├── FilterTransform: ConvertDType float64→float32 (all fields)
│   ├── Rename(from_key="b_group", to_key="inference_variables")  ← param name
│   ├── Concatenate([outcome, covariate, group] → summary_variables, axis=-1)
│   └── Concatenate([N, p_alloc] → inference_conditions, axis=-1)
├── inference_network: CouplingFlow
│   ├── depth=4, transform="affine", permutation="random", use_actnorm=True
│   ├── subnet="mlp", dropout=0.5, hidden_sizes=[64, 64]
│   └── base: DiagonalNormal
│       build_config: xz_shape=[300, 1], conditions_shape=[300, 4]  ← 1D problem!
└── summary_network: DeepSet
    ├── summary_dim=2
    ├── mlp_widths_equivariant=[32, 32]
    ├── mlp_widths_invariant_inner/outer/last=[32, 32]
    └── dropout=0.1
    build_config: input_shape=[300, 744, 3]  ← [batch, n_obs, 3 features]
```

---

## Options for Fix

### Option A: FlowMatching (Recommended for 1D)

`FlowMatching` from BayesFlow 2.0 handles 1D inference parameters natively
because it uses ODEs rather than coupling layers that require splitting.

```python
import bayesflow as bf

approximator = bf.ContinuousApproximator(
    inference_network=bf.networks.FlowMatching(
        subnet="mlp",
        subnet_kwargs={"dropout": 0.05, "hidden_sizes": [64, 64]},
    ),
    summary_network=bf.networks.DeepSet(
        summary_dim=2,
        mlp_widths_equivariant=[32, 32],
        mlp_widths_invariant_inner=[32, 32],
        mlp_widths_invariant_outer=[32, 32],
        mlp_widths_invariant_last=[32, 32],
        dropout=0.1,
    ),
)
```

### Option B: Pad Inference Variable to 2D

Keep `CouplingFlow` but pad `b_group` to a 2D vector before the flow, then
strip the padding after sampling. Add a `Pad` transform in the adapter, and a
corresponding `Slice` on the output.

```python
# In adapter (before Rename):
bf.adapters.transforms.Pad(keys=["b_group"], ...),  # pad to [batch, 2]
```

This avoids rewriting the inference network but adds adapter complexity.

### Option C: CouplingFlow with dim ≥ 2 via reshape

The cleanest fix while keeping CouplingFlow: treat `b_group` as a 2D vector by
duplicating it, run the flow on 2D, and marginalize post-hoc. Not recommended —
just use FlowMatching.

---

## Retraining Checklist

- [ ] Create training script at `dev/zz_scripts/train_ancova_cont_2arms_bf.py`
- [ ] Use `simulate_data_ancova_cont_2arms_batch()` (or equivalent Python)
      for data generation — ensures R/Python sim consistency
- [ ] Set inference variable name to `b_group` in the adapter `Rename` transform
      (this matches `get_bf_parameter_names()` in `R/class_design.R`)
- [ ] Use `FlowMatching` inference network (Option A above) for 1D robustness
- [ ] Train for ~100k samples, validate posterior coverage
- [ ] Save to `dev/bf_models/ancova_cont_2arms.keras` (overwrites old file)
- [ ] Upload to GitHub release tag `bf-models-v0.0.0.9000` (or bump tag)
- [ ] Clear local model cache: delete `%LOCALAPPDATA%/rctbayespower/bf/`
- [ ] Smoke test in R:
      ```r
      init_bf("rctbp-3-12")
      d <- build_design("ancova_cont_2arms", backend = "bf", target_params = "b_group")
      d@par_names_inference  # should be "b_group"
      ```

---

## Data Format for Training

The R batch simulator `simulate_data_ancova_cont_2arms_batch()` generates:

```
batch = {
  outcome:   matrix [n_sims, n_total]   # continuous outcome
  covariate: matrix [n_sims, n_total]   # standardized covariate
  group:     matrix [n_sims, n_total]   # binary 0/1 treatment indicator
  N:         int                        # sample size
  p_alloc:   float                      # treatment allocation probability
}
parameters = {
  b_group:     float  # treatment effect  ← inference target
  b_covariate: float  # covariate effect  ← nuisance (optional inference)
  intercept:   float  # ← nuisance
  sigma:       float  # ← nuisance
}
```

The prior for `b_group` used in the original model appears to be `Normal(0, 1)`
(standardized scale). Match this when retraining or update `design_prior` defaults.

---

## Workaround Until Retrained

Use the brms backend:

```r
design <- build_design(
  model_name = "ancova_cont_2arms",
  backend = "brms",          # default — no Python needed
  target_params = "b_arm2"   # brms parameter name for treatment
)
```

Note: `b_arm2` is the brms encoding of the `arm` factor's second level
(`treat_1`) under `contr.treatment` contrasts. It is numerically equivalent
to `b_group` from the BF model — both represent the treatment effect on the
outcome scale.

---

## Related Files

| File | Relevance |
|------|-----------|
| `R/class_design.R` | `get_bf_parameter_names()` — reads param name from model adapter |
| `R/backend_bf.R` | `load_bf_model_python()`, `init_bf()` |
| `R/model_cache.R` | `load_bf_model()`, `download_model()` |
| `R/models_ancova.R` | `simulate_data_ancova_cont_2arms_batch()` |
| `dev/bf_models/ancova_cont_2arms.keras` | Current (broken) model file |
| `dev/zz_scripts/minimal_bayesflow_workflow.R` | Low-level BF usage example |
