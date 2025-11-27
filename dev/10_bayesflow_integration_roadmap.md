# BayesFlow Integration Roadmap

## Current State (2025-11-27)

### Completed R Infrastructure

| Component | File | Status |
|-----------|------|--------|
| brms backend | `R/backend_brms.R` | ✅ Complete |
| BayesFlow backend | `R/backend_bf.R` | ✅ Complete (reticulate calls implemented) |
| Model caching | `R/model_cache.R` | ✅ Complete |
| Dual-backend model class | `R/class_model.R` | ✅ Complete |
| Worker dispatch | `R/worker_functions.R` | ✅ Complete |
| Batch simulation | `R/models_ancova.R` | ✅ Complete |
| Shared utilities | `R/utils_results.R` | ✅ Complete |

### Implementation Summary

The BayesFlow backend (`R/backend_bf.R`) is now fully implemented with:

1. **Python Environment Management**
   - `check_bf_available()` - Verifies Python/reticulate/BayesFlow availability
   - `init_bf_python()` - Initializes and caches Python modules
   - Module cache (`.bf_cache` environment) for efficient reuse

2. **Model Type Support**
   - `detect_bf_model_type()` - Identifies workflow/approximator/keras models
   - `sample_bf_model()` - Dispatches to appropriate sampling method
   - Supports: BasicWorkflow, Approximator, raw Keras models

3. **Core Functions**
   - `estimate_batch_bf()` - Single forward pass for batch of simulations
   - `estimate_single_bf()` - Handles single or batch data.frames
   - `estimate_sequential_bf()` - Sequential interim analyses

4. **Mock Mode for Testing**
   - `is_bf_mock_mode()` - Checks `RCTBP_MOCK_BF` environment variable
   - `mock_bf_samples()` - Generates data-driven mock posterior samples
   - Enables R CMD check and unit tests without Python

---

## BayesFlow Model Interface

### Input: Summary Statistics (8-dimensional)

```r
# compute_summaries_batch_ancova() extracts:
summaries <- c(
  mean_outcome_ctrl,      # 1
  mean_outcome_treat,     # 2
  sd_outcome_ctrl,        # 3
  sd_outcome_treat,       # 4
  n_ctrl,                 # 5
  n_treat,                # 6
  cor_outcome_covariate,  # 7
  mean_covariate          # 8
)
```

### Output: Posterior Samples

```r
# Matrix (batch_size x n_posterior_samples)
draws_matrix <- estimate_batch_bf(data_batch, bf_model, backend_args, target_params)
```

### Supported BayesFlow Model Types

| Type | Detection | Sampling Method |
|------|-----------|-----------------|
| BasicWorkflow | has `sample()` + `fit_online()` | `workflow$sample(conditions=dict, num_samples=int)` |
| Approximator | has `sample()` + `fit()` | `approximator$sample(data=dict, num_samples=int)` |
| Raw Keras | has `predict()` + `compile()` | `model$predict()` + Gaussian sampling |

---

## Phase 1: Python Training (Next Step)

### File: `python/train_ancova_2arms.py`

```python
import bayesflow as bf
import numpy as np

# 1. Define simulator matching R's simulate_data_ancova_cont_2arms
def simulate_ancova_2arms(rng, n_total=100, b_arm_treat=0.3, b_covariate=0.3, sigma=1.0, p_alloc=0.5):
    """Generate single simulation matching R function."""
    covariate = rng.normal(0, 1, n_total)
    group = rng.binomial(1, p_alloc, n_total)
    outcome = b_covariate * covariate + b_arm_treat * group + rng.normal(0, sigma, n_total)
    return {"outcome": outcome, "covariate": covariate, "group": group}

# 2. Define summary statistics extractor (must match R's compute_summaries_batch_ancova)
def compute_summaries(sim_data):
    """Extract fixed-length summary statistics."""
    outcome, covariate, group = sim_data["outcome"], sim_data["covariate"], sim_data["group"]
    ctrl_mask = group == 0
    treat_mask = group == 1
    return np.array([
        outcome[ctrl_mask].mean(),
        outcome[treat_mask].mean(),
        outcome[ctrl_mask].std(),
        outcome[treat_mask].std(),
        ctrl_mask.sum(),
        treat_mask.sum(),
        np.corrcoef(outcome, covariate)[0, 1],
        covariate.mean()
    ], dtype=np.float32)

# 3. Define prior (matches design priors)
def sample_prior(rng):
    """Sample parameters from prior."""
    # student_t(3, 0, 1) approximated as normal for simplicity
    b_arm_treat = rng.normal(0, 1)
    return {"b_arm_treat": np.array([b_arm_treat], dtype=np.float32)}

# 4. Build BayesFlow workflow
workflow = bf.BasicWorkflow(
    simulator=bf.simulators.make_simulator(
        prior_fn=sample_prior,
        likelihood_fn=simulate_ancova_2arms,
        summary_fn=compute_summaries
    ),
    inference_network=bf.networks.FlowMatching(),
    inference_variables=["b_arm_treat"],
    summary_variables=["summaries"]
)

# 5. Train
history = workflow.fit_online(epochs=50, batch_size=64, num_batches_per_epoch=200)

# 6. Save
import pickle
with open("ancova_cont_2arms.pkl", "wb") as f:
    pickle.dump(workflow, f)
```

---

## Phase 2: Testing Strategy

### Unit Tests

1. **`test-backend_bf.R`** - Test with mock mode enabled
   ```r
   withr::with_envvar(c(RCTBP_MOCK_BF = "TRUE"), {
     # Test estimate_batch_bf() returns correct dimensions
     # Test estimate_single_bf() handles single and batch data
     # Test compute_summaries_batch_ancova() computes correct stats
   })
   ```

2. **`test-model_cache.R`** - Test caching without downloads
   ```r
   # Test get_model_cache_dir() creates directories
   # Test list_models() returns correct structure
   # Test clear_model_cache() removes files
   ```

### Integration Tests (Requires Python)

1. **Posterior Calibration:** Compare brms vs BayesFlow posteriors on same data
2. **SBC (Simulation-Based Calibration):** Validate BayesFlow coverage
3. **Performance Benchmark:** Time comparison brms vs BayesFlow

### Mock Mode Usage

```r
# Enable mock mode for testing
Sys.setenv(RCTBP_MOCK_BF = "TRUE")

# Run tests - BayesFlow calls return mock samples based on data
result <- power_analysis(conditions, n_sims = 10, n_cores = 1)

# Disable mock mode
Sys.setenv(RCTBP_MOCK_BF = "")
```

---

## Phase 3: Power Analysis Orchestration

### Batch Processing Strategy

When `backend = "bf"`:
1. Group work units by `n_total` (homogeneous batches)
2. Process each batch with single BayesFlow forward pass
3. Distribute batches (not individual sims) across cores

```r
# Current worker_process_batch() already implements this:
worker_process_batch(work_units, design)
# - Simulates data for all work units
# - Calls estimate_single_bf() with list of data.frames
# - Returns combined results
```

---

## Remaining Tasks

### Short-term (Python Setup)
- [ ] Set up Python environment with BayesFlow 2.0
- [ ] Create training script for 2-arm ANCOVA
- [ ] Train minimal model and export as `.pkl`
- [ ] Test R↔Python round-trip with trained model

### Medium-term (Integration)
- [ ] Upload trained models to GitHub releases
- [ ] Create unit tests with mock mode
- [ ] Add `check_bf_available()` to model class validator
- [ ] Add automatic brms fallback when BayesFlow unavailable

### Long-term (Optimization)
- [ ] Train models for 3-arm ANCOVA
- [ ] Benchmark BayesFlow vs brms performance
- [ ] GPU support for large-scale simulations
- [ ] Variable sample size support in BayesFlow models

---

## Answered Questions

1. **BayesFlow 2.0 vs 1.x:** Targeting BayesFlow 2.0 API
   - Uses `BasicWorkflow.sample(conditions, num_samples)`
   - Or `Approximator.sample(data, num_samples)`

2. **Summary Statistics:** 8-dimensional vector sufficient for 2-arm ANCOVA
   - Matches between R (`compute_summaries_batch_ancova`) and Python training

3. **Variable Sample Sizes:** Group by `n_total` into homogeneous batches
   - Different sample sizes require separate BayesFlow calls

4. **Mock Mode:** Implemented via `RCTBP_MOCK_BF` environment variable
   - Generates data-driven mock samples for testing

5. **Model Versioning:** GitHub releases with tags `bf-models-v1`
   - Models downloaded on first use via `load_bf_model()`

---

## File Summary

| File | Purpose | Lines |
|------|---------|-------|
| `R/backend_bf.R` | BayesFlow estimation functions | ~700 |
| `R/backend_brms.R` | brms estimation functions | ~200 |
| `R/model_cache.R` | Model download and caching | ~330 |
| `R/utils_results.R` | Shared result utilities | ~45 |
| `R/class_model.R` | Dual-backend model class | ~600 |
| `R/worker_functions.R` | Parallel worker dispatch | ~380 |
