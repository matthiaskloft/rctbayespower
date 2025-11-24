# Backend Abstraction Implementation Progress

**Last Updated:** 2025-11-24
**Status:** Phase 1-8 Complete, Phase 9 (Adaptive) Deferred, Testing Phase Remaining

## Completed ✅

### Phase 1: Model Class Updates
**File: R/class_model.R**
- ✅ Added `bayesflow_model` property (for NPE/keras models)
- ✅ Added `backend_args` property (list for backend configuration)
- ✅ Created `backend` as getter (auto-detects "brms" or "npe")
- ✅ Validator enforces exactly ONE model (brms XOR bayesflow)
- ✅ Updated `build_model()` to accept both model types
- ✅ Updated print method to show backend and args

### Phase 2: Backend Abstraction Layer
**File: R/backends.R**
- ✅ `extract_posterior_rvars_brms()` - Extract rvars from brmsfit
- ✅ `extract_posterior_rvars_npe()` - Extract rvars from NPE arrays
- ✅ `estimate_posterior_brms()` - Fit brms models
- ✅ `estimate_posterior_npe()` - Generate posteriors via NPE (stub)
- ✅ `estimate_posterior()` - Generic dispatcher
- ✅ `extract_posterior_rvars()` - Generic dispatcher
- ✅ Helper stubs for NPE data preparation (user must implement)

### Phase 3: Backend-Agnostic Computation
**File: R/compute_measures.R**
- ✅ Created `compute_measures(posterior_rvars, design)` - works on rvars
- ✅ Updated to use rvar operations throughout
- ✅ Created `compute_measures_brmsfit()` wrapper for backward compatibility

### Phase 4: Core Integration
**File: R/simulate_single_run.R**
- ✅ Updated to use `id_iter` instead of `id_sim`
- ✅ Added `id_analysis` column (set to 0 for single analysis)
- ✅ Backend detection from model
- ✅ Uses `estimate_posterior()` dispatcher
- ✅ Extracts rvars via `extract_posterior_rvars()`
- ✅ Calls backend-agnostic `compute_measures()`

### Phase 5: Estimation Strategy Matrix
**File: R/estimation_single.R** ✅
- ✅ `estimate_single_brms()` - Single analysis with brms
- ✅ `estimate_single_npe()` - Single analysis with NPE (supports batching)
- ✅ `create_error_result()` - Helper for standardized errors

**File: R/estimation_sequential.R** ✅
- ✅ `estimate_sequential_brms()` - Sequential interim analyses with brms
- ✅ `estimate_sequential_npe()` - Batched sequential analyses with NPE
- ✅ Both handle interim decisions and tracking

### Phase 6: Worker Functions
**File: R/worker_functions.R** ✅
- ✅ `worker_process_single()` - Processes one (id_cond, id_iter) unit
- ✅ `worker_process_batch()` - Processes batch of units for NPE
- ✅ Both detect strategy from design (single vs sequential)
- ✅ Both call appropriate estimation function
- ✅ Return consistent structure with (id_cond, id_iter, id_analysis)
- ✅ `prepare_design_for_workers()` - Serializes design for parallel workers

### Phase 7: Power Analysis Dispatcher
**File: R/class_power_analysis.R** ✅
- ✅ Expand work units: conditions × iterations (NOT interims!)
- ✅ Detect backend and batch_size from model
- ✅ Choose between single/batch worker
- ✅ Distribute work via pbapply
- ✅ Flatten results with dplyr::bind_rows
- ✅ Handle design serialization for parallel workers via prepare_design_for_workers()
- ✅ Export all necessary functions to parallel workers
- ✅ Update brms_args handling to only apply to brms backend
- ✅ Enhanced verbose output showing backend, batch size, and interim info

### Phase 8: Design Class Updates
**File: R/class_design.R** ✅
- ✅ Added `analysis_at` property (vector of interim sample sizes)
- ✅ Added `interim_function` property (function for decisions)
- ✅ Added `adaptive` property (logical, default FALSE)
- ✅ Updated validators to check analysis_at monotonically increasing
- ✅ Validator ensures interim_function present when analysis_at specified
- ✅ Updated build_design() constructor with new parameters
- ✅ Updated print method to show interim analysis configuration

## In Progress / Remaining 🚧

### Phase 9: Adaptive Strategies (DEFERRED)
**File: R/estimation_adaptive.R** (TODO - complex, plan with Opus)
- `estimate_adaptive_brms()` - Adaptive with parameter modification
- `estimate_adaptive_npe()` - Batched adaptive (very complex)
- Requires data re-simulation logic
- Needs careful design for parameter tracking

## Key Design Decisions

### ID Structure
```r
raw_results <- tibble(
  id_cond,        # Condition identifier
  id_iter,        # Iteration within condition (resets per condition)
  id_analysis,    # Analysis step (0 = single, 1+ = interims)
  # ... measures ...
  n_analyzed,     # Sample size at this analysis
  stopped,        # Logical, has trial stopped?
  stop_reason,    # "stop_success", "stop_futility", NA
  interim_decision, # List-column with decision details
  converged, error
)
```
Primary key: `(id_cond, id_iter, id_analysis)`

### Estimation Strategy Matrix
|                  | brms                      | npe                         |
|------------------|---------------------------|-----------------------------|
| **Single**       | estimate_single_brms()    | estimate_single_npe()       |
| **Sequential**   | estimate_sequential_brms()| estimate_sequential_npe()   |
| **Adaptive**     | estimate_adaptive_brms()  | estimate_adaptive_npe()     |

### Work Unit Structure
- Work units = conditions × iterations ONLY
- Interims handled WITHIN each work unit
- Each worker gets (id_cond, id_iter) assignments
- Full dataset simulated once per unit
- Sequential analyses loop through analysis_at
- NPE batches across simulations at each interim step

### Batching Strategy
- Batch size in `model@backend_args$batch_size`
- Only NPE uses batching (brms always single)
- Work units grouped into batches
- Batch worker calls `estimate_*_npe()` with lists
- Results flattened back to per-simulation rows

## Next Steps

1. ✅ ~~Create `worker_functions.R` with both worker types~~
2. ✅ ~~Update `run.rctbp_power_analysis()` in `class_power_analysis.R`~~
3. ✅ ~~Add interim properties to design class~~
4. 🚧 Test with brms backend (should work with existing models)
5. Test sequential/interim logic with sample interim function
6. Prepare for NPE testing (when model available)
7. Update any affected unit tests
8. Update roxygen documentation if needed

## Testing Strategy

### Unit Tests
- Test each estimation function with mock data
- Test worker functions with small examples
- Test ID uniqueness and joining

### Integration Tests
- Full pipeline with brms backend
- Sequential analyses with stopping rules
- Batch processing with mock NPE

### Backward Compatibility
- Existing brms code should work unchanged
- `simulate_single_run()` still works for single analyses
- `compute_measures_brmsfit()` wrapper maintains compatibility

## Files Modified

✅ R/class_model.R (updated with backend properties)
✅ R/backends.R (new - abstraction layer)
✅ R/compute_measures.R (refactored for rvars)
✅ R/simulate_single_run.R (updated for new ID structure)
✅ R/estimation_single.R (new - single analysis strategies)
✅ R/estimation_sequential.R (new - sequential analysis strategies)
✅ R/worker_functions.R (new - work unit processing)
✅ R/class_power_analysis.R (updated - dispatcher logic)
✅ R/class_design.R (updated - interim analysis properties)
📋 R/estimation_adaptive.R (future, complex - deferred)
