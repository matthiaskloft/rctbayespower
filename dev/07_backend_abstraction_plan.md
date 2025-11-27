# Backend Abstraction Plan

**Created:** 2024-11-24
**Status:** ✅ IMPLEMENTED (2025-11-27)
**Implementation:** See `dev/10_bayesflow_integration_roadmap.md` for details

---

## Summary

This plan has been **fully implemented**. The backend abstraction layer now supports dual backends (brms + BayesFlow) with the following architecture:

### Implemented Components

| Component | File | Status |
|-----------|------|--------|
| brms backend | `R/backend_brms.R` | ✅ Complete |
| BayesFlow backend | `R/backend_bf.R` | ✅ Complete (reticulate) |
| Model caching | `R/model_cache.R` | ✅ Complete |
| Dual-backend model class | `R/class_model.R` | ✅ Complete |
| Worker dispatch | `R/worker_functions.R` | ✅ Complete |
| Batch simulation | `R/models_ancova.R` | ✅ Complete |
| Shared utilities | `R/utils_results.R` | ✅ Complete |

### Key Design Decisions (Implemented)

1. **Backend naming**: "bf" for BayesFlow (extensible for future NPE backends)
2. **Dual backend support**: Model holds BOTH brms and BayesFlow simultaneously
3. **Backend selection**: "brms", "bf", or "auto" (auto prefers bf if available)
4. **Python integration**: reticulate for BayesFlow model loading and inference
5. **Mock mode**: `RCTBP_MOCK_BF=TRUE` for testing without Python
6. **Summary statistics**: 8-dimensional vector for 2-arm ANCOVA

### What Changed from Original Plan

| Original Plan | Actual Implementation |
|---------------|----------------------|
| Single `estimation_model` property | Separate `brms_model` and `bayesflow_model` |
| "npe" backend name | "bf" backend name |
| `R/backends.R` dispatcher | Backend-specific files + worker dispatch |
| `compute_measures()` for both | Backend-specific summarization for speed |
| keras R package | reticulate for full BayesFlow compatibility |

### Next Steps

The R infrastructure is complete. Remaining work:

1. **Python**: Create BayesFlow training script
2. **Python**: Train models for 2-arm and 3-arm ANCOVA
3. **Integration**: Upload trained models to GitHub releases
4. **Testing**: Add unit tests with mock mode

See `dev/10_bayesflow_integration_roadmap.md` for detailed next steps.

---

## Original Plan (Archived)

The content below is the original plan from 2024-11-24. It has been superseded by the actual implementation documented above and in `10_bayesflow_integration_roadmap.md`.

<details>
<summary>Click to expand original plan</summary>

### Overview

Refactor rctbayespower to support multiple posterior estimation backends (brms, NPE/keras) and efficient interim analyses with parallelization. The key insight: convert all posterior samples to rvars (posterior package) for backend-agnostic computation.

### Core Design Principles

1. **Backend Agnostic Computation:** After converting posteriors to rvars, all downstream computations are identical regardless of backend
2. **Efficient Batching:** NPE processes multiple simulations in a single forward pass
3. **Parallel Interim Analysis:** All simulations at each interim timepoint are processed together
4. **Backward Compatibility:** Existing brms workflows remain unchanged

[... rest of original plan truncated for brevity ...]

</details>
