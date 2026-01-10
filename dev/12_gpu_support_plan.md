# GPU Support Implementation Plan

## Overview

This document outlines the implementation plan for GPU acceleration in the BayesFlow backend. GPU support can dramatically improve inference speed for neural posterior estimation, especially with large batch sizes.

**Current State:** BayesFlow backend works but defaults to CPU-only PyTorch backend with no GPU detection or configuration.

**Goal:** Enable automatic GPU detection and utilization with clear user feedback and easy configuration.

---

## Background Research Summary

### Keras 3 Multi-Backend Architecture

Keras 3 supports multiple backends ([Keras 3.0 announcement](https://keras.io/keras_3/)):
- **JAX**: Fastest for GPU/TPU, recommended by BayesFlow team
- **PyTorch**: Best Windows support, good CUDA integration
- **TensorFlow**: Legacy but stable
- **OpenVINO**: Inference-only

Key constraint: **Backend must be set BEFORE importing keras** via `KERAS_BACKEND` environment variable.

### GPU Support Requirements by Backend

| Backend | GPU Package | Verification Command |
|---------|-------------|---------------------|
| JAX | `jax[cuda12]` or `jax[cuda13]` | `jax.default_backend()` returns "gpu" |
| PyTorch | `torch` with CUDA | `torch.cuda.is_available()` returns True |
| TensorFlow | `tensorflow[and-cuda]` | `tf.config.list_physical_devices('GPU')` |

### Device Placement

- **JAX**: Automatic device placement, uses available GPU by default
- **PyTorch**: Requires explicit `.to("cuda")` for models/tensors
- **TensorFlow**: Automatic device placement with GPU memory growth option

---

## Implementation Phases

### Phase 1: GPU Detection and Status Reporting

**Goal:** Let users know what GPU capabilities are available.

#### 1.1 New Function: `check_gpu_available()`

```r
#' Check GPU Availability for BayesFlow
#'
#' Detects available GPU devices across supported backends (JAX, PyTorch, TensorFlow).
#' Returns detailed information about GPU configuration.
#'
#' @param backend Which backend to check: "auto", "jax", "torch", "tensorflow"
#' @param verbose Print detailed information (default TRUE)
#'
#' @return List with components:
#'   - available: logical, TRUE if any GPU is available
#'   - backend: character, which backend has GPU support
#'   - device_name: character, GPU device name(s)
#'   - device_count: integer, number of GPUs
#'   - memory_gb: numeric, total GPU memory in GB (if detectable
#'
#' @export
check_gpu_available <- function(backend = "auto", verbose = TRUE) {
 # Implementation details below
}
```

#### 1.2 Implementation Logic

```r
check_gpu_available <- function(backend = "auto", verbose = TRUE) {
  result <- list(
    available = FALSE,
    backend = NA_character_,
    device_name = NA_character_,
    device_count = 0L,
    memory_gb = NA_real_
  )

  # Check if Python/reticulate available
if (!check_bf_available(silent = TRUE)) {
    if (verbose) cli::cli_alert_warning("Python/BayesFlow not available")
    return(result)
  }

  backends_to_check <- if (backend == "auto") {
    c("jax", "torch", "tensorflow")
  } else {
    backend
  }

  for (be in backends_to_check) {
    gpu_info <- tryCatch({
      switch(be,
        jax = check_jax_gpu(),
        torch = check_torch_gpu(),
        tensorflow = check_tf_gpu()
      )
    }, error = function(e) NULL)

    if (!is.null(gpu_info) && gpu_info$available) {
      result <- gpu_info
      result$backend <- be
      break
    }
  }

  if (verbose) {
    if (result$available) {
      cli::cli_alert_success("GPU available: {result$device_name}")
      cli::cli_alert_info("Backend: {result$backend} | Devices: {result$device_count}")
    } else {
      cli::cli_alert_warning("No GPU detected - using CPU")
      cli::cli_alert_info("For GPU support, install: pip install jax[cuda12] or torch with CUDA")
    }
  }

  result
}
```

#### 1.3 Backend-Specific GPU Checks

```r
# JAX GPU check
check_jax_gpu <- function() {
  jax <- reticulate::import("jax", delay_load = TRUE)
  backend <- jax$default_backend()

  if (backend == "gpu") {
    devices <- jax$devices()
    device_info <- devices[[1]]
    list(
      available = TRUE,
      device_name = as.character(device_info$device_kind),
      device_count = length(devices),
      memory_gb = NA_real_  # JAX doesn't easily expose this
    )
  } else {
    list(available = FALSE)
  }
}

# PyTorch GPU check
check_torch_gpu <- function() {
  torch <- reticulate::import("torch", delay_load = TRUE)

  if (torch$cuda$is_available()) {
    device_count <- torch$cuda$device_count()
    device_name <- torch$cuda$get_device_name(0L)
    # Get memory in GB
    props <- torch$cuda$get_device_properties(0L)
    memory_gb <- props$total_memory / (1024^3)

    list(
      available = TRUE,
      device_name = device_name,
      device_count = as.integer(device_count),
      memory_gb = memory_gb
    )
  } else {
    list(available = FALSE)
  }
}

# TensorFlow GPU check
check_tf_gpu <- function() {
  tf <- reticulate::import("tensorflow", delay_load = TRUE)
  gpus <- tf$config$list_physical_devices("GPU")

  if (length(gpus) > 0) {
    # Get device name from first GPU
    device_name <- tryCatch({
      gpus[[1]]$name
    }, error = function(e) "Unknown GPU")

    list(
      available = TRUE,
      device_name = device_name,
      device_count = length(gpus),
      memory_gb = NA_real_  # Requires experimental API
    )
  } else {
    list(available = FALSE)
  }
}
```

---

### Phase 2: Backend Selection Enhancement

**Goal:** Allow users to choose backend and auto-select based on GPU availability.

#### 2.1 Enhanced `init_bf_python()` Function

Modify the existing function to support backend selection:

```r
#' Initialize BayesFlow Python Environment
#'
#' @param backend Keras backend: "auto", "jax", "torch", "tensorflow" (default "auto")
#' @param device Device to use: "auto", "gpu", "cpu" (default "auto")
#' @param verbose Print configuration messages (default TRUE)
#'
#' @details
#' When backend="auto":
#' 1. If GPU available: prefer JAX > PyTorch > TensorFlow
#' 2. If CPU only: use PyTorch (best compatibility)
#'
#' When device="auto":
#' 1. Use GPU if available
#' 2. Fall back to CPU
#'
#' @return List with bf, np, keras Python modules
#' @export
init_bf_python <- function(backend = "auto", device = "auto", verbose = TRUE) {
  # Check cache - but respect new backend/device settings
  cache_key <- paste(backend, device, sep = "_")
  if (exists("cache_key", envir = .bf_cache) &&
      .bf_cache$cache_key == cache_key &&
      !is.null(.bf_cache$bf)) {
    return(list(bf = .bf_cache$bf, np = .bf_cache$np, keras = .bf_cache$keras))
  }

  # Ensure availability
  check_bf_available(silent = FALSE)

  # Resolve backend
  resolved_backend <- resolve_keras_backend(backend, device, verbose)

  # Set KERAS_BACKEND before importing
  os <- reticulate::import("os")
  os$environ$`__setitem__`("KERAS_BACKEND", resolved_backend)

  if (verbose) {
    cli::cli_alert_info("Set KERAS_BACKEND={resolved_backend}")
  }

  # Import modules
  .bf_cache$bf <- reticulate::import("bayesflow", delay_load = TRUE)
  .bf_cache$np <- reticulate::import("numpy", convert = FALSE, delay_load = TRUE)
  .bf_cache$keras <- reticulate::import("keras", delay_load = TRUE)
  .bf_cache$cache_key <- cache_key

  # Report device status
  if (verbose) {
    gpu_info <- check_gpu_available(backend = resolved_backend, verbose = FALSE)
    if (gpu_info$available) {
      cli::cli_alert_success("Using GPU: {gpu_info$device_name}")
    } else {
      cli::cli_alert_info("Using CPU (no GPU detected for {resolved_backend} backend)")
    }
  }

  list(bf = .bf_cache$bf, np = .bf_cache$np, keras = .bf_cache$keras)
}
```

#### 2.2 Backend Resolution Logic

```r
#' Resolve Keras Backend Based on Preferences and Availability
#'
#' @param backend User preference: "auto", "jax", "torch", "tensorflow"
#' @param device Device preference: "auto", "gpu", "cpu"
#' @param verbose Print resolution messages
#'
#' @return Character: resolved backend name
#' @keywords internal
resolve_keras_backend <- function(backend, device, verbose = TRUE) {
  # If user specified explicit backend, use it
  if (backend != "auto") {
    return(backend)
  }

  # Auto-selection based on device preference
  if (device == "cpu") {
    # PyTorch has best CPU compatibility
    return("torch")
  }

  # Check GPU availability for each backend (preference order: jax > torch > tf)
  for (be in c("jax", "torch", "tensorflow")) {
    gpu_info <- tryCatch({
      switch(be,
        jax = check_jax_gpu(),
        torch = check_torch_gpu(),
        tensorflow = check_tf_gpu()
      )
    }, error = function(e) list(available = FALSE))

    if (gpu_info$available) {
      if (verbose) {
        cli::cli_alert_info("Auto-selected {be} backend (GPU available)")
      }
      return(be)
    }
  }

  # No GPU found, default to PyTorch
  if (verbose) {
    cli::cli_alert_info("Auto-selected torch backend (no GPU found)")
  }
  return("torch")
}
```

---

### Phase 3: Configuration System

**Goal:** Provide multiple ways to configure BayesFlow GPU settings.

#### 3.1 R Options System

```r
# Users can set options in .Rprofile or at runtime:
options(
  rctbayespower.keras_backend = "jax",      # "auto", "jax", "torch", "tensorflow"
  rctbayespower.device = "gpu",              # "auto", "gpu", "cpu"
  rctbayespower.gpu_memory_fraction = 0.9,   # Fraction of GPU memory to use
  rctbayespower.mixed_precision = FALSE      # Enable FP16 for faster inference
)
```

#### 3.2 Get Configuration Function

```r
#' Get BayesFlow Configuration
#'
#' Returns current BayesFlow configuration from R options and environment variables.
#'
#' @return Named list with configuration values
#' @export
get_bf_config <- function() {
  list(
    keras_backend = getOption("rctbayespower.keras_backend",
                              Sys.getenv("KERAS_BACKEND", "auto")),
    device = getOption("rctbayespower.device", "auto"),
    gpu_memory_fraction = getOption("rctbayespower.gpu_memory_fraction", 0.9),
    mixed_precision = getOption("rctbayespower.mixed_precision", FALSE)
  )
}
```

#### 3.3 Set Configuration Function

```r
#' Configure BayesFlow Settings
#'
#' Sets BayesFlow configuration options. Must be called before first use of
#' BayesFlow functions in a session.
#'
#' @param keras_backend Backend: "auto", "jax", "torch", "tensorflow"
#' @param device Device: "auto", "gpu", "cpu"
#' @param gpu_memory_fraction Fraction of GPU memory to use (0-1)
#' @param mixed_precision Use FP16 for faster inference (experimental)
#'
#' @export
configure_bf <- function(keras_backend = NULL,
                         device = NULL,
                         gpu_memory_fraction = NULL,
                         mixed_precision = NULL) {

  if (!is.null(keras_backend)) {
    match.arg(keras_backend, c("auto", "jax", "torch", "tensorflow"))
    options(rctbayespower.keras_backend = keras_backend)
  }

  if (!is.null(device)) {
    match.arg(device, c("auto", "gpu", "cpu"))
    options(rctbayespower.device = device)
  }

  if (!is.null(gpu_memory_fraction)) {
    stopifnot(gpu_memory_fraction > 0 && gpu_memory_fraction <= 1)
    options(rctbayespower.gpu_memory_fraction = gpu_memory_fraction)
  }

  if (!is.null(mixed_precision)) {
    stopifnot(is.logical(mixed_precision))
    options(rctbayespower.mixed_precision = mixed_precision)
  }

  # Clear cache to force re-initialization
  if (exists("bf", envir = .bf_cache)) {
    rm(list = ls(envir = .bf_cache), envir = .bf_cache)
    cli::cli_alert_info("BayesFlow cache cleared - new settings will apply on next use")
  }

  invisible(get_bf_config())
}
```

---

### Phase 4: Integration with Power Analysis

**Goal:** Seamlessly use GPU in power analysis workflow.

#### 4.1 Update `power_analysis()` Output

Add GPU status to the configuration output:

```r
# In run() method for rctbp_power_analysis, add to status display:
if (backend == "bf") {
  gpu_info <- check_gpu_available(verbose = FALSE)
  if (gpu_info$available) {
    cli::cli_alert_success("GPU: {gpu_info$device_name} ({gpu_info$backend} backend)")
  } else {
    cli::cli_alert_info("Device: CPU ({get_bf_config()$keras_backend} backend)")
  }
}
```

#### 4.2 Batch Size Recommendations

```r
#' Get Recommended Batch Size for BayesFlow
#'
#' Returns recommended batch size based on available GPU memory.
#'
#' @param model_memory_mb Approximate memory per sample in MB (default 10)
#'
#' @return Integer recommended batch size
#' @export
get_recommended_batch_size <- function(model_memory_mb = 10) {
  gpu_info <- check_gpu_available(verbose = FALSE)

  if (!gpu_info$available || is.na(gpu_info$memory_gb)) {
    # CPU default
    return(256L)
  }

  # Use 80% of GPU memory for batching
  available_mb <- gpu_info$memory_gb * 1024 * 0.8
  batch_size <- as.integer(available_mb / model_memory_mb)

  # Clamp to reasonable range
  batch_size <- max(64L, min(batch_size, 4096L))

  cli::cli_alert_info("Recommended batch size: {batch_size} (based on {round(gpu_info$memory_gb, 1)} GB GPU memory)")

  batch_size
}
```

---

### Phase 5: Documentation and Testing

#### 5.1 Vignette: GPU Setup Guide

Create `vignettes/articles/gpu-setup.qmd`:

```markdown
# GPU Setup for BayesFlow Backend

## Quick Start

1. Check GPU availability:
   ```r
   library(rctbayespower)
   check_gpu_available()
   ```

2. Configure backend (optional):
   ```r
   configure_bf(keras_backend = "jax", device = "gpu")
   ```

3. Run power analysis (GPU used automatically):
   ```r
   result <- power_analysis(conditions, n_sims = 1000)
   ```

## Backend Comparison

| Backend | GPU Support | Speed | Compatibility |
|---------|-------------|-------|---------------|
| JAX | Excellent | Fastest | Linux best |
| PyTorch | Good | Fast | All platforms |
| TensorFlow | Good | Medium | All platforms |

## Troubleshooting

### JAX GPU not detected
```bash
pip install --upgrade "jax[cuda12]"
```

### PyTorch GPU not detected
```bash
pip install torch --index-url https://download.pytorch.org/whl/cu121
```

### Verifying GPU in Python
```python
import jax
print(jax.default_backend())  # Should print "gpu"

import torch
print(torch.cuda.is_available())  # Should print True
```
```

#### 5.2 Unit Tests

```r
# tests/testthat/test-gpu.R

test_that("check_gpu_available returns correct structure", {
  skip_if_not(check_bf_available(silent = TRUE))

  result <- check_gpu_available(verbose = FALSE)

  expect_type(result, "list")
  expect_named(result, c("available", "backend", "device_name", "device_count", "memory_gb"))
  expect_type(result$available, "logical")
})

test_that("configure_bf validates inputs", {
  expect_error(configure_bf(keras_backend = "invalid"))
  expect_error(configure_bf(device = "invalid"))
  expect_error(configure_bf(gpu_memory_fraction = 1.5))
})

test_that("get_bf_config returns current settings", {
  result <- get_bf_config()
  expect_type(result, "list")
  expect_named(result, c("keras_backend", "device", "gpu_memory_fraction", "mixed_precision"))
})
```

---

## Implementation Order

### Sprint 1: GPU Detection (1-2 days)
1. [ ] Add `check_gpu_available()` function
2. [ ] Add backend-specific GPU check helpers
3. [ ] Add unit tests for GPU detection
4. [ ] Test on GPU and non-GPU machines

### Sprint 2: Backend Selection (1-2 days)
1. [ ] Update `init_bf_python()` with backend/device parameters
2. [ ] Add `resolve_keras_backend()` helper
3. [ ] Update Python dependency declaration for JAX
4. [ ] Test backend switching

### Sprint 3: Configuration System (1 day)
1. [ ] Add `configure_bf()` function
2. [ ] Add `get_bf_config()` function
3. [ ] Integrate with existing `init_bf_python()`
4. [ ] Document R options

### Sprint 4: Integration (1 day)
1. [ ] Update `power_analysis()` to show GPU status
2. [ ] Add `get_recommended_batch_size()`
3. [ ] Update CLAUDE.md with GPU info

### Sprint 5: Documentation (1 day)
1. [ ] Create GPU setup vignette
2. [ ] Update package documentation
3. [ ] Add troubleshooting guide

---

## Dependencies

### Python Package Updates

Current `py_require()`:
```r
c("bayesflow>=2.0", "keras>=3.0", "torch", "numpy")
```

Updated for JAX GPU support:
```r
# For auto-selection, install all backends:
c("bayesflow>=2.0", "keras>=3.0", "torch", "numpy", "jax[cuda12]", "tensorflow")

# Or minimal JAX-only:
c("bayesflow>=2.0", "keras>=3.0", "jax[cuda12]", "numpy")
```

### Environment Variables

| Variable | Purpose | Values |
|----------|---------|--------|
| `KERAS_BACKEND` | Keras backend selection | "jax", "torch", "tensorflow" |
| `CUDA_VISIBLE_DEVICES` | GPU device selection | "0", "0,1", etc. |
| `XLA_PYTHON_CLIENT_PREALLOCATE` | JAX memory allocation | "false" |
| `TF_FORCE_GPU_ALLOW_GROWTH` | TensorFlow memory growth | "true" |

---

## Risk Assessment

| Risk | Mitigation |
|------|------------|
| CUDA version mismatch | Document supported versions, provide troubleshooting |
| Backend not installed | Graceful fallback with informative message |
| GPU out of memory | Auto batch size recommendation, memory fraction config |
| Windows JAX issues | Default to PyTorch on Windows |
| Model incompatibility | Models trained on one backend may not work on another |

---

## Success Metrics

1. **User Experience**: `check_gpu_available()` provides clear status
2. **Performance**: GPU inference at least 5x faster than CPU
3. **Reliability**: Graceful fallback when GPU unavailable
4. **Documentation**: Users can set up GPU in < 10 minutes

---

## References

- [Keras 3 Getting Started](https://keras.io/getting_started/)
- [JAX Installation](https://docs.jax.dev/en/latest/installation.html)
- [BayesFlow Documentation](https://bayesflow.org)
- [Keras Multi-GPU with PyTorch](https://keras.io/guides/distributed_training_with_torch/)
- [Keras Multi-GPU with JAX](https://keras.io/guides/distributed_training_with_jax/)
