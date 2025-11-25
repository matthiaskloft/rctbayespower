# Backend Abstraction & Interim Analysis Refactoring Plan

**Created:** 2024-11-24
**Status:** Planning Phase
**Estimated Time:** 20-25 hours total

## Overview

Refactor rctbayespower to support multiple posterior estimation backends (brms, NPE/keras) and efficient interim analyses with parallelization. The key insight: convert all posterior samples to rvars (posterior package) for backend-agnostic computation.

## Core Design Principles

1. **Backend Agnostic Computation:** After converting posteriors to rvars, all downstream computations are identical regardless of backend
2. **Efficient Batching:** NPE processes multiple simulations in a single forward pass
3. **Parallel Interim Analysis:** All simulations at each interim timepoint are processed together
4. **Backward Compatibility:** Existing brms workflows remain unchanged

## Part 1: Backend Abstraction Layer

### 1.1 Update rctbp_model Class (R/class_model.R)

**Add properties:**
- `backend = S7::new_property(S7::class_character, default = "brms")` with validator for c("brms", "npe")
- `estimation_model = S7::class_any` (holds either brmsfit or keras model object)

**Backward compatibility:**
- Keep `brms_model` as alias pointing to `estimation_model` during transition period

**Validator update:**
```r
if (self@backend == "brms" && !inherits(self@estimation_model, "brmsfit")) {
  return("For backend='brms', estimation_model must be a brmsfit object")
}
if (self@backend == "npe" && !inherits(self@estimation_model, "keras.engine.training.Model")) {
  return("For backend='npe', estimation_model must be a keras model")
}
```

### 1.2 Create Backend Abstraction Interface (NEW: R/backends.R)

```r
# Generic estimation function that dispatches based on backend
estimate_posterior <- function(data, model, backend = "brms", backend_args = list()) {
  switch(backend,
    brms = estimate_posterior_brms(data, model, backend_args),
    npe = estimate_posterior_npe(data, model, backend_args),
    stop("Unknown backend: ", backend)
  )
}

# Backend-specific implementations
estimate_posterior_brms <- function(data, brms_model, backend_args = list()) {
  # brms_model is a brmsfit object
  # backend_args can contain chains, iter, cores, etc.
  do.call(stats::update, c(list(object = brms_model, newdata = data), backend_args))
}

estimate_posterior_npe <- function(data_batch, keras_model, backend_args = list()) {
  # keras_model is a trained neural network
  # backend_args contains batch_size, n_posterior_samples, etc.
  # data_batch can be a single dataset or stacked batch

  # Forward pass through neural network
  posterior_params <- keras_model$predict(data_batch)

  # Sample from the posterior distribution
  n_samples <- backend_args$n_posterior_samples %||% 4000
  posterior_samples <- sample_from_npe_output(posterior_params, n_samples)

  # Returns array: [batch_size, n_posterior_samples, n_parameters]
  return(posterior_samples)
}

# Unified posterior extraction
extract_posterior_rvars <- function(estimation_result, backend, target_params) {
  switch(backend,
    brms = brms::as_draws_rvars(estimation_result, variable = target_params),
    npe = convert_npe_to_rvars(estimation_result, target_params),
    stop("Unknown backend: ", backend)
  )
}

# Helper for NPE to rvar conversion
convert_npe_to_rvars <- function(npe_array, target_params) {
  # npe_array is [batch_size, n_samples, n_params] or [n_samples, n_params]
  # Convert to posterior::rvar format
  # Returns list of rvars, one per parameter
}
```

### 1.3 Refactor compute_measures_brmsfit (R/compute_measures.R)

**Changes:**
- Rename to: `compute_measures()` (backend-agnostic)
- Change signature: `compute_measures(posterior_rvars, design)`
- Remove brms dependencies: Replace all brms::as_draws_* calls with rvar operations
- Key change: Accept rvars directly instead of brmsfit object

```r
compute_measures <- function(posterior_rvars, design) {
  # posterior_rvars is already extracted, works for any backend
  # All existing logic but using rvar operations instead of brms functions

  # Example change:
  # OLD: posterior_samples <- brms::as_draws_rvars(brmsfit, variable = param)
  # NEW: posterior_samples <- posterior_rvars[[param]]  # Already an rvar
}
```

### 1.4 Update simulate_single_run (R/simulate_single_run.R)

**Key changes:**
- Rename variable: `fitted_model` → `estimation_result`
- Add backend detection and routing
- Use new abstraction functions

```r
simulate_single_run <- function(condition_arguments, id_sim, design) {
  # Extract model components based on backend
  backend <- design@model@backend %||% "brms"  # Default for compatibility

  if (backend == "brms") {
    # Legacy path - maintain exact current behavior
    estimation_model <- design@model@brms_model %||% design@model@estimation_model
  } else {
    estimation_model <- design@model@estimation_model
  }

  # Simulate data (unchanged)
  simulated_data <- do.call(
    design@model@data_simulation_fn,
    condition_arguments$sim_args
  )

  # NEW: Backend-aware estimation
  estimation_result <- estimate_posterior(
    data = simulated_data,
    model = estimation_model,
    backend = backend,
    backend_args = condition_arguments$backend_args %||% list()
  )

  # Extract rvars for backend-agnostic processing
  posterior_rvars <- extract_posterior_rvars(
    estimation_result,
    backend,
    design@target_params
  )

  # Compute measures using rvars (backend-agnostic)
  result <- compute_measures(posterior_rvars, design)

  return(result)
}
```

## Part 2: NPE Batching Strategy

### 2.1 Create Batch Simulator (NEW: R/simulate_batch.R)

```r
simulate_batch <- function(condition_arguments, batch_size, design) {
  # Generate batch_size simulations at once
  simulated_batch <- lapply(1:batch_size, function(i) {
    do.call(design@model@data_simulation_fn, condition_arguments$sim_args)
  })

  # Stack for NPE processing if needed
  if (design@model@backend == "npe") {
    # Convert list of data.frames to 3D array or appropriate format
    return(stack_simulations_for_npe(simulated_batch))
  }

  return(simulated_batch)
}

# Helper to prepare data for keras model
stack_simulations_for_npe <- function(sim_list) {
  # Convert list of simulations to format expected by keras
  # Typically a 3D array: [batch_size, n_observations, n_features]
}
```

### 2.2 Refactor Parallel Execution (R/class_power_analysis.R)

```r
# In run.rctbp_power_analysis method
if (design@model@backend == "npe") {
  # NPE: Process in batches for efficiency
  batch_size <- x@backend_args$batch_size %||% 32
  n_batches <- ceiling(n_total_sims / batch_size)

  results_raw_list <- pbapply::pblapply(cl, 1:n_batches, function(batch_idx) {
    # Generate batch of simulations
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, n_total_sims)
    actual_batch_size <- end_idx - start_idx + 1

    batch_data <- simulate_batch(condition_args, actual_batch_size, design)

    # Single NPE forward pass for entire batch
    batch_posteriors <- estimate_posterior(
      batch_data,
      design@model@estimation_model,  # keras model
      "npe",
      x@backend_args
    )

    # Process each simulation in batch
    lapply(1:actual_batch_size, function(i) {
      # Extract rvars for simulation i
      posterior_rvars <- extract_single_simulation_rvars(batch_posteriors, i)

      # Compute measures (backend-agnostic)
      measures <- compute_measures(posterior_rvars, design)
      measures$id_sim <- start_idx + i - 1
      measures
    })
  }) %>% unlist(recursive = FALSE)

} else {
  # Original brms path (unchanged for compatibility)
  # ... existing pblapply code ...
}
```

## Part 3: Interim Analysis Parallelization

### 3.1 Update simulate_single_run for Interim Support

```r
simulate_single_run <- function(condition_arguments, id_sim, design) {
  # Check if interim analyses are specified
  if (!is.null(design@analysis_at)) {
    return(simulate_with_interim(condition_arguments, id_sim, design))
  }

  # ... existing single-timepoint logic ...
}

simulate_with_interim <- function(condition_arguments, id_sim, design) {
  analysis_at <- design@analysis_at
  n_total <- condition_arguments$sim_args$n_total
  all_looks <- c(analysis_at[analysis_at < n_total], n_total)

  results_by_look <- list()
  current_params <- condition_arguments$sim_args
  stopped <- FALSE

  for (look_idx in seq_along(all_looks)) {
    current_n <- all_looks[look_idx]

    # Generate or subset data
    if (design@adaptive || look_idx == 1) {
      # Adaptive or first look: generate fresh data
      current_params$n_total <- current_n
      simulated_data <- do.call(design@model@data_simulation_fn, current_params)
    } else {
      # Non-adaptive: subset to current_n
      simulated_data <- simulated_data[1:current_n, ]
    }

    # Estimate posterior (backend-agnostic)
    estimation_result <- estimate_posterior(
      simulated_data,
      design@model@estimation_model,
      design@model@backend,
      condition_arguments$backend_args
    )

    # Extract rvars and compute measures
    posterior_rvars <- extract_posterior_rvars(
      estimation_result,
      design@model@backend,
      design@target_params
    )

    measures <- compute_measures(posterior_rvars, design)
    measures$interim_look <- look_idx
    measures$n_analyzed <- current_n

    # Interim decision
    if (look_idx < length(all_looks) && !is.null(design@interim_function)) {
      decision <- design@interim_function(
        interim_summaries = measures,
        current_n = current_n,
        analysis_at = current_n,
        n_total = n_total
      )

      measures$interim_decision <- decision$decision

      if (decision$decision %in% c("stop_success", "stop_futility")) {
        stopped <- TRUE
        measures$stopped_at_n <- current_n
      }

      # Update parameters if adaptive
      if (design@adaptive && !is.null(decision$modified_params)) {
        current_params <- modifyList(current_params, decision$modified_params)
      }
    }

    results_by_look[[look_idx]] <- measures
  }

  # Combine results from all looks
  do.call(rbind, results_by_look)
}
```

### 3.2 Batch Processing for Interim with NPE

For NPE, we want to process all simulations at a given interim timepoint together:

```r
# In power_analysis parallel execution
if (design@model@backend == "npe" && !is.null(design@analysis_at)) {
  # Special path: batch all simulations by interim timepoint

  for (look_idx in seq_along(all_looks)) {
    # Generate all simulation data for this look
    all_sim_data <- lapply(1:n_sims, function(i) {
      generate_data_for_look(condition_args[[i]], look_idx)
    })

    # Batch process through NPE
    batch_posteriors <- estimate_posterior(
      stack_simulations_for_npe(all_sim_data),
      design@model@estimation_model,
      "npe",
      backend_args
    )

    # Extract results for each simulation
    for (i in 1:n_sims) {
      results[[i]][[look_idx]] <- extract_and_compute(batch_posteriors, i, design)
    }

    # Make interim decisions
    decisions <- lapply(results, function(r) {
      design@interim_function(r[[look_idx]], ...)
    })

    # Update parameters for adaptive designs
    # ...
  }
}
```

## Part 4: User Interface Updates

### 4.1 Update power_analysis() constructor

```r
power_analysis <- function(conditions, n_sims = 100, n_cores = 1,
                          backend_args = NULL, run = TRUE, ...) {
  # Create power analysis object with backend_args
  power_obj <- rctbp_power_analysis(
    conditions = conditions,
    n_sims = n_sims,
    n_cores = n_cores,
    backend_args = backend_args %||% list(),
    ...
  )

  if (run) {
    power_obj <- run(power_obj)
  }

  return(power_obj)
}
```

### 4.2 Add backend_args to rctbp_power_analysis class

```r
# In class definition
backend_args = S7::new_property(S7::class_list, default = list())
```

### 4.3 Example Usage

```r
# For brms (unchanged)
brms_model <- build_model("ancova_cont_2arms")()
design <- build_design(model = brms_model, ...)
results <- power_analysis(conditions, n_sims = 100)

# For NPE
npe_model <- build_model(
  data_simulation_fn = simulate_rct_data,
  estimation_model = trained_keras_model,  # Pre-trained neural network
  backend = "npe",
  n_endpoints = 1,
  endpoint_types = "continuous",
  n_arms = 2
)

design <- build_design(
  model = npe_model,
  target_params = c("treatment_effect"),
  ...
)

# Run with NPE-specific configuration
results <- power_analysis(
  conditions = conditions,
  n_sims = 1000,
  backend_args = list(
    batch_size = 64,
    n_posterior_samples = 4000
  )
)
```

## Part 5: Implementation Phases

### Phase 1: Foundation (3-4 hours)
- Create R/backends.R with abstraction functions
- Implement estimate_posterior() generic and backend-specific versions
- Create rvar conversion utilities

### Phase 2: Model Updates (2-3 hours)
- Update rctbp_model class with backend and estimation_model properties
- Add validators for backend-model consistency
- Create backward compatibility layer for brms_model

### Phase 3: Compute Refactor (3-4 hours)
- Refactor compute_measures_brmsfit to compute_measures
- Change from brmsfit input to rvar input
- Update all internal computations to use rvar operations

### Phase 4: Core Integration (3-4 hours)
- Update simulate_single_run with backend routing
- Add backend_args parameter flow
- Test both backends work end-to-end

### Phase 5: NPE Batching (4-5 hours)
- Implement simulate_batch functionality
- Create NPE batch processing in power_analysis
- Add efficient array stacking utilities

### Phase 6: Interim Analysis (4-5 hours)
- Implement simulate_with_interim function
- Add batch processing for interim timepoints with NPE
- Ensure parallel execution works correctly

### Phase 7: Testing & Documentation (3-4 hours)
- Create comprehensive test suite for both backends
- Update all documentation
- Create examples for NPE usage

## Key Technical Decisions

### Why rvars as common interface?
- Standard in Bayesian R ecosystem (posterior package)
- Rich set of operations already available
- Natural representation for posterior samples
- Efficient vectorized operations

### Why estimation_model holds both types?
- Simplifies user interface
- Clear 1:1 mapping between backend and model type
- Validator ensures consistency
- No need for separate model slots

### Why backend_args instead of model properties?
- NPE configuration may vary by analysis (batch_size, n_samples)
- Keeps model objects lightweight
- Allows runtime configuration without modifying models
- Cleaner separation of concerns

## Migration Strategy

1. **Maintain full backward compatibility** - existing code continues to work
2. **Deprecation warnings** after 1-2 releases for brms_model property
3. **Documentation** emphasizes new backend parameter
4. **Examples** show both old and new approaches initially

## Future Extensions

This architecture makes it easy to add more backends:
- **INLA**: For approximate Bayesian inference
- **Direct Stan**: Without brms wrapper
- **Variational Inference**: Fast approximate posteriors
- **Bootstrap**: Frequentist alternative

Each new backend only requires:
1. Implementation of estimate_posterior_xxx()
2. Implementation of convert_xxx_to_rvars()
3. Adding to backend validator in model class

## Open Questions

1. **NPE model training**: Should the package provide utilities for training NPE models, or expect pre-trained models?
2. **Data format**: What's the optimal format for passing batched data to keras models?
3. **Memory management**: How to handle very large batches for NPE?
4. **Progress reporting**: How to show progress for batched NPE operations?
5. **Error handling**: How to gracefully handle NPE inference failures?

## Dependencies to Add

- `keras` or `tensorflow` for NPE support
- Ensure `posterior` package is imported for rvar operations
- Consider `abind` for efficient array manipulation

## Testing Strategy

1. **Unit tests** for each backend function
2. **Integration tests** comparing brms vs NPE results on same data
3. **Performance benchmarks** showing NPE speedup
4. **Interim analysis tests** for both backends
5. **Backward compatibility tests** ensuring old code still works