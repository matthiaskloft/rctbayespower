# Interim Analysis Implementation Plan
**Created:** 2025-10-29
**Updated:** 2025-11-26
**Status:** Mostly Implemented (Adaptive pending)

## Implementation Progress

| Component | Status | Notes |
|-----------|--------|-------|
| `rctbp_design` class properties | ✅ Done | Added `analysis_at`, `interim_function`, `adaptive` |
| `build_design()` constructor | ✅ Done | Accepts interim parameters |
| `build_conditions()` inheritance | ✅ Done | Inherits interim defaults from design |
| `prepare_design_for_workers()` | ✅ Done | Serializes interim properties |
| Sequential estimation (`estimation_sequential.R`) | ✅ Done | Handles brms and NPE backends |
| Worker strategy detection | ✅ Done | Detects single/sequential/adaptive |
| Interim helper functions | ✅ Done | `interim_continue()`, `interim_futility_only()`, `interim_success_futility()` |
| Result summarization for interim | ✅ Done | `summarize_sims_with_interim()` in `compute_measures.R` |
| Report display for interim | ✅ Done | Shows stopping rates, expected N, savings in print output |
| `run()` method interim handling | ✅ Done | Stores `by_look` and `overall` (as attribute) |
| Interim-specific plotting | ⏸️ Deferred | See "Future Plot Types" section below |
| Default stopping rule | ✅ Done | `analysis_at` without `interim_function` uses dec_scs/dec_ftl |
| **Look-dependent boundaries** | ✅ Done | `boundary_obf()`, `boundary_pocock()`, `boundary_linear()`, `boundary_power()` |
| **Function-valued p_sig_* thresholds** | ✅ Done | `p_sig_scs`/`p_sig_ftl` accept functions in `build_design()` |
| **Boundary re-analysis** | ✅ Done | `resummarize_boundaries()`, `compare_boundaries()` |
| Adaptive strategy implementation | 🔄 In Planning | Parameter modification between looks |

---

## Look-Dependent Stopping Boundaries (Implemented 2025-11-26)

### Overview

The package now supports look-dependent probability thresholds for sequential designs.
Instead of using fixed thresholds (e.g., `p_sig_scs = 0.975`) at all interim analyses,
users can specify boundary functions that return different thresholds based on
information fraction (`current_n / n_total`).

### Key Files

- **`R/boundaries.R`**: Boundary function factories and helpers
- **`R/class_design.R`**: Updated to accept function-valued `p_sig_scs`/`p_sig_ftl`
- **`R/estimation_sequential.R`**: Resolves thresholds per look using `resolve_threshold()`
- **`R/compute_measures.R`**: Added `resummarize_boundaries()`, `compare_boundaries()`

### Boundary Functions

| Function | Description | Formula |
|----------|-------------|---------|
| `boundary_obf(base)` | O'Brien-Fleming-style | `1 - (1 - base) * sqrt(info_frac)` |
| `boundary_pocock(threshold)` | Constant (Pocock-style) | `threshold` (constant) |
| `boundary_linear(start, end)` | Linear interpolation | `start + (end - start) * info_frac` |
| `boundary_power(base, rho)` | Power family (flexible) | `1 - (1 - base) * info_frac^(rho/2)` |

### Usage Example

```r
# Create design with function-valued thresholds
design <- build_design(
  model = model,
  target_params = "b_armtreat_1",
  p_sig_scs = boundary_obf(0.975),          # Stringent early, relaxed late
  p_sig_ftl = boundary_linear(0.70, 0.90),  # Lenient early, strict late
  analysis_at = c(0.5, 0.75)
)

# Run simulation once
result <- power_analysis(conditions, n_sims = 500)

# Compare multiple boundary configurations without re-simulation
comparison <- compare_boundaries(result, list(
  "Fixed" = list(success = 0.975, futility = 0.90),
  "OBF" = list(success = boundary_obf(0.975), futility = 0.90),
  "Stringent" = list(success = 0.99, futility = 0.95)
))

# Re-analyze with new boundaries (returns new power_analysis object)
result_obf <- resummarize_boundaries(result,
  p_sig_scs = boundary_obf(0.975),
  p_sig_ftl = boundary_linear(0.70, 0.90)
)
```

### Implementation Details

1. **S7 Class Validation**: `rctbp_design` validator tests function-valued thresholds at info_frac=0.5
2. **Threshold Resolution**: `resolve_threshold(threshold, info_frac)` in estimation loops
3. **Post-hoc Analysis**: Uses stored `pr_scs`/`pr_ftl` values (posterior probabilities)
4. **Re-summarization**: `resummarize_boundaries()` recomputes decisions and summaries

---

## Future Plot Types (TODO)

**Status:** Not Started
**Priority:** Medium

The following plot types should be implemented for sequential/interim analysis visualization:

### 1. Stopping Rate Plot (`type = "stopping"`)
- **Purpose**: Visualize cumulative stopping probability across interim looks
- **X-axis**: Look number (`id_look`) or sample size at look (`n_analyzed`)
- **Y-axis**: Cumulative stopping proportion
- **Lines**: By stopping reason (success, futility)
- **Data source**: `results_interim$cumul_stp`, `prop_scs_look`, `prop_ftl_look`
- **Facets**: By condition (if multiple)

### 2. Sample Size Distribution (`type = "sample_dist"`)
- **Purpose**: Show distribution of actual sample sizes across simulations
- **Visualization**: Histogram/density plot
- **Annotations**: Vertical lines for mean, median, mode
- **Data source**: `results_overall$n_mn`, `n_mdn`, `n_mode`
- **Facets**: By condition

### 3. Decision Summary (`type = "decisions"`)
- **Purpose**: Compare final decision proportions across conditions
- **Visualization**: Stacked bar chart
- **Bars**: One per condition
- **Segments**: `prop_stp_scs`, `prop_stp_ftl`, `prop_no_dec`
- **Data source**: `results_overall`

### Implementation Notes
- All plots should follow the ggplot2 + ggplotly pattern (see `R/plot_helpers.R`)
- Add as new files: `R/plot_stopping.R`, `R/plot_sample_dist.R`, `R/plot_decisions.R`
- Update `create_power_plot()` dispatcher in `R/plot_power_analysis.R`
- Auto-detect sequential data and suggest appropriate plot type

---

## Adaptive Strategy Implementation Plan

**Status:** Planning
**Primary Use Case:** Response-Adaptive Randomization (RAR)

### Problem Statement

Current sequential estimation generates ALL data upfront, then subsets for each interim. For adaptive designs, we need:
1. Generate batch 1 (subjects 1-50) with initial `p_alloc = c(0.5, 0.5)`
2. Analyze, decide to shift allocation based on interim results
3. Generate batch 2 (subjects 51-100) with modified `p_alloc = c(0.4, 0.6)`
4. Continue until `n_total` or stopping

Final dataset = concatenation of batches with **different generation parameters**.

### Current State

| Component | Status |
|-----------|--------|
| `design@adaptive` property | ✅ Exists (logical flag) |
| `interim_function` returns `modified_params` | ✅ Interface defined |
| `estimate_sequential_brms()` uses `modified_params` | ❌ Ignores it |
| Incremental data generation | ❌ Not implemented |
| Validation of `modified_params` | ❌ Not implemented |

### Implementation Phases

#### Phase 1: Incremental Data Generation

**File:** `R/estimation_sequential.R`

Add adaptive branch to `estimate_sequential_brms()`:

```r
if (adaptive) {
  # === ADAPTIVE PATH: Incremental generation ===
  cumulative_data <- NULL
  current_sim_args <- sim_args
  prev_n <- 0

  for (id_analysis in seq_along(analysis_schedule)) {
    current_n <- analysis_schedule[id_analysis]
    batch_size <- current_n - prev_n

    # Generate new batch with current parameters
    batch_args <- modifyList(current_sim_args, list(n_total = batch_size))
    new_batch <- do.call(data_simulation_fn, batch_args)
    new_batch$batch_id <- id_analysis  # Track which batch

    # Concatenate to cumulative data
    cumulative_data <- rbind(cumulative_data, new_batch)
    analysis_data <- cumulative_data

    # ... existing: fit model, compute measures ...

    # Apply parameter modifications for NEXT batch
    if (!is.null(interim_decision$modified_params) && !is_final) {
      validate_modified_params(interim_decision$modified_params, current_sim_args)
      current_sim_args <- modifyList(current_sim_args, interim_decision$modified_params)
    }

    prev_n <- current_n
  }
} else {
  # === NON-ADAPTIVE PATH: Current behavior (unchanged) ===
  full_data <- do.call(data_simulation_fn, sim_args)
  # ... existing loop subsetting full_data ...
}
```

#### Phase 2: Parameter Validation

**File:** `R/interim_functions.R`

```r
#' Validate Modified Parameters from Interim Function
#' @keywords internal
validate_modified_params <- function(modified_params, current_sim_args) {
  if (is.null(modified_params) || length(modified_params) == 0) {
    return(invisible(TRUE))
  }

  # Check param names are valid
  valid_params <- names(current_sim_args)
  invalid <- setdiff(names(modified_params), valid_params)
  if (length(invalid) > 0) {
    cli::cli_abort(c(
      "Invalid parameters in 'modified_params'",
      "x" = "Unknown parameters: {.val {invalid}}",
      "i" = "Valid parameters: {.val {valid_params}}"
    ))
  }

  # Validate p_alloc specifically
  if ("p_alloc" %in% names(modified_params)) {
    p_alloc <- modified_params$p_alloc
    if (abs(sum(p_alloc) - 1) > 1e-6) {
      cli::cli_abort("'p_alloc' must sum to 1, got {round(sum(p_alloc), 4)}")
    }
    if (any(p_alloc < 0) || any(p_alloc > 1)) {
      cli::cli_abort("'p_alloc' values must be between 0 and 1")
    }
  }

  invisible(TRUE)
}
```

#### Phase 3: Response-Adaptive Randomization Helper

**File:** `R/interim_functions.R`

```r
#' Interim Function Factory: Response-Adaptive Randomization
#'
#' Adjusts allocation ratio based on observed posterior probability of success.
#'
#' @param min_alloc Minimum allocation to any arm (default 0.2)
#' @param max_alloc Maximum allocation to any arm (default 0.8)
#' @param adaptation_strength How aggressively to adapt, 0-1 (default 0.5)
#' @param futility_threshold Optional futility stopping threshold
#'
#' @export
interim_rar <- function(min_alloc = 0.2, max_alloc = 0.8,
                        adaptation_strength = 0.5, futility_threshold = NULL) {
  function(interim_summaries, current_n, analysis_at, n_total) {
    pr_scs <- interim_summaries$pr_scs[1]

    # Check futility
    if (!is.null(futility_threshold)) {
      pr_ftl <- interim_summaries$pr_ftl[1]
      if (!is.na(pr_ftl) && pr_ftl >= futility_threshold) {
        return(list(decision = "stop_futility", modified_params = NULL))
      }
    }

    # Adapt allocation: pr_scs > 0.5 → favor treatment
    adjustment <- (pr_scs - 0.5) * adaptation_strength
    new_treat_alloc <- pmax(min_alloc, pmin(max_alloc, 0.5 + adjustment))

    list(
      decision = "continue",
      modified_params = list(p_alloc = c(1 - new_treat_alloc, new_treat_alloc))
    )
  }
}
```

#### Phase 4: Track Adaptation in Results

Add columns to results in `estimate_sequential_brms()`:
- `p_alloc_used`: Allocation ratio used for this analysis
- `params_modified`: Whether parameters were modified at this look
- `batch_id`: Which generation batch (for raw data)

#### Phase 5: Update Summarization

**File:** `R/compute_measures.R`

Add to `summarize_sims_with_interim()`:
- `prop_trials_adapted`: Proportion of trials with any adaptation
- `avg_adaptations_per_trial`: Average number of parameter changes

#### Phase 6: Warning for Ignored modified_params

If `adaptive=FALSE` but `interim_function` returns `modified_params`:
```r
cli::cli_warn(c(
 "Interim function returned 'modified_params' but design is not adaptive",
  "i" = "Set {.code adaptive = TRUE} in {.fn build_design} to enable",
  "!" = "Ignoring modified_params"
))
```

### Files to Modify

| File | Changes |
|------|---------|
| `R/estimation_sequential.R` | Add adaptive branch with incremental generation |
| `R/interim_functions.R` | Add `validate_modified_params()`, `interim_rar()` |
| `R/compute_measures.R` | Add adaptation metrics to summarization |
| `R/worker_functions.R` | Ensure `adaptive` + `sim_args` passed correctly |

### Scope Boundaries

**MVP (In Scope):**
- `p_alloc` modification (response-adaptive randomization)
- brms backend only
- `interim_rar()` helper function
- Basic adaptation tracking

**Future (Out of Scope):**
- NPE backend adaptive support
- Sample size re-estimation (`n_total` modification)
- Arm dropping (multi-arm)
- Dose modification

### Testing Strategy

1. `validate_modified_params()` catches invalid params
2. `interim_rar()` returns correct allocation shifts
3. Integration: Adaptive simulation produces batches with different allocation
4. Comparison: Same scenario adaptive vs non-adaptive

### Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Reproducibility varies with adaptation | Document; consider batch-specific seeds |
| Performance regression | Adaptive path only when `adaptive=TRUE` |
| NPE incompatibility | Document brms-only limitation for MVP |

### Estimated Effort

| Phase | Effort |
|-------|--------|
| Phase 1: Incremental generation | 2-3 hours |
| Phase 2: Validation | 30 min |
| Phase 3: `interim_rar()` | 1 hour |
| Phase 4-5: Results tracking | 1-2 hours |
| Phase 6: Warnings | 15 min |
| Documentation | 1 hour |
| **Total** | **~6-8 hours** |

---

### Naming Conventions (Updated)

The codebase uses these standardized short names:

| Full Name | Short Name | Usage |
|-----------|------------|-------|
| `p_sig_success` | `p_sig_scs` | Probability threshold for success |
| `p_sig_futility` | `p_sig_ftl` | Probability threshold for futility |
| `summarized_results` | `results_summ` | Aggregated results property |
| `raw_results` | `results_raw` | Individual simulation results |
| `threshold_success` | `thr_scs` | Effect size threshold for success |
| `threshold_futility` | `thr_ftl` | Effect size threshold for futility |
| `probability_success` | `pr_scs` | Posterior P(effect > threshold) |
| `probability_futility` | `pr_ftl` | Posterior P(effect < threshold) |
| `decision_success` | `dec_scs` | Binary decision indicator |
| `decision_futility` | `dec_ftl` | Binary decision indicator |

Result data frame columns: `id_iter`, `id_cond`, `id_look` (analysis index, 0=final only).

### Accessing Interim Results

When running sequential designs, results are structured as follows:

```r
# results_summ contains per-look metrics (grouped by condition, parameter, analysis)
result@results_summ
# Columns include: id_look, n_analyzed, pwr_scs, pwr_ftl, pr_scs, pr_ftl, ...

# Overall interim metrics (stopping rates, expected N) stored as attribute
attr(result@results_summ, "interim_overall")
# Columns: n_planned, n_expect, n_mdn, prop_stop_early, prop_stop_scs, prop_stop_ftl, n_saved, pct_saved

# Raw per-simulation results
result@results_raw
# Includes: stopped, stop_reason, interim_decision columns
```

## Overview
Design and implement an S7 class system for specifying interim analyses in sequential trial designs, integrated into the existing `rctbp_design` class architecture.

## Design Decisions (User Confirmed)

### 1. Architecture
- **Built into design**: Interim specs specified directly in `build_design()`, not as standalone class
- **Rationale**: Simpler API, interim analysis is a design characteristic

### 2. Adaptive Functionality
- **Adaptive parameter modification**: When `adaptive=TRUE`, interim function can modify simulation parameters between looks
- **Examples**: Change allocation ratios, adjust treatment assignment probabilities

### 3. Interim Function Interface
- **Input**: Posterior summaries from `compute_measures_brmsfit()`
- **Output**: Decision ("continue", "stop_success", "stop_futility") + optional modified parameters
- **Rationale**: Reuse existing measure computation, consistent with current architecture

### 4. Stopped Trials Handling
- **Continue to n_total**: Even if trial stops early, simulate and analyze all data up to `n_total`
- **Rationale**: Enables rich metrics about early stopping (sample size savings, decision consistency, error rates)
- **Benefits**:
  - Can validate if early decisions agree with final analysis
  - Calculate expected sample size accounting for early stops
  - Post-hoc analysis of alternative stopping rules

### 5. Conflicting Stopping Decisions ✅ IMPLEMENTED
- **Problem**: If both `dec_scs = 1` AND `dec_ftl = 1` at the same interim analysis, the thresholds are misconfigured
- **Detection**: In `interim_success_futility()` and similar functions, check if both stopping conditions are met
- **Action**: Raise an informative error explaining that thresholds and/or `p_sig_scs`/`p_sig_ftl` are set inappropriately
- **Rationale**:
  - Success decision: P(effect > threshold_success) >= p_sig_scs
  - Futility decision: P(effect < threshold_futility) >= p_sig_ftl
  - If both are true simultaneously, the ROPE (region between thresholds) is too narrow or probability thresholds are too permissive
- **Error message should suggest**:
  - Widen the gap between `thresholds_success` and `thresholds_futility`
  - Increase `p_sig_scs` and/or `p_sig_ftl` to require stronger evidence
  - Review the prior and likelihood to ensure posteriors aren't unreasonably wide

**Implementation location**: `R/interim_functions.R` in the decision functions

## Implementation Plan

### Phase 1: Update `rctbp_design` Class ✅ IMPLEMENTED

**File:** `R/class_design.R`

**Added properties:**
```r
analysis_at = S7::new_property(class = S7::class_numeric | NULL, default = NULL)
interim_function = S7::new_property(class = S7::class_function | NULL, default = NULL)
adaptive = S7::new_property(class = S7::class_logical, default = FALSE)
```

**Update validator:**
- Validate `analysis_at` is monotonically increasing if not NULL
- Validate `adaptive` is single logical value
- Check compatibility: if `interim_function` is provided, `analysis_at` must be specified
- Add informative error messages

**Update `build_design()` constructor:**
- Add parameters: `analysis_at = NULL`, `adaptive = FALSE`
- Update roxygen documentation with:
  - Parameter descriptions
  - Relationship between `analysis_at` and `n_total` (from conditions)
  - Examples of interim specifications
  - Notes about adaptive vs non-adaptive behavior

**Update `print.rctbp_design()` method:**
- Display interim analysis specifications:
  ```
  === Interim Analyses ===
  Analysis timepoints: 50, 100, 150
  Adaptive design: FALSE
  Interim function: Specified
  ```

---

### Phase 2: Create Interim Function Validation Helper

**File:** `R/class_interim.R` (create new file)

**Purpose:** Validate and document interim function structure

**Create S7 helper class (optional, for internal validation):**
```r
rctbp_interim <- S7::new_class(
  "rctbp_interim",
  properties = list(
    decision_fn = S7::class_function,
    parameter_names = S7::new_property(
      getter = function(self) {
        names(formals(self@decision_fn))
      }
    )
  ),
  validator = function(self) {
    # Validate required parameters exist
    required <- c("interim_summaries", "current_n", "analysis_at", "n_total")
    param_names <- names(formals(self@decision_fn))
    if (!all(required %in% param_names)) {
      return(paste("Interim function must have parameters:",
                   paste(required, collapse = ", ")))
    }
    NULL
  }
)
```

**Interim Function Signature Standard:**
```r
# User-provided function must follow this interface:
interim_fn <- function(interim_summaries, current_n, analysis_at, n_total, ...) {
  # interim_summaries: data.frame from compute_measures_brmsfit()
  #   Columns: parameter, threshold_success, threshold_futility,
  #            success_prob, futility_prob, power_success, power_futility,
  #            median, mad, mean, sd, rhat, ess_bulk, ess_tail
  # current_n: integer, current sample size at this interim look
  # analysis_at: integer, the planned sample size for this interim analysis
  # n_total: integer, the final planned sample size
  # ...: custom parameters from interim_args (optional)

  # Return: list with two elements:
  #   - decision: character, one of "continue", "stop_success", "stop_futility"
  #   - modified_params: named list of parameter changes (if adaptive=TRUE),
  #                      or NULL (if adaptive=FALSE or no changes)
  #     Example: list(p_alloc = c(0.4, 0.6)) to change allocation ratio

  return(list(
    decision = "continue",      # or "stop_success" or "stop_futility"
    modified_params = NULL      # or list(param_name = new_value)
  ))
}
```

**Add validation function:**
```r
validate_interim_function <- function(fn) {
  # Check it's a function
  if (!is.function(fn)) {
    stop("'interim_function' must be a function")
  }

  # Check required parameters
  required <- c("interim_summaries", "current_n", "analysis_at", "n_total")
  param_names <- names(formals(fn))
  missing <- setdiff(required, param_names)
  if (length(missing) > 0) {
    stop("Interim function missing required parameters: ",
         paste(missing, collapse = ", "))
  }

  invisible(TRUE)
}
```

**Add helper functions for common interim rules:**
```r
#' Create Simple Futility Stopping Rule
#' @export
interim_futility_only <- function(futility_threshold = 0.90) {
  function(interim_summaries, current_n, analysis_at, n_total) {
    futility_prob <- interim_summaries$futility_prob[1]

    if (futility_prob > futility_threshold) {
      return(list(decision = "stop_futility", modified_params = NULL))
    } else {
      return(list(decision = "continue", modified_params = NULL))
    }
  }
}

#' Create Success and Futility Stopping Rule
#' @export
interim_success_futility <- function(success_threshold = 0.99,
                                      futility_threshold = 0.90) {
  function(interim_summaries, current_n, analysis_at, n_total) {
    success_prob <- interim_summaries$success_prob[1]
    futility_prob <- interim_summaries$futility_prob[1]

    if (success_prob > success_threshold) {
      return(list(decision = "stop_success", modified_params = NULL))
    } else if (futility_prob > futility_threshold) {
      return(list(decision = "stop_futility", modified_params = NULL))
    } else {
      return(list(decision = "continue", modified_params = NULL))
    }
  }
}
```

---

### Phase 3: Refactor `simulate_single_run()`

**File:** `R/simulate_single_run.R`

**Current behavior (no interim):**
1. Simulate all data at once
2. Fit model once
3. Compute measures once
4. Return single-row data.frame

**New behavior (with interim):**

**When `analysis_at = NULL` (backward compatible):**
- Keep current behavior exactly as is

**When `analysis_at` is specified:**
1. Extract `analysis_at`, `adaptive`, `interim_function` from design
2. Create full analysis schedule: `all_looks <- c(analysis_at, n_total)`
3. Initialize tracking variables:
   ```r
   stopped <- FALSE
   stop_reason <- NA_character_
   stop_at_n <- NA_integer_
   stop_look <- NA_integer_
   current_params <- condition_arguments$sim_args  # May be modified if adaptive
   ```

4. **Loop over each look:**
   ```r
   results_list <- list()

   for (look_idx in seq_along(all_looks)) {
     current_n <- all_looks[look_idx]
     is_final_look <- (look_idx == length(all_looks))

     # === Data Simulation ===
     if (adaptive || look_idx == 1) {
       # Simulate/regenerate data up to current_n
       # Use current_params (may have been modified by interim function)
       simulated_data <- do.call(data_simulation_fn, current_params)
       # Subset to current_n if needed
       simulated_data <- simulated_data[1:current_n, ]
     } else {
       # Non-adaptive: use same dataset, just subset
       if (look_idx == 1) {
         simulated_data <- do.call(data_simulation_fn, current_params)
       }
       simulated_data <- simulated_data[1:current_n, ]
     }

     # === Model Fitting ===
     fitted_model <- tryCatch({
       do.call(function(...) {
         stats::update(object = brms_model, newdata = simulated_data, ...)
       }, brms_args_final)
     }, error = function(e) {
       warning("Model fitting failed at n=", current_n, ": ", e$message)
       return(NULL)
     })

     if (is.null(fitted_model)) {
       # Handle error case - return NA row for this look
       results_list[[look_idx]] <- create_error_result(...)
       next
     }

     # === Compute Measures ===
     interim_summaries <- compute_measures_brmsfit(fitted_model, design)

     # === Interim Decision (if not final look and interim_function exists) ===
     decision <- "final_analysis"
     modified_params <- NULL

     if (!is_final_look && !is.null(interim_function)) {
       # Call interim function
       interim_result <- tryCatch({
         interim_function(
           interim_summaries = interim_summaries,
           current_n = current_n,
           analysis_at = current_n,
           n_total = max(all_looks),
           ...  # Pass any interim_args here
         )
       }, error = function(e) {
         warning("Interim function failed at n=", current_n, ": ", e$message)
         list(decision = "continue", modified_params = NULL)
       })

       decision <- interim_result$decision
       modified_params <- interim_result$modified_params

       # Record stopping if it occurs
       if (decision %in% c("stop_success", "stop_futility") && !stopped) {
         stopped <- TRUE
         stop_reason <- decision
         stop_at_n <- current_n
         stop_look <- look_idx
       }

       # Apply modified parameters if adaptive
       if (adaptive && !is.null(modified_params)) {
         current_params <- modifyList(current_params, modified_params)
       }
     }

     # === Store Results ===
     result_row <- interim_summaries %>%
       dplyr::mutate(
         interim_look = look_idx,
         n_analyzed = current_n,
         stopped_at_prior_interim = stopped && (look_idx > stop_look),
         interim_decision = decision,
         stopped_at_n = if_else(stopped, stop_at_n, NA_integer_),
         stop_reason = if_else(stopped, stop_reason, NA_character_),
         id_sim = id_sim,
         id_cond = condition_arguments$id_cond,
         converged = 1L,
         error = NA_character_
       )

     results_list[[look_idx]] <- result_row
   }

   # Combine all looks into single data.frame
   results_df <- dplyr::bind_rows(results_list)
   ```

**Updated return structure:**
```r
# Multiple rows per simulation (one row per interim look + final)
# Columns:
# - All standard measures: parameter, threshold_success, success_prob, power_success, etc.
# - interim_look: 1, 2, 3, ..., n_looks
# - n_analyzed: actual sample size for this analysis
# - stopped_at_prior_interim: logical, TRUE if stopped before this look
# - interim_decision: "continue" / "stop_success" / "stop_futility" / "final_analysis"
# - stopped_at_n: sample size where stopping occurred (NA if no stop, same value for all rows after stopping)
# - stop_reason: "stop_success" / "stop_futility" / NA
# - id_sim, id_cond, converged, error: as before
```

---

### Phase 4: Update `build_conditions()` Validation

**File:** `R/class_conditions.R`

**Add validation in `build_conditions()`:**
```r
# After creating conditions_grid, validate against design

# Check analysis_at compatibility with n_total
if (!is.null(design@analysis_at)) {
  n_total_values <- condition_values$n_total
  if (is.null(n_total_values)) {
    n_total_values <- static_values$n_total
  }

  if (is.null(n_total_values)) {
    stop("'n_total' must be specified in condition_values or static_values when using interim analyses")
  }

  # Check all analysis_at values are less than all n_total values
  max_analysis_at <- max(design@analysis_at)
  min_n_total <- min(n_total_values)

  if (max_analysis_at >= min_n_total) {
    stop(
      paste0(
        "All 'analysis_at' values must be less than all 'n_total' values. ",
        "Maximum analysis_at: ", max_analysis_at,
        ", Minimum n_total: ", min_n_total
      )
    )
  }

  # Warn if analysis_at is very close to n_total (>90%)
  if (max_analysis_at / min_n_total > 0.9) {
    warning(
      paste0(
        "Final interim analysis is at ",
        round(max_analysis_at / min_n_total * 100, 1),
        "% of minimum n_total. ",
        "Consider an earlier final interim or larger n_total."
      )
    )
  }
}
```

---

### Phase 5: Update Results Processing

**File:** `R/class_power_analysis.R`

**Modify `run.rctbp_power_analysis()`:**
- No major changes needed - results will naturally have multiple rows per simulation
- The `results_df_raw` will be larger (n_sims × n_conditions × n_interim_looks rows)

**Create new function `summarize_sims_with_interim()`:**
```r
summarize_sims_with_interim <- function(results_df_raw, n_sims) {
  # Group by condition and interim look
  summary_by_look <- results_df_raw %>%
    dplyr::group_by(id_cond, interim_look, n_analyzed) %>%
    dplyr::summarize(
      # Standard power metrics
      power_success = mean(power_success, na.rm = TRUE),
      power_futility = mean(power_futility, na.rm = TRUE),
      success_prob_mean = mean(success_prob, na.rm = TRUE),
      futility_prob_mean = mean(futility_prob, na.rm = TRUE),

      # Interim-specific metrics
      prop_stop_scs = mean(interim_decision == "stop_success", na.rm = TRUE),
      prop_stop_ftl = mean(interim_decision == "stop_futility", na.rm = TRUE),
      prop_continue = mean(interim_decision == "continue", na.rm = TRUE),

      # Convergence
      prop_converged = mean(converged, na.rm = TRUE),

      # MCSE
      mcse_power_success = calculate_mcse_power(power_success, n_sims),
      mcse_power_futility = calculate_mcse_power(power_futility, n_sims),

      .groups = "drop"
    )

  # Summary across all looks (for overall trial outcomes)
  summary_overall <- results_df_raw %>%
    dplyr::filter(interim_look == max(interim_look)) %>%  # Only final look or where stopped
    dplyr::group_by(id_cond) %>%
    dplyr::summarize(
      # Overall trial outcomes
      prop_stop_early = mean(!is.na(stopped_at_n), na.rm = TRUE),
      prop_stop_scs = mean(stop_reason == "stop_success", na.rm = TRUE),
      prop_stop_ftl = mean(stop_reason == "stop_futility", na.rm = TRUE),

      # Expected sample size (accounting for early stops)
      n_expect = mean(n_analyzed, na.rm = TRUE),
      n_mdn = median(n_analyzed, na.rm = TRUE),

      # Sample size savings
      planned_n = max(n_analyzed, na.rm = TRUE),
      n_saved = planned_n - n_expect,
      pct_saved = 1 - n_expect / planned_n,

      .groups = "drop"
    )

  return(list(
    by_look = summary_by_look,
    overall = summary_overall
  ))
}
```

**Update `summarize_sims()` to detect interim analyses:**
```r
summarize_sims <- function(results_df_raw, n_sims) {
  # Check if interim analyses were performed
  has_interim <- "interim_look" %in% names(results_df_raw)

  if (has_interim) {
    return(summarize_sims_with_interim(results_df_raw, n_sims))
  } else {
    # Original summarization for non-interim analyses
    return(summarize_sims_original(results_df_raw, n_sims))
  }
}
```

---

### Phase 6: Update Print Methods

**File:** `R/class_power_analysis.R`

**Update `print.rctbp_power_analysis()`:**

When results include interim data:
```r
if (has_results && has_interim) {
  cat("\n=== Interim Analysis Results ===\n")

  # Overall stopping statistics
  overall_summary <- results_df$overall
  cat("Trials stopped early:",
      paste0(round(overall_summary$prop_stop_early * 100, 1), "%"),
      "\n")
  cat("  - Stopped for success:",
      paste0(round(overall_summary$prop_stop_scs * 100, 1), "%"),
      "\n")
  cat("  - Stopped for futility:",
      paste0(round(overall_summary$prop_stop_ftl * 100, 1), "%"),
      "\n")
  cat("\nExpected sample size:", round(overall_summary$n_expect, 0), "\n")
  cat("Planned sample size:", overall_summary$planned_n, "\n")
  cat("Sample size saved:", round(overall_summary$n_saved, 1), "\n")
  cat("Percent saved:",
      paste0(round(overall_summary$pct_saved * 100, 1), "%"),
      "\n")

  cat("\n=== Power by Interim Look ===\n")
  print(results_df$by_look %>%
        dplyr::select(interim_look, n_analyzed, power_success,
                     prop_stop_scs, prop_stop_ftl))
}
```

---

### Phase 7: Update Plotting Functions

**File:** `R/plot_power_analysis.R`

**Add new plot types for interim analyses:**

1. **Stopping Probability by Look:**
```r
plot_stopping_probability <- function(power_result) {
  # Stacked bar chart showing prop_stop_scs, prop_stop_ftl, prop_continue
  # by interim look and condition
}
```

2. **Expected Sample Size:**
```r
plot_expected_sample_size <- function(power_result) {
  # Bar chart comparing n_expect vs planned_n by condition
  # Show sample size savings
}
```

3. **Power Trajectory:**
```r
plot_power_trajectory <- function(power_result) {
  # Line plot showing power_success over interim looks
  # Separate lines for different conditions
}
```

4. **Decision Consistency:**
```r
plot_decision_consistency <- function(power_result) {
  # Heatmap or confusion matrix showing agreement between
  # interim stopping decision and final analysis result
}
```

**Update `plot.rctbp_power_analysis()` to auto-detect interim plots:**
```r
S7::method(plot, rctbp_power_analysis) <- function(x, type = "auto", ...) {
  has_interim <- "interim_look" %in% names(x@results_raw)

  if (type == "auto") {
    if (has_interim) {
      type <- "stopping_probability"  # Default for interim analyses
    } else {
      type <- "power_curve"  # Default for standard analyses
    }
  }

  # Dispatch to appropriate plotting function
  switch(type,
    stopping_probability = plot_stopping_probability(x),
    expected_sample_size = plot_expected_sample_size(x),
    power_trajectory = plot_power_trajectory(x),
    decision_consistency = plot_decision_consistency(x),
    power_curve = plot_power_curve(x),  # Original plot
    stop("Unknown plot type: ", type)
  )
}
```

---

### Phase 8: Documentation

**Update roxygen docs for all modified functions:**

1. **`build_design()`:**
   - Add `@param analysis_at` with detailed description and examples
   - Add `@param adaptive` with explanation of behavior
   - Add examples showing interim specifications
   - Add section explaining relationship with `n_total` from conditions

2. **`simulate_single_run()`:**
   - Document new return structure with interim columns
   - Add examples with and without interim analyses

3. **`print.rctbp_power_analysis()`:**
   - Document interim-specific output

4. **New interim helper functions:**
   - Full roxygen docs for `interim_futility_only()`, `interim_success_futility()`
   - Examples of custom interim functions

**Create vignette: "Interim Analyses in rctbayespower":**
- Introduction to sequential designs
- Specifying interim analyses with `analysis_at`
- Writing custom interim functions
- Adaptive vs non-adaptive designs
- Interpreting interim analysis results
- Examples:
  - Futility-only stopping
  - Success and futility stopping
  - Adaptive allocation based on interim results
  - Response-adaptive randomization

---

## Testing Strategy

### Unit Tests (create in `tests/testthat/test-interim.R`)

1. **Design validation:**
   - Test `analysis_at` must be monotonically increasing
   - Test `analysis_at` with `interim_function = NULL` gives warning/error
   - Test `adaptive` must be logical

2. **Interim function validation:**
   - Test interim function with missing parameters fails
   - Test interim function with correct parameters succeeds
   - Test helper functions return correct structure

3. **Simulation with interim:**
   - Test non-adaptive simulation produces same data at all looks
   - Test adaptive simulation can modify parameters
   - Test stopping at different interim looks
   - Test continuation to `n_total` even after stopping

4. **Results structure:**
   - Test return has correct number of rows (n_sims × n_looks)
   - Test columns include all interim metadata
   - Test stopped trials have consistent `stopped_at_n` values

### Integration Tests

1. **Full workflow:**
   - Build design → conditions → run analysis with interim
   - Verify results have interim structure
   - Verify print methods work
   - Verify plots work

2. **Edge cases:**
   - Stopping at first interim
   - Never stopping (all continue to final)
   - All trials stop at same interim
   - Mixed stopping patterns

---

## Implementation Timeline

**Phase 1-2 (Foundation):** 2-3 hours
- Update `rctbp_design` class
- Create interim function validation

**Phase 3 (Core Logic):** 4-6 hours
- Refactor `simulate_single_run()`
- Handle adaptive and non-adaptive cases
- Test basic functionality

**Phase 4-5 (Integration):** 2-3 hours
- Update `build_conditions()` validation
- Modify results processing and summarization

**Phase 6-7 (Presentation):** 3-4 hours
- Update print methods
- Create interim-specific plots

**Phase 8 (Documentation):** 2-3 hours
- Update all roxygen docs
- Create examples

**Testing:** 3-4 hours
- Write comprehensive tests
- Integration testing

**Total Estimated Time:** 16-23 hours

---

## Example Usage (Target API)

### Basic Futility Stopping
```r
# Use built-in helper
design <- build_design(
  model = ancova_model,
  target_params = "b_arms_treat",
  thresholds_success = 0.2,
  thresholds_futility = 0,
  p_sig_scs = 0.975,
  p_sig_ftl = 0.5,
  analysis_at = c(100, 150),  # Interim at 100, 150; final at n_total
  adaptive = FALSE,
  interim_function = interim_futility_only(futility_threshold = 0.90)
)

conditions <- build_conditions(
  design = design,
  condition_values = list(n_total = 200),
  static_values = list(
    p_alloc = list(c(0.5, 0.5)),
    true_parameter_values = list(b_arms_treat = 0.5, ...)
  )
)

result <- power_analysis(conditions = conditions, n_sims = 100)
print(result)  # Shows interim stopping statistics
plot(result, type = "stopping_probability")
```

### Custom Adaptive Allocation
```r
# Custom interim function that modifies allocation
my_adaptive_fn <- function(interim_summaries, current_n, analysis_at, n_total) {
  success_prob <- interim_summaries$success_prob[1]

  # If treatment looks promising, allocate more to treatment
  if (success_prob > 0.80) {
    new_alloc <- c(0.3, 0.7)  # 70% to treatment
    return(list(
      decision = "continue",
      modified_params = list(p_alloc = new_alloc)
    ))
  } else {
    return(list(decision = "continue", modified_params = NULL))
  }
}

design <- build_design(
  model = ancova_model,
  target_params = "b_arms_treat",
  thresholds_success = 0.2,
  thresholds_futility = 0,
  p_sig_scs = 0.975,
  p_sig_ftl = 0.5,
  analysis_at = c(50, 100),
  adaptive = TRUE,  # Must be TRUE for parameter modification
  interim_function = my_adaptive_fn
)
```

---

## Open Questions / Future Enhancements

1. **Multiple parameter monitoring:** How to handle interim decisions based on multiple parameters?
2. **Group sequential methods:** Integration with formal alpha-spending functions?
3. **Sample size re-estimation:** Allow increasing `n_total` based on interim results?
4. **Conditional power:** Add conditional power calculations at interim looks?
5. **Parallel performance:** Impact of interim analyses on parallelization efficiency?

---

## Planned Feature: Per-n_total analysis_at Specification

**Status:** Planned

### Problem

Currently, `analysis_at` is specified at the design level as proportions (e.g., `c(0.5, 0.75, 1)`), which works well when all conditions share the same relative interim schedule. However, users may want different absolute interim timepoints for different `n_total` values:

- n_total = 100: interim at 50, final at 100
- n_total = 200: interim at 50, 100, 150, final at 200
- n_total = 300: interim at 100, 200, final at 300

### Proposed Solution

Allow `analysis_at` to be specified in `condition_values` or `static_values` as a **named list** keyed by `n_total`, with values being either proportions or sample sizes:

```r
# Using sample sizes (integers)
conditions <- build_conditions(

  design = design,
  condition_values = list(
    n_total = c(100, 200, 300),
    analysis_at = list(
      "100" = c(50, 100),
      "200" = c(50, 100, 150, 200),
      "300" = c(100, 200, 300)
    )
  ),
  static_values = list(...)
)

# Using proportions (auto-converted using respective n_total)
conditions <- build_conditions(
  design = design,
  condition_values = list(
    n_total = c(100, 200, 300),
    analysis_at = list(
      "100" = c(0.5, 1),
      "200" = c(0.25, 0.5, 0.75, 1),
      "300" = c(0.33, 0.67, 1)
    )
  ),
  static_values = list(...)
)
```

### Implementation Notes

1. **Detection**: In `build_conditions()`, check if `analysis_at` is a named list
2. **Validation**:
   - Keys must match `n_total` values (as character)
   - Each vector must be monotonically increasing
   - If proportions: last value must be 1 (or auto-append)
   - If integers: last value must equal the corresponding n_total
3. **Processing**: For each condition row, look up `analysis_at` by its `n_total` key
4. **Conversion**: If proportions, convert to integers using that condition's `n_total`

### API Design Considerations

- Keep backward compatibility: single vector `analysis_at` still works (applied to all n_total)
- Named list is opt-in for per-n_total customization
- Clear error messages when keys don't match n_total values

---

## References

- Group Sequential Methods (Jennison & Turnbull, 1999)
- Bayesian Adaptive Designs (Berry et al., 2010)
- brms documentation for sequential model fitting
- Current package architecture in `development/package_architecture.md`
