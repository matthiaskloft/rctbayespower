# Interim Analysis Implementation Plan
**Created:** 2025-10-29
**Updated:** 2025-11-25
**Status:** Partially Implemented

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
| Result summarization for interim | ❌ Pending | `summarize_sims_with_interim()` |
| Interim-specific plotting | ❌ Pending | Stopping probability, expected N plots |
| Adaptive strategy implementation | ❌ Pending | Parameter modification between looks |

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

Result data frame columns: `sim_iter`, `sim_cond`, `sim_anlys` (analysis index, 0=final only).

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
      prop_stopped_success = mean(interim_decision == "stop_success", na.rm = TRUE),
      prop_stopped_futility = mean(interim_decision == "stop_futility", na.rm = TRUE),
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
      prop_trials_stopped_early = mean(!is.na(stopped_at_n), na.rm = TRUE),
      prop_stopped_success = mean(stop_reason == "stop_success", na.rm = TRUE),
      prop_stopped_futility = mean(stop_reason == "stop_futility", na.rm = TRUE),

      # Expected sample size (accounting for early stops)
      expected_n = mean(n_analyzed, na.rm = TRUE),
      median_n = median(n_analyzed, na.rm = TRUE),

      # Sample size savings
      planned_n = max(n_analyzed, na.rm = TRUE),
      sample_size_savings_pct = 100 * (1 - expected_n / planned_n),

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
      paste0(round(overall_summary$prop_trials_stopped_early * 100, 1), "%"),
      "\n")
  cat("  - Stopped for success:",
      paste0(round(overall_summary$prop_stopped_success * 100, 1), "%"),
      "\n")
  cat("  - Stopped for futility:",
      paste0(round(overall_summary$prop_stopped_futility * 100, 1), "%"),
      "\n")
  cat("\nExpected sample size:", round(overall_summary$expected_n, 0), "\n")
  cat("Planned sample size:", overall_summary$planned_n, "\n")
  cat("Sample size savings:",
      paste0(round(overall_summary$sample_size_savings_pct, 1), "%"),
      "\n")

  cat("\n=== Power by Interim Look ===\n")
  print(results_df$by_look %>%
        dplyr::select(interim_look, n_analyzed, power_success,
                     prop_stopped_success, prop_stopped_futility))
}
```

---

### Phase 7: Update Plotting Functions

**File:** `R/plot_power_analysis.R`

**Add new plot types for interim analyses:**

1. **Stopping Probability by Look:**
```r
plot_stopping_probability <- function(power_result) {
  # Stacked bar chart showing prop_stopped_success, prop_stopped_futility, prop_continue
  # by interim look and condition
}
```

2. **Expected Sample Size:**
```r
plot_expected_sample_size <- function(power_result) {
  # Bar chart comparing expected_n vs planned_n by condition
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

## References

- Group Sequential Methods (Jennison & Turnbull, 1999)
- Bayesian Adaptive Designs (Berry et al., 2010)
- brms documentation for sequential model fitting
- Current package architecture in `development/package_architecture.md`
