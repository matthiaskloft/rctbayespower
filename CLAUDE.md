# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`rctbayespower` is an R package for conducting Bayesian power analysis for randomized controlled trials (RCTs). The package supports dual backends:
- **brms/Stan**: Traditional MCMC-based Bayesian inference
- **BayesFlow**: Neural posterior estimation for fast amortized inference

The package provides tools for estimating power curves, determining optimal sample sizes, and incorporating prior knowledge about treatment effects using region of practical equivalence (ROPE) for decision making.

## Developer Documentation

Documentation is organized in `dev/` as numbered topic files:

| File | Topic |
|------|-------|
| [`01_architecture.md`](dev/01_architecture.md) | Package architecture, class hierarchy, backend system |
| [`02_s7_classes.md`](dev/02_s7_classes.md) | S7 class definitions, properties, patterns |
| [`03_workflow.md`](dev/03_workflow.md) | User workflow, API reference, debugging |
| [`04_development_guidelines.md`](dev/04_development_guidelines.md) | Code style, roxygen, R CMD check |
| [`05_testing.md`](dev/05_testing.md) | Testing strategy, parallel test setup |
| [`06_interim_analysis_plan.md`](dev/06_interim_analysis_plan.md) | Group sequential designs, boundary functions |
| [`07_backend_abstraction_plan.md`](dev/07_backend_abstraction_plan.md) | **[IMPLEMENTED]** Backend abstraction design |
| [`08_adaptive_trials_roadmap.md`](dev/08_adaptive_trials_roadmap.md) | **[PLANNED]** Binary, survival, adaptive designs |
| [`09_bayesian_adaptive_designs_reference.md`](dev/09_bayesian_adaptive_designs_reference.md) | Reference: Bayesian adaptive trial designs |
| [`10_bayesflow_integration_roadmap.md`](dev/10_bayesflow_integration_roadmap.md) | BayesFlow integration status and next steps |

Archived files in `dev/archive/`.

## Current Status (2025-11-27)

**Core Package State**: Functional with dual-backend support.

### Implemented

| Feature | Status |
|---------|--------|
| Core workflow (`build_model` → `build_design` → `build_conditions` → `power_analysis`) | ✅ |
| S7 class system (`rctbp_model`, `rctbp_design`, `rctbp_conditions`, `rctbp_power_analysis`) | ✅ |
| ANCOVA continuous models (2-arm and 3-arm) | ✅ |
| Design prior integration | ✅ |
| Parallelization with model caching | ✅ |
| Plotting (power curves, heatmaps) | ✅ |
| Documentation (vignettes, manual pages) | ✅ |
| Group sequential / interim analysis (non-adaptive) | ✅ |
| Look-dependent stopping boundaries | ✅ |
| Boundary re-analysis functions | ✅ |
| **Dual backend support (brms + BayesFlow)** | ✅ |
| **BayesFlow reticulate integration** | ✅ |
| **Mock mode for testing without Python** | ✅ |

### Backend System

| Component | File | Status |
|-----------|------|--------|
| brms backend | `R/backend_brms.R` | ✅ Complete |
| BayesFlow backend | `R/backend_bf.R` | ✅ Complete (reticulate calls) |
| Model caching | `R/model_cache.R` | ✅ Complete |
| Dual-backend model class | `R/class_model.R` | ✅ Complete |
| Worker dispatch | `R/worker_functions.R` | ✅ Complete |
| Batch simulation | `R/models_ancova.R` | ✅ Complete |
| Shared utilities | `R/utils_results.R` | ✅ Complete |

### Planned / In Progress

| Feature | Status | Plan |
|---------|--------|------|
| Adaptive interim analysis (parameter modification) | Planning | `06_interim_analysis_plan.md` |
| Train BayesFlow models | Next step | `10_bayesflow_integration_roadmap.md` |
| Binary/survival outcomes | Not started | `08_adaptive_trials_roadmap.md` |
| Test suite | 0% | `05_testing.md` |

### Quick Workflow Example (brms backend)

```r
model <- build_model(predefined_model = "ancova_cont_2arms")

# Discover available parameter names for target_params
model@parameter_names_brms    # brms parameters (for target_params)
model@parameter_names_sim_fn  # simulation function parameters

design <- build_design(
  model = model,
  target_params = "b_armtreat_1",  # Must match model@parameter_names_brms
  p_sig_scs = 0.975,
  p_sig_ftl = 0.5
)
conditions <- build_conditions(
  design = design,
  condition_values = list(n_total = c(100, 200), b_arm_treat = c(0.3, 0.5)),
  static_values = list(
    p_alloc = list(c(0.5, 0.5)),
    thresholds_success = 0.2,
    thresholds_futility = 0,
    intercept = 0,
    b_covariate = 0.3,
    sigma = 1
  )
)
result <- power_analysis(conditions = conditions, n_sims = 100, n_cores = 4)
plot(result)
result@results_summ
```

### BayesFlow Backend (When Available)

```r
# Check if BayesFlow is available
check_bf_available(silent = TRUE)

# Load pre-trained BayesFlow model
bf_model <- load_bf_model("ancova_cont_2arms")

# Add BayesFlow backend to existing model
model <- build_model(predefined_model = "ancova_cont_2arms")
model <- add_bf_backend(model, bf_model)

# Model now uses BayesFlow by default (faster)
model@active_backend  # "bf"

# Force brms backend if needed
model@backend <- "brms"

# Testing without Python (mock mode)
Sys.setenv(RCTBP_MOCK_BF = "TRUE")
# BayesFlow calls will return mock samples
```

**Note**: Parameter names vary by model. Use `model@parameter_names_brms` to discover available `target_params` and `model@parameter_names_sim_fn` for simulation arguments.

### Sequential Design with Look-Dependent Boundaries

```r
# Create design with O'Brien-Fleming-style stopping boundaries
design <- build_design(
  model = model,
  target_params = "b_armtreat_1",
  p_sig_scs = boundary_obf(0.975),        # Function: stringent early, relaxed late
  p_sig_ftl = boundary_linear(0.70, 0.90), # Function: lenient early, strict late
  analysis_at = c(0.5, 0.75)               # Interim at 50%, 75%; final at 100%
)

# Available boundary functions:
# - boundary_obf(base)        O'Brien-Fleming-style (most conservative early)
# - boundary_pocock(threshold) Constant threshold (same at all looks)
# - boundary_linear(start, end) Linear interpolation
# - boundary_power(base, rho)  Power family (rho controls curve shape)

# Post-hoc boundary comparison (no re-simulation needed)
result <- power_analysis(conditions, n_sims = 500)
comparison <- compare_boundaries(result, list(
  "Fixed 0.975" = list(success = 0.975, futility = 0.90),
  "OBF-style" = list(success = boundary_obf(0.975), futility = 0.90),
  "Stringent" = list(success = 0.99, futility = 0.95)
))

# Re-analyze with new boundaries (returns modified power_analysis object)
result_obf <- resummarize_boundaries(result,
  p_sig_scs = boundary_obf(0.975),
  p_sig_ftl = boundary_linear(0.70, 0.90)
)
```

## File Organization

### Core R Files

| File | Purpose |
|------|---------|
| `R/class_model.R` | Model class (dual backend support) |
| `R/class_design.R` | Design class definition |
| `R/class_conditions.R` | Conditions class definition |
| `R/class_power_analysis.R` | Power analysis + run() method |
| `R/models_ancova.R` | ANCOVA model builders + batch simulation |

### Backend Files

| File | Purpose |
|------|---------|
| `R/backend_brms.R` | brms-specific estimation functions |
| `R/backend_bf.R` | BayesFlow estimation + reticulate calls |
| `R/model_cache.R` | Model download and caching system |
| `R/utils_results.R` | Shared result utilities |
| `R/worker_functions.R` | Parallel worker dispatch |

### Deprecated Files (Empty, Kept for Reference)

| File | Replacement |
|------|-------------|
| `R/backends.R` | `R/backend_brms.R`, `R/backend_bf.R` |
| `R/estimation_single.R` | `R/backend_*.R` |
| `R/estimation_sequential.R` | `R/backend_*.R` |
| `R/simulate_single_run.R` | `R/worker_functions.R` |

## Development Practices

### File Organization

- **All R source files must be in `/R` root** - no subfolders allowed in `/R` directory
- Save new development documents into `dev/`

### Documentation Guidelines
- **Always update documentations directly in the .R file's roxygen documentation**
- **CRITICAL**: After updating roxygen comments, run `devtools::document()` to regenerate .Rd files
- **Never manually edit .Rd files** in the `man/` directory - they are auto-generated
- Only edit the README.Rmd file, the README.md file will get build from it later.

### Documentation Best Practices
- **Concise Example Guidelines**:
  - Examples in the documentation of a function need to be as short and concise as possible
  - Only one variant of the function should be demonstrated if the function has extended run time

### R CMD Check Best Practices

#### 1. Avoiding Documentation Mismatches
**Problem**: R CMD check fails when default parameter values in code don't match roxygen documentation.

**Solution**: Always ensure roxygen `@param` documentation matches the actual default values in function signatures.

**Example Fix**:
```r
# BAD: Code has default = 0.975 but docs say 0.95
#' @param p_sig_scs Probability threshold for success (default 0.95)
my_function <- function(p_sig_scs = 0.975) { ... }

# GOOD: Documentation matches code
#' @param p_sig_scs Probability threshold for success (default 0.975)
my_function <- function(p_sig_scs = 0.975) { ... }
```

### Documentation Conventions
- Don't use \code{\link{function_name}} in roxygen docs. Use [functionname()] instead.

### Code Writing Guidelines

#### Required Practices
- **Roxygen2 documentation** for all exported functions
- **Explicit namespacing**: Use `package::function()` (e.g., `dplyr::mutate()`)
- **CLI messaging**: Use `cli` package (`cli_alert_info`, `cli_abort`, `cli_inform`)
- **Pipe operator**: Base R `|>` (not magrittr `%>%`)
- **Parameter validation**: Extensive validation with informative errors at function start

#### Output and Messaging
- Always wrap argument names in quotes when displaying them in printed output such as in stop(), warning(), message(), or documentation. This improves clarity and avoids confusion with actual values or natural language.

#### CLI Efficiency Best Practices

**IMPORTANT**: For **reports and multi-line output**, collect ALL content first and output with a **single cat() call** to create one unified output block. For **status messages**, individual CLI calls are fine.

**Problem** (Reports/Multi-line Output Only):
- Calling CLI functions (even `cli::cli_bullets()`) creates separate console messages for each element
- Multiple `message()` or `cat()` calls create separate output blocks
- This fragments output in rendered documents and looks unprofessional

**Solution**: Build complete output as a character vector, collapse to a single string, and output once with `cat()`.

**Example - Report Functions (Multi-Section Output)**:

```r
# ❌ INCORRECT - Multiple output calls create separate blocks
message(paste(section1_lines, collapse = "\n"))  # Block 1
message(paste(section2_lines, collapse = "\n"))  # Block 2
message(paste(section3_lines, collapse = "\n"))  # Block 3

# ❌ ALSO INCORRECT - CLI functions create many separate messages
cli::cli_h2("Section Header")
for (item in items) {
  cli::cli_bullets(c("*" = paste0(item$name, ": ", item$value)))
}

# ✅ CORRECT - Build ALL sections, then output once
all_output <- character()

# Section 1
all_output <- c(all_output, "\n── Section 1 ──", "")
for (item in section1_items) {
  all_output <- c(all_output, paste0("  • ", item$name))
  all_output <- c(all_output, paste0("    ", item$description))
  all_output <- c(all_output, "")
}

# Section 2
all_output <- c(all_output, "\n── Section 2 ──", "")
for (item in section2_items) {
  all_output <- c(all_output, paste0("  • ", item$name))
  all_output <- c(all_output, paste0("    ", item$value))
}

# Output everything as ONE block
cat(paste(all_output, collapse = "\n"), "\n")
```

**Example - Status Messages (Individual Calls OK)**:

```r
# ✅ CORRECT - Status messages can be individual calls
cli::cli_alert_info("Starting {analysis_type} interpretation...")
# ... do work ...
cli::cli_alert_success("Analysis complete")
cli::cli_alert_info("Tokens used: {tokens$total}")
```

**Key Principles**:
1. **Reports/Print Methods**: Build helper functions that return text, collect all sections, single `cat()` call
2. **Status Messages**: Individual CLI calls are fine and encouraged for clarity
3. **Rule of thumb**: If output has multiple sections or will be rendered in docs, use single-call pattern
4. Use `cat()` for stdout (like reports), `message()` for stderr (like warnings)

**When to Apply Single-Call Pattern**:
- Report generation functions (`build_report.{class}()`)
- Parameter display functions (`show_interpret_args()`)
- Print methods for S3 classes (`print.chat_session()`, `print.variable_labels()`)
- Any function with multiple sections or many lines
- Functions whose output will be rendered in vignettes or documentation

**When Individual CLI Calls are OK**:
- Status messages (`cli::cli_alert_info()`, `cli::cli_alert_success()`)
- Progress updates during processing
- Interactive prompts
- Warnings (`cli::cli_warn()`)
- Error messages (`cli::cli_abort()`)
- Single informational messages
