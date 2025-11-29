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
| [`11_code_consistency_review.md`](dev/11_code_consistency_review.md) | Code consistency patterns, naming conventions |

Archived files in `dev/archive/`.

## Current Status (2025-11-29)

**Core Package State**: Functional with dual-backend support. API refactoring completed (model merged into design).

### Implemented

| Feature | Status |
|---------|--------|
| Core workflow (`build_design` → `build_conditions` → `power_analysis`) | ✅ |
| S7 class system (`rctbp_design`, `rctbp_conditions`, `rctbp_power_analysis`) | ✅ |
| **API refactoring: model merged into design** | ✅ |
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
| Merged design class | `R/class_design.R` | ✅ Complete (model properties merged) |
| Legacy model class | `R/class_model.R` | ✅ Deprecated (backward compat) |
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

### Quick Workflow Example

```r
# Build design with predefined model (defaults to brms backend)
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2"  # Use design@par_names_inference to discover
)

# Discover available parameter names
design@par_names_inference    # Parameters available for target_params
design@par_names_sim          # Simulation function parameters
design@backend                # "brms" or "bf"
show_condition_args(design)   # Show required arguments for build_conditions()

# Build conditions with three parameter types:
# - crossed: Cartesian product (all combinations)
# - linked: vary 1-to-1 with a crossed parameter
# - constant: same value for all conditions
conditions <- build_conditions(
  design = design,
  crossed = list(n_total = c(100, 200), b_arm_treat = c(0.3, 0.5)),
  constant = list(
    # Simulation parameters
    p_alloc = list(c(0.5, 0.5)),
    intercept = 0,
    b_covariate = 0.3,
    sigma = 1,
    # Decision parameters
    p_sig_scs = 0.975,      # Probability threshold for success
    p_sig_ftl = 0.5,        # Probability threshold for futility
    thresh_scs = 0.2,       # Effect size threshold for success
    thresh_ftl = 0          # Effect size threshold for futility
  )
)
result <- power_analysis(conditions = conditions, n_sims = 100, n_cores = 4)
plot(result)
result@results_conditions
```

### BayesFlow Backend (When Available)

```r
# Set up Python environment with GPU support (first time only)
setup_bf_python()  # Auto-detects CUDA, creates venv, installs packages

# Or with specific options:
setup_bf_python(cuda_version = "12.4")  # Specific CUDA version
setup_bf_python(cuda_version = "cpu")   # CPU-only

# Check status and verify installation
bf_status()                              # Show full environment status
bf_status(envname = "r-rctbayespower")   # Check specific environment
verify_bf_installation()                 # Check all packages
detect_cuda_version()                    # Just check CUDA
get_bf_env_info()                        # Get device (CPU/GPU) and environment info

# Check if BayesFlow is available
check_bf_available(silent = TRUE)

# Backend options:
# - "brms": Load brms model (default, always available)
# - "bf": Try BayesFlow, fall back to brms WITH WARNING if unavailable

# brms backend (default)
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2"
)
design@backend  # "brms"

# BayesFlow backend (falls back to brms with warning if unavailable)
design <- build_design(
  model_name = "ancova_cont_2arms",
  backend = "bf",
  target_params = "b_arm2"
)

# Specify Python environment for BayesFlow
result <- power_analysis(
  conditions,
  n_sims = 100,
  bf_args = list(
    envname = "r-rctbayespower",  # Use specific venv
    n_posterior_samples = 2000
  )
)
# Output shows: Device (CPU/GPU), Environment name, posterior samples, batch size

# Testing without Python (mock mode)
Sys.setenv(RCTBP_MOCK_BF = "TRUE")
# BayesFlow calls will return mock samples
```

**Note**: Parameter names vary by model. Use `design@par_names_inference` to discover available `target_params` and `design@par_names_sim` for simulation arguments.

### Sequential Design with Look-Dependent Boundaries

```r
# Create design
design <- build_design(
  model_name = "ancova_cont_2arms",
  target_params = "b_arm2"
)

# Create conditions with sequential analysis and boundary functions
# Use link() to co-vary analysis_at with n_total
conditions <- build_conditions(
  design = design,
  crossed = list(
    link(
      n_total = c(100, 200),
      # analysis_at accepts:
      # - Proportions (0, 1]: e.g., c(0.5, 1) means 50% and 100% of n_total
      # - Absolute integers: e.g., c(50, 100) for n_total=100
      # n_total is auto-appended if not the last element
      analysis_at = list(c(50, 100), c(100, 200))  # Absolute sample sizes
      # Or equivalently: list(c(0.5), c(0.5))  # Proportions (n_total auto-appended)
    )
  ),
  constant = list(
    b_arm_treat = 0.5,
    p_alloc = list(c(0.5, 0.5)),
    intercept = 0, b_covariate = 0.3, sigma = 1,
    thresh_scs = 0.2, thresh_ftl = 0,
    # Sequential analysis settings (Bayesian: use threshold parameter)
    p_sig_scs = boundary_obf(threshold = 0.95), # OBF shape ending at 0.95
    p_sig_ftl = boundary_linear(0.30, 0.50)     # Linear from 0.3 to 0.5
  )
)

# Available boundary functions (specify either alpha OR threshold):
#
# - boundary_obf(alpha=0.025)      Frequentist: OBF alpha-spending
# - boundary_obf(threshold=0.95)   Bayesian: OBF shape ending at threshold
# - boundary_pocock(alpha=0.025)   Frequentist: Pocock alpha-spending
# - boundary_pocock(threshold=0.95) Bayesian: constant threshold at all looks
# - boundary_hsd(alpha, gamma)     Hwang-Shih-DeCani family (requires gsDesign)
# - boundary_linear(start, end)    Linear interpolation between thresholds
# - boundary_power(base, rho)      Power family (rho=2: OBF-like shape)
# - boundary_constant(threshold)   Simple fixed threshold

# Post-hoc boundary comparison (no re-simulation needed)
result <- power_analysis(conditions, n_sims = 500)
comparison <- compare_boundaries(result, list(
  "Fixed 0.95" = list(success = 0.95, futility = 0.50),
  "OBF-shape" = list(success = boundary_obf(threshold = 0.95), futility = 0.50),
  "Stringent" = list(success = 0.99, futility = 0.70)
))

# Re-analyze with new boundaries (returns modified power_analysis object)
result_obf <- resummarize_boundaries(result,
  p_sig_scs = boundary_obf(threshold = 0.95),
  p_sig_ftl = boundary_linear(0.30, 0.50)
)
```

## File Organization

### Core Classes (`R/class_*.R`)

| File | Purpose |
|------|---------|
| `R/class_design.R` | Merged design class (includes model properties, dual backend) |
| `R/class_model.R` | Legacy model class (backward compatibility, see note below) |
| `R/class_conditions.R` | Conditions class definition |
| `R/class_power_analysis.R` | Power analysis + run() + print/summary methods |
| `R/models_ancova.R` | ANCOVA model builders + batch simulation |

### Backend System

| File | Purpose |
|------|---------|
| `R/backend_brms.R` | brms-specific estimation (single + sequential) |
| `R/backend_bf.R` | BayesFlow estimation + reticulate calls |
| `R/model_cache.R` | Model download and caching system |
| `R/utils_results.R` | Shared error result utilities |
| `R/worker_functions.R` | Parallel worker dispatch + S7 serialization |

### Boundaries

| File | Purpose |
|------|---------|
| `R/boundaries.R` | Stopping boundary functions (OBF, Pocock, linear, power) |

### Plotting (`R/plot_*.R`)

| File | Purpose |
|------|---------|
| `R/plot_power_analysis.R` | Main S7 plot method + dispatcher |
| `R/plot_power_curve.R` | Power curve visualization |
| `R/plot_heatmap.R` | 2D heatmap visualization |
| `R/plot_comparison.R` | Power vs probability comparison |
| `R/plot_helpers.R` | Shared utilities (pivot, colors, theme) |

### Reporting (`R/report_*.R`)

| File | Purpose |
|------|---------|
| `R/report_builders.R` | Build structured report data + topic reports |
| `R/report_renderers.R` | CLI/Markdown table rendering |

### Discovery/Helper Functions

| Function | File | Purpose |
|----------|------|---------|
| `show_predefined_models()` | `R/class_design.R` | List available predefined models |
| `show_target_params(model_name)` | `R/required_fn_args.R` | Show available target parameters for a model |
| `show_condition_args(design)` | `R/required_fn_args.R` | Show required arguments for build_conditions() |
| `show_boundaries()` | `R/boundaries.R` | List available boundary functions |

### Utilities

| File | Purpose |
|------|---------|
| `R/compute_measures.R` | Posterior measure computation from rvars |
| `R/verbosity.R` | Three-level verbosity control (0, 1, 2) |
| `R/output_system.R` | CLI/Markdown dual-mode output system |
| `R/MCSE.R` | Monte Carlo standard error calculations |
| `R/required_fn_args.R` | Helper functions for parameter discovery |
| `R/S7_helpers.R` | S7 utility functions |
| `R/s3_wrappers.R` | S3 method wrappers for S7 classes |

### Python Integration

| File | Purpose |
|------|---------|
| `R/setup_python.R` | Environment setup helpers (GPU detection, install) |
| `R/python_simulators.R` | Python simulator loading (reticulate) |

### Package Infrastructure

| File | Purpose |
|------|---------|
| `R/zzz.R` | .onLoad hook (S7 method registration) |
| `R/rctbayespower-package.R` | Package documentation + globalVariables |

### Legacy Files

| File/Directory | Status |
|------|--------|
| `R/class_model.R` | Deprecated - legacy model class kept for backward compat |
| `R/legacy/` | Archive of old API implementations (NOT loaded by package) |

### Deprecation Policy

**Package not yet released**: Since the package has not been released to CRAN yet, deprecated functions are removed rather than issuing deprecation warnings. Legacy code is archived in `R/legacy/` for reference.

**When package is released**: Add `lifecycle::deprecate_warn()` calls to any remaining legacy functions before removal.

## Development Practices

### Code Patterns (Verified 2025-11-28)

See [`dev/11_code_consistency_review.md`](dev/11_code_consistency_review.md) for full details.

#### File Structure
```r
# =============================================================================
# FILE TITLE IN CAPS
# =============================================================================
# Brief description of file purpose.

# Major section code...

# =============================================================================
# NEXT MAJOR SECTION
# =============================================================================
```

#### Naming Conventions

| Element | Convention | Examples |
|---------|------------|----------|
| S7 classes | `rctbp_*` prefix | `rctbp_design`, `rctbp_conditions` |
| Power columns | `pwr_*` prefix | `pwr_scs`, `pwr_ftl` |
| Probability columns | `pr_*` prefix | `pr_scs`, `pr_ftl` |
| Decision columns | `dec_*` prefix | `dec_scs`, `dec_ftl` |
| ROPE threshold cols | `thresh_*` or `thr_*` | `thresh_scs`, `thresh_ftl`, `thr_scs` |
| Standard error cols | `se_*` prefix | `se_pwr_scs`, `se_pr_ftl` |
| Success suffix | `_scs` | `pwr_scs`, `pr_scs`, `thresh_scs` |
| Futility suffix | `_ftl` | `pwr_ftl`, `pr_ftl`, `thresh_ftl` |
| Functions | snake_case | `build_design`, `power_analysis` |
| Parameters | snake_case | `n_total`, `p_sig_scs` |

#### Error Handling Pattern
```r
cli::cli_abort(c(
  "Main error message",
  "x" = "What went wrong: {.val {bad_value}}",
  "i" = "Helpful hint or suggestion"
))
```

#### S7 Class Pattern
```r
rctbp_classname <- S7::new_class(
  "rctbp_classname",
  properties = list(
    prop1 = S7::new_property(S7::class_numeric, default = 0),
    prop2 = S7::class_character | NULL
  ),
  validator = function(self) {
    if (invalid) cli::cli_abort(c("message", "x" = "...", "i" = "..."))
    NULL
  }
)
```

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
