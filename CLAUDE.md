# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`rctbayespower` is an R package for conducting Bayesian power analysis for randomized controlled trials (RCTs) using `brms` and Stan. The package provides tools for estimating power curves, determining optimal sample sizes, and incorporating prior knowledge about treatment effects using region of practical equivalence (ROPE) for decision making.

## Developer Documentation

Documentation is organized in `dev/` as numbered topic files:

| File | Topic |
|------|-------|
| [`01_architecture.md`](dev/01_architecture.md) | Package architecture, class hierarchy, parallelization |
| [`02_s7_classes.md`](dev/02_s7_classes.md) | S7 class definitions, properties, patterns |
| [`03_workflow.md`](dev/03_workflow.md) | User workflow, API reference, debugging |
| [`04_development_guidelines.md`](dev/04_development_guidelines.md) | Code style, roxygen, R CMD check |
| [`05_testing.md`](dev/05_testing.md) | Testing strategy, parallel test setup |
| [`06_interim_analysis_plan.md`](dev/06_interim_analysis_plan.md) | **[PLANNED]** Group sequential designs |
| [`07_backend_abstraction_plan.md`](dev/07_backend_abstraction_plan.md) | **[PLANNED]** NPE/neural posterior estimation |
| [`08_adaptive_trials_roadmap.md`](dev/08_adaptive_trials_roadmap.md) | **[PLANNED]** Binary, survival, adaptive designs |

Archived files in `dev/archive/`.

## Current Status (2025-11-24)

**Core Package State**: Functional and stable. Documentation up to date.

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

### Planned / In Progress

| Feature | Status | Plan |
|---------|--------|------|
| Interim analysis / group sequential | Planning | `06_interim_analysis_plan.md` |
| NPE backend abstraction | Placeholder code exists | `07_backend_abstraction_plan.md` |
| Binary/survival outcomes | Not started | `08_adaptive_trials_roadmap.md` |
| Test suite | 0% | `05_testing.md` |

### Quick Workflow Example

```r
model <- build_model(predefined_model = "ancova_cont_2arms")

# Discover available parameter names for target_params
model@parameter_names_brms    # brms parameters (for target_params)
model@parameter_names_sim_fn  # simulation function parameters

design <- build_design(
  model = model,
  target_params = "b_armtreat_1",  # Must match model@parameter_names_brms
  p_sig_success = 0.975,
  p_sig_futility = 0.5
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
result@summarized_results
```

**Note**: Parameter names vary by model. Use `model@parameter_names_brms` to discover available `target_params` and `model@parameter_names_sim_fn` for simulation arguments.

## Development Practices

### Documentation

Save new development documents into `dev/`.


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
#' @param p_sig_success Probability threshold for success (default 0.95)
my_function <- function(p_sig_success = 0.975) { ... }

# GOOD: Documentation matches code
#' @param p_sig_success Probability threshold for success (default 0.975) 
my_function <- function(p_sig_success = 0.975) { ... }
```

**Recent fixes applied**: Updated documentation for `p_sig_success` in `power_analysis.R`, and class system fixes throughout package.

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
