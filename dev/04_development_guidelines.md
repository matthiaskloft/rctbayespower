# Development Guidelines

**Last Updated:** 2025-11-28

See also: [`11_code_consistency_review.md`](11_code_consistency_review.md) for detailed patterns verified across all R files.

## Code Style Requirements

### Required Practices

- **Roxygen2 documentation** for all exported functions
- **Explicit namespacing**: Use `package::function()` (e.g., `dplyr::mutate()`)
- **CLI messaging**: Use `cli` package (`cli_alert_info`, `cli_abort`, `cli_inform`)
- **Pipe operator**: Base R `|>` (not magrittr `%>%`)
- **Parameter validation**: Extensive validation with informative errors at function start
- **Assignment**: Use `<-` not `=`
- **Booleans**: Use `TRUE`/`FALSE` not `T`/`F`

### Naming Conventions

| Element | Convention | Examples |
|---------|------------|----------|
| S7 classes | `rctbp_*` prefix | `rctbp_model`, `rctbp_design` |
| Power columns | `pwr_*` prefix | `pwr_eff`, `pwr_fut` |
| Probability columns | `pr_*` prefix | `pr_eff`, `pr_fut` |
| Decision columns | `dec_*` prefix | `dec_eff`, `dec_fut` |
| Standard error cols | `se_*` prefix | `se_pwr_eff`, `se_pr_fut` |
| Efficacy suffix | `_eff` | `pwr_eff`, `pr_eff` |
| Futility suffix | `_fut` | `pwr_fut`, `pr_fut` |
| Functions | snake_case | `build_model`, `power_analysis` |
| Parameters | snake_case | `n_total`, `thr_dec_eff` |

### File Structure

All R files should follow this structure:

```r
# =============================================================================
# FILE TITLE IN CAPS
# =============================================================================
# Brief description of file purpose.

# Section code...

# =============================================================================
# NEXT SECTION
# =============================================================================
```

### Error Handling Pattern

Use `cli::cli_abort()` with structured messages:

```r
cli::cli_abort(c(
  "Main error message",
  "x" = "What went wrong: {.val {bad_value}}",
  "i" = "Helpful hint or suggestion"
))
```

For warnings, use `cli::cli_warn()` with the same structure.

### Output and Messaging

- Wrap argument names in quotes when displaying in `stop()`, `warning()`, `message()`
- Example: `stop("Parameter 'n_total' must be positive")`

## Documentation Standards

### Critical Rules

1. **Always update documentation in `.R` files** (roxygen comments)
2. **Run `devtools::document()`** after any roxygen changes
3. **Never manually edit `.Rd` files** in `man/` - they are auto-generated
4. Only edit `README.Rmd`, not `README.md`

### Roxygen Template

```r
#' Title in Sentence Case
#'
#' Description of what the function does. Can be multiple sentences.
#'
#' @param x Description of parameter x
#' @param y Description of parameter y (default 0.5)
#' @return Description of return value
#' @export
#' @examples
#' function_name(1, 2)
function_name <- function(x, y = 0.5) {
 ...
}
```

### Link Syntax

Use `[function_name()]` not `\code{\link{function_name}}`

### Example Guidelines

- Examples must be as short and concise as possible
- Only demonstrate one variant if function has extended run time
- Use `\donttest{}` for slow examples

## R CMD Check Requirements

**Must pass with 0 ERRORs, 0 WARNINGs, 0 NOTEs**

### Common Issues

1. **Documentation mismatch**: Ensure roxygen `@param` defaults match code defaults
   ```r
   # BAD: Code has 0.975 but docs say 0.95
   #' @param p_sig_scs Threshold (default 0.95)
   my_fn <- function(p_sig_scs = 0.975) { ... }

   # GOOD: Match them
   #' @param p_sig_scs Threshold (default 0.975)
   my_fn <- function(p_sig_scs = 0.975) { ... }
   ```

2. **Undeclared global variables**: Add to `globalVariables()` or use `.data$var`

3. **Missing imports**: Add `@importFrom pkg fn` or use `pkg::fn()`

## CLI Output Best Practices

### For Reports/Multi-line Output

Build ALL content first, output with single `cat()`:

```r
# BAD - Multiple output calls create separate blocks
cli::cli_h2("Section 1")
cli::cli_bullets(c("*" = "Item 1"))

# GOOD - Single output
all_output <- character()
all_output <- c(all_output, "\n-- Section 1 --")
all_output <- c(all_output, "  * Item 1")
cat(paste(all_output, collapse = "\n"), "\n")
```

### For Status Messages

Individual CLI calls are fine:

```r
cli::cli_alert_info("Starting analysis...")
# ... work ...
cli::cli_alert_success("Complete")
```

### When to Use Each Pattern

**Single-call pattern**:
- Report generation functions
- Print methods for classes
- Multi-section output
- Output rendered in vignettes

**Individual CLI calls**:
- Status messages
- Progress updates
- Warnings and errors
- Single informational messages

## Development Workflow

```r
devtools::load_all()     # Load package (Ctrl+Shift+L)
devtools::document()     # Generate docs (Ctrl+Shift+D)
devtools::test()         # Run tests (Ctrl+Shift+T)
devtools::check()        # R CMD check (Ctrl+Shift+E)
```

## File Path Handling

- Never use `setwd()` in package code
- Use `system.file()` for package files
- Use `testthat::test_path()` in tests
- Always use relative paths

## Dependencies

- Minimize dependencies
- Prefer `Imports` over `Depends`
- Use `Suggests` for optional features
- Specify minimum versions when using new features

### Current Core Dependencies

- `brms` (>= 2.20.0)
- `posterior`
- `S7`
- `ggplot2`, `dplyr`, `tidyr`, `tibble`
- `parallel`, `cli`, `rlang`
