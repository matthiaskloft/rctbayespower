---
name: roxygen
description: Write roxygen documentation for rctbayespower. Use when documenting new functions, S7 classes, or methods. Provides templates and project-specific conventions.
---

# Roxygen Documentation Patterns

## Key Rules

1. **Cross-references**: Use `[function_name()]` NOT `\code{\link{function_name}}`
2. **Defaults**: Do NOT duplicate defaults in both docs and signature
3. **Arguments in errors**: Use `{.arg param_name}` in cli messages
4. **globalVariables**: Add to `R/rctbayespower-package.R`
5. **Computed properties**: Do NOT document S7 getters (they're not user-facing params)

## Exported Function Template

```r
#' Title in Sentence Case
#'
#' Brief description of what the function does.
#'
#' @param x Description of x parameter.
#' @param y Description of y parameter.
#'
#' @details
#' Additional details about behavior, edge cases, or usage notes.
#'
#' @return Description of return value.
#'
#' @export
#' @seealso [related_function()], [another_function()]
#'
#' @examples
#' # Simple example
#' result <- my_function(x = 1, y = 2)
#'
#' \dontrun{
#' # Slow or requires external resources
#' result <- my_function(x = 1, n_sims = 1000)
#' }
```

## Internal Function Template

```r
#' Title in Sentence Case
#'
#' Brief description.
#'
#' @param x Description.
#'
#' @return Description.
#'
#' @keywords internal
```

## S7 Class Template

```r
#' rctbp_classname Class
#'
#' Brief description of what this class represents.
#'
#' @slot slot_name Description of the slot.
#' @slot another_slot Description.
#'
#' @seealso [build_classname()], [related_class]
```

## S7 Constructor Template

```r
#' Build a Class Object
#'
#' Creates an [rctbp_classname] object with the specified configuration.
#'
#' @param param1 Description without repeating the default.
#' @param param2 Description. See [related_function()] for details.
#'
#' @details
#' Explain key behaviors, validation rules, or typical workflow.
#'
#' @return An S7 object of class "rctbp_classname"
#'
#' @export
#' @seealso [next_workflow_step()], [show_helper()]
#'
#' @examples
#' obj <- build_classname(param1 = "value")
```

## S7 Method Template (print/summary/plot)

```r
#' Print Method for rctbp_classname Objects
#'
#' Displays a summary of the object.
#'
#' @param x An S7 object of class "rctbp_classname"
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object.
#' @name print.rctbp_classname
#' @export
```

## @param Patterns

**Good:**
```r
#' @param n_sims Number of simulations to run.
#' @param backend Inference backend: "brms" (MCMC) or "bf" (BayesFlow).
#' @param target_params Character vector of parameter names to analyze.
```

**Bad (duplicating defaults):**
```r
#' @param n_sims Number of simulations to run (default 100).
```

**When to include defaults:** Only for non-obvious defaults or when the default has semantic meaning:
```r
#' @param trial_type Trial structure: "fixed" (single analysis),
#'   "group_sequential" (interim stops), or "adaptive" (parameter modification).
```

## Cross-Reference Examples

```r
#' @seealso [build_design()], [build_conditions()], [power_analysis()]
#'
#' See [show_predefined_models()] for available models.
#'
#' Decision thresholds are specified in [build_conditions()].
```

## globalVariables

When using NSE (non-standard evaluation) column names, add to `R/rctbayespower-package.R`:

```r
utils::globalVariables(c(

  "pwr_scs", "pwr_ftl", "pr_scs", "pr_ftl",
  "id_cond", "id_iter", "new_column_name"
))
```

## Full Reference

For complete patterns: `dev/05_code_consistency_review.md`
