# Roxygen Documentation Fix Action Plan
## rctbayespower Package
### Priority: HIGH - Blocks Package Usage

---

## CRITICAL FIXES (Must do first)

### Fix #1: Repair print.rctbp_power_analysis() Method
**Priority:** CRITICAL  
**Status:** Code error (runtime crash)  
**File:** `R/class_power_analysis.R`

**Problem:**
Lines 599 and 602 reference non-existent S7 properties:
```r
cat("Success thresholds:",
    paste(design@thresholds_success, collapse = ", "),  # Line 599 - ERROR!
    "\n")
cat("Futility thresholds:",
    paste(design@thresholds_futility, collapse = ", "),  # Line 602 - ERROR!
    "\n")
```

**Root Cause:**
The `rctbp_design` class does NOT have `thresholds_success` and `thresholds_futility` properties. These thresholds are stored per-condition in the conditions object, not as class properties.

**Design Class Properties (actual):**
- `model` - rctbp_model object
- `target_params` - character vector of parameters to analyze
- `p_sig_success` - numeric (0-1), posterior probability threshold for success
- `p_sig_futility` - numeric (0-1), posterior probability threshold for futility
- `design_name` - optional character string

**Solution:**
Remove lines 598-603 from the print method. These thresholds are per-condition and not summary statistics at the design level.

**Verify After Fix:**
Test that printing a power_analysis object doesn't crash:
```r
# Should not error
print(power_config)
```

---

### Fix #2: Correct build_model() Example
**Priority:** CRITICAL  
**Status:** Invalid API name in example  
**File:** `R/class_model.R`

**Problem:**
Line 166 in roxygen documentation:
```r
#' \dontrun{
#' # Method 1: Use predefined model (recommended)
#' ancova_model <- build_model(predefined_model = "ancova_cont")
#'                                                 ^^^^^^^^^^^^
#'                                    This model doesn't exist!
#' }
```

**Root Cause:**
Predefined models are named with full specifications:
- `"ancova_cont_2arms"` - ANCOVA with 2 arms
- `"ancova_cont_3arms"` - ANCOVA with 3 arms

There is no model named `"ancova_cont"` (incomplete name).

**Solution:**
Change line 166 to:
```r
#' ancova_model <- build_model(predefined_model = "ancova_cont_2arms")
```

**Verify After Fix:**
```r
# Should work without error
model <- build_model(predefined_model = "ancova_cont_2arms")
print(model)
```

**Alternative (Better):**
Consider showing both variants:
```r
#' # Method 1: 2-arm ANCOVA model (recommended)
#' model_2arm <- build_model(predefined_model = "ancova_cont_2arms")
#' 
#' # Method 2: 3-arm ANCOVA model
#' model_3arm <- build_model(predefined_model = "ancova_cont_3arms")
```

---

### Fix #3: Correct build_design() Example
**Priority:** CRITICAL  
**Status:** Invalid example code  
**File:** `R/class_design.R`

**Problem:**
Line 70 in example section:
```r
#' # Create an ANCOVA model
#' ancova_model <- build_model("ancova_cont_2arms")()
#'                                                   ^^
#'                              These parentheses are invalid!
```

**Root Cause:**
`build_model(predefined_model = "ancova_cont_2arms")` returns an S7 object (rctbp_model), not a function. You cannot call an S7 object as if it were a function.

**Solution:**
Remove the trailing `()`:
```r
#' # Create an ANCOVA model
#' ancova_model <- build_model(predefined_model = "ancova_cont_2arms")
```

**Verify After Fix:**
```r
# Should work without error
model <- build_model(predefined_model = "ancova_cont_2arms")
class(model)  # Should be: [1] "rctbayespower::rctbp_model"
```

---

## MEDIUM PRIORITY FIXES

### Fix #4: Update Class Name in required_fn_args.R
**Priority:** MEDIUM  
**Status:** Confusing documentation  
**File:** `R/required_fn_args.R`

**Problem:**
Line 7 parameter documentation:
```r
#' @param object Either an rctbp_design or rctbayespower_model object
```

The class is named `rctbp_model`, not `rctbayespower_model`.

**Solution:**
Change to:
```r
#' @param object Either an rctbp_design or rctbp_model object
```

---

### Fix #5: Clarify build_design() Documentation
**Priority:** MEDIUM  
**Status:** Incomplete/confusing information  
**File:** `R/class_design.R`

**Problem:**
Documentation mentions thresholds and interim schedules being properties, but they're actually per-condition:
```r
#' (thresholds, interim schedules) are defined per-condition in the conditions object.
```

But the documentation doesn't make it clear that:
- The design object contains the ANALYSIS decision criteria (`p_sig_success`, `p_sig_futility`)
- The CONDITIONS object contains the specific SCENARIO thresholds (`thresholds_success`, `thresholds_futility`) per condition

**Solution:**
Enhance the Details section to clarify the separation of concerns:
```r
#' \strong{Analysis Parameters vs. Condition Parameters:}
#' This object defines global analysis decision criteria (p_sig_success, p_sig_futility).
#' The specific decision thresholds (thresholds_success, thresholds_futility) and interim
#' analysis schedules are defined per-condition in the conditions object created by
#' [build_conditions()].
```

---

### Fix #6: Review and Fix print.rctbp_power_analysis() Documentation
**Priority:** MEDIUM  
**Status:** Code references non-existent properties  
**File:** `R/class_power_analysis.R`

**Problem:**
The roxygen documentation for the print method doesn't warn about the fact that it tries to access non-existent properties.

**Solution:**
Once Fix #1 is applied (removing the problematic lines), update the print method to show available properties instead. Suggested output:

```r
# Instead of trying to show thresholds from design:
cat("Target parameters:",
    paste(design@target_params, collapse = ", "),
    "\n")
cat("Success decision threshold:", design@p_sig_success, "\n")
cat("Futility decision threshold:", design@p_sig_futility, "\n")
cat("Note: Condition-specific decision thresholds are in the conditions object\n")
```

---

## LOW PRIORITY FIXES

### Fix #7: Standardize Roxygen Link Format
**Priority:** LOW  
**Status:** Style inconsistency (not functional)  
**Files:** Multiple `.Rd` files

**Issue:**
Per CLAUDE.md guidelines: "Don't use `\code{\link{function_name}}` in roxygen docs. Use `[functionname()]` instead."

**Current problematic patterns found in:**
- `power_analysis.Rd` line 22: `\code{\link[brms:brm]{brms::brm()}}`
- Others in auto-generated .Rd files

**Note:** These are auto-generated by roxygen2. To fix, update the roxygen source comments in .R files.

**Solution for .R files:**
Change from:
```r
#' function (default empty list)
```
To:
```r
#' function (default empty list). See [brms::brm()] for details.
```

---

## DOCUMENTATION REVIEW NEEDED

### Fix #8: Vignette Review
**Priority:** MEDIUM  
**Status:** Known issue from CLAUDE.md  
**Files:** All `.qmd` files in `vignettes/articles/`

**Issue:** Per CLAUDE.md documentation:
> "Vignettes: Still reference non-existent `power_analysis_ancova()` function - need complete rewriting"

**Files to check:**
- `vignettes/articles/01-introduction.qmd`
- `vignettes/articles/02-prior-specification.qmd`
- `vignettes/articles/03-algorithm-performance.qmd`
- `vignettes/articles/99-model-validation.qmd`

**Action Items:**
1. Search for `power_analysis_ancova(` in all vignette files
2. Replace with correct API usage:
   - Old: `power_analysis_ancova(n_sims = 100)`
   - New: Uses `build_model()` -> `build_design()` -> `build_conditions()` -> `power_analysis()`
3. Update examples to use current API

---

## IMPLEMENTATION CHECKLIST

### Step 1: Roxygen Comment Updates
- [ ] Update `R/class_power_analysis.R` lines 589-607 (remove threshold references)
- [ ] Update `R/class_model.R` line 166 (fix model name)
- [ ] Update `R/class_design.R` line 70 (remove extra parentheses)
- [ ] Update `R/required_fn_args.R` line 7 (fix class name)
- [ ] Enhance `R/class_design.R` Details section (add clarity)
- [ ] Review all link formats in roxygen comments

### Step 2: Regenerate Documentation
```r
# In R console in project directory:
devtools::document()
```
This will regenerate all `.Rd` files in `/man/` directory.

### Step 3: Run Tests
```r
# Verify that examples work
devtools::run_examples()

# Or manually test:
model <- build_model(predefined_model = "ancova_cont_2arms")
design <- build_design(model, target_params = "b_arms_treat", 
                      p_sig_success = 0.975, p_sig_futility = 0.5)
print(design)  # Should not error
```

### Step 4: Check Package
```r
# Run R CMD CHECK
devtools::check()
```

### Step 5: Vignette Review (Separate task)
- [ ] Review all vignette files for old API references
- [ ] Update examples to current API
- [ ] Rebuild vignettes

---

## AFFECTED FILES SUMMARY

### Files Requiring Roxygen Comment Edits:
1. `R/class_power_analysis.R` - Lines 589-607 (print method)
2. `R/class_model.R` - Line 166 (example)
3. `R/class_design.R` - Lines 70, 54-71 (example + documentation)
4. `R/required_fn_args.R` - Line 7 (parameter doc)

### Auto-Generated Files (don't edit directly):
- `man/power_analysis.Rd` - Regenerate from class_power_analysis.R
- `man/build_model.Rd` - Regenerate from class_model.R
- `man/build_design.Rd` - Regenerate from class_design.R
- All other .Rd files in `man/` - Will be regenerated by `devtools::document()`

### Files Requiring Content Review:
- `vignettes/articles/*.qmd` - Review for old API references

---

## TESTING VERIFICATION

After all fixes are applied, verify with:

```r
# Test 1: Create and print all objects
library(rctbayespower)

model <- build_model(predefined_model = "ancova_cont_2arms")
print(model)  # Should work

design <- build_design(
  model = model,
  target_params = "b_arms_treat",
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)
print(design)  # Should work

conditions <- build_conditions(
  design = design,
  condition_values = list(n_total = c(50, 100)),
  static_values = list(
    p_alloc = list(c(0.5, 0.5)),
    thresholds_success = 0.2,
    thresholds_futility = 0
  )
)
print(conditions)  # Should work

power_config <- power_analysis(
  conditions = conditions,
  n_sims = 10,
  n_cores = 1,
  run = FALSE
)
print(power_config)  # Should NOT error (this was broken before)

# Test 2: Run documentation examples
devtools::run_examples()

# Test 3: Package check
devtools::check()
```

---

## COMPLETION CRITERIA

- [ ] All CRITICAL issues resolved (Fixes #1-3)
- [ ] All MEDIUM issues addressed (Fixes #4-6)
- [ ] `devtools::document()` runs without warnings related to these changes
- [ ] `devtools::check()` passes (or only has pre-existing warnings)
- [ ] Manual tests pass without errors
- [ ] All examples run successfully
- [ ] Vignettes reviewed and updated (separate task)

