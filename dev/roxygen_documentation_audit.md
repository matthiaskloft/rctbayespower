# Roxygen Documentation Consistency Check Report
## rctbayespower Package
### Generated: 2025-11-24

---

## CRITICAL ISSUES FOUND

### 1. **CRITICAL: Property Name Mismatch in print.rctbp_power_analysis()**
**Severity: CRITICAL** - Runtime Error
**Location:** `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/R/class_power_analysis.R` (lines 599, 602)
**Issue:** The print method references non-existent properties `design@thresholds_success` and `design@thresholds_futility`
**Details:**
- Line 599: `paste(design@thresholds_success, collapse = ", ")`
- Line 602: `paste(design@thresholds_futility, collapse = ", ")`
- The actual rctbp_design class properties are:
  - `target_params` (character vector)
  - `p_sig_success` (numeric)
  - `p_sig_futility` (numeric)
  - `design_name` (optional character)
  - `model` (rctbp_model object)
**Impact:** This code will crash at runtime with error: `Error: cannot get property 'thresholds_success' from object of class 'rctbp_design'`
**Referenced in .Rd file:** `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/man/print.rctbp_power_analysis.Rd` (auto-generated)

### 2. **CRITICAL: Incorrect Model Reference in build_model() Example**
**Severity: CRITICAL** - Non-existent model name
**Location:** `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/R/class_model.R` (line 166)
**Issue:** Documentation example references non-existent predefined model
**Details:**
```r
# Line 166 in roxygen documentation
ancova_model <- build_model(predefined_model = "ancova_cont")
```
**Problem:** The predefined model is named `"ancova_cont_2arms"` or `"ancova_cont_3arms"`, not `"ancova_cont"`
**Impact:** Example code will fail with: `Error: Model 'ancova_cont' not found.`
**Available models:** `"ancova_cont_2arms"`, `"ancova_cont_3arms"`
**Referenced in .Rd file:** `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/man/build_model.Rd`

### 3. **Incorrect Function Name in build_design() Example**
**Severity: CRITICAL** - Non-existent function call
**Location:** `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/R/class_design.R` (line 107)
**Issue:** Example calls non-existent function
**Details:**
```r
# Line 70 in example section
ancova_model <- build_model("ancova_cont_2arms")()
                                                   ^^
```
**Problem:** `build_model()` returns an S7 model object, not a callable function. The `()` should be removed.
**Impact:** Example code will fail with type error when trying to call an S7 object
**Correct usage:** `ancova_model <- build_model(predefined_model = "ancova_cont_2arms")`
**Referenced in .Rd file:** `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/man/build_design.Rd`

---

## DOCUMENTATION INCONSISTENCIES

### 4. **Inconsistent Parameter Documentation**
**Severity: MEDIUM** - Documentation mismatch
**Location:** `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/R/required_fn_args.R` (line 7)
**Issue:** Parameter documentation references wrong class name
**Details:**
```r
#' @param object Either an rctbp_design or rctbayespower_model object
```
**Problem:** Class is named `rctbp_model`, not `rctbayespower_model`
**Impact:** Confusing for users reading documentation
**Correct form:** `Either an rctbp_design or rctbp_model object`

### 5. **Missing Documentation for S7 Properties**
**Severity: MEDIUM** - Incomplete documentation
**Location:** `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/R/class_design.R` (lines 54-71)
**Issue:** Documentation doesn't mention that decision criteria (thresholds, interim schedules) are per-condition
**Details:** The documentation states "Specific decision criteria (thresholds, interim schedules) are defined per-condition in the conditions object" but the actual rctbp_design class doesn't have threshold properties - they're passed per-condition.
**Documentation Status:** Confusing because it mentions thresholds_success/thresholds_futility that don't exist as class properties
**Impact:** Users may be confused about where to specify success/futility thresholds

### 6. **Stale Reference in print.rctbp_power_analysis**
**Severity: MEDIUM** - References non-existent properties
**Location:** `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/R/class_power_analysis.R` (lines 598-607)
**Issue:** Print method tries to access properties that don't belong to rctbp_design
**Details:**
The print method references design properties that don't exist in the rctbp_design class definition. These thresholds are stored per-condition in the conditions object, not in the design object.
**Impact:** Code will crash whenever a power_analysis object is printed
**Required Fix:** Remove lines accessing `design@thresholds_success` and `design@thresholds_futility` from the print method

---

## MINOR DOCUMENTATION ISSUES

### 7. **Vignette References**
**Status:** PARTIAL - Some vignettes may reference old API
**Location:** `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/vignettes/articles/`
**Note:** Per CLAUDE.md: "Vignettes: Still reference non-existent `power_analysis_ancova()` function - need complete rewriting"
**Files affected:** Vignette articles in `/vignettes/articles/`
**Current status:** Not checked in detail, but documented as known issue

### 8. **Link References in Documentation**
**Severity: LOW** - Style issue
**Location:** Multiple .Rd files
**Issue:** Some documentation uses `\code{\link{function_name}}` instead of `[functionname()]`
**Example:** Found in power_analysis.Rd line 22 and other files
**Per CLAUDE.md guideline:** "Don't use \code{\link{function_name}} in roxygen docs. Use [functionname()] instead."
**Impact:** Style inconsistency, not functional issue
**Status:** Roxygen generates these automatically; fix in source roxygen comments

---

## SUMMARY TABLE

| Issue ID | Severity | Component | File | Type |
|----------|----------|-----------|------|------|
| 1 | CRITICAL | print.rctbp_power_analysis | class_power_analysis.R | Code-Doc Mismatch |
| 2 | CRITICAL | build_model | class_model.R | Wrong API Name |
| 3 | CRITICAL | build_design | class_design.R | Invalid Example |
| 4 | MEDIUM | required_fn_args | required_fn_args.R | Class Name Mismatch |
| 5 | MEDIUM | build_design | class_design.R | Incomplete Doc |
| 6 | MEDIUM | class_power_analysis | class_power_analysis.R | Property Mismatch |
| 7 | MEDIUM | Vignettes | vignettes/ | API Reference |
| 8 | LOW | Multiple files | *.Rd | Link Format |

---

## RECOMMENDATIONS

### High Priority (Fix Immediately)
1. **Fix print.rctbp_power_analysis()** - Remove references to non-existent thresholds properties
2. **Fix build_model() example** - Change `"ancova_cont"` to `"ancova_cont_2arms"`
3. **Fix build_design() example** - Remove `()` from `build_model("ancova_cont_2arms")()`

### Medium Priority
4. Update required_fn_args.R parameter documentation
5. Clarify in build_design() docs that thresholds are per-condition
6. Review vignette examples for API correctness

### Low Priority
7. Standardize link format in roxygen comments ([\code{function}()] style)

---

## FILES REQUIRING UPDATES

### Must Edit (roxygen source comments):
- `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/R/class_model.R` - Lines 163-167
- `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/R/class_design.R` - Lines 104-117
- `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/R/class_power_analysis.R` - Lines 589-607
- `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/R/required_fn_args.R` - Line 7

### After editing roxygen comments:
- Run `devtools::document()` to regenerate .Rd files in `/man/` directory

### Vignettes to review:
- `/mnt/c/Users/Matze/Documents/GitHub/rctbayespower/vignettes/articles/*.qmd` - Check for `power_analysis_ancova()` references

