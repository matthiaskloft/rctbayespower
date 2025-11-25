# Quick Reference: Roxygen Documentation Issues

## Three CRITICAL Bugs to Fix NOW

### Bug #1: Remove Non-existent Properties (CRASH)
**File:** `R/class_power_analysis.R`  
**Lines:** 599, 602
**Current Code:**
```r
cat("Success thresholds:",
    paste(design@thresholds_success, collapse = ", "),  # ERROR!
    "\n")
cat("Futility thresholds:",
    paste(design@thresholds_futility, collapse = ", "),  # ERROR!
    "\n")
```
**Fix:** DELETE these 6 lines entirely

---

### Bug #2: Fix Model Name
**File:** `R/class_model.R`  
**Line:** 166
**Current:**
```r
#' ancova_model <- build_model(predefined_model = "ancova_cont")
```
**Fix to:**
```r
#' ancova_model <- build_model(predefined_model = "ancova_cont_2arms")
```

---

### Bug #3: Remove Invalid Function Call
**File:** `R/class_design.R`  
**Line:** 70
**Current:**
```r
#' ancova_model <- build_model("ancova_cont_2arms")()
```
**Fix to:**
```r
#' ancova_model <- build_model(predefined_model = "ancova_cont_2arms")
```

---

## Two More MEDIUM Fixes

### Fix #4: Correct Class Name
**File:** `R/required_fn_args.R`  
**Line:** 7
**Current:** `rctbayespower_model`  
**Fix to:** `rctbp_model`

---

### Fix #5: Document Thresholds Better
**File:** `R/class_design.R`  
**Section:** Details (after line 73)

Add this clarification:
```r
#' \strong{Design Parameters vs Condition Parameters:}
#' This design object specifies global analysis decision criteria 
#' (p_sig_success, p_sig_futility). The specific decision thresholds
#' (thresholds_success, thresholds_futility) are defined per-condition
#' in the conditions object created by [build_conditions()].
```

---

## After Making Changes

```r
# In R console:
setwd("path/to/rctbayespower")
devtools::document()
devtools::check()
```

---

## Testing

```r
library(rctbayespower)

# Test 1: build_model()
m <- build_model(predefined_model = "ancova_cont_2arms")
print(m)  # Should work

# Test 2: build_design()
d <- build_design(
  model = m,
  target_params = "b_arms_treat",
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)
print(d)  # Should work

# Test 3: power_analysis (THE CRITICAL ONE)
c <- build_conditions(
  design = d,
  condition_values = list(n_total = c(50, 100)),
  static_values = list(
    p_alloc = list(c(0.5, 0.5)),
    thresholds_success = 0.2,
    thresholds_futility = 0
  )
)

p <- power_analysis(
  conditions = c,
  n_sims = 10,
  run = FALSE
)
print(p)  # This was CRASHING before - should work now
```

---

## Report Files

All detailed information is in `/dev/`:

1. **roxygen_audit_SUMMARY.txt** - Executive summary (start here)
2. **roxygen_documentation_audit.md** - Detailed issues with examples
3. **roxygen_fixes_action_plan.md** - Step-by-step fix guide

