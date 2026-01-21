# Roxygen Sync

Use the `roxygen-sync` subagent to check and fix roxygen documentation.

**Reference**: `/roxygen` skill for documentation templates and conventions.

## Usage

```
/roxygen-sync                     # All R files
/roxygen-sync class_design.R      # Specific file
/roxygen-sync build_design        # Specific function
/roxygen-sync class_*.R           # Glob pattern
```

## Instructions

Launch the `roxygen-sync` subagent with a task based on the arguments.

**Context:** All R source files are in the `R/` directory.

**No arguments:**
> Scan all R files in R/ for roxygen documentation mismatches. Report each issue with file:line reference, then fix all issues found. After fixing, run devtools::document() then devtools::check(args = "--no-tests") to verify.

**File argument (ends in `.R`):**
> Scan R/{argument} for roxygen documentation mismatches. Report each issue with file:line reference, then fix all issues found. After fixing, run devtools::document() then devtools::check(args = "--no-tests") to verify.

**Glob pattern (contains `*`):**
> Scan R/{argument} for roxygen documentation mismatches. Report each issue with file:line reference, then fix all issues found. After fixing, run devtools::document() then devtools::check(args = "--no-tests") to verify.

**Function name argument:**
> Find the function `{argument}` in R/ files, then check its roxygen documentation for mismatches. Report issues with file:line reference, fix them, and run devtools::document() then devtools::check(args = "--no-tests") to verify.
