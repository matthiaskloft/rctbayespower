# R Package Check

Run R CMD check and analyze results.

## Steps

1. Run `devtools::check()` to perform full package check
2. Parse output for:
   - **ERRORS** (must fix)
   - **WARNINGS** (should fix)
   - **NOTES** (review and address if reasonable)
3. For each issue:
   - Identify the file and line number
   - Read the relevant code
   - Suggest a fix
4. If issues found, use `/r-cmd-check` skill for common fixes
5. Report summary: X errors, Y warnings, Z notes

## Common Issues

| Issue | Fix |
|-------|-----|
| "no visible binding for global variable" | Add to globalVariables() in rctbayespower-package.R |
| "Documented arguments but not in usage" | Update roxygen @param or function signature |
| "Undocumented arguments" | Add @param entries |
| "S3 method not registered" | Add to .onLoad() or use @exportS3Method |

## Success Criteria

- 0 errors
- 0 warnings
- Notes minimized (some acceptable for S7/brms)
