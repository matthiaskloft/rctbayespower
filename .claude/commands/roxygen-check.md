# Roxygen Documentation Check

Verify that roxygen documentation matches actual code.

## Steps

1. For each exported function in R/*.R files:
   - Extract @param entries from roxygen block
   - Extract actual function parameters from signature
   - Compare and report mismatches

2. Check for these issues:
   - **Missing @param**: Parameter in function but not documented
   - **Orphaned @param**: Documented but not in function
   - **Wrong default**: @param says "default X" but code has "default Y"
   - **Renamed param**: Old name in docs, new name in code

3. Report each issue with file:line reference

4. For each issue, provide the fix

## Common Mismatches

| Issue | Example | Fix |
|-------|---------|-----|
| Default mismatch | Docs say 0.95, code has 0.975 | Update @param text |
| Missing param | New `verbose` arg undocumented | Add @param verbose |
| Orphaned param | @param old_name but arg renamed | Update @param name |

## Verification Command

```r
# Check for roxygen warnings
devtools::document()
# Any "Documented arguments but not in usage" = orphaned
# Any "Undocumented arguments" = missing
```

## Rules

- @param descriptions should NOT repeat the default value if it's clear from the signature
- Exception: Complex defaults benefit from explanation
- Use `[function()]` for cross-references, not `\code{\link{}}`
