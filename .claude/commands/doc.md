# Update Documentation

Regenerate package documentation from roxygen comments.

## Steps

1. Run `devtools::document()` to regenerate man/*.Rd files
2. Check output for warnings:
   - Missing @param entries
   - Orphaned @param entries
   - Missing @return
   - Invalid cross-references
3. If warnings found, fix the roxygen comments in the source .R file
4. Re-run `devtools::document()` until clean
5. Optionally rebuild README: `devtools::build_readme()`

## Common Warnings

| Warning | Fix |
|---------|-----|
| "Documented arguments but not in usage" | Remove @param or add argument to function |
| "Undocumented arguments" | Add @param entry |
| "Unknown tag" | Check roxygen syntax |
| "Missing @return" | Add @return description |

## Rules

- NEVER manually edit man/*.Rd files
- Always edit roxygen comments in .R files
- Run devtools::document() after any roxygen changes
- Edit README.Rmd, not README.md
