---
name: verify-package
description: Verify package quality before committing. Run R CMD check, tests, and documentation builds. Use after significant changes. (project)
---

# Package Verification

Boris Cherny's #1 tip: "Give Claude a way to verify its work" - this 2-3x the quality.

## Quick Verification Sequence

```r
# 1. Regenerate documentation
devtools::document()

# 2. Run tests
devtools::test()

# 3. Full package check
devtools::check()
```

## Success Criteria

| Check | Target |
|-------|--------|
| Errors | 0 |
| Warnings | 0 |
| Notes | Minimize (some acceptable for S7/brms) |
| Tests | All pass |

## When to Use

- After implementing a new feature
- After refactoring existing code
- Before creating a commit
- Before creating a pull request

## Verification Loop Pattern

```
implement → test → fix → test → verify → commit
```

Keep iterating until all checks pass.

## Common Issues

Use `/r-cmd-check` skill for fixing common R CMD check issues.

### Acceptable Notes

These notes are expected and can be ignored:
- S7 class registration notes
- brms/Stan compilation notes
- Large data file notes (if intentional)

## Full Verification Checklist

1. [ ] `devtools::document()` runs without warnings
2. [ ] `devtools::test()` - all tests pass
3. [ ] `devtools::check()` - 0 errors, 0 warnings
4. [ ] Examples run without error
5. [ ] Vignettes build (if modified)
