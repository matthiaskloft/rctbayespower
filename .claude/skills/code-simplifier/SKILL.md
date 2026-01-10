---
name: code-simplifier
description: Simplify code after implementation. Remove over-engineering, unnecessary abstractions, and dead code. Use after completing a feature. (project)
---

# Code Simplification

Run this skill after completing an implementation to clean up and simplify the code.

## Simplification Checklist

### Remove Dead Code
- [ ] Unused variables
- [ ] Unused functions
- [ ] Commented-out code blocks
- [ ] Unreachable code paths

### Simplify Logic
- [ ] Flatten deeply nested conditionals
- [ ] Replace complex boolean expressions with named variables
- [ ] Remove unnecessary else branches after return/stop

### Remove Over-Engineering
- [ ] Replace abstractions used only once with inline code
- [ ] Remove premature optimization
- [ ] Simplify class hierarchies if inheritance isn't needed
- [ ] Remove unused function parameters

### Code Consolidation
- [ ] Consolidate duplicate code (only if 3+ occurrences)
- [ ] Move repeated magic values to named constants
- [ ] Combine related small functions if they're always used together

## Anti-Patterns to Remove

| Pattern | Simplification |
|---------|----------------|
| Wrapper function that just calls another | Remove wrapper, use direct call |
| Config object with one field | Use direct parameter |
| Factory that creates one type | Use constructor directly |
| Empty catch blocks | Remove or add proper handling |
| Redundant type checks | Trust the type system |

## When NOT to Simplify

- Code that's genuinely complex for good reason
- Well-documented intentional patterns
- Performance-critical hot paths
- Public API (breaking changes)

## Verification After Simplification

After simplifying, always run:
```r
devtools::test()
devtools::check()
```
