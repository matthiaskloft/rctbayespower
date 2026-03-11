---
name: workflow
description: Workflow automation overview for plan/implement/ship cycle. Use when starting a new feature, checking workflow status, or understanding the development pipeline.
---

# Development Workflow: Plan / Implement / Ship

## Phase Overview

| Phase | Command | Purpose | Output |
|-------|---------|---------|--------|
| Plan | `/plan <description>` | Design and document a feature | `dev/plans/<name>.md` |
| Implement | `/implement <name>` | Build in a worktree | Code + tests in `feat/<name>` branch |
| Ship | `/ship <name>` | Review, commit, PR, CI | Merged PR |
| Combined | `/implement-ship <name>` | Implement + Ship in one session | Merged PR |

## Plan Document Convention

- Location: `dev/plans/<kebab-case-name>.md`
- Template: `references/plan-template.md`
- Status tracked via STATUS table in each plan doc

## Phase Tracking

Each plan document has a STATUS table:

```markdown
| Phase | Status | Date | Notes |
|-------|--------|------|-------|
| Plan | DONE | 2026-03-10 | Approved by user |
| Phase 1: Core | TODO | | |
| Phase 2: Tests | TODO | | |
| Ship | TODO | | |
```

Statuses: `TODO`, `IN_PROGRESS`, `DONE`, `SHIPPED`, `SKIPPED`

## Pre-Ship Review Checklist (Quick Reference)

Before shipping, verify:
1. All tests pass (`devtools::test()`)
2. R CMD check clean (`devtools::check()`)
3. Naming conventions followed (`/code-patterns`)
4. Roxygen docs in sync
5. No over-engineering (code simplification pass)
6. Dev docs updated if applicable

Full checklist: `references/review-checklist.md`

## Resilience

All commands include fallback chains. If a preferred tool is unavailable, the next alternative is used automatically. See each command's source for details.
