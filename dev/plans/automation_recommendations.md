# Automation Recommendations Plan

> Created: 2026-03-11
> Status: Draft

## Overview

Fill remaining automation gaps identified by codebase analysis. The setup is already mature (7 skills, 5 agents, 9 commands, 3 hooks). These are targeted additions.

---

## Phase 1: context7 MCP Server

**Goal**: Live documentation lookup for brms, S7, cli, rlang, testthat without web searching.

**Steps**:
1. Install: `claude mcp add context7 -- npx -y @upstash/context7-mcp`
2. Verify it resolves docs for key dependencies (brms, S7, cli)
3. Add to `.mcp.json` if sharing with team

**Effort**: ~5 minutes
**Risk**: Low — read-only tool, no side effects

---

## Phase 2: Validation Skill + Agent

**Goal**: Orchestrated validation of the full pipeline across all models, backends, and features. The skill (`/rctbp-validation`) orchestrates; the agent (`integration-tester`) does the heavy lifting.

### Skill: `/rctbp-validation`

Project-specific name to avoid masking generic validation skills.

**Responsibilities** (orchestrator):
1. Read validation checklist (models × backends × design types × features)
2. Spawn `integration-tester` agent runs — parallel where independent
3. Collect pass/fail results from each agent run
4. Report summary matrix (model × backend × feature → pass/fail)
5. Flag failures for follow-up

**Steps**:
1. Create `.claude/skills/rctbp-validation/SKILL.md`
2. Define validation matrix:
   - All predefined models (`ancova_cont_2arms`, `ancova_bin_2arms`)
   - Both backends (brms, BayesFlow)
   - Fixed and group-sequential designs
   - Accrual, dropout, event-driven subsetting
   - All boundary functions
   - Decision criteria (ROPE effectiveness/futility)
   - Edge cases (single condition, single look, all-stop, all-continue)
3. Include orchestration logic: which combinations to run in parallel, how to aggregate results
4. Reference `dev/plans/` for the full validation strategy once planned

### Agent: `integration-tester`

**Responsibilities** (worker):
- Runs one pipeline end-to-end (design → conditions → power analysis → results) for a given model/backend/feature combination
- Returns structured pass/fail with diagnostics on failure
- Stateless — knows nothing about what else is being validated

**Steps**:
1. Create `.claude/agents/integration-tester.md`
2. Scope: single pipeline run, structured output
3. Model: sonnet (cost-effective for test execution)
4. Register in CLAUDE.md agents table

**Flow**:
```
User: /rctbp-validation
  → Skill reads validation matrix
  → Spawns integration-tester agents (parallel where independent)
  → Collects results
  → Reports summary matrix (pass/fail per combination)
  → Flags failures for follow-up
```

**Effort**: ~1.5 hours (skill + agent together)
**Risk**: Low — read-only analysis + test execution
**Dependency**: Benefits from v1 Feature Spec (TODO #1) to know exactly what to validate

---

## Phase 4: Related Test Hook (Optional)

**Goal**: Auto-run related tests when editing R source files.

**Steps**:
1. Create hook script `.claude/hooks/run_related_tests.sh`
   - Map `R/foo.R` → `tests/testthat/test-foo.R` (if exists)
   - Run `testthat::test_file()` on the matched test file
   - Return pass/fail status
2. Add as PostToolUse hook for Edit/Write on `R/*.R`
3. Gate behind env var `RCTBP_AUTO_TEST=TRUE` (opt-in, off by default)

**Trade-offs**:
- Pro: Catches regressions immediately during editing
- Con: Slows editing flow if tests are slow (brms compilation)
- Mitigation: Only run unit tests, skip integration/brms tests via env flag

**Effort**: ~1 hour
**Risk**: Medium — could slow workflow if not gated properly
**Dependency**: None, but implement last to avoid disrupting current flow

---

## Implementation Order

| Phase | Item | Effort | Dependencies |
|-------|------|--------|-------------|
| 1 | context7 MCP | 5 min | None |
| 2 | `/rctbp-validation` skill + `integration-tester` agent | 1.5 hours | Benefits from v1 feature spec |
| 4 | Related test hook (optional) | 1 hour | None |

## Not Recommended

- **Additional MCP servers**: GitHub needs covered by `gh` CLI permissions. No database, no external APIs to integrate.
- **Additional plugins**: Current plugin set (commit-commands, pr-review-toolkit, feature-dev, workflow-automation) is sufficient.
- **Additional hooks**: file_guard + styler + roxygen cover the critical paths. More hooks risk slowing the editing loop.
