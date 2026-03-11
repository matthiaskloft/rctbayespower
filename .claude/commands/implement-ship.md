# Implement and Ship a Feature

Combined workflow for: $ARGUMENTS

Parse arguments: `<feature-name>` and optional `--phase N`.

This command chains `/implement` and `/ship` in a single session. Useful for small features or individual phases that can be completed without a context break.

## Steps

### Phase 1: Implement

Follow all steps from the `/implement` command:
1. Read plan from `dev/plans/<feature-name>.md`
2. Determine which phase to implement
3. Create worktree with branch `feat/<feature-name>`
4. Implement following the plan
5. Update dev docs and plan STATUS
6. Run `/verify-package`

### Gate Check

After implementation completes, verify:
- All tests pass
- R CMD check is clean (no errors or warnings)

**If gate check fails**: report the problems and stop. User fixes manually, then runs `/ship` separately.

**If gate check passes**: proceed to Phase 2.

### Phase 2: Ship

Follow all steps from the `/ship` command:
1. Pre-commit review cycle (simplification, code review, naming check)
2. Commit with `/commit`
3. Push and create PR
4. Monitor CI
5. Cleanup after merge

## When to Use This vs. Separate Commands

| Scenario | Recommendation |
|----------|---------------|
| Small feature (<100 LOC) | `/implement-ship` |
| Single phase of a larger feature | `/implement-ship --phase N` |
| Large feature (>300 LOC) | `/implement` then `/ship` separately |
| Uncertain scope | `/implement` first, assess, then `/ship` |

## Rules

- All rules from both `/implement` and `/ship` apply
- If context window is getting large after implementation, recommend splitting into separate `/ship` call
- Don't skip the gate check between phases
