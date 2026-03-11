# Implement a Feature

Implement the planned feature: $ARGUMENTS

Parse arguments: `<feature-name>` and optional `--phase N`.

## Steps

1. **Read the plan**:
   - Read `dev/plans/<feature-name>.md`
   - If it doesn't exist, tell the user to run `/plan` first and stop

2. **Determine phase**:
   - If `--phase N` specified, implement that phase
   - Otherwise, scan the STATUS table for the first phase with status `TODO` — implement that one
   - If all phases are DONE, report that and stop

3. **Create worktree**:
   - Branch name: `feat/<feature-name>` (single phase) or `feat/<feature-name>-phase-N` (multi-phase)
   - ```bash
     git worktree add ../rctbayespower-<feature-name> -b feat/<feature-name>
     ```
   - If branch already exists, check it out instead of creating:
     ```bash
     git worktree add ../rctbayespower-<feature-name> feat/<feature-name>
     ```
   - Change working directory to the worktree

4. **Implement**:
   - Follow the plan's file-by-file instructions for the current phase
   - Use `/code-patterns` skill for naming conventions
   - Use `/s7-classes` skill if S7 class work is needed
   - For test writing: delegate to `test-writer` agent if available, otherwise write tests manually
   - Stay within the plan's scope — note out-of-scope discoveries in step 6

5. **Update dev docs**:
   - Add any out-of-scope issues to `dev/99_dev_todos.md`
   - Add implementation notes to the plan's NOTES section

6. **Update plan STATUS**:
   - Mark the current phase as `DONE` with today's date
   - Update in `dev/plans/<feature-name>.md`

7. **Verify**:
   - Run `/verify-package` skill if available
   - Fallback: run `devtools::test()` and `devtools::check()` manually
   - If issues found: fix them before reporting success

8. **Report next step**:
   - If more phases remain: "Run `/implement <feature-name>` for the next phase"
   - If all phases done: "Run `/ship <feature-name>` to review, commit, and create a PR"

## Rules

- Always work in a worktree — never implement directly on main
- Follow the plan — don't improvise major design changes
- If the plan seems wrong during implementation, note the issue and ask the user before deviating
- Keep commits atomic — one logical change per commit
- Don't ship from this command — that's `/ship`'s job
