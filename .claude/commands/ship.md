# Ship a Feature

Review, commit, and create a PR for: $ARGUMENTS

Parse arguments: `<feature-name>`.

## Pre-Commit Review Cycle (max 3 iterations)

1. **Get changed files**:
   ```bash
   git diff --name-only main...HEAD
   ```

2. **Code simplification** (fallback chain):
   - Try: `/code-simplifier` skill (local)
   - Alternative: `pr-review-toolkit:code-simplifier` agent
   - Fallback: manually apply simplification checklist from `.claude/skills/code-simplifier/SKILL.md`

3. **Code review** (fallback chain):
   - Try: review changed files against `/code-patterns` conventions
   - Alternative: `feature-dev:code-reviewer` or `pr-review-toolkit:code-reviewer` agent
   - Fallback: read each changed file and check conventions manually

4. **Naming check** (fallback chain):
   - Try: `naming-auditor` agent (local)
   - Fallback: grep-based naming check per `/code-patterns` conventions

5. **Fix all issues** found in steps 2-4

6. **Verify package** (fallback chain):
   - Try: `/verify-package` skill
   - Fallback: `devtools::test()` + `devtools::check()`

7. **If new issues found**, repeat from step 2 (max 3 iterations total)

## Commit and PR

8. **Commit** using `/commit` command (or `commit-commands:commit` skill)

9. **Push**:
   ```bash
   git push -u origin <branch-name>
   ```

10. **Create PR**:
    - Read the plan from `dev/plans/<feature-name>.md` for title and context
    - ```bash
      gh pr create --title "<title from plan>" --body "$(cat <<'EOF'
      ## Summary
      <bullets summarizing changes>

      ## Test plan
      <checklist of test verification>

      🤖 Generated with [Claude Code](https://claude.com/claude-code)
      EOF
      )"
      ```

## CI Monitoring

11. **Poll PR status** (fallback chain):
    - Try: `/loop 1m` with `gh pr checks` if available
    - Fallback: inline polling loop:
      ```bash
      gh pr checks <PR-URL>
      gh api repos/{owner}/{repo}/pulls/{number}/comments
      ```
    - Fix issues → commit → push
    - Stop when: all checks pass AND no unaddressed review comments
    - Max 30 iterations (timeout after 30 minutes)

## Cleanup (only after merge)

12. **Verify merge**:
    ```bash
    gh pr view <PR-URL> --json state
    ```
    - Must show `MERGED`. If not merged yet: report PR URL and stop. User merges manually and runs cleanup later.

13. **Clean up**:
    ```bash
    git checkout main
    git pull
    git worktree remove <worktree-path>
    git branch -d <branch-name>
    ```

14. **Update plan STATUS**: mark as `SHIPPED` with today's date

## Rules

- Never force-push unless explicitly asked
- Don't merge the PR — the user merges (or auto-merge handles it)
- If CI fails 3 times on the same issue, stop and ask the user
- If review comments require design changes, stop and discuss with user
- Report which tools were used vs. which fallbacks were needed
