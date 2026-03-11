# Plan a Feature

Design and document an implementation plan for: $ARGUMENTS

## Steps

1. **Sync with remote**:
   ```bash
   git fetch && git pull
   ```

2. **Gather context**:
   - Read `CLAUDE.md` for project conventions
   - Read `dev/99_dev_todos.md` for related TODOs
   - If the description references a dev doc (e.g., "see dev/20_interim..."), read it
   - Explore the relevant codebase area: identify affected files, existing patterns, test coverage

3. **Load the workflow skill** (`/workflow`) to understand the plan/implement/ship pipeline

4. **Read the plan template** from `.claude/skills/workflow/references/plan-template.md`

5. **Draft the plan**:
   - Fill in the template with concrete details
   - Identify all files to create and modify
   - Specify tests needed
   - Define a **verification/validation strategy**: how will correctness be confirmed beyond passing tests? (e.g., manual smoke tests, comparison with known results, numerical checks, backend consistency verification)
   - If the feature touches >3 files or requires >200 LOC: segment into numbered phases, each independently testable
   - List design decisions with options and rationale

6. **Present plan to user**:
   - Show the full draft
   - Highlight key design decisions that need user input
   - Ask about scope, phasing, and any concerns

7. **After user approval — critical review**:
   - Launch `plan-reviewer` agent (subagent_type="plan-reviewer") with the plan content
   - If agent is unavailable, perform manual critical review: check architectural fit, naming compliance, edge cases, test strategy, API consistency
   - Present review findings to user

8. **Incorporate feedback** from both user and reviewer

9. **Check for existing plan**:
   - Check if `dev/plans/<feature-name>.md` already exists
   - If yes: ask user whether to overwrite, append, or use a different name

10. **Save the plan** to `dev/plans/<feature-name>.md`
    - Use kebab-case for the filename
    - Set Phase "Plan" status to DONE with today's date

11. **Report next step**: Tell user to run `/implement <feature-name>` to start building

## Naming the Plan

Convert the feature description to a kebab-case name:
- "add interim analysis" → `interim-analysis`
- "support multiple endpoints" → `multiple-endpoints`
- If unclear, ask the user for a short name

## Rules

- Do NOT start implementation — only plan
- Do NOT create worktrees or branches
- Keep the plan focused and actionable — avoid vague TODOs
- Each phase should be independently testable and shippable
- Reference existing code patterns and conventions from `/code-patterns`
