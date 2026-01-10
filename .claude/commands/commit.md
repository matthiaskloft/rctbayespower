# Commit Workflow

Create a well-formatted git commit for the current changes.

## Steps

1. Run `git status` to see all changes
2. Run `git diff` to see unstaged changes
3. Run `git diff --staged` to see staged changes
4. Run `git log --oneline -5` to see recent commit style
5. Analyze all changes to determine commit type:
   - `Add` = new feature
   - `Update` = enhancement to existing feature
   - `Fix` = bug fix
   - `Refactor` = code restructuring
   - `Docs` = documentation only
   - `Test` = test changes only
6. Stage relevant files with `git add`
7. Create commit with message format:
   ```
   <type>: <concise summary in imperative mood>

   <optional body explaining why, not what>

   Co-Authored-By: Claude <noreply@anthropic.com>
   ```
8. Run `git status` to verify success

## Rules

- Focus on "why" not "what" in the body
- Use imperative mood ("Add feature" not "Added feature")
- Keep summary under 72 characters
- Do NOT commit .env, credentials, or secret files
