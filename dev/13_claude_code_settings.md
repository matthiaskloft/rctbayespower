# Claude Code Settings Configuration

This document explains the Claude Code settings configuration for the rctbayespower package, including permissions, hooks, and security considerations.

## Settings File Locations

Claude Code uses a hierarchical settings system:

| Location | Scope | Purpose | Version Control |
|----------|-------|---------|-----------------|
| `~/.claude/settings.json` | User (global) | Personal preferences across all projects | Not committed |
| `.claude/settings.json` | Project (shared) | Team settings, shared permissions | Committed to git |
| `.claude/settings.local.json` | Project (local) | Personal overrides for this project | Git-ignored |

**Precedence** (highest to lowest):
1. Enterprise managed policies
2. Command-line arguments
3. Local project settings (`.claude/settings.local.json`)
4. Shared project settings (`.claude/settings.json`)
5. User settings (`~/.claude/settings.json`)

Settings merge hierarchically, with more specific configurations overriding broader ones.

## Project Files

### `.claude/settings.json` (Shared)

Contains team-shared settings committed to version control:

- **Allow rules**: R package development tools and common commands
- **Deny rules**: Dangerous bash commands (`rm -rf`, `curl`, `wget`)
- **Hooks**: File protection and R development automation
- **Environment**: Project-specific environment variables

### `.claude/settings.local.json` (Local)

Personal overrides, git-ignored by default:

- `WebSearch` - Optional web search access
- `WebFetch(domain:github.com)` - GitHub access for documentation
- `Bash(git push:*)` - Personal choice to auto-allow pushes
- `Bash(rm:*)` - Safe single-file deletion (not recursive)

### `dev/claude_settings_permissions.json` (Template)

Full template that can be copied to `.claude/settings.json` for other R projects.

## Permission Pattern Syntax

### Bash Commands (Prefix Matching)

```json
"Bash(npm run lint)"      // Exact match only
"Bash(git commit:*)"      // Prefix match - matches "git commit -m ..."
"Bash(Rscript -e:*)"      // Matches any Rscript one-liner
```

**Important**: This is prefix matching, not regex. The `:*` suffix means "any arguments".

### File Operations (Glob Patterns)

```json
"Read"                    // Allow all reads
"Edit(R/**)"              // All files under R/ recursively
"Write(tests/*.R)"        // R files directly in tests/
"Edit(DESCRIPTION)"       // Specific file
```

Uses gitignore-style glob patterns.

### Tools Without Arguments

```json
"Glob"                    // File pattern search
"Grep"                    // Content search
"WebSearch"               // Web search
```

## Known Limitation: File Deny Rules Don't Work

**⚠️ CRITICAL**: File operation deny rules (Read, Edit, Write) [do not work](https://github.com/anthropics/claude-code/issues/6699) in Claude Code. This is a known bug. Only Bash deny rules are enforced.

**Workaround**: We use a PreToolUse hook (`.claude/hooks/file_guard.py`) to intercept and block access to sensitive files.

### Files Blocked by the Hook

The `file_guard.py` script blocks access to:

| Pattern | Description |
|---------|-------------|
| `.env`, `.env.*` | Environment files with secrets |
| `.Renviron` | R environment file |
| `credentials*` | Credential files |
| `*.pem`, `*.key` | SSL certificates and private keys |
| `*secret*` | Any file with "secret" in path |
| `*password*` | Any file with "password" in path |
| `*api_key*`, `*api-key*` | API key files |
| `id_rsa`, `id_ed25519` | SSH private keys |

### How the Hook Works

1. Claude initiates a Read/Edit/Write operation
2. PreToolUse hook receives the file path via stdin (JSON)
3. Hook checks if path matches any sensitive pattern
4. If sensitive: returns JSON with `"permissionDecision": "deny"`
5. If safe: exits with code 0 (allow)

## R Development Automation Hooks

PostToolUse hooks automate common R development tasks:

### Auto-Styling

When R files are edited, automatically runs:
```r
styler::style_file('path/to/file.R')
```

Ensures consistent code formatting across the project.

### Auto-Documentation

When R files or DESCRIPTION are edited, automatically runs:
```r
devtools::document()
```

Keeps roxygen documentation and NAMESPACE in sync.

**Note**: These hooks run after successful tool completion. If they fail, Claude receives feedback but the original edit is not reverted.

## Customization

### Adding More Sensitive File Patterns

Edit `.claude/hooks/file_guard.py` and add patterns to `BLOCKED_PATTERNS`:

```python
BLOCKED_PATTERNS = [
    r"\.env$",
    r"my_custom_secret",  # Add your pattern here
    # ...
]
```

### Disabling Automation Hooks

To disable auto-styling or auto-documentation, remove the corresponding PostToolUse entries from `.claude/settings.json`:

```json
{
  "hooks": {
    "PostToolUse": []  // Empty array disables all
  }
}
```

### Adding Personal Permissions

Add to `.claude/settings.local.json` (not committed):

```json
{
  "permissions": {
    "allow": [
      "WebSearch",
      "Bash(docker:*)",  // Your custom permission
      "Read(~/.config/myapp/*)"
    ]
  }
}
```

## Security Considerations

1. **Bash deny rules work**: `rm -rf`, `curl`, `wget` are blocked
2. **File deny rules don't work**: Use the hook instead
3. **git push in local settings**: Personal choice, not team-enforced
4. **Fail-open design**: Hook errors allow operations (to avoid breaking legitimate work)

## Skills

Four skills are available for rctbayespower development, providing progressive disclosure of project patterns:

### `code-patterns`

Essential code patterns for writing rctbayespower code. Triggered when writing new code, adding functions, or creating R files.

**Contains:**
- Naming conventions (rctbp_*, pwr_*, pr_*, dec_*)
- File header template
- Error handling pattern (cli::cli_abort)
- S7 class template

**Location:** `.claude/skills/code-patterns/SKILL.md`

### `r-cmd-check`

Fix R CMD check issues. Triggered when R CMD check has errors, warnings, or notes.

**Contains:**
- globalVariables location (`R/rctbayespower-package.R`)
- Roxygen defaults mismatch fix
- Roxygen link syntax fix
- Common issues table

**Location:** `.claude/skills/r-cmd-check/SKILL.md`

### `s7-classes`

Write S7 class definitions and methods. Triggered when creating new S7 classes, adding properties, defining validators, or registering methods.

**Structure:** Multi-file with references
```
s7-classes/
├── SKILL.md                    # Class template, property types
├── references/
│   ├── s7-patterns.md          # Computed properties, validation, methods
│   └── package-setup.md        # zzz.R, S3 wrappers, inheritance
└── S7_0-2-1_manual.html        # Complete S7 reference manual
```

**Location:** `.claude/skills/s7-classes/SKILL.md`

### `skill-creator`

Create new Claude Code skills. Triggered when writing a new skill or learning skill best practices.

**Structure:** Multi-file with references
```
skill-creator/
├── SKILL.md                           # Skill structure, template
└── references/
    └── skill-best-practices.md        # Progressive disclosure, context efficiency
```

**Location:** `.claude/skills/skill-creator/SKILL.md`

### Progressive Disclosure

```
Layer 1: CLAUDE.md (overview)           → Always loaded
Layer 2: Skills (essential patterns)    → Loaded when triggered
Layer 3: dev/ docs (full reference)     → Deep detail when needed
```

## References

- [Claude Code Settings Documentation](https://code.claude.com/docs/en/settings)
- [Hooks Reference](https://code.claude.com/docs/en/hooks)
- [Skills Reference](https://code.claude.com/docs/en/skills)
- [Deny Rules Bug Report](https://github.com/anthropics/claude-code/issues/6699)
- [Claude Code Best Practices](https://www.anthropic.com/engineering/claude-code-best-practices)
