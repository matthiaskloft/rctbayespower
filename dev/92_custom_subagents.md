# Custom Subagents

**Last Updated:** 2026-01-11

This document describes the custom Claude Code subagents configured for rctbayespower development.

## Overview

Subagents are specialized AI agents that Claude Code automatically delegates to when specific tasks are detected. They provide domain-specific knowledge and focused toolsets for common development workflows.

## Configuration

Subagents are defined as markdown files with YAML frontmatter in `.claude/agents/`. They are loaded at session start.

### File Format

```yaml
---
name: agent-name
description: When Claude should auto-delegate to this agent
tools: Read, Glob, Grep  # comma-separated list
model: sonnet  # haiku, sonnet, or opus
---
System prompt in markdown...
```

### Key Properties

| Property | Description |
|----------|-------------|
| `name` | Unique identifier for the agent |
| `description` | Triggers auto-delegation when user intent matches |
| `tools` | Comma-separated list of allowed tools |
| `model` | LLM model variant (haiku=fast, sonnet=balanced, opus=complex) |

## Available Subagents

### test-writer

| Property | Value |
|----------|-------|
| **File** | `.claude/agents/test-writer.md` |
| **Model** | sonnet |
| **Tools** | Read, Glob, Grep, Edit, Write, Bash |

**Purpose**: Generate comprehensive testthat tests for S7 classes, backends, and workflows.

**Triggers**: "write tests", "add test coverage", "test X function/class"

**Capabilities**:
- S7 class constructor and validator tests
- Backend-specific tests with skip conditions
- Integration workflow tests
- Mock mode testing for BayesFlow
- Package-specific naming conventions in assertions

### roxygen-sync

| Property | Value |
|----------|-------|
| **File** | `.claude/agents/roxygen-sync.md` |
| **Model** | sonnet |
| **Tools** | Read, Glob, Grep, Edit, Bash |

**Purpose**: Detect and fix roxygen documentation mismatches.

**Triggers**: "sync docs", "fix roxygen", "@param mismatch", "documentation out of sync"

**References**: `/roxygen` skill for templates when writing new documentation.

**Capabilities**:
- Detect missing/orphaned @param entries
- Fix default value mismatches
- Convert old link syntax to modern `[fn()]`
- Run devtools::document() and devtools::check(args = "--no-tests") after fixes

### backend-verifier

| Property | Value |
|----------|-------|
| **File** | `.claude/agents/backend-verifier.md` |
| **Model** | sonnet |
| **Tools** | Read, Glob, Grep, Bash |

**Purpose**: Verify brms and BayesFlow backend consistency.

**Triggers**: "verify backends", "check brms vs bf", "backend consistency"

**Capabilities**:
- Compare result structure between backends
- Verify decision logic consistency
- Test mock mode behavior
- Check error handling patterns

**Note**: Read-only agent (no Write/Edit tools).

### naming-auditor

| Property | Value |
|----------|-------|
| **File** | `.claude/agents/naming-auditor.md` |
| **Model** | haiku |
| **Tools** | Read, Glob, Grep |

**Purpose**: Audit and enforce package naming conventions.

**Triggers**: "audit naming", "check conventions", "verify naming standards"

**Capabilities**:
- Check `rctbp_*` class naming
- Verify `pwr_*/pr_*/dec_*/se_*` column naming
- Enforce snake_case for functions/parameters
- Flag `_eff/_fut` to `_scs/_ftl` migration needs
- Report violations with file:line references

**Note**: Read-only agent, uses haiku for fast pattern matching.

## Usage Examples

```
# Invoke test-writer
"Write tests for the rctbp_design class"
"Add test coverage for build_conditions"

# Invoke roxygen-sync
"Check if roxygen docs are in sync with code"
"Fix the @param entries in class_design.R"

# Invoke backend-verifier
"Verify that the new feature works with both backends"
"Check backend consistency for estimate functions"

# Invoke naming-auditor
"Audit naming conventions in R/"
"Check if all column names follow conventions"
```

## Tool Permissions

| Agent | Read | Write | Edit | Bash | Purpose |
|-------|------|-------|------|------|---------|
| test-writer | Yes | Yes | Yes | Yes | Create and run tests |
| roxygen-sync | Yes | No | Yes | Yes | Fix docs, run devtools |
| backend-verifier | Yes | No | No | Yes | Run verification commands |
| naming-auditor | Yes | No | No | No | Audit only |

## Adding New Subagents

1. Create a new `.md` file in `.claude/agents/`
2. Add YAML frontmatter with required properties
3. Write a focused system prompt with package-specific context
4. Restart Claude Code session to load the new agent
5. Document in this file

## References

- [Claude Code Sub-agents Docs](https://code.claude.com/docs/en/sub-agents)
- [awesome-claude-code-subagents](https://github.com/VoltAgent/awesome-claude-code-subagents)
