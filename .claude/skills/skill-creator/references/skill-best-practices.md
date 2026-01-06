# Skill Best Practices

Detailed guidance for creating effective Claude Code skills.

## Directory Structure Options

```
skill-name/
├── SKILL.md           # Required - main instructions
├── references/        # Documentation loaded on demand
│   ├── detailed.md
│   └── examples.md
├── scripts/           # Executable Python/Bash scripts
│   └── validate.py
└── assets/            # Templates, icons, binary files
    └── template.txt
```

## Context Efficiency

| Phase | Token Usage |
|-------|-------------|
| Metadata scan | ~100 tokens (name + description only) |
| Full activation | <5,000 tokens (SKILL.md content) |
| Reference load | On-demand (only when needed) |

## Progressive Disclosure Design

```
Layer 1: Description     → ~100 tokens, always scanned
Layer 2: SKILL.md body   → <5k tokens, loaded when triggered
Layer 3: references/     → Unbounded, loaded as needed
```

## Writing Good Descriptions

**Bad** (too vague):
```yaml
description: Help with S7 classes
```

**Good** (specific triggers):
```yaml
description: Write S7 class definitions and methods. Use when creating new S7 classes, adding properties, defining validators, or registering methods.
```

## When to Split Content

**Keep in SKILL.md:**
- Essential patterns needed 90% of the time
- Quick reference tables
- The "happy path" workflow

**Move to references/:**
- Detailed explanations
- Edge cases and troubleshooting
- Full official documentation
- Multiple examples

## Linking to References

In SKILL.md, use relative links:

```markdown
## References

For details, see:
- [Detailed patterns](references/detailed.md)
- [Examples](references/examples.md)
```

## Scripts Directory

For deterministic, repeatable operations:

```
scripts/
├── validate.py      # Validation logic
└── generate.sh      # Code generation
```

Claude can execute these without loading them into context.

## Assets Directory

For templates and binary files:

```
assets/
├── template.R       # Code templates
├── config.json      # Configuration templates
└── icon.png         # Binary assets
```

## Testing Skills

1. Verify skill activates for intended triggers
2. Check SKILL.md is concise enough
3. Confirm references load when needed
4. Test with edge cases

## Official Documentation

- Skills documentation: https://code.claude.com/docs/en/skills
- Anthropic skills blog: https://www.anthropic.com/engineering/equipping-agents-for-the-real-world-with-agent-skills
