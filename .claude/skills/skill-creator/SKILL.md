---
name: skill-creator
description: Create new Claude Code skills for rctbayespower. Use when writing a new skill, setting up skill structure, or learning skill best practices.
---

# Creating Claude Code Skills

## Skill Structure

```
skill-name/
├── SKILL.md              # Required - main instructions
└── references/           # Optional - loaded on demand
    ├── detailed-docs.md
    └── examples.md
```

## SKILL.md Template

```markdown
---
name: skill-name
description: Brief description. Use when [specific triggers]. Includes [key features].
---

# Skill Title

## Essential Pattern 1

[Concise code example or instruction]

## Essential Pattern 2

[Another key pattern]

## References

For details, see:
- [Topic 1](references/topic1.md)
- [Topic 2](references/topic2.md)
```

## Key Principles

1. **Description is the trigger** - Include "Use when..." to help Claude match requests
2. **Keep SKILL.md concise** - Under 100 lines ideal, max 500 lines
3. **Progressive disclosure** - Essential patterns in SKILL.md, details in references/
4. **Only add project-specific context** - Don't duplicate general knowledge

## What Goes Where

| Content | Location |
|---------|----------|
| Essential patterns | SKILL.md |
| Quick reference tables | SKILL.md |
| Detailed explanations | references/ |
| Full examples | references/ |
| External links | references/ |

## References

For detailed guidance, see:
- [Skill best practices](references/skill-best-practices.md)
