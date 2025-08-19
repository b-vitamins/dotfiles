# Output Styles Guide

## Available Custom Output Styles

### 1. Ultra Terse (`/output-style ultra-terse`)
- **Purpose**: Absolute minimum output
- **Use When**: Quick fixes, simple questions, no explanations needed
- **Behavior**: Code/commands only, single-word answers, no narration
- **Example**: Ask "what's 2+2" → Get "4"

### 2. Research Mode (`/output-style research-mode`)
- **Purpose**: ML research and academic paper writing
- **Use When**: Working on experiments, papers, presentations
- **Behavior**: 
  - Tracks experiments systematically
  - Ensures LaTeX quality
  - Manages citations properly
  - Creates reproducible research
- **Special**: Auto-checks citations, suggests Beamer conversions

### 3. Guix Expert (`/output-style guix-expert`)
- **Purpose**: Deep Guix system administration
- **Use When**: Package management, system configuration, manifest optimization
- **Behavior**:
  - Maps package names correctly
  - Creates package definitions
  - Optimizes manifests
  - Troubleshoots build failures
- **Special**: Auto-validates Guix syntax, suggests gc when needed

### 4. Test Driven (`/output-style test-driven`)
- **Purpose**: Strict TDD workflow enforcement
- **Use When**: Building new features with test-first approach
- **Behavior**:
  - Forces test writing before implementation
  - Ensures comprehensive coverage
  - Uses property testing for algorithms
  - Validates test quality with mutation testing
- **Workflow**: RED → GREEN → REFACTOR

## Quick Switching

```bash
# Switch to a style for current session
/output-style ultra-terse
/output-style research-mode
/output-style guix-expert
/output-style test-driven

# Return to default
/output-style default

# View current style
/output-style
```

## Style Selection Guide

| Situation | Recommended Style |
|-----------|------------------|
| Quick debugging | Ultra Terse |
| Writing paper | Research Mode |
| System config | Guix Expert |
| New feature development | Test Driven |
| General coding | Default |

## Creating New Styles

```bash
# Interactive creation
/output-style:new I want a style that...

# Manual creation
vim ~/.claude/output-styles/my-style.md
```

## Style File Structure

```markdown
---
name: Style Name
description: Brief description shown in menu
---

# Instructions

Core behavior definition...

## Specific Rules

Detailed behavioral rules...
```

## Integration with Workflow

- Styles affect main Claude loop (not subagents)
- Saved in `.claude/settings.local.json` per project
- Can be combined with CLAUDE.md instructions
- Work alongside hooks and agents

## Notes

- Output styles replace default software engineering prompt
- CLAUDE.md adds to (doesn't replace) instructions
- Agents are separate and unaffected by styles
- Styles are project-local when set via `/output-style`