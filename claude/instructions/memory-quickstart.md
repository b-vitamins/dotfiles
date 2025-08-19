# Memory System Quick Reference

## Memory Hierarchy (Highest → Lowest Priority)
1. **Enterprise**: `/etc/claude-code/CLAUDE.md` (N/A for personal use)
2. **Project**: `./CLAUDE.md` (in project root)
3. **User**: `~/.claude/CLAUDE.md` (your main config)
4. **Local**: `./CLAUDE.local.md` (deprecated, use imports)

## Quick Memory Commands

### Add Memory Instantly
```
# Your memory text here
```
Then select where to save (user/project)

### Edit Memory Files
```
/memory
```
Opens memory files in your editor

### View Loaded Memories
```
/memory
```
Shows all active memory files and imports

### Bootstrap Project Memory
```
/init
```
Creates initial CLAUDE.md for project

## Import Syntax

### Basic Import
```markdown
@~/.claude/instructions/python-development.md
```

### Inline Import
```markdown
Follow guidelines in @docs/coding-standards.md
```

### Multiple Imports
```markdown
@README.md
@docs/architecture.md
@~/.claude/project-specific.md
```

## Memory Loading Behavior

### Recursive Loading
- Starts in current directory
- Loads CLAUDE.md from each parent up to (not including) root
- Example: In `/home/b/projects/myapp/src/`
  - Loads: `./src/CLAUDE.md`, `./CLAUDE.md`, `~/projects/CLAUDE.md`

### Subtree Discovery
- CLAUDE.md in subdirectories loaded when accessing those files
- Example: `./frontend/CLAUDE.md` loaded when editing `./frontend/App.js`

### Import Depth
- Maximum 5 levels of recursive imports
- Prevents circular dependencies

## Best Practices

### Do's
- ✅ Use bullet points for individual memories
- ✅ Group related items under headings
- ✅ Be specific: "Use 4 spaces" not "indent properly"
- ✅ Include common commands
- ✅ Document project-specific patterns

### Don'ts
- ❌ Don't duplicate information across files
- ❌ Don't use vague instructions
- ❌ Don't include sensitive data
- ❌ Don't exceed 5 import levels

## Project Memory Template
```markdown
# Project Name

## Architecture
- MVC pattern with service layer
- PostgreSQL for persistence
- Redis for caching

## Development Commands
- `make test` - Run test suite
- `make lint` - Run linters
- `make deploy` - Deploy to staging

## Conventions
- API endpoints: `/api/v1/resource`
- Database migrations in `db/migrations/`
- Feature flags via environment variables

## Team Preferences
@~/.claude/team-standards.md
```

## User Memory Organization
```
~/.claude/
├── CLAUDE.md                    # Main user config
├── instructions/                # Modular instructions
│   ├── python-development.md
│   ├── guix-workflow.md
│   └── ...
├── project-templates/           # Reusable templates
│   ├── python-ml-project.md
│   └── latex-paper.md
└── agents/                      # Custom agents
    └── *.md
```

## Memory Shortcuts

### For Python Projects
```markdown
@~/.claude/project-templates/python-ml-project.md
@~/.claude/instructions/python-development.md
```

### For LaTeX Projects
```markdown
@~/.claude/project-templates/latex-paper.md
@~/.claude/instructions/latex-academic.md
```

### For Guix Projects
```markdown
@~/.claude/instructions/guix-workflow.md
```