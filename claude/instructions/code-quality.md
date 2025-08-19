# Code Quality & Maintenance Instructions

## Pre-Commit Workflow
ALWAYS run before committing:
1. `security-secret-scanner` → Check for exposed secrets
2. Language-specific linters/formatters
3. `git-commit-formatter` → Generate conventional commits

## Code Cleanup Sequence
1. `import-cleaner` → Remove unused imports
2. `dead-function-finder` → Identify unused functions
3. `pattern-replacer` → Systematic refactoring
4. `function-renamer` → Consistent naming

## Refactoring Agents
- `migration-planner` → Plan technology transitions
- `dependency-breaking-fixer` → Fix after major updates
- `patch-version-updater` → Semantic version bumps

## Language-Specific Quality

### Python
- `python-ruff-fixer` → Format and lint
- `python-pyright-resolver` → Type checking
- `pyclean-runner` → Remove __pycache__

### Rust
- `rust-clippy-fixer` → Idiomatic Rust
- `cargo-dependency-updater` → Keep deps current

### TypeScript/JavaScript
- `typescript-any-eliminator` → Remove any types
- `svelte-store-migrator` → Svelte 5 migration

### Emacs Lisp
- `elisp-checkdoc-fixer` → Documentation standards
- `emacs-batch-validator` → Validate .el files

## Testing Strategy
1. Write tests FIRST when fixing bugs
2. Use coverage analysis to find gaps
3. Property testing for algorithms
4. Mutation testing to validate test quality

## Security Practices
- Never commit: .env, secrets/, credentials
- Always scan before push
- Use environment variables for sensitive data