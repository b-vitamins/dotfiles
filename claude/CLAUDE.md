# User Configuration - Ayan Das

## Core Preferences
- **Communication**: Technical, terse, imperative voice
- **Forbidden**: we/you/our, marketing language, unicode
- **Documentation**: description→install→usage→license (Markdown only)

## Specialized Instructions
@~/.claude/instructions/memory-quickstart.md
@~/.claude/instructions/settings-reference.md
@~/.claude/instructions/guix-workflow.md
@~/.claude/instructions/python-development.md
@~/.claude/instructions/latex-academic.md
@~/.claude/instructions/code-quality.md
@~/.claude/instructions/agent-chains.md
@~/.claude/instructions/hooks-automation.md
@~/.claude/instructions/output-styles-guide.md
@~/.claude/instructions/quick-references.md

## Project Templates
@~/.claude/project-templates/python-ml-project.md
@~/.claude/project-templates/latex-paper.md

## System Environment
- **Hardware**: Ryzen 5600G, 64GB RAM, RTX 3060, 4TB NVMe
- **OS**: GNU Guix System, GNOME/Wayland
- **Languages**: Python 3.11, PyTorch 2.7.0, Rust 1.82.0

## Critical Rules
1. **Package Management**: ONLY Guix (see guix-workflow.md)
2. **Python**: Always python3, never python
3. **Testing**: pytest only, 100% coverage on public APIs
4. **Commits**: Conventional format via git-commit-formatter agent

## Available Agents (41 total)

### Python Testing Suite (9 agents)
- `python-unit-test-writer` - Comprehensive pytest unit tests
- `python-integration-test-writer` - Database/API interaction tests
- `python-e2e-test-writer` - Full workflow validation
- `python-property-test-writer` - Hypothesis-based property tests
- `python-security-test-writer` - Security vulnerability tests
- `python-performance-tester` - Benchmarks and load tests
- `python-regression-tester` - Bug fix validation
- `python-mutation-tester` - Test quality analysis
- `pytest-runner-guix` - Run tests in Guix environment

### Python/Guix Tools (6 agents)
- `python-import-resolver` - Fix ModuleNotFoundError
- `python-ruff-fixer` - Format and lint
- `python-pyright-resolver` - Type checking
- `guix-manifest-updater` - Update manifest.scm
- `manifest-dependency-checker` - Verify dependencies
- `guix-package-linter` - Package definition validation

### LaTeX Suite (8 agents)
- `latex-compiler-fixer` - Fix compilation errors
- `latex-package-resolver` - Package conflicts
- `latex-figure-organizer` - Figure management
- `latex-table-formatter` - Table formatting
- `latex-math-validator` - Math mode validation
- `latex-beamer-styler` - Presentation formatting
- `bibtex-citation-checker` - Citation verification
- `bibtex-entry-enricher` - Add DOIs/metadata

### Code Quality (9 agents)
- `import-cleaner` - Remove unused imports
- `dead-function-finder` - Find unused functions
- `function-renamer` - Rename across codebase
- `pattern-replacer` - Systematic refactoring
- `test-coverage-analyzer` - Coverage gaps
- `pyclean-runner` - Clean Python artifacts
- `elisp-checkdoc-fixer` - Emacs Lisp docs
- `emacs-batch-validator` - Validate .el files
- `security-secret-scanner` - Find exposed secrets

### Other Tools (9 agents)
- `cargo-dependency-updater` - Rust deps
- `rust-clippy-fixer` - Idiomatic Rust
- `dependency-breaking-fixer` - Fix breaking changes
- `patch-version-updater` - Version bumps
- `git-commit-formatter` - Conventional commits
- `migration-planner` - Tech migration plans
- `svelte-store-migrator` - Svelte 5 migration
- `typescript-any-eliminator` - Remove any types
- `paper-to-beamer` - Convert papers to slides

## Quick Commands
```bash
# Guix environment
guix shell -m manifest.scm -- <command>

# Format/lint/test
ruff format . && ruff check . && pytest -xvs

# Git with formatting
git add . && git-commit-formatter && git push
```

## Project Patterns
- manifest.scm at root → Guix project
- pyproject.toml → Poetry config (read-only, use Guix)
- Cargo.toml → Rust project
- *.tex → LaTeX document

## Daily Workflow
- ~/tmp cleared daily
- ~/projects/ flat structure (no Desktop)
- Delete experiments after completion
- 300 DPI for plots → assets/

## Current Focus
- Graduate student at Indian Institute of Science
- ML research projects
- Theory-practice bridge

## Emacs Integration
See dotfiles: `~/projects/dotfiles/emacs/`
Feature locations in emacs-xyz.scm (lines noted in full CLAUDE.md backup)