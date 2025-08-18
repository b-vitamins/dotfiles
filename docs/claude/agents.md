# Claude Code Agents Reference

This document provides a comprehensive reference for all custom Claude Code agents in the dotfiles repository. These agents are specialized AI assistants designed to handle specific development tasks efficiently.

## Table of Contents

1. [Overview](#overview)
2. [How to Use Agents](#how-to-use-agents)
3. [Agents by Category](#agents-by-category)
   - [Rust Development](#rust-development)
   - [Python Development (Guix)](#python-development-guix)
   - [Emacs Lisp & Guile](#emacs-lisp--guile)
   - [LaTeX Documents](#latex-documents)
   - [Bibliography Management](#bibliography-management)
   - [Code Quality & Refactoring](#code-quality--refactoring)
   - [Dependency Management](#dependency-management)
   - [Testing & Analysis](#testing--analysis)
   - [Frontend Development](#frontend-development)
4. [Agent Selection Guide](#agent-selection-guide)
5. [Workflow Examples](#workflow-examples)

## Overview

Each agent is designed to:
- Handle ONE specific task well
- Complete within a reasonable context window (~100k tokens)
- Work with your Guix-based development environment
- Never use Poetry, pip, or npm global commands
- Follow the principle of doing one thing excellently

Agents are stored in `claude/agents/` and can be invoked through Claude Code's Task tool.

## How to Use Agents

### Invoking an Agent

```
Use the [agent-name] agent to [specific task]
```

Example:
```
Use the python-ruff-fixer agent to fix linting issues in my Python code
```

### Agent Composition

Agents can be chained for complex workflows:
```
First use the test-coverage-analyzer agent to find untested functions,
then use the test-generator-single agent to write tests for the high-priority ones
```

## Agents by Category

### Rust Development

#### **cargo-dependency-updater**
- **Purpose**: Updates Rust crates systematically, one at a time
- **When to use**: Regular dependency maintenance, security updates
- **Key features**:
  - Handles one crate at a time
  - Runs `cargo check` and tests after each update
  - Respects MSRV (Minimum Supported Rust Version)
  - Creates atomic commits

#### **rust-clippy-fixer**
- **Purpose**: Fixes Clippy warnings to improve code quality
- **When to use**: Before commits, code review preparation, learning Rust idioms
- **Key features**:
  - Fixes one warning category at a time
  - Explains why each change improves code
  - Handles pedantic lints
  - Preserves functionality while improving style

### Python Development (Guix)

#### **guix-manifest-updater**
- **Purpose**: Maps Python dependencies from pyproject.toml to Guix packages
- **When to use**: Setting up new Python project, adding dependencies
- **Key features**:
  - Maps PyPI names to Guix package names
  - Handles special cases (PIL→pillow, sklearn→scikit-learn)
  - Tests imports after adding
  - Never uses Poetry commands

#### **python-ruff-fixer**
- **Purpose**: Fixes Python linting issues using Ruff via Guix shell
- **When to use**: Before commits, maintaining code quality
- **Key features**:
  - Uses `guix shell -m manifest.scm -- ruff`
  - Fixes one error category at a time
  - Auto-fixes safe issues first
  - Handles modern Python idioms (3.9+)

#### **python-pyright-resolver**
- **Purpose**: Resolves type checking errors systematically
- **When to use**: Improving type safety, fixing type errors
- **Key features**:
  - Works through Pyright errors one at a time
  - Adds proper type annotations
  - Handles Optional, Union, generics
  - Modern syntax (Python 3.10+)

#### **pytest-runner-guix**
- **Purpose**: Runs pytest and handles test failures
- **When to use**: Running tests, debugging failures
- **Key features**:
  - Uses Guix shell exclusively
  - Debugs one test failure at a time
  - Handles fixtures and mocking
  - Shows useful pytest options

#### **python-import-resolver**
- **Purpose**: Resolves ImportError and ModuleNotFoundError
- **When to use**: When packages missing from manifest.scm
- **Key features**:
  - Maps import names to Guix packages
  - Searches for correct package names
  - Updates manifest.scm
  - Verifies imports work

#### **manifest-dependency-checker**
- **Purpose**: Verifies all imports have packages in manifest.scm
- **When to use**: Before deployment, dependency audits
- **Key features**:
  - Reports only, no modifications
  - Shows missing packages with Guix names
  - Identifies unused packages
  - Distinguishes stdlib from third-party

#### **pyclean-runner**
- **Purpose**: Cleans Python cache files using pyclean
- **When to use**: Before commits, after refactoring
- **Key features**:
  - Removes `__pycache__`, `.pyc` files
  - Cleans test and type checking caches
  - Uses Guix shell for pyclean
  - Quick one-liner commands

### Emacs Lisp & Guile

#### **elisp-checkdoc-fixer**
- **Purpose**: Fixes documentation warnings in Emacs Lisp files
- **When to use**: Before committing .el files, package development
- **Key features**:
  - Ensures proper file headers
  - Fixes docstring formatting
  - Handles argument documentation
  - Follows Emacs conventions

#### **emacs-batch-validator**
- **Purpose**: Comprehensive validation of Emacs Lisp files
- **When to use**: Before commits, package submission
- **Key features**:
  - Runs checkdoc, elint, and byte-compilation
  - Achieves zero warnings/errors
  - Tests file loading
  - Updates init.el if needed

#### **guix-package-linter**
- **Purpose**: Lints and fixes Guix package definitions
- **When to use**: Creating/modifying Guix packages
- **Key features**:
  - Checks synopsis/description format
  - Verifies licenses
  - Ensures proper style
  - Handles different build systems

### LaTeX Documents

#### **latex-compiler-fixer**
- **Purpose**: Fixes LaTeX compilation errors one at a time
- **When to use**: When documents fail to compile
- **Key features**:
  - Reads log files for first error
  - Handles missing packages
  - Fixes common issues
  - Works with pdflatex/xelatex/lualatex

#### **latex-math-validator**
- **Purpose**: Validates math mode usage and fixes errors
- **When to use**: Academic papers with heavy math
- **Key features**:
  - Checks delimiter matching
  - Fixes text in math mode
  - Validates environments
  - Ensures proper spacing

#### **latex-table-formatter**
- **Purpose**: Formats and fixes LaTeX tables
- **When to use**: Improving table appearance, fixing overflows
- **Key features**:
  - Uses booktabs for professional look
  - Handles wide tables
  - Fixes alignment issues
  - Modern best practices

#### **latex-figure-organizer**
- **Purpose**: Organizes figure placement and references
- **When to use**: Managing documents with many figures
- **Key features**:
  - Ensures all figures referenced
  - Fixes placement issues
  - Standardizes labels
  - Handles subfigures

#### **latex-package-resolver**
- **Purpose**: Resolves package conflicts and load order
- **When to use**: Package clash errors, compatibility issues
- **Key features**:
  - Fixes load order (hyperref last)
  - Resolves option clashes
  - Handles obsolete packages
  - Platform-specific solutions

#### **latex-beamer-styler**
- **Purpose**: Improves Beamer presentation styling
- **When to use**: Creating professional presentations
- **Key features**:
  - Applies consistent themes
  - Fixes overcrowded frames
  - Handles progressive reveals
  - Modern styling practices

#### **paper-to-beamer**
- **Purpose**: Converts academic papers to Beamer presentations
- **When to use**: Creating teaching materials from papers
- **Key features**:
  - Preserves all mathematics
  - Creates comprehensive slides
  - Handles appendices
  - Academic presentation style

### Bibliography Management

#### **bibtex-citation-checker**
- **Purpose**: Verifies citation consistency
- **When to use**: Before paper submission
- **Key features**:
  - Finds missing BibTeX entries
  - Identifies unused entries
  - Reports only, no modifications
  - Handles multiple .bib files

#### **bibtex-entry-enricher**
- **Purpose**: Enriches single BibTeX entries
- **When to use**: Processing bibliography one entry at a time
- **Key features**:
  - Adds OpenAlex IDs
  - Finds official PDF links
  - Verifies metadata
  - Fixes formatting issues

#### **bibtex-file-enricher**
- **Purpose**: Enriches entire BibTeX files
- **When to use**: Comprehensive bibliography improvement
- **Key features**:
  - Multi-stage workflow
  - Finds missing PDFs
  - Verifies entry authenticity
  - Standardizes bibkeys

### Code Quality & Refactoring

#### **function-renamer**
- **Purpose**: Renames single function across codebase
- **When to use**: Precise refactoring tasks
- **Key features**:
  - Handles all references
  - Includes string references
  - Language-aware patterns
  - Verifies completeness

#### **pattern-replacer**
- **Purpose**: Replaces code patterns systematically
- **When to use**: Modernizing code, API migrations
- **Key features**:
  - One pattern type per run
  - Callback→async/await
  - require→import
  - Preserves behavior

#### **import-cleaner**
- **Purpose**: Removes unused imports
- **When to use**: Code cleanup, before commits
- **Key features**:
  - Language-aware detection
  - Preserves side-effect imports
  - Handles dynamic imports
  - Safe removal process

#### **dead-function-finder**
- **Purpose**: Identifies potentially unused functions
- **When to use**: Code cleanup planning
- **Key features**:
  - Reports only, no removal
  - Confidence levels
  - Checks dynamic usage
  - Framework awareness

#### **security-secret-scanner**
- **Purpose**: Finds exposed secrets and credentials
- **When to use**: Before commits, security audits
- **Key features**:
  - Never modifies files
  - Severity classification
  - Common pattern detection
  - False positive filtering

#### **git-commit-formatter**
- **Purpose**: Creates well-formatted commit messages
- **When to use**: Before every commit
- **Key features**:
  - Conventional commit format
  - Analyzes staged changes
  - Language-specific patterns
  - Breaking change notation

### Dependency Management

#### **dependency-patch-updater**
- **Purpose**: Updates only patch versions (bug fixes)
- **When to use**: Safe, regular updates
- **Key features**:
  - X.Y.Z → X.Y.Z+1 only
  - One package at a time
  - Immediate testing
  - Zero breaking changes

#### **dependency-breaking-fixer**
- **Purpose**: Fixes breaking changes from package updates
- **When to use**: After major/minor version updates
- **Key features**:
  - Follows migration guides
  - One package at a time
  - Systematic fixing
  - Preserves functionality

### Testing & Analysis

#### **test-coverage-analyzer**
- **Purpose**: Analyzes test coverage gaps
- **When to use**: Planning test writing
- **Key features**:
  - One module at a time
  - Priority classification
  - Reports only
  - Actionable insights

#### **type-any-eliminator**
- **Purpose**: Removes 'any' types from TypeScript
- **When to use**: Improving type safety
- **Key features**:
  - One 'any' at a time
  - Finds proper types
  - Progressive typing
  - Modern TypeScript

### Frontend Development

#### **svelte-store-migrator**
- **Purpose**: Modernizes Svelte stores
- **When to use**: Updating Svelte patterns
- **Key features**:
  - Migrates to readable/writable/derived
  - Preserves reactivity
  - Updates to $ syntax
  - TypeScript support

#### **migration-planner**
- **Purpose**: Plans technology migrations
- **When to use**: Major technology changes
- **Key features**:
  - Creates staged plans
  - Identifies affected files
  - No implementation
  - Risk assessment

## Agent Selection Guide

### Quick Decision Tree

1. **What language are you working with?**
   - Rust → `cargo-dependency-updater`, `rust-clippy-fixer`
   - Python → `python-*` agents (all use Guix)
   - Emacs Lisp → `elisp-checkdoc-fixer`, `emacs-batch-validator`
   - LaTeX → `latex-*` agents

2. **What task do you need?**
   - Fix errors → `*-fixer`, `*-resolver` agents
   - Analyze code → `*-analyzer`, `*-finder`, `*-checker` agents
   - Update code → `*-updater`, `*-migrator` agents
   - Format/style → `*-formatter`, `*-styler` agents

3. **Do you need modifications or just analysis?**
   - Just analysis → `*-analyzer`, `*-checker`, `*-finder`, `*-scanner`
   - Modifications → All other agents

### Common Workflows

#### Pre-commit Workflow
1. `security-secret-scanner` - Check for exposed secrets
2. `import-cleaner` - Remove unused imports
3. Language-specific linter (`python-ruff-fixer`, `rust-clippy-fixer`)
4. `git-commit-formatter` - Create proper commit message

#### Dependency Update Workflow
1. `dependency-patch-updater` - Safe updates first
2. For breaking changes: `dependency-breaking-fixer`
3. Run tests with language-specific runner
4. Update documentation if needed

#### LaTeX Document Workflow
1. `latex-compiler-fixer` - Fix compilation errors
2. `latex-math-validator` - Check math mode
3. `latex-table-formatter` - Improve tables
4. `latex-figure-organizer` - Organize figures
5. `bibtex-citation-checker` - Verify citations

#### Python Project Setup (Guix)
1. `guix-manifest-updater` - Create manifest from pyproject.toml
2. `python-import-resolver` - Fix any import errors
3. `manifest-dependency-checker` - Verify completeness
4. `python-ruff-fixer` - Initial code cleanup

## Workflow Examples

### Example 1: Refactoring a Function Name
```
Use the function-renamer agent to rename calculateTotal to computeSum across the codebase
```

### Example 2: Fixing Python Type Errors
```
Use the python-pyright-resolver agent to fix type errors in src/models.py
```

### Example 3: Comprehensive LaTeX Document Check
```
First use the latex-compiler-fixer agent to ensure it compiles,
then use the latex-math-validator agent to check all math,
then use the bibtex-citation-checker agent to verify citations
```

### Example 4: Dependency Security Update
```
Use the dependency-patch-updater agent to update all packages with security patches
```

## Best Practices

1. **Use one agent at a time** - Agents are designed for focused tasks
2. **Chain agents for complex workflows** - Combine specialized agents
3. **Trust agent expertise** - Each agent is optimized for its specific task
4. **Review agent output** - Especially for analysis agents before taking action
5. **Keep agents updated** - Check for new agents in `claude/agents/`

## Notes

- All Python agents use Guix shell exclusively (no Poetry/pip)
- LaTeX agents work with standard TeX distributions
- Agents respect your development environment preferences
- Analysis agents never modify files (safer for exploration)
- Each agent completes within reasonable context limits

For agent implementation details, see the individual agent files in `claude/agents/`.