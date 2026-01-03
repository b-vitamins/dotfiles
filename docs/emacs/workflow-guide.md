# Emacs Configuration Workflow Guide

This guide teaches you modern Emacs workflows through direct comparisons with traditional approaches, leveraging the power of the optimized configuration.

## Table of Contents

1. [File and Buffer Management](#file-and-buffer-management)
2. [Search and Navigation](#search-and-navigation)
3. [Code Editing and Completion](#code-editing-and-completion)
4. [Project Management](#project-management)
5. [Git Workflows](#git-workflows)
6. [Org-mode and Note-taking](#org-mode-and-note-taking)
7. [Window and Frame Management](#window-and-frame-management)
8. [Common Emacs Anti-patterns](#common-emacs-anti-patterns)

## File and Buffer Management

### Opening Files

Instead of:
```
C-x C-f ~/projects/webapp/src/components/Header.jsx
```
Do:
```
C-x C-f  # Then start typing "Header" - vertico will find it
```

Instead of:
```
C-x C-f and typing full path repeatedly
```
Do:
```
C-c r  # Recent files with instant preview
```

Instead of:
```
find-file with tab completion through deep directories
```
Do:
```
project-find-file (C-x p f)  # Smart project-aware file finding
```

Instead of:
```
Opening multiple related files one by one
```
Do:
```
consult-find  # Search all files with live preview
```

### Buffer Switching

Instead of:
```
C-x b buffer-name RET
```
Do:
```
C-x b  # Start typing - orderless matching finds any buffer
```

Instead of:
```
C-x C-b to see buffer list, then navigate
```
Do:
```
consult-buffer  # Live-filtered buffer list with previews
```

Instead of:
```
next-buffer (C-x →) repeatedly
```
Do:
```
C-x b  # Type pattern to jump directly to target buffer
```

Instead of:
```
Manually typing exact buffer names
```
Do:
```
C-x b proj RET  # Finds "my-project.py" buffer
```

#### Consult Buffer: Sources & Narrowing

`C-x b` runs `consult-buffer`, which merges multiple *sources* (e.g. buffers, files, bookmarks) into one picker. The headers you see (like `[Buffer]`, `[Recent File]`) are those sources.

- **Default behavior:** buffers are listed in MRU order, so `C-x b RET` usually jumps back to your previous buffer.
- **Cycle sources:** `TAB` / `S-TAB` cycles the active source narrowing.
- **Jump to a source:** type the source key, then `SPC` (e.g. `b SPC` buffers, `f SPC` recent files, `m SPC` bookmarks, `p SPC` project, `. SPC` files in current dir).
- **Widen back:** with empty input, press `DEL` to return to “all sources”.
- **Discover keys:** press `?` to see a live list of available narrowing keys for the current picker.
- **Customize order:** source cycling order is controlled by `bv-consult-narrow-cycle-order` (and the actual sources by `consult-buffer-sources`).

### File Operations

Instead of:
```
C-x C-w  # Save as, type full path
```
Do:
```
C-x C-w  # Vertico completion for directories and names
```

Instead of:
```
dired-mode, navigate, then open file
```
Do:
```
dired-jump (C-x C-j)  # Jump directly to current file in dired
```

Instead of:
```
find-file, realize you want a similar named file
```
Do:
```
consult-find  # Browse all related files with preview
```

Instead of:
```
Killing buffers one by one with C-x k
```
Do:
```
C-x k  # Kills current buffer without confirmation
```

## Search and Navigation

### Text Search

Instead of:
```
C-s search-term  # isearch forward
```
Do:
```
C-s  # Enhanced with orderless - search "func login" finds "function user_login"
```

Instead of:
```
M-x grep RET grep -r "pattern" .
```
Do:
```
consult-ripgrep  # Live grep with instant preview
```

Instead of:
```
M-x occur RET pattern  # Search in current buffer
```
Do:
```
consult-line  # Enhanced occur with live preview
```

Instead of:
```
grep-find for files containing pattern
```
Do:
```
consult-grep  # Multi-file grep with jump-to-line
```

### Code Navigation

Instead of:
```
C-M-s  # Regex search for function definitions
```
Do:
```
consult-imenu  # Jump to any function/class/heading
```

Instead of:
```
M-g g line-number  # Go to line
```
Do:
```
consult-goto-line  # Go to line with preview
```

Instead of:
```
M-x tags-search after building tags
```
Do:
```
xref-find-references  # LSP-powered semantic search
```

Instead of:
```
Manually searching for symbol definitions
```
Do:
```
xref-find-definitions (M-.)  # Jump to definition instantly
```

#### LSP “Peek” Flow (Eglot + Consult UI)

In eglot-managed buffers, use the dedicated peek keymap:

- `C-c l d` / `C-c l D` → definition (same window / other window)
- `C-c l R` → references
- `C-c l e` → diagnostics (fast picker UI)
- `C-c l h` → full docs at point

### Buffer-Specific Navigation

Instead of:
```
M-x outline-mode, then navigate headings
```
Do:
```
consult-outline  # Jump to any heading/section
```

Instead of:
```
Scrolling through large files to find sections
```
Do:
```
consult-line-multi  # Search across multiple buffers
```

Instead of:
```
bookmark-set, bookmark-jump for frequently used locations
```
Do:
```
consult-bookmark  # Enhanced bookmark navigation
```

## Code Editing and Completion

### Text Completion

Instead of:
```
M-x completion-at-point  # Basic completion
```
Do:
```
TAB  # Corfu popup with rich context and documentation
```

Instead of:
```
hippie-expand to complete from buffers
```
Do:
```
C-M-/  # Cape completion with multiple sources
```

Instead of:
```
dabbrev-expand for dynamic abbreviation
```
Do:
```
TAB  # Corfu integrates dabbrev and much more
```

Instead of:
```
Manual typing of common code patterns
```
Do:
```
TAB after typing snippet trigger  # Tempel templates
```

### Code Editing

Instead of:
```
M-x comment-region  # Comment/uncomment blocks
```
Do:
```
Smart commenting with language-aware patterns  # Built-in enhancement
```

Instead of:
```
Manual parentheses balancing
```
Do:
```
smartparens-mode  # Automatic balanced pairs
```

Instead of:
```
C-M-f, C-M-b for sexp navigation
```
Do:
```
Enhanced sexp navigation with smartparens
```

Instead of:
```
Manual indentation fixing
```
Do:
```
Automatic indentation based on language mode
```

### Language-Specific Features

Instead of:
```
python-mode basic features
```
Do:
```
python-mode + eglot + rich LSP features
```

Instead of:
```
Manual error checking
```
Do:
```
flymake-mode  # Real-time syntax checking
```

Instead of:
```
Basic syntax highlighting
```
Do:
```
Enhanced highlighting with language-specific configuration
```

Instead of:
```
Manual debugging setup
```
Do:
```
dape-mode  # Modern debugging with DAP protocol
```

#### Diagnostics Fix Loop (Flymake)

Think of Flymake as a tight loop: **jump → read → fix → repeat**.

- `M-n` / `M-p` jumps to next/prev diagnostic and shows it at point.
- `C-c ! q` opens a quick diagnostics picker (Consult UI when available).
- `C-c ! b` / `C-c ! P` shows buffer/project diagnostics lists.

#### Formatting (Unified)

Formatting uses one dispatcher so your muscle memory stays the same across languages:

- `C-c C-f` formats the current buffer in supported modes (C/C++/CUDA, Python, Rust).
- `M-x bv-format-on-save-mode` toggles buffer-local format-on-save.

## Project Management

### Project Detection

Instead of:
```
Manually setting project root directories
```
Do:
```
Automatic project detection via .git, manifest.scm, etc.
```

Instead of:
```
find-file within project directories
```
Do:
```
C-x p f  # Project-scoped file finding
```

Instead of:
```
grep within project manually
```
Do:
```
C-x p g  # Project-scoped grep search
```

Instead of:
```
Switching between projects manually
```
Do:
```
project-switch-project  # Fast project switching
```

### Project Workflows

Instead of:
```
eshell, cd to project root, run commands
```
Do:
```
project-eshell  # Shell in project root
```

Instead of:
```
compile command, manually setting directory
```
Do:
```
project-compile  # Compile within project context
```

Instead of:
```
Managing multiple project buffers manually
```
Do:
```
project-kill-buffers  # Clean up project buffers
```

Instead of:
```
Switching contexts between different projects
```
Do:
```
perspective-mode  # Isolated workspaces per project
```

### Project Organization

Instead of:
```
Opening related files one by one
```
Do:
```
consult-project-buffer  # All project buffers at once
```

Instead of:
```
Remembering project structure manually
```
Do:
```
Project-aware completion shows structure
```

Instead of:
```
Manual project bookmarking
```
Do:
```
Automatic project history and switching
```

## Git Workflows

### Basic Git Operations

Instead of:
```
M-x vc-next-action  # Basic version control
```
Do:
```
magit-status (C-x g)  # Full-featured Git interface
```

Instead of:
```
git add -p in terminal
```
Do:
```
magit-status, then 's' to stage hunks interactively
```

Instead of:
```
git log --oneline in terminal
```
Do:
```
magit-log-current  # Rich interactive log with diffs
```

Instead of:
```
Manual conflict resolution
```
Do:
```
magit merge conflicts with smerge-mode integration
```

### Advanced Git Features

Instead of:
```
git blame in terminal
```
Do:
```
magit-blame  # Inline blame with commit navigation
```

Instead of:
```
git stash manually
```
Do:
```
magit stash operations with previews
```

Instead of:
```
Creating and applying patches manually
```
Do:
```
magit patch creation and application
```

Instead of:
```
Interactive rebase in terminal
```
Do:
```
magit-rebase-interactive  # Visual rebase interface
```

### Git Integration

Instead of:
```
Switching to terminal for git operations
```
Do:
```
Stay in Emacs with magit for everything
```

Instead of:
```
Manual diff viewing
```
Do:
```
magit-diff with syntax highlighting
```

Instead of:
```
Separate tools for different git operations
```
Do:
```
Unified magit interface for complete workflow
```

Instead of:
```
git-gutter for change indicators
```
Do:
```
Integrated change indicators in fringe
```

## Org-mode and Note-taking

For the opinionated GTD + research system (capture → agenda → weekly review + metrics), see [Org Workflow](org-workflow.md).

### Basic Org Operations

Instead of:
```
outline-mode for document structure
```
Do:
```
org-mode  # Rich structured document editing
```

Instead of:
```
Manual TODO tracking
```
Do:
```
org-todo-list  # Integrated task management
```

Instead of:
```
Plain text notes without linking
```
Do:
```
org-roam  # Networked knowledge base
```

Instead of:
```
Manual scheduling and planning
```
Do:
```
org-agenda  # Powerful planning interface
```

### Research Workflows

Instead of:
```
Scattered notes across different tools
```
Do:
```
Centralized org-roam knowledge system
```

Instead of:
```
Manual citation management
```
Do:
```
org-cite with citar  # Integrated bibliography
```

Instead of:
```
Separate note-taking and reference management
```
Do:
```
Unified research workflow in org-mode
```

Instead of:
```
Manual daily note creation
```
Do:
```
org-roam-dailies  # Automatic daily notes
```

### Document Creation

Instead of:
```
LaTeX editing in separate tools
```
Do:
```
org-mode with LaTeX export  # WYSIWYG-like LaTeX
```

Instead of:
```
Manual slide creation
```
Do:
```
org-beamer-export  # Slides from org structure
```

Instead of:
```
Separate tools for different output formats
```
Do:
```
org-export  # Multiple formats from single source
```

Instead of:
```
Manual math formula editing
```
Do:
```
org-latex-preview  # Live LaTeX preview
```

## Window and Frame Management

### Window Navigation

Instead of:
```
C-x o  # other-window cycling
```
Do:
```
M-o  # ace-window - direct window selection
```

Instead of:
```
C-x 1, C-x 2, C-x 3  # Manual window management
```
Do:
```
winner-mode undo/redo  # C-c left/right for window history
```

Instead of:
```
Repeated C-x o to reach desired window
```
Do:
```
M-o then window letter  # Direct jump to any window
```

Instead of:
```
Manual window sizing with mouse
```
Do:
```
C-x {, C-x }  # Keyboard window resizing
```

### Frame Management

Instead of:
```
Multiple Emacs instances
```
Do:
```
M-n  # new-frame for additional workspace
```

Instead of:
```
Alt-tab between applications
```
Do:
```
M-`  # other-frame for frame switching
```

Instead of:
```
Closing entire Emacs session accidentally
```
Do:
```
C-x C-c  # Closes frame or exits if last frame
```

Instead of:
```
Manual frame arrangement
```
Do:
```
M-return  # toggle-frame-maximized
```

### Workspace Organization

Instead of:
```
All buffers mixed together
```
Do:
```
perspective-mode  # Project-based workspaces
```

Instead of:
```
Losing context when switching projects
```
Do:
```
Automatic perspective switching
```

Instead of:
```
Manual buffer organization
```
Do:
```
Project-aware buffer isolation
```

## Common Emacs Anti-patterns

### Inefficient File Access

Instead of:
```
C-x C-f, type full path every time
```
Do:
```
Use recent files, project files, or consult-find
```

Instead of:
```
Opening dired, navigating, then finding files
```
Do:
```
Direct file access with completion
```

Instead of:
```
Bookmarking every frequently accessed location
```
Do:
```
Let the system learn your patterns automatically
```

### Poor Buffer Management

Instead of:
```
Accumulating hundreds of open buffers
```
Do:
```
Use project-kill-buffers and selective buffer closing
```

Instead of:
```
Manually typing buffer names
```
Do:
```
Use pattern matching in buffer selection
```

Instead of:
```
Getting lost in buffer lists
```
Do:
```
Use grouping and filtering in buffer selection
```

### Suboptimal Search Patterns

Instead of:
```
Using basic isearch for everything
```
Do:
```
Choose appropriate search tool: consult-line, consult-ripgrep, etc.
```

Instead of:
```
grep without context or preview
```
Do:
```
Use consult tools with live preview
```

Instead of:
```
Searching in wrong scope (buffer vs project vs global)
```
Do:
```
Use scope-appropriate search commands
```

### Ineffective Completion Usage

Instead of:
```
Typing full command names
```
Do:
```
Use abbreviations with orderless completion
```

Instead of:
```
Tab-completing through long lists
```
Do:
```
Type distinctive patterns to narrow quickly
```

Instead of:
```
Ignoring completion documentation
```
Do:
```
Read the rich context provided by corfu/marginalia
```

### Manual Workflows That Should Be Automated

Instead of:
```
Manually saving and compiling code
```
Do:
```
Set up automatic compilation on save
```

Instead of:
```
Manual backup file management
```
Do:
```
Configure automatic backup handling
```

Instead of:
```
Repetitive text insertion
```
Do:
```
Create yasnippet templates or tempel snippets
```

Instead of:
```
Manual project setup for each language
```
Do:
```
Use dir-locals or project-specific configuration
```

### Poor Git Integration

Instead of:
```
Switching between Emacs and terminal for git
```
Do:
```
Use magit for all git operations
```

Instead of:
```
Manual merge conflict resolution
```
Do:
```
Use magit + smerge for visual conflict resolution
```

Instead of:
```
Forgetting to check git status before committing
```
Do:
```
Use magit-status as your git home base
```

### Org-mode Underutilization

Instead of:
```
Using org-mode just as a text format
```
Do:
```
Leverage agenda, capture, and linking features
```

Instead of:
```
Separate tools for notes, tasks, and scheduling
```
Do:
```
Integrate everything through org-mode ecosystem
```

Instead of:
```
Manual linking between related notes
```
Do:
```
Use org-roam for automatic relationship tracking
```

### Window Management Issues

Instead of:
```
Using mouse for window operations
```
Do:
```
Learn keyboard shortcuts for efficiency
```

Instead of:
```
Manually arranging windows every session
```
Do:
```
Use winner-mode to save/restore layouts
```

Instead of:
```
Working in single window all the time
```
Do:
```
Use multiple windows/frames for context
```

## Advanced Workflow Compositions

### Research Paper Workflow

Instead of:
```
Separate tools for reading, note-taking, and writing
```
Do:
```
org-roam → citar → org-latex → org-export pipeline
```

Instead of:
```
Manual citation formatting
```
Do:
```
citar + org-cite for automatic bibliography
```

Instead of:
```
Disconnected notes and references
```
Do:
```
Integrated knowledge graph with org-roam
```

### Development Project Workflow

Instead of:
```
IDE + terminal + git GUI separately
```
Do:
```
project-mode + eglot + magit + compilation integration
```

Instead of:
```
Manual project switching overhead
```
Do:
```
perspective-mode + project integration
```

Instead of:
```
Separate debugging setup
```
Do:
```
dape integration with project and language modes
```

### Daily Knowledge Work

Instead of:
```
Scattered task management across tools
```
Do:
```
org-agenda + org-capture + org-roam integration
```

Instead of:
```
Manual daily note creation
```
Do:
```
org-roam-dailies with template automation
```

Instead of:
```
Forgetting to review and follow up
```
Do:
```
org-agenda scheduling and review cycles
```

## Diagnostic and Optimization

### Performance Troubleshooting

Instead of:
```
Wondering why Emacs is slow
```
Do:
```
profiler-start, reproduce issue, profiler-report
```

Instead of:
```
Guessing which package causes problems
```
Do:
```
Use built-in debugging and profiling tools
```

### Configuration Issues

Instead of:
```
Editing init file directly
```
Do:
```
Understand the modular configuration system
```

Instead of:
```
Adding random configurations found online
```
Do:
```
Follow the established configuration patterns
```

Instead of:
```
Ignoring package loading errors
```
Do:
```
Check *Messages* buffer for diagnostic information
```

### Learning and Discovery

Instead of:
```
Memorizing all possible commands
```
Do:
```
Use which-key to discover available options
```

Instead of:
```
Random key mashing to find features
```
Do:
```
Use describe-key (C-h k) and describe-function (C-h f)
```

Instead of:
```
Asking others for every small thing
```
Do:
```
Use built-in help system and apropos commands
```

This workflow guide demonstrates how the carefully configured Emacs environment provides powerful, integrated workflows that eliminate context switching and manual overhead. The key is learning to trust the completion system, use appropriate search scopes, and leverage the rich integration between different tools rather than fighting against the configuration or falling back to external tools.
