# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/).

## [Unreleased]
### Added
- Emacs daemon service configuration for mileva and sparck machines
- Git completions integration from Guix profiles in Zsh configuration
- FZF productivity helpers in Zsh: ctrl+t/alt+c widgets, `fe`/`fcd`/`rgo`/`fssh`, and git staging helpers (`gaf`/`gap`/`grf`/`grp`)
- Comprehensive Zsh user manual with modern command replacements
- LD_LIBRARY_PATH export and codex alias in Zsh configuration
- Structured Emacs setup directory for organization files
- Git commit guidelines in CLAUDE.md to prevent accidental commits
- Emacs cursor pulse for easier point tracking during navigation
- Emacs config doctor (`bv-doctor` / `scripts/emacs-doctor.sh`) for batch validation
- DPI-aware per-monitor font scaling for GUI frames
- Tree-sitter grammar audit command (`bv-treesit-audit`)
- Project cockpit actions in `project-switch-project` (ripgrep, test, magit, dape)
- Visual undo tree via `vundo` with friendlier redo keys
- Direnv/envrc project environment syncing (`bv-envrc`)
- Jump-anywhere navigation with Avy + Embark dispatch (`bv-avy`)
- Circadian Alacritty light/dark theming with synced Zsh/FZF/Bat palettes
- Fast parallel ClamAV scan helper script with structured reports (`scripts/fast-clamscan.sh`)
### Changed
- Formatted Guix machine configurations with guix style for consistency
- Refactored scripts with guix style formatting and cleaned up new-client-cert
- Modularized Emacs configuration into focused components
- Updated BV font stack defaults and Unicode fallbacks (JetBrains Mono, FiraGO, Noto Serif)
- Refined BV Emacs light/dark theme palettes for improved contrast and a more polished completion UI
- Disabled underlines across BV themes (links, comments, diagnostics) in favor of color and subtle background emphasis
- Tuned header-line modeline padding and fringe/background integration for a cleaner look
- Enabled right-only window dividers for clearer window separation
- Enabled `save-place`, `auto-revert`, `repeat-mode`, and pixel-precise scrolling defaults
- Expanded Vertico multiform categories and tuned Consult preview behavior for faster “peek” navigation
- Made Corfu auto-popup mode-aware (manual in `git-commit-mode` buffers)
- Calmed Eglot progress/events noise and added a Flymake quickfix helper (`bv-flymake-quickfix`)
- Improved TTY parity (mouse wheel + Corfu terminal awareness) and prefer newer `.el` over stale `.elc`
- Extended tree-sitter remaps (yaml/json/toml/css/html/dockerfile) and set `treesit-font-lock-level` to 4
- Tightened diagnostics UX: Flymake next/prev now “peek” messages at point, plus `bv-flymake-show-at-point`
- Improved default search ergonomics with `bv-consult-search` (DWIM project ripgrep vs cross-buffer search)
- Made `xref` use ripgrep backend when available
- Open CUDA files (`.cu`/`.cuh`) in C++ tree-sitter mode by default
- Zsh prompt now shows active environment names (virtualenv/Guix/direnv) and `l` is the default `eza` listing alias
- Guix home profiles include `%shell-zsh` bundle for Zsh tool dependencies (fzf, ripgrep, bat, eza, fd, zoxide, direnv, wl-clipboard)
- Enhanced mileva machine configuration
- Updated mileva configuration: adjust Zsh plugin load order, disable beets service, add securityfs mount, and add NVIDIA profiling kernel arg
- Updated sparck configuration: adjust Zsh plugin load order, disable beets service, and add media converter packages
- Adjusted pre-commit whitespace checks to avoid conflicts with guix style Scheme formatting
- Hardened Git defaults: scoped Guix send-email settings, default-branch-aware aliases, POSIX-safe pre-commit hook, and safer global gitattributes
- Configured Alacritty to start Zsh as login shell
- Switched Alacritty hint launcher to `xdg-open` and added a copy-hint binding
- Updated Alacritty’s BV light/dark palettes to match BV Emacs themes (including selection/search surfaces)
### Fixed
- Added missing emacs-pgtk package import in Guix configurations
- Removed restrictive ZSH_EVAL_CONTEXT check preventing shell startup
- Eliminated Zsh welcome message for cleaner shell initialization
- Resolved Zsh startup errors and plugin loading issues
- Improved Zsh reliability: corrected Guix plugin load order, fixed `gco`, and fixed git/FZF selectors
- Removed redundant custom zprofile configuration
- Corrected multiple import and configuration errors in mileva.scm
- Fixed indentation in sparck system services configuration
- Restored completion-at-point in `git-commit-mode` buffers
### Removed
- Obsolete Emacs configuration files

## [2025-06-29]
### Added
- Stripped Emacs configuration with core modules and UI defaults.
- Core productivity modules for completion, navigation, development,
  and Git integration. Phase 2 now enabled in `init.el`.
- Language support modules for Python, Rust, Lisp and C/C++.
- Research modules for Org, research, reading, and writing.
- Shell, productivity, communication and multimedia modules.
- Integration with Minions to declutter Emacs mode line.
- Extensive enhancements to Org-mode configuration, including expanded TODO keywords, habit tracking, and LaTeX support.
- Comprehensive research workflow setup using Org Roam and Citar with enhanced PDF and citation management.
- Org-LaTeX setup file (`setupfile.org`) for consistent document headers and settings across Org documents.
- Comprehensive test suite for `bv-core.el` with 25+ unit tests covering configuration values, feature system, path utilities, macros, and timer management.
- Transient-based menu interface (`bv-transient.el`) providing interactive access to configuration management, feature registration, path operations, and developer tools.
- Complete test suite for `bv-transient.el` with mock-based testing covering interactive commands, value manipulation, feature management, and development utilities.
- Better defaults module (`bv-defaults.el`) providing sensible Emacs configurations with XDG-compliant file handling, modern editing defaults, enhanced keybindings, and automatic whitespace management.
- Comprehensive test suite for `bv-defaults.el` with 20+ unit tests covering directory infrastructure, custom variables, keymaps, file handling, whitespace management, and toggle commands.
- Comprehensive test suite for `bv-ui.el` with 50+ unit tests covering theme system, font configuration, mode line management, timer scheduling, and interactive commands.
### Changed
- Phase 3 modules are now enabled in `init.el`.
- Updated comments to reflect active module loading.
- Documented that every pull request must update `CHANGELOG.md`.
- Phase 4 modules are now enabled in `init.el`.
- Phase 5 modules are now enabled in `init.el`.
- Removed URL rewrite aliases from `git/gitconfig`.
- Pruned deprecated and platform-specific options from `git/gitconfig`.
- Removed Guix environment checks from `emacs/init.el`; rely on `EMACSLOADPATH`.
- Rewrote `emacs/lisp/bv-git.el` for improved Git integration.
- Replaced `emacs/early-init.el` with theme-aware flash prevention and
  performance optimizations.
- Replaced `init.el` with streamlined Guix-centric bootstrap.
- Replaced `bv-navigation.el` with expanded project and window management features.
- Replaced `bv-core.el` with expanded configuration and path helpers.
- Redesigned UI module with automatic theme switching and header line support.
- Major refactor of `bv-ui.el` with comprehensive UI configuration system including environment variable support, compatibility layer for Emacs 30+, enhanced theme switching with time-based automation, improved font management, configurable window decorations, and extensive documentation.
- Removed obsolete machine configurations for `leibniz` and manifests for haskell-manifest.scm, julia-manifest.scm, guile-manifest.scm, scientific-manifest.scm.
- Added new machine configurations for `mileva` (AMD Ryzen 9 5900X workstation) and `sparck` (ThinkPad laptop).
- Enhanced `mileva` home configuration with zprofile and fzf-tab integration.
- Enhanced `sparck` home configuration with comprehensive services.
- Optimized MPV configuration for RTX 3060 with GPU shaders and hardware acceleration.
- Completely rewrote Alacritty configuration for better Emacs workflow integration and improved keybindings.
- Complete rewrite of Zsh configuration with modern shell experience including advanced prompt, completion system, and plugin management.
- Added public SSH keys for new machines.
- Implemented git hooks for code quality and commit standards.
- Enhanced setup script with automatic git hook installation.
- Comprehensive update to .gitignore for modern development workflows.
- Streamlined README with focus on hot paths and better navigation.
- Updated CLAUDE.md with current context and comprehensive style guidelines.
- Replaced `bv-completion.el` with modernized configuration.
- Replaced `bv-development.el` with simplified configuration.
- Rewrote `bv-git.el` with improved Git integration.
- Replaced `bv-navigation.el` with expanded project and window management features.
- Updated `ragnar` system configuration.
- Delayed loading of research-related modules (`bv-org`, `bv-research`) in `init.el`.
- Default font updated to "SF Mono" with adjusted sizing for improved readability.
- Mode line time display reformatted for clarity and frequent updating.
- Org Roam capture templates expanded to include literature notes, concepts, problems, and timed tasks.
- Improved bibliographic integration in Citar with custom icon indicators.
- Enhanced Org Roam node display with backlink counts and directory context.
- Reorganized idle-time loading sections in `init.el` for clearer module initialization.
- Expanded `bv-core.el` with comprehensive feature system, XDG compliance, circular dependency detection, and improved path utilities.
- Enhanced `bv-transient.el` with cache directory management integration, adding "Open Cache Dir" and "Reset Cache Dirs" commands to the advanced menu.
- Reformatted `git/gitconfig` with consistent indentation, updated autocrlf setting to false, added new Guix patch aliases, and reorganized alias sections with comments.
- Enhanced `git/gitattributes` with improved Guix-specific file handling, including patch/diff binary treatment, enhanced Scheme file detection, and Texinfo documentation support.
- Added GROBID configuration file (`grobid/grobid.yaml`) with optimized settings for academic document processing, including deep learning model configurations and bibliographic consolidation.
- Enhanced `scripts/style.scm` with command-line argument support for flexible file and directory processing, including help/version options and improved error handling.
- Renamed `scripts/deduplicate.scm` to `scripts/dedup.scm` for brevity while maintaining the same Guix package deduplication functionality.
- Added Zsh configuration with modern shell setup including direnv integration, UTF-8 Japanese prompt indicators, completion system tuning, and quality-of-life aliases for navigation, Git, and utilities.
- Updated `.gitignore` to allow `zsh/zshenv` file for environment variable configuration.
- Enhanced `alacritty/alacritty.toml` with improved keyboard bindings including Ctrl+C interrupt signal support and consistent formatting throughout configuration sections.
- Major refactor of `guix/machines/ragnar.scm` with comprehensive home services reorganization including Zsh configuration with plugin support, improved GPG agent setup, enhanced SSH configuration with connection multiplexing, and streamlined system service management.
### Fixed
- Addressed syntax errors in `bv-writing.el` and `bv-core.el` that
  prevented productivity modules from loading.
- Balanced parentheses in `emacs/lisp/bv-core.el`.
- Added missing closing parenthesis in `bv-core.el` to fix initialization error.
- Added `bv-leader` macro and corrected quoting in completion and writing modules.
- Prevented startup errors when optional packages are missing.
- Closed unmatched parentheses in `bv-research.el` and removed invalid key binding from `bv-productivity.el`.
- Resolved syntax errors in `bv-lang-rust.el` and `bv-writing.el`.
- Fixed project switching configuration type in `bv-navigation.el`.
- Corrected dictionary list syntax and removed conflicting GPT keybindings.
- Fixed duplicate multimedia playlist keybinding and ensured parentheses
  balance in `bv-multimedia.el`.
- Balanced unmatched parentheses across core and multimedia modules.
- Guarded fringe configuration to avoid errors in non-graphical builds.
- Fixed invalid dictionary syntax and stray parens in `bv-writing.el`.
- Removed invalid `M-s` unbinding that caused a smartparens error.
- Added prefix map to resolve `P P` keybinding error in `bv-productivity.el`.
- Fixed syntax error in `bv-writing.el` at line 118 caused by unmatched closing bracket.
- Resolved keybinding conflict in `bv-communication.el` where 'w' was used as both a command and prefix key.
- Fixed `org-clocking-p` error in `bv-productivity.el` mode-line indicator by adding proper function existence check.
- Replaced multimedia configuration to resolve `M m` prefix key error.
- Checked for `git-gutter` before enabling hooks to avoid missing function errors.
- Added Guix profile directories to `load-path` to resolve missing packages like
  `highlight-indent-guides`.
- Guarded additional `prog-mode` hooks to defer loading optional packages.
- Replaced `bv-defaults.el` with streamlined configuration and modern defaults.
- Balanced parentheses in various files.
- Resolved minor syntax and keybinding conflicts in `bv-research.el` and `bv-ui.el`.
- Updated variable names in Emacs writing configuration for consistency.
- Corrected indentation in mileva system services configuration.
### Removed
- Old Emacs configuration to prepare for a new setup.
- Removed Airflow container service from `ragnar` machine.
- Eliminated Haskell configuration and packages.
- Removed flymake-indicator package configuration.
