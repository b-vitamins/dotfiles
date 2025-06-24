# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/).

## [Unreleased]
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
### Removed
- Old Emacs configuration to prepare for a new setup.
- Removed Airflow container service from `ragnar` machine.
- Eliminated Haskell configuration and packages.
- Removed flymake-indicator package configuration.
