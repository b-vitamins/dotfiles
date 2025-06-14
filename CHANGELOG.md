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
- Replaced `bv-core.el` with expanded configuration and path helpers.
- Redesigned UI module with automatic theme switching and header line support.
- Replaced `bv-completion.el` with modernized configuration.
- Replaced `bv-development.el` with simplified configuration.
- Rewrote `bv-git.el` with improved Git integration.
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
### Removed
- Old Emacs configuration to prepare for a new setup.
- Removed Airflow container service from `ragnar` machine.
- Eliminated Haskell configuration and packages.
- Removed flymake-indicator package configuration.
