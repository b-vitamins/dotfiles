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
### Fixed
- Balanced parentheses in `emacs/lisp/bv-core.el`.
- Added `bv-leader` macro and corrected quoting in completion and writing modules.
- Prevented startup errors when optional packages are missing.
- Closed unmatched parentheses in `bv-research.el` and removed invalid key binding from `bv-productivity.el`.
### Removed
- Old Emacs configuration to prepare for a new setup.
- Removed Airflow container service from `ragnar` machine.
