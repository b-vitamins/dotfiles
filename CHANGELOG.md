# Changelog

All notable changes to the `.config` repository will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- `.gitignore` file to manage ignored files within the `emacs` directory.
- Conditional configuration for `doom-themes`, `doom-modeline`, `rainbow-delimiters`, `rainbow-mode`, and `no-littering` based on system check (`bv-guix-p`).
- Introduced the use of a variable (`bv-not-guix-p`) to simplify system dependent conditional configuration.
- Setup blocks for `display-line-numbers`, `recentf`, and `display-time-format`.
- Default preferences configuration for improved Emacs experience.
- `emacs/lisp/bv-setup.el` file for custom Emacs setup functions and macros based on `setup.el`.
- Definitions for user information (`whoami`) and essential keybindings (`bv-essentials`).
- Added macros `:set`, `:set-default`, `:straight-if`, and `:option*`.
- `setup.el` loading via `straight.el` in `init.el`.

### Changed
- `init.el` improved with better error handling for `setup.el`.
- `bv-essentials.el` updated to include new default number of retries for bootstrapping `straight.el`.

### Removed
- Several functions from `emacs/lisp/bv-essentials.el` (`bv-windmove-nw`, `bv-windmove-ne`, `bv-windmove-sw`, `bv-windmove-se`, `bv-lsp-copy-diagnostic-at-point`) in anticipation of their inclusion elsewhere in the future.


## [0.1.0] - 2024-04-06

### Added
- Initial setup of the repository with basic structure.
- `emacs` directory.
- `early-init.el` and `init.el` Emacs configuration files.
- LICENSE file to specify the project's license.
- README.md to write a Haiku poem.
- VERSION file to track project versions.
