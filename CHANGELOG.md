# Changelog

All notable changes to the `.config` repository will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Configuration for `cycle-buffer.el`.
- Macro `:local-or-package` to `emacs/lisp/bv-setup.el`.
- Package file for `cycle-buffer.el` by Vladimir Alexiev to `emacs/lisp`.
- Configuration for `mjolnir-mode`, `windmove`, `windsize`, `ace-window`.
- Configuration for `emacs-guix`.
- `Iosevka Comfy` as default and fixed-pitch face, `DejaVu Sans` as variable-pitch face.
- Macro `:load-after` to `emacs/lisp/bv-setup.el`.
- `.gitignore` file to manage ignored files within the `emacs` directory.
- Configuration for `doom-themes`, `doom-modeline`, `rainbow-delimiters`, `rainbow-mode`, and `no-littering`.
- Variable `bv-not-guix-p` to simplify system dependent conditional configuration.
- Configuration for `display-line-numbers`, `recentf`, and `display-time-format`.
- Configuration for default preferences for improved Emacs experience.
- `setup.el` based custom macros to `emacs/lisp/bv-setup.el` for configuring Emacs.
- Configuration for user information (`whoami`) and essential keybindings (`bv-essentials`).
- Macros `:set`, `:set-default`, `:straight-if`, and `:option*` to `emacs/lisp/bv-setup.el`.
- load of `setup.el` via `straight.el` in `emacs/init.el`.

### Changed
-  refactored methods `cycle-buffer` and `cycle-buffer-show` in `cycle-buffer.el`.
- `emacs/init.el` improved with better error handling for `setup.el`.
- `emacs/lisp/bv-essentials.el` updated to include new default number of retries for bootstrapping `straight.el`.

### Removed
- Windmove related functions (`bv-windmove-nw`, `bv-windmove-ne`, `bv-windmove-sw`, `bv-windmove-se`, `bv-lsp-copy-diagnostic-at-point`) from `emacs/lisp/bv-essentials.el`


## [0.1.0] - 2024-04-06

### Added
- Initial setup of the repository with basic structure.
- `emacs` directory.
- `early-init.el` and `init.el` Emacs configuration files.
- LICENSE file to specify the project's license.
- README.md to write a Haiku poem.
- VERSION file to track project versions.
