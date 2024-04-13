# Changelog

All notable changes to the `.config` repository will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Configuration for `geiser` and `geiser-guile`.
- Configuration for `yasnippet`.
- A shell script `setup.sh` under repo root, to be executed post clone.
- A shell script `update-snippets.sh` under `emacs/scripts` to pull snippets from various snippet-repos.
- Configuration for `oauth2`, `pinentry`, `epa-file`, `auth-source`, `password-store`.
- Configuration for `dired`, `dired-hacks`, `dired-rainbow`.
- Configuration for `pdf-tools`.
- Configuration for `websocket`, `simple-httpd`, `org-roam-ui`.
- A `setupfile.org` under `emacs/setup/setupfile.org` holding sane defaults for headers.
- Configuration for `all-the-icons`, `embark`, `embark-consult`, `org-cite`, `citar`, and `org-roam`.
- Macro `:delay` to `emacs/lisp/bv-setup.el`.
- New Lisp module `emacs/lisp/bv-org-roam.el`.
- Configuration for `bv-latex` in the `Org` setup block.
- New Lisp module `emacs/lisp/bv-latex.el`.
- Macro `:alias` to `emacs/lisp/bv-setup.el`.
- Configuration for `org-fragtog` and `nerd-icon`.
- Configuration for `corfu`.
- Configuration for `savehist`.
- Confirmation messages to each setup block.
- Configuration for `org-mode` and `org-faces`.
- Function `bv-setup-org-fonts` to `emacs/lisp/bv-essentials.el`.
- Macro `:push-to` to `emacs/lisp/bv-setup.el`.
- Configuration for `adaptive-wrap`, `smartparens`, `nerd-icons`, `olivetti`, `which-key`, `vertico`, `orderless`, `marginalia`.
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
-  Changed permission to make `./setup.sh` executable.
-  Enabled `citar-embark-mode` in the `citar` setup block.
-  Updated `emacs/setup/setupfile.org` to include header declaration for definition, lemma, theorem, corollary, etc.
-  Variable `org-preview-latex-image-directory` set to `~/.local/latex-previews` instead of `"~/slipbox/.latex-previews/"`
-  Improved confirmation messages.
-  Refactor `:straight-if` to fallback to `straight.el` in case of package unavailability on a Guix system.
-  Refactor methods `cycle-buffer` and `cycle-buffer-show` in `cycle-buffer.el`.
- `emacs/init.el` improved with better error handling for `setup.el`.
- `emacs/lisp/bv-essentials.el` updated to include new default number of retries for bootstrapping `straight.el`.

### Removed
- Hooks involving non-existent functions `bv/zap-newline-at-eob`, `bv-auto-insert-bibliography`, `bv-org-buffer-default-face` in the `Org` setup block.
- Windmove related functions (`bv-windmove-nw`, `bv-windmove-ne`, `bv-windmove-sw`, `bv-windmove-se`, `bv-lsp-copy-diagnostic-at-point`) from `emacs/lisp/bv-essentials.el`.


## [0.1.0] - 2024-04-06

### Added
- Initial setup of the repository with basic structure.
- `emacs` directory.
- `early-init.el` and `init.el` Emacs configuration files.
- LICENSE file to specify the project's license.
- README.md to write a Haiku poem.
- VERSION file to track project versions.
