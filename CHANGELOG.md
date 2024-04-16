# Changelog

All notable changes to the `.config` repository will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Add gnome configurations for setting keybindings, user directories, power settings, and default apps using `gsettings`, `dconf`, and `xdg-utils`.
- Add `emacs-init.sh` to `emacs/scripts` - copies all files and folders under `emacs` directory from the dotfiles repository to `.config` of the local. As a prelude, deletes `.emacs.d` directory and its contents when they exist, so that `.config/emacs` is understood as the place for all emacs configuration.
- Add `style.sh` to `guix/scripts` - invokes `guix style` on all scheme files under the `guix` directory.
- Add `profile-init.sh` to `guix/scripts` - loops through the manifests under `guix/manifests` and calls `guix package` by chaining the manifest files using multiple `-m` optional arguments to create a new profile containing *all* the packages contained in *all* the manifests.
- Add `guix-init.sh` to `guix/scripts` - copies all files and folders in the `guix` directory from the dotfiles repository to `.config` of the local and triggers a `guix pull` subject to some conditions.
- Add `production-manifest.scm` to `guix/manifests`. It holds several applications and tools for professional audio, video, print production workflows.
- Add `desktop-manifest.scm` to `guix/manifests`. It holds several commonly used "desktop" applications.
- Add `development-manifest.scm` to `guix/manifests`. It holds several compilers, interpreters, frameworks, and toolchains common to software development workflows.
- Add `core-manifest.scm` to `guix/manifests`. It holds many commonly used basic packages.
- Add more packages to `guix/manifests/python-manifest.scm`.
- Add more fonts to `guix/manifests/fonts-manifest.scm`.
- Add system-wide packages `bluez`, `bluez-alsa`, `nss-certs`, `coreutils`, `gvfs`, `alacritty`, `firefox`, `git`, `gnome-tweaks`, `gnome-boxes`, `vlc`, `mpv`, `yt-dlp`, `font-dejavu`, `font-iosevka-comfy`, `font-config`, `imagemagick`, and `ffmpeg` to `guix/config.scm`.
- Add services `bluetooth-service-type`, `bitmask-service-type`, `docker-service-type`, `libvirt-services-type`, `rasdaemon-service-type`, `earlyoom-service-type`, `spice-vdagent-service-type`, `inputattach-service-type`, `nftables-service-type`, `syncthing-service-type`, `pam-limits-service-type` to `guix/config.scm`.
- `emacs-manifest.scm`, `texlive-manifest.scm`, `python-manifest.scm`, and `perl-manifest.scm` under `guix/manifests`.
- Guix channels in `guix/channels.scm` and associated signing-keys under `guix/keys`.
- Guix operating-system declaration in `guix/config.scm`
- `alacritty.toml` configuration file under `alacritty` folder.
- `.zshrc` configuration file and `lscolors.sh` under `zsh` folder.
- Configuration for `god-mode`.
- Configure `python-indent` and `py-indent-offset` variables.
- Add key-binding for `bv-blacken-buffer` in `python-mode`.
- Add function `bv-blacken-buffer` to `lisp/bv-essentials.el` for use in `python-mode`.
- Clock-report customization in org-agenda using variables `org-agenda-clockreport-parameter-plist`, and `org-agenda-clock-report-header`.
- Configuration for `blacken`.
- Configuration for `lsp-jedi`.
- Configuration for `python-mode`, `rust-mode`, `haskell-mode`, `gnuplot-mode`, `lua-mode`, `json-mode`, `dockerfile-mode`, `yaml-mode`, `toml-mode`, `julia-mode`, `cmake-mode`.
- Configuration for `flycheck-package`.
- Configuration for `rustic`.
- Configuration for `flycheck`, `flycheck-inline`, `flycheck-haskell`, `flycheck-rust`, `flycheck-guile`, `flycheck-cpplint`.
- Add function `bv-copy-flycheck-overlay-at-point-to-kill-ring` to `lisp/bv-essentials.el` for use with `flycheck`.
- Configuration for `lsp-mode`, `lsp-ui`.
- Configuration for `nerd-icons-dired`, `nerd-icons-corfu`, `nerd-icons-ibuffer`, `nerd-icons-completion`.
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
- Modified `%desktop-services` to 1) authorize substitute fetches from `https://substitutes.nonguix.org`, 2) add `bluez-alsa` to the dbus, 3) configure gdm to use wayland.
-  A new Guix operating-system declaration in `guix/config.scm`. Change is non-increment for all practical purposes. 
-  Moved all `nerd-icons` packages to the very end as a hack to make `nerd-icons-completion-mode` actually show icons during completions. Ideally, should have figured out the package interaction causing the icons to not show up. Will do a more principled fix later.
-  Path for `org-roam-directory` in `emacs/init.el`.
-  Path for `reconfigure` alias in `zsh/.zshrc`.
-  `alacritty` window dimension.
-  `.gitignore` to track `zsh/.zshrc` and `zsh/lscolors.sh`.
-  Change key-bindings for `lsp-mode` to use `:global` macro.
-  Add key-bindings for `lsp-mode` using `:bind` macro.
-  Replace `(:load-after flycheck-mode)` with `(:load-after flycheck)` in `flycheck-inline` setup block.
-  Hook `flycheck-package-setup`, `flycheck-rust-setup`, `flycheck-haskell-setup` to `flycheck-mode-hook`.
-  Add `:require` macro eval to `flycheck-guile`, `flycheck-package`, `flycheck-rust`, `flycheck-haskell`.
-  Add `(:load-after flycheck flycheck-inline)` macro eval to `flycheck-guile`, `flycheck-package`, `flycheck-rust`, `flycheck-haskell`.
-  Add success messages for `geiser` and `geiser-guile` setup blocks.
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
-  `amdgpu` and `radeon` from `modprobe.blacklist` kernel parameters in `guix/config.scm`.
-  `C-c b` key-binding for `bv-blacken-buffer` from `python-mode-map`.
- `blacken` package.
- `flycheck-cpplint` package.
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
