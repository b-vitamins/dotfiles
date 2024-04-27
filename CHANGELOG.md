# Changelog

All notable changes to the `.config` repository will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Add module `bv-file-navigation.el` for file navigation related functionality at `emacs/lisp/bv-file-navigation.el`.
- Add new functions in `emacs/lisp/bv-essentials.el` for improved window management:
  - `bv-open-file-in-window`: Main function to open files in a new window with adjustable direction (left/right), focus control, and window size ratio. Enhances the user's ability to manage their workspace by providing more flexibility in how documents are displayed and interacted with.
  - Convenience functions for opening files with predefined settings:
    - `bv-open-file-left-jump`: Opens a file in a new window to the left and switches focus.
    - `bv-open-file-left-stay`: Opens a file in a new window to the left without changing focus.
    - `bv-open-file-right-jump`: Opens a file in a new window to the right and jumps to it.
    - `bv-open-file-right-stay`: Opens a file in a new window to the right but keeps the focus in the original window.
- Add `:quit` macro in `emacs/lisp/bv-setup.el` to unconditionally abort the evaluation of the current setup body.
- Add function `bv-move-to-trash` to `emacs/lisp/bv-essentials.el` to delete open files from within Emacs.
- Add macro `bv-define-insert-delimiter` to `emacs/lisp/bv-essentials.el` and use it to define several functions for inserted unpaired versions of pairing delimiters.
- Add axiom, postulate, proof environments to `setupfile.org` in `emacs/setup`.
- Hook `smartparens-mode` to `org-mode`.
- Set default value of `left-margin-width` and `right-margin-width` to 2 column.
- Add `org-preview.el` (fast, async, latex preview) to `emacs/lisp` for further hacking.
- Add function `bv-zap-newline-at-eob` to `emacs/lisp/bv-essentials.el` for use during tangling yasnippet snippets. Hooked it to `org-babel-post-tangle-hook`.
- Add `angle` and `quotes` to `usetikzlibrary` import in the latex headers in `emacs/setup/setupfile.org`.
- Add evaluation of `bv-store-default-mode-line-colors` inside doom-themes setup block.
- Add function `bv-store-default-mode-line-colors` to `bv-essentials.el`, invoked inside the `doom-emacs` setup block to populate `bv-default-mode-line-foreground`, `bv-default-mode-line-background`, `bv-default-mode-line-inactive-foreground`, and `bv-default-mode-line-inactive-background`.
- Add variables `bv-default-mode-line-foreground`, `bv-default-mode-line-background`, `bv-default-mode-line-inactive-foreground`, and `bv-default-mode-line-inactive-background` to `bv-essentials.el`.
- Add `alacritty-init.sh` to `alacritty/scripts` - copies `alacritty/alacritty.toml` to `.config/alacritty`.
- Add `zsh-init.sh` to `zsh/scripts` - copies all files under `zsh` directory from the dotfile repository to `.config/zsh` of the local. Then creates `~/.zshrc` and `.zprofile` which source `.config/zsh/zshrc` and `.config/zsh/zprofile` respectively.
- Add `zsh/zshrc` and `zsh/zprofile` with zsh configuration.
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
- Renamed various file opening functions in `bv-file-navigation.el` that started with `open-my-<filename>` to `bv-open-my-<filename>`.
- Moved `bv-open-file-in-window`, `bv-open-file-left-jump`, `bv-open-file-left-stay`, `bv-open-file-right-jump`, `bv-open-file-right-stay` to a dedicated `bv-file-navigation.el` module.
- Reorganized keybindings in `init.el` to enhance accessibility and resolve key conflicts:
  - Unbound `C-c C-e` and `C-c C-f` to free up key prefixes for more intuitive bindings.
  - Added new global keybindings for quick delimiter insertion including parentheses, braces, brackets, and quotes.
  - Implemented robust file and buffer management shortcuts, facilitating easier navigation and organization of multiple files.
  - Enhanced code evaluation shortcuts for Emacs Lisp and integrated efficient search functionalities with grep.
  - Organized text manipulation and visual toggle keybindings to improve editing workflows and interface clarity.
  - Updated Olivetti mode toggle to `C-c C-c` from `C-c C-h` to optimize keybinding layout and avoid conflicts.
- Added check for empty string in the function `org-preview-create-formula-image` in file `emacs/lisp/org-preview.el` to prevent `aset` errors.
- Refined the handling of LaTeX fragment processing in `org-preview.el` to define critical variables such as `link`, `value`, and `block-type` within their operational scope. This update ensures more reliable and accurate rendering of LaTeX fragments in the buffer, particularly in complex documents with multiple types of content.
- Improved the organization of `org-preview.el` by rearranging variable and function declarations to enhance code readability and structure. Variables related to process compilers and converters are now grouped at the beginning of the file.
- Introduced `initiate-processing-steps` in `org-preview.el` to handle the initiation of LaTeX compilation and image conversion processes. This new function reduces code complexity in `org-preview-create-formula-image` by consolidating process initiation into `initiate-processing-steps`.
- Refactored the LaTeX file creation process in `org-preview.el` by introducing `create-texfile`, a new function that simplifies the generation of LaTeX files.
- Enhanced the modularity of `org-preview.el` by extracting configuration and utility functionalities into separate functions (`get-latex-header`, `get-image-properties`, `get-post-clean-items`, and `get-image-converter`). This refactoring aims to simplify `org-preview-create-formula-image`.
- Continued refactoring of LaTeX processing in `org-preview.el` by further modularizing `org-preview-process-generic`. Introduced `set-tex-process-sentinels` and `set-image-process-sentinels` to handle specific aspects of process completion and error management. This division enhances the clarity and maintainability of the code, ensuring each component is responsible for a distinct part of the processing logic.
- Extensively refactored LaTeX processing in `org-preview.el` by introducing multiple utility functions:
  - `org-preview-ensure-directory`: Ensures necessary directories are created and existing.
  - `determine-color`: Dynamically determines appropriate colors for LaTeX processing.
  - `prepare-options`: Sets up processing options based on current context.
  - `collect-latex-fragments`: Gathers all LaTeX fragments in the document for processing.
  - `org-preview-process-generic`: Acts as the central processing function for LaTeX fragments based on predefined conditions.
- Modularized the error handling for unsupported ImageMagick previews into the new function `org-preview-process-imagemagick`. This sets the foundation for future enhancements to include support for ImageMagick based previews, improving the architecture for upcoming feature integrations.
- Refactored `org-preview-format-latex` to improve code maintainability by extracting MathML processing into the new `org-preview-process-mathml` function. This modularization helps in isolating MathML-specific processing, facilitating easier modifications and maintenance.
- Refactored `org-preview-format-latex` to further enhance code maintainability by extracting HTML processing into the new `org-preview-process-html` function. This extraction continues to simplify the main function and clearly separates HTML processing logic, facilitating easier modifications and debugging.
- Refactored `org-preview-format-latex` to improve code maintainability by extracting MathJax processing into the new `org-preview-process-mathjax` function. This change simplifies the main function and isolates MathJax-specific logic, enhancing clarity and ease of updates.
- Improved the logging format in `org-preview-report` to use human-readable timestamps (HH:MM:SS) for better readability and debugging.
- Fix `LOCAL_BASE_DIR` path in `emacs/scripts/update-snippets.sh` shell script.
- Set `org-return-follows-link` to `t` in org-mode setup block.
- `corfu-auto-delay` to 0.30.
- face size for org headings.
- `bv-god-mode-update-mode-line` in `bv-essentials.el` to use `bv-default-mode-line-foreground`, `bv-default-mode-line-background`, `bv-default-mode-line-inactive-foreground`, and `bv-default-mode-line-inactive-background`.
- `olivetti-body-width` set to 100.
- `org-agenda-files` set to `~/main.org` and all of `~/slipbox`.
-  Use `:hook-into` in `org-fragtog` setup block to hook `org-fragtog-mode` to `org-mode-hook`.
-  `:scale` parameter of `org-format-latex-options` to 4.0.
-  Modified `.gitignore` to start tracking `zsh/zshrc` and `zsh/zprofile`.
-  Modified `%desktop-services` to 1) authorize substitute fetches from `https://substitutes.nonguix.org`, 2) add `bluez-alsa` to the dbus, 3) configure gdm to use wayland.
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
- Hardcoded `background-color` and `foreground-color` in `default-frame-alist`.
-  `zsh/.zshrc` file.
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
