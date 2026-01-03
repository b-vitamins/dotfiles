# Emacs Keybindings Reference

Comprehensive keybinding reference for the bv-emacs configuration. Organized by category for quick lookup.

## Quick Reference

| Key | Action | Description |
|-----|--------|-------------|
| `C-c a` | bv-app-map | Application launcher prefix |
| `C-c a o` | bv-org-map | Org command center |
| `C-c a d` | bv-debug-map | Debugger map |
| `C-c a E` | bv-terminal-map | Terminal/shell map |
| `C-c n` | org-roam-map | Org-roam operations |
| `C-c c` | citation-map | Citation operations |
| `M-o` | ace-window | Window switching |
| `C-s` | consult-line | Enhanced search |
| `s-p` | project-prefix-map | Project operations |
| `s-t` | bv-eat | Terminal |
| `s-d` | dired-jump | Directory editor |

## Table of Contents

- [Core Bindings](#core-bindings)
- [Window & Frame Management](#window--frame-management)
- [File & Buffer Operations](#file--buffer-operations)
- [Search & Navigation](#search--navigation)
- [Completion System](#completion-system)
- [Project Management](#project-management)
- [Version Control](#version-control)
- [Org Mode](#org-mode)
- [Org-roam](#org-roam)
- [Development](#development)
- [Language-specific](#language-specific)
- [Terminal & Shell](#terminal--shell)
- [Applications](#applications)
- [System Integration](#system-integration)

## Core Bindings

### Basic Operations
| Key | Command | Description |
|-----|---------|-------------|
| `C-x k` | kill-current-buffer | Kill current buffer |
| `C-x C-c` | bv--delete-frame-or-kill-emacs | Smart quit |
| `C-c r` | recentf-open-files | Open recent files |
| `M-:` | pp-eval-expression | Evaluate expression with pretty print |

### Help System
| Key | Command | Description |
|-----|---------|-------------|
| `C-h C-h` | bv-help-transient | Main help menu |
| `C-h .` | bv-describe-symbol-at-point | Describe symbol at point |
| `C-h D` | devdocs-lookup | DevDocs documentation |
| `M-p` | bv-quick-help | Quick help |

## Window & Frame Management

### Frame Operations
| Key | Command | Description |
|-----|---------|-------------|
| `M-n` | new-frame | Create new frame |
| `M-` | other-frame | Switch to other frame |
| `M-RET` | toggle-frame-maximized | Maximize/restore frame |

### Window Operations
| Key | Command | Description |
|-----|---------|-------------|
| `M-o` | ace-window | Switch windows with labels |

## File & Buffer Operations

### Buffer Management
| Key | Command | Description |
|-----|---------|-------------|
| `C-x b` | consult-buffer | Enhanced buffer switching |
| `C-x 4 b` | consult-buffer-other-window | Buffer in other window |
| `C-x 5 b` | consult-buffer-other-frame | Buffer in other frame |
| `C-x t b` | consult-buffer-other-tab | Buffer in other tab |
| `C-x r b` | consult-bookmark | Select bookmark |
| `C-x p b` | consult-project-buffer | Project buffer |

#### Consult Buffer (Minibuffer)
| Key | Command | Description |
|-----|---------|-------------|
| `TAB` | bv-consult-narrow-cycle-forward | Cycle source narrowing forward |
| `S-TAB` | bv-consult-narrow-cycle-backward | Cycle source narrowing backward |
| `?` | consult-narrow-help | Show narrowing keys and sources |
| `<key> SPC` | consult-narrow (via Consult) | Narrow to a source (e.g. `b`, `f`, `m`, `p`, `.`) |
| `DEL` (empty input) | consult-narrow | Widen back to all sources |

### File Operations
| Key | Command | Description |
|-----|---------|-------------|
| `s-d` | dired-jump | Jump to dired |

### Mouse Scrolling
Mouse wheel scrolling works in GUI by default. In `emacs -nw`, mouse support is
enabled automatically when available (via `xterm-mouse-mode` and
`mouse-wheel-mode`).

## Search & Navigation

### Enhanced Search
| Key | Command | Description |
|-----|---------|-------------|
| `C-s` | consult-line | Enhanced line search |
| `C-M-s` | bv-consult-ripgrep-or-line | Ripgrep or line search |
| `C-S-s` | bv-consult-line-symbol-at-point | Search symbol at point |

### Navigation
| Key | Command | Description |
|-----|---------|-------------|
| `M-g g` | consult-goto-line | Go to line |
| `M-g M-g` | consult-goto-line | Go to line (alternate) |
| `M-g o` | consult-outline | Navigate outline |
| `M-g m` | consult-mark | Go to mark |
| `M-g k` | consult-global-mark | Go to global mark |
| `M-g i` | consult-imenu | Navigate imenu |
| `M-g I` | consult-imenu-multi | Multi-buffer imenu |
| `M-g e` | consult-compile-error | Go to compile error |
| `M-g f` | consult-flymake | Go to flymake error |

### History
| Key | Command | Description |
|-----|---------|-------------|
| `M-y` | consult-yank-pop | Enhanced yank-pop |

## Completion System

### Vertico
| Key | Command | Description |
|-----|---------|-------------|
| `s-s` | vertico-repeat | Repeat last command |
| `M-R` | vertico-repeat | Repeat last command |
| `C-c C-r` | vertico-repeat-select | Select from repeat history |
| `C-M-n` | bv-vertico-next-from-outside | Next completion outside minibuffer |
| `C-M-p` | bv-vertico-previous-from-outside | Previous completion outside minibuffer |

#### Vertico Minibuffer
| Key | Command | Description |
|-----|---------|-------------|
| `C-j` | vertico-directory-enter | Enter directory |
| `RET` | vertico-directory-enter | Enter directory |
| `DEL` | vertico-directory-delete-char | Delete character |
| `C-w` | bv-vertico-kill-region-dwim | Smart kill region |
| `M-DEL` | vertico-directory-delete-word | Delete word |
| `C-l` | vertico-directory-up | Go up directory |
| `/` | bv-vertico-smart-slash | Smart slash |
| `S-SPC` | bv-vertico-restrict-to-matches | Restrict to matches |
| `M-q` | vertico-quick-jump | Quick jump |
| `C-M-n` | vertico-next-group | Next group |
| `C-M-p` | vertico-previous-group | Previous group |

#### Vertico Multiform
| Key | Command | Description |
|-----|---------|-------------|
| `M-V` | vertico-multiform-vertical | Vertical layout |
| `M-G` | vertico-multiform-grid | Grid layout |
| `M-B` | vertico-multiform-buffer | Buffer layout |

### Marginalia
| Key | Command | Description |
|-----|---------|-------------|
| `M-A` | marginalia-cycle | Cycle annotations |
| `M-T` | bv-marginalia-toggle | Toggle marginalia |

### Templates
| Key | Command | Description |
|-----|---------|-------------|
| `M-+` | tempel-insert | Insert template |
| `M-*` | tempel-expand | Expand template |

#### Tempel Map
| Key | Command | Description |
|-----|---------|-------------|
| `TAB` | tempel-next | Next field |
| `S-TAB` | tempel-previous | Previous field |
| `C-g` | tempel-abort | Abort template |
| `RET` | tempel-done | Complete template |

### Cape (Completion at Point)
| Key | Command | Description |
|-----|---------|-------------|
| `C-c p` | bv-cape-map | Cape completion map |
| `M-p` | bv-cape-map | Cape completion map |

## Project Management

### Project Operations
| Key | Command | Description |
|-----|---------|-------------|
| `s-p` | project-prefix-map | Project commands |

## Version Control

### Git Integration
Available through Transient menus and standard Git commands.

## Org Mode

### Org Command Center (`C-c a o`)
| Key | Command | Description |
|-----|---------|-------------|
| `C-c a o a` | org-agenda ("d") | Dashboard (Today/Next/Waiting/Inbox) |
| `C-c a o r` | org-agenda ("R") | Weekly review dashboard |
| `C-c a o w` | bv-org-weekly-review-create | Create/open weekly review entry |
| `C-c a o c` | org-capture | Capture menu |
| `C-c a o q` | bv-org-agenda-quick-task | Quick task capture (template "q") |
| `C-c a o v` | bv-org-agenda-calendar | Dashboard + calendar split |
| `C-c a o f` | consult-org-heading | Jump to heading across Org files |
| `C-c a o A` | consult-org-agenda | Search agenda items |
| `C-c a o s` | bv-org-search | Search `~/org` (ripgrep) |
| `C-c a o g` | (open goals file) | Open `~/org/goals.org` |
| `C-c a o t` | bv-org-timer-map | Timer submap |

### Org Timer (`C-c a o t`)
| Key | Command | Description |
|-----|---------|-------------|
| `s` | org-timer-start | Start timer |
| `p` | org-timer-pause-or-continue | Pause/continue timer |
| `t` | org-timer-set-timer | Set timer |
| `q` | org-timer-stop | Stop timer |

## Org-roam

### Main Org-roam Map (`C-c n`)
| Key | Command | Description |
|-----|---------|-------------|
| `f` | org-roam-node-find | Find node |
| `i` | org-roam-node-insert | Insert node |
| `I` | bv-org-roam-node-insert-immediate | Insert node immediately |
| `c` | bv-org-roam-capture-slip | Capture slip |
| `C` | bv-org-roam-quick-capture | Quick capture |
| `s` | bv-org-roam-capture-slip | Capture slip (alternate) |
| `r` | bv-org-roam-random | Random node |
| `b` | org-roam-buffer-toggle | Toggle buffer |
| `R` | bv-org-roam-add-reference | Add reference |
| `g` | bv-org-roam-find-by-tag | Find by tag |
| `/` | consult-org-roam-search | Search content |
| `l` | consult-org-roam-backlinks | Show backlinks |
| `L` | consult-org-roam-forward-links | Show forward links |
| `F` | consult-org-roam-file-find | Find files |

### Org Mode Context
| Key | Command | Description |
|-----|---------|-------------|
| `C-c n i` | org-roam-node-insert | Insert node in org-mode |

## Development

### LSP and Development Tools
| Key | Command | Description |
|-----|---------|-------------|
| `C-c a l` | eglot | Start LSP |
| `C-c a L` | eglot-shutdown | Stop LSP |
| `C-c a f` | flymake-mode | Toggle flymake |
| `C-c a F` | flymake-show-project-diagnostics | Show diagnostics |

#### Eglot (in eglot-managed buffers)
| Key | Command | Description |
|-----|---------|-------------|
| `C-c l d` | bv-eglot-peek-definition | Peek definition (Xref/Consult UI) |
| `C-c l D` | bv-eglot-peek-definition-other-window | Peek definition (other window) |
| `C-c l R` | bv-eglot-peek-references | Peek references (Xref/Consult UI) |
| `C-c l e` | bv-eglot-peek-diagnostics | Peek diagnostics (Consult/Flymake UI) |
| `C-c l s` | consult-eglot-symbols | Workspace symbols |
| `C-c l h` | bv-eglot-doc-buffer | Full docs at point |
| `C-c l r` | eglot-rename | Rename symbol |
| `C-c l a` | eglot-code-actions | Code actions |
| `C-c l f` | eglot-format | Format region |
| `C-c l F` | eglot-format-buffer | Format buffer |

#### Flymake (fix loop)
| Key | Command | Description |
|-----|---------|-------------|
| `M-n` | bv-flymake-goto-next-error | Next diagnostic (and show at point) |
| `M-p` | bv-flymake-goto-prev-error | Prev diagnostic (and show at point) |
| `C-c ! n` | bv-flymake-goto-next-error | Next diagnostic |
| `C-c ! p` | bv-flymake-goto-prev-error | Prev diagnostic |
| `C-c ! d` | bv-flymake-show-at-point | Show diagnostic at point |
| `C-c ! q` | bv-flymake-quickfix | Quick diagnostics UI |
| `C-c ! b` | flymake-show-buffer-diagnostics | Buffer diagnostics list |
| `C-c ! P` | flymake-show-project-diagnostics | Project diagnostics list |

### Debugging (Dape)
| Key | Command | Description |
|-----|---------|-------------|
| `C-c a d d` | dape | Start debugger |
| `C-c a d b` | dape-breakpoint-toggle | Toggle breakpoint |
| `C-c a d c` | dape-continue | Continue |
| `C-c a d n` | dape-next | Next line |
| `C-c a d s` | dape-step-in | Step in |
| `C-c a d o` | dape-step-out | Step out |
| `C-c a d r` | dape-restart | Restart |
| `C-c a d q` | dape-quit | Quit debugger |

### Elisp Development
| Key | Command | Description |
|-----|---------|-------------|
| `C-c a i` | ielm | Emacs Lisp REPL |

### Geiser (Scheme)
| Key | Command | Description |
|-----|---------|-------------|
| `C-c C-b` | bv-geiser-eval-buffer | Evaluate buffer |
| `C-c C-z` | bv-geiser-repl-here | Start REPL |

## Language-specific

### Ruby Development
| Key | Command | Description |
|-----|---------|-------------|
| `C-c C-z` | bv-ruby-console | Ruby console |
| `C-c C-Z` | bv-ruby-rails-console | Rails console |
| `C-c C-r` | bv-ruby-send-region | Send region |
| `C-c C-b` | bv-ruby-send-buffer | Send buffer |
| `C-c C-x` | bv-ruby-send-block | Send block |
| `C-c t f` | bv-ruby-test-file | Test file |
| `C-c t t` | bv-ruby-test-at-point | Test at point |
| `C-c t r` | bv-ruby-rake-test | Rake test |
| `C-c t s` | bv-ruby-rspec | RSpec |
| `C-c C-f` | bv-ruby-format-buffer | Format buffer |
| `C-c C-d` | bv-ruby-ri-at-point | Documentation at point |
| `C-c C-h` | bv-ruby-ri | Ruby documentation |

### R Development
| Key | Command | Description |
|-----|---------|-------------|
| `C-c C-z` | bv-r-start-repl | Start R REPL |
| `C-c C-r` | bv-r-eval-region | Evaluate region |
| `C-c C-b` | bv-r-eval-buffer | Evaluate buffer |
| `C-c C-f` | bv-r-eval-function | Evaluate function |
| `C-c C-l` | bv-r-eval-line | Evaluate line |
| `C-c C-F` | bv-r-format-buffer | Format buffer |
| `C-c C-h` | bv-r-help | R help |

## Terminal & Shell

### Terminal Operations
| Key | Command | Description |
|-----|---------|-------------|
| `s-t` | bv-eat | Terminal (EAT) |
| `s-e` | eshell | Eshell |
| `s-S` | bv-shell | Shell |
| `C-c a e` | bv-eat | Terminal (app map) |
| `C-c a E p` | bv-eat-project | Project terminal (EAT) |
| `C-c a E s` | eshell | Eshell (terminal map) |
| `C-c a E P` | bv-eshell-project | Project eshell |
| `C-c a E t` | bv-shell | Shell (terminal map) |
| `C-c a E T` | bv-shell-project | Project shell |

### Shell Mode
| Key | Command | Description |
|-----|---------|-------------|
| `M-r` | consult-history | Search shell history |

## Applications

### Application Map (`C-c a`)
| Key | Command | Description |
|-----|---------|-------------|
| `c` | bv-calendar | Calendar |
| `C` | world-clock | World clock |
| `d` | bv-debug-map | Debugger map (Dape) |
| `E` | bv-terminal-map | Terminal/shell map |
| `h` | devdocs-lookup | DevDocs lookup |
| `H` | devdocs-search | DevDocs search |
| `j` | org-roam-dailies-goto-today | Today's daily note |
| `m` | bv-mpv-map | MPV controls |
| `o` | bv-org-map | Org command center |
| `p` | project-switch-project | Switch project |
| `P` | bv-project-org-capture | Project org capture |
| `q` | org-ql-search | Org QL search |
| `Q` | org-ql-view | Org QL view |
| `s` | flyspell-mode | Spell check |
| `S` | ispell-word | Check word |
| `t` | tempel-insert | Insert template |
| `T` | tempel-expand | Expand template |
| `u` | browse-url | Browse URL |
| `U` | browse-url-at-point | Browse URL at point |
| `x` | re-builder | Regular expression builder |
| `*` | calc | Calculator |
| `=` | quick-calc | Quick calculator |
| `+` | bv-calc-eval | Evaluate expression |

### Remote Operations (`C-c a R`)
| Key | Command | Description |
|-----|---------|-------------|
| `f` | bv-tramp-find-file | Remote file |
| `d` | bv-tramp-dired | Remote directory |
| `s` | bv-tramp-shell | Remote shell |
| `e` | bv-tramp-eshell | Remote eshell |
| `r` | bv-tramp-sudo-find-file | Sudo file access |
| `c` | bv-tramp-cleanup | Cleanup connections |
| `C` | bv-tramp-cleanup-host | Cleanup host |

### Calculator
| Key | Command | Description |
|-----|---------|-------------|
| `C-x *` | calc-dispatch | Calculator dispatch |

### Media
| Key | Command | Description |
|-----|---------|-------------|
| `C-x w` | elfeed | RSS reader |

#### MPV Controls
| Key | Command | Description |
|-----|---------|-------------|
| `q` | quit-window | Quit |
| `d` | emms-playlist-mode-kill-track | Delete track |
| `c` | emms-playlist-clear | Clear playlist |

## System Integration

### Audio Controls
| Key | Command | Description |
|-----|---------|-------------|
| `XF86AudioRaiseVolume` | bv-audio-increase-volume | Increase volume |
| `XF86AudioLowerVolume` | bv-audio-decrease-volume | Decrease volume |
| `XF86AudioMute` | bv-audio-toggle-mute | Toggle mute |

### Weather
| Key | Command | Description |
|-----|---------|-------------|
| `C-c W` | bv-weather-show | Show weather |

### Perspectives (Workspaces)
| Key | Command | Description |
|-----|---------|-------------|
| `C-x P` | persp-mode-prefix-key | Perspective prefix |

## Transient Menus

### Main Transient Commands
| Key | Command | Description |
|-----|---------|-------------|
| `C-c g` | bv-gptel-transient | AI assistant |
| `C-c G` | bv-guix-transient | Guix operations |
| `C-c S` | bv-geiser-transient | Scheme development |
| `C-c m` | bv-emms-transient | Media player |
| `C-c e` | bv-ellama-transient | Local AI |
| `C-c b` | bv-ebdb-transient-menu | Contact database |
| `C-c N` | bv-ednc-transient | Notifications |
| `C-x q` | bv-power-transient | Power management |
| `C-c v` | bv-audio-transient | Audio controls |
| `C-c P` | bv-webpaste-transient | Web paste |
| `C-c y` | bv-ytdl-transient | YouTube downloader |

### Citation Management
| Key | Command | Description |
|-----|---------|-------------|
| `C-c c` | bv-citation-map | Citation operations |

## Specialized Applications

### EBDB (Contact Database)
| Key | Command | Description |
|-----|---------|-------------|
| `q` | quit-window | Quit |
| `/` | bv-ebdb-search-name | Search by name |
| `@` | bv-ebdb-search-email | Search by email |

### YouTube Downloader
| Key | Command | Description |
|-----|---------|-------------|
| `q` | quit-window | Quit |
| `a` | ytdl-download | Download |
| `d` | ytdl-download-delete | Delete download |

---

## Notes

- **Prefix notation**: `C-` = Ctrl, `M-` = Alt/Meta, `s-` = Super/Windows key
- **Application map**: `C-c a` provides access to most application-specific commands
- **Transient menus**: Self-documenting popup menus for complex operations
- **Mode-specific**: Some bindings only work in specific modes (indicated in descriptions)
- **Smart bindings**: Many commands adapt behavior based on context

For mode-specific bindings not covered here, use `C-h m` to see current mode's keybindings.
