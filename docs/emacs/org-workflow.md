# Org Basics (Barebones)

This configuration intentionally keeps Org tasks minimal:
- Default TODO keywords only (`TODO` / `DONE`)
- A single inbox file under `~/org/inbox.org`
- Stable clock-in/out (doesn’t steal windows)
- Optional idle auto clock-out

The directory `~/org` (and the inbox file) is created automatically if missing.

## Layout (`~/org`)

- `inbox.org`: quick capture + lightweight task list

## Keybindings

This repo uses an application prefix map:
- `C-c a` → `bv-app-map`
- `C-c a o` → Org (`bv-org-map`)

Org map (`C-c a o …`):
- `a`: `org-agenda`
- `c`: `org-capture`
- `i`: open `~/org/inbox.org`
- `I`: clock in (`bv-org-clock-in`)
- `O`: clock out (`bv-org-clock-out`)

Clocking (in Org buffers):
- Clock in: `C-c C-x TAB` (same as `C-c C-x C-i`)
- Clock out: `C-c C-x C-o`

## Capture

Open capture with `C-c a o c`, then:
- `t`: create a `* TODO …` entry under `~/org/inbox.org`

## Customization

Customize via `M-x customize-group RET bv-org`:
- `bv-org-directory`
- `bv-org-agenda-exclude-dirs`
- `bv-org-clock-idle-minutes`
- `bv-org-clock-idle-check-interval`
