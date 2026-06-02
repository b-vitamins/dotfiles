# Org Basics (Barebones)

This configuration intentionally keeps Org tasks minimal:
- Default TODO keywords only (`TODO` / `DONE`)
- A single Org file under `~/org/main.org`
- Stable clock-in/out (doesn’t steal windows)

The Org setup points agenda and capture at `~/org/main.org`.

## Layout (`~/org`)

- `main.org`: agenda, capture targets, notes, and archived entries

## Keybindings

This repo uses an application prefix map:
- `C-c` → BV leader
- `C-c o` → Org (`bv-org-map`)

Org map (`C-c o …`):
- `a`: `org-agenda`
- `c`: `org-capture`
- `m`: open `~/org/main.org`
- `I`: clock in (`bv-org-clock-in`)
- `O`: clock out (`bv-org-clock-out`)

Clocking (in Org buffers):
- Clock in: `C-c C-x TAB` (same as `C-c C-x C-i`)
- Clock out: `C-c C-x C-o`

## Capture

Open capture with `C-c o c`, then:
- `t`: append a `* TODO …` entry to `main.org`
- `n`: append a note to `main.org`

## Customization

Customize via `M-x customize-group RET bv-org`:
- `bv-org-directory`
