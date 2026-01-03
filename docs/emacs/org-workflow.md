# Org Workflow (PhD / Research)

This configuration ships an opinionated Org setup aimed at:
- Fast capture with automatic context
- One-task-at-a-time focus with clocking + idle clock-out
- A command-center agenda (Today + Next + Waiting + Deep work + Inbox)
- Weekly review + lightweight metrics (clocktables)

The hub is `~/org` (auto-created on startup).

## Layout (`~/org`)

Core files:
- `inbox.org`: GTD-style inbox + refile staging
- `projects.org`: long-lived areas/projects (Research/Thesis/Courses/…)
- `journal.org`: datetree journal entries
- `meetings.org`: structured meeting notes (datetree)
- `reading.org`: paper queue + reading notes
- `goals.org`: motivators, habits, and goal tracking
- `reviews.org`: weekly review entries + clocktable metrics

Archives:
- `~/org/archive/`: per-file datetree archives via `org-archive-subtree`

## Keybindings

This repo uses an application prefix map:
- `C-c a` → `bv-app-map`
- `C-c a o` → Org command center (`bv-org-map`)

Org command center (`C-c a o …`):
- `a`: Agenda dashboard (`org-agenda` → command `"d"`)
- `r`: Weekly review dashboard (`org-agenda` → command `"R"`)
- `w`: Create/open weekly review entry in `~/org/reviews.org` (and refresh clocktable)
- `c`: `org-capture`
- `q`: Capture “Quick task (today)” (template `"q"`)
- `v`: Dashboard + calendar split
- `f`: Jump to heading across Org files (`consult-org-heading`)
- `A`: Search agenda items (`consult-org-agenda`)
- `s`: Full-text search across `~/org` (`consult-ripgrep`)
- `g`: Open `~/org/goals.org`
- `t`: Org timer map

Org timer map (`C-c a o t …`):
- `s`: `org-timer-start`
- `p`: `org-timer-pause-or-continue`
- `t`: `org-timer-set-timer`
- `q`: `org-timer-stop`

## Capture 2.0 (templates)

Open capture with `C-c a o c`, then choose:
- `i` Inbox item → `inbox.org` / *Inbox*
- `q` Quick task (today) → `inbox.org` / *Inbox* (sets `NEXT` + `DEADLINE: %t`)
- `j` Journal → `journal.org` (datetree)
- `m` Meeting → `meetings.org` (datetree; Agenda/Notes/Decisions/Actions sections)
- `l` Link → `inbox.org` / *Links*
- `d` Idea → `inbox.org` / *Ideas*
- `r` Reading (paper → task + notes) → `reading.org` / *Queue*

Templates automatically attach context in a PROPERTIES drawer:
- `CREATED`: timestamp
- `PROJECT`: current project name (if available)
- `CLOCKED`: current clocked task (if any)
- `CONTEXT`: file:line (and `%a` capture link when available)

## Task System (states + “next action”)

TODO states:
- Active: `PROJ`, `TODO`, `NEXT`, `STARTED`, `WAITING`, `HOLD`, `SOMEDAY`
- Done: `DONE`, `CANCELLED`

Conventions:
- A `PROJ` should have exactly one `NEXT` (or one `STARTED`) item.
- A project with no `NEXT` is “stuck” and appears in the weekly review dashboard.

Automation:
- When you complete a task inside a `PROJ` subtree, another `TODO`/`STARTED` is promoted to `NEXT` if the project has no remaining `NEXT`.

## Clocking + gamification

Clocking is configured to be lightweight and persistent:
- Logs go to a `LOGBOOK` drawer
- Clock data persists across sessions
- Clock out happens automatically when a task is marked done
- Idle auto clock-out is enabled (default: 12 minutes)

Weekly review metrics:
- `C-c a o w` creates a new week in `~/org/reviews.org` with a `clocktable` for `:block lastweek`.

## Agenda command center

Dashboard (`org-agenda` command `"d"`):
- Today (schedule/deadlines)
- In progress (`STARTED`)
- Next actions (`NEXT`)
- Deep work (`deep` tag)
- Errands (`@errand` tag)
- Waiting / blocked (`WAITING`)
- Inbox (unprocessed tasks in `inbox.org`)

Weekly review (`org-agenda` command `"R"`):
- Past 2 weeks / Next 2 weeks
- Inbox
- Waiting
- Stuck projects (`PROJ` without `NEXT`/`STARTED`)

## Refile + archive rules

Processing the inbox:
- Use `org-refile` (`C-c C-w`) to move items into `projects.org` (and optionally `reading.org`/`goals.org`).

Archiving:
- Use `org-archive-subtree` to archive completed subtrees into `~/org/archive/…` (datetree).

## Customization knobs

Customize via `M-x customize-group RET bv-org`:
- `bv-org-directory`
- `bv-org-agenda-exclude-dirs`
- `bv-org-clock-idle-minutes`
- `bv-org-clock-idle-check-interval`
- `bv-org-auto-promote-next-action`
