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
- `i`: Inbox processor (fast refile loop)
- `p`: Focus (pick `NEXT` task → clock in → start timer)
- `P`: Paper pipeline (citar → reading project; optional org-roam note backlink)
- `m`: Metrics scoreboard (today/this-week, deep/writing/reading, streak)
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

Metrics scoreboard:
- `C-c a o m` shows a compact scoreboard with:
  - Total clocked time (today / this week)
  - Deep work time (tag `deep`)
  - Writing time (tag `writing`)
  - Reading time (tag `reading`)
  - Papers read (DONE “Read” tasks under tag `paper`)
  - A simple streak (days ≥ `bv-org-metrics-streak-minutes`)

## Agenda command center

Dashboard (`org-agenda` command `"d"`):
- Today (schedule/deadlines)
- In progress (`STARTED`)
- Next actions (`NEXT`)
- Habits (`STYLE="habit"` from `~/org/goals.org`)
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
- Or run the inbox processor: `C-c a o i` then press `r/d/c/a/s/q` to refile/done/cancel/archive/skip/quit.

Archiving:
- Use `org-archive-subtree` to archive completed subtrees into `~/org/archive/…` (datetree).

## Focus (Pomodoro)

`C-c a o p` starts a focus block:
- DWIM: if point is on a TODO heading, uses it; otherwise prompts for a `NEXT`/`STARTED` task.
- Clocks in and starts a countdown timer (`bv-org-focus-minutes`, default 45).
- When the timer finishes, auto clocks out (timer stop/done hooks).

## Paper pipeline (citar → reading)

`C-c a o P`:
- Pick a citation via citar
- Creates a `PROJ` in `~/org/reading.org` under *Queue* with `Read/Take notes/Write summary` tasks
- Adds tags `reading`/`writing` to make time-tracking visible in the scoreboard
- With `C-u`, also creates an org-roam note and links it from the reading entry

## Customization knobs

Customize via `M-x customize-group RET bv-org`:
- `bv-org-directory`
- `bv-org-agenda-exclude-dirs`
- `bv-org-clock-idle-minutes`
- `bv-org-clock-idle-check-interval`
- `bv-org-auto-promote-next-action`
