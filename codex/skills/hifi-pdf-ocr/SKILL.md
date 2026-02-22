---
name: hifi-pdf-ocr
description: High-fidelity manual transcription from PDFs or rendered page images into LaTeX, including mandatory 600 DPI page rendering, logical range planning, and strict page-by-page append workflows. Use when a user asks to transcribe books/papers, preserve equations/figures/captions faithfully, coordinate multiple agents on non-overlapping page ranges, or compile and validate transcription outputs.
---

# HiFi PDF OCR

Execute a deterministic pipeline for manual transcription with strict quality rules and multi-agent scaling.

## Inputs

- Source PDF path.
- Output workspace path.
- Target output format (usually LaTeX `.tex`).
- Logical grouping plan (chapters/sections or explicit page ranges).

## Required Workflow

1. Render page images first.
2. Build a logical grouping plan.
3. Dispatch work through native Codex multi-agent orchestration.
4. Transcribe with a strict one-page-read, one-page-append loop.
5. Remove `% Page` markers and stitch page-break interruptions.
6. Compile and repair only transcription/LaTeX syntax errors.
7. Report completion and unresolved diagram TODOs.

## Step 1: Render Images (Always First)

Always render images before any transcription work. Use `pdftoppm` via Guix Poppler.

Use:

```bash
scripts/render-pages.sh --pdf /path/to/book.pdf --outdir pages --dpi 600
```

This script runs:

```bash
guix shell poppler -- pdftoppm -r 600 -png ...
```

Render constraints:

- Keep filenames as `page-<n>.png` (1-based, no gaps).
- Never start transcription before render finishes.
- Re-render with `--overwrite` only when user asks to replace existing images.

## Step 2: Map Logical Groups

Create a grouping file (`plans/groups.csv`) using `assets/group-plan-template.csv`.

When planning groups:

- Prefer chapter/section boundaries from the table of contents when available.
- If chapter boundaries are unknown, define temporary contiguous ranges and revise later.
- Keep ranges non-overlapping and collectively exhaustive for the requested scope.

Read `references/group-plan-format.md` when creating or validating this plan.
Read `references/codex-multi-agent-notes.md` before spawning sub-agents.

## Step 3: Dispatch Parallel Work (Native Multi-agent)

Codex multi-agent dispatch is handled by Codex, not by these scripts.

Use scripts only to prepare deterministic, non-overlapping assignments. Then instruct Codex to spawn one sub-agent per assignment and wait for all results.

Before dispatching:

- Ensure multi-agent is enabled (`/experimental` -> `Multi-agents`, then restart), or set:
```toml
[features]
multi_agent = true
```
- Assume sub-agents inherit sandbox policy and use non-interactive approvals.
- Avoid tasks requiring new approvals inside sub-agents unless policy already permits them.

Use one of these assignment modes:

- Group-aware balancing:
```bash
python3 scripts/assign-groups.py --groups plans/groups.csv --agents 4 --out plans/agent-assignments.csv
```
- Pure page-range splitting:
```bash
python3 scripts/split-ranges.py --start 267 --end 400 --agents 3 --out plans/range-splits.csv
```

Dispatch prompt pattern:

- Ask Codex explicitly:
  - Spawn one agent per row in the assignment file.
  - Give each agent its exact page range and target output file.
  - Wait for all agents.
  - Summarize status per agent.

Use `assets/master-dispatch-prompt-template.md` for this parent prompt and `assets/agent-task-prompt-template.md` for per-agent instructions.

Parallelization and file-safety rules:

- Never let two agents write to the same file at the same time.
- Prefer one target `.tex` per group/chapter.
- If splitting one chapter across agents, write shard files such as `chapter-10-p267-300.tex` and merge in page order after all shards pass checks.
- Include each agent's exact start/end pages in the assignment record.

## Step 4: Transcribe with Strict Page Discipline

For each assigned page `p`:

1. Open exactly one image page (`pages/page-p.png`).
2. Manually transcribe that page content.
3. Append only that page to the target `.tex`.
4. Insert `% Page p` immediately before appended content.
5. Continue to page `p+1`.

Hard transcription constraints:

- Do not use OCR or text extraction.
- Do not batch-read or batch-write multiple pages.
- Keep modern LaTeX syntax (`\(...\)`, `\[...\]`, `\eqref{...}`).
- Preserve meaning and wording faithfully.
- Preserve continuity across page breaks (finish cut sentences correctly).

Figures and diagrams:

- Use placeholders for diagram redraws.
- Transcribe captions faithfully.
- Add explicit TODO comments for later redraw, e.g. `% TODO(diagrams): Recreate Fig. ...`.

Equation labeling and references:

- Add meaningful labels for referenced equations.
- Use `\eqref{...}` for in-text equation references.
- Use hyphen-only labels (never underscore).
- Keep consistent prefixes, e.g., `eq-ch10-...`, `fig-ch10-...`.

## Step 5: Cleanup Page Markers and Stitch Boundaries

After assigned ranges are complete (or after merging shards), remove page markers and stitch likely page-break interruptions:

```bash
python3 scripts/cleanup-page-markers.py chapter-*.tex
```

Behavior:

- Removes `% Page <n>` lines.
- Stitches likely sentence/list flow interrupted by marker-only page breaks.
- Keeps section/environment boundaries intact with conservative heuristics.

Useful modes:

- Dry validation (fail if markers still exist):
```bash
python3 scripts/cleanup-page-markers.py --check chapter-*.tex
```
- Remove markers without stitching:
```bash
python3 scripts/cleanup-page-markers.py --no-stitch chapter-*.tex
```

## Step 6: Compile and Repair

After completing the assigned range:

```bash
pdflatex -interaction=nonstopmode -halt-on-error <target.tex>
pdflatex -interaction=nonstopmode -halt-on-error <target.tex>
```

Repair only what is required to compile, without changing meaning/content.

Common allowed fixes:

- Unbalanced braces.
- Broken LaTeX delimiters.
- Label/reference typos.
- Environment closure issues.

## Step 7: Final Report

Report:

- Completed pages and output files.
- Whether compilation succeeded.
- Remaining diagram TODO items.
- Any unresolved ambiguities that need user confirmation.

Before final handoff, run the checklist in `references/transcription-quality-gate.md`.

## Scripts

- `scripts/render-pages.sh`: Render PDF pages to 600 DPI PNG using `guix shell poppler -- pdftoppm`.
- `scripts/split-ranges.py`: Split contiguous page ranges across agents.
- `scripts/assign-groups.py`: Balance chapter/group workloads across agents from a CSV plan.
- `scripts/cleanup-page-markers.py`: Remove `% Page` markers and stitch likely page-break interruptions.

## Assets

- `assets/group-plan-template.csv`: Starter schema for logical grouping.
- `assets/agent-task-prompt-template.md`: Reusable strict prompt template for each assigned range.
- `assets/master-dispatch-prompt-template.md`: Parent prompt template for spawn/wait/consolidate orchestration.
