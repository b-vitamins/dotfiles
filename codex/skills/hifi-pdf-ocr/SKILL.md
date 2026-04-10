---
name: hifi-pdf-ocr
description: High-fidelity manual transcription from PDFs or rendered page images into LaTeX, including mandatory 600 DPI page rendering, logical range planning, and strict page-by-page append workflows. Use when a user asks to transcribe books/papers, preserve equations/figures/captions faithfully, coordinate multiple agents on non-overlapping page ranges, or compile and validate transcription outputs.
---

# HiFi PDF OCR

Execute a deterministic pipeline for manual transcription or verification-repair with strict quality rules, explicit manager-side gates, and multi-agent scaling.

## Inputs

- Source PDF path.
- Output workspace path.
- Target output format (usually LaTeX `.tex`).
- Logical grouping plan (chapters/sections or explicit page ranges).
- Optional existing transcription files to audit/fix instead of transcribing from scratch.

## Required Workflow

1. Inspect current workspace state and classify the job as fresh transcription, verification-repair, or mixed.
2. Render page images first.
3. Build and validate a logical grouping plan.
4. Dispatch work through native Codex multi-agent orchestration with explicit file-safety and lifecycle controls.
5. Transcribe or verify with a strict one-page-read, one-page-append loop.
6. Verify exact `% Page` coverage before cleanup.
7. Remove `% Page` markers and stitch page-break interruptions.
8. Compile and repair only transcription/LaTeX syntax errors.
9. Run an independent image-based audit/fix pass for all touched pages when fidelity is high stakes.
10. Run a fresh acceptance sample before final signoff.
11. Report structural completeness separately from fidelity completeness and list unresolved diagram TODOs and diagram gaps.

## Step 0: Inspect Current State

Before any rendering or dispatch:

- Determine whether the scope is:
  - missing text that must be transcribed from scratch,
  - existing text that must be audited/fixed against images,
  - or a mix of both.
- If existing transcription files are present, do not treat them as authoritative for touched pages until re-verified from the rendered page images.
- Distinguish `structurally complete`, `compiled`, `text-audited`, `diagram-complete`, and `fully fidelity-complete` in all progress and final reports. Never collapse these into a single “done” claim.
- Inventory requested output files and existing rendered pages before spawning agents.

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

Before dispatch, validate that:

- rendered pages exist for the requested scope,
- planned ranges match the rendered inventory,
- every planned row has an explicit `target_tex`,
- and any repeated `target_tex` rows are intended to remain on the same agent unless shard files are used.

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
- If the user requires a specific sub-agent model or reasoning effort, pass it explicitly when spawning. If you cannot honor it exactly, stop and report the blocker instead of silently downgrading.

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
- `assign-groups.py` treats rows that share the same `target_tex` as a single ownership bundle. To parallelize one logical target, use shard files instead of shared direct writes.

Manager-side orchestration rules:

- Cap live workers by both platform concurrency limits and the number of disjoint write targets.
- Close completed or errored agents promptly before spawning replacements.
- If a worker stalls, hits quota, or returns partial progress, reassign only the unfinished pages and prefer smaller continuation chunks.
- Maintain an explicit assignment/status ledger so the parent can tell which pages are done, verified, fixed, or pending.

## Step 4: Transcribe or Verify with Strict Page Discipline

For each assigned page `p`:

1. Open exactly one image page (`pages/page-p.png`).
2. Manually transcribe or verify that page content from the image.
3. Append or patch only that page in the target `.tex`.
4. Insert `% Page p` immediately before appended content.
5. Continue to page `p+1`.

Hard transcription constraints:

- Do not use OCR or text extraction.
- Do not batch-read or batch-write multiple pages.
- Keep modern LaTeX syntax (`\(...\)`, `\[...\]`, `\eqref{...}`).
- Preserve meaning and wording faithfully.
- Preserve continuity across page breaks (finish cut sentences correctly).
- If auditing existing text, compare the page image against the current page-local text and fix discrepancies immediately. Do not trust pre-existing transcription for touched pages until the image check is complete.

Figures and diagrams:

- Use placeholders only for artwork or diagrams that are not being inserted as actual assets.
- Transcribe captions faithfully.
- Add explicit TODO comments for later redraw, e.g. `% TODO(diagrams): Recreate Fig. ...`.
- If a rendered page clearly shows numbered figure content but the deliverable still uses only a placeholder, record that gap in `plans/diagram-gaps.csv`. Such a page may still become `text-audited`, but it is not `diagram-complete`.
- Distinguish carefully between:
  - numbered figures with captions,
  - decorative chapter-opening artwork,
  - boxed algorithms,
  - and native LaTeX tables/matrices.
- Do not misclassify typeset tables, matrices, or algorithms as diagram TODOs just because they appear inside a `figure` or box on the page.

Equation labeling and references:

- Add meaningful labels for referenced equations.
- Use `\eqref{...}` for in-text equation references.
- Use hyphen-only labels (never underscore).
- Keep consistent prefixes, e.g., `eq-ch10-...`, `fig-ch10-...`.

## Step 5: Verify Exact Page Coverage Before Cleanup

Before removing markers, verify that each touched file has the exact expected `% Page` markers and no extras, and persist the result under `plans/`:

```bash
python3 scripts/verify-page-markers.py --csv plans/groups.csv --report plans/page-marker-verification.json
```

or for a single assignment:

```bash
python3 scripts/verify-page-markers.py --file transcriptions/chapter-10.tex --start 267 --end 300 --report plans/page-marker-verification.json
```

Use this gate before cleanup and before declaring an agent assignment complete. Do not delete `% Page` markers until the report file exists and says `ok: true`.

## Step 6: Cleanup Page Markers and Stitch Boundaries

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

## Step 7: Compile and Repair

After completing the assigned range:

```bash
pdflatex -interaction=nonstopmode -halt-on-error <target.tex>
pdflatex -interaction=nonstopmode -halt-on-error <target.tex>
```

Compile from the directory containing the target file, or otherwise preserve the target file's relative path context for graphics and includes.

Repair only what is required to compile, without changing meaning/content.

Common allowed fixes:

- Unbalanced braces.
- Broken LaTeX delimiters.
- Label/reference typos.
- Environment closure issues.

## Step 8: Independent Audit/Fix Pass

When the job is high stakes, long-running, or involves many sub-agents, a compile-only finish is not sufficient. After the initial transcription pass:

- run a second image-based audit/fix pass over every touched page,
- use non-overlapping assignments,
- record status per page in `plans/page-audit.csv` (`verified`, `fixed`, `pending`, or `blocked`),
- record unresolved numbered-figure/diagram gaps in `plans/diagram-gaps.csv`,
- and only declare `text-audited` when the page-audit ledger reaches full requested coverage with no `pending` rows.

This audit pass may reuse the same strict one-page loop, but it is a distinct pass with a distinct completion claim.

Use `assets/page-audit-template.csv` when creating the page audit ledger and `assets/diagram-gap-template.csv` for unresolved figure-content gaps.

## Step 9: Independent Acceptance Sample

Before final signoff, run a fresh acceptance sample that is independent from the main audit ledger owners:

```bash
python3 scripts/select-acceptance-pages.py --audit-csv plans/page-audit.csv --out plans/acceptance-sample.csv
```

Then:

- spawn fresh read-only agents or perform a fresh parent-side check on the sampled pages,
- include both `verified` and `fixed` pages in the sample,
- and record the sampled pages and findings in the final report.

Any defect found in the acceptance sample sends the job back to repair plus a new acceptance sample.

## Step 10: Final Report

Report:

- Completed pages and output files.
- Pages transcribed vs pages independently audited/fixed.
- Whether compilation succeeded.
- Whether the work is structurally complete, compiled, text-audited, diagram-complete, and fully fidelity-complete.
- Location of the durable marker-verification artifact.
- Location of the acceptance-sample artifact.
- Remaining diagram TODO items.
- Remaining diagram gaps from `plans/diagram-gaps.csv`.
- Any unresolved ambiguities that need user confirmation.

Before final handoff, run the checklist in `references/transcription-quality-gate.md`.

Definitions:

- `structurally complete`: every requested output target exists for the planned scope.
- `compiled`: the correct compile target has passed the required compile checks.
- `text-audited`: every requested page appears in `plans/page-audit.csv` with no `pending` rows.
- `diagram-complete`: `plans/diagram-gaps.csv` is empty or absent because there are no unresolved numbered-figure content gaps.
- `fully fidelity-complete`: `compiled`, `text-audited`, and `diagram-complete` are all true, `plans/page-marker-verification.json` exists with `ok: true`, and the independent acceptance sample found no defects.

## Scripts

- `scripts/render-pages.sh`: Render PDF pages to 600 DPI PNG using `guix shell poppler -- pdftoppm`.
- `scripts/split-ranges.py`: Split contiguous page ranges across agents.
- `scripts/assign-groups.py`: Balance chapter/group workloads across agents from a CSV plan.
- `scripts/verify-page-markers.py`: Verify exact page coverage from `% Page <n>` markers before cleanup.
- `scripts/cleanup-page-markers.py`: Remove `% Page` markers and stitch likely page-break interruptions.

## Assets

- `assets/group-plan-template.csv`: Starter schema for logical grouping.
- `assets/page-audit-template.csv`: Starter schema for page-level audit status.
- `assets/agent-task-prompt-template.md`: Reusable strict prompt template for each assigned range.
- `assets/master-dispatch-prompt-template.md`: Parent prompt template for spawn/wait/consolidate orchestration.
