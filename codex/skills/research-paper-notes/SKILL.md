---
name: research-paper-notes
description: Strict TeX-first forensic reader-notes for a single paper when complete TeX source is available locally or can be fetched from arXiv. Use for deep, source-anchored analysis of one ML, statistics, optimization, physics, or engineering paper when the user wants equations, assumptions, derivations, experiments, figures, quotes, and reproducibility details. Do not use for PDF-only or OCR-only inputs, partial source trees, casual summaries, broad literature reviews, or multi-paper synthesis.
---

# Research Paper Notes

Produce reader-notes, not a summary. Preserve the facts, assumptions, equations,
procedures, claims, limitations, and exact wording that a careful expert would
keep after reading the full paper from source.

The non-negotiable foundation is:

**Read the complete paper from TeX source, line by line, start to finish.**

If that cannot be done, stop. Do not improvise from the PDF, OCR, abstract page,
or a partial source tree. Write a failure manifest instead.

Read these references when needed:
- `references/workflow.md` for the staged process.
- `references/strict-gating.md` for fail-fast rules.
- `references/output-contract.md` for the exact deliverable.
- `references/audit-checklist.md` before final delivery.
- `references/arxiv-ingestion.md` when the input is an arXiv URL or identifier.

## Hard constraints

1. Require TeX source. Accept local source trees or arXiv source bundles only.
2. Treat TeX as authoritative. Use the PDF only as a companion for layout,
   figure, or page-reference cross-checks after the source read is complete.
3. Build and read `combined-source.tex` in contiguous chunk order from the first
   chunk to the last chunk. Do not skip ahead.
4. Record progress in `manifests/readthrough-log.json`. If the log is incomplete,
   do not draft notes.
5. Fail fast when the source gate fails. Partial or best-effort notes are out of
   scope for this skill.

## Evidence and missingness rules

Use these tags exactly:
- `[P]` for paper-stated content.
- `[D]` for values directly derived from reported paper quantities.
- `[I]` for analyst inference.

In audited sections, every substantive statement must begin with one or more
evidence tags followed by `Anchor:` or `Basis:`. Canonical forms are:
- `[P] Anchor: <section/eq/theorem/table/file:line>; <paper-stated statement>`
- `[D] Basis: <reported quantities and arithmetic>; <derived statement>`
- `[I] Basis: <specific paper evidence>; <inference>`
- `[P][D] Anchor: <paper location plus arithmetic basis>; <mixed reported-and-derived statement>`

Use these missingness labels exactly:
- `N/A` for not applicable.
- `Not reported` for relevant but absent.
- `Unclear` for ambiguous, conflicting, or unreadable.

Do not use `N/R`.

Every substantive statement in sections 1, 5, 8, 10, 11, 12, 16, 17, and 18 of
the final notes must carry either an evidence tag with an anchor or an explicit
`[I]` inference label.

Do not force the paper into a single subfield label. Represent it with compact,
multi-label coordinates instead: purpose, problem form, learning signal,
mechanism, and evidence regime.

## Workflow

### 1. Stage the workspace

Create a workspace under `paper-workspaces/<slug>/` with:
- `artifacts/`
- `extracted/`
- `notes/`
- `manifests/`

Run:

```bash
python3 scripts/prepare_paper_workspace.py --input "<arxiv-id-or-local-source>" --output-dir paper-workspaces
```

Inspect:
- `manifests/workspace.json`
- `manifests/source-gate-report.json`
- `extracted/readthrough-manifest.json`

If `strict_gate_passed` is false, stop.

### 2. Complete the sequential readthrough

Read `extracted/combined-source.tex` in the exact chunk order listed in
`extracted/readthrough-manifest.json`.

After each chunk, mark completion with a short factual summary:

```bash
python3 scripts/update_readthrough_log.py mark --log paper-workspaces/<slug>/manifests/readthrough-log.json --chunk-id C001 --summary "Defines setup, objective, and first notation block."
```

The script enforces sequential completion. Do not mark later chunks before
earlier ones are read.

When done, verify:

```bash
python3 scripts/update_readthrough_log.py status --log paper-workspaces/<slug>/manifests/readthrough-log.json --require-complete
```

If the log is incomplete, stop.

### 3. Build a fact ledger before prose

Create `notes/fact-ledger.md` from `assets/fact-ledger-template.md`.

Capture atomic facts with anchors for:
- external IDs and source provenance
- problem and setup
- atlas-style coordinates and claim surfaces
- notation and constants
- assumptions and where used
- method steps and equations
- formal and informal results
- experiments, metrics, baselines, compute, and seeds
- key numbers
- typed prior-work relations and stable artifact names
- figure and table takeaways
- exact quotes worth preserving
- contradictions and missing details

Do not draft the final notes before the ledger exists.

### 4. Draft the final notes

Use `assets/reader-notes-template.md` and
`references/output-contract.md`.

Keep extractive sections tightly source-grounded. Keep synthesis sections
paper-specific. Use theorem mode for formal results and derivation mode for
informal results; a paper may require both.

When the evidence supports it, make the output atlas-friendly:
- record external IDs explicitly
- assign multi-axis coordinates rather than one field label
- summarize the paper's claim surfaces compactly
- express prior-work links with typed relation labels
- use stable artifact names and versions where possible

### 5. Validate before delivery

Run:

```bash
python3 scripts/validate_reader_notes.py --notes paper-workspaces/<slug>/notes/reader-notes.md --workspace paper-workspaces/<slug>/manifests/workspace.json --readthrough-log paper-workspaces/<slug>/manifests/readthrough-log.json --report-out paper-workspaces/<slug>/manifests/validation-report.json --export-json paper-workspaces/<slug>/notes/reader-notes.json
```

If validation fails, fix the notes and rerun it.

## Helper scripts

- `scripts/prepare_paper_workspace.py`
- `scripts/fetch_arxiv_bundle.py`
- `scripts/index_tex_project.py`
- `scripts/update_readthrough_log.py`
- `scripts/validate_reader_notes.py`

## Conflict rules

- If a symbol is reused, preserve both readings and rename them locally as
  `symbol@context` in the notation table.
- If the abstract or introduction overclaims relative to formal results or
  tables, defer to the stronger evidence and flag the drift.
- If a paper spans multiple areas, keep multiple coordinates. Do not collapse it
  to a single bucket just for neatness.
- In prior-work comparison, prefer typed relation labels such as
  `inherits_from`, `compares_to`, `critiques`, `re-benchmarks`,
  `shares_artifact`, `transfers_to`, or `scales_from` when the evidence justifies
  them.
- If appendix material is necessary for a main claim, say so explicitly.
- Do not fabricate artifact links, commit hashes, confidence intervals,
  statistical tests, or source coverage.

## Style

Write compact, exact, technical notes. Avoid hype, filler, and generic advice.
Refusal on incomplete source is the intended behavior for this skill.
