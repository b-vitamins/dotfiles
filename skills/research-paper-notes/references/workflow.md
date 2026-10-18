# Workflow

## Stage 0: Ingest and stage

Run `scripts/prepare_paper_workspace.py`.

The script:
- normalizes an arXiv URL or identifier when needed
- fetches the source bundle and companion PDF for arXiv inputs
- extracts basic arXiv metadata when available
- stages local TeX inputs
- identifies the main TeX file
- builds `combined-source.tex`
- creates `readthrough-manifest.json`
- initializes or preserves `readthrough-log.json` when the chunk layout is unchanged
- scaffolds `notes/fact-ledger.md` and `notes/reader-notes.md` after a passing gate

Inspect first:
- `manifests/workspace.json`
- `manifests/source-gate-report.json`
- `extracted/combined-source.tex`
- `extracted/readthrough-manifest.json`
- `extracted/tex-outline.md`
- `notes/reader-notes.md`

If the source gate fails, stop.

## Stage 1: Sequential source readthrough

Read the combined source in chunk order.

Recommended discipline:
- keep `notes/fact-ledger.md` open while reading
- record the section, key claims, and anchors after each chunk
- mark each chunk complete in `readthrough-log.json`

Do not draft notes before the log is complete.

## Stage 2: Fact ledger

Build an atomic ledger for:
- setup
- notation
- assumptions
- method details
- formal and informal claims
- experiments and metrics
- figures and tables
- quotes
- contradictions and missing details

## Stage 3: Drafting

Use `assets/reader-notes-template.md`.

Keep extractive sections anchored. Keep synthesis sections paper-specific.
Separate paper-stated, derived, and inferred content.

## Stage 4: Validation

Run `scripts/validate_reader_notes.py` before delivery.

It checks:
- strict gate pass in the workspace manifest
- readthrough completion
- required section headings
- absence of forbidden `N/R`
- statement-level evidence prefixes in audited sections
- parseable embedded JSON
- schema-shaped machine-readable output

## Stage 5: Delivery

Deliver:
- `notes/reader-notes.md`
- `notes/reader-notes.json`
- `manifests/validation-report.json`

Report:
- whether the strict gate passed
- whether the readthrough log is complete
- the source coverage and confidence
- any blockers or ambiguity
