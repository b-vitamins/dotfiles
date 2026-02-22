# Transcription Quality Gate

Run this checklist before finishing any assigned range.

## Fidelity Checks

- Confirm every assigned page has a `% Page <n>` marker.
- Confirm no extra pages outside the assignment were appended.
- Confirm each page was transcribed from its image directly (no OCR).
- Confirm continuity for sentences cut by page breaks.

## LaTeX Checks

- Confirm modern math delimiters (`\(...\)` and `\[...\]`).
- Confirm every referenced equation has `\label{...}` and is cited with `\eqref{...}`.
- Confirm labels use hyphens only (no `_`).
- Confirm figure captions are transcribed exactly.
- Confirm diagram-only figures have placeholder + TODO comment.

## Parallel Safety Checks

- Confirm no overlap with other agents' page ranges.
- Confirm target output file matches assignment.
- If shard files are used, confirm shard filename includes page range.

## Post-Processing Checks

- Run `python3 scripts/cleanup-page-markers.py chapter-*.tex`.
- Confirm `% Page <n>` markers are removed from deliverable chapter files.
- Confirm stitched joins did not break section/environment boundaries.
- Optional verification: `python3 scripts/cleanup-page-markers.py --check chapter-*.tex`.

## Build Checks

- Run `pdflatex -interaction=nonstopmode -halt-on-error <target.tex>` twice.
- Fix only syntax/compilation issues; do not alter content meaning.
- Record unresolved TODO(diagrams) entries.
