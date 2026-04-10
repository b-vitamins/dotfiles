# Transcription Quality Gate

Run this checklist before finishing any assigned range.

## Fidelity Checks

- Confirm every assigned page has a `% Page <n>` marker.
- Confirm no extra pages outside the assignment were appended.
- Confirm each page was transcribed from its image directly (no OCR).
- Confirm continuity for sentences cut by page breaks.
- Confirm exact marker coverage with `python3 scripts/verify-page-markers.py` before cleanup.
- Confirm the marker check wrote a durable report under `plans/`, e.g. `plans/page-marker-verification.json`.
- If the job involved pre-existing text, confirm every touched page was re-verified from its image, not merely compiled.

## LaTeX Checks

- Confirm modern math delimiters (`\(...\)` and `\[...\]`).
- Confirm every referenced equation has `\label{...}` and is cited with `\eqref{...}`.
- Confirm labels use hyphens only (no `_`).
- Confirm figure captions are transcribed exactly.
- Confirm diagram-only figures have placeholder + TODO comment.
- Confirm any page that still has placeholder-only numbered figure content is logged in `plans/diagram-gaps.csv`.
- Confirm native LaTeX tables, matrices, and boxed algorithms have not been misclassified as diagram placeholders.
- Confirm decorative chapter-opening art is not reported as a numbered figure.

## Parallel Safety Checks

- Confirm no overlap with other agents' page ranges.
- Confirm target output file matches assignment.
- If shard files are used, confirm shard filename includes page range.
- Confirm any rows that share a `target_tex` were either co-assigned or sharded explicitly.

## Post-Processing Checks

- Run `python3 scripts/cleanup-page-markers.py chapter-*.tex`.
- Confirm `% Page <n>` markers are removed from deliverable chapter files.
- Confirm stitched joins did not break section/environment boundaries.
- Optional verification: `python3 scripts/cleanup-page-markers.py --check chapter-*.tex`.
- Do not remove markers until the page-coverage gate has passed.

## Build Checks

- Run `pdflatex -interaction=nonstopmode -halt-on-error <target.tex>` twice.
- Run it from the directory that preserves relative include/graphics paths.
- Fix only syntax/compilation issues; do not alter content meaning.
- Record unresolved TODO(diagrams) entries.

## Audit Ledger Checks

- Confirm `plans/page-audit.csv` exists and covers the full requested scope.
- Confirm `plans/page-audit.csv` uses only `verified`, `fixed`, `pending`, or `blocked` statuses.
- Confirm there are no `pending` rows before claiming `text-audited`.
- Confirm `plans/diagram-gaps.csv` is empty or absent before claiming `diagram-complete`.

## Acceptance Sample Checks

- Build `plans/acceptance-sample.csv` from `plans/page-audit.csv`.
- Confirm the sample includes both `verified` and `fixed` pages when both statuses exist.
- Run a fresh read-only image check on the sampled pages.
- If the sample finds a defect, return to repair mode and rerun the acceptance sample after fixes.

## Completion Claims

- Report `structurally complete`, `compiled`, `text-audited`, `diagram-complete`, and `fully fidelity-complete` separately.
- Do not claim `diagram-complete` while `plans/diagram-gaps.csv` has unresolved numbered-figure content gaps.
- Do not claim `fully fidelity-complete` until the independent audit/fix pass has covered all touched pages, the marker-verification artifact exists, and the acceptance sample is clean.
