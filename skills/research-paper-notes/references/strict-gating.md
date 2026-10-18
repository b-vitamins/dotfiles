# Strict gating

This skill is intentionally narrow. It is not a best-effort summarizer.

## Hard gate

Proceed only if all of the following are true:
1. TeX source exists locally or was fetched from arXiv.
2. A main TeX file can be identified.
3. Textual include chains can be resolved well enough to build
   `combined-source.tex`.
4. The readthrough manifest covers the whole combined source.
5. The readthrough log is complete before notes are drafted.

## Fail-fast conditions

Stop before analysis when any of the following hold:
- no TeX source is available
- the input is PDF-only, OCR-only, screenshot-only, or abstract-only
- the arXiv source fetch fails
- the main TeX file cannot be identified
- textual include chains are unresolved
- the combined source is missing major body content
- the readthrough log is incomplete

## Not fatal by themselves

Record these, but do not fail on them by themselves if source text coverage is
otherwise complete enough:
- missing figure image files
- missing `.bib` files when the body text is intact
- noisy companion PDFs
- missing supplemental binaries not required for reading the paper text

## Failure behavior

When the gate fails:
1. write `manifests/source-gate-report.json`
2. write `manifests/workspace.json` with `strict_gate_passed: false`
3. do not generate reader-notes
4. tell the user exactly what is missing and what would unblock the workflow
