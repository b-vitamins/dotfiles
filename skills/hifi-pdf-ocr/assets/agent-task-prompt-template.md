You are transcribing pages {start_page} through {end_page} from {pages_dir} into `{target_tex}`.

If this assignment is an audit/fix pass over existing text, do not trust existing page text until it has been re-verified from the rendered page image.

Required workflow:

1. Read exactly one page image at a time (`{pages_dir}/page-<n>.png`).
2. Append or patch only that page's transcription in `{target_tex}` immediately.
3. Insert `% Page <n>` before each appended page block.
4. Preserve continuity across page breaks.

Hard constraints:

- Do not use OCR/text extraction.
- Do not batch-read or batch-write multiple pages.
- Use modern LaTeX syntax (`\(...\)`, `\eqref{...}`).
- Add meaningful equation labels when equations are referenced.
- Use hyphen-only labels (`eq-...`, `fig-...`), never `_`.
- Keep diagram placeholders and transcribe captions exactly.
- Distinguish numbered figures, decorative chapter-opening art, boxed algorithms, and native LaTeX tables/matrices.
- If a page still contains placeholder-only content for a numbered figure visible on the rendered page, report it as an unresolved diagram gap. Do not describe that page as diagram-complete.
- If the parent provided an explicit model or reasoning requirement, do not silently downgrade. If you cannot honor it, stop and report the blocker.

After finishing `{end_page}`:

1. Run `python3 scripts/verify-page-markers.py --file {target_tex} --start {start_page} --end {end_page} --report plans/page-marker-verification.json`.
2. Run `pdflatex -interaction=nonstopmode -halt-on-error {target_tex}` from the directory that preserves `{target_tex}` relative paths.
3. Fix only compilation/syntax issues without changing meaning.
4. Keep `% Page <n>` markers in place for post-processing cleanup.
5. Report completed pages, marker-check status, compile status, unresolved TODO(diagrams), and any unresolved diagram gaps.
