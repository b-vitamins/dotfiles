You are transcribing pages {start_page} through {end_page} from {pages_dir} into `{target_tex}`.

Required workflow:

1. Read exactly one page image at a time (`{pages_dir}/page-<n>.png`).
2. Append only that page's transcription to `{target_tex}` immediately.
3. Insert `% Page <n>` before each appended page block.
4. Preserve continuity across page breaks.

Hard constraints:

- Do not use OCR/text extraction.
- Do not batch-read or batch-write multiple pages.
- Use modern LaTeX syntax (`\(...\)`, `\eqref{...}`).
- Add meaningful equation labels when equations are referenced.
- Use hyphen-only labels (`eq-...`, `fig-...`), never `_`.
- Keep diagram placeholders and transcribe captions exactly.

After finishing `{end_page}`:

1. Run `pdflatex -interaction=nonstopmode -halt-on-error {target_tex}`.
2. Fix only compilation/syntax issues without changing meaning.
3. Keep `% Page <n>` markers in place for post-processing cleanup.
4. Report completed pages and unresolved TODO(diagrams).
