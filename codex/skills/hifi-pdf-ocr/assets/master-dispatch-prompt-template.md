I have prepared page assignments in `{assignment_file}`.

Spawn one sub-agent per assignment row.

For each sub-agent:

- Use the role `{agent_role}` unless a better configured role is available.
- Restrict work to the assigned page range only.
- Write only to the assigned target file.
- Follow this strict workflow:
  1. Read one page image.
  2. Append only that page.
  3. Insert `% Page <n>` marker.
  4. Continue sequentially.
- Enforce constraints:
  - No OCR/text extraction.
  - No batch-read or batch-write across pages.
  - Modern LaTeX syntax and `\eqref{...}` references.
  - Hyphen-only labels.
  - Faithful figure captions; diagram placeholders with TODO comments.

Wait for all sub-agents to complete.

After all sub-agents finish:

- Merge shard outputs in strict page order if shard files were used.
- Run `python3 scripts/cleanup-page-markers.py chapter-*.tex`.
- Compile each chapter file with `pdflatex -interaction=nonstopmode -halt-on-error` (two passes).

Then provide a consolidated report with:

- Per-agent page range and output file.
- Completion status.
- Compile status.
- Unresolved TODO(diagrams) entries.
