I have prepared page assignments in `{assignment_file}`.

Spawn one sub-agent per assignment row.

For each sub-agent:

- Use the role `{agent_role}` unless a better configured role is available.
- Honor any explicit user-specified model and reasoning requirements. If they cannot be satisfied exactly, stop and report the blocker.
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
  - Distinguish numbered figures, decorative chapter-opening art, boxed algorithms, and native LaTeX tables/matrices.

Wait for all sub-agents to complete.
Close completed or errored agents before spawning replacements.
If an agent fails or stalls, reassign only the unfinished pages and prefer smaller continuation chunks.

After all sub-agents finish:

- Verify exact marker coverage with `python3 scripts/verify-page-markers.py --csv {assignment_file} --report plans/page-marker-verification.json`.
- Merge shard outputs in strict page order if shard files were used.
- Run an independent image-based audit/fix pass across all touched pages when fidelity is high stakes and record results in `plans/page-audit.csv`.
- Record unresolved numbered-figure content gaps in `plans/diagram-gaps.csv`.
- Run `python3 scripts/cleanup-page-markers.py` on all touched targets after the audit pass succeeds.
- Compile each touched target with `pdflatex -interaction=nonstopmode -halt-on-error` (two passes), using a working directory that preserves relative paths.
- Build a fresh acceptance sample with `python3 scripts/select-acceptance-pages.py --audit-csv plans/page-audit.csv --out plans/acceptance-sample.csv`.
- Run a fresh read-only check on the acceptance sample before final signoff.

Then provide a consolidated report with:

- Per-agent page range and output file.
- Completion status.
- Exact page-coverage status.
- Structural completeness vs text-audited vs diagram-complete vs fully fidelity-complete.
- Compile status.
- Marker-verification artifact path.
- Acceptance-sample artifact path.
- Remaining diagram gaps.
- Unresolved TODO(diagrams) entries.
