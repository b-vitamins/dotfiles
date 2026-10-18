# arXiv ingestion

The helper scripts use these public endpoints:
- `https://arxiv.org/abs/<id>`
- `https://arxiv.org/pdf/<id>.pdf`
- `https://arxiv.org/src/<id>`

Use an arXiv identifier with or without an explicit version suffix.

## Practical notes

- The source bundle may be a tar archive, a compressed single TeX file, or a raw
  payload. The fetch script stores the raw bundle and tries safe extraction.
- The source tree may contain generated files, figures, comments, or extra
  assets not needed for reading.
- Missing figure images do not automatically fail the strict gate if the textual
  source coverage is intact.
- Missing or unresolved textual includes do fail the strict gate.

## Compliance notes

- Use the workflow for research on user-requested papers.
- Do not claim rights to redistribute PDFs or source files beyond the paper's
  actual license.
- Respect arXiv rate limits and terms.
