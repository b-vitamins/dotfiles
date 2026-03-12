# Reader Notes: <title>

## Source coverage
- TeX source gate: PASS
- Source version used: <exact version or arXiv id>
- Source coverage: <main paper / appendix / source files / companion PDF>
- Source provenance: <local source tree / arXiv source bundle / other strict source>
- External IDs: <arXiv / DOI / OpenReview / OpenAlex / Semantic Scholar when known>
- Input quality: <native text / partial / unclear>
- Figure availability: <source assets / PDF pages / captions only>
- Readthrough coverage: <completed>/<total> chunks
- Confidence: <high / medium / low>

Audited statement syntax:
- `[P] Anchor: <section/eq/theorem/table/file:line>; <paper-stated statement>`
- `[D] Basis: <reported quantities and arithmetic>; <derived statement>`
- `[I] Basis: <specific paper evidence>; <inference>`
- `[P][D] Anchor: <paper location plus arithmetic basis>; <mixed reported-and-derived statement>`

## 0) Header
- Citation:
- External IDs:
- Coordinates:
  - purpose:
  - problem form:
  - learning signal:
  - mechanism:
  - evidence regime:

## 1) One-sentence claim (verifiable)
[P] Anchor: <section/eq/theorem/table/file:line>; <25-word measurable or falsifiable claim>

## 2) Problem & setup (canonicalized)
- Claim surfaces:
  - problem:
  - data:
  - objective:
  - mechanism:
  - regime:
  - evaluation:
  - deployment:

## 3) Notation & constants table

| symbol | meaning | type/units | default/typical value | source anchor | normalization note |
| --- | --- | --- | --- | --- | --- |

## 4) Method/Model (algorithmic core)

## 5) Theory/Derivation summary
[P] Anchor: <theorem/lemma/proposition/section>; <formal result or derivation claim>
[D] Basis: <reported quantities or algebraic trail>; <derived implication if needed>

## 6) Assumptions & conditions ledger

## 7) Experiments: reproduction checklist

## 8) Results with matched deltas
[P] Anchor: <table/figure/section>; <primary reported result statement>
[D] Basis: <matched baseline arithmetic>; <delta statement when defensibly derived>

| metric | dataset/task | condition | reported value/uncertainty | N/seeds | abs delta vs matched baseline | relative delta | CI or p-value | test | caveat |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

## 9) Figures -> findings map

## 10) Comparison to prior work
[P] Anchor: <related-work section or comparison table>; Relation: `<typed_label>`; <comparison statement>

Use typed labels when supported:
- `inherits_from`
- `compares_to`
- `critiques`
- `re-benchmarks`
- `shares_artifact`
- `transfers_to`
- `scales_from`

## 11) External validity & limits
[P] Anchor: <discussion/limitations/appendix>; <scope or failure-regime statement>

## 12) Threats to validity
[P] Anchor: <discussion/limitations/appendix>; <theory or empirical threat statement>

## 13) Vital verbatim sentences

## 14) Reproduction/verification plan

## 15) Artifacts

## 16) Red flags & green flags
[P] Anchor: <specific source anchor>; <paper-specific red or green flag>

## 17) Who should care & why it matters
[I] Basis: <specific evidence from the paper>; <implication for builders or theorists>

## 18) Open questions
[I] Basis: <specific evidence gap or tension in the paper>; <next experiment, theorem, or missing link>

## 19) Machine-readable block (JSON)
```json
{
  "title": null,
  "doi": null,
  "venue": null,
  "year": null,
  "paper_type": null,
  "external_ids": {
    "arxiv": null,
    "doi": null,
    "openreview_forum": null,
    "openalex": null,
    "semantic_scholar": null
  },
  "source_version": null,
  "source_coverage": null,
  "source_provenance": {
    "input_type": null,
    "input_label": null,
    "tex_source_authority": "TeX",
    "companion_pdf_used": null
  },
  "input_quality": null,
  "tex_source_gate": "PASS",
  "readthrough_complete": false,
  "readthrough_chunks": {
    "completed": 0,
    "total": 0
  },
  "coordinates": {
    "purpose": [],
    "problem_form": [],
    "learning_signal": [],
    "mechanism": [],
    "evidence_regime": []
  },
  "claim_surfaces": {
    "problem": null,
    "data": null,
    "objective": null,
    "mechanism": null,
    "regime": null,
    "evaluation": null,
    "deployment": null
  },
  "problem": null,
  "main_claim": null,
  "notation": [],
  "assumptions": [],
  "results": [],
  "algorithms": [],
  "datasets": [],
  "metrics": [],
  "hardware": {},
  "effects": [],
  "artifacts": {},
  "relation_hints": [],
  "repro_thresholds": [],
  "limitations": [],
  "red_flags": []
}
```
