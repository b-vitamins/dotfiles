# Output contract

Always include all sections below. Use `N/A`, `Not reported`, or `Unclear`
exactly as needed.

Before section 0, include a short **Source coverage** block with:
- TeX source gate
- source version used
- source coverage
- source provenance
- external IDs when known
- input quality
- figure availability
- readthrough coverage
- confidence

In audited sections, write one substantive statement block per bullet or
paragraph. Each block must begin with one or more evidence tags followed by
`Anchor:` or `Basis:`. Canonical forms are:
- `[P] Anchor: <section/eq/theorem/table/file:line>; <paper-stated statement>`
- `[D] Basis: <reported quantities and arithmetic>; <derived statement>`
- `[I] Basis: <specific paper evidence>; <inference>`
- `[P][D] Anchor: <paper location plus arithmetic basis>; <mixed reported-and-derived statement>`

## 0) Header

Citation, link or DOI, venue and year, domain keywords, paper type, and a
compact coordinate block.

The coordinate block should be multi-label rather than single-label, using:
- purpose
- problem form
- learning signal
- mechanism
- evidence regime

## 1) One-sentence claim (verifiable)

Maximum 25 words. It must be measurable or falsifiable. Use the audited-section
prefix grammar above.

## 2) Problem & setup (canonicalized)

State the objective, variables, spaces, constraints, regimes, and primary
objects.

Also record the main claim surfaces compactly when they are identifiable:
- problem surface
- data surface
- objective surface
- mechanism surface
- regime surface
- evaluation surface
- deployment surface

## 3) Notation & constants table

Use columns:
- symbol
- meaning
- type or units
- default or typical value
- source anchor
- normalization note

## 4) Method/Model (algorithmic core)

Give the short idea, core equations, concise numbered pseudocode if applicable,
and time, memory, or communication complexity when reported.

## 5) Theory/Derivation summary

Use theorem mode for formal results and derivation mode for informal results.
A single paper may require both.

Use the audited-section prefix grammar for each substantive result statement.

For formal results include:
- result ID and type
- hypotheses
- statement
- canonical restatement of the rate or constant structure
- proof sketch
- regime map

For informal derivations include:
- main claim
- derivation trail
- validity window
- limiting-case checks

## 6) Assumptions & conditions ledger

List A1...Ak with statement, scope, and where used.

## 7) Experiments: reproduction checklist

Cover data, preprocessing, splits, models, hyperparameters, seeds, hardware,
software, metrics, baselines, benchmark adoption, and ethics when relevant.

## 8) Results with matched deltas

Only compute deltas when comparisons are matched and sufficient numbers exist.
Only report CIs, p-values, or tests when reported or defensibly derivable.
Use the audited-section prefix grammar for each substantive result statement.

## 9) Figures -> findings map

For each material figure or table:
- question answered
- main number or equation
- caveat

Keep this section even for theory-heavy papers when a figure or table carries a
claim.

## 10) Comparison to prior work

Compare against the nearest 2 to 5 works only when the paper itself provides
enough evidence to do so responsibly.

When possible, express each comparison with a typed relation label from a small,
controlled set such as:
- `inherits_from`
- `compares_to`
- `critiques`
- `re-benchmarks`
- `shares_artifact`
- `transfers_to`
- `scales_from`

Use the audited-section prefix grammar for each substantive comparison block.

## 11) External validity & limits

State where the claims should hold or fail. Use the audited-section prefix
grammar above.

## 12) Threats to validity

Cover theory and empirical threats as applicable. Use the audited-section prefix
grammar above.

## 13) Vital verbatim sentences

Quote 3 to 8 short indispensable excerpts with anchors. Tag each as one of:
- definition
- claim
- limitation
- method
- result
- assumption

## 14) Reproduction/verification plan

State the minimal theoretical and empirical checks.

## 15) Artifacts

List code, data, weights, logs, licenses, and provenance. Prefer stable names,
versions, and roles over prose-only descriptions.

## 16) Red flags & green flags

Only include paper-specific items. Use the audited-section prefix grammar above.

## 17) Who should care & why it matters

State concrete implications for builders or theorists. Use the audited-section
prefix grammar above.

## 18) Open questions

Give 3 to 7 crisp next experiments, theorems, or missing links. Use the
audited-section prefix grammar above.

## 19) Machine-readable block (JSON)

Embed a valid JSON block matching `assets/reader-notes-schema.json`.

The JSON should preserve:
- external IDs
- source provenance
- multi-axis coordinates
- claim surfaces
- typed relation hints
