# Audit checklist

Use this after drafting and before delivery.

## Coverage

- Did the strict source gate pass?
- Is the readthrough log complete?
- Does the source coverage block state version, coverage, input quality, figure
  availability, readthrough coverage, confidence, provenance, and external IDs
  when known?

## Grounding

- Do sections 1, 5, 8, 10, 11, 12, 16, 17, and 18 use anchored statement prefixes such as `[P] Anchor:`, `[D] Basis:`, `[I] Basis:`, or `[P][D] Anchor:`?
- Are important claims anchored to theorem, equation, figure, table, section, or
  file-and-line references?
- If the abstract overclaims relative to the body, is that flagged?
- Does the output use multi-axis coordinates instead of forcing one field label?
- Are prior-work comparisons expressed with typed relation hints when the source
  supports them?

## Missingness

- Is `N/A` used only when truly inapplicable?
- Is `Not reported` used only when relevant content is absent?
- Is `Unclear` used only for ambiguity or unreadable source?
- Is `N/R` absent?

## Quantitative discipline

- Are deltas only computed on matched comparisons?
- Are CIs or p-values only included when reported or defensibly derivable?
- Are uncertainties described as reported rather than guessed?

## Theory discipline

- Are assumptions explicit and tied to results?
- Are formal and informal results handled with the right mode?
- Are hidden constants, asymptotic loopholes, or regime limits flagged where
  relevant?

## Output integrity

- Are all required headings present?
- Does the embedded JSON parse?
- Does the embedded JSON validate structurally, including nested required keys
  for coordinates, claim surfaces, provenance, and relation hints?
- Are the notes compact and faithful rather than promotional?
