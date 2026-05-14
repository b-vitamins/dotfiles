Review the sampled spoken-script sections for TTS readiness by comparing source and script line by line.

Inputs:

- Source TeX: `{source_tex}`
- Section plan: `{plan_file}`
- Checkpoint ledger: `{checkpoint_file}`
- Source brief: `{source_brief_file}`
- Acceptance sample: `{acceptance_sample_file}`
- Script directory: `{script_dir}`
- Output review artifact: `{acceptance_review_file}`

For each sampled row:

1. Read the source brief, the source line range, and the generated script.
2. Number or otherwise locate the script lines you are reviewing.
3. Walk through the source span in order. For each paragraph, equation, figure, table row, algorithm, or list item, identify the corresponding script passage.
4. Check fidelity against the source, not against memory.
5. Check whether the script sounds like a careful technical narrator rather than a parser.
6. Check whether ordinary English prose stayed recognizably close to the source.
7. Check whether displayed equations preserve structure, symbols, indices, conditioning bars, limits, measures, cases, and multiline rows.
8. Check whether fractions with external multiplicative factors make the denominator boundary audible.
9. Check that each displayed equation's spoken number is attached to the
   matching source equation content. Shifted numbering is a hard failure even
   if every equation number appears somewhere in the script.
10. Check that lettered subequations preserve suffixes such as `a`, `b`, and
   `c`, and that theorem, remark, definition, proposition, example, exercise,
   and problem numbers were not mistaken for equation numbers.
11. Check whether notation follows the source-local semantic decisions in the source brief, rather than falling back to mechanical readings.
12. Check that meaningful prose cross-references, such as `see Eq. ...`,
    survive unless the source brief explicitly marks them as non-content.
    If the brief corrected a source-local numbering anomaly, the spoken
    cross-reference must resolve to the intended displayed item under the
    corrected script numbering; preserving an obsolete source numeral that now
    points to different content is a fidelity failure.
13. Check that simple braced indices such as `z_{t-1}` read as `z sub t minus one`, not as LaTeX grouping.
14. Check for mechanical expansion artifacts introduced by applying source-brief notation wording blindly instead of integrating it into the surrounding sentence.
15. Check that powers on negative or multi-token bases keep the whole base in scope, for example `the whole quantity negative two t, raised to negative one half`.
16. Check that standalone navigational callouts such as `Exercise 20.1`, `Section 11.3`, and `Chapter 19` are not narrated as isolated paragraphs unless they contain actual problem content.
17. Check that default-out-of-scope material such as end-of-chapter problems, exercises, front matter, answers or solutions, bibliography or references, and index is not present unless the user explicitly requested it.
18. Check that captions, algorithms, tables, and lists in scope were handled intentionally.
17. Check section-boundary coherence across neighboring files when a sampled section begins or ends near a heading. If the source places a figure, table, algorithm, display, or caption after a heading but the adjacent prose introduces it before the heading, the concatenated scripts must still read in a coherent audiobook order.
18. Check that the script does not contain validator-appeasement fragments such as `with denominator ...`, `over the denominator ...`, `the denominator terms are ...`, or extra comments about grouping that are not part of natural narration.
19. Check recurring high-risk mathematical idioms when present:
    - bra-kets are matrix elements between states, not token strings like `tensor final state`
    - placeholders such as `\bullet` are arguments or quantities, not literal bullets
    - `|\phi|^2` and similar forms preserve absolute-value scope as `absolute value squared of phi`
    - connected or cumulant expectations such as `\langle A\rangle_c` attach `connected` or `cumulant` to the expectation, not to `A`
    - functional measures preserve the `D` and the field, as in `functional measure D phi of x`
    - common volume measures such as `\mathrm{d}^d x` and `\mathrm{d}^d q/(2\pi)^d` use listener-friendly forms such as `d-dimensional x` and `d-dimensional q over the quantity two pi raised to d`, not mechanical TeX readings like `d to the d x`
20. Check that the script does not invent explanatory filler absent from the source, such as unsupported claims about Gaussian forms, source contributions, denominators, or fluctuation quantities.

Report one row per sampled section with:

- target file
- pass/fail
- source line numbers reviewed
- script line numbers reviewed
- concrete problems found
- concrete repair instructions

Write the report to `{acceptance_review_file}`. Do not leave the review only in
the chat transcript; readiness tooling treats an acceptance sample without a
nonempty review artifact as not style-audited.

For failures, label each concrete problem with one or more issue types: `fidelity`, `omission`, `invention`, `notation`, `listenability`, `coverage`, `validator-appeasement`, or `acceptable-adaptation`.

Do not hide recurring polish debt under `none`. If a section would need human
cleanup before TTS because of repeated mechanical measure readings, ambiguous
fraction boundaries, or parser-like phrasing, mark it as failed with
`listenability` repair instructions even when the math is present.

Every row must include at least two concrete source-line references and two concrete script-line references that were checked. Do not submit a generic chapter-level pass note.

Do not mark a section as passing just because deterministic validation passed. Passing means the script is a high-quality substrate for a TTS engine without later human cleanup.
