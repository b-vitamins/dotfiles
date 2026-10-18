You are converting `{source_tex}` lines {start_line} through {end_line} into `{target_txt}`.

Write the script a technically competent voice actor would read for an audiobook version of the source. Preserve the mathematics completely, but make it sound like spoken English rather than LaTeX.

Read before writing:

- Source-local narration brief: `{source_brief_file}`
- Assigned source lines from `{source_tex}`

Work only on this assigned section and write only `{target_txt}`.

Core contract:

- Generate from the source lines, not from checkpoints, validation hints, or memory.
- Preserve the source's informational order and ordinary prose with minimal touch.
- Preserve all mathematical content, indices, conditioning bars, limits, measures, cases, captions, algorithms, tables, and lists in planned source spans.
- Follow source-brief boundary notes when LaTeX placement separates a figure, table, algorithm, display, or caption from the prose that introduces it. The concatenated audiobook flow matters more than mechanically starting with a heading when the brief says an attached artifact should be narrated first.
- End-of-chapter problems or exercises, front matter, answers or solutions, bibliography or references, and index are out of scope unless the parent explicitly included them in the section plan.
- Use natural spoken math. Stage dense expressions with phrases like `the numerator is`, `all over`, `the quantity`, `the first case is`, and `equivalently`.
- When a fraction is followed by a separate factor, make the boundary explicit so the listener can tell what is in the denominator and what is multiplied afterward.
- Resolve and speak real cross-references such as equations and figures. Omit only standalone navigation or drill callouts that the brief marks as non-content.
- If the source brief identifies stale hard-coded cross-references, speak the
  corrected target number for the equation, theorem, figure, or table that the
  prose actually refers to. Do not preserve an obsolete source numeral when it
  would point to a different item in the final script.
- For every numbered displayed equation, attach the spoken equation number to
  the exact source equation content. Do not shift neighboring equation numbers,
  and do not let theorem, remark, definition, proposition, example, exercise,
  or problem numbers advance the equation counter.
- Preserve printed letter suffixes on subequations, such as `Equation twelve
  point four point four a`; do not collapse them into the bare base equation.
- Spell out numbers unless the parent explicitly allowed Arabic numerals.
- Do not emit raw LaTeX, labels, planning metadata, checkpoint metadata, or compact notation.
- Do not run scripts, pandoc, regex converters, or local generators to create the final text.

Important style target:

The result should sound like a careful human narrator who understands the source. Do not optimize for a validator. Do not add explanatory filler that is not in the source. If an equation is hard to say, improve grouping and pacing rather than inventing commentary.

Common high-risk math idioms:

- Bra-kets such as `\langle f|U|i\rangle` are matrix elements between states with an operator in the middle; avoid token-order artifacts like `tensor final state`.
- Placeholder notation such as `\langle \bullet\rangle` means the average of the argument or quantity; do not literally narrate the bullet unless the source discusses the symbol.
- Absolute-value powers such as `|\phi|^2` should preserve scope, for example `absolute value squared of phi` or `the squared absolute value of phi`.
- Connected or cumulant expectations such as `\langle A\rangle_c` attach `connected` or `cumulant` to the expectation, not to `A`.
- Functional measures such as `\mathcal{D}\phi(x)` preserve both the measure and the field, for example `the functional measure D phi of x`.
- Volume measures such as `\mathrm{d}^d x`, `\mathrm{d}^d q/(2\pi)^d`, and `\mathrm{d}^{d-1}\mathbf{x}` should sound like measures over dimensional variables, for example `d-dimensional x`, `d-dimensional q over the quantity two pi raised to d`, and `d minus one-dimensional vector x`; avoid `d to the d x` when this natural form is available.
- Braced indices such as `z_{t-1}` are just grouping; say `z sub t minus one`, not `sub parenthesized`.
- Adjacent-letter indices should be separated in speech, such as `sigma sub x y`, not `sigma sub xy`.
- Avoid mechanical expansion artifacts. If the prose already supplies the mathematical object type, do not repeat that type just because the symbol is bold, decorated, or semantic in the source brief.
- For negative or multi-token bases with fractional powers, keep the whole base in scope, for example `the whole quantity negative two t, raised to negative one half`.

Before finishing:

1. Read the source span and your script side by side.
2. Confirm every paragraph, display, caption, table row, algorithm step, and list item in scope is represented or intentionally omitted under the brief.
3. Confirm equations preserve symbol identity, index identity, and structure.
4. Confirm section-boundary attachments from the brief will sound coherent when this file is concatenated with neighboring section files.
5. Confirm the prose still sounds like the source, not a summary.
6. Remove any validator-appeasement phrases such as detached `with denominator ...`, `over the denominator ...`, or commentary about grouping that a human narrator would not say.
7. Report any source ambiguity or internal inconsistency to the parent.
