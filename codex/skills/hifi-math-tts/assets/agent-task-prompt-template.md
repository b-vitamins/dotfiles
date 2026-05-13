You are converting `{source_tex}` lines {start_line} through {end_line} into the spoken script `{target_txt}`.

Work only on this assigned section.

Chapter-wide spoken notation lexicon:

`{lexicon_file}`

Required output:

- Plain text only.
- No Markdown bullets unless the source genuinely requires list structure.
- No raw LaTeX commands in the final script.

Required workflow:

1. Read the assigned source lines from `{source_tex}`.
2. Rewrite them into spoken technical prose optimized for TTS.
3. Write only to `{target_txt}`.
4. Preserve all checkpointed items in scope.
5. Use checkpoints only as coverage targets; never narrate checkpoint metadata, signature hints, render hints, or compact notation hints.

Hard constraints:

- Preserve ordinary English prose with minimal touch by default.
- Do not summarize away technical content.
- Do not silently omit equations, figures, algorithms, or exercises that are in scope.
- Preserve numbering in speakable form.
- Spell out all numbers in the final script. Do not leave Arabic numerals in the output unless the parent explicitly allows them.
- Preserve symbol meaning, indices, and conditioning structure.
- Follow the notation lexicon exactly. Do not invent alternate spoken names for the same symbol.
- If the parent expanded the lexicon from the audit suggestions, treat those entries as authoritative rather than optional.
- Do not write or use local helper scripts, pandoc, or bulk mechanical transforms to generate this section. The final text in `{target_txt}` must be written by you from the source lines.
- For every numbered displayed equation in scope, explicitly introduce it as `Equation ...` before speaking it.
- If the source has an explicit `\tag{...}`, use that printed tag as the spoken equation identifier. Do not invent a chapter-global ordinal and do not say `source tag`.
- Resolve `\eqref{...}` and `\ref{...}` against the source labels when needed so prose cross-references use the printed equation, figure, definition, or problem number.
- For dense displayed equations, use explicit grouping phrases such as `the quantity`, `all over`, `inside the norm`, `the first line is`, and `equivalently` so the result sounds narrated rather than flattened.
- If an equation contains a fraction, make the denominator audible with phrases like `all over`, `the numerator is`, or `the denominator is`.
- If a denominator contains a product, sum, power, factorial, norm, or absolute value, use `the numerator is ...; the denominator is ...` or `one all over the product of ...` rather than a flat `over ... times ...` phrase.
- If a power applies to a parenthesized expression or function value, make the base audible: say `the quantity D minus alpha, squared`, `the quantity two pi, cubed`, and `the quantity natural log of a, squared`.
- If a factorial applies to a compound expression, say `the factorial of the quantity ...`.
- If a function argument contains an assignment such as `x=0`, say `evaluated at x equals zero`, not `of x equals zero`.
- If an equation contains a norm, say `norm` explicitly and mark its scope with `the quantity` when needed.
- If an equation is multiline, narrate it as `the first line is ...` and then `equivalently ...` or `the second line is ...`.
- Do not summarize a mathematical row as `paired with the preceding line`, `companion relation`, or similar shorthand. If the source row has content, speak that row.
- If an equation is a matrix or column-vector relation, narrate rows or entries explicitly rather than flattening the matrix into token soup.
- If an equation has cases, read it as cases with explicit `if` / `otherwise` phrasing.
- If an equation uses a nontrivial measure, limit, trace, or constrained sum/product, make that structure audible.
- If the source uses hats, underlines, fraktur, or similar decorations meaningfully, keep one stable spoken form for each.
- If the source uses dotless `\imath` or `\jmath`, do not speak the command names. Use `i hat`, `j hat`, `vector i`, `vector j`, or a context-specific semantic phrase.
- Preserve indexed symbols exactly. Do not let `sub one`, `sub t`, or `sub t minus one` drift.
- For adjacent-letter indices, separate the letters in speech: use `sigma sub x y`, `R sub i a`, and `A sub n m`, not `sigma sub xy`, `R sub ia`, or `A sub nm`.
- If the source contains a figure caption with teaching content, narrate it explicitly.
- If the source contains an algorithm, narrate its title, inputs/outputs, and steps clearly.
- If the source contains an exercise, include it unless the parent explicitly excluded exercises.
- If the source appears internally inconsistent, typoed, or self-contradictory, do not silently repair it. Preserve it conservatively in speech and report it to the parent.
- Do not narrate LaTeX housekeeping or layout commands such as `\begin{document}`, `\end{document}`, `\nonumber`, `\label`, or environment delimiters.
- Do not narrate planning or validation metadata such as `source signature`, `checkpoint notation`, `render hint`, or `compact notation`.
- Do not narrate `source tag`; the tag itself is the equation number.
- If the source wording is awkward for speech, improve pacing and sentence breaks without changing meaning.
- For paragraphs that are mostly English with only mild inline math, keep the wording recognizably close to the source and prefer sentence splitting over paraphrase.
- If the prose already says `vector`, `matrix`, `operator`, or another object noun before a symbol, do not duplicate it mechanically. Say `the position vector r of t`, not `the position vector vector r of t`.
- If the source is a notation table or glossary, convert rows into short spoken entries and preserve the row order.
- If the parent specified a model or reasoning requirement, do not silently downgrade. Stop and report the blocker instead.

Style target:

- Sounds like a careful technical narrator, not a parser.
- Keeps equations speakable.
- Keeps prose faithful.
- Adds breathing room where needed.

Before finishing:

1. Check that the section heading and title are present in the script.
2. Check that all obvious numbered artifacts in the assigned span were preserved.
3. Check that ordinary prose paragraphs were not rewritten more than necessary.
4. Check that notation names match the lexicon and stay stable.
5. Check that the output does not contain raw LaTeX commands.
6. Check that the output does not contain Arabic numerals.
7. Check that equation indices did not drift, especially `one` versus `t` and `t minus one`.
8. Check that every numbered displayed equation in the span is explicitly introduced as `Equation ...`.
9. Check that dense equations use audible grouping rather than flat token soup.
10. Check that no stripped structural tokens such as `enddocument` or `nonumber` leaked into the output.
11. Check that no mechanical command names such as `imath` or `jmath` leaked into the output.
12. Check that adjacent-letter indices are separated and multiline rows are not compressed by reference to another line.
13. Check that planning metadata such as `compact checkpoint notation`, `compact source signature`, or `render hint` did not leak into the output.
14. Report any ambiguity or source-local issue that may need parent review.
