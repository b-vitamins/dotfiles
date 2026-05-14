# HiFi Math TTS Quality Gate

Run this checklist before claiming a script set is ready for TTS.

## Coverage

- Confirm every planned section in `plans/tts-sections.csv` has a corresponding output file.
- Confirm no output files were written outside the plan without being recorded.
- Confirm generation came from the canonical source `.tex`, not from memory or an already adapted script.
- Confirm the final transcript text was not produced by an ad-hoc local generator script, pandoc conversion, or regex-only rewrite pipeline.
- Confirm worker orchestration did not use shell-launched `codex exec`, `openai`, or approval/sandbox bypass CLI sessions.
- Confirm `plans/tts-worker-assignments.csv` records the dispatch mode, source ranges, target files, worker ids when available, and final status.
- Confirm `plans/tts-source-brief.md` exists and was written before section generation.
- Confirm notation and pronunciation decisions live in the source brief rather than a generated substitution table.
- Confirm multi-section sources did not use parent/manual/local serial rows in
  the worker ledger. Single-section sources may be parent-handled, but
  multi-section chapters and appendices require native worker dispatch or a
  recorded blocker.

## Fidelity

- Confirm ordinary prose anchors from `plans/tts-checkpoints.csv` are preserved unless there is a documented reason to rewrite more aggressively.
- Confirm the checkpoint ledger actually contains equation rows when the source has numbered displayed equations.
- Confirm all checkpointed equations are preserved in speakable form.
- Confirm every numbered displayed equation is explicitly introduced as `Equation ...`.
- Confirm explicit `\tag{...}` values are used as the spoken equation identifiers, including ranged tags, and that `source tag` is absent.
- Confirm each spoken equation number is attached to the matching source
  equation content. Do not accept a section merely because all equation numbers
  appear somewhere; shifted numbering is a hard mathematical fidelity failure.
- Confirm lettered subequations preserve their suffixes and are not collapsed
  into a bare base equation number unless the source prints them that way.
- Confirm theorem, remark, definition, proposition, example, exercise, and
  problem numbers were not mistaken for equation numbers.
- Confirm the checkpoint ledger has no duplicate equation identifiers for
  different displayed equations. If duplicates appear, stop and resolve the
  source-local printed numbering contract before generation.
- Confirm prose cross-references to equations, figures, definitions, and
  problems resolve to the intended source item rather than an invented ordinal.
  When the source brief corrects a local numbering anomaly, cross-references
  must follow the corrected item mapping; leaving an obsolete numeral that now
  points to different script content is a fidelity defect.
- Confirm meaningful prose cross-references from the source were not silently dropped unless the source brief explicitly marks them as non-content.
- Confirm indexed equation symbols did not drift.
- Confirm decorated symbols such as hats, underlines, or fraktur symbols did not drift into inconsistent spoken names.
- Confirm dotless LaTeX command names such as `imath` and `jmath` did not leak into the final spoken text; speak them as `i`, `j`, `i hat`, `j hat`, `vector i`, or `vector j` according to context.
- Confirm figure captions with teaching content are not silently dropped.
- Confirm algorithms preserve titles, inputs, outputs, and step order.
- Confirm end-of-chapter problems or exercises are excluded unless the user explicitly requested them.
- Confirm appendices are processed when selected as canonical sources.
- Confirm symbol names and indices did not drift.
- Confirm chapter-wide spoken naming is consistent with `plans/tts-source-brief.md`.
- Confirm source-local semantic notation is spoken semantically when appropriate; for example, Gaussian `\mathcal{N}` should not leak as `script n` when the source uses it as a normal distribution, and `\mathbf{I}` should not leak as `matrix i` when it denotes the identity matrix.
- Confirm bra-ket expressions are narrated as matrix elements or expectations rather than token strings such as `tensor final state`.
- Confirm placeholders such as `\bullet` and `\cdot` are narrated as arguments or quantities unless the source is explicitly discussing the glyph.
- Confirm connected and cumulant subscripts on expectation notation stay attached to the expectation rather than drifting onto the random variable.
- Confirm functional measures preserve the measure `D` and the following field or argument.
- Confirm notation tables or glossary sections were converted into orderly spoken entries rather than flattened into prose soup.
- Confirm source-local typos or inconsistencies were not silently normalized; if they were encountered, they should appear in the final report as source issues.

## Listenability

- Confirm the text reads like spoken technical prose, not raw source.
- Confirm long source sentences were broken up where needed.
- Confirm dense equations were verbalized rather than flattened into vague commentary.
- Confirm dense equations use audible grouping language such as `the quantity`, `all over`, `inside the norm`, or `equivalently` where appropriate.
- Confirm fraction denominators with products, powers, factorials, norms, or absolute values are not flattened into ambiguous `over ... times ...` phrasing.
- Confirm fractions followed by external multiplicative factors make the denominator boundary audible.
- Confirm grouping language is integrated into the equation reading and did not become validation-appeasement filler such as `with denominator ...`, `over the denominator ...`, or `the denominator terms are ...`.
- Confirm the script did not add invented explanatory filler such as unsupported claims about denominators, Gaussian forms, source contributions, or fluctuation quantities.
- Confirm parenthesized powers and function powers preserve scope, for example `(2 pi)^3`, `(D minus alpha)^2`, and `(natural log of a)^2`.
- Confirm absolute-value powers preserve scope, for example `absolute value squared of phi` rather than `absolute value of phi squared`.
- Confirm boundary/evaluation notation such as `psi(x=0,t)` is read as `evaluated at`, not as an ordinary argument `of x equals zero`.
- Confirm multiline equations do not summarize rows using phrases such as `paired with the preceding line` or `companion relation`; each mathematical row should be spoken explicitly.
- Confirm adjacent-letter indices are separated for speech, for example `sigma sub x y` rather than `sigma sub xy`.
- Confirm simple braced indices are not spoken as LaTeX grouping, for example use `z sub t minus one`, not `z sub parenthesized t minus one`.
- Confirm nontrivial measures, cases, limits, and operator names were spoken explicitly when present.
- Confirm common volume measures are not TeX-shaped when a natural dimensional phrase is available; prefer `d-dimensional x` or `d-dimensional q over the quantity two pi raised to d` over `d to the d x`.
- Confirm the final script contains no Arabic numerals unless the user explicitly allowed them.
- Confirm plural initialisms do not use apostrophe-s forms such as `S D E's`;
  use an expanded phrase or a non-possessive spoken form instead.
- Confirm stripped structural residue such as `enddocument` or `nonumber` did not leak into the final script.
- Confirm LaTeX metadata such as `\label{...}` was not narrated as source labels or bookkeeping text.
- Confirm planning metadata such as `compact checkpoint notation` did not leak into the final script.
- Confirm standalone italic navigational callouts such as `Exercise 20.1`, `Section 11.3`, or `Chapter 19` were omitted unless they were integrated into meaningful prose.
- Confirm source-brief pronunciation notes were not applied as blind substitutions that erase adjacent symbols, fields, arguments, measures, or operators.

## Validation

- Run `python3 scripts/validate-tts-script.py --plan plans/tts-sections.csv --checkpoints plans/tts-checkpoints.csv --source-brief plans/tts-source-brief.md --out plans/tts-validation.csv`.
- If the user explicitly requested normally excluded material and the plan used `--include-out-of-scope`, add `--allow-out-of-scope` to the validation command.
- Treat validation as a guardrail, not an oracle.
- Do not claim `validated` if the validator reports hard failures such as missing files, missing numbered artifacts, raw LaTeX residue, metadata leakage, or obvious validator-appeasement text.
- Treat warnings as review items for the line audit, not as strings to appease.

## Acceptance Sample

- Build `plans/tts-acceptance-sample.csv`.
- Review the sampled sections fresh against the source and the source brief, line by line or source-span by source-span.
- Record the review in a nonempty `plans/tts-acceptance-review.md` or
  `plans/tts-acceptance-review.csv`; a sample CSV alone is not evidence that a
  style audit occurred.
- For chapters with ten or fewer planned sections, review every planned section.
- Require concrete source-line and script-line references in each acceptance-review row; a generic pass paragraph is not sufficient.
- Use `assets/line-by-line-audit-prompt-template.md` for calibration runs, skill-improvement loops, or any user-requested deep quality audit.
- If the acceptance sample finds a defect pattern, return to repair mode and regenerate the affected sections.

## Whole Book

- For a whole-book run, build a manifest first and keep transient chapter-level artifacts disjoint under `plans/<source-stem>/`.
- Keep final script outputs disjoint under `scripts/<source-stem>/`.
- After the chapter runs finish, aggregate book-wide status into a single CSV so `ready-for-tts` can be checked at the book level rather than inferred informally.
- Run `scripts/audit-tts-run.py` before aggregation or final reporting so
  missing acceptance reviews and serial fallbacks are reported mechanically.

## Cleanup

- Run cleanup only after deterministic validation, acceptance sampling, and any book-level aggregation have passed.
- Remove transient `plans/`, `codex-run.log`, and `codex-last-message.txt` artifacts before final handoff.
- Final durable outputs should live under `scripts/`.

## Audio

- Audio rendering is optional.
- TTS or STT round-trip should be used sparingly as a calibration surface, not as the default pipeline.
- The default success condition is a script that is textually ready for TTS.

## Final Claims

- Report `source-briefed`, `source-covered`, `validated`, `style-audited`, and `ready-for-tts` separately.
- Do not claim `ready-for-tts` until the source brief exists, deterministic validation has no hard failures, and the acceptance sample is clean.
- Report cleanup status after transient artifacts have been removed.
