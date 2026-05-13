# HiFi Math TTS Quality Gate

Run this checklist before claiming a script set is ready for TTS.

## Coverage

- Confirm every planned section in `plans/tts-sections.csv` has a corresponding output file.
- Confirm no output files were written outside the plan without being recorded.
- Confirm generation came from the canonical source `.tex`, not from memory or an already adapted script.
- Confirm the final transcript text was not produced by an ad-hoc local generator script, pandoc conversion, or regex-only rewrite pipeline.
- Confirm worker orchestration did not use shell-launched `codex exec`, `openai`, or approval/sandbox bypass CLI sessions.
- Confirm `plans/tts-lexicon.csv` exists and reflects the current chapter's notation conventions.
- Confirm `plans/tts-lexicon-audit.csv` exists and has been reviewed.
- Confirm there are no unreviewed high-frequency uncovered notation candidates in the audit artifact.
- Prefer an audit result with zero uncovered candidates. If uncovered candidates remain, expand the lexicon and rerun the audit before generation or final signoff.

## Fidelity

- Confirm ordinary prose anchors from `plans/tts-checkpoints.csv` are preserved unless there is a documented reason to rewrite more aggressively.
- Confirm the checkpoint ledger actually contains equation rows when the source has numbered displayed equations.
- Confirm all checkpointed equations are preserved in speakable form.
- Confirm every numbered displayed equation is explicitly introduced as `Equation ...`.
- Confirm explicit `\tag{...}` values are used as the spoken equation identifiers, including ranged tags, and that `source tag` is absent.
- Confirm prose cross-references to equations, figures, definitions, and problems resolve to the printed source number rather than an invented ordinal.
- Confirm indexed equation symbols did not drift.
- Confirm decorated symbols such as hats, underlines, or fraktur symbols did not drift into inconsistent spoken names.
- Confirm dotless LaTeX command names such as `imath` and `jmath` did not leak into the final spoken text; speak them as `i`, `j`, `i hat`, `j hat`, `vector i`, or `vector j` according to context.
- Confirm figure captions with teaching content are not silently dropped.
- Confirm algorithms preserve titles, inputs, outputs, and step order.
- Confirm exercises are preserved unless the user explicitly excluded them.
- Confirm symbol names and indices did not drift.
- Confirm chapter-wide spoken naming is consistent with `plans/tts-lexicon.csv`.
- Confirm notation tables or glossary sections were converted into orderly spoken entries rather than flattened into prose soup.
- Confirm source-local typos or inconsistencies were not silently normalized; if they were encountered, they should appear in the final report as source issues.

## Listenability

- Confirm the text reads like spoken technical prose, not raw source.
- Confirm long source sentences were broken up where needed.
- Confirm dense equations were verbalized rather than flattened into vague commentary.
- Confirm dense equations use audible grouping language such as `the quantity`, `all over`, `inside the norm`, or `equivalently` where appropriate.
- Confirm fraction denominators with products, powers, factorials, norms, or absolute values are not flattened into ambiguous `over ... times ...` phrasing.
- Confirm parenthesized powers and function powers preserve scope, for example `(2 pi)^3`, `(D minus alpha)^2`, and `(natural log of a)^2`.
- Confirm boundary/evaluation notation such as `psi(x=0,t)` is read as `evaluated at`, not as an ordinary argument `of x equals zero`.
- Confirm multiline equations do not summarize rows using phrases such as `paired with the preceding line` or `companion relation`; each mathematical row should be spoken explicitly.
- Confirm adjacent-letter indices are separated for speech, for example `sigma sub x y` rather than `sigma sub xy`.
- Confirm nontrivial measures, cases, limits, and operator names were spoken explicitly when present.
- Confirm the final script contains no Arabic numerals unless the user explicitly allowed them.
- Confirm stripped structural residue such as `enddocument` or `nonumber` did not leak into the final script.
- Confirm LaTeX metadata such as `\label{...}` was not narrated as source labels or bookkeeping text.
- Confirm planning metadata such as `compact checkpoint notation`, `compact source signature`, or `render hint` did not leak into the final script.

## Validation

- Run `python3 scripts/validate-tts-script.py --plan plans/tts-sections.csv --checkpoints plans/tts-checkpoints.csv --lexicon plans/tts-lexicon.csv --out plans/tts-validation.csv`.
- Do not claim `validated` if the validator reports failures.
- Treat warnings as review items, not as automatic blockers, unless the user says otherwise.

## Acceptance Sample

- Build `plans/tts-acceptance-sample.csv`.
- Review the sampled sections fresh against the source.
- If the acceptance sample finds a defect pattern, return to repair mode and regenerate the affected sections.

## Whole Book

- For a whole-book run, build a manifest first and keep transient chapter-level artifacts disjoint under `plans/<source-stem>/`.
- Keep final script outputs disjoint under `scripts/<source-stem>/`.
- After the chapter runs finish, aggregate book-wide status into a single CSV so `ready-for-tts` can be checked at the book level rather than inferred informally.

## Cleanup

- Run cleanup only after deterministic validation, acceptance sampling, and any book-level aggregation have passed.
- Remove transient `plans/`, `codex-run.log`, and `codex-last-message.txt` artifacts before final handoff.
- Final durable outputs should live under `scripts/`.

## Audio

- Audio rendering is optional.
- TTS or STT round-trip should be used sparingly as a calibration surface, not as the default pipeline.
- The default success condition is a script that is textually ready for TTS.

## Final Claims

- Report `source-covered`, `validated`, `style-audited`, and `ready-for-tts` separately.
- Do not claim `ready-for-tts` until both deterministic validation and the acceptance sample are clean.
- Report cleanup status after transient artifacts have been removed.
