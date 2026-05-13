# Script Style Guide

Use this when generating or reviewing spoken scripts for technical source material.

## Goal

Produce text that a TTS engine can read as a pleasant technical audiobook while preserving the source meaning with high fidelity.

## General Rules

- Prefer natural spoken prose over literal punctuation reading.
- Prefer shorter sentences than the source when this improves listenability.
- Preserve the logic and informational order of the source.
- Do not turn the script into a summary.
- Do not hardcode one book's quirks into the workflow. Adapt to local source conventions.
- Spell out all numbers in the final script by default. This is the safe default for sensitive TTS engines such as Chatterbox.
- Preserve ordinary English prose with minimal touch by default.
- Keep chapter-wide notation naming stable from the first section to the last.

## Prose Preservation

- Treat ordinary explanatory English as canonical wording.
- Default operation: keep the wording, keep the order, and change only what speech requires.
- Allowed changes:
  - split long sentences
  - smooth punctuation for speaking
  - expand abbreviations or symbols into speech
  - insert brief lead-ins before dense equations
- Disallowed changes unless the source is genuinely awkward or ambiguous:
  - broad paraphrasing
  - rewriting a paragraph into your own explanation
  - deleting repeated emphasis that the author intentionally used
- A good rule: if a paragraph is mostly English with only mild inline math, the spoken script should still read recognizably like the source paragraph.

## Source-Local Anomalies

- Some books contain typos, inconsistent variable names, impossible ranges, or half-repaired errata.
- Do not silently clean these up in the spoken script unless the user supplied an explicit errata surface or asked for normalization.
- Default behavior:
  - preserve the local wording conservatively
  - keep the questionable symbol or range in spoken form
  - report the issue separately for review
- If a source-local issue makes the sentence literally unreadable, make the smallest possible speech-level repair and report exactly what was ambiguous.

## Naming Consistency

- Build and follow a chapter-wide spoken notation lexicon.
- Once a symbol gets a spoken name, keep it stable everywhere in the chapter.
- Examples:
  - `\mathbf{x}` should not alternate between `vector x`, `bold x`, and `x in bold`
  - `\mathcal{D}` should not alternate between `script d` and `calligraphic d`
  - a matrix should not become a vector later unless the source notation changes
- Prefer source-local consistency over generic habits.

## Notation-Heavy Chapters and Appendices

- Some chapters are not primarily expository prose. They may be notation summaries, appendices, glossary tables, or long derivations with only short connective text.
- In these sections, preserve the local pedagogical function rather than forcing normal paragraph narration.
- If the source is a notation table or glossary:
  - convert each row into a short spoken statement
  - prefer forms like `Symbol ... means ...`
  - keep the order of entries
  - do not merge many entries into a single paragraph
- If the source is an appendix proving a result step by step:
  - preserve the proof structure
  - keep local references like `first`, `next`, `finally`, `under this condition`
  - keep definitions and intermediate identities explicit

## Numbers

- Do not leave Arabic numerals in the final script unless the user explicitly asks for them.
- Spell out:
  - section, subsection, and chapter numbers
  - equation, figure, algorithm, and exercise numbers
  - years
  - dimensions such as `64 x 64`
  - counts, ranges, and step numbers
- Prefer forms such as:
  - `Equation twenty point forty-one`
  - `Figure twenty point eight`
  - `two thousand twenty-one`
  - `sixty-four by sixty-four`
  - `Step two point three`

## Headings

- Introduce chapter and section headings explicitly.
- For numbered sections, prefer spoken forms such as `Section twenty point two point four.`
- Say the title on its own line or in its own short paragraph when that improves pacing.

## Equations

- Introduce displayed equations explicitly, e.g. `Equation twenty point forty-one.`
- Speak the equation in a way that preserves structure and symbols.
- Keep symbol names stable. If the source uses `vector z sub t`, do not drift to a different symbol.
- For dense equations, add grouping language so the listener can recover structure without seeing the page.
- Preserve distinctions such as:
  - `beta_1` vs `beta_t`
  - `z_t` vs `z_{t-1}`
  - conditioning bars
  - norms, expectations, products, and summations
- Preserve decorations when they carry meaning:
  - hats
  - underlines
  - calligraphic or fraktur symbols
  - daggers or stars
- Do not read raw LaTeX commands.

## Figures and Captions

- If a figure caption carries teaching content, narrate it explicitly.
- A good default is `The caption of Figure ... reads:`
- Preserve substantive caption content faithfully.
- If the source references a figure in prose, keep the figure reference in the spoken script.

## Algorithms

- Treat boxed algorithms as procedural content, not decorative text.
- Preserve:
  - algorithm number and title
  - inputs and outputs
  - loop structure
  - step ordering
  - mathematical assignments
- Break long lines into readable spoken steps.

## Exercises

- Include exercises by default unless the user explicitly excludes them.
- Preserve exercise numbering in speakable form.
- Keep the problem statement faithful.

## Citations and Cross-References

- Keep citations in speakable form when they matter to the explanation.
- Keep cross-references such as `Figure 20.8`, `Equation 20.33`, or `Section 14.3`.

## Tables and Lists

- When the source uses tables to define notation, avoid flattening them into prose soup.
- Prefer one short spoken line per row or per logically grouped row.
- For bulleted or enumerated derivation cases, keep the list structure in spoken form.

## Listenability Heuristics

- Add paragraph breaks to create breathing room.
- Avoid overpacked sentences full of commas and parentheticals.
- When a formula is dense, prefer one lead-in sentence plus one or more short lines for the verbalized expression.
- Use repetition sparingly and only when it reduces ambiguity.

## What Not to Do

- Do not emit raw LaTeX syntax in the final script.
- Do not leave Arabic numerals in the final script unless the user explicitly asks for them.
- Do not paraphrase a difficult equation into a vague explanation.
- Do not silently drop captions, exercises, or algorithms.
- Do not rewrite ordinary prose more than necessary for speech.
- Do not rename notation casually across sections.
- Do not overfit to one book's narration style if the current source wants a different local treatment.
