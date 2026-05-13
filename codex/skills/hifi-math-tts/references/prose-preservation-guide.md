# Prose Preservation Guide

Use this when the source section is mostly explanatory English with only light inline math.

## Goal

Keep the author's prose recognizable while still making it pleasant for TTS.

The failure mode to avoid is unnecessary rewriting: the script becomes smooth but no longer feels like the source chapter.

## Default Policy

- Preserve ordinary prose nearly verbatim.
- Prefer sentence splitting over paraphrasing.
- Prefer punctuation smoothing over word substitution.
- Preserve informational order.

## Allowed Edits

- Split long sentences into two or more spoken sentences.
- Replace punctuation that sounds bad in TTS with short connective phrasing.
- Expand inline notation into spoken form when needed.
- Add a short orienting sentence before a dense displayed equation or algorithm.

## Disallowed Edits

- Rewriting a paragraph into your own explanation just because it sounds nicer.
- Reordering ideas for style alone.
- Dropping caveats, comparisons, or authorial emphasis.
- Replacing a precise textbook sentence with vague paraphrase.

## Practical Heuristic

For a paragraph with no displayed equation, caption, or algorithm:

- the first several words should usually survive unchanged
- key nouns and verbs should usually survive unchanged
- the spoken version should feel like the same paragraph with better pacing, not a new paragraph

## Inline Math

For light inline math embedded in prose:

- keep the surrounding sentence wording stable
- only expand the mathematical fragment into speech
- do not rewrite the whole sentence just because it contains symbols
- avoid duplicate descriptors when prose already names the object type; `the position vector \(\vec r(t)\)` should become `the position vector r of t`, not `the position vector vector r of t`

## When to Deviate More Aggressively

More aggressive adaptation is justified when:

- the source sentence is extremely long and would sound breathless
- punctuation or parentheses create TTS confusion
- the source is transitioning into a dense displayed derivation
- the source uses notation in a way that must be made explicit for listening

Even then, preserve the original informational content and order.
