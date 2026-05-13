---
name: hifi-math-tts
description: High-fidelity conversion of technical LaTeX chapters into TTS-ready spoken scripts, using agent-driven rewriting with deterministic section planning, checkpoint extraction, and validation for equations, figures, algorithms, and exercises. Use when turning equation-dense books, papers, or notes into audiobook-style narration without relying on iterative TTS/STT refinement loops.
---

# HiFi Math TTS

Generate spoken scripts for technical material by keeping language generation agentic and keeping coverage, file-safety, and fidelity gates deterministic.

This skill is for source-to-script transformation, not audio rendering. The default goal is a script that is pleasant for TTS and faithful to the source before any audio is generated.

Default numeric policy:

- Spell out numbers everywhere in the final spoken script.
- Do not leave Arabic numerals in the output unless the user explicitly asks for them.
- This applies to section numbers, equation numbers, figure numbers, algorithm numbers, exercise numbers, years, dimensions, and enumerated steps.

## Inputs

- Canonical source `.tex` file or files.
- Output workspace path.
- Output script directory, usually under `scripts/`.
- Optional existing spoken scripts to audit/fix instead of regenerating.
- Optional user constraints for style, audience, scope, or exclusions.

## Default Invocation Behavior

If the user only says to use this skill to generate scripts for a TTS engine, treat that as a request for a full source-to-script run in the current workspace.

Default assumptions:

- The current working directory is the book or chapter workspace.
- If multiple canonical `.tex` sources are present, this is a whole-book run.
- Use `plans/` only as transient validation state during the run.
- Write final spoken scripts under `scripts/` at the workspace root.
- For whole-book runs, use `scripts/<source-stem>/section-XX-tts.txt`, one directory per source.
- Include front matter, chapters, appendices, answers or solutions, bibliography or references, and index when those files are present as canonical sources.
- Finish by validating readiness, reporting the status, and cleaning transient artifacts so the workspace keeps the generated scripts rather than planning scaffolding.

Do not ask the user to choose output paths unless the workspace layout is genuinely ambiguous or the user explicitly asks for a different directory.

## Completion Claims

Always report these separately:

- `source-covered`: every planned source section has an output script.
- `validated`: deterministic validation passes with no unresolved failures.
- `style-audited`: a fresh read-only acceptance review has been completed on the sampled sections.
- `ready-for-tts`: `source-covered` + `validated` + `style-audited`.

Do not treat “wrote some scripts” as equivalent to `ready-for-tts`.

## Core Design

- Use code for structure, planning, and validation.
- Use a capable agent for the actual rewriting into speakable technical prose.
- Keep the skill book-agnostic.
- Adapt to book-local conventions dynamically from the source chapter, not by hardcoding one book's quirks into the skill.
- Preserve ordinary English prose with a minimal-touch policy by default.
- Enforce chapter-wide notation consistency with a deterministic lexicon artifact.
- Treat checkpoint and lexicon artifacts as private control data, never as text to narrate.
- Use printed equation tags when present; do not invent replacement equation ordinals.
- Do not use ad-hoc generator scripts, pandoc converters, regex rewrite programs, or other bulk mechanical transforms to produce final spoken scripts.
- Only the skill's own planning, audit, and validation scripts may be used programmatically. The final spoken text must be written by the model, section by section.

## Book-Scale Execution

For a whole book, do not treat the job as one monolithic rewrite.

- Discover the canonical chapter or appendix sources first.
- Process one canonical chapter or appendix source at a time.
- Give each chapter its own transient artifact directory under `plans/<source-stem>/`.
- Give each chapter its own final script directory under `scripts/<source-stem>/`.
- Treat front matter, answers or solutions, bibliography or references, and index as book sources when discovered.
- Do not share a chapter lexicon blindly across the whole book. Rebuild it per chapter.
- Declare a chapter ready only after that chapter validates, has a clean lexicon audit, and passes its acceptance sample.
- For book-wide progress reporting, aggregate chapter-level statuses after the chapter runs complete, then clean up transient artifacts.

This keeps the workflow scalable without requiring the agent to hold an entire book in active context.

## Required Workflow

1. Inspect current workspace state and classify the job as fresh generation, audit/fix, or mixed.
2. If the workspace is book-scale, build a canonical source manifest first.
3. Build a deterministic section plan from the canonical `.tex`.
4. Extract fidelity checkpoints from the source.
5. Build a notation lexicon from the source and drive the lexicon audit to a clean result.
6. Dispatch one agent per non-overlapping output file or batch disjoint files safely.
7. Generate or repair spoken scripts section by section.
8. Run deterministic validation across all outputs.
9. Run a fresh acceptance sample review on representative sections.
10. For a whole book, aggregate chapter-level status into a single deterministic report.
11. Clean transient planning, validation, and worker artifacts after readiness is confirmed.
12. Report completion claims and unresolved issues separately.

## Step 0: Inspect Current State

Before writing anything:

- Identify the canonical source `.tex` file.
- If multiple candidate source files exist, determine whether the job is chapter-local, book-wide, or appendix-only.
- Identify whether existing spoken scripts already exist and whether the task is generation or repair.
- Treat hand-curated spoken scripts as a comparison or audit surface unless the user explicitly says they are the new source of truth.
- Do not assume one book's narration conventions apply to another. Infer local conventions from the current source.

If the workspace is book-wide, build a deterministic manifest first:

```bash
python3 scripts/discover-tts-sources.py \
  --root . \
  --out plans/tts-book-manifest.csv
```

Use `assets/tts-book-manifest-template.csv` as the reference shape.

For each manifest row:

- treat `source_tex` as the canonical source for that unit of work
- write chapter-local artifacts under the row's `plans_dir`
- write chapter-local scripts under the row's `script_dir`
- keep all chapter runs disjoint

## Step 1: Build a Section Plan

Generate a section plan before dispatch:

```bash
python3 scripts/make-tts-plan.py \
  --tex transcriptions/chapter-20.tex \
  --script-dir scripts/chapter-20 \
  --out plans/tts-sections.csv
```

This creates one output target per contiguous heading block, plus an intro block before the first numbered section when present.

Use `assets/tts-section-plan-template.csv` as the reference shape.

Planning rules:

- Split on `\section`, `\subsection`, and `\subsubsection`.
- Keep the pre-first-section chapter intro as its own segment.
- Use one output `.txt` per planned row by default.
- Do not let two agents write to the same target file.

## Step 2: Extract Source Checkpoints

Build a deterministic checkpoint ledger from the same source:

```bash
python3 scripts/extract-tts-checkpoints.py \
  --tex transcriptions/chapter-20.tex \
  --plan plans/tts-sections.csv \
  --out plans/tts-checkpoints.csv
```

Use `assets/tts-checkpoint-template.csv` as the reference shape.

Checkpoints are the high-value things that must survive the rewrite:

- spoken section/chapter headers
- prose anchors for ordinary English paragraphs
- numbered displayed equations
- figure captions
- algorithm identifiers
- exercise identifiers

The purpose is not to force robotic wording. The purpose is to catch silent omission and notation drift.

## Step 3: Build a Notation Lexicon

Build a chapter-wide notation lexicon before dispatch:

```bash
python3 scripts/build-tts-lexicon.py \
  --tex transcriptions/chapter-20.tex \
  --out plans/tts-lexicon.csv
```

Use `assets/tts-lexicon-template.csv` as the reference shape.

This artifact is for chapter-wide spoken consistency, not for book-specific hardcoding.

Use it to keep notation stable across all generated sections, for example:

- `\mathbf{x}` -> `vector x`
- `\mathbf{W}` -> `matrix w`
- `\mathcal{D}` -> `script d`
- `\hat{\imath}`, `\hat{\jmath}` -> `i hat`, `j hat`; never say `imath` or `jmath`
- `\vec{\imath}`, `\vec{\jmath}` -> `vector i`, `vector j`, unless the local context demands a semantic name such as `current density vector j`

If the current source clearly wants a different local convention, adjust the generated lexicon and then enforce that choice consistently.

Audit its coverage before dispatch:

```bash
python3 scripts/audit-tts-lexicon.py \
  --tex transcriptions/chapter-20.tex \
  --lexicon plans/tts-lexicon.csv \
  --out plans/tts-lexicon-audit.csv
```

Use `assets/tts-lexicon-audit-template.csv` as the reference shape.

Do not assume the lexicon is complete just because it exists.

- Review uncovered notation candidates.
- If uncovered candidates remain, expand the lexicon from the audit suggestions and rerun the audit.
- Do not dispatch generation while semantically meaningful notation candidates remain uncovered.
- Report the audit artifact path and uncovered count in the final report.

Auto-expansion step:

```bash
python3 scripts/expand-tts-lexicon.py \
  --lexicon plans/tts-lexicon.csv \
  --audit plans/tts-lexicon-audit.csv \
  --out plans/tts-lexicon.csv
```

Then rerun the audit:

```bash
python3 scripts/audit-tts-lexicon.py \
  --tex transcriptions/chapter-20.tex \
  --lexicon plans/tts-lexicon.csv \
  --out plans/tts-lexicon-audit.csv
```

The goal is a clean audit before section generation begins, not a manual note saying the operator compensated later.

## Step 4: Dispatch Parallel Work

Read `references/script-style-guide.md` before drafting prompts.
Read `references/quality-gate.md` before final signoff.

Use Codex multi-agent orchestration when the plan has multiple disjoint targets.

Use `assets/master-dispatch-prompt-template.md` for the parent prompt and `assets/agent-task-prompt-template.md` for per-agent instructions.

Manager-side rules:

- One agent per target `.txt` file unless batching several disjoint targets onto one agent.
- Never let two agents write to the same file.
- Close completed or errored agents promptly before spawning replacements.
- If a worker fails, reassign only the unfinished section files.
- If the user requires a specific model or reasoning level for sub-agents, pass it explicitly and stop if it cannot be honored exactly.
- Use the runtime's native agent orchestration tools for sub-agents when they are available.
- Do not launch worker agents by shelling out to `codex exec`, `openai`, or similar CLI commands.
- Do not use `--dangerously-bypass-approvals-and-sandbox` or equivalent flags as part of this workflow.
- If native sub-agent tooling is unavailable, process the work locally in smaller sequential batches or report the orchestration blocker; do not create hidden shell-managed Codex sessions.
- Do not write local helper programs to mass-convert LaTeX into spoken scripts. If such a script appears in the workspace, treat it as a workflow violation and discard that generation path.

## Step 5: Generate Spoken Scripts

Each agent should work from the canonical source lines for its assigned section and write plain text optimized for TTS.

Hard rules:

- Preserve ordinary prose with minimal touch by default.
- Preserve technical meaning.
- Preserve all numbered equations, figures, algorithms, and exercises in scope unless the user explicitly excludes them.
- Do not silently drop difficult math.
- Do not emit raw LaTeX control sequences in the final script.
- Do not narrate LaTeX metadata such as `\label{...}` as source labels, bookkeeping labels, or internal identifiers.
- Spell out all numbers in the final script. Do not leave Arabic numerals in the output unless the user explicitly asks otherwise.
- Convert notation into natural spoken form while preserving symbols, indices, and dependencies.
- Add breathing room with paragraph breaks and shorter sentences where helpful.
- Prefer locally adaptive narration over book-specific hardcoding.
- Do not write or use custom local scripts, pandoc pipelines, or regex translators to generate the final `.txt` outputs. The spoken script itself must be authored by the model.

Important:

- This is not a summarization task.
- This is not a transcription task either.
- It is a fidelity-preserving adaptation into spoken technical prose.
- Ordinary English prose should usually survive almost verbatim.

Read `references/script-style-guide.md` before drafting or revising spoken text.
Read `references/prose-preservation-guide.md` when the source has long explanatory prose or when you need to decide how much rewriting is permissible.
Read `references/equation-speaking-guide.md` when the section contains dense or multiline equations.

## Step 6: Validate Deterministically

After generation or repair:

```bash
python3 scripts/validate-tts-script.py \
  --plan plans/tts-sections.csv \
  --checkpoints plans/tts-checkpoints.csv \
  --lexicon plans/tts-lexicon.csv \
  --out plans/tts-validation.csv
```

Validation checks include:

- every planned output file exists and is non-empty
- heading/title coverage
- prose-anchor coverage for ordinary English paragraphs
- equation, algorithm, and exercise identifiers preserved in speakable form
- figure captions not silently dropped
- forbidden notation aliases absent across the chapter
- obvious raw LaTeX residue is absent

If validation fails:

- do not declare `validated`
- reassign only failing sections
- repair from source, not from guesswork

## Step 7: Run an Acceptance Sample

Build a deterministic acceptance sample:

```bash
python3 scripts/select-acceptance-sections.py \
  --plan plans/tts-sections.csv \
  --checkpoints plans/tts-checkpoints.csv \
  --out plans/tts-acceptance-sample.csv
```

The sampler sizes itself automatically by default:

- baseline chapters sample 5 sections
- denser or longer chapters sample 6 or 7 sections
- use `--count` only when the user explicitly wants a different sample size

Review the sampled sections fresh and read-only against the source:

- check whether the script sounds plausibly pleasant for TTS from text alone
- check whether the math survived intact
- check whether the narration feels over-literal or under-specified

Audio rendering is optional and should be used sparingly as a calibration surface, not as the default execution loop.

## Step 8: Aggregate Whole-Book Status

For a whole-book run, finish by aggregating chapter-level status:

```bash
python3 scripts/aggregate-tts-book-results.py \
  --root . \
  --manifest plans/tts-book-manifest.csv \
  --out plans/tts-book-status.csv
```

Use `assets/tts-book-status-template.csv` as the reference shape.

This aggregate should be the final deterministic answer to questions like:

- how many chapters are actually ready
- which chapters still fail validation
- which chapters still have uncovered notation
- whether the whole book is ready for TTS

## Step 9: Final Cleanup

After the single source or whole book is confirmed ready, remove transient run artifacts:

```bash
python3 scripts/cleanup-tts-run.py \
  --root . \
  --scripts-dir scripts \
  --plans-dir plans
```

Cleanup rules:

- Run cleanup only after validation and acceptance sampling are complete and the result is ready.
- `plans/`, worker logs, and last-message files are transient and should not remain in the final workspace.
- The final durable output is `scripts/`.
- If cleanup fails because `scripts/` is missing or empty, stop and report the blocker rather than deleting anything.
- After cleanup, do not claim that validation artifacts still exist on disk; report the validation status and counts from the completed run.

## Step 10: Final Report

Report:

- source file and output directory
- section count
- per-agent assignments and status
- validation status and acceptance-sample status
- cleanup status
- unresolved failures or warnings
- `source-covered`, `validated`, `style-audited`, and `ready-for-tts`

## When to Reach for References

- Read `references/script-style-guide.md` before drafting or revising spoken text.
- Read `references/prose-preservation-guide.md` when prose fidelity is likely to be the dominant risk.
- Read `references/equation-speaking-guide.md` when the section contains dense or multiline equations.
- Read `references/quality-gate.md` before signoff or when deciding whether a section needs repair.
