---
name: hifi-math-tts
description: High-fidelity conversion of technical LaTeX chapters and appendices into TTS-ready spoken scripts, using agent-driven rewriting with deterministic section planning, checkpoint extraction, and validation for equations, figures, captions, and algorithms. Use when turning equation-dense books, papers, or notes into audiobook-style narration without relying on iterative TTS/STT refinement loops.
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
- Include chapters and appendices.
- Do not include front matter, end-of-chapter problems or exercises, answers or solutions, bibliography or references, or index unless the user explicitly asks for them.
- Finish by validating readiness, reporting the status, and cleaning transient artifacts so the workspace keeps the generated scripts rather than planning scaffolding.

Do not ask the user to choose output paths unless the workspace layout is genuinely ambiguous or the user explicitly asks for a different directory.

## Completion Claims

Always report these separately:

- `source-briefed`: a source-local director's brief was written before generation.
- `source-covered`: every planned source section has an output script.
- `validated`: deterministic validation has no hard failures.
- `style-audited`: a fresh read-only source-to-script line audit has been
  completed on the sampled sections and recorded in a nonempty
  `tts-acceptance-review.md` or `tts-acceptance-review.csv` artifact.
- `ready-for-tts`: `source-briefed` + `source-covered` + `validated` + `style-audited`.

Do not treat “wrote some scripts” as equivalent to `ready-for-tts`.

## Core Design

- The generator is model-first: a capable agent reads the source and writes the spoken script a technically competent voice actor would read.
- Scripts are guardrails and orchestration only. Use them for section planning, source evidence packaging, coverage checks, residue checks, review sampling, and file-safety.
- Do not let checkpoint rows, regexes, or coverage artifacts become the prose strategy.
- Keep the skill book-agnostic.
- Adapt to book-local conventions dynamically from the source chapter, not by hardcoding one book's quirks into the skill.
- Preserve ordinary English prose with a minimal-touch policy by default.
- Require a source brief before generation. The brief captures what deterministic scripts cannot know: local mathematical texture, omission policy, notation semantics, pronunciation decisions, and equation narration strategy.
- Treat checkpoint artifacts as private manager-side control data, never as text to narrate.
- Use printed equation tags when present; do not invent replacement equation ordinals.
- Do not use ad-hoc generator scripts, pandoc converters, regex rewrite programs, or other bulk mechanical transforms to produce final spoken scripts.
- Only the skill's own planning, source-packet, audit, and validation scripts may be used programmatically. The final spoken text must be written by the model, section by section.

## Book-Scale Execution

For a whole book, do not treat the job as one monolithic rewrite.

- Discover the canonical chapter or appendix sources first.
- Process one canonical chapter or appendix source at a time.
- Give each chapter its own transient artifact directory under `plans/<source-stem>/`.
- Give each chapter its own final script directory under `scripts/<source-stem>/`.
- Appendices are fair game and should be processed like chapters.
- Front matter, end-of-chapter problems or exercises, answers or solutions, bibliography or references, and index are out of scope by default.
- Do not share a source brief blindly across the whole book. Write one per canonical source.
- Declare a chapter ready only after that chapter has a source brief, validates without hard failures, and passes its acceptance sample.
- For book-wide progress reporting, aggregate chapter-level statuses after the chapter runs complete, then clean up transient artifacts.
- Treat a missing acceptance-review artifact as not style-audited, even if an
  acceptance sample CSV exists.
- Scale by waves, not by serial hand drafting. For each source, build the
  complete section assignment list, dispatch as many non-overlapping native
  sub-agents as the runtime can reliably support, close completed agents, and
  continue dispatching the remaining rows until the source is complete. Then
  validate and acceptance-review that source before moving to the next manifest
  row.
- Do not flatten an entire book into a single unbriefed global worker queue.
  Chapter and appendix source briefs are source-local contracts, and keeping
  wave dispatch inside one source at a time prevents notation and omission
  policy from bleeding across chapters.

This keeps the workflow scalable without requiring the agent to hold an entire book in active context.

## Required Workflow

1. Inspect current workspace state and classify the job as fresh generation, audit/fix, or mixed.
2. If the workspace is book-scale, build a canonical source manifest first.
3. Build a deterministic section plan from the canonical `.tex`.
4. Extract fidelity checkpoints from the source.
5. Build a source evidence packet and write a model-authored source brief.
6. Dispatch one agent per non-overlapping output file or batch disjoint files safely.
7. Generate or repair spoken scripts section by section.
8. Run deterministic validation across all outputs.
9. Run a fresh acceptance sample review on representative sections, reading the source and script line by line.
10. Run the orchestration/readiness audit so serial fallback, missing review artifacts, and duplicate equation identifiers cannot be hidden by prose reports.
11. For a whole book, aggregate chapter-level status into a single deterministic report.
12. Clean transient planning, validation, and worker artifacts after readiness is confirmed.
13. Report completion claims and unresolved issues separately.

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

If the user explicitly asks to include normally excluded material such as end-of-chapter problems, pass `--include-out-of-scope`.

Use `assets/tts-section-plan-template.csv` as the reference shape.

Planning rules:

- Split on `\section`, `\subsection`, and `\subsubsection`.
- Keep the pre-first-section chapter intro as its own segment.
- Stop before end-of-chapter `Problems`, `Exercises`, `Answers`, `Solutions`, `Bibliography`, `References`, or `Index` blocks unless the user explicitly asks to include them.
- Use one output `.txt` per planned row by default.
- Do not let two agents write to the same target file.

After generating the plan and before dispatch, do a short boundary-coherence
pass over each planned boundary. Inspect the last paragraph before a heading and
the first visible objects after it. LaTeX source order can place figures,
tables, algorithms, or displayed equations away from the prose that introduces
them. If a boundary would make the concatenated audiobook flow incoherent, record
the attachment in the source brief and worker assignments. When needed, instruct
the worker to narrate the attached object before the new heading inside that
section file, or otherwise adjust the plan before checkpoint extraction.

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
- table captions
- algorithm identifiers

The purpose is not to force robotic wording. The purpose is to catch silent omission and notation drift.

After checkpoint extraction, inspect the equation identifiers for duplicate
printed numbers and source-local numbering conflicts, especially when the
source mixes inferred equation labels with explicit `\tag{...}` commands or
manual prose references such as `(9.101)`. Duplicate equation identifiers are a
blocking ambiguity: resolve the source-local printed-number contract in the
source brief before dispatch, or stop and report the blocker. Do not generate a
script that assigns the same spoken equation number to two different displayed
equations.

Also inspect the number-to-content mapping, not only the presence of spoken
numbers. A script that says `Equation twelve point five point nine` for the
content of source equation twelve point five point ten is a hard fidelity
failure even though both numbers appear somewhere in the section. Pay special
attention to semantic labels, unlabeled displayed equations, lettered
subequations, and books whose theorem, remark, definition, or problem numbers
share the same chapter prefix as equations. Lettered subequations must keep
their suffixes: do not collapse `(12.4.4a)`, `(12.4.4b)`, and `(12.4.4c)` into
a single base equation unless the printed source itself does so.

## Step 3: Build a Source Brief

Package source evidence for model review. This packet is a convenience surface for the agent writing the brief; it is not a conversion table:

```bash
python3 scripts/build-tts-source-packet.py \
  --tex transcriptions/chapter-20.tex \
  --plan plans/tts-sections.csv \
  --out plans/tts-source-packet.md
```

Then write a source brief:

```text
Use assets/source-brief-prompt-template.md with:
  source_tex=transcriptions/chapter-20.tex
  plan_file=plans/tts-sections.csv
  checkpoint_file=plans/tts-checkpoints.csv
  source_packet_file=plans/tts-source-packet.md
  source_brief_file=plans/tts-source-brief.md
```

The source brief is a required artifact. It should capture source-local narration policy, non-content omissions, semantic notation decisions, pronunciation choices, equation-speaking policy, source-local few-shot examples, and section-level risk notes. Do not generate final scripts until it exists.

## Step 4: Dispatch Parallel Work (Native Multi-agent)

Read `references/script-style-guide.md` before drafting prompts.
Read `references/quality-gate.md` before final signoff.

Codex multi-agent dispatch is handled by Codex, not by these scripts.

Use scripts only to prepare deterministic, non-overlapping section assignments. Then instruct Codex to spawn one sub-agent per assignment row or per safe batch of disjoint rows, wait for all results, and summarize status per agent.

Use `assets/master-dispatch-prompt-template.md` for the parent prompt and `assets/agent-task-prompt-template.md` for per-agent instructions.
Record worker assignment and completion status in `plans/tts-worker-assignments.csv`.
Use `assets/tts-worker-assignments-template.csv` as the reference shape.

Manager-side rules:

- Use native Codex sub-agent orchestration for multi-section chapters, appendices, and book runs.
- Spawn one sub-agent per target `.txt` file by default, unless batching several disjoint targets onto one agent is clearly more appropriate.
- For sources with more assignment rows than can be active reliably at once,
  dispatch in bounded waves. Keep every pending, active, completed, failed, and
  reassigned row recorded in `plans/tts-worker-assignments.csv`, and continue
  until every planned target has an owner and a completed output.
- Serial generation for a multi-section source is a workflow failure unless native sub-agent tooling is genuinely unavailable or errors before dispatch.
- Do not treat a generic tool-policy caution, missing preauthorization wording, or absence of the literal word `subagents` in the user's request as a blocker. Invoking this skill requests its required multi-agent workflow.
- If sub-agent tooling is genuinely unavailable, record the concrete blocker in `plans/tts-worker-assignments.csv` and the final report, then stop and report the blocker. Do not continue with serial local generation for a multi-section source unless the user explicitly directs a serial fallback after seeing the blocker.
- Never let two agents write to the same file.
- Record each assignment before dispatch with source range, target path, planned mode, and worker id when available.
- Update the assignment row when the worker finishes, stalls, fails, or is reassigned.
- Close completed or errored agents promptly before spawning replacements when subagents are used.
- If a worker fails, reassign only the unfinished section files.
- If the user requires a specific model or reasoning level for sub-agents, pass it explicitly and stop if it cannot be honored exactly.
- Use the runtime's native agent orchestration tools for sub-agents when they are available.
- Do not launch worker agents by shelling out to `codex exec`, `openai`, or similar CLI commands.
- Do not use `--dangerously-bypass-approvals-and-sandbox` or equivalent flags as part of this workflow.
- Do not write local helper programs to mass-convert LaTeX into spoken scripts. If such a script appears in the workspace, treat it as a workflow violation and discard that generation path.

## Step 5: Generate Spoken Scripts

Each agent should work from the canonical source lines for its assigned section and write plain text optimized for TTS.

Each agent must read:

- the source brief, usually `plans/tts-source-brief.md`

The checkpoint ledger is manager-side validation data. Do not ask workers to write from compact checkpoint rows or validation artifacts.

Hard rules:

- Preserve ordinary prose with minimal touch by default.
- Preserve technical meaning.
- Preserve all numbered equations, figures, captions, algorithms, tables, and lists in planned source spans.
- Do not include end-of-chapter problems or exercises unless the user explicitly asks for them.
- Do not silently drop difficult math.
- Preserve meaningful source cross-references in prose, such as `see Eq. ...`, unless the source brief explicitly marks that callout as non-content.
- Do not add conceptual explanation that is not in the source merely to make an equation sound more intelligible. Improve grouping and pacing; do not invent commentary.
- Do not emit raw LaTeX control sequences in the final script.
- Do not narrate LaTeX metadata such as `\label{...}` as source labels, bookkeeping labels, or internal identifiers.
- Spell out all numbers in the final script. Do not leave Arabic numerals in the output unless the user explicitly asks otherwise.
- Convert notation into natural spoken form while preserving symbols, indices, and dependencies.
- Prefer semantic spoken forms when the source brief or local source evidence makes the meaning clear. For example, speak Gaussian `\mathcal{N}` notation as `normal distribution` or `normally distributed with mean ... and covariance ...` when this source uses it that way, and speak `\mathbf{I}` as `identity matrix` when it denotes identity.
- Speak bra-ket expressions semantically when present. For example, `\langle f|U|i\rangle` is a matrix element of `U` between final state `f` and initial state `i`; do not produce token phrases such as `tensor final state`.
- Treat placeholder symbols such as `\bullet` or `\cdot` inside expectation notation as placeholders for the argument or quantity, not as literal objects to be narrated, unless the source is explicitly discussing the symbol itself.
- Keep expectation and cumulant subscripts attached to the expectation, not to the random variable. For example, `\langle A\rangle_c` is the connected or cumulant average of `A`, not `A sub c`.
- Preserve composite functional measures. For example, `\mathcal{D}\phi(x)` should remain the functional measure `D phi of x` or the functional measure over `phi of x`; do not reduce it to `functional measure phi of x`.
- Prefer listener-friendly volume measures when the variable is clearly a coordinate or momentum vector. For example, speak `\mathrm{d}^d x` as `d-dimensional x`, `\mathrm{d}^d q/(2\pi)^d` as `d-dimensional q over the quantity two pi raised to d`, and `\mathrm{d}^{d-1}\mathbf{x}` as `d minus one-dimensional vector x`; avoid mechanical forms such as `d to the d x` when a natural dimensional phrase is available.
- Speak simple braced indices directly. For example, `z_{t-1}` is `z sub t minus one`, not `z sub parenthesized t minus one`.
- Add breathing room with paragraph breaks and shorter sentences where helpful.
- Prefer locally adaptive narration over book-specific hardcoding.
- Omit standalone italic navigation or drill callouts such as `Exercise 20.1`, `Section 11.3`, or `Chapter 19` when they are only cross-reference markers in the chapter body. End-of-chapter problem statements are out of scope by default.
- Do not write or use custom local scripts, pandoc pipelines, or regex translators to generate the final `.txt` outputs. The spoken script itself must be authored by the model.

Important:

- This is not a summarization task.
- This is not a transcription task either.
- It is a fidelity-preserving adaptation into spoken technical prose.
- Ordinary English prose should usually survive almost verbatim.
- The target is polished narrated math, not a literal rendering of LaTeX tokens. A good output should sound like a careful human technical narrator who understands the notation.

Read `references/script-style-guide.md` before drafting or revising spoken text.
Read `references/prose-preservation-guide.md` when the source has long explanatory prose or when you need to decide how much rewriting is permissible.
Read `references/equation-speaking-guide.md` when the section contains dense or multiline equations.

Before a section is considered complete, do a local self-polish pass:

- read the generated script against the source span line by line, not from memory
- remove isolated non-content callouts that would sound like stray interruptions
- replace mechanical notation readings with semantic spoken math where appropriate
- improve equation readings that sound like token streams while preserving exact structure
- keep ordinary prose close to the source, but repair pacing and punctuation for speech

## Step 6: Validate Deterministically

After generation or repair:

```bash
python3 scripts/validate-tts-script.py \
  --plan plans/tts-sections.csv \
  --checkpoints plans/tts-checkpoints.csv \
  --source-brief plans/tts-source-brief.md \
  --out plans/tts-validation.csv
```

If the user explicitly requested normally excluded material and the plan was
created with `--include-out-of-scope`, pass `--allow-out-of-scope` here too.

Validation checks include:

- every planned output file exists and is non-empty
- heading/title coverage
- prose-anchor coverage for ordinary English paragraphs
- equation and algorithm identifiers preserved in speakable form
- figure captions not silently dropped
- default-out-of-scope rows such as problems, exercises, references, or index are absent unless explicitly allowed
- obvious raw LaTeX residue is absent
- standalone navigational callouts and mechanical notation residue are absent

Validation is a guardrail, not an oracle. Missing files, raw LaTeX, lost numbered artifacts, and residue are hard failures. Style and mathematical quality are decided by the source-to-script line audit.

Readiness review must also confirm that the source brief was created before generation and was used by the workers.

If validation fails:

- do not declare `validated`
- reassign only failing sections
- repair from source, not from guesswork or by appeasing validation strings

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

- read every source line in each sampled span and compare it to the corresponding script passage
- check whether the script sounds plausibly pleasant for TTS from text alone
- check whether the math survived intact
- check whether the narration feels over-literal or under-specified
- record concrete source-line and script-line evidence for every defect

Use `assets/acceptance-review-prompt-template.md` for ordinary acceptance review. Use `assets/line-by-line-audit-prompt-template.md` for calibration runs, skill-improvement loops, or any user request to audit quality in detail.

Write the review to `plans/tts-acceptance-review.md` or
`plans/tts-acceptance-review.csv` for a single-source run, or to the
corresponding source-local `plans/<source-stem>/tts-acceptance-review.*` file
for a book run. An acceptance sample without this review artifact is not
`style-audited`.

After validation and acceptance review, run the readiness/orchestration audit:

```bash
python3 scripts/audit-tts-run.py \
  --root . \
  --plans-dir plans \
  --out plans/tts-run-audit.csv
```

For a single source whose artifacts live directly in `plans/`, this command
audits that source. For a whole book with source-local plan directories, it
audits every `plans/<source-stem>/` directory. Hard audit issues block
`ready-for-tts`.

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
- whether the whole book is ready for TTS

The aggregate must require a nonempty acceptance-review artifact, not merely an
acceptance-sample CSV, before reporting `style-audited`.

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
- `source-briefed`, `source-covered`, `validated`, `style-audited`, and `ready-for-tts`

## When to Reach for References

- Read `references/script-style-guide.md` before drafting or revising spoken text.
- Read `references/prose-preservation-guide.md` when prose fidelity is likely to be the dominant risk.
- Read `references/equation-speaking-guide.md` when the section contains dense or multiline equations.
- Read `references/quality-gate.md` before signoff or when deciding whether a section needs repair.
