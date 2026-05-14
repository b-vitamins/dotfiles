Perform a source-to-script line audit for a TTS generation run.

Use this prompt for calibration runs, quality-improvement loops, or any case where the user wants maximum diagnostic signal.

Inputs:

- Source TeX: `{source_tex}`
- Section plan: `{plan_file}`
- Checkpoint ledger: `{checkpoint_file}`
- Source brief: `{source_brief_file}`
- Script directory: `{script_dir}`
- Sections to audit: `{section_filter}`
- Output audit report: `{audit_report_file}`

Audit method:

1. Read the source brief first.
2. For each requested section, read the exact source line range from the section plan.
3. Number the corresponding generated script lines.
4. Walk the source in order, line by line or small source spans where a paragraph or displayed equation spans multiple lines.
5. For each source span, identify the corresponding script line span.
6. Classify the mapping as `faithful`, `acceptable-adaptation`, `omission`, `fidelity`, `notation`, `listenability`, `invention`, `coverage`, or `validator-appeasement`.
7. Record concrete source-line and script-line references for every non-faithful mapping.
8. Do not infer quality from deterministic validation. Validation only tells you what to inspect first.

High-signal failure modes to look for:

- ordinary prose unnecessarily paraphrased away from the source
- meaningful cross-references dropped from prose
- shifted equation numbering, where a spoken equation number is attached to
  the content of a neighboring source equation
- lettered subequations collapsed into a bare base number, or bare theorem,
  remark, definition, proposition, example, exercise, or problem numbers used
  as if they advanced the equation counter
- equations flattened into token streams
- denominators mentioned in detached validator-appeasement fragments
- invented conceptual commentary not present in the source
- placeholders such as `\bullet` narrated literally instead of as an argument
- bra-ket notation narrated as token order instead of as a matrix element between states
- absolute-value powers scoped ambiguously, such as `absolute value of phi squared`
- fraction boundaries made ambiguous when a fraction is followed by a separate multiplicative factor
- connected or cumulant expectations treated as if the variable has subscript `c`
- functional measures losing the `D` or the field
- volume measures such as `\mathrm{d}^d x` or `\mathrm{d}^d q/(2\pi)^d` rendered as mechanical TeX phrases like `d to the d x` when a natural dimensional measure phrase is available
- glossary-like source-brief entries applied as blind substitutions that erase adjacent notation
- mechanical expansion artifacts introduced by applying source-brief notation wording blindly instead of integrating it into the surrounding sentence
- negative or multi-token bases with fractional powers flattened so the listener cannot tell what the exponent applies to
- section-boundary incoherence where a figure, table, algorithm, display, or caption is separated from the prose that introduces it because of LaTeX placement, causing the concatenated audiobook to read in the wrong order
- tables, captions, algorithms, or list items skipped or collapsed

Write `{audit_report_file}` as Markdown with:

1. `Executive Finding`
   - One paragraph stating whether the scripts are ready, repairable, or fundamentally below target.

2. `Section Results`
   - One subsection per audited section.
   - Include source range, script file, and pass/fail.
   - Include a table with columns: `source lines`, `script lines`, `classification`, `finding`, `repair instruction`.

3. `Cross-Section Failure Patterns`
   - Group recurring problems by root cause rather than by isolated sentence.

4. `Skill Improvements Suggested`
   - List prompt, source-brief, validation, or workflow changes that would prevent the observed failure patterns.

5. `Regeneration Targets`
   - List the sections that must be regenerated before claiming TTS readiness.

Be strict. A pass means the script is a high-quality substrate for a TTS engine without later human cleanup.
