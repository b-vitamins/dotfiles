I have prepared a spoken-script section plan in `{plan_file}`, a source checkpoint ledger in `{checkpoint_file}`, a chapter-wide notation lexicon in `{lexicon_file}`, and a lexicon coverage audit in `{lexicon_audit_file}`.

Spawn one sub-agent per assignment row, or batch several rows onto one agent only when the target files are disjoint.

Use the runtime's native agent orchestration tools for these sub-agents. Do not launch workers by shelling out to `codex exec`, `openai`, or similar CLI commands, and do not use approval or sandbox bypass flags. If native sub-agent tooling is unavailable, process smaller batches locally or report the blocker instead of creating shell-managed Codex sessions.

For each sub-agent:

- Use the role `{agent_role}` unless a better configured role is available.
- Honor any explicit user-specified model and reasoning requirements exactly. If they cannot be satisfied, stop and report the blocker.
- Restrict work to the assigned source line range and target output file.
- Generate spoken technical prose from the canonical source rather than paraphrasing from memory.
- Preserve ordinary English prose with minimal touch unless a local passage genuinely needs heavier adaptation for speech.
- Preserve numbered equations, captions, algorithms, and exercises that are in scope.
- Spell out all numbers in the final script unless the user explicitly says otherwise.
- Follow the notation lexicon exactly and keep spoken symbol naming consistent across sections.
- Use explicit printed `\tag{...}` values as equation identifiers, resolve source cross-references before speaking them, and never narrate `source tag`.
- Render dotless `\imath` and `\jmath` notation as `i hat`, `j hat`, `vector i`, or `vector j` according to context; never emit `imath` or `jmath`.
- Speak adjacent-letter indices as separate letters and speak every mathematical row in multiline displays; do not use `paired with the preceding line` or `companion relation` as a substitute for a row.
- If the lexicon audit shows uncovered notation candidates in your assigned span, resolve them conservatively and report them to the parent.
- If the source appears internally inconsistent or typoed, preserve it conservatively and report it. Do not silently harmonize conflicting symbols or parameters.
- Do not emit raw LaTeX in the final script.
- Do not emit planning or validation metadata such as checkpoint notation, compact source signatures, or render hints.
- Do not write or use local generator scripts, pandoc conversions, or regex mass-rewrite helpers to produce the spoken scripts. Only the skill's planning and validation scripts may be used programmatically.

Wait for all sub-agents to finish.
Close completed or errored agents before spawning replacements.
If an agent fails or stalls, reassign only the unfinished target files.

After all sub-agents finish:

- Run `python3 scripts/validate-tts-script.py --plan {plan_file} --checkpoints {checkpoint_file} --out plans/tts-validation.csv`.
- Run `python3 scripts/validate-tts-script.py --plan {plan_file} --checkpoints {checkpoint_file} --lexicon {lexicon_file} --out plans/tts-validation.csv`.
- Repair only failing sections.
- Build a fresh acceptance sample with `python3 scripts/select-acceptance-sections.py --plan {plan_file} --checkpoints {checkpoint_file} --out plans/tts-acceptance-sample.csv`.
- Review the acceptance sample before claiming the result is ready for TTS.
- If `lexicon_audit_file` contains uncovered candidates, expand the lexicon from those suggestions and rerun the audit before generation is considered final.
- After validation and acceptance pass, clean transient planning and worker artifacts. Keep the final spoken scripts under `scripts/`; do not leave `plans/`, `codex-run.log`, or `codex-last-message.txt` as durable outputs.

Then provide a consolidated report with:

- per-agent source range and target file
- completion status
- validation status
- acceptance-sample status
- cleanup status
- unresolved failures or warnings
- source-covered vs validated vs style-audited vs ready-for-tts
