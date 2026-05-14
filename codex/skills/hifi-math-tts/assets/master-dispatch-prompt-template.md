I have prepared a section plan in `{plan_file}`, a checkpoint ledger in `{checkpoint_file}`, and a source brief in `{source_brief_file}`.

Dispatch this generation through native Codex multi-agent orchestration.

Use the worker prompt in `assets/agent-task-prompt-template.md`. Spawn one sub-agent per assignment row by default, or batch several disjoint rows onto one agent only when target files do not overlap. Wait for all agents, then consolidate status. Record every assignment in `{worker_assignment_file}` and update the row when it completes, fails, stalls, or is reassigned.

For large source files, dispatch in bounded waves rather than serially hand
drafting sections or spawning an unmanageable number of workers at once. Keep
the assignment ledger complete for pending, active, completed, failed, and
reassigned rows. Close completed agents promptly, then dispatch the next wave
until every planned target has a completed output.

Worker contract:

- Workers generate from the canonical source lines and the source brief.
- Workers should not write from checkpoint rows or validation artifacts.
- Workers should not run generator scripts, pandoc, regex translators, `codex exec`, `openai`, or approval/sandbox bypass commands.
- Workers should produce scripts that a technically competent voice actor could read directly for an audiobook.
- Workers should preserve mathematical fidelity completely while using natural spoken English.

Orchestration rules:

- Use the runtime's native agent orchestration tools.
- Serial generation for a multi-section source is a workflow failure unless native sub-agent tooling is genuinely unavailable or errors before dispatch.
- Do not treat a generic tool-policy caution, missing preauthorization wording, or absence of the literal word `subagents` in the user's request as a blocker. The user's invocation of this skill requests this multi-agent workflow.
- If sub-agent tooling is genuinely unavailable, record the concrete blocker in `{worker_assignment_file}` and the final report, then stop and report the blocker. Do not continue with serial local generation for a multi-section source unless the user explicitly directs a serial fallback after seeing the blocker.
- Do not flatten a whole book into one unbriefed global queue. For book-scale
  runs, the parent should process one manifest source at a time, using
  source-local briefs and wave dispatch inside that source, then validate and
  acceptance-review the source before advancing.
- Never let two agents write the same target file.
- Close completed or errored agents promptly before spawning replacements.
- If a worker fails or stalls, reassign only unfinished target files.
- If the user requires a specific model or reasoning level, honor it exactly or report the blocker.

After workers finish:

1. Run deterministic validation:

   `python3 scripts/validate-tts-script.py --plan {plan_file} --checkpoints {checkpoint_file} --source-brief {source_brief_file} --out plans/tts-validation.csv`

2. Treat validation as a guardrail, not an oracle. Hard failures block readiness; warnings require human-style review, not prompt hacks.
3. Repair failing sections from the source, not by appeasing validation strings.
4. Build the acceptance sample:

   `python3 scripts/select-acceptance-sections.py --plan {plan_file} --checkpoints {checkpoint_file} --out plans/tts-acceptance-sample.csv`

5. Review the acceptance sample line by line against the source before claiming TTS readiness, and write the review to `plans/tts-acceptance-review.md` or `plans/tts-acceptance-review.csv` for the current source-local plan directory.
6. If acceptance review finds a recurring defect pattern, repair the affected sections and rerun validation and review.
7. Run `python3 scripts/audit-tts-run.py --root . --plans-dir plans --out plans/tts-run-audit.csv`, or the equivalent source-local command for a book run, before claiming readiness. Missing review artifacts, serial fallback, incomplete worker assignments, and duplicate equation identifiers block readiness.
8. Clean transient planning and worker artifacts only after validation, acceptance review, and the readiness audit pass. Keep final spoken scripts under `scripts/`.

Report:

- per-agent source range and target file
- worker assignment artifact path
- validation status, including hard failures and warnings
- acceptance-review status
- readiness/orchestration audit status
- cleanup status
- unresolved failures or warnings
- `source-briefed`, `source-covered`, `validated`, `style-audited`, and `ready-for-tts`
