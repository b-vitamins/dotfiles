# Multi-agent Notes

Use this reference when dispatching transcription in parallel. Codex and Claude
Code both provide native sub-agents; use the mechanism of whichever runtime is
executing this skill.

## Runtime Behavior

- The runtime handles sub-agent lifecycle: spawn, route follow-ups, wait, consolidate.
- Parent prompts can explicitly request spawn-per-assignment behavior.
- Sub-agents inherit the parent's sandbox or permission policy.
- Sub-agents cannot obtain new approvals interactively. Pre-approve what workers
  need, or expect the action to fail and the error to surface to the parent.

## Codex

- In CLI: `/experimental` -> enable `Multi-agents`, then restart.
- Or set in config:

```toml
[features]
multi_agent = true
```

- Configure roles in `[agents]` in `~/.codex/config.toml` or project `.codex/config.toml`.
- Common built-ins: `default`, `worker`, `explorer`.
- Use role `config_file` for model/sandbox/developer-instruction overrides.

## Claude Code

- Spawn sub-agents with the Agent tool. Several Agent calls in one message run
  concurrently.
- Sub-agent types play the part of Codex roles: built-ins such as
  `general-purpose`, or custom agents in `~/.claude/agents/` or project
  `.claude/agents/`.
- Set the worker model with the Agent tool's `model` parameter or the custom
  agent's `model` frontmatter.
- Pre-approve the commands workers run (permission rules in `settings.json`) so
  they do not stall on permission prompts.

## Recommended Pattern for Transcription

1. Parent agent renders pages and prepares assignment CSV.
2. Parent agent spawns one sub-agent per assignment row.
3. Each sub-agent writes only its assigned output file(s).
4. Parent agent waits for completion and consolidates compile/report output.

## Manager Controls

- Keep write scopes disjoint. If two rows would write the same `target_tex`, bundle them onto one agent or use shard files.
- Respect any explicit model/reasoning requirements from the user. If the requested model cannot be forced, stop instead of silently degrading.
- Cap live agents by both platform concurrency and disjoint write ownership.
- Close completed or errored agents promptly before spawning more.
- If a worker fails, reuse only the unfinished suffix of the assignment, ideally in a smaller chunk.
- Persist the marker-verification result to a file under `plans/` before marker cleanup.
- Do not report full fidelity completion until a second image-based audit/fix pass has covered all touched pages, unresolved numbered-figure gaps are tracked, and a fresh acceptance sample has passed.
