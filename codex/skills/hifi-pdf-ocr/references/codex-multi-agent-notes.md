# Codex Multi-agent Notes

Use this reference when dispatching transcription in parallel.

## Runtime Behavior

- Codex handles sub-agent lifecycle: spawn, route follow-ups, wait, consolidate.
- Parent prompts can explicitly request spawn-per-assignment behavior.
- Sub-agents inherit parent sandbox policy.
- Sub-agents run with non-interactive approvals.
- If a sub-agent action needs new approval, it fails and error is surfaced to parent.

## Enablement

- In CLI: `/experimental` -> enable `Multi-agents`, then restart.
- Or set in config:

```toml
[features]
multi_agent = true
```

## Roles

- Configure roles in `[agents]` in `~/.codex/config.toml` or project `.codex/config.toml`.
- Common built-ins: `default`, `worker`, `explorer`.
- Use role `config_file` for model/sandbox/developer-instruction overrides.

## Recommended Pattern for Transcription

1. Parent agent renders pages and prepares assignment CSV.
2. Parent agent asks Codex to spawn one sub-agent per assignment row.
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
