---
name: remote-fleet-operator
description: Operate remote machines, pools, and project-bound compute environments through fleetctl. Use when the task involves running experiments on named machines, syncing repos to remote hosts, staging scripts, submitting jobs to schedulers, inspecting fleet config, importing SSH aliases, or migrating repo-local remote settings into the private home-local fleet inventory. Prefer this over ad hoc raw SSH whenever the host should come from the user's managed fleet.
---

# Remote Fleet Operator

Use `fleetctl` as the control plane for remote work.

## Goals

- Keep host and login data out of version-controlled repos.
- Reuse one global fleet inventory across projects.
- Avoid brittle shell quoting and repo-local SSH wrappers.
- Make remote actions boring: inspect, smoke test, sync, execute, submit.
- Keep sensitive connection material out of target metadata.
- Put each host's base `workdir` on the target itself.
- Use project bindings only for defaults and for project-specific absolute
  overrides when `workdir/<project-name>` is not correct.
- Encode site rules in reusable private protocol files under
  `~/.config/fleet/protocols.d/`, then bind targets to those protocols instead
  of teaching host-specific quirks inline.

## First checks

Run these before changing anything substantial:

```bash
fleetctl doctor
fleetctl list
fleetctl project show
```

If the current project is not bound, inspect the available targets or pools and
either use an explicit target or bind the project.

When a target may be scheduler-backed or otherwise policy-constrained, inspect:

```bash
fleetctl protocol show <target-or-pool>
fleetctl queue list <target-or-pool>
```

## Preferred workflow

1. Resolve context.
   Use `fleetctl project show`, `fleetctl resolve`, or `fleetctl show`.
2. Inspect protocol.
   Use `fleetctl protocol show` to decide whether the target is direct or
   scheduler-backed, whether the expected job runtime is host-side or Docker,
   and use `fleetctl queue list` when queue choice matters.
3. Verify transport.
   Run `fleetctl smoke <target-or-pool>` unless the user explicitly wants a
   direct attempt first.
4. Sync code when needed.
   Use `fleetctl sync push ...`.
5. Run the work through the narrowest interface that fits:
   - `fleetctl exec <target> -- ...` for direct-host argv execution
   - `fleetctl exec --login <target> -- ...` for administrative commands on a
     scheduler login surface
   - `fleetctl script <local-script> --target <target> -- ...` for staged
     scripts on direct hosts
   - `fleetctl submit <local-script> --target <target>` for compute jobs,
     especially when the resolved protocol is scheduler-backed
   - `fleetctl submit --native-batch <local-script> --target <target>` when a
     site expects a fully native scheduler script and `fleetctl` should not wrap
     the payload
6. Inspect jobs with `fleetctl job status`, `fleetctl job logs`, and
   `fleetctl job cancel`.

## Configuration tasks

When the user wants to bring hosts into the fleet:

- Import an existing SSH alias with `fleetctl import-ssh <alias>`.
- Migrate env-based machine config with
  `fleetctl migrate-env --env-file <path> --prefix <PREFIX>`.
- Define reusable protocols in `~/.config/fleet/protocols.d/*.toml` and point
  targets at them with `protocol = "name"`.
- Bind a project with `fleetctl project bind <path> --target <name>` or
  `--pool <name>`.
- When one project needs an absolute path that does not follow
  `workdir/<project-name>` on some hosts, use repeated
  `--target-root target=/remote/path` entries on the project binding.

## Transport rules

- Prefer `fleetctl` over raw `ssh` for any host that belongs in the managed
  fleet.
- Prefer `fleetctl exec` over `ssh <host> 'bash -lc ...'` when the user payload
  is naturally an argv vector on direct hosts.
- On scheduler-backed protocols, treat `fleetctl exec --login` as control-plane
  access only. Use `fleetctl submit` for compute.
- When a protocol reports `job_runtime = "docker"`, make the submitted script
  launch the workload inside the allocated container runtime instead of assuming
  host-side execution is the correct path.
- Use `--native-batch` when the site expects the scheduler script itself to
  contain the runtime setup or policy-specific directives.
- Prefer `fleetctl script` or `fleetctl submit` when the command is large enough
  that quoting would become fragile.
- Use raw `ssh` only for transport debugging or when `fleetctl` genuinely lacks
  the needed primitive.

## Privacy rules

- Do not write sensitive host or login information into repo files.
- Keep secrets under `~/.config/fleet/secrets/` or the configured secret
  backend.
- When reporting fleet state back to the user, avoid echoing sensitive host
  details unless they explicitly asked for them.
