# fleetctl - Private Remote Fleet Control

Run remote work through a private host inventory that lives under your home
directory instead of scattering machine definitions across individual repos.

## Overview

`fleetctl` keeps host metadata, secrets, project bindings, and SSH runtime state
in XDG paths:

- `~/.config/fleet/` - targets, pools, profiles, project bindings, and secrets
- `~/.local/state/fleet/` - `known_hosts` and runtime state
- `~/.cache/fleet/` - SSH control sockets

The dotfiles repo owns the operator interface, documentation, and Codex
instructions. Sensitive hostnames, IPs, usernames, passwords, and keys stay out
of version control.

## Core Ideas

- Targets are concrete machines.
- Pools are logical groups that resolve to one target.
- Protocols define site rules and host-class behavior such as direct execution
  versus scheduler-backed submission and queue defaults.
- Profiles define how remote commands run: direct SSH or scheduler-backed.
- Targets may define a host-level `workdir`, which is the base directory where
  projects land on that machine.
- Project bindings attach a local repo path to a default target or pool plus
  any exceptional project-specific absolute overrides.
- By default, a bound project resolves to `workdir/<project-name>`. Record a
  per-target root in the project binding only when that default path is wrong.
- `fleetctl` owns SSH transport details directly, including a private
  `known_hosts` file and multiplexed control sockets.

## Bootstrap

```bash
fleetctl init
fleetctl doctor
```

That creates the base layout, a `protocols.d/` directory, a default
`plain-ssh` profile, and a starter `slurm-batch` profile.

## Bringing Hosts In

### Import an existing SSH alias

```bash
fleetctl import-ssh gpu-a --name gpu-a
```

This resolves the alias through `ssh -G`, writes a private target record under
`~/.config/fleet/targets.d/`, and writes the sensitive connection material under
`~/.config/fleet/secrets/`.

### Migrate an env schema

```bash
fleetctl migrate-env --env-file ~/projects/repo-a/.env --prefix REPO_A_REMOTE
```

Use this when you need to move an existing env-based machine schema into the
private fleet inventory. Host-level workdirs are stored on targets, and only
non-standard project paths stay in the project binding.

If legacy target files still carry project-specific `remote_root` entries or
inline connection fields, `fleetctl doctor` warns so the stale shape does not
quietly linger.

## Project Binding

Bind a project to a default machine or pool:

```bash
fleetctl project bind ~/projects/repo-a --pool gpu-pool --profile plain-ssh
fleetctl project bind ~/projects/repo-a --pool gpu-pool \
  --target-root ampere=/data/ayand/repo-a \
  --target-root ada=/data/home/ayand/repo-a \
  --target-root hopper=/raid/phyayan/repo-a
fleetctl project show ~/projects/repo-a
fleetctl project list
```

Once a binding exists, most commands can omit the target when run from inside
that project.

## Protocol Inspection

Inspect a target before you assume it behaves like a plain SSH box:

```bash
fleetctl protocol show hopper
fleetctl queue list hopper
```

Scheduler-backed protocols expose queue presets, runtime hints such as
`job_runtime = "docker"`, and can require compute to flow through
`fleetctl submit` instead of `fleetctl exec`.

## Daily Workflow

### Connectivity smoke test

```bash
fleetctl smoke gpu-a
```

When a target defines a `workdir`, `smoke` reports whether that host-level
directory exists. When a bound project resolves to `workdir/<project-name>` or
to an explicit per-target project root, `smoke` reports that project root
separately instead of failing transport just because the repo has not been
synced there yet.

### Run a remote argv vector

```bash
fleetctl exec gpu-a -- python3 -m pytest tests/test_streaming.py
fleetctl exec --target gpu-a -- python3 -m pytest tests/test_streaming.py
```

`exec` runs the payload through a remote Python stub and avoids shell
interpolation of the user command itself. `--target` is available when you want
to force an explicit target without using the positional selector.

On scheduler-backed targets, direct login-surface execution requires an explicit
acknowledgment:

```bash
fleetctl exec --login hopper -- squeue --me
fleetctl exec --login volta -- sinfo -h -o '%P'
```

Use `--login` only for administrative commands on the submission surface. Do
not use it as a substitute for proper job submission.

### Upload and run a script

```bash
fleetctl script scripts/train.sh --target gpu-a -- --config configs/base.toml
```

### Sync a project

```bash
fleetctl sync push . --target gpu-a
fleetctl sync pull remote-downloads/ --target gpu-a artifacts/
```

### Submit through a scheduler profile

For direct targets, `submit` falls back to running the staged script directly.
For scheduler-backed protocols, `submit` wraps a normal script in a Slurm job
using the protocol's queue defaults:

```bash
fleetctl submit scripts/train.sh --target hopper
fleetctl submit scripts/train.sh --target hopper --queue q_1day-2G
fleetctl submit scripts/train.sh --target volta --time 00:10:00
fleetctl job status 12345 --target hopper --profile slurm-batch
fleetctl job logs 12345 --target hopper --profile slurm-batch
fleetctl job cancel 12345 --target hopper --profile slurm-batch
```

When `submit` parses a Slurm job ID, it also reports the resolved stdout and
stderr paths under the protocol's `job_output_dir`.

When a site expects a fully native scheduler script, bypass the wrapper:

```bash
fleetctl submit scripts/site_native.slurm --target volta --native-batch
```

That stages the batch script and hands it to the scheduler unchanged.

## Shell Integration

`fssh` in Zsh now merges classic SSH host discovery with `fleetctl list --format
ssh-hosts`. Selecting a `fleet:*` entry routes the session through `fleetctl ssh`
instead of raw `ssh`.

## Files You Will Actually Touch

- `~/.config/fleet/targets.d/*.toml` - non-sensitive target metadata
- `~/.config/fleet/targets.d/*.toml` may include host-level `workdir`, but not
  repo-specific absolute project paths or sensitive connection material
- `~/.config/fleet/protocols.d/*.toml` - reusable site rules and queue presets
- `~/.config/fleet/secrets/*.toml` - private connection material
- `~/.config/fleet/profiles.d/*.toml` - execution profiles
- `~/.config/fleet/projects.toml` - local path to remote defaults and any
  exceptional per-target project-root overrides

## Security Notes

- Keep `~/.config/fleet/` private; `fleetctl doctor` checks file modes.
- Prefer key auth. Keep password auth only as a temporary fallback.
- Do not commit `~/.config/fleet/` into any repo.
- Use `fleetctl show --secret <target>` for a redacted preview; add
  `--sensitive` only when you explicitly need the real host details on screen.
