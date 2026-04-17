## Remote Fleet

- Treat `fleetctl` as the default interface for remote hosts, experiments, and
  project sync.
- Prefer `fleetctl exec`, `fleetctl script`, `fleetctl sync`, and
  `fleetctl submit` over hand-written `ssh '...'` command strings.
- Before using an unfamiliar target, inspect its rules with
  `fleetctl protocol show <target>` and, for scheduler-backed targets,
  `fleetctl queue list <target>`.
- Respect the resolved protocol's runtime expectations. If `job_runtime` says
  `docker`, prepare the submitted job script to launch the containerized
  workload inside the scheduler allocation instead of running the payload on the
  host directly.
- When a site expects a fully native batch script, use
  `fleetctl submit --native-batch <script> --target <target>` so the staged file
  is handed to the scheduler unchanged.
- Before running substantial remote work on a host or pool, start with
  `fleetctl smoke <target>` unless the user explicitly asks to skip it.
- When a project has a fleet binding, rely on that binding instead of inventing
  repo-local remote config.
- Treat scheduler-backed protocols as login-node control surfaces:
  `fleetctl exec --login <target> -- ...` is for administrative commands such as
  `squeue`, `sinfo`, `sacct`, and `pwd`, while compute should go through
  `fleetctl submit`.
- Keep site policy and queue defaults in private fleet protocol files under
  `~/.config/fleet/protocols.d/`, not in repo code or project-specific hacks.
- Keep sensitive connection material out of target records. Host-level
  `workdir` values belong on targets, while project-specific remote-path
  overrides belong in `projects.toml` when `workdir/<project-name>` is not the
  right landing path.
- Keep sensitive hostnames, IPs, usernames, passwords, and key paths in the
  private fleet config under `~/.config/fleet`, never in version-controlled
  project files.
- Use raw `ssh` only when `fleetctl` cannot express the operation cleanly or
  when debugging the transport layer itself.
