# Full system ClamAV scans

[`scripts/fast-clamscan.sh`](../../scripts/fast-clamscan.sh) enumerates regular
files and scans them in batches with GNU Parallel. It reports detections and
incomplete scans, and does not remove or quarantine files by default.

## Requirements and database setup

Use Bash, ClamAV (`clamscan` and `freshclam`), GNU Parallel, GNU findutils and
GNU coreutils. These tools are already available on mileva. On other Guix
machines, they can be provided with:

```bash
guix shell bash clamav parallel findutils coreutils gawk grep sed
```

From the dotfiles checkout, download/update definitions as your normal user and
scan one small file to verify readiness:

```bash
./scripts/fast-clamscan.sh --update-db --no-size README.md
```

This creates `~/.local/share/clamav`, downloads official definitions with
FreshClam, and records the updater's configuration and log in a new report
directory. It works without a system `freshclam.conf` or a `clamav` account.
Run it again before each full scan to update the definitions. Downloads need
network access and several hundred megabytes of disk space; workers need
additional memory to load the database. The helper rejects `--update-db` with
`sudo`; update as your user first so your database remains writable by you.

An explicit `--db PATH` selects that database file/directory and never silently
falls back. Without it, the helper prefers the user-local database, then common
system database directories, then ClamAV's compiled default. When invoked with
`sudo`, it uses the invoking user's home for the user-local database. Official
definitions older than seven days are rejected; `--db-max-age N` adjusts this
check and zero disables it. This check does not date custom signature files.
For a standalone official CVD/CLD file, select its parent directory to enable
the age check, or explicitly disable it with `--db-max-age 0`.

## Run a full system scan

After updating the database:

```bash
sudo ./scripts/fast-clamscan.sh --no-size /
```

This includes mounted filesystems while excluding `/proc`, `/sys`, `/dev`,
`/run`, and the current report directory. It scans regular files and does not
follow symlinks. Root privileges allow it to read protected local files; mount
permissions and files changing during the scan can still produce errors.

To restrict traversal to selected filesystems, list their roots explicitly:

```bash
sudo ./scripts/fast-clamscan.sh -x --no-size / /data
```

With `-x`, other filesystem roots must be added explicitly. For example, add
`/home` if it is a separate filesystem. Overlapping targets are deduplicated.
Without `-x`, remote and removable mounts outside the default pruned paths are
also included. Add repeatable `--prune PATH` options to exclude unwanted trees.
ClamAV file-selection options such as `--file-list`, `--include`, `--exclude`
and `--recursive` are rejected because they would invalidate coverage totals.

The default is two workers, each loading its own database. Increase `--threads`
only when memory permits; `--chunk` controls files per worker invocation.
`--no-size` avoids an extra metadata pass before a large scan.

### Write an infected-only report

To get output shaped like `clamscan -r -i` while retaining parallel scanning,
use `--infected-report FILE`. The file contains only ClamAV `FOUND` lines and
one consolidated summary. It is overwritten on each run and created with mode
`0600`; the full structured report remains in the `--out` directory.

For a scan of your home directory:

```bash
mkdir -p "$HOME/clamav-reports"
./scripts/fast-clamscan.sh --no-size \
  --prune "$HOME/clamav-reports" \
  --out "$HOME/clamav-reports/run-$(date +%Y-%m-%d-%H-%M-%S)" \
  --infected-report "$HOME/clamav-reports/scan_report.txt" \
  "$HOME"
```

The report directory is pruned in this example so previous reports are not
scanned. Root privileges are unnecessary for files your account can read. For
a system scan, use the same options with `sudo` and `/`; the helper still finds
the invoking user's definitions and returns report ownership to that user.

ClamAV's file, archive and time limits still apply. The helper enables
`--alert-exceeds-max=yes` so size/expansion/recursion limit alerts are visible.
Limit detections mark coverage incomplete. Use repeatable `--extra-arg` options
to adjust supported engine settings, for example:

```bash
sudo ./scripts/fast-clamscan.sh --no-size \
  --extra-arg '--max-filesize=512M' \
  --extra-arg '--max-scansize=1G' \
  --extra-arg '--alert-encrypted=yes' /
```

Encrypted content cannot be inspected without decrypting it. Heuristic alerts,
including limit/encryption alerts, appear among detections; review the signature
before interpreting a detection as malware. A successful scan applies to the
configured roots, exclusions and engine settings.

## Reports and exit status

Each run needs a new output directory (`--out DIR`); existing directories are
never overwritten. `clam-report-latest` points to the most recent finished run
beside its report directory. An existing regular file at that name is preserved.
Reports are private and are returned to the invoking user when run with `sudo`,
including when a run fails early.
Default report directories and the latest symlink are ignored by Git.

Read `summary.txt` or `summary.md` first. `infected.tsv` lists detections,
`errors.log` includes scanner and worker failures, `enumeration-errors.log`
records traversal failures, and `joblog.tsv` records each worker's exit status.
When requested, the external `--infected-report` file gives a compact view for
the familiar infected-only workflow; use the structured reports to assess
errors and whether scan coverage was complete.
The file inventory is NUL-delimited in `filelist.nul`. Scan arguments support
unusual filenames, but ClamAV's text output and TSV reports cannot reliably
represent paths containing tabs or newlines; consult the inventory/raw log for
those paths.

`metrics.json` includes `scan_complete`, `failed_jobs`, `scanner_errors`, `runner_errors`,
`skipped_targets` and `limit_alerts`. `infected` counts distinct paths with
reported detections; `detections` counts individual signature records.
`clean_approx` is an estimate of files without reported detections and is `null`
when coverage is incomplete. It is not proof that files contain no malware.

| Exit | Meaning |
|---|---|
| 0 | No detections or execution errors within configured scan settings |
| 1 | Detections found, including heuristic/limit alerts |
| 2 | Preflight, enumeration, missing-target, worker or logging errors |
| 3 | Missing runtime dependency |
| 4 | No regular files to scan |

Errors take precedence over detections. Interrupted runs exit with an error;
their retained logs may be partial and final summaries may be absent.

## Regression checks

The integration suite uses real ClamAV with harmless local signatures and
simulated worker/updater failures. It needs Python 3 and the scan dependencies,
and does not download official definitions or scan outside temporary fixtures:

```bash
bash -n scripts/fast-clamscan.sh
shellcheck scripts/fast-clamscan.sh
python3 -m unittest discover -s tests -p 'test_fast_clamscan.py' -v
```

See the official [ClamAV scanning guide](https://docs.clamav.net/manual/Usage/Scanning.html)
and [FreshClam configuration guide](https://docs.clamav.net/manual/Usage/Configuration.html)
for engine limits and database configuration.
