#!/usr/bin/env bash
# Multi-process ClamAV scan using GNU parallel (2 workers by default).
# Database is configurable (default: $HOME/.local/share/clamav).
# All artifact names use hyphens; timestamps are yyyy-mm-dd-hh-mm-ss.
#
# Outputs:
#   - summary.txt / summary.md     Human-friendly run summary
#   - metrics.json                 Machine-readable metrics
#   - scan-raw.log                 Combined clamscan stdout/stderr
#   - infected.tsv                 path<TAB>signature
#   - signature-counts.tsv         count<TAB>signature
#   - top-infected-dirs.tsv        count<TAB>directory
#   - infected-by-ext.tsv          count<TAB>extension
#   - errors.log                   raw ERROR lines from clamscan
#   - errors-by-message.tsv        count<TAB>message
#   - joblog.tsv                   GNU parallel joblog
#   - parallel-progress.log        progress bar + misc stderr from parallel
#
# Exit codes (for CI or hooks):
#   0 = no detections, no execution errors
#   1 = detections found (including heuristic/scan-limit alerts)
#   2 = scan errors occurred (permission, I/O, etc.)
#   3 = missing dependency; 4 = no regular files to scan
#
# Each worker loads its own database; increase worker count only with RAM to spare.
# Chunking avoids reloading the database for every individual file.

set -Eeuo pipefail
umask 077

die() { printf 'Error: %s\n' "$*" >&2; exit 2; }
warn() { printf 'Warning: %s\n' "$*" >&2; }

# --------------------------- Configurable defaults --------------------------- #
THREADS=2                   # Parallel workers, each with its own database
CHUNK_SIZE=400              # Files per clamscan invocation
STAY_ON_FS=0                # 1 = stay on same filesystem(s)
DEFAULT_PRUNES=(/proc /sys /dev /run)
PRUNES=("${DEFAULT_PRUNES[@]}")
SIZE_MODE="auto"            # auto|always|none
SIZE_MAX_FILES=200000       # auto mode threshold

REAL_HOME="${HOME}"
if [[ ${EUID} -eq 0 && -n "${SUDO_USER-}" && "${SUDO_USER}" != "root" ]]; then
	# Read the account's home without evaluating shell code from an environment var.
	if command -v getent >/dev/null 2>&1; then
		account_entry="$(getent passwd "$SUDO_USER")" || die "Cannot resolve $SUDO_USER"
	else
		account_entry="$(awk -F: -v user="$SUDO_USER" '$1 == user {print; exit}' /etc/passwd)"
	fi
	IFS=: read -r _ _ _ _ _ REAL_HOME _ <<<"$account_entry"
	[[ -n "$REAL_HOME" ]] || die "Cannot determine home directory for $SUDO_USER"
fi

DB_PATH_DEFAULT="${REAL_HOME}/.local/share/clamav"   # User-local DB default
DB_PATH="${DB_PATH_DEFAULT}"
DB_EXPLICIT=0
UPDATE_DB=0
DB_MAX_AGE=7                # Days; zero disables official database age checking

# Sensible clamscan flags; we feed file paths, not dirs, so no -r.
CLAM_FLAGS_BASE=(
  --stdout
  --suppress-ok-results
  --follow-file-symlinks=0
  --follow-dir-symlinks=0
  --alert-exceeds-max=yes
)
EXTRA_CLAM_ARGS=()
MAKE_LATEST_LINK=1
INFECTED_REPORT=""

# Output directory (hyphens, yyyy-mm-dd-hh-mm-ss)
NOW="$(date +%Y-%m-%d-%H-%M-%S)"
OUTDIR="clam-report-${NOW}"

# ------------------------------ CLI handling -------------------------------- #
usage() {
  cat <<USAGE
Usage: $0 [options] <path> [<path> ...]

Options:
  -t, --threads N         Parallel workers (default ${THREADS})
  -c, --chunk N           Files per clamscan call (default ${CHUNK_SIZE})
  -o, --out DIR           Output directory (default ${OUTDIR})
  -d, --db PATH           Database file or directory (default ${DB_PATH_DEFAULT})
      --update-db         Update the user-local (or --db) directory using freshclam
      --db-max-age N      Reject official definitions older than N days (default 7; 0 disables)
  -x, --xdev              Don't cross filesystem boundaries
      --size              Always compute total bytes (slower on huge trees)
      --no-size            Skip byte sizing (faster startup)
      --size-max-files N  Auto mode threshold (default ${SIZE_MAX_FILES})
      --prune PATH         Add an extra pruned path (repeatable)
      --no-prunes         Don't auto-prune /proc,/sys,/dev,/run
      --extra "ARGS"      Extra clamscan args (split on whitespace)
      --extra-arg ARG     Extra clamscan arg (repeatable; safer for quoting)
      --infected-report FILE
                          Write FOUND lines plus one consolidated scan summary
      --no-latest          Don't create/update a clam-report-latest symlink
  -h, --help              Show help

Examples:
  $0 --update-db --no-size README.md
  sudo $0 --no-size --extra-arg '--alert-encrypted=yes' /
  sudo $0 -x --no-size / /data
  $0 --prune "$HOME/.cache" --extra-arg '--alert-encrypted=yes' ~

Notes:
  - Run --update-db without sudo before the full system scan; freshclam is needed.
  - The report directory must be new. Reports are excluded from this run's scan.
  - /proc, /sys, /dev and /run are excluded by default; symlinks are not followed.
  - Without -x, mounted filesystems are included. With -x, list each desired root.
  - Detections include Heuristics.Limits.Exceeded alerts for incompletely scanned files.
USAGE
}

if [[ $# -lt 1 ]]; then usage; exit 1; fi
while [[ $# -gt 0 ]]; do
  case "$1" in
    -t|--threads|-c|--chunk|-o|--out|-d|--db|--db-max-age|--size-max-files|--prune|--extra-arg|--infected-report)
      [[ $# -ge 2 && -n "$2" ]] || die "$1 requires a value"
      ;;
    --extra) [[ $# -ge 2 ]] || die "$1 requires a value" ;;
  esac
  case "$1" in
    -t|--threads) THREADS="${2:?}"; shift 2 ;;
    -c|--chunk)   CHUNK_SIZE="${2:?}"; shift 2 ;;
    -o|--out)     OUTDIR="${2:?}"; shift 2 ;;
    -d|--db)      DB_PATH="${2:?}"; DB_EXPLICIT=1; shift 2 ;;
    --update-db)  UPDATE_DB=1; shift ;;
    --db-max-age) DB_MAX_AGE="${2:?}"; shift 2 ;;
    -x|--xdev)    STAY_ON_FS=1; shift ;;
    --size)       SIZE_MODE="always"; shift ;;
    --no-size)    SIZE_MODE="none"; shift ;;
    --size-max-files) SIZE_MAX_FILES="${2:?}"; shift 2 ;;
    --prune)      PRUNES+=("${2:?}"); shift 2 ;;
    --no-prunes)  PRUNES=(); shift ;;
    --extra)
      extra_raw="${2:-}"
      if [[ -n "$extra_raw" ]]; then
        read -r -a extra_split <<<"$extra_raw"
        EXTRA_CLAM_ARGS+=("${extra_split[@]}")
      fi
      shift 2
      ;;
    --extra-arg)  EXTRA_CLAM_ARGS+=("${2:?}"); shift 2 ;;
    --infected-report) INFECTED_REPORT="${2:?}"; shift 2 ;;
    --no-latest)  MAKE_LATEST_LINK=0; shift ;;
    -h|--help)    usage; exit 0 ;;
    --)           shift; break ;;
    -*)
      echo "Unknown option: $1" >&2; usage; exit 2 ;;
    *) break ;;
  esac
done
TARGETS=( "$@" )
if [[ ${#TARGETS[@]} -eq 0 ]]; then
  echo "No targets given." >&2; usage; exit 2
fi

[[ "$THREADS" =~ ^[1-9][0-9]*$ ]] || die "--threads must be a positive integer (got: $THREADS)"
[[ "$CHUNK_SIZE" =~ ^[1-9][0-9]*$ ]] || die "--chunk must be a positive integer (got: $CHUNK_SIZE)"
[[ "$SIZE_MAX_FILES" =~ ^[1-9][0-9]*$ ]] || die "--size-max-files must be a positive integer (got: $SIZE_MAX_FILES)"
[[ "$DB_MAX_AGE" =~ ^(0|[1-9][0-9]*)$ ]] || die "--db-max-age must be a non-negative integer"
case "$SIZE_MODE" in
  auto|always|none) ;;
  *) die "--size/--no-size configuration invalid (SIZE_MODE=$SIZE_MODE)" ;;
esac
for arg in "${EXTRA_CLAM_ARGS[@]}"; do
  case "$arg" in
    -f|--file-list|--file-list=*|-r|--recursive|--recursive=*|--include|--include=*|--exclude|--exclude=*|--include-dir|--include-dir=*|--exclude-dir|--exclude-dir=*)
      die "File-selection option is incompatible with coverage accounting: $arg (use scan targets and --prune)"
      ;;
  esac
done

# ------------------------------- Prereqs ------------------------------------ #
for bin in clamscan parallel find awk sort grep tee tr wc date sed uname ln rm head stat realpath mkdir dirname cat id chmod; do
  command -v "$bin" >/dev/null || { echo "Missing dependency: $bin" >&2; exit 3; }
done

file_size_bytes() {
	file=$1
	if size=$(stat -c%s "$file" 2>/dev/null); then
		printf '%s' "$size"
		return 0
	fi
	if size=$(stat -f%z "$file" 2>/dev/null); then
		printf '%s' "$size"
		return 0
	fi
	wc -c <"$file" | tr -d ' '
}

format_epoch() {
	epoch=$1
	if date -d "@$epoch" '+%Y-%m-%d %H:%M:%S' >/dev/null 2>&1; then
		date -d "@$epoch" '+%Y-%m-%d %H:%M:%S'
		return 0
	fi
	date -r "$epoch" '+%Y-%m-%d %H:%M:%S' 2>/dev/null || printf '%s' "$epoch"
}

OUTDIR="$(realpath -m -- "$OUTDIR")"
mkdir -p -- "$(dirname "$OUTDIR")"
mkdir -- "$OUTDIR" || die "Report directory must be new: $OUTDIR"
OUTDIR="$(realpath -- "$OUTDIR")"
if [[ -n "$INFECTED_REPORT" ]]; then
	INFECTED_REPORT="$(realpath -m -- "$INFECTED_REPORT")"
	[[ "$INFECTED_REPORT" != "$OUTDIR" ]] || die "--infected-report must name a file, not the report directory"
	[[ ! -d "$INFECTED_REPORT" ]] || die "--infected-report must not name a directory: $INFECTED_REPORT"
	mkdir -p -- "$(dirname "$INFECTED_REPORT")"
fi

# Restore report ownership even when preflight, enumeration or scanning fails.
# shellcheck disable=SC2317 # Invoked indirectly by Bash's EXIT trap.
restore_report_owner() {
	if [[ ${EUID} -eq 0 && -n "${SUDO_USER-}" && "$SUDO_USER" != root ]]; then
		local uid gid
		uid=$(id -u "$SUDO_USER" 2>/dev/null) || return 0
		gid=$(id -g "$SUDO_USER" 2>/dev/null) || return 0
		chown -R "${uid}:${gid}" "$OUTDIR" 2>/dev/null || true
		if [[ -L "${LATEST_LINK-}" ]]; then
			chown -h "${uid}:${gid}" "$LATEST_LINK" 2>/dev/null || true
		fi
		if [[ -n "${INFECTED_REPORT-}" && -f "$INFECTED_REPORT" ]]; then
			chown "${uid}:${gid}" "$INFECTED_REPORT" 2>/dev/null || true
		fi
	fi
}
trap 'restore_report_owner' EXIT
trap 'warn "Scan interrupted; the report is incomplete."; exit 2' INT TERM
PARALLEL="--will-cite"
export PARALLEL
LC_ALL=C
export LC_ALL

# Logs and artifacts (hyphenated)
FILELIST="${OUTDIR}/filelist.nul"
RAW="${OUTDIR}/scan-raw.log"
INFECTED_TSV="${OUTDIR}/infected.tsv"
ERRORS_LOG="${OUTDIR}/errors.log"
ERRORS_BY_MSG="${OUTDIR}/errors-by-message.tsv"
JOBLOG="${OUTDIR}/joblog.tsv"
SUMMARY_TXT="${OUTDIR}/summary.txt"
SUMMARY_MD="${OUTDIR}/summary.md"
SIG_COUNTS="${OUTDIR}/signature-counts.tsv"
TOP_DIRS="${OUTDIR}/top-infected-dirs.tsv"
INF_BY_EXT="${OUTDIR}/infected-by-ext.tsv"
METRICS_JSON="${OUTDIR}/metrics.json"
PARALLEL_PROGRESS="${OUTDIR}/parallel-progress.log"
ENUM_ERRORS_LOG="${OUTDIR}/enumeration-errors.log"
: > "${ENUM_ERRORS_LOG}"

# Use an absolute target so nested relative --out paths work as well.
OUTDIR_PARENT="$(dirname "$OUTDIR")"
LATEST_LINK="${OUTDIR_PARENT}/clam-report-latest"
if (( MAKE_LATEST_LINK )) && [[ -e "$LATEST_LINK" && ! -L "$LATEST_LINK" ]]; then
	warn "Leaving existing non-symlink untouched: $LATEST_LINK"
	MAKE_LATEST_LINK=0
fi

# Canonical paths make default/custom prunes work for relative scan roots too.
for i in "${!PRUNES[@]}"; do PRUNES[i]="$(realpath -m -- "${PRUNES[i]}")"; done
PRUNES+=("$OUTDIR")
if [[ -n "$INFECTED_REPORT" ]]; then
	# Avoid scanning a report left by an earlier run.
	PRUNES+=("$INFECTED_REPORT")
fi

# -------------------------- DB selection & info ----------------------------- #
db_dir_has_sigs() {
	local dir="$1"
	[[ -d "$dir" ]] || return 1

	shopt -s nullglob
	local candidates=("$dir"/*.cvd "$dir"/*.cld "$dir"/*.ndb "$dir"/*.ldb "$dir"/*.hdb "$dir"/*.hsb "$dir"/*.yar "$dir"/*.yara)
	shopt -u nullglob
	(( ${#candidates[@]} > 0 ))
}

db_flag=()
DB_STATUS="clamscan-default"
DB_DIR_USED=""

if (( UPDATE_DB )); then
	command -v freshclam >/dev/null || die "--update-db requires freshclam"
	if [[ ${EUID} -eq 0 && -n "${SUDO_USER-}" && "$SUDO_USER" != root ]]; then
		die "Run --update-db without sudo first, then run the system scan with sudo."
	fi
	[[ ! -f "$DB_PATH" ]] || die "--update-db requires a database directory"
	DB_PATH="$(realpath -m -- "$DB_PATH")"
	[[ "$DB_PATH" != *$'\n'* && "$DB_PATH" != *$'\r'* && "$DB_PATH" != *'"'* ]] || die "Unsupported database directory for freshclam configuration"
	mkdir -p -- "$DB_PATH"
	FRESHCLAM_CONFIG="${OUTDIR}/freshclam.conf"
	{
		printf 'DatabaseDirectory "%s"\n' "$DB_PATH"
		printf 'DatabaseOwner %s\n' "$(id -un)"
		printf 'DatabaseMirror database.clamav.net\n'
		printf 'MaxAttempts 2\n'
	} >"$FRESHCLAM_CONFIG"
	echo "[*] Updating virus definitions in $DB_PATH..."
	if ! freshclam --config-file="$FRESHCLAM_CONFIG" --stdout >"${OUTDIR}/freshclam.log" 2>&1; then
		cat "${OUTDIR}/freshclam.log" >&2
		die "Database update failed; see ${OUTDIR}/freshclam.log"
	fi
	DB_EXPLICIT=1
fi

if (( DB_EXPLICIT )); then
	[[ -e "$DB_PATH" ]] || die "Database path does not exist: $DB_PATH (use --update-db to download definitions)"
	DB_PATH="$(realpath -- "$DB_PATH")"
	db_flag=("--database=$DB_PATH")
	DB_DIR_USED="$DB_PATH"
	DB_STATUS="custom"
elif db_dir_has_sigs "$DB_PATH"; then
	db_flag=( "--database=$DB_PATH" )
	DB_DIR_USED="$DB_PATH"
	DB_STATUS="user-default"
else
	for dir in /var/lib/clamav /var/db/clamav /usr/local/share/clamav /usr/share/clamav; do
		if db_dir_has_sigs "$dir"; then
			db_flag=( "--database=$dir" )
			DB_DIR_USED="$dir"
			DB_STATUS="system"
			break
		fi
	done
fi

# Optional database metadata (only if sigtool exists)
DB_META_TXT=""
if command -v sigtool >/dev/null 2>&1 && [[ -d "$DB_DIR_USED" ]]; then
  DB_META_TXT="$(for db_file in "$DB_DIR_USED"/*.{cvd,cld}; do
    [[ -f "$db_file" ]] || continue
    sigtool --info "$db_file" 2>/dev/null || true
  done)"
fi

CLAM_ARGS=("${CLAM_FLAGS_BASE[@]}")
CLAM_ARGS+=("--tempdir=$OUTDIR")
if (( ${#db_flag[@]} )); then CLAM_ARGS+=("${db_flag[@]}"); fi
if (( DB_MAX_AGE > 0 )); then
  # ClamAV's age option is for database directories. With a standalone custom
  # signature file, some versions silently load zero signatures when it is set.
  if [[ -f "$DB_DIR_USED" && ( "$DB_DIR_USED" == *.cvd || "$DB_DIR_USED" == *.cld ) ]]; then
    die "Use the official database's directory with --db for age checking, or --db-max-age 0 for a standalone CVD/CLD."
  fi
  if [[ "$DB_STATUS" == clamscan-default ]] ||
     { [[ -d "$DB_DIR_USED" ]] && compgen -G "$DB_DIR_USED/*.cvd" >/dev/null; } ||
     { [[ -d "$DB_DIR_USED" ]] && compgen -G "$DB_DIR_USED/*.cld" >/dev/null; }; then
    CLAM_ARGS+=("--fail-if-cvd-older-than=$DB_MAX_AGE")
  fi
fi
if (( ${#EXTRA_CLAM_ARGS[@]} )); then CLAM_ARGS+=("${EXTRA_CLAM_ARGS[@]}"); fi
# Summaries expose errors even when an infected file makes the worker exit 1.
# Keep these enabled for reporting regardless of extra scanner options.
CLAM_ARGS+=(--no-summary=no)

# Fail early (and clearly) if the database isn't usable.
DB_PROBE_LOG="${OUTDIR}/db-probe.log"
probe_file="${OUTDIR}/.db-probe-file"
printf 'ClamAV database readiness check\n' >"$probe_file"
set +e
probe_out="$(clamscan "${CLAM_ARGS[@]}" -- "$probe_file" 2>&1)"
probe_rc=$?
set -e
rm -f "$probe_file" 2>/dev/null || true
printf '%s\n' "$probe_out" >"$DB_PROBE_LOG"
if (( probe_rc != 0 )); then
	die "Database/scan options not usable (exit $probe_rc); see ${DB_PROBE_LOG}. Run --update-db without sudo or pass --db."
fi

# ---------------------------- File enumeration ------------------------------ #
echo "[*] Enumerating files..."

build_find() {
  local tgt="$1"
  local xdev=()
  (( STAY_ON_FS )) && xdev=(-xdev)

  local prune_expr=()
  if (( ${#PRUNES[@]} > 0 )); then
    for p in "${PRUNES[@]}"; do
      # find -path is a glob; escape metacharacters in literal directory names.
      p=${p//\\/\\\\}; p=${p//\*/\\*}; p=${p//\?/\\?}; p=${p//\[/\\[}
      prune_expr+=( -path "$p" -prune -o )
    done
  fi

  # Print only real files, NUL-delimited
  if ! find "$tgt" "${xdev[@]}" \( "${prune_expr[@]}" -type f -print0 \) 2>>"$ENUM_ERRORS_LOG"; then
    printf 'File enumeration failed for: %s\n' "$tgt" >>"$ENUM_ERRORS_LOG"
  fi
}

: > "$FILELIST"
SKIPPED_TARGETS=()
for t in "${TARGETS[@]}"; do
  [[ -e "$t" ]] || { warn "Skipping non-existent target: $t"; SKIPPED_TARGETS+=("$t"); continue; }
  build_find "$(realpath -- "$t")" >> "$FILELIST"
done
sort -zu -o "$FILELIST" "$FILELIST"

TOTAL_FILES=$(tr -cd '\0' < "$FILELIST" | wc -c | awk '{print $1}')
if [[ "$TOTAL_FILES" -eq 0 ]]; then
  echo "No files found to scan. Exiting." >&2
  if [[ -s "$ENUM_ERRORS_LOG" ]] || (( ${#SKIPPED_TARGETS[@]} > 0 )); then exit 2; fi
  exit 4
fi

TOTAL_BYTES=0
BYTES_MEASURED=false
if [[ "$SIZE_MODE" == "always" ]] || { [[ "$SIZE_MODE" == "auto" ]] && [[ "$TOTAL_FILES" -le "$SIZE_MAX_FILES" ]]; }; then
	echo "[*] Sizing files for throughput stats..."
	BYTES_MEASURED=true
	while IFS= read -r -d '' f; do
		sz=$(file_size_bytes "$f" 2>/dev/null || printf '0')
		TOTAL_BYTES=$(( TOTAL_BYTES + sz ))
	done < "$FILELIST"
else
	echo "[*] Skipping byte sizing (use --size to force)."
fi

# ------------------------------- Scanning ----------------------------------- #
echo "[*] Scanning ${TOTAL_FILES} files with ${THREADS} workers; chunk size ${CHUNK_SIZE}."
echo "[*] Output dir: ${OUTDIR}"

# Progress bar only when stderr is a TTY
PROGRESS_OPT=()
if [[ -t 2 ]]; then PROGRESS_OPT=(--bar); fi

START_TS="$(date +%s)"

# --quote preserves bash -c and every fixed argument, including regexes/spaces.
# A real pipeline waits for tee, so report parsing cannot race with log writes.
set +e
parallel --quote -0 -j "${THREADS}" -N "${CHUNK_SIZE}" --joblog "${JOBLOG}" "${PROGRESS_OPT[@]}" \
  bash -c 'clamscan "$@" 2>&1' _ "${CLAM_ARGS[@]}" -- :::: "${FILELIST}" \
  2> >(tee "${PARALLEL_PROGRESS}" >&2) | tee "${RAW}"
SCAN_PIPE_STATUS=("${PIPESTATUS[@]}")
PARALLEL_RC=${SCAN_PIPE_STATUS[0]}
LOGGING_RC=${SCAN_PIPE_STATUS[1]}
set -e

END_TS="$(date +%s)"
DUR=$(( END_TS - START_TS )); (( DUR == 0 )) && DUR=1
MBPS=""
if [[ "$BYTES_MEASURED" == true ]]; then
	MBPS=$(awk -v b="${TOTAL_BYTES}" -v s="${DUR}" 'BEGIN{printf "%.2f", (b/1048576)/s}')
else
	MBPS="n/a"
fi
FPS=$(awk -v n="${TOTAL_FILES}" -v s="${DUR}" 'BEGIN{printf "%.2f", n/s}')

# ------------------------------ Post-process -------------------------------- #
# Infected lines: "/path: Signature FOUND"
awk '
  / FOUND$/ {
    line=$0
    sub(/ FOUND$/,"",line)
    path=line
    sub(/: [^:]*$/, "", path)
    if (path!=line) {
      sig=substr(line,length(path)+3)
      printf "%s\t%s\n", path, sig
    }
  }
' "${RAW}" > "${INFECTED_TSV}"

grep -aE "(^ERROR(:|[[:space:]])|[[:space:]]ERROR$|^LibClamAV Error:|: Access denied$|^WARNING: .*Can't (open|access))" \
  "${RAW}" > "${ERRORS_LOG}" || [[ $? -eq 1 ]]
SCANNER_ERROR_COUNT=$(awk '/^Total errors: [0-9]+$/ {n+=$3} END {print n+0}' "$RAW")
SCANNER_SUMMARY_COUNT=$(awk '/^----------- SCAN SUMMARY -----------$/ {n++} END {print n+0}' "$RAW")
if (( SCANNER_ERROR_COUNT > 0 )); then
  printf 'ERROR: Scanner summaries report %d file/engine errors.\n' "$SCANNER_ERROR_COUNT" >>"$ERRORS_LOG"
fi

# GNU parallel counts failed jobs, including ClamAV's normal detection exit (1).
# Inspect individual job statuses to distinguish detections from errors/signals.
COMPLETED_JOBS=0
FAILED_JOBS=0
DETECTION_JOBS=0
if [[ -s "$JOBLOG" ]]; then
  read -r COMPLETED_JOBS FAILED_JOBS DETECTION_JOBS < <(
    awk -F'\t' 'NR>1 {
      jobs++
      if ($8!=0 || $7 !~ /^[01]$/) failed++
      else if ($7==1) detections++
    } END {printf "%d %d %d\n", jobs, failed, detections}' "$JOBLOG"
  )
fi
EXPECTED_JOBS=$(( (TOTAL_FILES + CHUNK_SIZE - 1) / CHUNK_SIZE ))
RUNNER_ERROR_COUNT=0
if (( COMPLETED_JOBS != EXPECTED_JOBS || SCANNER_SUMMARY_COUNT != COMPLETED_JOBS || LOGGING_RC != 0 || PARALLEL_RC > 101 )) ||
   { (( PARALLEL_RC != 0 && FAILED_JOBS == 0 && DETECTION_JOBS == 0 )); }; then
  RUNNER_ERROR_COUNT=1
  printf 'ERROR: Incomplete run or log failure (completed %d/%d jobs; %d summaries; parallel exit %d; log exit %d).\n' \
    "$COMPLETED_JOBS" "$EXPECTED_JOBS" "$SCANNER_SUMMARY_COUNT" "$PARALLEL_RC" "$LOGGING_RC" >>"$ERRORS_LOG"
fi
if (( FAILED_JOBS > 0 )); then
  printf 'ERROR: %d scanner workers failed; see joblog.tsv for exit values/signals.\n' "$FAILED_JOBS" >>"$ERRORS_LOG"
fi

ENUM_ERROR_COUNT=$(wc -l < "${ENUM_ERRORS_LOG}" | awk '{print $1}')
DETECTION_COUNT=$(wc -l < "${INFECTED_TSV}" | awk '{print $1}')
INFECTED_COUNT=$(awk -F'\t' '!seen[$1]++ {n++} END {print n+0}' "$INFECTED_TSV")
LIMIT_ALERT_COUNT=$(awk -F'\t' '$2 ~ /^Heuristics\.Limits\.Exceeded/ {n++} END {print n+0}' "$INFECTED_TSV")
ERROR_COUNT=$(wc -l < "${ERRORS_LOG}" | awk '{print $1}')
SKIPPED_TARGET_COUNT=${#SKIPPED_TARGETS[@]}
TOTAL_ERROR_COUNT=$(( ENUM_ERROR_COUNT + ERROR_COUNT + SKIPPED_TARGET_COUNT ))
SCANNED_FILE_COUNT=$(awk '/^Scanned files: [0-9]+$/ {n+=$3} END {print n+0}' "$RAW")
SCAN_COMPLETE=true
CLEAN_COUNT=null
if (( TOTAL_ERROR_COUNT > 0 || LIMIT_ALERT_COUNT > 0 || INFECTED_COUNT > TOTAL_FILES )) ||
   { (( DETECTION_JOBS > 0 && DETECTION_COUNT == 0 )); }; then
  SCAN_COMPLETE=false
else
  CLEAN_COUNT=$(( TOTAL_FILES - INFECTED_COUNT ))
fi

# Signature frequency
awk -F'\t' '{c[$2]++} END{for (s in c) printf "%d\t%s\n", c[s], s}' "${INFECTED_TSV}" \
  | sort -nr > "${SIG_COUNTS}" || true

# Top infected directories (by parent)
awk -F'\t' '{
  p=$1; sub(/\/[^/]+$/,"",p); if (p=="") p="/"; c[p]++
} END{for (d in c) printf "%d\t%s\n", c[d], d}' "${INFECTED_TSV}" \
  | sort -nr | head -n 20 > "${TOP_DIRS}" || true

# Infected by file extension
awk -F'\t' '{
  f=$1; ext=""
  if (match(f,/\.([^.\/]+)$/)) ext=tolower(substr(f,RSTART+1,RLENGTH-1)); else ext="(none)"
  c[ext]++
} END{for (e in c) printf "%d\t%s\n", c[e], e}' "${INFECTED_TSV}" \
  | sort -nr > "${INF_BY_EXT}" || true

# Errors by message (group by trailing phrase like "Permission denied")
awk -F': ' 'BEGIN{OFS="\t"} {msg=$NF; gsub(/^[[:space:]]+|[[:space:]]+$/,"",msg); c[msg]++}
  END{for (m in c) print c[m], m}' "${ERRORS_LOG}" \
  | sort -nr > "${ERRORS_BY_MSG}" || true

# ------------------------------- Summaries ---------------------------------- #
START_HUMAN="$(format_epoch "${START_TS}")"
END_HUMAN="$(format_epoch "${END_TS}")"
HOST="$(uname -n 2>/dev/null || echo unknown)"
KERNEL="$(uname -srmo 2>/dev/null || uname -sr 2>/dev/null || echo unknown)"

# Optional compact output matching the useful shape of `clamscan -r -i`:
# only detection lines, followed by one aggregate summary instead of one per
# parallel worker. The detailed error and coverage artifacts remain in OUTDIR.
if [[ -n "$INFECTED_REPORT" ]]; then
	grep -a ' FOUND$' "$RAW" >"$INFECTED_REPORT" || [[ $? -eq 1 ]]
	if [[ -s "$INFECTED_REPORT" ]]; then
		printf '\n' >>"$INFECTED_REPORT"
	fi
	KNOWN_VIRUSES=$(awk -F': ' '/^Known viruses: [0-9]+$/ {print $2; exit}' "$RAW")
	ENGINE_VERSION=$(awk -F': ' '/^Engine version: / {print $2; exit}' "$RAW")
	{
		echo "----------- SCAN SUMMARY -----------"
		if [[ -n "$KNOWN_VIRUSES" ]]; then
			printf 'Known viruses: %s\n' "$KNOWN_VIRUSES"
		fi
		if [[ -n "$ENGINE_VERSION" ]]; then
			printf 'Engine version: %s\n' "$ENGINE_VERSION"
		fi
		printf 'Files enumerated: %d\n' "$TOTAL_FILES"
		printf 'Scanned files: %d\n' "$SCANNED_FILE_COUNT"
		printf 'Infected files: %d\n' "$INFECTED_COUNT"
		printf 'Detection records: %d\n' "$DETECTION_COUNT"
		printf 'Total errors: %d\n' "$TOTAL_ERROR_COUNT"
		printf 'Scan complete: %s\n' "$SCAN_COMPLETE"
		printf 'Time: %d sec\n' "$DUR"
		printf 'Start Date: %s\n' "$START_HUMAN"
		printf 'End Date:   %s\n' "$END_HUMAN"
	} >>"$INFECTED_REPORT"
	chmod 600 "$INFECTED_REPORT"
fi

# TXT summary
{
  echo "=== ClamAV Parallel Scan Summary ==="
  echo "Host:            ${HOST}"
  echo "Kernel:          ${KERNEL}"
  echo "When:            ${START_HUMAN} -> ${END_HUMAN}  (${DUR}s)"
  echo "Workers:         ${THREADS}  (chunk size ${CHUNK_SIZE})"
  echo "Targets:         ${TARGETS[*]}"
  echo "Pruned paths:    ${PRUNES[*]}"
  echo "Stay on FS:      ${STAY_ON_FS}"
  if (( ${#SKIPPED_TARGETS[@]} > 0 )); then
    echo "Skipped targets: ${SKIPPED_TARGETS[*]}"
  fi
  echo "DB mode:         ${DB_STATUS}${DB_DIR_USED:+ (${DB_DIR_USED})}"
  echo "DB max age:      ${DB_MAX_AGE} days (0 = disabled; official databases only)"
  if [[ -n "$DB_META_TXT" ]]; then
    echo "DB metadata:"
    printf '  %s\n' "${DB_META_TXT//$'\n'/$'\n  '}"
  fi
  printf "Files enumerated: %d\n" "${TOTAL_FILES}"
  if [[ "$BYTES_MEASURED" == true ]]; then
    printf "Bytes enumerated: %d bytes (%.2f GiB)\n" "${TOTAL_BYTES}" "$(awk -v b=${TOTAL_BYTES} 'BEGIN{printf (b/1073741824)}')"
  else
    printf "Bytes enumerated: (skipped)\n"
  fi
  printf "Throughput:       %s MB/s, %s files/s\n" "${MBPS}" "${FPS}"
  printf "Files detected:   %d (%d detection records)\n" "${INFECTED_COUNT}" "${DETECTION_COUNT}"
  printf "Limit alerts:     %d\n" "${LIMIT_ALERT_COUNT}"
  printf "Scan complete:    %s (within configured roots/exclusions)\n" "${SCAN_COMPLETE}"
  if [[ "$CLEAN_COUNT" != null ]]; then
    printf "Without reported detections (approx): %d\n" "$CLEAN_COUNT"
  else
    echo "Without reported detections: unknown (incomplete scan)"
  fi
  printf "Scan errors:      %d\n" "${ERROR_COUNT}"
  printf "Enum errors:      %d\n" "${ENUM_ERROR_COUNT}"
  printf "Errors (total):   %d\n" "${TOTAL_ERROR_COUNT}"
  printf "Completed jobs:   %d/%d\n" "$COMPLETED_JOBS" "$EXPECTED_JOBS"
  printf "Failed workers:   %d\n" "$FAILED_JOBS"
  printf "Scanner-reported errors: %d\n" "$SCANNER_ERROR_COUNT"
  printf "Parallel rc:      %d\n" "${PARALLEL_RC}"
  echo
  echo "Artifacts:"
  echo "  ${RAW}"
  echo "  ${INFECTED_TSV}"
  echo "  ${SIG_COUNTS}"
  echo "  ${TOP_DIRS}"
  echo "  ${INF_BY_EXT}"
  echo "  ${ERRORS_LOG}"
  echo "  ${ERRORS_BY_MSG}"
  echo "  ${ENUM_ERRORS_LOG}"
  echo "  ${JOBLOG}"
  echo "  ${PARALLEL_PROGRESS}"
  echo "  ${METRICS_JSON}"
  if [[ -n "$INFECTED_REPORT" ]]; then
    echo "  ${INFECTED_REPORT}"
  fi
} | tee "${SUMMARY_TXT}"

# Markdown summary (for dotfiles/README)
{
  echo "# ClamAV Parallel Scan Summary"
  echo ""
  echo "- **Host:** ${HOST}"
  echo "- **Kernel:** ${KERNEL}"
  echo "- **When:** ${START_HUMAN} → ${END_HUMAN}  (${DUR}s)"
  echo "- **Workers:** ${THREADS}  (chunk size ${CHUNK_SIZE})"
  echo "- **Targets:** \`${TARGETS[*]}\`"
  echo "- **Pruned paths:** \`${PRUNES[*]}\`"
  echo "- **Stay on filesystem:** ${STAY_ON_FS}"
  if (( ${#SKIPPED_TARGETS[@]} > 0 )); then
    echo "- **Skipped targets:** \`${SKIPPED_TARGETS[*]}\`"
  fi
  echo "- **DB mode:** ${DB_STATUS}${DB_DIR_USED:+ (\`${DB_DIR_USED}\`)}"
  if [[ -n "$DB_META_TXT" ]]; then
    echo ""
    echo "### Database metadata"
    echo ""
    echo '```'
    echo "${DB_META_TXT}"
    echo '```'
  fi
  echo ""
  echo "### Totals"
  echo ""
  echo "| Metric | Value |"
  echo "|---|---:|"
  echo "| Files enumerated | ${TOTAL_FILES} |"
  echo "| Scan complete within configured roots/exclusions | ${SCAN_COMPLETE} |"
  if [[ "$BYTES_MEASURED" == true ]]; then
    echo "| Bytes enumerated | ${TOTAL_BYTES} |"
  else
    echo "| Bytes enumerated | (skipped) |"
  fi
  echo "| Throughput (MB/s) | ${MBPS} |"
  echo "| Files/s | ${FPS} |"
  echo "| Files with detections | ${INFECTED_COUNT} |"
  echo "| Detection records | ${DETECTION_COUNT} |"
  echo "| Scan limit alerts | ${LIMIT_ALERT_COUNT} |"
  echo "| Files without reported detections (approx; null = unknown) | ${CLEAN_COUNT} |"
  echo "| Scan errors | ${ERROR_COUNT} |"
  echo "| Enum errors | ${ENUM_ERROR_COUNT} |"
  echo "| Errors (total) | ${TOTAL_ERROR_COUNT} |"
  echo "| Parallel rc | ${PARALLEL_RC} |"
  echo "| Completed jobs | ${COMPLETED_JOBS}/${EXPECTED_JOBS} |"
  echo "| Failed workers | ${FAILED_JOBS} |"
  echo ""
  echo "### Quick links"
  echo ""
  echo "- \`${INFECTED_TSV}\`"
  echo "- \`${SIG_COUNTS}\`"
  echo "- \`${TOP_DIRS}\`"
  echo "- \`${INF_BY_EXT}\`"
  echo "- \`${ERRORS_LOG}\`"
  echo "- \`${ERRORS_BY_MSG}\`"
  echo "- \`${ENUM_ERRORS_LOG}\`"
  echo "- \`${JOBLOG}\`"
  echo "- \`${PARALLEL_PROGRESS}\`"
  echo "- \`${RAW}\`"
  if [[ -n "$INFECTED_REPORT" ]]; then
    echo "- \`${INFECTED_REPORT}\` (compact infected-only report)"
  fi
} > "${SUMMARY_MD}"

# Minimal JSON metrics (no jq required)
json_escape() {
  awk 'BEGIN {for (i=1;i<32;i++) controls[sprintf("%c",i)]=i}
    {
      if (NR>1) printf "\\n"
      for (i=1;i<=length($0);i++) {
        c=substr($0,i,1)
        if (c=="\\" || c=="\"") printf "\\%s",c
        else if (c in controls) printf "\\u%04x",controls[c]
        else printf "%s",c
      }
    }'
}
{
  printf '{\n'
  printf '  "host": "%s",\n'   "$(printf "%s" "$HOST" | json_escape)"
  printf '  "kernel": "%s",\n' "$(printf "%s" "$KERNEL" | json_escape)"
  printf '  "start_epoch": %d,\n' "${START_TS}"
  printf '  "end_epoch": %d,\n' "${END_TS}"
  printf '  "duration_sec": %d,\n' "${DUR}"
  printf '  "threads": %d,\n' "${THREADS}"
  printf '  "chunk_size": %d,\n' "${CHUNK_SIZE}"
  printf '  "targets": "%s",\n' "$(printf "%s" "${TARGETS[*]}" | json_escape)"
  printf '  "pruned_paths": "%s",\n' "$(printf "%s" "${PRUNES[*]}" | json_escape)"
  printf '  "stay_on_fs": %d,\n' "$STAY_ON_FS"
  printf '  "db_status": "%s",\n' "$(printf "%s" "$DB_STATUS" | json_escape)"
  printf '  "db_dir_used": "%s",\n' "$(printf "%s" "$DB_DIR_USED" | json_escape)"
  printf '  "db_max_age_days": %d,\n' "$DB_MAX_AGE"
  printf '  "infected_report": "%s",\n' "$(printf "%s" "$INFECTED_REPORT" | json_escape)"
  printf '  "total_files": %d,\n' "${TOTAL_FILES}"
  printf '  "bytes_measured": %s,\n' "${BYTES_MEASURED}"
  if [[ "$BYTES_MEASURED" == true ]]; then
    printf '  "total_bytes": %d,\n' "${TOTAL_BYTES}"
    printf '  "throughput_mb_s": %s,\n' "${MBPS}"
  else
    printf '  "total_bytes": null,\n'
    printf '  "throughput_mb_s": null,\n'
  fi
  printf '  "files_per_s": %s,\n' "${FPS}"
  printf '  "infected": %d,\n' "${INFECTED_COUNT}"
  printf '  "detections": %d,\n' "$DETECTION_COUNT"
  printf '  "limit_alerts": %d,\n' "$LIMIT_ALERT_COUNT"
  printf '  "scan_complete": %s,\n' "$SCAN_COMPLETE"
  printf '  "clean_approx": %s,\n' "${CLEAN_COUNT}"
  printf '  "scan_errors": %d,\n' "${ERROR_COUNT}"
  printf '  "enum_errors": %d,\n' "${ENUM_ERROR_COUNT}"
  printf '  "skipped_targets": %d,\n' "$SKIPPED_TARGET_COUNT"
  printf '  "errors_total": %d,\n' "${TOTAL_ERROR_COUNT}"
  printf '  "completed_jobs": %d,\n' "$COMPLETED_JOBS"
  printf '  "failed_jobs": %d,\n' "$FAILED_JOBS"
  printf '  "scanner_errors": %d,\n' "$SCANNER_ERROR_COUNT"
  printf '  "detection_jobs": %d,\n' "$DETECTION_JOBS"
  printf '  "runner_errors": %d,\n' "$RUNNER_ERROR_COUNT"
  printf '  "parallel_rc": %d\n' "${PARALLEL_RC}"
  printf '}\n'
} > "${METRICS_JSON}"

# ----------------------------- Exit semantics ------------------------------- #
# Prefer explicit failure on errors over infection-only
EXIT_CODE=0
if (( TOTAL_ERROR_COUNT > 0 )); then
  EXIT_CODE=2
elif (( DETECTION_COUNT > 0 || DETECTION_JOBS > 0 )); then
  EXIT_CODE=1
fi

if (( MAKE_LATEST_LINK )); then
  ln -sfnT -- "$OUTDIR" "$LATEST_LINK"
fi

# Final pointer for humans who can read
echo "[*] Summaries: ${SUMMARY_TXT}  |  ${SUMMARY_MD}"
if [[ -n "$INFECTED_REPORT" ]]; then
	echo "[*] Infected-only report: ${INFECTED_REPORT}"
fi
if (( MAKE_LATEST_LINK )); then
	echo "[*] Latest symlink -> ${LATEST_LINK} -> ${OUTDIR}"
fi
exit "${EXIT_CODE}"
