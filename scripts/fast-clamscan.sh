#!/usr/bin/env bash
# Rapid multi-process ClamAV scan using GNU parallel (12 threads by default).
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
#   0 = no infections, no errors
#   1 = infections found
#   2 = scan errors occurred (permission, I/O, etc.)
#
# You’ll still get performance that doesn’t set your RAM on fire:
# we scan CHUNK_SIZE files per clamscan process so the virus DB isn’t reloaded per file.

set -Eeuo pipefail
umask 077

die() { printf 'Error: %s\n' "$*" >&2; exit 2; }
warn() { printf 'Warning: %s\n' "$*" >&2; }

# --------------------------- Configurable defaults --------------------------- #
THREADS=12                  # Parallel workers
CHUNK_SIZE=400              # Files per clamscan invocation
STAY_ON_FS=0                # 1 = stay on same filesystem(s)
DEFAULT_PRUNES=(/proc /sys /dev /run)
PRUNES=("${DEFAULT_PRUNES[@]}")
SIZE_MODE="auto"            # auto|always|none
SIZE_MAX_FILES=200000       # auto mode threshold

REAL_HOME="${HOME}"
if [[ ${EUID} -eq 0 && -n "${SUDO_USER-}" && "${SUDO_USER}" != "root" ]]; then
	REAL_HOME="$(eval echo "~${SUDO_USER}")"
fi

DB_PATH_DEFAULT="${REAL_HOME}/.local/share/clamav"   # User-local DB default
DB_PATH="${DB_PATH_DEFAULT}"
DB_EXPLICIT=0

# Sensible clamscan flags; we feed file paths, not dirs, so no -r.
CLAM_FLAGS_BASE=(
  --no-summary
  --stdout
  --infected
  --follow-file-symlinks=0
  --follow-dir-symlinks=0
)
EXTRA_CLAM_ARGS=()
MAKE_LATEST_LINK=1

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
  -d, --db PATH           Custom ClamAV database dir (default ${DB_PATH_DEFAULT})
  -x, --xdev              Don't cross filesystem boundaries
      --size              Always compute total bytes (slower on huge trees)
      --no-size            Skip byte sizing (faster startup)
      --size-max-files N  Auto mode threshold (default ${SIZE_MAX_FILES})
      --prune PATH         Add an extra pruned path (repeatable)
      --no-prunes         Don't auto-prune /proc,/sys,/dev,/run
      --extra "ARGS"      Extra clamscan args (split on whitespace)
      --extra-arg ARG     Extra clamscan arg (repeatable; safer for quoting)
      --no-latest          Don't create/update a clam-report-latest symlink
  -h, --help              Show help

Examples:
  $0 -t 12 -d "\$HOME/.local/share/clamav" /home
  sudo $0 -t 12 -x --no-size --extra "--detect-pua=yes --alert-encrypted=yes" /
  $0 --extra-arg '--exclude=.*\\.cache' --extra-arg '--alert-encrypted=yes' ~

Notes:
  - ClamAV requires virus definitions. If clamscan errors with a missing DB,
    download them (e.g. via freshclam) or pass --db to a directory containing
    *.cvd/*.cld files.
USAGE
}

if [[ $# -lt 1 ]]; then usage; exit 1; fi
while [[ $# -gt 0 ]]; do
  case "$1" in
    -t|--threads) THREADS="${2:?}"; shift 2 ;;
    -c|--chunk)   CHUNK_SIZE="${2:?}"; shift 2 ;;
    -o|--out)     OUTDIR="${2:?}"; shift 2 ;;
    -d|--db)      DB_PATH="${2:?}"; DB_EXPLICIT=1; shift 2 ;;
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
case "$SIZE_MODE" in
  auto|always|none) ;;
  *) die "--size/--no-size configuration invalid (SIZE_MODE=$SIZE_MODE)" ;;
esac

# ------------------------------- Prereqs ------------------------------------ #
for bin in clamscan parallel find awk sort grep tee tr wc date sed uname ln rm mktemp head; do
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

OUTDIR="${OUTDIR%/}"
mkdir -p "$OUTDIR"
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

# Maintain a stable "latest" pointer
OUTDIR_PARENT="$(dirname "$OUTDIR")"
LATEST_LINK="${OUTDIR_PARENT}/clam-report-latest"
if (( MAKE_LATEST_LINK )); then
	if [[ -L "$LATEST_LINK" || -e "$LATEST_LINK" ]]; then rm -f "$LATEST_LINK"; fi
	ln -s "$OUTDIR" "$LATEST_LINK"
fi

# -------------------------- DB selection & info ----------------------------- #
db_dir_has_sigs() {
	local dir="$1"
	[[ -d "$dir" ]] || return 1

	shopt -s nullglob
	local candidates=("$dir"/*.cvd "$dir"/*.cld)
	shopt -u nullglob
	(( ${#candidates[@]} > 0 ))
}

db_flag=()
DB_STATUS="clamscan-default"
DB_DIR_USED=""

if db_dir_has_sigs "$DB_PATH"; then
	db_flag=( "--database=$DB_PATH" )
	DB_DIR_USED="$DB_PATH"
	DB_STATUS="custom"
	(( DB_EXPLICIT )) || DB_STATUS="user-default"
else
	if (( DB_EXPLICIT )); then
		if [[ -d "$DB_PATH" ]]; then
			DB_STATUS="custom-empty-fallback"
		else
			DB_STATUS="custom-missing-fallback"
		fi
	fi

	for dir in /var/lib/clamav /usr/local/share/clamav /usr/share/clamav; do
		if db_dir_has_sigs "$dir"; then
			db_flag=( "--database=$dir" )
			DB_DIR_USED="$dir"
			case "$DB_STATUS" in
				custom-*-fallback) DB_STATUS="${DB_STATUS}->system" ;;
				*) DB_STATUS="system" ;;
			esac
			break
		fi
	done
fi

# Optional database metadata (only if sigtool exists)
DB_META_TXT=""
if command -v sigtool >/dev/null 2>&1 && [[ -n "$DB_DIR_USED" ]]; then
  DB_META_TXT="$(sigtool --info "$DB_DIR_USED"/*.{cvd,cld} 2>/dev/null || true)"
fi

CLAM_ARGS=("${CLAM_FLAGS_BASE[@]}")
if (( ${#db_flag[@]} )); then CLAM_ARGS+=("${db_flag[@]}"); fi
if (( ${#EXTRA_CLAM_ARGS[@]} )); then CLAM_ARGS+=("${EXTRA_CLAM_ARGS[@]}"); fi

# Fail early (and clearly) if the database isn't usable.
DB_PROBE_LOG="${OUTDIR}/db-probe.log"
probe_file="${OUTDIR}/.db-probe-file"
: >"$probe_file"
set +e
probe_out="$(clamscan "${CLAM_ARGS[@]}" -- "$probe_file" 2>&1)"
probe_rc=$?
set -e
rm -f "$probe_file" 2>/dev/null || true
if (( probe_rc == 2 )); then
	printf '%s\n' "$probe_out" >"$DB_PROBE_LOG"
	die "ClamAV database not usable; see ${DB_PROBE_LOG}. Run freshclam or pass --db."
fi

# ---------------------------- File enumeration ------------------------------ #
echo "[*] Enumerating files..."

build_find() {
  local tgt="$1"
  local xdev=()
  (( STAY_ON_FS )) && xdev=(-xdev)

  local prune_expr=()
  if (( ${#PRUNES[@]} > 0 )); then
    for p in "${PRUNES[@]}"; do prune_expr+=( -path "$p" -prune -o ); done
  fi

  # Print only real files, NUL-delimited
  find "$tgt" "${xdev[@]}" \( "${prune_expr[@]}" -type f -print0 \) 2>>"$ENUM_ERRORS_LOG" || true
}

: > "$FILELIST"
SKIPPED_TARGETS=()
for t in "${TARGETS[@]}"; do
  [[ -e "$t" ]] || { warn "Skipping non-existent target: $t"; SKIPPED_TARGETS+=("$t"); continue; }
  build_find "$t" >> "$FILELIST"
done

TOTAL_FILES=$(tr -cd '\0' < "$FILELIST" | wc -c | awk '{print $1}')
if [[ "$TOTAL_FILES" -eq 0 ]]; then
  echo "No files found to scan. Exiting." >&2
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

# Run parallel: stdout -> tee RAW; stderr (progress) -> both screen and file
# shellcheck disable=SC2069
set +e
parallel -0 -j "${THREADS}" -N "${CHUNK_SIZE}" --line-buffer --joblog "${JOBLOG}" "${PROGRESS_OPT[@]}" \
  bash -c 'clamscan "$@" 2>&1' _ "${CLAM_ARGS[@]}" -- :::: "${FILELIST}" \
  1> >(tee "${RAW}") \
  2> >(tee "${PARALLEL_PROGRESS}" >&2)
PARALLEL_RC=$?
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
    i=index(line,": ")
    if (i>0) {
      path=substr(line,1,i-1)
      sig=substr(line,i+2)
      printf "%s\t%s\n", path, sig
    }
  }
' "${RAW}" > "${INFECTED_TSV}" || true

grep -a '^ERROR' "${RAW}" > "${ERRORS_LOG}" 2>/dev/null || true

ENUM_ERROR_COUNT=$(wc -l < "${ENUM_ERRORS_LOG}" | awk '{print $1}')
INFECTED_COUNT=$(wc -l < "${INFECTED_TSV}" | awk '{print $1}')
ERROR_COUNT=$(wc -l < "${ERRORS_LOG}" | awk '{print $1}')
CLEAN_COUNT=$(( TOTAL_FILES - INFECTED_COUNT ))
TOTAL_ERROR_COUNT=$(( ENUM_ERROR_COUNT + ERROR_COUNT ))

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
  if (match(f,/\.([^.\/]+)$/ ,m)) ext=tolower(m[1]); else ext="(none)"
  c[ext]++
} END{for (e in c) printf "%d\t%s\n", c[e], e}' "${INFECTED_TSV}" \
  | sort -nr > "${INF_BY_EXT}" || true

# Errors by message (group by trailing phrase like "Permission denied")
awk -F': ' 'BEGIN{OFS="\t"} /^ERROR/ {msg=$NF; gsub(/^[[:space:]]+|[[:space:]]+$/,"",msg); c[msg]++}
  END{for (m in c) print c[m], m}' "${ERRORS_LOG}" \
  | sort -nr > "${ERRORS_BY_MSG}" || true

# ------------------------------- Summaries ---------------------------------- #
START_HUMAN="$(format_epoch "${START_TS}")"
END_HUMAN="$(format_epoch "${END_TS}")"
HOST="$(uname -n 2>/dev/null || echo unknown)"
KERNEL="$(uname -srmo 2>/dev/null || uname -sr 2>/dev/null || echo unknown)"

# TXT summary
{
  echo "=== ClamAV Parallel Scan Summary ==="
  echo "Host:            ${HOST}"
  echo "Kernel:          ${KERNEL}"
  echo "When:            ${START_HUMAN} -> ${END_HUMAN}  (${DUR}s)"
  echo "Workers:         ${THREADS}  (chunk size ${CHUNK_SIZE})"
  echo "Targets:         ${TARGETS[*]}"
  if (( ${#SKIPPED_TARGETS[@]} > 0 )); then
    echo "Skipped targets: ${SKIPPED_TARGETS[*]}"
  fi
  echo "DB mode:         ${DB_STATUS}${DB_DIR_USED:+ (${DB_DIR_USED})}"
  if [[ -n "$DB_META_TXT" ]]; then
    echo "DB metadata:"
    echo "${DB_META_TXT}" | sed 's/^/  /'
  fi
  printf "Files enumerated: %d\n" "${TOTAL_FILES}"
  if [[ "$BYTES_MEASURED" == true ]]; then
    printf "Bytes enumerated: %d bytes (%.2f GiB)\n" "${TOTAL_BYTES}" "$(awk -v b=${TOTAL_BYTES} 'BEGIN{printf (b/1073741824)}')"
  else
    printf "Bytes enumerated: (skipped)\n"
  fi
  printf "Throughput:       %s MB/s, %s files/s\n" "${MBPS}" "${FPS}"
  printf "Infected:         %d\n" "${INFECTED_COUNT}"
  printf "Clean (approx):   %d\n" "${CLEAN_COUNT}"
  printf "Scan errors:      %d\n" "${ERROR_COUNT}"
  printf "Enum errors:      %d\n" "${ENUM_ERROR_COUNT}"
  printf "Errors (total):   %d\n" "${TOTAL_ERROR_COUNT}"
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
  if [[ "$BYTES_MEASURED" == true ]]; then
    echo "| Bytes enumerated | ${TOTAL_BYTES} |"
  else
    echo "| Bytes enumerated | (skipped) |"
  fi
  echo "| Throughput (MB/s) | ${MBPS} |"
  echo "| Files/s | ${FPS} |"
  echo "| Infected | ${INFECTED_COUNT} |"
  echo "| Clean (approx) | ${CLEAN_COUNT} |"
  echo "| Scan errors | ${ERROR_COUNT} |"
  echo "| Enum errors | ${ENUM_ERROR_COUNT} |"
  echo "| Errors (total) | ${TOTAL_ERROR_COUNT} |"
  echo "| Parallel rc | ${PARALLEL_RC} |"
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
} > "${SUMMARY_MD}"

# Minimal JSON metrics (no jq required)
json_escape() { sed 's/\\/\\\\/g; s/"/\\"/g'; }
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
  printf '  "db_status": "%s",\n' "$(printf "%s" "$DB_STATUS" | json_escape)"
  printf '  "db_dir_used": "%s",\n' "$(printf "%s" "$DB_DIR_USED" | json_escape)"
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
  printf '  "clean_approx": %d,\n' "${CLEAN_COUNT}"
  printf '  "scan_errors": %d,\n' "${ERROR_COUNT}"
  printf '  "enum_errors": %d,\n' "${ENUM_ERROR_COUNT}"
  printf '  "errors_total": %d,\n' "${TOTAL_ERROR_COUNT}"
  printf '  "parallel_rc": %d\n' "${PARALLEL_RC}"
  printf '}\n'
} > "${METRICS_JSON}"

# ----------------------------- Exit semantics ------------------------------- #
# Prefer explicit failure on errors over infection-only
EXIT_CODE=0
if (( TOTAL_ERROR_COUNT > 0 )); then
  EXIT_CODE=2
elif (( INFECTED_COUNT > 0 )); then
  EXIT_CODE=1
fi

# Make sure the report is readable by the invoking user when running via sudo.
if [[ ${EUID} -eq 0 && -n "${SUDO_USER-}" && "${SUDO_USER}" != "root" ]]; then
	if command -v id >/dev/null 2>&1 && command -v chown >/dev/null 2>&1; then
		uid=$(id -u "${SUDO_USER}" 2>/dev/null || printf '')
		gid=$(id -g "${SUDO_USER}" 2>/dev/null || printf '')
		if [[ -n "$uid" && -n "$gid" ]]; then
			chown -R "${uid}:${gid}" "${OUTDIR}" 2>/dev/null || true
			if (( MAKE_LATEST_LINK )); then
				chown -h "${uid}:${gid}" "${LATEST_LINK}" 2>/dev/null || true
			fi
		fi
	fi
fi

# Final pointer for humans who can read
echo "[*] Summaries: ${SUMMARY_TXT}  |  ${SUMMARY_MD}"
if (( MAKE_LATEST_LINK )); then
	echo "[*] Latest symlink -> ${LATEST_LINK} -> ${OUTDIR}"
fi
exit "${EXIT_CODE}"
