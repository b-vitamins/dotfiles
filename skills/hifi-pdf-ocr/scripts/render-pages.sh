#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Render PDF pages to PNG images at high resolution with Poppler's pdftoppm.
Requires pdftoppm on PATH, or its path in the PDFTOPPM environment variable.

Usage:
  render-pages.sh --pdf BOOK.pdf [--outdir pages] [--dpi 600] [--prefix page] [--start N] [--end N] [--overwrite]

Examples:
  render-pages.sh --pdf book.pdf --outdir pages --dpi 600
  render-pages.sh --pdf book.pdf --outdir pages --start 120 --end 180
EOF
}

pdf_path=""
outdir="pages"
dpi="600"
prefix="page"
start_page=""
end_page=""
overwrite="0"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --pdf)
      pdf_path="${2:-}"
      shift 2
      ;;
    --outdir)
      outdir="${2:-}"
      shift 2
      ;;
    --dpi)
      dpi="${2:-}"
      shift 2
      ;;
    --prefix)
      prefix="${2:-}"
      shift 2
      ;;
    --start)
      start_page="${2:-}"
      shift 2
      ;;
    --end)
      end_page="${2:-}"
      shift 2
      ;;
    --overwrite)
      overwrite="1"
      shift 1
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [[ -z "$pdf_path" ]]; then
  echo "Missing required argument: --pdf" >&2
  usage >&2
  exit 1
fi

if [[ ! -f "$pdf_path" ]]; then
  echo "PDF not found: $pdf_path" >&2
  exit 1
fi

if ! [[ "$dpi" =~ ^[0-9]+$ ]]; then
  echo "--dpi must be a positive integer." >&2
  exit 1
fi

if [[ -n "$start_page" ]] && ! [[ "$start_page" =~ ^[0-9]+$ ]]; then
  echo "--start must be a positive integer." >&2
  exit 1
fi

if [[ -n "$end_page" ]] && ! [[ "$end_page" =~ ^[0-9]+$ ]]; then
  echo "--end must be a positive integer." >&2
  exit 1
fi

if [[ -n "$start_page" && -n "$end_page" ]] && (( start_page > end_page )); then
  echo "--start must be <= --end." >&2
  exit 1
fi

mkdir -p "$outdir"

# A plain glob instead of compgen, which minimal bash builds lack.
existing=0
for file in "$outdir/${prefix}"-*.png; do
  if [[ -e "$file" ]]; then
    existing=1
    break
  fi
done

if [[ "$existing" == "1" ]]; then
  if [[ "$overwrite" == "1" ]]; then
    rm -f "$outdir/${prefix}-"*.png
  else
    echo "Output files already exist in $outdir for prefix ${prefix}-*.png." >&2
    echo "Use --overwrite to replace them." >&2
    exit 1
  fi
fi

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
raw_prefix="$tmpdir/raw-page"

pdftoppm="${PDFTOPPM:-pdftoppm}"
if ! command -v "$pdftoppm" >/dev/null 2>&1; then
  echo "pdftoppm not found. Install Poppler (poppler-utils on Debian/Ubuntu, poppler on" >&2
  echo "Homebrew, conda, or Guix), run this script inside an environment that provides it," >&2
  echo "or set PDFTOPPM to its path." >&2
  exit 1
fi

cmd=("$pdftoppm" -r "$dpi" -png)
if [[ -n "$start_page" ]]; then
  cmd+=(-f "$start_page")
fi
if [[ -n "$end_page" ]]; then
  cmd+=(-l "$end_page")
fi
cmd+=("$pdf_path" "$raw_prefix")

echo "Rendering with command:"
printf '  %q ' "${cmd[@]}"
printf '\n'
"${cmd[@]}"

# pdftoppm zero-pads page numbers to one width, so lexical order is page order.
rendered=()
while IFS= read -r file; do
  rendered+=("$file")
done < <(find "$tmpdir" -maxdepth 1 -type f -name 'raw-page-*.png' | sort)
if [[ "${#rendered[@]}" -eq 0 ]]; then
  echo "No pages were rendered." >&2
  exit 1
fi

count=0
for file in "${rendered[@]}"; do
  page_num="${file##*-}"
  page_num="${page_num%.png}"
  dest="$outdir/${prefix}-${page_num}.png"
  if [[ -e "$dest" ]]; then
    echo "Refusing to overwrite existing file: $dest" >&2
    exit 1
  fi
  mv "$file" "$dest"
  count=$((count + 1))
done

echo "Rendered $count pages to $outdir as ${prefix}-<page>.png."
