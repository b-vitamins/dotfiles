#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)

init_file="$repo_root/emacs/init.el"

if ! command -v emacs >/dev/null 2>&1; then
  printf '%s\n' "emacs not found in PATH" >&2
  exit 1
fi

exec emacs --batch \
  -l "$init_file" \
  --eval "(progn (require 'bv-doctor) (bv-doctor-run-live-batch))"
