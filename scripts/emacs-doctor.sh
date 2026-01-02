#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)

emacs_dir="$repo_root/emacs"
lisp_dir="$emacs_dir/lisp"

if ! command -v emacs >/dev/null 2>&1; then
  printf '%s\n' "emacs not found in PATH" >&2
  exit 1
fi

exec emacs -q --batch \
  -L "$lisp_dir" \
  --eval "(progn (require 'bv-doctor) (bv-doctor-run-batch))"
