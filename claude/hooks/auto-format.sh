#!/bin/bash
# Auto-format hook for Edit operations in Claude Code

FILE="$CLAUDE_TOOL_PARAM_file_path"

# Python formatting with ruff
if [[ "$FILE" == *.py ]]; then
  if [ -f "manifest.scm" ]; then
    # Use guix shell if manifest exists
    if guix shell -m manifest.scm -- which ruff >/dev/null 2>&1; then
      echo "Formatting Python file with ruff..." >&2
      guix shell -m manifest.scm -- ruff format "$FILE" 2>/dev/null
    fi
  elif command -v ruff >/dev/null 2>&1; then
    echo "Formatting Python file with ruff..." >&2
    ruff format "$FILE" 2>/dev/null
  fi
fi

# Rust formatting with rustfmt
if [[ "$FILE" == *.rs ]]; then
  if [ -f "manifest.scm" ]; then
    # Use guix shell if manifest exists
    if guix shell -m manifest.scm -- which rustfmt >/dev/null 2>&1; then
      echo "Formatting Rust file with rustfmt..." >&2
      guix shell -m manifest.scm -- rustfmt "$FILE" 2>/dev/null
    fi
  elif command -v rustfmt >/dev/null 2>&1; then
    echo "Formatting Rust file with rustfmt..." >&2
    rustfmt "$FILE" 2>/dev/null
  fi
fi

# Scheme formatting with guix style (for .scm files)
if [[ "$FILE" == *.scm ]] && [[ "$FILE" != *.el ]]; then
  if command -v guix >/dev/null 2>&1; then
    # Only format if it looks like a Guix package/service definition
    if grep -q "define-public\|define-module\|operating-system" "$FILE" 2>/dev/null; then
      echo "Formatting Scheme file with guix style..." >&2
      guix style -f "$FILE" 2>/dev/null || true
    fi
  fi
fi