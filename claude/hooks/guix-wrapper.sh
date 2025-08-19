#!/bin/bash
# Guix-aware wrapper for Bash commands in Claude Code
# This hook automatically wraps commands in guix shell when a manifest.scm exists

COMMAND="$CLAUDE_TOOL_PARAM_command"

# Replace python with python3
if [[ "$COMMAND" =~ ^python[[:space:]] ]]; then
  echo "NOTE: Replacing 'python' with 'python3'" >&2
  COMMAND="${COMMAND/python /python3 }"
fi

# Check if we should wrap in guix shell
if [ -f "manifest.scm" ]; then
  # Commands that need guix shell wrapping
  NEEDS_GUIX_REGEX="^(python3|pytest|ruff|pyright|mypy|black|isort|cargo|rustc|rustfmt|clippy|npm|node|pnpm|yarn)"

  # Skip if already a guix command or if it's a built-in command
  if [[ ! "$COMMAND" =~ ^guix ]] && [[ "$COMMAND" =~ $NEEDS_GUIX_REGEX ]]; then
    echo "NOTE: Wrapping command in 'guix shell -m manifest.scm --' due to manifest.scm presence" >&2
    COMMAND="guix shell -m manifest.scm -- $COMMAND"
  fi
fi

# Export the potentially modified command
export CLAUDE_TOOL_PARAM_command="$COMMAND"