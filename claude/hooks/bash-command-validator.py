#!/usr/bin/env python3
"""
Bash command validator for Guix System.
Blocks dangerous commands and enforces best practices.
"""
import json
import re
import sys

def validate_command(command: str) -> list[str]:
    """Validate bash commands against Guix best practices."""
    issues = []

    # Block package managers - but allow pip in guix shell context
    if re.search(r'\b(pip|pip3)\s+install', command) and 'guix shell' not in command:
        issues.append("Use 'guix shell -m manifest.scm' instead of pip install")

    if re.search(r'\bnpm\s+install\s+-g', command) and 'guix shell' not in command:
        issues.append("Use Guix packages instead of npm global installs")

    # cargo install is blocked, but cargo build/test/check are fine
    if re.search(r'\bcargo\s+install\b', command) and 'guix shell' not in command:
        issues.append("Use Guix packages instead of cargo install")

    if re.search(r'\b(apt|apt-get|yum|dnf|brew)\b', command):
        issues.append("Use Guix instead of system package managers")

    # Note: Removed artificial grep/find restrictions - both are perfectly valid tools

    # Python version - only check for python as a command, not in paths
    # Also allow shebang lines and python -m usage
    # Match python only when it's a command (not part of a path)
    if re.search(r'(?:^|[;&|]\s*)python(?:\s+(?!3|-m)|\s*$)', command) and '#!/usr/bin/env python' not in command:
        issues.append("Use 'python3' instead of 'python'")

    # Dangerous rm commands - be more precise
    if re.search(r'\brm\s+-rf\s+(/|~/?\s*$)', command):
        issues.append("DANGEROUS: rm -rf on home or root directory blocked")

    # Allow rm -rf * in specific subdirectories, block only in dangerous locations
    if re.search(r'\brm\s+-rf\s+\*', command) and not re.search(r'(\./|tmp/|build/|dist/|__pycache__|node_modules/)', command):
        issues.append("DANGEROUS: rm -rf * in current directory - be more specific")

    return issues

try:
    input_data = json.load(sys.stdin)
    tool_name = input_data.get("tool_name", "")
    command = input_data.get("tool_input", {}).get("command", "")

    if tool_name != "Bash":
        sys.exit(0)

    issues = validate_command(command)

    if issues:
        for issue in issues:
            print(f"• {issue}", file=sys.stderr)  # noqa: print
        sys.exit(2)  # Block and show to Claude

except Exception as e:
    print(f"Hook error: {e}", file=sys.stderr)  # noqa: print
    sys.exit(1)