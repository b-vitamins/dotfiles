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

    # Block package managers
    if re.search(r'\b(pip|pip3)\s+install', command):
        issues.append("Use 'guix shell -m manifest.scm' instead of pip install")

    if re.search(r'\bnpm\s+install\s+-g', command):
        issues.append("Use Guix packages instead of npm global installs")

    if re.search(r'\bcargo\s+install\b', command):
        issues.append("Use Guix packages instead of cargo install")

    if re.search(r'\b(apt|apt-get|yum|dnf|brew)\b', command):
        issues.append("Use Guix instead of system package managers")

    # Prefer modern tools
    if re.search(r'\bgrep\b(?!.*\|)', command) and not re.search(r'\brg\b', command):
        issues.append("Use 'rg' (ripgrep) instead of 'grep' for better performance")

    if re.search(r'\bfind\s+.*-name\b', command):
        issues.append("Use 'fd' or 'rg --files' instead of 'find -name'")

    # Python version
    if re.search(r'\bpython\s+(?!3)', command):
        issues.append("Use 'python3' instead of 'python'")

    # Dangerous rm commands
    if re.search(r'\brm\s+-rf\s+[~/]', command):
        issues.append("DANGEROUS: rm -rf on home or root directory blocked")

    if re.search(r'\brm\s+-rf\s+\*', command):
        issues.append("DANGEROUS: rm -rf * blocked - be more specific")

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