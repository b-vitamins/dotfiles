#!/usr/bin/env python3
"""
Intelligently suggest test running after Python file edits.
Only suggests when tests haven't been run recently.
"""
import json
import sys
import os
import re

# Track which files have had tests suggested (reset per session)
SUGGESTED_TESTS = set()

try:
    input_data = json.load(sys.stdin)
    tool_name = input_data.get("tool_name", "")
    tool_input = input_data.get("tool_input", {})

    if tool_name not in ["Edit", "MultiEdit", "Write"]:
        sys.exit(0)

    file_path = tool_input.get("file_path", "")

    # Only process Python files
    if not file_path.endswith(".py"):
        sys.exit(0)

    # Skip if we already suggested tests for this file
    if file_path in SUGGESTED_TESTS:
        sys.exit(0)

    # Check if it's a test file being edited
    is_test_file = "/test" in file_path or file_path.startswith("test_") or "_test.py" in file_path

    # For test files, only suggest if it's a new test file being created
    if is_test_file and tool_name == "Write":
        test_file = os.path.basename(file_path)
        output = {
            "decision": "continue",  # Don't block, just inform
            "reason": f"New test file '{test_file}' created. Consider running: pytest {file_path} -xvs"
        }
        SUGGESTED_TESTS.add(file_path)
        print(json.dumps(output))  # noqa: print
        sys.exit(0)

    # For implementation files, check if corresponding test exists
    if not is_test_file:
        module_name = os.path.basename(file_path).replace(".py", "")

        # Skip common non-testable files
        if module_name in ["__init__", "setup", "config", "settings", "__main__"]:
            sys.exit(0)

        # Try to detect if tests exist for this module
        possible_test_paths = [
            f"tests/test_{module_name}.py",
            f"test_{module_name}.py",
            f"tests/{module_name}_test.py",
            f"{module_name}_test.py"
        ]

        # Check current directory for test existence
        test_exists = any(os.path.exists(p) for p in possible_test_paths)

        if test_exists:
            # Only suggest for significant edits (not minor tweaks)
            if tool_name in ["Write", "MultiEdit"]:
                output = {
                    "decision": "continue",
                    "reason": f"Module '{module_name}' modified. Tests available at: {[p for p in possible_test_paths if os.path.exists(p)][0]}"
                }
                SUGGESTED_TESTS.add(file_path)
                print(json.dumps(output))  # noqa: print

except Exception:
    # Never block on errors
    sys.exit(0)