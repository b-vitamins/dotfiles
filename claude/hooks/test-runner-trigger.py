#!/usr/bin/env python3
"""
Automatically trigger test running after Python file edits.
Suggests running tests when Python files are modified.
"""
import json
import sys
import os
import re

try:
    input_data = json.load(sys.stdin)
    tool_name = input_data.get("tool_name", "")
    tool_input = input_data.get("tool_input", {})

    if tool_name not in ["Edit", "MultiEdit", "Write"]:
        sys.exit(0)

    file_path = tool_input.get("file_path", "")

    # Check if Python file was edited
    if file_path.endswith(".py"):
        # Check if it's a test file or implementation file
        if "/test" in file_path or file_path.startswith("test_"):
            # Test file edited - suggest running it
            test_file = os.path.basename(file_path)
            output = {
                "decision": "block",
                "reason": f"Test file '{test_file}' was modified. Run: pytest {file_path} -xvs"
            }
        else:
            # Implementation file edited - suggest running related tests
            module_name = os.path.basename(file_path).replace(".py", "")
            output = {
                "decision": "block",
                "reason": f"Python module '{module_name}' was modified. Consider running tests: pytest tests/test_{module_name}.py -xvs"
            }

        print(json.dumps(output))  # noqa: print
        sys.exit(0)

except Exception as e:
    # Don't block on errors
    sys.exit(0)