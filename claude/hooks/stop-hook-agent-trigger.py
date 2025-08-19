#!/usr/bin/env python3
"""
Stop hook that triggers agents after certain operations.
Runs appropriate agents based on what was done in the session.
"""
import json
import sys
import re

def analyze_transcript(transcript_path):
    """Analyze transcript to determine what was done."""
    actions = {
        "python_written": False,
        "tests_written": False,
        "latex_edited": False,
        "dependencies_changed": False,
        "commits_needed": False
    }

    try:
        with open(transcript_path, 'r') as f:
            content = f.read()

            # Check for Python file edits
            if re.search(r'"tool_name":\s*"(Write|Edit|MultiEdit)".*\.py"', content):
                actions["python_written"] = True

            # Check for test file creation
            if re.search(r'"tool_name":\s*"Write".*test_.*\.py"', content):
                actions["tests_written"] = True

            # Check for LaTeX edits
            if re.search(r'"tool_name":\s*"(Write|Edit|MultiEdit)".*\.tex"', content):
                actions["latex_edited"] = True

            # Check for manifest/dependency changes
            if re.search(r'"tool_name":\s*"(Write|Edit)".*manifest\.scm"', content) or \
               re.search(r'"tool_name":\s*"(Write|Edit)".*pyproject\.toml"', content):
                actions["dependencies_changed"] = True

            # Check for git adds
            if re.search(r'"tool_name":\s*"Bash".*git\s+add', content):
                actions["commits_needed"] = True

    except Exception:
        pass

    return actions

try:
    input_data = json.load(sys.stdin)
    transcript_path = input_data.get("transcript_path", "")
    stop_hook_active = input_data.get("stop_hook_active", False)

    # Don't create infinite loops
    if stop_hook_active:
        sys.exit(0)

    actions = analyze_transcript(transcript_path)

    suggestions = []

    if actions["python_written"] and not actions["tests_written"]:
        suggestions.append("Consider using python-unit-test-writer agent to create tests")

    if actions["tests_written"]:
        suggestions.append("Run pytest-runner-guix agent to execute tests")

    if actions["latex_edited"]:
        suggestions.append("Use latex-compiler-fixer agent to check compilation")

    if actions["dependencies_changed"]:
        suggestions.append("Run manifest-dependency-checker agent to verify dependencies")

    if actions["commits_needed"]:
        suggestions.append("Use git-commit-formatter agent for conventional commits")

    if suggestions:
        output = {
            "decision": "block",
            "reason": "Suggested next steps:\n• " + "\n• ".join(suggestions)
        }
        print(json.dumps(output))  # noqa: print
        sys.exit(0)

except Exception:
    sys.exit(0)