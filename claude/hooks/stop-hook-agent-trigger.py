#!/usr/bin/env python3
"""
Intelligently suggest agents after session operations.
Only suggests relevant actions that haven't been done.
"""
import json
import sys
import re
import os

def analyze_transcript(transcript_path):
    """Analyze transcript to determine what was done and what's needed."""
    actions = {
        "python_written": False,
        "tests_written": False,
        "tests_run": False,
        "latex_edited": False,
        "latex_compiled": False,
        "dependencies_changed": False,
        "dependencies_checked": False,
        "git_staged": False,
        "git_committed": False,
        "ruff_run": False,
        "pyright_run": False
    }

    files_modified = set()

    try:
        with open(transcript_path, 'r') as f:
            content = f.read()

            # Track file modifications
            for match in re.finditer(r'"file_path":\s*"([^"]+)"', content):
                files_modified.add(match.group(1))

            # Check for Python file edits
            python_files = [f for f in files_modified if f.endswith('.py')]
            if python_files:
                actions["python_written"] = True

            # Check if tests were written
            test_files = [f for f in python_files if 'test' in f.lower()]
            if test_files:
                actions["tests_written"] = True

            # Check if tests were run
            if re.search(r'pytest\s+.*-xvs|python.*-m\s+pytest', content):
                actions["tests_run"] = True

            # Check for LaTeX edits
            tex_files = [f for f in files_modified if f.endswith('.tex')]
            if tex_files:
                actions["latex_edited"] = True

            # Check if LaTeX was compiled
            if re.search(r'pdflatex|xelatex|lualatex|latex', content):
                actions["latex_compiled"] = True

            # Check for manifest/dependency changes
            if any(f.endswith('manifest.scm') or f.endswith('pyproject.toml') for f in files_modified):
                actions["dependencies_changed"] = True

            # Check if dependencies were verified
            if re.search(r'manifest-dependency-checker|guix\s+shell.*--check', content):
                actions["dependencies_checked"] = True

            # Check for git operations
            if re.search(r'git\s+add\s+', content):
                actions["git_staged"] = True

            if re.search(r'git\s+commit', content):
                actions["git_committed"] = True

            # Check if linters were run
            if re.search(r'\bruff\s+(check|format)', content):
                actions["ruff_run"] = True

            if re.search(r'\bpyright\b', content):
                actions["pyright_run"] = True

    except Exception:
        pass

    return actions, files_modified

def get_smart_suggestions(actions, files_modified):
    """Generate context-aware suggestions based on what was done."""
    suggestions = []

    # Python development suggestions
    if actions["python_written"]:
        # Only suggest tests if none were written AND tests haven't been run
        if not actions["tests_written"] and not actions["tests_run"]:
            non_test_files = [f for f in files_modified if f.endswith('.py') and 'test' not in f.lower()]
            if non_test_files and len(non_test_files) <= 3:  # Only for small changes
                suggestions.append("Consider writing tests for the Python code")

        # Only suggest running tests if tests exist but weren't run
        if actions["tests_written"] and not actions["tests_run"]:
            suggestions.append("Run 'pytest -xvs' to execute the tests")

        # Only suggest linting for substantial changes
        if not actions["ruff_run"] and len([f for f in files_modified if f.endswith('.py')]) > 2:
            suggestions.append("Run 'ruff format . && ruff check .' to lint Python code")

    # LaTeX suggestions
    if actions["latex_edited"] and not actions["latex_compiled"]:
        suggestions.append("Compile LaTeX document to check for errors")

    # Dependency suggestions
    if actions["dependencies_changed"] and not actions["dependencies_checked"]:
        suggestions.append("Verify dependencies with 'guix shell -m manifest.scm -- python3 -c \"import sys\"'")

    # Git suggestions - only if substantial work was done
    if actions["git_staged"] and not actions["git_committed"] and len(files_modified) > 1:
        suggestions.append("Consider committing staged changes")

    return suggestions

try:
    input_data = json.load(sys.stdin)
    transcript_path = input_data.get("transcript_path", "")
    stop_hook_active = input_data.get("stop_hook_active", False)

    # Don't create infinite loops
    if stop_hook_active:
        sys.exit(0)

    actions, files_modified = analyze_transcript(transcript_path)
    suggestions = get_smart_suggestions(actions, files_modified)

    # Only show suggestions if there are meaningful ones
    if suggestions and len(suggestions) <= 2:  # Don't overwhelm with suggestions
        output = {
            "decision": "approve",  # Don't block, just inform
            "reason": "Optional next steps:\n• " + "\n• ".join(suggestions)
        }
        print(json.dumps(output))  # noqa: print

    sys.exit(0)

except Exception:
    sys.exit(0)