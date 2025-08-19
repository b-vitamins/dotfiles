#!/usr/bin/env python3
"""
Load contextual information at session start.
Provides git status, recent commits, and project structure.
"""
import json
import subprocess
import sys
import os

def get_git_info():
    """Get current git status and recent commits."""
    info = []

    try:
        # Git branch
        branch = subprocess.run(
            ["git", "branch", "--show-current"],
            capture_output=True, text=True, timeout=2
        ).stdout.strip()
        if branch:
            info.append(f"Current branch: {branch}")

        # Git status summary
        status = subprocess.run(
            ["git", "status", "--short"],
            capture_output=True, text=True, timeout=2
        ).stdout.strip()
        if status:
            info.append(f"Modified files:\n{status}")

        # Recent commits
        commits = subprocess.run(
            ["git", "log", "--oneline", "-5"],
            capture_output=True, text=True, timeout=2
        ).stdout.strip()
        if commits:
            info.append(f"Recent commits:\n{commits}")

    except Exception:
        pass

    return "\n\n".join(info) if info else None

def get_project_info():
    """Get project type and key files."""
    info = []

    # Check for manifest.scm
    if os.path.exists("manifest.scm"):
        info.append("📦 Guix project detected (manifest.scm found)")
        info.append("Remember: Use 'guix shell -m manifest.scm -- <command>' for all development commands")

    # Check for Python project
    if os.path.exists("pyproject.toml"):
        info.append("🐍 Python project detected (pyproject.toml found)")

    # Check for Rust project
    if os.path.exists("Cargo.toml"):
        info.append("🦀 Rust project detected (Cargo.toml found)")

    # Check for LaTeX project
    tex_files = [f for f in os.listdir(".") if f.endswith(".tex")]
    if tex_files:
        info.append(f"📄 LaTeX project detected ({len(tex_files)} .tex files)")

    return "\n".join(info) if info else None

try:
    input_data = json.load(sys.stdin)
    hook_event = input_data.get("hook_event_name", "")

    if hook_event != "SessionStart":
        sys.exit(0)

    context_parts = []

    # Add git info
    git_info = get_git_info()
    if git_info:
        context_parts.append("=== Git Status ===\n" + git_info)

    # Add project info
    project_info = get_project_info()
    if project_info:
        context_parts.append("=== Project Context ===\n" + project_info)

    if context_parts:
        context = "\n\n".join(context_parts)
        output = {
            "hookSpecificOutput": {
                "hookEventName": "SessionStart",
                "additionalContext": context
            }
        }
        print(json.dumps(output))  # noqa: print

except Exception as e:
    # Don't fail session start
    sys.exit(0)