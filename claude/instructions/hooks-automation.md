# Claude Code Hooks Automation

## Active Hooks Configuration

### PreToolUse Hooks
1. **guix-wrapper.sh** (Bash commands)
   - Automatically wraps commands in `guix shell -m manifest.scm`
   - Replaces `python` → `python3`
   
2. **bash-command-validator.py** (Bash commands)
   - Blocks pip/npm/cargo install attempts
   - Blocks dangerous rm -rf commands
   - Enforces modern tool usage (rg over grep, fd over find)
   - Prevents system package manager usage

### PostToolUse Hooks
1. **auto-format.sh** (Edit/MultiEdit/Write)
   - Auto-formats Python files with ruff
   - Auto-formats Rust files with rustfmt
   - Formats Guix Scheme files with guix style

2. **test-runner-trigger.py** (Edit/MultiEdit/Write)
   - Suggests running tests after Python file edits
   - Prompts for pytest execution after test file changes
   - Reminds to run related tests after implementation changes

### SessionStart Hook
**session-context-loader.py**
- Loads git status and recent commits at session start
- Identifies project type (Guix/Python/Rust/LaTeX)
- Provides contextual reminders based on project files

### Stop Hook
**stop-hook-agent-trigger.py**
- Analyzes session activity
- Suggests appropriate agents based on work done:
  - Test writing after code changes
  - Compilation check after LaTeX edits
  - Dependency verification after manifest changes
  - Commit formatting after git adds

## Hook Exit Codes
- **0**: Success, continue normally
- **2**: Block action, feedback to Claude
- **Other**: Non-blocking error, show to user

## Hook Data Flow
```
Input (JSON via stdin) → Hook Script → Output (stdout/stderr + exit code)
```

## Key Environment Variables
- `$CLAUDE_PROJECT_DIR`: Absolute path to project root
- `$CLAUDE_TOOL_PARAM_*`: Tool-specific parameters

## Security Notes
- All hooks run with user permissions
- Scripts must be executable (`chmod +x`)
- Validate all inputs from stdin
- Use absolute paths for scripts
- Never process sensitive files (.env, secrets)

## Debugging Hooks
```bash
# View hook execution details
claude --debug

# Test hook manually
echo '{"tool_name":"Bash","tool_input":{"command":"pip install requests"}}' | ~/.claude/hooks/bash-command-validator.py

# Check hook registration
claude  # Then use /hooks command
```

## Hook Patterns for Common Tasks

### Block Dangerous Operations
```python
if re.search(r'dangerous_pattern', command):
    print("Error message", file=sys.stderr)
    sys.exit(2)  # Block and inform Claude
```

### Auto-Approve Safe Operations
```python
output = {
    "decision": "approve",
    "reason": "Auto-approved safe operation",
    "suppressOutput": True
}
print(json.dumps(output))
sys.exit(0)
```

### Add Context to Session
```python
output = {
    "hookSpecificOutput": {
        "hookEventName": "SessionStart",
        "additionalContext": "Context to add"
    }
}
print(json.dumps(output))
```

### Suggest Next Actions
```python
output = {
    "decision": "block",
    "reason": "Suggested actions:\n• Action 1\n• Action 2"
}
print(json.dumps(output))
```