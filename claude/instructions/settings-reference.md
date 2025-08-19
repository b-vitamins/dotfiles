# Settings Configuration Reference

## Current Settings Configuration

### User Settings (`~/.claude/settings.json`)
- **Model**: Opus (claude-opus-4-1)
- **Transcript Retention**: 7 days (reduced from default 30)
- **Git Co-authoring**: Disabled (cleaner commit messages)
- **Status Line**: Custom bash script with git/Guix awareness

### Environment Variables Set
```bash
# Performance & Privacy
DISABLE_AUTOUPDATER=1              # No auto-updates (Guix manages)
DISABLE_TELEMETRY=1                # Privacy: no telemetry
DISABLE_ERROR_REPORTING=1          # Privacy: no error reporting
DISABLE_NON_ESSENTIAL_MODEL_CALLS=1  # Save tokens
DISABLE_COST_WARNINGS=1            # No cost warnings

# Bash Configuration
BASH_DEFAULT_TIMEOUT_MS=300000     # 5 minutes default timeout
BASH_MAX_TIMEOUT_MS=600000         # 10 minutes max timeout
BASH_MAX_OUTPUT_LENGTH=50000       # 50K chars output limit

# Claude Configuration
CLAUDE_BASH_MAINTAIN_PROJECT_WORKING_DIR=1  # Stay in project dir
CLAUDE_CODE_MAX_OUTPUT_TOKENS=8192          # Increased token limit
USE_BUILTIN_RIPGREP=0                       # Use system ripgrep

# Development
PYTHON=python3                      # Always use python3
```

### Global Configuration
```bash
autoUpdates=false                   # Guix manages updates
theme=dark                          # Dark theme
preferredNotifChannel=notifications_disabled  # No notifications
```

## Settings Hierarchy (Precedence)

1. **Enterprise managed** → `/etc/claude-code/managed-settings.json`
2. **Command line args** → `claude --model opus`
3. **Project local** → `.claude/settings.local.json`
4. **Project shared** → `.claude/settings.json`
5. **User settings** → `~/.claude/settings.json`

## Useful Commands

### View Settings
```bash
claude config list              # List all settings
claude config get <key>         # Get specific setting
claude config list -g           # List global settings
```

### Modify Settings
```bash
claude config set <key> <value>           # Project setting
claude config set -g <key> <value>        # Global setting
claude config add <key> <value>           # Add to list
claude config remove <key> <value>        # Remove from list
```

## Project-Specific Settings

Create `.claude/settings.json` in project root:
```json
{
  "permissions": {
    "additionalDirectories": ["../shared/"],
    "allow": ["Bash(docker-compose*)"],
    "deny": ["Write(./production/**)"]
  },
  "env": {
    "PROJECT_ENV": "development"
  }
}
```

## Permission Patterns

### Allow Patterns
```json
"allow": [
  "Bash(make*)",           // All make commands
  "Bash(docker *)",        // All docker commands
  "Read(~/.*)",            // Read dotfiles
  "Write(tests/**)"        // Write to tests directory
]
```

### Deny Patterns
```json
"deny": [
  "Bash(rm -rf*)",         // Dangerous rm
  "Read(**/.env*)",        // Environment files
  "Write(production/**)",  // Production files
  "Bash(sudo*)"           // Sudo commands
]
```

### Ask Patterns
```json
"ask": [
  "Bash(git push*)",       // Confirm pushes
  "WebFetch",              // Confirm web requests
  "Write(*.config.js)"     // Confirm config changes
]
```

## Advanced Settings

### MCP Server Configuration
```json
{
  "enableAllProjectMcpServers": false,
  "enabledMcpjsonServers": ["memory", "github"],
  "disabledMcpjsonServers": ["filesystem"]
}
```

### API Key Helper (for dynamic auth)
```json
{
  "apiKeyHelper": "~/.claude/scripts/get-api-key.sh",
  "env": {
    "CLAUDE_CODE_API_KEY_HELPER_TTL_MS": "3600000"
  }
}
```

### Additional Directories Access
```json
{
  "permissions": {
    "additionalDirectories": [
      "../shared-libraries/",
      "~/documents/references/",
      "/tmp/claude-workspace/"
    ]
  }
}
```

## Timeout Configuration

For ML experiments and long-running processes:
```json
{
  "env": {
    "BASH_DEFAULT_TIMEOUT_MS": "1800000",  // 30 minutes
    "BASH_MAX_TIMEOUT_MS": "3600000"       // 1 hour
  }
}
```

## Privacy Configuration

Maximum privacy settings:
```json
{
  "cleanupPeriodDays": 1,
  "env": {
    "DISABLE_TELEMETRY": "1",
    "DISABLE_ERROR_REPORTING": "1",
    "DISABLE_BUG_COMMAND": "1",
    "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC": "1"
  }
}
```

## Performance Configuration

For large codebases:
```json
{
  "env": {
    "CLAUDE_CODE_MAX_OUTPUT_TOKENS": "16384",
    "BASH_MAX_OUTPUT_LENGTH": "100000",
    "MAX_MCP_OUTPUT_TOKENS": "50000"
  }
}
```