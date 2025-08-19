# Guix System Workflow Instructions

## CRITICAL: Package Management Rules
1. **NEVER** use system package managers (apt, yum, brew)
2. **NEVER** use language package managers globally (pip, npm -g, cargo install)
3. **ALWAYS** use manifest.scm for project dependencies
4. **ALWAYS** wrap commands: `guix shell -m manifest.scm -- <command>`

## Guix-Specific Agents
- `guix-manifest-updater` → Update manifest from pyproject.toml
- `manifest-dependency-checker` → Verify all imports covered
- `guix-package-linter` → Validate package definitions
- `python-import-resolver` → Fix imports with Guix packages

## Common Guix Commands
```bash
# Enter development environment
guix shell -m manifest.scm

# Update user packages
guix pull && guix package -u

# Search for packages
guix search <package>

# Show package details
guix show <package>

# Build from local definition
guix build -f package.scm
```

## Manifest Template
```scheme
(specifications->manifest
 '("python"
   "python-pytest"
   "python-numpy"
   "python-pandas"
   "python-requests"
   "python-black"
   "python-mypy"
   "python-ruff"))
```

## Package Name Mappings
- Python: prefix with `python-`
- Rust: often different (check `guix search`)
- Node: prefix with `node-`
- Emacs: prefix with `emacs-`

## Environment Variables
```bash
export GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"
```

## Troubleshooting
- Module not found → Check manifest.scm
- Command not found → Not in guix shell
- Build fails → Check guix-daemon is running