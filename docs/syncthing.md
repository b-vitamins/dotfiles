# Syncthing Configuration

Templates and utilities for Syncthing ignore patterns.

## Templates

### Project Templates
- `templates/stignore-base` - Common patterns for all projects
- `templates/stignore-python` - Python-specific ignore patterns
- `templates/stignore-rust` - Rust-specific ignore patterns
- `templates/stignore-js` - JavaScript/Node ignore patterns
- `templates/stignore-cpp` - C++ build artifacts
- `templates/stignore-latex` - LaTeX build artifacts

### Special Directory Templates
- `templates/stignore-gnupg` - GPG directory patterns
- `templates/stignore-password-store` - Password store patterns
- `templates/stignore-documents` - Document directory patterns
- `templates/stignore-media` - Media directories (music, pictures, videos)

## Usage

The `scripts/provision-stignore.sh` script handles all provisioning:

```bash
# Default: provision ~/projects
./scripts/provision-stignore.sh

# Provision home directories only
./scripts/provision-stignore.sh home

# Provision everything (projects + home)
./scripts/provision-stignore.sh all

# Provision specific directory
./scripts/provision-stignore.sh ~/workspace

# Options
./scripts/provision-stignore.sh --force all     # Force overwrite
./scripts/provision-stignore.sh --dry-run home  # Preview changes
./scripts/provision-stignore.sh --list          # List templates
```

## Template Structure

Each template contains patterns specific to its use case:
- **Base**: Git state, .claude/, editors, OS files, secrets
- **Python**: __pycache__, venv, ML artifacts (*.pth, data/, models/)
- **Rust**: target/, Cargo.lock
- **JS**: node_modules/, build outputs
- **C++**: build/, CMake files
- **LaTeX**: *.aux, *.synctex, build artifacts
- **GnuPG**: Lock files, sockets, random_seed
- **Password Store**: Git state, temporary files
- **Documents**: Office temp files, LaTeX artifacts, cloud sync conflicts
- **Media**: Download artifacts (*.part, *.ytdl), player caches, temp files

Templates are automatically combined based on detected project type.