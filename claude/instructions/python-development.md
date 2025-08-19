# Python Development Instructions

## Testing Philosophy
- Write tests BEFORE fixing bugs (regression tests)
- Use pytest exclusively (never unittest)
- Test coverage target: 100% for public APIs
- Test file naming: `test_<module>.py` in tests/ directory

## Python Testing Agent Chain
When working on Python code, use agents in this sequence:

### 1. Initial Development
- `python-import-resolver` → Fix import issues first
- `python-unit-test-writer` → Create comprehensive unit tests
- `python-property-test-writer` → Add property tests for algorithmic code

### 2. Integration & System Testing
- `python-integration-test-writer` → Test database/API interactions
- `python-e2e-test-writer` → Full workflow validation
- `python-performance-tester` → Benchmark critical paths

### 3. Quality Assurance
- `python-mutation-tester` → Validate test suite quality
- `python-security-test-writer` → Security vulnerability tests
- `test-coverage-analyzer` → Identify untested code paths

### 4. Bug Fixes & Maintenance
- `python-regression-tester` → Prevent regression after fixes
- `pytest-runner-guix` → Run tests in Guix environment

## Code Quality Standards
- Format with ruff (via `python-ruff-fixer`)
- Type check with pyright (via `python-pyright-resolver`)
- Clean imports with `import-cleaner`
- Remove dead code with `dead-function-finder`

## Guix Integration
- ALWAYS check for manifest.scm
- Use `guix shell -m manifest.scm -- <command>`
- Never use pip, poetry, venv, or conda
- Update manifest with `guix-manifest-updater` when adding deps
- Verify with `manifest-dependency-checker`

## Common Package Mappings
```
requests → python-requests
sklearn → python-scikit-learn
cv2 → opencv (with python-numpy)
PIL/Pillow → python-pillow
yaml → python-pyyaml
```