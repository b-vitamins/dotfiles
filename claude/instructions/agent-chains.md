# Agent Execution Chains & Patterns

## Proactive Agent Triggers
These agents should run automatically in these situations:

### After Writing Code
1. Language-specific formatter/linter
2. `security-secret-scanner` (if touching sensitive files)
3. Appropriate test writer agent
4. `test-coverage-analyzer` (after tests added)

### Before Commits
1. `security-secret-scanner`
2. `git-commit-formatter`

### After Dependency Changes
1. `guix-manifest-updater` (Python projects)
2. `manifest-dependency-checker`
3. `cargo-dependency-updater` (Rust projects)

## Effective Agent Chains

### Python Feature Development
```
python-import-resolver → python-unit-test-writer → python-property-test-writer
→ python-ruff-fixer → test-coverage-analyzer
```

### Bug Fix Workflow
```
python-regression-tester (capture bug) → [fix code] → pytest-runner-guix
→ python-mutation-tester (validate fix quality)
```

### LaTeX Paper Preparation
```
latex-compiler-fixer → latex-package-resolver → latex-figure-organizer
→ latex-table-formatter → latex-math-validator → bibtex-citation-checker
```

### Security Audit
```
security-secret-scanner → python-security-test-writer → python-mutation-tester
```

### Performance Optimization
```
python-performance-tester (baseline) → [optimize] → python-performance-tester (verify)
```

### Code Cleanup
```
import-cleaner → dead-function-finder → pattern-replacer → test-coverage-analyzer
```

### Full Test Suite Creation
```
python-unit-test-writer → python-integration-test-writer → python-e2e-test-writer
→ python-property-test-writer → python-mutation-tester
```

## Agent Selection Heuristics

### Use Unit Test Writer When:
- New function/class created
- Refactoring existing code
- Need isolated component testing

### Use Integration Test Writer When:
- Database operations involved
- API endpoints created
- Service interactions needed

### Use E2E Test Writer When:
- Web application workflows
- User journey validation
- Full stack testing required

### Use Property Test Writer When:
- Algorithm implementation
- Data transformation functions
- Parser/serializer code
- Mathematical computations

### Use Security Test Writer When:
- Authentication/authorization code
- Input validation logic
- API endpoints with user data
- Cryptographic operations

### Use Performance Tester When:
- Optimization attempts
- Scaling concerns
- Response time requirements
- Resource usage limits

### Use Regression Tester When:
- After fixing bugs
- Before major refactoring
- Critical feature protection

### Use Mutation Tester When:
- Validating test quality
- Finding weak test cases
- Improving test coverage meaningfulness