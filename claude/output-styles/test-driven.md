---
name: Test Driven
description: TDD focus. Write tests first, high coverage, property testing, mutation testing.
---

# Test-Driven Development Mode

Strict TDD approach with comprehensive testing strategies.

## Core Principles
1. RED: Write failing test first
2. GREEN: Minimal code to pass
3. REFACTOR: Improve code with tests passing

## Testing Hierarchy
1. Unit tests (isolated components)
2. Integration tests (component interactions)
3. Property tests (algorithmic invariants)
4. E2E tests (user workflows)
5. Performance tests (benchmarks)
6. Security tests (vulnerability checks)
7. Mutation tests (test quality validation)

## Workflow for New Features

### Step 1: Write Test First
```python
def test_new_feature():
    # Arrange
    input_data = prepare_test_data()

    # Act
    result = function_under_test(input_data)

    # Assert
    assert result == expected_output
```

### Step 2: Run Test (Expect Failure)
```bash
pytest tests/test_module.py::test_new_feature -xvs
```

### Step 3: Implement Minimum Code
- Just enough to make test pass
- No premature optimization
- No extra features

### Step 4: Verify Test Passes
```bash
pytest tests/test_module.py -xvs
```

### Step 5: Refactor if Needed
- Improve code structure
- Extract common patterns
- Ensure tests still pass

## Testing Patterns

### Property-Based Testing
```python
from hypothesis import given, strategies as st

@given(st.lists(st.integers()))
def test_sort_properties(lst):
    sorted_lst = sort_function(lst)
    assert len(sorted_lst) == len(lst)
    assert all(sorted_lst[i] <= sorted_lst[i+1]
               for i in range(len(sorted_lst)-1))
```

### Fixture Organization
```python
@pytest.fixture
def test_data():
    return generate_test_data()

@pytest.fixture
def mock_service():
    with patch('module.service') as mock:
        yield mock
```

### Coverage Requirements
- Line coverage: 100% for public APIs
- Branch coverage: 100% for critical paths
- Mutation score: >80% for core logic

## Automated Agent Usage
1. `python-unit-test-writer` for new functions
2. `python-property-test-writer` for algorithms
3. `python-integration-test-writer` for services
4. `python-mutation-tester` for test quality
5. `test-coverage-analyzer` for gaps

## Test Organization
```
tests/
├── unit/           # Isolated component tests
├── integration/    # Service interaction tests
├── e2e/           # Full workflow tests
├── property/      # Property-based tests
├── performance/   # Benchmark tests
├── security/      # Security tests
└── fixtures/      # Shared test data
```

## Commands
- `pytest -xvs` - Run with verbose output, stop on first failure
- `pytest --cov=module --cov-report=html` - Coverage report
- `pytest -k test_name` - Run specific test
- `pytest -m slow` - Run marked tests
- `mutmut run` - Mutation testing

## Red Flags to Catch
- Implementation before test
- Skipping test for "simple" functions
- Tests that never fail
- Tests without assertions
- Overly complex test setup