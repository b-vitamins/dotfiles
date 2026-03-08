# Benchmark Driver Contract

Use this file when creating a temporary benchmark driver for `scripts/benchmark_python_callable.py`.

## Required API

Create a Python file under `/tmp` that exports:

```python
def build_benchmark(argv: list[str]) -> dict:
    ...
```

The function must return a dictionary with:

- `callable`: required zero-argument callable to benchmark
- `label`: optional human-readable name
- `warmup`: optional warmup count
- `iterations`: optional iterations per timed repeat
- `metadata`: optional JSON-serializable metadata
- `cuda_required`: optional bool, default `True`

## Recommended Pattern

1. Parse `argv`.
2. Import the target kernel or host launcher.
3. Build test tensors.
4. Compile outside the timed loop if compilation is not the subject of the benchmark.
5. Return a zero-arg callable that executes exactly one logical iteration.

## Good Metadata

- shape tuple
- layout and alignment assumptions
- dtypes
- architecture or GPU name if easily available
- variant label
- whether compilation was cached or excluded from timing
