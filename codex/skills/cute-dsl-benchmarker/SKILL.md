---
name: cute-dsl-benchmarker
description: Benchmark CuTe DSL kernels repeatably with CUDA-event timing, controlled warmup and iteration loops, variant sweeps, and machine-readable reports. Use when comparing kernel variants, building autotune experiments, or producing clean timing artifacts under /tmp.
---

# CuTe DSL Benchmarker

Run repeatable performance measurements without polluting the project tree.

## Required Rules

- Run every command through `scripts/guix-run`. Never put `bash -lc` between `guix shell --` and the real command.
- Default all generated benchmark drivers and output directories to `/tmp` unless the user asks otherwise.
- If the driver imports project-local modules, launch the benchmark command from the target project's root so Python import resolution matches the code under test.
- Label tiny-grid runs as latency microbenches. Do not generalize throughput conclusions from launches that do not produce enough CTAs to keep the device busy.
- Keep compilation out of the timed loop unless the benchmark is explicitly about compile latency.
- Record exact shapes, layouts, dtypes, warmup count, iteration count, and variant labels in the report metadata.
- When a win is close enough that noise could matter, rerun the top candidates head-to-head with more repeats and inspect `stdev_ms`, not just `mean_ms`.

## Scripts

- `scripts/benchmark_python_callable.py`: Benchmark a Python callable returned by a temporary driver module.
- `scripts/compare_benchmarks.py`: Compare one or more `benchmark.json` or `matrix.json` reports.
- `scripts/run_benchmark_matrix.py`: Run a sweep of variants in isolated subprocesses and collect a matrix report.

## Benchmark Workflow

1. Write a tiny benchmark driver under `/tmp` that exports `build_benchmark(argv)`. See `references/driver_contract.md`.
2. Make the driver return a zero-arg callable plus metadata and sensible warmup/iteration defaults.
3. Run:

```bash
scripts/guix-run python3 scripts/benchmark_python_callable.py /tmp/my_bench.py -- --variant baseline
```

4. For multiple variants, either:
   - pass different `-- --variant ...` arguments to the same driver, or
   - write separate drivers under `/tmp`
5. Compare reports with:

```bash
scripts/guix-run python3 scripts/compare_benchmarks.py /tmp/bench-a/benchmark.json /tmp/bench-b/benchmark.json
```

   `compare_benchmarks.py` also accepts `matrix.json`, which is useful when a sweep produces several viable candidates and you want a quick ranking.

6. For repeatable sweeps, run the matrix helper:

```bash
scripts/guix-run python3 scripts/run_benchmark_matrix.py /tmp/my_bench.py --variant "--variant baseline" --variant "--variant tuned"
```

## Primary References

- Autotune and caching: `references/docs/guide/autotuning_gemm.md`, `references/docs/guide/dsl_jit_caching.md`
- Runtime/API: `references/docs/api/cute_runtime.md`, `references/docs/api/utils.md`
- Examples: `references/examples/elementwise_add.py`, `references/examples/elementwise_apply.py`, `references/examples/sgemm.py`, `references/examples/tensorop_gemm.py`, `references/examples/flash_attention_v2.py`

## Measurement Standard

- Use warmup runs.
- Use multiple timed repeats.
- Prefer CUDA-event timing for in-process kernels.
- Separate correctness checks from timing loops.
- Compare representative shapes, not just the smallest reproducible case.
- Use isolated subprocess sweeps when exploring risky variants so one illegal configuration does not poison the rest of the experiment.
- After heavy profiler sessions, rerun the best benchmark candidates cleanly before declaring a final ranking.
- Keep the report path and the exact command in the final answer.
