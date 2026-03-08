# Profiling Workflow

Use this file when building small profiling repros.

## Nsight Compute

1. Write a tiny driver under `/tmp` that performs one warmup launch and one real launch.
2. Run:

```bash
scripts/guix-run python3 scripts/run_ncu.py --label my-kernel -- python3 /tmp/profile_driver.py
```

3. Read `summary.txt` first.
4. Inspect section dumps only when a metric needs deeper explanation.

## Nsight Systems

Use Nsight Systems when timeline behavior matters: kernel overlap, stream ordering, host gaps, graph replay, or launch serialization.

Run:

```bash
scripts/guix-run python3 scripts/run_nsys.py --label my-kernel -- python3 /tmp/profile_driver.py
```

## Interpretation

- Low occupancy alone is not enough to justify a rewrite.
- High DRAM BW with low tensor utilization usually means the kernel is memory-bound.
- Shared-memory bank conflicts must be interpreted together with the actual copy pattern.
- Always connect the metric back to the code's intended dataflow.
