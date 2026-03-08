---
name: cute-dsl-profiler
description: Profile CuTe DSL kernels with Nsight Compute and Nsight Systems, capture reports under /tmp by default, and summarize occupancy, memory, scheduler, and instruction metrics. Use when a CuTe kernel needs hotspot analysis, roofline clues, warp stall analysis, or source-level performance investigation.
---

# CuTe DSL Profiler

Profile CuTe kernels with disciplined, minimal drivers and machine-readable artifacts.

## Required Rules

- Run every command through `scripts/guix-run`. Do not insert `bash -lc` after `guix shell --`.
- Default generated drivers and report directories to `/tmp`.
- If the driver imports project-local modules, run the profiling command from the target project's root.
- Prefer a tiny single-purpose profiling driver over profiling the full application stack when a small repro is possible.
- Warm up once before the profiled run when you build a custom driver.
- Check launch size before interpreting stalls. If grid size is well below SM count, call out underfill explicitly and profile a more representative shape before concluding the kernel is bad.
- For `run_nsys.py`, bracket the intended region with `torch.cuda.profiler.start()` / `stop()` so the default `cudaProfilerApi` capture range isolates the kernel of interest.
- For CuTe JIT source correlation, run with `CUTE_DSL_LINEINFO=1`. If the kernel may already be cached without line info, also give the profiling run a fresh `CUTE_DSL_CACHE_DIR` under `/tmp` so `run_ncu_source.py` does not silently inspect a stale non-lineinfo binary.

## Scripts

- `scripts/run_ncu.py`: Run a curated set of Nsight Compute sections against any command and write section dumps plus summary files.
- `scripts/run_ncu_source.py`: Run NCU Source Counters and rank hotspot instructions by shared-memory, stall, or memory-sector metrics.
- `scripts/summarize_ncu.py`: Re-summarize an existing NCU output directory.
- `scripts/run_nsys.py`: Capture an Nsight Systems report and stats dump for any command.

## Profiling Workflow

1. Create a small driver under `/tmp` that exercises the target kernel once per process invocation.
2. Run Nsight Compute:

```bash
scripts/guix-run python3 scripts/run_ncu.py --label my-kernel -- python3 /tmp/profile_driver.py
```

3. Run Nsight Systems when timeline context matters:

```bash
scripts/guix-run python3 scripts/run_nsys.py --label my-kernel -- python3 /tmp/profile_driver.py
```

4. Read `summary.txt`, then inspect the raw section files if a metric looks suspicious.
5. If NCU points to shared-memory conflicts or stall-heavy code, localize the hotspot with source counters:

```bash
scripts/guix-run python3 scripts/run_ncu_source.py --label my-kernel -- python3 /tmp/profile_driver.py
```

6. If `run_ncu_source.py` reports that source correlation is unavailable, rerun it with explicit line info and a fresh cache:

```bash
scripts/guix-run env CUTE_DSL_LINEINFO=1 CUTE_DSL_CACHE_DIR=/tmp/cute-lineinfo-cache python3 scripts/run_ncu_source.py --label my-kernel -- python3 /tmp/profile_driver.py
```

## Primary References

- Docs: `references/docs/guide/debugging.md`, `references/docs/guide/autotuning_gemm.md`, `references/docs/api/pipeline.md`, `references/docs/api/cute_nvgpu_cpasync.md`, `references/docs/api/cute_nvgpu_warp.md`, `references/docs/api/cute_nvgpu_warpgroup.md`
- Examples: `references/examples/sgemm.py`, `references/examples/tensorop_gemm.py`, `references/examples/flash_attention_v2.py`, `references/examples/all_reduce.py`

## Interpretation Standard

- Start with occupancy, DRAM BW, shared-memory throughput, and stall reasons.
- Compare profile output to the kernel's intended data movement pattern.
- Separate underfilled launches from true kernel bottlenecks.
- Use source counters when you need to know which instruction family is causing shared-memory or stall issues.
- Treat repeated `LDSM` hotspots plus shared-wavefront excess as a shared-memory layout or tile-shape problem before blaming global memory.
- Do not claim an optimization until the profile and a benchmark both move in the right direction.
