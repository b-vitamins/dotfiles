---
name: cute-dsl-optimizer
description: Optimize CuTe DSL kernels using layout changes, cpasync or TMA strategy, pipelining, tensor-core tiling, occupancy tradeoffs, caching, and autotuning heuristics. Use when iterating on performance for GEMM, attention, collective, or custom CuTe kernels after correctness is established.
---

# CuTe DSL Optimizer

Optimize CuTe kernels as a measured loop: baseline, hypothesis, targeted change, re-measure.

## Required Rules

- Run every command through `scripts/guix-run`. Do not place `bash -lc` between `guix shell --` and the actual command.
- Start from a correct, benchmarked baseline.
- Make every candidate variant compile and pass a small correctness check before treating its timing as meaningful.
- Change one performance family at a time so the result is attributable.
- Use `/tmp` for scratch variants, temporary benchmark drivers, and profile artifacts unless the user wants persistent files.
- Treat launch underfill as a workload issue first, not an automatic kernel-design failure.
- Before invasive source edits, sweep the legal configuration space the kernel already exposes: tile sizes, thread count, rasterization, fast-math toggles, and similar launch-time parameters can close large performance gaps without adding maintenance burden.

## Optimization Loop

1. Identify the current bottleneck from benchmark and profile data.
   - If the launch is underfilled, either scale the workload to a representative shape or explicitly keep the conclusion scoped to microbench latency.
   - If the profile points at shared-memory layout pressure, still check whether a different legal tile shape reduces the pressure before rewriting the kernel.
2. Choose one change family:
   - layout or padding
   - vectorization width
   - copy atom choice
   - cpasync or pipeline depth
   - CTA or warp mapping
   - shared-memory vs register tradeoff
   - JIT caching or autotune search space
   - feature toggle such as `fast_sigmoid` or block rasterization
3. Justify the change against the docs and a local example.
4. Re-compile, re-verify, and only then re-measure.
5. Revert flat or regressive source edits quickly. Keep the change only if it improves the targeted bottleneck without breaking maintainability.

## Primary References

- Docs: `references/docs/guide/autotuning_gemm.md`, `references/docs/guide/dsl_code_generation.md`, `references/docs/guide/dsl_jit_caching.md`, `references/docs/api/pipeline.md`, `references/docs/api/cute_nvgpu.md`, `references/docs/api/cute_nvgpu_common.md`, `references/docs/api/cute_nvgpu_cpasync.md`, `references/docs/api/cute_nvgpu_tcgen05.md`, `references/docs/api/cute_nvgpu_warp.md`, `references/docs/api/cute_nvgpu_warpgroup.md`, `references/docs/api/utils_sm90.md`, `references/docs/api/utils_sm100.md`
- Examples: `references/examples/sgemm.py`, `references/examples/tensorop_gemm.py`, `references/examples/flash_attention_v2.py`, `references/examples/all_reduce.py`, `references/examples/distributed_vector_add.py`

## Output Standard

- State the baseline measurement.
- State the hypothesis.
- State the exact code change.
- State the post-change measurement.
- State whether the change should be kept.
