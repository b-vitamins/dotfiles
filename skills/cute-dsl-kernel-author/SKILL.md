---
name: cute-dsl-kernel-author
description: Write new CuTe DSL kernels and host JIT launch code from problem specs or existing CUDA or CUTLASS patterns. Use when implementing a new CuTe DSL op, tutorial, or production kernel with @cute.kernel, @cute.jit, DLPack interop, and explicit launch configuration.
---

# CuTe DSL Kernel Author

Author correct CuTe DSL kernels from minimal but explicit assumptions.

## Required Rules

- Run every command through `scripts/guix-run`. Do not hide the real command behind `bash -lc`.
- Start from the closest local example in `references/examples/` instead of writing from a blank page when a similar pattern already exists.
- Use `/tmp` for temporary harnesses, generated repros, and throwaway validation scripts.
- Keep compile-time and runtime concerns separate: `@cute.jit` for host orchestration, `@cute.kernel` for device execution.

## Workflow

1. Read the closest docs and example first.
2. State the kernel contract:
   - tensor shapes and layouts
   - dtypes
   - alignment assumptions
   - launch geometry
   - correctness invariants
3. Implement the smallest correct version before adding performance features.
4. Use `from_dlpack` and explicit layout marking deliberately. Do not add `.mark_layout_dynamic()` unless the calling pattern requires it.
5. Validate with a small correctness case whenever practical.

## Preferred References

- Guides: `references/docs/guide/dsl_introduction.md`, `references/docs/guide/dsl_control_flow.md`, `references/docs/guide/dsl_code_generation.md`, `references/docs/guide/dsl_jit_arg_generation.md`, `references/docs/guide/framework_integration.md`
- API: `references/docs/api/cute.md`, `references/docs/api/cute_runtime.md`, `references/docs/api/utils.md`, `references/docs/api/pipeline.md`, `references/docs/api/cute_nvgpu_cpasync.md`
- Examples: `references/examples/hello_world.ipynb`, `references/examples/elementwise_add.py`, `references/examples/elementwise_apply.py`, `references/examples/call_from_jit.py`, `references/examples/dynamic_smem_size.py`, `references/examples/tensorop_gemm.py`, `references/examples/flash_attention_v2.py`

## Authoring Heuristics

- Prefer explicit shapes and layouts over hidden inference.
- Keep host launch code readable: construct tensors, compile if useful, then launch with obvious `grid`, `block`, `cluster`, and `smem`.
- If you borrow a performance pattern from a heavy example such as GEMM or flash attention, strip it back to only the pieces needed for the current problem.
