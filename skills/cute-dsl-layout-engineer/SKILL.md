---
name: cute-dsl-layout-engineer
description: Analyze and design CuTe layouts, tilers, thread-value mappings, tensor partitioning, predication, and memory movement patterns. Use when reasoning about layout algebra, copy atoms, partitioned tensors, TensorSSA flows, or why a CuTe layout or copy strategy works or fails.
---

# CuTe DSL Layout Engineer

Reason about CuTe as a layout and partitioning system first, not just as Python syntax.

## Required Rules

- Run every command through `scripts/guix-run`. Do not put `bash -lc` between `guix shell --` and the real command.
- Use `references/catalog.md` to see the local subset of docs and examples.
- When answering or editing code, always state the logical shape, stride, major mode, thread layout, value layout, and predication strategy you are relying on.
- Use `/tmp` for scratch notebooks, diagrams, and tiny repro drivers unless the user explicitly wants files in the current project.

## Workflow

1. Identify the tensor objects involved and write down:
   - logical shape
   - layout/stride
   - static vs dynamic layout status
   - contiguous dimension and alignment assumptions
2. Determine the partitioning scheme:
   - CTA tiler
   - thread layout
   - value layout
   - copy atom and whether vectorization is legal
3. Verify coordinate coverage:
   - every required element is produced exactly once
   - out-of-bounds accesses are predicated
   - thread/value order matches the intended memory coalescing pattern
4. Only then propose code changes.

## Primary References

- Concepts: `references/docs/guide/dsl_introduction.md`, `references/docs/guide/dsl_dynamic_layout.md`
- API: `references/docs/api/cute.md`, `references/docs/api/cute_arch.md`, `references/docs/api/cute_runtime.md`, `references/docs/api/utils.md`, `references/docs/api/pipeline.md`, `references/docs/api/cute_nvgpu_cpasync.md`, `references/docs/api/cute_nvgpu_warp.md`
- Examples: `references/examples/tensor.ipynb`, `references/examples/tensorssa.ipynb`, `references/examples/cute_layout_algebra.ipynb`, `references/examples/elementwise_add.py`, `references/examples/flash_attention_v2.py`

## Output Standard

- Explain layouts in terms of what each thread owns and why.
- Prefer table-like or coordinate-level explanations over vague prose.
- If multiple layouts are viable, compare them on coalescing, bank conflicts, register pressure, and ease of predication.
