---
name: cute-dsl-doc-navigator
description: Navigate and explain NVIDIA CuTe DSL documentation, API surface, notebooks, and local example corpus. Use when answering CuTe DSL questions, locating the right doc page or example, comparing APIs, building a reading path, or grounding later coding work in the mirrored CuTe references.
---

# CuTe DSL Doc Navigator

Resolve CuTe DSL questions against the local mirrored docs and examples before guessing.

## Required Rules

- Run every command through `scripts/guix-run`. Do not insert `bash -lc` or any other intermediate shell after `guix shell --`.
- Prefer the local copies in `references/docs/` and `references/examples/` over remote browsing.
- Use `references/catalog.md` first when you need to see the skill's full coverage.
- Cite exact local file paths when handing off to the user or another skill.

## Workflow

1. Classify the request as conceptual, API-level, example-driven, debugging, benchmarking, or optimization.
2. Open the narrowest matching references first:
   - onboarding: `references/docs/overview.md`, `references/docs/quick_start.md`
   - language model and decorators: `references/docs/guide/dsl_introduction.md`
   - codegen and control flow: `references/docs/guide/dsl_code_generation.md`, `references/docs/guide/dsl_control_flow.md`
   - runtime/API details: `references/docs/api/`
   - example hunting: `references/examples/`
3. If the docs and examples disagree, say so explicitly and show both sources.
4. When the task moves from reading into implementation, debugging, profiling, or optimization, route to the more specialized CuTe skill instead of trying to do everything here.

## Reading Paths

- First CuTe exposure:
  Read `references/docs/overview.md`, `references/docs/quick_start.md`, `references/docs/guide/notebooks.md`, then `references/examples/hello_world.ipynb`.
- Layout and tensor reasoning:
  Read `references/docs/guide/dsl_dynamic_layout.md`, `references/docs/api/cute.md`, `references/docs/api/utils.md`, then `references/examples/tensor.ipynb`, `references/examples/tensorssa.ipynb`, and `references/examples/cute_layout_algebra.ipynb`.
- Kernel authoring:
  Read `references/docs/guide/dsl_introduction.md`, `references/docs/guide/dsl_control_flow.md`, `references/docs/guide/dsl_jit_arg_generation.md`, then `references/examples/elementwise_add.py`.
- Performance work:
  Read `references/docs/guide/autotuning_gemm.md`, `references/docs/api/pipeline.md`, `references/docs/api/cute_nvgpu_cpasync.md`, then `references/examples/sgemm.py`, `references/examples/tensorop_gemm.py`, and `references/examples/flash_attention_v2.py`.

## Corpus

- `references/docs/` contains the complete local CuTe DSL markdown snapshot.
- `references/examples/` contains the complete local example corpus copied from `/home/b/projects/cute-docs/examples`.
- `references/catalog.md` lists the exact mirrored files.
