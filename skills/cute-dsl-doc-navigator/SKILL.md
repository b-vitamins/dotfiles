---
name: cute-dsl-doc-navigator
description: Navigate and explain NVIDIA CuTe DSL documentation, API surface, notebooks, and local example corpus. Use when answering CuTe DSL questions, locating the right doc page or example, comparing APIs, building a reading path, or grounding later coding work in the mirrored CuTe references.
---

# CuTe DSL Doc Navigator

Resolve CuTe DSL questions against the local NVIDIA CUTLASS **4.8.0** documentation
and release-pinned examples before guessing. Check the installed package version
separately: documentation availability does not establish runtime support.

## Required Rules

- Documentation work needs only file reading and search (`rg` or `grep`). It never needs a
  GPU or a CuTe installation; route anything that runs kernels to the specialized CuTe skills.
- Prefer the local copies in `references/docs/` and `references/examples/` over remote browsing.
- Use `references/catalog.md` first when you need to see the skill's full coverage.
- Cite exact local file paths when handing off to the user or another skill.
- Use `references/examples/cutlass-4.8.0/` for current examples. The unversioned
  examples are preserved historical material, not the 4.8.0 API authority.
- Read `references/version-4.8.0.md` for version provenance, architecture boundaries,
  and Hopper optimization entrypoints. Do not infer Hopper support from a
  Blackwell/Rubin example or treat experimental APIs as stable.

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
  Read `references/docs/overview.md`, `references/docs/quick_start.md`, `references/docs/guides/notebooks.md`, then `references/examples/cutlass-4.8.0/notebooks/1_dsl_features/01_hello_world.ipynb`.
- Layout and tensor reasoning:
  Read `references/docs/guide/dsl_dynamic_layout.md`, `references/docs/api/cute.md`, `references/docs/api/utils.md`, then `references/examples/cutlass-4.8.0/cute/notebooks/cute_layout_algebra.ipynb` and `references/examples/cutlass-4.8.0/cute/notebooks/composed_layout.ipynb`.
- Kernel authoring:
  Read `references/docs/guide/dsl_introduction.md`, `references/docs/guide/dsl_control_flow.md`, `references/docs/guide/dsl_jit_arg_generation.md`, then `references/examples/cutlass-4.8.0/cute/ampere/kernel/elementwise/elementwise_add.py`.
- Performance work:
  Read `references/docs/guides/autotuning_gemm.md`, `references/docs/guides/iket_profiling.md`, `references/docs/api/pipeline.md`, and `references/docs/api/cute_nvgpu_cpasync.md`. Use architecture-matched GEMM and attention examples under `references/examples/cutlass-4.8.0/cute/`.
- Hopper specialization:
  Read `references/docs/guides/mma/wgmma_programming.md`, `references/docs/api/cute_nvgpu_warpgroup.md`, `references/docs/api/utils_sm90.md`, then the Hopper persistent GEMM and FMHA examples identified in `references/version-4.8.0.md`.
- Lower-level and scheduling APIs:
  Read `references/docs/primitives.md`, `references/docs/ts_general/ts_introduction.md`, and `references/docs/ts_general/ts_validation.md`. Confirm target support before adopting an experimental example.

## Corpus

- `references/docs/` contains the published 4.8.0 CuTe DSL Markdown snapshot;
  `manifest.json` records source pages and any missing upstream assets.
- `references/examples/cutlass-4.8.0/` contains the full CuTe DSL example tree from
  NVIDIA CUTLASS tag `v4.8.0`, with commit and per-file hashes in `manifest.json`.
- `references/catalog.md` lists the exact mirrored files.

The mirrored files are generated from a pinned cute-docs snapshot by the suite generator
in `_cute-dsl-support/` next to this skill's source; refresh them there instead of editing
them by hand.
