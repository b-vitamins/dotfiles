# CuTe DSL 4.8.0: provenance and architecture boundaries

The documentation snapshot identifies itself as 4.8.0 in NVIDIA's published
page metadata. `docs/manifest.json` records page URLs and missing upstream
assets. The examples are from tag `v4.8.0`, commit
`098de2a652cf8f00fd70b2df54051c7eccbb855a`; their manifest contains file hashes.
The unversioned examples beside that directory are older curated material.

Primary release record:
https://github.com/NVIDIA/cutlass/releases/tag/v4.8.0

## Hopper / H200

- WGMMA programming: `docs/guides/mma/wgmma_programming.md`.
- Warpgroup operations and synchronization: `docs/api/cute_nvgpu_warpgroup.md`.
- TMA and asynchronous copies: `docs/api/cute_nvgpu_cpasync.md`.
- Shared-memory layout helpers: `docs/api/utils_sm90.md`.
- Pipelines: `docs/api/pipeline.md`.
- Persistent, warp-specialized GEMM:
  `examples/cutlass-4.8.0/cute/hopper/kernel/dense_gemm/dense_gemm_persistent.py`.
- Fused attention:
  `examples/cutlass-4.8.0/cute/hopper/kernel/attention/fmha.py`.

WGMMA requires architecture-correct compilation (the documented operations
target `sm_90a`), four-warp collective participation, register fences,
commit/wait discipline, and correct shared-memory lifetime. TMA requires its
own completion/barrier and visibility protocol; a WGMMA wait is not a universal
memory fence. Audit both producer/consumer synchronization and accumulator reuse.
Preserve an Ampere implementation: Hopper instructions cannot run on A100.

## Features to evaluate, not assume

The 4.8 release adds an opt-in `cute_ext` compiler pipeline, enabled by
`CUTE_DSL_USE_EXTENSION_COMPILER=1`. It can change emitted PTX/SASS; do not enable
it silently or assume its examples support every architecture. Many new examples
target Blackwell or Rubin, not Hopper. The release also fixes dynamic-stride
`cute.autovec_copy` vectorization and adds compiler-diagnostic examples.

Primitives and task scheduling are experimental APIs introduced in 4.7, available
in the refreshed documentation: `docs/primitives.md`, `docs/ts_general/`, and
`examples/cutlass-4.8.0/experimental/`. IKET profiling is documented in
`docs/guides/iket_profiling.md`. Evaluate these against installed-wheel support
and the actual device, rather than treating the release number as admission.

CuTe DSL 4.8 does not give A100/H200 native full-mantissa FP32 Tensor Core
multiplication. BF16/FP16/TF32 operands and FP32 accumulation are distinct
arithmetic choices. Declare operand, accumulation, recurrence, saved-state,
rounding, range, and error contracts. A mixed-precision implementation can be
appropriate without being strict FP32; stability and arithmetic fidelity need
separate checks. Validate emitted instructions and forward/backward behavior.

Inspect `importlib.metadata` and the imported `cutlass` path in the actual
execution environment. An updated lockfile or local docs mirror does not prove
that existing remote environments have been upgraded or tested.
