---
title: "nvgpu.tcgen05"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_nvgpu_tcgen05.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# nvgpu.tcgen05

## Repetition

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Repetition`

**Signature:** `Repetition(value)`

Bases: `Enum`

An enumeration for the number of repetitions of a given TMEM copy within the instruction.

### x1

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Repetition.x1`

**Signature:** `x1 = 1`

### x2

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Repetition.x2`

**Signature:** `x2 = 2`

### x4

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Repetition.x4`

**Signature:** `x4 = 4`

### x8

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Repetition.x8`

**Signature:** `x8 = 8`

### x16

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Repetition.x16`

**Signature:** `x16 = 16`

### x32

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Repetition.x32`

**Signature:** `x32 = 32`

### x64

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Repetition.x64`

**Signature:** `x64 = 64`

### x128

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Repetition.x128`

**Signature:** `x128 = 128`

## TmemLoadRedOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.TmemLoadRedOp`

**Signature:** `TmemLoadRedOp(value)`

Bases: `Enum`

An enumeration for the possible reduce operations for TMEM load operations.

### MAX

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.TmemLoadRedOp.MAX`

**Signature:** `MAX = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### MAXABS

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.TmemLoadRedOp.MAXABS`

**Signature:** `MAXABS = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### MIN

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.TmemLoadRedOp.MIN`

**Signature:** `MIN = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### MINABS

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.TmemLoadRedOp.MINABS`

**Signature:** `MINABS = <class 'sphinx.ext.autodoc.mock._MockObject'>`

## Pack

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Pack`

**Signature:** `Pack(value)`

Bases: `Enum`

An enumeration for the possible packing patterns for TMEM to RMEM copies.

### NONE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Pack.NONE`

**Signature:** `NONE = 1`

### PACK_16b_IN_32b

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Pack.PACK_16b_IN_32b`

**Signature:** `PACK_16b_IN_32b = 2`

## Unpack

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Unpack`

**Signature:** `Unpack(value)`

Bases: `Enum`

An enumeration for the possible unpacking patterns for RMEM to TMEM copies.

### NONE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Unpack.NONE`

**Signature:** `NONE = 1`

### UNPACK_32b_IN_16b

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Unpack.UNPACK_32b_IN_16b`

**Signature:** `UNPACK_32b_IN_16b = 2`

## Ld16x64bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Ld16x64bOp`

**Signature:** `Ld16x64bOp(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition = <Repetition.x1>, pack: ~cutlass.cute.nvgpu.tcgen05.copy.Pack = <Pack.NONE>)`

Bases: `_LdBase`

16x64b TMEM load Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-instructions-tcgen05-ld). This Operation corresponds to the `.16x64b` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Ld16x64bOp.__init__`

**Signature:** `__init__(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition = <Repetition.x1>, pack: ~cutlass.cute.nvgpu.tcgen05.copy.Pack = <Pack.NONE>) → None`

## Ld16x128bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Ld16x128bOp`

**Signature:** `Ld16x128bOp(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition = <Repetition.x1>, pack: ~cutlass.cute.nvgpu.tcgen05.copy.Pack = <Pack.NONE>)`

Bases: `_LdBase`

16x128b TMEM load Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-instructions-tcgen05-ld). This Operation corresponds to the `.16x128b` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Ld16x128bOp.__init__`

**Signature:** `__init__(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition = <Repetition.x1>, pack: ~cutlass.cute.nvgpu.tcgen05.copy.Pack = <Pack.NONE>) → None`

## Ld16x256bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Ld16x256bOp`

**Signature:** `Ld16x256bOp(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition = <Repetition.x1>, pack: ~cutlass.cute.nvgpu.tcgen05.copy.Pack = <Pack.NONE>)`

Bases: `_LdBase`

16x256b TMEM load Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-instructions-tcgen05-ld). This Operation corresponds to the `.16x256b` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Ld16x256bOp.__init__`

**Signature:** `__init__(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition = <Repetition.x1>, pack: ~cutlass.cute.nvgpu.tcgen05.copy.Pack = <Pack.NONE>) → None`

## Ld16x32bx2Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Ld16x32bx2Op`

**Signature:** `Ld16x32bx2Op(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition = <Repetition.x1>, pack: ~cutlass.cute.nvgpu.tcgen05.copy.Pack = <Pack.NONE>)`

Bases: `_LdBase`

16x32bx2 TMEM load Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-instructions-tcgen05-ld). This Operation corresponds to the `.16x32bx2` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Ld16x32bx2Op.__init__`

**Signature:** `__init__(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition = <Repetition.x1>, pack: ~cutlass.cute.nvgpu.tcgen05.copy.Pack = <Pack.NONE>) → None`

## Ld32x32bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Ld32x32bOp`

**Signature:** `Ld32x32bOp(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition = <Repetition.x1>, pack: ~cutlass.cute.nvgpu.tcgen05.copy.Pack = <Pack.NONE>)`

Bases: `_LdBase`

32x32b TMEM load Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-instructions-tcgen05-ld). This Operation corresponds to the `.32x32` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Ld32x32bOp.__init__`

**Signature:** `__init__(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition = <Repetition.x1>, pack: ~cutlass.cute.nvgpu.tcgen05.copy.Pack = <Pack.NONE>) → None`

## St16x64bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.St16x64bOp`

**Signature:** `St16x64bOp(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition, unpack: ~cutlass.cute.nvgpu.tcgen05.copy.Unpack = <Unpack.NONE>)`

Bases: `_StBase`

16x64b TMEM store Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-instructions-tcgen05-st). This Operation corresponds to the `.16x64` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.St16x64bOp.__init__`

**Signature:** `__init__(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition, unpack: ~cutlass.cute.nvgpu.tcgen05.copy.Unpack = <Unpack.NONE>) → None`

## St16x128bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.St16x128bOp`

**Signature:** `St16x128bOp(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition, unpack: ~cutlass.cute.nvgpu.tcgen05.copy.Unpack = <Unpack.NONE>)`

Bases: `_StBase`

16x128b TMEM store Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-instructions-tcgen05-st). This Operation corresponds to the `.16x128` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.St16x128bOp.__init__`

**Signature:** `__init__(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition, unpack: ~cutlass.cute.nvgpu.tcgen05.copy.Unpack = <Unpack.NONE>) → None`

## St16x256bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.St16x256bOp`

**Signature:** `St16x256bOp(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition, unpack: ~cutlass.cute.nvgpu.tcgen05.copy.Unpack = <Unpack.NONE>)`

Bases: `_StBase`

16x256b TMEM store Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-instructions-tcgen05-st). This Operation corresponds to the `.16x256` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.St16x256bOp.__init__`

**Signature:** `__init__(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition, unpack: ~cutlass.cute.nvgpu.tcgen05.copy.Unpack = <Unpack.NONE>) → None`

## St16x32bx2Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.St16x32bx2Op`

**Signature:** `St16x32bx2Op(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition, unpack: ~cutlass.cute.nvgpu.tcgen05.copy.Unpack = <Unpack.NONE>)`

Bases: `_StBase`

16x32x2b TMEM store Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-instructions-tcgen05-st). This Operation corresponds to the `.16x32x2` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.St16x32bx2Op.__init__`

**Signature:** `__init__(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition, unpack: ~cutlass.cute.nvgpu.tcgen05.copy.Unpack = <Unpack.NONE>) → None`

## St32x32bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.St32x32bOp`

**Signature:** `St32x32bOp(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition, unpack: ~cutlass.cute.nvgpu.tcgen05.copy.Unpack = <Unpack.NONE>)`

Bases: `_StBase`

32x32b TMEM store Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-instructions-tcgen05-st). This Operation corresponds to the `.32x32` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.St32x32bOp.__init__`

**Signature:** `__init__(repeat: ~cutlass.cute.nvgpu.tcgen05.copy.Repetition, unpack: ~cutlass.cute.nvgpu.tcgen05.copy.Unpack = <Unpack.NONE>) → None`

## OperandSource

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.OperandSource`

**Signature:** `OperandSource(value)`

Bases: `Enum`

An enumeration for the source memory location of the A input operand of the MMA.

### TMEM

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.OperandSource.TMEM`

**Signature:** `TMEM = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### SMEM

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.OperandSource.SMEM`

**Signature:** `SMEM = <class 'sphinx.ext.autodoc.mock._MockObject'>`

## CtaGroup

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.CtaGroup`

**Signature:** `CtaGroup(value)`

Bases: `Enum`

An enumeration for the `cta_group` qualifier of the MMA.

### ONE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.CtaGroup.ONE`

**Signature:** `ONE = 1`

### TWO

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.CtaGroup.TWO`

**Signature:** `TWO = 2`

## Field

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Field`

**Signature:** `Field(value)`

Bases: `Enum`

An enumeration for the fields of the MMA Atom that can be modified at runtime.

### NEGATE_A

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Field.NEGATE_A`

**Signature:** `NEGATE_A = 'neg_a'`

### NEGATE_B

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Field.NEGATE_B`

**Signature:** `NEGATE_B = 'neg_b'`

### ACCUMULATE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Field.ACCUMULATE`

**Signature:** `ACCUMULATE = 'accum_c'`

### SFA

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Field.SFA`

**Signature:** `SFA = 'sf_a'`

### SFB

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Field.SFB`

**Signature:** `SFB = 'sf_b'`

### METADATA

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Field.METADATA`

**Signature:** `METADATA = 'e'`

### DISABLE_OUTPUT_LANE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.Field.DISABLE_OUTPUT_LANE`

**Signature:** `DISABLE_OUTPUT_LANE = 'disable_output_lane'`

## CollectorOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.CollectorOp`

**Signature:** `CollectorOp(value)`

Bases: `Enum`

An enumeration for the collector operation qualifier of the MMA for SM107.

Collector operations control accumulator behavior in SM107 MMA instructions:

- DISCARD: Discard the accumulator value

- LASTUSE: Mark this as the last use of the accumulator

- FILL: Fill the accumulator

- USE: Use the accumulator normally

### DISCARD

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.CollectorOp.DISCARD`

**Signature:** `DISCARD = 0`

### LASTUSE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.CollectorOp.LASTUSE`

**Signature:** `LASTUSE = 1`

### FILL

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.CollectorOp.FILL`

**Signature:** `FILL = 2`

### USE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.CollectorOp.USE`

**Signature:** `USE = 3`

## MmaTF32Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaTF32Op`

**Signature:** `MmaTF32Op(instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode)`

Bases: `MmaOp`

TF32 tcgen05 MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::tf32` qualifier.

**Supported data type combinations:**

  A Data Type   B Data Type   Acc Type   Mma-K
  ------------- ------------- ---------- -------
  TF32          TF32          F32        8

**Supported architectures:** sm_100a, sm_100f, sm_103a, sm_103f, sm_110a, sm_110f

**Constraints:**

- CtaGroup.ONE: Mma-M in {64, 128}; 8 \<= Mma-N \<= 256, step 8

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- A and B support both K-major and MN-major (transpose), but only with 128B swizzling with 32B swizzle-atomicity. Transpose A requires a_src=SMEM. When a_src=TMEM, A is always K-major.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

- For completion of tcgen05 TMEM load/store operations, use `tcgen05.wait::ld` / `tcgen05.wait::st` (PTX waits).

- For ordering tcgen05 operations across threads, use `tcgen05.fence::before_thread_sync` / `tcgen05.fence::after_thread_sync` (PTX fences) together with an execution-order synchronization mechanism.

<!-- -->

    # CORRECT: warp-uniform tcgen05 MMA
    cute.gemm(mma_atom, d, a, b, c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaTF32Op.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 TF32 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaTF32Op.__init__`

**Signature:** `__init__(instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode) → None`

## MmaF16BF16Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF16BF16Op`

**Signature:** `MmaF16BF16Op(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode)`

Bases: `MmaOp`

F16/BF16 tcgen05 MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::f16` qualifier.

**Supported data type combinations:**

  A Data Type   B Data Type   Acc Type   Mma-K
  ------------- ------------- ---------- -------
  F16           F16           F16, F32   16
  BF16          BF16          F32        16

**Supported architectures:** sm_100a, sm_100f, sm_103a, sm_103f, sm_110a, sm_110f

**Constraints:**

- CtaGroup.ONE: Mma-M in {64, 128}; 8 \<= Mma-N \<= 256, step 8

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- A and B support both K-major and MN-major (transpose), except with 128B swizzling with 32B swizzle-atomicity. Transpose A requires a_src=SMEM. When a_src=TMEM, A is always K-major.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`, as `elect_one()` insertion is handled by the compiler.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

<!-- -->

    # CORRECT: warp-uniform tcgen05 MMA
    cute.gemm(mma_atom, d, a, b, c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF16BF16Op.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 F16/BF16 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF16BF16Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode) → None`

## MmaF16BF16SparseOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF16BF16SparseOp`

**Signature:** `MmaF16BF16SparseOp(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode, sparse_metadata_format: SparseMetadataFormat)`

Bases: `SparseMmaOp`

F16/BF16 tcgen05 Sparse MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma-sp). This Operation corresponds to the `.kind::f16` qualifier with sparse support.

**Supported data type combinations:**

  A Data Type   B Data Type   Acc Type   Mma-K
  ------------- ------------- ---------- -------
  F16           F16           F16, F32   32
  BF16          BF16          F32        32

**Supported architectures:** sm_100a, sm_100f, sm_103a, sm_103f, sm_110a, sm_110f

**Constraints:**

- CtaGroup.ONE: Mma-M in {64, 128}; 8 \<= Mma-N \<= 256, step 8

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- A and B support both K-major and MN-major (transpose), except with 128B swizzling with 32B swizzle-atomicity. Transpose A requires a_src=SMEM. When a_src=TMEM, A is always K-major.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`, as `elect_one()` insertion is handled by the compiler.

- Sparse MMA takes A with its metadata: pass it in `cute.gemm(...)` as `[a, e]`.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

<!-- -->

    # CORRECT: warp-uniform tcgen05 sparse MMA
    cute.gemm(mma_atom, d, [a, e], b, c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF16BF16SparseOp.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 F16/BF16 Sparse MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF16BF16SparseOp.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode, sparse_metadata_format: SparseMetadataFormat) → None`

## MmaFP8SparseOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaFP8SparseOp`

**Signature:** `MmaFP8SparseOp(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode, sparse_metadata_format: SparseMetadataFormat)`

Bases: `SparseMmaOp`

FP8 tcgen05 Sparse MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma-sp). This Operation corresponds to the `.kind::f8f6f4` qualifier with sparse support.

**Supported data type combinations:**

  A Data Type   B Data Type   Acc Type   Mma-K
  ------------- ------------- ---------- -------
  E4M3, E5M2    E4M3, E5M2    F16, F32   64

**Supported architectures:** sm_100a, sm_100f, sm_103a, sm_103f, sm_110a, sm_110f

**Constraints:**

- A and B data types must be the same

- CtaGroup.ONE: Mma-M in {64, 128}; 8 \<= Mma-N \<= 256, step 8

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- With B-major=MN: Mma-N step doubles (16 for CG1, 32 for CG2)

- A and B support both K-major and MN-major (transpose), except with 128B swizzling with 32B swizzle-atomicity. Transpose A requires a_src=SMEM. When a_src=TMEM, A is always K-major.

- With 8-bit B transpose (MN-major): N step changes to 16 for CG1, 32 for CG2.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`, as `elect_one()` insertion is handled by the compiler.

- Sparse MMA takes A with its metadata: pass it in `cute.gemm(...)` as `[a, e]`.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

<!-- -->

    # CORRECT: warp-uniform tcgen05 sparse MMA
    cute.gemm(mma_atom, d, [a, e], b, c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaFP8SparseOp.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 FP8 Sparse MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaFP8SparseOp.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode, sparse_metadata_format: SparseMetadataFormat) → None`

## MmaI8Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaI8Op`

**Signature:** `MmaI8Op(ab_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode)`

Bases: `MmaOp`

I8 tcgen05 MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::i8` qualifier.

**Supported data type combinations:**

  A Data Type   B Data Type   Acc Type   Mma-K
  ------------- ------------- ---------- -------
  Int8, Uint8   Int8, Uint8   Int32      32

**Supported architectures:** sm_100a, sm_100f, sm_103a, sm_103f, sm_110a, sm_110f

**Constraints:**

- CtaGroup.ONE: Mma-M in {64, 128}; Mma-N in {8, 16, 24, 32, 48, 64, 80, ..., 256} (step 8 for Mma-N \<= 32, then step 16 for Mma-N \> 32; values like 40, 56 are invalid)

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- A and B signedness are independent (mixed signed/unsigned allowed)

- A and B support both K-major and MN-major (transpose), except with 128B swizzling with 32B swizzle-atomicity. Transpose A requires a_src=SMEM. When a_src=TMEM, A is always K-major.

- With B MN-major (8-bit B transpose): Mma-N step changes to 16 for CG1, 32 for CG2.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`, as `elect_one()` insertion is handled by the compiler.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

<!-- -->

    # CORRECT: warp-uniform tcgen05 MMA
    cute.gemm(mma_atom, d, a, b, c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaI8Op.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 I8 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaI8Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode) → None`

## MmaFP8Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaFP8Op`

**Signature:** `MmaFP8Op(**kwargs)`

Bases: `MmaOp`

F8 tcgen05 MMA Operation.

Deprecated since version Use: [`MmaF8F6F4Op`](#cutlass.cute.nvgpu.tcgen05.MmaF8F6F4Op) instead.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma).

**Supported data type combinations:**

  A Data Type   B Data Type   Acc Type   Mma-K
  ------------- ------------- ---------- -------
  E4M3, E5M2    E4M3, E5M2    F16, F32   32

**Supported architectures:** sm_100a, sm_100f, sm_103a, sm_103f, sm_110a, sm_110f

**Constraints:**

- A and B data types must be the same

- CtaGroup.ONE: Mma-M in {64, 128}; 8 \<= Mma-N \<= 256, step 8

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- With B-major=MN: Mma-N step doubles (16 for CG1, 32 for CG2)

- A and B support both K-major and MN-major (transpose), except with 128B swizzling with 32B swizzle-atomicity. Transpose A requires a_src=SMEM. When a_src=TMEM, A is always K-major.

- With 8-bit B transpose (MN-major): N step changes to 16 for CG1, 32 for CG2.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`, as `elect_one()` insertion is handled by the compiler.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

<!-- -->

    # CORRECT: warp-uniform tcgen05 MMA
    cute.gemm(mma_atom, d, a, b, c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaFP8Op.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 F8 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaFP8Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode) → None`

## MmaF8F6F4Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF8F6F4Op`

**Signature:** `MmaF8F6F4Op(a_dtype: Type[cutlass.cute.typing.Numeric], b_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode)`

Bases: `MmaOp`

F8F6F4 tcgen05 MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma).

**Supported data type combinations:**

  A Data Type                    B Data Type                    Acc Type   Mma-K
  ------------------------------ ------------------------------ ---------- -------
  E4M3, E5M2, E3M2, E2M3, E2M1   E4M3, E5M2, E3M2, E2M3, E2M1   F16, F32   32

**Supported architectures:** sm_100a, sm_100f, sm_103a, sm_103f, sm_110a, sm_110f

**Constraints:**

- A and B data types are independent (mixed F8/F6/F4 allowed)

- CtaGroup.ONE: Mma-M in {64, 128}; 8 \<= Mma-N \<= 256, step 8

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- With B-major=MN: Mma-N step doubles (16 for CG1, 32 for CG2)

- A and B support both K-major and MN-major (transpose), except with 128B swizzling with 32B swizzle-atomicity. Transpose A requires a_src=SMEM. When a_src=TMEM, A is always K-major.

- With 8-bit B transpose (MN-major): N step changes to 16 for CG1, 32 for CG2.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`, as `elect_one()` insertion is handled by the compiler.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

<!-- -->

    # CORRECT: warp-uniform tcgen05 MMA
    cute.gemm(mma_atom, d, a, b, c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF8F6F4Op.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 F8F6F4 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF8F6F4Op.__init__`

**Signature:** `__init__(a_dtype: Type[cutlass.cute.typing.Numeric], b_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode) → None`

## SM107MmaFP8Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SM107MmaFP8Op`

**Signature:** `SM107MmaFP8Op(a_dtype: ~typing.Type[cutlass.cute.typing.Numeric], b_dtype: ~typing.Type[cutlass.cute.typing.Numeric], acc_dtype: ~typing.Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup, a_src: ~cutlass.cute.nvgpu.tcgen05.mma.OperandSource, a_major_mode: ~cutlass.cute.nvgpu.common.OperandMajorMode | ~cutlass.cute.nvgpu.tcgen05.mma.OperandMajorMode, b_major_mode: ~cutlass.cute.nvgpu.common.OperandMajorMode | ~cutlass.cute.nvgpu.tcgen05.mma.OperandMajorMode, a_collector_op: ~cutlass.cute.nvgpu.tcgen05.mma.CollectorOp | str = <CollectorOp.DISCARD>, b_collector_op: ~cutlass.cute.nvgpu.tcgen05.mma.CollectorOp | str = <CollectorOp.DISCARD>)`

Bases: `MmaOp`

F8 SM107 (Rubin) MMA Operation.

This MMA operation is specifically designed for SM107 architecture (Rubin), with support for collector operations and extended K dimension (64).

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma).

**Supported data type combinations:**

  A Data Type   B Data Type   Acc Type   Mma-K
  ------------- ------------- ---------- --------
  E4M3, E5M2    E4M3, E5M2    F16, F32   32, 64

**Supported architectures:** sm_107a

**Constraints:**

- A and B data types are independent (mixed FP8 allowed)

- CtaGroup.ONE: Mma-M in {64, 128}; 8 \<= Mma-N \<= 256, step 8

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- With B-major=MN: Mma-N step doubles (16 for CG1, 32 for CG2)

- A and B support both K-major and MN-major (transpose), except with 128B swizzling with 32B swizzle-atomicity. Transpose A requires a_src=SMEM. When a_src=TMEM, A is always K-major.

- With 8-bit B transpose (MN-major): N step changes to 16 for CG1, 32 for CG2.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`, as `elect_one()` insertion is handled by the compiler.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

<!-- -->

    # CORRECT: warp-uniform tcgen05 MMA
    cute.gemm(mma_atom, d, a, b, c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SM107MmaFP8Op.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 SM107 F8 MMA Operation'`

### a_collector_op

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SM107MmaFP8Op.a_collector_op`

**Signature:** `a_collector_op: CollectorOp = 0`

### b_collector_op

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SM107MmaFP8Op.b_collector_op`

**Signature:** `b_collector_op: CollectorOp = 0`

### admissible_archs

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SM107MmaFP8Op.admissible_archs`

**Signature:** `admissible_archs = [base_dsl.Arch.sm_107, base_dsl.Arch.sm_107a, base_dsl.Arch.sm_107f]`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SM107MmaFP8Op.__init__`

**Signature:** `__init__(a_dtype: ~typing.Type[cutlass.cute.typing.Numeric], b_dtype: ~typing.Type[cutlass.cute.typing.Numeric], acc_dtype: ~typing.Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup, a_src: ~cutlass.cute.nvgpu.tcgen05.mma.OperandSource, a_major_mode: ~cutlass.cute.nvgpu.common.OperandMajorMode | ~cutlass.cute.nvgpu.tcgen05.mma.OperandMajorMode, b_major_mode: ~cutlass.cute.nvgpu.common.OperandMajorMode | ~cutlass.cute.nvgpu.tcgen05.mma.OperandMajorMode, a_collector_op: ~cutlass.cute.nvgpu.tcgen05.mma.CollectorOp | str = <CollectorOp.DISCARD>, b_collector_op: ~cutlass.cute.nvgpu.tcgen05.mma.CollectorOp | str = <CollectorOp.DISCARD>) → None`

## MmaMXF8Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF8Op`

**Signature:** `MmaMXF8Op(**kwargs)`

Bases: `BlockScaledMmaOp`

MXF8 tcgen05 BlockScaled MMA Operation.

Deprecated since version Use: [`MmaMXF8F6F4Op`](#cutlass.cute.nvgpu.tcgen05.MmaMXF8F6F4Op) instead.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::mxf8f6f4` qualifier.

**Supported data type combinations:**

  A Data Type   B Data Type   SF Data Type   Acc Type   Mma-K   SF Vec Size
  ------------- ------------- -------------- ---------- ------- -------------
  E4M3, E5M2    E4M3, E5M2    UE8M0          F32        32      32

**Supported architectures:** sm_100a, sm_103a

**Constraints:**

- A and B data types must be the same

- CtaGroup.ONE: Mma-M = 128; 8 \<= Mma-N \<= 256, step 8

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- A and B support both K-major and MN-major (transpose), except with 128B swizzling with 32B swizzle-atomicity. Transpose A requires a_src=SMEM. When a_src=TMEM, A is always K-major.

- With 8-bit B transpose (MN-major): N step changes to 16 for CtaGroup.ONE, 32 for CtaGroup.TWO.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`, as `elect_one()` insertion is handled by the compiler.

- For block-scaled MMA, pass A and B as paired operands in `cute.gemm(...)`: `[a, sfa]` and `[b, sfb]`.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

<!-- -->

    # CORRECT: warp-uniform tcgen05 MMA
    cute.gemm(mma_atom, d, [a, sfa], [b, sfb], c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF8Op.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 MXF8 BlockScaled MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF8Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode) → None`

## MmaMXF8F6F4Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF8F6F4Op`

**Signature:** `MmaMXF8F6F4Op(a_dtype: Type[cutlass.cute.typing.Numeric], b_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode)`

Bases: `BlockScaledMmaOp`

MXF8F6F4 tcgen05 BlockScaled MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::mxf8f6f4` qualifier.

**Supported data type combinations:**

  A Data Type                    B Data Type                    SF Data Type   Acc Type   Mma-K   SF Vec Size
  ------------------------------ ------------------------------ -------------- ---------- ------- -------------
  E4M3, E5M2, E3M2, E2M3, E2M1   E4M3, E5M2, E3M2, E2M3, E2M1   UE8M0          F32        32      32

**Supported architectures:** sm_100a, sm_103a

**Constraints:**

- A and B data types are independent (mixed F8/F6/F4 allowed)

- CtaGroup.ONE: Mma-M = 128; 8 \<= Mma-N \<= 256, step 8

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- A and B support both K-major and MN-major (transpose), except with 128B swizzling with 32B swizzle-atomicity. Transpose A requires a_src=SMEM. When a_src=TMEM, A is always K-major.

- With 8-bit B transpose (MN-major): N step changes to 16 for CtaGroup.ONE, 32 for CtaGroup.TWO.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`, as `elect_one()` insertion is handled by the compiler.

- For block-scaled MMA, pass A and B as paired operands in `cute.gemm(...)`: `[a, sfa]` and `[b, sfb]`.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

<!-- -->

    # CORRECT: warp-uniform tcgen05 MMA
    cute.gemm(mma_atom, d, [a, sfa], [b, sfb], c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF8F6F4Op.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 MXF8F6F4 BlockScaled MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF8F6F4Op.__init__`

**Signature:** `__init__(a_dtype: Type[cutlass.cute.typing.Numeric], b_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode | OperandMajorMode, b_major_mode: OperandMajorMode | OperandMajorMode) → None`

## MmaMXF4Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF4Op`

**Signature:** `MmaMXF4Op(instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource)`

Bases: `BlockScaledMmaOp`

MXF4 tcgen05 BlockScaled MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::mxf4` qualifier.

**Supported data type combinations:**

  A Data Type   B Data Type   SF Data Type   Acc Type   Mma-K   SF Vec Size
  ------------- ------------- -------------- ---------- ------- -------------
  E2M1          E2M1          UE8M0          F32        64      32

**Supported architectures:** sm_100a, sm_103a

**Constraints:**

- CtaGroup.ONE: Mma-M = 128; 8 \<= Mma-N \<= 256, step 8

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- Transpose (MN-major) is not supported. Both A and B must be K-major.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`, as `elect_one()` insertion is handled by the compiler.

- For block-scaled MMA, pass A and B as paired operands in `cute.gemm(...)`: `[a, sfa]` and `[b, sfb]`.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

<!-- -->

    # CORRECT: warp-uniform tcgen05 MMA
    cute.gemm(mma_atom, d, [a, sfa], [b, sfb], c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF4Op.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 MXF4 BlockScaled MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF4Op.__init__`

**Signature:** `__init__(instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource) → None`

## MmaMXF4NVF4Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF4NVF4Op`

**Signature:** `MmaMXF4NVF4Op(sf_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource)`

Bases: `BlockScaledMmaOp`

MXF4NVF4 tcgen05 BlockScaled MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::mxf4nvf4` qualifier.

**Supported data type combinations:**

  A Data Type   B Data Type   SF Data Type   Acc Type   Mma-K   SF Vec Size
  ------------- ------------- -------------- ---------- ------- -------------
  E2M1          E2M1          UE8M0, UE4M3   F32        64      16

**Supported architectures:** sm_100a, sm_103a

**Constraints:**

- CtaGroup.ONE: Mma-M = 128; 8 \<= Mma-N \<= 256, step 8

- CtaGroup.TWO: Mma-M in {128, 256}; 16 \<= Mma-N \<= 256, step 16

- Transpose (MN-major) is not supported. Both A and B must be K-major.

**Execution Model:**

- `cute.gemm(...)` (PTX: `tcgen05.mma`) is asynchronous. Issue granularity is single-thread (for `.cta_group::1`) or single-thread in a CTA pair (for `.cta_group::2`), per PTX issue rules.

- In user code, issue `cute.gemm(...)` as warp-uniform and do not wrap it in `elect_one()`, as `elect_one()` insertion is handled by the compiler.

- For block-scaled MMA, pass A and B as paired operands in `cute.gemm(...)`: `[a, sfa]` and `[b, sfb]`.

- To observe/sequence MMA completion for dependent non-pipelined operations, call `cute.nvgpu.tcgen05.commit(...)` (PTX: `tcgen05.commit`) and follow the corresponding completion wait/synchronization path.

<!-- -->

    # CORRECT: warp-uniform tcgen05 MMA
    cute.gemm(mma_atom, d, [a, sfa], [b, sfb], c)

    # Signal completion of prior tcgen05 MMA operations
    with cute.arch.elect_one():
        cute.nvgpu.tcgen05.commit(mbar_ptr, None, cta_group)

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF4NVF4Op.descriptive_name`

**Signature:** `descriptive_name: ClassVar[str] = 'tcgen05 MXF4NVF4 BlockScaled MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF4NVF4Op.__init__`

**Signature:** `__init__(sf_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource) → None`

## SmemLayoutAtomKind

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind`

**Signature:** `SmemLayoutAtomKind(value)`

Bases: `Enum`

Enum class for the kinds of SMEM layout atoms for SM100.

Given a swizzle kind, an SMEM layout atom is the compact layout of smallest size that can be used to construct an SMEM layout using blocked product for operand A or B such that the resulting layout is legal for both TMA and UMMA.

Note that there are other ways of creating legal layouts for operand A and B.

### MN_INTER

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind.MN_INTER`

**Signature:** `MN_INTER = 1`

### MN_SW32

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind.MN_SW32`

**Signature:** `MN_SW32 = 2`

### MN_SW64

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind.MN_SW64`

**Signature:** `MN_SW64 = 3`

### MN_SW128

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind.MN_SW128`

**Signature:** `MN_SW128 = 4`

### MN_SW128_32B

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind.MN_SW128_32B`

**Signature:** `MN_SW128_32B = 5`

### K_INTER

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind.K_INTER`

**Signature:** `K_INTER = 6`

### K_SW32

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind.K_SW32`

**Signature:** `K_SW32 = 7`

### K_SW64

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind.K_SW64`

**Signature:** `K_SW64 = 8`

### K_SW128

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind.K_SW128`

**Signature:** `K_SW128 = 9`

## make_smem_layout_atom

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.make_smem_layout_atom`

**Signature:** `make_smem_layout_atom(kind: SmemLayoutAtomKind, element_type: Type[cutlass.cute.typing.Numeric]) → cutlass.cute.typing.ComposedLayout`

Makes a SMEM layout Atom.

This function creates a composed layout in unit of elements consistent with the requested layout Atom kind and element data type.

### Parameters

- **kind** ([*SmemLayoutAtomKind*](#cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind)) -- The kind of layout Atom

- **element_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element data type to construct the layout for

### Returns

The SMEM layout atom

### Return type

ComposedLayout

## tile_to_mma_shape

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.tile_to_mma_shape`

**Signature:** `tile_to_mma_shape(atom: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, mma_tile_shape: cutlass.cute.typing.Shape, order: cutlass.cute.typing.IntTuple | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

Tiles a layout to an MMA shape.

## commit

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.commit`

**Signature:** `commit(mbar_ptr: cutlass.cute.typing.Pointer, mask: ~typing.Any | None = None, cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup = <CtaGroup.ONE>) → None`

Perform an arrive operation on a mbarrier upon completion of previous MMA operations.

**Single-Thread Execution Required - DSL Does NOT Handle Automatically**: This operation **must** be wrapped in `cute.arch.elect_one()`. Without `elect_one()`, all 32 threads in the warp will execute the commit, causing 32x redundant `tcgen05.commit` PTX instructions.

    # CORRECT: Wrap tcgen05.commit in elect_one
    with cute.arch.elect_one():
        tcgen05.commit(barrier_ptr, None, cta_group)

    # WRONG: Without elect_one, all threads execute (32x redundant)
    tcgen05.commit(barrier_ptr, None, cta_group)

### Parameters

- **mbar_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the mbarrier in SMEM

- **mask** (*Int*) -- An optional multicast mask for the CTAs in the cluster to signal arrival to

- **cta_group** ([*CtaGroup*](#cutlass.cute.nvgpu.tcgen05.CtaGroup)) -- The CTA group size for the operation (ONE or TWO)

> **Note.**
>
> See also
>
> - `cute.arch.elect_one()` - **REQUIRED** wrapper for single-thread execution
>
> - `cute.arch.mbarrier_arrive()` - General barrier arrive operation

## is_tmem_load

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.is_tmem_load`

**Signature:** `is_tmem_load(atom: CopyAtom) → bool`

Returns whether a CopyAtom instance is a TMEM load.

## is_tmem_store

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.is_tmem_store`

**Signature:** `is_tmem_store(atom: CopyAtom) → bool`

Returns whether a CopyAtom instance is a TMEM store.

## get_tmem_copy_properties

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.get_tmem_copy_properties`

**Signature:** `get_tmem_copy_properties(atom: CopyAtom) → Tuple[int, int, int, Pack | Unpack]`

Returns the properties of a TMEM copy atom (number of data paths, bits, repetitions, and whether packing/unpacking is used).

## find_tmem_tensor_col_offset

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.find_tmem_tensor_col_offset`

**Signature:** `find_tmem_tensor_col_offset(tmem_tensor: cutlass.cute.typing.Tensor) → cutlass.cute.typing.Int`

Computes the TMEM column offset given a TMEM tensor.

### Parameters

**tmem_tensor** (*Tensor*) -- The TMEM tensor to use to compute the columns offset

### Returns

The columns offset

### Return type

Int

## make_tmem_copy

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.make_tmem_copy`

**Signature:** `make_tmem_copy(atom: CopyAtom, tmem_tensor: cutlass.cute.typing.Tensor) → TiledCopy`

Makes a Tiled Copy instance from a TMEM Copy Atom and a TMEM tensor.

## make_s2t_copy

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.make_s2t_copy`

**Signature:** `make_s2t_copy(atom: CopyAtom, tmem_tensor: cutlass.cute.typing.Tensor) → TiledCopy`

Makes a Tiled Copy instance from a TMEM Copy Atom and a TMEM tensor.

## append_s2t_broadcast_mode

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.append_s2t_broadcast_mode`

**Signature:** `append_s2t_broadcast_mode(op: _S2TCopyBase, smem_tensor: cutlass.cute.typing.Tensor) → cutlass.cute.typing.Tensor`

Makes the `tcgen05.cp` broadcast explicit in an SMEM operand's layout.

A broadcasting `tcgen05.cp` replicates one SMEM core matrix across `op.broadcast_factor` TMEM core matrices, but only TMEM carries that mode. `make_s2t_copy` builds its tiler from TMEM, so partitioning SMEM without this mode silently borrows modes outside MMA. This appends a size-`broadcast_factor`, zero-stride mode to the MN part of mode 0 (hierarchical `(MN, K)`). Non-broadcasting ops return `smem_tensor` unchanged; otherwise the returned tensor aliases it with a new layout.

### Parameters

- **op** (*\_S2TCopyBase*) -- SMEM to TMEM copy Operation being partitioned for

- **smem_tensor** (*Tensor*) -- SMEM operand with hierarchical `(MN, K)` mode 0

### Returns

`smem_tensor` with the broadcast made explicit in its layout

### Return type

Tensor

### Raises

**ValueError** -- If `op` is not an S2T copy Operation, or if `smem_tensor` lacks the expected hierarchical MMA mode

## get_s2t_smem_desc_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.get_s2t_smem_desc_tensor`

**Signature:** `get_s2t_smem_desc_tensor(atom: CopyAtom, smem_tensor: cutlass.cute.typing.Tensor) → cutlass.cute.typing.Tensor`

Returns the SMEM descriptor tensor from a S2T copy atom and a SMEM tensor.

## make_umma_smem_desc

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.make_umma_smem_desc`

**Signature:** `make_umma_smem_desc(src: cutlass.cute.typing.Pointer, layout: cutlass.cute.typing.Layout, major: str, next_src: cutlass.cute.typing.Pointer | None = None) → Any`

Construct shared memory descriptor for UMMA.

The make_umma_smem_desc operation accepts an input cute.ptr (optionally a nextSrc pointer for the second buffer in a circular buffer scheme), alongside a cute.layout and a major attr, then constructs the shared memory descriptor and returns it. The layout must be describing the buffer pointed to by the input pointer and the iterator must carry valid swizzle information.

There are 5 supported swizzle variants:

- S\<0, 4, 3\> \| SWIZZLE_NONE

- S\<1, 4, 3\> \| SWIZZLE_32B

- S\<2, 4, 3\> \| SWIZZLE_64B

- S\<3, 4, 3\> \| SWIZZLE_128B

- S\<2, 5, 2\> \| SWIZZLE_128B_BASE32B

The cute.ptr must carry shared address space and must be aligned to 16B.

### Parameters

- **src** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- The source pointer to shared memory

- **layout** (*Layout*) -- The layout describing the buffer

- **major** (*str*) -- The major mode attribute

- **next_src** (*Optional\[*[*Pointer*](../basic_data_types.md#cutlass.Pointer)*\]*) -- Optional next source pointer for circular buffer scheme

### Returns

The shared memory descriptor

### Return type

SmemDescType
