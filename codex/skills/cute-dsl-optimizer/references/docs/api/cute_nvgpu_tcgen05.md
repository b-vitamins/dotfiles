---
title: "tcgen05 submodule"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_nvgpu_tcgen05.html"
cutlass_docs_version: "4.4.1"
generated_at: "2026-03-07T11:33:21+00:00"
last_updated: "Mar 05, 2026"
---

# tcgen05 submodule

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

## OperandMajorMode

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.OperandMajorMode`

**Signature:** `OperandMajorMode(value)`

Bases: `Enum`

An enumeration for the majorness of the input operands of the MMA.

## OperandSource

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.OperandSource`

**Signature:** `OperandSource(value)`

Bases: `Enum`

An enumeration for the source memory location of the A input operand of the MMA.

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

## MmaTF32Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaTF32Op`

**Signature:** `MmaTF32Op(instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode)`

Bases: `MmaOp`

TF32 tcgen05 MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::tf32` qualifier.

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaTF32Op.descriptive_name`

**Signature:** `descriptive_name = 'tcgen05 TF32 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaTF32Op.__init__`

**Signature:** `__init__(instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode) → None`

## MmaF16BF16Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF16BF16Op`

**Signature:** `MmaF16BF16Op(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode)`

Bases: `MmaOp`

F16/BF16 tcgen05 MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::f16` qualifier.

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF16BF16Op.descriptive_name`

**Signature:** `descriptive_name = 'tcgen05 F16/BF16 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaF16BF16Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode) → None`

## MmaI8Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaI8Op`

**Signature:** `MmaI8Op(ab_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode)`

Bases: `MmaOp`

I8 tcgen05 MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::i8` qualifier.

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaI8Op.descriptive_name`

**Signature:** `descriptive_name = 'tcgen05 I8 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaI8Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode) → None`

## MmaFP8Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaFP8Op`

**Signature:** `MmaFP8Op(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode)`

Bases: `MmaOp`

F8 tcgen05 MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma).

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaFP8Op.descriptive_name`

**Signature:** `descriptive_name = 'tcgen05 F8 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaFP8Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode) → None`

## MmaMXF8Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF8Op`

**Signature:** `MmaMXF8Op(ab_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode)`

Bases: `BlockScaledMmaOp`

MXF8 tcgen05 BlockScaled MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::mxf8f6f4` qualifier.

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF8Op.descriptive_name`

**Signature:** `descriptive_name = 'tcgen05 MXF8 BlockScaled MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF8Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode) → None`

## MmaMXF4Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF4Op`

**Signature:** `MmaMXF4Op(instruction_shape: cutlass.cute.typing.Shape, cta_group: CtaGroup, a_src: OperandSource)`

Bases: `BlockScaledMmaOp`

MXF4 tcgen05 BlockScaled MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#tcgen05-mma-instructions-mma). This Operation corresponds to the `.kind::mxf4` qualifier.

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF4Op.descriptive_name`

**Signature:** `descriptive_name = 'tcgen05 MXF4 BlockScaled MMA Operation'`

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

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.MmaMXF4NVF4Op.descriptive_name`

**Signature:** `descriptive_name = 'tcgen05 MXF4NVF4 BlockScaled MMA Operation'`

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

**Signature:** `make_smem_layout_atom(kind: SmemLayoutAtomKind, element_type: Type[cutlass.cute.typing.Numeric], *, loc = None, ip = None) → cutlass.cute.typing.ComposedLayout`

Makes a SMEM layout Atom.

This function creates a composed layout in unit of elements consistent with the requested layout Atom kind and element data type.

### Parameters

-   **kind** ([*SmemLayoutAtomKind*](#cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind)) -- The kind of layout Atom

-   **element_type** (*Type\[Numeric\]*) -- The element data type to construct the layout for

### Returns

The SMEM layout atom

### Return type

ComposedLayout

## tile_to_mma_shape

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.tile_to_mma_shape`

**Signature:** `tile_to_mma_shape(atom, mma_tile_shape: cutlass.cute.typing.Shape, order: cutlass.cute.typing.IntTuple | None = None, *, loc = None, ip = None)`

Tiles a layout to an MMA shape.

## commit

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.commit`

**Signature:** `commit(mbar_ptr: cutlass.cute.typing.Pointer, mask = None, cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup = <CtaGroup.ONE>, *, loc = None, ip = None) → None`

Perform an arrive operation on a mbarrier upon completion of previous MMA operations.

### Parameters

-   **mbar_ptr** (*Pointer*) -- A pointer to the mbarrier in SMEM

-   **mask** (*Int*) -- An optional multicast mask for the CTAs in the cluster to signal arrival to

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

**Signature:** `find_tmem_tensor_col_offset(tmem_tensor: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Int`

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

**Signature:** `make_tmem_copy(atom: CopyAtom, tmem_tensor: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → TiledCopy`

Makes a Tiled Copy instance from a TMEM Copy Atom and a TMEM tensor.

## make_s2t_copy

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.make_s2t_copy`

**Signature:** `make_s2t_copy(atom: CopyAtom, tmem_tensor: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → TiledCopy`

Makes a Tiled Copy instance from a TMEM Copy Atom and a TMEM tensor.

## get_s2t_smem_desc_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.get_s2t_smem_desc_tensor`

**Signature:** `get_s2t_smem_desc_tensor(atom: CopyAtom, smem_tensor: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

Returns the SMEM descriptor tensor from a S2T copy atom and a SMEM tensor.

## make_umma_smem_desc

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.tcgen05.make_umma_smem_desc`

**Signature:** `make_umma_smem_desc(src: cutlass.cute.typing.Pointer, layout: cutlass.cute.typing.Layout, major: str, next_src: cutlass.cute.typing.Pointer | None = None, *, loc = None, ip = None)`

Construct shared memory descriptor for UMMA.

The make_umma_smem_desc operation accepts an input cute.ptr (optionally a nextSrc pointer for the second buffer in a circular buffer scheme), alongside a cute.layout and a major attr, then constructs the shared memory descriptor and returns it. The layout must be describing the buffer pointed to by the input pointer and the iterator must carry valid swizzle information.

There are 5 supported swizzle variants:

-   S\<0, 4, 3\> \| SWIZZLE_NONE

-   S\<1, 4, 3\> \| SWIZZLE_32B

-   S\<2, 4, 3\> \| SWIZZLE_64B

-   S\<3, 4, 3\> \| SWIZZLE_128B

-   S\<2, 5, 2\> \| SWIZZLE_128B_BASE32B

The cute.ptr must carry shared address space and must be aligned to 16B.

### Parameters

-   **src** (*Pointer*) -- The source pointer to shared memory

-   **layout** (*Layout*) -- The layout describing the buffer

-   **major** (*str*) -- The major mode attribute

-   **next_src** (*Optional\[Pointer\]*) -- Optional next source pointer for circular buffer scheme

### Returns

The shared memory descriptor

### Return type

SmemDescType
