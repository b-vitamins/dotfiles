---
title: "warpgroup submodule"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_nvgpu_warpgroup.html"
cutlass_docs_version: "4.4.1"
generated_at: "2026-03-07T11:33:21+00:00"
last_updated: "Mar 05, 2026"
---

# warpgroup submodule

## OperandMajorMode

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.OperandMajorMode`

**Signature:** `OperandMajorMode(value)`

Bases: `Enum`

An enumeration for the majorness of the input operands of the MMA.

## OperandSource

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.OperandSource`

**Signature:** `OperandSource(value)`

Bases: `Enum`

An enumeration for the source memory location of the A input operand of the MMA.

## Field

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.Field`

**Signature:** `Field(value)`

Bases: `Enum`

An enumeration for the fields of the MMA Atom that can be modified at runtime.

### ACCUMULATE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.Field.ACCUMULATE`

**Signature:** `ACCUMULATE = 'accum_c'`

## MmaF16BF16Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.MmaF16BF16Op`

**Signature:** `MmaF16BF16Op(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode)`

Bases: `MmaOp`

F16/BF16 warpgroup MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#asynchronous-warpgroup-level-matrix-instructions-wgmma-mma). This Operation covers the instructions using the `.f16` or `.bf16` qualifiers for the input operands.

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.MmaF16BF16Op.descriptive_name`

**Signature:** `descriptive_name = 'warpgroup F16/BF16 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.MmaF16BF16Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode) → None`

## MmaF8Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.MmaF8Op`

**Signature:** `MmaF8Op(a_dtype: Type[cutlass.cute.typing.Numeric], b_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode)`

Bases: `MmaOp`

F16/BF16 warpgroup MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#asynchronous-warpgroup-level-matrix-instructions-wgmma-mma). This Operation covers the instructions using the `.e4m3` or `.e5m2` qualifiers for the input operands.

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.MmaF8Op.descriptive_name`

**Signature:** `descriptive_name = 'warpgroup F8 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.MmaF8Op.__init__`

**Signature:** `__init__(a_dtype: Type[cutlass.cute.typing.Numeric], b_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], instruction_shape: cutlass.cute.typing.Shape, a_src: OperandSource, a_major_mode: OperandMajorMode, b_major_mode: OperandMajorMode) → None`

## SmemLayoutAtomKind

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind`

**Signature:** `SmemLayoutAtomKind(value)`

Bases: `Enum`

Enum class for the kinds of SMEM layout atoms for SM90.

Given a swizzle kind, an SMEM layout atom is the compact layout of smallest size that can be used to construct an SMEM layout using blocked product for operand A or B such that the resulting layout is legal for both TMA and UMMA.

Note that there are other ways of creating legal layouts for operand A and B.

### MN_INTER

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind.MN_INTER`

**Signature:** `MN_INTER = 1`

### MN_SW32

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind.MN_SW32`

**Signature:** `MN_SW32 = 2`

### MN_SW64

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind.MN_SW64`

**Signature:** `MN_SW64 = 3`

### MN_SW128

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind.MN_SW128`

**Signature:** `MN_SW128 = 4`

### K_INTER

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind.K_INTER`

**Signature:** `K_INTER = 5`

### K_SW32

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind.K_SW32`

**Signature:** `K_SW32 = 6`

### K_SW64

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind.K_SW64`

**Signature:** `K_SW64 = 7`

### K_SW128

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind.K_SW128`

**Signature:** `K_SW128 = 8`

## make_smem_layout_atom

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.make_smem_layout_atom`

**Signature:** `make_smem_layout_atom(kind: SmemLayoutAtomKind, element_type: Type[cutlass.cute.typing.Numeric], *, loc = None, ip = None) → cutlass.cute.typing.ComposedLayout`

Makes a SMEM layout Atom.

This function creates a composed layout in unit of elements consistent with the requested layout Atom kind and element data type.

### Parameters

-   **kind** ([*SmemLayoutAtomKind*](#cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind)) -- The kind of layout Atom

-   **element_type** (*Type\[Numeric\]*) -- The element data type to construct the layout for

### Returns

The SMEM layout atom

### Return type

ComposedLayout

## fence

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.fence`

**Signature:** `fence(*, loc = None, ip = None) → None`

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#asynchronous-multiply-and-accumulate-instruction-wgmma-fence).

## commit_group

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.commit_group`

**Signature:** `commit_group(*, loc = None, ip = None) → None`

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#asynchronous-warpgroup-level-matrix-instructions-wgmma-commit-group).

## wait_group

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.warpgroup.wait_group`

**Signature:** `wait_group(group, *, loc = None, ip = None) → None`

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#asynchronous-multiply-and-accumulate-instruction-wgmma-wait-group).
