---
title: "warp submodule"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_nvgpu_warp.html"
cutlass_docs_version: "4.4.1"
generated_at: "2026-03-07T11:33:21+00:00"
last_updated: "Mar 05, 2026"
---

# warp submodule

## Field

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warp.Field`

**Signature:** `Field(value)`

Bases: `Enum`

An enumeration for the fields of the MMA Atom that can be modified at runtime.

### ACCUMULATE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warp.Field.ACCUMULATE`

**Signature:** `ACCUMULATE = 'accum_c'`

### SFA

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warp.Field.SFA`

**Signature:** `SFA = 'sf_a'`

### SFB

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warp.Field.SFB`

**Signature:** `SFB = 'sf_b'`

## MmaF16BF16Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warp.MmaF16BF16Op`

**Signature:** `MmaF16BF16Op(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], shape_mnk: cutlass.cute.typing.Shape)`

Bases: `WarpMmaOp`

F16/BF16 warp-level MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#warp-level-matrix-instructions-mma). This Operation covers the instructions using the `.f16` or `.bf16` qualifiers for the input operands.

### ab_dtype

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warp.MmaF16BF16Op.ab_dtype`

**Signature:** `ab_dtype: Type[cutlass.cute.typing.Numeric]`

### acc_dtype

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warp.MmaF16BF16Op.acc_dtype`

**Signature:** `acc_dtype: Type[cutlass.cute.typing.Numeric]`

### shape_mnk

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warp.MmaF16BF16Op.shape_mnk`

**Signature:** `shape_mnk: cutlass.cute.typing.Shape`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.warp.MmaF16BF16Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], shape_mnk: cutlass.cute.typing.Shape) → None`

## MmaMXF4Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warp.MmaMXF4Op`

**Signature:** `MmaMXF4Op(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], sf_type: Type[cutlass.cute.typing.Numeric])`

Bases: `MmaSM120BlockScaledOp`

MXF4 warp-level MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#warp-level-matrix-instructions-mma). This Operation covers the instructions using the `.e2m1` qualifiers for the input operands. .kind = {.kind::mxf4}; .scale_vec_size = {.scale_vec::2X}; .stype = {.ue8m0};

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warp.MmaMXF4Op.descriptive_name`

**Signature:** `descriptive_name = 'warp-level MXF4 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.warp.MmaMXF4Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], sf_type: Type[cutlass.cute.typing.Numeric]) → None`

## MmaMXF4NVF4Op

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warp.MmaMXF4NVF4Op`

**Signature:** `MmaMXF4NVF4Op(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], sf_type: Type[cutlass.cute.typing.Numeric])`

Bases: `MmaSM120BlockScaledOp`

MXF4NVF4 warp-level MMA Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#warp-level-matrix-instructions-mma). This Operation covers the instructions using the `.e2m1` qualifiers for the input operands. .kind = {.kind::mxf4nvf4}; .scale_vec_size = {.scale_vec::2X, .scale_vec::4X}; .stype = {.ue8m0, .ue4m3};

### descriptive_name

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.warp.MmaMXF4NVF4Op.descriptive_name`

**Signature:** `descriptive_name = 'warp-level MXF4NVF4 MMA Operation'`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.warp.MmaMXF4NVF4Op.__init__`

**Signature:** `__init__(ab_dtype: Type[cutlass.cute.typing.Numeric], acc_dtype: Type[cutlass.cute.typing.Numeric], sf_type: Type[cutlass.cute.typing.Numeric]) → None`

## LdMatrix8x8x16bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warp.LdMatrix8x8x16bOp`

**Signature:** `LdMatrix8x8x16bOp(transpose: bool = False, num_matrices: int = 1, unpack_bits: cutlass.cute.typing.Optional.<class 'int'> | None = None)`

Bases: `BaseOp`

8x8 `ldmatrix` Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#warp-level-matrix-load-instruction-ldmatrix). This operation corresponds to the `.m8n8` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.warp.LdMatrix8x8x16bOp.__init__`

**Signature:** `__init__(transpose: bool = False, num_matrices: int = 1, unpack_bits: cutlass.cute.typing.Optional.<class 'int'> | None = None) → None`

## LdMatrix16x8x8bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warp.LdMatrix16x8x8bOp`

**Signature:** `LdMatrix16x8x8bOp(transpose: bool = False, num_matrices: int = 1, unpack_bits: cutlass.cute.typing.Optional.<class 'int'> | None = None)`

Bases: `BaseOp`

16x8 8b `ldmatrix` Operation with transpose

There is no direct PTX correspondance to this Op. This actually lowers to ldmatrix with the `.m16n16` qualifier and additional address and value permutations to match stmatrix.m16n8.trans. Useful for vectorizing with Ampere-style 8x8 matrix thread-value layouts

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.warp.LdMatrix16x8x8bOp.__init__`

**Signature:** `__init__(transpose: bool = False, num_matrices: int = 1, unpack_bits: cutlass.cute.typing.Optional.<class 'int'> | None = None) → None`

## LdMatrix16x16x8bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warp.LdMatrix16x16x8bOp`

**Signature:** `LdMatrix16x16x8bOp(transpose: bool = False, num_matrices: int = 1, unpack_bits: cutlass.cute.typing.Optional.<class 'int'> | None = None)`

Bases: `BaseOp`

16x16 `ldmatrix` Operation with transpose and optional unpacking to 8b container. Packed source container is 16x4b elements with 64b padding or 16x6b elements with 32b padding (total 128b per 16 elements)

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#warp-level-matrix-load-instruction-ldmatrix). This operation corresponds to the `.m16n16` and the `.b4x16_p64`,\`\`.b6x16_p32\`\`,\`\`.b8\`\` qualifiers.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.warp.LdMatrix16x16x8bOp.__init__`

**Signature:** `__init__(transpose: bool = False, num_matrices: int = 1, unpack_bits: cutlass.cute.typing.Optional.<class 'int'> | None = None) → None`

## StMatrix8x8x16bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warp.StMatrix8x8x16bOp`

**Signature:** `StMatrix8x8x16bOp(transpose: bool = False, num_matrices: int = 1, unpack_bits: cutlass.cute.typing.Optional.<class 'int'> | None = None)`

Bases: `BaseOp`

8x8 `stmatrix` Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#warp-level-matrix-instructions-stmatrix). This operation corresponds to the `m8n8` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.warp.StMatrix8x8x16bOp.__init__`

**Signature:** `__init__(transpose: bool = False, num_matrices: int = 1, unpack_bits: cutlass.cute.typing.Optional.<class 'int'> | None = None) → None`

## StMatrix16x8x8bOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.warp.StMatrix16x8x8bOp`

**Signature:** `StMatrix16x8x8bOp(transpose: bool = False, num_matrices: int = 1, unpack_bits: cutlass.cute.typing.Optional.<class 'int'> | None = None)`

Bases: `BaseOp`

16x8 `stmatrix` Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#warp-level-matrix-instructions-stmatrix). This operation corresponds to the `m16n8` qualifier.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.warp.StMatrix16x8x8bOp.__init__`

**Signature:** `__init__(transpose: bool = False, num_matrices: int = 1, unpack_bits: cutlass.cute.typing.Optional.<class 'int'> | None = None) → None`
