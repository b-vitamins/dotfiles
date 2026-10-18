---
title: "nvgpu.common"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_nvgpu_common.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# nvgpu.common

## OperandMajorMode

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.OperandMajorMode`

**Signature:** `OperandMajorMode(value)`

Bases: `Enum`

An enumeration for the majorness of the input operands of the MMA.

### MN

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.OperandMajorMode.MN`

**Signature:** `MN = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### K

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.OperandMajorMode.K`

**Signature:** `K = <class 'sphinx.ext.autodoc.mock._MockObject'>`

## OutputMajorMode

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.OutputMajorMode`

**Signature:** `OutputMajorMode(value)`

Bases: `Enum`

Major mode for the output operand D(M, N).

M = M-major (column-major): stride=(1, M), contiguous along M. N = N-major (row-major): stride=(N, 1), contiguous along N.

### M

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.OutputMajorMode.M`

**Signature:** `M = 'm'`

### N

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.OutputMajorMode.N`

**Signature:** `N = 'n'`

## normalize_field_to_ir_name

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.normalize_field_to_ir_name`

**Signature:** `normalize_field_to_ir_name(field: Any, admissible_fields: Any) → str`

Normalize a field specifier to its IR logical field name.

Accepted inputs:

- Enum value present in admissible_fields (must expose \_to_ir_field_name()).

- Exact string IR name (e.g., "accum_c", "neg_a", "sf_a").

Any other form is rejected.

## MmaUniversalOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.MmaUniversalOp`

**Signature:** `MmaUniversalOp(abacc_dtype: Type[cutlass.cute.typing.Numeric])`

Bases: `MmaOp`

The universal MMA Operation.

This Operation currently expects the A/B operands as well as the accumulator to share the same data types.

**Supported architectures:** all (universal FMA)

### Parameters

**abacc_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The data type for the A/B operands and the accumulator

### abacc_dtype

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MmaUniversalOp.abacc_dtype`

**Signature:** `abacc_dtype: Type[cutlass.cute.typing.Numeric]`

## MmaUniversalTrait

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.MmaUniversalTrait`

**Signature:** `MmaUniversalTrait(value: ir.Value)`

Bases: `Trait`

## CopyUniversalOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.CopyUniversalOp`

**Signature:** `CopyUniversalOp`

Bases: `CopyOp`

The universal Copy Operation.

This operation is equivalent to the `a = b` assignment without any extra memory attributes. For advanced memory features (memory order, memory scope, cache eviction priority, invariant loads, etc.) please use the specialized copy operations instead:

- [`CopyG2ROp`](#cutlass.cute.nvgpu.CopyG2ROp) -- global memory to register

- [`CopyR2GOp`](#cutlass.cute.nvgpu.CopyR2GOp) -- register to global memory

- [`CopyS2ROp`](#cutlass.cute.nvgpu.CopyS2ROp) -- shared memory to register

- [`CopyR2SOp`](#cutlass.cute.nvgpu.CopyR2SOp) -- register to shared memory

When creating a Copy Atom out of this operation, the expected usage pattern is

    op = cute.nvgpu.CopyUniversalOp()
    atom = cute.make_copy_atom(op, tensor_dtype, num_bits_per_copy=64)

- `tensor_dtype` is the data type used to build the reference TV Layout (either the source or the destination TV Layout) in unit of tensor elements and is used for partitioning by `TiledCopy` for example

- `num_bits_per_copy` is a kw argument specifying the number of bits to copy per Atom execution. This can be larger than the width of the above data type. When not provided, the compiler will do a best effort at auto-vectorizing.

## CopyUniversalTrait

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.CopyUniversalTrait`

**Signature:** `CopyUniversalTrait(value: ir.Value)`

Bases: `Trait`

## CopyG2ROp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.CopyG2ROp`

**Signature:** `CopyG2ROp`

Bases: `CopyOp`

The G2R copy operation.

When creating a Copy Atom out of this operation, the expected usage pattern is

    op = cute.nvgpu.CopyG2ROp()
    atom = cute.make_copy_atom(
        op,
        tensor_dtype,
        num_bits_per_copy=64,
        memory_order=cute.nvgpu.MemoryOrder.VOLATILE,
        memory_scope=cute.nvgpu.MemoryScope.SYS,
        l2_prefetch_size=cute.nvgpu.L2PrefetchSize.NONE,
        l1c_evict_priority=cute.nvgpu.CacheEvictionPriority.EVICT_NORMAL,
        load_cache_mode=cute.nvgpu.LoadCacheMode.ALWAYS,
        shared_space=cute.nvgpu.SharedSpace.CTA,
        invariant=False,
    )

## CopyG2RTrait

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.CopyG2RTrait`

**Signature:** `CopyG2RTrait(value: ir.Value)`

Bases: `Trait`

### unpack

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.CopyG2RTrait.unpack`

**Signature:** `unpack(*, cache_policy: cutlass.cute.typing.Int64 | None = None, **kwargs: Any) → ir.Value`

## CopyR2GOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.CopyR2GOp`

**Signature:** `CopyR2GOp`

Bases: `CopyOp`

The R2G copy operation.

When creating a Copy Atom out of this operation, the expected usage pattern is

    op = cute.nvgpu.CopyR2GOp()
    atom = cute.make_copy_atom(
        op,
        tensor_dtype,
        num_bits_per_copy=64,
        memory_order=cute.nvgpu.MemoryOrder.RELEASE,
        memory_scope=cute.nvgpu.MemoryScope.CLUSTER,
        l1c_evict_priority=cute.nvgpu.CacheEvictionPriority.EVICT_NORMAL,
        shared_space=cute.nvgpu.SharedSpace.CTA,
    )

## CopyR2GTrait

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.CopyR2GTrait`

**Signature:** `CopyR2GTrait(value: ir.Value)`

Bases: `Trait`

### unpack

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.CopyR2GTrait.unpack`

**Signature:** `unpack(*, cache_policy: cutlass.cute.typing.Int64 | None = None, **kwargs: Any) → ir.Value`

## CopyS2ROp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.CopyS2ROp`

**Signature:** `CopyS2ROp`

Bases: `CopyOp`

The S2R copy operation.

When creating a Copy Atom out of this operation, the expected usage pattern is

    op = cute.nvgpu.CopyS2ROp()
    atom = cute.make_copy_atom(
        op,
        tensor_dtype,
        num_bits_per_copy=64,
        memory_order=cute.nvgpu.MemoryOrder.WEAK,
        memory_scope=cute.nvgpu.MemoryScope.CTA,
        shared_space=cute.nvgpu.SharedSpace.CTA,
    )

## CopyS2RTrait

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.CopyS2RTrait`

**Signature:** `CopyS2RTrait(value: ir.Value)`

Bases: `Trait`

## CopyR2SOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.CopyR2SOp`

**Signature:** `CopyR2SOp`

Bases: `CopyOp`

The R2S copy operation.

When creating a Copy Atom out of this operation, the expected usage pattern is

    op = cute.nvgpu.CopyR2SOp()
    atom = cute.make_copy_atom(
        op,
        tensor_dtype,
        num_bits_per_copy=64,
        memory_order=cute.nvgpu.MemoryOrder.WEAK,
        memory_scope=cute.nvgpu.MemoryScope.CTA,
        shared_space=cute.nvgpu.SharedSpace.CTA,
    )

## CopyR2STrait

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.CopyR2STrait`

**Signature:** `CopyR2STrait(value: ir.Value)`

Bases: `Trait`

## MemoryOrder

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.MemoryOrder`

**Signature:** `MemoryOrder(value)`

Bases: `Enum`

An enumeration.

### WEAK

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryOrder.WEAK`

**Signature:** `WEAK = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### RELAXED

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryOrder.RELAXED`

**Signature:** `RELAXED = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### ACQUIRE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryOrder.ACQUIRE`

**Signature:** `ACQUIRE = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### RELEASE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryOrder.RELEASE`

**Signature:** `RELEASE = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### ACQ_REL

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryOrder.ACQ_REL`

**Signature:** `ACQ_REL = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### SC

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryOrder.SC`

**Signature:** `SC = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### MMIO

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryOrder.MMIO`

**Signature:** `MMIO = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### CONSTANT

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryOrder.CONSTANT`

**Signature:** `CONSTANT = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### VOLATILE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryOrder.VOLATILE`

**Signature:** `VOLATILE = <class 'sphinx.ext.autodoc.mock._MockObject'>`

## MemoryScope

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.MemoryScope`

**Signature:** `MemoryScope(value)`

Bases: `Enum`

An enumeration.

### CTA

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryScope.CTA`

**Signature:** `CTA = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### CLUSTER

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryScope.CLUSTER`

**Signature:** `CLUSTER = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### GPU

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryScope.GPU`

**Signature:** `GPU = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### SYS

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.MemoryScope.SYS`

**Signature:** `SYS = <class 'sphinx.ext.autodoc.mock._MockObject'>`

## L2PrefetchSize

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.L2PrefetchSize`

**Signature:** `L2PrefetchSize(value)`

Bases: `Enum`

An enumeration.

### NONE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.L2PrefetchSize.NONE`

**Signature:** `NONE = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### RESERVED

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.L2PrefetchSize.RESERVED`

**Signature:** `RESERVED = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### SIZE_64B

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.L2PrefetchSize.SIZE_64B`

**Signature:** `SIZE_64B = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### SIZE_128B

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.L2PrefetchSize.SIZE_128B`

**Signature:** `SIZE_128B = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### SIZE_256B

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.L2PrefetchSize.SIZE_256B`

**Signature:** `SIZE_256B = <class 'sphinx.ext.autodoc.mock._MockObject'>`

## CacheEvictionPriority

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.CacheEvictionPriority`

**Signature:** `CacheEvictionPriority(value)`

Bases: `Enum`

An enumeration.

### EVICT_NORMAL

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.CacheEvictionPriority.EVICT_NORMAL`

**Signature:** `EVICT_NORMAL = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### EVICT_FIRST

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.CacheEvictionPriority.EVICT_FIRST`

**Signature:** `EVICT_FIRST = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### EVICT_LAST

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.CacheEvictionPriority.EVICT_LAST`

**Signature:** `EVICT_LAST = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### EVICT_UNCHANGED

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.CacheEvictionPriority.EVICT_UNCHANGED`

**Signature:** `EVICT_UNCHANGED = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### NO_ALLOCATE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.CacheEvictionPriority.NO_ALLOCATE`

**Signature:** `NO_ALLOCATE = <class 'sphinx.ext.autodoc.mock._MockObject'>`

## LoadCacheMode

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.LoadCacheMode`

**Signature:** `LoadCacheMode(value)`

Bases: `Enum`

An enumeration.

### ALWAYS

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.LoadCacheMode.ALWAYS`

**Signature:** `ALWAYS = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### GLOBAL

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.LoadCacheMode.GLOBAL`

**Signature:** `GLOBAL = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### STREAMING

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.LoadCacheMode.STREAMING`

**Signature:** `STREAMING = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### LAST_USE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.LoadCacheMode.LAST_USE`

**Signature:** `LAST_USE = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### NONE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.LoadCacheMode.NONE`

**Signature:** `NONE = <class 'sphinx.ext.autodoc.mock._MockObject'>`

## StoreCacheMode

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.StoreCacheMode`

**Signature:** `StoreCacheMode(value)`

Bases: `Enum`

An enumeration.

### WRITE_BACK

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.StoreCacheMode.WRITE_BACK`

**Signature:** `WRITE_BACK = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### GLOBAL

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.StoreCacheMode.GLOBAL`

**Signature:** `GLOBAL = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### STREAMING

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.StoreCacheMode.STREAMING`

**Signature:** `STREAMING = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### WRITE_THROUGH

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.StoreCacheMode.WRITE_THROUGH`

**Signature:** `WRITE_THROUGH = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### NONE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.StoreCacheMode.NONE`

**Signature:** `NONE = <class 'sphinx.ext.autodoc.mock._MockObject'>`

## SharedSpace

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.SharedSpace`

**Signature:** `SharedSpace(value)`

Bases: `Enum`

An enumeration.

### CTA

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.SharedSpace.CTA`

**Signature:** `CTA = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### CLUSTER

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.SharedSpace.CLUSTER`

**Signature:** `CLUSTER = <class 'sphinx.ext.autodoc.mock._MockObject'>`

## make_tiled_tma_atom_A

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.make_tiled_tma_atom_A`

**Signature:** `make_tiled_tma_atom_A(op: CopyBulkTensorTileG2SOp | CopyBulkTensorTileG2SMulticastOp, gmem_tensor: cutlass.cute.typing.Tensor, smem_layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, mma_tiler_mnk: cutlass.cute.typing.Shape, tiled_mma: TiledMma, cluster_shape_vmnk: cutlass.cute.typing.Shape | None = None, *, internal_type: Type[cutlass.cute.typing.Numeric] | None = None) → TmaInfo`

Makes a TMA Copy atom mapping to `.tile` mode for `cp.async.bulk.tensor` PTX operation accounting for the MK projections of the TiledMMA for A tensor loads.

Given

- a GMEM tensor

- a SMEM layout

- a MMA Tiler

- a TiledMma

- a Cluster-level shape

this function figures out the bulk tensor asynchronous copy instruction to use with the maximum "TMA vector length" to copy tiles of the GMEM tensor to an SMEM buffer with the provided layout and consistent with the provided Tiler & tiled_mma (considering the M-mode & K-mode). The Cluster-level shape is used to determine the multicast factor across the N-mode for A tensor loads.

This function returns two results:

1.  the Copy Atom

2.  the so-called TMA tensor used to map logical coordinates of the GMEM tensor to coordinates that the TMA unit can consume. TMA tensors have so-called basis stride elements so that the associated layout can output coordinates. Otherwise, TMA tensors can be partitioned similarly to any other CuTe tensors using the algebra.

### Parameters

- **op** (*Union\[*[*CopyBulkTensorTileG2SOp*](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp)*,* [*CopyBulkTensorTileG2SMulticastOp*](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SMulticastOp)*\]*) -- The Copy Operation to construct an Atom for

- **gmem_tensor** (*Tensor*) -- The GMEM tensor to be loaded by this copy atom

- **smem_layout** (*Union\[Layout,* *ComposedLayout\]*) -- Shared memory layout to load the tensor into (PDSL)

- **mma_tiler_mnk** (*Shape*) -- The MMA Tiler shape (TILE_M, TILE_N, TILE_K) in MNK dimensions

- **tiled_mma** ([*atom.TiledMma*](cute.md#cutlass.cute.TiledMma)) -- The TiledMMA that will consume the load as operands

- **cluster_shape_vmnk** (*Shape*) -- The Cluster-level shape in VMNK dimensions

- **internal_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Optional element-format override used when the tensor element type does not match the copy type

### Returns

A TmaInfo containing the Copy Atom, TMA tensor, and SMEM layout

### Return type

[TmaInfo](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.TmaInfo)

## make_tiled_tma_atom_B

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.make_tiled_tma_atom_B`

**Signature:** `make_tiled_tma_atom_B(op: CopyBulkTensorTileG2SOp | CopyBulkTensorTileG2SMulticastOp, gmem_tensor: cutlass.cute.typing.Tensor, smem_layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, mma_tiler_mnk: cutlass.cute.typing.Shape, tiled_mma: TiledMma, cluster_shape_vmnk: cutlass.cute.typing.Shape | None = None, *, internal_type: Type[cutlass.cute.typing.Numeric] | None = None) → TmaInfo`

Makes a TMA Copy atom mapping to `.tile` mode for `cp.async.bulk.tensor` PTX operation accounting for the NK projections of the TiledMMA for B tensor loads.

Given

- a GMEM tensor

- a SMEM layout

- a MMA Tiler

- a TiledMma

- a Cluster-level shape

this function figures out the bulk tensor asynchronous copy instruction to use with the maximum "TMA vector length" to copy tiles of the GMEM tensor to an SMEM buffer with the provided layout and consistent with the provided Tiler & tiled_mma (considering the N-mode & K-mode). The Cluster-level shape is used to determine the multicast factor across the M-mode for B tensor loads.

This function returns two results:

1.  the Copy Atom

2.  the so-called TMA tensor used to map logical coordinates of the GMEM tensor to coordinates that the TMA unit can consume. TMA tensors have so-called basis stride elements so that the associated layout can output coordinates. Otherwise, TMA tensors can be partitioned similarly to any other CuTe tensors using the algebra.

### Parameters

- **op** (*Union\[*[*CopyBulkTensorTileG2SOp*](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp)*,* [*CopyBulkTensorTileG2SMulticastOp*](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SMulticastOp)*\]*) -- The Copy Operation to construct an Atom for

- **gmem_tensor** (*Tensor*) -- The GMEM tensor to be loaded by this copy atom

- **smem_layout** (*Union\[Layout,* *ComposedLayout\]*) -- Shared memory layout to load the tensor into (PDSL)

- **mma_tiler_mnk** (*Shape*) -- The MMA Tiler shape (TILE_M, TILE_N, TILE_K) in MNK dimensions

- **tiled_mma** (*core.TiledMma*) -- The TiledMMA that will consume the load as operands

- **cluster_shape_vmnk** (*Shape*) -- The Cluster-level shape in VMNK dimensions

- **internal_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Optional element-format override used when the tensor element type does not match the copy type

### Returns

A TmaInfo containing the Copy Atom, TMA tensor, and SMEM layout

### Return type

[TmaInfo](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.TmaInfo)

## make_tiled_tma_atom_C

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.make_tiled_tma_atom_C`

**Signature:** `make_tiled_tma_atom_C(op: CopyReduceBulkTensorTileS2GOp | CopyBulkTensorTileS2GOp, gmem_tensor: cutlass.cute.typing.Tensor, smem_layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, mma_tiler_mnk: cutlass.cute.typing.Shape, tiled_mma: TiledMma, *, internal_type: Type[cutlass.cute.typing.Numeric] | None = None) → TmaInfo`

Makes a TMA Copy atom mapping to `.tile` mode for `cp.async.bulk.tensor` PTX operation accounting for the MN projections of the TiledMMA for C tensor store/reduce operations.

Given

- a GMEM tensor

- a SMEM layout

- a MMA Tiler

- a TiledMma

- a Cluster-level shape

this function figures out the bulk tensor asynchronous store/reduce instruction to use with the maximum "TMA vector length" to copy tiles from the SMEM buffer to the GMEM tensor with the provided layout and consistent with the provided Tiler & tiled_mma (considering the M-mode & N-mode).

This function returns two results:

1.  the Copy Atom

2.  the so-called TMA tensor used to map logical coordinates of the GMEM tensor to coordinates that the TMA unit can consume. TMA tensors have so-called basis stride elements so that the associated layout can output coordinates. Otherwise, TMA tensors can be partitioned similarly to any other CuTe tensors using the algebra.

### Parameters

- **op** (*Union\[*[*CopyReduceBulkTensorTileS2GOp*](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyReduceBulkTensorTileS2GOp)*,* [*CopyBulkTensorTileS2GOp*](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileS2GOp)*\]*) -- The Copy Operation to construct an Atom for

- **gmem_tensor** (*Tensor*) -- The GMEM tensor to be loaded by this copy atom

- **smem_layout** (*Union\[Layout,* *ComposedLayout\]*) -- Shared memory layout to load the tensor into (PDSL)

- **mma_tiler_mnk** (*Shape*) -- The MMA Tiler shape (TILE_M, TILE_N, TILE_K) in MNK dimensions

- **tiled_mma** ([*atom.TiledMma*](cute.md#cutlass.cute.TiledMma)) -- The TiledMMA that will consume the load as operands

- **internal_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Optional element-format override used when the tensor element type does not match the copy type

### Returns

A TmaInfo containing the Copy Atom, TMA tensor, and SMEM layout

### Return type

[TmaInfo](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.TmaInfo)

## make_im2col_tma_atom_A

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.make_im2col_tma_atom_A`

**Signature:** `make_im2col_tma_atom_A(op: CopyBulkTensorIm2ColG2SOp | CopyBulkTensorIm2ColG2SMulticastOp, gmem_tensor: cutlass.cute.typing.Tensor, smem_layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, mma_tiler_mnk: cutlass.cute.typing.Shape, tiled_mma: TiledMma, filter_trs: Tuple[int, int, int], upper_padding_dhw: Tuple[int, int, int], lower_padding_dhw: Tuple[int, int, int], stride_dhw: Tuple[int, int, int], dilation_dhw: Tuple[int, int, int], cluster_shape_vmnk: cutlass.cute.typing.Shape | None = None, *, internal_type: Type[cutlass.cute.typing.Numeric] | None = None) → TmaInfo`

Makes a TMA Copy atom mapping to `.im2col` mode for `cp.async.bulk.tensor` PTX operation accounting for the MK projections of the TiledMMA for A tensor loads.

Given

- a GMEM tensor

- a SMEM layout

- a MMA Tiler

- a TiledMma

- a filter shape

- a padding shape

- a stride shape

- a dilation shape

- a Cluster-level shape

this function figures out the bulk tensor asynchronous copy instruction to use with the maximum "TMA vector length" to copy tiles of the GMEM tensor to/from an SMEM buffer with the provided layout while maintaining consistency with the provided Tiler.

This function returns two results:

1.  the Copy Atom

2.  the TMA tensor used to map logical coordinates of the GMEM tensor to coordinates that the TMA unit can consume. TMA tensors have so-called basis stride elements so that the associated layout can output coordinates. Otherwise, TMA tensors can be partitioned similarly to any other CuTe tensors using the algebra.

### Parameters

- **op** (*Union\[CopyBulkTensorIm2ColG2SOp,* *CopyBulkTensorIm2ColG2SMulticastOp\]*) -- The Copy Operation to construct an Atom for

- **gmem_tensor** (*Tensor*) -- The GMEM tensor to be loaded by this copy atom

- **smem_layout** (*Union\[Layout,* *ComposedLayout\]*) -- Shared memory layout to load the tensor into (PDSL)

- **mma_tiler_mnk** (*Shape*) -- The MMA Tiler shape (TILE_M, TILE_N, TILE_K) in MNK dimensions

- **tiled_mma** ([*atom.TiledMma*](cute.md#cutlass.cute.TiledMma)) -- The TiledMMA that will consume the load as operands

- **filter_trs** (*Tuple\[int,* *int,* *int\]*) -- The filter shape (T, R, S) in TRS dimensions

- **upper_padding_dhw** (*Tuple\[int,* *int,* *int\]*) -- The upper padding shape (D, H, W) in DHW dimensions

- **lower_padding_dhw** (*Tuple\[int,* *int,* *int\]*) -- The lower padding shape (D, H, W) in DHW dimensions

- **stride_dhw** (*Tuple\[int,* *int,* *int\]*) -- The stride shape (D, H, W) in DHW dimensions

- **dilation_dhw** (*Tuple\[int,* *int,* *int\]*) -- The dilation shape (D, H, W) in DHW dimensions

- **cluster_shape_vmnk** (*Shape*) -- The Cluster-level shape in VMNK dimensions

- **internal_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Optional element-format override used when the tensor element type does not match the copy type

### Returns

A TmaInfo containing the Copy Atom, TMA tensor, and SMEM layout

### Return type

[TmaInfo](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.TmaInfo)
