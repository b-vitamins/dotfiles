---
title: "cpasync submodule"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_nvgpu_cpasync.html"
cutlass_docs_version: "4.4.1"
generated_at: "2026-03-07T11:33:21+00:00"
last_updated: "Mar 05, 2026"
---

# cpasync submodule

## LoadCacheMode

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.LoadCacheMode`

**Signature:** `LoadCacheMode(value)`

Bases: `Enum`

An enumeration for the possible cache modes of a non-bulk `cp.async` instruction.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#cache-operators).

## CopyG2SOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyG2SOp`

**Signature:** `CopyG2SOp(cache_mode: LoadCacheMode = cutlass._mlir.dialects.cute_nvgpu.LoadCacheMode.always)`

Bases: `CopyOp`

Non-bulk asynchronous GMEM to SMEM Copy Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-non-bulk-copy).

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyG2SOp.__init__`

**Signature:** `__init__(cache_mode: LoadCacheMode = cutlass._mlir.dialects.cute_nvgpu.LoadCacheMode.always) → None`

## CopyBulkTensorTileG2SOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp`

**Signature:** `CopyBulkTensorTileG2SOp(cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup = <CtaGroup.ONE>)`

Bases: `TmaCopyOp`

Bulk tensor asynchrnous GMEM to SMEM Copy Operation using the TMA unit.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk-tensor). This Operation uses TMA in the `.tile` mode.

### cta_group

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp.cta_group`

**Signature:** `cta_group: CtaGroup = 1`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp.__init__`

**Signature:** `__init__(cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup = <CtaGroup.ONE>) → None`

## CopyBulkTensorTileG2SMulticastOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SMulticastOp`

**Signature:** `CopyBulkTensorTileG2SMulticastOp(cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup = <CtaGroup.ONE>)`

Bases: `TmaCopyOp`

Bulk tensor asynchrnous multicast GMEM to SMEM Copy Operation using the TMA unit.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk-tensor). This Operation uses TMA in the `.tile` mode.

### cta_group

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SMulticastOp.cta_group`

**Signature:** `cta_group: CtaGroup = 1`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SMulticastOp.__init__`

**Signature:** `__init__(cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup = <CtaGroup.ONE>) → None`

## CopyBulkTensorTileS2GOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileS2GOp`

**Signature:** `CopyBulkTensorTileS2GOp`

Bases: `TmaCopyOp`

Bulk tensor asynchronous SMEM to GMEM Copy Operation using the TMA unit.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk-tensor). This Operation uses TMA in the `.tile` mode.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileS2GOp.__init__`

**Signature:** `__init__() → None`

## CopyReduceBulkTensorTileS2GOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyReduceBulkTensorTileS2GOp`

**Signature:** `CopyReduceBulkTensorTileS2GOp(reduction_kind: cutlass._mlir.dialects.cute.ReductionOp = cutlass._mlir.dialects.cute.ReductionOp.ADD)`

Bases: `TmaCopyOp`

Bulk tensor asynchronous SMEM to GMEM Reduction Operation using the TMA unit.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-reduce-async-bulk). This Operation uses TMA in the `.tile` mode.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyReduceBulkTensorTileS2GOp.__init__`

**Signature:** `__init__(reduction_kind: cutlass._mlir.dialects.cute.ReductionOp = cutlass._mlir.dialects.cute.ReductionOp.ADD) → None`

## CopyDsmemStoreOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyDsmemStoreOp`

**Signature:** `CopyDsmemStoreOp`

Bases: `CopyOp`

Asynchronous Store operation to DSMEM with explicit synchronization.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-st-async).

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyDsmemStoreOp.__init__`

**Signature:** `__init__() → None`

## make_tiled_tma_atom

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.make_tiled_tma_atom`

**Signature:** `make_tiled_tma_atom(op: CopyBulkTensorTileG2SOp | CopyBulkTensorTileG2SMulticastOp | CopyBulkTensorTileS2GOp | CopyReduceBulkTensorTileS2GOp, gmem_tensor: cutlass.cute.typing.Tensor, smem_layout_: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, cta_tiler: cutlass.cute.typing.Tiler, num_multicast: int = 1, *, internal_type: Type[cutlass.cute.typing.Numeric] | None = None, loc = None, ip = None) → Tuple[CopyAtom, cutlass.cute.typing.Tensor]`

Makes a TMA Copy Atom in the `.tile` mode to copy tiles of a GMEM tensor to/from SMEM buffer with the given Layout.

Given

-   a GMEM tensor

-   a SMEM layout

-   a CTA-level Tiler

this function figures out the bulk tensor asynchronous copy instruction to use with the maximum "TMA vector length" to copy tiles of the GMEM tensor to/from an SMEM buffer with the provided layout while maintaining consistency with the provided Tiler.

This function returns two results:

1.  the Copy Atom

2.  a TMA tensor that maps logical coordinates of the GMEM tensor to coordinates consumed by the TMA unit. TMA tensors contain basis stride elements that enable their associated layout to compute coordinates. Like other CuTe tensors, TMA tensors can be partitioned.

### Parameters

-   **op** (*TMAOp*) -- The TMA Copy Operation to construct an Atom

-   **gmem_tensor** (*Tensor*) -- The GMEM tensor involved in the Copy

-   **smem_layout** (*Union\[Layout,* *ComposedLayout\]*) -- The SMEM layout to construct the Copy Atom, either w/ or w/o the stage mode

-   **cta_tiler** (*Tiler*) -- The CTA Tiler to use

-   **num_multicast** (*int*) -- The multicast factor

-   **internal_type** (*Type\[Numeric\]*) -- Optional internal data type to use when the tensor data type is not supported by the TMA unit

### Returns

A TMA Copy Atom associated with the TMA tensor

### Return type

Tuple\[[atom.CopyAtom](cute.md#cutlass.cute.CopyAtom), Tensor\]

## tma_partition

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.tma_partition`

**Signature:** `tma_partition(atom: CopyAtom, cta_coord: cutlass.cute.typing.Coord, cta_layout: cutlass.cute.typing.Layout, smem_tensor: cutlass.cute.typing.Tensor, gmem_tensor: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → Tuple[cutlass.cute.typing.Tensor, cutlass.cute.typing.Tensor]`

Tiles the GMEM and SMEM tensors for the provided TMA Copy Atom.

## create_tma_multicast_mask

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.create_tma_multicast_mask`

**Signature:** `create_tma_multicast_mask(cta_layout_vmnk: cutlass.cute.typing.Layout, cta_coord_vmnk: cutlass.cute.typing.Coord, mcast_mode: int, *, loc = None, ip = None) → cutlass.cute.typing.Int16`

Computes a multicast mask for a TMA load Copy.

### Parameters

-   **cta_layout_vmnk** (*Layout*) -- The VMNK layout of the cluster

-   **cta_coord_vmnk** (*Coord*) -- The VMNK coordinate of the current CTA

-   **mcast_mode** (*int*) -- The tensor mode in which to multicast

### Returns

The resulting mask

### Return type

Int16

## prefetch_descriptor

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.prefetch_descriptor`

**Signature:** `prefetch_descriptor(tma_atom: CopyAtom, *, loc = None, ip = None) → None`

Prefetches the TMA descriptor associated with the TMA Atom.

## copy_tensormap

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.copy_tensormap`

**Signature:** `copy_tensormap(tma_atom: CopyAtom, tensormap_ptr: cutlass.cute.typing.Pointer, *, loc = None, ip = None) → None`

Copies the tensormap held by a TMA Copy Atom to the memory location pointed to by the provided pointer.

### Parameters

-   **tma_atom** ([*CopyAtom*](cute.md#cutlass.cute.CopyAtom)) -- The TMA Copy Atom

-   **tensormap_ptr** (*Pointer*) -- The pointer to the memory location to copy the tensormap to

## update_tma_descriptor

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.update_tma_descriptor`

**Signature:** `update_tma_descriptor(tma_atom: CopyAtom, gmem_tensor: cutlass.cute.typing.Tensor, tma_desc_ptr: cutlass.cute.typing.Pointer, *, loc = None, ip = None) → None`

Updates the TMA descriptor in the memory location pointed to by the provided pointer using information from a TMA Copy Atom and the provided GMEM tensor.

Specifically, the following fields of the TMA descriptor will be updated:

1.  the GMEM tensor base address

2.  the GMEM tensor shape

3.  the GMEM tensor stride

Other fields of the TMA descriptor are left unchanged.

### Parameters

-   **tma_atom** ([*CopyAtom*](cute.md#cutlass.cute.CopyAtom)) -- The TMA Copy Atom

-   **gmem_tensor** (*Tensor*) -- The GMEM tensor

-   **tensormap_ptr** (*Pointer*) -- The pointer to the memory location of the descriptor to udpate

## fence_tma_desc_acquire

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.fence_tma_desc_acquire`

**Signature:** `fence_tma_desc_acquire(tma_desc_ptr: cutlass.cute.typing.Pointer, *, loc = None, ip = None) → None`

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).

## cp_fence_tma_desc_release

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.cp_fence_tma_desc_release`

**Signature:** `cp_fence_tma_desc_release(tma_desc_global_ptr: cutlass.cute.typing.Pointer, tma_desc_shared_ptr: cutlass.cute.typing.Pointer, *, loc = None, ip = None) → None`

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-tensormap-cp-fenceproxy).

## fence_tma_desc_release

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.fence_tma_desc_release`

**Signature:** `fence_tma_desc_release(*, loc = None, ip = None) → None`

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).
