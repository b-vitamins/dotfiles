---
title: "nvgpu.cpasync"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_nvgpu_cpasync.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# nvgpu.cpasync

## LoadCacheMode

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.LoadCacheMode`

**Signature:** `LoadCacheMode(value)`

Bases: `Enum`

An enumeration for the possible cache modes of a non-bulk `cp.async` instruction.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#cache-operators).

### ALWAYS

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.cpasync.LoadCacheMode.ALWAYS`

**Signature:** `ALWAYS = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### GLOBAL

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.cpasync.LoadCacheMode.GLOBAL`

**Signature:** `GLOBAL = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### STREAMING

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.cpasync.LoadCacheMode.STREAMING`

**Signature:** `STREAMING = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### LAST_USE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.cpasync.LoadCacheMode.LAST_USE`

**Signature:** `LAST_USE = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### NONE

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.cpasync.LoadCacheMode.NONE`

**Signature:** `NONE = <class 'sphinx.ext.autodoc.mock._MockObject'>`

## CopyG2SOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyG2SOp`

**Signature:** `CopyG2SOp(cache_mode: ~cutlass.cute.nvgpu.common.LoadCacheMode | ~cutlass.cute.nvgpu.cpasync.copy.LoadCacheMode = <LoadCacheMode.ALWAYS>)`

Bases: `CopyOp`

Non-bulk asynchronous GMEM to SMEM Copy Operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-non-bulk-copy).

### cache_mode

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyG2SOp.cache_mode`

**Signature:** `cache_mode: LoadCacheMode = <class 'sphinx.ext.autodoc.mock._MockObject'>`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyG2SOp.__init__`

**Signature:** `__init__(cache_mode: ~cutlass.cute.nvgpu.common.LoadCacheMode | ~cutlass.cute.nvgpu.cpasync.copy.LoadCacheMode = <LoadCacheMode.ALWAYS>)`

## CopyBulkTensorTileG2SOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp`

**Signature:** `CopyBulkTensorTileG2SOp(cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup = <CtaGroup.ONE>)`

Bases: `CopyG2STileBaseOp`

Bulk tensor asynchronous GMEM to SMEM Copy Operation using the TMA unit.

TMA copy operations are issued by a single thread within a warp, but the DSL **automatically handles this** by implicitly adding `elect_one()` around the copy operation.

    # CORRECT: TMA copy without elect_one
    cute.copy(
        tma_atom,
        gmem_tensor,  # TMA partition ensures single-thread automatically
        smem_tensor,
        tma_bar_ptr=barrier_ptr
    )

    # WRONG: Do NOT wrap in elect_one (can cause deadlock)
    with cute.arch.elect_one():  # INCORRECT
        cute.copy(tma_atom, gmem_tensor, smem_tensor, tma_bar_ptr=barrier_ptr)

While the TMA copy itself does not need `elect_one()`, barrier initialization and transaction byte setup **must** use `elect_one()`:

    # Barrier setup requires elect_one
    with cute.arch.elect_one():
        cute.arch.mbarrier_init(barrier_ptr, arrival_count)
        cute.arch.mbarrier_expect_tx(barrier_ptr, num_tma_bytes)

    # TMA copy does NOT need elect_one
    cute.copy(tma_atom, gmem_tensor, smem_tensor, tma_bar_ptr=barrier_ptr)

**PTX Programming Model**: In PTX, TMA operations (`cp.async.bulk.tensor`) must be issued by a single thread. The DSL automatically handles this.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk-tensor). This Operation uses TMA in the `.tile` mode.

> **Note.**
>
> See also
>
> - `cute.arch.elect_one()` - **NOT** needed for TMA copy, but needed for barrier setup
>
> - `cute.arch.mbarrier_init()` - Requires elect_one
>
> - `cute.arch.mbarrier_expect_tx()` - Requires elect_one
>
> - Tutorial example: `examples/blackwell/tutorial_tma/tma_v0.py`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp.__init__`

**Signature:** `__init__(cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup = <CtaGroup.ONE>) → None`

## CopyBulkTensorTileG2SMulticastOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SMulticastOp`

**Signature:** `CopyBulkTensorTileG2SMulticastOp(cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup = <CtaGroup.ONE>)`

Bases: `CopyG2STileBaseOp`

Bulk tensor asynchronous multicast GMEM to SMEM Copy Operation using the TMA unit.

TMA multicast operations are issued by a single thread within a warp, but the DSL **automatically handles this** by implicitly adding `elect_one()` around the copy operation.

    # CORRECT: TMA multicast without elect_one
    cute.copy(
        tma_atom.with_(mcast_mask=cluster_mask),
        gmem_tensor,
        smem_tensor,
        tma_bar_ptr=barrier_ptr
    )

    # WRONG: Do NOT wrap in elect_one (can cause deadlock)
    with cute.arch.elect_one():  # INCORRECT
        cute.copy(tma_atom.with_(mcast_mask=mask), gmem_tensor, smem_tensor)

**PTX Programming Model**: In PTX, TMA multicast operations (`cp.async.bulk.tensor.multicast`) must be issued by a single thread.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk-tensor). This Operation uses TMA in the `.tile` mode.

> **Note.**
>
> See also
>
> - `cute.arch.elect_one()` - **NOT** needed for TMA copy
>
> - [`CopyBulkTensorTileG2SOp`](#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp) - Non-multicast TMA load

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SMulticastOp.__init__`

**Signature:** `__init__(cta_group: ~cutlass.cute.nvgpu.tcgen05.mma.CtaGroup = <CtaGroup.ONE>) → None`

## CopyBulkTensorTileS2GOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileS2GOp`

**Signature:** `CopyBulkTensorTileS2GOp`

Bases: [`TmaCopyOp`](#cutlass.cute.nvgpu.cpasync.TmaCopyOp)

Bulk tensor asynchronous SMEM to GMEM Copy Operation using the TMA unit.

TMA store operations are issued by a single thread within a warp, but the DSL **automatically handles this** by implicitly adding `elect_one()` around the copy operation.

    # CORRECT: TMA store without elect_one
    cute.copy(
        tma_atom,
        smem_tensor,  # Source: shared memory
        gmem_tensor,  # Destination: global memory
    )

    # WRONG: Do NOT wrap in elect_one (causes deadlock)
    with cute.arch.elect_one():  # INCORRECT
        cute.copy(tma_atom, smem_tensor, gmem_tensor)

**PTX Programming Model**: In PTX, TMA store operations must be issued by a single thread. The DSL automatically handles this.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk-tensor). This Operation uses TMA in the `.tile` mode.

> **Note.**
>
> See also
>
> - `cute.arch.elect_one()` - **NOT** needed for TMA store
>
> - [`CopyBulkTensorTileG2SOp`](#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp) - TMA load operation
>
> - Tutorial example: `examples/blackwell/tutorial_tma/tma_v0.py`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileS2GOp.__init__`

**Signature:** `__init__() → None`

## CopyReduceBulkTensorTileS2GOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyReduceBulkTensorTileS2GOp`

**Signature:** `CopyReduceBulkTensorTileS2GOp(reduction_kind: _install_cutlass_mlir_autodoc_stub.<locals>._DocDialectObject = <class 'sphinx.ext.autodoc.mock._MockObject'>)`

Bases: [`TmaCopyOp`](#cutlass.cute.nvgpu.cpasync.TmaCopyOp)

Bulk tensor asynchronous SMEM to GMEM Reduction Operation using the TMA unit.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-reduce-async-bulk). This Operation uses TMA in the `.tile` mode.

### reduction_kind

**Kind:** attribute

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyReduceBulkTensorTileS2GOp.reduction_kind`

**Signature:** `reduction_kind`

alias of `_MockObject`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyReduceBulkTensorTileS2GOp.__init__`

**Signature:** `__init__(reduction_kind: _install_cutlass_mlir_autodoc_stub.<locals>._DocDialectObject = <class 'sphinx.ext.autodoc.mock._MockObject'>) → None`

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

## CopyBulkG2SOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkG2SOp`

**Signature:** `CopyBulkG2SOp`

Bases: `CopyOp`

Bulk copy asynchronous GMEM to SMEM Copy Operation.

The compiler does not perform thread election for this operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk).

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkG2SOp.__init__`

**Signature:** `__init__() → None`

## CopyBulkG2SMulticastOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkG2SMulticastOp`

**Signature:** `CopyBulkG2SMulticastOp`

Bases: `CopyOp`

Bulk multicast copy asynchronous GMEM to SMEM Copy Operation.

The compiler does not perform thread election for this operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk).

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkG2SMulticastOp.__init__`

**Signature:** `__init__() → None`

## CopyBulkS2GOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkS2GOp`

**Signature:** `CopyBulkS2GOp`

Bases: `CopyOp`

Bulk copy asynchronous SMEM to GMEM Copy Operation.

The compiler does not perform thread election for this operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk).

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkS2GOp.__init__`

**Signature:** `__init__() → None`

## CopyBulkS2GByteMaskOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkS2GByteMaskOp`

**Signature:** `CopyBulkS2GByteMaskOp`

Bases: `CopyOp`

Bulk copy asynchronous SMEM to GMEM Copy Operation with mask. The i-th bit in the 16-bit wide byteMask operand specifies whether the i-th byte of each 16-byte wide chunk of source data is copied to the destination.

The compiler does not perform thread election for this operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk).

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkS2GByteMaskOp.__init__`

**Signature:** `__init__() → None`

## CopyBulkS2SOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkS2SOp`

**Signature:** `CopyBulkS2SOp`

Bases: `CopyOp`

Bulk copy asynchronous SMEM CTA to Cluster Copy Operation.

The compiler does not perform thread election for this operation.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk).

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.CopyBulkS2SOp.__init__`

**Signature:** `__init__() → None`

## TmaCopyOp

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.TmaCopyOp`

**Signature:** `TmaCopyOp`

Bases: `CopyOp`

Base class for all TMA copy operations.

## TmaInfo

**Kind:** class

**Qualified name:** `cutlass.cute.nvgpu.cpasync.TmaInfo`

**Signature:** `TmaInfo(copy_atom: CopyAtom, tma_tensor: Any, smem_layout: Any | None = None)`

Bases: `object`

Container for TMA Copy Atom and related data.

This class uses software composition to bundle a CopyAtom with the SMEM layout and TMA tensor.

Supports tuple unpacking for backward compatibility:

    atom, tma_tensor = make_tiled_tma_atom(...)

Access smem_layout via the container:

    tma_info = make_tiled_tma_atom(...)
    layout = tma_info.smem_layout

### Parameters

- **atom** ([*CopyAtom*](cute.md#cutlass.cute.CopyAtom)) -- The TMA Copy Atom

- **tma_tensor** -- The TMA tensor for coordinate mapping

- **smem_layout** -- The SMEM layout used to construct the TMA descriptor

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.nvgpu.cpasync.TmaInfo.__init__`

**Signature:** `__init__(copy_atom: CopyAtom, tma_tensor: Any, smem_layout: Any | None = None) → None`

### atom

**Kind:** property

**Qualified name:** `cutlass.cute.nvgpu.cpasync.TmaInfo.atom`

**Signature:** `atom: CopyAtom`

The TMA Copy Atom.

### tma_tensor

**Kind:** property

**Qualified name:** `cutlass.cute.nvgpu.cpasync.TmaInfo.tma_tensor`

**Signature:** `tma_tensor: Any`

The TMA tensor for coordinate mapping.

### smem_layout

**Kind:** property

**Qualified name:** `cutlass.cute.nvgpu.cpasync.TmaInfo.smem_layout`

**Signature:** `smem_layout: Any`

The SMEM layout used to construct the TMA descriptor.

## make_tiled_tma_atom

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.make_tiled_tma_atom`

**Signature:** `make_tiled_tma_atom(op: CopyBulkTensorTileG2SOp | CopyBulkTensorTileG2SMulticastOp | CopyBulkTensorTileS2GOp | CopyBulkTensorIm2ColG2SOp | CopyBulkTensorIm2ColS2GOp | CopyReduceBulkTensorTileS2GOp | CopyBulkTensor2DScatter4S2GOp | CopyBulkTensor2DGather4G2SOp | CopyBulkTensor2DGather4G2SMulticastOp, gmem_tensor: cutlass.cute.typing.Tensor, smem_layout_: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, cta_tiler: cutlass.cute.typing.Tiler, num_multicast: int = 1, *, gmem_coord_tensor: cutlass.cute.typing.Tensor | None = None, internal_type: Type[cutlass.cute.typing.Numeric] | None = None) → TmaInfo`

Makes a TMA Copy Atom to copy tiles of a GMEM tensor to/from SMEM buffer with the given Layout.

Supports `.tile` mode (default), `.tile::gather4` mode, and `.tile::scatter4` mode. Gather4 and scatter4 require `gmem_coord_tensor` so their index tensor layout can drive the returned TMA tensor shape. For layout conventions, examples, and restrictions, see [`tma_partition()`](#cutlass.cute.nvgpu.cpasync.tma_partition).

Given

- a GMEM tensor

- a SMEM layout

- a CTA-level Tiler

- (optional) a GMEM index tensor for gather4/scatter4 mode

this function figures out the bulk tensor asynchronous copy instruction to use with the maximum "TMA vector length" to copy tiles of the GMEM tensor to/from an SMEM buffer with the provided layout while maintaining consistency with the provided Tiler.

This function returns two results:

1.  the Copy Atom

2.  a TMA tensor that maps logical coordinates of the GMEM tensor to coordinates consumed by the TMA unit. TMA tensors contain basis stride elements that enable their associated layout to compute coordinates. Like other CuTe tensors, TMA tensors can be partitioned.

### Parameters

- **op** (*TMAOp*) -- The TMA Copy Operation to construct an Atom

- **gmem_tensor** (*Tensor*) -- The GMEM tensor involved in the Copy

- **smem_layout** (*Union\[Layout,* *ComposedLayout\]*) -- The SMEM layout to construct the Copy Atom, either w/ or w/o the stage mode

- **cta_tiler** (*Tiler*) -- The CTA Tiler to use

- **num_multicast** (*int*) -- The multicast factor

- **gmem_coord_tensor** (*Optional\[Tensor\]*) -- The GMEM index tensor for gather4/scatter4 mode (required for gather4 and scatter4 ops); see [`tma_partition()`](#cutlass.cute.nvgpu.cpasync.tma_partition) for layout conventions and restrictions

- **internal_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Optional internal data type to use when the tensor data type is not supported by the TMA unit

### Returns

A TmaInfo containing the Copy Atom, TMA tensor, and SMEM layout

### Return type

[TmaInfo](#cutlass.cute.nvgpu.cpasync.TmaInfo)

## tma_partition

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.tma_partition`

**Signature:** `tma_partition(atom: CopyAtom, cta_coord: cutlass.cute.typing.Coord, cta_layout: cutlass.cute.typing.Layout, smem_tensor: cutlass.cute.typing.Tensor, gmem_tensor: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...]) → Tuple[cutlass.cute.typing.Tensor, cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, cutlass.cute.typing.Tensor, cutlass.cute.typing.Tensor]`

Tiles the GMEM and SMEM tensors for the provided TMA Copy Atom.

For standard TMA modes (tiled, im2col, etc.), pass a single GMEM tensor:

tAsA, tAgA = tma_partition(atom, cta_coord, cta_layout, sA, gA)

For gather4 mode, pass a list of `[data_coord_tensor, index_tensor]`:

tAsA, tAgA, tAgI = tma_partition(atom, cta_coord, cta_layout, sA, \[gA, gI\])

For scatter4 mode, pass the destination coordinate tensor and index tensor as `[dst_coord_tensor, index_tensor]` and then pass the returned pair as the destination of `cute.copy()`. The same index tensor must be passed to [`make_tiled_tma_atom()`](#cutlass.cute.nvgpu.cpasync.make_tiled_tma_atom) as `gmem_coord_tensor` so the returned TMA tensor shape follows the index tensor shape rather than the destination data tensor shape:

atom, gB = make_tiled_tma_atom(op, dst, smem_layout, cta_tiler, gmem_coord_tensor=gI) tBsB, tBgB, tBgI = tma_partition(atom, cta_coord, cta_layout, sB, \[gB, gI\]) cute.copy(atom, tBsB, \[tBgB\[crd\], tBgI\[crd\]\])

The 2D gather4 TMA atom (`CopyBulkTensor2DGather4G2SOp`) issues `cp.async.bulk.tensor.2d.tile::gather4`, which loads four rows from a GMEM tile by gathering at four indirectly-addressed indices. Each PTX instruction consumes 5 coordinates `{crd0, crd1_i0, crd1_i1, crd1_i2, crd1_i3}`: one *contiguous* coordinate (`crd0`) and four *gather* coordinates pulled from `gmem_coord_tensor`.

The `gmem_coord_tensor` is a rank-2 `Int32` tensor whose layout selects which mode is the gather mode. One mode is broadcast (stride 0) and the other supplies the per-row gather indices:

For TMA override, pass `(gmem_tensor, residue_tensor)` where the gmem_tensor tracks the gmem address and stride, residue_tensor is a coordinate tensor with negated strides to track the remaining shape to copy.

- **Column-major data, gather along rows:** `stride=(0, 1)` --- mode 0 is the broadcast (contiguous) mode, mode 1 carries the gather indices.

      # GMEM data is (M, N) col-major; gather along the M dimension.
      gI = cute.make_tensor(
          idx_ptr, cute.make_layout((M, N), stride=(0, 1))
      )
      atom, gA = cpasync.make_tiled_tma_atom(
          cpasync.CopyBulkTensor2DGather4G2SOp(),
          gA, smem_layout, cta_tiler,
          gmem_coord_tensor=gI,
      )
      tAsA, tAgA, tAgI = cpasync.tma_partition(
          atom, 0, cute.make_layout(1), sA, [gA, gI],
      )
      cute.copy(atom, [tAgA[crd], tAgI[crd]], tAsA, tma_bar_ptr=mbar)

- **Row-major data, gather along cols:** `stride=(1, 0)` --- mode 1 is the broadcast (contiguous) mode, mode 0 carries the gather indices.

      gI = cute.make_tensor(
          idx_ptr, cute.make_layout((M, N), stride=(1, 0))
      )

**Restrictions** (enforced by Python and MLIR verifiers):

- `gmem_coord_tensor` layout must be 2D.

- The gather mode size must be `>= 4` (4 row indices per instruction).

- Exactly one mode of `gmem_coord_tensor` must be a broadcast (stride 0); that broadcast mode is *not* the gather dimension.

### Parameters

- **atom** -- The TMA Copy Atom

- **cta_coord** -- CTA coordinate within the cluster

- **cta_layout** -- Layout of CTAs in the cluster

- **smem_tensor** -- The SMEM tensor to partition

- **gmem_tensor** -- A single GMEM tensor, `[data_coord_tensor, index_tensor]` for gather4, or `[dst_coord_tensor, index_tensor]` for scatter4

### Returns

`(smem_tensor, gmem_tensor)` for standard TMA, or `(smem_tensor, coord_tensor, index_tensor)` for gather4/scatter4

## create_tma_multicast_mask

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.create_tma_multicast_mask`

**Signature:** `create_tma_multicast_mask(cta_layout_vmnk: cutlass.cute.typing.Layout, cta_coord_vmnk: cutlass.cute.typing.Coord, mcast_mode: int) → cutlass.cute.typing.Int16`

Computes a multicast mask for a TMA load Copy.

### Parameters

- **cta_layout_vmnk** (*Layout*) -- The VMNK layout of the cluster

- **cta_coord_vmnk** (*Coord*) -- The VMNK coordinate of the current CTA

- **mcast_mode** (*int*) -- The tensor mode in which to multicast

### Returns

The resulting mask

### Return type

[Int16](../basic_data_types.md#cutlass.Int16)

## prefetch_descriptor

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.prefetch_descriptor`

**Signature:** `prefetch_descriptor(tma_atom: CopyAtom) → None`

Prefetches the TMA descriptor associated with the TMA Atom.

## copy_tensormap

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.copy_tensormap`

**Signature:** `copy_tensormap(tma_atom: CopyAtom, tensormap_ptr: cutlass.cute.typing.Pointer) → None`

Copies the tensormap held by a TMA Copy Atom to the memory location pointed to by the provided pointer.

### Parameters

- **tma_atom** ([*CopyAtom*](cute.md#cutlass.cute.CopyAtom)) -- The TMA Copy Atom

- **tensormap_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- The pointer to the memory location to copy the tensormap to

## update_tma_descriptor

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.update_tma_descriptor`

**Signature:** `update_tma_descriptor(tma_atom: CopyAtom, gmem_tensor: cutlass.cute.typing.Tensor, tma_desc_ptr: cutlass.cute.typing.Pointer) → None`

Updates the TMA descriptor in the memory location pointed to by the provided pointer using information from a TMA Copy Atom and the provided GMEM tensor.

Specifically, the following fields of the TMA descriptor will be updated:

1.  the GMEM tensor base address

2.  the GMEM tensor shape

3.  the GMEM tensor stride

Other fields of the TMA descriptor are left unchanged.

### Parameters

- **tma_atom** ([*CopyAtom*](cute.md#cutlass.cute.CopyAtom)) -- The TMA Copy Atom

- **gmem_tensor** (*Tensor*) -- The GMEM tensor

- **tensormap_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- The pointer to the memory location of the descriptor to udpate

## fence_tma_desc_acquire

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.fence_tma_desc_acquire`

**Signature:** `fence_tma_desc_acquire(tma_desc_ptr: cutlass.cute.typing.Pointer) → None`

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).

## cp_fence_tma_desc_release

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.cp_fence_tma_desc_release`

**Signature:** `cp_fence_tma_desc_release(tma_desc_global_ptr: cutlass.cute.typing.Pointer, tma_desc_shared_ptr: cutlass.cute.typing.Pointer) → None`

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-tensormap-cp-fenceproxy).

## fence_tma_desc_release

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.fence_tma_desc_release`

**Signature:** `fence_tma_desc_release() → None`

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).

## group_bulk_copy_modes

**Kind:** function

**Qualified name:** `cutlass.cute.nvgpu.cpasync.group_bulk_copy_modes`

**Signature:** `group_bulk_copy_modes(src: cutlass.cute.typing.Tensor, dst: cutlass.cute.typing.Tensor) → Tuple[cutlass.cute.typing.Tensor, cutlass.cute.typing.Tensor]`

Copy async bulk need group mode 0, acquiring whole tensor for bulk copy
