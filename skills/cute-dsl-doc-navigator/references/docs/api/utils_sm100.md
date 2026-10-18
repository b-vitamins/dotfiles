---
title: "Blackwell (SM100)"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/utils_sm100.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# Blackwell (SM100)

## compute_epilogue_tile_size

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.compute_epilogue_tile_size`

**Signature:** `compute_epilogue_tile_size(cta_tile_m: int, cta_tile_n: int, use_2cta: bool, elem_width_d: int, elem_width_c: int | None = None, d_is_m_major: bool = True, c_is_m_major: bool = True, tmem_warp_shape_mn: Tuple[int, int] | None = None) → tuple[int, int]`

Compute epilogue subtile dimensions `(tile_m, tile_n)` (pure Python, no MLIR).

Used by [`compute_epilogue_tile_shape()`](#cutlass.utils.sm100.compute_epilogue_tile_shape) and at kernel-discovery time for SMEM capacity estimation. Background -- SM100 epilogue flow --------------------------------- After the MMA, the accumulator lives in **TMEM** (Tensor Memory, 128 datapaths x N columns). The epilogue:

1.  Loads a subtile from TMEM into registers (`tcgen05.tmem_load`).

2.  Optionally loads a source tile C from GMEM -\> SMEM -\> registers.

3.  Applies the fusion (bias, activation, ...) in registers.

4.  Stores the result D to SMEM, then to GMEM via TMA.

The full CTA output (`cta_tile_m x cta_tile_n`) is processed in multiple **epilogue iterations**, each covering one subtile of shape `(tile_m, tile_n)`. Each subtile is worked on by 4 warps arranged in a `(warp_m, warp_n)` grid.

This function picks `(tile_m, tile_n)` to balance three constraints:

- **TMEM load** only supports 16 or 32 datapaths per warp, which caps `tile_m`.

- **TMA store alignment** requires a minimum contiguous transaction size, which sets `n_min` floors.

- **SMEM budget** -- the epilogue subtile lives in SMEM alongside the mainloop's A/B pipeline buffers. A larger subtile means fewer epilogue iterations (good) but steals SMEM from the mainloop, reducing pipeline depth (bad). `n_perf` targets the sweet spot found by benchmarking.

## Algorithm

1.  **Warp grid** `(warp_m, warp_n)`:

    - `(2, 2)` when `use_2cta and cta_tile_m == 64` -- each of the 2 M-warps gets 32 datapaths.

    - `(4, 1)` otherwise -- 4 warps split M, each gets `tile_m / 4` datapaths (16 or 32).

2.  **tile_m** `= min(cta_tile_m, 32 * warp_m)`

    `32` is `dp_full`, the number of TMEM datapaths in one subpartition (hardware constant). Capping here ensures each warp owns at most 32 datapaths, which is the widest mode the `tcgen05.tmem_load` instruction supports.

3.  **n_perf** -- performance target for N:

    - **Without source C** (elementwise-only epilogue): SMEM pressure is low because there is no C tile to stage. Target a fixed element count per iteration so that each iteration does enough work to amortise the epilogue overhead. The constants are:

          4096 elements  (general)  -> e.g. tile_m=128, n=32
          8192 elements  (4-bit)    -> e.g. tile_m=128, n=64

      4-bit elements are half a byte each, so doubling the count keeps roughly the same SMEM footprint while cutting epilogue iterations in half (experimentally best for 4-bit types).

    - **With source C** (residual-load epilogue): the source tile C also occupies SMEM, so the epilogue tile must be smaller to leave room for mainloop A/B pipeline stages. Targets are chosen by element width and CTA shape to balance SMEM partitioning:

          32-bit elements: n=16 when M>64 and N<=128, else n=32
          16-bit elements: n=32 when N<=128, else n=64
          <=8-bit elements: n=64

      Wider elements consume more bytes per element, so N is reduced to stay within the SMEM budget. When CTA N is large (\>128), N is increased because the mainloop tile is also large and SMEM is more abundant.

    After choosing, `n_perf` is halved until it evenly divides `cta_tile_n` (ensures the CTA output tiles evenly into subtiles with no ragged remainder).

4.  **n_min_d, n_min_c** -- hard minimums from TMA store alignment:

    - **M-major** (contiguous dim is M): `8 * warp_n`. N is the strided dimension so the minimum is small (8 elements per warp is enough for the store to issue).

    - **N-major** (contiguous dim is N): each TMA store transaction is 128 bits wide, so the minimum contiguous N per warp is `128 / elem_width` elements, times `warp_n` warps.

    - **FP6 special case**: TMA store only supports the SW128B swizzle mode for 6-bit types, requiring 128 contiguous elements per warp, i.e. `128 * warp_n`.

5.  **tile_n** `= min(cta_tile_n, max(n_perf, n_min_c, n_min_d))`.

    If the chosen N doesn't evenly divide `cta_tile_n`, fall back to `cta_tile_n` (process the full N in one iteration).

param cta_tile_m:

:   Per-CTA tile size in M.

param cta_tile_n:

:   Per-CTA tile size in N.

param use_2cta:

:   True when 2-CTA (2-SM) MMA instructions are used.

param elem_width_d:

:   Bit-width of output element type D (e.g. 16).

param elem_width_c:

:   Bit-width of source element type C, or `None` if the epilogue has no source (elementwise-only).

param d_is_m_major:

:   `True` if D is column-major (M-contiguous).

param c_is_m_major:

:   `True` if C is column-major (M-contiguous).

return:

:   `(tile_m, tile_n)` -- epilogue subtile dimensions.

## compute_acc_tmem_cols_per_stage

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.compute_acc_tmem_cols_per_stage`

**Signature:** `compute_acc_tmem_cols_per_stage(cta_tile_m: int, cta_tile_n: int, use_2cta: bool, mma_n: int, transform_a_source_is_tmem: bool) → int`

Compute the accumulator TMEM column footprint for one pipeline stage.

Returns the **raw** layout footprint --- the caller must enforce hardware allocation constraints (min 32 columns, power-of-2 total) at the final `alloc_tmem` call site. See `TmemAllocator.check_valid_num_columns` in `cutlass/memory/tmem.py`.

**How TMEM packing works**

TMEM has 128 datapaths (rows). M maps to datapaths, N maps to columns. When fewer than 128 DPs are needed, multiple N-values can share a column by occupying different DP rows:

    NonInterleaved (each N-tile owns its columns):

        columns 0..N-1       columns N..2N-1
        ┌────────────────┐  ┌────────────────┐
    DP  │ ████ tile 0    │  │ ████ tile 1    │  ← 64 DPs used
    0-  │                │  │                │
    127 │ ···· unused    │  │ ···· unused    │  ← 64 DPs wasted
        └────────────────┘  └────────────────┘
        Total: 2N columns

    Interleaved (pairs of N-tiles share columns):

        columns 0..N-1
        ┌────────────────┐
    DP  │ ████ tile 0    │  ← DPs 0-15, 32-47, 64-79, 96-111
    0-  │ ▓▓▓▓ tile 1    │  ← DPs 16-31, 48-63, 80-95, 112-127
    127 │                │
        └────────────────┘
        Total: N columns  (halved)

For **1CTA**, the accumulator uses Interleaved when A is from SMEM *and* `cta_tile_m` == 64. NonInterleaved is forced when `cta_tile_m` == 128 (all datapaths already occupied) or when A is from TMEM (each datapath can only access its own row, so A and C must share the same M-to-datapath mapping).

For **2CTA**, each SM has its own 128-DP TMEM and the fragment layout is computed for the per-CTA shape (M = `cta_tile_m`, which the caller must set to the per-CTA value). Because `cta_tile_m` only occupies part of the 128 DPs, the remaining rows can hold additional N-values in the same column. The number of N-values that share a column is 128 / `cta_tile_m`, so the columns needed are `cta_tile_n` / (128 / `cta_tile_m`):

- `cta_tile_m` = 32 → 128/32 = 4 per column → `cta_tile_n` / 4

- `cta_tile_m` = 64 → 128/64 = 2 per column → `cta_tile_n` / 2

- `cta_tile_m` = 128 → 128/128 = 1 (no sharing) → `cta_tile_n`

### Parameters

- **cta_tile_m** -- Per-CTA tile size in M dimension (for 2CTA the caller divides the full tile M by 2).

- **cta_tile_n** -- CTA tile size in N dimension.

- **use_2cta** -- Whether 2CTA MMA instructions are used.

- **mma_n** -- MMA atom size in N dimension.

- **transform_a_source_is_tmem** -- Whether operand A is sourced from TMEM (forces NonInterleaved allocation).

### Returns

TMEM columns per accumulator stage (before HW constraints).

## compute_epilogue_tile_shape

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.compute_epilogue_tile_shape`

**Signature:** `compute_epilogue_tile_shape(cta_tile_shape: cutlass.cute.typing.Shape, use_2cta_instrs: bool, layout_d: LayoutEnum, elem_ty_d: Type[_MockObject], *, layout_c: LayoutEnum | None = None, elem_ty_c: Type[_MockObject] | None = None, tmem_warp_shape_mn: Tuple[int, int] | None = None) → cutlass.cute.typing.Tile`

Attempts to compute a reasonable epilogue tile based on block tile shape or allows the user to provide one.

### Parameters

- **cta_tile_shape** (*cute.Shape*) -- A tuple or list representing the dimensions of the CTA tile, where cta_tile_shape\[0\] corresponds to the height (M) and cta_tile_shape\[1\] corresponds to the width (N) of the tile.

- **use_2cta_instrs** (*bool*) -- A flag indicating whether the configuration is for a 2SM setup.

- **layout_d** (*LayoutEnum*) -- The layout enum of the output tensor D.

- **elem_ty_d** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type of output tensor D.

- **layout_c** (*LayoutEnum,* *optional*) -- The layout enum of the input tensor C. Defaults to None.

- **elem_ty_c** (*Union\[Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\],* *None\],* *optional*) -- The element type for input tensor C. Defaults to None.

- **tmem_warp_shape_mn** (*Tuple\[int,* *int\],* *optional*) -- Optional (warp_m, warp_n) override for the tmem subpartition layout. When omitted, the layout is derived from `cta_tile_shape` and `use_2cta_instrs`.

### Returns

Returns epilog tiler, which is used in subsequent epilog partitions.

### Return type

cute.Tile

### Raises

**ValueError** -- If the computed tile cute.size does not meet minimum requirements based on CTA dimensions.

## get_smem_store_op

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.get_smem_store_op`

**Signature:** `get_smem_store_op(layout_d: LayoutEnum, elem_ty_d: Type[_MockObject], elem_ty_acc: Type[_MockObject], tiled_tmem_load: TiledCopy) → CopyAtom`

Selects the largest vectorized smem store atom available subject to constraint of gmem layout and chosen TMEM_LOAD's thread-value ownership.

### Parameters

- **layout_d** (*LayoutEnum*) -- The layout enum of the output tensor D.

- **elem_ty_d** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for output tensor D.

- **elem_ty_acc** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for accumulator.

- **tiled_tmem_load** ([*cute.TiledCopy*](cute.md#cutlass.cute.TiledCopy)) -- An instance of TiledCopy that represents the tmem load operation.

### Returns

Either SmemStoreMatrix or SimtSyncCopy, based on the input parameters.

### Return type

[cute.CopyAtom](cute.md#cutlass.cute.CopyAtom)

## get_tmem_load_op

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.get_tmem_load_op`

**Signature:** `get_tmem_load_op(cta_tile_shape: cutlass.cute.typing.Shape, layout_d: LayoutEnum, elem_ty_d: Type[_MockObject], elem_ty_acc: Type[_MockObject], epi_tile: cutlass.cute.typing.Tile, use_2cta_instrs: bool, *, tmem_warp_shape_mn: Tuple[int, int] | None = None) → CopyAtom`

Finds a performant TMEM_LOAD copy op for the selected epilogue tile (epi_tile), element types, and tcgen05.mma instruction used.

### Parameters

- **cta_tile_shape** (*cute.Shape*) -- A tuple or list representing the dimensions of the CTA tile.

- **layout_d** (*LayoutEnum*) -- The layout enum of the output tensor D.

- **elem_ty_d** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for output tensor D.

- **elem_ty_acc** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for accumulation.

- **epi_tile** (*cute.Tile*) -- The epilogue tile configuration.

- **use_2cta_instrs** (*bool*) -- A flag indicating whether the configuration is for 2 SMs.

### Returns

An instance of Sm100TmemLoad with the computed configuration.

### Return type

[cute.CopyAtom](cute.md#cutlass.cute.CopyAtom)

### Raises

**ValueError** -- If the function cannot handle the given combination of accumulation and dimension types, or if it cannot determine the appropriate configuration based on the input parameters.

## make_smem_layout_a

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.make_smem_layout_a`

**Signature:** `make_smem_layout_a(tiled_mma: TiledMma, mma_tiler_mnk: cutlass.cute.typing.Tile, a_dtype: Type[_MockObject], num_stages: int, *, is_k_major: bool | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

This function helps with:

1.  Get the partitioned shape of the A tensor based on the tiled_mma & MMA tiler.

2.  Select the heuristic SMEM layout atom based on the A tensor's majorness, the data type, and the major mode size.

3.  cute.Tile the SMEM layout atom to the MMA tile shape.

4.  Stage the SMEM layout based on the number of stages.

### Parameters

- **tiled_mma** ([*cute.TiledMma*](cute.md#cutlass.cute.TiledMma)) -- The tiled MMA used to partition tensor A

- **mma_tiler_mnk** (*cute.cute.Tile*) -- The MMA tile shape

- **a_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for tensor A

- **num_stages** (*int*) -- The number of pipeline stages for tensor A

### Returns

SMEM layout for tensor A

### Return type

Union\[cute.Layout, cute.ComposedLayout\]

## make_smem_layout_b

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.make_smem_layout_b`

**Signature:** `make_smem_layout_b(tiled_mma: TiledMma, mma_tiler_mnk: cutlass.cute.typing.Tile, b_dtype: Type[_MockObject], num_stages: int, *, is_k_major: bool | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

This function helps:

1.  Get the partitioned shape of the B tensor based on the tiled_mma & MMA tiler.

2.  Select the heuristic SMEM layout atom based on the B tensor's majorness, the data type, and the major mode size.

3.  cute.Tile the SMEM layout atom to the MMA tile shape.

4.  Stage the SMEM layout based on the number of stages.

### Parameters

- **tiled_mma** ([*cute.TiledMma*](cute.md#cutlass.cute.TiledMma)) -- The tiled MMA which is used to partition the B tensor.

- **mma_tiler_mnk** (*cute.cute.Tile*) -- The MMA tile shape.

- **b_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for the B tensor.

- **num_stages** (*int*) -- The stage of the B tensor.

### Returns

SMEM layout for the B tensor.

### Return type

Union\[cute.Layout, cute.ComposedLayout\]

## make_smem_layout_epi

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.make_smem_layout_epi`

**Signature:** `make_smem_layout_epi(epi_dtype: Type[_MockObject], epi_layout: LayoutEnum, epi_tile: cutlass.cute.typing.Tile, epi_stage: int) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

This function helps:

1.  Select the heuristic SMEM layout atom based on the epilog tile shape, the epilog tensor's majorness, and the element type.

2.  cute.Tile the SMEM layout atom to the epilog tile shape.

3.  Stage the SMEM layout based on the number of stages.

### Parameters

- **epi_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for the epilog tensor.

- **epi_layout** (*LayoutEnum*) -- The layout enum for the epilog tensor.

- **epi_tile** (*cute.cute.Tile*) -- The epilogue tile shape.

- **epi_stage** (*int*) -- The stage of the epilog tensor.

### Returns

SMEM layout for epilog tensors (usually C & D which are processed in the epilog)

### Return type

Union\[cute.Layout, cute.ComposedLayout\]

## make_trivial_tiled_mma

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.make_trivial_tiled_mma`

**Signature:** `make_trivial_tiled_mma(*args: Any, **kwargs: Any) → TiledMma`

Make a tiled MMA atom with given data type, leading dimension, cta group and mma tile shape. By default, the MMA atom is created with SMEM operand source for A.

Supports two calling conventions:

**New (recommended):** separate `a_dtype` and `b_dtype`:

    make_trivial_tiled_mma(
        a_dtype, b_dtype, a_leading_mode, b_leading_mode,
        acc_dtype, cta_group, mma_tiler_mn, [a_source])

**Legacy (deprecated):** single `ab_dtype`:

    make_trivial_tiled_mma(
        ab_dtype, a_leading_mode, b_leading_mode,
        acc_dtype, cta_group, mma_tiler_mn, [a_source])

## make_blockscaled_trivial_tiled_mma

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.make_blockscaled_trivial_tiled_mma`

**Signature:** `make_blockscaled_trivial_tiled_mma(*args: Any, **kwargs: Any) → TiledMma`

Make a BlockScaled tiled MMA atom with given data type, leading dimension, cta group and mma tile shape. By default, the MMA atom is created with SMEM operand source for A.

Supports two calling conventions:

**New (recommended):** separate `a_dtype` and `b_dtype`:

    make_blockscaled_trivial_tiled_mma(
        a_dtype, b_dtype, a_leading_mode, b_leading_mode,
        sf_dtype, sf_vec_size, cta_group, mma_tiler_mn, [a_source])

**Legacy (deprecated):** single `ab_dtype`:

    make_blockscaled_trivial_tiled_mma(
        ab_dtype, a_leading_mode, b_leading_mode,
        sf_dtype, sf_vec_size, cta_group, mma_tiler_mn, [a_source])

## cluster_shape_to_tma_atom_A

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.cluster_shape_to_tma_atom_A`

**Signature:** `cluster_shape_to_tma_atom_A(cluster_shape_mnk: cutlass.cute.typing.Shape, atom_thr_id: cutlass.cute.typing.Layout) → CopyBulkTensorTileG2SMulticastOp | CopyBulkTensorTileG2SOp`

Select the appropriate TMA copy atom for A based on the number of SMs and the multicast flag.

### Parameters

- **cluster_shape_mnk** (*cute.Shape*) -- The shape of the cluster

- **atom_thr_id** (*cute.Layout*) -- The thread ID of the atom

### Returns

The appropriate TMA copy atom kind

### Return type

[cpasync.CopyBulkTensorTileG2SMulticastOp](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SMulticastOp) or [cpasync.CopyBulkTensorTileG2SOp](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp)

### Raises

- **ValueError** -- If the atom_sm_cnt is invalid

- **ValueError** -- If the cluster shape is not divisible by the atom SM count

## cluster_shape_to_tma_atom_B

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.cluster_shape_to_tma_atom_B`

**Signature:** `cluster_shape_to_tma_atom_B(cluster_shape_mnk: cutlass.cute.typing.Shape, atom_thr_id: cutlass.cute.typing.Layout) → CopyBulkTensorTileG2SMulticastOp | CopyBulkTensorTileG2SOp`

Select the appropriate TMA copy atom for Bbased on the number of SMs and the multicast flag.

### Parameters

- **cluster_shape_mnk** (*cute.Shape*) -- The shape of the cluster

- **atom_thr_id** (*cute.Layout*) -- The thread ID of the atom

### Returns

The appropriate TMA copy atom kind

### Return type

[cpasync.CopyBulkTensorTileG2SMulticastOp](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SMulticastOp) or [cpasync.CopyBulkTensorTileG2SOp](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp)

### Raises

- **ValueError** -- If the atom_sm_cnt is invalid

- **ValueError** -- If the cluster shape is not divisible by the atom SM count

## cluster_shape_to_tma_atom_SFB

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.cluster_shape_to_tma_atom_SFB`

**Signature:** `cluster_shape_to_tma_atom_SFB(cluster_shape_mnk: cutlass.cute.typing.Shape, atom_thr_id: cutlass.cute.typing.Layout) → CopyBulkTensorTileG2SMulticastOp | CopyBulkTensorTileG2SOp`

Select the appropriate TMA copy atom for SFB based on the number of SMs and the multicast flag.

### Parameters

- **cluster_shape_mnk** (*cute.Shape*) -- The shape of the cluster

- **atom_thr_id** (*cute.Layout*) -- The thread ID of the atom

### Returns

The appropriate TMA copy atom kind

### Return type

[cpasync.CopyBulkTensorTileG2SMulticastOp](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SMulticastOp) or [cpasync.CopyBulkTensorTileG2SOp](cute_nvgpu_cpasync.md#cutlass.cute.nvgpu.cpasync.CopyBulkTensorTileG2SOp)

### Raises

- **ValueError** -- If the atom_sm_cnt is invalid

- **ValueError** -- If the cluster shape is not divisible by the atom SM count

## get_permutation_mnk

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.get_permutation_mnk`

**Signature:** `get_permutation_mnk(tile_shape_mnk: cutlass.cute.typing.Shape, sf_vec_size: int, use_mxf8f6f4: bool) → Tuple[int, int, int]`

Get the permutation of M, N, K for the tiled MMA.

### Parameters

- **tile_shape_mnk** (*cute.Shape*) -- The shape of the tile

- **sf_vec_size** (*int*) -- The vector size of the Scale Factor.

- **use_mxf8f6f4** (*bool*) -- Whether to use MXF8F6F4 or MXF4NVF4.

### Returns

The permutation of M, N, K

### Return type

Tuple\[int, int, int\]

### Raises

**ValueError** -- If the tile shape is not divisible by the sf_vec_size

## get_num_tmem_alloc_cols

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.get_num_tmem_alloc_cols`

**Signature:** `get_num_tmem_alloc_cols(tmem_tensors: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor], rounding: bool = True) → int`

## make_sparse_a_dtype

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.make_sparse_a_dtype`

**Signature:** `make_sparse_a_dtype(a_raw_dtype: Type[_MockObject]) → Type[_MockObject]`

Create sparse A data type based on raw A data type.

### Parameters

**a_raw_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Element type for A tensor

### Returns

Sparse A data type

### Return type

Type\[[Numeric](../basic_data_types.md#cutlass.Numeric)\]

## make_sparse_e_dtype

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.make_sparse_e_dtype`

**Signature:** `make_sparse_e_dtype(a_raw_dtype: Type[_MockObject]) → Type[_MockObject]`

Create sparse E data type based on raw A data type.

### Parameters

**a_raw_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Element type for A tensor

### Returns

Sparse E data type

### Return type

Type\[[Numeric](../basic_data_types.md#cutlass.Numeric)\]

## make_sparse_gmem_layout_e

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.make_sparse_gmem_layout_e`

**Signature:** `make_sparse_gmem_layout_e(problem_shape_mnkl: cutlass.cute.typing.Shape, a_raw_dtype: Type[_MockObject]) → cutlass.cute.typing.Layout`

Create GMEM layout for sparse metadata E tensor.

### Parameters

- **problem_shape_mnkl** (*cute.Shape*) -- Problem shape (M, N, K, L)

- **a_raw_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Element type for A tensor

### Returns

GMEM layout for metadata E tensor

### Return type

cute.Layout

## make_sparse_smem_layout_a

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.make_sparse_smem_layout_a`

**Signature:** `make_sparse_smem_layout_a(tiled_mma: TiledMma, mma_tiler_mnk: cutlass.cute.typing.Tile, a_raw_dtype: Type[_MockObject], num_stages: int, sparsity: int = 2) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

This function helps with:

1.  Get the partitioned shape of the sparse A tensor based on the tiled_mma & MMA tiler.

2.  Select the heuristic SMEM layout atom based on the A tensor's majorness, data type, and sparsity.

3.  Tile the SMEM layout atom to the MMA tile shape.

4.  Stage the SMEM layout based on the number of stages.

### Parameters

- **tiled_mma** ([*cute.TiledMma*](cute.md#cutlass.cute.TiledMma)) -- The sparse tiled MMA used to partition tensor A

- **mma_tiler_mnk** (*cute.Tile*) -- The MMA tile shape

- **a_raw_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for sparse tensor A

- **num_stages** (*int*) -- The number of pipeline stages for tensor A

- **sparsity** (*int*) -- The sparsity ratio for tensor A (default: 2 for 2:4 structured sparsity)

### Returns

SMEM layout for sparse tensor A

### Return type

Union\[cute.Layout, cute.ComposedLayout\]

## make_sparse_smem_layout_e

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.make_sparse_smem_layout_e`

**Signature:** `make_sparse_smem_layout_e(tiled_mma: TiledMma, mma_tiler_e_mnk: cutlass.cute.typing.Tile, a_raw_dtype: Type[_MockObject], num_stages: int) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

This function helps with:

1.  Get the partitioned shape of the E tensor based on the tiled_mma & MMA tiler.

2.  Create SMEM layout atom with swizzle and sparse metadata flags.

3.  Tile the SMEM layout atom to the MMA tile shape.

4.  Stage the SMEM layout based on the number of stages.

### Parameters

- **tiled_mma** ([*cute.TiledMma*](cute.md#cutlass.cute.TiledMma)) -- The sparse tiled MMA used to partition tensor E

- **mma_tiler_e_mnk** (*cute.Tile*) -- The E tile shape (M, N, K) in logical elements

- **a_raw_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Element type for A tensor

- **num_stages** (*int*) -- Number of pipeline stages for tensor E

### Returns

SMEM layout for tensor E

### Return type

Union\[cute.Layout, cute.ComposedLayout\]

## make_sparse_tmem_layout_e

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.make_sparse_tmem_layout_e`

**Signature:** `make_sparse_tmem_layout_e(tmem_shape: cutlass.cute.typing.Shape, a_raw_dtype: Type[_MockObject]) → cutlass.cute.typing.Layout`

Create TMEM layout for sparse metadata E tensor.

This function implements the logic from tmem_e_frg::make() in mma_traits_sm100.hpp. It creates a TMEM layout based on the A matrix data type and the metadata shape.

### Parameters

- **tmem_shape** (*cute.Shape*) -- Shape from E SMEM layout, expected format ((M_MMA, N_MMA), ...)

- **a_raw_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of A matrix (determines atom layout)

### Returns

TMEM layout for metadata tensor

### Return type

cute.Layout

### Raises

- **ValueError** -- If shape structure is invalid or M_MMA is not 128

- **ValueError** -- If N_MMA doesn't match expected value for the data type

- **ValueError** -- If a_raw_dtype bits is not supported

## thrfrg_SFA

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.thrfrg_SFA`

**Signature:** `thrfrg_SFA(sfa_tensor: cutlass.cute.typing.Tensor, tiled_mma: TiledMma) → cutlass.cute.typing.Tensor`

Thread-fragment scale factor A tensor for SM120 block-scaled MMA.

Implements the ThrFrg partitioning for scale factor A according to the corresponding C++ code in cutlass/include/cute/atom/mma_traits_sm120.hpp: SFALayout for SM120 MXF4 16x8x64 uses K=64, SM120 MXF8F6F4 16x8x32 uses K=32; the stride pattern `((_8,_0,_1), _16)` is shared.

## thrfrg_SFB

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.thrfrg_SFB`

**Signature:** `thrfrg_SFB(sfb_tensor: cutlass.cute.typing.Tensor, tiled_mma: TiledMma) → cutlass.cute.typing.Tensor`

Thread-fragment scale factor B tensor for SM120 block-scaled MMA.

Implements the ThrFrg partitioning for scale factor B according to the corresponding C++ code in cutlass/include/cute/atom/mma_traits_sm120.hpp: SFBLayout for SM120 MXF4 16x8x64 uses K=64, SM120 MXF8F6F4 16x8x32 uses K=32; the stride pattern `((_0,_1), _8)` is shared.

## partition_fragment_SFA

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.partition_fragment_SFA`

**Signature:** `partition_fragment_SFA(sfa_tensor: cutlass.cute.typing.Tensor, thr_mma: ThrMma, tidx: int) → cutlass.cute.typing.Tensor`

Partition and create a register fragment for scale factor A.

## partition_fragment_SFB

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.partition_fragment_SFB`

**Signature:** `partition_fragment_SFB(sfb_tensor: cutlass.cute.typing.Tensor, thr_mma: ThrMma, tidx: int) → cutlass.cute.typing.Tensor`

Partition and create a register fragment for scale factor B.

## get_layoutSFA_TV

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.get_layoutSFA_TV`

**Signature:** `get_layoutSFA_TV(tiled_mma: TiledMma) → cutlass.cute.typing.Layout`

Get the Thread-Value layout for scale factor A.

## get_layoutSFB_TV

**Kind:** function

**Qualified name:** `cutlass.utils.sm100.get_layoutSFB_TV`

**Signature:** `get_layoutSFB_TV(tiled_mma: TiledMma) → cutlass.cute.typing.Layout`

Get the Thread-Value layout for scale factor B.
