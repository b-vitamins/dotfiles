---
title: "Utilities"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/utils.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# Utilities

The `cutlass.utils` package contains utilities for developing kernels with CuTe DSL.

## cutlass.utils.get_smem_capacity_in_bytes

**Kind:** function

**Signature:** `cutlass.utils.get_smem_capacity_in_bytes(compute_capability: str | None = None) → int`

Get the shared memory capacity in bytes for a given compute capability.

Returns the maximum shared memory capacity in bytes available for the specified GPU compute capability.

### Parameters

**compute_capability** (*Optional\[str\]*) -- The compute capability string (e.g. "70", "75", "80")

### Returns

The shared memory capacity in bytes

### Return type

int

### Raises

**ValueError** -- If the compute capability is not supported

## cutlass.utils.get_kernel_smem_size

**Kind:** function

**Signature:** `cutlass.utils.get_kernel_smem_size(kernel: Callable) → int`

Get the total static shared memory allocation in bytes for a kernel.

Uses `cute.kernel_smem_size` to query the total smem bytes that will be allocated by a kernel. The result is lowered to a compile-time constant by `InferKernelSmemUsagePass`.

Must be called from within a `@cute.jit` body after the kernel's `.launch()` has been called, which triggers tracing and registers the kernel's MLIR symbol.

### Parameters

**kernel** (*Callable*) -- A `@cute.kernel`-decorated function. The MLIR symbol is retrieved automatically from state stored by the DSL after `.launch()`.

### Returns

Total shared memory allocated by the kernel, in bytes.

### Return type

int (i64 MLIR value during tracing)

## cutlass.utils.SmemAllocator

**Kind:** class

**Signature:** `cutlass.utils.SmemAllocator(**kwargs)`

Bases: `SmemAllocator`

Deprecated alias for cutlass.memory.smem.SmemAllocator.

### capacity_in_bytes

**Kind:** method

**Qualified name:** `cutlass.utils.SmemAllocator.capacity_in_bytes`

**Signature:** `static capacity_in_bytes(compute_capability: str | None = None) → int`

Get the shared memory capacity in bytes for a given compute capability.

Returns the maximum shared memory capacity in bytes available for the specified GPU compute capability.

#### Parameters

**compute_capability** (*Optional\[str\]*) -- The compute capability string (e.g. "70", "75", "80")

#### Returns

The shared memory capacity in bytes

#### Return type

int

#### Raises

**ValueError** -- If the compute capability is not supported

## cutlass.utils.TmemAllocator

**Kind:** class

**Signature:** `cutlass.utils.TmemAllocator(**kwargs)`

Bases: `TmemAllocator`

Deprecated alias for cutlass.memory.tmem.TmemAllocator.

## cutlass.utils.TmemBufferPool

**Kind:** class

**Signature:** `cutlass.utils.TmemBufferPool(**kwargs)`

Bases: `TmemBufferPool`

Deprecated alias for cutlass.memory.tmem.TmemBufferPool.

## cutlass.utils.get_num_tmem_alloc_cols

**Kind:** function

**Signature:** `cutlass.utils.get_num_tmem_alloc_cols(tmem_tensors: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor], rounding: bool = True, *, arch: str = 'sm_100') → int`

Get the total number of TMEM allocation columns for the given TMEM tensors.

### Parameters

- **tmem_tensors** (*Union\[cute.Tensor,* *List\[cute.Tensor\]\]*) -- The TMEM tensors to get the number of allocation columns for.

- **rounding** (*bool*) -- Whether to round up the number of allocation columns to the nearest power of 2.

- **arch** (*str*) -- The architecture of the GPU.

### Returns

The total number of TMEM allocation columns.

### Return type

int

### Raises

**ValueError** -- If the number of TMEM allocation columns exceeds the maximum capacity or is less than 32.

## cutlass.utils.compute_tmem_cols_from_layout

**Kind:** function

**Signature:** `cutlass.utils.compute_tmem_cols_from_layout(layout: cutlass.cute.typing.Layout, dtype: Type[_MockObject]) → int`

Compute the number of TMEM columns required for a layout with a given dtype.

This function calculates the column offset by recasting the layout to Int32 and computing its cosize, similar to how find_tmem_tensor_col_offset works but without requiring a tensor.

### Parameters

- **layout** (*cute.Layout*) -- The TMEM layout to compute columns for.

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The data type of the elements in the layout.

### Returns

The number of TMEM columns (always a Python int).

### Return type

int

### Raises

**ValueError** -- If the layout size cannot be determined at compile time.

## cutlass.utils.WorkTileInfo

**Kind:** class

**Signature:** `cutlass.utils.WorkTileInfo(tile_idx: cutlass.cute.typing.Coord, is_valid_tile: _MockObject)`

Bases: `object`

A class to represent information about a work tile.

### Variables

- **tile_idx** -- The index of the tile.

- **is_valid_tile** -- Whether the tile is valid.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.utils.WorkTileInfo.__init__`

**Signature:** `__init__(tile_idx: cutlass.cute.typing.Coord, is_valid_tile: _MockObject)`

### is_valid_tile

**Kind:** property

**Qualified name:** `cutlass.utils.WorkTileInfo.is_valid_tile`

**Signature:** `is_valid_tile: _MockObject`

Check latest tile returned by the scheduler is valid or not. Any scheduling requests after all tasks completed will return an invalid tile.

#### Returns

The validity of the tile.

#### Return type

[Boolean](../basic_data_types.md#cutlass.Boolean)

### tile_idx

**Kind:** property

**Qualified name:** `cutlass.utils.WorkTileInfo.tile_idx`

**Signature:** `tile_idx: cutlass.cute.typing.Coord`

Get the index of the tile.

#### Returns

The index of the tile.

#### Return type

cute.Coord

## cutlass.utils.PersistentTileSchedulerParams

**Kind:** class

**Signature:** `cutlass.utils.PersistentTileSchedulerParams(**kwargs)`

Bases: `MixedClusterParamsMixin`

A class to represent parameters for a persistent tile scheduler.

This class is designed to manage and compute the layout of clusters and tiles in a batched gemm problem.

### Variables

- **cluster_shape_mn** -- Shape of the cluster in (m, n) dimensions (K dimension cta count must be 1).

- **problem_layout_ncluster_mnl** -- Layout of the problem in terms of number of clusters in (m, n, l) dimensions.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.utils.PersistentTileSchedulerParams.__init__`

**Signature:** `__init__() → None`

Initializes the PersistentTileSchedulerParams with the given parameters.

#### Parameters

- **problem_shape_ntile_mnl** (*cute.Shape*) -- The shape of the problem in terms of number of CTA (Cooperative Thread Array) in (m, n, l) dimensions.

- **cluster_shape_mnk** (*cute.Shape*) -- The shape of the cluster in (m, n) dimensions.

- **swizzle_size** (*int*) -- Swizzling size in the unit of cluster. 1 means no swizzle

- **raster_along_m** (*bool*) -- Rasterization order of clusters. Only used when swizzle_size \> 1. True means along M, false means along N.

- **fallback_cluster_shape_mnk** (*Optional\[cute.Shape\]*) -- Optional. When provided and different from cluster_shape_mnk, the kernel runs in mixed-cluster mode.

#### Raises

**ValueError** -- If cluster_shape_k is not 1.

### \_extract_primary_mlir_values

**Kind:** method

**Qualified name:** `cutlass.utils.PersistentTileSchedulerParams._extract_primary_mlir_values`

**Signature:** `_extract_primary_mlir_values() → list[ir.Value]`

### \_primary_values_count

**Kind:** property

**Qualified name:** `cutlass.utils.PersistentTileSchedulerParams._primary_values_count`

**Signature:** `_primary_values_count: int`

### \_new_primary_from_mlir_values

**Kind:** method

**Qualified name:** `cutlass.utils.PersistentTileSchedulerParams._new_primary_from_mlir_values`

**Signature:** `_new_primary_from_mlir_values(values: list[ir.Value]) → PersistentTileSchedulerParams`

### get_grid_shape

**Kind:** method

**Qualified name:** `cutlass.utils.PersistentTileSchedulerParams.get_grid_shape`

**Signature:** `get_grid_shape(max_active_clusters: _MockObject) → Tuple[_MockObject, _MockObject, _MockObject]`

Computes the grid shape based on the maximum active clusters allowed.

#### Parameters

**max_active_clusters** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The maximum number of active clusters that can run in one wave.

#### Returns

A tuple containing the grid shape in (m, n, persistent_clusters). - m: self.cluster_shape_m. - n: self.cluster_shape_n. - persistent_clusters: Number of persistent clusters that can run.

## cutlass.utils.StaticPersistentTileScheduler

**Kind:** class

**Signature:** `cutlass.utils.StaticPersistentTileScheduler(params: PersistentTileSchedulerParams, num_persistent_clusters: _MockObject, current_work_linear_idx: _MockObject, cta_id_in_cluster: cutlass.cute.typing.Coord, num_tiles_executed: _MockObject)`

Bases: `object`

A scheduler for static persistent tile execution in CUTLASS/CuTe kernels.

### Variables

- **params** -- Tile schedule related params, including cluster shape and problem_layout_ncluster_mnl

- **num_persistent_clusters** -- Number of persistent clusters that can be launched

- **cta_id_in_cluster** -- ID of the CTA within its cluster

- **\_num_tiles_executed** -- Counter for executed tiles

- **\_current_work_linear_idx** -- Current cluster index

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.utils.StaticPersistentTileScheduler.__init__`

**Signature:** `__init__(params: PersistentTileSchedulerParams, num_persistent_clusters: _MockObject, current_work_linear_idx: _MockObject, cta_id_in_cluster: cutlass.cute.typing.Coord, num_tiles_executed: _MockObject)`

Initializes the StaticPersistentTileScheduler with the given parameters.

#### Parameters

- **params** ([*PersistentTileSchedulerParams*](#cutlass.utils.PersistentTileSchedulerParams)) -- Tile schedule related params, including cluster shape and problem_layout_ncluster_mnl.

- **num_persistent_clusters** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Number of persistent clusters that can be launched.

- **current_work_linear_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Current cluster index.

- **cta_id_in_cluster** (*cute.Coord*) -- ID of the CTA within its cluster.

- **num_tiles_executed** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Counter for executed tiles.

### create

**Kind:** method

**Qualified name:** `cutlass.utils.StaticPersistentTileScheduler.create`

**Signature:** `static create(params: PersistentTileSchedulerParams, block_idx: Tuple[_MockObject, _MockObject, _MockObject], grid_dim: Tuple[_MockObject, _MockObject, _MockObject]) → StaticPersistentTileScheduler`

Initialize the static persistent tile scheduler.

#### Parameters

- **params** ([*PersistentTileSchedulerParams*](#cutlass.utils.PersistentTileSchedulerParams)) -- Parameters for the persistent tile scheduler.

- **block_idx** (*Tuple\[*[*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*\]*) -- The 3d block index in the format (bidx, bidy, bidz).

- **grid_dim** (*Tuple\[*[*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*\]*) -- The 3d grid dimensions for kernel launch.

#### Returns

A StaticPersistentTileScheduler object.

#### Return type

[StaticPersistentTileScheduler](#cutlass.utils.StaticPersistentTileScheduler)

### get_grid_shape

**Kind:** method

**Qualified name:** `cutlass.utils.StaticPersistentTileScheduler.get_grid_shape`

**Signature:** `static get_grid_shape(params: PersistentTileSchedulerParams, max_active_clusters: _MockObject) → Tuple[_MockObject, _MockObject, _MockObject]`

Calculates the grid shape to be launched on GPU using problem shape, threadblock shape, and active cluster size.

#### Parameters

- **params** ([*PersistentTileSchedulerParams*](#cutlass.utils.PersistentTileSchedulerParams)) -- Parameters for grid shape calculation.

- **max_active_clusters** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Maximum active clusters allowed.

#### Returns

The calculated 3d grid shape.

#### Return type

Tuple\[[Integer](../basic_data_types.md#cutlass.Integer), [Integer](../basic_data_types.md#cutlass.Integer), [Integer](../basic_data_types.md#cutlass.Integer)\]

### \_get_current_work_for_linear_idx

**Kind:** method

**Qualified name:** `cutlass.utils.StaticPersistentTileScheduler._get_current_work_for_linear_idx`

**Signature:** `_get_current_work_for_linear_idx(current_work_linear_idx: _MockObject) → WorkTileInfo`

Compute current tile coord given current_work_linear_idx and cta_id_in_cluster.

#### Parameters

**current_work_linear_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The linear index of the current work.

#### Returns

An object containing information about the current tile coordinates and validity status.

#### Return type

[WorkTileInfo](#cutlass.utils.WorkTileInfo)

### \_get_cluster_work_idx_with_fastdivmod

**Kind:** method

**Qualified name:** `cutlass.utils.StaticPersistentTileScheduler._get_cluster_work_idx_with_fastdivmod`

**Signature:** `_get_cluster_work_idx_with_fastdivmod(current_work_linear_idx: _MockObject) → Tuple[_MockObject, _MockObject, _MockObject]`

FastDivmod optimized CLUSTER coordinate calculation.

CRITICAL: This should mimic problem_layout_ncluster_mnl.get_hier_coord() which returns CLUSTER coordinates, not tile coordinates!

#### Parameters

**current_work_linear_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Linear index in the work space

#### Returns

Cluster coordinates (m, n, l) or None if FastDivmod not available

#### Return type

Tuple\[[Int32](../basic_data_types.md#cutlass.Int32), [Int32](../basic_data_types.md#cutlass.Int32), [Int32](../basic_data_types.md#cutlass.Int32)\] or None

### get_current_work

**Kind:** method

**Qualified name:** `cutlass.utils.StaticPersistentTileScheduler.get_current_work`

**Signature:** `get_current_work() → WorkTileInfo`

### initial_work_tile_info

**Kind:** method

**Qualified name:** `cutlass.utils.StaticPersistentTileScheduler.initial_work_tile_info`

**Signature:** `initial_work_tile_info() → WorkTileInfo`

### advance_to_next_work

**Kind:** method

**Qualified name:** `cutlass.utils.StaticPersistentTileScheduler.advance_to_next_work`

**Signature:** `advance_to_next_work(*, advance_count: int = 1) → None`

### num_tiles_executed

**Kind:** property

**Qualified name:** `cutlass.utils.StaticPersistentTileScheduler.num_tiles_executed`

**Signature:** `num_tiles_executed: _MockObject`

## cutlass.utils.StaticPersistentRuntimeTileScheduler

**Kind:** class

**Signature:** `cutlass.utils.StaticPersistentRuntimeTileScheduler(**kwargs)`

Bases: [`StaticPersistentTileScheduler`](#cutlass.utils.StaticPersistentTileScheduler)

A scheduler for static persistent runtime tile execution in CUTLASS/CuTe kernels. This scheduler will always launch all the SMs and the scheduler will generate the real tile info for each SM.

### Variables

- **params** -- Tile schedule related params, including cluster shape and problem_layout_ncluster_mnl

- **num_persistent_clusters** -- Number of persistent clusters that can be launched

- **cta_id_in_cluster** -- ID of the CTA within its cluster

- **\_num_tiles_executed** -- Counter for executed tiles

- **\_current_work_linear_idx** -- Current cluster index

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.utils.StaticPersistentRuntimeTileScheduler.__init__`

**Signature:** `__init__(params: PersistentTileSchedulerParams, num_persistent_clusters: _MockObject, current_work_linear_idx: _MockObject, cta_id_in_cluster: cutlass.cute.typing.Coord, num_tiles_executed: _MockObject, inner_mode: int = 1)`

Initializes the StaticPersistentTileScheduler with the given parameters.

#### Parameters

- **params** ([*PersistentTileSchedulerParams*](#cutlass.utils.PersistentTileSchedulerParams)) -- Tile schedule related params, including cluster shape and problem_layout_ncluster_mnl.

- **num_persistent_clusters** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Number of persistent clusters that can be launched.

- **current_work_linear_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Current cluster index.

- **cta_id_in_cluster** (*cute.Coord*) -- ID of the CTA within its cluster.

- **num_tiles_executed** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Counter for executed tiles.

- **inner_mode** (*int*) -- The inner mode along which the linear index will be decomposed first.

### create

**Kind:** method

**Qualified name:** `cutlass.utils.StaticPersistentRuntimeTileScheduler.create`

**Signature:** `static create(params: PersistentTileSchedulerParams, block_idx: Tuple[_MockObject, _MockObject, _MockObject], grid_dim: Tuple[_MockObject, _MockObject, _MockObject], inner_mode: int = 1) → StaticPersistentRuntimeTileScheduler`

Initialize the static persistent tile scheduler.

#### Parameters

- **params** ([*PersistentTileSchedulerParams*](#cutlass.utils.PersistentTileSchedulerParams)) -- Parameters for the persistent tile scheduler.

- **block_idx** (*Tuple\[*[*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*\]*) -- The 3d block index in the format (bidx, bidy, bidz).

- **grid_dim** (*Tuple\[*[*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*\]*) -- The 3d grid dimensions for kernel launch.

- **inner_mode** (*int*) -- The inner mode along which the linear index will be decomposed first.

#### Returns

A StaticPersistentRuntimeTileScheduler object.

#### Return type

[StaticPersistentRuntimeTileScheduler](#cutlass.utils.StaticPersistentRuntimeTileScheduler)

### \_get_current_work_for_linear_idx

**Kind:** method

**Qualified name:** `cutlass.utils.StaticPersistentRuntimeTileScheduler._get_current_work_for_linear_idx`

**Signature:** `_get_current_work_for_linear_idx(current_work_linear_idx: _MockObject) → WorkTileInfo`

Compute current tile coord given current_work_linear_idx and cta_id_in_cluster.

#### Parameters

**current_work_linear_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The linear index of the current work.

#### Returns

An object containing information about the current tile coordinates and validity status.

#### Return type

[WorkTileInfo](#cutlass.utils.WorkTileInfo)

## cutlass.utils.TensorMapManager

**Kind:** class

**Signature:** `cutlass.utils.TensorMapManager(**kwargs)`

Bases: `TensorMapManager`

Deprecated alias for cutlass.tensor_utils.TensorMapManager.

## cutlass.utils.GroupSearchResult

**Kind:** class

**Signature:** `cutlass.utils.GroupSearchResult(**kwargs)`

Bases: `object`

The result of the group search for grouped gemm.

### Parameters

- **group_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The result group index

- **cta_tile_idx_m** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- CTA tile index along M dimension after rasterization

- **cta_tile_idx_n** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- CTA tile index along N dimension after rasterization

- **problem_shape_m** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The M dimension of the gemm problem

- **problem_shape_n** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The N dimension of the gemm problem

- **problem_shape_k** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The K dimension of the gemm problem

- **cta_tile_count_k** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Number of tiles along K dimension

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.utils.GroupSearchResult.__init__`

**Signature:** `__init__(group_idx: _MockObject, cta_tile_idx_m: _MockObject, cta_tile_idx_n: _MockObject, problem_shape_m: _MockObject, problem_shape_n: _MockObject, problem_shape_k: _MockObject, cta_tile_count_k: _MockObject) → None`

## cutlass.utils.GroupedGemmGroupSearchState

**Kind:** class

**Signature:** `cutlass.utils.GroupedGemmGroupSearchState(**kwargs)`

Bases: `object`

The state of group index search for grouped gemm.

The state will be initialized once and updated in every round of group index search.

### Parameters

- **start_group_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The group idx to start the search with

- **tile_count_prev_group** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Number of tiles before the matched group

- **tile_count_searched** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Number of tiles we have searched. When the matched group is found, it records the number of tiles including the matched group

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.utils.GroupedGemmGroupSearchState.__init__`

**Signature:** `__init__(start_group_idx: _MockObject, tile_count_prev_group: _MockObject, tile_count_searched: _MockObject, found: _MockObject) → None`

## cutlass.utils.create_initial_search_state

**Kind:** function

**Signature:** `cutlass.utils.create_initial_search_state() → GroupedGemmGroupSearchState`

Create an initial search state for grouped gemm.

### Returns

A new search state with initial values

### Return type

[GroupedGemmGroupSearchState](#cutlass.utils.GroupedGemmGroupSearchState)

## cutlass.utils.GroupedGemmTileSchedulerHelper

**Kind:** class

**Signature:** `cutlass.utils.GroupedGemmTileSchedulerHelper(**kwargs)`

Bases: `object`

A helper to translate the raw block index (x, y, z) from tile scheduler to real CTA tile index for grouped gemm.

### Parameters

- **group_count** (*int*) -- Number of groups in current grouped gemm problem

- **tile_sched_params** ([*PersistentTileSchedulerParams*](#cutlass.utils.PersistentTileSchedulerParams)) -- Parameter used to create the tile scheduler this helper works with

- **cluster_tile_shape_mnk** (*tuple\[int,* *int,* *int\]*) -- The shape of cluster tile as (m, n, k)

- **search_state** ([*GroupedGemmGroupSearchState*](#cutlass.utils.GroupedGemmGroupSearchState)) -- The initial search state

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.utils.GroupedGemmTileSchedulerHelper.__init__`

**Signature:** `__init__(group_count: int, tile_sched_params: PersistentTileSchedulerParams, cluster_tile_shape_mnk: tuple[int, int, int], search_state: GroupedGemmGroupSearchState) → None`

### delinearize_z

**Kind:** method

**Qualified name:** `cutlass.utils.GroupedGemmTileSchedulerHelper.delinearize_z`

**Signature:** `delinearize_z(cta_tile_coord: tuple, problem_shape_mnkl: cutlass.cute.typing.Tensor) → GroupSearchResult`

Delinearize the linear z index and return GroupSearchResult.

This function should be used by warps that need to know the CTA tile index on M and N dimensions.

#### Parameters

- **cta_tile_coord** (*tuple* *of* [*Int32*](../basic_data_types.md#cutlass.Int32)) -- The raw CTA coordinate from tile scheduler

- **problem_shape_mnkl** (*cute.Tensor*) -- Tensor containing gemm problem size (M, N, K, L) for each group

#### Returns

The search result containing group index and tile coordinates

#### Return type

[GroupSearchResult](#cutlass.utils.GroupSearchResult)

### search_cluster_tile_count_k

**Kind:** method

**Qualified name:** `cutlass.utils.GroupedGemmTileSchedulerHelper.search_cluster_tile_count_k`

**Signature:** `search_cluster_tile_count_k(cta_tile_coord: tuple, problem_shape_mnkl: cutlass.cute.typing.Tensor) → Tuple[_MockObject, _MockObject]`

Search the matched group for given linear index and compute the number of tiles along K dimension for the matched group.

This function should be used by warps that are only interested in the number of tiles along K dimension.

#### Parameters

- **cta_tile_coord** (*tuple* *of* [*Int32*](../basic_data_types.md#cutlass.Int32)) -- The raw CTA coordinate from tile scheduler

- **problem_shape_mnkl** (*cute.Tensor*) -- Tensor containing gemm problem size (M, N, K, L) for all groups

#### Returns

A tuple containing cluster count along K dimension and the group index

#### Return type

Tuple\[[Int32](../basic_data_types.md#cutlass.Int32), [Int32](../basic_data_types.md#cutlass.Int32)\]

### \_prefix_sum

**Kind:** method

**Qualified name:** `cutlass.utils.GroupedGemmTileSchedulerHelper._prefix_sum`

**Signature:** `_prefix_sum(value_per_thread: _MockObject) → _MockObject`

Perform prefix sum within a full warp.

#### Parameters

**value_per_thread** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The value for this thread to contribute to the prefix sum

#### Returns

The prefix sum result for this thread

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### \_get_problem_for_group

**Kind:** method

**Qualified name:** `cutlass.utils.GroupedGemmTileSchedulerHelper._get_problem_for_group`

**Signature:** `_get_problem_for_group(problem_shape_mnkl: cutlass.cute.typing.Tensor, group_idx: _MockObject) → cutlass.cute.typing.Tensor`

Load gemm problem (m,n,k,l) for the specified group from global memory to register.

#### Parameters

- **problem_shape_mnkl** (*cute.Tensor*) -- Tensor in global memory with layout (group_count, 4):(4, 1)

- **group_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The index of the group to load

#### Returns

The problem shape tensor for the specified group

#### Return type

cute.Tensor

### \_get_cluster_tile_count_mn

**Kind:** method

**Qualified name:** `cutlass.utils.GroupedGemmTileSchedulerHelper._get_cluster_tile_count_mn`

**Signature:** `_get_cluster_tile_count_mn(problem_shape: cutlass.cute.typing.Tensor) → _MockObject`

Compute total cluster count.

#### Parameters

**problem_shape** (*cute.Tensor*) -- Tensor containing problem shape (m, n, k, l)

#### Returns

The total cluster tile count for M and N dimensions

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### \_compute_cta_tile_coord

**Kind:** method

**Qualified name:** `cutlass.utils.GroupedGemmTileSchedulerHelper._compute_cta_tile_coord`

**Signature:** `_compute_cta_tile_coord(cluster_tile_idx: _MockObject, cta_tile_coord_in_cluster: tuple, cluster_tile_count_m: _MockObject, cluster_tile_count_n: _MockObject) → tuple`

Compute CTA tile indices along M and N dimensions based on the linear index within a group.

It uses the AlongM mode to decompose the linear index onto M and N dimensions.

#### Parameters

- **cluster_tile_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The linear index within a group

- **cta_tile_coord_in_cluster** (*tuple* *of* [*Int32*](../basic_data_types.md#cutlass.Int32)) -- CTA indices along M and N dimensions within a cluster

- **cluster_tile_count_m** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The number of clusters along M dimension of the matched group

- **cluster_tile_count_n** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The number of clusters along N dimension of the matched group

#### Returns

A tuple containing CTA tile indices along M and N dimensions

#### Return type

tuple of ([Int32](../basic_data_types.md#cutlass.Int32), [Int32](../basic_data_types.md#cutlass.Int32))

### \_group_search

**Kind:** method

**Qualified name:** `cutlass.utils.GroupedGemmTileSchedulerHelper._group_search`

**Signature:** `_group_search(linear_idx: _MockObject, problem_shape_mnkl: cutlass.cute.typing.Tensor, init_group_idx: _MockObject, init_tile_count_searched: _MockObject) → GroupedGemmGroupSearchState`

Search which group the linear index belongs to.

#### Parameters

- **linear_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The linear index to be decomposed

- **problem_shape_mnkl** (*cute.Tensor*) -- Tensor containing gemm problem size (M, N, K, L) for all groups

- **init_group_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The group idx to start the search with

- **init_tile_count_searched** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The number of tiles we have searched

#### Returns

The updated search state

#### Return type

[GroupedGemmGroupSearchState](#cutlass.utils.GroupedGemmGroupSearchState)

### \_group_search_and_load_problem_shape

**Kind:** method

**Qualified name:** `cutlass.utils.GroupedGemmTileSchedulerHelper._group_search_and_load_problem_shape`

**Signature:** `_group_search_and_load_problem_shape(linear_idx: _MockObject, problem_shape_mnkl: cutlass.cute.typing.Tensor, start_group_idx: _MockObject, tile_count_searched: _MockObject) → Tuple[_MockObject, cutlass.cute.typing.Tensor]`

Perform group search and load problem shape for the matched group.

#### Parameters

- **linear_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The linear index to be decomposed

- **problem_shape_mnkl** (*cute.Tensor*) -- Tensor containing gemm problem size (M, N, K, L) for all groups

- **start_group_idx** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The group idx to start the search with

- **tile_count_searched** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- The number of tiles we have searched

#### Returns

A tuple containing the final group index and the problem shape tensor

#### Return type

Tuple\[[Int32](../basic_data_types.md#cutlass.Int32), cute.Tensor\]

## cutlass.utils.HardwareInfo

**Kind:** class

**Signature:** `cutlass.utils.HardwareInfo(device_id: int = 0)`

Bases: `object`

device_id: CUDA device ID to get the hardware info.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.utils.HardwareInfo.__init__`

**Signature:** `__init__(device_id: int = 0)`

### get_max_active_clusters

**Kind:** method

**Qualified name:** `cutlass.utils.HardwareInfo.get_max_active_clusters`

**Signature:** `get_max_active_clusters(cluster_size: int, stream: cuda.bindings.driver.CUstream | None = None) → int`

Get the maximum number of active clusters for a given cluster size.

When a stream from a green context is provided, the occupancy calculation will reflect the reduced SM partition of the green context.

#### Parameters

- **cluster_size** (*int*) -- Number of blocks per cluster (must be between 1 and 32)

- **stream** (*driver.CUstream,* *optional*) -- Optional CUDA stream handle. If provided (especially from a green context), the occupancy calculation reflects the stream's SM partition.

#### Returns

Maximum number of active clusters

#### Return type

int

### get_l2_cache_size_in_bytes

**Kind:** method

**Qualified name:** `cutlass.utils.HardwareInfo.get_l2_cache_size_in_bytes`

**Signature:** `get_l2_cache_size_in_bytes() → int`

### get_device_multiprocessor_count

**Kind:** method

**Qualified name:** `cutlass.utils.HardwareInfo.get_device_multiprocessor_count`

**Signature:** `get_device_multiprocessor_count() → int`

### \_checkCudaErrors

**Kind:** method

**Qualified name:** `cutlass.utils.HardwareInfo._checkCudaErrors`

**Signature:** `_checkCudaErrors(result: Any) → Any`

### \_cudaGetErrorEnum

**Kind:** method

**Qualified name:** `cutlass.utils.HardwareInfo._cudaGetErrorEnum`

**Signature:** `_cudaGetErrorEnum(error: Any) → str`

### \_cuda_driver_version_ge

**Kind:** method

**Qualified name:** `cutlass.utils.HardwareInfo._cuda_driver_version_ge`

**Signature:** `_cuda_driver_version_ge(major: int, minor: int) → bool`

### \_cuda_driver_version_lt

**Kind:** method

**Qualified name:** `cutlass.utils.HardwareInfo._cuda_driver_version_lt`

**Signature:** `_cuda_driver_version_lt(major: int, minor: int) → bool`

### \_empty_kernel

**Kind:** method

**Qualified name:** `cutlass.utils.HardwareInfo._empty_kernel`

**Signature:** `_empty_kernel() → None`

### \_host_function

**Kind:** method

**Qualified name:** `cutlass.utils.HardwareInfo._host_function`

**Signature:** `_host_function() → None`

### \_get_device_function

**Kind:** method

**Qualified name:** `cutlass.utils.HardwareInfo._get_device_function`

**Signature:** `_get_device_function() → cuda.bindings.driver.CUfunction`

Get a device function by compiling a dummy kernel using cuteDSL pipeline.

## cutlass.utils.TransformMode

**Kind:** class

**Signature:** `cutlass.utils.TransformMode(value)`

Bases: `Enum`

An enumeration for the possible transform modes of a mixed-input GEMM.

### ConvertOnly

**Kind:** attribute

**Qualified name:** `cutlass.utils.TransformMode.ConvertOnly`

**Signature:** `ConvertOnly = 1`

### ConvertScale

**Kind:** attribute

**Qualified name:** `cutlass.utils.TransformMode.ConvertScale`

**Signature:** `ConvertScale = 2`

## cutlass.utils.scale_tma_partition

**Kind:** function

**Signature:** `cutlass.utils.scale_tma_partition(tCsS: cutlass.cute.typing.Tensor, tCgS: cutlass.cute.typing.Tensor, tma_atom_s: CopyAtom, block_in_cluster_coord_vmnk: cutlass.cute.typing.Coord, scale_cta_layout: cutlass.cute.typing.Layout) → tuple[cutlass.cute.typing.Tensor, cutlass.cute.typing.Tensor]`

Perform TMA partition for scale tensor. This method partitions the global memory and shared memory buffer for the scale tensor for TMA load.

### Parameters

- **tCsS** (*cute.Tensor*) -- Input scale shared memory tensor

- **tCgS** (*cute.Tensor*) -- Input scale global memory tensor

- **tma_atom_s** (*cute.CopyAtom*) -- TMA copy atom for scale tensor

- **block_in_cluster_coord_vmnk** (*cute.Coord*) -- CTA coord in the cluster

- **scale_cta_layout** (*cute.Layout*) -- Layout of CTA from the view of the scale tensor

### Returns

A tuple containing (tSsS, tSgS) where:

- tSsS: Partitioned scale tensor in shared memory

- tSgS: Partitioned scale tensor in global memory

### Return type

tuple\[cute.Tensor, cute.Tensor\]

## cutlass.utils.transform_partition

**Kind:** function

**Signature:** `cutlass.utils.transform_partition(transform_a_source: OperandSource, scale_mode: TransformMode, copy_atom_a_input: CopyAtom, copy_atom_a_transform: CopyAtom, sA_input: cutlass.cute.typing.Tensor, A_transform: cutlass.cute.typing.Tensor, transform_local_tidx: Int32) → tuple[TiledCopy | None, TiledCopy | None, cutlass.cute.typing.Tensor, cutlass.cute.typing.Tensor]`

Partition tensors for transform input and output. This method sets up the copy atoms and partitions the shared/tensor memory for the transformation of tensor A.

### Parameters

- **transform_a_source** (*tcgen05.OperandSource*) -- Where the transformed tensor A is stored (TMEM or SMEM)

- **scale_mode** (*TransformMode*) -- The transform mode (ConvertOnly or ConvertScale)

- **copy_atom_a_input** (*cute.CopyAtom*) -- Copy atom for loading A from shared memory

- **copy_atom_a_transform** (*cute.CopyAtom*) -- Copy atom for storing transformed A

- **sA_input** (*cute.Tensor*) -- Input tensor A in shared memory

- **A_transform** (*cute.Tensor*) -- Transformed tensor A in tensor or shared memory

- **transform_local_tidx** (*cutlass.Int32*) -- Local thread index for transformation warps

### Returns

A tuple containing (src_copy_a, dst_copy_a, tAsA_input, tA_transform) where:

- src_copy_a: Tiled copy for source tensor

- dst_copy_a: Tiled copy for destination tensor

- tAsA_input: Partitioned input tensor A

- tA_transform: Partitioned transformed tensor A

### Return type

tuple\[Optional\[[cute.TiledCopy](cute.md#cutlass.cute.TiledCopy)\], Optional\[[cute.TiledCopy](cute.md#cutlass.cute.TiledCopy)\], cute.Tensor, cute.Tensor\]

## cutlass.utils.scale_partition

**Kind:** function

**Signature:** `cutlass.utils.scale_partition(src_copy_a: TiledCopy, tCsS: cutlass.cute.typing.Tensor, transform_local_tidx: Int32, mma_dtype: type[Numeric]) → tuple[TiledCopy, cutlass.cute.typing.Tensor, cutlass.cute.typing.Tensor, cutlass.cute.typing.Tensor]`

Partition the scale tensor for transformation. This method prepares the copy atom and partitions the shared memory for the scale tensor.

### Parameters

- **src_copy_a** (*cute.TiledCopy*) -- Tiled copy for the source tensor

- **tCsS** (*cute.Tensor*) -- Scale tensor in shared memory

- **transform_local_tidx** (*cutlass.Int32*) -- Local thread index for transformation warps

- **mma_dtype** (*type\[cutlass.Numeric\]*) -- Data type for the MMA operation

### Returns

A tuple containing (smem_thr_copy_S, tSsS_trans, tSrS_copy, tSrS) where:

- smem_thr_copy_S: Tiled copy for the scale tensor

- tSsS_trans: Partitioned scale tensor for transformation

- tSrS_copy: Register fragment for the scale tensor

- tSrS: View of scale tensor used for transformation computation

### Return type

tuple\[[cute.TiledCopy](cute.md#cutlass.cute.TiledCopy), cute.Tensor, cute.Tensor, cute.Tensor\]

## cutlass.utils.get_gmem_layout_scale

**Kind:** function

**Signature:** `cutlass.utils.get_gmem_layout_scale(scale_shape_mkl: tuple[int, int, int], scale_granularity_m: int, scale_granularity_k: int, scale_major_mode: OperandMajorMode) → cutlass.cute.typing.Layout`

Get the layout of the scale tensor in global memory.

### Parameters

- **scale_shape_mkl** (*tuple\[int, int, int\]*) -- The shape of the scale tensor (M, K, L).

### Returns

The layout of the scale tensor in global memory.

### Return type

cute.Layout

## cutlass.utils.get_smem_layout_scale

**Kind:** function

**Signature:** `cutlass.utils.get_smem_layout_scale(mma_tiler: tuple[int, int, int], use_2cta_instrs: bool, scale_granularity_m: int, scale_granularity_k: int, scale_major_mode: OperandMajorMode, a_scale_dtype: type[Numeric], num_scale_load2trans_stage: int) → tuple[tuple[int, int], cutlass.cute.typing.ComposedLayout, cutlass.cute.typing.ComposedLayout]`

Get the layout of the scale tensor in shared memory.

### Returns

A tuple containing (scale_tile_shape, smem_layout_scale_per_stage, smem_layout_scale) where:

- scale_tile_shape: The tile shape

- smem_layout_scale_per_stage: Shared memory layout for scale tensor per stage

- smem_layout_scale: Shared memory layout for scale tensor

### Return type

tuple\[tuple\[int, int\], cute.ComposedLayout, cute.ComposedLayout\]

## cutlass.utils.compute_smem_layout

**Kind:** function

**Signature:** `cutlass.utils.compute_smem_layout(tiled_mma: TiledMma, mma_tiler_mnk: tuple[int, int, int], a_dtype: type[Numeric], b_dtype: type[Numeric], load2trans_stage_count: int, trans2mma_stage_count: int) → tuple[cutlass.cute.typing.ComposedLayout, cutlass.cute.typing.ComposedLayout, cutlass.cute.typing.ComposedLayout]`

Compute shared memory layouts for tensor A, transformed A and tensor B.

### Parameters

- **tiled_mma** (*cute.TiledMma*) -- The tiled MMA object defining the core computation.

- **mma_tiler_mnk** (*tuple\[int, int, int\]*) -- The shape (M, N, K) of the MMA tiler.

- **a_dtype** (*type\[cutlass.Numeric\]*) -- Data type of operand A.

- **b_dtype** (*type\[cutlass.Numeric\]*) -- Data type of operand B.

- **load2trans_stage_count** (*int*) -- Number of stages for load-to-transform pipeline.

- **trans2mma_stage_count** (*int*) -- Number of stages for transform-to-MMA pipeline.

### Returns

A tuple containing (smem_layout_a, smem_layout_a_transform, smem_layout_b) where:

- smem_layout_a: Shared memory layout for tensor A

- smem_layout_a_transform: Shared memory layout for transformed tensor A

- smem_layout_b: Shared memory layout for tensor B

### Return type

tuple\[cute.ComposedLayout, cute.ComposedLayout, cute.ComposedLayout\]

## cutlass.utils.get_transform_a_source

**Kind:** function

**Signature:** `cutlass.utils.get_transform_a_source(a_major_mode: OperandMajorMode) → OperandSource`

Determine the operand source for transformed A tensor based on the operand major mode.

## cutlass.utils.get_tma_atom_kind

**Kind:** function

**Signature:** `cutlass.utils.get_tma_atom_kind(mcast: Boolean, use_2cta_instrs: bool, is_b: bool) → CopyBulkTensorTileG2SMulticastOp | CopyBulkTensorTileG2SOp`

Get the TMA atom kind based on 1) whether it's a multicast operation, 2) whether 2CTA tcgen05.mma instruction is enabled, and 3) whether it's a B tensor

## cutlass.utils.get_copy_atom_a_transform

**Kind:** function

**Signature:** `cutlass.utils.get_copy_atom_a_transform(mma_dtype: type[Numeric], use_2cta_instrs: bool, transform_a_source: OperandSource, a_smem_shape: cutlass.cute.typing.Shape, a_dtype: type[Numeric]) → CopyAtom`

Determine the copy atom for transformed A tensor based on the operand source and tile size.

## cutlass.utils.is_valid_scale_granularity

**Kind:** function

**Signature:** `cutlass.utils.is_valid_scale_granularity(scale_granularity_m: int, scale_granularity_k: int, a_dtype: type[Numeric], k: int, mma_tiler_k: int) → bool`

Check if the scale granularity settings are valid for the given data type and problem size.

## cutlass.utils.get_divisibility

**Kind:** function

**Signature:** `cutlass.utils.get_divisibility(contiguous_dim_size: int, upper_bound: int = 128) → int`

Calculate the largest power of 2 divisibility factor for memory alignment.

## cutlass.utils.cluster_shape_to_tma_atom_A

**Kind:** function

**Signature:** `cutlass.utils.cluster_shape_to_tma_atom_A(cluster_shape_mnk: cutlass.cute.typing.Shape, atom_thr_id: cutlass.cute.typing.Layout) → CopyBulkTensorTileG2SMulticastOp | CopyBulkTensorTileG2SOp`

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

## cutlass.utils.cluster_shape_to_tma_atom_B

**Kind:** function

**Signature:** `cutlass.utils.cluster_shape_to_tma_atom_B(cluster_shape_mnk: cutlass.cute.typing.Shape, atom_thr_id: cutlass.cute.typing.Layout) → CopyBulkTensorTileG2SMulticastOp | CopyBulkTensorTileG2SOp`

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

## cutlass.utils.cluster_shape_to_tma_atom_SFB

**Kind:** function

**Signature:** `cutlass.utils.cluster_shape_to_tma_atom_SFB(cluster_shape_mnk: cutlass.cute.typing.Shape, atom_thr_id: cutlass.cute.typing.Layout) → CopyBulkTensorTileG2SMulticastOp | CopyBulkTensorTileG2SOp`

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

## cutlass.utils.compute_epilogue_tile_shape

**Kind:** function

**Signature:** `cutlass.utils.compute_epilogue_tile_shape(cta_tile_shape: cutlass.cute.typing.Shape, use_2cta_instrs: bool, layout_d: LayoutEnum, elem_ty_d: Type[_MockObject], *, layout_c: LayoutEnum | None = None, elem_ty_c: Type[_MockObject] | None = None, tmem_warp_shape_mn: Tuple[int, int] | None = None) → cutlass.cute.typing.Tile`

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

## cutlass.utils.get_permutation_mnk

**Kind:** function

**Signature:** `cutlass.utils.get_permutation_mnk(tile_shape_mnk: cutlass.cute.typing.Shape, sf_vec_size: int, use_mxf8f6f4: bool) → Tuple[int, int, int]`

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

## cutlass.utils.get_smem_layout_atom_ab

**Kind:** function

**Signature:** `cutlass.utils.get_smem_layout_atom_ab(major_mode: OperandMajorMode, element_type: Type[_MockObject], smem_shape_mn_k: cutlass.cute.typing.Tile, sparsity: int = 1) → SmemLayoutAtomKind`

Simple heuristics to select the optimal SMEM layout atom based on the majorness, the data type, and the major mode size.

### Parameters

- **major_mode** ([*cutlass.cute.nvgpu.OperandMajorMode*](cute_nvgpu_common.md#cutlass.cute.nvgpu.OperandMajorMode)) -- The major mode for the SMEM tensor is K major.

- **element_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for the SMEM tensor.

- **smem_shape_mn_k** (*cute.Tile*) -- The shape of the SMEM tensor.

- **sparsity** (*int*) -- The sparsity factor (1 for dense, 2 for 2:4 sparse, etc.)

### Returns

The SMEM layout atom kind

### Return type

[cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind](cute_nvgpu_tcgen05.md#cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind)

## cutlass.utils.get_smem_layout_atom_epi

**Kind:** function

**Signature:** `cutlass.utils.get_smem_layout_atom_epi(layout: LayoutEnum, element_type: Type[_MockObject], epi_tile: cutlass.cute.typing.Tile) → SmemLayoutAtomKind`

Simple heuristics to select the optimal SMEM layout atom for epilog tensors.

### Parameters

- **layout** (*LayoutEnum*) -- The layout enum for the SMEM tensor.

- **element_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for the SMEM tensor.

- **epi_tile** (*cute.Tile*) -- The epilogue tile shape.

### Returns

The SMEM layout atom kind

### Return type

[cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind](cute_nvgpu_tcgen05.md#cutlass.cute.nvgpu.tcgen05.SmemLayoutAtomKind)

## cutlass.utils.get_smem_store_op

**Kind:** function

**Signature:** `cutlass.utils.get_smem_store_op(layout_d: LayoutEnum, elem_ty_d: Type[_MockObject], elem_ty_acc: Type[_MockObject], tiled_tmem_load: TiledCopy) → CopyAtom`

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

## cutlass.utils.get_tmem_load_op

**Kind:** function

**Signature:** `cutlass.utils.get_tmem_load_op(cta_tile_shape: cutlass.cute.typing.Shape, layout_d: LayoutEnum, elem_ty_d: Type[_MockObject], elem_ty_acc: Type[_MockObject], epi_tile: cutlass.cute.typing.Tile, use_2cta_instrs: bool, *, tmem_warp_shape_mn: Tuple[int, int] | None = None) → CopyAtom`

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

## cutlass.utils.make_smem_layout

**Kind:** function

**Signature:** `cutlass.utils.make_smem_layout(leading_mode: OperandMajorMode, smem_tile_shape: cutlass.cute.typing.Tile, a_dtype: Type[_MockObject], num_stages: int) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

Construct a staged SMEM layout for an operand given its major mode and tile shape.

This helper:

1.  Selects a SMEM layout atom using simple heuristics based on the operand's major mode, element type, and the size of the major dimension in `smem_tile_shape`.

2.  Tiles the atom to `smem_tile_shape` and appends a staging dimension of length `num_stages`.

3.  Orders the `(M, N, stage)` axes so the major dimension is contiguous, then coalesces.

### Parameters

- **leading_mode** ([*cutlass.cute.nvgpu.OperandMajorMode*](cute_nvgpu_common.md#cutlass.cute.nvgpu.OperandMajorMode)) -- Operand major mode (`MN` or `K`) of the staged operand.

- **smem_tile_shape** (*cute.Tile*) -- 2D SMEM tile shape to stage (before the staging dimension is appended).

- **a_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Element type of the staged operand.

- **num_stages** (*int*) -- Number of pipeline stages (depth of the staging dimension).

### Returns

Staged SMEM layout for the operand.

### Return type

Union\[cute.Layout, cute.ComposedLayout\]

## cutlass.utils.make_smem_layout_a

**Kind:** function

**Signature:** `cutlass.utils.make_smem_layout_a(tiled_mma: TiledMma, mma_tiler_mnk: cutlass.cute.typing.Tile, a_dtype: Type[_MockObject], num_stages: int, *, is_k_major: bool | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

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

## cutlass.utils.make_smem_layout_b

**Kind:** function

**Signature:** `cutlass.utils.make_smem_layout_b(tiled_mma: TiledMma, mma_tiler_mnk: cutlass.cute.typing.Tile, b_dtype: Type[_MockObject], num_stages: int, *, is_k_major: bool | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

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

## cutlass.utils.make_smem_layout_epi

**Kind:** function

**Signature:** `cutlass.utils.make_smem_layout_epi(epi_dtype: Type[_MockObject], epi_layout: LayoutEnum, epi_tile: cutlass.cute.typing.Tile, epi_stage: int) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

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

## cutlass.utils.make_trivial_tiled_mma

**Kind:** function

**Signature:** `cutlass.utils.make_trivial_tiled_mma(*args: Any, **kwargs: Any) → TiledMma`

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

## cutlass.utils.make_blockscaled_trivial_tiled_mma

**Kind:** function

**Signature:** `cutlass.utils.make_blockscaled_trivial_tiled_mma(*args: Any, **kwargs: Any) → TiledMma`

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

## cutlass.utils.sm90_get_smem_layout_atom

**Kind:** function

**Signature:** `cutlass.utils.sm90_get_smem_layout_atom(layout: LayoutEnum, element_type: Type[_MockObject], major_mode_size: int) → Any`

Select the optimal shared memory layout atom based on parameters.

### Parameters

- **layout** (*LayoutEnum*) -- Layout enum of the tensor

- **element_type** (*type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of the elements

- **major_mode_size** (*int*) -- Size of the major mode dimension

### Returns

Selected shared memory layout atom kind

### Return type

[cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind](cute_nvgpu_warpgroup.md#cutlass.cute.nvgpu.warpgroup.SmemLayoutAtomKind)

## cutlass.utils.sm90_make_trivial_tiled_mma

**Kind:** function

**Signature:** `cutlass.utils.sm90_make_trivial_tiled_mma(a_dtype: ~typing.Type[~sphinx.ext.autodoc.mock._MockObject], b_dtype: ~typing.Type[~sphinx.ext.autodoc.mock._MockObject], a_leading_mode: ~cutlass.cute.nvgpu.common.OperandMajorMode, b_leading_mode: ~cutlass.cute.nvgpu.common.OperandMajorMode, acc_dtype: ~typing.Type[~sphinx.ext.autodoc.mock._MockObject], atom_layout_mnk: ~typing.Tuple[int, int, int], tiler_mn: ~typing.Tuple[int, int], a_source: ~cutlass.cute.nvgpu.warpgroup.mma.OperandSource = <OperandSource.RMEM>) → TiledMma`

Make a tiled MMA atom with given data type, leading dimension, cta group and mma tile shape. By default, the MMA atom is created with SMEM operand source for A.

### Parameters

- **a_dtype** (*type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of operand A.

- **b_dtype** (*type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of operand B.

- **a_leading_mode** ([*cutlass.cute.nvgpu.OperandMajorMode*](cute_nvgpu_common.md#cutlass.cute.nvgpu.OperandMajorMode)) -- Leading dimension of operand A (1 for K, 0 for M/N).

- **b_leading_mode** ([*cutlass.cute.nvgpu.OperandMajorMode*](cute_nvgpu_common.md#cutlass.cute.nvgpu.OperandMajorMode)) -- Leading dimension of operand B (1 for K, 0 for M/N).

- **acc_dtype** (*type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of the accumulator.

- **atom_layout_mnk** (*Tuple\[int,* *int,* *int\]*) -- A integer tuple describing the tiling of Atom across threads.

- **tiler_mn** (*Tuple\[int,* *int\]*) -- The shape (M, N) of the cta tiler.

### Returns

A tiled MMA atom.

### Return type

[cute.TiledMma](cute.md#cutlass.cute.TiledMma)

### Raises

**TypeError** -- If the data type is not supported.

## cutlass.utils.block_copy

**Kind:** function

**Signature:** `cutlass.utils.block_copy(tiled_copy: ~cutlass.cute.atom.TiledCopy, src: cutlass.cute.typing.Tensor, dst: cutlass.cute.typing.Tensor, *, **kwargs: ~typing.Any) → None`

Performs a block-level copy operation.

This function adds an abstraction layer over the cute.copy usage model by allowing operands with layouts shaped like tiles to be passed directly. This removes the need to manually partition. The API is designed to support multiple copy kinds; currently TMA-based copies and S2T (SMEM to TMEM) copies are supported.

**TMA copy requirements**:

When using TMA-based tiled copies, the `src` and `dst` tensors must have their first mode representing the TMATile, i.e. tensors shaped as `(TMATile, Rest...)`. For a rank-2 tensor with logical layout (e.g., `(TILE_M, TILE_N)`), call `group_modes(tensor, 0, 2)` before passing it to this function.

**TMA multicast support**:

For TMA-based copies that enable compiler-driven multicast in a 2D cluster, pass the `tma_multicast` argument as a dict with the following keys:

- `cluster_shape`: a tuple of 2 integers `(cluster_m, cluster_n)` representing the **2D cluster shape**.

- `multicast_dim`: either `"M"` or `"N"` indicating which cluster dimension the multicast happens along.

- `use_2cta_mma_inst` (optional): a `bool` indicating whether to use 2CTA MMA instructions when the loaded data is consumed by MMA. Defaults to `False` when omitted.

**S2T (SMEM to TMEM) copy**:

When using S2T copy operations (e.g., `tcgen05.Cp4x32x128bOp`), the function automatically handles the filtering, partitioning, and SMEM descriptor creation. Pass a copy atom created with `cute.make_copy_atom(tcgen05.Cp*Op(...), dtype)` along with source (SMEM) and destination (TMEM) tensors.

Examples:

    # 1) TMA load without compiler-driven multicast
    #    Note: group_modes is called to make the first mode TMATile
    block_copy(tma_atom_a, group_modes(tCgA_, 0, 2), group_modes(tCsA_, 0, 2),
               tma_bar_ptr=tma_bar_ptr)

    # 2) TMA load with compiler-driven multicast along M in a (4,2) cluster
    block_copy(
        tma_atom_a,
        group_modes(tCgA_, 0, 2),
        group_modes(tCsA_, 0, 2),
        tma_multicast={
            "cluster_shape": (4, 2),
            "multicast_dim": "M",
            "use_2cta_mma_inst": True,
        },
        tma_bar_ptr=tma_bar_ptr,
    )

    # 3) TMA store
    #    Note that `tma_bar_ptr` and CTA params (`cta_coord` and `cta_layout`)
    #    are not needed for TMA store
    block_copy(tma_atom_c, group_modes(tCsC_, 0, 2), group_modes(tCgC_, 0, 2))

    # 4) S2T copy (SMEM to TMEM)
    copy_atom_s2t = cute.make_copy_atom(
        tcgen05.Cp4x32x128bOp(tcgen05.CtaGroup.ONE), sf_dtype
    )
    block_copy(copy_atom_s2t, tCsSF, tCtSF)

### Parameters

- **tiled_copy** ([*TiledCopy*](cute.md#cutlass.cute.TiledCopy)) -- The tiled_copy or copy_atom of the current copy operation.

- **src** (*Tensor*) -- The source tensor.

- **dst** (*Tensor*) -- The destination tensor.

- **tma_multicast** (*dict,* *optional*) -- Optional dict for TMA multicast configuration with keys `cluster_shape`, `multicast_dim`, and optionally `use_2cta_mma_inst`.

## cutlass.utils.ClcDynamicPersistentTileSchedulerParams

**Kind:** class

**Signature:** `cutlass.utils.ClcDynamicPersistentTileSchedulerParams(**kwargs)`

Bases: `MixedClusterParamsMixin`

A class to represent parameters for a dynamic persistent tile scheduler.

This class is designed to manage and compute the layout of clusters and tiles in a batched gemm problem.

### Variables

**cluster_shape_mn** -- Shape of the cluster in (m, n) dimensions (K dimension cta count must be 1).

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileSchedulerParams.__init__`

**Signature:** `__init__() → None`

Initializes the ClcDynamicPersistentTileSchedulerParams with the given parameters.

#### Parameters

- **problem_shape_ntile_mnl** (*cute.Shape*) -- The shape of the problem in terms of number of CTA (Cooperative Thread Array) in (m, n, l) dimensions.

- **cluster_shape_mnk** (*cute.Shape*) -- The shape of the cluster in (m, n) dimensions.

- **swizzle_size** (*int*) -- Swizzling size in the unit of cluster. 1 means no swizzle

- **raster_along_m** (*bool*) -- Rasterization order of clusters. Only used when swizzle_size \> 1. True means along M, false means along N.

- **fallback_cluster_shape_mnk** (*Optional\[cute.Shape\]*) -- Optional. When provided and different from cluster_shape_mnk, the kernel runs in mixed-cluster mode.

#### Raises

**ValueError** -- If cluster_shape_k is not 1.

### \_extract_primary_mlir_values

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileSchedulerParams._extract_primary_mlir_values`

**Signature:** `_extract_primary_mlir_values() → list[ir.Value]`

### \_primary_values_count

**Kind:** property

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileSchedulerParams._primary_values_count`

**Signature:** `_primary_values_count: int`

### \_new_primary_from_mlir_values

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileSchedulerParams._new_primary_from_mlir_values`

**Signature:** `_new_primary_from_mlir_values(values: list[ir.Value]) → ClcDynamicPersistentTileSchedulerParams`

### get_grid_shape

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileSchedulerParams.get_grid_shape`

**Signature:** `get_grid_shape() → Tuple[_MockObject, _MockObject, _MockObject]`

Computes the grid shape based on the problem shape and cluster shape.

#### Returns

the grid is the CTA numbers that has aligned with cluster shape.

## cutlass.utils.ClcDynamicPersistentTileScheduler

**Kind:** class

**Signature:** `cutlass.utils.ClcDynamicPersistentTileScheduler(**kwargs)`

Bases: `object`

A scheduler for dynamic persistent tile execution in CUTLASS/CuTe kernels.

### Variables

- **params** -- Tile schedule related params, including cluster shape.

- **cta_id_in_cluster** -- ID of the CTA within its cluster

- **\_num_tiles_executed** -- Counter for executed tiles

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileScheduler.__init__`

**Signature:** `__init__(params: ClcDynamicPersistentTileSchedulerParams, cta_id_in_cluster: cutlass.cute.typing.Coord, num_tiles_executed: _MockObject, clc_response_ptr: cutlass.cute.typing.Pointer, block_idx: Tuple[_MockObject, _MockObject, _MockObject], insert_fence: bool = True)`

Initializes the ClcDynamicPersistentTileScheduler with the given parameters.

#### Parameters

- **params** ([*ClcDynamicPersistentTileSchedulerParams*](#cutlass.utils.ClcDynamicPersistentTileSchedulerParams)) -- Tile schedule related params, including cluster shape.

- **cta_id_in_cluster** (*cute.Coord*) -- ID of the CTA within its cluster.

- **num_tiles_executed** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Counter for executed tiles.

- **clc_response_ptr** (*cute.Pointer*) -- Pointer of the clc rsponse.

- **block_idx** (*Tuple\[*[*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*\]*) -- The block index.

- **insert_fence** (*bool*) -- Whether to insert a fence to ensure generic-async proxy order. CLC issue is in async proxy while loading the response from shared memory is in generic proxy. A cross-proxy fence is needed to ensure producer's next issue won't race with consumer's current loading. Therefore the scheduler inserts a fence by default after loading the response. Developers may insert the fence in pipeline acquire/release functions. In that case, the fence here can be omitted.

### create

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileScheduler.create`

**Signature:** `static create(params: ClcDynamicPersistentTileSchedulerParams, block_idx: Tuple[_MockObject, _MockObject, _MockObject], grid_dim: Tuple[_MockObject, _MockObject, _MockObject], clc_response_ptr: cutlass.cute.typing.Pointer, insert_fence: bool = True) → ClcDynamicPersistentTileScheduler`

Initialize the dynamic persistent tile scheduler.

#### Parameters

- **params** ([*ClcDynamicPersistentTileSchedulerParams*](#cutlass.utils.ClcDynamicPersistentTileSchedulerParams)) -- Parameters for the persistent tile scheduler.

- **block_idx** (*Tuple\[*[*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*\]*) -- The 3d block index in the format (bidx, bidy, bidz).

- **grid_dim** (*Tuple\[*[*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*,* [*Integer*](../basic_data_types.md#cutlass.Integer)*\]*) -- The 3d grid dimensions for kernel launch.

#### Returns

A ClcDynamicPersistentTileScheduler object.

#### Return type

[ClcDynamicPersistentTileScheduler](#cutlass.utils.ClcDynamicPersistentTileScheduler)

### get_grid_shape

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileScheduler.get_grid_shape`

**Signature:** `get_grid_shape() → Tuple[_MockObject, _MockObject, _MockObject]`

Calculates the grid shape to be launched on GPU using problem shape, threadblock shape, and active cluster size.

#### Parameters

**params** ([*ClcDynamicPersistentTileSchedulerParams*](#cutlass.utils.ClcDynamicPersistentTileSchedulerParams)) -- Parameters for grid shape calculation.

#### Returns

The calculated 3d grid shape.

#### Return type

Tuple\[[Integer](../basic_data_types.md#cutlass.Integer), [Integer](../basic_data_types.md#cutlass.Integer), [Integer](../basic_data_types.md#cutlass.Integer)\]

### \_swizzle_and_rasterize

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileScheduler._swizzle_and_rasterize`

**Signature:** `_swizzle_and_rasterize(x_idx: _MockObject, y_idx: _MockObject, z_idx: _MockObject) → Tuple[_MockObject, _MockObject, _MockObject]`

Swizzle and rasterize the given coordinates for leader CTA of the cluster. x_idx, y_idx, and z_idx must be divisible by cluster shape x, y, and z respectively. They should not be offset by the ID of the CTA in the cluster.

### work_tile_info_from_clc_response

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileScheduler.work_tile_info_from_clc_response`

**Signature:** `work_tile_info_from_clc_response(result_addr: cutlass.cute.typing.Pointer) → WorkTileInfo`

Simulates parsing CLC response data in Python. result_addr: 16-byte response data (simulating shared memory access)

### get_current_work

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileScheduler.get_current_work`

**Signature:** `get_current_work() → WorkTileInfo`

### initial_work_tile_info

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileScheduler.initial_work_tile_info`

**Signature:** `initial_work_tile_info() → WorkTileInfo`

### advance_to_next_work

**Kind:** method

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileScheduler.advance_to_next_work`

**Signature:** `advance_to_next_work(mbarrier_addr: cutlass.cute.typing.Pointer) → None`

### num_tiles_executed

**Kind:** property

**Qualified name:** `cutlass.utils.ClcDynamicPersistentTileScheduler.num_tiles_executed`

**Signature:** `num_tiles_executed: _MockObject`

## cutlass.utils.print_latex

**Kind:** function

**Signature:** `cutlass.utils.print_latex(x: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, *, color: ~typing.Callable = <function tikz_color_bwx8>, render_func: ~typing.Callable[[str], None] | None = None) → None`

Prints a layout.

### Parameters

- **x** (*Union\[Layout,* *ComposedLayout\]*) -- A layout

- **color** (*Callable*) -- A function that returns TiKZ colors

- **render_func** (*Callable\[\[str\],* *None\]* *\|* *None*) -- Optional callback fed the `{tikzpicture}` body (without the standalone-document wrapper) in a single call, instead of printing a full LaTeX document to stdout.

## cutlass.utils.print_latex_tv

**Kind:** function

**Signature:** `cutlass.utils.print_latex_tv(layout_tv: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, tile_mn: cutlass.cute.typing.IntTuple | cutlass.cute.typing.Layout, *, color: ~typing.Callable = <function tikz_color_tv>, palette: str | ~typing.Callable | None = None, title: str | None = None, axis_labels: bool = False, render_func: ~typing.Callable[[str], None] | None = None) → None`

Prints a tv layout for a tile M N. Everything must be static.

### Parameters

- **layout_tv** (*Union\[Layout,* *ComposedLayout\]*) -- A static thread value layout

- **tile_mn** (*Union\[IntTuple,* *Layout\]*) -- A static M N tile

- **color** (*Callable*) -- A function `color(tid, vid) -> str` returning a TikZ fill color for the cell owned by thread `tid` value `vid`. Used when `palette` is `None`; ignored otherwise.

- **palette** (*Optional\[Union\[str,* *Callable\]\]*) -- Optional richer cell coloring that supersedes `color`. Either the name of a built-in palette (a key of `PALETTES`: the color `"pastel"`, `"rainbow"`, `"rainbow_dual"` or the monochrome `"white"`, `"bw"`, `"bw_dual"`) or a factory `palette(num_tid, num_vid) -> cell` where `cell(tid, vid)` returns either a TikZ fill string (one fill spanning the cell) or a list of ``` Band``s `` (stacked horizontal fills).``  ``The factory form lets a palette capture the thread / value counts -- needed to spread hues evenly over the wheel -- without widening the per-cell  (tid, `` vid)` contract.

- **title** (*Optional\[str\]*) -- Optional title drawn above the figure, one line per `\n`-separated segment (e.g. the operator / function / tensor that produced this layout). LaTeX specials are escaped.

- **axis_labels** (*bool*) -- When `True`, annotate the M (row, downward) and N (column, rightward) axis directions. The picture's coordinate basis runs M down and N right; these labels make that orientation explicit so a reader can tell whether a thread's values are contiguous along the memory-major axis.

- **render_func** (*Callable\[\[str\],* *None\]* *\|* *None*) -- Optional callback fed the `{tikzpicture}` body (without the standalone-document wrapper) in a single call, instead of printing a full LaTeX document to stdout.

## cutlass.utils.is_fp8_dtype

**Kind:** function

**Signature:** `cutlass.utils.is_fp8_dtype(dtype: Type[cutlass.cute.typing.Numeric]) → bool`

Check if dtype is a float8 type that doesn't support dlpack.

### Parameters

- **dtype** (*Type\[cutlass.Numeric\]*) -- The cutlass numeric type to check

### Returns

True if the dtype is Float8E5M2 or Float8E4M3FN, False otherwise

## cutlass.utils.create_cute_tensor_for_fp8

**Kind:** function

**Signature:** `cutlass.utils.create_cute_tensor_for_fp8(storage_tensor: Any, dtype: Type[cutlass.cute.typing.Numeric], leading_dim: int, source_f32_tensor: Any | None = None, assumed_align: int = 16, mark_dynamic_layout: bool = True) → cutlass.cute.typing.Tensor`

Create cute tensor, handling float8 types that don't support dlpack.

For float8 types, the storage_tensor should use byte storage (for DLPack compatibility). The source_f32_tensor provides the actual float32 values to convert to fp8.

### Parameters

- **storage_tensor** -- Tensor for DLPack (byte storage for fp8, otherwise the actual dtype)

- **dtype** -- Target cutlass dtype

- **leading_dim** -- Leading dimension for dynamic layout

- **source_f32_tensor** -- Float32 source data for fp8 conversion (required for fp8)

- **assumed_align** -- Assumed alignment for the DLPack tensor

- **mark_dynamic_layout** -- Whether to mark the resulting tensor layout dynamic

### Returns

A cute tensor with the appropriate dtype and layout
