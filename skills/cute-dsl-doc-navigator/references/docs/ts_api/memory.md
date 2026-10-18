---
title: "task_scheduling.memory"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/ts_api/memory.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# task_scheduling.memory {#module-cutlass.experimental.task_scheduling.memory}

Unified memory allocators for the TS framework.

Provides declarative layout mechanisms for SMEM and TMEM: resources declare requirements as allocation objects, and allocators compute a flat layout with optional phase-based aliasing.

Typical usage:

    allocator = SmemAllocator()

    # Resources expose their allocations
    for alloc in resource.get_smem_requirements():
        allocator.add(alloc)

    # Optional: alias allocations whose lifetimes don't overlap.
    # Each inner list is a "phase" — allocations within a phase coexist
    # and get sequential offsets.  Different phases reuse the same
    # physical region.
    allocator.add_alias_group([
        [smem_ab._alloc_a, smem_ab._alloc_b],   # phase 1: coexist
        [epilogue._alloc_scratch],                # phase 2: reuses
    ])

    allocator.compute_layout()   # pure Python — sets .offset on each alloc
    # allocator.allocate() called later at DSL-trace time to emit the
    # cutlass.Array(..., space=cutlass.AddressSpace.smem) op.

## Classes

SmemAllocation / TmemAllocation

:   Describe named SMEM (byte) or TMEM (column) regions. After `compute_layout()`, the `.offset` field contains the offset from the base of the unified block.

    `SmemAllocation` accepts an optional `dtype` and `count`. When `dtype` is provided without explicit `size_bytes`, the size is auto-computed as `count * dtype.width // 8`. The stored dtype enables `SmemAllocator.get()` to return a typed array without re-specifying the element type.

ResourceContext

:   Frozen context object embedded in `StageInfo`. Carries `smem_base` and `tmem_ptr_i32`.

\_LayoutAllocator

:   Base class with common declaration and layout logic.

SmemAllocator / TmemAllocator

:   Concrete allocators for SMEM bytes (with alignment) and TMEM columns (no alignment). `SmemAllocator` can also emit a single `cutlass.Array(..., space=cutlass.AddressSpace.smem)` call at DSL-trace time.

*class* cutlass.experimental.task_scheduling.memory.SmemAllocation(

*name: str*,

*size_bytes: int = 0*,

*alignment: int = 128*,

*dtype: Any \| None = None*,

*count: int = 1*,

)

Bases: `object`

Declares a named SMEM region.

name

:   Human-readable label for debugging.

    Type:

    :   str

<!-- -->

size_bytes

:   Required size in bytes. When 0 and `dtype` is provided, auto-computed as `count * dtype.width // 8`.

    Type:

    :   int

<!-- -->

alignment

:   Required alignment in bytes (default 128 for TMA).

    Type:

    :   int

<!-- -->

dtype

:   Optional element type. When set, enables `SmemAllocator.get()` to return a typed `cutlass.Array` without re-specifying the type.

    Type:

    :   Any

<!-- -->

count

:   Number of elements (default 1). Used with `dtype` to auto-compute `size_bytes` and as the shape for `get()`.

    Type:

    :   int

<!-- -->

offset

:   Byte offset from SMEM base, set by `SmemAllocator.compute_layout()`.

    Type:

    :   int

<!-- -->

name*: str*

:

<!-- -->

size_bytes*: int = 0*

:

<!-- -->

alignment*: int = 128*

:

<!-- -->

dtype*: Any = None*

:

<!-- -->

count*: int = 1*

:

<!-- -->

offset*: int = 0*

:

\_\_init\_\_(

*name: str*,

*size_bytes: int = 0*,

*alignment: int = 128*,

*dtype: Any \| None = None*,

*count: int = 1*,

) → None

*class* cutlass.experimental.task_scheduling.memory.TmemAllocation(*name: str*, *num_columns: int*)

:   Bases: `object`

    Declares a named TMEM column region.

    name

    :   Human-readable label for debugging.

        Type:

        :   str

    num_columns

    :   Number of TMEM columns required.

        Type:

        :   int

    offset

    :   Column offset from TMEM base, set by `TmemAllocator.compute_layout()`.

        Type:

        :   int

    name*: str*

    :

    num_columns*: int*

    :

    offset*: int = 0*

    :

    \_\_init\_\_(*name: str*, *num_columns: int*) → None

    :

*class* cutlass.experimental.task_scheduling.memory.ResourceContext(

*smem_base: Any \| None = None*,

*tmem_ptr_i32: Any \| None = None*,

)

Bases: `object`

Read-only context embedded in `StageInfo`.

smem_base

:   `cutlass.Array` base pointer for the unified SMEM allocation. `None` when no `SmemAllocator` is in use.

    Type:

    :   Any

<!-- -->

tmem_ptr_i32

:   Shared-memory `cutlass.Array[Int32, 1]` written by `nvvm.tcgen05_alloc`. Resources use it to derive their TMEM addresses via `tmem_ptr_i32.load()` + offset. `None` when TMEM is not in use.

    Type:

    :   Any

<!-- -->

smem_base*: Any = None*

:

<!-- -->

tmem_ptr_i32*: Any = None*

:

\_\_init\_\_(

*smem_base: Any \| None = None*,

*tmem_ptr_i32: Any \| None = None*,

) → None

*class* cutlass.experimental.task_scheduling.memory.SmemAllocator

Bases: `_LayoutAllocator`

SMEM layout allocator with alignment-aware bump allocation.

Collects `SmemAllocation` objects, computes a flat byte layout (with phase-based alias groups), and emits a single `cutlass.Array(..., space=cutlass.AddressSpace.smem)` at DSL-trace time.

Also tracks barrier SMEM consumed by pipeline resources (computed from `PipelineConfig.num_stages` when `barrier_ptr` is not pre-allocated). See `barrier_smem_bytes`.

Example --- epilogue scratch reuses A+B SMEM:

    allocator.add_alias_group([
        [smem_ab._alloc_a, smem_ab._alloc_b],  # phase 1: coexist
        [epilogue._alloc_scratch],               # phase 2: reuses
    ])

\_\_init\_\_() → None

:

<!-- -->

add_resource(*resource: Any*) → None

:   Register data allocations and accumulate barrier SMEM.

    Parameters:

    :   **resource** (*Any*) -- Resource exposing `get_smem_requirements()` and, optionally, a `pipeline_config` whose barrier storage should be placed in the unified SMEM block.

    Notes

    Resources that belong to a `PipelineGroup` (i.e. `resource.pipeline_group is not None`) have their barrier SMEM managed by the group, so individual barrier accounting is skipped. Call `add_pipeline_group()` to register the group's barrier requirements instead.

<!-- -->

add_pipeline_group(*group: Any*) → None

:   Register a `PipelineGroup`'s barrier SMEM requirements.

    Individual members' data SMEM should still be registered via `add_resource()`. Their per-resource barrier accounting is automatically skipped (see `add_resource`).

add_tmem_ptr(

*alloc: [SmemAllocation](#cutlass.experimental.task_scheduling.memory.SmemAllocation)*,

) → [SmemAllocation](#cutlass.experimental.task_scheduling.memory.SmemAllocation)

Register the TMEM-pointer infrastructure slot.

Parameters:

:   **alloc** ([*SmemAllocation*](#cutlass.experimental.task_scheduling.memory.SmemAllocation)) -- SMEM allocation that stores the 32-bit TMEM base pointer.

Returns:

:   The same allocation object after registration.

Return type:

:   [SmemAllocation](#cutlass.experimental.task_scheduling.memory.SmemAllocation)

Notes

The allocation is added to the layout like any other, but is also remembered so that `TaskManager.setup_resources_and_tasks` can automatically derive the typed `cutlass.Array` pointer and populate `ResourceContext.tmem_ptr_i32` without caller intervention.

*property* tmem_ptr_alloc*: [SmemAllocation](#cutlass.experimental.task_scheduling.memory.SmemAllocation) \| None*

:   The TMEM-pointer allocation, or `None` if not registered.

<!-- -->

*property* barrier_smem_bytes*: int*

:   Total SMEM bytes used for pipeline mbarrier storage.

    Counts `num_stages × 2 × 8` for every resource whose `PipelineConfig.barrier_ptr` is `None` (i.e. barrier storage will be allocated by `create_pipeline`).

<!-- -->

*property* smem_base*: Any*

:   Unified SMEM base pointer (available after `allocate()`).

get(

*alloc: [SmemAllocation](#cutlass.experimental.task_scheduling.memory.SmemAllocation)*,

) → Any

Derive a typed `cutlass.Array` using the dtype/count stored on *alloc*.

Must be called after `allocate()`. Requires `alloc.dtype` to have been set when the `SmemAllocation` was constructed. For reinterpret-cast access, use `get_as_type()` instead.

get_as_type(

*alloc: [SmemAllocation](#cutlass.experimental.task_scheduling.memory.SmemAllocation)*,

*dtype: Any*,

*count: int = 1*,

) → Any

Derive a typed `cutlass.Array` with a custom dtype (reinterpret cast).

Must be called after `allocate()`. Use when the desired access type differs from the allocation's declared dtype, or when the allocation was declared with raw `size_bytes` only.

get_typed_ptr(

*alloc: [SmemAllocation](#cutlass.experimental.task_scheduling.memory.SmemAllocation)*,

*dtype: Any*,

*count: int = 1*,

) → Any

Deprecated: use `get()` or `get_as_type()` instead.

*property* total_smem_bytes*: int*

:   Total data SMEM bytes (excludes barriers, available after `compute_layout()`).

<!-- -->

allocate() → Any

:   Emit a single `cutlass.Array(..., space=cutlass.AddressSpace.smem)` for the unified layout.

    The allocation covers both data regions and pipeline barrier storage, so `create_pipeline()` does not need to allocate barrier SMEM separately. Call `assign_barrier_ptrs()` after this method to pre-set `barrier_ptr` on each resource's `PipelineConfig`.

    Returns the `cutlass.Array` base pointer. Must be called at DSL-trace time (inside a `@cute.kernel` or `@cute.jit`).

<!-- -->

assign_barrier_ptrs() → None

:   Pre-assign barrier storage within the unified SMEM block.

    For each resource that needs pipeline barriers, creates a `cute.Pointer` into the barrier region (after data) and replaces the resource's `PipelineConfig` with one that has `barrier_ptr` set. This prevents `create_pipeline()` from allocating barrier SMEM separately.

    Also handles `PipelineGroup` entries registered via `add_pipeline_group()`. Each group receives a single pointer spanning `(N + 1) × S` barrier entries.

    Must be called after `allocate()` at DSL-trace time.

<!-- -->

*class* cutlass.experimental.task_scheduling.memory.TmemAllocator

:   Bases: `_LayoutAllocator`

    TMEM column layout allocator (no hardware allocation).

    Collects `TmemAllocation` objects and computes column offsets. Does **not** emit any allocation intrinsics --- the kernel calls `nvvm.tcgen05_alloc` manually using `total_tmem_columns`.

    Example:

        tmem_alloc = TmemAllocator()
        tmem_alloc.add_resource(tmem_c_resource)
        tmem_alloc.compute_layout()
        num_cols = tmem_alloc.total_tmem_columns  # pass to tcgen05_alloc

    *property* total_tmem_columns*: int*

    :   Total TMEM columns required (available after `compute_layout()`).
