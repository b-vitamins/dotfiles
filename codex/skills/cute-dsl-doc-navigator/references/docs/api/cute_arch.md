---
title: "arch"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_arch.html"
cutlass_docs_version: "4.4.1"
generated_at: "2026-03-07T11:33:21+00:00"
last_updated: "Mar 05, 2026"
---

# arch

The `cute.arch` module provides lightweight wrappers for NVVM Operation builders which implement CUDA built-in device functions such as `thread_idx`. It integrates seamlessly with CuTe DSL types.

These wrappers enable source location tracking through the `@dsl_user_op` decorator. The module includes the following functionality:

-   Core CUDA built-in functions such as `thread_idx`, `warp_idx`, `block_dim`, `grid_dim`, `cluster_dim`, and related functions

-   Memory barrier management functions including `mbarrier_init`, `mbarrier_arrive`, `mbarrier_wait`, and associated operations

-   Low-level shared memory (SMEM) management capabilities, with `SmemAllocator` as the recommended interface

-   Low-level tensor memory (TMEM) management capabilities, with `TmemAllocator` as the recommended interface

## API documentation

### make_warp_uniform

**Kind:** function

**Qualified name:** `cutlass.cute.arch.make_warp_uniform`

**Signature:** `make_warp_uniform(value: cutlass.cute.typing.Int, *, loc = None, ip = None) → cutlass.cute.typing.Int32`

Provides a compiler hint indicating that the specified value is invariant across all threads in the warp, which may enable performance optimizations.

#### Parameters

**value** (*Int*) -- The integer value to be marked as warp-uniform.

#### Returns

The input value, marked as warp-uniform.

#### Return type

Int32

### elect_one

**Kind:** function

**Qualified name:** `cutlass.cute.arch.elect_one`

**Signature:** `elect_one(*, loc = None, ip = None) → IfOpRegion`

Elects one thread within a warp.

    with elect_one():
        # Only one thread in the warp executes the code in this context
        pass

### mbarrier_init

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_init`

**Signature:** `mbarrier_init(mbar_ptr: cutlass.cute.typing.Pointer, cnt: cutlass.cute.typing.Int, *, loc = None, ip = None) → None`

Initializes a mbarrier with the specified thread arrival count.

#### Parameters

-   **mbar_ptr** (*Pointer*) -- A pointer to the mbarrier in SMEM

-   **cnt** (*Int*) -- The arrival count of the mbarrier

### mbarrier_init_fence

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_init_fence`

**Signature:** `mbarrier_init_fence(*, loc = None, ip = None) → None`

A fence operation that applies to the mbarrier initializations.

### mbarrier_arrive_and_expect_tx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_arrive_and_expect_tx`

**Signature:** `mbarrier_arrive_and_expect_tx(mbar_ptr: cutlass.cute.typing.Pointer, bytes: cutlass.cute.typing.Int, peer_cta_rank_in_cluster = None, *, loc = None, ip = None) → None`

Arrives on a mbarrier and expects a specified number of transaction bytes.

#### Parameters

-   **mbar_ptr** (*Pointer*) -- A pointer to the mbarrier in SMEM

-   **bytes** (*Int*) -- The number of transaction bytes

-   **peer_cta_rank_in_cluster** -- An optional CTA rank in cluster. If provided, the pointer to the mbarrier is converted to a remote address in the peer CTA's SMEM.

### mbarrier_expect_tx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_expect_tx`

**Signature:** `mbarrier_expect_tx(mbar_ptr: cutlass.cute.typing.Pointer, bytes: cutlass.cute.typing.Int, peer_cta_rank_in_cluster = None, *, loc = None, ip = None) → None`

Expects a specified number of transaction bytes without an arrive.

#### Parameters

-   **mbar_ptr** (*Pointer*) -- A pointer to the mbarrier in SMEM

-   **bytes** (*Int*) -- The number of transaction bytes

-   **peer_cta_rank_in_cluster** -- An optional CTA rank in cluster. If provided, the pointer to the mbarrier is converted to a remote address in the peer CTA's SMEM.

### mbarrier_wait

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_wait`

**Signature:** `mbarrier_wait(mbar_ptr: cutlass.cute.typing.Pointer, phase: cutlass.cute.typing.Int, *, loc = None, ip = None) → None`

Waits on a mbarrier with a specified phase.

#### Parameters

-   **mbar_ptr** (*Pointer*) -- A pointer to the mbarrier in SMEM

-   **phase** (*Int*) -- The phase to wait for (either 0 or 1)

### mbarrier_try_wait

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_try_wait`

**Signature:** `mbarrier_try_wait(mbar_ptr: cutlass.cute.typing.Pointer, phase: cutlass.cute.typing.Int, *, loc = None, ip = None) → cutlass.cute.typing.Boolean`

Attempts to wait on a mbarrier with a specified phase in a non-blocking fashion.

#### Parameters

-   **mbar_ptr** (*Pointer*) -- A pointer to the mbarrier in SMEM

-   **phase** (*Int*) -- The phase to wait for (either 0 or 1)

#### Returns

A boolean value indicating whether the wait operation was successful

#### Return type

Boolean

### mbarrier_conditional_try_wait

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_conditional_try_wait`

**Signature:** `mbarrier_conditional_try_wait(cond, mbar_ptr: cutlass.cute.typing.Pointer, phase: cutlass.cute.typing.Int, *, loc = None, ip = None) → cutlass.cute.typing.Boolean`

Conditionally attempts to wait on a mbarrier with a specified phase in a non-blocking fashion.

#### Parameters

-   **cond** -- A boolean predicate

-   **mbar_ptr** (*Pointer*) -- A pointer to the mbarrier in SMEM

-   **phase** (*Int*) -- The phase to wait for (either 0 or 1)

#### Returns

A boolean value indicating whether the wait operation was successful

#### Return type

Boolean

### mbarrier_arrive

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_arrive`

**Signature:** `mbarrier_arrive(mbar_ptr: cutlass.cute.typing.Pointer, peer_cta_rank_in_cluster: cutlass.cute.typing.Int | None = None, arrive_count: cutlass.cute.typing.Int = 1, *, loc = None, ip = None) → None`

Arrives on an mbarrier.

#### Parameters

-   **mbar_ptr** (*Pointer*) -- A pointer to the mbarrier in SMEM

-   **peer_cta_rank_in_cluster** -- An optional CTA rank in cluster. If provided, the pointer to the mbarrier is converted to a remote address in the peer CTA's SMEM.

### lane_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.lane_idx`

**Signature:** `lane_idx(*, loc = None, ip = None) → cutlass.cute.typing.Int32`

Returns the lane index of the current thread within the warp.

### warp_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.warp_idx`

**Signature:** `warp_idx(*, loc = None, ip = None) → cutlass.cute.typing.Int32`

Returns the warp index within a CTA.

### thread_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.thread_idx`

**Signature:** `thread_idx(*, loc = None, ip = None) → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the thread index within a CTA.

### block_dim

**Kind:** function

**Qualified name:** `cutlass.cute.arch.block_dim`

**Signature:** `block_dim(*, loc = None, ip = None) → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the number of threads in each dimension of the CTA.

### block_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.block_idx`

**Signature:** `block_idx(*, loc = None, ip = None) → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the CTA identifier within a grid.

### grid_dim

**Kind:** function

**Qualified name:** `cutlass.cute.arch.grid_dim`

**Signature:** `grid_dim(*, loc = None, ip = None) → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the number of CTAs in each dimension of the grid.

### cluster_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_idx`

**Signature:** `cluster_idx(*, loc = None, ip = None) → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the cluster identifier within a grid.

### cluster_dim

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_dim`

**Signature:** `cluster_dim(*, loc = None, ip = None) → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the number of clusters in each dimension of the grid.

### cluster_size

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_size`

**Signature:** `cluster_size(*, loc = None, ip = None) → cutlass.cute.typing.Int32`

Returns the number of CTA within the cluster.

### block_in_cluster_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.block_in_cluster_idx`

**Signature:** `block_in_cluster_idx(*, loc = None, ip = None) → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the CTA index within a cluster across all dimensions.

### block_in_cluster_dim

**Kind:** function

**Qualified name:** `cutlass.cute.arch.block_in_cluster_dim`

**Signature:** `block_in_cluster_dim(*, loc = None, ip = None) → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the dimensions of the cluster.

### block_idx_in_cluster

**Kind:** function

**Qualified name:** `cutlass.cute.arch.block_idx_in_cluster`

**Signature:** `block_idx_in_cluster(*, loc = None, ip = None) → cutlass.cute.typing.Int32`

Returns the linearized identifier of the CTA within the cluster.

### barrier

**Kind:** function

**Qualified name:** `cutlass.cute.arch.barrier`

**Signature:** `barrier(*, barrier_id = None, number_of_threads = None, loc = None, ip = None) → None`

Creates a barrier, optionally named.

### barrier_arrive

**Kind:** function

**Qualified name:** `cutlass.cute.arch.barrier_arrive`

**Signature:** `barrier_arrive(*, barrier_id = None, number_of_threads = None, loc = None, ip = None) → None`

### sync_threads

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sync_threads`

**Signature:** `sync_threads(*, loc = None, ip = None) → None`

Synchronizes all threads within a CTA.

### sync_warp

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sync_warp`

**Signature:** `sync_warp(mask: cutlass.cute.typing.Int = 4294967295, *, loc = None, ip = None) → None`

Performs a warp-wide sync with an optional mask.

### fence_acq_rel_cta

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_acq_rel_cta`

**Signature:** `fence_acq_rel_cta(*, loc = None, ip = None) → None`

Fence operation with acquire-release semantics.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).

### fence_acq_rel_cluster

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_acq_rel_cluster`

**Signature:** `fence_acq_rel_cluster(*, loc = None, ip = None) → None`

Fence operation with acquire-release semantics.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).

### fence_acq_rel_gpu

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_acq_rel_gpu`

**Signature:** `fence_acq_rel_gpu(*, loc = None, ip = None) → None`

Fence operation with acquire-release semantics.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).

### fence_acq_rel_sys

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_acq_rel_sys`

**Signature:** `fence_acq_rel_sys(*, loc = None, ip = None) → None`

Fence operation with acquire-release semantics.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).

### cp_async_commit_group

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cp_async_commit_group`

**Signature:** `cp_async_commit_group(*, loc = None, ip = None) → None`

Commits all prior initiated but uncommitted cp.async instructions.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-commit-group).

### cp_async_wait_group

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cp_async_wait_group`

**Signature:** `cp_async_wait_group(n, *, loc = None, ip = None) → None`

Waits till only a specified numbers of cp.async groups are pending.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-wait-group-cp-async-wait-all).

### cp_async_bulk_commit_group

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cp_async_bulk_commit_group`

**Signature:** `cp_async_bulk_commit_group(*, loc = None, ip = None) → None`

Commits all prior initiated but uncommitted cp.async.bulk instructions.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk-commit-group).

### cp_async_bulk_wait_group

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cp_async_bulk_wait_group`

**Signature:** `cp_async_bulk_wait_group(group, *, read = None, loc = None, ip = None) → None`

Waits till only a specified numbers of cp.async.bulk groups are pending.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk-wait-group).

### cluster_wait

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_wait`

**Signature:** `cluster_wait(*, loc = None, ip = None) → None`

A cluster-wide wait operation.

### cluster_arrive

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_arrive`

**Signature:** `cluster_arrive(*, aligned = None, loc = None, ip = None) → None`

A cluster-wide arrive operation.

### cluster_arrive_relaxed

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_arrive_relaxed`

**Signature:** `cluster_arrive_relaxed(*, aligned = None, loc = None, ip = None) → None`

A cluster-wide arrive operation with relaxed semantics.

### vote_ballot_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.vote_ballot_sync`

**Signature:** `vote_ballot_sync(pred: cutlass.cute.typing.Boolean, mask: cutlass.cute.typing.Int = 4294967295, *, loc = None, ip = None) → cutlass.cute.typing.Int32`

Performs a ballot operation across the warp.

It copies the predicate from each thread in mask into the corresponding bit position of destination register d, where the bit position corresponds to the thread's lane id.

#### Parameters

-   **pred** (*Boolean*) -- The predicate value for the current thread

-   **mask** (*Int,* *optional*) -- A 32-bit integer mask specifying which threads participate, defaults to all threads (0xFFFFFFFF)

#### Returns

A 32-bit integer where each bit represents a thread's predicate value

#### Return type

Int32

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-vote-sync).

### vote_any_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.vote_any_sync`

**Signature:** `vote_any_sync(pred: cutlass.cute.typing.Boolean, mask: cutlass.cute.typing.Int = 4294967295, *, loc = None, ip = None) → cutlass.cute.typing.Boolean`

True if source predicate is True for any non-exited threads in mask. Negate the source predicate to compute .none.

#### Parameters

-   **pred** (*Boolean*) -- The predicate value for the current thread

-   **mask** (*Int,* *optional*) -- A 32-bit integer mask specifying which threads participate, defaults to all threads (0xFFFFFFFF)

#### Returns

A boolean value indicating if the source predicate is True for all non-exited threads in mask

#### Return type

Boolean

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-vote-sync).

### vote_all_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.vote_all_sync`

**Signature:** `vote_all_sync(pred: cutlass.cute.typing.Boolean, mask: cutlass.cute.typing.Int = 4294967295, *, loc = None, ip = None) → cutlass.cute.typing.Boolean`

True if source predicate is True for all non-exited threads in mask. Negate the source predicate to compute .none.

#### Parameters

-   **pred** (*Boolean*) -- The predicate value for the current thread

-   **mask** (*Int,* *optional*) -- A 32-bit integer mask specifying which threads participate, defaults to all threads (0xFFFFFFFF)

#### Returns

A boolean value indicating if the source predicate is True for all non-exited threads in mask

#### Return type

Boolean

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-vote-sync).

### vote_uni_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.vote_uni_sync`

**Signature:** `vote_uni_sync(pred: cutlass.cute.typing.Boolean, mask: cutlass.cute.typing.Int = 4294967295, *, loc = None, ip = None) → cutlass.cute.typing.Boolean`

True f source predicate has the same value in all non-exited threads in mask. Negating the source predicate also computes .uni

#### Parameters

-   **pred** (*Boolean*) -- The predicate value for the current thread

-   **mask** (*Int,* *optional*) -- A 32-bit integer mask specifying which threads participate, defaults to all threads (0xFFFFFFFF)

#### Returns

A boolean value indicating if the source predicate is True for all non-exited threads in mask

#### Return type

Boolean

### warp_redux_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.warp_redux_sync`

**Signature:** `warp_redux_sync(value: cutlass.cute.typing.Numeric, kind: Literal['fmax', 'fmin', 'max', 'min', 'add', 'xor', 'or', 'and'], mask_and_clamp: cutlass.cute.typing.Int = 4294967295, *, abs: bool | None = None, nan: bool | None = None, loc = None, ip = None) → cutlass.cute.typing.Numeric`

Perform warp-level reduction operation across threads.

Reduces values from participating threads in a warp according to the specified operation. All threads in the mask receive the same result.

#### Parameters

-   **value** (*Numeric*) -- Input value to reduce

-   **kind** (*Literal\[\"add\",* *\"and\",* *\"max\",* *\"min\",* *\"or\",* *\"xor\",* *\"fmin\",* *\"fmax\"\]*) -- Reduction operation. Supported operations: - Integer types (Int32/Uint32): "add", "and", "max", "min", "or", "xor" - Float types (Float32): "fmax", "fmin" (or "max"/"min" which auto-convert to "fmax"/"fmin")

-   **mask_and_clamp** (*Int*) -- Warp participation mask (default: FULL_MASK = 0xFFFFFFFF)

-   **abs** (*bool*) -- Apply absolute value before reduction (float types only)

-   **nan** (*Optional\[bool\]*) -- Enable NaN propagation for fmax/fmin operations (float types only)

#### Returns

Reduced value (same for all participating threads)

#### Return type

Numeric

### atomic_max_float32

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_max_float32`

**Signature:** `atomic_max_float32(ptr, value: cutlass.cute.typing.Float32, *, positive_only: bool = True, loc = None, ip = None) → cutlass.cute.typing.Float32`

Performs an atomic max operation on a float32 value in global memory.

This implementation works correctly for non-negative values (\>= 0) using direct bitcast.

#### Parameters

-   **ptr** -- Pointer to the memory location

-   **value** (*Float32*) -- The float32 value to compare and potentially store (should be \>= 0 for correct results)

-   **positive_only** (*bool*) -- If True (default), assumes input values are non-negative. This parameter is provided for API compatibility and future extensions.

#### Returns

The old value at the memory location

#### Return type

Float32

### atomic_add

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_add`

**Signature:** `atomic_add(ptr, val: cutlass.cute.typing.Numeric | cutlass._mlir.ir.Value, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None, loc = None, ip = None) → cutlass.cute.typing.Numeric | cutlass._mlir.ir.Value`

Performs an atomic addition operation.

Atomically adds val to the value at memory location ptr and returns the old value.

#### Parameters

-   **ptr** -- Pointer to memory location

-   **val** (*Union\[Numeric,* *ir.Value\]*) -- Value to add (scalar Numeric or vector ir.Value)

-   **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

-   **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

Union\[Numeric, ir.Value\]

### atomic_and

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_and`

**Signature:** `atomic_and(ptr, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None, loc = None, ip = None) → cutlass.cute.typing.Numeric`

Performs an atomic bitwise AND operation.

Atomically computes bitwise AND of val with the value at memory location ptr and returns the old value.

#### Parameters

-   **ptr** -- Pointer to memory location

-   **val** (*Numeric*) -- Value for AND operation

-   **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

-   **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

Numeric

### atomic_or

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_or`

**Signature:** `atomic_or(ptr, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None, loc = None, ip = None) → cutlass.cute.typing.Numeric`

Performs an atomic bitwise OR operation.

Atomically computes bitwise OR of val with the value at memory location ptr and returns the old value.

#### Parameters

-   **ptr** -- Pointer to memory location

-   **val** (*Numeric*) -- Value for OR operation

-   **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

-   **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

Numeric

### atomic_xor

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_xor`

**Signature:** `atomic_xor(ptr, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None, loc = None, ip = None) → cutlass.cute.typing.Numeric`

Performs an atomic bitwise XOR operation.

Atomically computes bitwise XOR of val with the value at memory location ptr and returns the old value.

#### Parameters

-   **ptr** -- Pointer to memory location

-   **val** (*Numeric*) -- Value for XOR operation

-   **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

-   **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

Numeric

### atomic_max

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_max`

**Signature:** `atomic_max(ptr, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None, loc = None, ip = None) → cutlass.cute.typing.Numeric`

Performs an atomic maximum operation.

Atomically computes maximum of val and the value at memory location ptr and returns the old value.

#### Parameters

-   **ptr** -- Pointer to memory location

-   **val** (*Numeric*) -- Value for MAX operation

-   **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

-   **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

Numeric

### atomic_min

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_min`

**Signature:** `atomic_min(ptr, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None, loc = None, ip = None) → cutlass.cute.typing.Numeric`

Performs an atomic minimum operation.

Atomically computes minimum of val and the value at memory location ptr and returns the old value.

#### Parameters

-   **ptr** -- Pointer to memory location

-   **val** (*Numeric*) -- Value for MIN operation

-   **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

-   **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

Numeric

### atomic_exch

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_exch`

**Signature:** `atomic_exch(ptr, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None, loc = None, ip = None) → cutlass.cute.typing.Numeric`

Performs an atomic exchange operation.

Atomically exchanges val with the value at memory location ptr and returns the old value.

#### Parameters

-   **ptr** -- Pointer to memory location

-   **val** (*Numeric*) -- Value to exchange

-   **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

-   **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

Numeric

### atomic_cas

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_cas`

**Signature:** `atomic_cas(ptr, *, cmp: cutlass.cute.typing.Numeric, val: cutlass.cute.typing.Numeric, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None, loc = None, ip = None) → cutlass.cute.typing.Numeric`

Performs an atomic compare-and-swap (CAS) operation.

Atomically compares the value at the memory location with cmp. If they are equal, stores val at the memory location and returns the old value.

#### Parameters

-   **ptr** -- Pointer to memory location. Supports: - ir.Value (LLVM pointer) - cute.ptr (\_Pointer instance)

-   **cmp** (*Numeric*) -- Value to compare against current memory value

-   **val** (*Numeric*) -- Value to store if comparison succeeds

-   **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

-   **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

Numeric

### store

**Kind:** function

**Qualified name:** `cutlass.cute.arch.store`

**Signature:** `store(ptr, val: cutlass.cute.typing.Numeric | cutlass._mlir.ir.Value, *, level1_eviction_priority: Literal['evict_normal', 'evict_first', 'evict_last', 'evict_no_allocate', 'evict_unchanged'] | None = None, cop: Literal['wb', 'cg', 'cs', 'wt'] | None = None, ss: Literal['cta', 'cluster'] | None = None, sem: Literal['relaxed', 'release'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None, loc = None, ip = None) → None`

Store a value to a memory location.

#### Parameters

-   **ptr** -- Pointer to store to. Supports: - ir.Value (LLVM pointer) - cute.ptr (\_Pointer instance)

-   **val** (*Union\[Numeric,* *ir.Value\]*) -- Value to store (scalar Numeric or vector ir.Value)

-   **level1_eviction_priority** -- L1 cache eviction policy string literal: "evict_normal" : .level1::eviction_priority = .L1::evict_normal "evict_first" : .level1::eviction_priority = .L1::evict_first "evict_last" : .level1::eviction_priority = .L1::evict_last "evict_no_allocate" : .level1::eviction_priority = .L1::no_allocate "evict_unchanged" : .level1::eviction_priority = .L1::evict_unchanged

-   **cop** -- Store cache modifier string literal:

-   **ss** -- Shared memory space string literal: "cta" : .ss = .shared::cta "cluster" : .ss = .shared::cluster None : .ss = .global

-   **sem** -- Memory semantic string literal:

-   **scope** -- Memory scope string literal:

### load

**Kind:** function

**Qualified name:** `cutlass.cute.arch.load`

**Signature:** `load(ptr, dtype: type[cutlass.cute.typing.Numeric] | cutlass._mlir.ir.VectorType, *, sem: Literal['relaxed', 'acquire'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None, level1_eviction_priority: Literal['evict_normal', 'evict_first', 'evict_last', 'evict_no_allocate', 'evict_unchanged'] | None = None, cop: Literal['ca', 'cg', 'cs', 'lu', 'cv'] | None = None, ss: Literal['cta', 'cluster'] | None = None, level_prefetch_size: Literal['size_64b', 'size_128b', 'size_256b'] | None = None, loc = None, ip = None) → cutlass.cute.typing.Numeric | cutlass._mlir.ir.Value`

Load a value from a memory location.

#### Parameters

-   **ptr** -- Pointer to load from. Supports: - ir.Value (LLVM pointer) - cute.ptr (\_Pointer instance)

-   **dtype** (*Union\[type\[Numeric\],* *ir.VectorType\]*) -- Data type to load. Can be: - Scalar: Numeric type class (Int8, Uint8, Int32, Float32, etc.) - Vector: ir.VectorType for vectorized load (e.g., ir.VectorType.get(\[4\], Int64.mlir_type))

-   **sem** -- Memory semantic string literal:

-   **scope** -- Memory scope string literal:

-   **level1_eviction_priority** -- L1 cache eviction policy string literal: "evict_normal" : .level1::eviction_priority = .L1::evict_normal "evict_first" : .level1::eviction_priority = .L1::evict_first "evict_last" : .level1::eviction_priority = .L1::evict_last "evict_no_allocate" : .level1::eviction_priority = .L1::no_allocate "evict_unchanged" : .level1::eviction_priority = .L1::evict_unchanged

-   **cop** -- Load cache modifier string literal:

-   **ss** -- Shared memory space string literal: "cta" : .ss = .shared::cta "cluster" : .ss = .shared::cluster None : .ss = .global

-   **level_prefetch_size** -- L2 cache prefetch size hint string literal: "size_64b" : .level::prefetch_size = .L2::64B "size_128b" : .level::prefetch_size = .L2::128B "size_256b" : .level::prefetch_size = .L2::256B

#### Returns

Loaded value (scalar Numeric or vector ir.Value)

#### Return type

Union\[Numeric, ir.Value\]

### popc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.popc`

**Signature:** `popc(value: cutlass.cute.typing.Numeric, *, loc = None, ip = None) → cutlass.cute.typing.Numeric`

Performs a population count operation.

### fence_proxy

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_proxy`

**Signature:** `fence_proxy(kind: Literal['alias', 'async', 'async.global', 'async.shared', 'tensormap', 'generic'], *, space: Literal['cta', 'cluster'] | None = None, use_intrinsic = None, loc = None, ip = None) → None`

Fence operation to ensure memory consistency between proxies.

#### Parameters

-   **kind** (*Literal\[\"alias\",* *\"async\",* *\"async.global\",* *\"async.shared\",* *\"tensormap\",* *\"generic\"\]*) -- Proxy kind string literal: - "alias" : Alias proxy - "async" : Async proxy - "async.global" : Async global proxy - "async.shared" : Async shared proxy - "tensormap" : Tensormap proxy - "generic" : Generic proxy

-   **space** (*Optional\[Literal\[\"cta\",* *\"cluster\"\]\]*) -- Shared memory space scope string literal (optional): - "cta" : CTA (Cooperative Thread Array) scope - "cluster" : Cluster scope

-   **use_intrinsic** -- Whether to use intrinsic version

### warpgroup_reg_alloc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.warpgroup_reg_alloc`

**Signature:** `warpgroup_reg_alloc(reg_count: int, *, loc = None, ip = None) → None`

### warpgroup_reg_dealloc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.warpgroup_reg_dealloc`

**Signature:** `warpgroup_reg_dealloc(reg_count: int, *, loc = None, ip = None) → None`

### setmaxregister_increase

**Kind:** function

**Qualified name:** `cutlass.cute.arch.setmaxregister_increase`

**Signature:** `setmaxregister_increase(reg_count: int, *, loc = None, ip = None)`

### setmaxregister_decrease

**Kind:** function

**Qualified name:** `cutlass.cute.arch.setmaxregister_decrease`

**Signature:** `setmaxregister_decrease(reg_count: int, *, loc = None, ip = None)`

### fmax

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fmax`

**Signature:** `fmax(a: float | cutlass.cute.typing.Float32, b: float | cutlass.cute.typing.Float32, *, loc = None, ip = None) → cutlass.cute.typing.Float32`

### rcp_approx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.rcp_approx`

**Signature:** `rcp_approx(a: float | cutlass.cute.typing.Float32, *, loc = None, ip = None)`

### exp2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.exp2`

**Signature:** `exp2(a: float | cutlass.cute.typing.Float32, *, loc = None, ip = None) → cutlass.cute.typing.Float32`

### cvt_i8x4_to_f32x4

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8x4_to_f32x4`

**Signature:** `cvt_i8x4_to_f32x4(src_vec4, *, loc = None, ip = None)`

### cvt_i8x2_to_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8x2_to_f32x2`

**Signature:** `cvt_i8x2_to_f32x2(src_vec2, *, loc = None, ip = None)`

### cvt_i8_bf16

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8_bf16`

**Signature:** `cvt_i8_bf16(src_i8, *, loc = None, ip = None)`

### cvt_i8x2_to_bf16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8x2_to_bf16x2`

**Signature:** `cvt_i8x2_to_bf16x2(src_vec2, *, loc = None, ip = None)`

### cvt_i8x4_to_bf16x4

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8x4_to_bf16x4`

**Signature:** `cvt_i8x4_to_bf16x4(src_vec4, *, loc = None, ip = None)`

### cvt_f32x2_bf16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_f32x2_bf16x2`

**Signature:** `cvt_f32x2_bf16x2(src_vec2, *, loc = None, ip = None)`

### alloc_smem

**Kind:** function

**Qualified name:** `cutlass.cute.arch.alloc_smem`

**Signature:** `alloc_smem(element_type: Type[cutlass.cute.typing.Numeric], size_in_elems: int, alignment: int | None = None, *, loc = None, ip = None) → cutlass.cute.typing.Pointer`

Statically allocates SMEM.

#### Parameters

-   **element_type** (*Type\[Numeric\]*) -- The pointee type of the pointer.

-   **size_in_elems** (*int*) -- The size of the allocation in terms of number of elements of the pointee type

-   **alignment** (*int*) -- An optional pointer alignment for the allocation

#### Returns

A pointer to the start of the allocation

#### Return type

Pointer

### get_dyn_smem

**Kind:** function

**Qualified name:** `cutlass.cute.arch.get_dyn_smem`

**Signature:** `get_dyn_smem(element_type: Type[cutlass.cute.typing.Numeric], alignment: int | None = None, *, loc = None, ip = None) → cutlass.cute.typing.Pointer`

Retrieves a pointer to a dynamic SMEM allocation.

#### Parameters

-   **element_type** (*Type\[Numeric\]*) -- The pointee type of the pointer.

-   **alignment** (*int*) -- An optional pointer alignment, the result pointer is offset appropriately

#### Returns

A pointer to the start of the dynamic SMEM allocation with a correct alignement

#### Return type

Pointer

### get_dyn_smem_size

**Kind:** function

**Qualified name:** `cutlass.cute.arch.get_dyn_smem_size`

**Signature:** `get_dyn_smem_size(*, loc = None, ip = None) → int`

Gets the size in bytes of the dynamic shared memory that was specified at kernel launch time. This can be used for bounds checking during shared memory allocation.

#### Returns

The size of dynamic shared memory in bytes

#### Return type

int

### get_max_tmem_alloc_cols

**Kind:** function

**Qualified name:** `cutlass.cute.arch.get_max_tmem_alloc_cols`

**Signature:** `get_max_tmem_alloc_cols(compute_capability: str) → int`

Get the tensor memory capacity in columns for a given compute capability.

Returns the maximum TMEM capacity in columns available for the specified GPU compute capability.

#### Parameters

**compute_capability** (*str*) -- The compute capability string (e.g. "sm_100", "sm_103")

#### Returns

The TMEM capacity in columns

#### Return type

int

#### Raises

**ValueError** -- If the compute capability is not supported

### get_min_tmem_alloc_cols

**Kind:** function

**Qualified name:** `cutlass.cute.arch.get_min_tmem_alloc_cols`

**Signature:** `get_min_tmem_alloc_cols(compute_capability: str) → int`

Get the minimum TMEM allocation columns for a given compute capability.

Returns the minimum TMEM allocation columns available for the specified GPU compute capability.

#### Parameters

**compute_capability** (*str*) -- The compute capability string (e.g. "sm_100", "sm_103")

#### Returns

The minimum TMEM allocation columns

#### Return type

int

#### Raises

**ValueError** -- If the compute capability is not supported

### retrieve_tmem_ptr

**Kind:** function

**Qualified name:** `cutlass.cute.arch.retrieve_tmem_ptr`

**Signature:** `retrieve_tmem_ptr(element_type: Type[cutlass.cute.typing.Numeric], alignment: int, ptr_to_buffer_holding_addr: cutlass.cute.typing.Pointer, *, loc = None, ip = None) → cutlass.cute.typing.Pointer`

Retrieves a pointer to TMEM with the provided element type and alignment.

#### Parameters

-   **element_type** (*Type\[Numeric\]*) -- The pointee type of the pointer.

-   **alignment** (*int*) -- The alignment of the result pointer

-   **ptr_to_buffer_holding_addr** (*Pointer*) -- A pointer to a SMEM buffer holding the TMEM address of the start of the allocation allocation

#### Returns

A pointer to TMEM

#### Return type

Pointer

### alloc_tmem

**Kind:** function

**Qualified name:** `cutlass.cute.arch.alloc_tmem`

**Signature:** `alloc_tmem(num_columns: cutlass.cute.typing.Int, smem_ptr_to_write_address: cutlass.cute.typing.Pointer, is_two_cta = None, *, arch: str = 'sm_100', loc = None, ip = None) → None`

Allocates TMEM.

#### Parameters

-   **num_columns** (*Int*) -- The number of TMEM columns to allocate

-   **smem_ptr_to_write_address** (*Pointer*) -- A pointer to a SMEM buffer where the TMEM address is written to

-   **is_two_cta** -- Optional boolean parameter for 2-CTA MMAs

-   **arch** (*str*) -- The architecture of the GPU.

### relinquish_tmem_alloc_permit

**Kind:** function

**Qualified name:** `cutlass.cute.arch.relinquish_tmem_alloc_permit`

**Signature:** `relinquish_tmem_alloc_permit(is_two_cta = None, *, loc = None, ip = None) → None`

Relinquishes the right to allocate TMEM so that other CTAs potentially in a different grid can allocate.

### dealloc_tmem

**Kind:** function

**Qualified name:** `cutlass.cute.arch.dealloc_tmem`

**Signature:** `dealloc_tmem(tmem_ptr: cutlass.cute.typing.Pointer, num_columns: cutlass.cute.typing.Int, is_two_cta = None, *, arch: str = 'sm_100', loc = None, ip = None) → None`

Deallocates TMEM using the provided pointer and number of columns.

#### Parameters

-   **tmem_ptr** (*Pointer*) -- A pointer to the TMEM allocation to de-allocate

-   **num_columns** (*Int*) -- The number of columns in the TMEM allocation

-   **is_two_cta** -- Optional boolean parameter for 2-CTA MMAs

### prmt

**Kind:** function

**Qualified name:** `cutlass.cute.arch.prmt`

**Signature:** `prmt(src, src_reg_shifted, prmt_indices, *, loc = None, ip = None)`

### cvt_i8_bf16_intrinsic

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8_bf16_intrinsic`

**Signature:** `cvt_i8_bf16_intrinsic(vec_i8, length, *, loc = None, ip = None)`

Fast conversion from int8 to bfloat16. It converts a vector of int8 to a vector of bfloat16.

#### Parameters

-   **vec_i8** (*1D vector* *of* *int8*) -- The input vector of int8.

-   **length** (*int*) -- The length of the input vector.

#### Returns

The output 1D vector of bfloat16 with the same length as the input vector.

#### Return type

1D vector of bfloat16

### cvt_i4_bf16_intrinsic

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i4_bf16_intrinsic`

**Signature:** `cvt_i4_bf16_intrinsic(vec_i4, length, *, with_shuffle = False, loc = None, ip = None)`

Fast conversion from int4 to bfloat16. It converts a vector of int4 to a vector of bfloat16.

#### Parameters

-   **vec_i4** (*1D vector* *of* *int4*) -- The input vector of int4.

-   **length** (*int*) -- The length of the input vector.

-   **with_shuffle** (*bool*) -- Whether the input vec_i4 follows a specific shuffle pattern. If True, for consecutive 8 int4 values with indices of (0, 1, 2, 3, 4, 5, 6, 7), the input elements are shuffled to (0, 2, 1, 3, 4, 6, 5, 7). For tailing elements less than 8, the shuffle pattern is (0, 2, 1, 3) for 4 elements. No shuffle is needed for less than 4 elements. Shuffle could help to produce converted bf16 values in the natural order of (0, 1, 2 ,3 ,4 ,5 ,6 ,7) without extra prmt instructions and thus better performance.

#### Returns

The output 1D vector of bfloat16 with the same length as the input vector.

#### Return type

1D vector of bfloat16

### issue_clc_query

**Kind:** function

**Qualified name:** `cutlass.cute.arch.issue_clc_query`

**Signature:** `issue_clc_query(mbar_ptr: cutlass.cute.typing.Pointer, clc_response_ptr: cutlass.cute.typing.Pointer, loc = None, ip = None) → None`

The clusterlaunchcontrol.try_cancel instruction requests atomically cancelling the launch of a cluster that has not started running yet. It asynchronously writes an opaque response to shared memory indicating whether the operation succeeded or failed. On success, the opaque response contains the ctaid of the first CTA of the canceled cluster.

#### Parameters

-   **mbar_ptr** (*Pointer*) -- A pointer to the mbarrier address in SMEM

-   **clc_response_ptr** (*Pointer*) -- A pointer to the cluster launch control response address in SMEM

### clc_response

**Kind:** function

**Qualified name:** `cutlass.cute.arch.clc_response`

**Signature:** `clc_response(result_addr: cutlass.cute.typing.Pointer, loc = None, ip = None) → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

After loading response from clusterlaunchcontrol.try_cancel instruction into 16-byte register, it can be further queried using clusterlaunchcontrol.query_cancel instruction. If the cluster is canceled successfully, predicate p is set to true; otherwise, it is set to false. If the request succeeded, clusterlaunchcontrol.query_cancel.get_first_ctaid extracts the CTA id of the first CTA in the canceled cluster. By default, the instruction returns a .v4 vector whose first three elements are the x, y and z coordinate of first CTA in canceled cluster.

#### Parameters

**result_addr** (*Pointer*) -- A pointer to the cluster launch control response address in SMEM
