---
title: "arch"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_arch.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# arch

> **Note.**
>
> `cutlass.cute.arch` contains low-level CUDA primitive wrappers. These APIs are being moved into [Primitives](../primitives.md); prefer the Primitives page for new primitive-level documentation as the migration lands.

The `cute.arch` module provides lightweight wrappers for NVVM Operation builders which implement CUDA built-in device functions such as `thread_idx`. It integrates seamlessly with CuTe DSL types.

These wrappers enable source location tracking through the `@dsl_user_op` decorator. The module includes the following functionality:

- Core CUDA built-in functions such as `thread_idx`, `warp_idx`, `block_dim`, `grid_dim`, `cluster_dim`, and related functions

- Memory barrier management functions including `mbarrier_init`, `mbarrier_arrive`, `mbarrier_wait`, and associated operations

- Low-level shared memory (SMEM) management capabilities, with `SmemAllocator` as the recommended interface

- Low-level tensor memory (TMEM) management capabilities, with `TmemAllocator` as the recommended interface

## API documentation

### make_warp_uniform

**Kind:** function

**Qualified name:** `cutlass.cute.arch.make_warp_uniform`

**Signature:** `make_warp_uniform(value: cutlass.cute.typing.Int) → cutlass.cute.typing.Int32`

Provides a compiler hint indicating that the specified value is invariant across all threads in the warp, which may enable performance optimizations.

#### Parameters

**value** (*Int*) -- The integer value to be marked as warp-uniform.

#### Returns

The input value, marked as warp-uniform.

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### elect_one

**Kind:** function

**Qualified name:** `cutlass.cute.arch.elect_one`

**Signature:** `elect_one() → IfOpRegion`

Elects one thread within a warp to execute single-threaded operations.

This function uses the PTX `elect.sync` instruction to select exactly one thread per warp to execute the code within its context. All other threads in the warp skip the block and reconverge after it.

See the PTX ISA documentation on [elect.sync](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-elect-sync).

**When to Use elect_one:**

`elect_one()` is **required** for operations that must be executed by a single thread for correctness, including:

- **Barrier initialization and transaction setup** (`mbarrier_init`, `mbarrier_expect_tx`, `mbarrier_arrive_and_expect_tx`)

- **tcgen05 commit operations** (`tcgen05.commit`) - DSL does NOT automatically guard these, unlike C++ which uses `elect_one_sync()` internally

- **Single-thread state setup**

**When NOT to Use elect_one:**

Do NOT use `elect_one()` for operations that already handle single-threaded execution internally:

- **TMA copy operations** (`cute.copy` with TMA atoms) - TMA partitioning ensures only one thread within a warp issues the operation automatically. Wrapping in `elect_one()` can cause GPU deadlock.

<!-- -->

    # CORRECT: Initialize barrier with elect_one
    with elect_one():
        cute.arch.mbarrier_init(barrier_ptr, arrival_count)
        cute.arch.mbarrier_expect_tx(barrier_ptr, num_bytes)

    # CORRECT: tcgen05.commit requires elect_one in DSL
    with elect_one():
        tcgen05.commit(barrier_ptr, None, cta_group)

    # CORRECT: TMA copy does not need elect_one
    cute.copy(
        tma_atom,
        gmem_tensor,  # TMA handles single-thread internally
        smem_tensor,
        tma_bar_ptr=barrier_ptr
    )

**PTX Programming Model:**

In the PTX programming model, certain cluster-scoped and CTA-scoped operations must be issued by a single thread to maintain correctness. The `elect.sync` instruction provides a warp-uniform way to select this thread with proper synchronization.

#### Returns

A context manager that executes its block on exactly one thread per warp

#### Return type

IfOpRegion

> **Note.**
>
> See also
>
> - `cute.arch.mbarrier_init()` - Requires elect_one
>
> - `cute.arch.mbarrier_expect_tx()` - Requires elect_one
>
> - `cute.arch.mbarrier_arrive_and_expect_tx()` - Requires elect_one
>
> - PTX ISA documentation on `elect.sync`
>
> - Tutorial example: `examples/blackwell/tutorial_tma/tma_v0.py`

### mbarrier_init

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_init`

**Signature:** `mbarrier_init(mbar_ptr: cutlass.cute.typing.Pointer, cnt: cutlass.cute.typing.Int) → None`

Initializes a mbarrier with the specified thread arrival count.

**Single-Thread Execution Required**: This operation **must** be executed by only one thread per CTA. Use `cute.arch.elect_one()` to ensure proper synchronization:

    with cute.arch.elect_one():
        cute.arch.mbarrier_init(barrier_ptr, arrival_count)

**PTX Mapping**: This operation maps to the PTX `mbarrier.init.shared::cta.b64` form, which must be issued by a single thread for correctness.

#### Parameters

- **mbar_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the mbarrier in SMEM

- **cnt** (*Int*) -- The arrival count of the mbarrier

> **Note.**
>
> See also
>
> - `cute.arch.elect_one()` - Required wrapper for single-thread execution
>
> - `cute.arch.mbarrier_expect_tx()` - Also requires elect_one
>
> - PTX ISA documentation on `mbarrier.init`

### mbarrier_init_fence

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_init_fence`

**Signature:** `mbarrier_init_fence() → None`

A fence operation that applies to the mbarrier initializations.

### mbarrier_arrive_and_expect_tx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_arrive_and_expect_tx`

**Signature:** `mbarrier_arrive_and_expect_tx(mbar_ptr: cutlass.cute.typing.Pointer, bytes: cutlass.cute.typing.Int, peer_cta_rank_in_cluster: cutlass.cute.typing.Optional.cutlass.cute.typing.Int | None = None, relaxed: bool = False, scope: _install_cutlass_mlir_autodoc_stub.<locals>._DocDialectObject = <class 'sphinx.ext.autodoc.mock._MockObject'>) → None`

Arrives on a mbarrier and expects a specified number of transaction bytes.

Each thread that executes this operation increases the mbarrier tx-count by the specified byte count and then performs an arrive-on operation, which decrements the pending arrival count by 1.

To ensure proper synchronization, most calls to this function should be wrapped in `cute.arch.elect_one()`.

    with cute.arch.elect_one():
        cute.arch.mbarrier_arrive_and_expect_tx(barrier_ptr, num_transaction_bytes)

This is a combined operation that both declares transaction bytes and arrives at the barrier. It is commonly used with TMA operations in pipelined kernels.

See the PTX ISA documentation on [mbarrier.arrive.expect_tx](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-mbarrier-arrive-expect-tx).

#### Parameters

- **mbar_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the mbarrier in SMEM

- **bytes** (*Int*) -- The number of transaction bytes

- **peer_cta_rank_in_cluster** -- An optional CTA rank in cluster. If provided, the pointer to the mbarrier is converted to a remote address in the peer CTA's SMEM.

- **relaxed** -- If True, the arrive operation has relaxed semantics and does not provide any ordering or visibility guarantees.

- **scope** -- Scope of threads participating in the arrive/wait operations.

> **Note.**
>
> See also
>
> - `cute.arch.elect_one()` - Required wrapper for single-thread execution
>
> - `cute.arch.mbarrier_init()` - Also requires elect_one
>
> - `cute.arch.mbarrier_expect_tx()` - Expect_tx without arrive

### mbarrier_expect_tx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_expect_tx`

**Signature:** `mbarrier_expect_tx(mbar_ptr: cutlass.cute.typing.Pointer, bytes: cutlass.cute.typing.Int, peer_cta_rank_in_cluster: cutlass.cute.typing.Optional.cutlass.cute.typing.Int | None = None, *, scope: cutlass.cute.typing.Optional.<class '_DocDialectObject'> | None = None) → None`

Expects a specified number of transaction bytes without an arrive.

Each thread that executes this operation increases the mbarrier tx-count by the specified byte count.

To ensure proper synchronization, most calls to this function should be wrapped in `cute.arch.elect_one()`.

    with cute.arch.elect_one():
        cute.arch.mbarrier_expect_tx(barrier_ptr, num_transaction_bytes)

This is commonly used with TMA operations to set the expected transaction size before issuing a TMA load.

See the PTX ISA documentation on [mbarrier.expect_tx](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-mbarrier-expect-tx).

#### Parameters

- **mbar_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the mbarrier in SMEM

- **bytes** (*Int*) -- The number of transaction bytes

- **peer_cta_rank_in_cluster** -- An optional CTA rank in cluster. If provided, the pointer to the mbarrier is converted to a remote address in the peer CTA's SMEM.

- **scope** -- Scope of threads participating in the mbarrier operation. Defaults to CTA for local barriers and CLUSTER for remote CTA targets.

> **Note.**
>
> See also
>
> - `cute.arch.elect_one()` - Recommended wrapper for single-thread execution
>
> - `cute.arch.mbarrier_init()` - initialize mbarrier
>
> - `cute.arch.mbarrier_arrive_and_expect_tx()` - Combined arrive and expect_tx

### mbarrier_wait

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_wait`

**Signature:** `mbarrier_wait(mbar_ptr: cutlass.cute.typing.Pointer, phase: cutlass.cute.typing.Int) → None`

Waits on a mbarrier with a specified phase.

#### Parameters

- **mbar_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the mbarrier in SMEM

- **phase** (*Int*) -- The phase to wait for (either 0 or 1)

### mbarrier_try_wait

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_try_wait`

**Signature:** `mbarrier_try_wait(mbar_ptr: cutlass.cute.typing.Pointer, phase: cutlass.cute.typing.Int) → cutlass.cute.typing.Boolean`

Attempts to wait on a mbarrier with a specified phase. This uses PTX `mbarrier.try_wait`, which may suspend the executing thread if the phase is incomplete.

#### Parameters

- **mbar_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the mbarrier in SMEM

- **phase** (*Int*) -- The phase to wait for (either 0 or 1)

#### Returns

A boolean value indicating whether the wait operation was successful

#### Return type

[Boolean](../basic_data_types.md#cutlass.Boolean)

### mbarrier_conditional_try_wait

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_conditional_try_wait`

**Signature:** `mbarrier_conditional_try_wait(cond: cutlass.cute.typing.Boolean, mbar_ptr: cutlass.cute.typing.Pointer, phase: cutlass.cute.typing.Int) → cutlass.cute.typing.Boolean`

Conditionally attempts to wait on a mbarrier with a specified phase using PTX `mbarrier.try_wait` semantics.

#### Parameters

- **cond** -- A boolean predicate

- **mbar_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the mbarrier in SMEM

- **phase** (*Int*) -- The phase to wait for (either 0 or 1)

#### Returns

A boolean value indicating whether the wait operation was successful

#### Return type

[Boolean](../basic_data_types.md#cutlass.Boolean)

### mbarrier_arrive

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_arrive`

**Signature:** `mbarrier_arrive(mbar_ptr: cutlass.cute.typing.Pointer, peer_cta_rank_in_cluster: cutlass.cute.typing.Optional.cutlass.cute.typing.Int | None = None, arrive_count: cutlass.cute.typing.Int = 1, *, relaxed: bool = False, scope: cutlass.cute.typing.Optional.<class '_DocDialectObject'> | None = None) → None`

Arrives on an mbarrier.

#### Parameters

- **mbar_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the mbarrier in SMEM

- **peer_cta_rank_in_cluster** -- An optional CTA rank in cluster. If provided, the pointer to the mbarrier is converted to a remote address in the peer CTA's SMEM.

- **scope** -- Scope of threads participating in the mbarrier operation. Defaults to CTA, including for remote CTA targets.

### mbarrier_test_wait

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mbarrier_test_wait`

**Signature:** `mbarrier_test_wait(mbar_ptr: cutlass.cute.typing.Pointer, phase: cutlass.cute.typing.Int) → cutlass.cute.typing.Boolean`

Tests if a mbarrier with a specified phase is complete.

#### Parameters

- **mbar_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the mbarrier in SMEM

- **phase** (*Int*) -- The phase to wait for (either 0 or 1)

#### Returns

A boolean value indicating whether the wait operation was successful

#### Return type

[Boolean](../basic_data_types.md#cutlass.Boolean)

### lane_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.lane_idx`

**Signature:** `lane_idx() → cutlass.cute.typing.Int32`

Returns the lane index of the current thread within the warp.

### warp_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.warp_idx`

**Signature:** `warp_idx() → cutlass.cute.typing.Int32`

Returns the logical warp index within a CTA, computed from the CTA thread index registers.

### physical_warp_id

**Kind:** function

**Qualified name:** `cutlass.cute.arch.physical_warp_id`

**Signature:** `physical_warp_id() → cutlass.cute.typing.Int32`

Returns the physical warp slot identifier from the PTX `%warpid` special register.

PTX documents `%warpid` as a diagnostic register whose value may change during execution, for example after rescheduling. Use [`warp_idx()`](#cutlass.cute.arch.warp_idx) when kernel code needs a stable logical warp index within a CTA.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-warpid).

### thread_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.thread_idx`

**Signature:** `thread_idx() → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the thread index within a CTA.

### block_dim

**Kind:** function

**Qualified name:** `cutlass.cute.arch.block_dim`

**Signature:** `block_dim() → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the number of threads in each dimension of the CTA.

### block_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.block_idx`

**Signature:** `block_idx() → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the CTA identifier within a grid.

### grid_dim

**Kind:** function

**Qualified name:** `cutlass.cute.arch.grid_dim`

**Signature:** `grid_dim() → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the number of CTAs in each dimension of the grid.

### cluster_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_idx`

**Signature:** `cluster_idx() → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the cluster identifier within a grid.

### cluster_dim

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_dim`

**Signature:** `cluster_dim() → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the number of clusters in each dimension of the grid.

### cluster_size

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_size`

**Signature:** `cluster_size() → cutlass.cute.typing.Int32`

Returns the number of CTA within the cluster.

### block_in_cluster_idx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.block_in_cluster_idx`

**Signature:** `block_in_cluster_idx() → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the CTA index within a cluster across all dimensions.

### block_in_cluster_dim

**Kind:** function

**Qualified name:** `cutlass.cute.arch.block_in_cluster_dim`

**Signature:** `block_in_cluster_dim() → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

Returns the dimensions of the cluster.

### block_idx_in_cluster

**Kind:** function

**Qualified name:** `cutlass.cute.arch.block_idx_in_cluster`

**Signature:** `block_idx_in_cluster() → cutlass.cute.typing.Int32`

Returns the linearized identifier of the CTA within the cluster.

### dynamic_smem_size

**Kind:** function

**Qualified name:** `cutlass.cute.arch.dynamic_smem_size`

**Signature:** `dynamic_smem_size() → cutlass.cute.typing.Int32`

Returns the dynamic shared-memory size requested at launch.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-dynamic-smem-size>

### shuffle_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.shuffle_sync`

**Signature:** `shuffle_sync(value: cutlass.cute.typing.Numeric | TensorSSA, offset: cutlass.cute.typing.Int, mask: cutlass.cute.typing.Int = 4294967295, mask_and_clamp: cutlass.cute.typing.Int = 31, *, kind: _install_cutlass_mlir_autodoc_stub.<locals>._DocDialectObject = <class 'sphinx.ext.autodoc.mock._MockObject'>) → cutlass.cute.typing.Numeric | TensorSSA`

Shuffles a value within the threads of a warp.

#### Parameters

- **value** ([*Numeric*](../basic_data_types.md#cutlass.Numeric) *or* [*TensorSSA*](cute.md#cutlass.cute.TensorSSA)) -- The value to shuffle

- **mask** (*Int*) -- A mask describing the threads participating in this operation

- **offset** (*Int*) -- A source lane or a source lane offset depending on kind

- **mask_and_clamp** (*Int*) -- An integer containing two packed values specifying a mask for logically splitting warps into sub-segments and an upper bound for clamping the source lane index.

- **kind** (*ShflKind*) -- The kind of shuffle, can be idx, up, down, or bfly

#### Returns

The shuffled value

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### shuffle_sync_up

**Kind:** function

**Qualified name:** `cutlass.cute.arch.shuffle_sync_up`

**Signature:** `shuffle_sync_up(value: cutlass.cute.typing.Numeric | TensorSSA, offset: cutlass.cute.typing.Int, mask: cutlass.cute.typing.Int = 4294967295, *, mask_and_clamp: cutlass.cute.typing.Int = 0, kind: _install_cutlass_mlir_autodoc_stub.<locals>._DocDialectObject = <class 'sphinx.ext.autodoc.mock._MockObject'>) → cutlass.cute.typing.Numeric | TensorSSA`

Shuffles a value within the threads of a warp.

#### Parameters

- **value** ([*Numeric*](../basic_data_types.md#cutlass.Numeric) *or* [*TensorSSA*](cute.md#cutlass.cute.TensorSSA)) -- The value to shuffle

- **mask** (*Int*) -- A mask describing the threads participating in this operation

- **offset** (*Int*) -- A source lane or a source lane offset depending on kind

- **mask_and_clamp** (*Int*) -- An integer containing two packed values specifying a mask for logically splitting warps into sub-segments and an upper bound for clamping the source lane index.

- **kind** (*ShflKind*) -- The kind of shuffle, can be idx, up, down, or bfly

#### Returns

The shuffled value

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### shuffle_sync_down

**Kind:** function

**Qualified name:** `cutlass.cute.arch.shuffle_sync_down`

**Signature:** `shuffle_sync_down(value: cutlass.cute.typing.Numeric | TensorSSA, offset: cutlass.cute.typing.Int, mask: cutlass.cute.typing.Int = 4294967295, mask_and_clamp: cutlass.cute.typing.Int = 31, *, kind: _install_cutlass_mlir_autodoc_stub.<locals>._DocDialectObject = <class 'sphinx.ext.autodoc.mock._MockObject'>) → cutlass.cute.typing.Numeric | TensorSSA`

Shuffles a value within the threads of a warp.

#### Parameters

- **value** ([*Numeric*](../basic_data_types.md#cutlass.Numeric) *or* [*TensorSSA*](cute.md#cutlass.cute.TensorSSA)) -- The value to shuffle

- **mask** (*Int*) -- A mask describing the threads participating in this operation

- **offset** (*Int*) -- A source lane or a source lane offset depending on kind

- **mask_and_clamp** (*Int*) -- An integer containing two packed values specifying a mask for logically splitting warps into sub-segments and an upper bound for clamping the source lane index.

- **kind** (*ShflKind*) -- The kind of shuffle, can be idx, up, down, or bfly

#### Returns

The shuffled value

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### shuffle_sync_bfly

**Kind:** function

**Qualified name:** `cutlass.cute.arch.shuffle_sync_bfly`

**Signature:** `shuffle_sync_bfly(value: cutlass.cute.typing.Numeric | TensorSSA, offset: cutlass.cute.typing.Int, mask: cutlass.cute.typing.Int = 4294967295, mask_and_clamp: cutlass.cute.typing.Int = 31, *, kind: _install_cutlass_mlir_autodoc_stub.<locals>._DocDialectObject = <class 'sphinx.ext.autodoc.mock._MockObject'>) → cutlass.cute.typing.Numeric | TensorSSA`

Shuffles a value within the threads of a warp.

#### Parameters

- **value** ([*Numeric*](../basic_data_types.md#cutlass.Numeric) *or* [*TensorSSA*](cute.md#cutlass.cute.TensorSSA)) -- The value to shuffle

- **mask** (*Int*) -- A mask describing the threads participating in this operation

- **offset** (*Int*) -- A source lane or a source lane offset depending on kind

- **mask_and_clamp** (*Int*) -- An integer containing two packed values specifying a mask for logically splitting warps into sub-segments and an upper bound for clamping the source lane index.

- **kind** (*ShflKind*) -- The kind of shuffle, can be idx, up, down, or bfly

#### Returns

The shuffled value

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### barrier

**Kind:** function

**Qualified name:** `cutlass.cute.arch.barrier`

**Signature:** `barrier(*, barrier_id: cutlass.cute.typing.Int | None = None, number_of_threads: cutlass.cute.typing.Int | None = None) → None`

Creates a barrier, optionally named.

### barrier_arrive

**Kind:** function

**Qualified name:** `cutlass.cute.arch.barrier_arrive`

**Signature:** `barrier_arrive(*, barrier_id: cutlass.cute.typing.Int | None = None, number_of_threads: cutlass.cute.typing.Int | None = None, aligned: bool = True) → None`

Issue a non-blocking arrive on a CTA-scoped named barrier.

The PTX ISA distinguishes two flavors of the arrive instruction:

- `aligned=True` (default) emits `bar.arrive` (legacy syntax with implicit `.aligned`). All threads in the CTA must reach this instruction (i.e. it must lie outside divergent control flow).

- `aligned=False` emits `barrier.cta.arrive` (no `.aligned` modifier), which is required when the participating threads do not necessarily execute the same instruction (e.g. when only a subset of threads or warps in the CTA issue the arrive).

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#parallel-synchronization-and-communication-instructions-bar-barrier) for details on the aligned vs. unaligned variants.

### sync_threads

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sync_threads`

**Signature:** `sync_threads() → None`

Synchronizes all threads within a CTA.

### sync_warp

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sync_warp`

**Signature:** `sync_warp(mask: cutlass.cute.typing.Int = 4294967295) → None`

Performs a warp-wide sync with an optional mask.

### fence_acq_rel_cta

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_acq_rel_cta`

**Signature:** `fence_acq_rel_cta() → None`

Fence operation with acquire-release semantics at CTA (block) scope.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).

### fence_acq_rel_cluster

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_acq_rel_cluster`

**Signature:** `fence_acq_rel_cluster() → None`

Fence operation with acquire-release semantics at cluster scope.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).

### fence_acq_rel_gpu

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_acq_rel_gpu`

**Signature:** `fence_acq_rel_gpu() → None`

Fence operation with acquire-release semantics at GPU (device) scope.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).

### fence_acq_rel_sys

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_acq_rel_sys`

**Signature:** `fence_acq_rel_sys() → None`

Fence operation with acquire-release semantics at system scope.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-membar).

### cp_async_commit_group

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cp_async_commit_group`

**Signature:** `cp_async_commit_group() → None`

Commits all prior initiated but uncommitted cp.async instructions.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-commit-group).

### cp_async_wait_group

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cp_async_wait_group`

**Signature:** `cp_async_wait_group(n: cutlass.cute.typing.Int) → None`

Waits till only a specified numbers of cp.async groups are pending.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-wait-group-cp-async-wait-all).

### cp_async_bulk_commit_group

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cp_async_bulk_commit_group`

**Signature:** `cp_async_bulk_commit_group() → None`

Commits all prior initiated but uncommitted cp.async.bulk instructions.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk-commit-group).

### cp_async_bulk_wait_group

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cp_async_bulk_wait_group`

**Signature:** `cp_async_bulk_wait_group(group: cutlass.cute.typing.Int, *, read: bool | None = None) → None`

Waits till only a specified numbers of cp.async.bulk groups are pending.

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#data-movement-and-conversion-instructions-cp-async-bulk-wait-group).

### cluster_wait

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_wait`

**Signature:** `cluster_wait() → None`

A cluster-wide wait operation.

### cluster_arrive

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_arrive`

**Signature:** `cluster_arrive(*, aligned: bool | None = None) → None`

A cluster-wide arrive operation.

### cluster_arrive_relaxed

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cluster_arrive_relaxed`

**Signature:** `cluster_arrive_relaxed(*, aligned: bool | None = None) → None`

A cluster-wide arrive operation with relaxed semantics.

### vote_ballot_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.vote_ballot_sync`

**Signature:** `vote_ballot_sync(pred: cutlass.cute.typing.Boolean, mask: cutlass.cute.typing.Int = 4294967295) → cutlass.cute.typing.Int32`

Performs a ballot operation across the warp.

It copies the predicate from each thread in mask into the corresponding bit position of destination register d, where the bit position corresponds to the thread's lane id.

#### Parameters

- **pred** ([*Boolean*](../basic_data_types.md#cutlass.Boolean)) -- The predicate value for the current thread

- **mask** (*Int,* *optional*) -- A 32-bit integer mask specifying which threads participate, defaults to all threads (0xFFFFFFFF)

#### Returns

A 32-bit integer where each bit represents a thread's predicate value

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-vote-sync).

### vote_any_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.vote_any_sync`

**Signature:** `vote_any_sync(pred: cutlass.cute.typing.Boolean, mask: cutlass.cute.typing.Int = 4294967295) → cutlass.cute.typing.Boolean`

True if source predicate is True for any non-exited threads in mask. Negate the source predicate to compute .none.

#### Parameters

- **pred** ([*Boolean*](../basic_data_types.md#cutlass.Boolean)) -- The predicate value for the current thread

- **mask** (*Int,* *optional*) -- A 32-bit integer mask specifying which threads participate, defaults to all threads (0xFFFFFFFF)

#### Returns

A boolean value indicating if the source predicate is True for all non-exited threads in mask

#### Return type

[Boolean](../basic_data_types.md#cutlass.Boolean)

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-vote-sync).

### vote_all_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.vote_all_sync`

**Signature:** `vote_all_sync(pred: cutlass.cute.typing.Boolean, mask: cutlass.cute.typing.Int = 4294967295) → cutlass.cute.typing.Boolean`

True if source predicate is True for all non-exited threads in mask. Negate the source predicate to compute .none.

#### Parameters

- **pred** ([*Boolean*](../basic_data_types.md#cutlass.Boolean)) -- The predicate value for the current thread

- **mask** (*Int,* *optional*) -- A 32-bit integer mask specifying which threads participate, defaults to all threads (0xFFFFFFFF)

#### Returns

A boolean value indicating if the source predicate is True for all non-exited threads in mask

#### Return type

[Boolean](../basic_data_types.md#cutlass.Boolean)

See the [PTX documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-vote-sync).

### vote_uni_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.vote_uni_sync`

**Signature:** `vote_uni_sync(pred: cutlass.cute.typing.Boolean, mask: cutlass.cute.typing.Int = 4294967295) → cutlass.cute.typing.Boolean`

True f source predicate has the same value in all non-exited threads in mask. Negating the source predicate also computes .uni

#### Parameters

- **pred** ([*Boolean*](../basic_data_types.md#cutlass.Boolean)) -- The predicate value for the current thread

- **mask** (*Int,* *optional*) -- A 32-bit integer mask specifying which threads participate, defaults to all threads (0xFFFFFFFF)

#### Returns

A boolean value indicating if the source predicate is True for all non-exited threads in mask

#### Return type

[Boolean](../basic_data_types.md#cutlass.Boolean)

### warp_redux_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.warp_redux_sync`

**Signature:** `warp_redux_sync(value: cutlass.cute.typing.Numeric, kind: Literal['fmax', 'fmin', 'max', 'min', 'umax', 'umin', 'add', 'xor', 'or', 'and'], mask_and_clamp: cutlass.cute.typing.Int = 4294967295, *, abs: bool | None = None, nan: bool | None = None) → cutlass.cute.typing.Numeric`

Perform warp-level reduction operation across threads.

Reduces values from participating threads in a warp according to the specified operation. All threads in the mask receive the same result.

#### Parameters

- **value** ([*Numeric*](../basic_data_types.md#cutlass.Numeric)) -- Input value to reduce

- **kind** (*Literal\[\"add\",* *\"and\",* *\"max\",* *\"min\",* *\"umax\",* *\"umin\",* *\"or\",* *\"xor\",* *\"fmin\",* *\"fmax\"\]*) --

  Reduction operation. Supported operations:

  - Integer types (Int32/Uint32): "add", "and", "max", "min", "umax", "umin", "or", "xor" "max"/"min" auto-promote to "umax"/"umin" for unsigned types (Uint32/Uint64).

  - Float types (Float32): "fmax", "fmin" (or "max"/"min" which auto-convert to "fmax"/"fmin")

- **mask_and_clamp** (*Int*) -- Warp participation mask (default: FULL_MASK = 0xFFFFFFFF)

- **abs** (*bool*) -- Apply absolute value before reduction (float types only)

- **nan** (*Optional\[bool\]*) -- Enable NaN propagation for fmax/fmin operations (float types only)

#### Returns

Reduced value (same for all participating threads)

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### atomic_max_float32

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_max_float32`

**Signature:** `atomic_max_float32(ptr: ir.Value | cutlass.cute.typing.Pointer, value: cutlass.cute.typing.Float32, *, positive_only: bool = True) → cutlass.cute.typing.Float32`

Deprecated: use atomic_fmax instead.

### atomic_fmax

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_fmax`

**Signature:** `atomic_fmax(ptr: ir.Value | cutlass.cute.typing.Pointer, val: cutlass.cute.typing.Float32, *, sign_bit: bool | None = None, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → cutlass.cute.typing.Float32`

Atomically apply fmax through integer-bitcast atomics.

This wrapper handles `+inf`, `-inf`, and sign-bit-zero NaNs, including canonical NaN.

#### Parameters

- **ptr** (*Union\[ir.Value,* [*Pointer*](../basic_data_types.md#cutlass.Pointer)*\]*) -- Pointer to the memory location.

- **val** ([*Float32*](../basic_data_types.md#cutlass.Float32)) -- Value to combine with memory.

- **sign_bit** (*Optional\[bool\],* *optional*) -- Known sign bit of `val`, defaults to None. WARNING: if provided, it MUST match `val`'s actual sign --- a wrong value silently produces incorrect results (no type error). Leave as None unless the sign is a compile-time constant.

- **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\],* *optional*) -- Memory semantic, defaults to None.

- **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\],* *optional*) -- Memory scope, defaults to None.

#### Returns

Old value at `ptr`.

#### Return type

[Float32](../basic_data_types.md#cutlass.Float32)

### atomic_fmin

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_fmin`

**Signature:** `atomic_fmin(ptr: ir.Value | cutlass.cute.typing.Pointer, val: cutlass.cute.typing.Float32, *, sign_bit: bool | None = None, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → cutlass.cute.typing.Float32`

Atomically apply fmin through integer-bitcast atomics.

This wrapper handles `+inf`, `-inf`, and sign-bit-zero NaNs, including canonical NaN.

#### Parameters

- **ptr** (*Union\[ir.Value,* [*Pointer*](../basic_data_types.md#cutlass.Pointer)*\]*) -- Pointer to the memory location.

- **val** ([*Float32*](../basic_data_types.md#cutlass.Float32)) -- Value to combine with memory.

- **sign_bit** (*Optional\[bool\],* *optional*) -- Known sign bit of `val`, defaults to None. WARNING: if provided, it MUST match `val`'s actual sign --- a wrong value silently produces incorrect results (no type error). Leave as None unless the sign is a compile-time constant.

- **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\],* *optional*) -- Memory semantic, defaults to None.

- **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\],* *optional*) -- Memory scope, defaults to None.

#### Returns

Old value at `ptr`.

#### Return type

[Float32](../basic_data_types.md#cutlass.Float32)

### atomic_add

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_add`

**Signature:** `atomic_add(ptr: ir.Value | cutlass.cute.typing.Pointer, val: cutlass.cute.typing.Numeric | ir.Value, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → cutlass.cute.typing.Numeric | ir.Value`

Performs an atomic addition operation.

Atomically adds val to the value at memory location ptr and returns the old value.

#### Parameters

- **ptr** -- Pointer to memory location

- **val** (*Union\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*,* *ir.Value\]*) -- Value to add (scalar Numeric or vector ir.Value)

- **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

- **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

Union\[[Numeric](../basic_data_types.md#cutlass.Numeric), ir.Value\]

### atomic_and

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_and`

**Signature:** `atomic_and(ptr: ir.Value | cutlass.cute.typing.Pointer, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → cutlass.cute.typing.Numeric`

Performs an atomic bitwise AND operation.

Atomically computes bitwise AND of val with the value at memory location ptr and returns the old value.

#### Parameters

- **ptr** -- Pointer to memory location

- **val** ([*Numeric*](../basic_data_types.md#cutlass.Numeric)) -- Value for AND operation

- **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

- **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### atomic_or

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_or`

**Signature:** `atomic_or(ptr: ir.Value | cutlass.cute.typing.Pointer, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → cutlass.cute.typing.Numeric`

Performs an atomic bitwise OR operation.

Atomically computes bitwise OR of val with the value at memory location ptr and returns the old value.

#### Parameters

- **ptr** -- Pointer to memory location

- **val** ([*Numeric*](../basic_data_types.md#cutlass.Numeric)) -- Value for OR operation

- **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

- **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### atomic_xor

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_xor`

**Signature:** `atomic_xor(ptr: ir.Value | cutlass.cute.typing.Pointer, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → cutlass.cute.typing.Numeric`

Performs an atomic bitwise XOR operation.

Atomically computes bitwise XOR of val with the value at memory location ptr and returns the old value.

#### Parameters

- **ptr** -- Pointer to memory location

- **val** ([*Numeric*](../basic_data_types.md#cutlass.Numeric)) -- Value for XOR operation

- **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

- **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### atomic_max

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_max`

**Signature:** `atomic_max(ptr: ir.Value | cutlass.cute.typing.Pointer, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → cutlass.cute.typing.Numeric`

Performs an atomic maximum operation.

Atomically computes maximum of val and the value at memory location ptr and returns the old value.

#### Parameters

- **ptr** -- Pointer to memory location

- **val** ([*Numeric*](../basic_data_types.md#cutlass.Numeric)) -- Value for MAX operation

- **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

- **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### atomic_min

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_min`

**Signature:** `atomic_min(ptr: ir.Value | cutlass.cute.typing.Pointer, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → cutlass.cute.typing.Numeric`

Performs an atomic minimum operation.

Atomically computes minimum of val and the value at memory location ptr and returns the old value.

#### Parameters

- **ptr** -- Pointer to memory location

- **val** ([*Numeric*](../basic_data_types.md#cutlass.Numeric)) -- Value for MIN operation

- **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

- **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### atomic_exch

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_exch`

**Signature:** `atomic_exch(ptr: ir.Value | cutlass.cute.typing.Pointer, val: cutlass.cute.typing.Numeric, *, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → cutlass.cute.typing.Numeric`

Performs an atomic exchange operation.

Atomically exchanges val with the value at memory location ptr and returns the old value.

#### Parameters

- **ptr** -- Pointer to memory location

- **val** ([*Numeric*](../basic_data_types.md#cutlass.Numeric)) -- Value to exchange

- **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

- **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### atomic_cas

**Kind:** function

**Qualified name:** `cutlass.cute.arch.atomic_cas`

**Signature:** `atomic_cas(ptr: ir.Value | cutlass.cute.typing.Pointer, *, cmp: cutlass.cute.typing.Numeric, val: cutlass.cute.typing.Numeric, sem: Literal['relaxed', 'release', 'acquire', 'acq_rel'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → cutlass.cute.typing.Numeric`

Performs an atomic compare-and-swap (CAS) operation.

Atomically compares the value at the memory location with cmp. If they are equal, stores val at the memory location and returns the old value.

#### Parameters

- **ptr** -- Pointer to memory location. Supports: - ir.Value (LLVM pointer) - cute.ptr (\_Pointer instance)

- **cmp** ([*Numeric*](../basic_data_types.md#cutlass.Numeric)) -- Value to compare against current memory value

- **val** ([*Numeric*](../basic_data_types.md#cutlass.Numeric)) -- Value to store if comparison succeeds

- **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\",* *\"acquire\",* *\"acq_rel\"\]\]*) -- Memory semantic ("relaxed", "release", "acquire", "acq_rel")

- **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope ("gpu", "cta", "cluster", "sys")

#### Returns

Old value at memory location

#### Return type

[Numeric](../basic_data_types.md#cutlass.Numeric)

### store

**Kind:** function

**Qualified name:** `cutlass.cute.arch.store`

**Signature:** `store(ptr: ir.Value | cutlass.cute.typing.Pointer, val: cutlass.cute.typing.Numeric | ir.Value, *, level1_eviction_priority: Literal['evict_normal', 'evict_first', 'evict_last', 'evict_no_allocate', 'evict_unchanged'] | None = None, cop: Literal['wb', 'cg', 'cs', 'wt'] | None = None, ss: Literal['cta', 'cluster'] | None = None, sem: Literal['relaxed', 'release'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → None`

Store a value to a memory location.

#### Parameters

- **ptr** -- Pointer to store to. Supports: - ir.Value (LLVM pointer) - cute.ptr (\_Pointer instance)

- **val** (*Union\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*,* *ir.Value\]*) -- Value to store (scalar Numeric or vector ir.Value)

- **level1_eviction_priority** -- L1 cache eviction policy string literal: "evict_normal" : .level1::eviction_priority = .L1::evict_normal "evict_first" : .level1::eviction_priority = .L1::evict_first "evict_last" : .level1::eviction_priority = .L1::evict_last "evict_no_allocate" : .level1::eviction_priority = .L1::no_allocate "evict_unchanged" : .level1::eviction_priority = .L1::evict_unchanged

- **cop** -- Store cache modifier string literal:

- **ss** -- Shared memory space string literal: "cta" : .ss = .shared::cta "cluster" : .ss = .shared::cluster None : .ss = .global

- **sem** -- Memory semantic string literal:

- **scope** -- Memory scope string literal:

### load

**Kind:** function

**Qualified name:** `cutlass.cute.arch.load`

**Signature:** `load(ptr: ir.Value | cutlass.cute.typing.Pointer, dtype: type[cutlass.cute.typing.Numeric] | _install_cutlass_mlir_autodoc_stub.<locals>._DocMlirType, *, sem: ~typing.Literal['relaxed', 'acquire'] | None = None, scope: ~typing.Literal['gpu', 'cta', 'cluster', 'sys'] | None = None, level1_eviction_priority: ~typing.Literal['evict_normal', 'evict_first', 'evict_last', 'evict_no_allocate', 'evict_unchanged'] | None = None, cop: ~typing.Literal['ca', 'cg', 'cs', 'lu', 'cv'] | None = None, ss: ~typing.Literal['cta', 'cluster'] | None = None, level_prefetch_size: ~typing.Literal['size_64b', 'size_128b', 'size_256b'] | None = None) → cutlass.cute.typing.Numeric | ir.Value`

Load a value from a memory location.

#### Parameters

- **ptr** -- Pointer to load from. Supports: - ir.Value (LLVM pointer) - cute.ptr (\_Pointer instance)

- **dtype** (*Union\[type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\],* *ir.VectorType\]*) -- Data type to load. Can be: - Scalar: Numeric type class (Int8, Uint8, Int32, Float32, etc.) - Vector: ir.VectorType for vectorized load (e.g., ir.VectorType.get(\[4\], Int64.mlir_type))

- **sem** -- Memory semantic string literal:

- **scope** -- Memory scope string literal:

- **level1_eviction_priority** -- L1 cache eviction policy string literal: "evict_normal" : .level1::eviction_priority = .L1::evict_normal "evict_first" : .level1::eviction_priority = .L1::evict_first "evict_last" : .level1::eviction_priority = .L1::evict_last "evict_no_allocate" : .level1::eviction_priority = .L1::no_allocate "evict_unchanged" : .level1::eviction_priority = .L1::evict_unchanged

- **cop** -- Load cache modifier string literal:

- **ss** -- Shared memory space string literal: "cta" : .ss = .shared::cta "cluster" : .ss = .shared::cluster None : .ss = .global

- **level_prefetch_size** -- L2 cache prefetch size hint string literal: "size_64b" : .level::prefetch_size = .L2::64B "size_128b" : .level::prefetch_size = .L2::128B "size_256b" : .level::prefetch_size = .L2::256B

#### Returns

Loaded value (scalar Numeric or vector ir.Value)

#### Return type

Union\[[Numeric](../basic_data_types.md#cutlass.Numeric), ir.Value\]

### red

**Kind:** function

**Qualified name:** `cutlass.cute.arch.red`

**Signature:** `red(ptr: ir.Value | cutlass.cute.typing.Pointer, val: cutlass.cute.typing.Numeric | ir.Value, *, op: Literal['add', 'min', 'max', 'umin', 'umax', 'and', 'or', 'xor'], dtype: Literal['b32', 'b64', 'u32', 'u64', 's32', 's64', 'f32', 'f64', 'f16', 'f16x2', 'bf16', 'bf16x2'] | type[cutlass.cute.typing.Numeric], sem: Literal['relaxed', 'release'] | None = None, scope: Literal['gpu', 'cta', 'cluster', 'sys'] | None = None) → None`

Perform an atomic reduction operation on a memory location.

Atomically computes: ptr = ptr x val, where x is the reduction operation.

#### Parameters

- **ptr** -- Pointer to memory location (global or shared). Supports: - ir.Value (LLVM pointer) - cute.ptr (\_Pointer instance)

- **val** (*Union\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*,* *ir.Value\]*) -- Value to reduce with the memory location (scalar Numeric or vector ir.Value)

- **op** (*Literal\[\"add\",* *\"min\",* *\"max\",* *\"umin\",* *\"umax\",* *\"and\",* *\"or\",* *\"xor\"\]*) -- Reduction operation string literal: "add" : Addition "min" : Minimum (signedness determined by dtype) "max" : Maximum (signedness determined by dtype) "umin" : Unsigned minimum (alias for "min", forces dtype to unsigned) "umax" : Unsigned maximum (alias for "max", forces dtype to unsigned) "and" : Bitwise AND "or" : Bitwise OR "xor" : Bitwise XOR

- **dtype** (*Union\[str,* *type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]\]*) -- Data type. Supports string literals ("b32", "b64", "u32", "u64", "s32", "s64", "f32", "f64", "f16", "f16x2", "bf16", "bf16x2") or cutlass types (Uint32, Uint64, Int32, Int64, Float32, Float64, Float16, BFloat16)

- **sem** (*Optional\[Literal\[\"relaxed\",* *\"release\"\]\]*) -- Memory ordering semantics string literal: "relaxed" : Relaxed memory ordering "release" : Release memory ordering None : No memory ordering specified

- **scope** (*Optional\[Literal\[\"gpu\",* *\"cta\",* *\"cluster\",* *\"sys\"\]\]*) -- Memory scope string literal: "gpu" : GPU scope "cta" : CTA/block scope "cluster" : Cluster scope "sys" : System scope None : No scope specified

#### Returns

None (operation modifies memory in-place)

#### Return type

None

> **Note.**
>
> This operation modifies memory in-place and returns None. The old value is NOT returned (unlike atomic_add, atomic_max, etc.). For operations that need the old value, use the atomic\_\* functions instead.

### popc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.popc`

**Signature:** `popc(value: cutlass.cute.typing.Numeric) → cutlass.cute.typing.Numeric`

Performs a population count operation.

### fence_proxy

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_proxy`

**Signature:** `fence_proxy(kind: Literal['alias', 'async', 'async.global', 'async.shared', 'tensormap', 'generic'], *, space: Literal['cta', 'cluster'] | None = None, use_intrinsic: bool | None = None) → None`

Fence operation to ensure memory consistency between proxies.

#### Parameters

- **kind** (*Literal\[\"alias\",* *\"async\",* *\"async.global\",* *\"async.shared\",* *\"tensormap\",* *\"generic\"\]*) -- Proxy kind string literal: - "alias" : Alias proxy - "async" : Async proxy - "async.global" : Async global proxy - "async.shared" : Async shared proxy - "tensormap" : Tensormap proxy - "generic" : Generic proxy

- **space** (*Optional\[Literal\[\"cta\",* *\"cluster\"\]\]*) -- Shared memory space scope string literal (optional): - "cta" : CTA (Cooperative Thread Array) scope - "cluster" : Cluster scope

- **use_intrinsic** -- Whether to use intrinsic version

### fence_view_async_tmem_load

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_view_async_tmem_load`

**Signature:** `fence_view_async_tmem_load(*, kind: Literal['load', 'store'] = 'load') → None`

Perform a fence operation on the async TMEM load or store.

> **Note.**
>
> This function is only available on sm_100a and above. The fence is required to synchronize the TMEM load/store and let the pipeline release or commit the buffer.
>
> Take a mma2acc pipeline as an example of LOAD fence, the ACC tensor is from TMEM. `` ` ` # Start to copy ACC from TMEM to register cute.copy(tmem_load, tACC, rACC) fence_view_async_tmem_load() # After fence, we can ensure the TMEM buffer is consumed totally. # Release the buffer to let the MMA know it can overwrite the buffer. mma2accum_pipeline.consumer_release(curr_consumer_state) ` ` `` Take a TS GEMM kernel as an example of STORE fence, the A tensor is from TMEM. `` ` ` # Start to copy A from register to TMEM cute.copy(tmem_store, rA, tA) fence_view_async_tmem_store() # After fence, we can ensure the TMEM buffer is ready. # Commit the buffer to let the MMA know it can start to load A. tmem_mma_pipeline.producer_commit(curr_producer_state) ` ` ``

#### Parameters

**kind** (*Literal\[\"load\",* *\"store\"\]*) -- The kind of fence operation to perform ("load", "store").

### fence_view_async_tmem_store

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fence_view_async_tmem_store`

**Signature:** `fence_view_async_tmem_store(*, kind: Literal['load', 'store'] = 'store') → None`

Perform a fence operation on the async TMEM load or store.

> **Note.**
>
> This function is only available on sm_100a and above. The fence is required to synchronize the TMEM load/store and let the pipeline release or commit the buffer.
>
> Take a mma2acc pipeline as an example of LOAD fence, the ACC tensor is from TMEM. `` ` ` # Start to copy ACC from TMEM to register cute.copy(tmem_load, tACC, rACC) fence_view_async_tmem_load() # After fence, we can ensure the TMEM buffer is consumed totally. # Release the buffer to let the MMA know it can overwrite the buffer. mma2accum_pipeline.consumer_release(curr_consumer_state) ` ` `` Take a TS GEMM kernel as an example of STORE fence, the A tensor is from TMEM. `` ` ` # Start to copy A from register to TMEM cute.copy(tmem_store, rA, tA) fence_view_async_tmem_store() # After fence, we can ensure the TMEM buffer is ready. # Commit the buffer to let the MMA know it can start to load A. tmem_mma_pipeline.producer_commit(curr_producer_state) ` ` ``

#### Parameters

**kind** (*Literal\[\"load\",* *\"store\"\]*) -- The kind of fence operation to perform ("load", "store").

### warpgroup_reg_alloc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.warpgroup_reg_alloc`

**Signature:** `warpgroup_reg_alloc(reg_count: int) → None`

### warpgroup_reg_dealloc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.warpgroup_reg_dealloc`

**Signature:** `warpgroup_reg_dealloc(reg_count: int) → None`

### setmaxregister_increase

**Kind:** function

**Qualified name:** `cutlass.cute.arch.setmaxregister_increase`

**Signature:** `setmaxregister_increase(reg_count: int) → None`

### setmaxregister_decrease

**Kind:** function

**Qualified name:** `cutlass.cute.arch.setmaxregister_decrease`

**Signature:** `setmaxregister_decrease(reg_count: int) → None`

### fma_packed_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fma_packed_f32x2`

**Signature:** `fma_packed_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], src_c: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32] | None, *, calc_func: ~typing.Callable = <class '_DocDialectObject'>, result_type_inferred: bool = True, rnd: ~typing.Literal['rn', 'rz', 'rm', 'rp', 'none'] | None = 'rn', ftz: bool | None = None) → Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32]`

### fma_f16

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fma_f16`

**Signature:** `fma_f16(a: cutlass.cute.typing.Numeric, b: cutlass.cute.typing.Numeric, c: cutlass.cute.typing.Numeric, *, rnd: Literal['rn'] | None = 'rn', ftz: bool | None = None, sat: bool | None = None, relu: bool | None = None, oob: bool | None = None) → cutlass.cute.typing.Float16`

Fused multiply-add for f16 values via `nvvm.fma`.

### fma_bf16

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fma_bf16`

**Signature:** `fma_bf16(a: cutlass.cute.typing.Numeric, b: cutlass.cute.typing.Numeric, c: cutlass.cute.typing.Numeric, *, rnd: Literal['rn'] | None = 'rn', relu: bool | None = None, oob: bool | None = None) → cutlass.cute.typing.BFloat16`

Fused multiply-add for bf16 values via `nvvm.fma`.

### fma_packed_f16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fma_packed_f16x2`

**Signature:** `fma_packed_f16x2(a: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], b: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], c: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], *, rnd: Literal['rn'] | None = 'rn', ftz: bool | None = None, sat: bool | None = None, relu: bool | None = None, oob: bool | None = None) → cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Float16, cutlass.cute.typing.Float16]`

Fused multiply-add for packed f16x2 values via `nvvm.fma`.

### fma_packed_bf16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fma_packed_bf16x2`

**Signature:** `fma_packed_bf16x2(a: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], b: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], c: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], *, rnd: Literal['rn'] | None = 'rn', relu: bool | None = None, oob: bool | None = None) → cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.BFloat16, cutlass.cute.typing.BFloat16]`

Fused multiply-add for packed bf16x2 values via `nvvm.fma`.

### fma_packed_f32x2_f16x2_f32x2_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fma_packed_f32x2_f16x2_f32x2_f32x2`

**Signature:** `fma_packed_f32x2_f16x2_f32x2_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], src_c: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, calc_func: ~typing.Callable = <class '_DocDialectObject'>, dtype: ~typing.Type[cutlass.cute.typing.Numeric] = cutlass.cute.typing.Float16, rnd: ~typing.Literal['rn', 'rz', 'rm', 'rp', 'none'] | None = 'rn') → Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32]`

### fma_packed_f32x2_bf16x2_f32x2_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fma_packed_f32x2_bf16x2_f32x2_f32x2`

**Signature:** `fma_packed_f32x2_bf16x2_f32x2_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], src_c: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, calc_func: ~typing.Callable = <class '_DocDialectObject'>, dtype: ~typing.Type[cutlass.cute.typing.Numeric] = cutlass.cute.typing.BFloat16, rnd: ~typing.Literal['rn', 'rz', 'rm', 'rp', 'none'] | None = 'rn') → Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32]`

### mul_packed_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mul_packed_f32x2`

**Signature:** `mul_packed_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, src_c: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32] | None = None, calc_func: ~typing.Callable = <class '_DocDialectObject'>, result_type_inferred: bool = True, rnd: ~typing.Literal['rn', 'rz', 'rm', 'rp', 'none'] | None = 'rn', ftz: bool | None = None) → Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32]`

### add_packed_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.add_packed_f32x2`

**Signature:** `add_packed_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, src_c: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32] | None = None, calc_func: ~typing.Callable = <class '_DocDialectObject'>, result_type_inferred: bool = True, rnd: ~typing.Literal['rn', 'rz', 'rm', 'rp', 'none'] | None = 'rn', ftz: bool | None = None) → Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32]`

### add_packed_f16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.add_packed_f16x2`

**Signature:** `add_packed_f16x2(a: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], b: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], *, rnd: Literal['rn'] | None = None, ftz: bool | None = None, sat: bool | None = None, predicate: cutlass.cute.typing.Boolean | None = None) → cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric]`

Add packed f16x2 values with PTX `add{.rnd}{.ftz}{.sat}.f16x2`.

### add_packed_bf16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.add_packed_bf16x2`

**Signature:** `add_packed_bf16x2(a: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], b: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], *, rnd: Literal['rn'] | None = None, predicate: cutlass.cute.typing.Boolean | None = None) → cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric]`

Add packed bf16x2 values with PTX `add{.rnd}.bf16x2`.

### add_packed_f16x2_f32x2_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.add_packed_f16x2_f32x2_f32x2`

**Signature:** `add_packed_f16x2_f32x2_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, calc_func: ~typing.Callable = <class '_DocDialectObject'>, dtype: ~typing.Type[~cutlass.cute.arch.nvvm_wrappers._NumericT] = cutlass.cute.typing.Float16) → Tuple[_NumericT, _NumericT]`

### add_packed_bf16x2_f32x2_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.add_packed_bf16x2_f32x2_f32x2`

**Signature:** `add_packed_bf16x2_f32x2_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, calc_func: ~typing.Callable = <class '_DocDialectObject'>, dtype: ~typing.Type[~cutlass.cute.arch.nvvm_wrappers._NumericT] = cutlass.cute.typing.BFloat16) → Tuple[_NumericT, _NumericT]`

### add_packed_f32x2_f16x2_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.add_packed_f32x2_f16x2_f32x2`

**Signature:** `add_packed_f32x2_f16x2_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, calc_func: ~typing.Callable = <class '_DocDialectObject'>, dtype: ~typing.Type[cutlass.cute.typing.Numeric] = cutlass.cute.typing.Float16, rnd: ~typing.Literal['rn', 'rz', 'rm', 'rp', 'none'] | None = 'rn') → Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32]`

### add_packed_f32x2_bf16x2_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.add_packed_f32x2_bf16x2_f32x2`

**Signature:** `add_packed_f32x2_bf16x2_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, calc_func: ~typing.Callable = <class '_DocDialectObject'>, dtype: ~typing.Type[cutlass.cute.typing.Numeric] = cutlass.cute.typing.BFloat16, rnd: ~typing.Literal['rn', 'rz', 'rm', 'rp', 'none'] | None = 'rn') → Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32]`

### sub_packed_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sub_packed_f32x2`

**Signature:** `sub_packed_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, src_c: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32] | None = None, calc_func: ~typing.Callable = <class '_DocDialectObject'>, result_type_inferred: bool = True, rnd: ~typing.Literal['rn', 'rz', 'rm', 'rp', 'none'] | None = 'rn', ftz: bool | None = None) → Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32]`

### sub_packed_f16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sub_packed_f16x2`

**Signature:** `sub_packed_f16x2(a: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], b: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], *, rnd: Literal['rn'] | None = None, ftz: bool | None = None, sat: bool | None = None, predicate: cutlass.cute.typing.Boolean | None = None) → cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric]`

Subtract packed f16x2 values with PTX sub{.rn}{.ftz}{.sat}.f16x2.

### sub_packed_bf16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sub_packed_bf16x2`

**Signature:** `sub_packed_bf16x2(a: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], b: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], *, rnd: Literal['rn'] | None = None, ftz: bool | None = None, sat: bool | None = None, predicate: cutlass.cute.typing.Boolean | None = None) → cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric]`

Subtract packed bf16x2 values with PTX sub{.rn}.bf16x2.

### sub_packed_f16x2_f32x2_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sub_packed_f16x2_f32x2_f32x2`

**Signature:** `sub_packed_f16x2_f32x2_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, calc_func: ~typing.Callable = <class '_DocDialectObject'>, dtype: ~typing.Type[~cutlass.cute.arch.nvvm_wrappers._NumericT] = cutlass.cute.typing.Float16) → Tuple[_NumericT, _NumericT]`

### sub_packed_bf16x2_f32x2_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sub_packed_bf16x2_f32x2_f32x2`

**Signature:** `sub_packed_bf16x2_f32x2_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, calc_func: ~typing.Callable = <class '_DocDialectObject'>, dtype: ~typing.Type[~cutlass.cute.arch.nvvm_wrappers._NumericT] = cutlass.cute.typing.BFloat16) → Tuple[_NumericT, _NumericT]`

### sub_packed_f32x2_f16x2_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sub_packed_f32x2_f16x2_f32x2`

**Signature:** `sub_packed_f32x2_f16x2_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, calc_func: ~typing.Callable = <class '_DocDialectObject'>, dtype: ~typing.Type[cutlass.cute.typing.Numeric] = cutlass.cute.typing.Float16, rnd: ~typing.Literal['rn', 'rz', 'rm', 'rp', 'none'] | None = 'rn') → Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32]`

### sub_packed_f32x2_bf16x2_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sub_packed_f32x2_bf16x2_f32x2`

**Signature:** `sub_packed_f32x2_bf16x2_f32x2(src_a: ~typing.Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], src_b: ~typing.Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32], *, calc_func: ~typing.Callable = <class '_DocDialectObject'>, dtype: ~typing.Type[cutlass.cute.typing.Numeric] = cutlass.cute.typing.BFloat16, rnd: ~typing.Literal['rn', 'rz', 'rm', 'rp', 'none'] | None = 'rn') → Tuple[cutlass.cute.typing.Float32, cutlass.cute.typing.Float32]`

### fmax

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fmax`

**Signature:** `fmax(a: float | cutlass.cute.typing.Float32, b: float | cutlass.cute.typing.Float32, *, abs: bool = False, nan: bool = False, ftz: bool = False) → cutlass.cute.typing.Float32`

Floating-point max via `nvvm.fmax` (FMNMX, not `arith.max`/SEL).

#### Parameters

- **abs** -- When True, lower to the xorsign-abs form `sign(a ^ b) * max(|a|, |b|)` (FMNMX.XORSIGN.ABS); default False is a plain max.

- **nan** -- When True, propagate NaN following IEEE 754 `maximum` (FMNMX.NaN); default False keeps the NaN-quiet `maximumNumber` behavior of the underlying NVVM op.

- **ftz** -- When True, flush denormal inputs and outputs to zero (FMNMX.FTZ); default False preserves denormals.

### fmin

**Kind:** function

**Qualified name:** `cutlass.cute.arch.fmin`

**Signature:** `fmin(a: float | cutlass.cute.typing.Float32, b: float | cutlass.cute.typing.Float32, *, abs: bool = False, nan: bool = False, ftz: bool = False) → cutlass.cute.typing.Float32`

Floating-point min via `nvvm.fmin` (FMNMX, not `arith.min`/SEL).

#### Parameters

- **abs** -- When True, lower to the xorsign-abs form `sign(a ^ b) * min(|a|, |b|)` (FMNMX.XORSIGN.ABS); default False is a plain min.

- **nan** -- When True, propagate NaN following IEEE 754 `minimum` (FMNMX.NaN); default False keeps the NaN-quiet `minimumNumber` behavior of the underlying NVVM op.

- **ftz** -- When True, flush denormal inputs and outputs to zero (FMNMX.FTZ); default False preserves denormals.

### rcp_approx

**Kind:** function

**Qualified name:** `cutlass.cute.arch.rcp_approx`

**Signature:** `rcp_approx(a: float | cutlass.cute.typing.Float32) → cutlass.cute.typing.Float32`

### exp2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.exp2`

**Signature:** `exp2(a: float | cutlass.cute.typing.Float32) → cutlass.cute.typing.Float32`

### exp2_packed_f16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.exp2_packed_f16x2`

**Signature:** `exp2_packed_f16x2(a: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], *, predicate: cutlass.cute.typing.Boolean | None = None) → cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Float16, cutlass.cute.typing.Float16]`

Approximate base-2 exponential for packed f16x2 values.

Lowers to PTX `ex2.approx.f16x2`.

### exp2_packed_bf16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.exp2_packed_bf16x2`

**Signature:** `exp2_packed_bf16x2(a: cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.Numeric, cutlass.cute.typing.Numeric], *, predicate: cutlass.cute.typing.Boolean | None = None) → cutlass.cute.typing.Int32 | Tuple[cutlass.cute.typing.BFloat16, cutlass.cute.typing.BFloat16]`

Approximate base-2 exponential for packed bf16x2 values.

Lowers to PTX `ex2.approx.ftz.bf16x2`.

### cvt_i8x4_to_f32x4

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8x4_to_f32x4`

**Signature:** `cvt_i8x4_to_f32x4(src_vec4: ir.Value) → ir.Value`

### cvt_i8x2_to_f32x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8x2_to_f32x2`

**Signature:** `cvt_i8x2_to_f32x2(src_vec2: ir.Value) → ir.Value`

### cvt_i8_bf16

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8_bf16`

**Signature:** `cvt_i8_bf16(src_i8: ir.Value) → ir.Value`

### cvt_i8x2_to_bf16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8x2_to_bf16x2`

**Signature:** `cvt_i8x2_to_bf16x2(src_vec2: ir.Value) → ir.Value`

### cvt_i8x4_to_bf16x4

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8x4_to_bf16x4`

**Signature:** `cvt_i8x4_to_bf16x4(src_vec4: ir.Value) → ir.Value`

### cvt_f32_tf32

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_f32_tf32`

**Signature:** `cvt_f32_tf32(src_f32: float | cutlass.cute.typing.Float32 | ir.Value) → ir.Value`

Convert one f32 value to a TF32 MMA operand register.

### cvt_f32x2_bf16x2

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_f32x2_bf16x2`

**Signature:** `cvt_f32x2_bf16x2(src_vec2: ir.Value) → ir.Value`

### smid

**Kind:** function

**Qualified name:** `cutlass.cute.arch.smid`

**Signature:** `smid() → cutlass.cute.typing.Int32`

Returns the SM (Streaming Multiprocessor) ID of the current thread.

The SM ID is a unique identifier for the streaming multiprocessor executing the current thread. Valid range is 0 to nsmid() - 1.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-smid>

#### Returns

SM ID of the current thread

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### nsmid

**Kind:** function

**Qualified name:** `cutlass.cute.arch.nsmid`

**Signature:** `nsmid() → cutlass.cute.typing.Int32`

Returns the number of SMs (Streaming Multiprocessors) on the device.

This returns the total count of SMs available on the GPU, which defines the valid range for smid() as \[0, nsmid() - 1\].

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-nsmid>

#### Returns

Total number of SMs on the device

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### total_smem_size

**Kind:** function

**Qualified name:** `cutlass.cute.arch.total_smem_size`

**Signature:** `total_smem_size() → cutlass.cute.typing.Int32`

Returns the `%total_smem_size` special-register value for the CTA.

PTX defines this as the total statically and dynamically allocated CTA shared memory, excluding the reserved system region, reported in multiples of the target architecture's shared-memory allocation unit.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-total-smem-size>

#### Returns

Total CTA shared-memory size in target allocation units

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### aggr_smem_size

**Kind:** function

**Qualified name:** `cutlass.cute.arch.aggr_smem_size`

**Signature:** `aggr_smem_size() → cutlass.cute.typing.Int32`

Returns the `%aggr_smem_size` special-register value for the CTA (sm_90+).

PTX defines this as user shared memory plus the reserved system shared-memory region for the CTA.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-aggr-smem-size>

#### Returns

Aggregate CTA shared-memory size

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### gridid

**Kind:** function

**Qualified name:** `cutlass.cute.arch.gridid`

**Signature:** `gridid() → cutlass.cute.typing.Int32`

Returns a unique identifier for the current grid launch.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-gridid>

#### Returns

Grid launch identifier

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### nwarpid

**Kind:** function

**Qualified name:** `cutlass.cute.arch.nwarpid`

**Signature:** `nwarpid() → cutlass.cute.typing.Int32`

Returns the maximum number of warp slots (warp IDs) per SM.

Defines the valid range for physical_warp_id() as \[0, nwarpid() - 1\].

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-nwarpid>

#### Returns

Number of warp slots per SM

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### warpsize

**Kind:** function

**Qualified name:** `cutlass.cute.arch.warpsize`

**Signature:** `warpsize() → cutlass.cute.typing.Int32`

Returns the warp size in threads as a runtime register read (`%WARP_SZ`).

Prefer the compile-time `WARP_SIZE` constant unless a runtime read is specifically required; both are 32 on current hardware.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-warpid-nwarpid-warpsize>

#### Returns

Warp size in threads

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### globaltimer

**Kind:** function

**Qualified name:** `cutlass.cute.arch.globaltimer`

**Signature:** `globaltimer() → cutlass.cute.typing.Int64`

Returns the 64-bit global nanosecond timer (`%globaltimer`).

Intended for use by NVIDIA tools: the behavior is target-specific and may change or be removed on future architectures.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-globaltimer-globaltimer-lo-globaltimer-hi>

#### Returns

Global timer value in nanoseconds

#### Return type

[Int64](../basic_data_types.md#cutlass.Int64)

### globaltimer_lo

**Kind:** function

**Qualified name:** `cutlass.cute.arch.globaltimer_lo`

**Signature:** `globaltimer_lo() → cutlass.cute.typing.Int32`

Returns the low 32 bits of the global nanosecond timer (`%globaltimer_lo`).

#### Returns

Low 32 bits of the global timer

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### clock

**Kind:** function

**Qualified name:** `cutlass.cute.arch.clock`

**Signature:** `clock() → cutlass.cute.typing.Int32`

Returns a 32-bit clock counter value.

Reads the per-SM clock counter, which can be used for timing and profiling. The counter wraps around on overflow. For extended range, use clock64().

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-clock>

#### Returns

32-bit clock counter value

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### clock64

**Kind:** function

**Qualified name:** `cutlass.cute.arch.clock64`

**Signature:** `clock64() → cutlass.cute.typing.Int64`

Returns a 64-bit clock counter value.

Reads the per-SM 64-bit clock counter, providing extended range compared to the 32-bit clock(). Useful for timing longer operations without overflow.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-clock64>

#### Returns

64-bit clock counter value

#### Return type

[Int64](../basic_data_types.md#cutlass.Int64)

### match_sync

**Kind:** function

**Qualified name:** `cutlass.cute.arch.match_sync`

**Signature:** `match_sync(mask: int | cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, value: int | cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, kind: Literal['any', 'all'] = 'any') → cutlass.cute.typing.Uint32`

Finds threads in a warp with matching values using warp-synchronous matching.

Performs a broadcast and compare of the operand value across threads specified by the mask. Returns a mask indicating which threads have matching values.

- "any" mode: Returns mask of threads that have the same value as any other thread

- "all" mode: Returns mask of threads where all active threads have the same value

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-match-sync>

#### Parameters

- **mask** (*Union\[int,* [*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Mask of participating threads (typically 0xFFFFFFFF for full warp)

- **value** (*Union\[int,* [*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Value to match across threads

- **kind** (*Literal\[\"any\",* *\"all\"\]*) -- Match mode - "any" or "all"

#### Returns

Mask of threads with matching values

#### Return type

[Uint32](../basic_data_types.md#cutlass.Uint32)

### clz

**Kind:** function

**Qualified name:** `cutlass.cute.arch.clz`

**Signature:** `clz(value: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64) → cutlass.cute.typing.Uint32`

Counts the number of leading zero bits (count leading zeros).

<https://docs.nvidia.com/cuda/parallel-thread-execution/#integer-arithmetic-instructions-clz>

Returns the number of consecutive zero bits starting from the most significant bit. For a 32-bit value, returns a value in range \[0, 32\]. For 64-bit, range is \[0, 64\].

#### Parameters

**value** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*,* [*Int64*](../basic_data_types.md#cutlass.Int64)*,* [*Uint64*](../basic_data_types.md#cutlass.Uint64)*\]*) -- Input value (32-bit or 64-bit integer)

#### Returns

Count of leading zero bits (same bit width as input)

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Int64](../basic_data_types.md#cutlass.Int64)\]

### bfind

**Kind:** function

**Qualified name:** `cutlass.cute.arch.bfind`

**Signature:** `bfind(value: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64) → cutlass.cute.typing.Uint32`

Finds the bit position of the most significant non-sign bit.

For unsigned, finds the most significant 1 bit. For signed, finds the most significant bit that differs from the sign bit. Returns 0xFFFFFFFF if not found.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#integer-arithmetic-instructions-bfind>

#### Parameters

**value** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*,* [*Int64*](../basic_data_types.md#cutlass.Int64)*,* [*Uint64*](../basic_data_types.md#cutlass.Uint64)*\]*) -- Input value (32-bit or 64-bit integer)

#### Returns

Bit position (0-31 or 0-63) or 0xFFFFFFFF if not found

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Int64](../basic_data_types.md#cutlass.Int64)\]

### brev

**Kind:** function

**Qualified name:** `cutlass.cute.arch.brev`

**Signature:** `brev(value: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64`

Reverses the bits in the value.

Returns the input value with bits reversed. Bit 0 becomes bit 31 (or 63), bit 1 becomes bit 30 (or 62), etc.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#integer-arithmetic-instructions-brev>

#### Parameters

**value** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*,* [*Int64*](../basic_data_types.md#cutlass.Int64)*,* [*Uint64*](../basic_data_types.md#cutlass.Uint64)*\]*) -- Input value (32-bit or 64-bit integer)

#### Returns

Bit-reversed value (same type as input)

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32), [Int64](../basic_data_types.md#cutlass.Int64), [Uint64](../basic_data_types.md#cutlass.Uint64)\]

### bfe

**Kind:** function

**Qualified name:** `cutlass.cute.arch.bfe`

**Signature:** `bfe(value: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, start: int | cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, length: int | cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64`

Extract bit field from value and place the zero or sign-extended result. Source start gives the bit field starting bit position, and source length gives the bit field length in bits.

The result and value must have the same type.

Start and length are 32 bits, but are restricted to the 8-bit value range 0..255.

The sign bit of the extracted field is defined as:

Uint32 or Uint64 value: zero

Int32 or Int64 value: Most significant bit (msb) of input value if the extracted field extends beyond the msb of the input value, otherwise if the bit field length is zero, the result is zero.

The result is padded with the sign bit of the extracted field. If the start position is beyond the msb of the input, the result is filled with the replicated sign bit of the extracted field.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#integer-arithmetic-instructions-bfe>

#### Parameters

- **value** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Source value to extract from

- **start** (*Union\[int,* [*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Starting bit position (0-31)

- **length** (*Union\[int,* [*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Number of bits to extract (0-32)

#### Returns

Extracted bit field (right-justified)

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32)\]

### bfi

**Kind:** function

**Qualified name:** `cutlass.cute.arch.bfi`

**Signature:** `bfi(replacement: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, value: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, start: int | cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, length: int | cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64`

Inserts a bit field into a value (bit field insert).

Replaces a contiguous sequence of bits in the value with bits from the replacement operand. Bits outside the specified field are preserved from the original value.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#integer-arithmetic-instructions-bfi>

#### Parameters

- **value** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Original value to insert into

- **replacement** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Value containing bits to insert

- **start** (*Union\[int,* [*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Starting bit position (0-31)

- **length** (*Union\[int,* [*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Number of bits to insert (0-32)

#### Returns

Value with bit field replaced

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32)\]

**Architecture**: SM 20+

**Example**:

    # Insert 0xF into bits [11:8] of 0x12345678
    result = bfi(Uint32(0x12345678), Uint32(0xF), start=8, length=4)
    # Returns 0x12345F78

### mul_hi

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mul_hi`

**Signature:** `mul_hi(a: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, b: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64`

Multiplies two values and returns the high-order bits of the result.

Performs a full-width multiplication and returns the upper half of the result. For 32-bit inputs, returns bits \[63:32\]. For 64-bit inputs, returns bits \[127:64\].

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#integer-arithmetic-instructions-mul-hi>

#### Parameters

- **a** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*,* [*Int64*](../basic_data_types.md#cutlass.Int64)*,* [*Uint64*](../basic_data_types.md#cutlass.Uint64)*\]*) -- First multiplicand

- **b** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*,* [*Int64*](../basic_data_types.md#cutlass.Int64)*,* [*Uint64*](../basic_data_types.md#cutlass.Uint64)*\]*) -- Second multiplicand

#### Returns

High-order bits of the product (same type as inputs)

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32), [Int64](../basic_data_types.md#cutlass.Int64), [Uint64](../basic_data_types.md#cutlass.Uint64)\]

### mul_wide

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mul_wide`

**Signature:** `mul_wide(a: cutlass.cute.typing.Int16 | cutlass.cute.typing.Uint16 | cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, b: cutlass.cute.typing.Int16 | cutlass.cute.typing.Uint16 | cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64`

Multiplies two narrow values and returns a wide result.

Performs multiplication with automatic widening of the result type. 16-bit inputs produce 32-bit result. 32-bit inputs produce 64-bit result.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#integer-arithmetic-instructions-mul>

#### Parameters

- **a** (*Union\[*[*Int16*](../basic_data_types.md#cutlass.Int16)*,* [*Uint16*](../basic_data_types.md#cutlass.Uint16)*,* [*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- First multiplicand (16-bit or 32-bit)

- **b** (*Union\[*[*Int16*](../basic_data_types.md#cutlass.Int16)*,* [*Uint16*](../basic_data_types.md#cutlass.Uint16)*,* [*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Second multiplicand (must match signedness of a)

#### Returns

Wide product (32-bit for 16-bit inputs, 64-bit for 32-bit inputs)

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32), [Int64](../basic_data_types.md#cutlass.Int64), [Uint64](../basic_data_types.md#cutlass.Uint64)\]

### mul24

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mul24`

**Signature:** `mul24(a: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, b: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, hi: bool = False) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32`

Fast 24-bit integer multiplication.

Multiplies the low 24 bits of each operand. Bits \[31:24\] are ignored. Result can be either low 32 bits (hi=False) or high 32 bits (hi=True).

t = a \* b; d = t\<47..16\> \# for .hi variant (if hi is True) d = t\<31..0\> \# for .lo variant (if hi is False)

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#integer-arithmetic-instructions-mul24>

#### Parameters

- **a** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- First operand (only low 24 bits used)

- **b** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Second operand (only low 24 bits used)

- **hi** (*bool*) -- If True, return high 32 bits; if False, return low 32 bits

#### Returns

Product of low 24 bits

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32)\]

### mad24

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mad24`

**Signature:** `mad24(a: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, b: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, c: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, hi: bool = False) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32`

Fast 24-bit integer multiply-add.

Computes (a \* b) + c using only the low 24 bits of a and b. Result can be either low 32 bits (hi=False) or high 32 bits (hi=True).

t = a \* b d = t\<47..16\> + c \# for .hi variant (if hi is True) d = t\<31..0\> + c \# for .lo variant (if hi is False)

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#integer-arithmetic-instructions-mad24>

#### Parameters

- **a** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- First multiplicand (only low 24 bits used)

- **b** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Second multiplicand (only low 24 bits used)

- **c** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Addend (all 32 bits used)

- **hi** (*bool*) -- If True, return high 32 bits; if False, return low 32 bits

#### Returns

(a \* b) + c

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32)\]

### add_cc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.add_cc`

**Signature:** `add_cc(a: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, b: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64`

Addition with carry-out (sets carry flag).

Performs addition and sets the carry flag for use by subsequent addc() operations. This is the first operation in a multi-precision addition chain.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#extended-precision-arithmetic-instructions-add-cc>

#### Parameters

- **a** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- First operand

- **b** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Second operand

#### Returns

Sum (a + b)

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32)\]

### addc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.addc`

**Signature:** `addc(a: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, b: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64`

Addition with carry-in (reads carry flag).

Performs addition including the carry flag set by add_cc() or previous addc(). This continues a multi-precision addition chain.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#extended-precision-arithmetic-instructions-addc>

#### Parameters

- **a** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- First operand

- **b** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Second operand

#### Returns

Sum (a + b + carry_flag)

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32)\]

### sub_cc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sub_cc`

**Signature:** `sub_cc(a: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, b: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64`

Subtraction with carry-out (sets carry/borrow flag).

Performs subtraction and sets the carry flag for use by subsequent subc() operations. This is the first operation in a multi-precision subtraction chain.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#extended-precision-arithmetic-instructions-sub-cc>

#### Parameters

- **a** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Value to subtract from

- **b** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Value to subtract

#### Returns

Difference (a - b)

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32)\]

### subc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.subc`

**Signature:** `subc(a: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, b: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64`

Subtraction with carry-in (reads carry/borrow flag).

Performs subtraction including the carry flag set by sub_cc() or previous subc(). This continues a multi-precision subtraction chain.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#extended-precision-arithmetic-instructions-subc>

#### Parameters

- **a** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Value to subtract from

- **b** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Value to subtract

#### Returns

Difference (a - b - carry_flag)

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32)\]

### mad_cc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.mad_cc`

**Signature:** `mad_cc(a: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, b: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, c: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64`

Multiply-add with carry-out (sets carry flag).

Performs (a \* b) + c and sets the carry flag for use by subsequent madc() operations. This starts a multi-precision multiply-add chain.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#extended-precision-arithmetic-instructions-mad-cc>

#### Parameters

- **a** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- First multiplicand

- **b** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Second multiplicand

- **c** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Addend

#### Returns

Low 32 bits of (a \* b) + c

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32)\]

### madc

**Kind:** function

**Qualified name:** `cutlass.cute.arch.madc`

**Signature:** `madc(a: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, b: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64, c: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32 | cutlass.cute.typing.Int64 | cutlass.cute.typing.Uint64`

Multiply-add with carry-in (reads carry flag).

Performs (a \* b) + c + carry_flag. This continues a multi-precision multiply-add chain.

See https://docs.nvidia.com/cuda/parallel-thread-execution/#extended-precision-arithmetic-instructions-madc

#### Parameters

- **a** (*Union\[Int32, Uint32\]*) -- First multiplicand

- **b** (*Union\[Int32, Uint32\]*) -- Second multiplicand

- **c** (*Union\[Int32, Uint32\]*) -- Addend

#### Returns

Low 32 bits of (a \* b) + c + carry_flag

#### Return type

Union\[Int32, Uint32\]

### activemask

**Kind:** function

**Qualified name:** `cutlass.cute.arch.activemask`

**Signature:** `activemask() → cutlass.cute.typing.Uint32`

Returns the mask of currently active threads in the warp.

Returns a 32-bit mask where bit N is set if thread N in the warp is active (not exited or diverged away). This reflects the current execution state.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-activemask>

#### Returns

Mask of active threads in warp

#### Return type

[Uint32](../basic_data_types.md#cutlass.Uint32)

### lanemask_lt

**Kind:** function

**Qualified name:** `cutlass.cute.arch.lanemask_lt`

**Signature:** `lanemask_lt() → cutlass.cute.typing.Uint32`

Returns mask of lanes with ID less than current lane.

Returns a 32-bit mask where bit N is set if N \< current_lane_id. For lane 0, returns 0x00000000. For lane 31, returns 0x7FFFFFFF.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-lanemask-lt>

#### Returns

Mask of lanes with index \< current lane

#### Return type

[Uint32](../basic_data_types.md#cutlass.Uint32)

### lanemask_le

**Kind:** function

**Qualified name:** `cutlass.cute.arch.lanemask_le`

**Signature:** `lanemask_le() → cutlass.cute.typing.Uint32`

Returns mask of lanes with ID less than or equal to current lane.

Returns a 32-bit mask where bit N is set if N \<= current_lane_id. For lane 0, returns 0x00000001. For lane 31, returns 0xFFFFFFFF.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-lanemask-le>

#### Returns

Mask of lanes with index \<= current lane

#### Return type

[Uint32](../basic_data_types.md#cutlass.Uint32)

### lanemask_eq

**Kind:** function

**Qualified name:** `cutlass.cute.arch.lanemask_eq`

**Signature:** `lanemask_eq() → cutlass.cute.typing.Uint32`

Returns mask with only the current lane's bit set.

Returns a 32-bit mask where only bit current_lane_id is set. Equivalent to (1 \<\< lane_idx()).

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-lanemask-eq>

#### Returns

Mask with only current lane bit set

#### Return type

[Uint32](../basic_data_types.md#cutlass.Uint32)

### lanemask_ge

**Kind:** function

**Qualified name:** `cutlass.cute.arch.lanemask_ge`

**Signature:** `lanemask_ge() → cutlass.cute.typing.Uint32`

Returns mask of lanes with ID greater than or equal to current lane.

Returns a 32-bit mask where bit N is set if N \>= current_lane_id. For lane 0, returns 0xFFFFFFFF. For lane 31, returns 0x80000000.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-lanemask-ge>

#### Returns

Mask of lanes with index \>= current lane

#### Return type

[Uint32](../basic_data_types.md#cutlass.Uint32)

### lanemask_gt

**Kind:** function

**Qualified name:** `cutlass.cute.arch.lanemask_gt`

**Signature:** `lanemask_gt() → cutlass.cute.typing.Uint32`

Returns mask of lanes with ID greater than current lane.

Returns a 32-bit mask where bit N is set if N \> current_lane_id. For lane 0, returns 0xFFFFFFFE. For lane 31, returns 0x00000000.

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#special-registers-lanemask-gt>

#### Returns

Mask of lanes with index \> current lane

#### Return type

[Uint32](../basic_data_types.md#cutlass.Uint32)

### add_sat_int

**Kind:** function

**Qualified name:** `cutlass.cute.arch.add_sat_int`

**Signature:** `add_sat_int(a: cutlass.cute.typing.Int32, b: cutlass.cute.typing.Int32) → cutlass.cute.typing.Int32`

Saturating signed 32-bit addition.

Performs addition with saturation. If the result overflows, it saturates to INT32_MAX (0x7FFFFFFF). If it underflows, saturates to INT32_MIN (0x80000000).

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#integer-arithmetic-instructions-add>

#### Parameters

- **a** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- First operand

- **b** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Second operand

#### Returns

Saturated sum

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### sub_sat_int

**Kind:** function

**Qualified name:** `cutlass.cute.arch.sub_sat_int`

**Signature:** `sub_sat_int(a: cutlass.cute.typing.Int32, b: cutlass.cute.typing.Int32) → cutlass.cute.typing.Int32`

Saturating signed 32-bit subtraction.

Performs subtraction with saturation. If the result overflows, it saturates to INT32_MAX (0x7FFFFFFF). If it underflows, saturates to INT32_MIN (0x80000000).

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#integer-arithmetic-instructions-sub>

#### Parameters

- **a** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Minuend

- **b** ([*Int32*](../basic_data_types.md#cutlass.Int32)) -- Subtrahend

#### Returns

Saturated difference

#### Return type

[Int32](../basic_data_types.md#cutlass.Int32)

### lop3

**Kind:** function

**Qualified name:** `cutlass.cute.arch.lop3`

**Signature:** `lop3(a: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, b: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, c: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, lut: int) → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32`

Three-input logic operation with lookup table.

Performs an arbitrary 3-input boolean function defined by an 8-bit lookup table. Each bit of the LUT corresponds to one combination of input bits (a, b, c).

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#logic-and-shift-instructions-lop3>

#### Parameters

- **a** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- First input

- **b** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Second input

- **c** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Third input

- **lut** (*Union\[int,* [*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- 8-bit lookup table defining the boolean function

#### Returns

Result of the 3-input logic operation

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32)\]

### shf

**Kind:** function

**Qualified name:** `cutlass.cute.arch.shf`

**Signature:** `shf(a: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, b: cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, shift: int | cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32, kind: Literal['l', 'r', 'clamp_left', 'clamp_right'] = 'l') → cutlass.cute.typing.Int32 | cutlass.cute.typing.Uint32`

Funnel shift operation.

Concatenates two 32-bit values into a 64-bit value and shifts/extracts a 32-bit result.

- "l" (left): Shift left, extract high 32 bits

- "r" (right): Shift right, extract low 32 bits

- "clamp_left": Clamp shift left amount to \[0, 32\]

- "clamp_right": Clamp shift right amount to \[0, 32\]

See <https://docs.nvidia.com/cuda/parallel-thread-execution/#logic-and-shift-instructions-shf>

#### Parameters

- **a** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- First 32-bit value (high part of concatenation)

- **b** (*Union\[*[*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Second 32-bit value (low part of concatenation)

- **shift** (*Union\[int,* [*Int32*](../basic_data_types.md#cutlass.Int32)*,* [*Uint32*](../basic_data_types.md#cutlass.Uint32)*\]*) -- Shift amount

- **kind** (*Literal\[\"l\",* *\"r\",* *\"clamp\"\]*) -- Shift direction - "l" (left), "r" (right), or "clamp"

#### Returns

32-bit result after funnel shift

#### Return type

Union\[[Int32](../basic_data_types.md#cutlass.Int32), [Uint32](../basic_data_types.md#cutlass.Uint32)\]

### alloc_smem

**Kind:** function

**Qualified name:** `cutlass.cute.arch.alloc_smem`

**Signature:** `alloc_smem(element_type: Type[cutlass.cute.typing.Numeric], size_in_elems: int, alignment: int | None = None) → cutlass.cute.typing.Pointer`

Statically allocates SMEM.

#### Parameters

- **element_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The pointee type of the pointer.

- **size_in_elems** (*int*) -- The size of the allocation in terms of number of elements of the pointee type

- **alignment** (*int*) -- An optional pointer alignment for the allocation

#### Returns

A pointer to the start of the allocation

#### Return type

[Pointer](../basic_data_types.md#cutlass.Pointer)

### get_dyn_smem

**Kind:** function

**Qualified name:** `cutlass.cute.arch.get_dyn_smem`

**Signature:** `get_dyn_smem(element_type: Type[cutlass.cute.typing.Numeric], alignment: int | None = None) → cutlass.cute.typing.Pointer`

Retrieves a pointer to a dynamic SMEM allocation.

#### Parameters

- **element_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The pointee type of the pointer.

- **alignment** (*int*) -- An optional pointer alignment, the result pointer is offset appropriately

#### Returns

A pointer to the start of the dynamic SMEM allocation with a correct alignement

#### Return type

[Pointer](../basic_data_types.md#cutlass.Pointer)

### get_dyn_smem_size

**Kind:** function

**Qualified name:** `cutlass.cute.arch.get_dyn_smem_size`

**Signature:** `get_dyn_smem_size() → int`

Gets the size in bytes of the dynamic shared memory that was specified at kernel launch time. This can be used for bounds checking during shared memory allocation.

#### Returns

The size of dynamic shared memory in bytes

#### Return type

int

### store_async_dsmem

**Kind:** function

**Qualified name:** `cutlass.cute.arch.store_async_dsmem`

**Signature:** `store_async_dsmem(smem_ptr: cutlass.cute.typing.Pointer, value: cutlass.cute.typing.Int | Tuple[cutlass.cute.typing.Int, cutlass.cute.typing.Int] | Tuple[cutlass.cute.typing.Int, cutlass.cute.typing.Int, cutlass.cute.typing.Int, cutlass.cute.typing.Int], mbar_ptr: cutlass.cute.typing.Pointer, peer_cta_rank: cutlass.cute.typing.Int) → None`

Asynchronous store to a remote CTA's shared memory via `st.async.shared::cluster`.

The store completion is tracked by the mbarrier's transaction count (`mbarrier::complete_tx::bytes`), allowing the caller to use a relaxed mbarrier arrive.

#### Parameters

- **smem_ptr** -- Destination pointer in this CTA's shared memory.

- **value** -- i32 value, or tuple of 2/4 i32 values to store.

- **mbar_ptr** -- Mbarrier pointer in this CTA's shared memory. Mapped to the peer CTA via `nvvm.mapa`.

- **peer_cta_rank** -- Target CTA rank in the cluster.

#### Raises

- **TypeError** -- If smem_ptr or mbar_ptr is not a CuTe pointer.

- **ValueError** -- If value is not a scalar or 2/4-tuple, or if smem_ptr/mbar_ptr is not in the SMEM address space, or if smem_ptr is not aligned to 4 \* len(value) bytes.

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

**Signature:** `retrieve_tmem_ptr(element_type: Type[cutlass.cute.typing.Numeric], alignment: int, ptr_to_buffer_holding_addr: cutlass.cute.typing.Pointer) → cutlass.cute.typing.Pointer`

Retrieves a pointer to TMEM with the provided element type and alignment.

#### Parameters

- **element_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The pointee type of the pointer.

- **alignment** (*int*) -- The alignment of the result pointer

- **ptr_to_buffer_holding_addr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to a SMEM buffer holding the TMEM address of the start of the allocation allocation

#### Returns

A pointer to TMEM

#### Return type

[Pointer](../basic_data_types.md#cutlass.Pointer)

### alloc_tmem

**Kind:** function

**Qualified name:** `cutlass.cute.arch.alloc_tmem`

**Signature:** `alloc_tmem(num_columns: cutlass.cute.typing.Int, smem_ptr_to_write_address: cutlass.cute.typing.Pointer, is_two_cta: bool | None = None, *, arch: str = 'sm_100') → None`

Allocates TMEM.

#### Parameters

- **num_columns** (*Int*) -- The number of TMEM columns to allocate

- **smem_ptr_to_write_address** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to a SMEM buffer where the TMEM address is written to

- **is_two_cta** -- Optional boolean parameter for 2-CTA MMAs

- **arch** (*str*) -- The architecture of the GPU.

### relinquish_tmem_alloc_permit

**Kind:** function

**Qualified name:** `cutlass.cute.arch.relinquish_tmem_alloc_permit`

**Signature:** `relinquish_tmem_alloc_permit(is_two_cta: bool | None = None) → None`

Relinquishes the right to allocate TMEM so that other CTAs potentially in a different grid can allocate.

### dealloc_tmem

**Kind:** function

**Qualified name:** `cutlass.cute.arch.dealloc_tmem`

**Signature:** `dealloc_tmem(tmem_ptr: cutlass.cute.typing.Pointer, num_columns: cutlass.cute.typing.Int, is_two_cta: bool | None = None, *, arch: str = 'sm_100') → None`

Deallocates TMEM using the provided pointer and number of columns.

#### Parameters

- **tmem_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the TMEM allocation to de-allocate

- **num_columns** (*Int*) -- The number of columns in the TMEM allocation

- **is_two_cta** -- Optional boolean parameter for 2-CTA MMAs

- **arch** (*str*) -- The architecture of the GPU.

### prmt

**Kind:** function

**Qualified name:** `cutlass.cute.arch.prmt`

**Signature:** `prmt(src: cutlass.cute.typing.Int, src_reg_shifted: cutlass.cute.typing.Int, prmt_indices: cutlass.cute.typing.Int) → ir.Value`

### cvt_i8_bf16_intrinsic

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i8_bf16_intrinsic`

**Signature:** `cvt_i8_bf16_intrinsic(vec_i8: ir.Value, length: int) → ir.Value`

Fast conversion from int8 to bfloat16. It converts a vector of int8 to a vector of bfloat16.

#### Parameters

- **vec_i8** (*1D vector* *of* *int8*) -- The input vector of int8.

- **length** (*int*) -- The length of the input vector.

#### Returns

The output 1D vector of bfloat16 with the same length as the input vector.

#### Return type

1D vector of bfloat16

### cvt_i4_bf16_intrinsic

**Kind:** function

**Qualified name:** `cutlass.cute.arch.cvt_i4_bf16_intrinsic`

**Signature:** `cvt_i4_bf16_intrinsic(vec_i4: ir.Value, length: int, *, with_shuffle: bool = False) → ir.Value`

Fast conversion from int4 to bfloat16. It converts a vector of int4 to a vector of bfloat16.

#### Parameters

- **vec_i4** (*1D vector* *of* *int4*) -- The input vector of int4.

- **length** (*int*) -- The length of the input vector.

- **with_shuffle** (*bool*) -- Whether the input vec_i4 follows a specific shuffle pattern. If True, for consecutive 8 int4 values with indices of (0, 1, 2, 3, 4, 5, 6, 7), the input elements are shuffled to (0, 2, 1, 3, 4, 6, 5, 7). For tailing elements less than 8, the shuffle pattern is (0, 2, 1, 3) for 4 elements. No shuffle is needed for less than 4 elements. Shuffle could help to produce converted bf16 values in the natural order of (0, 1, 2 ,3 ,4 ,5 ,6 ,7) without extra prmt instructions and thus better performance.

#### Returns

The output 1D vector of bfloat16 with the same length as the input vector.

#### Return type

1D vector of bfloat16

### issue_clc_query

**Kind:** function

**Qualified name:** `cutlass.cute.arch.issue_clc_query`

**Signature:** `issue_clc_query(mbar_ptr: cutlass.cute.typing.Pointer, clc_response_ptr: cutlass.cute.typing.Pointer, multicast: bool = True) → None`

The clusterlaunchcontrol.try_cancel instruction requests atomically cancelling the launch of a cluster that has not started running yet. It asynchronously writes an opaque response to shared memory indicating whether the operation succeeded or failed. On success, the opaque response contains the ctaid of the first CTA of the canceled cluster.

#### Parameters

- **mbar_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the mbarrier address in SMEM

- **clc_response_ptr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the cluster launch control response address in SMEM

- **multicast** (*bool*) -- Whether to use multicast variant (default: True)

### clc_response

**Kind:** function

**Qualified name:** `cutlass.cute.arch.clc_response`

**Signature:** `clc_response(result_addr: cutlass.cute.typing.Pointer) → Tuple[cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32, cutlass.cute.typing.Int32]`

After loading response from clusterlaunchcontrol.try_cancel instruction into 16-byte register, it can be further queried using clusterlaunchcontrol.query_cancel instruction. If the cluster is canceled successfully, predicate p is set to true; otherwise, it is set to false. If the request succeeded, clusterlaunchcontrol.query_cancel.get_first_ctaid extracts the CTA id of the first CTA in the canceled cluster. By default, the instruction returns a .v4 vector whose first three elements are the x, y and z coordinate of first CTA in canceled cluster.

#### Parameters

**result_addr** ([*Pointer*](../basic_data_types.md#cutlass.Pointer)) -- A pointer to the cluster launch control response address in SMEM
