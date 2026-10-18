---
title: "task_scheduling.resources"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/ts_api/resources.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# task_scheduling.resources {#module-cutlass.experimental.task_scheduling.resources}

Resource abstractions for the Task Scheduling (TS) framework.

**Compile-time abstraction**: Pipeline operations (acquire, release, commit, wait) defined here are traced by the DSL at compile time and produce the same mbarrier PTX instructions as hand-coded bare-metal kernels. There is no additional runtime overhead from using these abstractions --- the framework is a code generator, not a runtime scheduler.

This module defines the data-flow building blocks that a warp-specialised kernel assembles into a pipeline:

## Classes

PipelineConfig

:   Immutable descriptor that selects a pipeline type (TMA, UMMA, CLC, etc.) and captures its parameters (stage count, byte count, cooperative groups, signalling policy). Static factory methods hide the per-type details.

TileSchedulerConfig

:   Pairs a `TileSchedulerType` with the scheduler-specific parameters (static persistent or CLC dynamic persistent) and, for CLC, the SMEM response pointer.

StageInfo

:   Read-only snapshot passed into every `producer_work` / `consumer_work` call. Carries the current loop offset, pipeline stage index, mbarrier pointer, per-call index, and the active `WorkTileInfo`.

MemoryResource

:

    Base class for every resource in the dataflow graph. A resource owns:

    :   - an optional `PipelineConfig` -\> materialised pipeline + barriers,

        - per-role variable dicts (`consumer_vars`, `producer_vars`),

        - pipeline state objects (`consumer_state`, `producer_state`).

    Subclasses declare public data-flow state as `TaskLocalVariable` fields and implement consumer / producer work methods to define the resource's behaviour at each schedule stage.

WorkQueue (MemoryResource)

:   Specialised resource that wraps a tile scheduler (`StaticPersistentTileScheduler` or `ClcDynamicPersistentTileScheduler`). It drives the persistent work loop by producing/consuming `WorkTileInfo` tiles.

## Typical lifecycle (driven by `TaskManager`)

> - `resource.create()` allocates SMEM barriers and materialises the pipeline object. Called once per resource from `TaskManager.setup_resources_and_tasks()`.
>
> - `resource.initialize_runtime_state_internal()` initialises pipeline states, status flags, and task-local storage defaults. Executed once per kernel invocation, outside any dynamic control flow.
>
> - `resource.create_consumer_variables_internal()` / `resource.create_producer_variables_internal()` populates `consumer_vars` / `producer_vars` dicts with the user-defined variables that flow between resources.
>
> - `consumer_work(stage_info)` / `producer_work(stage_info)` runs user-defined per-stage logic invoked by the `Task` schedule.
>
> - `copy_consumer_vars_to(dst_resource)` propagates matching consumer variables into a downstream resource's producer variables. This is automatic and called by `Task`.

*class* cutlass.experimental.task_scheduling.resources.WorkTileInfo(

*tile_idx: cutlass.cute.typing.Coord*,

*is_valid_tile: [Boolean](../basic_data_types.md#cutlass.Boolean)*,

)

Bases: [`WorkTileInfo`](../api/utils.md#cutlass.utils.WorkTileInfo)

TS-local 3D work tile with scalarized loop-carried fields.

TS loop-carries work tiles through staged persistent control flow and advances them in tail schedule entries. The released CUTLASS work-tile class stores `tile_idx` as one tuple field, which makes in-place updates fail IR dominance checks in nested dynamic regions. Keep that released class untouched and scalarize only the TS-local subclass.

\_\_init\_\_(

*tile_idx: cutlass.cute.typing.Coord*,

*is_valid_tile: [Boolean](../basic_data_types.md#cutlass.Boolean)*,

) → None

*property* tile_idx*: cutlass.cute.typing.Coord*

:   Get the index of the tile.

    Returns:

    :   The index of the tile.

    Return type:

    :   cute.Coord

<!-- -->

*property* is_valid_tile*: [Boolean](../basic_data_types.md#cutlass.Boolean)*

:   Check latest tile returned by the scheduler is valid or not. Any scheduling requests after all tasks completed will return an invalid tile.

    Returns:

    :   The validity of the tile.

    Return type:

    :   [Boolean](../basic_data_types.md#cutlass.Boolean)

update_from(

*other: [WorkTileInfo](../api/utils.md#cutlass.utils.WorkTileInfo)*,

) → None

*class* cutlass.experimental.task_scheduling.resources.PipelineConfig(

*num_stages: int*,

*num_bytes: int*,

*producer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*consumer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*pipeline_type: \~cutlass.experimental.task_scheduling.enums.PipelineType*,

*barrier_ptr: cutlass.cute.typing.Pointer \| None = None*,

*cta_layout_vmnk: cutlass.cute.typing.Layout \| tuple \| None = None*,

*producer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*consumer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*consumer_wait_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads \| None = None*,

*umma_consumer_producer_op: \~cutlass.pipeline.helpers.PipelineOp = PipelineOp.AsyncThread*,

*advance_on_wait: bool = False*,

*advance_on_acquire: bool = False*,

*num_bytes_per_warp_per_cta: int \| None = None*,

*mcast_mode_mn: tuple\[int*,

*int\] = (1*,

*1)*,

*interleave_stride: int \| tuple\[int*,

*int*,

*int*,

*int\] = 1*,

*async_producer_op: \~cutlass.pipeline.helpers.PipelineOp = PipelineOp.AsyncThread*,

)

Bases: `object`

Immutable descriptor for a single pipeline instance.

Captures everything `MemoryResource.create_pipeline()` needs to materialise a concrete pipeline object (barrier storage, stage count, transaction bytes, cooperative groups, CTA layout, and signalling policy).

Users should not construct `PipelineConfig` directly. Instead, use one of the static factory methods which fill in the correct `PipelineType` and sensible defaults:

- `create_async_async_pipeline_cfg` - generic async-producer + async-consumer pipeline.

- `create_tma_async_pipeline_cfg` - TMA-producer + async-consumer pipeline.

- `create_tma_umma_pipeline_cfg` - TMA-producer + UMMA-consumer pipeline.

- `create_umma_async_pipeline_cfg` - UMMA-producer + async-consumer pipeline.

- `create_async_umma_pipeline_cfg` - async-producer + UMMA-consumer pipeline.

- `create_umma_umma_pipeline_cfg` - UMMA-producer + UMMA-consumer pipeline.

- `create_clc_fetch_async_pipeline_cfg`- CLC tile-fetch + async-consumer pipeline.

num_stages

:   Number of buffering stages (pipeline depth).

    Type:

    :   int

<!-- -->

num_bytes

:   Expected transaction byte count per stage (0 when not applicable).

    Type:

    :   int

<!-- -->

producer_group, consumer_group

:   Cooperative groups that define the producer / consumer agents.

    Type:

    :   [pipeline.CooperativeGroup](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)

<!-- -->

pipeline_type

:   Selects the concrete pipeline implementation.

    Type:

    :   [PipelineType](enums.md#cutlass.experimental.task_scheduling.enums.PipelineType)

<!-- -->

barrier_ptr

:   Pre-allocated SMEM barrier storage (Int64, 2 \* num_stages). When *None*, `MemoryResource.create_pipeline()` allocates it.

    Type:

    :   cute.Pointer, optional

<!-- -->

cta_layout_vmnk

:   Cluster decomposition layout; required for UMMA / CLC pipelines.

    Type:

    :   cute.Layout, optional

<!-- -->

producer_signaling_threads

:   Which threads execute producer-side barrier operations (acquire, commit). `CtaLeader` restricts signalling to CTA 0.

    Type:

    :   [SignalingThreads](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)

<!-- -->

consumer_signaling_threads

:   Which threads execute consumer-side barrier operations (wait, release). Used for ConsumerRelease (and ConsumerWait when `consumer_wait_signaling_threads` is *None*).

    Type:

    :   [SignalingThreads](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)

<!-- -->

consumer_wait_signaling_threads

:   Override for ConsumerTryWait / ConsumerWait signaling threads. When *None* (default), falls back to `consumer_signaling_threads`. Set this when ConsumerWait and ConsumerRelease need different CTA signaling (e.g. split-consumer pattern where one task waits on all CTAs and another releases on leader CTA only).

    Type:

    :   [SignalingThreads](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads) or None

<!-- -->

async_producer_op

:   Producer-side barrier operation for `AsyncAsync`. Defaults to `AsyncThread`. `AsyncLoad` selects cp.async-style producer commits that use `cp.async.mbarrier.arrive` on local per-CTA full barriers.

    Type:

    :   [pipeline.PipelineOp](../api/pipeline.md#cutlass.pipeline.PipelineOp)

<!-- -->

umma_consumer_producer_op

:   Producer-side barrier operation for `AsyncUmma`. Defaults to `AsyncThread`. `AsyncLoad` selects cp.async-style producer commits and local per-CTA arrivals.

    Type:

    :   [pipeline.PipelineOp](../api/pipeline.md#cutlass.pipeline.PipelineOp)

<!-- -->

num_bytes_per_warp_per_cta

:   Declares that each producer CTA routes its TMA completion to the leader-visible full barrier. The value is the per-producer-warp, per-CTA transaction byte count; validation checks that `num_bytes` covers every producer warp across the full cluster.

    Type:

    :   int, optional

<!-- -->

mcast_mode_mn

:   Multicast mode passed to CUTLASS TMA pipeline creation for cluster arrival masks.

    Type:

    :   tuple\[int, int\]

<!-- -->

advance_on_wait

:   Controls when the pipeline stage index is advanced. When *False* (default), the stage is advanced on `release`; when *True*, the stage is advanced on the `wait` call and a separate pipeline state is used to release the consumer. Disabled by default pending performance measurements.

    Type:

    :   bool

<!-- -->

advance_on_acquire

:   Producer-side analogue of `advance_on_wait`. When *True*, `ProducerAcquire` advances `producer_state` immediately. `ProducerWork` either derives the just-acquired state from `producer_state` or follows the lagging `producer_commit_state`. `ProducerCommit` always uses `producer_commit_state`.

    Type:

    :   bool

<!-- -->

interleave_stride

:   Stride for interleaved pipeline advancement. When the stride is \> 1, N lanes share one pipeline with `num_stages` total barriers; each lane starts at its lane index and advances by the stride, which must evenly divide `num_stages`. A lane may be selected by a task-local warp or by a task's domain start. A single integer applies the same stride to every role. A 4-tuple assigns role-specific strides interpreted as `(producer_acquire, producer_commit, consumer_wait, consumer_release)`. Splitting `producer_acquire` from `producer_commit` requires `advance_on_acquire`; splitting `consumer_wait` from `consumer_release` requires `advance_on_wait`.

    Type:

    :   int or tuple\[int, int, int, int\]

<!-- -->

num_stages*: int*

:

<!-- -->

num_bytes*: int*

:

<!-- -->

producer_group*: [CooperativeGroup](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)*

:

<!-- -->

consumer_group*: [CooperativeGroup](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)*

:

<!-- -->

pipeline_type*: [PipelineType](enums.md#cutlass.experimental.task_scheduling.enums.PipelineType)*

:

<!-- -->

barrier_ptr*: cutlass.cute.typing.Pointer \| None = None*

:

<!-- -->

cta_layout_vmnk*: cutlass.cute.typing.Layout \| tuple \| None = None*

:

<!-- -->

producer_signaling_threads*: [SignalingThreads](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads) = 1*

:

<!-- -->

consumer_signaling_threads*: [SignalingThreads](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads) = 1*

:

<!-- -->

consumer_wait_signaling_threads*: [SignalingThreads](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads) \| None = None*

:

<!-- -->

umma_consumer_producer_op*: [PipelineOp](../api/pipeline.md#cutlass.pipeline.PipelineOp) = 1*

:

<!-- -->

advance_on_wait*: bool = False*

:

<!-- -->

advance_on_acquire*: bool = False*

:

<!-- -->

num_bytes_per_warp_per_cta*: int \| None = None*

:

<!-- -->

mcast_mode_mn*: tuple\[int, int\] = (1, 1)*

:

<!-- -->

interleave_stride*: int \| tuple\[int, int, int, int\] = 1*

:

<!-- -->

async_producer_op*: [PipelineOp](../api/pipeline.md#cutlass.pipeline.PipelineOp) = 1*

:

<!-- -->

*property* interleave_strides*: tuple\[int, int, int, int\]*

:   Return `(producer_acquire, producer_commit, consumer_wait, consumer_release)` strides.

<!-- -->

*property* producer_acquire_interleave_stride*: int*

:   Stride used when advancing the producer acquire cursor.

<!-- -->

*property* producer_commit_interleave_stride*: int*

:   Stride used when advancing the producer commit cursor.

<!-- -->

*property* consumer_wait_interleave_stride*: int*

:   Stride used when `advance_on_wait` advances consumer wait state.

<!-- -->

*property* consumer_release_interleave_stride*: int*

:   Stride used when advancing consumer release state.

<!-- -->

*property* max_interleave_stride*: int*

:   Largest role-specific interleave stride.

<!-- -->

*property* has_interleaved_stride*: bool*

:   Whether any role advances through the pipeline with stride \> 1.

*static* create_async_async_pipeline_cfg(

*num_stages: int*,

*producer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*consumer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*cta_layout_vmnk: cutlass.cute.typing.Layout*,

*producer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*consumer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*barrier_ptr: cutlass.cute.typing.Pointer \| None = None*,

*producer_op: \~cutlass.pipeline.helpers.PipelineOp = PipelineOp.AsyncThread*,

*advance_on_acquire: bool = False*,

*interleave_stride: int \| tuple\[int*,

*int*,

*int*,

*int\] = 1*,

) → [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

Create a config for a generic async pipeline.

`producer_op=AsyncLoad` matches async global-to-shared producers that signal full barriers with `cp.async.mbarrier.arrive` while their consumers still release through ordinary async-thread mbarriers.

Parameters:

:   - **num_stages** (*int*) -- Number of buffering stages.

    - **producer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **consumer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **cta_layout_vmnk** (*cute.Layout*) -- CTA cluster layout.

    - **producer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **consumer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **barrier_ptr** (*cute.Pointer,* *optional*) -- Pre-allocated barrier storage. `None` lets the allocator place it.

    - **producer_op** ([*pipeline.PipelineOp*](../api/pipeline.md#cutlass.pipeline.PipelineOp)*,* *optional*) -- Async producer operation, normally `AsyncThread` or `AsyncLoad`.

    - **advance_on_acquire** (*bool,* *optional*) -- Advance producer state at acquire time instead of commit time.

    - **interleave_stride** (*int* *or* *tuple\[int,* *int,* *int,* *int\],* *optional*) -- Role-specific pipeline-state stride. Mismatched opening and closing roles require the corresponding advance flag.

Returns:

:   Configuration for an `AsyncAsync` pipeline.

Return type:

:   [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

*static* create_tma_async_pipeline_cfg(

*num_stages: int*,

*num_bytes: int*,

*producer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*consumer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*cta_layout_vmnk: cutlass.cute.typing.Layout \| None = None*,

*producer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*consumer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*barrier_ptr: cutlass.cute.typing.Pointer \| None = None*,

*advance_on_wait: bool = False*,

*interleave_stride: int \| tuple\[int*,

*int*,

*int*,

*int\] = 1*,

*num_bytes_per_warp_per_cta: int \| None = None*,

) → [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

Create a config for a TMA-producer async pipeline.

Parameters:

:   - **num_stages** (*int*) -- Number of buffering stages.

    - **num_bytes** (*int*) -- TMA transaction byte count for each stage.

    - **producer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **consumer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **cta_layout_vmnk** (*cute.Layout,* *optional*) -- CTA cluster layout.

    - **producer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **consumer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **barrier_ptr** (*cute.Pointer,* *optional*) -- Pre-allocated barrier storage. `None` lets the allocator place it.

    - **advance_on_wait** (*bool,* *optional*) --

      Setting advance_on_wait=True on a PipelineConfig decouples the consumer's wait cursor from its release cursor by introducing two independent pipeline state objects:

        State                      Advances when       Points to
        -------------------------- ------------------- ---------------------------------
        `consumer_state`           `ConsumerWait`      the *next* buffer to wait on
        `consumer_release_state`   `ConsumerRelease`   the *oldest* un-released buffer

      With this split, the consumer can wait on buffer N+1 (advancing consumer_state) while still holding buffer N (not yet released via consumer_release_state).

    - **interleave_stride** (*int* *or* *tuple\[int,* *int,* *int,* *int\],* *optional*) -- Role-specific pipeline-state stride. Mismatched opening and closing roles require the corresponding advance flag.

    - **num_bytes_per_warp_per_cta** (*int,* *optional*) -- Per-producer-warp, per-CTA transaction byte count for leader-routed clustered TMA completion validation.

Returns:

:   Configuration for a `TmaAsync` pipeline.

Return type:

:   [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

*static* create_tma_umma_pipeline_cfg(

*num_stages: int*,

*num_bytes: int*,

*producer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*consumer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*cta_layout_vmnk: cutlass.cute.typing.Layout*,

*producer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*consumer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*barrier_ptr: cutlass.cute.typing.Pointer \| None = None*,

*advance_on_wait: bool = False*,

*interleave_stride: int \| tuple\[int*,

*int*,

*int*,

*int\] = 1*,

*mcast_mode_mn: tuple\[int*,

*int\] = (1*,

*1)*,

*num_bytes_per_warp_per_cta: int \| None = None*,

) → [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

Create a config for TMA producer + UMMA consumer async pipeline.

Parameters:

:   - **num_stages** (*int*) -- Number of buffering stages.

    - **num_bytes** (*int*) -- TMA transaction byte count for each stage.

    - **producer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **consumer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **cta_layout_vmnk** (*cute.Layout*) -- CTA cluster layout.

    - **producer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **consumer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **barrier_ptr** (*cute.Pointer,* *optional*) -- Pre-allocated barrier storage. `None` lets the allocator place it.

    - **advance_on_wait** (*bool,* *optional*) -- Advance consumer state at wait time instead of release time.

    - **interleave_stride** (*int* *or* *tuple\[int,* *int,* *int,* *int\],* *optional*) -- Role-specific pipeline-state stride. Mismatched opening and closing roles require the corresponding advance flag.

    - **mcast_mode_mn** (*tuple\[int,* *int\],* *optional*) -- TMA multicast mode in M and N.

    - **num_bytes_per_warp_per_cta** (*int,* *optional*) -- Per-producer-warp, per-CTA transaction byte count for validation.

Returns:

:   Configuration for a `TmaUmma` pipeline.

Return type:

:   [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

*static* create_umma_async_pipeline_cfg(

*num_stages: int*,

*producer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*consumer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*cta_layout_vmnk: cutlass.cute.typing.Layout*,

*producer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*consumer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*barrier_ptr: cutlass.cute.typing.Pointer \| None = None*,

*interleave_stride: int \| tuple\[int*,

*int*,

*int*,

*int\] = 1*,

) → [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

Create a config for a UMMA-producer async pipeline.

Parameters:

:   - **num_stages** (*int*) -- Number of buffering stages.

    - **producer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **consumer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **cta_layout_vmnk** (*cute.Layout*) -- CTA cluster layout.

    - **producer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **consumer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **barrier_ptr** (*cute.Pointer,* *optional*) -- Pre-allocated barrier storage. `None` lets the allocator place it.

    - **interleave_stride** (*int* *or* *tuple\[int,* *int,* *int,* *int\],* *optional*) -- Role-specific pipeline-state stride. Mismatched opening and closing roles require the corresponding advance flag.

Returns:

:   Configuration for an `UmmaAsync` pipeline.

Return type:

:   [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

*static* create_async_umma_pipeline_cfg(

*num_stages: int*,

*producer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*consumer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*cta_layout_vmnk: cutlass.cute.typing.Layout*,

*producer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*consumer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*consumer_wait_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads \| None = None*,

*producer_op: \~cutlass.pipeline.helpers.PipelineOp = PipelineOp.AsyncThread*,

*barrier_ptr: cutlass.cute.typing.Pointer \| None = None*,

*advance_on_wait: bool = False*,

*advance_on_acquire: bool = False*,

*interleave_stride: int \| tuple\[int*,

*int*,

*int*,

*int\] = 1*,

) → [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

Create a config for an async-producer + UMMA-consumer pipeline.

Used when async producer threads create data and the UMMA warp consumes it. `producer_op=AsyncLoad` matches async global-to-shared producer commits that use `cp.async.mbarrier.arrive` on local per-CTA full barriers.

Parameters:

:   - **num_stages** (*int*) -- Number of buffering stages.

    - **producer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **consumer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **cta_layout_vmnk** (*cute.Layout*) -- CTA cluster layout.

    - **producer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **consumer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **consumer_wait_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Override for wait-side signaling when wait and release use different participants.

    - **producer_op** ([*pipeline.PipelineOp*](../api/pipeline.md#cutlass.pipeline.PipelineOp)*,* *optional*) -- Async producer operation, normally `AsyncThread` or `AsyncLoad`.

    - **barrier_ptr** (*cute.Pointer,* *optional*) -- Pre-allocated barrier storage. `None` lets the allocator place it.

    - **advance_on_wait** (*bool,* *optional*) -- Advance consumer state at wait time instead of release time.

    - **advance_on_acquire** (*bool,* *optional*) -- Advance producer state at acquire time instead of commit time.

    - **interleave_stride** (*int* *or* *tuple\[int,* *int,* *int,* *int\],* *optional*) -- Role-specific pipeline-state stride. Mismatched opening and closing roles require the corresponding advance flag.

Returns:

:   Configuration for an `AsyncUmma` pipeline.

Return type:

:   [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

*static* create_umma_umma_pipeline_cfg(

*num_stages: int*,

*producer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*consumer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*cta_layout_vmnk: cutlass.cute.typing.Layout*,

*producer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*consumer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*barrier_ptr: cutlass.cute.typing.Pointer \| None = None*,

) → [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

Create a config for a UMMA producer + UMMA consumer pipeline.

Parameters:

:   - **num_stages** (*int*) -- Number of buffering stages.

    - **producer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **consumer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **cta_layout_vmnk** (*cute.Layout*) -- CTA cluster layout.

    - **producer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **consumer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **barrier_ptr** (*cute.Pointer,* *optional*) -- Pre-allocated barrier storage. `None` lets the allocator place it.

Returns:

:   Configuration for an `UmmaUmma` pipeline.

Return type:

:   [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

*static* create_clc_fetch_async_pipeline_cfg(

*num_stages: int*,

*num_bytes: int*,

*producer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*consumer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*cta_layout_vmnk: cutlass.cute.typing.Layout*,

*producer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.CtaLeader: 2\>*,

*consumer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*barrier_ptr: cutlass.cute.typing.Pointer \| None = None*,

) → [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

Create a config for a CLC tile-fetch async pipeline.

Parameters:

:   - **num_stages** (*int*) -- Number of buffering stages.

    - **num_bytes** (*int*) -- Work-tile fetch transaction byte count for each stage.

    - **producer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **consumer_group** ([*pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups on the producer and consumer sides.

    - **cta_layout_vmnk** (*cute.Layout*) -- CTA cluster layout.

    - **producer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **consumer_signaling_threads** ([*SignalingThreads*](enums.md#cutlass.experimental.task_scheduling.enums.SignalingThreads)*,* *optional*) -- Threads that execute producer- and consumer-side barrier operations.

    - **barrier_ptr** (*cute.Pointer,* *optional*) -- Pre-allocated barrier storage. `None` lets the allocator place it.

Returns:

:   Configuration for a `ClcFetchAsync` pipeline.

Return type:

:   [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)

\_\_init\_\_(

*num_stages: int*,

*num_bytes: int*,

*producer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*consumer_group: \~cutlass.pipeline.helpers.CooperativeGroup*,

*pipeline_type: \~cutlass.experimental.task_scheduling.enums.PipelineType*,

*barrier_ptr: cutlass.cute.typing.Pointer \| None = None*,

*cta_layout_vmnk: cutlass.cute.typing.Layout \| tuple \| None = None*,

*producer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*consumer_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads = \<SignalingThreads.All: 1\>*,

*consumer_wait_signaling_threads: \~cutlass.experimental.task_scheduling.enums.SignalingThreads \| None = None*,

*umma_consumer_producer_op: \~cutlass.pipeline.helpers.PipelineOp = PipelineOp.AsyncThread*,

*advance_on_wait: bool = False*,

*advance_on_acquire: bool = False*,

*num_bytes_per_warp_per_cta: int \| None = None*,

*mcast_mode_mn: tuple\[int*,

*int\] = (1*,

*1)*,

*interleave_stride: int \| tuple\[int*,

*int*,

*int*,

*int\] = 1*,

*async_producer_op: \~cutlass.pipeline.helpers.PipelineOp = PipelineOp.AsyncThread*,

) → None

*class* cutlass.experimental.task_scheduling.resources.TileSchedulerConfig(

*tile_scheduler_type: [TileSchedulerType](enums.md#cutlass.experimental.task_scheduling.enums.TileSchedulerType)*,

*tile_scheduler_params: [PersistentTileSchedulerParams](../api/utils.md#cutlass.utils.PersistentTileSchedulerParams) \| [ClcDynamicPersistentTileSchedulerParams](../api/utils.md#cutlass.utils.ClcDynamicPersistentTileSchedulerParams)*,

*response_ptr: cutlass.cute.typing.Pointer \| None = None*,

)

Bases: `object`

Immutable descriptor that pairs a tile-scheduler type with its params.

tile_scheduler_type

:   `StaticPersistent` or `ClcDynamicPersistent`.

    Type:

    :   [TileSchedulerType](enums.md#cutlass.experimental.task_scheduling.enums.TileSchedulerType)

<!-- -->

tile_scheduler_params

:   Scheduler-specific parameters (grid dims, cluster shape, etc.).

    Type:

    :   [PersistentTileSchedulerParams](../api/utils.md#cutlass.utils.PersistentTileSchedulerParams)

<!-- -->

response_ptr

:   SMEM response buffer pointer; required only for CLC dynamic mode.

    Type:

    :   cute.Pointer, optional

<!-- -->

tile_scheduler_type*: [TileSchedulerType](enums.md#cutlass.experimental.task_scheduling.enums.TileSchedulerType)*

:

<!-- -->

tile_scheduler_params*: [PersistentTileSchedulerParams](../api/utils.md#cutlass.utils.PersistentTileSchedulerParams) \| [ClcDynamicPersistentTileSchedulerParams](../api/utils.md#cutlass.utils.ClcDynamicPersistentTileSchedulerParams)*

:

<!-- -->

response_ptr*: cutlass.cute.typing.Pointer \| None = None*

:

*static* create_static_persistent_tile_scheduler_params(

*tile_scheduler_params: [PersistentTileSchedulerParams](../api/utils.md#cutlass.utils.PersistentTileSchedulerParams)*,

) → [TileSchedulerConfig](#cutlass.experimental.task_scheduling.resources.TileSchedulerConfig)

Create a config for the static persistent tile scheduler.

Parameters:

:   **tile_scheduler_params** ([*PersistentTileSchedulerParams*](../api/utils.md#cutlass.utils.PersistentTileSchedulerParams)) -- CUTLASS static persistent scheduler parameters.

Returns:

:   Configuration selecting `TileSchedulerType.StaticPersistent`.

Return type:

:   [TileSchedulerConfig](#cutlass.experimental.task_scheduling.resources.TileSchedulerConfig)

*static* create_clc_dynamic_persistent_tile_scheduler_params(

*tile_scheduler_params: [ClcDynamicPersistentTileSchedulerParams](../api/utils.md#cutlass.utils.ClcDynamicPersistentTileSchedulerParams)*,

*response_ptr: cutlass.cute.typing.Pointer*,

) → [TileSchedulerConfig](#cutlass.experimental.task_scheduling.resources.TileSchedulerConfig)

Create a config for the CLC dynamic persistent tile scheduler.

Parameters:

:   - **tile_scheduler_params** ([*ClcDynamicPersistentTileSchedulerParams*](../api/utils.md#cutlass.utils.ClcDynamicPersistentTileSchedulerParams)) -- CUTLASS CLC dynamic scheduler parameters.

    - **response_ptr** (*cute.Pointer*) -- SMEM response buffer pointer used by the CLC fetch pipeline.

Returns:

:   Configuration selecting `TileSchedulerType.ClcDynamicPersistent`.

Return type:

:   [TileSchedulerConfig](#cutlass.experimental.task_scheduling.resources.TileSchedulerConfig)

\_\_init\_\_(

*tile_scheduler_type: [TileSchedulerType](enums.md#cutlass.experimental.task_scheduling.enums.TileSchedulerType)*,

*tile_scheduler_params: [PersistentTileSchedulerParams](../api/utils.md#cutlass.utils.PersistentTileSchedulerParams) \| [ClcDynamicPersistentTileSchedulerParams](../api/utils.md#cutlass.utils.ClcDynamicPersistentTileSchedulerParams)*,

*response_ptr: cutlass.cute.typing.Pointer \| None = None*,

) → None

*class* cutlass.experimental.task_scheduling.resources.StageInfo(

*loop_offset: int*,

*loop_start: int*,

*loop_end: int*,

*loop_step: int*,

*stage_idx: int \| None*,

*label: object*,

*barrier: Array \| None*,

*work_tile: [WorkTileInfo](#cutlass.experimental.task_scheduling.resources.WorkTileInfo) \| None*,

*num_active_stages: \_MockObject = 0*,

*context: [ResourceContext](memory.md#cutlass.experimental.task_scheduling.memory.ResourceContext) \| None = None*,

*task_cache: object \| None = None*,

)

Bases: `object`

Read-only context passed to `producer_work` / `consumer_work`.

loop_offset

:   Current iteration index within the K-tile loop over the computational domain.

    Type:

    :   int

<!-- -->

loop_start, loop_end, loop_step

:   Bounds and stride of the K-tile loop (`range(loop_start, loop_end, loop_step)`).

    Type:

    :   int

<!-- -->

stage_idx

:   Pipeline stage index for the current operation (`None` when the resource has no pipeline).

    Type:

    :   int or None

<!-- -->

label

:   User-defined compile-time work label from the schedule entry. Selects a named work hook:

        if cutlass.const_expr(stage_info.label == WorkLabel.K_DESC):
            ...  # K descriptor logic
        elif cutlass.const_expr(stage_info.label == WorkLabel.V_DESC):
            ...  # V descriptor logic

    `None` when no label is specified (backward-compatible default).

    Type:

    :   object or None

<!-- -->

barrier

:   Mbarrier pointer for the current pipeline stage (`None` when the resource has no pipeline).

    Type:

    :   cutlass.Array or None

<!-- -->

work_tile

:   Tile coordinates and validity flag from the tile scheduler.

    Type:

    :   [WorkTileInfo](#cutlass.experimental.task_scheduling.resources.WorkTileInfo) or None

<!-- -->

num_active_stages

:   Number of producer stages currently in flight for delayed-commit producer schedules. This is 0 for the default immediate-commit schedule.

    Type:

    :   int

<!-- -->

context

:   Unified context carrying `smem_base`, `tmem_ptr_i32`, and any future framework-level state. `None` when no allocator is in use.

    Type:

    :   [ResourceContext](memory.md#cutlass.experimental.task_scheduling.memory.ResourceContext) or None

<!-- -->

task_cache

:   Optional task-defined payload returned by `Task.make_task_cache()`. This is intended for kernel-specific cached values that hot resource paths want to read without widening `StageInfo` field-by-field. The payload shape is task-defined and should remain fixed per task class.

    Type:

    :   object or None

<!-- -->

loop_offset*: int*

:

<!-- -->

loop_start*: int*

:

<!-- -->

loop_end*: int*

:

<!-- -->

loop_step*: int*

:

<!-- -->

stage_idx*: int \| None*

:

<!-- -->

label*: object*

:

<!-- -->

barrier*: Array \| None*

:

<!-- -->

work_tile*: [WorkTileInfo](#cutlass.experimental.task_scheduling.resources.WorkTileInfo) \| None*

:

<!-- -->

num_active_stages*: \_MockObject = 0*

:

<!-- -->

context*: [ResourceContext](memory.md#cutlass.experimental.task_scheduling.memory.ResourceContext) \| None = None*

:

<!-- -->

task_cache*: object = None*

:

\_\_init\_\_(

*loop_offset: int*,

*loop_start: int*,

*loop_end: int*,

*loop_step: int*,

*stage_idx: int \| None*,

*label: object*,

*barrier: Array \| None*,

*work_tile: [WorkTileInfo](#cutlass.experimental.task_scheduling.resources.WorkTileInfo) \| None*,

*num_active_stages: \_MockObject = 0*,

*context: [ResourceContext](memory.md#cutlass.experimental.task_scheduling.memory.ResourceContext) \| None = None*,

*task_cache: object \| None = None*,

) → None

*class* cutlass.experimental.task_scheduling.resources.TaskLocalVariable(

*dtype: object = \<object object\>*,

*default: object = \<object object\>*,

*default_factory: \~collections.abc.Callable\[\[\]*,

*object\] \| None = None*,

*docs: str \| None = None*,

*runtime_slot_name: str \| None = None*,

)

Bases: `object`

Metadata for one resource-owned variable in captured TS schedules.

A `TaskLocalVariable` is the public identity of a logical variable owned by a `MemoryResource`. In generated code it materializes as task-local register state, with consumer work calls producing new versions of that register value. The current implementation still lowers through string-named `consumer_vars` / `producer_vars` slots, but schedule code should carry `TaskLocalVariable` identities rather than naming those internal dictionaries directly.

dtype

:   DSL type of the value stored in the task-local register slot.

    Type:

    :   object

<!-- -->

default

:   Sink-safe initial value used before any producer writes and at SSA joins.

    Type:

    :   object

<!-- -->

default_factory

:   Factory for the initial value. Mutually exclusive with `default`.

    Type:

    :   Callable\[\[\], object\], optional

<!-- -->

docs

:   Short user-facing description rendered by API documentation.

    Type:

    :   str, optional

<!-- -->

runtime_slot_name

:   Override for the internal slot name. Most resources use the dataclass field name.

    Type:

    :   str, optional

<!-- -->

dtype*: object = \<object object\>*

:

<!-- -->

default*: object = \<object object\>*

:

<!-- -->

default_factory*: Callable\[\[\], object\] \| None = None*

:

<!-- -->

docs*: str \| None = None*

:

<!-- -->

runtime_slot_name*: str \| None = None*

:

<!-- -->

*static* uninitialized() → Field

:   Declare a dataclass field that backs a `TaskLocalVariable` slot.

    The slot must be assigned a `TaskLocalVariable` instance during `__init__` or `__post_init__` of the owning class:

        item: TaskLocalVariable = TaskLocalVariable.uninitialized()

        def __post_init__(self) -> None:
            self.item = TaskLocalVariable(dtype=..., default=...)

    Forgetting the assignment surfaces as a clear `ValueError` the first time TS walks the resource's task-local variables (see `bind_task_local_variables`).

    The returned `Field` carries framework metadata under the `"ts"` namespace so that `@consumer_work(returns=...)` can verify that a field reference points at an actual task-local slot rather than an arbitrary `dataclasses.Field`.

    Returns:

    :   Field placeholder to assign a concrete `TaskLocalVariable` in the owning resource constructor or `__post_init__`.

    Return type:

    :   dataclasses.Field

<!-- -->

*property* owner*: object \| None*

:   Resource instance that owns this variable, once bound.

<!-- -->

*property* field_name*: str \| None*

:   Stable resource field name used for legacy slot lowering.

<!-- -->

*property* slot_name*: str \| None*

:   Current internal slot name for this variable.

\_\_init\_\_(

*dtype: object = \<object object\>*,

*default: object = \<object object\>*,

*default_factory: \~collections.abc.Callable\[\[\]*,

*object\] \| None = None*,

*docs: str \| None = None*,

*runtime_slot_name: str \| None = None*,

) → None

cutlass.experimental.task_scheduling.resources.consumer_work(*method: \~collections.abc.Callable\[\[\...\], \~typing.Any\] \| None = None, \*, work_attrs: \~cutlass.experimental.task_scheduling.enums.WorkAttr = \<WorkAttr.NONE: 0\>, returns: str \| \~dataclasses.Field \| tuple\[str \| \~dataclasses.Field, \...\] \| list\[str \| \~dataclasses.Field\] \| None = None*) → Callable\[\[\...\], Any\]

:   Register a method as a named consumer work function on a `MemoryResource`.

    Consumer work reads data out of the owning `MemoryResource` from the resource's point of view. The decorator registers the method under its Python name, which is also the raw `schedule_list` label.

    Parameters:

    :   - **method** (*Callable,* *optional*) -- Method being decorated. Omitted when using decorator-factory form.

        - **work_attrs** ([*cutlass.experimental.task_scheduling.enums.WorkAttr*](enums.md#cutlass.experimental.task_scheduling.enums.WorkAttr)*,* *optional*) -- Verification-visible attributes for the work callback. Use `WorkAttr.AUXILIARY` for helper work that carries no data payload.

        - **returns** (*str* *or* *dataclasses.Field* *or* *sequence,* *optional*) -- `TaskLocalVariable` output slot or slots updated by this consumer. Field references must point at fields declared with `TaskLocalVariable.uninitialized()`.

    Returns:

    :   Decorated method or decorator factory.

    Return type:

    :   Callable

    Notes

    Captured schedules pass returned values as data-flow tokens. A typical method declaration is:

        item: TaskLocalVariable = TaskLocalVariable.uninitialized()

        @consumer_work(returns=item)
        @cute.jit
        def load(self, stage_info):
            return self.tensor[stage_info.loop_offset]

    When a raw schedule list is used and a resource has multiple named consumer methods, the label is the final tuple element, for example `(smem, ScheduleStage.ConsumerWork, "build_desc_a")`.

<!-- -->

cutlass.experimental.task_scheduling.resources.producer_work(*method: \~collections.abc.Callable\[\[\...\], \~typing.Any\] \| None = None, \*, work_attrs: \~cutlass.experimental.task_scheduling.enums.WorkAttr = \<WorkAttr.NONE: 0\>*) → Callable\[\[\...\], Any\]

:   Register a method as a named producer work function on a `MemoryResource`.

    Producer work writes data into the owning `MemoryResource` from the resource's point of view. Captured schedules pass consumer tokens into producer keyword parameters by name.

    Parameters:

    :   - **method** (*Callable,* *optional*) -- Method being decorated. Omitted when using decorator-factory form.

        - **work_attrs** ([*cutlass.experimental.task_scheduling.enums.WorkAttr*](enums.md#cutlass.experimental.task_scheduling.enums.WorkAttr)*,* *optional*) -- Verification-visible attributes for the work callback. Use `WorkAttr.AUXILIARY` for helper work that carries no data payload.

    Returns:

    :   Decorated method or decorator factory.

    Return type:

    :   Callable

    Notes

    A typical captured producer receives token values as keyword parameters:

        @producer_work
        @cute.jit
        def store(self, stage_info, *, item):
            self.tensor[stage_info.loop_offset] = item

    When a raw schedule list is used and a resource has multiple named producer methods, the label is the final tuple element, for example `(smem, ScheduleStage.ProducerWork, "tma_load_a")`.

*class* cutlass.experimental.task_scheduling.resources.MemoryResource(

*\**,

*name: \~sphinx.ext.autodoc.mock.\_MockObject = \'\'*,

*is_barrier: \~sphinx.ext.autodoc.mock.\_MockObject = False*,

*pipeline_config: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*producer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*pipeline: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_wait_signaling_threads: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

)

Bases: `object`

Base class for every resource in the TS dataflow graph.

A `MemoryResource` represents a named piece of memory (GMEM, SMEM, TMEM, etc.) together with the pipeline that guards access to it. Subclasses override the hook methods below to define resource-specific behaviour; the `Task` scheduler calls them at the appropriate points.

## Named work functions

Instead of (or in addition to) overriding the monolithic `consumer_work` / `producer_work`, subclasses can use the `@consumer_work` / `@producer_work` decorators to register multiple named work methods. The schedule-list label selects which method to call at trace time:

    @consumer_work
    @cute.jit
    def k_desc(self, stage_info):
        ...

    @consumer_work
    @cute.jit
    def v_desc(self, stage_info):
        ...

In the schedule list, each `ConsumerWork` entry must carry a label when two or more named methods are registered:

    (smem_kv, ScheduleStage.ConsumerWork, "k_desc")

Label validation:

- Typos raise `ValueError` with a did-you-mean suggestion.

- Missing labels with ≥2 named methods raise `ValueError`.

- A single named method auto-dispatches without a label.

- No named methods → monolithic `consumer_work()` is called.

## Hook methods (override in subclasses)

- `get_smem_requirements() -> list[SmemAllocation]` returns `SmemAllocation` objects for data SMEM this resource needs. Default returns `[]`.

- `get_tmem_requirements() -> list[TmemAllocation]` returns `TmemAllocation` objects for TMEM columns this resource needs. Default returns `[]`.

- `create_consumer_variables() -> dict` returns a `{name: default}` dict of consumer variables that the consumer side produces and forwards to downstream resources via `copy_consumer_vars_to`.

- `create_producer_variables() -> dict` returns a `{name: default}` dict of variables consumed by the producer side and populated from an upstream resource's consumer vars.

- `consumer_aux_work(stage_info)` is helper-variable logic executed during `ScheduleStage.ConsumerAuxWork`.

- `consumer_work(stage_info)` is user logic executed during `ScheduleStage.ConsumerWork`. Overridden by `@consumer_work` methods when labels are used.

- `producer_aux_work(stage_info)` is helper-variable logic executed during `ScheduleStage.ProducerAuxWork`.

- `producer_work(stage_info)` is user logic executed during `ScheduleStage.ProducerWork`. Overridden by `@producer_work` methods when labels are used.

name

:   Human-readable label (used in debug prints and PTX comments).

    Type:

    :   str

<!-- -->

pipeline_config

:   Pipeline descriptor; `None` for resources that need no pipeline (e.g. GMEM source/sink).

    Type:

    :   [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig) or None

<!-- -->

consumer_vars, producer_vars

:   Variable dicts populated by `create_consumer/producer_variables`.

    Type:

    :   dict

<!-- -->

pipeline

:   Materialised pipeline object, set by `create()`.

    Type:

    :   object or None

<!-- -->

consumer_state, producer_state

:   Pipeline state objects (stage index + phase bit). With `advance_on_acquire`, `producer_state` is the acquire cursor and `producer_commit_state` is the lagging commit cursor. Producer work derives the just-acquired state from `producer_state` or follows the commit cursor, depending on the work slot.

<!-- -->

consumer_status, producer_status

:   Boolean flags used by try_wait / try_acquire.

<!-- -->

is_barrier

:   `True` for pure signaling resources that carry no data (e.g. sequence barriers, done notifications). Barrier resources get relaxed dependency-backing rules in `_verify_resource_deps`.

    Type:

    :   bool

<!-- -->

dummy

:   Keep DSL state live across dynamic control-flow boundaries.

    Type:

    :   bool

<!-- -->

consumer_var_names*: ClassVar\[Tuple\[str, \...\] \| None\] = None*

:

<!-- -->

name*: \_MockObject = \'\'*

:

<!-- -->

is_barrier*: \_MockObject = False*

:

<!-- -->

pipeline_config*: \_MockObject = None*

:

<!-- -->

consumer_vars*: \_MockObject*

:

<!-- -->

producer_vars*: \_MockObject*

:

<!-- -->

pipeline*: \_MockObject = None*

:

<!-- -->

consumer_state*: Any = None*

:

<!-- -->

producer_state*: Any = None*

:

<!-- -->

producer_commit_state*: Any = None*

:

<!-- -->

consumer_status*: Any = None*

:

<!-- -->

producer_status*: Any = None*

:

<!-- -->

consumer_release_state*: Any = None*

:

<!-- -->

consumer_work_stage*: Any = None*

:

<!-- -->

consumer_wait_signaling_threads*: \_MockObject = None*

:

<!-- -->

pipeline_group*: \_MockObject = None*

:

<!-- -->

dummy*: \_MockObject = False*

:

<!-- -->

*property* state_src*: [MemoryResource](#cutlass.experimental.task_scheduling.resources.MemoryResource)*

:   The object that owns this resource's *consumer* pipeline state.

    Returns `self` by default. For a member of a Merge or FusedMerge `PipelineGroup` this returns the group itself, so all members share the group's canonical consumer state (one consumer drives pipeline progression). Fork members and non-group resources use their own state (returns `self`).

    **Producer state is NOT redirected.** Each producer task independently tracks its own stage via `resource.producer_state`. Code on the producer side must use `resource.producer_state` directly, not `resource.state_src.producer_state`.

    Pre-resolving the state source as a plain Python attribute access (instead of a `_resolve_dispatch` function call inside `@cute.jit` regions) avoids DSL-tracer side effects that would otherwise emit IR dominance errors. The `_state_src_owner` field is set by `PipelineGroup.__post_init__` for Merge and FusedMerge members.

create_pipeline(

*pipeline_config: [PipelineConfig](#cutlass.experimental.task_scheduling.resources.PipelineConfig)*,

) → object

Materialise a pipeline object from the given config.

Allocates SMEM barrier storage (if not pre-supplied in `pipeline_config.barrier_ptr`), then dispatches to the correct pipeline constructor based on `pipeline_config.pipeline_type`.

All pipelines are created with `defer_sync=True` so that barrier initialisation fencing is left to the caller (`TaskManager` / kernel).

get_producer_acquire_state() → object

:   Return the state used for producer-side empty-barrier waits.

<!-- -->

get_producer_commit_state() → object

:   Return the state used for producer-side full-barrier commits.

get_producer_work_state(

*follows_acquire: bool = True*,

) → object

Return the state used for producer-side data movement.

With `advance_on_acquire`, work does not have an independent pipeline cursor. Work that follows acquire derives its stage from the acquire cursor in `Task._create_stage_info`; work that does not follow acquire uses the lagging commit cursor.

create() → None

:   Materialise the pipeline (if configured).

    Called once per resource from `TaskManager.setup_resources_and_tasks()`. Subclasses that need additional one-time setup (e.g. `WorkQueue` creating its tile scheduler) should call `super().create()` first.

    Members of a `PipelineGroup` skip pipeline creation --- their barrier ops are routed through the group's shared pipeline.

create_consumer_variables_internal(

*captured_schedule: bool = False*,

) → None

Populate `consumer_vars` by calling the user hook (legacy only).

create_producer_variables_internal(

*captured_schedule: bool = False*,

) → None

Populate `producer_vars` by calling the user hook (legacy only).

In captured-schedule mode, `producer_vars` is auto-allocated by `Task._allocate_slots_from_routing` from upstream `consumer_vars` based on captured schedule data-flow tokens, and overriding `create_producer_variables` is forbidden. This method becomes a no-op (and raises on illegal overrides) --- the actual slot allocation runs from `Task.init_variables`.

create_consumer_variables() → dict

:   Return initial consumer variable dict. Override in subclasses.

<!-- -->

create_producer_variables() → dict

:   Return initial producer variable dict. Override in subclasses.

<!-- -->

get_smem_requirements() → List\[[SmemAllocation](memory.md#cutlass.experimental.task_scheduling.memory.SmemAllocation)\]

:   Return SMEM allocations required by this resource.

    Override in subclasses that use data SMEM (not barrier SMEM, which is managed by `create_pipeline`). The returned `SmemAllocation` objects should be stored as instance attributes so the resource can read their `.offset` later.

    Default returns an empty list (no data SMEM needed).

<!-- -->

get_tmem_requirements() → List\[[TmemAllocation](memory.md#cutlass.experimental.task_scheduling.memory.TmemAllocation)\]

:   Return TMEM column allocations required by this resource.

    Override in subclasses that use TMEM. The returned `TmemAllocation` objects should be stored as instance attributes so the resource can read their `.offset` later.

    Default returns an empty list (no TMEM needed).

<!-- -->

get_producer_requirements() → list \| None

:   Return allocations accessed during `ProducerWork`.

    Override in subclasses where the producer only accesses a subset of the resource's allocations. Return a list containing any mix of `SmemAllocation` and `TmemAllocation` objects.

    The exhaustive checker uses this to build a producer-specific alias map so that `prod_work` only conflicts with aliases that overlap these ranges.

    Default returns `None` (all allocations from `get_smem_requirements` + `get_tmem_requirements` are considered producer-accessible).

<!-- -->

get_consumer_requirements() → list \| None

:   Return allocations accessed during `ConsumerWork`.

    Symmetric counterpart of `get_producer_requirements`. Override in subclasses where the consumer only accesses a subset of the resource's allocations.

    Default returns `None` (all allocations are consumer-accessible).

initialize_runtime_state_internal(

*context: [ResourceContext](memory.md#cutlass.experimental.task_scheduling.memory.ResourceContext) \| None = None*,

*captured_schedule: \_MockObject = False*,

) → None

Initialize pipeline state/status and task-local storage defaults.

Always creates `consumer_status` / `producer_status` (Int32) and `consumer_state` / `producer_state` (pipeline state or dummy Int32) so that the DSL tree shape is consistent regardless of whether a pipeline is attached.

For CLC pipelines the producer state uses `ProducerConsumer` mode to support both roles.

set_consumer_var(

*name: \_MockObject*,

*value: object*,

) → None

Save a named consumer variable (called from `consumer_work`).

get_consumer_var(*name: str*) → object

:   Read a named consumer variable (called from consumer work).

get_producer_var(

*name: \_MockObject*,

) → object

Read a named producer variable (called from `producer_work`).

copy_consumer_vars_to(

*dst_resource: [MemoryResource](#cutlass.experimental.task_scheduling.resources.MemoryResource)*,

*var_names: list\[str \| tuple\[str, str\]\] \| None = None*,

) → None

Forward matching consumer vars into *dst_resource*'s producer vars.

When *var_names* is `None` (broadcast mode), every key present in both `self.consumer_vars` and `dst_resource.producer_vars` is copied. When *var_names* is given, only those specific routes are copied. A route may be `"name"` for same-name copy or `("src_name", "dst_name")` for explicit remapping.

Called automatically by `Task._consumer_work` after `consumer_work(stage_info)` returns, for destinations whose `dst_stage` in the resolved slot routing is a `Producer*Work` stage.

copy_consumer_vars_to_consumer_of(

*dst_resource: [MemoryResource](#cutlass.experimental.task_scheduling.resources.MemoryResource)*,

*var_names: list\[str \| tuple\[str, str\]\] \| None = None*,

) → None

Forward matching consumer vars into *dst_resource*'s consumer vars.

Counterpart to [`copy_consumer_vars_to()`](#cutlass.experimental.task_scheduling.resources.MemoryResource.copy_consumer_vars_to) for consumer-to-consumer variable flow: the source resource's `consumer_vars` are copied into the destination resource's `consumer_vars` so that a later `consumer_work*` call on `dst_resource` can read them with `get_consumer_var`.

When *var_names* is `None` (broadcast mode), every key present in both `self.consumer_vars` and `dst_resource.consumer_vars` is copied. When *var_names* is given, only those specific routes are copied. A route may be `"name"` for same-name copy or `("src_name", "dst_name")` for explicit remapping.

Called automatically by `Task._consumer_work*` when the resolved slot routing for this slot targets a `ConsumerWork*` stage on `dst_resource` (which must therefore be in `src_resources`, since consumer vars belong to the consumer side of a resource).

store_consumer_status(*value: [Boolean](../basic_data_types.md#cutlass.Boolean)*) → None

:   Store consumer_status via self so the IR pass instruments the write.

    Converts the Boolean (i1) from consumer_try_wait to Int32 to avoid IR boolean-ref edge cases (see initialize_runtime_state_internal).

<!-- -->

load_consumer_status() → [Int32](../basic_data_types.md#cutlass.Int32)

:   Load consumer_status via self so the IR pass instruments the read.

    Returns Int32; the pipeline's consumer_wait already compares with 0 via arith.cmpi so no conversion back to Boolean is needed.

<!-- -->

store_producer_status(*value: [Boolean](../basic_data_types.md#cutlass.Boolean)*) → None

:   Store producer_status via self so the IR pass instruments the write.

    Converts the Boolean (i1) from producer_try_acquire to Int32 to avoid IR boolean-ref edge cases (see initialize_runtime_state_internal).

<!-- -->

load_producer_status() → [Int32](../basic_data_types.md#cutlass.Int32)

:   Load producer_status via self so the IR pass instruments the read.

    Returns Int32; the pipeline's producer_acquire already compares with 0 via arith.cmpi so no conversion back to Boolean is needed.

consumer_work(

*stage_info: [StageInfo](#cutlass.experimental.task_scheduling.resources.StageInfo)*,

) → None

Consumer-side user logic.

Parameters:

:   - **stage_info** ([*StageInfo*](#cutlass.experimental.task_scheduling.resources.StageInfo)) -- Current loop, stage, barrier, work-tile, and resource context.

    - **of** (*Override in subclasses that use the monolithic work hook instead*)

    - **methods.** (*named \@consumer_work*)

consumer_aux_work(

*stage_info: [StageInfo](#cutlass.experimental.task_scheduling.resources.StageInfo)*,

) → None

Helper-variable consumer work that does not model memory access.

Parameters:

:   **stage_info** ([*StageInfo*](#cutlass.experimental.task_scheduling.resources.StageInfo)) -- Current loop, stage, barrier, work-tile, and resource context.

producer_aux_work(

*stage_info: [StageInfo](#cutlass.experimental.task_scheduling.resources.StageInfo)*,

) → None

Helper-variable producer work that does not model memory access.

Parameters:

:   **stage_info** ([*StageInfo*](#cutlass.experimental.task_scheduling.resources.StageInfo)) -- Current loop, stage, barrier, work-tile, and resource context.

producer_work(

*stage_info: [StageInfo](#cutlass.experimental.task_scheduling.resources.StageInfo)*,

) → None

Producer-side user logic.

Parameters:

:   - **stage_info** ([*StageInfo*](#cutlass.experimental.task_scheduling.resources.StageInfo)) -- Current loop, stage, barrier, work-tile, and resource context.

    - **of** (*Override in subclasses that use the monolithic work hook instead*)

    - **methods.** (*named \@producer_work*)

<!-- -->

physical_ranges() → list\[tuple\[str, int, int\]\]

:   Declare physical memory regions this resource occupies.

    Returns a list of `(memory_space, start_col, end_col)` tuples describing the physical address ranges. Used by the cross-tile aliasing verifier to detect potential data races between resources that share the same physical memory (e.g. TMEM column overlaps).

    Override in subclasses whose physical storage overlaps with other resources. Default: empty (no declared ranges, no aliasing checks).

\_\_init\_\_(

*\**,

*name: \~sphinx.ext.autodoc.mock.\_MockObject = \'\'*,

*is_barrier: \~sphinx.ext.autodoc.mock.\_MockObject = False*,

*pipeline_config: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*producer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*pipeline: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_wait_signaling_threads: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

) → None

*class* cutlass.experimental.task_scheduling.resources.PdlWaitBarrier(

*\**,

*name: \~sphinx.ext.autodoc.mock.\_MockObject = \'\'*,

*is_barrier: \~sphinx.ext.autodoc.mock.\_MockObject = True*,

*pipeline_config: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*producer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*pipeline: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_wait_signaling_threads: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

)

Bases: [`MemoryResource`](#cutlass.experimental.task_scheduling.resources.MemoryResource)

Wait side of CUDA Programmatic Dependent Launch (PDL).

A barrier-only resource (`is_barrier=True`, no `PipelineConfig`) whose single user-facing method, [`wait_griddep()`](#cutlass.experimental.task_scheduling.resources.PdlWaitBarrier.wait_griddep), emits a `griddepcontrol.wait` PTX instruction. The instruction blocks the issuing thread until the direct predecessor grid dependency has completed and made its global-memory results visible.

## Wiring contract

Any TS resource whose data is sourced from the predecessor grid (e.g. SMEM-A loaded via TMA, GMEM-A read directly, DSMEM-A copied from a peer CTA) declares the dependency by listing `pdl_wait` as one of that resource's upstreams in `TaskManager.resource_dependency_graph`, e.g. `resource_dependency_graph[smem_a] = [pdl_wait, ...]`. Because `PdlWaitBarrier` has `is_barrier=True`, the verifier interprets the edge as *ordering-only*: no consumer/producer variable copy plan is set up, but it does require the schedule entry that emits the wait to precede the producer entries of any task that produces the dependent resource.

The `wait_griddep` entry may sit in any phase (Head, Loop with `LoopFirstIter` / `LoopLastIter` guards, Tail, post-WTL).

## Encouraged pattern

The encouraged pattern is **inline wait**: every task that produces a PDL-dependent resource issues its own `pdl_wait.wait_griddep()` call.

> **Note.**
>
> See also
>
> [`PdlLaunchBarrier`](#cutlass.experimental.task_scheduling.resources.PdlLaunchBarrier), `side.`

is_barrier*: \_MockObject = True*

:

wait_griddep(

*stage_info: [StageInfo](#cutlass.experimental.task_scheduling.resources.StageInfo)*,

) → None

\_\_init\_\_(

*\**,

*name: \~sphinx.ext.autodoc.mock.\_MockObject = \'\'*,

*is_barrier: \~sphinx.ext.autodoc.mock.\_MockObject = True*,

*pipeline_config: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*producer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*pipeline: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_wait_signaling_threads: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

) → None

*class* cutlass.experimental.task_scheduling.resources.PdlLaunchBarrier(

*\**,

*name: \~sphinx.ext.autodoc.mock.\_MockObject = \'\'*,

*is_barrier: \~sphinx.ext.autodoc.mock.\_MockObject = True*,

*pipeline_config: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*producer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*pipeline: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_wait_signaling_threads: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

)

Bases: [`MemoryResource`](#cutlass.experimental.task_scheduling.resources.MemoryResource)

Launch-dependents side of CUDA Programmatic Dependent Launch (PDL).

A barrier-only resource (`is_barrier=True`, no `PipelineConfig`) whose single user-facing method, [`launch_griddep()`](#cutlass.experimental.task_scheduling.resources.PdlLaunchBarrier.launch_griddep), emits a `griddepcontrol.launch_dependents` PTX instruction. The instruction notifies the successor grid that it may begin launching CTAs.

## Wiring contract

A `PdlLaunchBarrier` carries no data dependency: it does **not** appear on the consumer side of any data flow. Therefore it is not expected as a destination in `resource_dependency_graph`. The verifier treats `launch_griddep` entries as schedule-only emissions that may sit in any task and any phase. The exhaustive interleaving checker still requires every executable launch interleaving to have already executed at least one `PdlWaitBarrier.wait_griddep`.

No global "at least one launch" rule is enforced --- a kernel may legitimately omit launch (e.g. when it is the last grid in a pipeline chain or launch is gated by a non-PDL host policy).

> **Note.**
>
> See also
>
> [`PdlWaitBarrier`](#cutlass.experimental.task_scheduling.resources.PdlWaitBarrier)

is_barrier*: \_MockObject = True*

:

launch_griddep(

*stage_info: [StageInfo](#cutlass.experimental.task_scheduling.resources.StageInfo)*,

) → None

\_\_init\_\_(

*\**,

*name: \~sphinx.ext.autodoc.mock.\_MockObject = \'\'*,

*is_barrier: \~sphinx.ext.autodoc.mock.\_MockObject = True*,

*pipeline_config: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*producer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*pipeline: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_wait_signaling_threads: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

) → None

cutlass.experimental.task_scheduling.resources.PDL_BARRIER_TYPES*: tuple = (\<class \'cutlass.experimental.task_scheduling.resources.PdlWaitBarrier\'\>, \<class \'cutlass.experimental.task_scheduling.resources.PdlLaunchBarrier\'\>)*

:   Tuple of all PDL barrier classes for `isinstance` checks.

    Use this in framework code that needs to recognise any PDL-style barrier regardless of whether it is the wait side (`PdlWaitBarrier`) or the launch side (`PdlLaunchBarrier`).

*class* cutlass.experimental.task_scheduling.resources.WorkQueue(

*tile_scheduler_config: [TileSchedulerConfig](#cutlass.experimental.task_scheduling.resources.TileSchedulerConfig)*,

*\*\*kwargs: Any*,

)

Bases: [`MemoryResource`](#cutlass.experimental.task_scheduling.resources.MemoryResource)

Resource that wraps a persistent tile scheduler.

It participates in the schedule of every task:

- **Static persistent mode** - Number of launched CTAs is exactly to fill 1 wave of SMs. Work-tile indices are assigned statically to each CTA. It needs no dedicated scheduler warp. `get_and_advance_work_tile` calls `advance_to_next_work` directly.

- **CLC dynamic persistent mode** - a dedicated scheduler warp acts as the producer, issuing work-tile fetch requests in `fetch_work_tile`. Consumer tasks simply wait on the pipeline.

In both modes, the consumer-side variable `work_tile` (`WorkTileInfo`) carries the tile coordinates and validity flag that other resources read.

tile_scheduler

:   Materialised scheduler, set by `create()`.

    Type:

    :   [StaticPersistentTileScheduler](../api/utils.md#cutlass.utils.StaticPersistentTileScheduler) or [ClcDynamicPersistentTileScheduler](../api/utils.md#cutlass.utils.ClcDynamicPersistentTileScheduler)

<!-- -->

tile_scheduler_config

:   Descriptor selecting the scheduler type and parameters.

    Type:

    :   [TileSchedulerConfig](#cutlass.experimental.task_scheduling.resources.TileSchedulerConfig)

<!-- -->

tile_scheduler*: [StaticPersistentTileScheduler](../api/utils.md#cutlass.utils.StaticPersistentTileScheduler) \| [ClcDynamicPersistentTileScheduler](../api/utils.md#cutlass.utils.ClcDynamicPersistentTileScheduler) \| None = None*

:

\_\_init\_\_(

*tile_scheduler_config: [TileSchedulerConfig](#cutlass.experimental.task_scheduling.resources.TileSchedulerConfig)*,

*\*\*kwargs: Any*,

) → None

tile_scheduler_config*: \_MockObject = None*

:

<!-- -->

work_tile*: \_MockObject*

:

<!-- -->

skip_work_tile*: \_MockObject*

:

<!-- -->

create_tile_scheduler() → [StaticPersistentTileScheduler](../api/utils.md#cutlass.utils.StaticPersistentTileScheduler) \| [ClcDynamicPersistentTileScheduler](../api/utils.md#cutlass.utils.ClcDynamicPersistentTileScheduler)

:   Instantiate the concrete tile scheduler from `tile_scheduler_config`.

<!-- -->

create() → None

:   Create the pipeline (from base class) and the tile scheduler.

<!-- -->

initial_work_tile_info() → [WorkTileInfo](#cutlass.experimental.task_scheduling.resources.WorkTileInfo)

:   Return the initial work tile from the underlying scheduler.

skip_work_tile_if(

*work_tile: [WorkTileInfo](#cutlass.experimental.task_scheduling.resources.WorkTileInfo)*,

) → [Boolean](../basic_data_types.md#cutlass.Boolean)

Default skipped-tile predicate used by captured TS schedules.

init_work_tile(

*stage_info: [StageInfo](#cutlass.experimental.task_scheduling.resources.StageInfo)*,

) → tuple\[[WorkTileInfo](#cutlass.experimental.task_scheduling.resources.WorkTileInfo), [cutlass.Boolean](../basic_data_types.md#cutlass.Boolean)\]

Seed the persistent loop state before the first work tile.

get_and_advance_work_tile(

*stage_info: [StageInfo](#cutlass.experimental.task_scheduling.resources.StageInfo)*,

) → [WorkTileInfo](#cutlass.experimental.task_scheduling.resources.WorkTileInfo)

Typed-schedule work-tile advance callback.

fetch_work_tile(

*stage_info: [StageInfo](#cutlass.experimental.task_scheduling.resources.StageInfo)*,

) → None

Typed-schedule work-tile fetch callback.

Takes no routed input: the current work tile is supplied by the persistent-loop machinery through `stage_info`, so schedules call `wq.fetch_work_tile()` directly.

producer_tail() → None

:   Drain in-flight pipeline stages after the persistent loop exits.
