---
title: "task_scheduling.pipeline"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/ts_api/pipeline.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# task_scheduling.pipeline {#module-cutlass.experimental.task_scheduling.pipeline}

TS pipeline overloads.

CuTeDSL TMA pipelines arm transaction barriers from every warp that calls `producer_acquire` because `MbarrierArray.arrive_and_expect_tx` is guarded only by per-warp `elect_one`. TS sometimes intentionally maps several TMA producer warps in one task to the same full barrier and wants one software arrival carrying the aggregate transaction byte count. `TaskWarpLeader` therefore keeps every producer warp in the empty-barrier acquire path, but guards the full-barrier transaction arrive to the first warp assigned to the task.

cutlass.experimental.task_scheduling.pipeline.dsl_user_op(*fn=None*, *\*args*, *\*\*kwargs*)

:

*class* cutlass.experimental.task_scheduling.pipeline.TSPipelineTmaAsync(

*sync_object_full: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*sync_object_empty: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*num_stages: int*,

*producer_mask: \_MockObject \| None*,

*consumer_mask: \_MockObject \| None*,

*is_signaling_thread: \_MockObject*,

*is_task_warp_leader: \_MockObject*,

)

Bases: [`PipelineTmaAsync`](../api/pipeline.md#cutlass.pipeline.PipelineTmaAsync)

TMA async pipeline with TS task-warp-leader barrier arming.

is_task_warp_leader

:   Predicate selecting the first warp assigned to the task. Only this warp arms the transaction barrier when TS uses task-warp-leader signaling.

    Type:

    :   [Boolean](../basic_data_types.md#cutlass.Boolean)

<!-- -->

is_task_warp_leader*: \_MockObject*

:

*static* create(

*\**,

*num_stages: int*,

*producer_group: [CooperativeGroup](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)*,

*consumer_group: [CooperativeGroup](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)*,

*tx_count: int*,

*barrier_storage: cutlass.cute.typing.Pointer*,

*is_task_warp_leader: \_MockObject*,

*cta_layout_vmnk: cutlass.cute.typing.Layout \| None = None*,

*mcast_mode_mn: tuple\[int, int\] = (1, 1)*,

*defer_sync: bool = False*,

) → [TSPipelineTmaAsync](#cutlass.experimental.task_scheduling.pipeline.TSPipelineTmaAsync)

Create an TS TMA async pipeline.

Parameters:

:   - **num_stages** (*int*) -- Number of pipeline stages.

    - **producer_group** ([*cutlass.pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups for producer and consumer sides.

    - **consumer_group** ([*cutlass.pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups for producer and consumer sides.

    - **tx_count** (*int*) -- Transaction byte count expected by the full barrier.

    - **barrier_storage** (*cute.Pointer*) -- SMEM mbarrier storage.

    - **is_task_warp_leader** ([*Boolean*](../basic_data_types.md#cutlass.Boolean)) -- Predicate controlling task-warp-leader transaction arming.

    - **cta_layout_vmnk** (*cute.Layout,* *optional*) -- Cluster layout.

    - **mcast_mode_mn** (*tuple\[int,* *int\],* *optional*) -- TMA multicast mode.

    - **defer_sync** (*bool,* *optional*) -- Defer pipeline initialization synchronization.

Returns:

:   Pipeline instance with TS producer acquire semantics.

Return type:

:   [TSPipelineTmaAsync](#cutlass.experimental.task_scheduling.pipeline.TSPipelineTmaAsync)

producer_acquire(

*state: [PipelineState](../api/pipeline.md#cutlass.pipeline.PipelineState)*,

*try_acquire_token: \_MockObject \| None = None*,

) → None

Wait for empty on all producer warps; arm full only on task leader.

\_\_init\_\_(

*sync_object_full: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*sync_object_empty: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*num_stages: int*,

*producer_mask: \_MockObject \| None*,

*consumer_mask: \_MockObject \| None*,

*is_signaling_thread: \_MockObject*,

*is_task_warp_leader: \_MockObject*,

) → None

*class* cutlass.experimental.task_scheduling.pipeline.TSPipelineUmmaUmma(

*sync_object_full: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*sync_object_empty: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*num_stages: int*,

*producer_mask: \_MockObject \| None*,

*consumer_mask: \_MockObject \| None*,

*cta_group: [CtaGroup](../api/cute_nvgpu_tcgen05.md#cutlass.cute.nvgpu.tcgen05.CtaGroup)*,

)

Bases: [`PipelineAsync`](../api/pipeline.md#cutlass.pipeline.PipelineAsync)

Pipeline for a UMMA producer feeding a UMMA consumer.

cta_group

:   TCGen05 CTA group used by producer commit and consumer release.

    Type:

    :   [cute.nvgpu.tcgen05.CtaGroup](../api/cute_nvgpu_tcgen05.md#cutlass.cute.nvgpu.tcgen05.CtaGroup)

<!-- -->

cta_group*: [CtaGroup](../api/cute_nvgpu_tcgen05.md#cutlass.cute.nvgpu.tcgen05.CtaGroup)*

:

*static* create(

*\**,

*num_stages: int*,

*producer_group: [CooperativeGroup](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)*,

*consumer_group: [CooperativeGroup](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)*,

*barrier_storage: cutlass.cute.typing.Pointer*,

*cta_layout_vmnk: cutlass.cute.typing.Layout \| None = None*,

*defer_sync: bool = False*,

) → [TSPipelineUmmaUmma](#cutlass.experimental.task_scheduling.pipeline.TSPipelineUmmaUmma)

Create an async pipeline with TCGen05 arrivals on both sides.

Parameters:

:   - **num_stages** (*int*) -- Number of pipeline stages.

    - **producer_group** ([*cutlass.pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups for producer and consumer sides.

    - **consumer_group** ([*cutlass.pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups for producer and consumer sides.

    - **barrier_storage** (*cute.Pointer*) -- SMEM mbarrier storage.

    - **cta_layout_vmnk** (*cute.Layout,* *optional*) -- Cluster layout used to select CTA group and peer masks.

    - **defer_sync** (*bool,* *optional*) -- Defer pipeline initialization synchronization.

Returns:

:   Pipeline with UMMA-style full and empty barrier arrivals.

Return type:

:   [TSPipelineUmmaUmma](#cutlass.experimental.task_scheduling.pipeline.TSPipelineUmmaUmma)

producer_commit(

*state: [PipelineState](../api/pipeline.md#cutlass.pipeline.PipelineState)*,

) → None

UMMA producer commit of the full barrier.

consumer_release(

*state: [PipelineState](../api/pipeline.md#cutlass.pipeline.PipelineState)*,

) → None

UMMA consumer release of the empty barrier.

\_\_init\_\_(

*sync_object_full: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*sync_object_empty: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*num_stages: int*,

*producer_mask: \_MockObject \| None*,

*consumer_mask: \_MockObject \| None*,

*cta_group: [CtaGroup](../api/cute_nvgpu_tcgen05.md#cutlass.cute.nvgpu.tcgen05.CtaGroup)*,

) → None

*class* cutlass.experimental.task_scheduling.pipeline.TSPipelineAsyncUmma(

*sync_object_full: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*sync_object_empty: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*num_stages: int*,

*producer_mask: \_MockObject \| None*,

*consumer_mask: \_MockObject \| None*,

*cta_group: [CtaGroup](../api/cute_nvgpu_tcgen05.md#cutlass.cute.nvgpu.tcgen05.CtaGroup)*,

*producer_op: [PipelineOp](../api/pipeline.md#cutlass.pipeline.PipelineOp) = PipelineOp.AsyncThread*,

)

Bases: [`PipelineAsyncUmma`](../api/pipeline.md#cutlass.pipeline.PipelineAsyncUmma)

TS async-producer pipeline feeding a UMMA consumer.

CUTLASS `PipelineUmmaConsumerAsync` does not use the generic async-thread destination-rank arrive for 2SM producer commits. It clears SM100's peer bit on the full-barrier address and emits one cluster-shared mbarrier arrive per producer thread, targeting SM0 of the collaborating pair.

producer_op*: [PipelineOp](../api/pipeline.md#cutlass.pipeline.PipelineOp) = 1*

:

*static* create(

*\**,

*num_stages: int*,

*producer_group: [CooperativeGroup](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)*,

*consumer_group: [CooperativeGroup](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)*,

*barrier_storage: cutlass.cute.typing.Pointer*,

*cta_layout_vmnk: cutlass.cute.typing.Layout \| None = None*,

*producer_op: [PipelineOp](../api/pipeline.md#cutlass.pipeline.PipelineOp) = PipelineOp.AsyncThread*,

*defer_sync: bool = False*,

) → [TSPipelineAsyncUmma](#cutlass.experimental.task_scheduling.pipeline.TSPipelineAsyncUmma)

Create an TS async-producer to UMMA-consumer pipeline.

Parameters:

:   - **num_stages** (*int*) -- Number of pipeline stages.

    - **producer_group** ([*cutlass.pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups for producer and consumer sides.

    - **consumer_group** ([*cutlass.pipeline.CooperativeGroup*](../api/pipeline.md#cutlass.pipeline.CooperativeGroup)) -- Cooperative groups for producer and consumer sides.

    - **barrier_storage** (*cute.Pointer*) -- SMEM mbarrier storage.

    - **cta_layout_vmnk** (*cute.Layout,* *optional*) -- Cluster layout.

    - **producer_op** ([*cutlass.pipeline.PipelineOp*](../api/pipeline.md#cutlass.pipeline.PipelineOp)*,* *optional*) -- Producer-side operation, either `AsyncThread` or `AsyncLoad`.

    - **defer_sync** (*bool,* *optional*) -- Defer pipeline initialization synchronization.

Returns:

:   Pipeline instance with TS producer commit semantics.

Return type:

:   [TSPipelineAsyncUmma](#cutlass.experimental.task_scheduling.pipeline.TSPipelineAsyncUmma)

producer_commit(

*state: [PipelineState](../api/pipeline.md#cutlass.pipeline.PipelineState)*,

) → None

Publish full barrier using CUTLASS 2SM UMMA-consumer semantics.

\_\_init\_\_(

*sync_object_full: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*sync_object_empty: [SyncObject](../api/pipeline.md#cutlass.pipeline.SyncObject)*,

*num_stages: int*,

*producer_mask: \_MockObject \| None*,

*consumer_mask: \_MockObject \| None*,

*cta_group: [CtaGroup](../api/cute_nvgpu_tcgen05.md#cutlass.cute.nvgpu.tcgen05.CtaGroup)*,

*producer_op: [PipelineOp](../api/pipeline.md#cutlass.pipeline.PipelineOp) = PipelineOp.AsyncThread*,

) → None
