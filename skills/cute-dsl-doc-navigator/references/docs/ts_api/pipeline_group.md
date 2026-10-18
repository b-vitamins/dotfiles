---
title: "task_scheduling.pipeline_group"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/ts_api/pipeline_group.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# task_scheduling.pipeline_group {#module-cutlass.experimental.task_scheduling.pipeline_group}

PipelineGroup --- shared pipeline barriers for fork/merge dataflow patterns.

`PipelineGroup` is a `MemoryResource` subclass (`is_barrier=True`) that groups multiple data resources (members) under a shared set of pipeline barriers. It enables:

- **Merge** (N producers → 1 consumer): multiple producers each fill their own member; one consumer waits for all of them.

- **Fork** (1 producer → N consumers): one producer fills all members; multiple consumers each read their own member.

- **FusedMerge** (N producers → 1 consumer): like `Merge` but with a single shared full barrier instead of one per producer.

## Barrier Layout (Heterogeneous)

`Merge` (N producers, 1 consumer) with S pipeline stage:

    [full_0 × S] [full_1 × S] … [full_{N-1} × S] [shared_empty × S]
    Total entries: (N + 1) × S

Each member's pipeline object receives a pointer to its dedicated full barrier section and the shared empty barrier.

`Fork` (1 producer, N consumers) --- shared full barrier, per-member empty barriers:

    [shared_full × S] [empty_0 × S] [empty_1 × S] … [empty_{N-1} × S]
    Total entries: (N + 1) × S

Each member's pipeline object receives a pointer to the shared full barrier and its dedicated empty barrier section.

Both layouts use `(N + 1) × S` barrier entries total.

`FusedMerge` (N producers, 1 consumer) --- shared full AND shared empty barrier:

    [shared_full × S] [shared_empty × S]
    Total entries: 2 × S

Every member's pipeline object receives a pointer to the shared full barrier and the shared empty barrier.

## Allowed Pipeline Types

All six public `PipelineType` values are supported:

  PipelineType   Producer   Consumer   Pipeline class
  -------------- ---------- ---------- -----------------------
  AsyncAsync     async      async      `PipelineAsync`
  TmaAsync       tma        async      `PipelineTmaAsync`
  TmaUmma        tma        umma       `PipelineTmaUmma`
  UmmaAsync      umma       async      `PipelineUmmaAsync`
  AsyncUmma      async      umma       `TSPipelineAsyncUmma`
  UmmaUmma       umma       umma       `TSPipelineUmmaUmma`

Valid heterogeneous merge pairs (consumer side homogeneous):

- Consumer = `async`: AsyncAsync + TmaAsync, AsyncAsync + UmmaAsync, TmaAsync + UmmaAsync

- Consumer = `umma`: TmaUmma + AsyncUmma, TmaUmma + UmmaUmma, AsyncUmma + UmmaUmma

Valid heterogeneous fork pairs (producer side homogeneous):

- Producer = `tma`: TmaAsync + TmaUmma

- Producer = `async`: AsyncAsync + AsyncUmma

- Producer = `umma`: UmmaAsync + UmmaUmma

## Producer / Consumer Decomposition

The merge/fork topology is auto-derived from the kind decomposition:

- Producer kinds differ → **Merge**

- Consumer kinds differ → **Fork**

- Both homogeneous → uses explicit `mode` parameter

- Both differ → **Error**

*class* cutlass.experimental.task_scheduling.pipeline_group.PipelineGroup(

*\**,

*name: \~sphinx.ext.autodoc.mock.\_MockObject = \'\'*,

*is_barrier: \~sphinx.ext.autodoc.mock.\_MockObject = False*,

*pipeline_config: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*producer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*pipeline: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_wait_signaling_threads: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*members: \~typing.List\[\~cutlass.experimental.task_scheduling.resources.MemoryResource\] = \<factory\>*,

*mode: \~sphinx.ext.autodoc.mock.\_MockObject = PipelineGroupMode.Merge*,

)

Bases: [`MemoryResource`](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)

Shared pipeline barriers derived by merging member pipeline configs.

Each member declares its own `pipeline_config` exactly as it would for a standalone pipeline. The group validates those member configs and creates a shared barrier layout for the side driven by a single task:

- `Merge` collapses the consumer side. Members commit individually, and the shared consumer task calls `group.release()` once.

- `Fork` collapses the producer side. Members release individually, and the shared producer task calls `group.commit()` once.

- `FusedMerge` collapses both sides under shared barriers. Members commit individually, and the shared consumer task calls `group.wait()` and `group.release()` once.

`PipelineGroup`:

1.  Validates compatibility (same `num_stages` and compatible `pipeline_type` on the collapsed side).

2.  Derives a merged config from the member configs. The collapsed side's cooperative-group size must match across members; the many side keeps per-member barriers.

3.  Creates one shared pipeline with `(N + 1) * num_stages` barriers.

4.  Re-points each member at the shared barrier allocation.

Raises `ValueError` if member configs are not mergeable.

When an explicit `pipeline_config` is supplied by the caller it is validated against the members using the same mode-specific compatibility rules. When `pipeline_config` is `None`, it is derived automatically.

members

:   Data resources whose pipelines are merged into this group.

    Type:

    :   List\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\]

<!-- -->

mode

:   `Merge` - N producers, 1 consumer; collapse the consumer side. `Fork` - 1 producer, N consumers; collapse the producer side. `FusedMerge` - N producers, 1 consumer; collapse both sides.

    Type:

    :   [PipelineGroupMode](enums.md#cutlass.experimental.task_scheduling.enums.PipelineGroupMode)

<!-- -->

members*: List\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\]*

:

<!-- -->

mode*: \_MockObject = \'Merge\'*

:

<!-- -->

*property* group_pipeline*: \_MockObject*

:   Access the first member's pipeline for group-level ops.

    Group-level barrier operations need a pipeline object to call the underlying barrier API. Instead of aliasing `self.pipeline` (which breaks under `inject_leaves`), this property reads the first member's pipeline on demand.

<!-- -->

*property* is_heterogeneous*: bool*

:   True when members have different pipeline types.

    Heterogeneous groups maintain per-member full barriers and a shared empty barrier, whereas homogeneous groups share a single merged pipeline across all members.

<!-- -->

*property* num_barriers_per_stage*: int*

:   Number of distinct mbarriers this group's layout uses per stage.

<!-- -->

create() → None

:   Create pipeline(s) for this group and assign to members.

    Merge/Fork use per-member barriers. FusedMerge uses shared barriers but members still have distinct pipelines to arrive with their own op conventions.

    A per-member pipeline object of the correct type is constructed and assigned to `m.pipeline`.

\_\_init\_\_(

*\**,

*name: \~sphinx.ext.autodoc.mock.\_MockObject = \'\'*,

*is_barrier: \~sphinx.ext.autodoc.mock.\_MockObject = False*,

*pipeline_config: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*producer_vars: \~sphinx.ext.autodoc.mock.\_MockObject = \<factory\>*,

*pipeline: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*consumer_wait_signaling_threads: \~sphinx.ext.autodoc.mock.\_MockObject \| None = None*,

*members: \~typing.List\[\~cutlass.experimental.task_scheduling.resources.MemoryResource\] = \<factory\>*,

*mode: \~sphinx.ext.autodoc.mock.\_MockObject = PipelineGroupMode.Merge*,

) → None
