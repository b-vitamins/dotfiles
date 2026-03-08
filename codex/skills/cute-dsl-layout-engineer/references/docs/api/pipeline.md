---
title: "cutlass.pipeline"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/pipeline.html"
cutlass_docs_version: "4.4.1"
generated_at: "2026-03-07T11:33:21+00:00"
last_updated: "Mar 05, 2026"
---

# cutlass.pipeline

## Agent

**Kind:** class

**Qualified name:** `cutlass.pipeline.Agent`

**Signature:** `Agent(value)`

Bases: `Enum`

Agent indicates what is participating in the pipeline synchronization.

### Thread

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.Agent.Thread`

**Signature:** `Thread = 1`

### ThreadBlock

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.Agent.ThreadBlock`

**Signature:** `ThreadBlock = 2`

### ThreadBlockCluster

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.Agent.ThreadBlockCluster`

**Signature:** `ThreadBlockCluster = 3`

## CooperativeGroup

**Kind:** class

**Qualified name:** `cutlass.pipeline.CooperativeGroup`

**Signature:** `CooperativeGroup(agent: Agent, size: int = 1, alignment = None)`

Bases: `object`

CooperativeGroup contains size and alignment restrictions for an Agent.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.CooperativeGroup.__init__`

**Signature:** `__init__(agent: Agent, size: int = 1, alignment = None)`

## PipelineOp

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineOp`

**Signature:** `PipelineOp(value)`

Bases: `Enum`

PipelineOp assigns an operation to an agent corresponding to a specific hardware feature.

### AsyncThread

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOp.AsyncThread`

**Signature:** `AsyncThread = 1`

### TCGen05Mma

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOp.TCGen05Mma`

**Signature:** `TCGen05Mma = 2`

### TmaLoad

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOp.TmaLoad`

**Signature:** `TmaLoad = 3`

### ClcLoad

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOp.ClcLoad`

**Signature:** `ClcLoad = 4`

### TmaStore

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOp.TmaStore`

**Signature:** `TmaStore = 5`

### Composite

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOp.Composite`

**Signature:** `Composite = 6`

### AsyncLoad

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOp.AsyncLoad`

**Signature:** `AsyncLoad = 7`

## SyncObject

**Kind:** class

**Qualified name:** `cutlass.pipeline.SyncObject`

**Signature:** `SyncObject`

Bases: `ABC`

Abstract base class for hardware synchronization primitives.

This class defines the interface for different types of hardware synchronization mechanisms including shared memory barriers, named barriers, and fences.

### arrive

**Kind:** method

**Qualified name:** `cutlass.pipeline.SyncObject.arrive`

**Signature:** `abstract arrive() → None`

### wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.SyncObject.wait`

**Signature:** `abstract wait() → None`

### arrive_and_wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.SyncObject.arrive_and_wait`

**Signature:** `abstract arrive_and_wait() → None`

### arrive_and_drop

**Kind:** method

**Qualified name:** `cutlass.pipeline.SyncObject.arrive_and_drop`

**Signature:** `abstract arrive_and_drop() → None`

### get_barrier

**Kind:** method

**Qualified name:** `cutlass.pipeline.SyncObject.get_barrier`

**Signature:** `abstract get_barrier() → cutlass.cute.typing.Pointer | int | None`

### max

**Kind:** method

**Qualified name:** `cutlass.pipeline.SyncObject.max`

**Signature:** `abstract max() → int | None`

### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.SyncObject._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

## MbarrierArray

**Kind:** class

**Qualified name:** `cutlass.pipeline.MbarrierArray`

**Signature:** `MbarrierArray`

Bases: [`SyncObject`](#cutlass.pipeline.SyncObject)

MbarrierArray implements an abstraction for an array of smem barriers.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.__init__`

**Signature:** `__init__(barrier_storage: cutlass.cute.typing.Pointer, num_stages: int, agent: tuple[PipelineOp, CooperativeGroup], tx_count: int = 0, *, loc = None, ip = None) → None`

### recast_to_new_op_type

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.recast_to_new_op_type`

**Signature:** `recast_to_new_op_type(new_op_type: PipelineOp) → MbarrierArray`

Creates a copy of MbarrierArray with a different op_type without re-initializing barriers

### mbarrier_init

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.mbarrier_init`

**Signature:** `mbarrier_init(*, loc = None, ip = None) → None`

Initializes an array of mbarriers using warp 0.

### arrive

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.arrive`

**Signature:** `arrive(index: int, dst: int, cta_group: CtaGroup | None = None, *, loc = None, ip = None) → None`

Select the arrive corresponding to this MbarrierArray's PipelineOp.

#### Parameters

-   **index** (*int*) -- Index of the mbarrier in the array to arrive on

-   **dst** (*int* *\|* *None*) -- Destination parameter for selective arrival, which can be either a mask or destination cta rank. When None, both `TCGen05Mma` and `AsyncThread` will arrive on their local mbarrier. - For `TCGen05Mma`, `dst` serves as a multicast mask (e.g., 0b1011 allows arrive signal to be multicast to CTAs in the cluster with rank = 0, 1, and 3). - For `AsyncThread`, `dst` serves as a destination cta rank (e.g., 3 means threads will arrive on the mbarrier with rank = 3 in the cluster).

-   **cta_group** (`cute.nvgpu.tcgen05.CtaGroup`, optional) -- CTA group for `TCGen05Mma`, defaults to None for other op types

### arrive_mbarrier

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.arrive_mbarrier`

**Signature:** `arrive_mbarrier(index: int, dst_rank: int | None = None, *, loc = None, ip = None) → None`

### arrive_cp_async_mbarrier

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.arrive_cp_async_mbarrier`

**Signature:** `arrive_cp_async_mbarrier(index: int, *, loc = None, ip = None)`

### arrive_tcgen05mma

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.arrive_tcgen05mma`

**Signature:** `arrive_tcgen05mma(index: int, mask: int | None, cta_group: CtaGroup, *, loc = None, ip = None) → None`

### arrive_and_expect_tx

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.arrive_and_expect_tx`

**Signature:** `arrive_and_expect_tx(index: int, tx_count: int, *, loc = None, ip = None) → None`

### arrive_and_expect_tx_with_dst

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.arrive_and_expect_tx_with_dst`

**Signature:** `arrive_and_expect_tx_with_dst(index: int, tx_count: int, dst: int | None = None, *, loc = None, ip = None) → None`

### try_wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.try_wait`

**Signature:** `try_wait(index: int, phase: int, *, loc = None, ip = None) → cutlass.cutlass_dsl.Boolean`

### wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.wait`

**Signature:** `wait(index: int, phase: int, *, loc = None, ip = None) → None`

### arrive_and_wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.arrive_and_wait`

**Signature:** `arrive_and_wait(index: int, phase: int, dst: int, cta_group: CtaGroup | None = None, *, loc = None, ip = None) → None`

### arrive_and_drop

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.arrive_and_drop`

**Signature:** `arrive_and_drop(*, loc = None, ip = None) → None`

### get_barrier

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.get_barrier`

**Signature:** `get_barrier(index: int, *, loc = None, ip = None) → cutlass.cute.typing.Pointer`

### max

**Kind:** method

**Qualified name:** `cutlass.pipeline.MbarrierArray.max`

**Signature:** `max() → int`

### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.MbarrierArray._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

## NamedBarrier

**Kind:** class

**Qualified name:** `cutlass.pipeline.NamedBarrier`

**Signature:** `NamedBarrier(barrier_id: int, num_threads: int)`

Bases: [`SyncObject`](#cutlass.pipeline.SyncObject)

NamedBarrier is an abstraction for named barriers managed by hardware. There are 16 named barriers available, with barrier_ids 0-15.

See the [PTX documentation](https://https://docs.nvidia.com/cuda/parallel-thread-execution/#parallel-synchronization-and-communication-instructions-bar).

### barrier_id

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.NamedBarrier.barrier_id`

**Signature:** `barrier_id: int`

### num_threads

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.NamedBarrier.num_threads`

**Signature:** `num_threads: int`

### arrive

**Kind:** method

**Qualified name:** `cutlass.pipeline.NamedBarrier.arrive`

**Signature:** `arrive(*, loc = None, ip = None) → None`

The aligned flavor of arrive is used when all threads in the CTA will execute the same instruction. See PTX documentation.

### arrive_unaligned

**Kind:** method

**Qualified name:** `cutlass.pipeline.NamedBarrier.arrive_unaligned`

**Signature:** `arrive_unaligned(*, loc = None, ip = None) → None`

The unaligned flavor of arrive can be used with an arbitrary number of threads in the CTA.

### wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.NamedBarrier.wait`

**Signature:** `wait(*, loc = None, ip = None) → None`

NamedBarriers do not have a standalone wait like mbarriers, only an arrive_and_wait. If synchronizing two warps in a producer/consumer pairing, the arrive count would be 32 using mbarriers but 64 using NamedBarriers. Only threads from either the producer or consumer are counted for mbarriers, while all threads participating in the sync are counted for NamedBarriers.

### wait_unaligned

**Kind:** method

**Qualified name:** `cutlass.pipeline.NamedBarrier.wait_unaligned`

**Signature:** `wait_unaligned(*, loc = None, ip = None) → None`

### arrive_and_wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.NamedBarrier.arrive_and_wait`

**Signature:** `arrive_and_wait(*, loc = None, ip = None) → None`

### arrive_and_drop

**Kind:** method

**Qualified name:** `cutlass.pipeline.NamedBarrier.arrive_and_drop`

**Signature:** `arrive_and_drop(*, loc = None, ip = None) → None`

### sync

**Kind:** method

**Qualified name:** `cutlass.pipeline.NamedBarrier.sync`

**Signature:** `sync(*, loc = None, ip = None) → None`

### get_barrier

**Kind:** method

**Qualified name:** `cutlass.pipeline.NamedBarrier.get_barrier`

**Signature:** `get_barrier(*, loc = None, ip = None) → int`

### max

**Kind:** method

**Qualified name:** `cutlass.pipeline.NamedBarrier.max`

**Signature:** `max() → int`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.NamedBarrier.__init__`

**Signature:** `__init__(barrier_id: int, num_threads: int) → None`

### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.NamedBarrier._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

## PipelineOrder

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineOrder`

**Signature:** `PipelineOrder(sync_object_full: SyncObject, depth: int, length: int, group_id: int, state: PipelineState)`

Bases: `object`

PipelineOrder is used for managing ordered pipeline execution with multiple groups.

This class implements a pipeline ordering mechanism where work is divided into groups and stages, allowing for controlled progression through pipeline stages with proper synchronization between different groups.

The pipeline ordering works as follows:

-   The pipeline is divided into 'length' number of groups

-   Each group has 'depth' number of stages

-   Groups execute in a specific order with synchronization barriers

-   Each group waits for the previous group to complete before proceeding

**Example:**

    # Create pipeline order with 3 groups, each with 2 stages
    pipeline_order = PipelineOrder.create(
        barrier_storage=smem_ptr,      # shared memory pointer for barriers
        depth=2,                       # 2 stages per group
        length=3,                      # 3 groups total
        group_id=0,                    # current group ID (0, 1, or 2)
        producer_group=producer_warp   # cooperative group for producers
    )

    # In the pipeline loop
    for stage in range(num_stages):
        pipeline_order.wait()          # Wait for previous group to complete
        # Process current stage
        pipeline_order.arrive()        # Signal completion to next group

### sync_object_full

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOrder.sync_object_full`

**Signature:** `sync_object_full: SyncObject`

### depth

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOrder.depth`

**Signature:** `depth: int`

### length

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOrder.length`

**Signature:** `length: int`

### group_id

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOrder.group_id`

**Signature:** `group_id: int`

### state

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineOrder.state`

**Signature:** `state: PipelineState`

### create

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineOrder.create`

**Signature:** `static create(barrier_storage: cutlass.cute.typing.Pointer, depth: int, length: int, group_id: int, producer_group: CooperativeGroup, defer_sync: bool = False)`

### get_barrier_for_current_stage_idx

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineOrder.get_barrier_for_current_stage_idx`

**Signature:** `get_barrier_for_current_stage_idx(group_id)`

### arrive

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineOrder.arrive`

**Signature:** `arrive(*, loc = None, ip = None)`

### wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineOrder.wait`

**Signature:** `wait(*, loc = None, ip = None)`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineOrder.__init__`

**Signature:** `__init__(sync_object_full: SyncObject, depth: int, length: int, group_id: int, state: PipelineState) → None`

## TmaStoreFence

**Kind:** class

**Qualified name:** `cutlass.pipeline.TmaStoreFence`

**Signature:** `TmaStoreFence(num_stages: int = 0)`

Bases: [`SyncObject`](#cutlass.pipeline.SyncObject)

TmaStoreFence is used for a multi-stage epilogue buffer.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.TmaStoreFence.__init__`

**Signature:** `__init__(num_stages: int = 0) → None`

### arrive

**Kind:** method

**Qualified name:** `cutlass.pipeline.TmaStoreFence.arrive`

**Signature:** `arrive(*, loc = None, ip = None) → None`

### wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.TmaStoreFence.wait`

**Signature:** `wait(*, loc = None, ip = None) → None`

### arrive_and_wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.TmaStoreFence.arrive_and_wait`

**Signature:** `arrive_and_wait(*, loc = None, ip = None) → None`

### arrive_and_drop

**Kind:** method

**Qualified name:** `cutlass.pipeline.TmaStoreFence.arrive_and_drop`

**Signature:** `arrive_and_drop(*, loc = None, ip = None) → None`

### get_barrier

**Kind:** method

**Qualified name:** `cutlass.pipeline.TmaStoreFence.get_barrier`

**Signature:** `get_barrier(*, loc = None, ip = None) → None`

### max

**Kind:** method

**Qualified name:** `cutlass.pipeline.TmaStoreFence.max`

**Signature:** `max() → None`

### tail

**Kind:** method

**Qualified name:** `cutlass.pipeline.TmaStoreFence.tail`

**Signature:** `tail(*, loc = None, ip = None) → None`

### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.TmaStoreFence._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

## PipelineUserType

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineUserType`

**Signature:** `PipelineUserType(value)`

Bases: `Enum`

An enumeration.

### Producer

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineUserType.Producer`

**Signature:** `Producer = 1`

### Consumer

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineUserType.Consumer`

**Signature:** `Consumer = 2`

### ProducerConsumer

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineUserType.ProducerConsumer`

**Signature:** `ProducerConsumer = 3`

## PipelineState

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineState`

**Signature:** `PipelineState(stages: int, count, index, phase)`

Bases: `object`

Pipeline state contains an index and phase bit corresponding to the current position in the circular buffer.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineState.__init__`

**Signature:** `__init__(stages: int, count, index, phase)`

### clone

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineState.clone`

**Signature:** `clone() → PipelineState`

### index

**Kind:** property

**Qualified name:** `cutlass.pipeline.PipelineState.index`

**Signature:** `index: cutlass.cutlass_dsl.Int32`

### count

**Kind:** property

**Qualified name:** `cutlass.pipeline.PipelineState.count`

**Signature:** `count: cutlass.cutlass_dsl.Int32`

### stages

**Kind:** property

**Qualified name:** `cutlass.pipeline.PipelineState.stages`

**Signature:** `stages: int`

### phase

**Kind:** property

**Qualified name:** `cutlass.pipeline.PipelineState.phase`

**Signature:** `phase: cutlass.cutlass_dsl.Int32`

### reset_count

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineState.reset_count`

**Signature:** `reset_count(*, loc = None, ip = None)`

### advance

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineState.advance`

**Signature:** `advance(*, loc = None, ip = None) → None`

### reverse

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineState.reverse`

**Signature:** `reverse(*, loc = None, ip = None)`

## PipelineAsync

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineAsync`

**Signature:** `PipelineAsync(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None)`

Bases: `object`

PipelineAsync is a generic pipeline class where both the producer and consumer are AsyncThreads. It also serves as a base class for specialized pipeline classes.

This class implements a producer-consumer pipeline pattern where both sides operate asynchronously. The pipeline maintains synchronization state using barrier objects to coordinate between producer and consumer threads.

The pipeline state transitions of one pipeline entry(mbarrier) can be represented as:

+-----------+-----------+------------+-----------+------------+-----------+
| Barrier   | State     | p.acquire  | p.commit  | c.wait     | c.release |
+===========+===========+============+===========+============+===========+
| empty_bar | empty     | \<Return\> | n/a       | n/a        | -         |
+-----------+-----------+------------+-----------+------------+-----------+
| empty_bar | wait      | \<Block\>  | n/a       | n/a        | -\> empty |
+-----------+-----------+------------+-----------+------------+-----------+
| full_bar  | wait      | n/a        | -\> full  | \<Block \> | n/a       |
+-----------+-----------+------------+-----------+------------+-----------+
| full_bar  | full      | n/a        | -         | \<Return\> | n/a       |
+-----------+-----------+------------+-----------+------------+-----------+

: Table 2 Pipeline State Transitions

Where:

-   p: producer

-   c: consumer

-   \<Block\>: This action is blocked until transition to a state allow it to proceed by other side - e.g. `p.acquire()` is blocked until `empty_bar` transition to `empty` state by `c.release()`

    Array of mbarriers as circular buffer:

         Advance Direction
       <-------------------

        Producer   Consumer
            |         ^
            V         |
       +-----------------+
     --|X|X|W|D|D|D|D|R|X|<-.
    /  +-----------------+   \
    |                        |
    `------------------------'

Where:

-   X: Empty buffer (initial state)

-   W: Producer writing (producer is waiting for buffer to be empty)

-   D: Data ready (producer has written data to buffer)

-   R: Consumer reading (consumer is consuming data from buffer)

**Example:**

    # Create pipeline with 5 stages
    pipeline = PipelineAsync.create(
        num_stages=5,                   # number of pipeline stages
        producer_group=producer_warp,
        consumer_group=consumer_warp
        barrier_storage=smem_ptr,       # smem pointer for array of mbarriers in shared memory
    )

    producer, consumer = pipeline.make_participants()
    # Producer side
    for i in range(num_iterations):
        handle = producer.acquire_and_advance()  # Wait for buffer to be empty & Move index to next stage
        # Write data to pipeline buffer
        handle.commit()   # Signal buffer is full

    # Consumer side
    for i in range(num_iterations):
        handle = consumer.wait_and_advance()     # Wait for buffer to be full & Move index to next stage
        # Read data from pipeline buffer
        handle.release()  # Signal buffer is empty

### sync_object_full

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineAsync.sync_object_full`

**Signature:** `sync_object_full: SyncObject`

### sync_object_empty

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineAsync.sync_object_empty`

**Signature:** `sync_object_empty: SyncObject`

### num_stages

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineAsync.num_stages`

**Signature:** `num_stages: int`

### producer_mask

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineAsync.producer_mask`

**Signature:** `producer_mask: cutlass.cutlass_dsl.Int32 | None`

### consumer_mask

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineAsync.consumer_mask`

**Signature:** `consumer_mask: cutlass.cutlass_dsl.Int32 | None`

### \_make_sync_object

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync._make_sync_object`

**Signature:** `static _make_sync_object(barrier_storage: cutlass.cute.typing.Pointer, num_stages: int, agent: tuple[PipelineOp, CooperativeGroup], tx_count: int = 0) → SyncObject`

Returns a SyncObject corresponding to an agent's PipelineOp.

### create

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.create`

**Signature:** `static create(*, num_stages: int, producer_group: CooperativeGroup, consumer_group: CooperativeGroup, barrier_storage: cutlass.cute.typing.Pointer | None = None, producer_mask: cutlass.cutlass_dsl.Int32 | None = None, consumer_mask: cutlass.cutlass_dsl.Int32 | None = None, defer_sync: bool = False)`

Creates and initializes a new PipelineAsync instance.

This helper function computes necessary attributes and returns an instance of PipelineAsync with the specified configuration for producer and consumer synchronization.

#### Parameters

-   **barrier_storage** (*cute.Pointer*) -- Pointer to the shared memory address for this pipeline's mbarriers

-   **num_stages** (*int*) -- Number of buffer stages for this pipeline

-   **producer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- `CooperativeGroup` for the producer agent

-   **consumer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- `CooperativeGroup` for the consumer agent

-   **producer_mask** (*Int32,* *optional*) -- Mask for signaling arrives for the producer agent

-   **consumer_mask** (*Int32,* *optional*) -- Mask for signaling arrives for the consumer agent

#### Raises

**ValueError** -- If barrier_storage is not a cute.Pointer instance

#### Returns

A new `PipelineAsync` instance

#### Return type

[PipelineAsync](#cutlass.pipeline.PipelineAsync)

### producer_acquire

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.producer_acquire`

**Signature:** `producer_acquire(state: PipelineState, try_acquire_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None)`

### producer_try_acquire

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.producer_try_acquire`

**Signature:** `producer_try_acquire(state: PipelineState, *, loc = None, ip = None)`

### producer_commit

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.producer_commit`

**Signature:** `producer_commit(state: PipelineState, *, loc = None, ip = None)`

### consumer_wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.consumer_wait`

**Signature:** `consumer_wait(state: PipelineState, try_wait_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None)`

### consumer_try_wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.consumer_try_wait`

**Signature:** `consumer_try_wait(state: PipelineState, *, loc = None, ip = None)`

### consumer_release

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.consumer_release`

**Signature:** `consumer_release(state: PipelineState, *, loc = None, ip = None)`

### producer_get_barrier

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.producer_get_barrier`

**Signature:** `producer_get_barrier(state: PipelineState, *, loc = None, ip = None) → cutlass.cute.typing.Pointer`

### producer_tail

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.producer_tail`

**Signature:** `producer_tail(state: PipelineState, *, loc = None, ip = None)`

Make sure the last used buffer empty signal is visible to producer. Producer tail is usually executed by producer before exit, to avoid dangling mbarrier arrive signals after kernel exit.

#### Parameters

**state** ([*PipelineState*](#cutlass.pipeline.PipelineState)) -- The pipeline state that points to next useful buffer

### make_producer

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.make_producer`

**Signature:** `make_producer(*, loc = None, ip = None)`

### make_consumer

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.make_consumer`

**Signature:** `make_consumer(*, loc = None, ip = None)`

### make_participants

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.make_participants`

**Signature:** `make_participants(*, loc = None, ip = None)`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsync.__init__`

**Signature:** `__init__(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None) → None`

## PipelineCpAsync

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineCpAsync`

**Signature:** `PipelineCpAsync(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None)`

Bases: [`PipelineAsync`](#cutlass.pipeline.PipelineAsync)

PipelineCpAsync is used for CpAsync producers and AsyncThread consumers

### create

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineCpAsync.create`

**Signature:** `static create(barrier_storage: cutlass.cute.typing.Pointer, num_stages: cutlass.cutlass_dsl.Int32, producer_group: CooperativeGroup, consumer_group: CooperativeGroup, producer_mask: cutlass.cutlass_dsl.Int32 | None = None, consumer_mask: cutlass.cutlass_dsl.Int32 | None = None, defer_sync: bool = False)`

Helper function that computes necessary attributes and returns a `PipelineCpAsync` instance.

#### Parameters

-   **barrier_storage** (*cute.Pointer*) -- Pointer to the shared memory address for this pipeline's mbarriers

-   **num_stages** (*Int32*) -- Number of buffer stages for this pipeline

-   **producer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- `CooperativeGroup` for the producer agent

-   **consumer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- `CooperativeGroup` for the consumer agent

-   **producer_mask** (*Int32,* *optional*) -- Mask for signaling arrives for the producer agent, defaults to None

-   **consumer_mask** (*Int32,* *optional*) -- Mask for signaling arrives for the consumer agent, defaults to None

#### Returns

A new `PipelineCpAsync` instance configured with the provided parameters

#### Return type

[PipelineCpAsync](#cutlass.pipeline.PipelineCpAsync)

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineCpAsync.__init__`

**Signature:** `__init__(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None) → None`

## PipelineTmaAsync

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineTmaAsync`

**Signature:** `PipelineTmaAsync(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, is_signalling_thread: cutlass.cutlass_dsl.Boolean)`

Bases: [`PipelineAsync`](#cutlass.pipeline.PipelineAsync)

PipelineTmaAsync is used for TMA producers and AsyncThread consumers (e.g. Hopper mainloops).

### is_signalling_thread

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineTmaAsync.is_signalling_thread`

**Signature:** `is_signalling_thread: cutlass.cutlass_dsl.Boolean`

### init_empty_barrier_arrive_signal

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaAsync.init_empty_barrier_arrive_signal`

**Signature:** `static init_empty_barrier_arrive_signal(cta_layout_vmnk: cutlass.cute.typing.Layout, tidx: cutlass.cutlass_dsl.Int32, mcast_mode_mn: tuple[int, int] = (1, 1))`

Initialize the empty barrier arrive signal.

This function determines which threads should signal empty barrier arrives based on the cluster layout and multicast modes. It returns the destination CTA rank and whether the current thread should signal.

#### Parameters

-   **cta_layout_vmnk** (*cute.Layout*) -- Layout describing the cluster shape and CTA arrangement

-   **tidx** (*Int32*) -- Thread index within the warp

-   **mcast_mode_mn** (*tuple\[int,* *int\]*) -- Tuple specifying multicast modes for m and n dimensions (each 0 or 1), defaults to (1,1)

#### Raises

**AssertionError** -- If both multicast modes are disabled (0,0)

#### Returns

Tuple containing destination CTA rank and boolean indicating if current thread signals

#### Return type

tuple\[Int32, Boolean\]

### create

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaAsync.create`

**Signature:** `static create(*, num_stages: int, producer_group: CooperativeGroup, consumer_group: CooperativeGroup, tx_count: int, barrier_storage: cutlass.cute.typing.Pointer | None = None, cta_layout_vmnk: cutlass.cute.typing.Layout | None = None, tidx: cutlass.cutlass_dsl.Int32 | None = None, mcast_mode_mn: tuple[int, int] = (1, 1), defer_sync: bool = False)`

Create a new `PipelineTmaAsync` instance.

#### Parameters

-   **num_stages** (*int*) -- Number of buffer stages for this pipeline

-   **producer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- `CooperativeGroup` for the producer agent

-   **consumer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- `CooperativeGroup` for the consumer agent

-   **tx_count** (*int*) -- Number of bytes expected to be written to the transaction barrier for one stage

-   **barrier_storage** (*cute.Pointer,* *optional*) -- Pointer to the shared memory address for this pipeline's mbarriers, defaults to None

-   **cta_layout_vmnk** (*cute.Layout,* *optional*) -- Layout of the cluster shape, defaults to None

-   **tidx** (*Int32,* *optional*) -- Thread index to consumer async threads, defaults to None

-   **mcast_mode_mn** (*tuple\[int,* *int\],* *optional*) -- Tuple specifying multicast modes for m and n dimensions (each 0 or 1), defaults to (1,1)

#### Raises

**ValueError** -- If barrier_storage is not a cute.Pointer instance

#### Returns

New `PipelineTmaAsync` instance

#### Return type

[PipelineTmaAsync](#cutlass.pipeline.PipelineTmaAsync)

### producer_acquire

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaAsync.producer_acquire`

**Signature:** `producer_acquire(state: PipelineState, try_acquire_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None)`

TMA producer commit conditionally waits on buffer empty and sets the transaction barrier.

### producer_commit

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaAsync.producer_commit`

**Signature:** `producer_commit(state: PipelineState, *, loc = None, ip = None)`

TMA producer commit is a noop since TMA instruction itself updates the transaction count.

### consumer_release

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaAsync.consumer_release`

**Signature:** `consumer_release(state: PipelineState, *, loc = None, ip = None)`

TMA consumer release conditionally signals the empty buffer to the producer.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaAsync.__init__`

**Signature:** `__init__(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, is_signalling_thread: cutlass.cutlass_dsl.Boolean) → None`

## PipelineTmaUmma

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineTmaUmma`

**Signature:** `PipelineTmaUmma(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, is_leader_cta: bool, cta_group: CtaGroup)`

Bases: [`PipelineAsync`](#cutlass.pipeline.PipelineAsync)

PipelineTmaUmma is used for TMA producers and UMMA consumers (e.g. Blackwell mainloops).

### is_leader_cta

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineTmaUmma.is_leader_cta`

**Signature:** `is_leader_cta: bool`

### cta_group

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineTmaUmma.cta_group`

**Signature:** `cta_group: CtaGroup`

### \_make_sync_object

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaUmma._make_sync_object`

**Signature:** `_make_sync_object(barrier_storage: cutlass.cute.typing.Pointer, num_stages: int, agent: tuple[PipelineOp, CooperativeGroup], tx_count: int = 0, *, loc = None, ip = None) → SyncObject`

Returns a SyncObject corresponding to an agent's PipelineOp.

### \_compute_mcast_arrival_mask

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaUmma._compute_mcast_arrival_mask`

**Signature:** `_compute_mcast_arrival_mask(cta_layout_vmnk: cutlass.cute.typing.Layout, mcast_mode_mn: tuple[int, int], *, loc = None, ip = None)`

Computes a mask for signaling arrivals to multicasting threadblocks.

### \_compute_is_leader_cta

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaUmma._compute_is_leader_cta`

**Signature:** `_compute_is_leader_cta(cta_layout_vmnk: cutlass.cute.typing.Layout, *, loc = None, ip = None)`

Computes leader threadblocks for 2CTA kernels. For 1CTA, all threadblocks are leaders.

### create

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaUmma.create`

**Signature:** `create(*, num_stages: int, producer_group: CooperativeGroup, consumer_group: CooperativeGroup, tx_count: int, barrier_storage: cutlass.cute.typing.Pointer = None, cta_layout_vmnk: cutlass.cute.typing.Layout | None = None, mcast_mode_mn: tuple[int, int] = (1, 1), defer_sync: bool = False, loc = None, ip = None)`

Creates and initializes a new PipelineTmaUmma instance.

#### Parameters

-   **num_stages** (*int*) -- Number of buffer stages for this pipeline

-   **producer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- CooperativeGroup for the producer agent

-   **consumer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- CooperativeGroup for the consumer agent

-   **tx_count** (*int*) -- Number of bytes expected to be written to the transaction barrier for one stage

-   **barrier_storage** (*cute.Pointer,* *optional*) -- Pointer to the shared memory address for this pipeline's mbarriers

-   **cta_layout_vmnk** (*cute.Layout,* *optional*) -- Layout of the cluster shape

-   **mcast_mode_mn** (*tuple\[int,* *int\],* *optional*) -- Tuple specifying multicast modes for m and n dimensions (each 0 or 1)

#### Raises

**ValueError** -- If barrier_storage is not a cute.Pointer instance

#### Returns

A new PipelineTmaUmma instance configured with the provided parameters

#### Return type

[PipelineTmaUmma](#cutlass.pipeline.PipelineTmaUmma)

### consumer_release

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaUmma.consumer_release`

**Signature:** `consumer_release(state: PipelineState, *, loc = None, ip = None)`

UMMA consumer release buffer empty, cta_group needs to be provided.

### producer_acquire

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaUmma.producer_acquire`

**Signature:** `producer_acquire(state: PipelineState, try_acquire_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None)`

TMA producer commit conditionally waits on buffer empty and sets the transaction barrier for leader threadblocks.

### producer_commit

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaUmma.producer_commit`

**Signature:** `producer_commit(state: PipelineState)`

TMA producer commit is a noop since TMA instruction itself updates the transaction count.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaUmma.__init__`

**Signature:** `__init__(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, is_leader_cta: bool, cta_group: CtaGroup) → None`

## PipelineAsyncUmma

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineAsyncUmma`

**Signature:** `PipelineAsyncUmma(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, cta_group: CtaGroup)`

Bases: [`PipelineAsync`](#cutlass.pipeline.PipelineAsync)

PipelineAsyncUmma is used for AsyncThread producers and UMMA consumers (e.g. Blackwell input fusion pipelines).

### cta_group

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineAsyncUmma.cta_group`

**Signature:** `cta_group: CtaGroup`

### \_compute_leading_cta_rank

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsyncUmma._compute_leading_cta_rank`

**Signature:** `_compute_leading_cta_rank(cta_v_size, *, loc = None, ip = None)`

Computes the leading CTA rank.

### \_compute_is_leader_cta

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsyncUmma._compute_is_leader_cta`

**Signature:** `_compute_is_leader_cta(cta_layout_vmnk: cutlass.cute.typing.Layout, *, loc = None, ip = None)`

Computes leader threadblocks for 2CTA kernels. For 1CTA, all threadblocks are leaders.

### \_compute_peer_cta_mask

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsyncUmma._compute_peer_cta_mask`

**Signature:** `_compute_peer_cta_mask(cta_layout_vmnk: cutlass.cute.typing.Layout, *, loc = None, ip = None)`

Computes a mask for signaling arrivals to multicasting threadblocks.

### create

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsyncUmma.create`

**Signature:** `create(*, num_stages: int, producer_group: CooperativeGroup, consumer_group: CooperativeGroup, barrier_storage: cutlass.cute.typing.Pointer = None, cta_layout_vmnk: cutlass.cute.typing.Layout | None = None, defer_sync: bool = False, loc = None, ip = None)`

Creates and initializes a new PipelineAsyncUmma instance.

#### Parameters

-   **num_stages** (*int*) -- Number of buffer stages for this pipeline

-   **producer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- CooperativeGroup for the producer agent

-   **consumer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- CooperativeGroup for the consumer agent

-   **barrier_storage** (*cute.Pointer,* *optional*) -- Pointer to the shared memory address for this pipeline's mbarriers

-   **cta_layout_vmnk** (*cute.Layout,* *optional*) -- Layout of the cluster shape

#### Raises

**ValueError** -- If barrier_storage is not a cute.Pointer instance

#### Returns

A new PipelineAsyncUmma instance configured with the provided parameters

#### Return type

[PipelineAsyncUmma](#cutlass.pipeline.PipelineAsyncUmma)

### consumer_release

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsyncUmma.consumer_release`

**Signature:** `consumer_release(state: PipelineState, *, loc = None, ip = None)`

UMMA consumer release buffer empty, cta_group needs to be provided.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineAsyncUmma.__init__`

**Signature:** `__init__(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, cta_group: CtaGroup) → None`

## PipelineUmmaAsync

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineUmmaAsync`

**Signature:** `PipelineUmmaAsync(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, cta_group: CtaGroup)`

Bases: [`PipelineAsync`](#cutlass.pipeline.PipelineAsync)

PipelineUmmaAsync is used for UMMA producers and AsyncThread consumers (e.g. Blackwell accumulator pipelines).

### cta_group

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineUmmaAsync.cta_group`

**Signature:** `cta_group: CtaGroup`

### \_compute_tmem_sync_mask

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineUmmaAsync._compute_tmem_sync_mask`

**Signature:** `_compute_tmem_sync_mask(cta_layout_vmnk: cutlass.cute.typing.Layout, *, loc = None, ip = None)`

Computes a mask to signal completion of tmem buffers for 2CTA kernels.

### \_compute_peer_cta_rank

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineUmmaAsync._compute_peer_cta_rank`

**Signature:** `_compute_peer_cta_rank(*, loc = None, ip = None)`

Computes a mask to signal release of tmem buffers for 2CTA kernels.

### create

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineUmmaAsync.create`

**Signature:** `create(*, num_stages: int, producer_group: CooperativeGroup, consumer_group: CooperativeGroup, barrier_storage: cutlass.cute.typing.Pointer = None, cta_layout_vmnk: cutlass.cute.typing.Layout | None = None, defer_sync: bool = False, loc = None, ip = None)`

Creates an instance of PipelineUmmaAsync with computed attributes.

#### Parameters

-   **num_stages** (*int*) -- Number of buffer stages for this pipeline

-   **producer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- `CooperativeGroup` for the producer agent

-   **consumer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- `CooperativeGroup` for the consumer agent

-   **barrier_storage** (*cute.Pointer,* *optional*) -- Pointer to the shared memory address for this pipeline's mbarriers

-   **cta_layout_vmnk** (*cute.Layout,* *optional*) -- Layout of the cluster shape

#### Raises

**ValueError** -- If barrier_storage is not a cute.Pointer instance

#### Returns

New instance of `PipelineUmmaAsync`

#### Return type

[PipelineUmmaAsync](#cutlass.pipeline.PipelineUmmaAsync)

### producer_commit

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineUmmaAsync.producer_commit`

**Signature:** `producer_commit(state: PipelineState, *, loc = None, ip = None)`

UMMA producer commit buffer full, cta_group needs to be provided.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineUmmaAsync.__init__`

**Signature:** `__init__(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, cta_group: CtaGroup) → None`

## PipelineClcFetchAsync

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync`

**Signature:** `PipelineClcFetchAsync(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, is_signalling_thread: cutlass.cutlass_dsl.Boolean)`

Bases: `object`

PipelineClcFetchAsync implements a producer-consumer pipeline for Cluster Launch Control based dynamic scheduling. Both producer and consumer operate asynchronously using barrier synchronization to coordinate across pipeline stages and cluster CTAs.

-   Producer: waits for empty buffer, signals full barrier with transection bytes across all CTAs in cluster, hardware autosignals each CTA's mbarrier when transaction bytes are written, then the satte advance to next buffer slot.

-   Consumer: waits for full barrier, then load respinse from local SMEM, then sigals CTA 0's empty barrier to allow buffer reuse.

### sync_object_full

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.sync_object_full`

**Signature:** `sync_object_full: SyncObject`

### sync_object_empty

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.sync_object_empty`

**Signature:** `sync_object_empty: SyncObject`

### num_stages

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.num_stages`

**Signature:** `num_stages: int`

### producer_mask

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.producer_mask`

**Signature:** `producer_mask: cutlass.cutlass_dsl.Int32 | None`

### consumer_mask

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.consumer_mask`

**Signature:** `consumer_mask: cutlass.cutlass_dsl.Int32 | None`

### is_signalling_thread

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.is_signalling_thread`

**Signature:** `is_signalling_thread: cutlass.cutlass_dsl.Boolean`

### \_init_full_barrier_arrive_signal

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync._init_full_barrier_arrive_signal`

**Signature:** `static _init_full_barrier_arrive_signal(cta_layout_vmnk: cutlass.cute.typing.Layout, tidx: cutlass.cutlass_dsl.Int32)`

Computes producer barrier signaling parameters, returns destination CTA rank (0 to cluster_size-1) based on thread ID, and a boolean flag indicating if this thread participates in signaling.

#### Parameters

-   **cta_layout_vmnk** -- Cluster layout defining CTA count

-   **tidx** -- Thread ID within the CTA

### create

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.create`

**Signature:** `static create(*, num_stages: int, producer_group: CooperativeGroup, consumer_group: CooperativeGroup, tx_count: int, barrier_storage: cutlass.cute.typing.Pointer | None = None, producer_mask: cutlass.cutlass_dsl.Int32 | None = None, consumer_mask: cutlass.cutlass_dsl.Int32 | None = None, cta_layout_vmnk: cutlass.cute.typing.Layout | None = None, defer_sync: bool = False)`

This helper function computes any necessary attributes and returns an instance of PipelineClcFetchAsync.

#### Parameters

-   **barrier_storage** (*cute.Pointer*) -- Pointer to the shared memory address for this pipeline's mbarriers

-   **num_stages** (*int*) -- Number of buffer stages for this pipeline

-   **producer_group** (*CooperativeGroup*) -- CooperativeGroup for the producer agent

-   **consumer_group** (*CooperativeGroup*) -- CooperativeGroup for the consumer agent

-   **tx_count** (*int*) -- Number of bytes expected to be written to the transaction barrier for one stage

-   **producer_mask** (*Int32, optional*) -- Mask for signaling arrives for the producer agent, defaults to None

-   **consumer_mask** (*Int32, optional*) -- Mask for signaling arrives for the consumer agent, defaults to None

### producer_acquire

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.producer_acquire`

**Signature:** `producer_acquire(state: PipelineState, try_acquire_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None)`

Producer acquire waits for empty buffer and sets transaction expectation on full barrier.

#### Parameters

-   **state** -- Pipeline state pointing to the current buffer stage

-   **try_acquire_token** -- Optional token to skip the empty barrier wait

### consumer_wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.consumer_wait`

**Signature:** `consumer_wait(state: PipelineState, try_wait_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None)`

Consumer waits for full barrier to be signaled by hardware multicast.

#### Parameters

-   **state** -- Pipeline state pointing to the current buffer stage

-   **try_wait_token** -- Optional token to skip the full barrier wait

### consumer_release

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.consumer_release`

**Signature:** `consumer_release(state: PipelineState, *, loc = None, ip = None)`

### producer_get_barrier

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.producer_get_barrier`

**Signature:** `producer_get_barrier(state: PipelineState, *, loc = None, ip = None) → cutlass.cute.typing.Pointer`

### producer_tail

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.producer_tail`

**Signature:** `producer_tail(state: PipelineState, try_acquire_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None)`

Ensures all in-flight buffers are released before producer exits.

#### Parameters

-   **state** -- Pipeline state with current position in the buffer

-   **try_acquire_token** -- Optional token to skip the empty barrier waits

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineClcFetchAsync.__init__`

**Signature:** `__init__(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, is_signalling_thread: cutlass.cutlass_dsl.Boolean) → None`

## PipelineTmaMultiConsumersAsync

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineTmaMultiConsumersAsync`

**Signature:** `PipelineTmaMultiConsumersAsync(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, is_leader_cta: bool, sync_object_empty_umma: SyncObject, sync_object_empty_async: SyncObject, cta_group: CtaGroup)`

Bases: [`PipelineAsync`](#cutlass.pipeline.PipelineAsync)

PipelineTmaMultiConsumersAsync is used for TMA producers and UMMA+Async consumers.

### is_leader_cta

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineTmaMultiConsumersAsync.is_leader_cta`

**Signature:** `is_leader_cta: bool`

### sync_object_empty_umma

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineTmaMultiConsumersAsync.sync_object_empty_umma`

**Signature:** `sync_object_empty_umma: SyncObject`

### sync_object_empty_async

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineTmaMultiConsumersAsync.sync_object_empty_async`

**Signature:** `sync_object_empty_async: SyncObject`

### cta_group

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineTmaMultiConsumersAsync.cta_group`

**Signature:** `cta_group: CtaGroup`

### create

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaMultiConsumersAsync.create`

**Signature:** `static create(*, num_stages: int, producer_group: CooperativeGroup, consumer_group_umma: CooperativeGroup, consumer_group_async: CooperativeGroup, tx_count: int, barrier_storage: cutlass.cute.typing.Pointer | None = None, cta_layout_vmnk: cutlass.cute.typing.Layout | None = None, defer_sync: bool = False)`

This helper function computes any necessary attributes and returns an instance of PipelineTmaMultiConsumersAsync.

#### Parameters

-   **barrier_storage** (*cute.Pointer*) -- Pointer to the smem address for this pipeline's mbarriers

-   **num_stages** (*Int32*) -- Number of buffer stages for this pipeline

-   **producer_group** (*CooperativeGroup*) -- CooperativeGroup for the producer agent

-   **consumer_group_umma** (*CooperativeGroup*) -- CooperativeGroup for the UMMA consumer agent

-   **consumer_group_async** (*CooperativeGroup*) -- CooperativeGroup for the AsyncThread consumer agent

-   **tx_count** (*int*) -- Number of bytes expected to be written to the transaction barrier for one stage

-   **cta_layout_vmnk** (*cute.Layout \| None*) -- Layout of the cluster shape

### producer_acquire

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaMultiConsumersAsync.producer_acquire`

**Signature:** `producer_acquire(state: PipelineState, try_acquire_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None)`

TMA producer acquire waits on buffer empty and sets the transaction barrier for leader threadblocks.

### producer_commit

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaMultiConsumersAsync.producer_commit`

**Signature:** `producer_commit(state: PipelineState, *, loc = None, ip = None)`

TMA producer commit is a noop since TMA instruction itself updates the transaction count.

### consumer_release

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaMultiConsumersAsync.consumer_release`

**Signature:** `consumer_release(state: PipelineState, op_type: PipelineOp, *, loc = None, ip = None)`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaMultiConsumersAsync.__init__`

**Signature:** `__init__(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None, is_leader_cta: bool, sync_object_empty_umma: SyncObject, sync_object_empty_async: SyncObject, cta_group: CtaGroup) → None`

## PipelineTmaStore

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineTmaStore`

**Signature:** `PipelineTmaStore(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None)`

Bases: [`PipelineAsync`](#cutlass.pipeline.PipelineAsync)

PipelineTmaStore is used for synchronizing TMA stores in the epilogue. It does not use mbarriers.

### create

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaStore.create`

**Signature:** `static create(*, num_stages: int, producer_group: CooperativeGroup)`

This helper function computes any necessary attributes and returns an instance of `PipelineTmaStore`.

#### Parameters

-   **num_stages** (*int*) -- Number of buffer stages for this pipeline

-   **producer_group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- `CooperativeGroup` for the producer agent

#### Returns

A new `PipelineTmaStore` instance

#### Return type

[PipelineTmaStore](#cutlass.pipeline.PipelineTmaStore)

### producer_acquire

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaStore.producer_acquire`

**Signature:** `producer_acquire(*, loc = None, ip = None)`

### producer_commit

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaStore.producer_commit`

**Signature:** `producer_commit(*, loc = None, ip = None)`

### consumer_wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaStore.consumer_wait`

**Signature:** `consumer_wait(*, loc = None, ip = None)`

### consumer_release

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaStore.consumer_release`

**Signature:** `consumer_release(*, loc = None, ip = None)`

### producer_tail

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaStore.producer_tail`

**Signature:** `producer_tail(*, loc = None, ip = None)`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineTmaStore.__init__`

**Signature:** `__init__(sync_object_full: SyncObject, sync_object_empty: SyncObject, num_stages: int, producer_mask: cutlass.cutlass_dsl.Int32 | None, consumer_mask: cutlass.cutlass_dsl.Int32 | None) → None`

## PipelineProducer

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineProducer`

**Signature:** `PipelineProducer(pipeline, state, group: CooperativeGroup)`

Bases: `object`

A class representing a producer in an asynchronous pipeline.

This class manages the producer side of an asynchronous pipeline, handling synchronization and state management for producing data. It provides methods for acquiring, committing, and advancing through pipeline stages.

### Variables

-   **\_\_pipeline** -- The asynchronous pipeline this producer belongs to

-   **\_\_state** -- The current state of the producer in the pipeline

-   **\_\_group** -- The cooperative group this producer operates in

**Examples:**

    pipeline = PipelineAsync.create(...)
    producer, consumer = pipeline.make_participants()
    for i in range(iterations):
        # Try to acquire the current buffer without blocking
        try_acquire_token = producer.try_acquire()

        # Do something else independently
        ...

        # Wait for current buffer to be empty & Move index to next stage
        # If try_acquire_token is True, return immediately
        # If try_acquire_token is False, block until buffer is empty
        handle = producer.acquire_and_advance(try_acquire_token)

        # Produce data
        handle.commit()

### ImmutableResourceHandle

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineProducer.ImmutableResourceHandle`

**Signature:** `ImmutableResourceHandle(_ImmutableResourceHandle__origin: cutlass.pipeline.sm90.PipelineAsync, _ImmutableResourceHandle__immutable_state: cutlass.pipeline.helpers.PipelineState)`

Bases: `ImmutableResourceHandle`

#### barrier

**Kind:** property

**Qualified name:** `cutlass.pipeline.PipelineProducer.ImmutableResourceHandle.barrier`

**Signature:** `barrier`

Get the barrier pointer for the current pipeline stage.

##### Returns

Pointer to the barrier for the current stage

##### Return type

cute.Pointer

#### commit

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineProducer.ImmutableResourceHandle.commit`

**Signature:** `commit(*, loc = None, ip = None)`

Signal that data production is complete for the current stage.

This allows consumers to start processing the data.

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineProducer.ImmutableResourceHandle.__init__`

**Signature:** `__init__(_ImmutableResourceHandle__origin: PipelineAsync, _ImmutableResourceHandle__immutable_state: PipelineState) → None`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineProducer.__init__`

**Signature:** `__init__(pipeline, state, group: CooperativeGroup)`

Initialize a new Producer instance.

#### Parameters

-   **pipeline** ([*PipelineAsync*](#cutlass.pipeline.PipelineAsync)) -- The pipeline this producer belongs to

-   **state** ([*PipelineState*](#cutlass.pipeline.PipelineState)) -- Initial pipeline state

-   **group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- The cooperative group for synchronization

### \_\_pipeline

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineProducer.__pipeline`

**Signature:** `__pipeline: PipelineAsync`

### \_\_state

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineProducer.__state`

**Signature:** `__state: PipelineState`

### \_\_group

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineProducer.__group`

**Signature:** `__group: CooperativeGroup`

### clone

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineProducer.clone`

**Signature:** `clone()`

Create a new Producer instance with the same state.

### reset

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineProducer.reset`

**Signature:** `reset(*, loc = None, ip = None)`

Reset the count of how many handles this producer has committed.

### acquire

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineProducer.acquire`

**Signature:** `acquire(try_acquire_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None) → ImmutableResourceHandle`

Wait for the current buffer to be empty before producing data. This is a blocking operation.

#### Parameters

**try_acquire_token** (*Optional\[Boolean\]*) -- Optional token to try to acquire the buffer

#### Returns

A handle to the producer for committing the data

#### Return type

[ImmutableResourceHandle](#cutlass.pipeline.PipelineProducer.ImmutableResourceHandle)

### advance

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineProducer.advance`

**Signature:** `advance(*, loc = None, ip = None)`

Move to the next pipeline stage.

### acquire_and_advance

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineProducer.acquire_and_advance`

**Signature:** `acquire_and_advance(try_acquire_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None) → ImmutableResourceHandle`

Acquire the current buffer and advance to the next pipeline stage.

This method combines the acquire() and advance() operations into a single call. It first waits for the current buffer to be empty before producing data, then advances the pipeline to the next stage.

#### Parameters

**try_acquire_token** (*Optional\[Boolean\]*) -- Token indicating whether to try non-blocking acquire. If True, returns immediately without waiting. If False or None, blocks until buffer is empty.

#### Returns

A handle to the producer that can be used to commit data to the acquired buffer stage

#### Return type

[ImmutableResourceHandle](#cutlass.pipeline.PipelineProducer.ImmutableResourceHandle)

### try_acquire

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineProducer.try_acquire`

**Signature:** `try_acquire(*, loc = None, ip = None) → cutlass.cutlass_dsl.Boolean`

Attempt to acquire the current buffer without blocking.

This method tries to acquire the current buffer stage for producing data without waiting. It can be used to check buffer availability before committing to a blocking acquire operation.

#### Returns

A boolean token indicating whether the buffer was successfully acquired

#### Return type

Boolean

### commit

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineProducer.commit`

**Signature:** `commit(handle: ImmutableResourceHandle | None = None, *, loc = None, ip = None)`

Signal that data production is complete for the current stage.

This allows consumers to start processing the data.

#### Parameters

**handle** (*Optional\[*[*ImmutableResourceHandle*](#cutlass.pipeline.PipelineProducer.ImmutableResourceHandle)*\]*) -- Optional handle to commit, defaults to None

#### Raises

**AssertionError** -- If provided handle does not belong to this producer

### tail

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineProducer.tail`

**Signature:** `tail(*, loc = None, ip = None)`

Ensure all used buffers are properly synchronized before producer exit.

This should be called before the producer finishes to avoid dangling signals.

## PipelineConsumer

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineConsumer`

**Signature:** `PipelineConsumer(pipeline, state: PipelineState, group: CooperativeGroup)`

Bases: `object`

A class representing a consumer in an asynchronous pipeline.

The Consumer class manages the consumer side of an asynchronous pipeline, handling synchronization and state management for consuming data. It provides methods for waiting, releasing, and advancing through pipeline stages.

### Variables

-   **\_\_pipeline** -- The asynchronous pipeline this consumer belongs to

-   **\_\_state** -- The current state of the consumer in the pipeline

-   **\_\_group** -- The cooperative group this consumer operates in

**Examples:**

    pipeline = PipelineAsync.create(...)
    producer, consumer = pipeline.make_participants()
    for i in range(iterations):
        # Try to wait for buffer to be full
        try_wait_token = consumer.try_wait()

        # Do something else independently
        ...

        # Wait for buffer to be full & Move index to next stage
        # If try_wait_token is True, return immediately
        # If try_wait_token is False, block until buffer is full
        handle = consumer.wait_and_advance(try_wait_token)

        # Consume data
        handle.release(  )  # Signal buffer is empty

        # Alternative way to do this is:
        # handle.release()  # Signal buffer is empty

### ImmutableResourceHandle

**Kind:** class

**Qualified name:** `cutlass.pipeline.PipelineConsumer.ImmutableResourceHandle`

**Signature:** `ImmutableResourceHandle(_ImmutableResourceHandle__origin: cutlass.pipeline.sm90.PipelineAsync, _ImmutableResourceHandle__immutable_state: cutlass.pipeline.helpers.PipelineState)`

Bases: `ImmutableResourceHandle`

#### release

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineConsumer.ImmutableResourceHandle.release`

**Signature:** `release(*, loc = None, ip = None)`

Signal that data production is complete for the current stage. This allows consumers to start processing the data.

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineConsumer.ImmutableResourceHandle.__init__`

**Signature:** `__init__(_ImmutableResourceHandle__origin: PipelineAsync, _ImmutableResourceHandle__immutable_state: PipelineState) → None`

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineConsumer.__init__`

**Signature:** `__init__(pipeline, state: PipelineState, group: CooperativeGroup)`

Initialize a new Consumer instance.

#### Parameters

-   **pipeline** ([*PipelineAsync*](#cutlass.pipeline.PipelineAsync)) -- The pipeline this consumer belongs to

-   **state** ([*PipelineState*](#cutlass.pipeline.PipelineState)) -- Initial pipeline state

-   **group** ([*CooperativeGroup*](#cutlass.pipeline.CooperativeGroup)) -- The cooperative group for synchronization

### \_\_pipeline

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineConsumer.__pipeline`

**Signature:** `__pipeline: PipelineAsync`

### \_\_group

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineConsumer.__group`

**Signature:** `__group: CooperativeGroup`

### \_\_state

**Kind:** attribute

**Qualified name:** `cutlass.pipeline.PipelineConsumer.__state`

**Signature:** `__state: PipelineState`

### clone

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineConsumer.clone`

**Signature:** `clone()`

Create a new Consumer instance with the same state.

### reset

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineConsumer.reset`

**Signature:** `reset(*, loc = None, ip = None)`

Reset the count of how many handles this consumer has consumed.

### wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineConsumer.wait`

**Signature:** `wait(try_wait_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None) → ImmutableResourceHandle`

Wait for data to be ready in the current buffer. This is a blocking operation that will not return until data is available.

#### Parameters

**try_wait_token** (*Optional\[Boolean\]*) -- Token used to attempt a non-blocking wait for the buffer. If provided and True, returns immediately if buffer is not ready.

#### Returns

An immutable handle to the consumer that can be used to release the buffer once data consumption is complete

#### Return type

[ImmutableResourceHandle](#cutlass.pipeline.PipelineConsumer.ImmutableResourceHandle)

### advance

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineConsumer.advance`

**Signature:** `advance(*, loc = None, ip = None)`

Advance the consumer to the next pipeline stage.

This updates the internal state to point to the next buffer in the pipeline. Should be called after consuming data from the current buffer.

### wait_and_advance

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineConsumer.wait_and_advance`

**Signature:** `wait_and_advance(try_wait_token: cutlass.cutlass_dsl.Boolean | None = None, *, loc = None, ip = None) → ImmutableResourceHandle`

Atomically wait for data and advance to next pipeline stage.

This is a convenience method that combines wait() and advance() into a single atomic operation. It will block until data is available in the current buffer, then automatically advance to the next stage.

#### Parameters

**try_wait_token** (*Optional\[Boolean\]*) -- Token used to attempt a non-blocking wait for the buffer. If provided and True, returns immediately if buffer is not ready.

#### Returns

An immutable handle to the consumer that can be used to release the buffer once data consumption is complete

#### Return type

[ImmutableResourceHandle](#cutlass.pipeline.PipelineConsumer.ImmutableResourceHandle)

### try_wait

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineConsumer.try_wait`

**Signature:** `try_wait(*, loc = None, ip = None) → cutlass.cutlass_dsl.Boolean`

Non-blocking check if data is ready in the current buffer.

This method provides a way to test if data is available without blocking. Unlike wait(), this will return immediately regardless of buffer state.

#### Returns

True if data is ready to be consumed, False if the buffer is not yet ready

#### Return type

Boolean

### release

**Kind:** method

**Qualified name:** `cutlass.pipeline.PipelineConsumer.release`

**Signature:** `release(handle: ImmutableResourceHandle | None = None, *, loc = None, ip = None)`

Signal that data consumption is complete for the current stage. This allows producers to start producing new data.

## make_pipeline_state

**Kind:** function

**Qualified name:** `cutlass.pipeline.make_pipeline_state`

**Signature:** `make_pipeline_state(type: PipelineUserType, stages: int, *, loc = None, ip = None)`

Creates a pipeline state. Producers are assumed to start with an empty buffer and have a flipped phase bit of 1.

## pipeline_init_arrive

**Kind:** function

**Qualified name:** `cutlass.pipeline.pipeline_init_arrive`

**Signature:** `pipeline_init_arrive(cluster_shape_mn: cutlass.cute.typing.Layout | None = None, is_relaxed: bool = False, *, loc = None, ip = None)`

Fences the mbarrier_init and sends an arrive if using clusters.

## pipeline_init_wait

**Kind:** function

**Qualified name:** `cutlass.pipeline.pipeline_init_wait`

**Signature:** `pipeline_init_wait(cluster_shape_mn: cutlass.cute.typing.Layout | None = None, *, loc = None, ip = None)`

Syncs the threadblock or cluster

## agent_sync

**Kind:** function

**Qualified name:** `cutlass.pipeline.agent_sync`

**Signature:** `agent_sync(group: Agent, is_relaxed: bool = False, *, loc = None, ip = None)`

Syncs all threads within an agent.

## arrive

**Kind:** function

**Qualified name:** `cutlass.pipeline.arrive`

**Signature:** `arrive(barrier_id: int, num_threads: int, *, loc = None, ip = None)`

The aligned flavor of arrive is used when all threads in the CTA will execute the same instruction. See PTX documentation.

## arrive_unaligned

**Kind:** function

**Qualified name:** `cutlass.pipeline.arrive_unaligned`

**Signature:** `arrive_unaligned(barrier_id: int, num_threads: int, *, loc = None, ip = None)`

The unaligned flavor of arrive can be used with an arbitrary number of threads in the CTA.

## wait

**Kind:** function

**Qualified name:** `cutlass.pipeline.wait`

**Signature:** `wait(*, loc = None, ip = None)`

NamedBarriers do not have a standalone wait like mbarriers, only an arrive_and_wait. If synchronizing two warps in a producer/consumer pairing, the arrive count would be 32 using mbarriers but 64 using NamedBarriers. Only threads from either the producer or consumer are counted for mbarriers, while all threads participating in the sync are counted for NamedBarriers.

## wait_unaligned

**Kind:** function

**Qualified name:** `cutlass.pipeline.wait_unaligned`

**Signature:** `wait_unaligned(barrier_id: int, num_threads: int, *, loc = None, ip = None)`

## arrive_and_wait

**Kind:** function

**Qualified name:** `cutlass.pipeline.arrive_and_wait`

**Signature:** `arrive_and_wait(barrier_id: int, num_threads: int, *, loc = None, ip = None)`

## sync

**Kind:** function

**Qualified name:** `cutlass.pipeline.sync`

**Signature:** `sync(barrier_id: int = 0, *, loc = None, ip = None)`
