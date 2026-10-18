---
title: "task_scheduling.task"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/ts_api/task.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# task_scheduling.task {#module-cutlass.experimental.task_scheduling.task}

Task for the Task Scheduling (TS) framework.

**Compile-time abstraction**: `Task.run()` is traced by the DSL into a monolithic loop. The captured schedule list, loop guards, and domain computation are all resolved at trace time. A task whose resolved domain has zero loop iterations emits no loop-body work. The generated PTX is identical to a hand-coded bare-metal kernel with the same schedule.

This module provides the Task class and schedule normalisation utilities that sit on top of the resource abstractions defined in `resources.py`.

*Tasks* are assigned to a contiguous range of warps and bind resource roles to a captured `ScheduleResult`. A task reads from its `src_resources` (consumer side) and writes to its `dst_resources` (producer side). The captured schedule records ordered pipeline operations such as acquire, wait, work, commit, and release. `TaskManager` executes every task: each warp enters `task.run()`, but only the warps in `[warp_idx, warp_idx + num_warps)` execute that task's schedule body.

*class* cutlass.experimental.task_scheduling.task.Task(

*src_resources: List\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\]*,

*dst_resources: List\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\]*,

*warp_idx: int*,

*num_warps: int*,

*\**,

*schedule: ScheduleResult*,

*num_registers: int \| None = None*,

*name: str = \'\'*,

*debug_print: bool = False*,

*iket_enable_profiling: bool = False*,

*iket_profiling_stages: FrozenSet\[[ScheduleStage](enums.md#cutlass.experimental.task_scheduling.enums.ScheduleStage)\] \| None = None*,

*iket_profiling_resources: FrozenSet\[str\] \| None = None*,

*run_only_on_cta_id: int \| None = None*,

)

Bases: `TraversableLeafMixin`

A unit of warp-specialised work in the TS framework.

A Task maps a contiguous range of warps to a pair of resource lists (`src_resources` consumed by the task and `dst_resources` produced by the task). Its schedule is supplied only as a `ScheduleResult` captured by the `@schedule` decorator. The captured schedule defines the pipeline operations (e.g. wait, acquire, work, commit, release) executed each iteration of the main loop over the computational domain.

The captured schedule list is normalized into three phases (head / loop / tail) at construction time.

name

:   Human-readable label used in debug prints and PTX comments.

    Type:

    :   str

<!-- -->

src_resources

:   Resources from which this task reads (consumer side).

    Type:

    :   List\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\]

<!-- -->

dst_resources

:   Resources to which this task writes (producer side).

    Type:

    :   List\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\]

<!-- -->

warp_idx

:   Index of the first warp assigned to this task.

    Type:

    :   [cutlass.Int32](../basic_data_types.md#cutlass.Int32)

<!-- -->

num_warps

:   Number of consecutive warps assigned to this task.

    Type:

    :   int

<!-- -->

num_registers

:   Per-task register budget (must be 8-256, divisible by 8). `setmaxregister_increase/decrease` is emitted accordingly.

    Type:

    :   int \| None

<!-- -->

schedule

:   Captured output of the `@schedule` decorator. It supplies the schedule list, loop bounds, loop step, unroll hint, routing table, and any persistent-work-tile metadata.

    Type:

    :   ScheduleResult

<!-- -->

domain_getter

:   Explicit `Task` subclass method captured from the callable `end` bound, e.g. `domain_loop(MyTask.get_domain)`. When present, `Task` validates that this instance inherits that exact `get_domain` override.

    Type:

    :   callable \| None

<!-- -->

is_persistent

:   `True` when `src_resources` contains a `WorkQueue`.

    Type:

    :   bool

<!-- -->

run_only_on_cta_id

:   Hoist CTA selection for the whole task body to one explicit CTA rank. When set, the CTA-rank check is hoisted into `is_selected()` so the entire task body, including all pipeline operations, runs only on that CTA. Requires that every pipelined resource touched by this task sets the matching signaling side to `SignalingThreads.CtaLeader`; a `ValueError` is raised at trace time otherwise.

    Type:

    :   int \| None

<!-- -->

debug_print

:   Emit per-step debug prints to stdout (default: False).

    Type:

    :   bool

<!-- -->

iket_enable_profiling

:   Enable per-step IKET `range_start / range_end` markers around each selected `(resource, ScheduleStage)` entry. Each unique pair consumes 1 of 31 IKET marker slots; use `iket_profiling_stages` and `iket_profiling_resources` to stay within the limit (default: False).

    Type:

    :   bool

<!-- -->

iket_profiling_stages

:   Restrict per-step IKET markers to the given `ScheduleStage` values. `None` means all stages are profiled (default: None).

    Type:

    :   Optional\[FrozenSet\[[ScheduleStage](enums.md#cutlass.experimental.task_scheduling.enums.ScheduleStage)\]\]

<!-- -->

iket_profiling_resources

:   Restrict per-step IKET markers to resources whose `name` is in this set. `None` means all resources are profiled (default: None).

    Type:

    :   Optional\[FrozenSet\[str\]\]

<!-- -->

head_schedule_list, loop_schedule_list, tail_schedule_list

:   Normalised schedule phases, each a list of `(MemoryResource, ScheduleStage, call_id)` tuples.

\_\_init\_\_(

*src_resources: List\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\]*,

*dst_resources: List\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\]*,

*warp_idx: int*,

*num_warps: int*,

*\**,

*schedule: ScheduleResult*,

*num_registers: int \| None = None*,

*name: str = \'\'*,

*debug_print: bool = False*,

*iket_enable_profiling: bool = False*,

*iket_profiling_stages: FrozenSet\[[ScheduleStage](enums.md#cutlass.experimental.task_scheduling.enums.ScheduleStage)\] \| None = None*,

*iket_profiling_resources: FrozenSet\[str\] \| None = None*,

*run_only_on_cta_id: int \| None = None*,

) → None

Create a task from a captured schedule.

`schedule` is the sole source of schedule metadata. It supplies the schedule list, slot routing, loop bounds, loop step, unroll hint, skip predicate, and persistent-work-tile placement metadata captured by the `@schedule` decorator.

Parameters:

:   - **src_resources** (*List\[*[*MemoryResource*](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*\]*) -- Resources read by this task on the consumer side.

    - **dst_resources** (*List\[*[*MemoryResource*](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*\]*) -- Resources written by this task on the producer side.

    - **warp_idx** (*int*) -- Index of the first warp assigned to this task.

    - **num_warps** (*int*) -- Number of consecutive warps assigned to this task.

    - **schedule** (*object*) -- Captured result returned by a `@schedule` function. Must be provided; legacy constructor schedule metadata is not accepted by this API.

    - **num_registers** (*int,* *optional*) -- Per-task register budget. When set, it must be in `[8, 256]` and divisible by 8.

    - **name** (*str,* *optional*) -- Human-readable task name used in diagnostics and PTX comments.

    - **debug_print** (*bool,* *optional*) -- Emit per-step debug prints from generated code.

    - **iket_enable_profiling** (*bool,* *optional*) -- Emit per-step IKET `range_start` / `range_end` markers for selected head, loop, and tail schedule entries.

    - **iket_profiling_stages** (*Optional\[FrozenSet\[*[*ScheduleStage*](enums.md#cutlass.experimental.task_scheduling.enums.ScheduleStage)*\]\],* *optional*) -- Restrict per-step IKET markers to the selected schedule stages. `None` profiles all stages.

    - **iket_profiling_resources** (*Optional\[FrozenSet\[str\]\],* *optional*) -- Restrict per-step IKET markers to resources with matching names. `None` profiles all resources.

    - **run_only_on_cta_id** (*int,* *optional*) -- Hoist task selection so all task work runs only on the given CTA rank. Requires matching CTA-leader signaling on every pipelined resource touched by the task.

<!-- -->

*property* resources*: chain*

:   Iterate over all resources (src then dst) without duplicates.

<!-- -->

get_domain(*tile_coord: object*) → Any

:   Return the iteration domain for the given tile coordinate.

    Override in subclasses for tile-dependent domain logic. The default implementation returns `self.domain`.

    Parameters:

    :   **tile_coord** (*object*) -- `work_tile.tile_idx` -- a 3-tuple `(bx, by, bz)` of the current tile's block coordinates.

    Returns:

    :   The number of loop iterations for this tile.

    Return type:

    :   int or [cutlass.Int32](../basic_data_types.md#cutlass.Int32)

<!-- -->

is_selected() → bool

:   Return `True` if the current warp falls within this task's range.

init_variables(

*context: [ResourceContext](memory.md#cutlass.experimental.task_scheduling.memory.ResourceContext) \| None = None*,

) → None

Create resource variables outside dynamic control flow.

Must be called before any `scf.if` dispatch to keep the DSL IR structure stable. `TaskManager.run()` calls this for every task before entering the dispatch chain.

Parameters:

:   **context** ([*ResourceContext*](memory.md#cutlass.experimental.task_scheduling.memory.ResourceContext) *or* *None*) -- Carries `smem_base` and/or `tmem_ptr_i32` when allocators are in use.

<!-- -->

make_task_cache() → object

:   Return an optional task-defined payload captured into `StageInfo`.

    Override in task subclasses when hot resource paths need a few cached task-local values without widening `StageInfo` field-by-field.

<!-- -->

run_body() → None

:   Gate on warp selection and execute the task body.

    Call only after `init_variables()` has already been invoked for *all* tasks.

<!-- -->

run() → None

:   Top-level entry point executed by `TaskManager.run()`.

    1.  Initialises function-level and role-level variables on *every* resource (outside dynamic control flow to keep the DSL IR structure stable).

    2.  Gates on `is_selected()` so only the assigned warps proceed.

    3.  Sets the register budget via `_set_max_register`.

    4.  Dispatches to persistent or non-persistent body.
