---
title: "task_scheduling.task_manager"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/ts_api/task_manager.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# task_scheduling.task_manager {#module-cutlass.experimental.task_scheduling.task_manager}

TaskManager for the Task Scheduling (TS) framework.

**Compile-time abstraction**: The entire TS framework (TaskManager, Task, MemoryResource, pipeline acquire/release/commit) is traced away during DSL compilation. The resulting PTX is a single monolithic loop --- identical to what a hand-coded bare-metal kernel would produce. There is NO runtime task dispatch and NO framework overhead in the generated GPU code.

`TaskManager` owns the full list of tasks and the resource dependency graph. Construction prints the schedule table and runs validation checks. It then provides two entry points called from the kernel:

- `setup_resources_and_tasks()` - materialises and initializes pipelines and barriers for every resource (calls `resource.create()`).

- `run()` - executes all tasks (calls `task.run()` for each).

*class* cutlass.experimental.task_scheduling.task_manager.DependencyGraphEdge(

*upstream: [MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*,

*downstream: [MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*,

*backing_tasks: Tuple\[[Task](task.md#cutlass.experimental.task_scheduling.task.Task), \...\]*,

*edge_kind: str*,

)

Bases: `object`

Renderable metadata for one resource dependency graph edge.

upstream*: [MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*

:

<!-- -->

downstream*: [MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*

:

<!-- -->

backing_tasks*: Tuple\[[Task](task.md#cutlass.experimental.task_scheduling.task.Task), \...\]*

:

<!-- -->

edge_kind*: str*

:

\_\_init\_\_(

*upstream: [MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*,

*downstream: [MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*,

*backing_tasks: Tuple\[[Task](task.md#cutlass.experimental.task_scheduling.task.Task), \...\]*,

*edge_kind: str*,

) → None

*class* cutlass.experimental.task_scheduling.task_manager.DependencyGraphControlEdge(

*source: [MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*,

*target_node_id: str*,

*target_label: str*,

*edge_label: str*,

)

Bases: `object`

Renderable metadata for a collapsed non-dataflow control edge.

source*: [MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*

:

<!-- -->

target_node_id*: str*

:

<!-- -->

target_label*: str*

:

<!-- -->

edge_label*: str*

:

\_\_init\_\_(

*source: [MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*,

*target_node_id: str*,

*target_label: str*,

*edge_label: str*,

) → None

*class* cutlass.experimental.task_scheduling.task_manager.TaskManager(

*tasks: List\[[Task](task.md#cutlass.experimental.task_scheduling.task.Task)\]*,

*resource_dependency_graph: Dict\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource), List\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\]\]*,

*dma_consumer_release_labels: Dict\[Tuple\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource), [MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\], Set\[str\]\] \| None = None*,

*iket_enable_profiling: bool = False*,

*iket_profiling_stages: FrozenSet\[[ScheduleStage](enums.md#cutlass.experimental.task_scheduling.enums.ScheduleStage)\] \| None = None*,

*iket_profiling_resources: FrozenSet\[str\] \| None = None*,

*skip_validation: bool = False*,

*smem_allocator: [SmemAllocator](memory.md#cutlass.experimental.task_scheduling.memory.SmemAllocator) \| None = None*,

*tmem_allocator: [TmemAllocator](memory.md#cutlass.experimental.task_scheduling.memory.TmemAllocator) \| None = None*,

*tmem_ptr_i32: Any \| None = None*,

*verbose: bool = True*,

*smem_capacity_bytes: int \| None = None*,

*tmem_capacity_columns: int \| None = None*,

*exhaustive_deadlock_race_check: bool = True*,

*assume_pdl_wait_completed: bool = False*,

)

Bases: `object`

Orchestrates the execution of all tasks and their shared resources.

tasks

:   All tasks in execution order.

    Type:

    :   List\[[Task](task.md#cutlass.experimental.task_scheduling.task.Task)\]

<!-- -->

resources

:   De-duplicated union of every task's `src_resources` and `dst_resources`, preserving first-seen order.

    Type:

    :   List\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\]

Notes

Kernel entry points:

- `setup_resources_and_tasks()` - materialises and initializes pipelines and barriers by calling `resource.create()` on each resource. Must be called once before `run()`.

- `run()` - executes every task by calling `task.run()`. Validation has already run during construction via `print_and_verify()`.

\_\_init\_\_(

*tasks: List\[[Task](task.md#cutlass.experimental.task_scheduling.task.Task)\]*,

*resource_dependency_graph: Dict\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource), List\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\]\]*,

*dma_consumer_release_labels: Dict\[Tuple\[[MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource), [MemoryResource](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)\], Set\[str\]\] \| None = None*,

*iket_enable_profiling: bool = False*,

*iket_profiling_stages: FrozenSet\[[ScheduleStage](enums.md#cutlass.experimental.task_scheduling.enums.ScheduleStage)\] \| None = None*,

*iket_profiling_resources: FrozenSet\[str\] \| None = None*,

*skip_validation: bool = False*,

*smem_allocator: [SmemAllocator](memory.md#cutlass.experimental.task_scheduling.memory.SmemAllocator) \| None = None*,

*tmem_allocator: [TmemAllocator](memory.md#cutlass.experimental.task_scheduling.memory.TmemAllocator) \| None = None*,

*tmem_ptr_i32: Any \| None = None*,

*verbose: bool = True*,

*smem_capacity_bytes: int \| None = None*,

*tmem_capacity_columns: int \| None = None*,

*exhaustive_deadlock_race_check: bool = True*,

*assume_pdl_wait_completed: bool = False*,

) → None

Parameters:

:   - **tasks** (*List\[*[*Task*](task.md#cutlass.experimental.task_scheduling.task.Task)*\]*) -- All tasks in execution order.

    - **resource_dependency_graph** (*Dict\[*[*MemoryResource*](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*,* *List\[*[*MemoryResource*](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*\]\]*) --

      Explicit dataflow dependency graph. Each key is a *downstream* resource whose producer work depends on the consumer work of the upstream resources listed in the value.

      For example, in a pipeline `GmemAb --> SmemAb --> TmemC --> GmemD`:

          resource_dependency_graph = {
              smem_ab: [gmem_ab],
              tmem_c:  [smem_ab],
              gmem_d:  [tmem_c],
          }

      The manager verifies that every declared edge `(upstream --> downstream)` is backed by a task whose `src_resources` contain the upstream resource and whose `dst_resources` contain the downstream resource.

    - **dma_consumer_release_labels** (*Dict\[Tuple\[*[*MemoryResource*](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*,* [*MemoryResource*](resources.md#cutlass.experimental.task_scheduling.resources.MemoryResource)*\],* *Set\[str\]\],* *optional*) -- Edge-specific named consumer-release labels for DMA ordering validation. Use this when one upstream resource feeds multiple downstream DMA producers through different named consumer work functions. Keys are `(upstream, downstream)` resource pairs.

    - **iket_enable_profiling** (*bool,* *optional*) -- Enable per-step IKET `range_start/end` instrumentation on the selected head, loop, and tail schedule entries. Use `iket_profiling_stages` and `iket_profiling_resources` to keep the marker count within the hardware limit. Default False.

    - **iket_profiling_stages** (*frozenset* *of* [*ScheduleStage*](enums.md#cutlass.experimental.task_scheduling.enums.ScheduleStage) *or* *None,* *optional*) -- Whitelist of `ScheduleStage` values for per-step IKET instrumentation. When `None` (default), every stage that appears in the schedule list is instrumented. Pass e.g. `frozenset({ScheduleStage.ProducerWork, ScheduleStage.ConsumerWork})` to restrict tracing to actual data-movement / compute work and stay well within the 31-event hardware limit.

    - **iket_profiling_resources** (*frozenset* *of* *str* *or* *None,* *optional*) -- Whitelist of resource *names* for per-step IKET instrumentation. When `None` (default), entries for all resources are instrumented. Pass a set of names to restrict tracing to specific resources.

    - **skip_validation** (*bool,* *optional*) -- When `True`, verification checks still run but failures are emitted as warnings instead of raising exceptions. Use this when domains are runtime values (not Python `int`), since the deadlock simulation falls back to `domain=1` and may report false-positive deadlocks. For accurate verification, prefer validate-only mode with realistic `int` domains. Default False.

    - **smem_allocator** ([*cutlass.experimental.task_scheduling.memory.SmemAllocator*](memory.md#cutlass.experimental.task_scheduling.memory.SmemAllocator) *or* *None,* *optional*) -- Unified SMEM allocator with pre-computed layout. When set, `setup_resources_and_tasks()` calls `smem_allocator.allocate()` and the resulting `smem_base` is threaded through `ResourceContext` / `StageInfo` to all resources.

    - **tmem_allocator** ([*cutlass.experimental.task_scheduling.memory.TmemAllocator*](memory.md#cutlass.experimental.task_scheduling.memory.TmemAllocator) *or* *None,* *optional*) -- TMEM column allocator with pre-computed layout. When set, a usage report is printed during `print_and_verify()`.

    - **tmem_ptr_i32** (*array* *or* *None,* *optional*) -- Shared-memory `Int32` scalar written by `nvvm.tcgen05_alloc`. When set, it is included in the `ResourceContext` so resources can derive TMEM addresses.

    - **verbose** (*bool,* *optional*) -- When `False`, suppresses informational output (schedule tables, register budgets, aliasing notes). Errors and warnings are always emitted. Default `True`.

    - **smem_capacity_bytes** (*int* *or* *None,* *optional*) -- Maximum SMEM bytes per CTA (data + barriers). When `None`, defaults to `(228 − 1) × 1024 = 232448 B` (SM100/SM90). Override for other architectures.

    - **tmem_capacity_columns** (*int* *or* *None,* *optional*) -- Maximum TMEM columns per SM. When `None`, defaults to `512` (SM100). Override for other architectures.

    - **exhaustive_deadlock_race_check** (*bool,* *optional*) -- When `True`, run the exhaustive BFS interleaving checker (`check_all_interleavings`) that explores all valid schedule interleavings to detect deadlocks, aliasing race conditions, and PDL launch-before-wait ordering violations. Significantly more expensive than the structural checks. Pass `False` to opt out for performance-sensitive paths (e.g. FMHA). Default `True`.

    - **assume_pdl_wait_completed** (*bool,* *optional*) -- Treat PDL wait as already executed before the TS schedule. This is for kernels that emit `griddepcontrol.wait` before a PDL-dependent access that happens outside TS.

get_marmaid_string_dependency_graph(

*direction: str = \'TD\'*,

*include_resource_details: bool = True*,

*collapse_work_queue_edges: bool = True*,

) → str

Return a Mermaid flowchart string for the resource dependency graph.

Edge labels identify the task(s) that realize each dependency and the verifier-level edge kind: `data`, `ordering`, `ping-pong`, or `work-queue`.

Parameters:

:   - **direction** -- Mermaid flowchart direction (for example `"TD"` for top-down or `"LR"` for left-to-right). Must be non-empty.

    - **include_resource_details** -- When `True`, resource node labels include pipeline type, stage count, and barrier metadata. When `False`, only resource names are shown.

    - **collapse_work_queue_edges** -- When `True` (default), WorkQueue control-token dependencies are collapsed into one edge to all persistent tasks so they do not obscure the dataflow. When `False`, each work-queue dependency is drawn as its own edge.

Returns:

:   A Mermaid flowchart source string for graph/chart rendering. The text has no external dependencies and can be pasted into Markdown or rendered by any Mermaid-compatible tool.

<!-- -->

*property* smem_allocator*: [SmemAllocator](memory.md#cutlass.experimental.task_scheduling.memory.SmemAllocator) \| None*

:   The SMEM allocator (if configured).

<!-- -->

print_and_verify() → None

:   Print the full schedule table and run all verification checks.

    When `skip_validation=True` was passed to the constructor, each check still runs but failures are emitted as warnings instead of raising.

    **False-positive caveat:** when task `domain` or `domain_start` is a runtime value (not a Python `int`), the deadlock simulation falls back to `domain=1` and `domain_start=0`. This can produce false-positive deadlock reports for schedules that require more iterations to balance (e.g. `domain_start > 0`). For accurate verification, use validate-only mode with realistic Python `int` domains and `domain_start`.

<!-- -->

setup_resources_and_tasks() → None

:   Materialise and initialize pipelines and barriers for every resource.

    When an `SmemAllocator` is configured, emits a single `cutlass.Array(..., space=cutlass.AddressSpace.smem)` for all declared data SMEM and stores the base pointer for later use by `run()`.

    Then calls `resource.create()` on each unique resource (allocates SMEM mbarriers, instantiates the pipeline object, initializes barriers). Must be called exactly once before `run()`.

    This is a plain Python method (not `@cute.jit`) so that `_is_setup` is assigned at Python level and never auto-promoted to a staged Boolean by the DSL tracer.

<!-- -->

run() → None

:   Execute all tasks.

    Each `task.run()` call gates on the current warp index, so every warp enters this method but only the warps assigned to a given task execute its schedule body.

    When an `SmemAllocator` or `tmem_ptr_i32` is configured, builds a `ResourceContext` and passes it to every task's `init_variables`.

    This is a plain Python method (not `@cute.jit`) so that the `_is_setup` assertion runs at Python level against a plain bool, avoiding staged-Boolean and early-exit issues.

    Raises:

    :   **AssertionError** -- If `setup_resources_and_tasks()` has not been called.
