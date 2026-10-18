---
title: "task_scheduling"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/ts_api/ts.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# task_scheduling {#module-cutlass.experimental.task_scheduling}

Task Scheduling (TS) public API.

The top-level package re-exports the captured-schedule helpers and work decorators most kernels need directly:

`schedule`

:   Decorator that traces a schedule function into a `Schedule`.

`domain_loop` and `work_tile_loop`

:   Context managers for the runtime domain loop and optional persistent work-tile loop.

`consumer_work` and `producer_work`

:   Decorators for resource methods that read from or write into a resource.

`WorkAttr`

:   Work callback attributes such as `WorkAttr.AUXILIARY`.

<!-- -->

cutlass.experimental.task_scheduling.consumer_work(*method: \~collections.abc.Callable\[\[\...\], \~typing.Any\] \| None = None, \*, work_attrs: \~cutlass.experimental.task_scheduling.enums.WorkAttr = \<WorkAttr.NONE: 0\>, returns: str \| \~dataclasses.Field \| tuple\[str \| \~dataclasses.Field, \...\] \| list\[str \| \~dataclasses.Field\] \| None = None*) → Callable\[\[\...\], Any\]

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

cutlass.experimental.task_scheduling.domain_loop(

*\*bounds: object \| Callable\[\[\...\], object\]*,

*unroll: int \| None = None*,

) → Iterator\[DomainLoopProxy\]

Open a domain (for-) loop over the work tile's iteration space.

Bounds mirror Python's `range`: `domain_loop(end)`, `domain_loop(start, end)`, or `domain_loop(start, end, step)` --- an omitted `start` defaults to `0` and an omitted `step` to `1`. **Any** bound may be a callable, which makes that dimension dynamic: pass a `Task` subclass method accessed on the class (`MyTask.get_domain`, not on an instance) so its `self` stays an explicit parameter; the Task runtime calls it as `(self, work_tile_coord)` to get that bound for the current work tile.

Parameters:

:   - **\*bounds** (*int,* *DSL value, or* *callable*) -- 1 to 3 range-style bounds (see above).

    - **unroll** (*int* *or* *None*) -- Loop unroll hint. `None` lets the compiler decide.

<!-- -->

*class* cutlass.experimental.task_scheduling.Every(*period: int*, *start: int = 0*)

:   Bases: [`IterationPredicate`](enums.md#cutlass.experimental.task_scheduling.enums.IterationPredicate)

    Periodic guard using zero-based iteration counts.

    Fires on counts `start, start + period, start + 2 * period, ...`.

    period*: int*

    :

    start*: int = 0*

    :

    fires(*iter_idx: int*, *num_iters: int*) → bool

    :   Return whether the guarded block runs on iteration `iter_idx`.

    \_\_init\_\_(*period: int*, *start: int = 0*) → None

    :

<!-- -->

*class* cutlass.experimental.task_scheduling.IterationPredicate

:   Bases: [`BlockGuard`](enums.md#cutlass.experimental.task_scheduling.enums.BlockGuard)

    Loop guard decidable at trace time from the zero-based iteration count.

    fires(*iter_idx: int*, *num_iters: int*) → bool

    :   Return whether the guarded block runs on iteration `iter_idx`.

*class* cutlass.experimental.task_scheduling.OpaqueCondition(

*key: Hashable*,

*negated: bool = False*,

*resource: object \| None = None*,

*method_label: str \| None = None*,

*result_name: str \| None = None*,

)

Bases: [`BlockGuard`](enums.md#cutlass.experimental.task_scheduling.enums.BlockGuard)

Loop guard backed by a runtime value enumerated under `key`.

`negated` is `True` for [`when_false()`](#cutlass.experimental.task_scheduling.when_false) blocks. Optional `resource` / `method_label` identify the work-call output that produced the stored guard value; when omitted the verifier still honors `key` correlation. `result_name` selects that stored output. Runtime execution reads the stored value instead of re-invoking the producing method; integer-like values are false only when zero.

key*: Hashable*

:

<!-- -->

negated*: bool = False*

:

<!-- -->

resource*: object \| None = None*

:

<!-- -->

method_label*: str \| None = None*

:

<!-- -->

result_name*: str \| None = None*

:

\_\_init\_\_(

*key: Hashable*,

*negated: bool = False*,

*resource: object \| None = None*,

*method_label: str \| None = None*,

*result_name: str \| None = None*,

) → None

cutlass.experimental.task_scheduling.producer_work(*method: \~collections.abc.Callable\[\[\...\], \~typing.Any\] \| None = None, \*, work_attrs: \~cutlass.experimental.task_scheduling.enums.WorkAttr = \<WorkAttr.NONE: 0\>*) → Callable\[\[\...\], Any\]

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

cutlass.experimental.task_scheduling.schedule(

*fn: Callable\[\[\...\], None\]*,

) → Callable\[\[\...\], Schedule\]

Decorator that traces a schedule function into a `Schedule`.

Parameters:

:   **fn** (*Callable*) -- Function whose arguments are `MemoryResource` instances and whose body records resource calls through schedule-builder context managers.

Returns:

:   Wrapper that accepts concrete resources and returns the captured `Schedule`.

Return type:

:   Callable\[..., Schedule\]

Notes

The decorated function receives `ResourceProxy` wrappers for each `MemoryResource`. Method calls on the proxies record schedule entries and routing edges. `with work_tile_loop(wq):` and `with domain_loop(start, end, step):` mark the structural boundaries. Plain Python control flow inside the function executes at trace time.

cutlass.experimental.task_scheduling.when_false(

*cond: object*,

*\**,

*key: Hashable \| None = None*,

) → Iterator\[None\]

Open a block that runs when `cond` is false.

`when_false` accepts opaque runtime conditions only; iteration predicates cannot be negated because their complement may not be contiguous in the lowered loop schedule.

cutlass.experimental.task_scheduling.when_true(

*cond: object*,

*\**,

*key: Hashable \| None = None*,

) → Iterator\[None\]

Open a block that runs when `cond` is true.

Iteration predicates are only valid inside `domain_loop()`. Opaque runtime conditions are work-call outputs backed by `TaskLocalVariable` slots; they may be used as sibling blocks or inside `skippable()`, but condition blocks are intentionally not nestable.

*class* cutlass.experimental.task_scheduling.WorkAttr(*value*)

Bases: `IntFlag`

Verification-visible attributes attached to TS work callbacks.

Work attributes describe semantic properties that the schedule verifier must account for. `AUXILIARY` marks callbacks that do not model memory access and should not participate in normal resource ordering checks. The type is an `IntFlag` so future verifier attributes can be composed without changing the decorator API.

NONE *= 0*

:

<!-- -->

AUXILIARY *= 1*

:

*classmethod* is_valid_combination(

*value: [WorkAttr](enums.md#cutlass.experimental.task_scheduling.enums.WorkAttr)*,

) → bool

Return true for work-attribute sets supported by verification.

*classmethod* validate(

*value: [WorkAttr](enums.md#cutlass.experimental.task_scheduling.enums.WorkAttr)*,

*field_name: str*,

) → None

Raise if `value` is not a supported work-attribute flag set.

is_auxiliary() → bool

:   Return true when auxiliary-work semantics are requested.

cutlass.experimental.task_scheduling.work_tile_loop(

*wq: object*,

*\**,

*skip_if: Callable\[\[\...\], [Boolean](../basic_data_types.md#cutlass.Boolean)\] \| None = None*,

) → Iterator\[WorkTileLoopProxy\]

Open the persistent work-tile (while) loop over `wq`.

`skip_if` is an optional predicate `(work_queue, work_tile) -> Boolean`; when set, `skippable()` regions in the body are omitted for skipped tiles while the surrounding WorkQueue bookkeeping still runs. It may be a plain `(work_queue, work_tile)` callable or a method of `wq`.
