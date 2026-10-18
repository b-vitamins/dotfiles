---
name: cute-dsl-debugger
description: Debug broken CuTe DSL programs, including JIT failures, DLPack interop issues, dynamic-layout mistakes, launch problems, shared-memory sizing, and incorrect tensor partitioning or predicates. Use when CuTe code errors, miscompiles, crashes, deadlocks, or returns wrong results.
---

# CuTe DSL Debugger

Debug CuTe failures by reducing them to the smallest reproducible phase and isolating the violated assumption.

Paths: `<skill-dir>` is the directory containing this file, and `<scratch>` is a scratch
directory outside the project tree (the system temporary directory unless the user names another).

## Required Rules

- Use the project's existing CuTe environment for everything you run, including this skill's
  scripts. Check it first with `python3 <skill-dir>/scripts/check_env.py`;
  `references/environment.md` covers requirements and setup options. If the check fails, report
  what is missing and ask before installing anything.
- Reproduce with the smallest possible script, usually in `<scratch>`, before making broad edits.
- Classify the failure before fixing it:
  - import/environment
  - JIT compile
  - launch configuration
  - runtime memory behavior
  - silent wrong answer

## Debug Workflow

1. Reproduce the failure in a tiny script in `<scratch>` if the original program is large.
2. Inspect the likely fault class:
   - DLPack conversion and alignment
   - static vs dynamic layout marking
   - `grid`, `block`, `cluster`, `smem`
   - partition shape mismatch
   - missing predication at boundaries
   - JIT cache reuse against the wrong assumptions
3. Read the matching doc section and compare with the closest example.
4. Fix the root cause, not just the observed symptom.
5. Leave behind a minimal validation path when practical.

## Primary References

- Debug and limits: `references/docs/guide/debugging.md`, `references/docs/limitations.md`, `references/docs/faqs.md`
- JIT and runtime: `references/docs/guide/dsl_jit_arg_generation.md`, `references/docs/guide/dsl_jit_caching.md`, `references/docs/guide/dsl_jit_compilation_options.md`, `references/docs/api/cute_runtime.md`
- Examples: `references/examples/call_bypass_dlpack.py`, `references/examples/call_from_jit.py`, `references/examples/dynamic_smem_size.py`, `references/examples/smem_allocator.py`

## Failure Patterns To Check Early

- wrong `assumed_align` on `from_dlpack`
- unnecessary or missing `.mark_layout_dynamic()`
- mismatched tile shape vs problem shape
- invalid shared memory sizing
- predication masks computed from the wrong coordinate tensor
- scratch driver import failures because the helper module directory is not on `sys.path`
- validation bugs where the reference comparison runs before the kernel under test
