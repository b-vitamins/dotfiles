---
name: cute-dsl-refactor-review
description: Review and refactor existing CuTe DSL code for correctness, clarity, API fitness, and maintainable structure without gratuitous regressions. Use when auditing a CuTe DSL patch, modernizing examples, simplifying layouts or control flow, or preparing CuTe code for upstreaming.
---

# CuTe DSL Refactor Review

Review CuTe DSL code like a performance-sensitive compiler-facing codebase, not like generic Python.

## Required Rules

- Run every command through `scripts/guix-run`. No intermediate shell after `guix shell --`.
- If the user asks for review, findings come first with file references and concrete risks.
- Preserve behavior unless the user explicitly wants semantic change.
- Use `/tmp` for one-off reduction scripts or inspection helpers.

## Review Workflow

1. Establish invariants before changing code:
   - tensor semantics
   - layout assumptions
   - copy and predication behavior
   - launch geometry
   - caching and compilation boundaries
2. Check the local docs for API drift and deprecations.
3. Flag high-risk issues first:
   - wrong layout or partition assumptions
   - stale or unsafe JIT caching
   - hidden dynamic-layout costs
   - incorrect predication or boundary handling
   - readability changes that obscure performance-critical intent
4. Refactor in small, behavior-preserving steps.

## Primary References

- Stability and drift: `references/docs/deprecation.md`, `references/docs/api/changelog.md`
- Behavioral constraints: `references/docs/functionality.md`, `references/docs/limitations.md`, `references/docs/faqs.md`
- API: `references/docs/api/cute.md`, `references/docs/api/cute_runtime.md`, `references/docs/api/utils.md`
- Examples: `references/examples/elementwise_add.py`, `references/examples/elementwise_apply.py`, `references/examples/call_from_jit.py`, `references/examples/sgemm.py`

## Output Standard

- Findings first.
- Then unresolved assumptions.
- Then a concise refactor summary if needed.
