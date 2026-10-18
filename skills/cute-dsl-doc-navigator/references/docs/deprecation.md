---
title: "Deprecation Policy"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/deprecation.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# Deprecation Policy {#deprecation-policy}

## Purpose

The goal of this policy is to evolve the DSL and its APIs while keeping user programs stable. Features or APIs are deprecated only when they are redundant, unsafe, or block better designs.

## Deprecation Process

**Step 1 --- Soft Deprecation**

When a feature is considered for removal, it is first annotated with the `@deprecated` decorator or `DeprecationWarning` and documented with a suggested alternative. At this stage, the feature continues to work normally.

Users are encouraged to provide feedback and describe their use cases. If there is strong justification, we may keep or redesign the feature.

**Step 2 --- Removal (the subsequent release)**

If no valid use cases remain, the deprecated feature will be removed in the following **minor** release.

> **Note.**
>
> The release version follows the format `<major>.<minor>.<patch>`.

## Communication

All deprecations are announced through:

- This page

- In-code warning messages

## Soft Deprecations

**Version 4.2.1**

- `cute.arch.warpgroup_reg_alloc` and `cute.arch.warpgroup_reg_dealloc` → Scheduled for deprecation. Use `cute.arch.setmaxregister_increase` and `cute.arch.setmaxregister_decrease` instead.

- `alignment` argument in `CooperativeGroup` constructor → Scheduled for deprecation. It was unused; no replacement is suggested.

- `cute.AddressSpace` → Supported today as a quiet compatibility alias. It is scheduled for deprecation in a later release, where accesses will start emitting a deprecation warning. Use `cutlass.AddressSpace` for new code. The lowercase address-space members such as `gmem`, `smem`, `rmem`, `tmem`, and `dsmem` keep the same spelling.

## Deprecated Features

*(None currently.)*
