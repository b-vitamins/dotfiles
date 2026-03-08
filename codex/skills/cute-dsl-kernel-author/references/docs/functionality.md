---
title: "Functionality"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/functionality.html"
cutlass_docs_version: "4.4.1"
generated_at: "2026-03-07T11:33:21+00:00"
last_updated: "Mar 05, 2026"
---

# Functionality {#functionality}

The CUTLASS DSL 4.0 release supports **Python 3.12** only. It shares the same driver requirements as the [CUDA Toolkit 12.9](https://docs.nvidia.com/cuda/cuda-toolkit-release-notes/index.html). Specifically, the driver version must be 575.51.03 or later.

Currently, only Linux x86_64 is supported. Additional platform support will be added in future releases.

## Supported MMA Operations

**NVIDIA Ampere Architecture:**

-   FP16 / BF16 tensor core instructions

**NVIDIA Hopper Architecture:**

-   FP16 / BF16

-   FP8

**NVIDIA Blackwell Architecture:**

-   FP16 / BF16

-   TF32

-   I8

-   F8

## Notable Limitations

For current constraints and unsupported features, refer to the [Limitations](limitations.md) section.
