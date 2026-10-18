---
title: "Hopper (SM90)"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/utils_sm90.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# Hopper (SM90)

## get_smem_store_op

**Kind:** function

**Qualified name:** `cutlass.utils.sm90.get_smem_store_op`

**Signature:** `get_smem_store_op(layout_d: LayoutEnum, elem_ty_d: Type[_MockObject], elem_ty_acc: Type[_MockObject]) → CopyAtom`

Selects the largest vectorized smem store atom available subject to constraint of gmem layout.

## Parameters:

layout_dLayoutEnum

:   The layout enum of the output tensor D.

elem_ty_dType\[Numeric\]

:   The element type for output tensor D.

elem_ty_accType\[Numeric\]

:   The element type for accumulator.

## Returns:

Either SmemStoreMatrix or SimtSyncCopy, based on the input parameters.

## make_smem_layout_a

**Kind:** function

**Qualified name:** `cutlass.utils.sm90.make_smem_layout_a`

**Signature:** `make_smem_layout_a(a_layout: LayoutEnum, mma_tiler_mnk: cutlass.cute.typing.Tile, a_dtype: Type[_MockObject], num_stages: int) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

This function helps with:

1.  Get the partitioned shape of the A tensor based on the MMA tiler.

2.  Select the heuristic SMEM layout atom based on the A tensor's majorness, the data type, and the major mode size.

3.  cute.Tile the SMEM layout atom to the MMA tile shape.

4.  Stage the SMEM layout based on the number of stages.

### Parameters

- **a_layout** (*LayoutEnum*) -- The layout enum for tensor A

- **mma_tiler_mnk** (*cute.cute.Tile*) -- The MMA tile shape

- **a_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for tensor A

- **num_stages** (*int*) -- The number of pipeline stages for tensor A

### Returns

SMEM layout for tensor A

### Return type

Union\[cute.Layout, cute.ComposedLayout\]

## make_smem_layout_b

**Kind:** function

**Qualified name:** `cutlass.utils.sm90.make_smem_layout_b`

**Signature:** `make_smem_layout_b(b_layout: LayoutEnum, mma_tiler_mnk: cutlass.cute.typing.Tile, b_dtype: Type[_MockObject], num_stages: int) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

This function helps with:

1.  Get the partitioned shape of the B tensor based on the MMA tiler.

2.  Select the heuristic SMEM layout atom based on the B tensor's majorness, the data type, and the major mode size.

3.  cute.Tile the SMEM layout atom to the MMA tile shape.

4.  Stage the SMEM layout based on the number of stages.

### Parameters

- **b_layout** (*LayoutEnum*) -- The layout enum for tensor B

- **mma_tiler_mnk** (*cute.cute.Tile*) -- The MMA tile shape

- **b_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for tensor B

- **num_stages** (*int*) -- The number of pipeline stages for tensor B

### Returns

SMEM layout for tensor B

### Return type

Union\[cute.Layout, cute.ComposedLayout\]

## make_smem_layout_epi

**Kind:** function

**Qualified name:** `cutlass.utils.sm90.make_smem_layout_epi`

**Signature:** `make_smem_layout_epi(epi_dtype: Type[_MockObject], epi_layout: LayoutEnum, epi_tile: cutlass.cute.typing.Tile, epi_stage: int, smem_trg_shape: cutlass.cute.typing.Layout | None = None, smem_order: tuple | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

This function helps:

1.  Select the heuristic SMEM layout atom based on the epilog tile shape, the epilog tensor's majorness, and the element type.

2.  cute.Tile the SMEM layout atom to the epilog tile shape.

3.  Stage the SMEM layout based on the number of stages.

### Parameters

- **epi_dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The element type for the epilog tensor.

- **epi_layout** (*LayoutEnum*) -- The layout enum for the epilog tensor.

- **epi_tile** (*cute.cute.Tile*) -- The epilogue tile shape.

- **epi_stage** (*int*) -- The stage of the epilog tensor.

- **smem_trg_shape** (*cute.Layout* *\|* *None*) -- Target shape for SMEM layout (optional).

- **smem_order** (*tuple* *\|* *None*) -- Order for SMEM layout (optional).

### Returns

SMEM layout for epilog tensors (usually C & D which are processed in the epilog)

### Return type

Union\[cute.Layout, cute.ComposedLayout\]

## compute_tile_shape_or_override

**Kind:** function

**Qualified name:** `cutlass.utils.sm90.compute_tile_shape_or_override`

**Signature:** `compute_tile_shape_or_override(tile_shape_mnk: tuple[int, int, int], element_type: type[_MockObject], is_cooperative: bool = False, epi_tile_override: tuple[int, int] | None = None) → tuple[int, int]`

Compute the epilogue tile shape or use override if provided.

### Parameters

- **tile_shape_mnk** (*Tuple\[int,* *int,* *int\]*) -- CTA tile shape (M,N,K)

- **element_type** (*type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of elements

- **is_cooperative** (*bool*) -- Whether to use cooperative approach

- **epi_tile_override** (*Tuple\[int,* *int\] or* *None*) -- Optional override for epilogue tile shape

### Returns

Computed epilogue tile shape

### Return type

Tuple\[int, int\]
