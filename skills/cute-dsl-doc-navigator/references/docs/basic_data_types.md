---
title: "Basic Data Types"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/basic_data_types.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# Basic Data Types {#basic-data-types}

## Scalar Types

### Numeric

*class* cutlass.Numeric(

*value: bool \| int \| float \| Value \| [Numeric](#cutlass.Numeric)*,

)

Base class for all numeric types in the DSL.

This class provides the foundation for both Integer and Float types, implementing basic arithmetic operations.

Parameters:

:   **value** (*Union\[bool,* *int,* *float,* *Value,* [*Numeric*](#cutlass.Numeric)*\]*) -- The value to store in the numeric type

Variables:

:   **value** (*Union\[bool,* *int,* *float,* *Value\]*) -- The stored numeric value

\_\_init\_\_(

*value: bool \| int \| float \| Value \| [Numeric](#cutlass.Numeric)*,

) → None

bitcast(

*dtype: Type\[[Numeric](#cutlass.Numeric)\]*,

) → [Numeric](#cutlass.Numeric)

Reinterpret the bits of this value as a different numeric type.

The source and target types must have the same bit width.

Parameters:

:   **dtype** -- Target DSL type (e.g., `Float32` when self is `Int32`).

Returns:

:   A new instance of `dtype` with the same bit pattern.

<!-- -->

to(*dtype: Type*) → Any

:   Convert this numeric value to another numeric type.

    If the target type is the same as the current type, returns self. Otherwise, creates a new instance of the target type with the same value.

    Parameters:

    :   **dtype** (*Union\[Type\[\"Numeric\"\],* *Type\[int\],* *Type\[float\],* *Type\[bool\]\]*) -- The target numeric type to convert to

    Returns:

    :   A new instance of the target type, or self if types match

    Return type:

    :   [Numeric](#cutlass.Numeric)

    Raises:

    :   - **ValueError** -- If trying to convert an MLIR value to a static Python type

        - **TypeError** -- If trying to convert to unsupported float types like Float8E4M3, Float8E4M3B11FNUZ, Float4E2M1FN, Float6E3M2FN, or Float6E2M3FN

    > **Note.**
    >
    > Unsupported destination float types:
    >
    > :   - Float8E4M3
    >
    >     - Float8E4M3B11FNUZ
    >
    >     - Float4E2M1FN
    >
    >     - Float6E3M2FN
    >
    >     - Float6E2M3FN

    Example:

        # Convert between DSL numeric types.
        x = Int32(5)
        y = x.to(Float32)  # Converts to Float32(5.0)

        # Convert to Python primitive types.
        # They are considered static values at JIT time.
        z = x.to(int)      # Returns Python int 5.
        w = y.to(float)    # Returns Python float 5.0.

        # This raises ValueError because MLIR values are not static.
        mlir_val = arith.constant(T.i32(), 42)
        num = Int32(mlir_val)
        num.to(int)

### Integer

*class* cutlass.Integer(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

A class representing integer values with specific width and signedness.

This class provides functionality to create and manipulate integer values with configurable width and signedness. It supports conversion from various input types including Python scalars, MLIR Values, and other numeric types.

Parameters:

:   **x** (*Union\[bool,* *int,* *float,* *ir.Value,* [*Integer*](#cutlass.Integer)*,* [*Float*](#cutlass.Float)*\]*) -- The input value to convert to this integer type

Returns:

:   A new Integer instance with the converted value

Return type:

:   [Integer](#cutlass.Integer)

Raises:

:   - **AssertionError** -- If the type has no exactly representable range

    - **NotImplementedError** -- If converting between different Integer types

    - **ValueError** -- If the input type is not supported for conversion

    - **OverflowError** -- If converting float infinity to integer, or an integer too wide for any C integer type

Type conversion behavior:

-

  Python scalars (bool, int, float):

  :   - Converted through the target dtype's C cast

      - NaN and infinity values are rejected

      - A value whose magnitude exceeds the target width is narrowed by the dtype's C cast and emits `TYPE_INT_LITERAL_OUT_OF_RANGE` for an integer literal (`Int8(256) -> 0`) or `TYPE_FLOAT_TO_INT_OUT_OF_RANGE` for a float one (`Int32(1e40)`). An integer wraps, dropping the high bits; a float follows the host CPU's own conversion and is therefore architecture-defined, matching NumPy on the same machine. To materialize a specific bit pattern intentionally, mask first (`Int8(256 & 0xFF)`).

-

  MLIR Value with IntegerType:

  :   - Width differences handled by signless to signed/unsigned conversion

      - Example: i8 -\> i8/ui8 depending on target type

-

  MLIR Value with FloatType:

  :   - Uses MLIR float-to-int conversion

      - NaN and infinity values is undefined behavior

      - Example: f32 -\> i32/ui32 depending on target type

-

  Integer:

  :   - Uses MLIR int-to-int conversion or the target dtype's C cast

      - Example: Int32(Int32(5)) =\> 5

-

  Float:

  :   - Uses MLIR float-to-int conversion

      - Example: Int32(Float(5.7)) -\> 5

Example usage:

    x = Int32(5)  # From integer
    y = Int32(True)  # From boolean
    z = Int32(3.7)  # From float (truncates)
    w = Int32(x)  # From same Integer type
    c5 = arith.constant(5, T.i32())
    a = Int32(c5)  # Treat c5 as int32 bitwise

\_\_init\_\_(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

) → None

### Boolean

*class* cutlass.Boolean(*a: bool \| int \| float \| ir.Value \| [Numeric](#cutlass.Numeric)*)

Boolean type representation in the DSL.

This class represents boolean values in the DSL, with a width of 1 bit. It supports conversion from various types to boolean values.

Parameters:

:   - **a** (*Union\[bool,* *int,* *float,* *\"Value\",* [*Numeric*](#cutlass.Numeric)*\]*) -- Value to convert to Boolean

    - **loc** (*Optional\[Location\],* *optional*) -- Source location information, defaults to None

    - **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point for MLIR operations, defaults to None

Raises:

:   **DSLRuntimeError** -- If the input value cannot be converted to Boolean

Conversion rules:

1.  Python bool/int/float: - Converted using Python's bool() function - Example: Boolean(1) -\> True, Boolean(0) -\> False

2.  Numeric: - Uses the Numeric.value to construct Boolean recursively

3.  MLIR Value with IntegerType: - If width is 1: Direct assignment - Otherwise: Compares with 0 using arith.cmpi

4.  MLIR Value with FloatType: - Compares with 0.0 using arith.cmpf - Uses unordered comparison to handle NaN values

\_\_init\_\_(

*a: bool \| int \| float \| ir.Value \| [Numeric](#cutlass.Numeric)*,

) → None

ir_value_int8() → ir.Value

:   Returns int8 ir value of Boolean. When we need to store Boolean tensor element, use ir_value_int8().

    Parameters:

    :   - **loc** (*Optional\[Location\],* *optional*) -- Source location information, defaults to None

        - **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point for MLIR operations, defaults to None

    Returns:

    :   The int8 value of this Boolean

    Return type:

    :   ir.Value

### Signed Integer Types

*class* cutlass.Int4(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Int8(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Int16(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Int32(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Int64(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Int128(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

### Unsigned Integer Types

The public unsigned scalar classes currently start at `cutlass.Uint8`. Packed 1-bit and 2-bit fields, such as predicate masks or sparse metadata, are represented through wider storage types or API-specific packed operands rather than separate `cutlass.Uint1` or `cutlass.Uint2` scalar classes.

*class* cutlass.Uint8(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Uint16(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Uint32(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Uint64(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Uint128(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

### Float

*class* cutlass.Float(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

A class representing floating-point values.

Parameters:

:   **x** (*Union\[bool,* *int,* *float,* *ir.Value,* [*Integer*](#cutlass.Integer)*,* [*Float*](#cutlass.Float)*\]*) -- The input value to convert to this float type.

Type conversion behavior:

1.  Python scalars (bool, int, float):

    - Kept at full Python-float precision and narrowed during IR emission

    - Example: Float32(1.7) -\> 1.7

    - A value whose magnitude cannot be represented in the target type collapses to +/-inf (overflow) or 0 (underflow) and emits a `TYPE_FLOAT_LITERAL_OVERFLOW` / `TYPE_FLOAT_LITERAL_UNDERFLOW` warning, e.g. `Float32(1e40) -> inf`. Ordinary precision/rounding loss (`Float32(0.1)`) is not flagged.

2.  MLIR Value with FloatType: - If width differs: converts between float types - Example: f16 -\> f32

3.  MLIR Value with IntegerType: - Not supported, raises ValueError

4.  Integer: - Converts using MLIR int-to-float operation - Example: Float32(Int32(5)) -\> 5.0

5.  Float: - Direct conversion between float types - Example: Float32(Float32(1.5)) -\> 1.5

> **Note.**
>
> The following narrow precision types are only supported in device code:
>
> 8-bit float types:
>
> :   - Float8E5M2
>
>     - Float8E4M3
>
>     - Float8E4M3FN
>
>     - Float8E8M0FNU
>
>     - Float8E4M3B11FNUZ
>
> 6-bit float types:
>
> :   - Float6E3M2FN
>
>     - Float6E2M3FN
>
> 4-bit float types:
>
> :   - Float4E2M1FN
>
> Narrow precision types and special floating-point formats support matrix on device:

Raises:

:   **ValueError** -- If conversion from the input type is not supported

\_\_init\_\_(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

) → None

### Standard Floating-Point Types

*class* cutlass.Float16(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.BFloat16(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.TFloat32(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Float32(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Float64(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

### Narrow Floating-Point Types

*class* cutlass.Float8E5M2(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Float8E4M3(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Float8E4M3FN(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Float8E4M3B11FNUZ(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Float8E8M0FNU(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Float6E3M2FN(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Float6E2M3FN(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Float4E2M1FN(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

*class* cutlass.Float4E2M1FNx2(

*x: bool \| int \| float \| ir.Value \| [Integer](#cutlass.Integer) \| [Float](#cutlass.Float)*,

)

Packed FP4 E2M1 --- 2 elements per byte (matches `torch.float4_e2m1fn_x2`).

Shape and strides on any layout carrying this dtype are interpreted in **fp4x2 tensor-element units**. One tensor element is already one packed storage unit, so `create_tensor_map_tiled_from_view` uses `width == 8` directly when converting stride units for TMA.

`width` is the packed 8-bit tensor-element width and `mlir_type` is the packed storage type `i8`. Internal helpers that still need scalar FP4 lane precision treat this packed dtype specially by class identity.

Use this dtype when the input is already organized in packed fp4x2 storage units (for example a `torch.uint8` buffer viewed as `torch.float4_e2m1fn_x2`) or when the kernel allocates layouts directly from packed extents.

## Pointer

*class* cutlass.Pointer(

*base: ir.Value*,

*\**,

*dtype: Type\[[Numeric](#cutlass.Numeric)\] \| None = None*,

*space: [AddressSpace](api/cute.md#cutlass.cute.AddressSpace) \| int \| None = None*,

)

An `llvm.ptr` value with element dtype metadata.

`Pointer` is the canonical low-level DSL pointer type in the `cutlass` namespace. It subclasses `ir.Value` so it can be passed directly to MLIR ops that require a pointer operand.

\_\_init\_\_(

*base: ir.Value*,

*\**,

*dtype: Type\[[Numeric](#cutlass.Numeric)\] \| None = None*,

*space: [AddressSpace](api/cute.md#cutlass.cute.AddressSpace) \| int \| None = None*,

) → None

## Vector

*class* cutlass.Vector(*v: ir.Value*, *\**, *dtype: Type\[[Numeric](#cutlass.Numeric)\] \| None = None*)

Wrap an MLIR `vector<NxTy>` register value with DSL type information.

Provides element extraction (`vec[i]` / `vec[a:b]`), element-wise arithmetic (`+`, `-`, `*`, `/`), type conversion ([`to()`](#cutlass.Vector.to)), and bit-reinterpretation ([`bitcast()`](#cutlass.Vector.bitcast)) on top of a raw MLIR vector.

Vectors live entirely in registers --- they carry no memory address and do not support in-place element assignment.

Registered as the MLIR value caster for `ir.VectorType`, so any op that returns a vector automatically produces a `Vector` instance.

Parameters:

:   - **v** (*ir.Value*) -- Underlying MLIR vector value.

    - **dtype** (*type,* *optional*) -- DSL element type (e.g. `Float32`, `Int32`). Inferred from the MLIR element type when omitted.

\_\_init\_\_(

*v: ir.Value*,

*\**,

*dtype: Type\[[Numeric](#cutlass.Numeric)\] \| None = None*,

) → None

bitcast(*dtype: type*) → [Vector](#cutlass.Vector)

:   Reinterpret the vector bits as a different element type.

    The total bit width is preserved; the element count adjusts proportionally. For example, `vector<4xi32>` bitcast to `Float16` yields `vector<8xf16>` (4 × 32 = 8 × 16 bits).

    Parameters:

    :   **dtype** (*Type\[*[*Numeric*](#cutlass.Numeric)*\]*) -- Target DSL element type (e.g. `Float32`, `Float16`).

    Returns:

    :   A new [`Vector`](#cutlass.Vector) with bits reinterpreted as `dtype`.

    Return type:

    :   [Vector](#cutlass.Vector)

    Raises:

    :   **TypeError** -- If `dtype` is not a subclass of `Numeric`.

<!-- -->

*property* dtype*: Type\[[Numeric](#cutlass.Numeric)\]*

:   The DSL element type (e.g., Float32, Int32).

*static* from_elements(

*scalars: tuple*,

*dtype: Type\[[Numeric](#cutlass.Numeric)\]*,

) → [Vector](#cutlass.Vector)

Build a 1-D `Vector` from a tuple of scalar values.

ir_value() → ir.Value

:   Return the underlying MLIR vector value.

<!-- -->

numel() → int

:   Total number of elements (product of all shape dimensions).

reduce(

*op: Literal\[\'add\', \'mul\', \'min\', \'max\'\] = \'add\'*,

*\**,

*dim: int \| list\[int\] \| None = None*,

*acc: Any = None*,

) → Any

Reduce the vector using the specified combining operation.

When `dim` is `None` (default), reduces **all** dimensions to a scalar via `vector.reduction`. When `dim` is an int or list of ints, reduces only those dimensions via `vector.multi_reduction`, returning a lower-rank [`Vector`](#cutlass.Vector).

Parameters:

:   - **op** -- Reduction operation --- one of `"add"`, `"mul"`, `"min"`, `"max"`. For `"min" / "max"` the combining kind adapts automatically to the element type (float vs signed vs unsigned integer).

    - **dim** -- Dimension(s) to reduce. `None` reduces all dims to a scalar. An int or list of ints reduces only those dims.

    - **acc** -- Optional accumulator. For scalar reduction a scalar value; for multi-dim reduction a vector matching the result shape.

Returns:

:   Scalar (when `dim is None`) or [`Vector`](#cutlass.Vector) (when `dim` is specified).

Examples

    v = cute.full((4,), 3.0, dtype=cutlass.Float32).to_vector()
    v.reduce("add")          # 12.0  (scalar)

    m = cute.full((4, 8), 1.0, dtype=cutlass.Float32).to_vector()
    m.reduce("add", dim=1)   # vector<4xf32>, each element = 8.0
    m.reduce("add", dim=0)   # vector<8xf32>, each element = 4.0

> **Note.**
>
> This method operates on a `Vector` value. If a higher-level API in a downstream library returns a different SSA wrapper with its own `reduce(...)` method and a different signature, call that library's `.to_vector()` (or equivalent) to get a plain `Vector` first so this 1-arg form applies.

> **Note.**
>
> `Vector.reduce` builds an MLIR `vector.reduction` over the elements of one register vector. It is not the warp-collective `nvvm.redux_sync` API. If a backend or target-specific lowering maps a reduction to PTX `redux.sync`, PTX legality still applies: integer/bitwise `redux.sync` forms require `sm_80` or higher, while `redux.sync.f32` min/max support was added in PTX ISA 8.6 and is limited to `sm_100a` plus the `sm_100f` family support added in PTX ISA 8.8. For examples that must remain portable to generic `sm_120` targets, prefer an explicit scalar fold or shuffle tree for `Float32` min/max instead of relying on a lowering that may choose `redux.sync.f32`.

*property* shape*: tuple\[int, \...\]*

:   The logical shape of the vector array (1D, 2D, or 3D).

<!-- -->

to(*dtype: type*) → [Vector](#cutlass.Vector)

:   Convert the vector elements to a different numeric type.

    Parameters:

    :   **dtype** (*Type\[*[*Numeric*](#cutlass.Numeric)*\]*) -- Target DSL element type (e.g. `Float16`, `Int32`).

    Returns:

    :   A new [`Vector`](#cutlass.Vector) with the same shape and elements cast to `dtype`.

    Return type:

    :   [Vector](#cutlass.Vector)

    Raises:

    :   **TypeError** -- If `dtype` is not a subclass of `Numeric`.

    Example:

        vec_f32 = cute.full((4,), 1.5, dtype=cutlass.Float32).to_vector()
        vec_i32 = vec_f32.to(cutlass.Int32)    # fp → int truncation
        vec_f16 = vec_f32.to(cutlass.Float16)  # fp32 → fp16 narrowing

<!-- -->

to_elements() → tuple\[[Numeric](#cutlass.Numeric), \...\]

:   Extract every vector lane as scalar DSL values.

    This is useful when a vectorized operation should define many scalar SSA values that are then consumed independently.

<!-- -->

with_signedness(*signed: bool \| None*) → [Vector](#cutlass.Vector)

:   Override ArithValue.with_signedness for keyword-only \_\_init\_\_.
