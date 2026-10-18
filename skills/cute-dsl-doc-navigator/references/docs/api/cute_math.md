---
title: "Math"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_math.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# Math

The `cutlass.cute.math` module provides math functions for CuTe DSL programs.

## API documentation

CuTeDSL math API --- Array-aware wrappers over `cutlass._mlir_helpers.math`.

All math implementations live in the foundation module. This file is the canonical CuTeDSL-side surface and adds `Array` support on top of the foundation ops: each op unwraps an `Array`'s underlying `Vector` before delegating, then rewraps the result as an `Array` when appropriate.

For `TensorSSA` / `Vector` / scalar inputs the wrapper is transparent. `TensorSSA` inherits from `Vector`, so the foundation's Vector dispatch path handles CuTeDSL tensor values directly --- no unwrap happens --- and per-element ops preserve a `TensorSSA`'s CuTe nested shape via the `_wrap_like` polymorphic hook on the `Vector` base class.

`abs` is also aliased to `absf` for backwards compatibility with the historical CuTeDSL name.

### RoundingMode

**Kind:** class

**Qualified name:** `cutlass.cute.math.RoundingMode`

**Signature:** `RoundingMode(value)`

Bases: `str`, `Enum`

IEEE 754 rounding modes for floating-point operations.

#### NEAREST_EVEN

**Kind:** attribute

**Qualified name:** `cutlass.cute.math.RoundingMode.NEAREST_EVEN`

**Signature:** `NEAREST_EVEN = 'rn'`

#### ZERO

**Kind:** attribute

**Qualified name:** `cutlass.cute.math.RoundingMode.ZERO`

**Signature:** `ZERO = 'rz'`

#### UP

**Kind:** attribute

**Qualified name:** `cutlass.cute.math.RoundingMode.UP`

**Signature:** `UP = 'rp'`

#### DOWN

**Kind:** attribute

**Qualified name:** `cutlass.cute.math.RoundingMode.DOWN`

**Signature:** `DOWN = 'rm'`

### sin

**Kind:** function

**Qualified name:** `cutlass.cute.math.sin`

**Signature:** `sin(x: Numeric | Vector | float | int | bool, fastmath: bool = False, approx: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise sine of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand (in radians)

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the sine of each element

#### Return type

MathOperand

Example:

    y = sin(a)  # Compute sine

### cos

**Kind:** function

**Qualified name:** `cutlass.cute.math.cos`

**Signature:** `cos(x: Numeric | Vector | float | int | bool, fastmath: bool = False, approx: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise cosine of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand (in radians)

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the cosine of each element

#### Return type

MathOperand

Example:

    y = cos(a)  # Compute cosine

### exp2

**Kind:** function

**Qualified name:** `cutlass.cute.math.exp2`

**Signature:** `exp2(x: Numeric | Vector | float | int | bool, fastmath: bool = False, approx: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise base-2 exponential of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing 2 raised to the power of each element

#### Return type

MathOperand

Example:

    y = exp2(a)  # Compute 2^x

### log2

**Kind:** function

**Qualified name:** `cutlass.cute.math.log2`

**Signature:** `log2(x: Numeric | Vector | float | int | bool, fastmath: bool = False, approx: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise base-2 logarithm of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the base-2 logarithm of each element

#### Return type

MathOperand

Example:

    y = log2(a)  # Compute log base 2

### tanh

**Kind:** function

**Qualified name:** `cutlass.cute.math.tanh`

**Signature:** `tanh(x: Numeric | Vector | float | int | bool, fastmath: bool = False, approx: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise hyperbolic tangent of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the hyperbolic tangent of each element

#### Return type

MathOperand

Example:

    y = tanh(a)  # Compute hyperbolic tangent

### rsqrt

**Kind:** function

**Qualified name:** `cutlass.cute.math.rsqrt`

**Signature:** `rsqrt(x: Numeric | Vector | float | int | bool, fastmath: bool = False, approx: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise reciprocal square root of the input operand.

Computes 1/√x element-wise.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the reciprocal square root of each element

#### Return type

MathOperand

Example:

    y = rsqrt(a)  # Compute 1/√x

### sqrt

**Kind:** function

**Qualified name:** `cutlass.cute.math.sqrt`

**Signature:** `sqrt(x: Numeric | Vector | float | int | bool, fastmath: bool = False, approx: bool = False, ftz: bool = False, rounding: RoundingMode | None = None) → Numeric | Vector | float | int | bool`

Compute element-wise square root of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the square root of each element

#### Return type

MathOperand

Example:

    y = sqrt(a)  # Compute square root

### exp

**Kind:** function

**Qualified name:** `cutlass.cute.math.exp`

**Signature:** `exp(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise exponential of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the exponential of each element

#### Return type

MathOperand

Example:

    y = exp(a)  # Compute exponential

### log

**Kind:** function

**Qualified name:** `cutlass.cute.math.log`

**Signature:** `log(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise natural logarithm of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the natural logarithm of each element

#### Return type

MathOperand

Example:

    y = log(a)  # Compute natural logarithm

### log10

**Kind:** function

**Qualified name:** `cutlass.cute.math.log10`

**Signature:** `log10(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise base-10 logarithm of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the base-10 logarithm of each element

#### Return type

MathOperand

Example:

    y = log10(a)  # Compute log base 10

### tan

**Kind:** function

**Qualified name:** `cutlass.cute.math.tan`

**Signature:** `tan(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise tangent of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand (in radians)

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the tangent of each element

#### Return type

MathOperand

Example:

    y = tan(a)  # Compute tangent

### acos

**Kind:** function

**Qualified name:** `cutlass.cute.math.acos`

**Signature:** `acos(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise arc cosine of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the arc cosine of each element in input operand

#### Return type

MathOperand

Example:

    y = acos(a)  # Compute arc cosine

### asin

**Kind:** function

**Qualified name:** `cutlass.cute.math.asin`

**Signature:** `asin(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise arc sine of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the arc sine of each element in input operand

#### Return type

MathOperand

Example:

    y = asin(a)  # Compute arc sine

### atan

**Kind:** function

**Qualified name:** `cutlass.cute.math.atan`

**Signature:** `atan(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise arc tangent of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the arc tangent of each element in input operand

#### Return type

MathOperand

Example:

    y = atan(a)  # Compute arc tangent

### atan2

**Kind:** function

**Qualified name:** `cutlass.cute.math.atan2`

**Signature:** `atan2(y: Numeric | Vector | float | int | bool, x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise arc tangent of two tensors.

Computes atan2(a, b) element-wise. The function atan2(a, b) is the angle in radians between the positive x-axis and the point given by the coordinates (b, a).

#### Parameters

- **a** (*MathOperand*) -- First input operand (y-coordinates)

- **b** (*MathOperand*) -- Second input operand (x-coordinates)

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the arc tangent of a/b element-wise

#### Return type

MathOperand

Example:

    theta = atan2(y, x)  # Compute angles

### sinh

**Kind:** function

**Qualified name:** `cutlass.cute.math.sinh`

**Signature:** `sinh(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise hyperbolic sine of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the hyperbolic sine of each element

#### Return type

MathOperand

Example:

    y = sinh(a)  # Compute hyperbolic sine

### cosh

**Kind:** function

**Qualified name:** `cutlass.cute.math.cosh`

**Signature:** `cosh(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise hyperbolic cosine of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the hyperbolic cosine of each element

#### Return type

MathOperand

Example:

    y = cosh(a)  # Compute hyperbolic cosine

### acosh

**Kind:** function

**Qualified name:** `cutlass.cute.math.acosh`

**Signature:** `acosh(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise inverse hyperbolic cosine of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand (must be \>= 1.0)

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the inverse hyperbolic cosine of each element

#### Return type

MathOperand

Example:

    y = acosh(a)  # Compute inverse hyperbolic cosine

### asinh

**Kind:** function

**Qualified name:** `cutlass.cute.math.asinh`

**Signature:** `asinh(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise inverse hyperbolic sine of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the inverse hyperbolic sine of each element

#### Return type

MathOperand

Example:

    y = asinh(a)  # Compute inverse hyperbolic sine

### atanh

**Kind:** function

**Qualified name:** `cutlass.cute.math.atanh`

**Signature:** `atanh(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise inverse hyperbolic tangent of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand (must be in (-1, 1))

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the inverse hyperbolic tangent of each element

#### Return type

MathOperand

Example:

    y = atanh(a)  # Compute inverse hyperbolic tangent

### erf

**Kind:** function

**Qualified name:** `cutlass.cute.math.erf`

**Signature:** `erf(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise error function of the input operand.

The error function is defined as: erf(x) = 2/√π ∫\[0 to x\] exp(-t²) dt

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the error function value for each element

#### Return type

MathOperand

Example:

    y = erf(a)  # Compute error function

### erfc

**Kind:** function

**Qualified name:** `cutlass.cute.math.erfc`

**Signature:** `erfc(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise complementary error function of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the complementary error function of each element

#### Return type

MathOperand

Example:

    y = erfc(a)  # Compute complementary error function

### pow

**Kind:** function

**Qualified name:** `cutlass.cute.math.pow`

**Signature:** `pow(base: Numeric | Vector | float | int | bool, exponent: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise power of the input tensors.

#### Parameters

- **a** (*MathOperand*) -- Input operand (base)

- **b** (*MathOperand*) -- Input operand (exponent)

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing a raised to the power b for each element

#### Return type

MathOperand

Example:

    z = pow(a, b)  # Compute a^b

### fpowi

**Kind:** function

**Qualified name:** `cutlass.cute.math.fpowi`

**Signature:** `fpowi(base: Numeric | Vector | float | int | bool, exponent: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Float raised to integer power: base\^exponent (exponent is integer).

### ipowi

**Kind:** function

**Qualified name:** `cutlass.cute.math.ipowi`

**Signature:** `ipowi(base: Numeric | Vector | float | int | bool, exponent: Numeric | Vector | float | int | bool) → Numeric | Vector | float | int | bool`

Integer raised to integer power: base\^exponent.

### cbrt

**Kind:** function

**Qualified name:** `cutlass.cute.math.cbrt`

**Signature:** `cbrt(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise cube root of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the cube root of each element

#### Return type

MathOperand

Example:

    y = cbrt(a)  # Compute cube root

### expm1

**Kind:** function

**Qualified name:** `cutlass.cute.math.expm1`

**Signature:** `expm1(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise exp(x) - 1 of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing exp(x) - 1 of each element

#### Return type

MathOperand

Example:

    y = expm1(a)  # Compute exp(x) - 1

### log1p

**Kind:** function

**Qualified name:** `cutlass.cute.math.log1p`

**Signature:** `log1p(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise log(1 + x) of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing log(1 + x) of each element

#### Return type

MathOperand

Example:

    y = log1p(a)  # Compute log(1 + x)

### sincos

**Kind:** function

**Qualified name:** `cutlass.cute.math.sincos`

**Signature:** `sincos(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → tuple`

Combined sine and cosine: returns (sin(x), cos(x)).

More efficient than separate sin() and cos() calls.

### abs

**Kind:** function

**Qualified name:** `cutlass.cute.math.abs`

**Signature:** `abs(x: Numeric | Vector | float | int | bool, fastmath: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Absolute value (float or integer).

#### Parameters

**ftz** -- Flush denormals to zero (float only, uses nvvm.fabs)

### absi

**Kind:** function

**Qualified name:** `cutlass.cute.math.absi`

**Signature:** `absi(x: Numeric | Vector | float | int | bool) → Numeric | Vector | float | int | bool`

Integer absolute value.

### copysign

**Kind:** function

**Qualified name:** `cutlass.cute.math.copysign`

**Signature:** `copysign(mag: Numeric | Vector | float | int | bool, sign: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise copysign, combining magnitude of a with sign of b.

#### Parameters

- **a** (*MathOperand*) -- Input operand (magnitude source)

- **b** (*MathOperand*) -- Input operand (sign source)

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the copysign of each element

#### Return type

MathOperand

Example:

    z = copysign(a, b)  # Copy sign of b to magnitude of a

### neg

**Kind:** function

**Qualified name:** `cutlass.cute.math.neg`

**Signature:** `neg(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Floating-point negation.

### ceil

**Kind:** function

**Qualified name:** `cutlass.cute.math.ceil`

**Signature:** `ceil(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise ceiling of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the ceiling of each element

#### Return type

MathOperand

Example:

    y = ceil(a)  # Compute ceiling

### floor

**Kind:** function

**Qualified name:** `cutlass.cute.math.floor`

**Signature:** `floor(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise floor of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the largest integer less than or equal to each element in input operand

#### Return type

MathOperand

Example:

    y = floor(a)  # Compute floor

### round

**Kind:** function

**Qualified name:** `cutlass.cute.math.round`

**Signature:** `round(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise round to nearest integer (ties away from zero) of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the rounded value of each element

#### Return type

MathOperand

Example:

    y = round(a)  # Round to nearest integer

### roundeven

**Kind:** function

**Qualified name:** `cutlass.cute.math.roundeven`

**Signature:** `roundeven(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise round to nearest integer (ties to even) of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the rounded value of each element

#### Return type

MathOperand

Example:

    y = roundeven(a)  # Round to nearest integer (ties to even)

### trunc

**Kind:** function

**Qualified name:** `cutlass.cute.math.trunc`

**Signature:** `trunc(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Compute element-wise truncation toward zero of the input operand.

#### Parameters

- **a** (*MathOperand*) -- Input operand

- **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

Result containing the truncated value of each element

#### Return type

MathOperand

Example:

    y = trunc(a)  # Truncate toward zero

### clamp

**Kind:** function

**Qualified name:** `cutlass.cute.math.clamp`

**Signature:** `clamp(x: Numeric | Vector | float | int | bool, lo: Numeric | Vector | float | int | bool, hi: Numeric | Vector | float | int | bool, fastmath: bool = False, propagate_nan: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Clamp value to range \[lo, hi\].

Lowered unconditionally as `max(lo, min(x, hi))`. The math dialect's `clampf` op currently has no LLVM translation interface registered, so a direct lowering fails at JIT time on scalar f16 inputs (and any other type). Composition via min/max picks up the right per-type lowering (`arith.minnumf` / `arith.maximumf` / `nvvm.fmin` / `nvvm.fmax`).

### min

**Kind:** function

**Qualified name:** `cutlass.cute.math.min`

**Signature:** `min(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, propagate_nan: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Element-wise minimum.

#### Parameters

- **propagate_nan** -- If True, NaN propagates (IEEE 754 minimum). If False, NaN is ignored (IEEE 754 minimumNumber).

- **ftz** -- Flush denormals to zero (float only, uses nvvm.fmin).

### max

**Kind:** function

**Qualified name:** `cutlass.cute.math.max`

**Signature:** `max(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, propagate_nan: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Element-wise maximum.

#### Parameters

- **propagate_nan** -- If True, NaN propagates (IEEE 754 maximum). If False, NaN is ignored (IEEE 754 maximumNumber).

- **ftz** -- Flush denormals to zero (float only, uses nvvm.fmax).

### isnan

**Kind:** function

**Qualified name:** `cutlass.cute.math.isnan`

**Signature:** `isnan(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Test if value is NaN. Returns i1.

### isinf

**Kind:** function

**Qualified name:** `cutlass.cute.math.isinf`

**Signature:** `isinf(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Test if value is infinite. Returns i1.

### isfinite

**Kind:** function

**Qualified name:** `cutlass.cute.math.isfinite`

**Signature:** `isfinite(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Test if value is finite. Returns i1.

### isnormal

**Kind:** function

**Qualified name:** `cutlass.cute.math.isnormal`

**Signature:** `isnormal(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Test if value is normal (not zero, subnormal, inf, or NaN). Returns i1.

### fma

**Kind:** function

**Qualified name:** `cutlass.cute.math.fma`

**Signature:** `fma(a: Numeric | Vector | float | int | bool, b: Numeric | Vector | float | int | bool, c: Numeric | Vector | float | int | bool, fastmath: bool = False, rounding: RoundingMode | None = None, ftz: bool = False) → Numeric | Vector | float | int | bool`

Fused multiply-add: a \* b + c.

### add

**Kind:** function

**Qualified name:** `cutlass.cute.math.add`

**Signature:** `add(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, fastmath: bool = False, rounding: RoundingMode | None = None, ftz: bool = False) → Numeric | Vector | float | int | bool`

Floating-point addition.

### sub

**Kind:** function

**Qualified name:** `cutlass.cute.math.sub`

**Signature:** `sub(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Floating-point subtraction.

Note: NVVM sub uses a different intrinsic convention (rounding as an integer arg), so rounding/ftz are not supported here. Callers that need explicit rounding control on subtraction should emit the PTX-level `sub.<rn|rz|rm|rp>[.ftz].f32` intrinsic directly.

### mul

**Kind:** function

**Qualified name:** `cutlass.cute.math.mul`

**Signature:** `mul(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, fastmath: bool = False, rounding: RoundingMode | None = None, ftz: bool = False) → Numeric | Vector | float | int | bool`

Floating-point multiplication.

### div

**Kind:** function

**Qualified name:** `cutlass.cute.math.div`

**Signature:** `div(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, fastmath: bool = False, approx: bool = False, ftz: bool = False, full: bool = False, rounding: RoundingMode | None = None) → Numeric | Vector | float | int | bool`

Floating-point division.

### rem

**Kind:** function

**Qualified name:** `cutlass.cute.math.rem`

**Signature:** `rem(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Floating-point remainder.

### rcp

**Kind:** function

**Qualified name:** `cutlass.cute.math.rcp`

**Signature:** `rcp(x: Numeric | Vector | float | int | bool, fastmath: bool = False, approx: bool = False, rounding: RoundingMode | None = None, ftz: bool = False) → Numeric | Vector | float | int | bool`

Reciprocal (1/x).

### absf

**Kind:** function

**Qualified name:** `cutlass.cute.math.absf`

**Signature:** `absf(x: Numeric | Vector | float | int | bool, fastmath: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Absolute value (float or integer).

#### Parameters

**ftz** -- Flush denormals to zero (float only, uses nvvm.fabs)
