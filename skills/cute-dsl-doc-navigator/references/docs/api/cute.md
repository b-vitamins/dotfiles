---
title: "Core"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# Core

## cutlass.cute

### Swizzle

**Kind:** class

**Qualified name:** `cutlass.cute.Swizzle`

**Signature:** `Swizzle(value = None, *args, **kwargs)`

Bases: `Value`

Swizzle is a transformation that permutes the elements of a layout.

Swizzles are used to rearrange data elements to improve memory access patterns and computational efficiency.

Swizzle is defined by three parameters:

- MBase: The number of least-significant bits to keep constant

- BBits: The number of bits in the mask

- SShift: The distance to shift the mask

The mask is applied to the least-significant bits of the layout.

    0bxxxxxxxxxxxxxxxYYYxxxxxxxZZZxxxx
                                  ^--^ MBase is the number of least-sig bits to keep constant
                     ^-^       ^-^     BBits is the number of bits in the mask
                       ^---------^     SShift is the distance to shift the YYY mask
                                          (pos shifts YYY to the right, neg shifts YYY to the left)

    e.g. Given
    0bxxxxxxxxxxxxxxxxYYxxxxxxxxxZZxxx

    the result is
    0bxxxxxxxxxxxxxxxxYYxxxxxxxxxAAxxx where AA = ZZ `xor` YY

#### num_bits

**Kind:** property

**Qualified name:** `cutlass.cute.Swizzle.num_bits`

**Signature:** `num_bits: int`

Returns the number of bits in the mask (B in Sw\<B,M,S\>).

#### num_base

**Kind:** property

**Qualified name:** `cutlass.cute.Swizzle.num_base`

**Signature:** `num_base: int`

Returns the number of least-significant bits to keep constant (M in Sw\<B,M,S\>).

#### num_shift

**Kind:** property

**Qualified name:** `cutlass.cute.Swizzle.num_shift`

**Signature:** `num_shift: int`

Returns the distance to shift the mask (S in Sw\<B,M,S\>).

### struct

**Kind:** class

**Qualified name:** `cutlass.cute.struct`

**Signature:** `struct(cls: type)`

Bases: `object`

Decorator to abstract C structure in Python DSL.

**Usage:**

    # Supports base_dsl scalar int/float elements, array and nested struct:
    @cute.struct
    class complex:
        real : cutlass.Float32
        imag : cutlass.Float32

    @cute.struct
    class StorageA:
        mbarA : cute.struct.MemRange[cutlass.Int64, stage]
        compA : complex
        intA : cutlass.Int16

    # Supports alignment for its elements:
    @cute.struct
    class StorageB:
        a: cute.struct.Align[
            cute.struct.MemRange[cutlass.Float32, size_a], 1024
        ]
        b: cute.struct.Align[
            cute.struct.MemRange[cutlass.Float32, size_b], 1024
        ]
        x: cute.struct.Align[cutlass.Int32, 16]
        compA: cute.struct.Align[complex, 16]

    # Statically get size and alignment:
    size = StorageB.__sizeof__()
    align = StorageB.__alignof__()

    # Allocate and referencing elements:
    storage = allocator.allocate(StorageB)

    storage.a[0] ...
    storage.x.ptr ...
    storage.compA.real.ptr ...

#### Parameters

**cls** -- The struct class with annotations.

#### Returns

The decorated struct class.

#### \_MemRangeMeta

**Kind:** class

**Qualified name:** `cutlass.cute.struct._MemRangeMeta`

**Signature:** `_MemRangeMeta(name: str, bases: tuple[type, ...], dct: Dict[str, Any])`

Bases: `type`

A metaclass for creating MemRange classes.

This metaclass is used to dynamically create MemRange classes with specific data types and sizes.

##### Variables

- **\_dtype** -- The data type of the MemRange.

- **\_size** -- The size of the MemRange.

##### \_dtype

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct._MemRangeMeta._dtype`

**Signature:** `_dtype: Type[cutlass.cute.typing.Numeric] | None = None`

##### \_size

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct._MemRangeMeta._size`

**Signature:** `_size: int | None = None`

##### size

**Kind:** property

**Qualified name:** `cutlass.cute.struct._MemRangeMeta.size`

**Signature:** `size: int | None`

##### elem_width

**Kind:** property

**Qualified name:** `cutlass.cute.struct._MemRangeMeta.elem_width`

**Signature:** `elem_width: int`

##### size_in_bytes

**Kind:** property

**Qualified name:** `cutlass.cute.struct._MemRangeMeta.size_in_bytes`

**Signature:** `size_in_bytes: int`

#### MemRange

**Kind:** class

**Qualified name:** `cutlass.cute.struct.MemRange`

**Signature:** `MemRange`

Bases: `object`

Defines a range of memory by MemRange\[T, size\].

##### \_dtype

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct.MemRange._dtype`

**Signature:** `_dtype: Type[cutlass.cute.typing.Numeric] | None = None`

##### \_size

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct.MemRange._size`

**Signature:** `_size: int | None = None`

#### \_MemRangeData

**Kind:** class

**Qualified name:** `cutlass.cute.struct._MemRangeData`

**Signature:** `_MemRangeData(dtype: Type[cutlass.cute.typing.Numeric] | None, size: int | None, base: cutlass.cute.typing.Pointer | None)`

Bases: `object`

Represents a range of memory.

##### Parameters

- **dtype** -- The data type.

- **size** -- The size of the memory range in bytes.

- **base** -- The base address of the memory range.

##### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.struct._MemRangeData.__init__`

**Signature:** `__init__(dtype: Type[cutlass.cute.typing.Numeric] | None, size: int | None, base: cutlass.cute.typing.Pointer | None) → None`

Initializes a new memory range.

###### Parameters

- **dtype** -- The data type.

- **size** -- Size of the memory range in bytes. A size of **0** is accepted, but in that case the range can only be used for its address (e.g. as a partition marker).

- **base** -- The base address of the memory range.

##### data_ptr

**Kind:** method

**Qualified name:** `cutlass.cute.struct._MemRangeData.data_ptr`

**Signature:** `data_ptr() → cutlass.cute.typing.Pointer`

Returns start pointer to the data in this memory range.

###### Returns

A pointer to the start of the memory range.

###### Raises

**AssertionError** -- If the size of the memory range is negative.

##### get_tensor

**Kind:** method

**Qualified name:** `cutlass.cute.struct._MemRangeData.get_tensor`

**Signature:** `get_tensor(layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, swizzle: Swizzle | None = None, dtype: Type[cutlass.cute.typing.Numeric] | None = None) → cutlass.cute.typing.Tensor`

Creates a tensor from the memory range.

###### Parameters

- **layout** -- The layout of the tensor.

- **swizzle** -- Optional swizzle pattern.

- **dtype** -- Optional data type; defaults to the memory range's data type if not specified.

###### Returns

A tensor representing the memory range.

###### Raises

- **TypeError** -- If the layout is incompatible with the swizzle.

- **AssertionError** -- If the size of the memory range is not greater than zero.

#### \_AlignMeta

**Kind:** class

**Qualified name:** `cutlass.cute.struct._AlignMeta`

**Signature:** `_AlignMeta(name: str, bases: tuple[type, ...], dct: Dict[str, Any])`

Bases: `type`

Aligns the given object by setting its alignment attribute.

##### Parameters

- **v** -- The object to align. Must be a struct, MemRange, or a scalar type.

- **align** -- The alignment value to set.

##### Raises

**TypeError** -- If the object is not a struct, MemRange, or a scalar type.

##### Variables

- **\_dtype** -- The data type to be aligned.

- **\_align** -- The alignment of the data type.

##### \_dtype

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct._AlignMeta._dtype`

**Signature:** `_dtype: Any | None = None`

##### \_align

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct._AlignMeta._align`

**Signature:** `_align: int | None = None`

##### dtype

**Kind:** property

**Qualified name:** `cutlass.cute.struct._AlignMeta.dtype`

**Signature:** `dtype: Any | None`

##### align

**Kind:** property

**Qualified name:** `cutlass.cute.struct._AlignMeta.align`

**Signature:** `align: int | None`

#### Align

**Kind:** class

**Qualified name:** `cutlass.cute.struct.Align`

**Signature:** `Align`

Bases: `object`

Aligns the given type by Align\[T, alignment\].

##### \_dtype

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct.Align._dtype`

**Signature:** `_dtype: Any | None = None`

##### \_align

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct.Align._align`

**Signature:** `_align: int | None = None`

#### \_ScalarData

**Kind:** class

**Qualified name:** `cutlass.cute.struct._ScalarData`

**Signature:** `_ScalarData(*args: Any, **kwargs: Any)`

Bases: `_Pointer`

Represents a scalar value at a given pointer location in memory.

This class provides utility methods to get a scalar pointer. It wraps a pointer to a scalar element and enables element-wise memory operations.

##### Variables

**\_ptr** -- The underlying pointer to the scalar value.

##### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.struct._ScalarData.__init__`

**Signature:** `__init__(ptr: _Pointer) → None`

##### to_llvm_ptr

**Kind:** method

**Qualified name:** `cutlass.cute.struct._ScalarData.to_llvm_ptr`

**Signature:** `to_llvm_ptr() → ir.Value`

Get the LLVM pointer representation of this pointer. (Used by internal API to propagate loc and ip)

###### Parameters

- **loc** (*Optional\[Location\]*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for MLIR, defaults to None

###### Returns

The LLVM pointer representation

###### Return type

ir.Value

##### ptr

**Kind:** property

**Qualified name:** `cutlass.cute.struct._ScalarData.ptr`

**Signature:** `ptr: cutlass.cute.typing.Pointer`

Get the underlying pointer.

###### Returns

The pointer to the scalar value.

###### Return type

[Pointer](../basic_data_types.md#cutlass.Pointer)

##### dtype

**Kind:** property

**Qualified name:** `cutlass.cute.struct._ScalarData.dtype`

**Signature:** `dtype: Type[cutlass.cute.typing.Numeric]`

Get the data type of the scalar value.

###### Returns

The numeric data type of the underlying pointer.

###### Return type

Type\[[Numeric](../basic_data_types.md#cutlass.Numeric)\]

##### value

**Kind:** property

**Qualified name:** `cutlass.cute.struct._ScalarData.value`

**Signature:** `value: Value`

Get the raw MLIR value of the underlying pointer.

Deprecated since version Using: `struct.scalar` as pointer is deprecated. Use explicit `struct.scalar.ptr` for pointer instead.

###### Returns

The MLIR value of the underlying pointer.

###### Return type

ir.Value

#### \_is_scalar_type

**Kind:** method

**Qualified name:** `cutlass.cute.struct._is_scalar_type`

**Signature:** `static _is_scalar_type(dtype: Any) → bool`

Checks if the given type is a scalar numeric type.

##### Parameters

**dtype** -- The type to check.

##### Returns

True if the type is a subclass of Numeric, False otherwise.

#### \_install_dynamic_expression_protocol

**Kind:** method

**Qualified name:** `cutlass.cute.struct._install_dynamic_expression_protocol`

**Signature:** `static _install_dynamic_expression_protocol(cls: type, decorator: Any) → None`

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.struct.__init__`

**Signature:** `__init__(cls: type) → None`

Initializes a new struct decorator instance.

##### Parameters

**cls** -- The class representing the structured data type.

##### Raises

**TypeError** -- If the struct is empty.

#### size_in_bytes

**Kind:** method

**Qualified name:** `cutlass.cute.struct.size_in_bytes`

**Signature:** `size_in_bytes() → int`

Returns the size of the struct in bytes.

##### Returns

The size of the struct.

#### align_offset

**Kind:** method

**Qualified name:** `cutlass.cute.struct.align_offset`

**Signature:** `static align_offset(offset: Any, align: int) → Any`

Return the round-up offset up to the next multiple of align.

### E

**Kind:** function

**Qualified name:** `cutlass.cute.E`

**Signature:** `E(mode: int | List[int]) → ScaledBasis | int`

Create a unit ScaledBasis element with the specified mode.

This function creates a ScaledBasis with value 1 and the given mode. The mode represents the coordinate axis or dimension in the layout.

#### Parameters

**mode** (*Union\[int,* *List\[int\]\]*) -- The mode (dimension) for the basis element, either a single integer or a list of integers

#### Returns

A ScaledBasis with value 1 and the specified mode

#### Return type

[ScaledBasis](#cutlass.cute.ScaledBasis)

#### Raises

**TypeError** -- If mode is not an integer or a list

**Examples:**

    # Create a basis element for the first dimension (mode 0)
    e0 = E(0)

    # Create a basis element for the second dimension (mode 1)
    e1 = E(1)

    # Create a basis element for a hierarchical dimension
    e_hier = E([0, 1])

### get_divisibility

**Kind:** function

**Qualified name:** `cutlass.cute.get_divisibility`

**Signature:** `get_divisibility(x: cutlass.cute.typing.Int) → int`

### is_static

**Kind:** function

**Qualified name:** `cutlass.cute.is_static`

**Signature:** `is_static(x: object) → bool`

Check if a value is statically known at compile time.

In CuTe, static values are those whose values are known at compile time, as opposed to dynamic values which are only known at runtime.

This function checks if a value is static by recursively traversing its type hierarchy and checking if all components are static.

Static values include:

- Python literals (bool, int, float, None)

- Static ScaledBasis objects

- Static ComposedLayout objects

- Static IR types

- Tuples containing only static values

Dynamic values include:

- Numeric objects (representing runtime values)

- Dynamic expressions

- Any tuple containing dynamic values

#### Parameters

**x** (*Any*) -- The value to check

#### Returns

True if the value is static, False otherwise

#### Return type

bool

#### Raises

**TypeError** -- If an unsupported type is provided

### has_underscore

**Kind:** function

**Qualified name:** `cutlass.cute.has_underscore`

**Signature:** `has_underscore(a: cutlass.cute.typing.XTuple) → bool`

### pretty_str

**Kind:** function

**Qualified name:** `cutlass.cute.pretty_str`

**Signature:** `pretty_str(arg: object) → str`

Constructs a concise readable pretty string.

### printf

**Kind:** function

**Qualified name:** `cutlass.cute.printf`

**Signature:** `printf(*args: Any, end: str = '\n') → None`

Print one or more values with optional formatting.

This function provides printf-style formatted printing capabilities. It can print values directly or format them using C-style format strings. The function supports printing various types including layouts, numeric values, tensors, pointers, and other CuTe objects.

The function accepts either: 1. A list of values to print directly 2. A format string followed by values to format

#### Parameters

- **args** (*Any*) -- Variable length argument list containing either: - One or more values to print directly - A format string followed by values to format

- **loc** (*Optional\[Location\]*) -- Source location information for debugging, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for code generation, defaults to None

- **end** (*Optional\[str\]*) -- Suffix for the printed value, defaults to newline

#### Raises

- **ValueError** -- If no arguments are provided

- **TypeError** -- If an unsupported argument type is passed

**Examples:**

Direct printing of values:

    a = cute.make_layout(shape=(10, 10), stride=(10, 1))
    b = cutlass.Float32(1.234)
    cute.printf(a, b)  # Prints values directly

Formatted printing:

    # Using format string with generic format specifiers
    cute.printf("a={}, b={}", a, b)

    # Using format string with C-style format specifiers
    cute.printf("a={}, b=%.2f", a, b)

### front

**Kind:** function

**Qualified name:** `cutlass.cute.front`

**Signature:** `front(input: Any) → Any`

Recursively get the first element of input.

This function traverses a hierarchical structure (like a layout or tensor) and returns the first element at the deepest level. It's particularly useful for accessing the first stride value in a layout to determine properties like majorness.

#### Parameters

- **input** (*Union\[Tensor,* *Layout,* *Stride\]*) -- The hierarchical structure to traverse

- **loc** (*source location,* *optional*) -- Source location where it's called, defaults to None

- **ip** (*insertion pointer,* *optional*) -- Insertion pointer for IR generation, defaults to None

#### Returns

The first element at the deepest level of the input structure

#### Return type

Union\[int, float, bool, ir.Value\]

### is_major

**Kind:** function

**Qualified name:** `cutlass.cute.is_major`

**Signature:** `is_major(mode: int | List[int], stride: cutlass.cute.typing.Stride) → bool`

Check whether a mode has a statically-known unit stride.

Dynamic stride expressions are not considered major, even if their runtime value could be 1. This keeps the result a meta-level Python boolean that is safe to use while tracing a JIT function.

### assume

**Kind:** function

**Qualified name:** `cutlass.cute.assume`

**Signature:** `assume(src: Any, divby: int | None = None, pow2: bool | None = None) → Any`

### make_swizzle

**Kind:** function

**Qualified name:** `cutlass.cute.make_swizzle`

**Signature:** `make_swizzle(b: int, m: int, s: int) → Swizzle`

### static

**Kind:** function

**Qualified name:** `cutlass.cute.static`

**Signature:** `static(value: Any) → Any`

### get_leaves

**Kind:** function

**Qualified name:** `cutlass.cute.get_leaves`

**Signature:** `get_leaves(value: Any) → Any`

### depth

**Kind:** function

**Qualified name:** `cutlass.cute.depth`

**Signature:** `depth(a: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout) → int`

Returns the depth (nesting level) of a tuple, layout, or tensor.

The depth of a tuple is the maximum depth of its elements plus 1. For an empty tuple, the depth is 1. For layouts and tensors, the depth is determined by the depth of their shape. For non-tuple values (e.g., integers), the depth is considered 0.

#### Parameters

**a** (*Union\[XTuple,* *Layout,* *ComposedLayout,* *Tensor,* *Any\]*) -- The object whose depth is to be determined

#### Returns

The depth of the input object

#### Return type

int

**Example:**

    depth(1)                # 0
    depth((1, 2))           # 1
    depth(((1, 2), (3, 4))) # 2

### is_congruent

**Kind:** function

**Qualified name:** `cutlass.cute.is_congruent`

**Signature:** `is_congruent(a: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor, b: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor) → bool`

Returns whether a is congruent to b.

Congruence is an equivalence relation between hierarchical structures.

Two objects are congruent if:

- They have the same rank, AND

- They are both non-tuple values, OR

- They are both tuples AND all corresponding elements are congruent.

Congruence requires type matching at each level -- scalar values match with scalar values, and tuples match with tuples of the same rank.

#### Parameters

- **a** (*Union\[XTuple,* *Layout,* *ComposedLayout,* *Tensor\]*) -- First object to compare

- **b** (*Union\[XTuple,* *Layout,* *ComposedLayout,* *Tensor\]*) -- Second object to compare

#### Returns

True if a and b are congruent, False otherwise

#### Return type

bool

### is_weakly_congruent

**Kind:** function

**Qualified name:** `cutlass.cute.is_weakly_congruent`

**Signature:** `is_weakly_congruent(a: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor, b: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor) → bool`

Returns whether a is weakly congruent to b.

Weak congruence is a partial order on hierarchical structures.

Object X is weakly congruent to object Y if:

- X is a non-tuple value, OR

- X and Y are both tuples of the same rank AND all corresponding elements are weakly congruent.

Weak congruence allows scalar values to match with tuples, making it useful for determining whether an object has a hierarchical structure "up to" another.

#### Parameters

- **a** (*Union\[XTuple,* *Layout,* *ComposedLayout,* *Tensor\]*) -- First object to compare

- **b** (*Union\[XTuple,* *Layout,* *ComposedLayout,* *Tensor\]*) -- Second object to compare

#### Returns

True if a and b are weakly congruent, False otherwise

#### Return type

bool

### group_modes

**Kind:** function

**Qualified name:** `cutlass.cute.group_modes`

**Signature:** `group_modes(input: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor | cutlass.cute.typing.XTuple, begin: int, end: int | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor | cutlass.cute.typing.XTuple`

Group modes of a hierarchical tuple or layout into a single mode.

This function groups a range of modes from the input object into a single mode, creating a hierarchical structure. For tuples, it creates a nested tuple containing the specified range of elements. For layouts and other CuTe objects, it creates a hierarchical representation where the specified modes are grouped together.

#### Parameters

- **input** (*Layout,* *ComposedLayout,* *tuple,* *Shape,* *Stride,* *etc.*) -- Input object to group modes from (layout, tuple, etc.)

- **beg** (*int*) -- Beginning index of the range to group (inclusive)

- **end** (*int*) -- Ending index of the range to group (exclusive)

- **loc** (*optional*) -- Source location for MLIR, defaults to None

- **ip** (*optional*) -- Insertion point, defaults to None

#### Returns

A new object with the specified modes grouped

#### Return type

Same type as input with modified structure

**Examples:**

    # Group modes in a tuple
    t = (2, 3, 4, 5)
    grouped = group_modes(t, 1, 3)  # (2, (3, 4), 5)

    # Group modes in a layout
    layout = make_layout((2, 3, 4, 5))
    grouped_layout = group_modes(layout, 1, 3)  # Layout with shape (2, (3, 4), 5)

    # Group modes in a shape
    shape = make_shape(2, 3, 4, 5)
    grouped_shape = group_modes(shape, 0, 2)  # Shape ((2, 3), 4, 5)

### slice\_

**Kind:** function

**Qualified name:** `cutlass.cute.slice_`

**Signature:** `slice_(src: cutlass.cute.typing.Layout | _ComposedLayout | cutlass.cute.typing.Tensor | cutlass.cute.typing.XTuple, coord: cutlass.cute.typing.Coord) → cutlass.cute.typing.Layout | _ComposedLayout | cutlass.cute.typing.Tensor | cutlass.cute.typing.XTuple`

Perform a slice operation on a source object using the given coordinate.

This function implements CuTe's slicing operation which extracts a subset of elements from a source object (tensor, layout, etc.) based on a coordinate pattern. The slice operation preserves the structure of the source while selecting specific elements.

#### Parameters

- **src** (*Union\[Tensor,* *Layout,* *IntTuple,* *Value\]*) -- Source object to be sliced (tensor, layout, tuple, etc.)

- **coord** (*Coord*) -- Coordinate pattern specifying which elements to select

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

A new object containing the sliced elements

#### Return type

Union\[Tensor, Layout, IntTuple, tuple\]

#### Raises

**ValueError** -- If the coordinate pattern is incompatible with source

**Examples:**

    # Layout slicing
    layout = make_layout((4,4))

    # Select 1st index of first mode and keep all elements in second mode
    sub_layout = slice_(layout, (1, None))

    # Basic tensor slicing
    tensor = make_tensor(...)           # Create a 2D tensor

    # Select 1st index of first mode and keep all elements in second mode
    sliced = slice_(tensor, (1, None))

    # Select 2nd index of second mode and keep all elements in first mode
    sliced = slice_(tensor, (None, 2))

> **Note.**
>
> - None represents keeping all elements in that mode
>
> - Slicing preserves the layout/structure of the original object
>
> - Can be used for: \* Extracting sub-tensors/sub-layouts \* Creating views into data \* Selecting specific patterns of elements

### dice

**Kind:** function

**Qualified name:** `cutlass.cute.dice`

**Signature:** `dice(src: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.XTuple, dicer: cutlass.cute.typing.Coord) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.XTuple`

Keep modes in input when it is paired with an integer in dicer.

This function performs dicing operation on the input based on the dicer coordinate. Dicing is a fundamental operation in CuTe that allows selecting specific modes from a tensor or layout based on a coordinate pattern.

#### Parameters

- **dicer** (*Coord*) -- A static coordinate indicating how to dice the input

- **input** (*Union\[IntTuple,* *Shape,* *Stride,* *Coord,* *Layout,* *ComposedLayout\]*) -- The operand to be diced on

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

The diced result with selected modes from the input

#### Return type

Union\[IntTuple, Shape, Stride, Coord, Layout, ComposedLayout\]

#### Raises

- **TypeError** -- If dicer has an unsupported type

- **ValueError** -- If input is not provided

**Examples:**

    # Basic dicing of a layout
    layout = make_layout((32,16,8))

    # Keep only first and last modes
    diced = dice((1,None,1), layout)

> **Note.**
>
> - The dicer coordinate must be static
>
> - Use underscore (\_) to remove a mode

### prepend

**Kind:** function

**Qualified name:** `cutlass.cute.prepend`

**Signature:** `prepend(input: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.XTuple, elem: Any, up_to_rank: int | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.XTuple`

Extend input to rank up_to_rank by prepending elem in front of input.

This function extends the input object by prepending elements to reach a desired rank. It supports various CuTe types including shapes, layouts, tensors etc.

#### Parameters

- **input** (*Union\[Shape,* *Stride,* *Coord,* *IntTuple,* *Tile,* *Layout,* *ComposedLayout,* *Tensor\]*) -- Source to be prepended to

- **elem** (*Union\[Shape,* *Stride,* *Coord,* *IntTuple,* *Tile,* *Layout\]*) -- Element to prepend to input

- **up_to_rank** (*Union\[None,* *int\],* *optional*) -- The target rank after extension, defaults to None

- **loc** (*Optional\[Location\]*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point, defaults to None

#### Returns

The extended result with prepended elements

#### Return type

Union\[Shape, Stride, Coord, IntTuple, Tile, Layout, ComposedLayout, Tensor\]

#### Raises

- **ValueError** -- If up_to_rank is less than input's current rank

- **TypeError** -- If input or elem has unsupported type

**Examples:**

    # Prepend to a Shape
    shape = (4,4)
    prepend(shape, 2)                   # Returns (2,4,4)

    # Prepend to a Layout
    layout = make_layout((8,8))
    prepend(layout, make_layout((2,)))  # Returns (2,8,8):(1,1,8)

    # Prepend with target rank
    coord = (1,1)
    prepend(coord, 0, up_to_rank=4)     # Returns (0,0,1,1)

### append

**Kind:** function

**Qualified name:** `cutlass.cute.append`

**Signature:** `append(input: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.XTuple, elem: Any, up_to_rank: int | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.XTuple`

Extend input to rank up_to_rank by appending elem to the end of input.

This function extends the input object by appending elements to reach a desired rank. It supports various CuTe types including shapes, layouts, tensors etc.

#### Parameters

- **input** (*Union\[Shape,* *Stride,* *Coord,* *IntTuple,* *Tile,* *Layout,* *ComposedLayout,* *Tensor\]*) -- Source to be appended to

- **elem** (*Union\[Shape,* *Stride,* *Coord,* *IntTuple,* *Tile,* *Layout\]*) -- Element to append to input

- **up_to_rank** (*Union\[None,* *int\],* *optional*) -- The target rank after extension, defaults to None

- **loc** (*Optional\[Location\]*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point, defaults to None

#### Returns

The extended result with appended elements

#### Return type

Union\[Shape, Stride, Coord, IntTuple, Tile, Layout, ComposedLayout, Tensor\]

#### Raises

- **ValueError** -- If up_to_rank is less than input's current rank

- **TypeError** -- If input or elem has unsupported type

**Examples:**

    # Append to a Shape
    shape = (4,4)
    append(shape, 2)                   # Returns (4,4,2)

    # Append to a Layout
    layout = make_layout((8,8))
    append(layout, make_layout((2,)))  # Returns (8,8,2):(1,8,1)

    # Append with target rank
    coord = (1,1)
    append(coord, 0, up_to_rank=4)     # Returns (1,1,0,0)

> **Note.**
>
> - The function preserves the structure of the input while extending it
>
> - Can be used to extend tensors, layouts, shapes and other CuTe types
>
> - When up_to_rank is specified, fills remaining positions with elem
>
> - Useful for tensor reshaping and layout transformations

### prepend_ones

**Kind:** function

**Qualified name:** `cutlass.cute.prepend_ones`

**Signature:** `prepend_ones(t: cutlass.cute.typing.Tensor, up_to_rank: int | None = None) → cutlass.cute.typing.Tensor`

### append_ones

**Kind:** function

**Qualified name:** `cutlass.cute.append_ones`

**Signature:** `append_ones(t: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, up_to_rank: int | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

### repeat_as_tuple

**Kind:** function

**Qualified name:** `cutlass.cute.repeat_as_tuple`

**Signature:** `repeat_as_tuple(x: Any, n: int) → tuple`

Creates a tuple with x repeated n times.

This function creates a tuple by repeating the input value x n times.

#### Parameters

- **x** (*Any*) -- The value to repeat

- **n** (*int*) -- Number of times to repeat x

#### Returns

A tuple containing x repeated n times

#### Return type

tuple

**Examples:**

    repeat_as_tuple(1, 1)     # Returns (1,)
    repeat_as_tuple(1, 3)     # Returns (1, 1, 1)
    repeat_as_tuple(None, 4)  # Returns (None, None, None, None)

### repeat

**Kind:** function

**Qualified name:** `cutlass.cute.repeat`

**Signature:** `repeat(x: Any, n: int) → Any`

Creates an object by repeating x n times.

This function creates an object by repeating the input value x n times. If n=1, returns x directly, otherwise returns a tuple of x repeated n times.

#### Parameters

- **x** (*Any*) -- The value to repeat

- **n** (*int*) -- Number of times to repeat x

#### Returns

x if n=1, otherwise a tuple containing x repeated n times

#### Return type

Union\[Any, tuple\]

#### Raises

**ValueError** -- If n is less than 1

**Examples:**

    repeat(1, 1)     # Returns 1
    repeat(1, 3)     # Returns (1, 1, 1)
    repeat(None, 4)  # Returns (None, None, None, None)

### repeat_like

**Kind:** function

**Qualified name:** `cutlass.cute.repeat_like`

**Signature:** `repeat_like(x: Any, target: Any) → Any`

Creates an object congruent to target and filled with x.

This function recursively creates a nested tuple structure that matches the structure of the target, with each leaf node filled with the value x.

#### Parameters

- **x** (*Any*) -- The value to fill the resulting structure with

- **target** (*Union\[tuple,* *Any\]*) -- The structure to mimic

#### Returns

A structure matching target but filled with x

#### Return type

Union\[tuple, Any\]

**Examples:**

    repeat_like(0, (1, 2, 3))      # Returns (0, 0, 0)
    repeat_like(1, ((1, 2), 3))    # Returns ((1, 1), 1)
    repeat_like(2, 5)              # Returns 2

### flatten

**Kind:** function

**Qualified name:** `cutlass.cute.flatten`

**Signature:** `flatten(a: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor | cutlass.cute.typing.XTuple) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor | cutlass.cute.typing.XTuple`

Flattens a CuTe data structure into a simpler form.

For tuples, this function flattens the structure into a single-level tuple. For layouts, it returns a new layout with flattened shape and stride. For tensors, it returns a new tensor with flattened layout. For other types, it returns the input unchanged.

#### Parameters

**a** (*Union\[IntTuple,* *Coord,* *Shape,* *Stride,* *Layout,* *Tensor\]*) -- The structure to flatten

#### Returns

The flattened structure

#### Return type

Union\[tuple, Any\]

**Examples:**

    flatten((1, 2, 3))                      # Returns (1, 2, 3)
    flatten(((1, 2), (3, 4)))               # Returns (1, 2, 3, 4)
    flatten(5)                              # Returns 5
    flatten(Layout(shape, stride))          # Returns Layout(flatten(shape), flatten(stride))
    flatten(Tensor(layout))                 # Returns Tensor(flatten(layout))

### filter_zeros

**Kind:** function

**Qualified name:** `cutlass.cute.filter_zeros`

**Signature:** `filter_zeros(input: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, *, target_profile: cutlass.cute.typing.Stride | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

Filter out zeros from a layout or tensor.

This function removes zero-stride dimensions from a layout or tensor. Refer to [NVIDIA/cutlass](https://github.com/NVIDIA/cutlass/blob/main/media/docs/cpp/cute/02_layout_algebra.md) for more layout algebra operations.

#### Parameters

- **input** (*Layout* *or* *Tensor*) -- The input layout or tensor to filter

- **target_profile** (*Stride,* *optional*) -- Target stride profile for the filtered result, defaults to None

- **loc** (*optional*) -- Source location for MLIR, defaults to None

- **ip** (*optional*) -- Insertion point, defaults to None

#### Returns

The filtered layout or tensor with zeros removed

#### Return type

Layout or Tensor

#### Raises

**TypeError** -- If input is not a Layout or Tensor

### filter

**Kind:** function

**Qualified name:** `cutlass.cute.filter`

**Signature:** `filter(input: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor`

Filter a layout or tensor.

This function filters a layout or tensor according to CuTe's filtering rules.

#### Parameters

- **input** (*Layout* *or* *Tensor*) -- The input layout or tensor to filter

- **loc** (*optional*) -- Source location for MLIR, defaults to None

- **ip** (*optional*) -- Insertion point, defaults to None

#### Returns

The filtered layout or tensor

#### Return type

Layout or Tensor

#### Raises

**TypeError** -- If input is not a Layout or Tensor

### shape_div

**Kind:** function

**Qualified name:** `cutlass.cute.shape_div`

**Signature:** `shape_div(lhs: cutlass.cute.typing.Shape, rhs: cutlass.cute.typing.Shape) → cutlass.cute.typing.Shape`

Perform element-wise division of shapes.

This function performs element-wise division between two shapes.

#### Parameters

- **lhs** (*Shape*) -- Left-hand side shape

- **rhs** (*Shape*) -- Right-hand side shape

- **loc** (*optional*) -- Source location for MLIR, defaults to None

- **ip** (*optional*) -- Insertion point, defaults to None

#### Returns

The result of element-wise division

#### Return type

Shape

### ceil_div

**Kind:** function

**Qualified name:** `cutlass.cute.ceil_div`

**Signature:** `ceil_div(input: cutlass.cute.typing.Shape, tiler: cutlass.cute.typing.Tiler) → cutlass.cute.typing.Shape`

Compute the ceiling division of a target shape by a tiling specification.

This function computes the number of tiles required to cover the target domain. It is equivalent to the second mode of zipped_divide(input, tiler).

#### Parameters

- **input** (*Shape*) -- A tuple of integers representing the dimensions of the target domain.

- **tiler** (*Union\[Layout,* *Shape,* *Tile\]*) -- The tiling specification.

- **loc** (*optional*) -- Optional location information for IR diagnostics.

- **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions.

#### Returns

A tuple of integers representing the number of tiles required along each dimension, i.e. the result of the ceiling division of the input dimensions by the tiler dimensions.

#### Return type

Shape

Example:

    import cutlass.cute as cute
    @cute.jit
    def foo():
        input = (10, 6)
        tiler = (3, 4)
        result = cute.ceil_div(input, tiler)
        print(result)  # Outputs: (4, 2)

### round_up

**Kind:** function

**Qualified name:** `cutlass.cute.round_up`

**Signature:** `round_up(a: cutlass.cute.typing.IntTuple, b: cutlass.cute.typing.IntTuple) → cutlass.cute.typing.IntTuple`

Rounds up elements of a using elements of b.

### make_layout

**Kind:** function

**Qualified name:** `cutlass.cute.make_layout`

**Signature:** `make_layout(shape: cutlass.cute.typing.Shape | Iterable[cutlass.cute.typing.Layout], *, stride: cutlass.cute.typing.Stride | None = None) → cutlass.cute.typing.Layout`

Create a CuTe Layout object from shape and optional stride information.

A Layout in CuTe represents the mapping between logical and physical coordinates of a tensor. This function creates a Layout object that defines how tensor elements are arranged in memory.

As an alternative to a shape, an iterable of `Layout` objects may be passed, in which case each layout becomes a separate mode of the result (the `stride` argument is ignored). This mirrors CuTe's variadic `make_layout(layoutA, layoutB, ...)`.

#### Parameters

- **shape** (*Union\[Shape,* *Iterable\[Layout\]\]*) -- Shape of the layout defining the size of each mode, or an iterable of Layout objects to concatenate (each becomes a mode)

- **stride** (*Union\[Stride,* *None\]*) -- Optional stride values for each mode, defaults to None (ignored when shape is an iterable of layouts)

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

A new Layout object with the specified shape and stride

#### Return type

Layout

**Examples:**

    # Create a 2D compact left-most layout with shape (4,4)
    layout = make_layout((4,4))                     # compact left-most layout

    # Create a left-most layout with custom strides
    layout = make_layout((4,4), stride=(1,4))       # left-most layout with strides (1,4)

    # Create a layout for a 3D tensor
    layout = make_layout((32,16,8))                 # left-most layout

    # Create a layout with custom strides
    layout = make_layout((2,2,2), stride=(4,1,2))   # layout with strides (4,1,2)

    # Concatenate layouts: each becomes a mode of the result
    mode0 = make_layout(64, stride=1)
    mode1 = make_layout(128, stride=64)
    combined = make_layout([mode0, mode1])          # (64,128):(1,64)

> **Note.**
>
> - If stride is not provided, a default compact left-most stride is computed based on the shape
>
> - The resulting layout maps logical coordinates to physical memory locations
>
> - The layout object can be used for tensor creation and memory access patterns
>
> - Strides can be used to implement: \* Row-major vs column-major layouts \* Padding and alignment \* Blocked/tiled memory arrangements \* Interleaved data formats
>
> - Stride is keyword only argument to improve readability, e.g. \* make_layout((3,4), (1,4)) can be confusing with make_layout(((3,4), (1,4))) \* make_layout((3,4), stride=(1,4)) is more readable
>
> - When passing an iterable of layouts, each layout becomes a separate mode

### make_identity_layout

**Kind:** function

**Qualified name:** `cutlass.cute.make_identity_layout`

**Signature:** `make_identity_layout(shape: cutlass.cute.typing.Shape) → cutlass.cute.typing.Layout`

Create an identity layout with the given shape.

An identity layout maps logical coordinates directly to themselves without any transformation. This is equivalent to a layout with stride (1@0,1@1,...,1@(N-1)).

#### Parameters

- **shape** (*Shape*) -- The shape of the layout

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

A new identity Layout object with the specified shape

#### Return type

Layout

**Examples:**

    # Create a 2D identity layout with shape (4,4)
    layout = make_identity_layout((4,4))     # stride=(1@0,1@1)

    # Create a 3D identity layout
    layout = make_identity_layout((32,16,8)) # stride=(1@0,1@1,1@2)

> **Note.**
>
> - An identity layout is a special case where each coordinate maps to itself
>
> - Useful for direct coordinate mapping without any transformation

### make_ordered_layout

**Kind:** function

**Qualified name:** `cutlass.cute.make_ordered_layout`

**Signature:** `make_ordered_layout(shape: cutlass.cute.typing.Shape, order: cutlass.cute.typing.Shape) → cutlass.cute.typing.Layout`

Create a layout with a specific ordering of dimensions.

This function creates a layout where the dimensions are ordered according to the specified order parameter, allowing for custom dimension ordering in the layout.

#### Parameters

- **shape** (*Shape*) -- The shape of the layout

- **order** (*Shape*) -- The ordering of dimensions

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

A new Layout object with the specified shape and dimension ordering

#### Return type

Layout

**Examples:**

    # Create a row-major layout
    layout = make_ordered_layout((4,4), order=(1,0))

    # Create a column-major layout
    layout = make_ordered_layout((4,4), order=(0,1))         # stride=(1,4)

    # Create a layout with custom dimension ordering for a 3D tensor
    layout = make_ordered_layout((32,16,8), order=(2,0,1))   # stride=(128,1,16)

> **Note.**
>
> - The order parameter specifies the ordering of dimensions from fastest-varying to slowest-varying
>
> - For a 2D tensor, (0,1) creates a column-major layout, while (1,0) creates a row-major layout
>
> - The length of order must match the rank of the shape

### make_layout_like

**Kind:** function

**Qualified name:** `cutlass.cute.make_layout_like`

**Signature:** `make_layout_like(input: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor) → cutlass.cute.typing.Layout`

### make_composed_layout

**Kind:** function

**Qualified name:** `cutlass.cute.make_composed_layout`

**Signature:** `make_composed_layout(inner: Any, offset: cutlass.cute.typing.IntTuple, outer: cutlass.cute.typing.Layout) → cutlass.cute.typing.ComposedLayout`

Create a composed layout by composing an inner transformation with an outer layout.

A composed layout applies a sequence of transformations to coordinates. The composition is defined as (inner ∘ offset ∘ outer), where the operations are applied from right to left.

#### Parameters

- **inner** (*Union\[Layout,* [*Swizzle*](#cutlass.cute.Swizzle)*\]*) -- The inner transformation (can be a Layout or Swizzle)

- **offset** (*IntTuple*) -- An integral offset applied between transformations

- **outer** (*Layout*) -- The outer (right-most) layout that is applied first

- **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

#### Returns

A new ComposedLayout representing the composition

#### Return type

ComposedLayout

**Examples:**

    # Create a basic layout
    inner = make_layout(...)
    outer = make_layout((4,4), stride=(E(0), E(1)))

    # Create a composed layout with an offset
    composed = make_composed_layout(inner, (2,0), outer)

> **Note.**
>
> - The composition applies transformations in the order: outer → offset → inner
>
> - The stride divisibility condition must be satisfied for valid composition
>
> - Certain compositions (like Swizzle with scaled basis) are invalid and will raise errors
>
> - Composed layouts inherit many properties from the outer layout

### size_in_bytes

**Kind:** function

**Qualified name:** `cutlass.cute.size_in_bytes`

**Signature:** `size_in_bytes(dtype: Type[cutlass.cute.typing.Numeric], layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | None) → cutlass.cute.typing.Int`

Calculate the size in bytes based on its data type and layout. The result is rounded up to the nearest byte.

Supports both regular Numeric types.

#### Parameters

- **dtype** (*Union\[Type\[Numeric\]\]*) -- The DSL numeric data type

- **layout** (*Layout, optional*) -- The layout of the elements. If None, the function returns 0

- **loc** (*optional*) -- Location information for diagnostics, defaults to None

- **ip** (*optional*) -- Instruction pointer for diagnostics, defaults to None

#### Returns

The total size in bytes. Returns 0 if the layout is None

#### Return type

int

### coalesce

**Kind:** function

**Qualified name:** `cutlass.cute.coalesce`

**Signature:** `coalesce(input: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor, *, target_profile: cutlass.cute.typing.Coord | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor`

### crd2idx

**Kind:** function

**Qualified name:** `cutlass.cute.crd2idx`

**Signature:** `crd2idx(coord: cutlass.cute.typing.Coord, layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | tuple | int) → cutlass.cute.typing.Int`

Convert a multi-dimensional coordinate into a value using the specified layout.

This function computes the inner product of the flattened coordinate and stride:

index = sum(flatten(coord)\[i\] \* flatten(stride)\[i\] for i in range(len(coord)))

#### Parameters

- **coord** (*Coord*) -- A tuple or list representing the multi-dimensional coordinate (e.g., (i, j) for a 2D layout).

- **layout** (*Layout* *or* *ComposedLayout*) -- A layout object that defines the memory storage layout, including shape and stride, used to compute the inner product.

- **loc** (*optional*) -- Optional location information for IR diagnostics.

- **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions.

#### Returns

The result of applying the layout transformation to the provided coordinate.

#### Return type

Any type that the layout maps to

**Example:**

    import cutlass.cute as cute
    @cute.jit
    def foo():
        L = cute.make_layout((5, 4), stride=(4, 1))
        idx = cute.crd2idx((2, 3), L)
        # Computed as: 2 * 4 + 3 = 11
        print(idx)
    foo()  # Expected output: 11

### idx2crd

**Kind:** function

**Qualified name:** `cutlass.cute.idx2crd`

**Signature:** `idx2crd(idx: cutlass.cute.typing.IntTuple, shape: cutlass.cute.typing.Shape) → cutlass.cute.typing.IntTuple`

Convert a linear index back into a nested coordinate using the specified layout.

Mapping from a linear index to the corresponding nested coordinate in the layout's coordinate space. It essentially "unfolds" a linear index into its constituent coordinate components.

#### Parameters

- **idx** (*: int/Integer/Tuple*) -- The linear index to convert back to coordinates.

- **shape** (*Shape*) -- Shape of the layout defining the size of each mode

- **loc** (*optional*) -- Optional location information for IR diagnostics.

- **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions.

#### Returns

The result of applying the layout transformation to the provided coordinate.

#### Return type

Coord

**Examples:**

    import cutlass.cute as cute
    @cute.jit
    def foo():
        coord = cute.idx2crd(11, (5, 4))
        # idx2crd is always lexicographical ordering (left-to-right)
        # For shape (m, n, l, ...), coord = (idx % m, idx // m % n, idx // m // n % l, ...
        # Computed as: (11 % 5, 11 // 5 % 4) = (1, 2)
        cute.printf("coord: {}", coord)

    foo()  # Expected output: (1, 2)

### increment_coord

**Kind:** function

**Qualified name:** `cutlass.cute.increment_coord`

**Signature:** `increment_coord(coord: cutlass.cute.typing.Coord, shape: cutlass.cute.typing.Shape) → cutlass.cute.typing.Coord`

Colexicographically increment a coordinate within a coordinate space defined by a shape.

Increments the leftmost mode first. When a mode reaches its shape limit, it wraps to 0 and carries to the next mode.

#### Parameters

- **coord** (*Coord*) -- The coordinate to increment.

- **shape** (*Shape*) -- The shape defining the coordinate space bounds.

- **loc** (*optional*) -- Optional location information for IR diagnostics.

- **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions.

#### Returns

The incremented coordinate.

#### Return type

Coord

#### Raises

**ValueError** -- If the coordinate and shape are not congruent or if the coordinate contains an underscore.

**Example:**

    import cutlass.cute as cute
    @cute.jit
    def foo():
        coord = cute.increment_coord((2, 0, 0), (3, 3, 3))
        # Increments colexicographically: (2,0,0) -> (0,1,0)
        cute.printf("coord: {}", coord)
    foo()  # Expected output: coord: (0, 1, 0)

### recast_layout

**Kind:** function

**Qualified name:** `cutlass.cute.recast_layout`

**Signature:** `recast_layout(new_type_bits: int, old_type_bits: int, src_layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

Recast a layout from one data type to another.

#### Parameters

- **new_type_bits** (*int*) -- The new data type bits

- **old_type_bits** (*int*) -- The old data type bits

- **src_layout** (*Union\[Layout,* *ComposedLayout\]*) -- The layout to recast

- **loc** (*optional*) -- Optional location information for IR diagnostics.

- **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions.

#### Returns

The recast layout

#### Return type

Layout or ComposedLayout

**Example:**

    import cutlass.cute as cute
    @cute.jit
    def foo():
        # Create a layout
        L = cute.make_layout((2, 3, 4))
        # Recast the layout to a different data type
        L_recast = cute.recast_layout(16, 8, L)
        print(L_recast)
    foo()  # Expected output: (2, 3, 4)

### slice_and_offset

**Kind:** function

**Qualified name:** `cutlass.cute.slice_and_offset`

**Signature:** `slice_and_offset(coord: cutlass.cute.typing.Coord, src: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout) → tuple`

### shape

**Kind:** function

**Qualified name:** `cutlass.cute.shape`

**Signature:** `shape(input: cutlass.cute.typing.Shape | cutlass.cute.typing.Tensor | cutlass.cute.typing.Layout | cutlass.cute.typing.Tile, *, mode: int | None = None) → cutlass.cute.typing.Shape`

Returns the shape of a tensor, layout or tiler.

For shapes, this function is identical to get.

This function extracts the shape information from the input object. For tensors and layouts, it returns their internal shape property. For tilers, it unpacks the shape from the tile representation.

#### Parameters

- **input** (*Union\[Tensor,* *Layout,* *Tile\]*) -- The object to extract shape from

- **mode** (*Optional\[int\]*) -- Optional mode selector to extract specific dimensions from the shape

- **loc** (*Optional\[Location\]*) -- Source location for MLIR operation tracking

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for MLIR operation

#### Returns

The shape of the input object, optionally filtered by mode

#### Return type

Shape

**Example:**

    # Get shape of a layout
    l0 = cute.make_layout((2, 3, 4))
    s0 = cute.shape(l0)  # => (2, 3, 4)

    # Get shape of a hierarchical tiler
    l1 = cute.make_layout(1)
    s1 = cute.shape((l0, l1))  # => ((2, 3, 4), 1)

    # Get specific mode from a shape
    s2 = cute.shape(l0, mode=0)  # => 2

### recast_ptr

**Kind:** function

**Qualified name:** `cutlass.cute.recast_ptr`

**Signature:** `recast_ptr(ptr: cutlass.cute.typing.Pointer, swizzle_: Swizzle | None = None, dtype: Type[cutlass.cute.typing.Numeric] | None = None) → cutlass.cute.typing.Pointer`

### make_ptr

**Kind:** function

**Qualified name:** `cutlass.cute.make_ptr`

**Signature:** `make_ptr(dtype: ~typing.Type[cutlass.cute.typing.Numeric] | _install_cutlass_mlir_autodoc_stub.<locals>._DocMlirType, value: int | cutlass.cute.typing.Integer | ir.Value, mem_space: cutlass.cute.typing.AddressSpace | None = None, *, assumed_align: int | None = None, swizzle_: ~cutlass.cute.core.Swizzle | None = None) → cutlass.cute.typing.Pointer`

### composition

**Kind:** function

**Qualified name:** `cutlass.cute.composition`

**Signature:** `composition(lhs: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor, rhs: cutlass.cute.typing.Layout | cutlass.cute.typing.Shape | cutlass.cute.typing.Tile) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor`

Compose two layout representations using the CuTe layout algebra.

Compose a left-hand layout (or tensor) with a right-hand operand into a new layout R, such that for every coordinate c in the domain of the right-hand operand, the composed layout satisfies:

R(c) = A(B(c))

where A is the left-hand operand provided as `lhs` and B is the right-hand operand provided as `rhs`. In this formulation, B defines the coordinate domain while A applies its transformation to B's output, and the resulting layout R inherits the stride and shape adjustments from A.

Satisfies:

:   cute.shape(cute.composition(lhs, rhs)) is compatible with cute.shape(rhs)

#### Parameters

- **lhs** (*Layout* *or* *Tensor*) -- The left-hand operand representing the transformation to be applied.

- **rhs** (*Layout,* *Shape, or* *Tile, or* *int* *or* *tuple*) -- The right-hand operand defining the coordinate domain. If provided as an int or tuple, it will be converted to a tile layout.

- **loc** (*optional*) -- Optional location information for IR diagnostics.

- **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions.

#### Returns

A new composed layout R, such that for all coordinates c in the domain of `rhs`, R(c) = lhs(rhs(c)).

#### Return type

Layout or Tensor

**Example:**

    import cutlass.cute as cute
    @cute.jit
    def foo():
        # Create a layout that maps (i,j) to i*4 + j
        L1 = cute.make_layout((2, 3), stride=(4, 1))
        # Create a layout that maps (i,j) to i*3 + j
        L2 = cute.make_layout((3, 4), stride=(3, 1))
        # Compose L1 and L2
        L3 = cute.composition(L1, L2)
        # L3 now maps coordinates through L2 then L1

### complement

**Kind:** function

**Qualified name:** `cutlass.cute.complement`

**Signature:** `complement(input: cutlass.cute.typing.Layout, cotarget: cutlass.cute.typing.Layout | cutlass.cute.typing.Shape) → cutlass.cute.typing.Layout`

Compute the complement layout of the input layout with respect to the cotarget.

The complement of a layout A with respect to cotarget n is a layout A\* such that for every k in Z_n and c in the domain of A, there exists a unique c\* in the domain of A\* where k = A(c) + A\*(c\*).

This operation is useful for creating layouts that partition a space in complementary ways, such as row and column layouts that together cover a matrix.

#### Parameters

- **input** (*Layout*) -- The layout to compute the complement of

- **cotarget** (*Union\[Layout,* *Shape\]*) -- The target layout or shape that defines the codomain

- **loc** (*optional*) -- Optional location information for IR diagnostics

- **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions

#### Returns

The complement layout

#### Return type

Layout

**Example:**

    import cutlass.cute as cute
    @cute.jit
    def foo():
        # Create a right-major layout for a 4x4 matrix
        row_layout = cute.make_layout((4, 4), stride=(4, 1))
        # Create a left-major layout that complements the row layout
        col_layout = cute.complement(row_layout, 16)
        # The two layouts are complementary under 16

### right_inverse

**Kind:** function

**Qualified name:** `cutlass.cute.right_inverse`

**Signature:** `right_inverse(input: cutlass.cute.typing.Layout) → cutlass.cute.typing.Layout`

### left_inverse

**Kind:** function

**Qualified name:** `cutlass.cute.left_inverse`

**Signature:** `left_inverse(input: cutlass.cute.typing.Layout) → cutlass.cute.typing.Layout`

### logical_product

**Kind:** function

**Qualified name:** `cutlass.cute.logical_product`

**Signature:** `logical_product(block: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, tiler: cutlass.cute.typing.Tile) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

### zipped_product

**Kind:** function

**Qualified name:** `cutlass.cute.zipped_product`

**Signature:** `zipped_product(block: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, tiler: cutlass.cute.typing.Layout) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

### tiled_product

**Kind:** function

**Qualified name:** `cutlass.cute.tiled_product`

**Signature:** `tiled_product(block: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, tiler: cutlass.cute.typing.Layout) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

### flat_product

**Kind:** function

**Qualified name:** `cutlass.cute.flat_product`

**Signature:** `flat_product(block: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, tiler: cutlass.cute.typing.Layout) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

### raked_product

**Kind:** function

**Qualified name:** `cutlass.cute.raked_product`

**Signature:** `raked_product(block: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, tiler: cutlass.cute.typing.Layout) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

### blocked_product

**Kind:** function

**Qualified name:** `cutlass.cute.blocked_product`

**Signature:** `blocked_product(block: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, tiler: cutlass.cute.typing.Layout) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

### logical_divide

**Kind:** function

**Qualified name:** `cutlass.cute.logical_divide`

**Signature:** `logical_divide(target: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, tiler: cutlass.cute.typing.Tiler) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

### zipped_divide

**Kind:** function

**Qualified name:** `cutlass.cute.zipped_divide`

**Signature:** `zipped_divide(target: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, tiler: cutlass.cute.typing.Tiler) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

`zipped_divide` is `logical_divide` with Tiler modes and Rest modes gathered together: `(Tiler,Rest)`

- When Tiler is Layout, this has no effect as `logical_divide` results in the same.

- When Tiler is `Tile` (nested tuple of `Layout`) or `Shape`, this zips modes into standard form `((BLK_A,BLK_B),(a,b,x,y))`

For example, if `target` has shape `(s, t, r)` and `tiler` has shape `(BLK_A, BLK_B)`, then the result will have shape `((BLK_A, BLK_B), (ceil_div(s, BLK_A), ceil_div(t, BLK_B), r))`.

#### Parameters

- **target** (*Layout* *or* *Tensor*) -- The layout or tensor to partition.

- **tiler** (*Tiler*) -- The tiling specification (can be a Layout, Shape, Tile).

- **loc** (*optional*) -- Optional MLIR IR location information.

- **ip** (*optional*) -- Optional MLIR IR insertion point.

#### Returns

A zipped (partitioned) version of the target.

#### Return type

Layout or Tensor

**Example:**

    layout = cute.make_layout((128, 64), stride=(64, 1))
    tiler = (8, 8)
    result = cute.zipped_divide(layout, tiler)  # result shape: ((8, 8), (16, 8))

### tiled_divide

**Kind:** function

**Qualified name:** `cutlass.cute.tiled_divide`

**Signature:** `tiled_divide(target: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, tiler: cutlass.cute.typing.Tiler) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

### flat_divide

**Kind:** function

**Qualified name:** `cutlass.cute.flat_divide`

**Signature:** `flat_divide(target: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, tiler: cutlass.cute.typing.Tile) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

### max_common_layout

**Kind:** function

**Qualified name:** `cutlass.cute.max_common_layout`

**Signature:** `max_common_layout(a: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, b: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor) → cutlass.cute.typing.Layout`

### max_common_vector

**Kind:** function

**Qualified name:** `cutlass.cute.max_common_vector`

**Signature:** `max_common_vector(a: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, b: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor) → int`

### tile_to_shape

**Kind:** function

**Qualified name:** `cutlass.cute.tile_to_shape`

**Signature:** `tile_to_shape(atom: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, trg_shape: cutlass.cute.typing.Shape, order: cutlass.cute.typing.Shape) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

### local_partition

**Kind:** function

**Qualified name:** `cutlass.cute.local_partition`

**Signature:** `local_partition(target: cutlass.cute.typing.Tensor, tiler: cutlass.cute.typing.Layout | cutlass.cute.typing.Shape, index: int | cutlass.cute.typing.Numeric, proj: cutlass.cute.typing.XTuple = 1) → cutlass.cute.typing.Tensor`

### local_tile

**Kind:** function

**Qualified name:** `cutlass.cute.local_tile`

**Signature:** `local_tile(input: cutlass.cute.typing.Tensor, tiler: cutlass.cute.typing.Tiler, coord: cutlass.cute.typing.Coord, proj: cutlass.cute.typing.XTuple | None = None) → cutlass.cute.typing.Tensor`

Partition a tensor into tiles using a tiler and extract a single tile at the provided coordinate.

The `local_tile` operation applies a `zipped_divide` to split the `input` tensor by the `tiler` and then slices out a single tile using the provided coord. This is commonly used for extracting block-, thread-, or CTA-level tiles for parallel operations.

\\\[\\text{local_tile}(input, tiler, coord) = \\text{zipped_divide}(input, tiler)\[coord\]\\\]

This function corresponds to the CUTE/C++ local_tile utility: <https://docs.nvidia.com/cutlass/media/docs/cpp/cute/03_tensor.html#local-tile>

#### Parameters

- **input** (*Tensor*) -- The input tensor to partition into tiles.

- **tiler** (*Tiler*) -- The tiling specification (can be a Layout, Shape, Tile).

- **coord** (*Coord*) -- The coordinate to select within the remainder ("rest") modes after tiling. This selects which tile to extract.

- **proj** (*XTuple,* *optional*) -- (Optional) Projection onto tiling modes; specify to project out unused tiler modes, e.g., when working with projections of tilers in multi-mode partitioning. Default is None for no projection.

- **loc** (*Any,* *optional*) -- (Optional) MLIR location, for diagnostic/debugging.

- **ip** (*Any,* *optional*) -- (Optional) MLIR insertion point, used in IR building context.

#### Returns

A new tensor representing the local tile selected at the given coordinate.

#### Return type

Tensor

**Examples**

1.  Tiling a 2D tensor and extracting a tile:

    > # input: (16, 24)
    >     tensor : cute.Tensor
    >     tiler = (2, 4)
    >     coord = (1, 1)
    >
    >     # output: (8, 6)
    >     # - zipped_divide(tensor, tiler)     -> ((2, 4), (8, 6))
    >     # - local_tile(tensor, tiler, coord) -> (8, 6)
    >     result = cute.local_tile(tensor, tiler=tiler, coord=coord)

2.  Using a stride projection for specialized tiling:

    > # input: (16, 24)
    >     tensor : cute.Tensor
    >     tiler = (2, 2, 4)
    >     coord = (0, 1, 1)
    >     proj = (1, None, 1)
    >
    >     # output: (8, 6)
    >     # projected_tiler: (2, 4)
    >     # projected_coord: (0, 1)
    >     # - zipped_divide(tensor, projected_tiler)               -> ((2, 4), (8, 6))
    >     # - local_tile(tensor, projected_tiler, projected_coord) -> (8, 6)
    >     result = cute.local_tile(tensor, tiler=tiler, coord=coord, proj=proj)

### make_layout_image_mask

**Kind:** function

**Qualified name:** `cutlass.cute.make_layout_image_mask`

**Signature:** `make_layout_image_mask(lay: cutlass.cute.typing.Layout, coord: cutlass.cute.typing.Coord, mode: int) → cutlass.cute.typing.Int16`

Makes a 16-bit integer mask of the image of a layout sliced at a given mode and accounting for the offset given by the input coordinate for the other modes.

Static layouts unroll the OR-chain at trace time. Dynamic layouts emit an `scf.for` over the multicast axis.

### leading_dim

**Kind:** function

**Qualified name:** `cutlass.cute.leading_dim`

**Signature:** `leading_dim(shape: cutlass.cute.typing.Shape, stride: cutlass.cute.typing.Stride) → int | Tuple[int, ...] | None`

Find the leading dimension of a shape and stride.

#### Parameters

- **shape** (*Shape*) -- The shape of the tensor or layout

- **stride** (*Stride*) -- The stride of the tensor or layout

#### Returns

The leading dimension index or indices

#### Return type

Union\[int, Tuple\[int, ...\], None\]

The return value depends on the stride pattern:

- If a single leading dimension is found, returns an integer index

- If nested leading dimensions are found, returns a tuple of indices

- If no leading dimension is found, returns None

### make_layout_tv

**Kind:** function

**Qualified name:** `cutlass.cute.make_layout_tv`

**Signature:** `make_layout_tv(thr_layout: cutlass.cute.typing.Layout, val_layout: cutlass.cute.typing.Layout) → Tuple[cutlass.cute.typing.Shape, cutlass.cute.typing.Layout]`

Create a thread-value layout by repeating the val_layout over the thr_layout.

This function creates a thread-value layout that maps between `(thread_idx, value_idx)` coordinates and logical `(M,N)` coordinates. The thread and value layouts must be compact to ensure proper partitioning.

This implements the thread-value partitioning pattern where data is partitioned across threads and values within each thread.

#### Parameters

- **thr_layout** (*Layout*) -- Layout mapping from `(TileM,TileN)` coordinates to thread IDs (must be compact)

- **val_layout** (*Layout*) -- Layout mapping from `(ValueM,ValueN)` coordinates to value IDs within each thread

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

#### Returns

A tuple containing `tiler_mn` and `layout_tv`

#### Return type

Tuple\[Shape, Layout\]

where:

:   - `tiler_mn` is tiler and `shape(tiler_mn)` is compatible with `shape(zipped_divide(x, tiler_mn))[0]`

    - `layout_tv`: Thread-value layout mapping (thread_idx, value_idx) -\> (M,N)

**Example:**

The below code creates a TV Layout that maps thread/value coordinates to the logical coordinates in a `(4,6)` tensor:

:   - *Tiler MN*: `(4,6)`

    - *TV Layout*: `((3,2),(2,2)):((8,2),(4,1))`

<!-- -->

    thr_layout = cute.make_layout((2, 3), stride=(3, 1))
    val_layout = cute.make_layout((2, 2), stride=(2, 1))
    tiler_mn, layout_tv = cute.make_layout_tv(thr_layout, val_layout)

  --- -------- -------- -------- -------- -------- --------
      0        1        2        3        4        5
  0   T0, V0   T0, V1   T1, V0   T1, V1   T2, V0   T2, V1
  1   T0, V2   T0, V3   T1, V2   T1, V3   T2, V2   T2, V3
  2   T3, V0   T3, V1   T4, V0   T4, V1   T5, V0   T5, V1
  3   T3, V2   T3, V3   T4, V2   T4, V3   T5, V2   T5, V3
  --- -------- -------- -------- -------- -------- --------

  : Table 6 TV Layout {#id1}

### get_nonswizzle_portion

**Kind:** function

**Qualified name:** `cutlass.cute.get_nonswizzle_portion`

**Signature:** `get_nonswizzle_portion(layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

Extract the non-swizzle portion from a layout.

For a simple Layout, the entire layout is considered non-swizzled and is returned as-is. For a ComposedLayout, the inner layout (non-swizzled portion) is extracted and returned, effectively separating the base layout from any swizzle transformation that may be applied.

#### Parameters

- **layout** (*Union\[Layout,* *ComposedLayout\]*) -- A Layout or ComposedLayout from which to extract the non-swizzle portion.

- **loc** (*optional*) -- Optional location information for IR diagnostics.

- **ip** (*optional*) -- Optional

#### Returns

The non-swizzle portion of the input layout. For Layout objects, returns the layout itself. For ComposedLayout objects, returns the outer layout component.

#### Return type

Layout

#### Raises

**TypeError** -- If the layout is neither a Layout nor a ComposedLayout.

### get_swizzle_portion

**Kind:** function

**Qualified name:** `cutlass.cute.get_swizzle_portion`

**Signature:** `get_swizzle_portion(layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout) → Swizzle`

Extract or create the swizzle portion from a layout.

For a simple Layout (which has no explicit swizzle), a default identity swizzle is created. For a ComposedLayout, the outer layout is checked and returned if it is a Swizzle object. Otherwise, a default identity swizzle is created. The default identity swizzle has parameters (0, 4, 3), which represents a no-op swizzle transformation.

#### Parameters

- **layout** (*Union\[Layout,* *ComposedLayout\]*) -- A Layout or ComposedLayout from which to extract the swizzle portion.

- **loc** (*optional*) -- Optional location information for IR diagnostics.

- **ip** (*optional*) -- Optional

#### Returns

The swizzle portion of the layout. For Layout objects or ComposedLayout objects without a Swizzle outer component, returns a default identity swizzle (0, 4, 3). For ComposedLayout objects with a Swizzle outer component, returns that swizzle.

#### Return type

[Swizzle](#cutlass.cute.Swizzle)

#### Raises

**TypeError** -- If the layout is neither a Layout nor a ComposedLayout.

### nullspace

**Kind:** function

**Qualified name:** `cutlass.cute.nullspace`

**Signature:** `nullspace(layout: cutlass.cute.typing.Layout) → cutlass.cute.typing.Layout`

Computes the nullspace (kernel) of a layout.

Returns a layout l such that layout(l(i)) == 0 for all i \< size(l), nullspace(l) == make_layout(1, stride=0), and size(l) == size(layout) / size(filter_zeros(layout))

#### Parameters

- **layout** (*Layout*) -- The layout to compute the nullspace of.

- **loc** (*optional*) -- Optional location information for IR diagnostics.

- **ip** (*optional*) -- Optional

#### Returns

The nullspace of the layout

#### Return type

Layout

#### Raises

**TypeError** -- If the layout is not a Layout.

### AddressSpace

**Kind:** class

**Qualified name:** `cutlass.cute.AddressSpace`

**Signature:** `AddressSpace(value)`

Bases: `IntEnum`

Public CUTLASS address-space enum.

Values match the CuTe MLIR dialect address-space encoding, but this enum is intentionally defined in Python so public APIs do not depend on a dialect binding object.

#### generic

**Kind:** attribute

**Qualified name:** `cutlass.cute.AddressSpace.generic`

**Signature:** `generic = 0`

#### gmem

**Kind:** attribute

**Qualified name:** `cutlass.cute.AddressSpace.gmem`

**Signature:** `gmem = 1`

#### smem

**Kind:** attribute

**Qualified name:** `cutlass.cute.AddressSpace.smem`

**Signature:** `smem = 3`

#### rmem

**Kind:** attribute

**Qualified name:** `cutlass.cute.AddressSpace.rmem`

**Signature:** `rmem = 5`

#### tmem

**Kind:** attribute

**Qualified name:** `cutlass.cute.AddressSpace.tmem`

**Signature:** `tmem = 6`

#### dsmem

**Kind:** attribute

**Qualified name:** `cutlass.cute.AddressSpace.dsmem`

**Signature:** `dsmem = 7`

#### cmem

**Kind:** attribute

**Qualified name:** `cutlass.cute.AddressSpace.cmem`

**Signature:** `cmem = 4`

### CacheEvictionPriority

**Kind:** attribute

**Qualified name:** `cutlass.cute.CacheEvictionPriority`

**Signature:** `CacheEvictionPriority`

alias of `_DocDialectObject`

### ScaledBasis

**Kind:** class

**Qualified name:** `cutlass.cute.ScaledBasis`

**Signature:** `ScaledBasis(value: Any, mode: int | List[int])`

Bases: `object`

A class representing a scaled basis element in CuTe's layout algebra.

ScaledBasis is used to represent elements in the layout algebra, particularly in the context of composition operations. It consists of a value (scale) and a mode that identifies mode of the basis element.

#### Parameters

- **value** (*Union\[int,* [*Integer*](../basic_data_types.md#cutlass.Integer)*,* *Ratio,* *ir.Value\]*) -- The scale value

- **mode** (*Union\[int,* *List\[int\]\]*) -- The mode identifying the basis element

#### Raises

**TypeError** -- If mode is not an integer or list of integers

**Examples:**

    # Create a scaled basis with integer scale and mode
    sb1 = ScaledBasis(2, 0)  # 2 * E(0)

    # Create a scaled basis with a Ratio scale
    sb2 = ScaledBasis(Ratio(1, 2), 1)  # (1/2) * E(1)

    # Create a scaled basis with a list of modes
    sb3 = ScaledBasis(4, [0, 1])  # 4 * E([0, 1])

    # Scaled basis elements are commonly used in layout strides
    layout = make_layout((4, 8), stride=(ScaledBasis(2, 0), ScaledBasis(1, 1)))

    # This creates a layout with strides (2@0, 1@1) representing
    # a coordinate system where each dimension has its own basis

    # Example: Mapping coordinates to indices using the layout
    coord = (2, 3)
    idx = crd2idx(coord, layout)  # Maps (2, 3) to (4, 3)

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.ScaledBasis.__init__`

**Signature:** `__init__(value: Any, mode: int | List[int]) → None`

#### is_static

**Kind:** method

**Qualified name:** `cutlass.cute.ScaledBasis.is_static`

**Signature:** `is_static() → bool`

Check if the value is statically known.

##### Returns

True if the value is not a dynamic expression

##### Return type

bool

#### to

**Kind:** method

**Qualified name:** `cutlass.cute.ScaledBasis.to`

**Signature:** `to(dtype: type) → Any`

Convert to another type.

##### Parameters

- **dtype** (*type*) -- The target type for conversion

- **loc** (*Location,* *optional*) -- The source location for the operation, defaults to None

- **ip** (*InsertionPoint,* *optional*) -- The insertion point for the operation, defaults to None

##### Returns

The ScaledBasis converted to the specified type

##### Raises

**TypeError** -- If conversion to the specified type is not supported

#### value

**Kind:** property

**Qualified name:** `cutlass.cute.ScaledBasis.value`

**Signature:** `value: Any`

Get the scale value.

##### Returns

The scale value

#### mode

**Kind:** property

**Qualified name:** `cutlass.cute.ScaledBasis.mode`

**Signature:** `mode: List[int]`

Get the mode identifying the basis element.

##### Returns

The mode as a list of integers

##### Return type

List\[int\]

### Atom

**Kind:** class

**Qualified name:** `cutlass.cute.Atom`

**Signature:** `Atom(op: Op, trait: Trait)`

Bases: `ABC`

Atom base class.

An Atom is the composition of

- a MMA or Copy Operation;

- an internal MMA or Copy Trait.

An Operation is a pure Python class that is used to model a specific MMA or Copy instruction. The Trait wraps the underlying IR Value and provides access to the metadata of the instruction encoded using CuTe Layouts. When the Trait can be constructed straighforwardly from an Operation, the `make_mma_atom` or `make_copy_atom` API should be used. There are cases where constructing the metadata is not trivial and requires more information, for example to determine the number of bytes copied per TMA instruction ("the TMA vector length"). In such cases, dedicated helper functions are provided with an appropriate API such that the Atom is constructed internally in an optimal fashion for the user.

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.Atom.__init__`

**Signature:** `__init__(op: Op, trait: Trait) → None`

#### op

**Kind:** property

**Qualified name:** `cutlass.cute.Atom.op`

**Signature:** `op: Op`

#### type

**Kind:** property

**Qualified name:** `cutlass.cute.Atom.type`

**Signature:** `type: _install_cutlass_mlir_autodoc_stub.<locals>._DocMlirType`

#### set

**Kind:** method

**Qualified name:** `cutlass.cute.Atom.set`

**Signature:** `set(modifier: Any, value: Any) → None`

Sets runtime fields of the Atom.

Some Atoms have runtime state, for example a tcgen05 MMA Atom

    tiled_mma = cute.make_tiled_mma(some_tcgen05_mma_op)
    tiled_mma.set(cute.nvgpu.tcgen05.Field.ACCUMULATE, True)

The `set` method provides a way to the user to modify such runtime state. Modifiable fields are provided by arch-specific enumerations, for example `tcgen05.Field`. The Atom instance internally validates the field as well as the value provided by the user to set the field to.

#### get

**Kind:** method

**Qualified name:** `cutlass.cute.Atom.get`

**Signature:** `get(field: Any) → Any`

Gets runtime fields of the Atom.

Some Atoms have runtime state, for example a tcgen05 MMA Atom

    tiled_mma = cute.make_tiled_mma(some_tcgen05_mma_op)
    accum = tiled_mma.get(cute.nvgpu.tcgen05.Field.ACCUMULATE)

The `get` method provides a way to the user to access such runtime state. Modifiable fields are provided by arch-specific enumerations, for example `tcgen05.Field`. The Atom instance internally validates the field as well as the value provided by the user to set the field to.

#### with\_

**Kind:** method

**Qualified name:** `cutlass.cute.Atom.with_`

**Signature:** `with_(*, **kwargs: ~typing.Any) → Atom`

Returns a new Atom with the new Operation and Trait with the given runtime state. The runtime state is provided as keyword arguments and it is Atom-specific.

    tiled_copy = cute.make_tiled_copy(tma_copy_op)
    new_tiled_copy = tiled_copy.with_(tma_bar_ptr=tma_bar_ptr, cache_policy=cute.CacheEvictionPriority.EVICT_LAST)

The `with_` method provides a way to the user to modify such runtime state or create an executable Atom (e.g. an Executable TMA Load Atom).

#### \_unpack

**Kind:** method

**Qualified name:** `cutlass.cute.Atom._unpack`

**Signature:** `_unpack(*, **kwargs: ~typing.Any) → ir.Value`

#### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.Atom._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

### MmaAtom

**Kind:** class

**Qualified name:** `cutlass.cute.MmaAtom`

**Signature:** `MmaAtom(op: Op, trait: Trait)`

Bases: [`Atom`](#cutlass.cute.Atom)

The MMA Atom class.

#### thr_id

**Kind:** property

**Qualified name:** `cutlass.cute.MmaAtom.thr_id`

**Signature:** `thr_id: cutlass.cute.typing.Layout`

#### shape_mnk

**Kind:** property

**Qualified name:** `cutlass.cute.MmaAtom.shape_mnk`

**Signature:** `shape_mnk: cutlass.cute.typing.Shape`

#### tv_layout_A

**Kind:** property

**Qualified name:** `cutlass.cute.MmaAtom.tv_layout_A`

**Signature:** `tv_layout_A: cutlass.cute.typing.Layout`

#### tv_layout_B

**Kind:** property

**Qualified name:** `cutlass.cute.MmaAtom.tv_layout_B`

**Signature:** `tv_layout_B: cutlass.cute.typing.Layout`

#### tv_layout_C

**Kind:** property

**Qualified name:** `cutlass.cute.MmaAtom.tv_layout_C`

**Signature:** `tv_layout_C: cutlass.cute.typing.Layout`

#### make_fragment_A

**Kind:** method

**Qualified name:** `cutlass.cute.MmaAtom.make_fragment_A`

**Signature:** `make_fragment_A(input: Any) → _MockObject`

#### make_fragment_B

**Kind:** method

**Qualified name:** `cutlass.cute.MmaAtom.make_fragment_B`

**Signature:** `make_fragment_B(input: Any) → _MockObject`

#### make_fragment_C

**Kind:** method

**Qualified name:** `cutlass.cute.MmaAtom.make_fragment_C`

**Signature:** `make_fragment_C(input: Any) → _MockObject`

#### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.MmaAtom._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

### CopyAtom

**Kind:** class

**Qualified name:** `cutlass.cute.CopyAtom`

**Signature:** `CopyAtom(op: Op, trait: Trait)`

Bases: [`Atom`](#cutlass.cute.Atom)

The Copy Atom class.

#### value_type

**Kind:** property

**Qualified name:** `cutlass.cute.CopyAtom.value_type`

**Signature:** `value_type: Type[cutlass.cute.typing.Numeric]`

#### thr_id

**Kind:** property

**Qualified name:** `cutlass.cute.CopyAtom.thr_id`

**Signature:** `thr_id: cutlass.cute.typing.Layout`

#### layout_src_tv

**Kind:** property

**Qualified name:** `cutlass.cute.CopyAtom.layout_src_tv`

**Signature:** `layout_src_tv: cutlass.cute.typing.Layout`

#### layout_dst_tv

**Kind:** property

**Qualified name:** `cutlass.cute.CopyAtom.layout_dst_tv`

**Signature:** `layout_dst_tv: cutlass.cute.typing.Layout`

#### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.CopyAtom._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

### TiledCopy

**Kind:** class

**Qualified name:** `cutlass.cute.TiledCopy`

**Signature:** `TiledCopy(op: Op, trait: Trait)`

Bases: [`CopyAtom`](#cutlass.cute.CopyAtom)

The tiled Copy class.

#### layout_tv_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledCopy.layout_tv_tiled`

**Signature:** `layout_tv_tiled: cutlass.cute.typing.Layout`

#### tiler_mn

**Kind:** property

**Qualified name:** `cutlass.cute.TiledCopy.tiler_mn`

**Signature:** `tiler_mn: cutlass.cute.typing.Tile`

#### layout_src_tv_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledCopy.layout_src_tv_tiled`

**Signature:** `layout_src_tv_tiled: cutlass.cute.typing.Layout`

#### layout_dst_tv_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledCopy.layout_dst_tv_tiled`

**Signature:** `layout_dst_tv_tiled: cutlass.cute.typing.Layout`

#### size

**Kind:** property

**Qualified name:** `cutlass.cute.TiledCopy.size`

**Signature:** `size: int`

#### get_slice

**Kind:** method

**Qualified name:** `cutlass.cute.TiledCopy.get_slice`

**Signature:** `get_slice(thr_idx: int | cutlass.cute.typing.Int32) → ThrCopy`

#### retile

**Kind:** method

**Qualified name:** `cutlass.cute.TiledCopy.retile`

**Signature:** `retile(src: cutlass.cute.typing.Tensor) → cutlass.cute.typing.Tensor`

#### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.TiledCopy._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

### TiledMma

**Kind:** class

**Qualified name:** `cutlass.cute.TiledMma`

**Signature:** `TiledMma(op: Op, trait: Trait)`

Bases: [`MmaAtom`](#cutlass.cute.MmaAtom)

The tiled MMA class.

#### tv_layout_A_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.tv_layout_A_tiled`

**Signature:** `tv_layout_A_tiled: cutlass.cute.typing.Layout`

#### tv_layout_B_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.tv_layout_B_tiled`

**Signature:** `tv_layout_B_tiled: cutlass.cute.typing.Layout`

#### tv_layout_C_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.tv_layout_C_tiled`

**Signature:** `tv_layout_C_tiled: cutlass.cute.typing.Layout`

#### permutation_mnk

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.permutation_mnk`

**Signature:** `permutation_mnk: cutlass.cute.typing.Tile`

#### thr_layout_vmnk

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.thr_layout_vmnk`

**Signature:** `thr_layout_vmnk: cutlass.cute.typing.Layout`

#### size

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.size`

**Signature:** `size: int`

#### get_tile_size

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma.get_tile_size`

**Signature:** `get_tile_size(mode_idx: int) → cutlass.cute.typing.Shape`

#### get_slice

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma.get_slice`

**Signature:** `get_slice(thr_idx: int | cutlass.cute.typing.Int32) → ThrMma`

#### \_partition_shape

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma._partition_shape`

**Signature:** `_partition_shape(operand_id: Any, shape: cutlass.cute.typing.Shape) → cutlass.cute.typing.XTuple`

#### partition_shape_A

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma.partition_shape_A`

**Signature:** `partition_shape_A(shape_mk: cutlass.cute.typing.Shape) → cutlass.cute.typing.XTuple`

#### partition_shape_B

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma.partition_shape_B`

**Signature:** `partition_shape_B(shape_nk: cutlass.cute.typing.Shape) → cutlass.cute.typing.XTuple`

#### partition_shape_C

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma.partition_shape_C`

**Signature:** `partition_shape_C(shape_mn: cutlass.cute.typing.Shape) → cutlass.cute.typing.XTuple`

#### \_thrfrg

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma._thrfrg`

**Signature:** `_thrfrg(operand_id: Any, input: cutlass.cute.typing.Layout) → cutlass.cute.typing.Layout`

#### \_thrfrg_A

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma._thrfrg_A`

**Signature:** `_thrfrg_A(input: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

#### \_thrfrg_B

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma._thrfrg_B`

**Signature:** `_thrfrg_B(input: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

#### \_thrfrg_C

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma._thrfrg_C`

**Signature:** `_thrfrg_C(input: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

#### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.TiledMma._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

### ThrMma

**Kind:** class

**Qualified name:** `cutlass.cute.ThrMma`

**Signature:** `ThrMma(op: Op, trait: Trait, thr_idx: int | cutlass.cute.typing.Int32)`

Bases: [`TiledMma`](#cutlass.cute.TiledMma)

The thread MMA class for modeling a thread-slice of a tiled MMA.

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.ThrMma.__init__`

**Signature:** `__init__(op: Op, trait: Trait, thr_idx: int | cutlass.cute.typing.Int32) → None`

#### thr_idx

**Kind:** property

**Qualified name:** `cutlass.cute.ThrMma.thr_idx`

**Signature:** `thr_idx: int | cutlass.cute.typing.Int32`

#### partition_A

**Kind:** method

**Qualified name:** `cutlass.cute.ThrMma.partition_A`

**Signature:** `partition_A(input_mk: cutlass.cute.typing.Tensor) → cutlass.cute.typing.Tensor`

#### partition_B

**Kind:** method

**Qualified name:** `cutlass.cute.ThrMma.partition_B`

**Signature:** `partition_B(input_nk: cutlass.cute.typing.Tensor) → cutlass.cute.typing.Tensor`

#### partition_C

**Kind:** method

**Qualified name:** `cutlass.cute.ThrMma.partition_C`

**Signature:** `partition_C(input_mn: cutlass.cute.typing.Tensor) → cutlass.cute.typing.Tensor`

#### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.ThrMma._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

### ThrCopy

**Kind:** class

**Qualified name:** `cutlass.cute.ThrCopy`

**Signature:** `ThrCopy(op: Op, trait: Trait, thr_idx: int | cutlass.cute.typing.Int32)`

Bases: [`TiledCopy`](#cutlass.cute.TiledCopy)

The thread Copy class for modeling a thread-slice of a tiled Copy.

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.ThrCopy.__init__`

**Signature:** `__init__(op: Op, trait: Trait, thr_idx: int | cutlass.cute.typing.Int32) → None`

#### thr_idx

**Kind:** property

**Qualified name:** `cutlass.cute.ThrCopy.thr_idx`

**Signature:** `thr_idx: int | cutlass.cute.typing.Int32`

#### partition_S

**Kind:** method

**Qualified name:** `cutlass.cute.ThrCopy.partition_S`

**Signature:** `partition_S(src: cutlass.cute.typing.Tensor) → cutlass.cute.typing.Tensor`

#### partition_D

**Kind:** method

**Qualified name:** `cutlass.cute.ThrCopy.partition_D`

**Signature:** `partition_D(dst: cutlass.cute.typing.Tensor) → cutlass.cute.typing.Tensor`

#### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.ThrCopy._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

### TensorSSA

**Kind:** class

**Qualified name:** `cutlass.cute.TensorSSA`

**Signature:** `TensorSSA(value: ir.Value, shape: cutlass.cute.typing.Shape, dtype: Type[cutlass.cute.typing.Numeric] | None = None)`

Bases: [`Vector`](../basic_data_types.md#cutlass.Vector)

A class representing thread local data from CuTe Tensor in value semantic and immutable.

#### Parameters

- **value** (*ir.Value*) -- Flatten vector as ir.Value holding logic data of SSA Tensor

- **shape** (*Shape*) -- The nested shape in CuTe of the vector

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of the tensor elements

#### Variables

- **\_shape** -- The nested shape in CuTe of the vector

- **\_dtype** -- Data type of the tensor elements

#### Raises

**ValueError** -- If shape is not static

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.__init__`

**Signature:** `__init__(value: ir.Value, shape: cutlass.cute.typing.Shape, dtype: Type[cutlass.cute.typing.Numeric] | None = None) → None`

Create a [`TensorSSA`](#cutlass.cute.TensorSSA) object: an immutable, thread-local tensor backed by a flattened MLIR vector.

##### Parameters

- **value** (`ir.Value`) -- A `ir.Value` holding the flattened MLIR vector value of the tensor.

- **shape** (*Shape*) -- The logical (possibly nested) shape of the tensor.

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\],* *optional*) -- The data type of the tensor elements. If None, this is inferred from the MLIR element type.

##### Keyword Arguments

- **loc** -- Optional location for op construction.

- **ip** -- Optional insertion point for op construction.

##### Raises

**ValueError** -- If `value` is not an `ir.Value`, is not of vector type, or if `shape` is not statically known.

> **Note.**
>
> - Instances are immutable and represent per-thread local SSA values using value semantics.
>
> - The tensor's broadcast shape and static element type are registered; dynamic shapes are not supported.

#### from_vector

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.from_vector`

**Signature:** `static from_vector(value: ir.Value, *, dtype: Type[cutlass.cute.typing.Numeric] | None = None, shape: cutlass.cute.typing.Shape | None = None) → TensorSSA`

Construct a [`TensorSSA`](#cutlass.cute.TensorSSA) from a given MLIR vector value.

This helper interprets the given 1D or n-D MLIR vector value and returns a TensorSSA view. If the input is an n-D vector, it shape-casts it into a 1D vector holding the same number of elements.

##### Parameters

- **value** -- The ir.Value representing an MLIR vector value (1D or n-D).

- **dtype** -- Optional explicit type of the elements. Deduced from MLIR type if not provided.

- **loc** -- Optional MLIR location.

- **ip** -- Optional MLIR insertion point.

##### Returns

A TensorSSA view over the vector value.

#### to_vector

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.to_vector`

**Signature:** `to_vector(*, force_flatten: bool = False) → Vector`

Convert the tensor to `Vector` carrying the tensor's dtype.

Returns a `Vector` wrapping the underlying MLIR vector value; the DSL dtype is propagated so callers can use `Vector.reduce()`, `Vector.to()`, and element-wise arithmetic.

#### dtype

**Kind:** property

**Qualified name:** `cutlass.cute.TensorSSA.dtype`

**Signature:** `dtype: Type[cutlass.cute.typing.Numeric]`

The DSL element type (e.g., Float32, Int32).

#### element_type

**Kind:** property

**Qualified name:** `cutlass.cute.TensorSSA.element_type`

**Signature:** `element_type: Type[cutlass.cute.typing.Numeric]`

#### \_wrap_like

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA._wrap_like`

**Signature:** `_wrap_like(result_ir: ir.Value) → TensorSSA`

Preserve CuTe nested shape when the math foundation wraps a per-element op's result back into a TensorSSA.

#### \_count

**Kind:** property

**Qualified name:** `cutlass.cute.TensorSSA._count`

**Signature:** `_count: int`

Total element count --- flatten CuTe nested shape before multiplying.

Overrides `Vector._count`, which assumes a flat MLIR shape tuple. TensorSSA carries a possibly-nested CuTe shape (e.g. `((4, 2), 8)`), so the base implementation's `result *= dim` produces garbage for nested shapes (tuple-repetition instead of arithmetic). `numel` picks up this override automatically.

#### shape

**Kind:** property

**Qualified name:** `cutlass.cute.TensorSSA.shape`

**Signature:** `shape: cutlass.cute.typing.Shape`

The logical shape of the vector array (1D, 2D, or 3D).

#### \_apply_op

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA._apply_op`

**Signature:** `_apply_op(op: Callable, other: object, flip: bool = False) → TensorSSA`

#### apply_op

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.apply_op`

**Signature:** `apply_op(op: Callable, other: object, flip: bool = False) → TensorSSA`

Apply a binary operation to this tensor and another operand.

This public API method wraps the internal `_apply_op` for external usage, allowing custom operations to be performed on tensors.

##### Parameters

- **op** (*Callable*) -- The operation function (e.g., `operator.add`, `operator.mul`, etc.).

- **other** ([*TensorSSA*](#cutlass.cute.TensorSSA) *or* *ArithValue* *or* *scalar*) -- The other operand. Can be a [`TensorSSA`](#cutlass.cute.TensorSSA), ArithValue, or scalar.

- **flip** (*bool,* *optional*) -- If `True`, flips the operands (applies operation as `op(other, self)`).

- **loc** (*object,* *optional*) -- MLIR location, optional.

- **ip** (*object,* *optional*) -- MLIR insertion point, optional.

##### Returns

The result of applying the binary operation.

##### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

**Example**

    import operator

    tensor1 = cute.Tensor(...)
    tensor2 = cute.Tensor(...)
    result = tensor1.apply_op(operator.add, tensor2)
    # Equivalent to: tensor1 + tensor2

#### broadcast_to

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.broadcast_to`

**Signature:** `broadcast_to(target_shape: cutlass.cute.typing.Shape) → TensorSSA`

Broadcast the tensor to the target shape.

This method broadcasts the tensor to match a target shape following NumPy-style broadcasting rules. Dimensions of size 1 can be broadcast to any size, and missing dimensions are added with size 1.

##### Parameters

- **target_shape** (*Shape*) -- The desired output shape

- **loc** (*Optional\[Location\]*) -- Source location for MLIR operations, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for MLIR operations, defaults to None

##### Returns

A new tensor broadcast to the target shape

##### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

##### Raises

**ValueError** -- If shapes are incompatible for broadcasting

**Examples:**

    # Broadcast a (1, 4) tensor to (3, 4)
    src = cute.full((1, 4), 1.0, Float32)
    dst = src.broadcast_to((3, 4))
    # dst now has shape (3, 4) with the first row replicated

#### \_flatten_shape_and_coord

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA._flatten_shape_and_coord`

**Signature:** `_flatten_shape_and_coord(crd: cutlass.cute.typing.Coord) → Tuple[cutlass.cute.typing.Shape, cutlass.cute.typing.Coord]`

#### \_build_result

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA._build_result`

**Signature:** `_build_result(res_vect: ir.Value, res_shp: cutlass.cute.typing.Shape, *, row_major: bool = False) → TensorSSA`

#### reshape

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.reshape`

**Signature:** `reshape(shape: cutlass.cute.typing.Shape) → TensorSSA`

Reshape the tensor to a new shape.

##### Parameters

**shape** (*Shape*) -- The new shape to reshape to.

##### Returns

A new tensor with the same elements but with the new shape.

##### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

##### Raises

- **NotImplementedError** -- If dynamic size is not supported

- **ValueError** -- If the new shape is not compatible with the current shape

#### to

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.to`

**Signature:** `to(dtype: Type[cutlass.cute.typing.Numeric]) → TensorSSA`

Convert the tensor to a different numeric type.

##### Parameters

**dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The target numeric type to cast to.

##### Returns

A new tensor with the same shape but with elements cast to the target type.

##### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

##### Raises

- **TypeError** -- If dtype is not a subclass of Numeric.

- **NotImplementedError** -- If dtype is an unsigned integer type.

#### bitcast

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.bitcast`

**Signature:** `bitcast(dtype: Type[cutlass.cute.typing.Numeric]) → TensorSSA`

Reinterpret the bits of this tensor as a different element type.

Total bit width is preserved; the element count adjusts proportionally. For example, a `TensorSSA` of shape `(4,)` with `Float32` bitcast to `Float16` yields a `TensorSSA` of shape `(8,)` with `Float16` (4 × 32 = 8 × 16 bits). Multi-dimensional shapes are flattened.

##### Parameters

**dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Target DSL element type (e.g. `Int32`, `Float16`).

##### Returns

A new [`TensorSSA`](#cutlass.cute.TensorSSA) with bits reinterpreted as `dtype`.

##### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

##### Raises

**TypeError** -- If `dtype` is not a subclass of `Numeric`.

#### ir_value

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.ir_value`

**Signature:** `ir_value() → TensorSSA`

Return the underlying MLIR vector value.

#### ir_value_int8

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.ir_value_int8`

**Signature:** `ir_value_int8() → ir.Value`

Returns int8 ir value of Boolean tensor. When we need to store Boolean tensor ssa, use ir_value_int8().

##### Parameters

- **loc** (*Optional\[Location\],* *optional*) -- Source location information, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point for MLIR operations, defaults to None

##### Returns

The int8 value of this Boolean

##### Return type

ir.Value

#### reduce

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.reduce`

**Signature:** `reduce(op: _install_cutlass_mlir_autodoc_stub.<locals>._DocDialectObject, init_val: object, reduction_profile: cutlass.cute.typing.Coord) → TensorSSA | ir.Value`

Perform reduce on selected modes with given predefined reduction op.

##### Parameters

- **op** (*operator*) -- The reduction operator to use (operator.add or operator.mul)

- **init_val** (*numeric*) -- The initial value for the reduction

- **reduction_profile** (*Coord*) -- Specifies which dimensions to reduce. Dimensions marked with None are kept.

##### Returns

The reduced tensor

##### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

**Examples:**

    reduce(f32 o (4,))
      => f32

    reduce(f32 o (4, 5))
      => f32
    reduce(f32 o (4, (5, 4)), reduction_profile=(None, 1))
      => f32 o (4,)
    reduce(f32 o (4, (5, 4)), reduction_profile=(None, (None, 1)))
      => f32 o (4, (5,))

### ReductionOp

**Kind:** attribute

**Qualified name:** `cutlass.cute.ReductionOp`

**Signature:** `ReductionOp`

alias of `_DocDialectObject`

### ReductionKind

**Kind:** attribute

**Qualified name:** `cutlass.cute.ReductionKind`

**Signature:** `ReductionKind`

alias of `_DocDialectObject`

### print_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.print_tensor`

**Signature:** `print_tensor(tensor: cutlass.cute.typing.Tensor | TensorSSA, *, verbose: bool = False) → None`

Print content of the tensor in human readable format.

Outputs the tensor data in a structured format showing both metadata and the actual data values. The output includes tensor type information, layout details, and a formatted array representation of the values.

#### Parameters

- **tensor** (*Tensor*) -- The tensor to print

- **verbose** (*bool*) -- If True, includes additional debug information in the output

- **loc** (*source location,* *optional*) -- Source location where it's called, defaults to None

- **ip** (*insertion pointer,* *optional*) -- Insertion pointer for IR generation, defaults to None

#### Raises

**NotImplementedError** -- If the tensor type doesn't support trivial dereferencing

**Example output:**

    tensor(raw_ptr<@..., Float32, generic, align(4)> o (8,5):(5,1), data=
           [[-0.4326, -0.5434,  0.1238,  0.7132,  0.8042],
            [-0.8462,  0.9871,  0.4389,  0.7298,  0.6948],
            [ 0.3426,  0.5856,  0.1541,  0.2923,  0.6976],
            [-0.1649,  0.8811,  0.1788,  0.1404,  0.2568],
            [-0.2944,  0.8593,  0.4171,  0.8998,  0.1766],
            [ 0.8814,  0.7919,  0.7390,  0.4566,  0.1576],
            [ 0.9159,  0.7577,  0.6918,  0.0754,  0.0591],
            [ 0.6551,  0.1626,  0.1189,  0.0292,  0.8655]])

### make_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.make_tensor`

**Signature:** `make_tensor(iterator: cutlass.cute.typing.Pointer | cutlass.cute.typing.IntTuple | ir.Value, layout: cutlass.cute.typing.Shape | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout) → cutlass.cute.typing.Tensor`

Creates a tensor by composing an engine (iterator/pointer) with a layout.

A tensor is defined as T = E ∘ L, where E is an engine (array, pointer, or counting iterator) and L is a layout that maps logical coordinates to physical offsets. The tensor evaluates coordinates by applying the layout mapping and dereferencing the engine at the resulting offset.

#### Parameters

- **iterator** (*Union\[*[*Pointer*](../basic_data_types.md#cutlass.Pointer)*,* *IntTuple,* *ir.Value\]*) -- Engine component that provides data access capabilities. Can be: - A pointer (Pointer type) - An integer or integer tuple for coordinate tensors - A shared memory descriptor (SmemDescType)

- **layout** (*Union\[Shape,* *Layout,* *ComposedLayout\]*) -- Layout component that defines the mapping from logical coordinates to physical offsets. Can be: - A shape tuple that will be converted to a layout - A Layout object - A ComposedLayout object (must be a normal layout)

- **loc** (*Optional\[Location\]*) -- Source location for MLIR operation tracking, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for MLIR operation, defaults to None

#### Returns

A tensor object representing the composition E ∘ L

#### Return type

Tensor

#### Raises

- **TypeError** -- If iterator type is not a supported type

- **ValueError** -- If layout is a composed layout with customized inner functions

**Examples:**

    # Create a tensor with row-major layout from a pointer
    ptr = make_ptr(Float32, base_ptr, AddressSpace.gmem)
    layout = make_layout((64, 128), stride=(128, 1))
    tensor = make_tensor(ptr, layout)

    # Create a tensor with hierarchical layout in shared memory
    smem_ptr = make_ptr(Float16, base_ptr, AddressSpace.smem)
    layout = make_layout(((128, 8), (1, 4, 1)), stride=((32, 1), (0, 8, 4096)))
    tensor = make_tensor(smem_ptr, layout)

    # Create a coordinate tensor
    layout = make_layout(2, stride=16 * E(0))
    tensor = make_tensor(5, layout)  # coordinate tensor with iterator starting at 5

Notes

- The engine (iterator) must support random access operations

- Common engine types include raw pointers, arrays, and random-access iterators

- The layout defines both the shape (logical dimensions) and stride (physical mapping)

- Supports both direct coordinate evaluation T(c) and partial evaluation (slicing)

- ComposedLayouts must be "normal" layouts (no inner functions)

- For coordinate tensors, the iterator is converted to a counting sequence

### make_identity_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.make_identity_tensor`

**Signature:** `make_identity_tensor(shape: cutlass.cute.typing.Shape) → cutlass.cute.typing.Tensor`

Creates an identity tensor with the given shape.

An identity tensor maps each coordinate to itself, effectively creating a counting sequence within the shape's bounds. This is useful for generating coordinate indices or creating reference tensors for layout transformations.

#### Parameters

- **shape** (*Shape*) -- The shape defining the tensor's dimensions. Can be a simple integer sequence or a hierarchical structure ((m,n),(p,q))

- **loc** (*Optional\[Location\]*) -- Source location for MLIR operation tracking, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for MLIR operation, defaults to None

#### Returns

A tensor that maps each coordinate to itself

#### Return type

Tensor

**Examples:**

    # Create a simple 1D coord tensor
    tensor = make_identity_tensor(6)  # [0,1,2,3,4,5]

    # Create a 2D coord tensor
    tensor = make_identity_tensor((3,2))  # [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]

    # Create hierarchical coord tensor
    tensor = make_identity_tensor(((2,1),3))
    # [((0,0),0),((1,0),0),((0,0),1),((1,0),1),((0,0),2),((1,0),2)]

Notes

- The shape parameter follows CuTe's IntTuple concept

- Coordinates are ordered colexicographically

- Useful for generating reference coordinates in layout transformations

### make_fragment_like

**Kind:** function

**Qualified name:** `cutlass.cute.make_fragment_like`

**Signature:** `make_fragment_like(src: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor, dtype: Type[cutlass.cute.typing.Numeric] | None = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

### make_rmem_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.make_rmem_tensor`

**Signature:** `make_rmem_tensor(layout_or_shape: cutlass.cute.typing.Layout | cutlass.cute.typing.Shape, dtype: Type[cutlass.cute.typing.Numeric]) → cutlass.cute.typing.Tensor`

Creates a tensor in register memory with the specified layout/shape and data type.

This function allocates a tensor in register memory (rmem) usually on stack with either a provided layout or creates a new layout from the given shape. The tensor will have elements of the specified numeric data type.

#### Parameters

- **layout_or_shape** (*Union\[Layout,* *Shape\]*) -- Either a Layout object defining the tensor's memory organization, or a Shape defining its dimensions

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The data type for tensor elements (must be a Numeric type)

- **loc** (*Optional\[Location\]*) -- Source location for MLIR operation tracking, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for MLIR operation, defaults to None

#### Returns

A tensor allocated in register memory

#### Return type

Tensor

**Examples:**

    # Create rmem tensor with explicit layout
    layout = make_layout((128, 32))
    tensor = make_rmem_tensor(layout, cutlass.Float16)

    # Create rmem tensor directly from shape
    tensor = make_rmem_tensor((64, 64), cutlass.Float32)

Notes

- Uses 32-byte alignment to support .128 load/store operations

- Boolean types are stored as 8-bit integers

- Handles both direct shapes and Layout objects

### make_rmem_tensor_like

**Kind:** function

**Qualified name:** `cutlass.cute.make_rmem_tensor_like`

**Signature:** `make_rmem_tensor_like(src: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor | TensorSSA, dtype: Type[cutlass.cute.typing.Numeric] | None = None) → cutlass.cute.typing.Tensor`

Creates a tensor in register memory with the same shape as the input layout but

:   compact col-major strides. This is equivalent to calling make_rmem_tensor(make_layout_like(tensor)).

This function allocates a tensor in register memory (rmem) usually on stack with with the compact layout like the source. The tensor will have elements of the specified numeric data type or the same as the source.

#### Parameters

- **src** (*Union\[Layout,* *ComposedLayout,* *Tensor\]*) -- The source layout or tensor whose shape will be matched

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\],* *optional*) -- The element type for the fragment tensor, defaults to None

- **loc** (*Location,* *optional*) -- Source location for MLIR operations, defaults to None

- **ip** (*InsertionPoint,* *optional*) -- Insertion point for MLIR operations, defaults to None

#### Returns

A new layout or fragment tensor with matching shape

#### Return type

Union\[Layout, Tensor\]

**Examples:**

Creating a rmem tensor from a tensor:

    smem_tensor = cute.make_tensor(smem_ptr, layout)
    rmem_tensor = cute.make_rmem_tensor_like(smem_tensor, cutlass.Float32)
    # frag_tensor will be a register-backed tensor with the same shape

Creating a fragment with a different element type:

    tensor = cute.make_tensor(gmem_ptr, layout)
    rmem_bool_tensor = cute.make_rmem_tensor_like(tensor, cutlass.Boolean)
    # bool_frag will be a register-backed tensor with Boolean elements

**Notes**

- When used with a Tensor, if a type is provided, it will create a new fragment tensor with that element type.

- For layouts with ScaledBasis strides, the function creates a fragment from the shape only.

- This function is commonly used in GEMM and other tensor operations to create register storage for intermediate results.

### recast_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.recast_tensor`

**Signature:** `recast_tensor(src: cutlass.cute.typing.Tensor, dtype: Type[cutlass.cute.typing.Numeric], swizzle_: object | None = None) → cutlass.cute.typing.Tensor`

Recast a tensor to a different data type by changing the element interpretation.

This function reinterprets the memory of a tensor with a different element type, adjusting both the iterator pointer type and the layout to maintain consistency.

#### Parameters

- **src** (*Tensor*) -- The source tensor to recast

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- The target data type for tensor elements

- **swizzle** (*Optional,* *unused*) -- Optional swizzle parameter (reserved for future use), defaults to None

- **loc** (*Optional\[Location\]*) -- Source location for MLIR operation tracking, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for MLIR operation, defaults to None

#### Returns

A new tensor with the same memory but reinterpreted as dtype

#### Return type

Tensor

#### Raises

**TypeError** -- If dtype is not a subclass of Numeric

**Examples:**

    # Create a Float32 tensor
    tensor_f32 = make_rmem_tensor((4, 8), Float32)

    # Recast to Int32 to manipulate bits
    tensor_i32 = recast_tensor(tensor_f32, Int32)

    # Both tensors share the same memory, but interpret it differently

### find

**Kind:** function

**Qualified name:** `cutlass.cute.find`

**Signature:** `find(t: cutlass.cute.typing.XTuple, x: int, hierarchical: bool = True) → int | Tuple[int, ...] | None`

Find the first position of a value `x` in a hierarchical structure `t`.

Searches for the first occurrence of x in t, optionally excluding positions where a comparison value matches. The search can traverse nested structures and returns either a single index or a tuple of indices for nested positions.

#### Parameters

- **t** (*XTuple*) -- The search space

- **x** (*int*) -- The static integer x to search for

#### Returns

Index if found at top level, tuple of indices showing nested position, or None if not found

#### Return type

Union\[int, Tuple\[int, ...\], None\]

### find_if

**Kind:** function

**Qualified name:** `cutlass.cute.find_if`

**Signature:** `find_if(t: cutlass.cute.typing.XTuple, pred_fn: Callable[[cutlass.cute.typing.XTuple, int | Tuple[int, ...]], bool], hierarchical: bool = True) → int | Tuple[int, ...] | None`

### transform_leaf

**Kind:** function

**Qualified name:** `cutlass.cute.transform_leaf`

**Signature:** `transform_leaf(f: Callable[[...], cutlass.cute.typing.XTuple], *args: cutlass.cute.typing.XTuple) → cutlass.cute.typing.XTuple`

Apply a function to the leaf nodes of nested tuple structures.

This function traverses nested tuple structures in parallel and applies the function f to corresponding leaf nodes. All input tuples must have the same nested structure.

#### Parameters

- **f** (*Callable*) -- Function to apply to leaf nodes

- **args** -- One or more nested tuple structures with matching profiles

#### Returns

A new nested tuple with the same structure as the inputs, but with leaf values transformed by f

#### Raises

**TypeError** -- If the input tuples have different nested structures

**Example:**

    >>> transform_leaf(lambda x: x + 1, (1, 2))
    (2, 3)
    >>> transform_leaf(lambda x, y: x + y, (1, 2), (3, 4))
    (4, 6)
    >>> transform_leaf(lambda x: x * 2, ((1, 2), (3, 4)))
    ((2, 4), (6, 8))

### basis_value

**Kind:** function

**Qualified name:** `cutlass.cute.basis_value`

**Signature:** `basis_value(e: ScaledBasis | Any) → cutlass.cute.typing.Int | ir.Value | Ratio`

Extract the value from a ScaledBasis or return the input as-is.

If the input is a ScaledBasis, returns its value component. Otherwise, returns the input unchanged.

#### Parameters

**e** (*Any*) -- The input element (ScaledBasis or any other type)

#### Returns

The value of the ScaledBasis or the input itself

#### Return type

Any

**Examples:**

    >>> basis_value(ScaledBasis(5, 0))
    5
    >>> basis_value(42)
    42

### basis_get

**Kind:** function

**Qualified name:** `cutlass.cute.basis_get`

**Signature:** `basis_get(basis: ScaledBasis | cutlass.cute.typing.Numeric | int, t: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout) → cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

Apply the mode indices from a ScaledBasis to get an element from a tuple, layout, or composed layout.

If the basis is a ScaledBasis or Numeric with mode indices, this function uses those indices to extract the corresponding element from the tuple using hierarchical indexing. If the basis is not a ScaledBasis or has no modes, returns the tuple, layout, or composed layout as-is.

#### Parameters

- **basis** ([*ScaledBasis*](#cutlass.cute.ScaledBasis)) -- The basis element (ScaledBasis)

- **t** (*Union\[XTuple,* *Layout,* *ComposedLayout\]*) -- The tuple, layout, or composed layout to index into

#### Returns

The element at the position specified by the basis modes, or t itself

#### Return type

Union\[XTuple, Layout, ComposedLayout\]

**Examples:**

    >>> basis_get(ScaledBasis(2, 1), (10, 20, 30))
    20
    >>> basis_get(ScaledBasis(2, [0, 1]), ((10, 20), (30, 40)))
    20
    >>> basis_get(5, (10, 20, 30))  # Non-basis returns tuple as-is
    (10, 20, 30)

### flatten_to_tuple

**Kind:** function

**Qualified name:** `cutlass.cute.flatten_to_tuple`

**Signature:** `flatten_to_tuple(a: cutlass.cute.typing.XTuple) → Tuple[Any, ...]`

Flattens a potentially nested tuple structure into a flat tuple.

This function recursively traverses the input structure and flattens it into a single-level tuple, preserving the order of elements.

#### Parameters

**a** (*Union\[IntTuple,* *Coord,* *Shape,* *Stride\]*) -- The structure to flatten

#### Returns

A flattened tuple containing all elements from the input

#### Return type

tuple

**Examples:**

    flatten_to_tuple((1, 2, 3))       # Returns (1, 2, 3)
    flatten_to_tuple(((1, 2), 3))     # Returns (1, 2, 3)
    flatten_to_tuple((1, (2, (3,))))  # Returns (1, 2, 3)

### unflatten

**Kind:** function

**Qualified name:** `cutlass.cute.unflatten`

**Signature:** `unflatten(sequence: Tuple[Any, ...] | List[Any] | Iterable[Any], profile: cutlass.cute.typing.XTuple) → cutlass.cute.typing.XTuple`

Unflatten a flat tuple into a nested tuple structure according to a profile.

This function transforms a flat sequence of elements into a nested tuple structure that matches the structure defined by the profile parameter. It traverses the profile structure and populates it with elements from the sequence.

sequence must be long enough to fill the profile. Raises RuntimeError if it is not.

#### Parameters

- **sequence** (*Union\[Tuple\[Any,* *\...\],* *List\[Any\],* *Iterable\[Any\]\]*) -- A flat sequence of elements to be restructured

- **profile** (*XTuple*) -- A nested tuple structure that defines the shape of the output

#### Returns

A nested tuple with the same structure as profile but containing elements from sequence

#### Return type

XTuple

**Examples:**

    unflatten([1, 2, 3, 4], ((0, 0), (0, 0)))  # Returns ((1, 2), (3, 4))

### product

**Kind:** function

**Qualified name:** `cutlass.cute.product`

**Signature:** `product(a: cutlass.cute.typing.IntTuple | cutlass.cute.typing.Shape) → cutlass.cute.typing.IntTuple`

### product_like

**Kind:** function

**Qualified name:** `cutlass.cute.product_like`

**Signature:** `product_like(a: cutlass.cute.typing.IntTuple, target_profile: cutlass.cute.typing.XTuple) → cutlass.cute.typing.IntTuple`

Return product of the given IntTuple or Shape at leaves of target_profile.

This function computes products according to the structure defined by target_profile.

#### Parameters

- **a** (*IntTuple* *or* *Shape*) -- The input tuple or shape

- **target_profile** (*XTuple*) -- The profile that guides how products are computed

- **loc** (*optional*) -- Source location for MLIR, defaults to None

- **ip** (*optional*) -- Insertion point, defaults to None

#### Returns

The resulting tuple with products computed according to target_profile

#### Return type

IntTuple or Shape

#### Raises

- **TypeError** -- If inputs have incompatible types

- **ValueError** -- If inputs have incompatible shapes

### product_each

**Kind:** function

**Qualified name:** `cutlass.cute.product_each`

**Signature:** `product_each(a: cutlass.cute.typing.IntTuple) → cutlass.cute.typing.IntTuple`

### elem_less

**Kind:** function

**Qualified name:** `cutlass.cute.elem_less`

**Signature:** `elem_less(lhs: cutlass.cute.typing.Shape | cutlass.cute.typing.IntTuple | cutlass.cute.typing.Coord, rhs: cutlass.cute.typing.Shape | cutlass.cute.typing.IntTuple | cutlass.cute.typing.Coord) → cutlass.cute.typing.Boolean`

### tuple_cat

**Kind:** function

**Qualified name:** `cutlass.cute.tuple_cat`

**Signature:** `tuple_cat(*tuples: cutlass.cute.typing.XTuple) → Tuple[Any, ...]`

Concatenate multiple tuples into a single tuple.

This function takes any number of tuples and concatenates them into a single tuple. Non-tuple arguments are treated as single-element tuples.

#### Parameters

**tuples** (*tuple* *or* *any*) -- Variable number of tuples to concatenate

#### Returns

A single concatenated tuple

#### Return type

tuple

**Examples:**

    >>> tuple_cat((1, 2), (3, 4))
    (1, 2, 3, 4)
    >>> tuple_cat((1,), (2, 3), (4,))
    (1, 2, 3, 4)
    >>> tuple_cat(1, (2, 3))
    (1, 2, 3)

### transform_apply

**Kind:** function

**Qualified name:** `cutlass.cute.transform_apply`

**Signature:** `transform_apply(*args: cutlass.cute.typing.XTuple, f: Callable[[...], cutlass.cute.typing.XTuple], g: Callable[[...], cutlass.cute.typing.XTuple]) → cutlass.cute.typing.XTuple`

Transform elements of tuple(s) with f, then apply g to all results.

This function applies f to corresponding elements across input tuple(s), then applies g to all transformed results. It mimics the C++ CuTe implementation.

Supports multiple signatures:

- transform_apply(t, f, g): For single tuple, computes g(f(t\[0\]), f(t\[1\]), ...)

- transform_apply(t0, t1, f, g): For two tuples, computes g(f(t0\[0\], t1\[0\]), f(t0\[1\], t1\[1\]), ...)

- transform_apply(t0, t1, t2, ..., f, g): For multiple tuples of same length

For non-tuple inputs, f is applied to the input(s) and g is applied to that single result.

#### Parameters

- **args** -- One or more tuples (or non-tuples) to transform

- **f** (*Callable*) -- The function to apply to each element (or corresponding elements across tuples)

- **g** (*Callable*) -- The function to apply to all transformed elements

- **loc** (*optional*) -- Source location for MLIR, defaults to None

- **ip** (*optional*) -- Insertion point, defaults to None

#### Returns

The result of applying g to all transformed elements

#### Return type

any

**Examples:**

    >>> transform_apply((1, 2, 3), f=lambda x: x * 2, g=lambda *args: sum(args))
    12  # (1*2 + 2*2 + 3*2) = 12
    >>> transform_apply((1, 2), f=lambda x: (x, x+1), g=tuple_cat)
    (1, 2, 2, 3)
    >>> transform_apply((1, 2), (3, 4), f=lambda x, y: x + y, g=lambda *args: args)
    (4, 6)

### filter_tuple

**Kind:** function

**Qualified name:** `cutlass.cute.filter_tuple`

**Signature:** `filter_tuple(*args: cutlass.cute.typing.XTuple, f: Callable[[...], Tuple[Any, ...]]) → Tuple[Any, ...]`

Filter and flatten tuple elements by applying a function.

The function f should return tuples, which are then concatenated together to produce the final result. This is useful for filtering and transforming tuple structures in a single pass.

#### Parameters

- **t** (*Union\[tuple,* *ir.Value,* *int\]*) -- The tuple to filter

- **f** (*Callable*) -- The function to apply to each element of t

- **loc** (*optional*) -- Source location for MLIR, defaults to None

- **ip** (*optional*) -- Insertion point, defaults to None

#### Returns

A concatenated tuple of all results

#### Return type

tuple

**Examples:**

    >>> # Keep only even numbers, wrapped in tuples
    >>> filter_tuple((1, 2, 3, 4), lambda x: (x,) if x % 2 == 0 else ())
    (2, 4)
    >>> # Duplicate each element
    >>> filter_tuple((1, 2, 3), lambda x: (x, x))
    (1, 1, 2, 2, 3, 3)

### unwrap

**Kind:** function

**Qualified name:** `cutlass.cute.unwrap`

**Signature:** `unwrap(x: cutlass.cute.typing.XTuple) → cutlass.cute.typing.XTuple`

Unwraps the input tuple if it is a single-element tuple, otherwise returns the input.

Example: \>\>\> unwrap((1,)) 1 \>\>\> unwrap(((1, 2, 3),)) (1, 2, 3) \>\>\> unwrap((1, 2, 3)) (1, 2, 3)

### wrap

**Kind:** function

**Qualified name:** `cutlass.cute.wrap`

**Signature:** `wrap(x: cutlass.cute.typing.XTuple) → Tuple[Any, ...]`

Wraps the input into a tuple if not a tuple.

### domain_offset

**Kind:** function

**Qualified name:** `cutlass.cute.domain_offset`

**Signature:** `domain_offset(coord: cutlass.cute.typing.Coord, tensor: cutlass.cute.typing.Tensor) → cutlass.cute.typing.Tensor`

Offset the tensor domain by the given coordinate.

This function creates a new tensor by offsetting the iterator/pointer of the input tensor by the amount corresponding to the given coordinate in its layout.

#### Parameters

- **coord** (*Coord*) -- The coordinate offset to apply

- **tensor** (*Tensor*) -- The source tensor to offset

- **loc** (*Optional\[Location\]*) -- Source location for MLIR operation tracking, defaults to None

- **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for MLIR operation, defaults to None

#### Returns

A new tensor with the offset iterator

#### Return type

Tensor

#### Raises

**ValueError** -- If the tensor type doesn't support domain offsetting

**Examples:**

    # Create a tensor with a row-major layout
    ptr = make_ptr(Float32, base_ptr, AddressSpace.gmem)
    layout = make_layout((64, 128), stride=(128, 1))
    tensor = make_tensor(ptr, layout)

    # Offset by coordinate (3, 5)
    offset_tensor = domain_offset((3, 5), tensor)
    # offset_tensor now points to element at (3, 5)

### make_atom

**Kind:** function

**Qualified name:** `cutlass.cute.make_atom`

**Signature:** `make_atom(ty: _install_cutlass_mlir_autodoc_stub.<locals>._DocMlirType, values: ~typing.List[ir.Value] | None = None) → _MockObject`

This is a wrapper around the \_cute_ir.make_atom operation, providing default value for the values argument.

### make_mma_atom

**Kind:** function

**Qualified name:** `cutlass.cute.make_mma_atom`

**Signature:** `make_mma_atom(op: ~cutlass.cute.atom.MmaOp, *, **kwargs: ~typing.Any) → MmaAtom`

Makes an MMA Atom from an MMA Operation.

This function creates an MMA Atom from a given MMA Operation. Arbitrary kw arguments can be provided for Op-specific additional parameters. They are not used as of today.

#### Parameters

**op** (*MmaOp*) -- The MMA Operation to construct an Atom for

#### Returns

The MMA Atom

#### Return type

[MmaAtom](#cutlass.cute.MmaAtom)

### make_tiled_mma

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_mma`

**Signature:** `make_tiled_mma(op_or_atom: ~cutlass.cute.atom.Op | ~cutlass.cute.atom.MmaAtom, atom_layout_mnk: cutlass.cute.typing.Layout | ~typing.Tuple[~typing.Any, ...] = (1, 1, 1), permutation_mnk: cutlass.cute.typing.Tiler | None = None, *, **kwargs: ~typing.Any) → TiledMma`

Makes a tiled MMA from an MMA Operation or an MMA Atom.

#### Parameters

- **op_or_atom** (*Union\[Op,* [*MmaAtom*](#cutlass.cute.MmaAtom)*\]*) -- The MMA Operation or Atom

- **atom_layout_mnk** (*Layout*) -- A Layout describing the tiling of Atom across threads

- **permutation_mnk** (*Tiler*) -- A permutation Tiler describing the tiling of Atom across values including any permutation of such tiling

#### Returns

The resulting tiled MMA

#### Return type

[TiledMma](#cutlass.cute.TiledMma)

### make_copy_atom

**Kind:** function

**Qualified name:** `cutlass.cute.make_copy_atom`

**Signature:** `make_copy_atom(op: ~cutlass.cute.atom.CopyOp, copy_internal_type: ~typing.Type[cutlass.cute.typing.Numeric], *, **kwargs: ~typing.Any) → CopyAtom`

Makes a Copy Atom from a Copy Operation.

This function creates a Copy Atom from a given Copy Operation. Arbitrary kw arguments can be provided for Op-specific additional parameters.

Example:

    op = cute.nvgpu.CopyUniversalOp()
    atom = cute.make_copy_atom(op, tensor_dtype, num_bits_per_copy=64)

#### Parameters

- **op** (*CopyOp*) -- The Copy Operation to construct an Atom for

- **copy_internal_type** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Element type used to construct the source/destination layouts in units of tensor elements

#### Returns

The Copy Atom

#### Return type

[CopyAtom](#cutlass.cute.CopyAtom)

### make_tiled_copy_tv

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_tv`

**Signature:** `make_tiled_copy_tv(atom: CopyAtom, thr_layout: cutlass.cute.typing.Layout, val_layout: cutlass.cute.typing.Layout) → TiledCopy`

Create a tiled copy given separate thread and value layouts.

A TV partitioner is inferred based on the input layouts. The input thread layout must be compact.

#### Parameters

- **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

- **thr_layout** (*Layout*) -- Layout mapping from `(TileM,TileN)` coordinates to thread IDs (must be compact)

- **val_layout** (*Layout*) -- Layout mapping from `(ValueM,ValueN)` coordinates to value IDs

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

#### Returns

A tiled copy for the partitioner

#### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

### make_tiled_copy

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy`

**Signature:** `make_tiled_copy(atom: CopyAtom, layout_tv: cutlass.cute.typing.Layout, tiler_mn: Any) → TiledCopy`

Create a tiled type given a TV partitioner and tiler.

#### Parameters

- **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom, e.g. smit_copy and simt_async_copy, tma_load, etc.

- **layout_tv** (*Layout*) -- Thread-value layout

- **tiler_mn** (*Tiler*) -- Tile size

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

#### Returns

A tiled copy for the partitioner

#### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

### make_tiled_copy_S

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_S`

**Signature:** `make_tiled_copy_S(atom: CopyAtom, tiled_copy: TiledCopy) → TiledCopy`

Create a tiled copy out of the copy_atom that matches the Src-Layout of tiled_copy.

#### Parameters

- **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

- **tiled_copy** ([*TiledCopy*](#cutlass.cute.TiledCopy)) -- Tiled copy

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

#### Returns

A tiled copy for the partitioner

#### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

### make_tiled_copy_D

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_D`

**Signature:** `make_tiled_copy_D(atom: CopyAtom, tiled_copy: TiledCopy) → TiledCopy`

Create a tiled copy out of the copy_atom that matches the Dst-Layout of tiled_copy.

#### Parameters

- **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

- **tiled_copy** ([*TiledCopy*](#cutlass.cute.TiledCopy)) -- Tiled copy

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

#### Returns

A tiled copy for the partitioner

#### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

### make_tiled_copy_A

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_A`

**Signature:** `make_tiled_copy_A(atom: CopyAtom, tiled_mma: TiledMma) → TiledCopy`

Create a tiled copy out of the copy_atom that matches the A-Layout of tiled_mma.

#### Parameters

- **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

- **tiled_mma** ([*TiledMma*](#cutlass.cute.TiledMma)) -- Tiled MMA

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

#### Returns

A tiled copy for the partitioner

#### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

### make_tiled_copy_B

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_B`

**Signature:** `make_tiled_copy_B(atom: CopyAtom, tiled_mma: TiledMma) → TiledCopy`

Create a tiled copy out of the copy_atom that matches the B-Layout of tiled_mma.

#### Parameters

- **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

- **tiled_mma** ([*TiledMma*](#cutlass.cute.TiledMma)) -- Tiled MMA

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

#### Returns

A tiled copy for the partitioner

#### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

### make_tiled_copy_C

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_C`

**Signature:** `make_tiled_copy_C(atom: CopyAtom, tiled_mma: TiledMma) → TiledCopy`

Create a tiled copy out of the copy_atom that matches the C-Layout of tiled_mma.

#### Parameters

- **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

- **tiled_mma** ([*TiledMma*](#cutlass.cute.TiledMma)) -- Tiled MMA

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

#### Returns

A tiled copy for the partitioner

#### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

### make_tiled_copy_C_atom

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_C_atom`

**Signature:** `make_tiled_copy_C_atom(atom: CopyAtom, mma: TiledMma) → TiledCopy`

Create the smallest tiled copy that can retile LayoutC_TV for use with pipelined epilogues with subtiled stores.

#### Parameters

- **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

- **mma** ([*TiledMma*](#cutlass.cute.TiledMma)) -- Tiled MMA

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

#### Returns

A tiled copy for partitioner

#### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

#### Raises

**ValueError** -- If the number value of CopyAtom's source layout is greater than the size of TiledMma's LayoutC_TV

### make_cotiled_copy

**Kind:** function

**Qualified name:** `cutlass.cute.make_cotiled_copy`

**Signature:** `make_cotiled_copy(atom: CopyAtom, atom_layout_tv: cutlass.cute.typing.Layout, data_layout: cutlass.cute.typing.Layout) → TiledCopy`

Produce a TiledCopy from thread and value offset maps. The TV Layout maps threads and values to the codomain of the data_layout. It is verified that the intended codomain is valid within data_layout. Useful when threads and values don't care about owning specific coordinates, but care more about the vector-width and offsets between them.

#### Parameters

- **atom** (*copy atom,* *e.g. simt_copy and simt_async_copy,* *tgen05.st,* *etc.*)

- **atom_layout_tv** (*(tid,* *vid)* *-\> data addr*)

- **data_layout** (*data coord -\> data addr*)

- **loc** (*source location for mlir* *(optional)*)

- **ip** (*insertion point* *(optional)*)

#### Returns

A tuple of A tiled copy and atom

#### Return type

tiled_copy

### copy_atom_call

**Kind:** function

**Qualified name:** `cutlass.cute.copy_atom_call`

**Signature:** `copy_atom_call(atom: CopyAtom, src: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...], dst: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...], *, pred: cutlass.cute.typing.Tensor | None = None, **kwargs: Any) → None`

Execute a single copy atom operation.

The copy_atom_call operation executes a copy atom with the given operands. Source and destination tensors have layout profile `(V)`.

The `V-mode` represents either:

- A singular mode directly consumable by the provided Copy Atom

- A composite mode requiring recursive decomposition, structured as `(V, Rest...)`,

For src/dst layout like `(V, Rest...)`, the layout profile of `pred` must match `(Rest...)`.

- Certain Atoms may require additional operation-specific keyword arguments.

- Current implementation limits `V-mode` rank to 2 or less. Support for higher ranks is planned for future releases.

Both `src` and `dst` operands are variadic, containing a variable number of tensors:

- For regular copy, `src` and `dst` each contain a single tensor.

- For copy with auxiliary operands, they contain the main tensor followed by auxiliary tensors. For example:

  - For static load to tensor memory, `dst` = \[data, stat\].

  - For SPARSIFY, `dst` = \[data, metadata\].

  - For TMA gather4, `src` = \[data_coord_tensor, index_tensor\].

  - For TMA scatter4, `dst` = \[dst_coord_tensor, index_tensor\].

#### Parameters

- **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom specifying the transfer operation

- **src** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- Source tensor(s) with layout profile `(V)`. Can be a single Tensor or a list/tuple of Tensors for operations with auxiliary source operands.

- **dst** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- Destination tensor(s) with layout profile `(V)`. Can be a single Tensor or a list/tuple of Tensors for operations with auxiliary destination operands.

- **pred** (*Optional\[Tensor\],* *optional*) -- Optional predication tensor for conditional transfers, defaults to None

- **loc** (*Any,* *optional*) -- Source location information, defaults to None

- **ip** (*Any,* *optional*) -- Insertion point, defaults to None

- **kwargs** (*Dict\[str,* *Any\]*) -- Additional copy atom specific arguments

#### Raises

**TypeError** -- If source and destination element type bit widths differ

#### Returns

None

#### Return type

None

**Examples**:

    # Regular copy atom operation
    cute.copy_atom_call(copy_atom, src, dst)

    # Predicated copy atom operation
    cute.copy_atom_call(copy_atom, src, dst, pred=pred)

    # Static load to tensor memory: load with row-wise reduction (MAX, MIN, MAXABS, MINABS)
    cute.copy_atom_call(loadtm_stat_atom, src, [data, stat])

    # TMA gather4: coord tensor plus four gather indices from index tensor
    cute.copy_atom_call(tma_gather4_atom, [data_coord_tensor, index_tensor], dst)

### mma_atom_call

**Kind:** function

**Qualified name:** `cutlass.cute.mma_atom_call`

**Signature:** `mma_atom_call(atom: ~cutlass.cute.atom.MmaAtom, d: cutlass.cute.typing.Tensor, a: cutlass.cute.typing.Tensor | ~typing.List[cutlass.cute.typing.Tensor] | ~typing.Tuple[cutlass.cute.typing.Tensor, ...], b: cutlass.cute.typing.Tensor | ~typing.List[cutlass.cute.typing.Tensor] | ~typing.Tuple[cutlass.cute.typing.Tensor, ...], c: cutlass.cute.typing.Tensor, *, **kwargs: ~typing.Any) → None`

Execute a single MMA atom operation.

The mma_atom_call operation executes an MMA atom with the given operands. This performs a matrix multiplication and accumulation operation: D = A \* B + C

Note: The tensors 'd', 'a', 'b', and 'c' must only have a single fragment.

The operands a and b are variadic, each containing a variable number of tensors:

- For regular MMA, a and b contain the MMA A and B tensors respectively.

- For MMA with auxiliary operands, a and b contain the MMA A and B tensors followed by their respective auxiliary tensors.

Auxiliary operands examples:

- For BlockScaledMMA, a = \[A, SFA\] and b = \[B, SFB\].

- For SparseMMA, a = \[A, E\] and b = \[B\].

- For BlockScaledSparseMMA, a = \[A, SFA, E\] and b = \[B, SFB\].

Runtime keyword arguments in `kwargs` are forwarded to the atom trait's `unpack` logic. For SM100 tcgen05 MMA atoms, you can pass `disable_output_lane` to control per-lane output writes through `tcgen05.mma.disable_output_lane` lowering. The expected mask length is 4 lanes for `cta_group::1` and 8 lanes for `cta_group::2`.

#### Parameters

- **atom** ([*MmaAtom*](#cutlass.cute.MmaAtom)) -- The MMA atom to execute

- **d** (*Tensor*) -- Destination tensor (output accumulator)

- **a** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- A tensor or list of tensors containing the MMA A tensor and optional auxiliary tensors

- **b** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- B tensor or list of tensors containing the MMA B tensor and optional auxiliary tensors

- **c** (*Tensor*) -- Input accumulator tensor

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

Examples:

    # Regular MMA atom call
    cute.mma_atom_call(mma_atom, d_tensor, a_tensor, b_tensor, c_tensor)

    # Block-scaled MMA atom call
    cute.mma_atom_call(mma_atom, d_tensor, [a_tensor, sfa_tensor],
                      [b_tensor, sfb_tensor], c_tensor)

### basic_copy

**Kind:** function

**Qualified name:** `cutlass.cute.basic_copy`

**Signature:** `basic_copy(src: cutlass.cute.typing.Tensor, dst: cutlass.cute.typing.Tensor) → None`

Performs a basic element-wise copy.

This functions **assumes** the following pre-conditions: 1. size(src) == size(dst)

When the src and dst shapes are static, the pre-conditions are actually verified and the element-wise loop is fully unrolled.

#### Parameters

- **src** (*Tensor*) -- Source tensor

- **dst** (*Tensor*) -- Destination tensor

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

### basic_copy_if

**Kind:** function

**Qualified name:** `cutlass.cute.basic_copy_if`

**Signature:** `basic_copy_if(pred: cutlass.cute.typing.Tensor, src: cutlass.cute.typing.Tensor, dst: cutlass.cute.typing.Tensor) → None`

Performs a basic predicated element-wise copy.

This functions **assumes** the following pre-conditions: 1. size(src) == size(dst) 2. size(src) == size(pred)

When all shapes are static, the pre-conditions are actually verified and the element-wise loop is fully unrolled.

### autovec_copy

**Kind:** function

**Qualified name:** `cutlass.cute.autovec_copy`

**Signature:** `autovec_copy(src: cutlass.cute.typing.Tensor, dst: cutlass.cute.typing.Tensor, *, l1c_evict_priority: ~cutlass.cute.nvgpu.common.CacheEvictionPriority = <CacheEvictionPriority.EVICT_NORMAL>) → None`

Auto-vectorization SIMT copy policy.

Given a source and destination tensors that are statically shaped, this policy figures out the largest safe vector width that the copy instruction can take and performs the copy. Any extra memory attributes are forwarded to the specialized copy op.

### copy

**Kind:** function

**Qualified name:** `cutlass.cute.copy`

**Signature:** `copy(atom: CopyAtom, src: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...], dst: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...], *, pred: cutlass.cute.typing.Tensor | None = None, unroll_factor: int | None = None, **kwargs: Any) → None`

Facilitates data transfer between two tensors conforming to layout profile `(V, Rest...)`.

#### Parameters

- **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom specifying the transfer operation

- **src** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- Source tensor or list of tensors with layout profile `(V, Rest...)`

- **dst** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- Destination tensor or list of tensors with layout profile `(V, Rest...)`

- **pred** (*Optional\[Tensor\],* *optional*) -- Optional predication tensor for conditional transfers, defaults to None

- **unroll_factor** (*Optional\[int\],* *optional*) -- Optional unroll count for loop over Rest... modes, defaults to None for fully unroll when Rest... modes are static

- **loc** (*Any,* *optional*) -- Source location information, defaults to None

- **ip** (*Any,* *optional*) -- Insertion point, defaults to None

- **kwargs** (*Dict\[str,* *Any\]*) -- Additional copy atom specific arguments

#### Raises

- **TypeError** -- If source and destination element type bit widths differ

- **ValueError** -- If source and destination ranks differ

- **ValueError** -- If source and destination mode-1 sizes differ

- **NotImplementedError** -- If `V-mode` rank exceeds 2

#### Returns

None

#### Return type

None

The `V-mode` represents either:

- A singular mode directly consumable by the provided Copy Atom

- A composite mode requiring recursive decomposition, structured as `(V, Rest...)`, and src/dst layout like `((V, Rest...), Rest...)`

The algorithm recursively processes the `V-mode`, decomposing it until reaching the minimum granularity compatible with the provided Copy Atom's requirements.

Source and destination tensors must be partitioned in accordance with the Copy Atom specifications. Post-partitioning, both tensors will exhibit a `(V, Rest...)` layout profile.

The operands src and dst are variadic, each containing a variable number of tensors:

- For regular copy, src and dst contain single source and destination tensors respectively.

- For copy with auxiliary operands, src and dst contain the primary tensors followed by their respective auxiliary tensors.

**Precondition:** The size of mode 1 must be equal for both source and destination tensors: `size(src, mode=[1]) == size(dst, mode=[1])`

**Examples**:

TMA copy operation with multicast functionality:

    cute.copy(tma_atom, src, dst, tma_bar_ptr=mbar_ptr, mcast_mask=mask, cache_policy=policy)

Optional predication is supported through an additional tensor parameter. For partitioned tensors with logical profile `((ATOM_V,ATOM_REST),REST,...)`, the predication tensor must maintain profile compatibility with `(ATOM_REST,REST,...)`.

In general, Copy Atoms requiring single-threaded execution handle thread election internally. The non-tensor bulk copy operations (`CopyBulkG2SOp`, `CopyBulkG2SMulticastOp`, `CopyBulkS2GOp`, `CopyBulkS2GByteMaskOp`, and `CopyBulkS2SOp`) are exceptions: they do not perform thread election.

> **Note.**
>
> - Certain Atoms may require additional operation-specific keyword arguments.
>
> - Current implementation limits `V-mode` rank to 2 or less. Support for higher ranks is planned for future releases.

### prefetch

**Kind:** function

**Qualified name:** `cutlass.cute.prefetch`

**Signature:** `prefetch(atom: CopyAtom, src: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...]) → None`

The Prefetch algorithm.

The "prefetch" expects source tensors to be partitioned according to the provided Copy Atom. Prefetch is used for loading tensors from global memory to L2.

Prefetch accepts Copy Atom but not all are allowed. Currently, only supports TMA prefetch.

For standard TMA modes (tiled, im2col), pass a single GMEM tensor:

    cute.prefetch(tma_prefetch, tAgA)

For 2D `tile::gather4` mode, pass a list `[data_tensor, gmem_index_tensor]` (mirrors `cute.copy()` for the same mode); see `cute.nvgpu.cpasync.tma_partition()` for the gather4 layout conventions:

    cute.prefetch(tma_atom, [tAgA, tAgI])

Pass the whole multi-stage partitioned tensors --- the lowering's internal loop walks every rest-dimension entry, so a single `cute.prefetch` call covers every stage and lives outside any per-stage loop.

Non-tensor bulk copy prefetch requires explicit thread election.

### gemm

**Kind:** function

**Qualified name:** `cutlass.cute.gemm`

**Signature:** `gemm(atom: ~cutlass.cute.atom.MmaAtom, d: cutlass.cute.typing.Tensor, a: cutlass.cute.typing.Tensor | ~typing.List[cutlass.cute.typing.Tensor] | ~typing.Tuple[cutlass.cute.typing.Tensor, ...], b: cutlass.cute.typing.Tensor | ~typing.List[cutlass.cute.typing.Tensor] | ~typing.Tuple[cutlass.cute.typing.Tensor, ...], c: cutlass.cute.typing.Tensor, *, **kwargs: ~typing.Any) → None`

The GEMM algorithm.

Computes `D <- A * B + C` where `C` and `D` can alias. Note that some MMA Atoms (e.g. warpgroup-wide or tcgen05 MMAs) require manually setting an "accumulate" boolean field.

All tensors must be partitioned according to the provided MMA Atom.

For MMA Atoms that require single-threaded execution, the gemm op automatically handles thread election internally. Manual thread selection is not required in such cases.

Following dispatch rules are supported:

- Dispatch \[1\]: (V) x (V) =\> (V) =\> (V,1,1) x (V,1,1) =\> (V,1,1)

- Dispatch \[2\]: (M) x (N) =\> (M,N) =\> (1,M,1) x (1,N,1) =\> (1,M,N)

- Dispatch \[3\]: (M,K) x (N,K) =\> (M,N) =\> (1,M,K) x (1,N,K) =\> (1,M,N)

- Dispatch \[4\]: (V,M) x (V,N) =\> (V,M,N) =\> (V,M,1) x (V,N,1) =\> (V,M,N)

- Dispatch \[5\]: (V,M,K) x (V,N,K) =\> (V,M,N)

The operands a and b are variadic, each containing a variable number of tensors:

- For regular GEMM, a and b contain the GEMM A and B tensors respectively.

- For GEMM with auxiliary operands, a and b contain the GEMM A and B tensors followed by their respective auxiliary tensors.

Auxiliary operands examples:

- For BlockScaledGemm, a = \[A, SFA\] and b = \[B, SFB\].

- For SparseGemm, a = \[A, E\] and b = \[B\].

- For BlockScaledSparseGemm, a = \[A, SFA, E\] and b = \[B, SFB\].

Runtime keyword arguments in `kwargs` are forwarded to the underlying MMA atom trait. For SM100 tcgen05 MMA atoms, `disable_output_lane` provides a per-lane write-disable mask for `tcgen05.mma.disable_output_lane` lowering. The expected lane count is 4 for `cta_group::1` and 8 for `cta_group::2`.

#### Parameters

- **atom** ([*MmaAtom*](#cutlass.cute.MmaAtom)) -- MMA atom

- **d** (*Tensor*) -- Destination tensor (output accumulator)

- **a** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- A tensor or list of tensors containing the GEMM A tensor and optional auxiliary tensors

- **b** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- B tensor or list of tensors containing the GEMM B tensor and optional auxiliary tensors

- **c** (*Tensor*) -- Input accumulator tensor

- **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

- **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point for MLIR, defaults to None

- **kwargs** (*dict*) -- Additional keyword arguments

#### Returns

None

#### Return type

None

### full

**Kind:** function

**Qualified name:** `cutlass.cute.full`

**Signature:** `full(shape: cutlass.cute.typing.Shape, fill_value: ir.Value | int | float | bool | cutlass.cute.typing.Numeric, dtype: Type[cutlass.cute.typing.Numeric]) → TensorSSA`

Return a new TensorSSA of given shape and type, filled with fill_value.

#### Parameters

- **shape** (*tuple*) -- Shape of the new tensor.

- **fill_value** (*scalar*) -- Value to fill the tensor with.

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of the tensor.

#### Returns

Tensor of fill_value with the specified shape and dtype.

#### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

### full_like

**Kind:** function

**Qualified name:** `cutlass.cute.full_like`

**Signature:** `full_like(a: TensorSSA | cutlass.cute.typing.Tensor, fill_value: object, dtype: Type[cutlass.cute.typing.Numeric] | None = None) → TensorSSA`

Return a full TensorSSA with the same shape and type as a given array.

#### Parameters

- **a** (*array_like*) -- The shape and data-type of a define these same attributes of the returned array.

- **fill_value** (*array_like*) -- Fill value.

- **dtype** (*Union\[None,* *Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]\],* *optional*) -- Overrides the data type of the result, defaults to None

#### Returns

Tensor of fill_value with the same shape and type as a.

#### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

> **Note.**
>
> See also
>
> [`empty_like()`](#cutlass.cute.empty_like): Return an empty array with shape and type of input. [`ones_like()`](#cutlass.cute.ones_like): Return an array of ones with shape and type of input. [`zeros_like()`](#cutlass.cute.zeros_like): Return an array of zeros with shape and type of input. [`full()`](#cutlass.cute.full): Return a new array of given shape filled with value.

**Examples:**

    frg = cute.make_rmem_tensor((2, 3), Float32)
    a = frg.load()
    b = cute.full_like(a, 1.0)

### empty_like

**Kind:** function

**Qualified name:** `cutlass.cute.empty_like`

**Signature:** `empty_like(a: TensorSSA | cutlass.cute.typing.Tensor, dtype: Type[cutlass.cute.typing.Numeric] | None = None) → TensorSSA`

Return a new TensorSSA with the same shape and type as a given array, without initializing entries.

#### Parameters

- **a** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- The shape and data-type of a define these same attributes of the returned array.

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\],* *optional*) -- Overrides the data type of the result, defaults to None

#### Returns

Uninitialized tensor with the same shape and type (unless overridden) as a.

#### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

### ones_like

**Kind:** function

**Qualified name:** `cutlass.cute.ones_like`

**Signature:** `ones_like(a: TensorSSA | cutlass.cute.typing.Tensor, dtype: Type[cutlass.cute.typing.Numeric] | None = None) → TensorSSA`

Return a TensorSSA of ones with the same shape and type as a given array.

#### Parameters

- **a** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- The shape and data-type of a define these same attributes of the returned array.

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\],* *optional*) -- Overrides the data type of the result, defaults to None

#### Returns

Tensor of ones with the same shape and type (unless overridden) as a.

#### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

### zeros_like

**Kind:** function

**Qualified name:** `cutlass.cute.zeros_like`

**Signature:** `zeros_like(a: TensorSSA | cutlass.cute.typing.Tensor, dtype: Type[cutlass.cute.typing.Numeric] | None = None) → TensorSSA`

Return a TensorSSA of zeros with the same shape and type as a given array.

#### Parameters

- **a** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- The shape and data-type of a define these same attributes of the returned array.

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\],* *optional*) -- Overrides the data type of the result, defaults to None

#### Returns

Tensor of zeros with the same shape and type (unless overridden) as a.

#### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

### where

**Kind:** function

**Qualified name:** `cutlass.cute.where`

**Signature:** `where(cond: TensorSSA, x: TensorSSA | cutlass.cute.typing.Numeric, y: TensorSSA | cutlass.cute.typing.Numeric) → TensorSSA`

Return elements chosen from x or y depending on condition; will auto broadcast x or y if needed.

#### Parameters

- **cond** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- Where True, yield x, where False, yield y.

- **x** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* [*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Values from which to choose when condition is True.

- **y** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* [*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Values from which to choose when condition is False.

#### Returns

A tensor with elements from x where condition is True, and elements from y where condition is False.

#### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

### any\_

**Kind:** function

**Qualified name:** `cutlass.cute.any_`

**Signature:** `any_(x: TensorSSA) → cutlass.cute.typing.Boolean`

Test whether any tensor element evaluates to True.

#### Parameters

**x** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- Input tensor.

#### Returns

Returns a TensorSSA scalar containing True if any element of x is True, False otherwise.

#### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

### all\_

**Kind:** function

**Qualified name:** `cutlass.cute.all_`

**Signature:** `all_(x: TensorSSA) → cutlass.cute.typing.Boolean`

Test whether all tensor elements evaluate to True.

#### Parameters

**x** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- Input tensor.

#### Returns

Returns a TensorSSA scalar containing True if all elements of x are True, False otherwise.

#### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

### union

**Kind:** class

**Qualified name:** `cutlass.cute.union`

**Signature:** `union(cls: type)`

Bases: [`struct`](#cutlass.cute.struct)

Decorator to abstract C union in Python DSL.

Similar to cute.struct, but lays out objects as a union:

- All objects start at offset 0

- The alignment is the maximum alignment of all objects

- The size is the maximum size of all objects

**Usage:**

    # Define a union with scalar int/float elements:
    @cute.union
    class value_union:
        as_int : cutlass.Int32
        as_float : cutlass.Float32

    @cute.union
    class data_union:
        small : cutlass.Int16
        medium : cutlass.Int32
        large : cutlass.Int64

    # Supports alignment for its elements:
    @cute.union
    class aligned_union:
        a: cute.struct.Align[cutlass.Float32, 16]
        b: cute.struct.Align[cutlass.Int32, 8]

    # Statically get size and alignment:
    size = data_union.__sizeof__()
    align = data_union.__alignof__()

    # Allocate and reference elements:
    allocator = cutlass.memory.SmemAllocator()
    value = allocator.allocate(data_union)

    # Access union members (all at the same offset):
    value.small.ptr ...
    value.medium.ptr ...
    value.large.ptr ...

#### Parameters

**cls** -- The union class with annotations.

#### Returns

The decorated union class.

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.union.__init__`

**Signature:** `__init__(cls: type) → None`

Initializes a new cute.union decorator instance.

##### Parameters

**cls** -- The class representing the union data type.

##### Raises

**TypeError** -- If the union is empty.

#### size_in_bytes

**Kind:** method

**Qualified name:** `cutlass.cute.union.size_in_bytes`

**Signature:** `size_in_bytes() → int`

Returns the size of the union in bytes.

##### Returns

The size of the union.

### FastDivmodDivisor

**Kind:** class

**Qualified name:** `cutlass.cute.FastDivmodDivisor`

**Signature:** `FastDivmodDivisor(divisor: cutlass.cute.typing.Integer, is_power_of_2: bool | None = None)`

Bases: `object`

First-class FastDivmod divisor with operator overloading support.

This class wraps a FastDivmod divisor and enables natural Python operator syntax.

Deprecated since version Use: [`FastDivmodDivisorV2`](#cutlass.cute.FastDivmodDivisorV2) instead. V2 additionally carries the scalar divisor across kernel boundaries (2 MLIR values per object instead of 1), so `.divisor` is readable inside kernels; arithmetic is unchanged. This class keeps the legacy 1-value serialization contract for existing integrations.

#### Variables

- **divisor** -- The original divisor value (publicly accessible)

- **\_divisor_mlir** -- The FastDivmod divisor MLIR value (internal)

**Example:**

    quotient, remainder = divmod(dividend, divisor)
    quotient = dividend // divisor
    remainder = dividend % divisor

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.FastDivmodDivisor.__init__`

**Signature:** `__init__(divisor: cutlass.cute.typing.Integer, is_power_of_2: bool | None = None) → None`

Create a FastDivmod divisor for optimized division operations.

##### Parameters

- **divisor** -- The divisor value (should be runtime-dynamic value)

- **is_power_of_2** -- Whether divisor is known to be a power of 2. Defaults to False.

#### divisor

**Kind:** property

**Qualified name:** `cutlass.cute.FastDivmodDivisor.divisor`

**Signature:** `divisor: cutlass.cute.typing.Integer`

Get the original divisor value.

This allows users to access the divisor value that was used to create this FastDivmodDivisor object. This is useful for passing the divisor value to other functions or for storing it in data structures without needing to manually track the divisor value separately.

##### Returns

The original divisor value

##### Return type

[Integer](../basic_data_types.md#cutlass.Integer)

**Example:**

    batch_size = 32
    batch_fdd = cute.fast_divmod_create_divisor(batch_size)
    print(f"Divisor: {batch_fdd.divisor}")  # Access the divisor value
    some_function(divisor=batch_fdd.divisor)  # Pass to other functions

> **Note.**
>
> After this object crosses a kernel boundary (e.g. stored in a params structure passed to a `@cute.kernel`), the returned value still references host-side SSA and fails MLIR region isolation if used inside the kernel (OSS issue #3243). Use [`FastDivmodDivisorV2`](#cutlass.cute.FastDivmodDivisorV2) to read the divisor inside a kernel.

#### \_divisor

**Kind:** property

**Qualified name:** `cutlass.cute.FastDivmodDivisor._divisor`

**Signature:** `_divisor: Value`

### FastDivmodDivisorV2

**Kind:** class

**Qualified name:** `cutlass.cute.FastDivmodDivisorV2`

**Signature:** `FastDivmodDivisorV2(divisor: cutlass.cute.typing.Integer, is_power_of_2: bool | None = None)`

Bases: [`FastDivmodDivisor`](#cutlass.cute.FastDivmodDivisor)

FastDivmod divisor whose `.divisor` property is readable inside kernels.

Same arithmetic behavior as [`FastDivmodDivisor`](#cutlass.cute.FastDivmodDivisor) (`divmod`, `//`, `%`), but serializes **two** MLIR values across region boundaries --- the encoded FastDivmod plus the scalar divisor --- so `.divisor` resolves to in-region SSA after the object crosses a kernel boundary (OSS issue #3243):

    @dataclass
    class Params:
        fdd: cute.FastDivmodDivisorV2

    @cute.kernel
    def kernel(out: cute.Tensor, params: Params):
        out[0] = params.fdd.divisor  # OK: region-local SSA

[`FastDivmodDivisor`](#cutlass.cute.FastDivmodDivisor) keeps the legacy 1-value serialization contract for backward compatibility; its `.divisor` is not readable inside a kernel.

### fast_divmod_create_divisor

**Kind:** function

**Qualified name:** `cutlass.cute.fast_divmod_create_divisor`

**Signature:** `fast_divmod_create_divisor(divisor: cutlass.cute.typing.Integer) → FastDivmodDivisor`

Create a FastDivmod divisor for optimized division operations.

This function creates a FastDivmod divisor that precomputes auxiliary values to enable fast division and modulus operations without using division instructions.

The returned FastDivmodDivisor object supports natural Python operator syntax.

#### Parameters

**divisor** ([*Integer*](../basic_data_types.md#cutlass.Integer)) -- The divisor value (should be runtime-dynamic value)

#### Returns

FastDivmodDivisor object with operator overloading support

#### Return type

[FastDivmodDivisor](#cutlass.cute.FastDivmodDivisor)

**Example:**

    divisor = fast_divmod_create_divisor(batch_size)
    quotient, remainder = divmod(linear_idx, divisor)
    quotient = linear_idx // divisor
    remainder = linear_idx % divisor

### fast_divmod_create_divisor_v2

**Kind:** function

**Qualified name:** `cutlass.cute.fast_divmod_create_divisor_v2`

**Signature:** `fast_divmod_create_divisor_v2(divisor: cutlass.cute.typing.Integer) → FastDivmodDivisorV2`

Create a FastDivmod divisor whose `.divisor` is readable inside kernels.

Behaves like [`fast_divmod_create_divisor()`](#cutlass.cute.fast_divmod_create_divisor), but the returned [`FastDivmodDivisorV2`](#cutlass.cute.FastDivmodDivisorV2) serializes both the encoded FastDivmod and the scalar divisor across kernel boundaries, so `.divisor` resolves to region-local SSA inside a kernel (OSS issue #3243).

#### Parameters

**divisor** ([*Integer*](../basic_data_types.md#cutlass.Integer)) -- The divisor value (should be runtime-dynamic value)

#### Returns

FastDivmodDivisorV2 object with operator overloading support

#### Return type

[FastDivmodDivisorV2](#cutlass.cute.FastDivmodDivisorV2)

**Example:**

    divisor = fast_divmod_create_divisor_v2(batch_size)
    quotient, remainder = divmod(linear_idx, divisor)
    d = divisor.divisor  # readable on host AND inside kernels

### RoundingMode

**Kind:** class

**Qualified name:** `cutlass.cute.RoundingMode`

**Signature:** `RoundingMode(value)`

Bases: `str`, `Enum`

IEEE 754 rounding modes for floating-point operations.

#### NEAREST_EVEN

**Kind:** attribute

**Qualified name:** `cutlass.cute.RoundingMode.NEAREST_EVEN`

**Signature:** `NEAREST_EVEN = 'rn'`

#### ZERO

**Kind:** attribute

**Qualified name:** `cutlass.cute.RoundingMode.ZERO`

**Signature:** `ZERO = 'rz'`

#### UP

**Kind:** attribute

**Qualified name:** `cutlass.cute.RoundingMode.UP`

**Signature:** `UP = 'rp'`

#### DOWN

**Kind:** attribute

**Qualified name:** `cutlass.cute.RoundingMode.DOWN`

**Signature:** `DOWN = 'rm'`

### sin

**Kind:** function

**Qualified name:** `cutlass.cute.sin`

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

**Qualified name:** `cutlass.cute.cos`

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

**Qualified name:** `cutlass.cute.exp2`

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

**Qualified name:** `cutlass.cute.log2`

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

**Qualified name:** `cutlass.cute.tanh`

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

**Qualified name:** `cutlass.cute.rsqrt`

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

**Qualified name:** `cutlass.cute.sqrt`

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

**Qualified name:** `cutlass.cute.exp`

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

**Qualified name:** `cutlass.cute.log`

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

**Qualified name:** `cutlass.cute.log10`

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

**Qualified name:** `cutlass.cute.tan`

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

**Qualified name:** `cutlass.cute.acos`

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

**Qualified name:** `cutlass.cute.asin`

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

**Qualified name:** `cutlass.cute.atan`

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

**Qualified name:** `cutlass.cute.atan2`

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

**Qualified name:** `cutlass.cute.sinh`

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

**Qualified name:** `cutlass.cute.cosh`

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

**Qualified name:** `cutlass.cute.acosh`

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

**Qualified name:** `cutlass.cute.asinh`

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

**Qualified name:** `cutlass.cute.atanh`

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

**Qualified name:** `cutlass.cute.erf`

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

**Qualified name:** `cutlass.cute.erfc`

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

**Qualified name:** `cutlass.cute.pow`

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

**Qualified name:** `cutlass.cute.fpowi`

**Signature:** `fpowi(base: Numeric | Vector | float | int | bool, exponent: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Float raised to integer power: base\^exponent (exponent is integer).

### ipowi

**Kind:** function

**Qualified name:** `cutlass.cute.ipowi`

**Signature:** `ipowi(base: Numeric | Vector | float | int | bool, exponent: Numeric | Vector | float | int | bool) → Numeric | Vector | float | int | bool`

Integer raised to integer power: base\^exponent.

### cbrt

**Kind:** function

**Qualified name:** `cutlass.cute.cbrt`

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

**Qualified name:** `cutlass.cute.expm1`

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

**Qualified name:** `cutlass.cute.log1p`

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

**Qualified name:** `cutlass.cute.sincos`

**Signature:** `sincos(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → tuple`

Combined sine and cosine: returns (sin(x), cos(x)).

More efficient than separate sin() and cos() calls.

### abs

**Kind:** function

**Qualified name:** `cutlass.cute.abs`

**Signature:** `abs(x: Numeric | Vector | float | int | bool, fastmath: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Absolute value (float or integer).

#### Parameters

**ftz** -- Flush denormals to zero (float only, uses nvvm.fabs)

### absi

**Kind:** function

**Qualified name:** `cutlass.cute.absi`

**Signature:** `absi(x: Numeric | Vector | float | int | bool) → Numeric | Vector | float | int | bool`

Integer absolute value.

### copysign

**Kind:** function

**Qualified name:** `cutlass.cute.copysign`

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

**Qualified name:** `cutlass.cute.neg`

**Signature:** `neg(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Floating-point negation.

### ceil

**Kind:** function

**Qualified name:** `cutlass.cute.ceil`

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

**Qualified name:** `cutlass.cute.floor`

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

**Qualified name:** `cutlass.cute.round`

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

**Qualified name:** `cutlass.cute.roundeven`

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

**Qualified name:** `cutlass.cute.trunc`

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

**Qualified name:** `cutlass.cute.clamp`

**Signature:** `clamp(x: Numeric | Vector | float | int | bool, lo: Numeric | Vector | float | int | bool, hi: Numeric | Vector | float | int | bool, fastmath: bool = False, propagate_nan: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Clamp value to range \[lo, hi\].

Lowered unconditionally as `max(lo, min(x, hi))`. The math dialect's `clampf` op currently has no LLVM translation interface registered, so a direct lowering fails at JIT time on scalar f16 inputs (and any other type). Composition via min/max picks up the right per-type lowering (`arith.minnumf` / `arith.maximumf` / `nvvm.fmin` / `nvvm.fmax`).

### min

**Kind:** function

**Qualified name:** `cutlass.cute.min`

**Signature:** `min(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, propagate_nan: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Element-wise minimum.

#### Parameters

- **propagate_nan** -- If True, NaN propagates (IEEE 754 minimum). If False, NaN is ignored (IEEE 754 minimumNumber).

- **ftz** -- Flush denormals to zero (float only, uses nvvm.fmin).

### max

**Kind:** function

**Qualified name:** `cutlass.cute.max`

**Signature:** `max(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, propagate_nan: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Element-wise maximum.

#### Parameters

- **propagate_nan** -- If True, NaN propagates (IEEE 754 maximum). If False, NaN is ignored (IEEE 754 maximumNumber).

- **ftz** -- Flush denormals to zero (float only, uses nvvm.fmax).

### isnan

**Kind:** function

**Qualified name:** `cutlass.cute.isnan`

**Signature:** `isnan(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Test if value is NaN. Returns i1.

### isinf

**Kind:** function

**Qualified name:** `cutlass.cute.isinf`

**Signature:** `isinf(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Test if value is infinite. Returns i1.

### isfinite

**Kind:** function

**Qualified name:** `cutlass.cute.isfinite`

**Signature:** `isfinite(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Test if value is finite. Returns i1.

### isnormal

**Kind:** function

**Qualified name:** `cutlass.cute.isnormal`

**Signature:** `isnormal(x: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Test if value is normal (not zero, subnormal, inf, or NaN). Returns i1.

### fma

**Kind:** function

**Qualified name:** `cutlass.cute.fma`

**Signature:** `fma(a: Numeric | Vector | float | int | bool, b: Numeric | Vector | float | int | bool, c: Numeric | Vector | float | int | bool, fastmath: bool = False, rounding: RoundingMode | None = None, ftz: bool = False) → Numeric | Vector | float | int | bool`

Fused multiply-add: a \* b + c.

### add

**Kind:** function

**Qualified name:** `cutlass.cute.add`

**Signature:** `add(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, fastmath: bool = False, rounding: RoundingMode | None = None, ftz: bool = False) → Numeric | Vector | float | int | bool`

Floating-point addition.

### sub

**Kind:** function

**Qualified name:** `cutlass.cute.sub`

**Signature:** `sub(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Floating-point subtraction.

Note: NVVM sub uses a different intrinsic convention (rounding as an integer arg), so rounding/ftz are not supported here. Callers that need explicit rounding control on subtraction should emit the PTX-level `sub.<rn|rz|rm|rp>[.ftz].f32` intrinsic directly.

### mul

**Kind:** function

**Qualified name:** `cutlass.cute.mul`

**Signature:** `mul(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, fastmath: bool = False, rounding: RoundingMode | None = None, ftz: bool = False) → Numeric | Vector | float | int | bool`

Floating-point multiplication.

### div

**Kind:** function

**Qualified name:** `cutlass.cute.div`

**Signature:** `div(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, fastmath: bool = False, approx: bool = False, ftz: bool = False, full: bool = False, rounding: RoundingMode | None = None) → Numeric | Vector | float | int | bool`

Floating-point division.

### rem

**Kind:** function

**Qualified name:** `cutlass.cute.rem`

**Signature:** `rem(x: Numeric | Vector | float | int | bool, y: Numeric | Vector | float | int | bool, fastmath: bool = False) → Numeric | Vector | float | int | bool`

Floating-point remainder.

### rcp

**Kind:** function

**Qualified name:** `cutlass.cute.rcp`

**Signature:** `rcp(x: Numeric | Vector | float | int | bool, fastmath: bool = False, approx: bool = False, rounding: RoundingMode | None = None, ftz: bool = False) → Numeric | Vector | float | int | bool`

Reciprocal (1/x).

### absf

**Kind:** function

**Qualified name:** `cutlass.cute.absf`

**Signature:** `absf(x: Numeric | Vector | float | int | bool, fastmath: bool = False, ftz: bool = False) → Numeric | Vector | float | int | bool`

Absolute value (float or integer).

#### Parameters

**ftz** -- Flush denormals to zero (float only, uses nvvm.fabs)

### jit

**Kind:** function

**Qualified name:** `cutlass.cute.jit`

**Signature:** `jit(fn = None, *args, **kwargs)`

### kernel

**Kind:** function

**Qualified name:** `cutlass.cute.kernel`

**Signature:** `kernel(fn = None, *args, **kwargs)`

### register_jit_arg_adapter

**Kind:** function

**Qualified name:** `cutlass.cute.register_jit_arg_adapter`

**Signature:** `register_jit_arg_adapter(*_args, **_kwargs)`

### DisableCuteExtCompile

**Kind:** attribute

**Qualified name:** `cutlass.cute.DisableCuteExtCompile`

**Signature:** `DisableCuteExtCompile`

alias of `_MockObject`

### ffi

**Kind:** function

**Qualified name:** `cutlass.cute.ffi`

**Signature:** `ffi(*, name: str | None = None, params_types: list | None = None, return_type: _install_cutlass_mlir_autodoc_stub.<locals>._DocMlirType | None = None, inline: bool = True, source: str | ~cutlass.base_dsl.ffi.BitCode | None = None) → FFI`

### extern

**Kind:** function

**Qualified name:** `cutlass.cute.extern`

**Signature:** `extern(func: ~typing.Any = None, *, name: str | None = None, inline: bool = True, source: ~cutlass.base_dsl.ffi.BitCode | None = None, name_mangler: ~typing.Any = None, overloaded: bool | None = None, implicit_convert: ~typing.Any = <function _implicit_convert>) → Any`

Decorator to mark a function as an external FFI call.

Calls to the function dynamically resolve to a concrete extern function based on runtime argument types.

#### Parameters

- **name** (*str,* *optional*) -- External symbol name. Defaults to Python function's name.

- **inline** (*bool,* *default=True*) -- Whether to mark the function and call sites for inlining.

- **source** ([*BitCode*](#cutlass.cute.BitCode)*,* *optional*) -- External bitcode file to link (e.g., BitCode("lib.bc")).

- **name_mangler** (*callable,* *optional*) -- Custom name mangling function. Defaults to default_name_mangler.

- **overloaded** (*bool,* *optional*) -- Whether to enable name mangling. Auto-detected if None (True if multiple \@overload variants or non-concrete signature).

- **implicit_convert** (*callable,* *optional*) -- Custom callback for implicit type conversions (signature: (arg, typ) -\> arg).

#### Return type

A callable that dynamically dispatches to the correct FFI overload.

Examples

Basic usage:

    >>> `@extern`
    ... def my_func(x: Int32) -> Float32:
    ...     ...

With bitcode linking:

    >>> `@extern`(source=BitCode("mylib.bc"))
    ... def external_sqrt(x: Float32) -> Float32:
    ...     ...

Multiple overloads:

    >>> `@extern`
    ... `@overload`
    ... def compute(x: Int32) -> Int32:
    ...     ...
    >>> `@overload`
    ... def compute(x: Float32) -> Float32:
    ...     ...

TypeVar-based generic:

    >>> T = TypeVar('T')
    >>> `@extern`
    ... def identity(x: T) -> T:
    ...     ...

### BitCode

**Kind:** class

**Qualified name:** `cutlass.cute.BitCode`

**Signature:** `BitCode(path: str)`

Bases: `object`

Specifies an external bitcode file to link when compiling.

#### path

**Kind:** attribute

**Qualified name:** `cutlass.cute.BitCode.path`

**Signature:** `path`

Filesystem path to the .bc (LLVM bitcode) file.

##### Type

str

#### id0

**Kind:** attribute

**Signature:** `path: str`

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.BitCode.__init__`

**Signature:** `__init__(path: str) → None`

### ConstValue

**Kind:** class

**Qualified name:** `cutlass.cute.ConstValue`

**Signature:** `ConstValue(types: tuple[_install_cutlass_mlir_autodoc_stub.<locals>._DocMlirType], value: ~typing.Any)`

Bases: `object`

Represents a constant value and its MLIR types

#### types

**Kind:** attribute

**Qualified name:** `cutlass.cute.ConstValue.types`

**Signature:** `types: tuple[_install_cutlass_mlir_autodoc_stub.<locals>._DocMlirType]`

#### value

**Kind:** attribute

**Qualified name:** `cutlass.cute.ConstValue.value`

**Signature:** `value: Any`

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.ConstValue.__init__`

**Signature:** `__init__(types: tuple[_install_cutlass_mlir_autodoc_stub.<locals>._DocMlirType], value: ~typing.Any) → None`

### mangle

**Kind:** function

**Qualified name:** `cutlass.cute.mangle`

**Signature:** `mangle(name: str) → str`

Mangle a string to be a valid function symbol

## Overview

CuTe DSL provides a set of core types that form the foundation of tensor layout algebra and GPU programming. These types enable precise control over memory layout, data representation, and tensor operations. This document covers CuTe layout-algebra and structured-memory helper types available through `cutlass.cute`.

## Core Numeric Types

### IntValue

`IntValue` is an internal representation of constrained integer types with divisibility information. It serves as a proxy for constrained integer types in the CuTe IR, automatically tracking divisibility constraints that are crucial for layout operations.

**Key Features:**

- Inherits from `ArithValue` with extensions for divisibility tracking

- Automatically emits `cute.get_scalars` operations in the IR

- Supports arithmetic operations that propagate divisibility information

- Used internally for type-safe integer operations in layout algebra

**API Methods:**

- `get_typed_value()` - Returns the value as an IntTupleType

- `get_divisibility()` - Returns the divisibility constraint of the value

- `divisibility` - Property that returns the divisibility constraint

**Supported Operations:**

The `IntValue` type supports standard arithmetic operations with divisibility tracking:

    # Addition, subtraction, multiplication, division, and modulo
    result = int_val1 + int_val2
    result = int_val1 - int_val2
    result = int_val1 * int_val2
    result = int_val1 // int_val2
    result = int_val1 % int_val2

**String Representation:**

    # IntValue with divisibility 1
    str(int_val)  # Returns "?"

    # IntValue with divisibility 4
    str(int_val)  # Returns "?{div=4}"

### Ratio

`Ratio` represents a rational number as a ratio of two integers. It is used in CuTe to represent exact fractional values that arise in tensor layout operations, particularly in composition operations where divisibility conditions may not be satisfied.

**Constructor:**

    ratio = cute.Ratio(numerator, denominator)

#### param numerator

The numerator of the ratio

#### type numerator

int

#### param denominator

The denominator of the ratio

#### type denominator

int

#### raises TypeError

If numerator or denominator are not integers

**Methods:**

- `is_integral()` - Returns `True` if the ratio represents an integer value (numerator divisible by denominator)

- `reduced()` - Returns a new Ratio with numerator and denominator reduced to lowest terms

- `to(dtype)` - Converts the ratio to another type (Ratio, float, or int)

**Arithmetic Operations:**

    # Multiplication with another ratio
    ratio1 = cute.Ratio(1, 2)
    ratio2 = cute.Ratio(3, 4)
    result = ratio1 * ratio2  # Returns Ratio(3, 8)

    # Multiplication with integer
    ratio = cute.Ratio(2, 3)
    result = ratio * 5  # Returns Ratio(10, 3)
    result = 5 * ratio  # Returns Ratio(10, 3)

#### Parameters

- **Conversion**

<!-- -->

    ratio = cute.Ratio(3, 2)

    # Convert to float
    float_val = ratio.to(float)  # Returns 1.5

    # Convert to int (floor division)
    int_val = ratio.to(int)  # Returns 1

## Layout Algebra Types

### ScaledBasis

`ScaledBasis` represents a scaled basis element in CuTe's layout algebra. It consists of a scale value and a mode that identifies which basis element in the layout algebra is being referenced. ScaledBasis elements are fundamental to CuTe's coordinate system representation.

**Constructor:**

    sb = cute.ScaledBasis(value, mode)

#### param value

The scale value

#### type value

Union\[int, Integer, Ratio, ir.Value\]

#### param mode

The mode identifying the basis element

#### type mode

Union\[int, List\[int\]\]

#### raises TypeError

If mode is not an integer or list of integers

**Examples:**

    # Create a scaled basis with integer scale and mode
    sb1 = cute.ScaledBasis(2, 0)  # 2 * E(0)

    # Create a scaled basis with a Ratio scale
    sb2 = cute.ScaledBasis(cute.Ratio(1, 2), 1)  # (1/2) * E(1)

    # Create a scaled basis with a list of modes
    sb3 = cute.ScaledBasis(4, [0, 1])  # 4 * E([0, 1])

    # Scaled basis elements are commonly used in layout strides
    layout = cute.make_layout((4, 8), stride=(cute.ScaledBasis(2, 0), cute.ScaledBasis(1, 1)))

    # This creates a layout with strides (2@0, 1@1) representing
    # a coordinate system where each dimension has its own basis

    # Example: Mapping coordinates to indices using the layout
    coord = (2, 3)
    idx = cute.crd2idx(coord, layout)  # Maps (2, 3) to (4, 3)

**Properties:**

- `value` - Get the scale value

- `mode` - Get the mode as a list of integers

- `is_static()` - Returns `True` if the value is statically known

**Methods:**

- `to(dtype)` - Convert to another type (ScaledBasis or internal \_ScaledBasis)

**Operations:**

    # Right multiplication by a scale factor
    sb = cute.ScaledBasis(2, 0)
    result = 3 * sb  # Creates ScaledBasis(6, 0)

**Utility Function:**

    # Create a basis element with unit scale
    basis = cute.E(mode)  # Equivalent to ScaledBasis(1, mode)

### Swizzle

`Swizzle` is a transformation that permutes the elements of a layout. Swizzles are used to rearrange data elements to improve memory access patterns and computational efficiency, particularly for avoiding bank conflicts in shared memory.

**Swizzle Parameters:**

A swizzle is defined by three parameters:

- **MBase**: The number of least-significant bits to keep constant

- **BBits**: The number of bits in the mask

- **SShift**: The distance to shift the mask

**Bit Pattern:**

    0bxxxxxxxxxxxxxxxYYYxxxxxxxZZZxxxx
                                  ^--^ MBase (least-sig bits kept constant)
                     ^-^       ^-^     BBits (number of bits in mask)
                       ^---------^     SShift (distance to shift YYY)
                                          (positive: right, negative: left)

    Given:    0bxxxxxxxxxxxxxxxxYYxxxxxxxxxZZxxx
    Result:   0bxxxxxxxxxxxxxxxxYYxxxxxxxxxAAxxx
              where AA = ZZ xor YY

**Usage:**

Swizzles are typically created using CuTe's swizzle factory functions and composed with layouts to create optimized memory access patterns.

### Layout

`Layout` is CuTe's core abstraction for representing tensor layouts. A Layout maps from a logical coordinate space to an index space, defined by a pair of (Shape, Stride). Layouts present a common interface to multidimensional array access that abstracts away the details of how array elements are organized in memory.

**Key Concepts:**

- **Shape**: Defines the abstract dimensions of the Layout

- **Stride**: Defines how coordinates within the Shape map to linear indices

- **Hierarchical Structure**: CuTe layouts are inherently hierarchical, constructed from smaller nested layouts

**Properties:**

- `shape` - An IntTuple representing the dimensions of the layout

- `stride` - An IntTuple representing the strides of the layout

- `max_alignment` - The maximum alignment of the layout in bytes

**Examples:**

    # Creating a layout with shape (4,8) and default stride (column major)
    layout = cute.make_layout((4, 8))

    # Creating a layout with explicit shape and stride (row major)
    layout = cute.make_layout((4, 8), stride=(8, 1))

    # Accessing layout properties
    shape = layout.shape      # Returns (4, 8)
    stride = layout.stride    # Returns (8, 1)

    # Mapping a coordinate to an index: (2, 3) -> 2 * 8 + 3 * 1 = 19
    idx = cute.crd2idx((2, 3), layout)

**Layout Operations:**

Layouts support a rich algebra of operations:

- **Concatenation**: Combining layouts along dimensions

- **Coalescence**: Merging adjacent modes

- **Composition**: Composing layouts with functions or other layouts

- **Complement**: Computing the complement space

- **Inversion**: Inverting the layout mapping

**String Representation:**

    layout = cute.make_layout((4, 8), stride=(1, 4))
    print(layout)  # Prints "shape:stride" format, e.g., "(4,8):(1,4)"

### ComposedLayout

`ComposedLayout` represents a composition of layouts and transformations. It is a generalization of normal layouts that can support arbitrary function mappings from coordinate to coordinate as an inner layout.

**Structure:**

A ComposedLayout consists of three components:

- **inner**: The inner transformation (Swizzle or Layout)

- **offset**: An offset applied to coordinates

- **outer**: The outer layout

**Properties:**

- `inner` - Returns the inner transformation (Union\[Swizzle, Layout\])

- `offset` - Returns the offset as an IntTuple

- `outer` - Returns the outer layout

- `shape` - Returns the shape of the composed layout

- `max_alignment` - Returns the maximum alignment

- `is_normal` - Returns `True` if this is a normal layout (not a general composition)

**Examples:**

    # ComposedLayouts are typically created through composition operations
    # For example, composing a layout with a swizzle
    layout = cute.make_layout((8, 8))
    swizzle = cute.make_swizzle(...)
    composed = cute.composition(swizzle, layout)

    # Accessing components
    inner = composed.inner      # Returns the swizzle
    outer = composed.outer      # Returns the layout
    offset = composed.offset    # Returns the offset

**String Representation:**

    print(composed)  # Prints "inner o offset o outer" format

## Structured Data Types

### struct

The `struct` decorator abstracts C structures in Python DSL. It allows you to define structured data types with precise control over layout, alignment, and nesting.

**Supported Elements:**

- Base DSL scalar int/float elements

- Arrays (MemRange)

- Nested structures

- Aligned elements

**Basic Usage:**

    # Define a simple struct
    @cute.struct
    class complex:
        real : cutlass.Float32
        imag : cutlass.Float32

    # Define a struct with arrays and nested structures
    @cute.struct
    class StorageA:
        mbarA : cute.struct.MemRange[cutlass.Int64, stage]
        compA : complex
        intA : cutlass.Int16

**Alignment Control:**

    # Define a struct with explicit alignment
    @cute.struct
    class StorageB:
        a: cute.struct.Align[
            cute.struct.MemRange[cutlass.Float32, size_a], 1024
        ]
        b: cute.struct.Align[
            cute.struct.MemRange[cutlass.Float32, size_b], 1024
        ]
        x: cute.struct.Align[cutlass.Int32, 16]
        compA: cute.struct.Align[complex, 16]

**Static Queries:**

    # Get size and alignment at compile time
    size = StorageB.__sizeof__()
    align = StorageB.__alignof__()

**Allocation and Access:**

    # Allocate and reference elements
    storage = allocator.allocate(StorageB)

    # Access struct members
    storage.a[0] = ...
    storage.x = ...
    ... = storage.compA.real.ptr
    ... = storage.x.ptr.load()

**Methods:**

- `__sizeof__()` - Returns the size of the struct in bytes

- `__alignof__()` - Returns the alignment of the struct in bytes

- `size_in_bytes()` - Returns the size of the struct in bytes

#### struct.MemRange

`MemRange` defines a contiguous range of memory with a specific element type and size.

**Syntax:**

    cute.struct.MemRange[dtype, size]

##### param dtype

The data type (must be a DSL scalar type)

##### type dtype

Type\[Numeric\]

##### param size

The number of elements in the range

##### type size

int

**Properties:**

- `size` - Number of elements in the range

- `elem_width` - Width of each element in bits

- `size_in_bytes` - Total size in bytes

**Methods:**

- `data_ptr()` - Returns a pointer to the start of the memory range

- `get_tensor(layout, swizzle=None, dtype=None)` - Creates a tensor from the memory range

- `__getitem__(index)` - Returns the element at the specified index

**Examples:**

    @cute.struct
    class Buffer:
        data : cute.struct.MemRange[cutlass.Float32, 128]

    # Allocate buffer
    buf = allocator.allocate(Buffer)

    # Get pointer to data
    ptr = buf.data.data_ptr()

    # Access individual elements
    element = buf.data[5]

    # Create tensor from memory range
    layout = cute.make_layout((8, 16))
    tensor = buf.data.get_tensor(layout)

#### struct.Align

`Align` specifies explicit alignment requirements for struct members.

**Syntax:**

    cute.struct.Align[dtype, alignment]

##### param dtype

The type to align (scalar, MemRange, or struct)

##### type dtype

Type

##### param alignment

The alignment in bytes (must be \> 0)

##### type alignment

int

**Properties:**

- `dtype` - The data type being aligned

- `align` - The alignment value

**Examples:**

    @cute.struct
    class AlignedStorage:
        # Align scalar to 16 bytes
        counter: cute.struct.Align[cutlass.Int32, 16]

        # Align array to 1024 bytes
        buffer: cute.struct.Align[
            cute.struct.MemRange[cutlass.Float32, 256], 1024
        ]

### union

The `union` decorator abstracts C unions in Python DSL. Similar to `struct`, but all members start at offset 0, and the size is the maximum size of all members.

**Layout Characteristics:**

- All objects start at offset 0

- Alignment is the maximum alignment of all objects

- Size is the maximum size of all objects

**Usage:**

    # Define a union with scalar elements
    @cute.union
    class value_union:
        as_int : cutlass.Int32
        as_float : cutlass.Float32

    # Allocate union
    val = allocator.allocate(value_union)

    # Access different interpretations of same memory
    val.as_int = 42
    float_val = val.as_float.ptr.load()  # Interpret same bits as float

**Methods:**

Same as `struct`:

- `__sizeof__()` - Returns the size of the union in bytes

- `__alignof__()` - Returns the alignment of the union in bytes

## Type Hierarchies and Relationships

### Parameters

- **Protocol Support**

Many CuTe types implement standard Python protocols for integration:

- `__str__()` - String representation for debugging

- `__eq__()` / `__ne__()` - Equality comparison

- `__getitem__()` - Indexing operations

- `__add__()` / `__sub__()` / `__mul__()` / `__floordiv__()` / `__mod__()` - Arithmetic

**MLIR Integration:**

Internal types like `IntValue`, `Layout`, and `ComposedLayout` are registered as MLIR value casters, enabling seamless integration with the underlying compiler infrastructure.

## Best Practices

**Memory Alignment:**

- Always specify alignment requirements for shared memory structures to avoid bank conflicts

- Use `struct.Align` to enforce alignment constraints

- Check `max_alignment` properties to verify layout and structured-storage alignment

**Layout Operations:**

- Prefer built-in layout operations (`make_layout`, `composition`, etc.) over manual construction

- Use `ScaledBasis` for explicit control over stride modes in multi-modal layouts

- Leverage `ComposedLayout` for complex transformations like swizzling

## See Also

- [Introduction](../guide/dsl_introduction.md) - Introduction to CuTe DSL decorators and calling conventions

- [Basic Data Types](../basic_data_types.md) - Numeric, pointer, vector, and array API reference

- [Control Flow](../guide/dsl_control_flow.md) - Control flow with static and dynamic values

- [Static vs Dynamic layouts](../guide/dsl_dynamic_layout.md) - Working with static and dynamic layouts

- [JIT Function Argument Generation](../guide/dsl_jit_arg_generation.md) - Type annotations for JIT and kernel arguments

- [Integration with Frameworks](../guides/framework_integration.md) - Integration with deep learning frameworks

- [Debugging](../guides/debugging.md) - Debugging techniques for CuTe DSL programs
