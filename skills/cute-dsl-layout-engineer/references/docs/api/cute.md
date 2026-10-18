---
title: "cutlass.cute"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute.html"
cutlass_docs_version: "4.4.1"
generated_at: "2026-03-07T11:33:21+00:00"
last_updated: "Mar 05, 2026"
---

# cutlass.cute

## Swizzle

**Kind:** class

**Qualified name:** `cutlass.cute.Swizzle`

**Signature:** `Swizzle(*args: Any, **kwargs: Any)`

Bases: `Value`

Swizzle is a transformation that permutes the elements of a layout.

Swizzles are used to rearrange data elements to improve memory access patterns and computational efficiency.

Swizzle is defined by three parameters:

-   MBase: The number of least-significant bits to keep constant

-   BBits: The number of bits in the mask

-   SShift: The distance to shift the mask

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

## struct

**Kind:** class

**Qualified name:** `cutlass.cute.struct`

**Signature:** `struct(cls)`

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
    storage.x ...
    storage.compA.real ...

### Parameters

**cls** -- The struct class with annotations.

### Returns

The decorated struct class.

### \_MemRangeMeta

**Kind:** class

**Qualified name:** `cutlass.cute.struct._MemRangeMeta`

**Signature:** `_MemRangeMeta(name, bases, dct)`

Bases: `type`

A metaclass for creating MemRange classes.

This metaclass is used to dynamically create MemRange classes with specific data types and sizes.

#### Variables

-   **\_dtype** -- The data type of the MemRange.

-   **\_size** -- The size of the MemRange.

#### \_dtype

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct._MemRangeMeta._dtype`

**Signature:** `_dtype = None`

#### \_size

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct._MemRangeMeta._size`

**Signature:** `_size = None`

#### size

**Kind:** property

**Qualified name:** `cutlass.cute.struct._MemRangeMeta.size`

**Signature:** `size`

#### elem_width

**Kind:** property

**Qualified name:** `cutlass.cute.struct._MemRangeMeta.elem_width`

**Signature:** `elem_width`

#### size_in_bytes

**Kind:** property

**Qualified name:** `cutlass.cute.struct._MemRangeMeta.size_in_bytes`

**Signature:** `size_in_bytes`

### MemRange

**Kind:** class

**Qualified name:** `cutlass.cute.struct.MemRange`

**Signature:** `MemRange`

Bases: `object`

Defines a range of memory by MemRange\[T, size\].

### \_MemRangeData

**Kind:** class

**Qualified name:** `cutlass.cute.struct._MemRangeData`

**Signature:** `_MemRangeData(dtype, size, base)`

Bases: `object`

Represents a range of memory.

#### Parameters

-   **dtype** -- The data type.

-   **size** -- The size of the memory range in bytes.

-   **base** -- The base address of the memory range.

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.struct._MemRangeData.__init__`

**Signature:** `__init__(dtype, size, base)`

Initializes a new memory range.

##### Parameters

-   **dtype** -- The data type.

-   **size** -- Size of the memory range in bytes. A size of **0** is accepted, but in that case the range can only be used for its address (e.g. as a partition marker).

-   **base** -- The base address of the memory range.

#### data_ptr

**Kind:** method

**Qualified name:** `cutlass.cute.struct._MemRangeData.data_ptr`

**Signature:** `data_ptr(*, loc = None, ip = None)`

Returns start pointer to the data in this memory range.

##### Returns

A pointer to the start of the memory range.

##### Raises

**AssertionError** -- If the size of the memory range is negative.

#### get_tensor

**Kind:** method

**Qualified name:** `cutlass.cute.struct._MemRangeData.get_tensor`

**Signature:** `get_tensor(layout, swizzle = None, dtype = None, *, loc = None, ip = None)`

Creates a tensor from the memory range.

##### Parameters

-   **layout** -- The layout of the tensor.

-   **swizzle** -- Optional swizzle pattern.

-   **dtype** -- Optional data type; defaults to the memory range's data type if not specified.

##### Returns

A tensor representing the memory range.

##### Raises

-   **TypeError** -- If the layout is incompatible with the swizzle.

-   **AssertionError** -- If the size of the memory range is not greater than zero.

### \_AlignMeta

**Kind:** class

**Qualified name:** `cutlass.cute.struct._AlignMeta`

**Signature:** `_AlignMeta(name, bases, dct)`

Bases: `type`

Aligns the given object by setting its alignment attribute.

#### Parameters

-   **v** -- The object to align. Must be a struct, MemRange, or a scalar type.

-   **align** -- The alignment value to set.

#### Raises

**TypeError** -- If the object is not a struct, MemRange, or a scalar type.

#### Variables

-   **\_dtype** -- The data type to be aligned.

-   **\_align** -- The alignment of the data type.

#### \_dtype

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct._AlignMeta._dtype`

**Signature:** `_dtype = None`

#### \_align

**Kind:** attribute

**Qualified name:** `cutlass.cute.struct._AlignMeta._align`

**Signature:** `_align = None`

#### dtype

**Kind:** property

**Qualified name:** `cutlass.cute.struct._AlignMeta.dtype`

**Signature:** `dtype`

#### align

**Kind:** property

**Qualified name:** `cutlass.cute.struct._AlignMeta.align`

**Signature:** `align`

### Align

**Kind:** class

**Qualified name:** `cutlass.cute.struct.Align`

**Signature:** `Align`

Bases: `object`

Aligns the given type by Align\[T, alignment\].

### \_is_scalar_type

**Kind:** method

**Qualified name:** `cutlass.cute.struct._is_scalar_type`

**Signature:** `static _is_scalar_type(dtype)`

Checks if the given type is a scalar numeric type.

#### Parameters

**dtype** -- The type to check.

#### Returns

True if the type is a subclass of Numeric, False otherwise.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.struct.__init__`

**Signature:** `__init__(cls)`

Initializes a new struct decorator instance.

#### Parameters

**cls** -- The class representing the structured data type.

#### Raises

**TypeError** -- If the struct is empty.

### size_in_bytes

**Kind:** method

**Qualified name:** `cutlass.cute.struct.size_in_bytes`

**Signature:** `size_in_bytes() → int`

Returns the size of the struct in bytes.

#### Returns

The size of the struct.

### align_offset

**Kind:** method

**Qualified name:** `cutlass.cute.struct.align_offset`

**Signature:** `static align_offset(offset, align)`

Return the round-up offset up to the next multiple of align.

## E

**Kind:** function

**Qualified name:** `cutlass.cute.E`

**Signature:** `E(mode: int | List[int]) → ScaledBasis`

Create a unit ScaledBasis element with the specified mode.

This function creates a ScaledBasis with value 1 and the given mode. The mode represents the coordinate axis or dimension in the layout.

### Parameters

**mode** (*Union\[int,* *List\[int\]\]*) -- The mode (dimension) for the basis element, either a single integer or a list of integers

### Returns

A ScaledBasis with value 1 and the specified mode

### Return type

ScaledBasis

### Raises

**TypeError** -- If mode is not an integer or a list

**Examples:**

    # Create a basis element for the first dimension (mode 0)
    e0 = E(0)

    # Create a basis element for the second dimension (mode 1)
    e1 = E(1)

    # Create a basis element for a hierarchical dimension
    e_hier = E([0, 1])

## get_divisibility

**Kind:** function

**Qualified name:** `cutlass.cute.get_divisibility`

**Signature:** `get_divisibility(x: int | cutlass.cute.typing.Integer) → int`

## is_static

**Kind:** function

**Qualified name:** `cutlass.cute.is_static`

**Signature:** `is_static(x: Any) → bool`

Check if a value is statically known at compile time.

In CuTe, static values are those whose values are known at compile time, as opposed to dynamic values which are only known at runtime.

This function checks if a value is static by recursively traversing its type hierarchy and checking if all components are static.

Static values include:

-   Python literals (bool, int, float, None)

-   Static ScaledBasis objects

-   Static ComposedLayout objects

-   Static IR types

-   Tuples containing only static values

Dynamic values include:

-   Numeric objects (representing runtime values)

-   Dynamic expressions

-   Any tuple containing dynamic values

### Parameters

**x** (*Any*) -- The value to check

### Returns

True if the value is static, False otherwise

### Return type

bool

### Raises

**TypeError** -- If an unsupported type is provided

## has_underscore

**Kind:** function

**Qualified name:** `cutlass.cute.has_underscore`

**Signature:** `has_underscore(a: cutlass.cute.typing.XTuple) → bool`

## pretty_str

**Kind:** function

**Qualified name:** `cutlass.cute.pretty_str`

**Signature:** `pretty_str(arg) → str`

Constructs a concise readable pretty string.

## printf

**Kind:** function

**Qualified name:** `cutlass.cute.printf`

**Signature:** `printf(*args, loc = None, ip = None) → None`

Print one or more values with optional formatting.

This function provides printf-style formatted printing capabilities. It can print values directly or format them using C-style format strings. The function supports printing various types including layouts, numeric values, tensors, and other CuTe objects.

The function accepts either: 1. A list of values to print directly 2. A format string followed by values to format

### Parameters

-   **args** (*Any*) -- Variable length argument list containing either: - One or more values to print directly - A format string followed by values to format

-   **loc** (*Optional\[Location\]*) -- Source location information for debugging, defaults to None

-   **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for code generation, defaults to None

### Raises

-   **ValueError** -- If no arguments are provided

-   **TypeError** -- If an unsupported argument type is passed

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

## front

**Kind:** function

**Qualified name:** `cutlass.cute.front`

**Signature:** `front(input, *, loc = None, ip = None)`

Recursively get the first element of input.

This function traverses a hierarchical structure (like a layout or tensor) and returns the first element at the deepest level. It's particularly useful for accessing the first stride value in a layout to determine properties like majorness.

### Parameters

-   **input** (*Union\[Tensor,* *Layout,* *Stride\]*) -- The hierarchical structure to traverse

-   **loc** (*source location,* *optional*) -- Source location where it's called, defaults to None

-   **ip** (*insertion pointer,* *optional*) -- Insertion pointer for IR generation, defaults to None

### Returns

The first element at the deepest level of the input structure

### Return type

Union\[int, float, bool, ir.Value\]

## is_major

**Kind:** function

**Qualified name:** `cutlass.cute.is_major`

**Signature:** `is_major(mode, stride: cutlass.cute.typing.Stride, *, loc = None, ip = None) → bool`

Check whether a mode in stride is the major mode.

## assume

**Kind:** function

**Qualified name:** `cutlass.cute.assume`

**Signature:** `assume(src, divby = None, *, loc = None, ip = None)`

## make_swizzle

**Kind:** function

**Qualified name:** `cutlass.cute.make_swizzle`

**Signature:** `make_swizzle(b, m, s, *, loc = None, ip = None)`

## static

**Kind:** function

**Qualified name:** `cutlass.cute.static`

**Signature:** `static(value, *, loc = None, ip = None)`

## get_leaves

**Kind:** function

**Qualified name:** `cutlass.cute.get_leaves`

**Signature:** `get_leaves(value, *, loc = None, ip = None)`

## depth

**Kind:** function

**Qualified name:** `cutlass.cute.depth`

**Signature:** `depth(a: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout) → int`

Returns the depth (nesting level) of a tuple, layout, or tensor.

The depth of a tuple is the maximum depth of its elements plus 1. For an empty tuple, the depth is 1. For layouts and tensors, the depth is determined by the depth of their shape. For non-tuple values (e.g., integers), the depth is considered 0.

### Parameters

**a** (*Union\[XTuple,* *Layout,* *ComposedLayout,* *Tensor,* *Any\]*) -- The object whose depth is to be determined

### Returns

The depth of the input object

### Return type

int

**Example:**

    >>> depth(1)
    0
    >>> depth((1, 2))
    1
    >>> depth(((1, 2), (3, 4)))
    2

## rank

**Kind:** function

**Qualified name:** `cutlass.cute.rank`

**Signature:** `rank(a: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, mode: List[int] = []) → int`

Returns the rank (dimensionality) of a tuple, layout, or tensor.

The rank of a tuple is its length. For layouts and tensors, the rank is determined by the rank of their shape. For non-tuple values (e.g., integers), the rank is considered 1 for convenience.

### Parameters

**a** (*Union\[XTuple,* *Layout,* *ComposedLayout,* *Tensor,* *Any\]*) -- The object whose rank is to be determined

### Returns

The rank of the input object

### Return type

int

This function is used in layout algebra to determine the dimensionality of tensors and layouts for operations like slicing and evaluation.

## is_congruent

**Kind:** function

**Qualified name:** `cutlass.cute.is_congruent`

**Signature:** `is_congruent(a: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor, b: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor) → bool`

Returns whether a is congruent to b.

Congruence is an equivalence relation between hierarchical structures.

Two objects are congruent if:

-   They have the same rank, AND

-   They are both non-tuple values, OR

-   They are both tuples AND all corresponding elements are congruent.

Congruence requires type matching at each level -- scalar values match with scalar values, and tuples match with tuples of the same rank.

### Parameters

-   **a** (*Union\[XTuple,* *Layout,* *ComposedLayout,* *Tensor\]*) -- First object to compare

-   **b** (*Union\[XTuple,* *Layout,* *ComposedLayout,* *Tensor\]*) -- Second object to compare

### Returns

True if a and b are congruent, False otherwise

### Return type

bool

## is_weakly_congruent

**Kind:** function

**Qualified name:** `cutlass.cute.is_weakly_congruent`

**Signature:** `is_weakly_congruent(a: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor, b: cutlass.cute.typing.XTuple | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor) → bool`

Returns whether a is weakly congruent to b.

Weak congruence is a partial order on hierarchical structures.

Object X is weakly congruent to object Y if:

-   X is a non-tuple value, OR

-   X and Y are both tuples of the same rank AND all corresponding elements are weakly congruent.

Weak congruence allows scalar values to match with tuples, making it useful for determining whether an object has a hierarchical structure "up to" another.

### Parameters

-   **a** (*Union\[XTuple,* *Layout,* *ComposedLayout,* *Tensor\]*) -- First object to compare

-   **b** (*Union\[XTuple,* *Layout,* *ComposedLayout,* *Tensor\]*) -- Second object to compare

### Returns

True if a and b are weakly congruent, False otherwise

### Return type

bool

## get

**Kind:** function

**Qualified name:** `cutlass.cute.get`

**Signature:** `get(input, mode: List[int], *, loc = None, ip = None)`

Extract a specific element or sub-layout from a layout or tuple.

This function recursively traverses the input according to the mode indices, extracting the element at the specified path. For layouts, this operation corresponds to extracting a specific sub-layout.

### Parameters

-   **input** (*Layout,* *ComposedLayout,* *tuple*) -- The input layout or tuple to extract from

-   **mode** (*List\[int\]*) -- Indices specifying the path to traverse for extraction

-   **loc** (*optional*) -- Source location for MLIR, defaults to None

-   **ip** (*optional*) -- Insertion point, defaults to None

### Returns

The extracted element or sub-layout

### Return type

Layout, ComposedLayout, or element type

### Raises

-   **ValueError** -- If any index in mode is out of range

-   **TypeError** -- If mode contains non-integer elements or if input has unsupported type

### Postcondition

`get(t, mode=find(x,t)) == x if find(x,t) != None else True`

**Examples:**

    layout = make_layout(((4, 8), (16, 1), 8), stride=((1, 4), (32, 0), 512))
    sub_layout = get(layout, mode=[0, 1])   # 8:4
    sub_layout = get(layout, mode=[1])      # (16, 1):(32, 0)

## select

**Kind:** function

**Qualified name:** `cutlass.cute.select`

**Signature:** `select(input, mode: List[int], *, loc = None, ip = None)`

Select modes from input.

### Parameters

-   **input** (*Layout,* *ComposedLayout,* *tuple*) -- Input to select from

-   **mode** (*List\[int\]*) -- Indices specifying which dimensions or elements to select

-   **loc** (*optional*) -- Source location for MLIR, defaults to None

-   **ip** (*optional*) -- Insertion point, defaults to None

### Returns

A new instance with selected dimensions/elements

### Return type

Layout, ComposedLayout, tuple

### Raises

-   **ValueError** -- If any index in mode is out of range

-   **TypeError** -- If the input type is invalid

**Examples:**

    # Select specific dimensions from a layout
    layout = make_layout((4, 8, 16), stride=(32, 4, 1))
    selected = select(layout, mode=[0, 2])  # Select mode 0 and mode 2
    # Result: (4, 16):(32, 1)

    # Select elements from a tuple
    t = (1, 2, 3, 4, 5)
    selected = select(t, mode=[0, 2, 4])  # Select mode 0, mode 2, and mode 4
    # Result: (1, 3, 5)

## group_modes

**Kind:** function

**Qualified name:** `cutlass.cute.group_modes`

**Signature:** `group_modes(input, begin: int, end: int | None = None, *, loc = None, ip = None)`

Group modes of a hierarchical tuple or layout into a single mode.

This function groups a range of modes from the input object into a single mode, creating a hierarchical structure. For tuples, it creates a nested tuple containing the specified range of elements. For layouts and other CuTe objects, it creates a hierarchical representation where the specified modes are grouped together.

### Parameters

-   **input** (*Layout,* *ComposedLayout,* *tuple,* *Shape,* *Stride,* *etc.*) -- Input object to group modes from (layout, tuple, etc.)

-   **beg** (*int*) -- Beginning index of the range to group (inclusive)

-   **end** (*int*) -- Ending index of the range to group (exclusive)

-   **loc** (*optional*) -- Source location for MLIR, defaults to None

-   **ip** (*optional*) -- Insertion point, defaults to None

### Returns

A new object with the specified modes grouped

### Return type

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

## slice\_

**Kind:** function

**Qualified name:** `cutlass.cute.slice_`

**Signature:** `slice_(src, coord: cutlass.cute.typing.Coord, *, loc = None, ip = None)`

Perform a slice operation on a source object using the given coordinate.

This function implements CuTe's slicing operation which extracts a subset of elements from a source object (tensor, layout, etc.) based on a coordinate pattern. The slice operation preserves the structure of the source while selecting specific elements.

### Parameters

-   **src** (*Union\[Tensor,* *Layout,* *IntTuple,* *Value\]*) -- Source object to be sliced (tensor, layout, tuple, etc.)

-   **coord** (*Coord*) -- Coordinate pattern specifying which elements to select

-   **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

-   **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

### Returns

A new object containing the sliced elements

### Return type

Union\[Tensor, Layout, IntTuple, tuple\]

### Raises

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
> -   None represents keeping all elements in that mode
>
> -   Slicing preserves the layout/structure of the original object
>
> -   Can be used for: \* Extracting sub-tensors/sub-layouts \* Creating views into data \* Selecting specific patterns of elements

## prepend

**Kind:** function

**Qualified name:** `cutlass.cute.prepend`

**Signature:** `prepend(input, elem, up_to_rank: int | None = None, *, loc = None, ip = None)`

Extend input to rank up_to_rank by prepending elem in front of input.

This function extends the input object by prepending elements to reach a desired rank. It supports various CuTe types including shapes, layouts, tensors etc.

### Parameters

-   **input** (*Union\[Shape,* *Stride,* *Coord,* *IntTuple,* *Tile,* *Layout,* *ComposedLayout,* *Tensor\]*) -- Source to be prepended to

-   **elem** (*Union\[Shape,* *Stride,* *Coord,* *IntTuple,* *Tile,* *Layout\]*) -- Element to prepend to input

-   **up_to_rank** (*Union\[None,* *int\],* *optional*) -- The target rank after extension, defaults to None

-   **loc** (*Optional\[Location\]*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\]*) -- Insertion point, defaults to None

### Returns

The extended result with prepended elements

### Return type

Union\[Shape, Stride, Coord, IntTuple, Tile, Layout, ComposedLayout, Tensor\]

### Raises

-   **ValueError** -- If up_to_rank is less than input's current rank

-   **TypeError** -- If input or elem has unsupported type

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

## append

**Kind:** function

**Qualified name:** `cutlass.cute.append`

**Signature:** `append(input, elem, up_to_rank: int | None = None, *, loc = None, ip = None)`

Extend input to rank up_to_rank by appending elem to the end of input.

This function extends the input object by appending elements to reach a desired rank. It supports various CuTe types including shapes, layouts, tensors etc.

### Parameters

-   **input** (*Union\[Shape,* *Stride,* *Coord,* *IntTuple,* *Tile,* *Layout,* *ComposedLayout,* *Tensor\]*) -- Source to be appended to

-   **elem** (*Union\[Shape,* *Stride,* *Coord,* *IntTuple,* *Tile,* *Layout\]*) -- Element to append to input

-   **up_to_rank** (*Union\[None,* *int\],* *optional*) -- The target rank after extension, defaults to None

-   **loc** (*Optional\[Location\]*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\]*) -- Insertion point, defaults to None

### Returns

The extended result with appended elements

### Return type

Union\[Shape, Stride, Coord, IntTuple, Tile, Layout, ComposedLayout, Tensor\]

### Raises

-   **ValueError** -- If up_to_rank is less than input's current rank

-   **TypeError** -- If input or elem has unsupported type

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
> -   The function preserves the structure of the input while extending it
>
> -   Can be used to extend tensors, layouts, shapes and other CuTe types
>
> -   When up_to_rank is specified, fills remaining positions with elem
>
> -   Useful for tensor reshaping and layout transformations

## prepend_ones

**Kind:** function

**Qualified name:** `cutlass.cute.prepend_ones`

**Signature:** `prepend_ones(t: cutlass.cute.typing.Tensor, up_to_rank: int | None = None, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

## append_ones

**Kind:** function

**Qualified name:** `cutlass.cute.append_ones`

**Signature:** `append_ones(t, up_to_rank: int | None = None, *, loc = None, ip = None)`

## repeat_as_tuple

**Kind:** function

**Qualified name:** `cutlass.cute.repeat_as_tuple`

**Signature:** `repeat_as_tuple(x, n) → tuple`

Creates a tuple with x repeated n times.

This function creates a tuple by repeating the input value x n times.

### Parameters

-   **x** (*Any*) -- The value to repeat

-   **n** (*int*) -- Number of times to repeat x

### Returns

A tuple containing x repeated n times

### Return type

tuple

**Examples:**

    repeat_as_tuple(1, 1)     # Returns (1,)
    repeat_as_tuple(1, 3)     # Returns (1, 1, 1)
    repeat_as_tuple(None, 4)  # Returns (None, None, None, None)

## repeat

**Kind:** function

**Qualified name:** `cutlass.cute.repeat`

**Signature:** `repeat(x, n)`

Creates an object by repeating x n times.

This function creates an object by repeating the input value x n times. If n=1, returns x directly, otherwise returns a tuple of x repeated n times.

### Parameters

-   **x** (*Any*) -- The value to repeat

-   **n** (*int*) -- Number of times to repeat x

### Returns

x if n=1, otherwise a tuple containing x repeated n times

### Return type

Union\[Any, tuple\]

### Raises

**ValueError** -- If n is less than 1

**Examples:**

    repeat(1, 1)     # Returns 1
    repeat(1, 3)     # Returns (1, 1, 1)
    repeat(None, 4)  # Returns (None, None, None, None)

## repeat_like

**Kind:** function

**Qualified name:** `cutlass.cute.repeat_like`

**Signature:** `repeat_like(x, target)`

Creates an object congruent to target and filled with x.

This function recursively creates a nested tuple structure that matches the structure of the target, with each leaf node filled with the value x.

### Parameters

-   **x** (*Any*) -- The value to fill the resulting structure with

-   **target** (*Union\[tuple,* *Any\]*) -- The structure to mimic

### Returns

A structure matching target but filled with x

### Return type

Union\[tuple, Any\]

**Examples:**

    repeat_like(0, (1, 2, 3))      # Returns (0, 0, 0)
    repeat_like(1, ((1, 2), 3))    # Returns ((1, 1), 1)
    repeat_like(2, 5)              # Returns 2

## flatten

**Kind:** function

**Qualified name:** `cutlass.cute.flatten`

**Signature:** `flatten(a)`

Flattens a CuTe data structure into a simpler form.

For tuples, this function flattens the structure into a single-level tuple. For layouts, it returns a new layout with flattened shape and stride. For tensors, it returns a new tensor with flattened layout. For other types, it returns the input unchanged.

### Parameters

**a** (*Union\[IntTuple,* *Coord,* *Shape,* *Stride,* *Layout,* *Tensor\]*) -- The structure to flatten

### Returns

The flattened structure

### Return type

Union\[tuple, Any\]

**Examples:**

    flatten((1, 2, 3))                      # Returns (1, 2, 3)
    flatten(((1, 2), (3, 4)))               # Returns (1, 2, 3, 4)
    flatten(5)                              # Returns 5
    flatten(Layout(shape, stride))          # Returns Layout(flatten(shape), flatten(stride))
    flatten(Tensor(layout))                 # Returns Tensor(flatten(layout))

## filter_zeros

**Kind:** function

**Qualified name:** `cutlass.cute.filter_zeros`

**Signature:** `filter_zeros(input, *, target_profile = None, loc = None, ip = None)`

Filter out zeros from a layout or tensor.

This function removes zero-stride dimensions from a layout or tensor. Refer to [NVIDIA/cutlass](https://github.com/NVIDIA/cutlass/blob/main/media/docs/cpp/cute/02_layout_algebra.md) for more layout algebra operations.

### Parameters

-   **input** (*Layout* *or* *Tensor*) -- The input layout or tensor to filter

-   **target_profile** (*Stride,* *optional*) -- Target stride profile for the filtered result, defaults to None

-   **loc** (*optional*) -- Source location for MLIR, defaults to None

-   **ip** (*optional*) -- Insertion point, defaults to None

### Returns

The filtered layout or tensor with zeros removed

### Return type

Layout or Tensor

### Raises

**TypeError** -- If input is not a Layout or Tensor

## filter

**Kind:** function

**Qualified name:** `cutlass.cute.filter`

**Signature:** `filter(input, *, loc = None, ip = None)`

Filter a layout or tensor.

This function filters a layout or tensor according to CuTe's filtering rules.

### Parameters

-   **input** (*Layout* *or* *Tensor*) -- The input layout or tensor to filter

-   **loc** (*optional*) -- Source location for MLIR, defaults to None

-   **ip** (*optional*) -- Insertion point, defaults to None

### Returns

The filtered layout or tensor

### Return type

Layout or Tensor

### Raises

**TypeError** -- If input is not a Layout or Tensor

## size

**Kind:** function

**Qualified name:** `cutlass.cute.size`

**Signature:** `size(a: cutlass.cute.typing.IntTuple | cutlass.cute.typing.Shape | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor, mode: List[int] = [], *, loc = None, ip = None) → cutlass.cute.typing.Int`

Return size of domain of layout or tensor.

Computes the size (number of elements) in the domain of a layout or tensor. For layouts, this corresponds to the shape of the coordinate space. See [NVIDIA/cutlass](https://github.com/NVIDIA/cutlass/blob/main/media/docs/cpp/cute/01_layout.md) for more details on layout domains.

### Parameters

-   **a** (*IntTuple,* *Shape,* *Layout,* *ComposedLayout* *or* *Tensor*) -- The input object whose size to compute

-   **mode** (*list* *of* *int,* *optional*) -- List of mode(s) for size calculation. If empty, computes total size, defaults to \[\]

-   **loc** (*optional*) -- Source location for MLIR, defaults to None

-   **ip** (*optional*) -- Insertion point, defaults to None

### Returns

Static size of layout or tensor if static, otherwise a Value

### Return type

int or Value

### Raises

**ValueError** -- If mode contains non-integer elements

## shape_div

**Kind:** function

**Qualified name:** `cutlass.cute.shape_div`

**Signature:** `shape_div(lhs: cutlass.cute.typing.Shape, rhs: cutlass.cute.typing.Shape, *, loc = None, ip = None) → cutlass.cute.typing.Shape`

Perform element-wise division of shapes.

This function performs element-wise division between two shapes.

### Parameters

-   **lhs** (*Shape*) -- Left-hand side shape

-   **rhs** (*Shape*) -- Right-hand side shape

-   **loc** (*optional*) -- Source location for MLIR, defaults to None

-   **ip** (*optional*) -- Insertion point, defaults to None

### Returns

The result of element-wise division

### Return type

Shape

## ceil_div

**Kind:** function

**Qualified name:** `cutlass.cute.ceil_div`

**Signature:** `ceil_div(input: cutlass.cute.typing.Shape, tiler: cutlass.cute.typing.Tiler, *, loc = None, ip = None) → cutlass.cute.typing.Shape`

Compute the ceiling division of a target shape by a tiling specification.

This function computes the number of tiles required to cover the target domain. It is equivalent to the second mode of zipped_divide(input, tiler).

### Parameters

-   **input** (*Shape*) -- A tuple of integers representing the dimensions of the target domain.

-   **tiler** (*Union\[Layout,* *Shape,* *Tile\]*) -- The tiling specification.

-   **loc** (*optional*) -- Optional location information for IR diagnostics.

-   **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions.

### Returns

A tuple of integers representing the number of tiles required along each dimension, i.e. the result of the ceiling division of the input dimensions by the tiler dimensions.

### Return type

Shape

Example:

    import cutlass.cute as cute
    @cute.jit
    def foo():
        input = (10, 6)
        tiler = (3, 4)
        result = cute.ceil_div(input, tiler)
        print(result)  # Outputs: (4, 2)

## round_up

**Kind:** function

**Qualified name:** `cutlass.cute.round_up`

**Signature:** `round_up(a: cutlass.cute.typing.IntTuple, b: cutlass.cute.typing.IntTuple) → cutlass.cute.typing.IntTuple`

Rounds up elements of a using elements of b.

## make_layout

**Kind:** function

**Qualified name:** `cutlass.cute.make_layout`

**Signature:** `make_layout(shape: cutlass.cute.typing.Shape, *, stride: cutlass.cute.typing.Stride | None = None, loc = None, ip = None) → cutlass.cute.typing.Layout`

Create a CuTe Layout object from shape and optional stride information.

A Layout in CuTe represents the mapping between logical and physical coordinates of a tensor. This function creates a Layout object that defines how tensor elements are arranged in memory.

### Parameters

-   **shape** (*Shape*) -- Shape of the layout defining the size of each mode

-   **stride** (*Union\[Stride,* *None\]*) -- Optional stride values for each mode, defaults to None

-   **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

-   **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

### Returns

A new Layout object with the specified shape and stride

### Return type

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

> **Note.**
>
> -   If stride is not provided, a default compact left-most stride is computed based on the shape
>
> -   The resulting layout maps logical coordinates to physical memory locations
>
> -   The layout object can be used for tensor creation and memory access patterns
>
> -   Strides can be used to implement: \* Row-major vs column-major layouts \* Padding and alignment \* Blocked/tiled memory arrangements \* Interleaved data formats
>
> -   Stride is keyword only argument to improve readability, e.g. \* make_layout((3,4), (1,4)) can be confusing with make_layout(((3,4), (1,4))) \* make_layout((3,4), stride=(1,4)) is more readable

## make_identity_layout

**Kind:** function

**Qualified name:** `cutlass.cute.make_identity_layout`

**Signature:** `make_identity_layout(shape: cutlass.cute.typing.Shape, *, loc = None, ip = None) → cutlass.cute.typing.Layout`

Create an identity layout with the given shape.

An identity layout maps logical coordinates directly to themselves without any transformation. This is equivalent to a layout with stride (1@0,1@1,...,1@(N-1)).

### Parameters

-   **shape** (*Shape*) -- The shape of the layout

-   **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

-   **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

### Returns

A new identity Layout object with the specified shape

### Return type

Layout

**Examples:**

    # Create a 2D identity layout with shape (4,4)
    layout = make_identity_layout((4,4))     # stride=(1@0,1@1)

    # Create a 3D identity layout
    layout = make_identity_layout((32,16,8)) # stride=(1@0,1@1,1@2)

> **Note.**
>
> -   An identity layout is a special case where each coordinate maps to itself
>
> -   Useful for direct coordinate mapping without any transformation

## make_ordered_layout

**Kind:** function

**Qualified name:** `cutlass.cute.make_ordered_layout`

**Signature:** `make_ordered_layout(shape: cutlass.cute.typing.Shape, order: cutlass.cute.typing.Shape, *, loc = None, ip = None) → cutlass.cute.typing.Layout`

Create a layout with a specific ordering of dimensions.

This function creates a layout where the dimensions are ordered according to the specified order parameter, allowing for custom dimension ordering in the layout.

### Parameters

-   **shape** (*Shape*) -- The shape of the layout

-   **order** (*Shape*) -- The ordering of dimensions

-   **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

-   **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

### Returns

A new Layout object with the specified shape and dimension ordering

### Return type

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
> -   The order parameter specifies the ordering of dimensions from fastest-varying to slowest-varying
>
> -   For a 2D tensor, (0,1) creates a column-major layout, while (1,0) creates a row-major layout
>
> -   The length of order must match the rank of the shape

## make_layout_like

**Kind:** function

**Qualified name:** `cutlass.cute.make_layout_like`

**Signature:** `make_layout_like(input: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Layout`

## make_composed_layout

**Kind:** function

**Qualified name:** `cutlass.cute.make_composed_layout`

**Signature:** `make_composed_layout(inner, offset: cutlass.cute.typing.IntTuple, outer: cutlass.cute.typing.Layout, *, loc = None, ip = None) → cutlass.cute.typing.ComposedLayout`

Create a composed layout by composing an inner transformation with an outer layout.

A composed layout applies a sequence of transformations to coordinates. The composition is defined as (inner ∘ offset ∘ outer), where the operations are applied from right to left.

### Parameters

-   **inner** (*Union\[Layout,* [*Swizzle*](#cutlass.cute.Swizzle)*\]*) -- The inner transformation (can be a Layout or Swizzle)

-   **offset** (*IntTuple*) -- An integral offset applied between transformations

-   **outer** (*Layout*) -- The outer (right-most) layout that is applied first

-   **loc** (*Optional\[Location\]*) -- Source location information, defaults to None

-   **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for IR generation, defaults to None

### Returns

A new ComposedLayout representing the composition

### Return type

ComposedLayout

**Examples:**

    # Create a basic layout
    inner = make_layout(...)
    outer = make_layout((4,4), stride=(E(0), E(1)))

    # Create a composed layout with an offset
    composed = make_composed_layout(inner, (2,0), outer)

> **Note.**
>
> -   The composition applies transformations in the order: outer → offset → inner
>
> -   The stride divisibility condition must be satisfied for valid composition
>
> -   Certain compositions (like Swizzle with scaled basis) are invalid and will raise errors
>
> -   Composed layouts inherit many properties from the outer layout

## cosize

**Kind:** function

**Qualified name:** `cutlass.cute.cosize`

**Signature:** `cosize(a: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor, mode: List[int] = [], *, loc = None, ip = None)`

Return size of codomain of layout or tensor. Return static value if type is static.

For a layout `L = S:D` where `S` is the shape and `D` is the stride, the codomain size is the minimum size needed to store all possible offsets generated by the layout. This is calculated by taking the maximum offset plus 1.

For example, given a layout `L = (4,(3,2)):(2,(8,1))`:

:   -   Shape `S = (4,(3,2))`

    -   Stride `D = (2,(8,1))`

    -   Maximum offset = `2*(4-1) + 8*(3-1) + 1*(2-1) = 6 + 16 + 1 = 23`

    -   Therefore `cosize(L) = 24`

**Examples:**

    L = cute.make_layout((4,(3,2)), stride=(2,(8,1))) # L = (4,(3,2)):(2,(8,1))
    print(cute.cosize(L))  # => 24

### Parameters

-   **a** (*Union\[Layout,* *ComposedLayout,* *Tensor\]*) -- Layout, ComposedLayout, or Tensor object

-   **mode** (*List\[int\],* *optional*) -- List of mode(s) for cosize calculation. If empty, calculates over all modes. If specified, calculates cosize only for the given modes.

-   **loc** (*optional*) -- Location information for diagnostics, defaults to None

-   **ip** (*optional*) -- Instruction pointer for diagnostics, defaults to None

### Returns

Static size of layout or tensor (fast fold) if static, or a dynamic Value

### Return type

Union\[int, Value\]

## size_in_bytes

**Kind:** function

**Qualified name:** `cutlass.cute.size_in_bytes`

**Signature:** `size_in_bytes(dtype: Type[cutlass.cute.typing.Numeric], layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | None, *, loc = None, ip = None) → int | cutlass.cute.typing.Integer`

Calculate the size in bytes based on its data type and layout. The result is rounded up to the nearest byte.

### Parameters

-   **dtype** (*Type\[Numeric\]*) -- The DSL numeric data type

-   **layout** (*Layout,* *optional*) -- The layout of the elements. If None, the function returns 0

-   **loc** (*optional*) -- Location information for diagnostics, defaults to None

-   **ip** (*optional*) -- Instruction pointer for diagnostics, defaults to None

### Returns

The total size in bytes. Returns 0 if the layout is None

### Return type

int

## coalesce

**Kind:** function

**Qualified name:** `cutlass.cute.coalesce`

**Signature:** `coalesce(input, *, target_profile: cutlass.cute.typing.Coord | None = None, loc = None, ip = None)`

## crd2idx

**Kind:** function

**Qualified name:** `cutlass.cute.crd2idx`

**Signature:** `crd2idx(coord: cutlass.cute.typing.Coord, layout, *, loc = None, ip = None)`

Convert a multi-dimensional coordinate into a value using the specified layout.

This function computes the inner product of the flattened coordinate and stride:

index = sum(flatten(coord)\[i\] \* flatten(stride)\[i\] for i in range(len(coord)))

### Parameters

-   **coord** (*Coord*) -- A tuple or list representing the multi-dimensional coordinate (e.g., (i, j) for a 2D layout).

-   **layout** (*Layout* *or* *ComposedLayout*) -- A layout object that defines the memory storage layout, including shape and stride, used to compute the inner product.

-   **loc** (*optional*) -- Optional location information for IR diagnostics.

-   **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions.

### Returns

The result of applying the layout transformation to the provided coordinate.

### Return type

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

## idx2crd

**Kind:** function

**Qualified name:** `cutlass.cute.idx2crd`

**Signature:** `idx2crd(idx, shape, *, loc = None, ip = None)`

Convert a linear index back into a multi-dimensional coordinate using the specified layout.

Mapping from a linear index to the corresponding multi-dimensional coordinate in the layout's coordinate space. It essentially "unfolds" a linear index into its constituent coordinate components.

### Parameters

-   **idx** (*: int/Integer/Tuple*) -- The linear index to convert back to coordinates.

-   **shape** (*Shape*) -- Shape of the layout defining the size of each mode

-   **loc** (*optional*) -- Optional location information for IR diagnostics.

-   **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions.

### Returns

The result of applying the layout transformation to the provided coordinate.

### Return type

Coord

**Examples:**

    import cutlass.cute as cute
    @cute.jit
    def foo():
        coord = cute.idx2crd(11, (5, 4))
        # idx2crd is always col-major
        # For shape (m, n, l, ...), coord = (idx % m, idx // m % n, idx // m // n % l, ...
        # Computed as: (11 % 5, 11 // 5 % 4) = (1, 2)
        print(coord)

    foo()  # Expected output: (1, 2)

## recast_layout

**Kind:** function

**Qualified name:** `cutlass.cute.recast_layout`

**Signature:** `recast_layout(new_type_bits: int, old_type_bits: int, src_layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, *, loc = None, ip = None)`

Recast a layout from one data type to another.

### Parameters

-   **new_type_bits** (*int*) -- The new data type bits

-   **old_type_bits** (*int*) -- The old data type bits

-   **src_layout** (*Union\[Layout,* *ComposedLayout\]*) -- The layout to recast

-   **loc** (*optional*) -- Optional location information for IR diagnostics.

-   **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions.

### Returns

The recast layout

### Return type

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

## slice_and_offset

**Kind:** function

**Qualified name:** `cutlass.cute.slice_and_offset`

**Signature:** `slice_and_offset(coord, src, *, loc = None, ip = None)`

## recast_ptr

**Kind:** function

**Qualified name:** `cutlass.cute.recast_ptr`

**Signature:** `recast_ptr(ptr: cutlass.cute.typing.Pointer, swizzle_ = None, dtype: Type[cutlass.cute.typing.Numeric] | None = None, loc = None, ip = None) → cutlass.cute.typing.Pointer`

## make_ptr

**Kind:** function

**Qualified name:** `cutlass.cute.make_ptr`

**Signature:** `make_ptr(dtype: Type[cutlass.cute.typing.Numeric] | None, value, mem_space: cutlass.cute.typing.AddressSpace = cutlass.cute.typing.AddressSpace.generic, *, assumed_align = None, loc = None, ip = None) → cutlass.cute.typing.Pointer`

## composition

**Kind:** function

**Qualified name:** `cutlass.cute.composition`

**Signature:** `composition(lhs, rhs: cutlass.cute.typing.Layout | cutlass.cute.typing.Shape | cutlass.cute.typing.Tile, *, loc = None, ip = None)`

Compose two layout representations using the CuTe layout algebra.

Compose a left-hand layout (or tensor) with a right-hand operand into a new layout R, such that for every coordinate c in the domain of the right-hand operand, the composed layout satisfies:

R(c) = A(B(c))

where A is the left-hand operand provided as `lhs` and B is the right-hand operand provided as `rhs`. In this formulation, B defines the coordinate domain while A applies its transformation to B's output, and the resulting layout R inherits the stride and shape adjustments from A.

Satisfies:

:   cute.shape(cute.composition(lhs, rhs)) is compatible with cute.shape(rhs)

### Parameters

-   **lhs** (*Layout* *or* *Tensor*) -- The left-hand operand representing the transformation to be applied.

-   **rhs** (*Layout,* *Shape, or* *Tile, or* *int* *or* *tuple*) -- The right-hand operand defining the coordinate domain. If provided as an int or tuple, it will be converted to a tile layout.

-   **loc** (*optional*) -- Optional location information for IR diagnostics.

-   **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions.

### Returns

A new composed layout R, such that for all coordinates c in the domain of `rhs`, R(c) = lhs(rhs(c)).

### Return type

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

## complement

**Kind:** function

**Qualified name:** `cutlass.cute.complement`

**Signature:** `complement(input: cutlass.cute.typing.Layout, cotarget: cutlass.cute.typing.Layout | cutlass.cute.typing.Shape, *, loc = None, ip = None) → cutlass.cute.typing.Layout`

Compute the complement layout of the input layout with respect to the cotarget.

The complement of a layout A with respect to cotarget n is a layout A\* such that for every k in Z_n and c in the domain of A, there exists a unique c\* in the domain of A\* where k = A(c) + A\*(c\*).

This operation is useful for creating layouts that partition a space in complementary ways, such as row and column layouts that together cover a matrix.

### Parameters

-   **input** (*Layout*) -- The layout to compute the complement of

-   **cotarget** (*Union\[Layout,* *Shape\]*) -- The target layout or shape that defines the codomain

-   **loc** (*optional*) -- Optional location information for IR diagnostics

-   **ip** (*optional*) -- Optional instruction pointer or context for underlying IR functions

### Returns

The complement layout

### Return type

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

## right_inverse

**Kind:** function

**Qualified name:** `cutlass.cute.right_inverse`

**Signature:** `right_inverse(input: cutlass.cute.typing.Layout, *, loc = None, ip = None) → cutlass.cute.typing.Layout`

## left_inverse

**Kind:** function

**Qualified name:** `cutlass.cute.left_inverse`

**Signature:** `left_inverse(input: cutlass.cute.typing.Layout, *, loc = None, ip = None) → cutlass.cute.typing.Layout`

## logical_product

**Kind:** function

**Qualified name:** `cutlass.cute.logical_product`

**Signature:** `logical_product(block, tiler: cutlass.cute.typing.Tile, *, loc = None, ip = None)`

## zipped_product

**Kind:** function

**Qualified name:** `cutlass.cute.zipped_product`

**Signature:** `zipped_product(block, tiler: cutlass.cute.typing.Layout, *, loc = None, ip = None)`

## tiled_product

**Kind:** function

**Qualified name:** `cutlass.cute.tiled_product`

**Signature:** `tiled_product(block, tiler: cutlass.cute.typing.Layout, *, loc = None, ip = None)`

## flat_product

**Kind:** function

**Qualified name:** `cutlass.cute.flat_product`

**Signature:** `flat_product(block, tiler: cutlass.cute.typing.Layout, *, loc = None, ip = None)`

## raked_product

**Kind:** function

**Qualified name:** `cutlass.cute.raked_product`

**Signature:** `raked_product(block, tiler: cutlass.cute.typing.Layout, *, loc = None, ip = None)`

## blocked_product

**Kind:** function

**Qualified name:** `cutlass.cute.blocked_product`

**Signature:** `blocked_product(block, tiler: cutlass.cute.typing.Layout, *, loc = None, ip = None)`

## logical_divide

**Kind:** function

**Qualified name:** `cutlass.cute.logical_divide`

**Signature:** `logical_divide(target, tiler: cutlass.cute.typing.Tiler, *, loc = None, ip = None)`

## zipped_divide

**Kind:** function

**Qualified name:** `cutlass.cute.zipped_divide`

**Signature:** `zipped_divide(target, tiler: cutlass.cute.typing.Tiler, *, loc = None, ip = None)`

## tiled_divide

**Kind:** function

**Qualified name:** `cutlass.cute.tiled_divide`

**Signature:** `tiled_divide(target, tiler: cutlass.cute.typing.Tiler, *, loc = None, ip = None)`

## flat_divide

**Kind:** function

**Qualified name:** `cutlass.cute.flat_divide`

**Signature:** `flat_divide(target, tiler: cutlass.cute.typing.Tile, *, loc = None, ip = None)`

## max_common_layout

**Kind:** function

**Qualified name:** `cutlass.cute.max_common_layout`

**Signature:** `max_common_layout(a: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, b: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Layout`

## max_common_vector

**Kind:** function

**Qualified name:** `cutlass.cute.max_common_vector`

**Signature:** `max_common_vector(a: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, b: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, *, loc = None, ip = None) → int`

## tile_to_shape

**Kind:** function

**Qualified name:** `cutlass.cute.tile_to_shape`

**Signature:** `tile_to_shape(atom, trg_shape: cutlass.cute.typing.Shape, order: cutlass.cute.typing.Shape, *, loc = None, ip = None)`

## local_partition

**Kind:** function

**Qualified name:** `cutlass.cute.local_partition`

**Signature:** `local_partition(target: cutlass.cute.typing.Tensor, tiler: cutlass.cute.typing.Layout | cutlass.cute.typing.Shape, index: int | cutlass.cute.typing.Numeric, proj: cutlass.cute.typing.XTuple = 1, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

## local_tile

**Kind:** function

**Qualified name:** `cutlass.cute.local_tile`

**Signature:** `local_tile(input: cutlass.cute.typing.Tensor, tiler: cutlass.cute.typing.Tiler, coord: cutlass.cute.typing.Coord, proj: cutlass.cute.typing.XTuple | None = None, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

## make_layout_image_mask

**Kind:** function

**Qualified name:** `cutlass.cute.make_layout_image_mask`

**Signature:** `make_layout_image_mask(lay: cutlass.cute.typing.Layout, coord: cutlass.cute.typing.Coord, mode: int, *, loc = None, ip = None) → cutlass.cute.typing.Int16`

Makes a 16-bit integer mask of the image of a layout sliced at a given mode and accounting for the offset given by the input coordinate for the other modes.

## leading_dim

**Kind:** function

**Qualified name:** `cutlass.cute.leading_dim`

**Signature:** `leading_dim(shape: cutlass.cute.typing.Shape, stride: cutlass.cute.typing.Stride) → int | Tuple[int, ...] | None`

Find the leading dimension of a shape and stride.

### Parameters

-   **shape** (*Shape*) -- The shape of the tensor or layout

-   **stride** (*Stride*) -- The stride of the tensor or layout

### Returns

The leading dimension index or indices

### Return type

Union\[int, Tuple\[int, ...\], None\]

The return value depends on the stride pattern:

-   If a single leading dimension is found, returns an integer index

-   If nested leading dimensions are found, returns a tuple of indices

-   If no leading dimension is found, returns None

## make_layout_tv

**Kind:** function

**Qualified name:** `cutlass.cute.make_layout_tv`

**Signature:** `make_layout_tv(thr_layout: cutlass.cute.typing.Layout, val_layout: cutlass.cute.typing.Layout, *, loc = None, ip = None) → Tuple[cutlass.cute.typing.Shape, cutlass.cute.typing.Layout]`

Create a thread-value layout by repeating the val_layout over the thr_layout.

This function creates a thread-value layout that maps between `(thread_idx, value_idx)` coordinates and logical `(M,N)` coordinates. The thread and value layouts must be compact to ensure proper partitioning.

This implements the thread-value partitioning pattern where data is partitioned across threads and values within each thread.

### Parameters

-   **thr_layout** (*Layout*) -- Layout mapping from `(TileM,TileN)` coordinates to thread IDs (must be compact)

-   **val_layout** (*Layout*) -- Layout mapping from `(ValueM,ValueN)` coordinates to value IDs within each thread

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

### Returns

A tuple containing `tiler_mn` and `layout_tv`

### Return type

Tuple\[Shape, Layout\]

where:

:   -   `tiler_mn` is tiler and `shape(tiler_mn)` is compatible with `shape(zipped_divide(x, tiler_mn))[0]`

    -   `layout_tv`: Thread-value layout mapping (thread_idx, value_idx) -\> (M,N)

**Example:**

The below code creates a TV Layout that maps thread/value coordinates to the logical coordinates in a `(4,6)` tensor:

:   -   *Tiler MN*: `(4,6)`

    -   *TV Layout*: `((3,2),(2,2)):((8,2),(4,1))`

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

  : Table 1 TV Layout

## get_nonswizzle_portion

**Kind:** function

**Qualified name:** `cutlass.cute.get_nonswizzle_portion`

**Signature:** `get_nonswizzle_portion(layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, *, loc = None, ip = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout`

Extract the non-swizzle portion from a layout.

For a simple Layout, the entire layout is considered non-swizzled and is returned as-is. For a ComposedLayout, the inner layout (non-swizzled portion) is extracted and returned, effectively separating the base layout from any swizzle transformation that may be applied.

### Parameters

-   **layout** (*Union\[Layout,* *ComposedLayout\]*) -- A Layout or ComposedLayout from which to extract the non-swizzle portion.

-   **loc** (*optional*) -- Optional location information for IR diagnostics.

-   **ip** (*optional*) -- Optional

### Returns

The non-swizzle portion of the input layout. For Layout objects, returns the layout itself. For ComposedLayout objects, returns the outer layout component.

### Return type

Layout

### Raises

**TypeError** -- If the layout is neither a Layout nor a ComposedLayout.

## get_swizzle_portion

**Kind:** function

**Qualified name:** `cutlass.cute.get_swizzle_portion`

**Signature:** `get_swizzle_portion(layout: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, *, loc = None, ip = None) → cutlass._mlir.ir.register_value_caster`

Extract or create the swizzle portion from a layout.

For a simple Layout (which has no explicit swizzle), a default identity swizzle is created. For a ComposedLayout, the outer layout is checked and returned if it is a Swizzle object. Otherwise, a default identity swizzle is created. The default identity swizzle has parameters (0, 4, 3), which represents a no-op swizzle transformation.

### Parameters

-   **layout** (*Union\[Layout,* *ComposedLayout\]*) -- A Layout or ComposedLayout from which to extract the swizzle portion.

-   **loc** (*optional*) -- Optional location information for IR diagnostics.

-   **ip** (*optional*) -- Optional

### Returns

The swizzle portion of the layout. For Layout objects or ComposedLayout objects without a Swizzle outer component, returns a default identity swizzle (0, 4, 3). For ComposedLayout objects with a Swizzle outer component, returns that swizzle.

### Return type

[Swizzle](#cutlass.cute.Swizzle)

### Raises

**TypeError** -- If the layout is neither a Layout nor a ComposedLayout.

## transform_leaf

**Kind:** function

**Qualified name:** `cutlass.cute.transform_leaf`

**Signature:** `transform_leaf(f, *args)`

Apply a function to the leaf nodes of nested tuple structures.

This function traverses nested tuple structures in parallel and applies the function f to corresponding leaf nodes. All input tuples must have the same nested structure.

### Parameters

-   **f** (*Callable*) -- Function to apply to leaf nodes

-   **args** -- One or more nested tuple structures with matching profiles

### Returns

A new nested tuple with the same structure as the inputs, but with leaf values transformed by f

### Raises

**TypeError** -- If the input tuples have different nested structures

**Example:**

    >>> transform_leaf(lambda x: x + 1, (1, 2))
    (2, 3)
    >>> transform_leaf(lambda x, y: x + y, (1, 2), (3, 4))
    (4, 6)
    >>> transform_leaf(lambda x: x * 2, ((1, 2), (3, 4)))
    ((2, 4), (6, 8))

## find_if

**Kind:** function

**Qualified name:** `cutlass.cute.find_if`

**Signature:** `find_if(t: tuple | cutlass._mlir.ir.Value | int, pred_fn: Callable[[tuple | cutlass._mlir.ir.Value | int, int], bool], *, loc = None, ip = None) → int | Tuple[int, ...] | None`

## find

**Kind:** function

**Qualified name:** `cutlass.cute.find`

**Signature:** `find(t: tuple | cutlass._mlir.ir.Value | int, x: int, *, loc = None, ip = None) → int | Tuple[int, ...] | None`

Find the first position of a value `x` in a hierarchical structure `t`.

Searches for the first occurrence of x in t, optionally excluding positions where a comparison value matches. The search can traverse nested structures and returns either a single index or a tuple of indices for nested positions.

### Parameters

-   **t** (*Union\[tuple,* *ir.Value,* *int\]*) -- The search space

-   **x** (*int*) -- The static integer x to search for

### Returns

Index if found at top level, tuple of indices showing nested position, or None if not found

### Return type

Union\[int, Tuple\[int, ...\], None\]

## flatten_to_tuple

**Kind:** function

**Qualified name:** `cutlass.cute.flatten_to_tuple`

**Signature:** `flatten_to_tuple(a: cutlass.cute.typing.XTuple) → Tuple[Any, ...]`

Flattens a potentially nested tuple structure into a flat tuple.

This function recursively traverses the input structure and flattens it into a single-level tuple, preserving the order of elements.

### Parameters

**a** (*Union\[IntTuple,* *Coord,* *Shape,* *Stride\]*) -- The structure to flatten

### Returns

A flattened tuple containing all elements from the input

### Return type

tuple

**Examples:**

    flatten_to_tuple((1, 2, 3))       # Returns (1, 2, 3)
    flatten_to_tuple(((1, 2), 3))     # Returns (1, 2, 3)
    flatten_to_tuple((1, (2, (3,))))  # Returns (1, 2, 3)

## unflatten

**Kind:** function

**Qualified name:** `cutlass.cute.unflatten`

**Signature:** `unflatten(sequence: Tuple[Any, ...] | List[Any] | Iterable[Any], profile: cutlass.cute.typing.XTuple) → cutlass.cute.typing.XTuple`

Unflatten a flat tuple into a nested tuple structure according to a profile.

This function transforms a flat sequence of elements into a nested tuple structure that matches the structure defined by the profile parameter. It traverses the profile structure and populates it with elements from the sequence.

sequence must be long enough to fill the profile. Raises RuntimeError if it is not.

### Parameters

-   **sequence** (*Union\[Tuple\[Any,* *\...\],* *List\[Any\],* *Iterable\[Any\]\]*) -- A flat sequence of elements to be restructured

-   **profile** (*XTuple*) -- A nested tuple structure that defines the shape of the output

### Returns

A nested tuple with the same structure as profile but containing elements from sequence

### Return type

XTuple

**Examples:**

    unflatten([1, 2, 3, 4], ((0, 0), (0, 0)))  # Returns ((1, 2), (3, 4))

## product

**Kind:** function

**Qualified name:** `cutlass.cute.product`

**Signature:** `product(a: cutlass.cute.typing.IntTuple | cutlass.cute.typing.Shape, *, loc = None, ip = None)`

## product_like

**Kind:** function

**Qualified name:** `cutlass.cute.product_like`

**Signature:** `product_like(a: cutlass.cute.typing.IntTuple, target_profile: cutlass.cute.typing.XTuple, *, loc = None, ip = None) → cutlass.cute.typing.IntTuple`

Return product of the given IntTuple or Shape at leaves of target_profile.

This function computes products according to the structure defined by target_profile.

### Parameters

-   **a** (*IntTuple* *or* *Shape*) -- The input tuple or shape

-   **target_profile** (*XTuple*) -- The profile that guides how products are computed

-   **loc** (*optional*) -- Source location for MLIR, defaults to None

-   **ip** (*optional*) -- Insertion point, defaults to None

### Returns

The resulting tuple with products computed according to target_profile

### Return type

IntTuple or Shape

### Raises

-   **TypeError** -- If inputs have incompatible types

-   **ValueError** -- If inputs have incompatible shapes

## product_each

**Kind:** function

**Qualified name:** `cutlass.cute.product_each`

**Signature:** `product_each(a: cutlass.cute.typing.IntTuple, *, loc = None, ip = None) → cutlass.cute.typing.IntTuple`

## elem_less

**Kind:** function

**Qualified name:** `cutlass.cute.elem_less`

**Signature:** `elem_less(lhs: cutlass.cute.typing.Shape | cutlass.cute.typing.IntTuple | cutlass.cute.typing.Coord, rhs: cutlass.cute.typing.Shape | cutlass.cute.typing.IntTuple | cutlass.cute.typing.Coord, *, loc = None, ip = None) → cutlass.cute.typing.Boolean`

## tuple_cat

**Kind:** function

**Qualified name:** `cutlass.cute.tuple_cat`

**Signature:** `tuple_cat(*tuples)`

Concatenate multiple tuples into a single tuple.

This function takes any number of tuples and concatenates them into a single tuple. Non-tuple arguments are treated as single-element tuples.

### Parameters

**tuples** (*tuple* *or* *any*) -- Variable number of tuples to concatenate

### Returns

A single concatenated tuple

### Return type

tuple

**Examples:**

    >>> tuple_cat((1, 2), (3, 4))
    (1, 2, 3, 4)
    >>> tuple_cat((1,), (2, 3), (4,))
    (1, 2, 3, 4)
    >>> tuple_cat(1, (2, 3))
    (1, 2, 3)

## transform_apply

**Kind:** function

**Qualified name:** `cutlass.cute.transform_apply`

**Signature:** `transform_apply(*args, f: Callable, g: Callable)`

Transform elements of tuple(s) with f, then apply g to all results.

This function applies f to corresponding elements across input tuple(s), then applies g to all transformed results. It mimics the C++ CuTe implementation.

Supports multiple signatures:

-   transform_apply(t, f, g): For single tuple, computes g(f(t\[0\]), f(t\[1\]), ...)

-   transform_apply(t0, t1, f, g): For two tuples, computes g(f(t0\[0\], t1\[0\]), f(t0\[1\], t1\[1\]), ...)

-   transform_apply(t0, t1, t2, ..., f, g): For multiple tuples of same length

For non-tuple inputs, f is applied to the input(s) and g is applied to that single result.

### Parameters

-   **args** -- One or more tuples (or non-tuples) to transform

-   **f** (*Callable*) -- The function to apply to each element (or corresponding elements across tuples)

-   **g** (*Callable*) -- The function to apply to all transformed elements

-   **loc** (*optional*) -- Source location for MLIR, defaults to None

-   **ip** (*optional*) -- Insertion point, defaults to None

### Returns

The result of applying g to all transformed elements

### Return type

any

**Examples:**

    >>> transform_apply((1, 2, 3), f=lambda x: x * 2, g=lambda *args: sum(args))
    12  # (1*2 + 2*2 + 3*2) = 12
    >>> transform_apply((1, 2), f=lambda x: (x, x+1), g=tuple_cat)
    (1, 2, 2, 3)
    >>> transform_apply((1, 2), (3, 4), f=lambda x, y: x + y, g=lambda *args: args)
    (4, 6)

## filter_tuple

**Kind:** function

**Qualified name:** `cutlass.cute.filter_tuple`

**Signature:** `filter_tuple(*args, f: Callable)`

Filter and flatten tuple elements by applying a function.

The function f should return tuples, which are then concatenated together to produce the final result. This is useful for filtering and transforming tuple structures in a single pass.

### Parameters

-   **t** (*Union\[tuple,* *ir.Value,* *int\]*) -- The tuple to filter

-   **f** (*Callable*) -- The function to apply to each element of t

-   **loc** (*optional*) -- Source location for MLIR, defaults to None

-   **ip** (*optional*) -- Insertion point, defaults to None

### Returns

A concatenated tuple of all results

### Return type

tuple

**Examples:**

    >>> # Keep only even numbers, wrapped in tuples
    >>> filter_tuple((1, 2, 3, 4), lambda x: (x,) if x % 2 == 0 else ())
    (2, 4)
    >>> # Duplicate each element
    >>> filter_tuple((1, 2, 3), lambda x: (x, x))
    (1, 1, 2, 2, 3, 3)

## TensorSSA

**Kind:** class

**Qualified name:** `cutlass.cute.TensorSSA`

**Signature:** `TensorSSA(*args: Any, **kwargs: Any)`

Bases: `ArithValue`

A class representing thread local data from CuTe Tensor in value semantic and immutable.

### Parameters

-   **value** (*ir.Value*) -- Flatten vector as ir.Value holding logic data of SSA Tensor

-   **shape** (*Shape*) -- The nested shape in CuTe of the vector

-   **dtype** (*Type\[Numeric\]*) -- Data type of the tensor elements

### Variables

-   **\_shape** -- The nested shape in CuTe of the vector

-   **\_dtype** -- Data type of the tensor elements

### Raises

**ValueError** -- If shape is not static

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.__init__`

**Signature:** `__init__(value, shape: cutlass.cute.typing.Shape, dtype: Type[cutlass.cute.typing.Numeric])`

Initialize a new TensorSSA object.

#### Parameters

-   **value** (*ir.Value*) -- Flatten vector as ir.Value holding logic data of SSA Tensor

-   **shape** (*Shape*) -- The nested shape in CuTe of the vector

-   **dtype** (*Type\[Numeric\]*) -- Data type of the tensor elements

#### Raises

**ValueError** -- If shape is not static

### dtype

**Kind:** property

**Qualified name:** `cutlass.cute.TensorSSA.dtype`

**Signature:** `dtype: Type[cutlass.cute.typing.Numeric]`

### element_type

**Kind:** property

**Qualified name:** `cutlass.cute.TensorSSA.element_type`

**Signature:** `element_type: Type[cutlass.cute.typing.Numeric]`

### shape

**Kind:** property

**Qualified name:** `cutlass.cute.TensorSSA.shape`

**Signature:** `shape`

### \_apply_op

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA._apply_op`

**Signature:** `_apply_op(op, other: TensorSSA, flip = False, *, loc, ip) → TensorSSA`

### apply_op

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.apply_op`

**Signature:** `apply_op(op, other, flip = False, *, loc = None, ip = None) → TensorSSA`

Apply a binary operation to this tensor and another operand.

This is a public interface to the internal \_apply_op method, providing a stable API for external users who need to apply custom operations.

#### Parameters

-   **op** -- The operation function (e.g., operator.add, operator.mul, etc.)

-   **other** -- The other operand (TensorSSA, ArithValue, or scalar)

-   **flip** -- Whether to flip the operands (for right-hand operations)

-   **loc** -- MLIR location (optional)

-   **ip** -- MLIR insertion point (optional)

#### Returns

The result of the operation

#### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

Example

    >>> tensor1 = cute.Tensor(...)
    >>> tensor2 = cute.Tensor(...)
    >>> result = tensor1.apply_op(operator.add, tensor2)
    >>> # Equivalent to: tensor1 + tensor2

### broadcast_to

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.broadcast_to`

**Signature:** `broadcast_to(target_shape: cutlass.cute.typing.Shape, *, loc = None, ip = None) → TensorSSA`

Broadcast the tensor to the target shape.

### \_flatten_shape_and_coord

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA._flatten_shape_and_coord`

**Signature:** `_flatten_shape_and_coord(crd, *, loc = None, ip = None)`

### \_build_result

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA._build_result`

**Signature:** `_build_result(res_vect, res_shp, *, row_major = False, loc = None, ip = None)`

### reshape

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.reshape`

**Signature:** `reshape(shape: cutlass.cute.typing.Shape, *, loc = None, ip = None) → TensorSSA`

Reshape the tensor to a new shape.

#### Parameters

**shape** (*Shape*) -- The new shape to reshape to.

#### Returns

A new tensor with the same elements but with the new shape.

#### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

#### Raises

-   **NotImplementedError** -- If dynamic size is not supported

-   **ValueError** -- If the new shape is not compatible with the current shape

### to

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.to`

**Signature:** `to(dtype: Type[cutlass.cute.typing.Numeric], *, loc = None, ip = None)`

Convert the tensor to a different numeric type.

#### Parameters

**dtype** (*Type\[Numeric\]*) -- The target numeric type to cast to.

#### Returns

A new tensor with the same shape but with elements cast to the target type.

#### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

#### Raises

-   **TypeError** -- If dtype is not a subclass of Numeric.

-   **NotImplementedError** -- If dtype is an unsigned integer type.

### ir_value

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.ir_value`

**Signature:** `ir_value(*, loc = None, ip = None)`

### ir_value_int8

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.ir_value_int8`

**Signature:** `ir_value_int8(*, loc = None, ip = None)`

Returns int8 ir value of Boolean tensor. When we need to store Boolean tensor ssa, use ir_value_int8().

#### Parameters

-   **loc** (*Optional\[Location\],* *optional*) -- Source location information, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point for MLIR operations, defaults to None

#### Returns

The int8 value of this Boolean

#### Return type

ir.Value

### reduce

**Kind:** method

**Qualified name:** `cutlass.cute.TensorSSA.reduce`

**Signature:** `reduce(op, init_val, reduction_profile: cutlass.cute.typing.Coord, *, loc = None, ip = None)`

Perform reduce on selected modes with given predefined reduction op.

#### Parameters

-   **op** (*operator*) -- The reduction operator to use (operator.add or operator.mul)

-   **init_val** (*numeric*) -- The initial value for the reduction

-   **reduction_profile** (*Coord*) -- Specifies which dimensions to reduce. Dimensions marked with None are kept.

#### Returns

The reduced tensor

#### Return type

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

## make_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.make_tensor`

**Signature:** `make_tensor(iterator, layout: cutlass.cute.typing.Shape | cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

Creates a tensor by composing an engine (iterator/pointer) with a layout.

A tensor is defined as T = E ∘ L, where E is an engine (array, pointer, or counting iterator) and L is a layout that maps logical coordinates to physical offsets. The tensor evaluates coordinates by applying the layout mapping and dereferencing the engine at the resulting offset.

### Parameters

-   **iterator** (*Union\[Pointer,* *IntTuple,* *ir.Value\]*) -- Engine component that provides data access capabilities. Can be: - A pointer (Pointer type) - An integer or integer tuple for coordinate tensors - A shared memory descriptor (SmemDescType)

-   **layout** (*Union\[Shape,* *Layout,* *ComposedLayout\]*) -- Layout component that defines the mapping from logical coordinates to physical offsets. Can be: - A shape tuple that will be converted to a layout - A Layout object - A ComposedLayout object (must be a normal layout)

-   **loc** (*Optional\[Location\]*) -- Source location for MLIR operation tracking, defaults to None

-   **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for MLIR operation, defaults to None

### Returns

A tensor object representing the composition E ∘ L

### Return type

Tensor

### Raises

-   **TypeError** -- If iterator type is not a supported type

-   **ValueError** -- If layout is a composed layout with customized inner functions

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

-   The engine (iterator) must support random access operations

-   Common engine types include raw pointers, arrays, and random-access iterators

-   The layout defines both the shape (logical dimensions) and stride (physical mapping)

-   Supports both direct coordinate evaluation T(c) and partial evaluation (slicing)

-   ComposedLayouts must be "normal" layouts (no inner functions)

-   For coordinate tensors, the iterator is converted to a counting sequence

## make_identity_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.make_identity_tensor`

**Signature:** `make_identity_tensor(shape: cutlass.cute.typing.Shape, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

Creates an identity tensor with the given shape.

An identity tensor maps each coordinate to itself, effectively creating a counting sequence within the shape's bounds. This is useful for generating coordinate indices or creating reference tensors for layout transformations.

### Parameters

-   **shape** (*Shape*) -- The shape defining the tensor's dimensions. Can be a simple integer sequence or a hierarchical structure ((m,n),(p,q))

-   **loc** (*Optional\[Location\]*) -- Source location for MLIR operation tracking, defaults to None

-   **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for MLIR operation, defaults to None

### Returns

A tensor that maps each coordinate to itself

### Return type

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

-   The shape parameter follows CuTe's IntTuple concept

-   Coordinates are ordered colexicographically

-   Useful for generating reference coordinates in layout transformations

## make_fragment

**Kind:** function

**Qualified name:** `cutlass.cute.make_fragment`

**Signature:** `make_fragment(layout_or_shape: cutlass.cute.typing.Layout | cutlass.cute.typing.Shape, dtype: Type[cutlass.cute.typing.Numeric], *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

## make_fragment_like

**Kind:** function

**Qualified name:** `cutlass.cute.make_fragment_like`

**Signature:** `make_fragment_like(src, dtype = None, *, loc = None, ip = None)`

## make_rmem_tensor_like

**Kind:** function

**Qualified name:** `cutlass.cute.make_rmem_tensor_like`

**Signature:** `make_rmem_tensor_like(src: cutlass.cute.typing.Layout | cutlass.cute.typing.ComposedLayout | cutlass.cute.typing.Tensor | TensorSSA, dtype: Type[cutlass.cute.typing.Numeric] | None = None, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

Creates a tensor in register memory with the same shape as the input layout but

:   compact col-major strides. This is equivalent to calling make_rmem_tensor(make_layout_like(tensor)).

This function allocates a tensor in register memory (rmem) usually on stack with with the compact layout like the source. The tensor will have elements of the specified numeric data type or the same as the source.

### Parameters

-   **src** (*Union\[Layout,* *ComposedLayout,* *Tensor\]*) -- The source layout or tensor whose shape will be matched

-   **dtype** (*Type\[Numeric\],* *optional*) -- The element type for the fragment tensor, defaults to None

-   **loc** (*Location,* *optional*) -- Source location for MLIR operations, defaults to None

-   **ip** (*InsertionPoint,* *optional*) -- Insertion point for MLIR operations, defaults to None

### Returns

A new layout or fragment tensor with matching shape

### Return type

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

-   When used with a Tensor, if a type is provided, it will create a new fragment tensor with that element type.

-   For layouts with ScaledBasis strides, the function creates a fragment from the shape only.

-   This function is commonly used in GEMM and other tensor operations to create register storage for intermediate results.

## make_rmem_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.make_rmem_tensor`

**Signature:** `make_rmem_tensor(layout_or_shape: cutlass.cute.typing.Layout | cutlass.cute.typing.Shape, dtype: Type[cutlass.cute.typing.Numeric], *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

Creates a tensor in register memory with the specified layout/shape and data type.

This function allocates a tensor in register memory (rmem) usually on stack with either a provided layout or creates a new layout from the given shape. The tensor will have elements of the specified numeric data type.

### Parameters

-   **layout_or_shape** (*Union\[Layout,* *Shape\]*) -- Either a Layout object defining the tensor's memory organization, or a Shape defining its dimensions

-   **dtype** (*Type\[Numeric\]*) -- The data type for tensor elements (must be a Numeric type)

-   **loc** (*Optional\[Location\]*) -- Source location for MLIR operation tracking, defaults to None

-   **ip** (*Optional\[InsertionPoint\]*) -- Insertion point for MLIR operation, defaults to None

### Returns

A tensor allocated in register memory

### Return type

Tensor

**Examples:**

    # Create rmem tensor with explicit layout
    layout = make_layout((128, 32))
    tensor = make_rmem_tensor(layout, cutlass.Float16)

    # Create rmem tensor directly from shape
    tensor = make_rmem_tensor((64, 64), cutlass.Float32)

Notes

-   Uses 32-byte alignment to support .128 load/store operations

-   Boolean types are stored as 8-bit integers

-   Handles both direct shapes and Layout objects

## recast_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.recast_tensor`

**Signature:** `recast_tensor(src: cutlass.cute.typing.Tensor, dtype: Type[cutlass.cute.typing.Numeric], swizzle_ = None, *, loc = None, ip = None)`

## domain_offset

**Kind:** function

**Qualified name:** `cutlass.cute.domain_offset`

**Signature:** `domain_offset(coord: cutlass.cute.typing.Coord, tensor: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

## print_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.print_tensor`

**Signature:** `print_tensor(tensor: cutlass.cute.typing.Tensor | TensorSSA, *, verbose: bool = False, loc = None, ip = None)`

Print content of the tensor in human readable format.

Outputs the tensor data in a structured format showing both metadata and the actual data values. The output includes tensor type information, layout details, and a formatted array representation of the values.

### Parameters

-   **tensor** (*Tensor*) -- The tensor to print

-   **verbose** (*bool*) -- If True, includes additional debug information in the output

-   **loc** (*source location,* *optional*) -- Source location where it's called, defaults to None

-   **ip** (*insertion pointer,* *optional*) -- Insertion pointer for IR generation, defaults to None

### Raises

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

## full

**Kind:** function

**Qualified name:** `cutlass.cute.full`

**Signature:** `full(shape, fill_value, dtype: Type[cutlass.cute.typing.Numeric], *, loc = None, ip = None) → TensorSSA`

Return a new TensorSSA of given shape and type, filled with fill_value.

### Parameters

-   **shape** (*tuple*) -- Shape of the new tensor.

-   **fill_value** (*scalar*) -- Value to fill the tensor with.

-   **dtype** (*Type\[Numeric\]*) -- Data type of the tensor.

### Returns

Tensor of fill_value with the specified shape and dtype.

### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

## full_like

**Kind:** function

**Qualified name:** `cutlass.cute.full_like`

**Signature:** `full_like(a: TensorSSA | cutlass.cute.typing.Tensor, fill_value, dtype: Type[cutlass.cute.typing.Numeric] | None = None, *, loc = None, ip = None) → TensorSSA`

Return a full TensorSSA with the same shape and type as a given array.

### Parameters

-   **a** (*array_like*) -- The shape and data-type of a define these same attributes of the returned array.

-   **fill_value** (*array_like*) -- Fill value.

-   **dtype** (*Union\[None,* *Type\[Numeric\]\],* *optional*) -- Overrides the data type of the result, defaults to None

### Returns

Tensor of fill_value with the same shape and type as a.

### Return type

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

## empty_like

**Kind:** function

**Qualified name:** `cutlass.cute.empty_like`

**Signature:** `empty_like(a, dtype = None, *, loc = None, ip = None)`

Return a new TensorSSA with the same shape and type as a given array, without initializing entries.

### Parameters

-   **a** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- The shape and data-type of a define these same attributes of the returned array.

-   **dtype** (*Type\[Numeric\],* *optional*) -- Overrides the data type of the result, defaults to None

### Returns

Uninitialized tensor with the same shape and type (unless overridden) as a.

### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

## ones_like

**Kind:** function

**Qualified name:** `cutlass.cute.ones_like`

**Signature:** `ones_like(a, dtype = None, *, loc = None, ip = None)`

Return a TensorSSA of ones with the same shape and type as a given array.

### Parameters

-   **a** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- The shape and data-type of a define these same attributes of the returned array.

-   **dtype** (*Type\[Numeric\],* *optional*) -- Overrides the data type of the result, defaults to None

### Returns

Tensor of ones with the same shape and type (unless overridden) as a.

### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

## zeros_like

**Kind:** function

**Qualified name:** `cutlass.cute.zeros_like`

**Signature:** `zeros_like(a, dtype = None, *, loc = None, ip = None)`

Return a TensorSSA of zeros with the same shape and type as a given array.

### Parameters

-   **a** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- The shape and data-type of a define these same attributes of the returned array.

-   **dtype** (*Type\[Numeric\],* *optional*) -- Overrides the data type of the result, defaults to None

### Returns

Tensor of zeros with the same shape and type (unless overridden) as a.

### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

## where

**Kind:** function

**Qualified name:** `cutlass.cute.where`

**Signature:** `where(cond: TensorSSA, x: TensorSSA | cutlass.cute.typing.Numeric, y: TensorSSA | cutlass.cute.typing.Numeric, *, loc = None, ip = None) → TensorSSA`

Return elements chosen from x or y depending on condition; will auto broadcast x or y if needed.

### Parameters

-   **cond** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- Where True, yield x, where False, yield y.

-   **x** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Values from which to choose when condition is True.

-   **y** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Values from which to choose when condition is False.

### Returns

A tensor with elements from x where condition is True, and elements from y where condition is False.

### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

## any\_

**Kind:** function

**Qualified name:** `cutlass.cute.any_`

**Signature:** `any_(x: TensorSSA, *, loc = None, ip = None) → cutlass.cute.typing.Boolean`

Test whether any tensor element evaluates to True.

### Parameters

**x** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- Input tensor.

### Returns

Returns a TensorSSA scalar containing True if any element of x is True, False otherwise.

### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

## all\_

**Kind:** function

**Qualified name:** `cutlass.cute.all_`

**Signature:** `all_(x: TensorSSA, *, loc = None, ip = None) → cutlass.cute.typing.Boolean`

Test whether all tensor elements evaluate to True.

### Parameters

**x** ([*TensorSSA*](#cutlass.cute.TensorSSA)) -- Input tensor.

### Returns

Returns a TensorSSA scalar containing True if all elements of x are True, False otherwise.

### Return type

[TensorSSA](#cutlass.cute.TensorSSA)

## Atom

**Kind:** class

**Qualified name:** `cutlass.cute.Atom`

**Signature:** `Atom(op: Op, trait: Trait)`

Bases: `ABC`

Atom base class.

An Atom is the composition of

-   a MMA or Copy Operation;

-   an internal MMA or Copy Trait.

An Operation is a pure Python class that is used to model a specific MMA or Copy instruction. The Trait wraps the underlying IR Value and provides access to the metadata of the instruction encoded using CuTe Layouts. When the Trait can be constructed straighforwardly from an Operation, the `make_mma_atom` or `make_copy_atom` API should be used. There are cases where constructing the metadata is not trivial and requires more information, for example to determine the number of bytes copied per TMA instruction ("the TMA vector length"). In such cases, dedicated helper functions are provided with an appropriate API such that the Atom is constructed internally in an optimal fashion for the user.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.Atom.__init__`

**Signature:** `__init__(op: Op, trait: Trait) → None`

### op

**Kind:** property

**Qualified name:** `cutlass.cute.Atom.op`

**Signature:** `op: Op`

### type

**Kind:** property

**Qualified name:** `cutlass.cute.Atom.type`

**Signature:** `type`

### set

**Kind:** method

**Qualified name:** `cutlass.cute.Atom.set`

**Signature:** `set(modifier, value, *, loc = None, ip = None) → None`

Sets runtime fields of the Atom.

Some Atoms have runtime state, for example a tcgen05 MMA Atom

    tiled_mma = cute.make_tiled_mma(some_tcgen05_mma_op)
    tiled_mma.set(cute.nvgpu.tcgen05.Field.ACCUMULATE, True)

The `set` method provides a way to the user to modify such runtime state. Modifiable fields are provided by arch-specific enumerations, for example `tcgen05.Field`. The Atom instance internally validates the field as well as the value provided by the user to set the field to.

### get

**Kind:** method

**Qualified name:** `cutlass.cute.Atom.get`

**Signature:** `get(field, *, loc = None, ip = None) → Any`

Gets runtime fields of the Atom.

Some Atoms have runtime state, for example a tcgen05 MMA Atom

    tiled_mma = cute.make_tiled_mma(some_tcgen05_mma_op)
    accum = tiled_mma.get(cute.nvgpu.tcgen05.Field.ACCUMULATE)

The `get` method provides a way to the user to access such runtime state. Modifiable fields are provided by arch-specific enumerations, for example `tcgen05.Field`. The Atom instance internally validates the field as well as the value provided by the user to set the field to.

### with\_

**Kind:** method

**Qualified name:** `cutlass.cute.Atom.with_`

**Signature:** `with_(*, loc = None, ip = None, **kwargs) → Atom`

Returns a new Atom with the new Operation and Trait with the given runtime state. The runtime state is provided as keyword arguments and it is Atom-specific.

    tiled_copy = cute.make_tiled_copy(tma_copy_op)
    new_tiled_copy = tiled_copy.with_(tma_bar_ptr=tma_bar_ptr, cache_policy=cute.CacheEvictionPriority.EVICT_LAST)

The `with_` method provides a way to the user to modify such runtime state or create an executable Atom (e.g. an Executable TMA Load Atom).

### \_unpack

**Kind:** method

**Qualified name:** `cutlass.cute.Atom._unpack`

**Signature:** `_unpack(*, loc = None, ip = None, **kwargs) → cutlass._mlir.ir.Value`

### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.Atom._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

## MmaAtom

**Kind:** class

**Qualified name:** `cutlass.cute.MmaAtom`

**Signature:** `MmaAtom(op: Op, trait: Trait)`

Bases: [`Atom`](#cutlass.cute.Atom)

The MMA Atom class.

### thr_id

**Kind:** property

**Qualified name:** `cutlass.cute.MmaAtom.thr_id`

**Signature:** `thr_id`

### shape_mnk

**Kind:** property

**Qualified name:** `cutlass.cute.MmaAtom.shape_mnk`

**Signature:** `shape_mnk`

### tv_layout_A

**Kind:** property

**Qualified name:** `cutlass.cute.MmaAtom.tv_layout_A`

**Signature:** `tv_layout_A`

### tv_layout_B

**Kind:** property

**Qualified name:** `cutlass.cute.MmaAtom.tv_layout_B`

**Signature:** `tv_layout_B`

### tv_layout_C

**Kind:** property

**Qualified name:** `cutlass.cute.MmaAtom.tv_layout_C`

**Signature:** `tv_layout_C`

### make_fragment_A

**Kind:** method

**Qualified name:** `cutlass.cute.MmaAtom.make_fragment_A`

**Signature:** `make_fragment_A(input, *, loc = None, ip = None)`

### make_fragment_B

**Kind:** method

**Qualified name:** `cutlass.cute.MmaAtom.make_fragment_B`

**Signature:** `make_fragment_B(input, *, loc = None, ip = None)`

### make_fragment_C

**Kind:** method

**Qualified name:** `cutlass.cute.MmaAtom.make_fragment_C`

**Signature:** `make_fragment_C(input, *, loc = None, ip = None)`

### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.MmaAtom._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

## CopyAtom

**Kind:** class

**Qualified name:** `cutlass.cute.CopyAtom`

**Signature:** `CopyAtom(op: Op, trait: Trait)`

Bases: [`Atom`](#cutlass.cute.Atom)

The Copy Atom class.

### value_type

**Kind:** property

**Qualified name:** `cutlass.cute.CopyAtom.value_type`

**Signature:** `value_type: Type[cutlass.cute.typing.Numeric]`

### thr_id

**Kind:** property

**Qualified name:** `cutlass.cute.CopyAtom.thr_id`

**Signature:** `thr_id: cutlass.cute.typing.Layout`

### layout_src_tv

**Kind:** property

**Qualified name:** `cutlass.cute.CopyAtom.layout_src_tv`

**Signature:** `layout_src_tv: cutlass.cute.typing.Layout`

### layout_dst_tv

**Kind:** property

**Qualified name:** `cutlass.cute.CopyAtom.layout_dst_tv`

**Signature:** `layout_dst_tv: cutlass.cute.typing.Layout`

### smem_layout

**Kind:** property

**Qualified name:** `cutlass.cute.CopyAtom.smem_layout`

**Signature:** `smem_layout`

Convenience property to access the SMEM layout for TMA copy atoms.

This is a shortcut for `atom.op.smem_layout` that checks if the operation is a TMA operation and provides a clearer error message if not.

#### Returns

The SMEM layout

#### Return type

Layout or ComposedLayout

#### Raises

-   **TypeError** -- If the operation is not a TMA operation

-   **ValueError** -- If the SMEM layout is not set

Example

    >>> layout = tma_atom.smem_layout  # Instead of tma_atom.op.smem_layout

### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.CopyAtom._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

## TiledCopy

**Kind:** class

**Qualified name:** `cutlass.cute.TiledCopy`

**Signature:** `TiledCopy(op: Op, trait: Trait)`

Bases: [`CopyAtom`](#cutlass.cute.CopyAtom)

The tiled Copy class.

### layout_tv_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledCopy.layout_tv_tiled`

**Signature:** `layout_tv_tiled: cutlass.cute.typing.Layout`

### tiler_mn

**Kind:** property

**Qualified name:** `cutlass.cute.TiledCopy.tiler_mn`

**Signature:** `tiler_mn: cutlass.cute.typing.Tile`

### layout_src_tv_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledCopy.layout_src_tv_tiled`

**Signature:** `layout_src_tv_tiled: cutlass.cute.typing.Layout`

### layout_dst_tv_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledCopy.layout_dst_tv_tiled`

**Signature:** `layout_dst_tv_tiled: cutlass.cute.typing.Layout`

### size

**Kind:** property

**Qualified name:** `cutlass.cute.TiledCopy.size`

**Signature:** `size: int`

### get_slice

**Kind:** method

**Qualified name:** `cutlass.cute.TiledCopy.get_slice`

**Signature:** `get_slice(thr_idx: int | cutlass.cute.typing.Int32) → ThrCopy`

### retile

**Kind:** method

**Qualified name:** `cutlass.cute.TiledCopy.retile`

**Signature:** `retile(src, *, loc = None, ip = None)`

### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.TiledCopy._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

## TiledMma

**Kind:** class

**Qualified name:** `cutlass.cute.TiledMma`

**Signature:** `TiledMma(op: Op, trait: Trait)`

Bases: [`MmaAtom`](#cutlass.cute.MmaAtom)

The tiled MMA class.

### tv_layout_A\_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.tv_layout_A_tiled`

**Signature:** `tv_layout_A_tiled`

### tv_layout_B\_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.tv_layout_B_tiled`

**Signature:** `tv_layout_B_tiled`

### tv_layout_C\_tiled

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.tv_layout_C_tiled`

**Signature:** `tv_layout_C_tiled`

### permutation_mnk

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.permutation_mnk`

**Signature:** `permutation_mnk`

### thr_layout_vmnk

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.thr_layout_vmnk`

**Signature:** `thr_layout_vmnk`

### size

**Kind:** property

**Qualified name:** `cutlass.cute.TiledMma.size`

**Signature:** `size: int`

### get_tile_size

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma.get_tile_size`

**Signature:** `get_tile_size(mode_idx: int) → cutlass.cute.typing.Shape`

### get_slice

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma.get_slice`

**Signature:** `get_slice(thr_idx: int | cutlass.cute.typing.Int32) → ThrMma`

### \_partition_shape

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma._partition_shape`

**Signature:** `_partition_shape(operand_id, shape, *, loc = None, ip = None)`

### partition_shape_A

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma.partition_shape_A`

**Signature:** `partition_shape_A(shape_mk, *, loc = None, ip = None)`

### partition_shape_B

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma.partition_shape_B`

**Signature:** `partition_shape_B(shape_nk, *, loc = None, ip = None)`

### partition_shape_C

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma.partition_shape_C`

**Signature:** `partition_shape_C(shape_mn, *, loc = None, ip = None)`

### \_thrfrg

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma._thrfrg`

**Signature:** `_thrfrg(operand_id, input: cutlass.cute.typing.Layout, *, loc = None, ip = None) → cutlass.cute.typing.Layout`

### \_thrfrg_A

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma._thrfrg_A`

**Signature:** `_thrfrg_A(input: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

### \_thrfrg_B

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma._thrfrg_B`

**Signature:** `_thrfrg_B(input: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

### \_thrfrg_C

**Kind:** method

**Qualified name:** `cutlass.cute.TiledMma._thrfrg_C`

**Signature:** `_thrfrg_C(input: cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Layout | cutlass.cute.typing.Tensor`

### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.TiledMma._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

## ThrMma

**Kind:** class

**Qualified name:** `cutlass.cute.ThrMma`

**Signature:** `ThrMma(op: Op, trait: Trait, thr_idx: int | cutlass.cute.typing.Int32)`

Bases: [`TiledMma`](#cutlass.cute.TiledMma)

The thread MMA class for modeling a thread-slice of a tiled MMA.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.ThrMma.__init__`

**Signature:** `__init__(op: Op, trait: Trait, thr_idx: int | cutlass.cute.typing.Int32) → None`

### thr_idx

**Kind:** property

**Qualified name:** `cutlass.cute.ThrMma.thr_idx`

**Signature:** `thr_idx`

### partition_A

**Kind:** method

**Qualified name:** `cutlass.cute.ThrMma.partition_A`

**Signature:** `partition_A(input_mk: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

### partition_B

**Kind:** method

**Qualified name:** `cutlass.cute.ThrMma.partition_B`

**Signature:** `partition_B(input_nk: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

### partition_C

**Kind:** method

**Qualified name:** `cutlass.cute.ThrMma.partition_C`

**Signature:** `partition_C(input_mn: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.ThrMma._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

## ThrCopy

**Kind:** class

**Qualified name:** `cutlass.cute.ThrCopy`

**Signature:** `ThrCopy(op: Op, trait: Trait, thr_idx: int | cutlass.cute.typing.Int32)`

Bases: [`TiledCopy`](#cutlass.cute.TiledCopy)

The thread Copy class for modeling a thread-slice of a tiled Copy.

### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.ThrCopy.__init__`

**Signature:** `__init__(op: Op, trait: Trait, thr_idx: int | cutlass.cute.typing.Int32) → None`

### thr_idx

**Kind:** property

**Qualified name:** `cutlass.cute.ThrCopy.thr_idx`

**Signature:** `thr_idx`

### partition_S

**Kind:** method

**Qualified name:** `cutlass.cute.ThrCopy.partition_S`

**Signature:** `partition_S(src: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

### partition_D

**Kind:** method

**Qualified name:** `cutlass.cute.ThrCopy.partition_D`

**Signature:** `partition_D(dst: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → cutlass.cute.typing.Tensor`

### \_abc_impl

**Kind:** attribute

**Qualified name:** `cutlass.cute.ThrCopy._abc_impl`

**Signature:** `_abc_impl = <_abc._abc_data object>`

## make_atom

**Kind:** function

**Qualified name:** `cutlass.cute.make_atom`

**Signature:** `make_atom(ty, values = None, *, loc = None, ip = None)`

This is a wrapper around the \_cute_ir.make_atom operation, providing default value for the values argument.

## make_mma_atom

**Kind:** function

**Qualified name:** `cutlass.cute.make_mma_atom`

**Signature:** `make_mma_atom(op: MmaOp, *, loc = None, ip = None, **kwargs) → MmaAtom`

Makes an MMA Atom from an MMA Operation.

This function creates an MMA Atom from a given MMA Operation. Arbitrary kw arguments can be provided for Op-specific additional parameters. They are not used as of today.

### Parameters

**op** (*MmaOp*) -- The MMA Operation to construct an Atom for

### Returns

The MMA Atom

### Return type

[MmaAtom](#cutlass.cute.MmaAtom)

## make_tiled_mma

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_mma`

**Signature:** `make_tiled_mma(op_or_atom: Op | MmaAtom, atom_layout_mnk = (1, 1, 1), permutation_mnk = None, *, loc = None, ip = None, **kwargs) → TiledMma`

Makes a tiled MMA from an MMA Operation or an MMA Atom.

### Parameters

-   **op_or_atom** (*Union\[Op,* [*MmaAtom*](#cutlass.cute.MmaAtom)*\]*) -- The MMA Operation or Atom

-   **atom_layout_mnk** (*Layout*) -- A Layout describing the tiling of Atom across threads

-   **permutation_mnk** (*Tiler*) -- A permutation Tiler describing the tiling of Atom across values including any permutation of such tiling

### Returns

The resulting tiled MMA

### Return type

[TiledMma](#cutlass.cute.TiledMma)

## make_copy_atom

**Kind:** function

**Qualified name:** `cutlass.cute.make_copy_atom`

**Signature:** `make_copy_atom(op: CopyOp, copy_internal_type: Type[cutlass.cute.typing.Numeric], *, loc = None, ip = None, **kwargs) → CopyAtom`

Makes a Copy Atom from a Copy Operation.

This function creates a Copy Atom from a given Copy Operation. Arbitrary kw arguments can be provided for Op-specific additional parameters.

Example:

    op = cute.nvgpu.CopyUniversalOp()
    atom = cute.make_copy_atom(op, tensor_dtype, num_bits_per_copy=64)

### Parameters

-   **op** (*CopyOp*) -- The Copy Operation to construct an Atom for

-   **copy_internal_type** (*Type\[Numeric\]*) -- An internal data type used to construct the source/destination layouts in unit of tensor elements

### Returns

The Copy Atom

### Return type

[CopyAtom](#cutlass.cute.CopyAtom)

## make_tiled_copy_tv

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_tv`

**Signature:** `make_tiled_copy_tv(atom: CopyAtom, thr_layout: cutlass.cute.typing.Layout, val_layout: cutlass.cute.typing.Layout, *, loc = None, ip = None) → TiledCopy`

Create a tiled copy given separate thread and value layouts.

A TV partitioner is inferred based on the input layouts. The input thread layout must be compact.

### Parameters

-   **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

-   **thr_layout** (*Layout*) -- Layout mapping from `(TileM,TileN)` coordinates to thread IDs (must be compact)

-   **val_layout** (*Layout*) -- Layout mapping from `(ValueM,ValueN)` coordinates to value IDs

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

### Returns

A tiled copy for the partitioner

### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

## make_tiled_copy

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy`

**Signature:** `make_tiled_copy(atom, layout_tv, tiler_mn, *, loc = None, ip = None)`

Create a tiled type given a TV partitioner and tiler.

### Parameters

-   **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom, e.g. smit_copy and simt_async_copy, tma_load, etc.

-   **layout_tv** (*Layout*) -- Thread-value layout

-   **tiler_mn** (*Tiler*) -- Tile size

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

### Returns

A tiled copy for the partitioner

### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

## make_tiled_copy_S

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_S`

**Signature:** `make_tiled_copy_S(atom, tiled_copy, *, loc = None, ip = None)`

Create a tiled copy out of the copy_atom that matches the Src-Layout of tiled_copy.

### Parameters

-   **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

-   **tiled_copy** ([*TiledCopy*](#cutlass.cute.TiledCopy)) -- Tiled copy

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

### Returns

A tiled copy for the partitioner

### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

## make_tiled_copy_D

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_D`

**Signature:** `make_tiled_copy_D(atom, tiled_copy, *, loc = None, ip = None)`

Create a tiled copy out of the copy_atom that matches the Dst-Layout of tiled_copy.

### Parameters

-   **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

-   **tiled_copy** ([*TiledCopy*](#cutlass.cute.TiledCopy)) -- Tiled copy

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

### Returns

A tiled copy for the partitioner

### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

## make_tiled_copy_A

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_A`

**Signature:** `make_tiled_copy_A(atom, tiled_mma, *, loc = None, ip = None)`

Create a tiled copy out of the copy_atom that matches the A-Layout of tiled_mma.

### Parameters

-   **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

-   **tiled_mma** ([*TiledMma*](#cutlass.cute.TiledMma)) -- Tiled MMA

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

### Returns

A tiled copy for the partitioner

### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

## make_tiled_copy_B

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_B`

**Signature:** `make_tiled_copy_B(atom, tiled_mma, *, loc = None, ip = None)`

Create a tiled copy out of the copy_atom that matches the B-Layout of tiled_mma.

### Parameters

-   **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

-   **tiled_mma** ([*TiledMma*](#cutlass.cute.TiledMma)) -- Tiled MMA

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

### Returns

A tiled copy for the partitioner

### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

## make_tiled_copy_C

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_C`

**Signature:** `make_tiled_copy_C(atom, tiled_mma, *, loc = None, ip = None)`

Create a tiled copy out of the copy_atom that matches the C-Layout of tiled_mma.

### Parameters

-   **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

-   **tiled_mma** ([*TiledMma*](#cutlass.cute.TiledMma)) -- Tiled MMA

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

### Returns

A tiled copy for the partitioner

### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

## make_tiled_copy_C\_atom

**Kind:** function

**Qualified name:** `cutlass.cute.make_tiled_copy_C_atom`

**Signature:** `make_tiled_copy_C_atom(atom: CopyAtom, mma: TiledMma, *, loc = None, ip = None)`

Create the smallest tiled copy that can retile LayoutC_TV for use with pipelined epilogues with subtiled stores.

### Parameters

-   **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom

-   **mma** ([*TiledMma*](#cutlass.cute.TiledMma)) -- Tiled MMA

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

### Returns

A tiled copy for partitioner

### Return type

[TiledCopy](#cutlass.cute.TiledCopy)

### Raises

**ValueError** -- If the number value of CopyAtom's source layout is greater than the size of TiledMma's LayoutC_TV

## make_cotiled_copy

**Kind:** function

**Qualified name:** `cutlass.cute.make_cotiled_copy`

**Signature:** `make_cotiled_copy(atom: CopyAtom, atom_layout_tv: cutlass.cute.typing.Layout, data_layout: cutlass.cute.typing.Layout, *, loc = None, ip = None) → TiledCopy`

Produce a TiledCopy from thread and value offset maps. The TV Layout maps threads and values to the codomain of the data_layout. It is verified that the intended codomain is valid within data_layout. Useful when threads and values don't care about owning specific coordinates, but care more about the vector-width and offsets between them.

### Parameters

-   **atom** (*copy atom,* *e.g. simt_copy and simt_async_copy,* *tgen05.st,* *etc.*)

-   **atom_layout_tv** (*(tid,* *vid)* *-\> data addr*)

-   **data_layout** (*data coord -\> data addr*)

-   **loc** (*source location for mlir* *(optional)*)

-   **ip** (*insertion point* *(optional)*)

### Returns

A tuple of A tiled copy and atom

### Return type

tiled_copy

## copy_atom_call

**Kind:** function

**Qualified name:** `cutlass.cute.copy_atom_call`

**Signature:** `copy_atom_call(atom: CopyAtom, src: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...], dst: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...], *, pred: cutlass.cute.typing.Tensor | None = None, loc = None, ip = None, **kwargs) → None`

Execute a single copy atom operation.

The copy_atom_call operation executes a copy atom with the given operands. Source and destination tensors have layout profile `(V)`.

The `V-mode` represents either:

-   A singular mode directly consumable by the provided Copy Atom

-   A composite mode requiring recursive decomposition, structured as `(V, Rest...)`,

For src/dst layout like `(V, Rest...)`, the layout profile of `pred` must match `(Rest...)`.

-   Certain Atoms may require additional operation-specific keyword arguments.

-   Current implementation limits `V-mode` rank to 2 or less. Support for higher ranks is planned for future releases.

Both `src` and `dst` operands are variadic, containing a variable number of tensors:

-   For regular copy, `src` and `dst` each contain a single tensor.

-   For copy with auxiliary operands, they contain the main tensor followed by auxiliary tensors. For example:

### Parameters

-   **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom specifying the transfer operation

-   **src** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- Source tensor(s) with layout profile `(V)`. Can be a single Tensor or a list/tuple of Tensors for operations with auxiliary source operands.

-   **dst** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- Destination tensor(s) with layout profile `(V)`. Can be a single Tensor or a list/tuple of Tensors for operations with auxiliary destination operands.

-   **pred** (*Optional\[Tensor\],* *optional*) -- Optional predication tensor for conditional transfers, defaults to None

-   **loc** (*Any,* *optional*) -- Source location information, defaults to None

-   **ip** (*Any,* *optional*) -- Insertion point, defaults to None

-   **kwargs** (*Dict\[str,* *Any\]*) -- Additional copy atom specific arguments

### Raises

**TypeError** -- If source and destination element type bit widths differ

### Returns

None

### Return type

None

**Examples**:

    # Regular copy atom operation
    cute.copy_atom_call(copy_atom, src, dst)

    # Predicated copy atom operation
    cute.copy_atom_call(copy_atom, src, dst, pred=pred)

## mma_atom_call

**Kind:** function

**Qualified name:** `cutlass.cute.mma_atom_call`

**Signature:** `mma_atom_call(atom: MmaAtom, d: cutlass.cute.typing.Tensor, a: cutlass.cute.typing.Tensor, b: cutlass.cute.typing.Tensor, c: cutlass.cute.typing.Tensor, *, loc = None, ip = None, **kwargs) → None`

Execute a single MMA atom operation.

The mma_atom_call operation executes an MMA atom with the given operands. This performs a matrix multiplication and accumulation operation: D = A \* B + C

Note: The tensors 'd', 'a', 'b', and 'c' must only have a single fragment.

### Parameters

-   **atom** ([*MmaAtom*](#cutlass.cute.MmaAtom)) -- The MMA atom to execute

-   **d** (*Tensor*) -- Destination tensor (output accumulator)

-   **a** (*Tensor*) -- First source tensor (matrix A)

-   **b** (*Tensor*) -- Second source tensor (matrix B)

-   **c** (*Tensor*) -- Third source tensor (input accumulator C)

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

Examples:

    # Call an MMA atom operation
    cute.mma_atom_call(mma_atom, d_tensor, a_tensor, b_tensor, c_tensor)

## gemm

**Kind:** function

**Qualified name:** `cutlass.cute.gemm`

**Signature:** `gemm(atom: MmaAtom, d: cutlass.cute.typing.Tensor, a: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...], b: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...], c: cutlass.cute.typing.Tensor, *, loc = None, ip = None, **kwargs) → None`

The GEMM algorithm.

Computes `D <- A * B + C` where `C` and `D` can alias. Note that some MMA Atoms (e.g. warpgroup-wide or tcgen05 MMAs) require manually setting an "accumulate" boolean field.

All tensors must be partitioned according to the provided MMA Atom.

For MMA Atoms that require single-threaded execution, the gemm op automatically handles thread election internally. Manual thread selection is not required in such cases.

Following dispatch rules are supported:

-   Dispatch \[1\]: (V) x (V) =\> (V) =\> (V,1,1) x (V,1,1) =\> (V,1,1)

-   Dispatch \[2\]: (M) x (N) =\> (M,N) =\> (1,M,1) x (1,N,1) =\> (1,M,N)

-   Dispatch \[3\]: (M,K) x (N,K) =\> (M,N) =\> (1,M,K) x (1,N,K) =\> (1,M,N)

-   Dispatch \[4\]: (V,M) x (V,N) =\> (V,M,N) =\> (V,M,1) x (V,N,1) =\> (V,M,N)

-   Dispatch \[5\]: (V,M,K) x (V,N,K) =\> (V,M,N)

Operand flexibility:

-   a and b can be a single Tensor (regular GEMM) or a sequence \[operand, scale_factor\] for block-scaled GEMM.

### Parameters

-   **atom** ([*MmaAtom*](#cutlass.cute.MmaAtom)) -- MMA atom

-   **d** (*Tensor*) -- Destination tensor

-   **a** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- First source tensor or sequence for advanced modes (e.g., \[a, sfa\])

-   **b** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- Second source tensor or sequence for advanced modes (e.g., \[b, sfb\])

-   **c** (*Tensor*) -- Third source tensor

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point for MLIR, defaults to None

-   **kwargs** (*dict*) -- Additional keyword arguments

### Returns

None

### Return type

None

## copy

**Kind:** function

**Qualified name:** `cutlass.cute.copy`

**Signature:** `copy(atom: CopyAtom, src: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...], dst: cutlass.cute.typing.Tensor | List[cutlass.cute.typing.Tensor] | Tuple[cutlass.cute.typing.Tensor, ...], *, pred: cutlass.cute.typing.Tensor | None = None, loc = None, ip = None, **kwargs) → None`

Facilitates data transfer between two tensors conforming to layout profile `(V, Rest...)`.

### Parameters

-   **atom** ([*CopyAtom*](#cutlass.cute.CopyAtom)) -- Copy atom specifying the transfer operation

-   **src** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- Source tensor or list of tensors with layout profile `(V, Rest...)`

-   **dst** (*Union\[Tensor,* *List\[Tensor\],* *Tuple\[Tensor,* *\...\]\]*) -- Destination tensor or list of tensors with layout profile `(V, Rest...)`

-   **pred** (*Optional\[Tensor\],* *optional*) -- Optional predication tensor for conditional transfers, defaults to None

-   **loc** (*Any,* *optional*) -- Source location information, defaults to None

-   **ip** (*Any,* *optional*) -- Insertion point, defaults to None

-   **kwargs** (*Dict\[str,* *Any\]*) -- Additional copy atom specific arguments

### Raises

-   **TypeError** -- If source and destination element type bit widths differ

-   **ValueError** -- If source and destination ranks differ

-   **ValueError** -- If source and destination mode-1 sizes differ

-   **NotImplementedError** -- If `V-mode` rank exceeds 2

### Returns

None

### Return type

None

The `V-mode` represents either:

-   A singular mode directly consumable by the provided Copy Atom

-   A composite mode requiring recursive decomposition, structured as `(V, Rest...)`, and src/dst layout like `((V, Rest...), Rest...)`

The algorithm recursively processes the `V-mode`, decomposing it until reaching the minimum granularity compatible with the provided Copy Atom's requirements.

Source and destination tensors must be partitioned in accordance with the Copy Atom specifications. Post-partitioning, both tensors will exhibit a `(V, Rest...)` layout profile.

The operands src and dst are variadic, each containing a variable number of tensors:

-   For regular copy, src and dst contain single source and destination tensors respectively.

-   For copy with auxiliary operands, src and dst contain the primary tensors followed by their respective auxiliary tensors.

**Precondition:** The size of mode 1 must be equal for both source and destination tensors: `size(src, mode=[1]) == size(dst, mode=[1])`

**Examples**:

TMA copy operation with multicast functionality:

    cute.copy(tma_atom, src, dst, tma_bar_ptr=mbar_ptr, mcast_mask=mask, cache_policy=policy)

Optional predication is supported through an additional tensor parameter. For partitioned tensors with logical profile `((ATOM_V,ATOM_REST),REST,...)`, the predication tensor must maintain profile compatibility with `(ATOM_REST,REST,...)`.

For Copy Atoms requiring single-threaded execution, thread election is managed automatically by the copy operation. External thread selection mechanisms are not necessary.

> **Note.**
>
> -   Certain Atoms may require additional operation-specific keyword arguments.
>
> -   Current implementation limits `V-mode` rank to 2 or less. Support for higher ranks is planned for future releases.

## basic_copy

**Kind:** function

**Qualified name:** `cutlass.cute.basic_copy`

**Signature:** `basic_copy(src: cutlass.cute.typing.Tensor, dst: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → None`

Performs a basic element-wise copy.

This functions **assumes** the following pre-conditions: 1. size(src) == size(dst)

When the src and dst shapes are static, the pre-conditions are actually verified and the element-wise loop is fully unrolled.

### Parameters

-   **src** (*Tensor*) -- Source tensor

-   **dst** (*Tensor*) -- Destination tensor

-   **loc** (*Optional\[Location\],* *optional*) -- Source location for MLIR, defaults to None

-   **ip** (*Optional\[InsertionPoint\],* *optional*) -- Insertion point, defaults to None

## basic_copy_if

**Kind:** function

**Qualified name:** `cutlass.cute.basic_copy_if`

**Signature:** `basic_copy_if(pred: cutlass.cute.typing.Tensor, src: cutlass.cute.typing.Tensor, dst: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → None`

Performs a basic predicated element-wise copy.

This functions **assumes** the following pre-conditions: 1. size(src) == size(dst) 2. size(src) == size(pred)

When all shapes are static, the pre-conditions are actually verified and the element-wise loop is fully unrolled.

## autovec_copy

**Kind:** function

**Qualified name:** `cutlass.cute.autovec_copy`

**Signature:** `autovec_copy(src: cutlass.cute.typing.Tensor, dst: cutlass.cute.typing.Tensor, *, l1c_evict_priority: CacheEvictionPriority = cutlass._mlir.dialects.cute.CacheEvictionPriority.EVICT_NORMAL, loc = None, ip = None) → None`

Auto-vectorization SIMT copy policy.

Given a source and destination tensors that are statically shaped, this policy figures out the largest safe vector width that the copy instruction can take and performs the copy.

## prefetch

**Kind:** function

**Qualified name:** `cutlass.cute.prefetch`

**Signature:** `prefetch(atom: CopyAtom, src: cutlass.cute.typing.Tensor, *, loc = None, ip = None) → None`

The Prefetch algorithm.

The "prefetch" expects source tensors to be partitioned according to the provided Copy Atom. Prefetch is used for loading tensors from global memory to L2.

Prefetch accepts Copy Atom but not all are allowed. Currently, only supports TMA prefetch.

    cute.prefetch(tma_prefetch, src)

For Copy Atoms that require single-threaded execution, the copy op automatically handles thread election internally. Manual thread selection is not required in such cases.

## acos

**Kind:** function

**Qualified name:** `cutlass.cute.acos`

**Signature:** `acos(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise arc cosine of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the arc cosine of each element in input tensor

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = acos(y)  # Compute arc cosine

## asin

**Kind:** function

**Qualified name:** `cutlass.cute.asin`

**Signature:** `asin(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise arc sine of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the arc sine of each element in input tensor

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = asin(y)  # Compute arc sine

## atan

**Kind:** function

**Qualified name:** `cutlass.cute.atan`

**Signature:** `atan(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise arc tangent of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the arc tangent of each element in input tensor

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = atan(y)  # Compute arc tangent

## atan2

**Kind:** function

**Qualified name:** `cutlass.cute.atan2`

**Signature:** `atan2(a: TensorSSA | cutlass.cute.typing.Numeric, b: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise arc tangent of two tensors.

Computes atan2(a, b) element-wise. The function atan2(a, b) is the angle in radians between the positive x-axis and the point given by the coordinates (b, a).

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- First input tensor (y-coordinates)

-   **b** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Second input tensor (x-coordinates)

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the arc tangent of a/b element-wise

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    y = cute.make_rmem_tensor(ptr1, layout).load()  # y coordinates
    x = cute.make_rmem_tensor(ptr2, layout).load()  # x coordinates
    theta = atan2(y, x)  # Compute angles

## cos

**Kind:** function

**Qualified name:** `cutlass.cute.cos`

**Signature:** `cos(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise cosine of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor (in radians)

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the cosine of each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = cos(y)  # Compute cosine

## erf

**Kind:** function

**Qualified name:** `cutlass.cute.erf`

**Signature:** `erf(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise error function of the input tensor.

The error function is defined as: erf(x) = 2/√π ∫\[0 to x\] exp(-t²) dt

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the error function value for each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = erf(y)  # Compute error function

## exp

**Kind:** function

**Qualified name:** `cutlass.cute.exp`

**Signature:** `exp(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise exponential of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the exponential of each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = exp(y)  # Compute exponential

## exp2

**Kind:** function

**Qualified name:** `cutlass.cute.exp2`

**Signature:** `exp2(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise base-2 exponential of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing 2 raised to the power of each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = exp2(y)  # Compute 2^x

## log

**Kind:** function

**Qualified name:** `cutlass.cute.log`

**Signature:** `log(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise natural logarithm of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the natural logarithm of each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = log(y)  # Compute natural logarithm

## log10

**Kind:** function

**Qualified name:** `cutlass.cute.log10`

**Signature:** `log10(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise base-10 logarithm of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the base-10 logarithm of each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = log10(y)  # Compute log base 10

## log2

**Kind:** function

**Qualified name:** `cutlass.cute.log2`

**Signature:** `log2(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise base-2 logarithm of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the base-2 logarithm of each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = log2(y)  # Compute log base 2

## rsqrt

**Kind:** function

**Qualified name:** `cutlass.cute.rsqrt`

**Signature:** `rsqrt(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise reciprocal square root of the input tensor.

Computes 1/√x element-wise.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the reciprocal square root of each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = rsqrt(y)  # Compute 1/√x

## sin

**Kind:** function

**Qualified name:** `cutlass.cute.sin`

**Signature:** `sin(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise sine of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor (in radians)

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the sine of each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = sin(y)  # Compute sine

## sqrt

**Kind:** function

**Qualified name:** `cutlass.cute.sqrt`

**Signature:** `sqrt(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise square root of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the square root of each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = sqrt(y)  # Compute square root

## tan

**Kind:** function

**Qualified name:** `cutlass.cute.tan`

**Signature:** `tan(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise tangent of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor (in radians)

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the tangent of each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = tan(y)  # Compute tangent

## tanh

**Kind:** function

**Qualified name:** `cutlass.cute.tanh`

**Signature:** `tanh(a: TensorSSA | cutlass.cute.typing.Numeric, fastmath: bool = False) → TensorSSA | cutlass.cute.typing.Numeric`

Compute element-wise hyperbolic tangent of the input tensor.

### Parameters

-   **a** (*Union\[*[*TensorSSA*](#cutlass.cute.TensorSSA)*,* *Numeric\]*) -- Input tensor

-   **fastmath** (*bool,* *optional*) -- Enable fast math optimizations, defaults to False

### Returns

Tensor containing the hyperbolic tangent of each element

### Return type

Union\[[TensorSSA](#cutlass.cute.TensorSSA), Numeric\]

Example:

    x = cute.make_rmem_tensor(layout)  # Create tensor
    y = x.load()  # Load values
    z = tanh(y)  # Compute hyperbolic tangent
