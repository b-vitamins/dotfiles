---
title: "Runtime"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_runtime.html"
cutlass_docs_version: "4.8.0"
generated_at: "2026-10-13T17:05:03+00:00"
last_updated: "Sep 21, 2026"
---

# Runtime

> **Note.**
>
> `cutlass.cute.runtime` is kept for compatibility. Runtime APIs are being moved to `cutlass.runtime`; prefer the new namespace when it is available.

## API documentation

### \_Pointer

**Kind:** class

**Qualified name:** `cutlass.cute.runtime._Pointer`

**Signature:** `_Pointer(*args: Any, **kwargs: Any)`

Bases: `Pointer`

Runtime representation of a pointer that can inter-operate with various data structures, including numpy arrays and device memory.

#### Parameters

- **pointer** (*int* *or* *pointer-like object*) -- The pointer to the data

- **dtype** (*Type*) -- Data type of the elements pointed to

- **mem_space** ([*AddressSpace*](cute.md#cutlass.cute.AddressSpace)*,* *optional*) -- Memory space where the pointer resides, defaults to generic

- **assumed_align** (*int,* *optional*) -- Assumed alignment of input pointer in bytes, defaults to None

#### Variables

- **\_pointer** -- The underlying pointer

- **\_dtype** -- Data type of the elements

- **\_addr_space** -- Memory space of the pointer

- **\_assumed_align** -- Alignment of the pointer in bytes

- **\_desc** -- C-type descriptor for the pointer

- **\_c_pointer** -- C-compatible pointer representation

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Pointer.__init__`

**Signature:** `__init__(pointer: int, dtype: Type[cutlass.cute.typing.Numeric], mem_space: cutlass.cute.typing.AddressSpace = cutlass.cute.typing.AddressSpace.generic, assumed_align: int | None = None) → None`

#### size_in_bytes

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Pointer.size_in_bytes`

**Signature:** `size_in_bytes() → int`

#### mlir_type

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Pointer.mlir_type`

**Signature:** `mlir_type: _install_cutlass_mlir_autodoc_stub.<locals>._DocMlirType`

#### dtype

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Pointer.dtype`

**Signature:** `dtype: Type[cutlass.cute.typing.Numeric]`

#### memspace

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Pointer.memspace`

**Signature:** `memspace: cutlass.cute.typing.AddressSpace`

#### align

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Pointer.align`

**Signature:** `align(min_align: int) → cutlass.cute.typing.Pointer`

### \_Tensor

**Kind:** class

**Qualified name:** `cutlass.cute.runtime._Tensor`

**Signature:** `_Tensor(*args: Any, **kwargs: Any)`

Bases: `Tensor`

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Tensor.__init__`

**Signature:** `__init__(tensor: object, assumed_align: int | None = None, use_32bit_stride: bool = False, *, enable_tvm_ffi: bool = False) → None`

#### load_dltensor

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Tensor.load_dltensor`

**Signature:** `load_dltensor() → None`

Lazily load the DLTensorWrapper.

This function loads the DLTensorWrapper when needed, avoiding overhead in the critical path of calling JIT functions.

#### mark_layout_dynamic

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Tensor.mark_layout_dynamic`

**Signature:** `mark_layout_dynamic(leading_dim: int | None = None) → _Tensor`

Marks the tensor layout as dynamic based on the leading dimension.

##### Parameters

**leading_dim** (*int,* *optional*) -- The leading dimension of the layout, defaults to None

When `leading_dim` is None, the leading dimension is deduced as follows:

- If exactly one dimension has stride 1, that dimension is used.

- If multiple dimensions have stride 1 but exactly one of them has size \> 1, that dimension is used.

- If multiple dimensions have stride 1 but none or more than one has size \> 1, an error is raised.

- If no dimension has stride 1, all strides remain dynamic.

When `leading_dim` is explicitly specified, marks the layout as dynamic while setting the stride at `leading_dim` to 1. Also validates that the specified `leading_dim` is consistent with the existing layout by checking that the corresponding stride of that dimension is 1.

Limitation: only support flat layout for now. Will work on supporting nested layout in the future.

##### Returns

The tensor with dynamic layout

##### Return type

[\_Tensor](#cutlass.cute.runtime._Tensor)

#### mark_compact_shape_dynamic

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Tensor.mark_compact_shape_dynamic`

**Signature:** `mark_compact_shape_dynamic(mode: int, stride_order: tuple[int, ...] | None = None, divisibility: int = 1) → _Tensor`

Marks the tensor shape as dynamic and propagates dynamic and divisibility information to the corresponding strides.

##### Parameters

- **mode** (*int*) -- The mode of the compact shape, defaults to 0

- **stride_order** (*tuple\[int, ...\], optional*) -- Consistent with torch.Tensor.dim_order. Defaults to None.

- **divisibility** (*int, optional*) -- The divisibility constraint for the compact shape, defaults to 1

Indicates the order of the modes (dimensions) if the current layout were converted to row-major order. It starts from the outermost to the innermost dimension.

##### Returns

The tensor with dynamic compact shape

##### Return type

\_Tensor

If `stride_order` is not provided, the stride ordering will be automatically deduced from the layout. Automatic deduction is only possible when exactly one dimension has a stride of 1 (compact layout). An error is raised if automatic deduction fails.

If `stride_order` is explicitly specified, it does the consistency check with the layout.

For example:

- Layout: (4,2):(1,4) has stride_order: (1,0) indicates the innermost dimension is 0(4:1), the outermost dimension is 1(2:4)

- Layout: (5,3,2,4):(3,1,15,30) has stride_order: (3,2,0,1) indicates the innermost dimension is 1(3:1), the outermost dimension is 3(4:30).

Using torch.Tensor.dim_order() to get the stride order of the torch tensor.

``` python
a = torch.empty(3, 4)
t = cute.runtime.from_dlpack(a)
t = t.mark_compact_shape_dynamic(mode=0, stride_order=a.dim_order())
```

#### element_type

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.element_type`

**Signature:** `element_type: Type[cutlass.cute.typing.Numeric]`

#### dtype

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.dtype`

**Signature:** `dtype: Type[cutlass.cute.typing.Numeric]`

#### memspace

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.memspace`

**Signature:** `memspace: cutlass.cute.typing.AddressSpace`

#### size_in_bytes

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.size_in_bytes`

**Signature:** `size_in_bytes: int`

#### mlir_type

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.mlir_type`

**Signature:** `mlir_type: _install_cutlass_mlir_autodoc_stub.<locals>._DocMlirType`

#### iterator

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.iterator`

**Signature:** `iterator: _Pointer`

#### layout

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.layout`

**Signature:** `layout: NoReturn`

#### shape

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.shape`

**Signature:** `shape: cutlass.cute.typing.Shape`

#### stride

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.stride`

**Signature:** `stride: cutlass.cute.typing.Stride`

#### leading_dim

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.leading_dim`

**Signature:** `leading_dim: int | tuple[int, ...] | None`

Get the leading dimension of this Tensor.

##### Returns

The leading dimension index or indices

##### Return type

int or tuple or None

The return value depends on the tensor's stride pattern:

- If a single leading dimension is found, returns an integer index

- If nested leading dimensions are found, returns a tuple of indices

- If no leading dimension is found, returns None

#### fill

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Tensor.fill`

**Signature:** `fill(value: cutlass.cute.typing.Numeric) → None`

#### data_ptr

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.data_ptr`

**Signature:** `data_ptr: int`

#### dynamic_shapes_mask

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.dynamic_shapes_mask`

**Signature:** `dynamic_shapes_mask: tuple[int, ...]`

Get the mask of dynamic shapes in the tensor.

#### dynamic_strides_mask

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.dynamic_strides_mask`

**Signature:** `dynamic_strides_mask: tuple[int, ...]`

Get the mask of dynamic strides in the tensor.

### \_FakeTensor

**Kind:** class

**Qualified name:** `cutlass.cute.runtime._FakeTensor`

**Signature:** `_FakeTensor(*args: Any, **kwargs: Any)`

Bases: `Tensor`

Fake Tensor implementation as a placeholder. It mimics the interface of Tensor, but does not hold real data or allow indexing. Used for compilation or testing situations where only shape/type/layout information is needed. All attempts to access or mutate data will raise errors.

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._FakeTensor.__init__`

**Signature:** `__init__(dtype: Type[cutlass.cute.typing.Numeric], shape: tuple[int | cutlass.cute.typing.SymInt, ...], *, stride: tuple[int | cutlass.cute.typing.SymInt, ...], memspace: cutlass.cute.typing.AddressSpace = cutlass.cute.typing.AddressSpace.gmem, assumed_align: int | None = None, use_32bit_stride: bool = False) → None`

#### mlir_type

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.mlir_type`

**Signature:** `mlir_type: _install_cutlass_mlir_autodoc_stub.<locals>._DocMlirType`

#### element_type

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.element_type`

**Signature:** `element_type: Type[cutlass.cute.typing.Numeric]`

#### dtype

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.dtype`

**Signature:** `dtype: Type[cutlass.cute.typing.Numeric]`

#### memspace

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.memspace`

**Signature:** `memspace: cutlass.cute.typing.AddressSpace`

#### iterator

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.iterator`

**Signature:** `iterator: NoReturn`

#### shape

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.shape`

**Signature:** `shape: cutlass.cute.typing.Shape`

#### stride

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.stride`

**Signature:** `stride: cutlass.cute.typing.Stride`

#### leading_dim

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.leading_dim`

**Signature:** `leading_dim: int | tuple[int, ...] | None`

#### dynamic_shapes_mask

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.dynamic_shapes_mask`

**Signature:** `dynamic_shapes_mask: tuple[int, ...]`

#### dynamic_strides_mask

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.dynamic_strides_mask`

**Signature:** `dynamic_strides_mask: tuple[int, ...]`

#### fill

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._FakeTensor.fill`

**Signature:** `fill(value: cutlass.cute.typing.Numeric) → None`

### make_fake_compact_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.runtime.make_fake_compact_tensor`

**Signature:** `make_fake_compact_tensor(dtype: Type[cutlass.cute.typing.Numeric], shape: tuple[int | cutlass.cute.typing.SymInt, ...], *, stride_order: tuple[int, ...] | None = None, memspace: cutlass.cute.typing.AddressSpace = cutlass.cute.typing.AddressSpace.gmem, assumed_align: int | None = None, use_32bit_stride: bool = False) → _FakeTensor`

Create a fake tensor descriptor with a compact layout derived from shape.

This is the usual builder for `cute.compile(...)` when the logical tensor is compact and you want the runtime stride tuple to be derived automatically from `shape` and `stride_order`. Each entry in `shape` may be a static Python `int` or a dynamic `SymInt`. Dynamic dimensions become runtime-bound scalar parameters on the compiled callable.

#### Parameters

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of the tensor elements.

- **shape** (*tuple\[int* *\|* *SymInt,* *\...\]*) -- Tensor extents in elements, one per mode. Each entry may be static (`int`) or dynamic (`SymInt`).

- **stride_order** (*tuple\[int,* *\...\],* *optional*) -- Permutation describing which mode is fastest-changing. `0` means the innermost / stride-1 mode, `len(shape)-1` the slowest-changing mode. If omitted, the default is left-to-right order `(0, 1, ..., n-1)`.

- **memspace** ([*AddressSpace*](cute.md#cutlass.cute.AddressSpace)*,* *optional*) -- Memory space where the fake tensor resides. Defaults to AddressSpace.gmem.

- **assumed_align** (*int,* *optional*) -- Assumed byte alignment of the base pointer. If `None`, defaults to one element width in bytes (and at least 1).

- **use_32bit_stride** (*bool,* *optional*) -- Use 32-bit symbolic strides instead of 64-bit ones for dynamic layouts. This only affects dynamically-derived stride entries and is useful when the compact layout provably fits in int32.

#### Returns

An instance of a fake tensor with the given properties and compact layout.

#### Return type

[\_FakeTensor](#cutlass.cute.runtime._FakeTensor)

Use [`make_fake_tensor()`](#cutlass.cute.runtime.make_fake_tensor) instead when the logical layout is non-compact or when you need to spell the stride tuple explicitly.

**Examples:**

    @cute.jit
    def foo(x: cute.Tensor):
        ...

    x = make_fake_compact_tensor(
        cutlass.Float32, (100, cute.sym_int32(divisibility=8)), stride_order=(1, 0)
    )

    # Compiled function will take a tensor with the type:
    #   tensor<ptr<f32, generic> o (100,?{div=8}):(?{i32 div=8},1)>
    compiled_foo = cute.compile(foo, x)

    # Default stride order is left-to-right order (0, 1, ..., n-1)
    y = make_fake_compact_tensor(cutlass.Float32, (8, 3, 2))
    assert y.stride == (1, 8, 24)

### make_fake_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.runtime.make_fake_tensor`

**Signature:** `make_fake_tensor(dtype: Type[cutlass.cute.typing.Numeric], shape: tuple[int | cutlass.cute.typing.SymInt, ...], stride: tuple[int | cutlass.cute.typing.SymInt, ...], *, memspace: cutlass.cute.typing.AddressSpace = cutlass.cute.typing.AddressSpace.gmem, assumed_align: int | None = None) → _FakeTensor`

Create a fake tensor descriptor with an explicit layout.

Use this builder for `cute.compile(...)` when the logical tensor layout is not compact, when you already know the exact stride tuple, or when you want fake-tensor layout to match an external contract exactly. `shape` and `stride` are both expressed in elements, not bytes.

#### Parameters

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of the tensor elements.

- **shape** (*tuple\[int* *\|* *SymInt,* *\...\]*) -- Tensor extents in elements, one per mode. Each entry may be static (`int`) or dynamic (`SymInt`). Dynamic dimensions become runtime-bound scalar parameters on the compiled callable.

- **stride** (*tuple\[int* *\|* *SymInt,* *\...\]*) -- Explicit stride tuple in elements. Must have the same rank as `shape`. Each entry may be static (`int`) or dynamic (`SymInt`).

- **memspace** ([*AddressSpace*](cute.md#cutlass.cute.AddressSpace)*,* *optional*) -- Memory space where the fake tensor resides. Defaults to AddressSpace.gmem.

- **assumed_align** (*int,* *optional*) -- Assumed byte alignment of the base pointer. If `None`, defaults to one element width in bytes (and at least 1).

#### Returns

An instance of a fake tensor with the given properties.

#### Return type

[\_FakeTensor](#cutlass.cute.runtime._FakeTensor)

If the same runtime symbolic quantity appears in multiple positions, reuse the same `SymInt` object at every occurrence. Different `SymInt` objects are treated as distinct runtime parameters even if they share the same `symbol` string.

Use [`make_fake_compact_tensor()`](#cutlass.cute.runtime.make_fake_compact_tensor) instead when the layout is compact and you want the stride tuple inferred from `shape` and a mode order.

**Examples:**

    @cute.jit
    def foo(x: cute.Tensor):
        ...

    sym_m = cute.sym_int64(symbol="M")
    sym_ld = cute.sym_int64(divisibility=16, symbol="LD")

    # Row-major logical layout: contiguous K dimension, explicit leading dim.
    x = make_fake_tensor(
        cutlass.Float16,
        shape=(sym_m, 128),
        stride=(sym_ld, 1),
    )

    compiled_foo = cute.compile(foo, x)

### from_dlpack

**Kind:** function

**Qualified name:** `cutlass.cute.runtime.from_dlpack`

**Signature:** `from_dlpack(tensor_dlpack: object, assumed_align: int | None = None, use_32bit_stride: bool = False, *, enable_tvm_ffi: bool = False, force_tf32: bool = False) → cutlass.cute.typing.Tensor`

Convert from tensor object supporting \_\_dlpack\_\_() to a CuTe Tensor.

#### Parameters

- **tensor_dlpack** (*object*) -- Tensor object that supports the DLPack protocol

- **assumed_align** (*int,* *optional*) -- Assumed alignment of the tensor (bytes), defaults to None, if None, will use the element size bytes as the assumed alignment.

- **use_32bit_stride** (*bool,* *optional*) -- Whether to use 32-bit stride, defaults to False. When True, the dynamic stride bitwidth will be set to 32 for small problem size (cosize(layout) \<= Int32_max) for better performance. This is only applied when the dimension is dynamic.

- **enable_tvm_ffi** (*bool,* *optional*) -- Whether to enable TVM-FFI, defaults to False. When True, the tensor will be converted to a TVM-FFI function compatible tensor.

- **force_tf32** (*bool,* *optional*) -- Whether to force the element type to TFloat32 if the element type is Float32.

#### Returns

A CuTe Tensor object

#### Return type

Tensor

For packed subbyte torch dtypes such as `torch.float4_e2m1fn_x2`, `from_dlpack` returns the logical element layout expected by CuTe instead of the packed storage layout. For example, a torch tensor with shape `(128, 128)` and dtype `torch.float4_e2m1fn_x2` is exposed as a logical FP4 tensor with shape `(128, 256)`.

**Examples:**

    import torch
    from cutlass.cute.runtime import from_dlpack
    x = torch.randn(100, 100)
    y = from_dlpack(x)
    y.shape
    # (100, 100)
    type(y)
    # <class 'cutlass.cute.Tensor'>

### make_ptr

**Kind:** function

**Qualified name:** `cutlass.cute.runtime.make_ptr`

**Signature:** `make_ptr(dtype: Type[cutlass.cute.typing.Numeric], value: int | _Pointer, mem_space: cutlass.cute.typing.AddressSpace = cutlass.cute.typing.AddressSpace.generic, assumed_align: int | None = None) → cutlass.cute.typing.Pointer`

Create a pointer from a memory address

#### Parameters

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of the pointer elements

- **value** (*Union\[int,* *ctypes.\_Pointer\]*) -- Memory address as integer or ctypes pointer

- **mem_space** ([*AddressSpace*](cute.md#cutlass.cute.AddressSpace)*,* *optional*) -- Memory address space, defaults to AddressSpace.generic

- **align_bytes** (*int,* *optional*) -- Alignment in bytes, defaults to None

#### Returns

A pointer object

#### Return type

[Pointer](../basic_data_types.md#cutlass.Pointer)

    import numpy as np
    import ctypes

    from cutlass import Float32
    from cutlass.cute.runtime import make_ptr

    # Create a numpy array
    a = np.random.randn(16, 32).astype(np.float32)

    # Get pointer address as integer
    ptr_address = a.ctypes.data_as(ctypes.POINTER(ctypes.c_float))

    # Create pointer from address
    y = make_ptr(cutlass.Float32, ptr_address)

    # Check properties
    print(y.element_type)
    print(type(y))  # <class 'cutlass.cute.Pointer'>

### nullptr

**Kind:** function

**Qualified name:** `cutlass.cute.runtime.nullptr`

**Signature:** `nullptr(dtype: Type[cutlass.cute.typing.Numeric], mem_space: cutlass.cute.typing.AddressSpace = cutlass.cute.typing.AddressSpace.generic, assumed_align: int | None = None) → cutlass.cute.typing.Pointer`

Create a null pointer which is useful for compilation

#### Parameters

- **dtype** (*Type\[*[*Numeric*](../basic_data_types.md#cutlass.Numeric)*\]*) -- Data type of the pointer elements

- **mem_space** ([*AddressSpace*](cute.md#cutlass.cute.AddressSpace)*,* *optional*) -- Memory address space, defaults to AddressSpace.generic

#### Returns

A null pointer object

#### Return type

[Pointer](../basic_data_types.md#cutlass.Pointer)

### TensorAdapter

**Kind:** class

**Qualified name:** `cutlass.cute.runtime.TensorAdapter`

**Signature:** `TensorAdapter(arg: object)`

Bases: `object`

Convert a DLPack protocol supported tensor/array to a cute tensor.

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.runtime.TensorAdapter.__init__`

**Signature:** `__init__(arg: object) → None`
