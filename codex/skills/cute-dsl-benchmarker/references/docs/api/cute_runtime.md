---
title: "Runtime"
source_url: "https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_api/cute_runtime.html"
cutlass_docs_version: "4.4.1"
generated_at: "2026-03-07T11:33:21+00:00"
last_updated: "Mar 05, 2026"
---

# Runtime

## API documentation

### \_Pointer

**Kind:** class

**Qualified name:** `cutlass.cute.runtime._Pointer`

**Signature:** `_Pointer(*args: Any, **kwargs: Any)`

Bases: `Pointer`

Runtime representation of a pointer that can inter-operate with various data structures, including numpy arrays and device memory.

#### Parameters

-   **pointer** (*int* *or* *pointer-like object*) -- The pointer to the data

-   **dtype** (*Type*) -- Data type of the elements pointed to

-   **mem_space** (*\_cute_ir.AddressSpace,* *optional*) -- Memory space where the pointer resides, defaults to generic

-   **assumed_align** (*int,* *optional*) -- Assumed alignment of input pointer in bytes, defaults to None

#### Variables

-   **\_pointer** -- The underlying pointer

-   **\_dtype** -- Data type of the elements

-   **\_addr_space** -- Memory space of the pointer

-   **\_assumed_align** -- Alignment of the pointer in bytes

-   **\_desc** -- C-type descriptor for the pointer

-   **\_c_pointer** -- C-compatible pointer representation

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Pointer.__init__`

**Signature:** `__init__(pointer, dtype, mem_space: cutlass._mlir.dialects.cute.AddressSpace = cutlass._mlir.dialects.cute.AddressSpace.generic, assumed_align = None)`

#### size_in_bytes

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Pointer.size_in_bytes`

**Signature:** `size_in_bytes() → int`

#### mlir_type

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Pointer.mlir_type`

**Signature:** `mlir_type: cutlass._mlir.ir.Type`

#### dtype

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Pointer.dtype`

**Signature:** `dtype: Type[cutlass.cute.typing.Numeric]`

#### memspace

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Pointer.memspace`

**Signature:** `memspace`

#### align

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Pointer.align`

**Signature:** `align(min_align: int, *, loc = None, ip = None) → cutlass.cute.typing.Pointer`

### \_Tensor

**Kind:** class

**Qualified name:** `cutlass.cute.runtime._Tensor`

**Signature:** `_Tensor(*args: Any, **kwargs: Any)`

Bases: `Tensor`

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Tensor.__init__`

**Signature:** `__init__(tensor, assumed_align = None, use_32bit_stride = False, *, enable_tvm_ffi = False)`

#### load_dltensor

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Tensor.load_dltensor`

**Signature:** `load_dltensor()`

Lazily load the DLTensorWrapper.

This function loads the DLTensorWrapper when needed, avoiding overhead in the critical path of calling JIT functions.

#### mark_layout_dynamic

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Tensor.mark_layout_dynamic`

**Signature:** `mark_layout_dynamic(leading_dim: int | None = None)`

Marks the tensor layout as dynamic based on the leading dimension.

##### Parameters

**leading_dim** (*int,* *optional*) -- The leading dimension of the layout, defaults to None

When `leading_dim` is None, automatically deduces the leading dimension from the tensor layout. The layout can be deduced only when exactly one dimension has a stride of 1. Raises an error if the layout cannot be automatically deduced.

When `leading_dim` is explicitly specified, marks the layout as dynamic while setting the stride at `leading_dim` to 1. Also validates that the specified `leading_dim` is consistent with the existing layout by checking that the corresponding stride of that dimension is 1.

Limitation: only support flat layout for now. Will work on supporting nested layout in the future.

##### Returns

The tensor with dynamic layout

##### Return type

[\_Tensor](#cutlass.cute.runtime._Tensor)

#### mark_compact_shape_dynamic

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Tensor.mark_compact_shape_dynamic`

**Signature:** `mark_compact_shape_dynamic(mode: int, stride_order: tuple[int, ...] | None = None, divisibility: int = 1)`

Marks the tensor shape as dynamic and propagates dynamic and divisibility information to the corresponding strides.

##### Parameters

-   **mode** (*int*) -- The mode of the compact shape, defaults to 0

-   **stride_order** (*tuple\[int, ...\], optional*) -- Consistent with torch.Tensor.dim_order. Defaults to None.

-   **divisibility** (*int, optional*) -- The divisibility constraint for the compact shape, defaults to 1

Indicates the order of the modes (dimensions) if the current layout were converted to row-major order. It starts from the outermost to the innermost dimension.

##### Returns

The tensor with dynamic compact shape

##### Return type

\_Tensor

If `stride_order` is not provided, the stride ordering will be automatically deduced from the layout. Automatic deduction is only possible when exactly one dimension has a stride of 1 (compact layout). An error is raised if automatic deduction fails.

If `stride_order` is explicitly specified, it does the consistency check with the layout.

For example:

-   Layout: (4,2):(1,4) has stride_order: (1,0) indicates the innermost dimension is 0(4:1), the outermost dimension is 1(2:4)

-   Layout: (5,3,2,4):(3,1,15,30) has stride_order: (3,2,0,1) indicates the innermost dimension is 1(3:1), the outermost dimension is 3(4:30).

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

#### memspace

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.memspace`

**Signature:** `memspace`

#### size_in_bytes

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.size_in_bytes`

**Signature:** `size_in_bytes: int`

#### mlir_type

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.mlir_type`

**Signature:** `mlir_type: cutlass._mlir.ir.Type`

#### iterator

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.iterator`

**Signature:** `iterator`

#### layout

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.layout`

**Signature:** `layout`

#### shape

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.shape`

**Signature:** `shape`

#### stride

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.stride`

**Signature:** `stride`

#### leading_dim

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.leading_dim`

**Signature:** `leading_dim`

Get the leading dimension of this Tensor.

##### Returns

The leading dimension index or indices

##### Return type

int or tuple or None

The return value depends on the tensor's stride pattern:

-   If a single leading dimension is found, returns an integer index

-   If nested leading dimensions are found, returns a tuple of indices

-   If no leading dimension is found, returns None

#### fill

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._Tensor.fill`

**Signature:** `fill(value: cutlass.cute.typing.Numeric)`

#### data_ptr

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.data_ptr`

**Signature:** `data_ptr`

#### dynamic_shapes_mask

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.dynamic_shapes_mask`

**Signature:** `dynamic_shapes_mask`

Get the mask of dynamic shapes in the tensor.

#### dynamic_strides_mask

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._Tensor.dynamic_strides_mask`

**Signature:** `dynamic_strides_mask`

Get the mask of dynamic strides in the tensor.

### \_get_cute_type_str

**Kind:** function

**Qualified name:** `cutlass.cute.runtime._get_cute_type_str`

**Signature:** `_get_cute_type_str(inp)`

### \_FakeCompactTensor

**Kind:** class

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor`

**Signature:** `_FakeCompactTensor(*args: Any, **kwargs: Any)`

Bases: `Tensor`

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor.__init__`

**Signature:** `__init__(dtype, shape, stride_order, memspace = None, assumed_align = None, use_32bit_stride = False)`

#### mlir_type

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor.mlir_type`

**Signature:** `mlir_type: cutlass._mlir.ir.Type`

#### element_type

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor.element_type`

**Signature:** `element_type: Type[cutlass.cute.typing.Numeric]`

#### memspace

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor.memspace`

**Signature:** `memspace`

#### iterator

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor.iterator`

**Signature:** `iterator`

#### shape

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor.shape`

**Signature:** `shape`

#### stride

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor.stride`

**Signature:** `stride`

#### leading_dim

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor.leading_dim`

**Signature:** `leading_dim`

#### dynamic_shapes_mask

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor.dynamic_shapes_mask`

**Signature:** `dynamic_shapes_mask`

#### dynamic_strides_mask

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor.dynamic_strides_mask`

**Signature:** `dynamic_strides_mask`

#### fill

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._FakeCompactTensor.fill`

**Signature:** `fill(value: cutlass.cute.typing.Numeric)`

### \_FakeTensor

**Kind:** class

**Qualified name:** `cutlass.cute.runtime._FakeTensor`

**Signature:** `_FakeTensor(*args: Any, **kwargs: Any)`

Bases: `Tensor`

Fake Tensor implementation as a placeholder. It mimics the interface of Tensor, but does not hold real data or allow indexing. Used for compilation or testing situations where only shape/type/layout information is needed. All attempts to access or mutate data will raise errors.

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._FakeTensor.__init__`

**Signature:** `__init__(dtype, shape, *, stride, memspace = None, assumed_align = None)`

#### mlir_type

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.mlir_type`

**Signature:** `mlir_type: cutlass._mlir.ir.Type`

#### element_type

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.element_type`

**Signature:** `element_type: Type[cutlass.cute.typing.Numeric]`

#### memspace

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.memspace`

**Signature:** `memspace`

#### iterator

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.iterator`

**Signature:** `iterator`

#### shape

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.shape`

**Signature:** `shape`

#### stride

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.stride`

**Signature:** `stride`

#### dynamic_shapes_mask

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.dynamic_shapes_mask`

**Signature:** `dynamic_shapes_mask`

#### dynamic_strides_mask

**Kind:** property

**Qualified name:** `cutlass.cute.runtime._FakeTensor.dynamic_strides_mask`

**Signature:** `dynamic_strides_mask`

#### fill

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._FakeTensor.fill`

**Signature:** `fill(value: cutlass.cute.typing.Numeric)`

### make_fake_compact_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.runtime.make_fake_compact_tensor`

**Signature:** `make_fake_compact_tensor(dtype, shape, *, stride_order = None, memspace = None, assumed_align = None, use_32bit_stride = False)`

Create a fake tensor with the specified shape, element type, and a compact memory layout.

#### Parameters

-   **dtype** (*Type\[Numeric\]*) -- Data type of the tensor elements.

-   **shape** (*tuple\[int,* *\...\]*) -- Shape of the tensor.

-   **stride_order** (*tuple\[int,* *\...\],* *optional*) -- Order in which strides (memory layout) are assigned to the tensor dimensions. If None, the default layout is left-to-right order (known as column-major order for flatten layout). Otherwise, it should be a permutation order of the dimension indices.

-   **memspace** (*str,* *optional*) -- Memory space where the fake tensor resides. Optional.

-   **assumed_align** (*int,* *optional*) -- Assumed byte alignment for the tensor data. If None, the default alignment is used.

-   **use_32bit_stride** (*bool,* *optional*) -- Whether to use 32-bit stride for dynamic dimensions. If True and the total size of the layout (cosize(layout)) fits within int32, then dynamic strides will use 32-bit integers for improved performance. Only applies when dimensions are dynamic. Defaults to False.

#### Returns

An instance of a fake tensor with the given properties and compact layout.

#### Return type

[\_FakeCompactTensor](#cutlass.cute.runtime._FakeCompactTensor)

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

    # Default stride order is left-to-right order: (1, 8)
    y = make_fake_compact_tensor(cutlass.Float32, (8, 3))

### make_fake_tensor

**Kind:** function

**Qualified name:** `cutlass.cute.runtime.make_fake_tensor`

**Signature:** `make_fake_tensor(dtype, shape, stride, *, memspace = None, assumed_align = None)`

Create a fake tensor with the specified element type, shape, and stride.

#### Parameters

-   **dtype** (*Type\[Numeric\]*) -- Data type of the tensor elements.

-   **shape** (*tuple\[int,* *\...\]*) -- Shape of the tensor.

-   **stride** (*tuple\[int,* *\...\]*) -- Stride of the tensor.

-   **assumed_align** (*int,* *optional*) -- Assumed byte alignment for the tensor data. If None, the default alignment is used. Defaults to None.

#### Returns

An instance of a fake tensor with the given properties.

#### Return type

[\_FakeTensor](#cutlass.cute.runtime._FakeTensor)

### \_FakeStream

**Kind:** class

**Qualified name:** `cutlass.cute.runtime._FakeStream`

**Signature:** `_FakeStream(*, use_tvm_ffi_env_stream: bool = False)`

Bases: `object`

A fake stream that can be used as a placeholder for a stream in compilation.

When use_tvm_ffi_env_stream is True and the function is compiled with TVM-FFI, the argument will be skipped from the function signature and we pass in this value through the environment stream obtained from caller context (e.g. torch.cuda.current_stream()).

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.runtime._FakeStream.__init__`

**Signature:** `__init__(*, use_tvm_ffi_env_stream: bool = False)`

#### use_tvm_ffi_env_stream

**Kind:** attribute

**Qualified name:** `cutlass.cute.runtime._FakeStream.use_tvm_ffi_env_stream`

**Signature:** `use_tvm_ffi_env_stream: bool`

### make_fake_stream

**Kind:** function

**Qualified name:** `cutlass.cute.runtime.make_fake_stream`

**Signature:** `make_fake_stream(*, use_tvm_ffi_env_stream: bool = False)`

Create a fake stream that can be used as a placeholder for a stream in compilation.

When use_tvm_ffi_env_stream is True and the function is compiled with TVM-FFI, the argument will be skipped from the function signature and we pass in this value through the environment stream obtained from caller context (e.g. torch.cuda.current_stream()). This can speedup the calling process since we no longer need to do stream query in python.

#### Parameters

**use_tvm_ffi_env_stream** (*bool*) -- Whether to skip this parameter use environment stream instead.

### from_dlpack

**Kind:** function

**Qualified name:** `cutlass.cute.runtime.from_dlpack`

**Signature:** `from_dlpack(tensor_dlpack, assumed_align = None, use_32bit_stride = False, *, enable_tvm_ffi = False, force_tf32 = False) → cutlass.cute.typing.Tensor`

Convert from tensor object supporting \_\_dlpack\_\_() to a CuTe Tensor.

#### Parameters

-   **tensor_dlpack** (*object*) -- Tensor object that supports the DLPack protocol

-   **assumed_align** (*int,* *optional*) -- Assumed alignment of the tensor (bytes), defaults to None, if None, will use the element size bytes as the assumed alignment.

-   **use_32bit_stride** (*bool,* *optional*) -- Whether to use 32-bit stride, defaults to False. When True, the dynamic stride bitwidth will be set to 32 for small problem size (cosize(layout) \<= Int32_max) for better performance. This is only applied when the dimension is dynamic.

-   **enable_tvm_ffi** (*bool,* *optional*) -- Whether to enable TVM-FFI, defaults to False. When True, the tensor will be converted to a TVM-FFI function compatible tensor.

-   **force_tf32** (*bool,* *optional*) -- Whether to force the element type to TFloat32 if the element type is Float32.

#### Returns

A CuTe Tensor object

#### Return type

Tensor

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

**Signature:** `make_ptr(dtype: Type[cutlass.cute.typing.Numeric], value: int | _Pointer, mem_space: cutlass.cute.typing.AddressSpace = cutlass.cute.typing.AddressSpace.generic, assumed_align = None) → cutlass.cute.typing.Pointer`

Create a pointer from a memory address

#### Parameters

-   **dtype** (*Type\[Numeric\]*) -- Data type of the pointer elements

-   **value** (*Union\[int,* *ctypes.\_Pointer\]*) -- Memory address as integer or ctypes pointer

-   **mem_space** (*AddressSpace,* *optional*) -- Memory address space, defaults to AddressSpace.generic

-   **align_bytes** (*int,* *optional*) -- Alignment in bytes, defaults to None

#### Returns

A pointer object

#### Return type

Pointer

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

**Signature:** `nullptr(dtype: Type[cutlass.cute.typing.Numeric], mem_space: cutlass.cute.typing.AddressSpace = cutlass.cute.typing.AddressSpace.generic, assumed_align = None) → cutlass.cute.typing.Pointer`

Create a null pointer which is useful for compilation

#### Parameters

-   **dtype** (*Type\[Numeric\]*) -- Data type of the pointer elements

-   **mem_space** (*AddressSpace,* *optional*) -- Memory address space, defaults to AddressSpace.generic

#### Returns

A null pointer object

#### Return type

Pointer

### TensorAdapter

**Kind:** class

**Qualified name:** `cutlass.cute.runtime.TensorAdapter`

**Signature:** `TensorAdapter(arg)`

Bases: `object`

Convert a DLPack protocol supported tensor/array to a cute tensor.

#### \_\_init\_\_

**Kind:** method

**Qualified name:** `cutlass.cute.runtime.TensorAdapter.__init__`

**Signature:** `__init__(arg)`

### find_runtime_libraries

**Kind:** function

**Qualified name:** `cutlass.cute.runtime.find_runtime_libraries`

**Signature:** `find_runtime_libraries(*, enable_tvm_ffi: bool = True) → List[str]`

Find the runtime libraries that needs to be available for loading modules.

#### Parameters

**enable_tvm_ffi** (*bool,* *optional*) -- Whether to enable TVM-FFI.

#### Returns

A list of runtime libraries that needs to be available for loading modules.

#### Return type

list

### load_module

**Kind:** function

**Qualified name:** `cutlass.cute.runtime.load_module`

**Signature:** `load_module(file_path: str, *, enable_tvm_ffi: bool = False)`

Load a module from a file path.

#### Parameters

-   **file_path** (*str*) -- The path to the module file

-   **enable_tvm_ffi** (*bool,* *optional*) -- Whether to enable TVM-FFI, defaults to True. When True, the module will be loaded as a TVM-FFI module.

#### Returns

A module object

#### Return type

module
