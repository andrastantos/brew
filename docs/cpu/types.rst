Types and their manipulation
============================

Register Types
--------------

Each general purpose register has a type associated with it. Types are changed and propagated by instructions. Register types can be loaded and stored independent of the values contained in the registers. The type of :code:`$tpc`, :code:`$spc` and :code:`$pc` is always assumed to be :code:`INT32`.

The meaning of various instruction opcodes depend on the register types they operate on.

.. note::
  This means that instruction scheduling cannot be statically performed without having access to the register types. There could be dependencies on previous instructions still in the pipeline. The ISA is formulated in a way that the result type can often be determined from the source types alone, without regard of the values involved in the operations (obvious exceptions are the loading and storing of types). An implementation can pre-calculate the types of future register updates, eliminating the latency impact of the pipeline in most cases.

.. note::
  Since compilers (at least GCC) don't differentiate between signed and unsigned integer types, the HW doesn't do that either. This means that certain integer operations have signed and unsigned variants.

There are up to 15 basic register types supported by the ISA, but only following have a fixed definition:

==========    =========  ============== ==================== ========================== ============ ==========
Type code     Type name  Scalar/Vector  Fixed/Floating point Compatible type            Logic type   Note
==========    =========  ============== ==================== ========================== ============ ==========
0x0           INT32      Scalar         Fixed                VINT*, VSINT*, VUINT*      INT32        32-bit integer: default type after reset
0x1           VINT32     Vector         Fixed                INT32                      VINT32       32-bit integer vector
0x2           VINT16     Vector         Fixed                INT16                      VINT16       16-bit integer vector
0x3           VINT8      Vector         Fixed                INT32                      VINT8        8-bit integer vector
0x4           FP32       Scalar         Float                VFP*, FP64                 INT32        32-bit float
0x5           FP64       Scalar         Float                VFP*, FP32                 VINT32       64-bit float
0x6           VFP32      Vector         Float                FP64, FP32                 VINT32       32-bit float vector
0x7           VFP64      Vector         Float                FP64, FP32                 VINT32       64-bit float vector
==========    =========  ============== ==================== ========================== ============ ==========

.. note:: Not all implementations are required to support all types. See :ref:`family compatibility<family_compatibility>` for details.

.. note:: the 64-bit floating-point types :code:`FP64` and :code:`VFP64` require at least 64-bit wide registers. While their logic type is :code:`VINT32`, integer vector support is not required. Logic types are a documentation tool to describe type compatibility between arguments of certain operations, but don't describe actual register types. Crucially, no instruction takes the logic type of any of its argument as its output type.

.. note:: There isn't a way to directly load any register with an immediate value greater then 32 bits. This includes 64-bit scalar and all vector types. (For vector types the immediate value is :ref:`broadcast<type_broadcast>` to all elements).

.. note:: A native 64-bit integer type is missing from the type-system, due to the ISA being geared toward a 32-bit datapath implementation.

The rest of the type slots are specific and may be mapped differently on different implementations. Code depending on these types should check for support in the :::TODO::: CSR registers.

Changing of types
-----------------

.. todo:: this is in direct contradiction with other parts of the spec! I need to make up my mind about it!!!!

When the type of a register is changed, it's value is not guaranteed to remain intact. This is because physical vector or floating-point registers might be maintained in different, independent register files. It would be very inconvenient, in case of vector registers neigh impossible to maintain the value through a type change. This also means that type changes and operations cannot be reordered by the compiler (they *can* be reordered by the processor if the implementation happens to maintains value through type changes). It also means that type changes must precede loads.

.. note:: this is going to be extra fun to convince gcc about the dependencies between type updates and value updates so that it won't reorder and hoist things in the wrong order.

.. todo:: This is not going to work well: if we don't preserve values, the type override prefix instruction won't work. If we do, the prefix implementation is extra painful! Maybe the prefix should go? But then again, that was there to make compiler implementations (for compilers that are not register-type-aware) reasonable. BTW: for that it would be crucial that the result type is not written back into the destination. That bit used to be there, but I yanked it. It needs to go back and I need to find a way to express that in assembly.

Type compatibility
------------------

While the table above gives specific compatibility rules, the following general guidelines are followed:

A vector type is said to be compatible with a scalar type when:

* A fixed point vector type is compatible with INT32
* A floating point vector type is compatible with FP32 and FP64

Between scalar types, fixed point and floating point. This means that use of such extended types almost certainly will lead to compatibility issues with existing code and compilers, especially if used as parameter-passing or across caller-saved registers.