Types and their manipulation
============================

Register Types
--------------

Each general purpose register has a type associated with it. Types are changed and propagated by instructions. Register types can be loaded and stored independent of the values contained in the registers. The type of :code:`$tpc`, :code:`$spc` and :code:`$pc` is always assumed to be :code:`INT32`.

The meaning of various instruction opcodes depend on the register types they operate on.

.. note::
  This means that instruction scheduling cannot be statically performed without having access to the register types. There could be dependencies on previous instructions still in the pipeline. The ISA is formulated in a way that the result type can almost always be determined from the source types alone, without regard of the values involved in the operations (obvious exceptions are the loading and storing of types). An implementation can pre-calculate the types of future register updates, eliminating the latency impact of the pipeline in most cases.

.. note::
  Since compilers (at least GCC) don't differentiate between signed and unsigned integer types, the HW doesn't do that either. This means that certain integer operations have signed and unsigned variants.

There are up to 15 basic register types supported by the ISA, but only following have a fixed definition:

==========    =========  ============== ==================== ============ ==========
Type code     Type name  Scalar/Vector  Fixed/Floating point Logic type   Note
==========    =========  ============== ==================== ============ ==========
0x0           INT32      Scalar         Fixed                INT32        32-bit integer: default type after reset
0x1           FP32       Scalar         Float                INT32        32-bit float
==========    =========  ============== ==================== ============ ==========

.. note:: Not all implementations are required to support all types. See :ref:`family compatibility<family_compatibility>` for details.

The rest of the type slots are implementation specific and may be mapped differently on different implementations. Code depending on these types should check for support in the :::TODO::: CSR registers.

