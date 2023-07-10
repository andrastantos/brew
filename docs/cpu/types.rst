Types and their manipulation
============================

When the type of a register is changed, it's value is not guaranteed to remain intact. This is because physical vector or floating-point registers might be maintained in different, independent register files. It would be very inconvenient, in case of vector registers neigh impossible to maintain the value through a type change. This also means that type changes and operations cannot be reordered by the compiler (they *can* be reordered by the processor if the implementation happens to maintains value through type changes). It also means that type changes must precede loads.

.. note:: this is going to be extra fun to convince gcc about the dependencies between type updates and value updates so that it won't reorder and hoist things in the wrong order.

.. note:: This is not going to work well: if we don't preserve values, the type override prefix instruction won't work. If we do, the prefix implementation is extra painful! Maybe the prefix should go? But then again, that was there to make compiler implementations (for compilers that are not register-type-aware) reasonable. BTW: for that it would be crucial that the result type is not written back into the destination. That bit used to be there, but I yanked it. It needs to go back and I need to find a way to express that in assembly.


Register Types
--------------

Each general purpose register has a type associated with it. Types are changed and propagated by instructions. They can be loaded and stored independent of the values contained in the registers. The type of :code:`$tpc`, :code:`$spc` and :code:`$pc` is always assumed to be :code:`INT32`.

The meaning of various instruction opcodes depend on the register types they operate on.

.. note::
  This means that instruction scheduling cannot be statically performed without having access to the register types. There could be dependencies on previous instructions still in the pipeline. The ISA is formulated in a way that the result type can often be determined from the source types alone, without regard of the values involved in the operations (obvious exceptions are the loading and storing of types). An implementation can pre-calculate the types of future register updates, eliminating the latency impact of the pipeline in most cases.

.. note::
  Since compilers (at least GCC) don't differentiate between signed and unsigned integer types, the HW doesn't do that either. This means that certain integer operations have signed and unsigned variants.

There are up to 15 register types supported by the ISA, but only the following are defined:

==========    =========   ==========
Type code     Type        Note
==========    =========   ==========
0x0           INT32       32-bit integer: this is the default type of all registers after reset
0x1           FP32        32-bit float
0x2           RES1        reserved
0x3           RES2        reserved
0x4           VINT32      32-bit integer vector
0x5           VFP32       32-bit float vector
0x6           VINT16      16-bit integer vector
0x7           VINT8       8-bit integer vector
0x8           VFP16       16-bit float vector
0x9           VUINT32S    Unsigned, saturated version on VINT32
0xa           VSINT32S    Signed, saturated version on VINT32
0xb           VUINT16S    Unsigned, saturated version on VINT16
0xc           VSINT16S    Signed, saturated version on VINT16
0xd           VUINT8X4S   Unsigned, saturated version on VINT8
0xe           VSINT8X4S   Signed, saturated version on VINT8
==========    =========   ==========

Type-less variant
-----------------

A type-less variant of the ISA is possible: in this case, all registers are assumed to have the type of INT32 and type-change instructions have no effect.

.. todo:: The compatibility story of the typeless subset is rather shaky. We need more thought on that!

