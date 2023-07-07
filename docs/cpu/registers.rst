Registers
=========

The Brew ISA defines 15 general purpose registers, from :code:`$r0` through :code:`$r14`. Normally, these registers are encoded in the instruction set by the following values:

=============   ===========
Register name   Field value
=============   ===========
$r0             0x0
$r1             0x1
$r2             0x2
$r3             0x3
$r4             0x4
$r5             0x5
$r6             0x6
$r7             0x7
$r8             0x8
$r9             0x9
$r10            0xa
$r11            0xb
$r12            0xc
$r13            0xd
$r14            0xe
=============   ===========

In assembly, the following register aliases can be used as well:

* $sp:  alias to $r12
* $fp:  alias to $r13
* $lr:  alias to $r14

.. note::
  Aliases have (almost) nothing to do with HW and only make assembly unambiguous and/or easier to read. The only exception is that the ISA supports compact load-store operations with $r12 and $r13 as their base registers. There's no functional difference between these compact and the full-format load/store operations, but the intent is that by using these instructions to access stack-local variables allows much more compact code-size (close to ARM THUMB compactness).

On top of the 15 general purpose registers, Brew defines two special registers:

$pc:
 denotes the current-context program counter, the pointer to the currently executing instruction.

$tpc:
 denotes the task-mode program counter. If the current execution context is TASK mode, $tpc is the same as $pc. In SCHEDULER mode, it's not.

.. note:: $pc and $tpc are 31-bit registers.

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

