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

The type of the data held in a register is stored as side-band information next to the register data. The meaning of various instruction opcodes depend on the register types they operate on.

.. note::
  Since compilers (at least GCC) don't differentiate between signed and unsigned integer types, the HW doesn't do that either. This means that certain integer operations have signed and unsigned variants.

There are up to 15 register types supported by the ISA, but only the following are defined:

==========    =========   ==========
Type code     Type        Note
==========    =========   ==========
0x0           INT32       32-bit integer: this is the default type of all registers after reset
0x1           INT16X2     2-way 16-bit integer vector
0x2           INT8X4      4-way 8-bit integer vector
0x3           UINT16X2S   Unsigned, saturated version on INT16X2
0x4           SINT16X2S   Signed, saturated version on INT16X2
0x5           UINT8X4S    Unsigned, saturated version on INT8X4
0x6           SINT8X4S    Signed, saturated version on INT8X4
0x8           FP32        32-bit float
0x9           FP16X2      2-way 16-bit float vector
==========    =========   ==========

