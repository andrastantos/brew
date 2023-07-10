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
