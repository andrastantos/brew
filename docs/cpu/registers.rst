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

Register content fields
-----------------------

The following data fields are stored with every register:

* Value
* Type
* Size
* Dirty

These fields can be accessed using the '.' notation.
The :code:`value` field holds the result of the latest write to the register.
The :code:`type` field contains the type of the value held in the register. This field is updated by most operations that modify the value, except for load/store operations. It is also updated by type overrides and type-casts.
The :code:`size` field contains the number of valid bytes held in the :code:`value` field of the register. The :code:`size` field is set to the :code:`min($rX.size, sizeof(type))` when type alone is changed. When :code:`value` changes, the :code:`size` field is adjusted to reflect the total number of bytes held in :code:`value`. For vector operations that use :code:`vstart` and :code:`vend`, :code:`size` reflects :code:`vend`, not the size of the source, nor the size of the destination type.

The :code:`dirty` bit is set whenever either :code:`type` or :code:`value` fields are changed. It can be cleared through by writing to the appropriate CSR.

.. todo:: We have a security hole still: what if someone sets vstart to a high value? Then we won't write many of the destination lanes, yet set size as if we did. So, previously held values could be unmasked. Should we hold a start field as well? Should we zero out elements below vstart? Should not allow modification of vstart from task mode? Man, this is getting convoluted real fast!

Register value access
---------------------

Each register contains VLEN number of bytes. However not all of these bytes hold valid values. Only bytes from 0 to :code:`size` contain valid values. If a byte outside of this range is read, those bytes are masked and 0 is returned instead.

For certain operations, wrap-around addressing of vector registers is used: if the byte index is greater than :code:`VLEN`, indexing is restarted at 0.

.. note:: since :code:`VLEN` is required to be a power of 2, this means truncating byte address to :code:`log2(VLEN)` number of bits.
