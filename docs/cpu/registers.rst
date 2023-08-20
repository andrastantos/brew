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
* Dirty

These fields can be accessed using the '.' notation.
* The :code:`value` field holds the result of the latest write to the register.
* The :code:`type` field contains the type of the value held in the register. This field is updated by most operations that modify the value, except for load/store operations. It is also updated by type overrides and type-casts.
* The :code:`dirty` bit is set whenever either :code:`type` or :code:`value` fields are changed. It can be cleared through by writing to the appropriate CSR.

Register value access
---------------------

Each register contains VLEN number of bytes. However not all of these bytes hold valid values. Only bytes from 0 to :code:`size` contain valid values. If a byte outside of this range is read, those bytes are masked and 0 is returned instead.

For certain operations, wrap-around addressing of vector registers is used: if the byte index is greater than :code:`VLEN`, indexing is restarted at 0.

.. note:: since :code:`VLEN` is required to be a power of 2, this means truncating byte address to :code:`log2(VLEN)` number of bits.


.. _register_value_and_type_updates:

Register value and type updates
-------------------------------

When a value of a register is updated, it's possible that only part of the full HW register receives a new value. This can be due to the following reasons:

* The type of the register is one where the value is shorter than the HW register length
* The :code:`vend` value is lower then :code:`vlen`

It is also possible that the accessible size of a register is adjusted in run-time to be greater then size of the latest update. Some instances of that:

* A scalar registers type is changed to a vector type
* A 'full' store operation is performed on a vector register
* :code:`vend` is adjusted to be greater than what was used during the latest updates

In all of these cases, values stored in the upper portions of a register could be unmasked and accessed. This poses a potential security risk. It is paramount that such unmasking is prevented.

.. note:: Imagine the following scenario: the kernel uses a vector-register based :code:`memcpy` implementation to copy sensitive data from one place to another. This means that some vector registers contain sensitive data. Upon return to user mode, the context is restored and new register values (and types) are loaded from memory. Let's say these new types are scalar types. Still, the higher bytes of the HW register contain sensitive data. If user code, for instance, could change the register type and gain access to the upper bytes of the vector register, this sensitive data could be exposed.

To mitigate such information leak, Brew specifies the following logic:

* Whenever a vector value is written to a register, the bytes between :code:`vend` and :code:`vlen` are effectively written as 0.
* Whenever a scalar value is written to a register, the bytes beyond the size of the scalar type are effectively written as 0.
* Whenever the type of a register is changed to a scalar type, the bytes beyond the size of the scalar type are effectively written as 0.

It is important to note that some implementations may chose not to physically zero out the upper bytes of a register. This is especially true for implementations where the HW register size is wider than the access width of the register file.

These implementations can maintain side-band information (a size field) next to each register, which is used to 'simulate' the prescribed behavior.

.. note:: Another security hole could emerge in an implementation using the 'size' field the following scenario: let's say that :code:`$r0` was used in a vector operation and is now holding sensitive information. It's type is adjusted to :code:`INT32`, so the size field is set to 4. Yet, sensitive values are held in upper bytes of the HW register. In user code, we could change the type back to - say - :code:`VINT32`, set :code:`vend` to :code:`vlen`, but *also* set :code:`vstart` to :code:`vlen` or some other high value. Now, let's perform a vector load operation. The load will not do anything, and per the definition of :code:`vstart` it will not touch low bytes of the destination register. Finally, it'll change the 'size' field to that of :code:`vstart`, unmasking sensitive data. Notice, that this problem would not occur with an implementation that physically zeros out register value: the point where we change the type to :code:`INT32` would have destroyed all sensitive information.

The :code:`vstart` register is writable only in SCHEDULER-mode. TASK mode only has read-only access to this register.


Size handling
~~~~~~~~~~~~~

Whenever either a new type and/or a new value is assigned to a register, it's run-time size needs to be adjusted. The following rules are applied:

* If both value and type is assigned, the run-time size is set to:
  * For scalar types, the size is set to the size of the type
  * For vector types, the size is set to the size of :code:`VEND` if that controlled the production of the result, otherwise the size of the type is used.
* If only a type is assigned, the run-time size is set to the minimum of the current run-time size and the size of the new type.
* If only the value is changed, the run-time size of the destination is set to:
  * For scalar types, the size is not changed
  * For vector types, the size is set to the size of :code:`VEND` if that controlled the production of the result, otherwise the size of the destination type is used.

.. _vstart_handling:

:code:`vstart` handling
-----------------------

The :code:`vstart` register controls the first byte (and through that element) a vector load/store operation considers. The :code:`vstart` register can be set through the following means:

* The successful completion of any vector load/store operation sets this register to 0
* An exception during the execution of a vector operation sets this register to the byte-index of the first element to be retried.
* In SCHEDULER-mode, the register can be set using a CSR write

Vector operations that don't load or store vector registers from memory ignore the setting of :code:`vstart` and don't alter its value either. If an exception is raised during the execution of these operations, they are fully retried, if necessary.

.. note:: most operations raise exceptions prior any side-effects and that includes vector operations as well. It's only multi-cycle vector operations that can't test for potential exceptions prior starting execution that are different. The only class of exceptions that can't easily be tested for a-priory are memory access violations in a long, multi-cycle vector load/store operation. This is the reason that :code:`vstart` is only used by load/store operations.

It is up to the implementation if :code:`vstart` is implemented. After all this is really an optimization around retries for loads/stores. The implications of this are the same as that of load/store multiple. Namely, that we adhere to the normal memory semantics, where multiple stores of the same value to the same address and multiple loads from the same address are harmless. This of course isn't true for I/O, which is to say that vector loads/stores are not supported to I/O regions, or at least not guaranteed on all implementations. The other consequence is that load-lock/store-conditional operations are not really reasonable with vector types either. If the implementation doesn't support :code:`vstart` vector load/stores always operate on the full vector and :code:`vstart` is set to constant 0. The memory exception handler uses the :code:`EADDR` register to determine the target address of the violation and the right course of action to be taken, including a potential retry.

.. _vend_handling:

:code:`vend` handling
---------------------

The :code:`vend` register controls the last byte (and through that element) any vector operation considers. The :code:`vend` register can be set through the following means:

* By the :ref:`set_vend<rd_eq_set_vend_ra>` operation.
* In SCEDULER mode, by writing to the appropriate CSR register.

The value of :code:`vend` points to the byte after the last element in the vector register to be processed and must be aligned to an element boundary. In case of a misaligned :code:`vend`, vector operations truncate the value to align with the lane size. For instance, if :code:`vend` is set to 7 while the element length is 4, the two lowest order bits of :code:`vend` are ignored, and :code:`vend` is treated as if it was set to 4.

