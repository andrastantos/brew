
.. _inv_ra:

INV[$rA]
---------------------

*Instruction code*: 0x1ee.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Invalidates any data- or instruction-cache lines that contain the logical address corresponding to the value of :code:`$rA`. Cache invalidation applies to both L1 and L2 level caches. System-level caches (L3), if exist are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

Dirty lines in data-caches are flushed to memory as they are invalidated.

The implementation is not allowed to throw exceptions even if the memory location violates access permissions. In these cases, the invalidation request is silently ignored.


.. _pc_eq_mem_ra:

$pc <- MEM32[$rA]
---------------------------------------------

*Instruction code*: 0x2ee.

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA`. The value stored in :code:`$pc`. This is an indirect branch.

The LSB of the call target address carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an :code:`exc_unaligned` exception is raised.


.. _tpc_eq_mem_ra:

$tpc <- MEM32[$rA]
---------------------------------------------

*Instruction code*: 0x3ee.


*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA`. The value stored in :code:`$tpc`. This is an indirect branch in TASK mode.

The LSB of the call target address carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an :code:`exc_unaligned` exception is raised.

.. _call_eq_mem_ra:

call MEM32[$rA]
---------------------------------------------

*Instruction code*: 0x4ee.


*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA`. The value stored in :code:`$pc`. The current value of :code:`$pc` is loaded into :code:`$lr`. This is an indirect call operation.

The LSB of the call target address carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an :code:`exc_unaligned` exception is raised.









.. _inv_ra_plus_value:

INV[$rA + VALUE]
---------------------

*Instruction code*: 0x1fe. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Invalidates any data- or instruction-cache lines that contain the logical address corresponding to the value of :code:`$rA + VALUE`. Cache invalidation applies to both L1 and L2 level caches. System-level caches (L3), if exist are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

The value of FIELD_E is sign-extended prior to addition to the base register :code:`$rA`. Thus an offset range of -32768 to 32767 is supported.

Dirty lines in data-caches are flushed to memory as they are invalidated.

The implementation is not allowed to throw exceptions even if the memory location violates access permissions. In these cases, the invalidation request is silently ignored.


.. _pc_eq_mem_ra_plus_value:

$pc <- MEM32[$rA + VALUE]
---------------------------------------------

*Instruction code*: 0x2fe. 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA + VALUE`. The value stored in :code:`$pc`. This is an indirect branch.

The value of FIELD_E is sign-extended prior to addition to the base register :code:`$rA`. Thus an offset range of -32768 to 32767 is supported.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an :code:`exc_unaligned` exception is raised.


.. _tpc_eq_mem_ra_plus_value:

$tpc <- MEM32[$rA + VALUE]
---------------------------------------------

*Instruction code*: 0x3fe. 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA + VALUE`. The value stored in :code:`$tpc`. This is an indirect branch in TASK mode.

The value of FIELD_E is sign-extended prior to addition to the base register :code:`$rA`. Thus an offset range of -32768 to 32767 is supported.

The LSB of the branch target address carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an :code:`exc_unaligned` exception is raised.


.. _call_mem_ra_plus_value:

call MEM32[$rA + VALUE]
---------------------------------------------

*Instruction code*: 0x4fe. 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA + VALUE`. The value stored in :code:`$pc`. The current value of :code:`$pc` is loaded into :code:`$lr`. This is an indirect call operation.

The value of FIELD_E is sign-extended prior to addition to the base register :code:`$rA`. Thus an offset range of -32768 to 32767 is supported.

The LSB of the call target address carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an :code:`exc_unaligned` exception is raised.







.. _inv_value:

INV[VALUE]
---------------------

*Instruction code*: 0x1fef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Invalidates any data- or instruction-cache lines that contain the logical address corresponding to the value of :code:`VALUE`. Cache invalidation applies to both L1 and L2 level caches. System-level caches (L3), if exist are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

Dirty lines in data-caches are flushed to memory as they are invalidated.

The implementation is not allowed to throw exceptions even if the memory location violates access permissions. In these cases, the invalidation request is silently ignored.


.. _pc_eq_mem_value:

$pc <- MEM32[VALUE]
---------------------------------------------

*Instruction code*: 0x2fef 0x**** 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`VALUE`. The value stored in :code:`$pc`. This is an indirect branch.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an :code:`exc_unaligned` exception is raised.


.. _tpc_eq_mem_value:

$tpc <- MEM32[VALUE]
---------------------------------------------

*Instruction code*: 0x3fef 0x**** 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`VALUE`. The value stored in :code:`$tpc`. This is an indirect branch in TASK mode.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an :code:`exc_unaligned` exception is raised.


.. _call_mem_value:

call MEM32[VALUE]
---------------------------------------------

*Instruction code*: 0x4fef 0x**** 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`VALUE`. The value stored in :code:`$pc`. The current value of :code:`$pc` is loaded into :code:`$lr`. This is an indirect call operation.

The LSB of the call target address carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an :code:`exc_unaligned` exception is raised.


