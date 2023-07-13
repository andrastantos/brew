
INV[$rA]
---------------------

*Instruction code*: 0x1ee.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Invalidates any data- or instruction-cache lines that contain the logical address corresponding to the value of :code:`$rA`. Cache invalidation applies to both L1 and L2 level caches. System-level caches (L3) are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

Dirty lines in data-caches are flushed to memory as they are invalidated.

The implementation is not allowed to throw exceptions even if the memory location violates access permissions. In these cases, the invalidation request is silently ignored.


$pc <- MEM32[$rA]
---------------------------------------------

*Instruction code*: 0x2ee.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA`. The value stored in :code:`$pc`. This is an indirect branch.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.


$tpc <- MEM32[$rA]
---------------------------------------------

*Instruction code*: 0x3ee.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA`. The value stored in :code:`$tpc`. This is an indirect branch in TASK mode.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.









INV[$rA + VALUE]
---------------------

*Instruction code*: 0x1fe. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Invalidates any data- or instruction-cache lines that contain the logical address corresponding to the value of :code:`$rA + VALUE`. Cache invalidation applies to both L1 and L2 level caches. System-level caches (L3) are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

The value of FIELD_E is computed by truncating VALUE to 16 bits. The implementation sign-extend the value of FIELD_E prior to addition to the base register :code:`$rA`. Thus an offset range of -32768 to 32767 is supported.

Dirty lines in data-caches are flushed to memory as they are invalidated.

The implementation is not allowed to throw exceptions even if the memory location violates access permissions. In these cases, the invalidation request is silently ignored.


$pc <- MEM32[$rA + VALUE]
---------------------------------------------

*Instruction code*: 0x2fe. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA + VALUE`. The value stored in :code:`$pc`. This is an indirect branch.

The value of FIELD_E is computed by truncating VALUE to 16 bits. The implementation sign-extend the value of FIELD_E prior to addition to the base register :code:`$rA`. Thus an offset range of -32768 to 32767 is supported.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.


$tpc <- MEM32[$rA + VALUE]
---------------------------------------------

*Instruction code*: 0x3fe. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA + VALUE`. The value stored in :code:`$tpc`. This is an indirect branch in TASK mode.

The value of FIELD_E is computed by truncating VALUE to 16 bits. The implementation sign-extend the value of FIELD_E prior to addition to the base register :code:`$rA`. Thus an offset range of -32768 to 32767 is supported.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.







INV[VALUE]
---------------------

*Instruction code*: 0x1fef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Invalidates any data- or instruction-cache lines that contain the logical address :code:`VALUE`. Cache invalidation applies to both L1 and L2 level caches. System-level caches (L3) are not invalidated. In a multi-processor system, only local caches (caches that are in the path-to-memory for the core executing the instruction) are invalidated.

FIELD_E simply stores VALUE.

Dirty lines in data-caches are flushed to memory as they are invalidated.

The implementation is not allowed to throw exceptions even if the memory location violates access permissions. In these cases, the invalidation request is silently ignored.


$pc <- MEM32[VALUE]
---------------------------------------------

*Instruction code*: 0x2fef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`VALUE`. The value stored in :code:`$pc`. This is an indirect branch.

FIELD_E simply stores VALUE.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.


$tpc <- MEM32[VALUE]
---------------------------------------------

*Instruction code*: 0x3fef 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`VALUE`. The value stored in :code:`$tpc`. This is an indirect branch in TASK mode.

FIELD_E simply stores VALUE.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.





