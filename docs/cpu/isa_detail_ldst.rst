

$rD <- MEM8[$rA]
---------------------------------------------

*Instruction code*: 0x.e4.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       4       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 8-bit value from memory location pointed to by :code:`$rA`. The value is zero-extended and stored in :code:`$rD`. The type of :code:`$rD` is not modified.

The implementation is allowed to throw exceptions if the memory access violates access permissions.

$rD <- MEM16[$rA]
---------------------------------------------

*Instruction code*: 0x.e5.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       5       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 16-bit value from memory location pointed to by :code:`$rA`. The value is zero-extended and stored in :code:`$rD`. The type of :code:`$rD` is not modified.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 16-bit word boundary, an unaligned access exception is thrown.

$rD <- MEM[$rA]
---------------------------------------------

*Instruction code*: 0x.e6.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       6       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA`. The value stored in :code:`$rD`. The type of :code:`$rD` is not modified.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.

$rD <- MEMLL[$rA]
---------------------------------------------

*Instruction code*: 0x.e7.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       7       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 32-bit value from memory location pointed to by :code:`$rA`. The value stored in :code:`$rD`. The type of :code:`$rD` is not modified. A load-lock is placed on the memory location.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown. If an exception is thrown, no lock is placed.

MEM8[$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.e8.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       8       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The lowest 8 bits of :code:`$rD` is stored in the memory location pointed to by :code:`$rA`.

The implementation is allowed to throw exceptions if the memory access violates access permissions.

MEM16[$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.e9.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       9       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The lowest 16 bits of :code:`$rD` is stored in the memory location pointed to by :code:`$rA`.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 16-bit word boundary, an unaligned access exception is thrown.


MEM[$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.ea.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       a       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The value of :code:`$rD` is stored in the memory location pointed to by :code:`$rA`.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown.



MEMSC[$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.eb.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       b       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
The value of :code:`$rD` is stored in the memory location pointed to by :code:`$rA`, if and only if a still valid load-lock exists for the same address for the same processor. If such a lock is not found, the store fails and no memory update is performed.

The value of :code:`$rD` is set to 0 if the store succeeded and to non-zero if it failed. The actual non-zero value is implementation-defined and is not required to be constant, only that it is never zero. The type of :code:`$rD` is not changed.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 32-bit word boundary, an unaligned access exception is thrown. In case of an exception, neither the existence of a lock nor the value stored in memory is altered.



$rD <- SMEM8[$rA]
---------------------------------------------

*Instruction code*: 0x.ec.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       c       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 8-bit value from memory location pointed to by :code:`$rA`. The value is sign-extended and stored in :code:`$rD`. The type of :code:`$rD` is not modified.

The implementation is allowed to throw exceptions if the memory access violates access permissions.

$rD <- SMEM16[$rA]
---------------------------------------------

*Instruction code*: 0x.ed.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       e       |       d       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Loads the 16-bit value from memory location pointed to by :code:`$rA`. The value is sign-extended and stored in :code:`$rD`. The type of :code:`$rD` is not modified.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is not aligned to a 16-bit word boundary, an unaligned access exception is thrown.















Offset-indirect load/store group
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  ====================================    ==================
Instruction code    Assembly                                Operation
==================  ====================================    ==================
0x.f4. 0x****       $rD <- MEM8[$rA+FIELD_E]                8-bit unsigned load from MEM[$rA+FIELD_E] into $rD
0x.f5. 0x****       $rD <- MEM16[$rA+FIELD_E]               16-bit unsigned load from MEM[$rA+FIELD_E] into $rD
0x.f6. 0x****       $rD <- MEM[32][$rA+FIELD_E]             32-bit load from MEM[$rA+FIELD_E] into $rD
0x.f7. 0x****       $rD <- MEMLL[$rA+FIELD_E]           32-bit unsigned load-reserve (exclusive load)
0x.f8. 0x****       MEM8[$rA+FIELD_E] <- $rD                8-bit store to MEM[$rA+FIELD_E] from $rD
0x.f9. 0x****       MEM16[$rA+FIELD_E] <- $rD               16-bit store to MEM[$rA+FIELD_E] from $rD
0x.fa. 0x****       MEM[32][$rA+FIELD_E] <- $rD             32-bit store to MEM[$rA+FIELD_E] from $rD
0x.fb. 0x****       MEMSR[32][$rA+FIELD_E] <- $rD           32-bit store-release (exclusive store)
0x.fc. 0x****       $rD <- SMEM8[$rA+FIELD_E]               8-bit signed load from MEM[$rA+FIELD_E] into $rD
0x.fd. 0x****       $rD <- SMEM16[$rA+FIELD_E]              16-bit signed load from MEM[$rA+FIELD_E] into $rD
==================  ====================================    ==================

.. note:: FIELD_E is sign-extended before addition
.. note:: Loads don't change the type of a register.



Absolute load/store group
~~~~~~~~~~~~~~~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ==========================  ==================
Instruction code           Assembly                    Operation
=========================  ==========================  ==================
0x.f4f 0x**** 0x****       $rD <- MEM8[FIELD_E]        8-bit unsigned load from MEM[FIELD_E] into $rD
0x.f5f 0x**** 0x****       $rD <- MEM16[FIELD_E]       16-bit unsigned load from MEM[FIELD_E] into $rD
0x.f6f 0x**** 0x****       $rD <- MEM[32][FIELD_E]     32-bit load from MEM[FIELD_E] into $rD
0x.f7f 0x**** 0x****       $rD <- MEMLL[FIELD_E]   32-bit unsigned load-reserve (exclusive load)
0x.f8f 0x**** 0x****       MEM8[FIELD_E] <- $rD        8-bit store to MEM[FIELD_E] from $rD
0x.f9f 0x**** 0x****       MEM16[FIELD_E] <- $rD       16-bit store to MEM[FIELD_E] from $rD
0x.faf 0x**** 0x****       MEM[32][FIELD_E] <- $rD     32-bit store to MEM[FIELD_E] from $rD
0x.fbf 0x**** 0x****       MEMSR[32][FIELD_E] <- $rD   32-bit store-release (exclusive store)
0x.fcf 0x**** 0x****       $rD <- SMEM8[FIELD_E]       8-bit signed load from MEM[FIELD_E] into $rD
0x.fdf 0x**** 0x****       $rD <- SMEM16[FIELD_E]      16-bit signed load from MEM[FIELD_E] into $rD
=========================  ==========================  ==================

.. note:: Loads don't change the type of a register.

