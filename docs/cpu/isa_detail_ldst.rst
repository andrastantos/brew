

.. _rd_eq_mem8_ra:

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

.. _rd_eq_mem16_ra:

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

.. _rd_eq_mem_ra:

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

.. _rd_eq_memll_ra:

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

.. _mem8_ra_eq_rd:

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

.. _mem16_ra_eq_rd:

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


.. _mem_ra_eq_rd:

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



.. _memsc_ra_eq_rd:

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



.. _rd_eq_smem8_ra:

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

.. _rd_eq_smem16_ra:

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







.. _mem_ra_eq_full_rd:

MEM[$rA] <- full $rD
--------------------

*Instruction code*: 0x.ef.

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

Store full $rD (no use/modification of vstart vend)


.. _mem_value_eq_full_rd:

MEM[VALUE] <- full $rD
----------------------

*Instruction code*: 0x.eff 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

Store full $rD (no use/modification of vstart vend)


.. _rd_eq_mem8_ra_plus_value:

$rD <- MEM8[$rA + VALUE]
------------------------

*Instruction code*: 0x.f4. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

8-bit unsigned load from MEM[$rA+VALUE] into $rD


.. _rd_eq_mem8_value:

$rD <- MEM8[VALUE]
------------------

*Instruction code*: 0x.f4f 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

8-bit unsigned load from MEM[VALUE] into $rD


.. _rd_eq_mem16_ra_plus_value:

$rD <- MEM16[$rA + VALUE]
-------------------------

*Instruction code*: 0x.f5. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

16-bit unsigned load from MEM[$rA+VALUE] into $rD


.. _rd_eq_mem16_value:

$rD <- MEM16[VALUE]
-------------------

*Instruction code*: 0x.f5f 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

16-bit unsigned load from MEM[VALUE] into $rD


.. _rd_eq_mem_ra_plus_value:

$rD <- MEM[$rA + VALUE]
-----------------------

*Instruction code*: 0x.f6. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

32-bit load from MEM[$rA+VALUE] into $rD


.. _rd_eq_mem_value:

$rD <- MEM[VALUE]
-----------------

*Instruction code*: 0x.f6f 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

32-bit load from MEM[VALUE] into $rD


.. _rd_eq_memll_ra_plus_value:

$rD <- MEMLL[$rA + VALUE]
-------------------------

*Instruction code*: 0x.f7. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

32-bit unsigned load-lock (exclusive load)


.. _rd_eq_memll_value:

$rD <- MEMLL[VALUE]
-------------------

*Instruction code*: 0x.f7f 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

32-bit unsigned load-lock (exclusive load)


.. _mem8_ra_plus_value_eq_rd:

MEM8[$rA + VALUE] <- $rD
------------------------

*Instruction code*: 0x.f8. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

8-bit store to MEM[$rA+VALUE] from $rD


.. _mem8_value_eq_rd:

MEM8[VALUE] <- $rD
------------------

*Instruction code*: 0x.f8f 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

8-bit store to MEM[VALUE] from $rD


.. _mem16_ra_plus_value_eq_rd:

MEM16[$rA + VALUE] <- $rD
-------------------------

*Instruction code*: 0x.f9. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

16-bit store to MEM[$rA+VALUE] from $rD


.. _mem16_value_eq_rd:

MEM16[VALUE] <- $rD
-------------------

*Instruction code*: 0x.f9f 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

16-bit store to MEM[VALUE] from $rD


.. _mem_ra_plus_value_eq_rd:

MEM[$rA + VALUE] <- $rD
-----------------------

*Instruction code*: 0x.fa. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

32-bit store to MEM[$rA+VALUE] from $rD


.. _mem_value_eq_rd:

MEM[VALUE] <- $rD
-----------------

*Instruction code*: 0x.faf 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

32-bit store to MEM[VALUE] from $rD


.. _memsc_ra_plus_value_eq_rd:

MEMSC[$rA + VALUE] <- $rD
-------------------------

*Instruction code*: 0x.fb. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

32-bit store-conditional (exclusive store)


.. _memsc_value_eq_rd:

MEMSC[VALUE] <- $rD
-------------------

*Instruction code*: 0x.fbf 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

32-bit store-conditional (exclusive store)


.. _rd_eq_smem8_ra_plus_value:

$rD <- SMEM8[$rA + VALUE]
-------------------------

*Instruction code*: 0x.fc. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

8-bit signed load from MEM[$rA+VALUE] into $rD


.. _rd_eq_smem8_value:

$rD <- SMEM8[VALUE]
-------------------

*Instruction code*: 0x.fcf 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

8-bit signed load from MEM[VALUE] into $rD


.. _rd_eq_smem16_ra_plus_value:

$rD <- SMEM16[$rA + VALUE]
--------------------------

*Instruction code*: 0x.fd. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

16-bit signed load from MEM[$rA+VALUE] into $rD


.. _rd_eq_smem16_value:

$rD <- SMEM16[VALUE]
--------------------

*Instruction code*: 0x.fdf 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

16-bit signed load from MEM[VALUE] into $rD


.. _full_rd_eq_mem_ra:

full $rD <- MEM[$rA]
--------------------

*Instruction code*: 0x.ff.

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

Load full $rD (no use/modification of vstart vend)


.. _full_rd_eq_mem_value:

full $rD <- MEM[VALUE]
----------------------

*Instruction code*: 0x.fff 0x**** 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

Load full $rD (no use/modification of vstart vend)

