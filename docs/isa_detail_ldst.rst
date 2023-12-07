

.. _rd_eq_mem8_ra:

$rD <- MEM8[$rA]
---------------------------------------------

*Instruction code*: 0x.e4.

*Exceptions*: Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, an 8-bit value is loaded from memory location :code:`$rA`, zero-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions.

.. _rd_eq_mem16_ra:

$rD <- MEM16[$rA]
---------------------------------------------

*Instruction code*: 0x.e5.

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, a 16-bit value is loaded from memory location :code:`$rA`, zero-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.

.. _rd_eq_mem_ra:

$rD <- MEM32[$rA]
---------------------------------------------

*Instruction code*: 0x.e6.

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine which parts of the register :code:`$rD` to load from memory location :code:`$rA`.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.


.. _rd_eq_memll_ra:

$rD <- MEMLL[$rA]
---------------------------------------------

*Instruction code*: 0x.e7.

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if :code:`$rD` is load from memory location :code:`$rA`. If the load is permitted to proceed, a 32-bit value is loaded from memory and placed in :code:`$rD`, while a load-lock is placed on the memory location.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.



.. _mem8_ra_eq_rd:

MEM8[$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.e8.

*Exceptions*: Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine if :code:`$rD` can be stored at memory location :code:`$rA`. Only the lowest 8-bits of :code:`$rD` are stored.

This store operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions.



.. _mem16_ra_eq_rd:

MEM16[$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.e9.

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine if :code:`$rD` can be stored at memory location :code:`$rA`. Only the lowest 16 bits of :code:`$rD` are stored.

This store operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.



.. _mem_ra_eq_rd:

MEM32[$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.ea.

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine which parts of the register :code:`$rD` to store at memory location :code:`$rA`.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.




.. _memsc_ra_eq_rd:

MEMSC[$rA] <- $rD
---------------------------------------------

*Instruction code*: 0x.eb.

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine if the register :code:`$rD` is stored at memory location :code:`$rA`.  If the store is permitted to proceed, the value of :code:`$rD` is stored in the memory location pointed to by :code:`$rA`, if and only if a still valid load-lock exists for the same address for the same processor. If such a lock is not found, the store fails and no memory update is performed.

The value of :code:`$rD` is set to 0 if the store succeeded and to non-zero if it failed. The actual non-zero value is implementation-defined and is not required to be constant, only that it is never zero. The type of :code:`$rD` is set to INT32.

This store operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised. In case of an exception, neither the existence of a lock nor the value stored in memory is altered.



.. _rd_eq_smem8_ra:

$rD <- SMEM8[$rA]
---------------------------------------------

*Instruction code*: 0x.ec.

*Exceptions*: Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, an 8-bit value is loaded from memory location :code:`$rA`, sign-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions.

.. _rd_eq_smem16_ra:

$rD <- SMEM16[$rA]
---------------------------------------------

*Instruction code*: 0x.ed.

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, a 16-bit value is loaded from memory location :code:`$rA`, sign-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.







.. _rd_eq_mem8_ra_plus_value:

$rD <- MEM8[$rA + VALUE]
------------------------

*Instruction code*: 0x.f4. 0x****

*Exceptions*: Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, an 8-bit value is loaded from memory location :code:`$rA + VALUE`, zero-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

:code:`VALUE` is computed from FIELD_e by sign-extending it to 32 bits.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions.


.. _rd_eq_mem8_value:

$rD <- MEM8[VALUE]
------------------

*Instruction code*: 0x.f4f 0x**** 0x****

*Exceptions*: Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, an 8-bit value is loaded from memory location :code:`VALUE`, zero-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions.


.. _rd_eq_mem16_ra_plus_value:

$rD <- MEM16[$rA + VALUE]
-------------------------

*Instruction code*: 0x.f5. 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, a 16-bit value is loaded from memory location :code:`$rA + VALUE`, zero-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

:code:`VALUE` is computed from FIELD_e by sign-extending it to 32 bits.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.


.. _rd_eq_mem16_value:

$rD <- MEM16[VALUE]
-------------------

*Instruction code*: 0x.f5f 0x**** 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, a 16-bit value is loaded from memory location :code:`VALUE`, zero-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.


.. _rd_eq_mem_ra_plus_value:

$rD <- MEM32[$rA + VALUE]
-----------------------

*Instruction code*: 0x.f6. 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine which parts of the register :code:`$rD` to load from memory location :code:`$rA`.

:code:`VALUE` is computed from FIELD_e by sign-extending it to 32 bits.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.


.. _rd_eq_mem_value:

$rD <- MEM32[VALUE]
-----------------

*Instruction code*: 0x.f6f 0x**** 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine which parts of the register :code:`$rD` to load from memory location :code:`$rA`.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.



.. _rd_eq_memll_ra_plus_value:

$rD <- MEMLL[$rA + VALUE]
-------------------------

*Instruction code*: 0x.f7. 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if :code:`$rD` is load from memory location :code:`$rA + VALUE`. If the load is permitted to proceed, a 32-bit value is loaded from memory and placed in :code:`$rD`, while a load-lock is placed on the memory location.

:code:`VALUE` is computed from FIELD_e by sign-extending it to 32 bits.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.


.. _rd_eq_memll_value:

$rD <- MEMLL[VALUE]
-------------------

*Instruction code*: 0x.f7f 0x**** 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if :code:`$rD` is load from memory location :code:`VALUE`. If the load is permitted to proceed, a 32-bit value is loaded from memory and placed in :code:`$rD`, while a load-lock is placed on the memory location.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.


.. _mem8_ra_plus_value_eq_rd:

MEM8[$rA + VALUE] <- $rD
------------------------

*Instruction code*: 0x.f8. 0x****

*Exceptions*: Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine if :code:`$rD` can be stored at memory location :code:`$rA + VALUE`. Only the lowest 8-bits of :code:`$rD` are stored.

:code:`VALUE` is computed from FIELD_e by sign-extending it to 32 bits.

This store operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions.


.. _mem8_value_eq_rd:

MEM8[VALUE] <- $rD
------------------

*Instruction code*: 0x.f8f 0x**** 0x****

*Exceptions*: Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine if :code:`$rD` can be stored at memory location :code:`VALUE`. Only the lowest 8-bits of :code:`$rD` are stored.

This store operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions.


.. _mem16_ra_plus_value_eq_rd:

MEM16[$rA + VALUE] <- $rD
-------------------------

*Instruction code*: 0x.f9. 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine if :code:`$rD` can be stored at memory location :code:`$rA + VALUE`. Only the lowest 16 bits of :code:`$rD` are stored.

:code:`VALUE` is computed from FIELD_e by sign-extending it to 32 bits.

This store operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.



.. _mem16_value_eq_rd:

MEM16[VALUE] <- $rD
-------------------

*Instruction code*: 0x.f9f 0x**** 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine if :code:`$rD` can be stored at memory location :code:`VALUE`. Only the lowest 16 bits of :code:`$rD` are stored.

This store operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.


.. _mem_ra_plus_value_eq_rd:

MEM32[$rA + VALUE] <- $rD
-----------------------

*Instruction code*: 0x.fa. 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine which parts of the register :code:`$rD` to store at memory location :code:`$rA + VALUE`.

:code:`VALUE` is computed from FIELD_e by sign-extending it to 32 bits.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.


.. _mem_value_eq_rd:

MEM32[VALUE] <- $rD
-----------------

*Instruction code*: 0x.faf 0x**** 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine which parts of the register :code:`$rD` to store at memory location :code:`$rA + VALUE`.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.


.. _memsc_ra_plus_value_eq_rd:

MEMSC[$rA + VALUE] <- $rD
-------------------------

*Instruction code*: 0x.fb. 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine if the register :code:`$rD` is stored at memory location :code:`$rA + VALUE`.  If the store is permitted to proceed, the value of :code:`$rD` is stored in the memory location pointed to by :code:`$rA + VALUE`, if and only if a still valid load-lock exists for the same address for the same processor. If such a lock is not found, the store fails and no memory update is performed.

:code:`VALUE` is computed from FIELD_e by sign-extending it to 32 bits.

The value of :code:`$rD` is set to 0 if the store succeeded and to non-zero if it failed. The actual non-zero value is implementation-defined and is not required to be constant, only that it is never zero. The type of :code:`$rD` is set to INT32.

This store operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised. In case of an exception, neither the existence of a lock nor the value stored in memory is altered.


.. _memsc_value_eq_rd:

MEMSC[VALUE] <- $rD
-------------------

*Instruction code*: 0x.fbf 0x**** 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine if the register :code:`$rD` is stored at memory location :code:`VALUE`.  If the store is permitted to proceed, the value of :code:`$rD` is stored in the memory location pointed to by :code:`VALUE`, if and only if a still valid load-lock exists for the same address for the same processor. If such a lock is not found, the store fails and no memory update is performed.

The value of :code:`$rD` is set to 0 if the store succeeded and to non-zero if it failed. The actual non-zero value is implementation-defined and is not required to be constant, only that it is never zero. The type of :code:`$rD` is set to INT32.

This store operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised. In case of an exception, neither the existence of a lock nor the value stored in memory is altered.


.. _rd_eq_smem8_ra_plus_value:

$rD <- SMEM8[$rA + VALUE]
-------------------------

*Instruction code*: 0x.fc. 0x****

*Exceptions*: Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, an 8-bit value is loaded from memory location :code:`$rA + VALUE`, sign-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

:code:`VALUE` is computed from FIELD_e by sign-extending it to 32 bits.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions.


.. _rd_eq_smem8_value:

$rD <- SMEM8[VALUE]
-------------------

*Instruction code*: 0x.fcf 0x**** 0x****

*Exceptions*: Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, an 8-bit value is loaded from memory location :code:`VALUE`, sign-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions.


.. _rd_eq_smem16_ra_plus_value:

$rD <- SMEM16[$rA + VALUE]
--------------------------

*Instruction code*: 0x.fd. 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, a 16-bit value is loaded from memory location :code:`$rA + VALUE`, sign-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

:code:`VALUE` is computed from FIELD_e by sign-extending it to 32 bits.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.



.. _rd_eq_smem16_value:

$rD <- SMEM16[VALUE]
--------------------

*Instruction code*: 0x.fdf 0x**** 0x****

*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine if register :code:`$rD` is loaded. If the load is permitted to proceed, a 16-bit value is loaded from memory location :code:`VALUE`, sign-extended to 32-bits and assigned to :code:`$rD`. The 32-bit result is written to :code:`$rD`.

This operation only handles scalar types.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.






