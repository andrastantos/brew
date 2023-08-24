




.. _mem_rs_plus_tiny_offset_eq_rd:

MEM32[$rS + tiny OFFSET] <- $rD
---------------------------------------------

*Instruction code*: 0x.c**

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |            OFS            | S |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`store type handling<store_type_handling>` to determine which parts of the register :code:`$rD` to store at memory location :code:`$rS + OFFSET`. The field OFS is computed by dividing OFFSET by four, then truncating it to 7 bits. Thus, the offset range of -256 to 252 is supported in steps of 4. The base register, :code:`$rS` is :code:`$r12` (:code:`$fp`) if field S is 0, :code:`$r13` (:code:`$sp`) otherwise.

This instruction is useful for stack-frame manipulations.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.

.. _rd_eq_mem_rs_plus_tiny_offset:

$rD <- MEM32[$rS + tiny OFFSET]
---------------------------------------------

*Instruction code*: 0x.d**

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |            OFS            | S |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: :code:`exc_unaligned`; Implementation defined

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`load type handling<load_type_handling>` to determine which parts of the register :code:`$rD` to load from memory location :code:`$rS + OFFSET`. The field OFS is computed by dividing OFFSET by four, then truncating it to 7 bits. Thus, the offset range of -256 to 252 is supported in steps of 4. The base register, :code:`$rS` is :code:`$r12` (:code:`$fp`) if field S is 0, :code:`$r13` (:code:`$sp`) otherwise.

This instruction is useful for stack-frame manipulations.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an :code:`exc_unaligned` exception is raised.
