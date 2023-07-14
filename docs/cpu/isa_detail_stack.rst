




MEM[$rS + tiny OFFSET] <- $rD
---------------------------------------------

*Instruction code*: 0x.c**

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |            OFS            | S |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Store the 32-bit value of :code:`$rD` at memory location :code:`$rS + OFFSET`. The field OFS is computed by dividing OFFSET by four, then truncating it to 7 bits. Thus, the offset range of -256 to 252 is supported in steps of 4. The base register, :code:`$rS` is :code:`$r12` (:code:`$fp`) if field S is 0, :code:`$r13` (:code:`$sp`) otherwise. This instruction is useful for stack-frame manipulations.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an unaligned access exception is thrown.


$rD <- MEM[$rS + tiny OFFSET]
---------------------------------------------

*Instruction code*: 0x.d**

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |            OFS            | S |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Load the 32-bit value into :code:`$rD` from memory location :code:`$rS + OFFSET`. The type of :code:`$rD` is not modified. The field OFS is computed by dividing OFFSET by four, then truncating it to 7 bits. Thus, the offset range of -256 to 252 is supported in steps of 4. The base register, :code:`$rS` is :code:`$r12` (:code:`$fp`) if field S is 0, :code:`$r13` (:code:`$sp`) otherwise. This instruction is useful for stack-frame manipulations.

The implementation is allowed to throw exceptions if the memory access violates access permissions. If the resulting memory reference is unaligned, an unaligned access exception is thrown.

