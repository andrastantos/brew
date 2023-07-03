

$rD <- $rA == 0
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.00.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.


$rD <- $rA != 0
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.01.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.

$rD <- $rA < 0
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.02.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.

$rD <- $rA >= 0
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.03.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.

$rD <- $rA > 0
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.04.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.

$rD <- $rA <= 0
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.05.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.










$rD <- $rB == $rA
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.1..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type.


$rD <- $rB != $rA
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.2..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type.

$rD <- signed $rB < $rA
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.3..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type. For integral types, the comparison is done using signed arithmetic. For floating point types, a normal floating-point comparison is performed.


$rD <- signed $rB >= $rA
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.4..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type. For integral types, the comparison is done using signed arithmetic. For floating point types, a normal floating-point comparison is performed.



$rD <- $rB < $rA
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.5..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type.


$rD <- $rB >= $rA
----------------------------------------------------------

*Instruction code*: 0xfff0 0x.6..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type.




$rD <- interpolate $rA, $rB
---------------------------------

*Instruction code*: 0xfff1 0x.0..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
This instruction performs linear interpolation between adjacent lanes of $rA using the value of $rB as a fractional 32-bit value.

A 2-lane operation is as follows::

  $rD(0) <- $rA(0) *    $rB  + $rA(1) * (1-$rB)
  $rD(1) <- $rA(0) * (1-$rB) + $rA(1) *    $rB

A 4-lane operation is as follows::

  $rD(0) <- $rA(0) *    $rB  + $rA(1) * (1-$rB)
  $rD(1) <- $rA(0) * (1-$rB) + $rA(1) *    $rB
  $rD(3) <- $rA(3) *    $rB  + $rA(4) * (1-$rB)
  $rD(4) <- $rA(3) * (1-$rB) + $rA(4) *    $rB

In the above the indices of the registers denote lane indices. For floating-point or scalar types an invalid instruction exception is thrown. The type of the operation and the destination type is determined by the type of :code:`$rA`.


$rD <- full $rA * $rB >>> VALUE
-----------------------------------

*Instruction code*: 0xff0* 0x.0.. or 0xff1* 0x.0..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 0 | 0 | 0 |      FIELD_F      |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

=========================  ==================================  ==================
Instruction code           Assembly                            Operation
=========================  ==================================  ==================
0xff0* 0x.0..              $rD <- full $rA * $rB >>> FIELD_F
0xff1* 0x.0..              $rD <- full $rA * $rB >>> FIELD_F
0xff0* 0x.1..              $rD <- full $rA * $rB >> FIELD_F
0xff1* 0x.1..              $rD <- full $rA * $rB >> FIELD_F
=========================  ==================================  ==================

.. todo::
  This group can be implemented in the 0xfff. style in the following way: 0xfff8/0xfff9: arithmetic shift by FIELD_C[+32], 0xfffa/0xfffb: logical shift by FIELD_C[+32]. That would free up this whole extension group and result in a much more compact encoding, but the binutils is not built this way at the moment.

Prefix instructions
-------------------

Prefix instructions can precede any other instruction to modify their behavior.

.. note::
  *Exception behavior*: If a prefixed instruction throws an exception, $tpc points to the (first) prefix instruction after entering SCHEDULER mode. This allows the recovery code to decode and potentially retry the excepted instruction.

.. note::
  *Interrupt behavior*: If an interrupt is handled during the execution of a prefixed instruction, $tpc points to the (first) prefix instruction after entering SCHEDULER mode. None of the side-effects of the prefixed instruction take effect. If any of the side-effects of the prefixed instruction have taken effect, the whole instruction must be carried to completion and $tpc points to the subsequent instruction after entering SCHEDULER mode. In other words, under no circumstances can $tpc point anywhere between the first prefix and it's corresponding instruction when entering SCHEDULER mode.

.. note::
  *Prefix concatenation*: Every processor implementation has a maximum instruction length it supports. In this version of the spec, it's 64 bits. So, if the instruction (pre-)decode stage finds an instruction longer then that maximum, it raises an invalid instruction exception (or more precisely, it replaces the decoded instruction with that of the SII instruction. Without this provision it would be possible to create arbitrarily long instruction sequences in TASK mode. That in turn would prevent interrupts from being raised, effectively locking up the system (at least up to the point of exhausting the addressable RAM space).

Type override
~~~~~~~~~~~~~

This prefix instruction allows for the changing the way the subsequent operation interprets source operand types. It doesn't actually change the source register types. It also allows for explicit control of whether the destination type is written or not.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_A    | 1 | 1 | 1 | D | 1 | 1 | 1 | 1 |    TYPE_B     | ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Type override for $rA (TYPE_A) and $rB (TYPE_B). If D is set, $rD type is written
back into the register file. If cleared, $rD's type is not changed.


Unused instruction groups
-------------------------

All of the following instruction groups are explicitly reserved for future use. All processor implementations must raise an invalid instruction exception upon encountering them. The instruction size is shown as guidance for future uses: a simple instruction size decode logic would identify these groups as the shown size. It is not guaranteed that they are going to be used in such a fashion nor does it really matter for processor implementations following this version of the ISA specification.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 |    FIELD_C    | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

.. note:: FIELD_C != 0xf
.. note::
  The branch predictor is allowed to treat this group as a conditional branch

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |    FIELD_B    | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

.. note:: FIELD_B != 0xf

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

.. note:: FIELD_C != 0xc; FIELD_C != 0xd; FIELD_C != 0xf; FIELD_D != 0xf

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

.. note:: FIELD_D != 0xf

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    | 1 | 1 | 1 | 0 |    FIELD_B    | 1 | 1 | 1 | 1 |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...
  |                         FIELD_E  lower 16 bits              ...
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+-...

  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  ...                       FIELD_E   upper 16 bits               |
  ...-+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

.. note:: FIELD_D != 0xf; FIELD_B != 0xf

.. note::
  if FIELD_D == 0x2 and FIELD_B == 0xe, the branch predictor is allowed to treat this instruction code as a conditional branch.

Cache invalidation
==================

There are instructions to invalidate individual data-, instruction- and L2 cache lines. These are encoded in the various load/store groups. There is no way to distinguish which cache we intend to invalidate. There is also no instruction provided for complete cache invalidation: this functionality is to be provided through memory-mapped CSRs.

