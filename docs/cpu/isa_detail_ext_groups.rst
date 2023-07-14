

$rD <- $rA == 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.00.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.


$rD <- $rA != 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.01.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.

$rD <- $rA < 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.02.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.

$rD <- $rA >= 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.03.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.

$rD <- $rA > 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.04.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.

$rD <- $rA <= 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.05.

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Lane prediction operation. For each lane of :code:`$rA`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the source type.










$rD <- $rB == $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.1..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type.


$rD <- $rB != $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.2..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type.

$rD <- signed $rB < $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.3..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type. For integral types, the comparison is done using signed arithmetic. For floating point types, a normal floating-point comparison is performed.


$rD <- signed $rB >= $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.4..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type. For integral types, the comparison is done using signed arithmetic. For floating point types, a normal floating-point comparison is performed.



$rD <- $rB < $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.5..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type.


$rD <- $rB >= $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.6..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~
Lane prediction operation. The type of the operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and is assumed to be that of :code:`$rA`. For each lane of :code:`$rA` and :code:`$rB`, the condition is evaluated. If the condition is true, all corresponding bits on :code:`$rD` are set to 1. If the condition is false, those same bits are set to 0. For 8-bit wide lanes, 8-bits of :code:`$rD` are set per lane. For 16-bit wide lanes, 16-bits of :code:`$rD` are set per lane. For scalar types, all bits of :code:`$rD` are set at once. The destination type is set to INT32, independent of the operation type.




$rD <- interpolate $rA, $rB
---------------------------------

*Instruction code*: 0xf1ff 0x.1..

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

*Instruction code*: 0xf4ff 0x.*..
*Instruction code*: 0xf5ff 0x.*..
*Instruction code*: 0xf6ff 0x.*..
*Instruction code*: 0xf7ff 0x.*..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 0 | 1 | FLD_F |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

A scaled multiply operation. The result of the 64-bit product of :code:`$rA` and :code:`$rB` is arithmetically shifted to the left before being stored in the result register. The bottom 4 bits of VALUE is stored in FIELD_C, the top 2 bits in FLD_F. The type of the operation is determined by the type of :code:`$rA`. :code:`$rB` is ignored and is assumed to be of the same type. If the type denotes a floating-point type, an invalid instruction exception is thrown. The result type is the type of the operation.

.. todo::
  This is not how BINUTILS is coded up at the moment. We need to follow-up with the changes there.

$rD <- full $rA * $rB >> VALUE
-----------------------------------

*Instruction code*: 0xf8ff 0x.*..
*Instruction code*: 0xf9ff 0x.*..
*Instruction code*: 0xfaff 0x.*..
*Instruction code*: 0xfbff 0x.*..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 0 | FLD_F |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+


*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

A scaled multiply operation. The result of the 64-bit product of :code:`$rA` and :code:`$rB` is logically shifted to the left before being stored in the result register. The bottom 4 bits of VALUE is stored in FIELD_C, the top 2 bits in FLD_F. The type of the operation is determined by the type of :code:`$rA`. :code:`$rB` is ignored and is assumed to be of the same type. If the type denotes a floating-point type, an invalid instruction exception is thrown. The result type is the type of the operation.

.. todo::
  This is not how BINUTILS is coded up at the moment. We need to follow-up with the changes there.
