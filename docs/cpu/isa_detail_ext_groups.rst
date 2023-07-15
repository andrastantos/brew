

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









$rD <- sum $rA
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.01.

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Computes the reduction sum of $rA, puts the total in $rD. If $rA is of a float type, the type of $rD is set to :code:`FP32`. Otherwise, the type of $rD is set to :code:`INT32`.




$rD <- set_vend $rA
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.02.

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

This instruction, given a desired vector length in bytes in $rA returns (and sets :code:`vend` to) the number of bytes to be processed in a vector register.

The returned value is the smaller of :code:`vlen` and $rA.

.. note:: Since element type is not known to this instruction, its possible that the returned :code:`vend` is not aligned to an element boundary. While an unaligned :code:`vend` will get truncated by the subsequent vector instructions, $rD also contains an unaligned value which can be misused. It is the responsibility of the programmer to make sure that the requested byte count is a multiple of the size of the vector elements.

$rD <- interpolate $rA, $rB
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.1..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

This instruction performs linear interpolation between adjacent lanes of $rA using the value of $rB as the interpolator.

If $rB is of an integral type, it is assumed to be a fractional value between 0 and 1. If it's a floating-point type, its value must be between 0.0 and 1.0.

If the value of $rB is not within the requisite range, the outcome of the operation is implementation-defined.

If $rB is a scalar type, it's broadcast to all lanes. If $rB is a vector type, its value is used lane-wise::

  $rD(i*2+0) <- $rA(i*2+0) *    $rB(i*2+0)  + $rA(i*2+1) *    $rB(i*2+1)
  $rD(i*2+1) <- $rA(i*2+0) * (1-$rB(i*2+0)) + $rA(i*2+1) * (1-$rB(i*2+1))

.. todo:: Extension group encoding changed. Toolset needs updating.

.. todo:: Do we really want to support this for floating-point types? There are a boat-load of multiplies here!



$rD <- $rD(i) <- $rA($rB(i))
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.2..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Each lane of $rD is set to the lane of $rA referenced by the corresponding lane of $rB.

.. todo:: Original lane-swizzle:
  0x.af. 0x****              $rD <- lane_swizzle $rA, VALUE
  got removed. Toolset needs updating.



$rD <- (cast TYPE_B)$rA
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.3..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Element-wise type-cast $rA to TYPE_B




$rD <- compress $rA & $rB
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.4..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Element-wise compressed selection of $rA, $rB being the selector




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
