

.. _rd_eq_ra_eq_0:

$rD <- $rA == 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.00.

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?




.. _rd_eq_ra_ne_0:

$rD <- $rA != 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.01.

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?



.. _rd_eq_ra_lt_0:

$rD <- $rA < 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.02.

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?



.. _rd_eq_ra_ge_0:

$rD <- $rA >= 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.03.

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?



.. _rd_eq_ra_gt_0:

$rD <- $rA > 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.04.

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?



.. _rd_eq_ra_le_0:

$rD <- $rA <= 0
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.05.

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?












.. _rd_eq_rb_eq_ra:

$rD <- $rB == $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.1..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?




.. _rd_eq_rb_ne_ra:

$rD <- $rB != $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.2..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?



.. _rd_eq_signed_rb_lt_ra:

$rD <- signed $rB < $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.3..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?




.. _rd_eq_signed_rb_ge_ra:

$rD <- signed $rB >= $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.4..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?





.. _rd_eq_rb_lt_ra:

$rD <- $rB < $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.5..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?




.. _rd_eq_rb_ge_ra:

$rD <- $rB >= $rA
----------------------------------------------------------

*Instruction code*: 0xf0ff 0x.6..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane predication operation that uses :ref:`predication type handling<predication_type_handling>` to determine the source operand and destination types.

It evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated; the result lane is set to all 0-s if the condition evaluates to false, and all 1-s otherwise.

For scalar types, the result is set to 0 of the condition evaluates to false and 0xffffffff otherwise.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?











.. _rd_eq_sum_ra:

$rD <- sum $rA
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.01.

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

Computes the reduction sum of $rA, puts the total in $rD. If $rA is of a float type, the type of $rD is set to :code:`FP32`. Otherwise, the type of $rD is set to :code:`INT32`. If $rA is of a vector type, each lane is converted to the result type, then summed together (this means that either FP32 or INT32 math is used for the summation). The result is then placed in :code:`$rD`. If :code:`$rA` is a scalar type, it is simply copied to :code:`$rD`.

For vector types, only lanes up to :code:`VEND` are considered. :code:`VSTART` is ignored by the operation.


.. _rd_eq_set_vend_ra:

$rD <- set_vend $rA
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.02.

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

This instruction, given a desired vector length in bytes in :code:`$rA` returns (and sets :code:`VEND` to) the number of bytes to be processed in a vector register.

The returned value is the smaller of :code:`VLEN` and :code:`$rA`.

.. note:: Since element type is not known to this instruction, its possible that the returned :code:`VEND` is not aligned to an element boundary. While an unaligned :code:`VEND` will get truncated by the subsequent vector instructions, :code:`$rD` also contains an unaligned value which can be misused. It is the responsibility of the programmer to make sure that the requested byte count is a multiple of the size of the vector elements.



.. _rd_eq_interpolate_ra,_rb:

$rD <- interpolate $rA, $rB
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.1..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This instruction performs linear interpolation between adjacent lanes of :code:`$rA` using the value of :code:`$rB` as the interpolator.

The operation uses a modified version of :ref:'standard type handling<std_type_handling>' to determine operand and result types. If :code:`$rA` is not a vector type, a :code:`exc_type` exception is raised.

If :code:`$rB` is of a fixed point type, it is assumed to be an unsigned, fractional value between 0 and 1. If it is a floating-point type, its value must be between 0.0 and 1.0.

If the value of $rB is not within the requisite range (for floating-point types), the outcome of the operation is implementation-defined.

The result lanes are computes as follows::

  $rD(i*2+0) <- $rA(i*2+0) *    $rB(i*2+0)  + $rA(i*2+1) *    $rB(i*2+1)
  $rD(i*2+1) <- $rA(i*2+0) * (1-$rB(i*2+0)) + $rA(i*2+1) * (1-$rB(i*2+1))

.. note:: For fixed point operands, the multiplication is performed with the appropriate shifts to represent (lanes of) :code:`$rB` in the 0 to 1 unsigned range. As a consequence, no saturation or overflow can happen.

.. todo:: Extension group encoding changed. Toolset needs updating.

An implementation can limit the number of types this instruction is supported on. For instance, it is possible that only integer types are supported. In that case, if an unsupported type is encountered, the implementation raises an :code:`exc_type` exception.




.. _rd_eq_rd(i)_eq_ra(rb(i)):

$rD(i) <- $rA($rB(i))
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.2..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

This is a lane swizzle operation.

If :code:`$rA` is not a vector type, a :code:`exc_type` exception is raised. If :code:`$rB` is a vector type, it must have the same lane-size as :code:`$rA`, otherwise a :code:`exc_type` exception is raised. If :code:`$rB` is INT32, it is :ref:`broadcast<type_broadcast>` to the logic type of :code:`$rA`. The result type is that of :code:`$rA`.

Each lane of $rD (as governed by :code:`VEND`) is set to the lane of :code:`$rA`` referenced by the corresponding lane of :code:`$rB`. Lane indices in are zero-based. The lane index is wrapped by the number of lanes specified for the type by :code:`VLEN`. If lanes beyond the size of :code:`$rA` are accessed, they read as 0.

.. todo:: Original lane-swizzle: 0x.af. 0x****  ($rD <- lane_swizzle $rA, VALUE)   got removed. Toolset needs updating.



.. _rd_eq_(cast_type_b)ra:

$rD <- (cast TYPE_B)$rA
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.3..

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

The result type is determined by TYPE_B.

If :code:`$rA` is a vector type with a lane-count lower than that of TYPE_B, lanes indices are circularly addressed. If :code:`rA` is a scalar type, its value is reused for each lane. If TYPE_B denotes a scalar type, while :code:`$rA` is of a vector type, only the first lane of :code:`$rA` is considered.

For vector results, each lane of :code:`$rD` is assigned to the corresponding lane in :code:`$rA`, converted to the result lane type. This includes conversion between scalar types, which involves potential sign- or zero-extension as well as truncation and saturation; between floating-point types, which involves adjustment of both exponent and mantissa precisions; as well as conversion between floating and fixed point types.

For scalar results, the sample applies, except for only a single element.

For vector results, the operation is controlled by :code:`VEND`, but not by :code:`VSTART`.

Circular addressing example: If TYPE_B is VFP16 while type of :code:`$rA` is INT32 and :code:`VLEN` of the implementation is 16, the following lane indexing applies::

  $rD(0) <- (FP16)$rA(0)
  $rD(1) <- (FP16)$rA(1)
  $rD(2) <- (FP16)$rA(2)
  $rD(3) <- (FP16)$rA(3)
  $rD(4) <- (FP16)$rA(0)
  $rD(5) <- (FP16)$rA(1)
  $rD(6) <- (FP16)$rA(2)
  $rD(7) <- (FP16)$rA(3)

Scalar example: If TYPE_B is VFP32 while type of :code:`$rA` is INT32 and :code:`VEND` is set to 4, the following lane indexing applies::

  $rD(0) <- (FP16)$rA
  $rD(1) <- (FP16)$rA
  $rD(2) <- (FP16)$rA
  $rD(3) <- (FP16)$rA


.. todo:: What to do in case of an overflow? Set an FP sticky-bit?


.. _rd_eq_compress_ra_and_rb:

$rD <- compress $rA & $rB
----------------------------------------------------------

*Instruction code*: 0xf1ff 0x.4..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and result types.

The operation examines each byte of :code:`$rB`. If the byte is 0, the corresponding byte in :code:`$rA` is skipped. If not, the corresponding byte in :code:`$rA` is appended to :code:`$rD`. In other words, the vector :code:`$rA` is element-wise compressed using predicate :code:`$rB`. The remaining bytes of :code:`$rD` are loaded with 0. The operation of the instruction is controlled by :code:`VEND` but not by :code:`VSTART`.

.. note:: If :code:`$rA` is a scalar type, it is still treated per the pervious description. Since the operation considers only bytes, not lanes, it's not dependent on the 'vector-ness' of the operands.




.. _rd_eq_full_ra_times_rb_asr_value:

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


*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types with the following modification: if either :code:`$rA` or :code:`$rB` is a floating-point type, an :code:`exc_type` exception is raised.

This is a scaled multiply operation. The result of the double-wide product of the lanes of :code:`$rA` and :code:`$rB` is arithmetically shifted to the left before being stored in the result register lane.

For scalar operands, a single 64-bit multiplication followed by an arithmetic shift is performed.

The bottom 4 bits of VALUE is stored in FIELD_C, the top 2 bits in FLD_F.

.. todo::
  This is not how BINUTILS is coded up at the moment. We need to follow-up with the changes there.


.. _rd_eq_full_ra_times_rb_lsr_value:

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


*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types with the following modification: if either :code:`$rA` or :code:`$rB` is a floating-point type, an :code:`exc_type` exception is raised.

This is a scaled multiply operation. The result of the double-wide product of the lanes of :code:`$rA` and :code:`$rB` is logically shifted to the left before being stored in the result register lane.

For scalar operands, a single 64-bit multiplication followed by a logical shift is performed.

The bottom 4 bits of VALUE is stored in FIELD_C, the top 2 bits in FLD_F.

.. todo::
  This is not how BINUTILS is coded up at the moment. We need to follow-up with the changes there.

