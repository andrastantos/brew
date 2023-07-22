
.. _rd_eq_ra_xor_rb:

$rD <- $rA ^ $rB
--------------------------

*Instruction code*: 0x.1..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting types a bit-wise 'xor' operation is performed.



.. _rd_eq_ra_or_rb:

$rD <- $rA | $rB
--------------------------

*Instruction code*: 0x.2..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting types a bit-wise 'or' operation is performed.


.. _rd_eq_ra_and_rb:

$rD <- $rA & $rB
--------------------------

*Instruction code*: 0x.3..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting types a bit-wise 'and' operation is performed.


.. _rd_eq_ra_plus_rb:

$rD <- $rA + $rB
--------------------------

*Instruction code*: 0x.4..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point lane-wise addition is performed.

.. _rd_eq_ra_minus_rb:

$rD <- $rA - $rB
--------------------------

*Instruction code*: 0x.5..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point lane-wise subtraction is performed.

.. _rd_eq_ra_lsl_rb:

$rD <- $rA << $rB
--------------------------

*Instruction code*: 0x.6..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types.

If the resulting operands are vector types, each lane if :code:`$rA` is left-shifted by the value in the corresponding lane of :code:`$rB`. If the resulting operands are scalar types, :code:`$rA` is left-shifted by :code:`$rB'. 0-s are shifted in from the right.


.. _rd_eq_ra_lsr_rb:

$rD <- $rA >> $rB
--------------------------

*Instruction code*: 0x.7..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types.

If the resulting operands are vector types, each lane if :code:`$rA` is right-shifted by the value in the corresponding lane of :code:`$rB`. If the resulting operands are scalar types, :code:`$rA` is right-shifted by :code:`$rB'. 0-s are shifted in from the left.

.. _rd_eq_ra_asr_rb:

$rD <- $rA >>> $rB
--------------------------

*Instruction code*: 0x.8..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types.

If the resulting operands are vector types, each lane if :code:`$rA` is right-shifted by the value in the corresponding lane of :code:`$rB`. If the resulting operands are scalar types, :code:`$rA` is right-shifted by :code:`$rB'. The MSB of each lane is replicated as it is shifted in from the left.

.. _rd_eq_ra_times_rb:

$rD <- $rA * $rB
--------------------------

*Instruction code*: 0x.9..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point lane-wise multiplication is performed.


.. _rd_eq_notra_and_rb:

$rD <- $rA & ~$rB
--------------------------

*Instruction code*: 0x.a..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. With the resulting types, first a binary inverse of :code:`$rB` is performed. The result of that is then bit-wise 'and'-ed with :code:`$rA`.

This operation is useful for lane predication: if :code:`$rB` contains a bit-wise predicate, the following sequence of instructions can be used to assemble a predicated lane-selection for vector operations::

  $r8 <- $r8 & $r3   # Mask lanes by predicate
  $r9 <- $r9 & ~$r3  # Inverse-mask lanes by predicate
  $r8 <- $r8 | $r9   # Combine lanes

.. todo:: The inversion is swapped from $rA to $rB. This needs to be followed up in the toolset and Espresso.

.. _rd_eq_tiny_rb_plus_const:

$rD <- tiny $rB + CONST
--------------------------

*Instruction code*: 0x.b..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point lane-wise addition is performed.

FIELD_A contains the ones complement value of CONST. The valid range is -7 to 7. This value is sign-extended to 32-bits during type handling, which makes the operation rather pointless for floating-point types.









.. _rd_eq_short_value_xor_ra:

$rD <- short VALUE ^ $rA
--------------------------

*Instruction code*: 0x.1f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting types a bit-wise 'xor' operation is performed.


.. _rd_eq_short_value_or_ra:

$rD <- short VALUE | $rA
--------------------------

*Instruction code*: 0x.2f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting types a bit-wise 'or' operation is performed.

.. _rd_eq_short_value_and_ra:

$rD <- short VALUE & $rA
--------------------------

*Instruction code*: 0x.3f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting types a bit-wise 'and' operation is performed.


.. _rd_eq_short_value_plus_ra:

$rD <- short VALUE + $rA
--------------------------

*Instruction code*: 0x.4f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point lane-wise addition is performed.

.. _rd_eq_short_value_minus_ra:

$rD <- short VALUE - $rA
--------------------------

*Instruction code*: 0x.5f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point lane-wise subtraction is performed.


.. _rd_eq_short_ra_lsl_value:

$rD <- short $rA << VALUE
--------------------------

*Instruction code*: 0x.6f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types.

If the resulting operands are vector types, each lane if :code:`$rA` is left-shifted by the value in the corresponding lane of :code:`$rB`. If the resulting operands are scalar types, :code:`$rA` is left-shifted by :code:`$rB'. 0-s are shifted in from the right.

.. _rd_eq_short_ra_lsr_value:

$rD <- short $rA >> VALUE
--------------------------

*Instruction code*: 0x.7f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types.

If the resulting operands are vector types, each lane if :code:`$rA` is right-shifted by the value in the corresponding lane of :code:`$rB`. If the resulting operands are scalar types, :code:`$rA` is right-shifted by :code:`$rB'. 0-s are shifted in from the left.

.. _rd_eq_short_ra_asr_value:

$rD <- short $rA >>> VALUE
--------------------------

*Instruction code*: 0x.8f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types.

If the resulting operands are vector types, each lane if :code:`$rA` is right-shifted by the value in the corresponding lane of :code:`$rB`. If the resulting operands are scalar types, :code:`$rA` is right-shifted by :code:`$rB'. The MSB of each lane is replicated as it is shifted in from the left.

.. _rd_eq_short_value_times_ra:

$rD <- short VALUE * $rA
--------------------------

*Instruction code*: 0x.9f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point lane-wise multiplication is performed.























.. _rd_eq_value_xor_rb:

$rD <- VALUE ^ $rB
--------------------------

*Instruction code*: 0x.1.f 0x**** 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting types a bit-wise 'xor' operation is performed.


.. _rd_eq_value_or_rb:

$rD <- VALUE | $rB
--------------------------

*Instruction code*: 0x.2.f 0x**** 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting types a bit-wise 'or' operation is performed.

.. _rd_eq_value_and_rb:

$rD <- VALUE & $rB
--------------------------

*Instruction code*: 0x.3.f 0x**** 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting types a bit-wise 'and' operation is performed.

.. _rd_eq_value_plus_rb:

$rD <- VALUE + $rB
--------------------------

*Instruction code*: 0x.4.f 0x**** 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point lane-wise addition is performed.

.. _rd_eq_value_minus_rb:

$rD <- VALUE - $rB
--------------------------

*Instruction code*: 0x.5.f 0x**** 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point lane-wise subtraction is performed.


.. _rd_eq_value_lsl_rb:

$rD <- VALUE << $rB
--------------------------

*Instruction code*: 0x.6.f 0x**** 0x*****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types.

If the resulting operands are vector types, each lane if :code:`$rA` is left-shifted by the value in the corresponding lane of :code:`$rB`. If the resulting operands are scalar types, :code:`$rA` is left-shifted by :code:`$rB'. 0-s are shifted in from the right.

.. _rd_eq_value_lsr_rb:

$rD <- VALUE >> $rB
--------------------------

*Instruction code*: 0x.7.f 0x**** 0x*****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types.

If the resulting operands are vector types, each lane if :code:`$rA` is right-shifted by the value in the corresponding lane of :code:`$rB`. If the resulting operands are scalar types, :code:`$rA` is right-shifted by :code:`$rB'. 0-s are shifted in from the left.

.. _rd_eq_value_asr_rb:

$rD <- VALUE >>> $rB
--------------------------

*Instruction code*: 0x.8.f 0x**** 0x*****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types.

If the resulting operands are vector types, each lane if :code:`$rA` is right-shifted by the value in the corresponding lane of :code:`$rB`. If the resulting operands are scalar types, :code:`$rA` is right-shifted by :code:`$rB'. The MSB of each lane is replicated as it is shifted in from the left.

.. _rd_eq_value_times_rb:

$rD <- VALUE * $rB
--------------------------

*Instruction code*: 0x.9.f 0x**** 0x*****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point lane-wise multiplication is performed.
