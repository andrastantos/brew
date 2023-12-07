
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

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point subtraction is performed.

.. _rd_eq_ra_lsl_rb:

$rD <- $rA << $rB
--------------------------

*Instruction code*: 0x.6..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types. Then, :code:`$rA` is left-shifted by :code:`$rB`. 0-s are shifted in from the right.


.. _rd_eq_ra_lsr_rb:

$rD <- $rA >> $rB
--------------------------

*Instruction code*: 0x.7..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types. Then, :code:`$rA` is right-shifted by :code:`$rB`. 0-s are shifted in from the left.

.. _rd_eq_ra_asr_rb:

$rD <- $rA >>> $rB
--------------------------

*Instruction code*: 0x.8..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types. Then, :code:`$rA` is right-shifted by :code:`$rB`. The MSB of each lane is replicated as it is shifted in from the left.

.. _rd_eq_ra_times_rb:

$rD <- $rA * $rB
--------------------------

*Instruction code*: 0x.9..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point multiplication is performed.


.. _rd_eq_type_rb:

$rD <- TYPE_NAME $rB
--------------------------

*Instruction code*: 0x.a..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

Converts the value stored in :code:`$rB` to type 'type' and stores in in :code:`$rD`. If such conversion is not possible or an unsupported type is specified, the instruction raises a :code:`exc_type` exception.

Type is encoded in FIELD_A.

.. todo:: This used to be the ~$rA & $rB instruction. That needs to be removed and this to be added in the toolset and Espresso.

.. _rd_eq_tiny_rb_plus_const:

$rD <- tiny $rB + CONST
--------------------------

*Instruction code*: 0x.b..

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point addition is performed.

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

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point addition is performed.

.. _rd_eq_short_value_minus_ra:

$rD <- short VALUE - $rA
--------------------------

*Instruction code*: 0x.5f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point subtraction is performed.


.. _rd_eq_short_ra_lsl_value:

$rD <- short $rA << VALUE
--------------------------

*Instruction code*: 0x.6f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types. Then, :code:`$rA` is left-shifted by :code:`$rB`. 0-s are shifted in from the right.

.. _rd_eq_short_ra_lsr_value:

$rD <- short $rA >> VALUE
--------------------------

*Instruction code*: 0x.7f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types. Then, :code:`$rA` is right-shifted by :code:`$rB`. 0-s are shifted in from the left.

.. _rd_eq_short_ra_asr_value:

$rD <- short $rA >>> VALUE
--------------------------

*Instruction code*: 0x.8f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types. Then, :code:`$rA` is right-shifted by :code:`$rB`. The MSB of each lane is replicated as it is shifted in from the left.

.. _rd_eq_short_value_times_ra:

$rD <- short VALUE * $rA
--------------------------

*Instruction code*: 0x.9f. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point multiplication is performed.























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

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point addition is performed.

.. _rd_eq_value_minus_rb:

$rD <- VALUE - $rB
--------------------------

*Instruction code*: 0x.5.f 0x**** 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point subtraction is performed.


.. _rd_eq_value_lsl_rb:

$rD <- VALUE << $rB
--------------------------

*Instruction code*: 0x.6.f 0x**** 0x*****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types. Then, :code:`$rA` is left-shifted by :code:`$rB`. 0-s are shifted in from the right.

.. _rd_eq_value_lsr_rb:

$rD <- VALUE >> $rB
--------------------------

*Instruction code*: 0x.7.f 0x**** 0x*****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types. Then, :code:`$rA` is right-shifted by :code:`$rB`. 0-s are shifted in from the left.

.. _rd_eq_value_asr_rb:

$rD <- VALUE >>> $rB
--------------------------

*Instruction code*: 0x.8.f 0x**** 0x*****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`shift type handling<shift_type_handling>` to determine the operand and destination types. Then, :code:`$rA` is right-shifted by :code:`$rB`. The MSB of each lane is replicated as it is shifted in from the left.

.. _rd_eq_value_times_rb:

$rD <- VALUE * $rB
--------------------------

*Instruction code*: 0x.9.f 0x**** 0x*****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting types either an integer or a float-point multiplication is performed.
