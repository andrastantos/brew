
.. _rd_eq_ra_xor_rb:

$rD <- $rA ^ $rB
--------------------------

*Instruction code*: 0x.1..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       1       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'xor'. Destination type is set to match that of :code:`$rA`


.. _rd_eq_ra_or_rb:

$rD <- $rA | $rB
--------------------------

*Instruction code*: 0x.2..

*Assembly mnemonics*:
  NOP: encodes to 0x2222, which is $r2 <- $r2 | $r2
  $rD <- $rS: encodes to 0xD2SS

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       2       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'or'. Destination type is set to match that of :code:`$rA`

.. _rd_eq_ra_and_rb:

$rD <- $rA & $rB
--------------------------

*Instruction code*: 0x.3..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       3       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'and'. Destination type is set to match that of :code:`$rA`

.. _rd_eq_ra_plus_rb:

$rD <- $rA + $rB
--------------------------

*Instruction code*: 0x.4..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       4       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the sum of :code:`$rA` and :code:`$rB`. The type of operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and for the purposes of the operation is assumed to be the same as :code:`$rA`. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise addition is performed. For floating-point types a floating-point additional is performed.

.. _rd_eq_ra_minus_rb:

$rD <- $rA - $rB
--------------------------

*Instruction code*: 0x.5..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       5       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the difference of :code:`$rA` and :code:`$rB`. The type of operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and for the purposes of the operation is assumed to be the same as :code:`$rA`. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise subtraction is performed. For floating-point types a floating-point subtraction is performed.


.. _rd_eq_ra_lsl_rb:

$rD <- $rA << $rB
--------------------------

*Instruction code*: 0x.6..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       6       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rA` is left-shifted by :code:`$rB` bits and is assigned to :code:`$rD`. 0-s are shifted in from the right. The type of operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be INT32. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed.

.. _rd_eq_ra_lsr_rb:

$rD <- $rA >> $rB
--------------------------

*Instruction code*: 0x.7..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       7       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rA` is right-shifted by :code:`$rB` bits and is assigned to :code:`$rD`. 0-s are shifted in from the left. The type of operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be INT32. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed.

.. _rd_eq_ra_asr_rb:

$rD <- $rA >>> $rB
--------------------------

*Instruction code*: 0x.8..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       8       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rA` is right-shifted arithmetically by :code:`$rB` bits and is assigned to :code:`$rD`. The MSB of :code:`$rA` is repeatedly shifted in from the left. The type of operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and assumed to be INT32. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed.

.. _rd_eq_ra_times_rb:

$rD <- $rA * $rB
--------------------------

*Instruction code*: 0x.9..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       9       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the product of :code:`$rA` and :code:`$rB`. The type of operation is determined by the type of :code:`$rA`. The type of :code:`$rB` is ignored and for the purposes of the operation is assumed to be the same as :code:`$rA`. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise multiplication is performed. For floating-point types a floating-point multiplication is performed.

.. _rd_eq_notra_and_rb:

$rD <- ~$rA & $rB
--------------------------

*Instruction code*: 0x.a..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       a       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'negate-and'. Destination type is set to match that of :code:`$rA`. This operation is useful for lane predication: if :code:`$rA` contains a bit-wise predicate, the following sequence of instructions can be used to assemble a predicated lane-selection for vector operations:

::

  $r8 <- $r3 & $r8   # Mask lanes by predicate
  $r9 <- ~$r3 & $r9  # Inverse-mask lanes by predicate
  $r8 <- $r8 | $r9   # Combine lanes


.. _rd_eq_tiny_rb_plus_const:

$rD <- tiny $rB + CONST
--------------------------

*Instruction code*: 0x.b..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       b       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the sum of :code:`$rB` and the one-s complemented value of FIELD_A. The range of the constant is between -7 and 7. The type of operation is determined by the type of :code:`$rB`. The type of CONST is assumed to be INT32. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise addition is performed. For floating-point an invalid instruction exception is raised.









.. _rd_eq_short_value_xor_ra:

$rD <- short VALUE ^ $rA
--------------------------

*Instruction code*: 0x.1f. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'xor'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is the sign-extended value stored in FIELD_E.


.. _rd_eq_short_value_or_ra:

$rD <- short VALUE | $rA
--------------------------

*Instruction code*: 0x.2f. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'or'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is the sign-extended value stored in FIELD_E.

.. _rd_eq_short_value_and_ra:

$rD <- short VALUE & $rA
--------------------------

*Instruction code*: 0x.3f. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'and'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is the sign-extended value stored in FIELD_E.

.. _rd_eq_short_value_plus_ra:

$rD <- short VALUE + $rA
--------------------------

*Instruction code*: 0x.4f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the sum of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rB`. For vector types a lane-wise addition is performed. For floating-point types an invalid instruction exception is thrown. The binary value of VALUE is the sign-extended value stored in FIELD_E.

.. _rd_eq_short_value_minus_ra:

$rD <- short VALUE - $rA
--------------------------

*Instruction code*: 0x.5f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the difference of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rB`. For vector types a lane-wise subtraction is performed. For floating-point types an invalid instruction exception is thrown. The binary value of VALUE is the sign-extended value stored in FIELD_E.


.. _rd_eq_short_ra_lsl_value:

$rD <- short $rA << VALUE
--------------------------

*Instruction code*: 0x.6f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is left-shifted by :code:`$rB` bits and is assigned to :code:`$rD`. 0-s are shifted in from the right. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is the sign-extended value stored in FIELD_E.

.. _rd_eq_short_ra_lsr_value:

$rD <- short $rA >> VALUE
--------------------------

*Instruction code*: 0x.7f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is right-shifted by :code:`$rB` bits and is assigned to :code:`$rD`. 0-s are shifted in from the left. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is the sign-extended value stored in FIELD_E.

.. _rd_eq_short_ra_asr_value:

$rD <- short $rA >>> VALUE
--------------------------

*Instruction code*: 0x.8f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is right-shifted arithmetically by :code:`$rB` bits and is assigned to :code:`$rD`. The MSB of :code:`$rA` is repeatedly shifted in from the left. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is the sign-extended value stored in FIELD_E.

.. _rd_eq_short_value_times_ra:

$rD <- short VALUE * $rA
--------------------------

*Instruction code*: 0x.9f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the product of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise multiplication is performed. For floating-point types an invalid instruction exception is thrown. The binary value of VALUE is the sign-extended value stored in FIELD_E.






















.. _rd_eq_value_xor_rb:

$rD <- VALUE ^ $rB
--------------------------

*Instruction code*: 0x.1.f 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'xor'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is stored in FIELD_E.


.. _rd_eq_value_or_rb:

$rD <- VALUE | $rB
--------------------------

*Instruction code*: 0x.2.f 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'or'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is stored in FIELD_E.

.. _rd_eq_value_and_rb:

$rD <- VALUE & $rB
--------------------------

*Instruction code*: 0x.3.f 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'and'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is stored in FIELD_E.

.. _rd_eq_value_plus_rb:

$rD <- VALUE + $rB
--------------------------

*Instruction code*: 0x.4.f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the sum of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rB`. For vector types a lane-wise addition is performed. For floating-point types a floating-point additional is performed. The binary value of VALUE is stored in FIELD_E.

.. _rd_eq_value_minus_rb:

$rD <- VALUE - $rB
--------------------------

*Instruction code*: 0x.5.f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the difference of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rB`. For vector types a lane-wise subtraction is performed. For floating-point types a floating-point subtraction is performed. The binary value of VALUE is stored in FIELD_E.


.. _rd_eq_value_lsl_rb:

$rD <- VALUE << $rB
--------------------------

*Instruction code*: 0x.6.f 0x**** 0x*****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is left-shifted by :code:`$rB` bits and is assigned to :code:`$rD`. 0-s are shifted in from the right. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is stored in FIELD_E.

.. _rd_eq_value_lsr_rb:

$rD <- VALUE >> $rB
--------------------------

*Instruction code*: 0x.7.f 0x**** 0x*****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is right-shifted by :code:`$rB` bits and is assigned to :code:`$rD`. 0-s are shifted in from the left. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is stored in FIELD_E.

.. _rd_eq_value_asr_rb:

$rD <- VALUE >>> $rB
--------------------------

*Instruction code*: 0x.8.f 0x**** 0x*****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is right-shifted arithmetically by :code:`$rB` bits and is assigned to :code:`$rD`. The MSB of :code:`$rA` is repeatedly shifted in from the left. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is stored in FIELD_E.

.. _rd_eq_value_times_rb:

$rD <- VALUE * $rB
--------------------------

*Instruction code*: 0x.9.f 0x**** 0x*****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the product of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise multiplication is performed. For floating-point types a floating-point multiplication is performed. The binary value of VALUE is stored in FIELD_E.


















