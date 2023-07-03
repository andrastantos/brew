
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


$rD <- tiny $rB + CONST
--------------------------

*Instruction code*: 0x.4..

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       b       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the sum of :code:`$rB` and the one-s complemented value of FIELD_A. The range of the constant is between -7 and 7. The type of operation is determined by the type of :code:`$rB`. The type of CONST is assumed to be INT32. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise addition is performed. For floating-point an invalid instruction exception is raised.









$rD <- short VALUE ^ $rB
--------------------------

*Instruction code*: 0x.1f. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'xor'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is the sign-extended value stored in FIELD_E.


$rD <- short VALUE | $rB
--------------------------

*Instruction code*: 0x.2f. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'or'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is the sign-extended value stored in FIELD_E.

$rD <- short VALUE & $rB
--------------------------

*Instruction code*: 0x.3f. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'and'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is the sign-extended value stored in FIELD_E.

$rD <- short VALUE + $rB
--------------------------

*Instruction code*: 0x.4f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the sum of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rB`. For vector types a lane-wise addition is performed. For floating-point types an invalid instruction exception is thrown. The binary value of VALUE is the sign-extended value stored in FIELD_E.

$rD <- short VALUE - $rB
--------------------------

*Instruction code*: 0x.5f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the difference of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rB`. For vector types a lane-wise subtraction is performed. For floating-point types an invalid instruction exception is thrown. The binary value of VALUE is the sign-extended value stored in FIELD_E.


$rD <- $rB << short VALUE
--------------------------

*Instruction code*: 0x.6f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is left-shifted by :code:`$rB` bits and is assigned to :code:`$rD`. 0-s are shifted in from the right. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is the sign-extended value stored in FIELD_E.

$rD <- $rB >> short VALUE
--------------------------

*Instruction code*: 0x.7f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is right-shifted by :code:`$rB` bits and is assigned to :code:`$rD`. 0-s are shifted in from the left. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is the sign-extended value stored in FIELD_E.

$rD <- $rB >>> short VALUE
--------------------------

*Instruction code*: 0x.8f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is right-shifted arithmetically by :code:`$rB` bits and is assigned to :code:`$rD`. The MSB of :code:`$rA` is repeatedly shifted in from the left. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is the sign-extended value stored in FIELD_E.

$rD <- short VALUE * $rB
--------------------------

*Instruction code*: 0x.9f. 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the product of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise multiplication is performed. For floating-point types an invalid instruction exception is thrown. The binary value of VALUE is the sign-extended value stored in FIELD_E.



$rD <- lane_swizzle $rA, VALUE
-----------------------------------

*Instruction code*: 0x.af. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$rD` is set to the lane-swizzled value of :code:`$rB`. Destination type is set to match that of :code:`$rA`. FIELD_E encodes the source byte for each destination byte in the following manner:

=============  ===================
FIELD_E bits   destination byte
=============  ===================
0..1           0
2..3           1
4..5           2
6..8           3
=============  ===================

Each bit-field of FIELD_E denotes a source byte as follows:

=============  ===================
source code    source byte
=============  ===================
0              0
1              1
2              2
3              3
=============  ===================

VALUE is a 4-digit number in assembly, each digit representing a destination byte. The value of each digit ranges from 0 to 3, encoding the corresponding source byte. Any combination is valid. For instance VALUE=0000 would replicate byte 0 into all 4 bytes, VALUE=0123 would swap the bytes around. VALUE=3210 copies source to destination without modification.























$rD <- VALUE ^ $rB
--------------------------

*Instruction code*: 0x.1.f 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'xor'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is stored in FIELD_E.


$rD <- VALUE | $rB
--------------------------

*Instruction code*: 0x.2.f 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'or'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is stored in FIELD_E.

$rD <- VALUE & $rB
--------------------------

*Instruction code*: 0x.3.f 0x**** 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Bit-wise 'and'. Destination type is set to match that of :code:`$rB`. The binary value of VALUE is stored in FIELD_E.

$rD <- VALUE + $rB
--------------------------

*Instruction code*: 0x.4.f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the sum of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rB`. For vector types a lane-wise addition is performed. For floating-point types a floating-point additional is performed. The binary value of VALUE is stored in FIELD_E.

$rD <- VALUE - $rB
--------------------------

*Instruction code*: 0x.5.f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the difference of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rB`. For vector types a lane-wise subtraction is performed. For floating-point types a floating-point subtraction is performed. The binary value of VALUE is stored in FIELD_E.


$rD <- VALUE << $rB
--------------------------

*Instruction code*: 0x.6.f 0x**** 0x*****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is left-shifted by :code:`$rB` bits and is assigned to :code:`$rD`. 0-s are shifted in from the right. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is stored in FIELD_E.

$rD <- VALUE >> $rB
--------------------------

*Instruction code*: 0x.7.f 0x**** 0x*****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is right-shifted by :code:`$rB` bits and is assigned to :code:`$rD`. 0-s are shifted in from the left. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is stored in FIELD_E.

$rD <- VALUE >>> $rB
--------------------------

*Instruction code*: 0x.8.f 0x**** 0x*****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

VALUE is right-shifted arithmetically by :code:`$rB` bits and is assigned to :code:`$rD`. The MSB of :code:`$rA` is repeatedly shifted in from the left. The type of VALUE and the operation is determined by the type of :code:`$rB` (even though for the operation, :code:`$rB` is treaded as an INT32). Destination type is set to match that of :code:`$rB`. For vector types a lane-wise shift is performed. For scalar types, a binary shift is performed. The binary value of VALUE is stored in FIELD_E.

$rD <- VALUE * $rB
--------------------------

*Instruction code*: 0x.9.f 0x**** 0x*****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

:code:`$rD` is set to the product of VALUE and :code:`$rB`. The type of operation is determined by the type of :code:`$rB`. The type of VALUE is assumed to be the same as :code:`$rB`. Destination type is set to match that of :code:`$rA`. For vector types a lane-wise multiplication is performed. For floating-point types a floating-point multiplication is performed. The binary value of VALUE is stored in FIELD_E.


















