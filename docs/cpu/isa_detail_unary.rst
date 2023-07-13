$rD <- tiny CONST
--------------------------

*Instruction code*: 0x.01.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       1       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Load $rD with constant stored in FIELD_A. Constant value is one-s complement of FIELD_A. Range is -7 to +7. The destination type is not altered.


$rD <- $pc + CONST
--------------------------

*Instruction code*: 0x.02.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       2       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Load $rD with $pc + constant stored in FIELD_A. Constant value is twice the one-s complement of FIELD_A. Range is -14 to +14. Useful for call return address calculation. The destination type is set to INT32.



$rD <- -$rA
--------------------------

*Instruction code*: 0x.03.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       3       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Negative operation. The actual negation depends on the type. For vector types lane-wise negation is used. For floating point types, floating-point negation is used. The destination type is set to that of :code:`$rA`.

$rD <- ~$rA
--------------------------

*Instruction code*: 0x.04.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       4       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Binary inversion. Destination type is set to that of :code:`$rA`.



$rD <- bse $rA
--------------------------

*Instruction code*: 0x.05.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       5       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Sign-extend from byte. For vector types, operation is per-lane. Floating point types are treated as integer. Destination type is set to that of :code:`$rA`



$rD <- wse $rA
--------------------------

*Instruction code*: 0x.06.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       6       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Sign-extend from word. For vector types, this operation is per-lane. Floating point types are treated as integer. Destination type is set to that of :code:`$rA`



$rD <- float $rA
--------------------------

*Instruction code*: 0x.07.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       7       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Convert to float. No-op if $rA is already a float


$rD <- int $rA
--------------------------

*Instruction code*: 0x.08.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       8       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Convert to integer. No-op if $rA is already integer

$rD <- 1 / $rA
--------------------------

*Instruction code*: 0x.09.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       9       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: SII

*Type variants*: Yes

Description
~~~~~~~~~~~

Reciprocal if :code:`$rA` is of a float type. Otherwise, an invalid instruction exception is thrown


$rD <- rsqrt $rA
--------------------------

*Instruction code*: 0x.0a.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       a       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Reciprocal square-root if :code:`$rA` is of a float type. Otherwise, an invalid instruction exception is thrown.


$rD <- size $rA
--------------------------

*Instruction code*: 0x.0b.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       b       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Reduction sum if :code:`$rA` is of a vector type. Otherwise and invalid instruction exception is thrown.

.. todo:: Detail result type.

.. todo:: This used to be the reduction sum. No toolset support at the moment.


type $rD <- $rA
--------------------------

*Instruction code*: 0x.0c.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       c       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Sets type of $rD as denoted by $rA. All 32 bits of :code:`$rA` are meaningful in this instruction. If an unsupported type is used, an invalid instruction exception is thrown. This instruction doesn't change the bit-pattern stored in :code:`$rD`.


$rD <- type $rA
--------------------------

*Instruction code*: 0x.0d.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       d       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Loads type value of $rA into $rD



type $rD <- FIELD_A
--------------------------

*Instruction code*: 0x.0e.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       e       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Sets type of $rD as denoted by FIELD_A.

.. todo:: assembly should use descriptive type names, instead of numeric values.

