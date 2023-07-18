.. _rd_eq_tiny_const:

$rD <- tiny CONST
--------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "CONST" },
      { "name": "1",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}

*Instruction code*: 0x.01.

*Exceptions*: None

Description
~~~~~~~~~~~

Load $rD with constant stored in FIELD_A. Constant value is one-s complement of FIELD_A. Range is -7 to +7, which is sign-extended to 32 bits. The destination type is not altered.

::
    $rD.value := CONST
    $rD.type := $rD.type
    $rD.size := 4
    $rD.dirty := true




.. _rd_eq_pc_plus_const:

$rD <- $pc + CONST
--------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "CONST" },
      { "name": "2",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}

*Instruction code*: 0x.02.

*Exceptions*: None

Description
~~~~~~~~~~~

Load $rD with $pc + constant stored in FIELD_A. Constant value is twice the one-s complement of FIELD_A. Range is -14 to +14, which is sign-extended to 32 bits. Useful for call return address calculation. The destination type is set to INT32.

::
    $rD.value := $pc + CONST
    $rD.type := INT32
    $rD.size := 4
    $rD.dirty := true




.. _rd_eq_minus_ra:

$rD <- -$rA
--------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "3",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}

*Instruction code*: 0x.03.

*Exceptions*: None

Description
~~~~~~~~~~~

Negation operation. The actual negation depends on the type. For vector types lane-wise negation is used. For floating point types, floating-point negation is used. For integer types, 2-s complement negation is used. The destination type is set to that of :code:`$rA`.

::
    $rD.value(i) := -$A.value(i) for i in lane_range(vstart, vend)
    $rD.type := $rA.type
    $rD.size := vend
    $rD.dirty := true





.. _rd_eq_notra:

$rD <- ~$rA
--------------------------

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "4",         "bits": 4 },
      { "name": "0",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ]}

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

::
    $rD.value := ~$A.value
    $rD.type := $rA.type
    $rD.size := $rA.size
    $rD.dirty := true

.. note:: binary inversion is the same operation independent of type.

.. _rd_eq_bse_ra:

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



.. _rd_eq_wse_ra:

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



.. _rd_eq_float_ra:

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


.. _rd_eq_int_ra:

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

.. _rd_eq_1_/_ra:

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


.. _rd_eq_rsqrt_ra:

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


.. _rd_eq_size_ra:

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


.. _type_rd_eq_ra:

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


.. _rd_eq_type_ra:

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



.. _type_rd_eq_field_a:

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

