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

*Type variants*: Yes

Description
~~~~~~~~~~~

The immediate CONST is stored in FIELD_A. Constant value is one-s complement of FIELD_A. Range is -7 to +7, which is sign-extended to 32 bits.




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

*Type variants*: Yes

Description
~~~~~~~~~~~

Load $rD with $pc + constant stored in FIELD_A. Constant value is twice the one-s complement of FIELD_A. Range is -14 to +14, which is sign-extended to 32 bits. Useful for call return address calculation. The destination type is set to INT32.





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

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. On the resulting type either an integer or a float-point lane-wise negation is performed.





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

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting type a bit-wise inversion is performed.

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

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting type a lane-wise sign-extension from 8 bits to the lane width is performed. If the lane-width is less then or equal to 8 bits, the operation simply assigns the input to the output.





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

The operation uses :ref:`logic type handling<logic_type_handling>` to determine operand and destination types. On the resulting type a lane-wise sign-extension from 16 bits to the lane width is performed. If the lane-width is less then or equal to 16 bits, the operation simply assigns the input to the output.



.. _rd_eq_popcnt_ra:

$rD <- popcnt $rA
--------------------------

*Instruction code*: 0x.07.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       7       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Write the number of bits set in $rA into $rD.

.. todo:: This is a new instruction. No toolset support at the moment.




.. _rd_eq_1_/_ra:

$rD <- 1 / $rA
--------------------------

*Instruction code*: 0x.08.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       8       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. If the input type is not of a floating point type, an :code:`exc_type` exception is raised.

After the types are determined, the reciprocal of each lane is computed. If a zero value is encountered in an element, the corresponding result is set to :code:`NaN`. The :code:`fdv` status bit in the :ref:`:code:`fpstat`<fpstat>` CSR register is set.

.. todo:: Instruction code changed. Needs toolset update.

.. _rd_eq_rsqrt_ra:

$rD <- rsqrt $rA
--------------------------

*Instruction code*: 0x.09.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       9       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. If the input type is not of a floating point type, an :code:`exc_type` exception is raised.

After the types are determined, the reciprocal-square-root of each lane is computed. If a non-positive value is encountered in an element, the corresponding result is set to :code:`NaN`. The :code:`fnv` status bit in the :ref:`:code:`fpstat`<fpstat>` CSR register is set.

.. todo:: Instruction code changed. Needs toolset update.



.. _type_rd_eq_ra:

type $rD <- $rA
--------------------------

*Instruction code*: 0x.0c.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       c       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

Sets type of $rD as denoted by $rA. All 32 bits of :code:`$rA` are meaningful in this instruction. If an unsupported type is specified, a :code:`exc_type` exception is raised.

.. _rd_eq_type_ra:

$rD <- type $rA
--------------------------

*Instruction code*: 0x.0d.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       d       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Loads type value of $rA into $rD. The type of $rA is set to INT32.



.. _type_rd_eq_field_a:

type $rD <- FIELD_A
--------------------------

*Instruction code*: 0x.0e.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       e       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

Sets type of $rD as denoted by FIELD_A. If an unsupported type is specified, a :code:`exc_type` exception is raised.

.. todo:: assembly should use descriptive type names, instead of numeric values.

