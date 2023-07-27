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

This operation uses :ref:`load type handling<load_type_handling>` to determine if :ref:`broadcasting<type_broadcast>` needs to be performed. The immediate CONST is stored in FIELD_A. Constant value is one-s complement of FIELD_A. Range is -7 to +7, which is sign-extended to 32 bits.




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




.. _rd_eq_float_ra:

$rD <- float $rA
--------------------------

*Instruction code*: 0x.07.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       7       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types *with modifications*; the output type is set according to the following table:

================   ===============
Input logic type   Output type
================   ===============
INT32              FP32
VINT32             VFP32
VINT16             VFP16
VINT8              type error: :code:`exc_type` is raised
================   ===============

After the types are determined, each lane is converted to a corresponding floating point number. If the input is already of a floating point type, the values are simply assigned to the output without alteration.

.. _rd_eq_int_ra:

$rD <- int $rA
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


The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types *with modifications*; the output type is set according to the following table:

================   ===============
Input logic type   Output type
================   ===============
INT32              INT32
VINT32             VINT32
VINT16             VINT16
VINT8              VINT8
================   ===============

After the types are determined, each lane is converted to a corresponding integer number. If the input is already of a fixed point type, the values are simply assigned to the output without alteration.

.. todo:: What to do in case of an overflow? Set an FP sticky-bit?


.. _rd_eq_1_/_ra:

$rD <- 1 / $rA
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

After the types are determined, the reciprocal of each lane is computed. If a zero value is encountered in an element, the corresponding result is set to :code:`NaN`. The :code:`fdv` status bit in the :ref:`:code:`FPSTAT` <csr_fpstat>` CSR register is set.


.. _rd_eq_rsqrt_ra:

$rD <- rsqrt $rA
--------------------------

*Instruction code*: 0x.0a.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       a       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

The operation uses :ref:`standard type handling<std_type_handling>` to determine operand and destination types. If the input type is not of a floating point type, an :code:`exc_type` exception is raised.

After the types are determined, the reciprocal-square-root of each lane is computed. If a non-positive value is encountered in an element, the corresponding result is set to :code:`NaN`. The :code:`fnv` status bit in the :ref:`:code:`FPSTAT` <csr_fpstat>` CSR register is set.




.. _rd_eq_size_ra:

$rD <- size $rA
--------------------------

*Instruction code*: 0x.0b.

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       b       |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Load the run-time size (in bytes) of $rA into $rD.

.. todo:: This used to be the reduction sum. No toolset support at the moment.

.. todo:: We don't specify the 'size' field anymore. This instruction is free for something else now!!!

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

Sets type of $rD as denoted by $rA. All 32 bits of :code:`$rA` are meaningful in this instruction. If an unsupported type is specified, a :code:`exc_type` exception is raised. The value stored in the register is adjusted per :ref:`register_value_and_type_updates` rules.

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

Sets type of $rD as denoted by FIELD_A. If an unsupported type is specified, a :code:`exc_type` exception is raised. The value stored in the register is adjusted per :ref:`register_value_and_type_updates` rules.

.. todo:: assembly should use descriptive type names, instead of numeric values.

