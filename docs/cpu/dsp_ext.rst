DSP extension
=============

The DSP extension adds several custom types and a few instructions. There are two main goals of this extension:

#. To add support for saturated vector-math
#. To add support for fixed-point fractional math

New types
----------------

==========    =========  ============== ==================== ========================== ============ ==========
Type code     Type name  Scalar/Vector  Fixed/Floating point Compatible type            Logic type   Note
==========    =========  ============== ==================== ========================== ============ ==========
0x9           VUINT32S   Vector         Fixed                INT32                      VINT32       Unsigned, saturated version on VINT32
0xa           VSINT32S   Vector         Fixed                INT32                      VINT32       Signed, saturated version on VINT32
0xb           VUINT16S   Vector         Fixed                INT32                      VINT16       Unsigned, saturated version on VINT16
0xc           VSINT16S   Vector         Fixed                INT32                      VINT16       Signed, saturated version on VINT16
0xd           VUINT8S    Vector         Fixed                INT32                      VINT8        Unsigned, saturated version on VINT8
0xe           VSINT8S    Vector         Fixed                INT32                      VINT8        Signed, saturated version on VINT8
==========    =========  ============== ==================== ========================== ============ ==========

New Instructions
----------------

Scaled multiply group
~~~~~~~~~~~~~~~~~~~~~


.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "f",         "bits": 4 },
      { "name": "FLD_F",     "bits": 2, attr: "shift" },
      { "name": "FLD_O",     "bits": 2, attr: "op kind" },
      { "name": "f",         "bits": 4 },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "$rA" },
      { "name": "FIELD_B",   "bits": 4, attr: "$rB" },
      { "name": "FIELD_C",   "bits": 4, attr: "shift" },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |       f       |  OP   | FLD_F |       f       |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

========================================================== ========================================== ==================
Instruction code                                           Assembly                                   Operation
========================================================== ========================================== ==================
:ref:`0xf4ff 0x.*..<rd_eq_full_ra_times_rb_asr_value>`     $rD <- full $rA * $rB >>> FIELD_C + 0
:ref:`0xf5ff 0x.*..<rd_eq_full_ra_times_rb_asr_value>`     $rD <- full $rA * $rB >>> FIELD_C + 8
:ref:`0xf6ff 0x.*..<rd_eq_full_ra_times_rb_asr_value>`     $rD <- full $rA * $rB >>> FIELD_C + 16
:ref:`0xf7ff 0x.*..<rd_eq_full_ra_times_rb_asr_value>`     $rD <- full $rA * $rB >>> FIELD_C + 32
:ref:`0xf8ff 0x.*..<rd_eq_full_ra_times_rb_lsr_value>`     $rD <- full $rA * $rB >> FIELD_C + 0
:ref:`0xf9ff 0x.*..<rd_eq_full_ra_times_rb_lsr_value>`     $rD <- full $rA * $rB >> FIELD_C + 8
:ref:`0xfaff 0x.*..<rd_eq_full_ra_times_rb_lsr_value>`     $rD <- full $rA * $rB >> FIELD_C + 16
:ref:`0xfbff 0x.*..<rd_eq_full_ra_times_rb_lsr_value>`     $rD <- full $rA * $rB >> FIELD_C + 32
========================================================== ========================================== ==================

.. todo::
  This is not how BINUTILS is coded up at the moment. We need to follow-up with the changes there.

.. todo:: Extension group encoding changed. Toolset needs updating.

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

Updates to instruction behavior
-------------------------------

Type broadcast
~~~~~~~~~~~~~~

Type broadcast behavior is extended for the new types as follows:

For broadcasting INT32 to lanes of VUINT16S or VUINT8S the top-most 16 or 24 bits respectively are checked for all 0-s. If they are not, the value of all 1-s are broadcast. Otherwise, the bottom 16 or 8 bits are respectively replicated into each lane.
For broadcasting INT32 to lanes of VSINT16S or VSINT8S the top-most 16 or 24 bits respectively are checked for all 0-s or all 1-s. If they are not, the MSB bit of the source is copied into the MSB of all lanes, and the inverse of the MSB bit of the source is copied into all other bits of all lanes.

