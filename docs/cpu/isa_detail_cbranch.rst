

.. _if_any_ra_eq_0__pc_eq_pc_plus_value:

if any $rA == 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf00. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?



.. _if_any_ra_ne_0__pc_eq_pc_plus_value:

if any $rA != 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf01. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_any_ra_lt_0___pc_eq_pc_plus_value:

if any $rA < 0   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf02. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_any_ra_ge_0__pc_eq_pc_plus_value:

if any $rA >= 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf03. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_any_ra_gt_0___pc_eq_pc_plus_value:

if any $rA > 0   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf04. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_any_ra_le_0__pc_eq_pc_plus_value:

if any $rA <= 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf05. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_ra_eq_0__pc_eq_pc_plus_value:

if all $rA == 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf08. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_ra_ne_0__pc_eq_pc_plus_value:

if all $rA != 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf09. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_ra_lt_0___pc_eq_pc_plus_value:

if all $rA < 0   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf0a. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_ra_ge_0__pc_eq_pc_plus_value:

if all $rA >= 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf0b. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_ra_gt_0___pc_eq_pc_plus_value:

if all $rA > 0   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf0c. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_ra_le_0__pc_eq_pc_plus_value:

if all $rA <= 0  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf0d. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_any_rb_eq_ra___pc_eq_pc_plus_value:

if any $rB == $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf1.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_any_rb_ne_ra___pc_eq_pc_plus_value:

if any $rB != $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf2.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_any_signed_rb_lt_ra__pc_eq_pc_plus_value:

if any signed $rB < $rA  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf3.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, an signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_any_signed_rb_ge_ra_pc_eq_pc_plus_value:

if any signed $rB >= $rA $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf4.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_any_rb_lt_ra____pc_eq_pc_plus_value:

if any $rB < $rA    $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf5.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a unsigned comparison is performed, even for VSINT16S and VSINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_any_rb_ge_ra___pc_eq_pc_plus_value:

if any $rB >= $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf6.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a unsigned comparison is performed, even for VSINT16S and VSINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_rb_eq_ra___pc_eq_pc_plus_value:

if all $rB == $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xf9.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_rb_ne_ra___pc_eq_pc_plus_value:

if all $rB != $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xfa.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_signed_rb_lt_ra__pc_eq_pc_plus_value:

if all signed $rB < $rA  $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xfb.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_signed_rb_ge_ra_pc_eq_pc_plus_value:

if all signed $rB >= $rA $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xfc.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a signed comparison is performed, even for VUINT16S and VUINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_rb_lt_ra____pc_eq_pc_plus_value:

if all $rB < $rA    $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xfd.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a unsigned comparison is performed, even for VSINT16S and VSINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?


.. _if_all_rb_ge_ra___pc_eq_pc_plus_value:

if all $rB >= $rA   $pc <- $pc + VALUE
----------------------------------------------------------

*Instruction code*: 0xfe.. 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: Yes

Description
~~~~~~~~~~~

This operation uses :ref:`conditional branch type handling<cbranch_type_handling>` to determine the source operand types. It then evaluates the condition: for vector types, the condition for each lane up to value based on :code:`VEND` is evaluated and then aggregated. For scalar types, a single comparison is performed. For fixed point types, a unsigned comparison is performed, even for VSINT16S and VSINT8S types.

.. note:: The value of :code:`VSTART` is ignored during the comparison. If the instruction is retried, all lanes are compared again.

If the condition evaluates true, the branch is taken. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65534, in increments of 2 bytes.

.. todo:: How to handle NaN and INF values? Should we have special branches for those?

















.. _if_ra_c__eq_1_pc_eq_pc_plus_value:

if $rA[C]  == 1 $pc <- $pc + VALUE
---------------------------------------------

*Instruction code*: 0xf.f. 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If bit-position C of :code:`$rA` is set, the instruction flow is branched. The comparison is type-independent. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The value of 'C' is coded in FIELD_C in using the following table:

======= ===============
C       FIELD_C
======= ===============
0       0
1       1
2       2
3       3
4       4
5       5
6       6
7       7
8       8
9       9
a       14
b       15
c       16
d       30
e       31
======= ===============




.. _if_rb_c__eq_0_pc_eq_pc_plus_value:

if $rB[C]  == 0 $pc <- $pc + VALUE
---------------------------------------------

*Instruction code*: 0xf..f 0x****

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~
Conditional branch operation. If bit-position C of :code:`$rB` is not set, the instruction flow is branched. The comparison is type-independent. The value of FIELD_E is computed as follows::

  FIELD_E = (VALUE & 0xfffe) | ((VALUE >> 31) & 1)

In other words, the MSB of VALUE is copied to the LSB of FIELD_E, then the value is truncated to 16 bits. The relative branch target thus can be between -65536 and 65535, in increments of 2 bytes.

The value of 'C' is coded in FIELD_C in using the following table:

======= ===============
C       FIELD_C
======= ===============
0       0
1       1
2       2
3       3
4       4
5       5
6       6
7       7
8       8
9       9
a       14
b       15
c       16
d       30
e       31
======= ===============





.. _if_any_type_r0...r3___ne_types_pc_eq_pc_plus_br_offs:

if any type $r0...$r3   != types $pc <- $pc + br_offs
-------------------------------------------------------

*Instruction code*: 0x001f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r0        TYPE_A
$r1        TYPE_B
$r2        TYPE_C
$r3        TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.



.. _if_all_type_r0...r3___ne_types_pc_eq_pc_plus_br_offs:

if all type $r0...$r3   != types $pc <- $pc + br_offs
-------------------------------------------------------

*Instruction code*: 0x002f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r0        TYPE_A
$r1        TYPE_B
$r2        TYPE_C
$r3        TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_type_rd_not_in_field_f_pc_eq_pc_plus_field_e:

if type $rD not in FIELD_F $pc <- $pc + FIELD_E
-----------------------------------------------

*Instruction code*: 0x.03f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "bits": 1 },
      { "name": "TYPE_MASK", "bits": 15, attr: "types mask" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |   |                           TYPE MASK                       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction compares the type of $rD to the mask provided. If the bit corresponding to the type in the type mask is set to 0, the instruction branches.

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type.



.. _if_any_type_r4...r7___ne_types_pc_eq_pc_plus_br_offs:

if any type $r4...$r7   != types $pc <- $pc + br_offs
-----------------------------------------------------

*Instruction code*: 0x101f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r4        TYPE_A
$r5        TYPE_B
$r6        TYPE_C
$r7        TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_all_type_r4...r7___ne_types_pc_eq_pc_plus_br_offs:

if all type $r4...$r7   != types $pc <- $pc + br_offs
-----------------------------------------------------

*Instruction code*: 0x102f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r4        TYPE_A
$r5        TYPE_B
$r6        TYPE_C
$r7        TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_any_type_r8...r11__ne_types_pc_eq_pc_plus_br_offs:

if any type $r8...$r11  != types $pc <- $pc + br_offs
-----------------------------------------------------

*Instruction code*: 0x201f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r8        TYPE_A
$r9        TYPE_B
$r10       TYPE_C
$r11       TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_all_type_r8...r11__ne_types_pc_eq_pc_plus_br_offs:

if all type $r8...$r11  != types $pc <- $pc + br_offs
-----------------------------------------------------

*Instruction code*: 0x202f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r8        TYPE_A
$r9        TYPE_B
$r10       TYPE_C
$r11       TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_any_type_r12...r14_ne_types_pc_eq_pc_plus_br_offs:

if any type $r12...$r14 != types $pc <- $pc + br_offs
-----------------------------------------------------

*Instruction code*: 0x301f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "ignored" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r12        TYPE_A
$r13        TYPE_B
$r14        TYPE_C
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_all_type_r12...r14_ne_types_pc_eq_pc_plus_br_offs:

if all type $r12...$r14 != types $pc <- $pc + br_offs
-----------------------------------------------------

*Instruction code*: 0x302f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "ignored" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r12        TYPE_A
$r13        TYPE_B
$r14        TYPE_C
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.

.. _if_any_type_r0...r3_eq_types_pc_eq_pc_plus_br_offs:

if any type $r0...$r3 == types $pc <- $pc + br_offs
----------------------------------------------------

*Instruction code*: 0x401f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r0        TYPE_A
$r1        TYPE_B
$r2        TYPE_C
$r3        TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_all_type_r0...r3_eq_types_pc_eq_pc_plus_br_offs:

if all type $r0...$r3 == types $pc <- $pc + br_offs
----------------------------------------------------

*Instruction code*: 0x402f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r0        TYPE_A
$r1        TYPE_B
$r2        TYPE_C
$r3        TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.



.. _if_any_type_r4...r7_eq_types_pc_eq_pc_plus_br_offs:

if any type $r4...$r7 == types $pc <- $pc + br_offs
-------------------------------------------------------

*Instruction code*: 0x501f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r4        TYPE_A
$r5        TYPE_B
$r6        TYPE_C
$r7        TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_all_type_r4...r7_eq_types_pc_eq_pc_plus_br_offs:

if all type $r4...$r7 == types $pc <- $pc + br_offs
-------------------------------------------------------


*Instruction code*: 0x502f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r4        TYPE_A
$r5        TYPE_B
$r6        TYPE_C
$r7        TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_any_type_r8...r11__eq_types_pc_eq_pc_plus_br_offs:

if any type $r8...$r11  == types $pc <- $pc + br_offs
-------------------------------------------------------


*Instruction code*: 0x601f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r8        TYPE_A
$r9        TYPE_B
$r10       TYPE_C
$r11       TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_all_type_r8...r11__eq_types_pc_eq_pc_plus_br_offs:

if all type $r8...$r11  == types $pc <- $pc + br_offs
-------------------------------------------------------

*Instruction code*: 0x602f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "types D" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r8        TYPE_A
$r9        TYPE_B
$r10       TYPE_C
$r11       TYPE_D
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_any_type_r12...r14_eq_types_pc_eq_pc_plus_br_offs:

if any type $r12...$r14 == types $pc <- $pc + br_offs
-------------------------------------------------------

*Instruction code*: 0x701f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "ignored" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r12        TYPE_A
$r13        TYPE_B
$r14        TYPE_C
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.


.. _if_all_type_r12...r14_eq_types_pc_eq_pc_plus_br_offs:

if all type $r12...$r14 == types $pc <- $pc + br_offs
-------------------------------------------------------

*Instruction code*: 0x702f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_B ",  "bits": 4, attr: "op kind" },
      { "name": "0"          "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "op kind" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16, attr: "br_offs" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "TYPE_A", "bits": 4, attr: "types A" },
      { "name": "TYPE_B", "bits": 4, attr: "types B" },
      { "name": "TYPE_C", "bits": 4, attr: "types C" },
      { "name": "TYPE_D", "bits": 4, attr: "ignored" },
  ]
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |    FIELD_B    |       f       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                            FIELD_E                            |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |     TYPE_D    |     TYPE_C    |     TYPE_B    |     TYPE_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

Description
~~~~~~~~~~~

Conditional branch operation. The instruction performs a set of comparisons between the expected and actual types and branches if the conditions prescribed in the instructions are met. A register can be excluded from the test by setting their corresponding type field to 0xf.

Registers are assigned to their type fields as follows:

========== ===============
Register   Type field
========== ===============
$r12        TYPE_A
$r13        TYPE_B
$r14        TYPE_C
========== ===============

This instruction can be used in function prologs to check that operands passed in registers are indeed of the expected type. Variants are provided for both checking for allowed types or disallowed ones.
