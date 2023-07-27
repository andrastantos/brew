Instruction Set Details
=======================

Type handling
-------------


.. _type_broadcast:

Type broadcast
~~~~~~~~~~~~~~

A scalar type can be broadcast (repeated) to all lanes of a vector type. During this operation, the size of the element can change.

Broadcasting normally happens when an operation is performed with one scalar and one vector operation. The goal is to produce a vector out of the scalar operand, one where the lane-width matches that of the vector operand.

For broadcasting INT32 to lanes of VINT16 or VINT8, the top-most 16 or 24 bits are truncated respectively.
For broadcasting INT32 to lanes of VUINT16S or VUINT8S the top-most 16 or 24 bits respectively are checked for all 0-s. If they are not, the value of all 1-s are broadcast. Otherwise, the bottom 16 or 8 bits are respectively replicated into each lane.
For broadcasting INT32 to lanes of VSINT16S or VSINT8S the top-most 16 or 24 bits respectively are checked for all 0-s or all 1-s. If they are not, the MSB bit of the source is copied into the MSB of all lanes, and the inverse of the MSB bit of the source is copied into all other bits of all lanes.

For broadcasting FP32 into VFP16, the bottom 3 bits of the exponent and the bottom 13 bits of the mantissa are truncated. The resulting FP16 number is then copied to all lanes.

.. _std_type_handling:

Standard Type Determination and Coalescing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type of the up to two operands of the instruction is examined.

If the two types are not compatible, an :code:`exc_type` exception is raised.

Of the two compatible types, if either is a vector type, the result type is going to be the same vector type. If both are scalar, the result type is going to be that type. (Compatibility requires that if both operands are scalar or vector, they must have the same type.)

If the instruction has only one operand, the result type is the type of that single operand.

If one of the operands of the instruction is an immediate constant, the result type is that of the non-immediate operand.

If one of the two input operands is of a scalar type, while the other is of a vector type, the scalar one is :ref:`broadcast<type_broadcast>` to the vector type of the other operand.

If one of the two input operands is an immediate constant, its type is assumed to be the compatible scalar type of the non-immediate operand.

.. note:: For 16-bit immediates, the immediate value is sign-extended to 32-bits before a type is assigned to it. This means that 16-bit immediates for floating-point operations are rather meaningless, even though the operation itself will be carried out and is considered valid.

.. _logic_type_handling:

Logic Type Determination and Coalescing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For certain logical operations (and, or, xor etc.) a the following logic is used to determine the operand and destination types.

The type of the up to two operands of the instruction is examined.

If the *logic* types of the two operands are not compatible, an :code:`exc_type` exception is raised.

The result type is determined by the type of $rA for operations with two register-operand or by the type of the single register-operand if only one exists.

.. note:: the result type determination is asymmetrical in this case. This allows for lane-prediction operations (:code:`$rD <- $rA & $rB`; :code:`$rD <- $rA & ~$rB`) to maintain the result type, however they create some unintuitive behavior when the type of $rA is scalar and $rB is vector. In those cases $rA gets broadcast and executed as vector operation, but then all but the first 32 bit of the result gets discarded.

If one of the two input operands is of a scalar type, while the other is of a vector type, the scalar one is :ref:`broadcast<type_broadcast>` to the logic type of the other operand.

If one of the two input operands is an immediate constant, its type is assumed to be the compatible scalar logic type of the non-immediate operand.


.. _shift_type_handling:

Shift Type Determination and Coalescing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For shift operations the following logic is used to determine the operand and destination types.

If either operand is an immediate value for the instruction, it is assumed to be of type INT32 (even for 16-bit immediates, which are first sign-extended to 32-bits).

The operand type of the second operand (the shift amount) is checked to be of a fixed point type. If not, a :code:`exc_type` exception is raised.

If both the shift value and the shift amount are of a scalar type, a scalar shift is performed.

If both the shift value and the shift amount is of a vector type, their lane width is checked to be the same. If not, a :code:`exc_type` exception is raised.

.. note:: The logic in checking the lane width is that vectors of identical lane width have the same number of lanes, which allows for unambiguous lane assignment and simplifies shift amount muxing.

If the shift value is of a scalar type, while the shift amount is a vector type, the shift value is :ref:`broadcast<type_broadcast>` to the logic type of the other operand.

If the shift amount is of a vector type, while the shift amount is a scalar type, the shift amount is :ref:`broadcast<type_broadcast>` to the logic type of the other operand.

The result type is what the shift value type turns out to be after these transformations.


.. _load_type_handling:

Load Type Determination and Coalescing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For load operations, the destination type is not altered. Instead, the type of the destination is used to control the way the load is performed.

For immediate loads (where the value is part of the instruction), the value is first sign-extended to 32-bits, then :ref:`broadcast<type_broadcast>` to the destination type and finally assigned to the destination register. :code:`VEND` controls the lanes affected by the load. An implementation can optimize operation by skipping lanes below :code:`VSTART`, however this behavior is not mandatory. If these lanes are skipped, their value will be unaffected.

For loads from memory, the length of the load of scalar types is determined by the operation: 32, 16 or 8 bits are loaded.

For vector types, 8- and 16-bit loads as well as MEMLL operations raise an :code:`exc_type` exception. Other load operations use :code:`VSTART` and :code:`VEND` to determine the lanes to be loaded. The logical address of the load is provided in the instruction corresponds to the first byte of the vector register, even if :code:`VSTART` is non-0. An implementation can choose to optimize loads by skipping bytes between 0 and :code:`VSTART`, though this behavior is not mandatory. If implemented, the value of skipped bytes is not altered.

For 'full' loads of vector types, the value of :code:`VEND` is ignored, and treaded as if it was set to :code:`VLEN`.

An implementation is not required to guarantee in-order load of bytes within the vector register.

If - during the operation - an implementation-defined access violation exception occurs, the :code:`VSTART` register will be set to the first byte that was not completely loaded. This behavior is required by all implementations, even those that ignore :code:`VSTART` during loads.

.. todo:: this is weird. This means that vector types really only support MEM[]-style loads and stores. all the other variants make only sense for scalars. Are we sure we're using the instruction space efficiently?

.. _store_type_handling:

Store Type Handling
-------------------

For store operations, the length of the store is determined by many factors.

If the value to be stored is of a scalar type, an 8- 16- or 32-bit store is performed, depending on the operation.

For vector types, 8- and 16-bit stores as well as MEMSC operations raise an :code:`exc_type` exception. Other store operations use :code:`VSTART` and :code:`VEND` to determine the lanes to be stored. The logical address of the store is provided in the instruction corresponds to the first byte of the vector register, even if :code:`VSTART` is non-0. An implementation can choose to optimize stores by skipping bytes between 0 and :code:`VSTART`, though this behavior is not mandatory.

For 'full' stores of vector types, the value of :code:`VEND` is ignored, and treaded as if it was set to :code:`VLEN`.

An implementation is not required to guarantee in-order store of bytes within the vector register.

If - during the operation - an implementation-defined access violation exception occurs, the :code:`VSTART` register will be set to the first byte that was not completely stored. This behavior is required by all implementations, even those that ignore :code:`VSTART` during stores.



.. _cbranch_type_handling:

Conditional Branch Type Handling
--------------------------------

Conditional branch operations use :ref:`standard type handling<std_type_handling>` to determine source types, but - for obvious reasons - doesn't care about the destination type.

.. _predication_type_handling:

Predication Type Handling
--------------------------------

Lane predication operations use :ref:`standard type handling<std_type_handling>` to determine source types, but the result type is set to the logic type of what the normal output type would be.



These rules make sure that now content of a register can be unmasked by type and/or value updates.

.. include:: isa_detail_ex_mode_chg.rst
.. include:: isa_detail_fence.rst
.. include:: isa_detail_pc_manip.rst
.. include:: isa_detail_csr.rst
.. include:: isa_detail_state_regs.rst
.. include:: isa_detail_unary.rst
.. include:: isa_detail_imm.rst
.. include:: isa_detail_cbranch.rst
.. include:: isa_detail_stack.rst
.. include:: isa_detail_pc_ldst.rst
.. include:: isa_detail_ldst.rst
.. include:: isa_detail_binary_alu.rst
.. include:: isa_detail_ext_groups.rst
.. include:: isa_detail_prefix_groups.rst
.. include:: isa_detail_ld_st_multiple.rst

Pseudo instructions
-------------------

NOP is equivalent to $r2 <- $r2 | $r2
$rD <- $rS is equivalent to $rD <- $rS | $rS
