Instruction Set Details
=======================

Type handling
-------------


.. _std_type_handling:

Standard Type Determination and Coalescing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type of the up to two operands of the instruction is examined.

If the two types are not compatible, an :code:`exc_type` exception is raised.

If the instruction has only one operand, the result type is the type of that single operand.

If one of the operands of the instruction is an immediate constant, the result type is that of the non-immediate operand.

.. note:: For smaller than 32-bit immediates, the value is sign-extended to 32-bits before a type is assigned to it. This means that such immediates for floating-point operations are rather meaningless, even though the operation itself will be carried out and is considered valid.

.. _logic_type_handling:

Logic Type Determination and Coalescing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For certain logical operations (and, or, xor etc.) a the following logic is used to determine the operand and destination types.

The type of the up to two operands of the instruction is examined.

If the *logic* types of the two operands are not compatible, an :code:`exc_type` exception is raised.

The result type is determined by the type of :code:`$rA` for operations with two register-operand or by the type of the single register-operand if only one exists.

.. note:: the result type determination is asymmetrical in this case. This allows for :code:`$rD <- $rA & $rB` and :code:`$rD <- $rA & ~$rB` operations to maintain the result type, allowing their use as lane-prediction instructions for vector-extensions.

If one of the two input operands is an immediate constant, its type is assumed to be the 32-bit compatible scalar logic type of the non-immediate operand.

.. note:: For smaller than 32-bit immediates, the value is sign-extended to 32-bits before a type is assigned to it.


.. _shift_type_handling:

Shift Type Determination and Coalescing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For shift operations the following logic is used to determine the operand and destination types.

If either operand is an immediate value for the instruction, it is assumed to be of type INT32 (even for 16-bit immediates, which are first sign-extended to 32-bits).

The operand type of the second operand (the shift amount) is checked to be of a fixed point type. If not, a :code:`exc_type` exception is raised.

The result type is what the shift value type turns out to be after these transformations.

.. note:: Shift variants, where the shift amount is an immediate, the result type will always be INT32.

.. _load_type_handling:

Load Type Determination and Coalescing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For load operations, the destination type is not altered.

For immediate loads (where the value is part of the instruction), the value is first sign-extended to 32-bits, then assigned to the destination register.

For loads from memory, the length of the load is determined by the operation: 32, 16 or 8 bits are loaded.

The offset register must be of logical type :code:`INT32`. If not, a :code:`exc_type` exception is raised.


.. _store_type_handling:

Store Type Handling
-------------------

For store operations, the length of the store is 8- 16- or 32-bits, depending on the operation.

The offset register must be of logical type :code:`INT32`. If not, a :code:`exc_type` exception is raised.




.. _cbranch_type_handling:

Conditional Branch Type Handling
--------------------------------

Conditional branch operations use :ref:`standard type handling<std_type_handling>` to determine source types, but - for obvious reasons - doesn't care about the destination type.



.. include:: isa_detail_ex_mode_chg.rst
.. include:: isa_detail_fence.rst
.. include:: isa_detail_pc_manip.rst
.. include:: isa_detail_misc.rst
.. include:: isa_detail_csr.rst
.. include:: isa_detail_state_regs.rst
.. include:: isa_detail_unary.rst
.. include:: isa_detail_imm.rst
.. include:: isa_detail_cbranch.rst
.. include:: isa_detail_stack.rst
.. include:: isa_detail_pc_ldst.rst
.. include:: isa_detail_ldst.rst
.. include:: isa_detail_binary_alu.rst
.. include:: isa_detail_prefix_groups.rst

Pseudo instructions
-------------------

NOP is equivalent to $r2 <- $r2 | $r2
$rD <- $rS is equivalent to $rD <- $rS | $rS
