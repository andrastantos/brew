.. _rd_eq_value:

$rD <- VALUE
--------------------------

*Instruction code*: 0x.00f 0x**** 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Loads :code:`$rD` with the value of FIELD_E. The type of :code:`$rD` is not changed. The type of FIELD_E is determined using :ref:`standard type handling<std_type_handling>`.


.. _pc_eq_value:

$pc <- VALUE
--------------------------

*Instruction code*: 0x20ef 0x**** 0x****

*Exceptions*: Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$pc` with the value of FIELD_E. This is an absolute jump operation. The LSB of :code:`$rD` carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

.. note:: A branch operation can obviously cause exceptions, if the branch target is invalid as defined by the memory protection scheme of the implementation. However, in those cases, the exception is raised during the fetch attempt of the target instruction, not during the execution of the branch operation.

.. _tpc_eq_value:

$tpc <- VALUE
--------------------------

*Instruction code*: 0x30ef 0x**** 0x****

*Exceptions*: Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$tpc` with the value of FIELD_E. This is an absolute jump operation in TASK mode. The LSB of :code:`$rD` carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

.. note:: A branch operation can obviously cause exceptions, if the branch target is invalid as defined by the memory protection scheme of the implementation. However, in those cases, the exception is raised during the fetch attempt of the target instruction, not during the execution of the branch operation.


.. _type_r0...r7_eq_value:

type $r0...$r7 <- VALUE
--------------------------

*Instruction code*: 0x80ef 0x**** 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: No

Description
~~~~~~~~~~~
Loads the type of :code:`$r0` through :code:`$r7` with the types encoded in each 4-bit niggle of FIELD_E. The lowest 4 bits determine the type of :code:`$r0`. The highest 4 bits determine the type of :code:`$r7`. If a nibble has a value of 0xf, the type of the corresponding register is not changed.

If an unsupported (or reserved) type is specified, a :code:`exc_type` exception is raised.

.. _type_r8...r14_eq_value:

type $r8...$r14 <- VALUE
--------------------------

*Instruction code*: 0x90ef 0x**** 0x****

*Exceptions*: :code:`exc_type`

*Type variants*: No

Description
~~~~~~~~~~~
Loads the type of :code:`$r8` through :code:`$r14` with the types encoded in each 4-bit niggle of FIELD_E. The lowest 4 bits determine the type of :code:`$r8`. The next-to-highest 4 bits determine the type of :code:`$r14`. The highest nibble of FIELD_E is ignored. If a nibble has a value of 0xf, the type of the corresponding register is not changed.

If an unsupported (or reserved) type is specified, a :code:`exc_type` exception is raised.


.. _rd_eq_short_value:

$rD <- short VALUE
--------------------------

*Instruction code*: 0x.0f0 0x****

*Exceptions*: None

*Type variants*: Yes

Description
~~~~~~~~~~~

Loads :code:`$rD` with the value of FIELD_E. The type of :code:`$rD` is not changed. The type of FIELD_E is determined using :ref:`standard type handling<std_type_handling>`.


.. _pc_eq_short_value:

$pc <- short VALUE
--------------------------

*Instruction code*: 0x20fe 0x****

*Exceptions*: Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$pc` with the sign-extended value of FIELD_E. This is an absolute jump operation. The LSB of :code:`$rD` carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

.. note:: A branch operation can obviously cause exceptions, if the branch target is invalid as defined by the memory protection scheme of the implementation. However, in those cases, the exception is raised during the fetch attempt of the target instruction, not during the execution of the branch operation.


.. _tpc_eq_short_value:

$tpc <- short VALUE
--------------------------

*Instruction code*: 0x30fe 0x****

*Exceptions*: Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$tpc` with the sign-extended value of FIELD_E. This is an absolute jump operation in TASK mode. The LSB of the immediate value carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

.. note:: A branch operation can obviously cause exceptions, if the branch target is invalid as defined by the memory protection scheme of the implementation. However, in those cases, the exception is raised during the fetch attempt of the target instruction, not during the execution of the branch operation.

.. _call_short_value:

call short VALUE
--------------------------

*Instruction code*: 0x40fe 0x****

*Exceptions*: Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$pc` with the sign-extended value of FIELD_E, while loading :code:`$lr` with the current value of :code:`$pc`. This is an absolute call operation. The LSB of the immediate value carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

.. note:: A call operation can obviously cause exceptions, if the call target is invalid as defined by the memory protection scheme of the implementation. However, in those cases, the exception is raised during the fetch attempt of the target instruction, not during the execution of the branch operation.

.. _call_value:

call VALUE
--------------------------

*Instruction code*: 0x40ef 0x**** 0x****

*Exceptions*: Implementation defined

*Type variants*: No

Description
~~~~~~~~~~~
Loads :code:`$pc` with the 32-bit value of FIELD_E, while loading :code:`$lr` with the current value of :code:`$pc`. This is an absolute call operation. The LSB of the immediate value carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

.. note:: A call operation can obviously cause exceptions, if the call target is invalid as defined by the memory protection scheme of the implementation. However, in those cases, the exception is raised during the fetch attempt of the target instruction, not during the execution of the branch operation.


