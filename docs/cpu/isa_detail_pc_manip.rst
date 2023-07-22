.. _pc_eq_rd:

$pc <- $rD
----------

*Instruction code*: 0x.002

*Exceptions*: implementation defined

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$pc` is loaded with the value in :code:`$rD`. This is an indirect jump. The type of :code:`$rD` is ignored and as INT32. The LSB of :code:`$rD` carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.

.. note:: A branch operation can obviously cause exceptions, if the branch target is invalid as defined by the memory protection scheme of the implementation. However, in those cases, the exception is raised during the fetch attempt of the target instruction, not during the execution of the branch operation.


.. _tpc_eq_rd:

$tpc <- $rD
-----------

*Instruction code*: 0x.003

*Exceptions*: implementation defined

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$tpc` is loaded with the value in :code:`$rD`. This is an indirect jump in TASK mode. The type of :code:`$rD` is ignored and as INT32. The LSB of :code:`$rD` carries implementation-defined meaning, including potential exceptions. If the implementation doesn't define a meaning for the LSB, it is ignored and no exceptions are raised.


.. _rd_eq_pc:

$rD <- $pc
----------

*Instruction code*: 0x.004

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$rD` is loaded with the value in :code:`$pc`, which points to the currently executing instruction. The LSB loaded into :code:`$rD` carries implementation-defined meaning. There's no guarantee that the LSB is preserved between a pair of :code:`$pc <- $rD` and :code:`$rD <- $pc` instructions. If the implementation doesn't define a meaning, the LSB will be loaded with 0. The type of :code:`$rD` is set to INT32.

.. _rd_eq_tpc:

$rD <- $tpc
-----------

*Instruction code*: 0x.005

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$rD` is loaded with the value in :code:`$tpc`. In TASK-mode, this will be the address of the currently executing instruction. The LSB loaded into :code:`$rD` carries implementation-defined meaning. There's no guarantee that the LSB is preserved between a pair of :code:`$tpc <- $rD` and :code:`$rD <- $tpc` instructions. If the implementation doesn't define a meaning, the LSB will be loaded with 0. The type of :code:`$rD` is set to INT32.



