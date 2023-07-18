.. _pc_eq_rd:

$pc <- $rD
----------

*Instruction code*: 0x.002

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       2       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: implementation defined

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$pc` is loaded with the value in :code:`$rD`. This is an indirect jump. The LSB of :code:`$rD` carries implementation-defined meaning.

*Notes*: The CPU implementation can define various exceptions, based on its memory protection mechanism employed. The LSB of the target address carries implementation-defined meaning. If such a meaning is not defined, an implementation ignores the LSB.


.. _tpc_eq_rd:

$tpc <- $rD
-----------

*Instruction code*: 0x.003

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       3       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: implementation defined

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$tpc` is loaded with the value in :code:`$rD`. This is an indirect jump in TASK mode. The LSB of :code:`$rD` carries implementation-defined meaning.

*Notes*: The CPU implementation can define various exceptions, based on its memory protection mechanism. These exceptions only apply in TASK mode. If the instruction is executed in SCHEDULER-mode, the only exceptions that are allowed to be raised are the ones related to the LSB of :code:`$rD`. The LSB of the target address carries implementation-defined meaning. If such a meaning is not defined, an implementation ignores the LSB.

.. _rd_eq_pc:

$rD <- $pc
----------

*Instruction code*: 0x.004

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       4       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$rD` is loaded with the value in :code:`$pc`, which points to the currently executing instruction. The LSB loaded into :code:`$rD` carries implementation-defined meaning. There's no guarantee that the LSB is preserved between a pair of :code:`$pc <- $rD` and :code:`$rD <- $pc` instructions.

.. _rd_eq_tpc:

$rD <- $tpc
-----------

*Instruction code*: 0x.005

::

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       0       |       0       |       4       |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

:code:`$rD` is loaded with the value in :code:`$tpc`. In TASK-mode, this will be the address of the currently executing instruction. The LSB loaded into :code:`$rD` carries implementation-defined meaning. There's no guarantee that the LSB is preserved between a pair of :code:`$tpc <- $rD` and :code:`$rD <- $tpc` instructions.



