.. _r0...r14_eq_mem_rd_@_ra:

$r0...$r14 <- MEM[$rD] @ $rA
----------------------------

*Instruction code*: 0x.f0. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

load any combination of registers with FIELD_E as mask; skip-mask in $rA. See :ref:`load/store multiple<load_store_multiple>` for detailed description.


.. _r0...r14_eq_mem_rd:

$r0...$r14 <- MEM[$rD]
----------------------

*Instruction code*: 0x.f0f 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

load any combination of registers with FIELD_E as mask. See :ref:`load/store multiple<load_store_multiple>` for detailed description.


.. _mem_rd_eq_r0...r14_@_ra:

MEM[$rD] <- $r0...$r14 @ $rA
----------------------------

*Instruction code*: 0x.f1. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

store any combination of registers with FIELD_E as mask; skip-mask in $rA. See :ref:`load/store multiple<load_store_multiple>` for detailed description.


.. _mem_rd_eq_r0...r14:

MEM[$rD] <- $r0...$r14
----------------------

*Instruction code*: 0x.f1f 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

store any combination of registers with FIELD_E as mask. See :ref:`load/store multiple<load_store_multiple>` for detailed description.


.. _r0...r14_eq_pop_rd_@_ra:

$r0...$r14 <- POP[$rD] @ $rA
----------------------------

*Instruction code*: 0x.f2. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

pop any combination of registers with FIELD_E as mask; skip-mask in $rA. See :ref:`load/store multiple<load_store_multiple>` for detailed description.


.. _r0...r14_eq_pop_rd:

$r0...$r14 <- POP[$rD]
----------------------

*Instruction code*: 0x.f2f 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

pop any combination of registers with FIELD_E as mask. See :ref:`load/store multiple<load_store_multiple>` for detailed description.


.. _push_rd_eq_r0...r14_@_ra:

PUSH[$rD] <- $r0...$r14 @ $rA
-----------------------------

*Instruction code*: 0x.f3. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

push any combination of registers with FIELD_E as mask; skip-mask in $rA. See :ref:`load/store multiple<load_store_multiple>` for detailed description.


.. _push_rd_eq_r0...r14:

PUSH[$rD] <- $r0...$r14
-----------------------

*Instruction code*: 0x.f3f 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

push any combination of registers with FIELD_E as mask. See :ref:`load/store multiple<load_store_multiple>` for detailed description.

