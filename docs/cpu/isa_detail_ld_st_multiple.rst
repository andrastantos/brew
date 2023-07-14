$r0...$r14 <- MEM[$rD] @ $rA
----------------------------

*Instruction code*: 0x.f0. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

load any combination of registers with FIELD_E as mask; skip-mask in $rA


$r0...$r14 <- MEM[$rD]
----------------------

*Instruction code*: 0x.f0f 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

load any combination of registers with FIELD_E as mask


MEM[$rD] <- $r0...$r14 @ $rA
----------------------------

*Instruction code*: 0x.f1. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

store any combination of registers with FIELD_E as mask; skip-mask in $rA


MEM[$rD] <- $r0...$r14
----------------------

*Instruction code*: 0x.f1f 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

store any combination of registers with FIELD_E as mask


$r0...$r14 <- POP[$rD] @ $rA
----------------------------

*Instruction code*: 0x.f2. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

pop any combination of registers with FIELD_E as mask; skip-mask in $rA


$r0...$r14 <- POP[$rD]
----------------------

*Instruction code*: 0x.f2f 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

pop any combination of registers with FIELD_E as mask


PUSH[$rD] <- $r0...$r14 @ $rA
-----------------------------

*Instruction code*: 0x.f3. 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

push any combination of registers with FIELD_E as mask; skip-mask in $rA


PUSH[$rD] <- $r0...$r14
-----------------------

*Instruction code*: 0x.f3f 0x****

*Exceptions*: TBD

*Type variants*: TBD

Description
~~~~~~~~~~~

push any combination of registers with FIELD_E as mask


Load/store multiple
===================

**These are very complex instructions.**

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_A",   "bits": 4, attr: "offset" },
      { "name": "FIELD_B",   "bits": 4, attr: "op kind" },
      { "name": "f",         "bits": 4 },
      { "name": "FIELD_D",   "bits": 4, attr: "$rD" },
  ],
  }

.. wavedrom::

  {config: {bits: 16}, config: {hspace: 500},
  reg: [
      { "name": "FIELD_E", "bits": 16 },
  ],
  }

..
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |    FIELD_D    |       f       |    FIELD_B    |    FIELD_A    |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |                         FIELD_E                               |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

==================  =======================================    ==================
Instruction code    Assembly                                   Operation
==================  =======================================    ==================
0x.f0. 0x****       $r0...$r14 <- MEM[$rD] @ $rA               load any combination of registers with FIELD_E as mask; skip-mask in $rA
0x.f1. 0x****       MEM[$rD] <- $r0...$r14 @ $rA               store any combination of registers with FIELD_E as mask; skip-mask in $rA
0x.f2. 0x****       $r0...$r14 <- POP[$rD] @ $rA               pop any combination of registers with FIELD_E as mask; skip-mask in $rA
0x.f3. 0x****       PUSH[$rD] <- $r0...$r14 @ $rA              push any combination of registers with FIELD_E as mask; skip-mask in $rA
0x.f0f 0x****       $r0...$r14 <- MEM[$rD]                     load any combination of registers with FIELD_E as mask
0x.f1f 0x****       MEM[$rD] <- $r0...$r14                     store any combination of registers with FIELD_E as mask
0x.f2f 0x****       $r0...$r14 <- POP[$rD]                     pop any combination of registers with FIELD_E as mask
0x.f3f 0x****       PUSH[$rD] <- $r0...$r14                    push any combination of registers with FIELD_E as mask
==================  =======================================    ==================

These operations can essentially load and store the whole architectural state of the processor. The state can be masked and skipped by either a skip-mask or the dirty state. Memory and stack-based variants are provided.

PUSH: on input $rD points to 4 bytes before the beginning of the storage area. $rD is decremented by the size of the storage area used.
POP: on input $rD points to the end of the storage area. $rD is incremented by the size of the storage area used.
load/store: on input $rD points to the beginning of the storage area It is not modified by the operation, unless of course it's part of the set

If POP specifies $rD as its return set, the value and type of $rD is restored from the storage area and returned instead of the updated $rD.

Suggested storage layout:

========== ================= ====================
Size       Used for inst.    Contents
========== ================= ====================
2          all               Mask used during store (value of FIELD_E)
2          ld/st only        DIRTY
1          ld/st only        VSTART
1          ld/st only        VEND
1          ld/st only        FP status and control register
1          ld/st only        padding
8          all               Type info for all 15 registers.
.          all               PAD to allow for aligned storage of 1st register value
.          all               Value of 1st register
.          all               PAD to allow for aligned storage of 2nd register value
.          all               Value of 2nd register
...        all               ...
.          all               PAD to allow for aligned storage of Nth register value
.          all               Value of Nth register
========== ================= ====================

PUSH (being a decrementing operation) writes the structure backwards. POP reads it forwards, which means that the first things it reads are the fixed length fields, which allows it to reconstruct the rest of the structure. Since loads and stores always have a pointer to the beginning of the structure, they don't have a problem with size determination.

Values of registers are stored in full, independent of the current state of :code:`vstart` and :code:`vend`.

Now, for the operation: Only registers that are marked in FIELD_E, the mask are touched. The storage space is compacted and untouched register storage is removed. Type info may be skipped, or written as 0xf for non-touched registers.

In other words, FIELD_E, the mask determines the exact layout of the storage area. Successful read-back is only possible if the same FIELD_E mask is used.

.. todo:: we could store the mask at the top of the storage, in which case POP/load doesn't even have to specify it. Maybe worth considering?

.. todo:: skip-mask for storage/push is stupid: I can't even describe how it works, let alone why it would be useful!

There is another ways to influence storage of elements: If the MSB of FIELD_E is set, the :code:`dirty` flag of each register is used to determine if actual storage of values takes place. If the :code:`dirty` flag is set, both type and value is committed to storage. If cleared, only the type is written; the storage area for the value of the register is not modified. The CPU maintains a :code:`dirty` bit for each register which is set every time either the value or type of the register is changed. Skipping clean registers speeds up saving of the context.

Upon load/pop operations, there are two additional ways to influence the restoration of registers:

* If the MSB of FIELD_E is set, the :code:`dirty` flag of each register is used to determine of the value of the register is to be restored. If the :code:`dirty` flag is set, the value and type is restored. If the :code:`dirty` flag is clear, neither the value nor the type of the target register is touched. While type is always read from memory, its commitment to the register is masked. Reading of the value from memory is skipped. The point of skipping clean registers is to provide quick context restoration when only some of the registers changed value since the storage of them took place. It is important to realize that the :code:`dirty` flag is based on the current architectural state of the processor, not the DIRTY value held in the storage structure.
* A skip-mask register ($rA) can be provided. If specified, a '1' in the appropriate bit will instruct the store/push operations to skip the actual restoration of the values. As far as semantics goes, setting the skip bit is the same as clearing the :code:`dirty` bit.

The restoration of a type and value only occurs if neither a skip-mask bit is set nor is the :code:`dirty` bit clear.

Special fields:

We need to carefully consider the storage/restoration of VSTART/VEND. These are all things that deserve restoration, but at the same time, can change during exception handling. If they are restored automatically, their new value needs to be patched into the struct by the exception handler. Maybe not the end of the world, but means the exception handler needs to crack open the storage struct: it's not a black box anymore.

We also need to carefully consider the restoration of the DIRTY flag: if stored, it shows which register values were not updated. That's fine. Upon restore, we need to use the *current* :code:`dirty` state to determine if we need to load the values from the struct. Then, we restore the DIRTY state as well. There is different handling for the two reasons for skipping though:
* If we skip a restoration due to the current :code:`dirty` value, we need to restore the dirty bit from storage
* If we skip a restoration due to the skip-mask in $rA, we need to *set* the dirty bit, independent of what is in storage.


*Exception behavior*: If an exception (due to access violation during memory access) is raised, $tpc points to the load/store multiple instruction. It however is generally not guaranteed that no loads or stores have been performed. Consequently, some of the side-effects might have already taken place and the exception handler is in no position to know which ones. It is however safe to assume that the operation can be retried, as long as the following conditions are met:

* Address translation after the retry generates the same physical addresses for store multiple operations
* The target address is in regular memory as opposed to I/O or CSR space

For a load/pop multiple where the base register is marked for load, the implementation must ensure that the new register value only takes effect after the operation fully completes; in other words, if an exception occurs, the base register value and type is guaranteed not to be modified.

If the operation - after handling of the exception - is retried, the implementation is free to restart the whole instruction. While optimizations are possible, the expectation is that exceptions within these instructions are rather rare (as they are primarily used for stack-frame management or context-change both of which generally should succeed).

.. note::

  The MSB of the mask field controls 'DIRTY' behavior.

.. todo::

  These instructions are not supported by the toolset, or Espresso.

.. note::

  These instructions should *not* make use of or modify vstart/vend: they store/load full HW registers, based on type.
