
.. _load_store_multiple:

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

These operations can load and store the whole architectural state of the processor. The state can be masked and skipped by either a skip-mask or the dirty state. Memory and stack-based variants are provided.

An implementation defined storage area is used to do store the specified portion of the processor state. For every implementation, there is a well documented maximum size for state storage. The actual layout of the storage area is implementation defined.

The storage area pointer is provided to these instructions in :code:`$rD`

POP: on input $rD points to 4 bytes *before* the beginning of the storage area. $rD is incremented by the size of the storage area used.
PUSH: on input $rD points to the end of the storage area. $rD is decremented by the size of the storage area used.
load/store: on input $rD points to the beginning of the storage area It is not modified by the operation, unless of course it's part of the set

Upon return :code:`$rD` is incremented/decremented by the size of the storage area.

If POP specifies $rD as its return set, the value and type of $rD is restored from the storage area and returned instead of the updated $rD.

Suggested storage layout:

========== ================= ====================
Size       Used for inst.    Contents
========== ================= ====================
2          all               Mask used during store (value of FIELD_E)
2          ld/st only        DIRTY
1          ld/st only        VSTART
1          ld/st only        VEND
1          ld/st only        FPSTAT
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

The size of the storage area depends on the parameters passed to the instructions as well as the processor implementation.

PUSH (being a decrementing operation) writes the structure backwards. POP reads it forwards, which means that the first things it reads are the fixed length fields, which allows it to reconstruct the rest of the structure. Since loads and stores always have a pointer to the beginning of the structure, they don't have a problem with size determination.

Values of registers are stored in full, independent of the current state of :code:`vstart` and :code:`vend`.

Only registers that are marked in FIELD_E, the mask are touched. The storage space is compacted and untouched register storage is removed. Type info may be skipped, or written as 0xf for non-touched registers.

The LSB of FIELD_E corresponds to :code:`$r0`, bit 14 corresponds to :code:`$r14`. When a bit is set to '1', the corresponding register is stored. When a bit is set to 0, the corresponding register is not touched.

In other words, FIELD_E, the mask determines the exact layout of the storage area. Successful read-back is only possible if the same FIELD_E mask is used.

.. todo:: we could store the mask at the top of the storage, in which case POP/load doesn't even have to specify it. Maybe worth considering?

.. todo:: skip-mask for storage/push is stupid: I can't even describe how it works, let alone why it would be useful!

Dirty flag handling
-------------------

For store/push operations, if the MSB of FIELD_E is set, the :code:`dirty` flag of each register is used to determine if actual storage of values takes place. If the :code:`dirty` flag is set, both type and value is committed to storage. If cleared, only the type is written; While storage area for the value is still reserved, the value is not written into memory.


For load/pop operations, if the MSB of FIELD_E is set, the :code:`dirty` flag of each register is used to determine if the value of the register is to be restored. If the :code:`dirty` flag is set, the value and type is restored. If the :code:`dirty` flag is clear, neither the value nor the type of the target register is touched. It is important to realize that the :code:`dirty` flag is based on the current architectural state of the processor, not the DIRTY value held in the storage structure.

.. note:: skipping clean registers speeds up saving of the context, the feature needs to be used carefully: since the storage area for the register is not modified, a subsequent restoration, especially one that ignores the :code:`dirty` flag state can pick up stale content from memory.

For load/pop operation, the DIRTY flags for each register from the storage area.
* As discussed, the determination if a value or type is loaded from the storage area is dependent on the *current* :code:`dirty` state, not the one stored.
* If a registers value is restored, its corresponding dirty state is also restored from the storage area.
* If restoration is skipped due to the current :code:`dirty` value, its dirty state is still restored from storage
* If restoration is skipped due to the skip-mask in $rA, the dirty bit is *set*, independent of what is in storage.
* If restoration is skipped due to both the current :code:`dirty` value and the skip-mask, the dirty bit is *set*, independent of what is in storage.

Skip-mask handling
------------------

For load/pop operations a skip-mask register ($rA) can be provided. If specified, a '1' in the appropriate bit will instruct the store/push operations to skip the restoration of the type or value of the given register.

.. note:: Skip-masks are useful for returning values from scheduler context to tasks upon the return from a SYSCALL for example.

Exception behavior
------------------

If an exception (due to access violation during memory access) is raised, $tpc points to the load/store multiple instruction. It however is generally not guaranteed that no loads or stores have been performed. Consequently, some of the side-effects might have already taken place and the exception handler is in no position to know which ones. It is however safe to assume that the operation can be retried, as long as the following conditions are met:

* Address translation after the retry generates the same physical addresses for store multiple operations
* The target address is in regular memory as opposed to I/O or CSR space

For a load/pop multiple where the base register is marked for load, the implementation ensures that the base register retains its value and type if an exception occurs. The restored value and type can only become visible if no exceptions occur.

If the operation - after handling of the exception - is retried, the implementation restarts the whole instruction.

.. todo::

  These instructions are not supported by the toolset, or Espresso.
