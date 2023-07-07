Task and Scheduler modes
------------------------

There are only two execution contexts: TASK mode and SCHEDULER mode. They roughly correspond to user-space and kernel-space in other architectures, but there are differences that warrant a different naming. There is no concept of rings such as on ia32.

Each context has its program counter: :code:`$tpc` for TASK mode and :code:`$spc` for SCHEDULER mode. The processor updates the appropriate program counter depending on the execution context. Most instructions deal with the 'program counter of the current context', which is simply named :code:`$pc`, which is an alias to either :code:`$tpc` or :code:`$spc`.

There are very limited ways to switch contexts: the processor can enter SCHEDULER mode though interrupts or exceptions (including software interrupts). The only way to enter TASK mode is by the use of the :code:`stm` instruction.

After a mode-switch, execution continues from the contexts :code:`$pc`. There is no concept of an interrupt or exception vector: when TASK mode gets interrupted, SCHEDULER mode execution continues from the instruction pointed to by :code:`$spc`. After the execution of an :code:`stm` instruction, the execution continues from wherever :code:`$tpc` point to.

