Basic concepts
==============

Register field encoding
-----------------------

The ISA uses 4-bit fields to encode register addresses, which normally would allow for 16 registers. The ISA however only has 15 addressable registers. This allows us to use the 16th code (0xf) to escape several easy to decode instruction spaces.

Task and Scheduler modes
------------------------

There are only two execution contexts: TASK mode and SCHEDULER mode. These roughly correspond to user-space and kernel-space in other architectures, but there are differences (described later) that warrant a different naming. There is no concept of rings such as on ia32.

There is no way to enable or disable interrupts. To be more precise: interrupts are *always* enabled in TASK mode and *always* disabled in SCHEDULER mode. This already shows a key difference between the traditional kernel and user spaces and SCHEDULER and TASK modes on Brew: since interrupts are disabled in SCHEDULER mode, we want to spend as little time in SCHEDULER as possible. SCHEDULER mode code should determine the source of the interrupt and - thus the name - schedule the TASK-mode interrupt handler to be executed. This also means that much of what normally would be the kernel of the OS would need to run in TASK mode.

Each context has its program counter: :code:`$tpc` and :code:`$spc`. The processor updates the appropriate program counter depending on the execution context. As we will see in the instruction set description, most instructions deal with the 'program counter of the current context'. We will label that simply as :code:`$pc`, but it's important to understand, that that's just an alias to either :code:`$tpc` or :code:`$spc`.

There are very limited ways to switch contexts: we can enter SCHEDULER mode though interrupts or exceptions. The only way to enter TASK mode is by the use of the :code:`stm` instruction.

After a mode-switch, execution continues from the contexts :code:`$pc`. There is no concept of an interrupt or exception vector: when TASK mode gets interrupted, SCHEDULER mode execution continues from the instruction pointed to by :code:`$spc`. Since the only way to get to ask mode is to execute the :code:`stm` instruction in SCHEDULER mode, the end result is that SCHEDULER-mode execution simply continues after the :code:`stm` instruction.

Process isolation
-----------------

There are no privileged instructions. Every instruction can be executed in both contexts with exactly the same semantics. Process-isolation and protection is achieved by two key concepts:

* There is a slight imbalance in the ISA: there is a way to influence the current contexts' :code:`$pc` and :code:`$tpc`; there is no way to influence :code:`$spc` (unless of course that happens to be the current context :code:`$pc`).
* All resources (I/O, memory, even CSRs) are accessed through memory references. These references go through an MMU, which controls per-task access rights.

The end result is that there is no way to influence what SCHEDULER mode code is doing from TASK mode, unless of course the programmer provides one. SCHEDULER mode code then controls (or rather, initially controls but later delegates) MMU access-right management for individual TASK-mode processes. These processes have their access rights enforced by the MMU. The MMU registers and page-tables are themselves accessed through the MMU and thus are subject to access control: a process normally can't change it's own memory mapping and access rights.

The details of how SCHEDULER mode execution uses the MMU is not part of the Brew spec (it is the domain of a particular implementation), but normally, it would bypass the MMU.

.. note::

    The fact the SCHEDULER-mode is so limited puts a lot of overhead on system calls as there are two context switches involved: from user-task to SCHEDULER and from there to system-task. On the way back, a similar double-switch needs to happen. At the same time, this is not any less efficient then a simple task-switch in a pre-emptive OS. It's just that the OS itself is a task. In that regard, this implementation is similar to micro-kernel OS-es.

    Due to the double-switching nature, it is important that implementations optimize for this behavior. Most of the overhead doesn't come from the actual switching of the user-visible context, but from all the flushing of the internal CPU state: caches, TLBs. Specific design techniques can and should be used to minimize this impact.

Stack management
----------------

The ISA is strictly designed for a 2-read;1-write port register file. This means that:

* There are no PUSH/PULL instructions (as PULL would require two writes into the register file). Similarly, there aren't any post-increment post-decrement pre-increment or pre-decrement memory operations.
* There aren't CALL/RETURN instructions either (return would require two writes).

These instructions are replaced by short instruction sequences and helped by the following realization: modern programming languages and their compilers manage stack in blocks. It's rare that a single-element push or pop operation is useful, mostly its adjusting the stack by a full call-frame worth of data. The overhead of calls and returns is miniscule compared to all the work in setting up and tearing down these call-frames, so CPU resources are better spent elsewhere.

Register types
--------------

Each register has a type associated with it. Types are changed and propagated by instructions. They can be loaded and stored independent of the values contained in the registers.

The dynamic typing of registers gives great flexibility ot the ISA and allows future growth and extensibility. There is however overhead associated with managing register types. This overhead is minimized if the compiler dedicates types to certain registers and keep them constant as much as possible.

.. note::

    Almost all processors have a similar concept for floating-point (and vector) registers: they have their own type, except this type is fixed. One can see the Brew way of dealing with register types as a more flexible partitioning of the register-file. A way that can adapt to the needs of the application at hand.

Register types determine the semantics of many operations, especially in the unary, binary ALU groups and in conditional branches.

Type-less variant
-----------------

A type-less variant of the ISA is possible: in this case, all registers are assumed to have the type of INT32 and type-change instructions have no effect.

