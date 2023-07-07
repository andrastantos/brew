Process isolation
-----------------

There are no privileged instructions. Every instruction can be executed in both SCHEDULER and TASK mode with exactly the same semantics. Process-isolation and protection is achieved by two key concepts:

* There is a slight imbalance in the ISA: there is a way to influence the current contexts' :code:`$pc` and :code:`$tpc`; there is no way to influence :code:`$spc` (unless of course that happens to be the current context :code:`$pc`).
* All resources (I/O, memory, even CSRs) are accessed through memory references. These references go through an MMU, which controls per-task access rights.

The end result is that there is no way to influence what SCHEDULER mode code is doing from TASK mode, unless of course the programmer provides one. SCHEDULER mode code controls (or rather, initially controls but later delegates) MMU access-right management for individual TASK-mode processes. These processes have their access rights enforced by the MMU. The MMU registers and page-tables are themselves accessed through the MMU and thus are subject to access control: a process normally can't change it's own memory mapping and access rights.

Since all I/O and even CSR access is subject to MMU re-mapping, complete access control can be achieved with an appropriately capable MMU implementation.

.. note::
    The actual MMU specification is outside of scope for the Brew architecture. IT is part of a particular implementation. A traditional, page-table-based MMU with read/write access right control is sufficient.

.. note::
    The details of how SCHEDULER and TASK mode execution uses the MMU is not part of the Brew spec (it is the domain of a particular implementation), but normally, it SCHEDULER mode would bypass the MMU while TASK mode would be subject to it.

.. note::
    The fact the SCHEDULER-mode is so limited puts some overhead on system calls as there are two context switches involved: from user-task to SCHEDULER mode and from there to system-task. On the way back, a similar double-switch needs to happen. At the same time, this is not any less efficient then a simple task-switch in a pre-emptive OS. It's just that the OS itself is a task. In that regard, this implementation is similar to micro-kernel OS-es.

    Due to the double-switching nature, it is important that implementations optimize for this behavior. Most of the overhead doesn't come from the actual switching of the user-visible context, but from all the flushing of the internal CPU state: caches, TLBs. Specific design techniques can and should be used to minimize this impact.

