Exceptions and Interrupts
=========================

While working on the `Cray simulator <http://www.modularcircuits.com/blog/articles/the-cray-files/>`_, I came across an interesting implementation idea, one that I haven't seen in any modern processor: these machines didn't have an interrupt or exception vector; they had different execution contexts. In my variant of this idea, the processor is maintaining two PCs for two contexts: :code:`$spc` for a context I call the SCHEDULER and another (:code:`$tpc`) for the TASK context. The program counter (or :code:`$pc`) is just a reference to one or the other, depending on the execution context. The operation is the following:

In SCHEDULER mode, interrupts are *always* disabled. You can't enable them. Period. The best you can do is to wait for an interrupt (with the :ref:`woi<woi>` instruction). There is a special instruction, that takes you into TASK mode (:ref:`stm<stm>`). This instruction simply sets the execution context to TASK mode, so execution continues from the current :code:`$tpc`.

In TASK mode, interrupts are *always* enabled. You can't disable them. Period. Whenever an interrupt or exception occurs, the processor switches back to SCHEDULER mode and continues from wherever :code:`$spc` points to.

This is very confusing at first, because it appears that interrupts just get the processor to start execution from a seemingly random place. To understand what's going on, you have to think about how we entered TASK mode to begin with. The only way to enter TASK mode is to execute the :code:`stm` instruction. Where does :code:`$spc` point to when TASK mode execution starts? After this :code:`stm` instruction. So, when the processor returns to SCHEDULER mode, it continues execution *after* the :code:`stm` instruction. Pretty neat: :code:`stm` works almost as a procedure call and TASK mode 'returns', whenever there's an event needing the attention of the SCHEDULER.

In practice, the SCHEDULER mode code is nothing but ... well ... a scheduler loop: it figures out the reason for the interrupt or exception, finds the handler task for it, and enters TASK mode to 'call' the handler. This could involve switching to a different process (in the case of a timer interrupt in a multi-tasking machine) or entering for instance the keyboard driver in case of a keyboard interrupt. It could also call an exception handler in case of an access violation. It's a very natural way of writing such code.

Software-generated exceptions (system calls, software break-points, what not) are handled the same way: the TASK mode process is simply interrupted and execution is returned to SCHEDULER mode.

There of course needs to be a way to set a task up: there are instructions that can manipulate :code:`$tpc` specifically. This is different from branch operations which work on :code:`$pc`, that is the program counter of the executing context. These :code:`$tpc` manipulation instructions turn into branches if the processor happens to be in TASK mode, but they don't change execution order, if executed in SCHEDULER mode.

.. _exception_handling:

Exception Handling
------------------

Brew defines several exception sources. Many more sources can be defined by the implementation. Exceptions are precise, meaning that when the exception handling mechanism is invoked:

#. All the side-effects of instructions preceding the excepting one have fully taken effect
#. None of the side-effects of any instructions, subsequent to the excepting one have taken effect
#. The excepting instruction can be retried, if their side-effects only involve memory locations (as opposed to memory-mapped I/O for instance) and registers.

Instructions that store at most a single, aligned 32-bit or smaller quantity and at most modify a single CPU register satisfy a stronger constraint on exceptions:

#. None of the side-effects of the excepting instruction have taken effect.

Exceptions in TASK mode cause the processor to enter SCHEDULER mode. Interrupts are considered one of the possible exceptions.

When a TASK mode exception occurs, execution commences in SCHEDULER mode. :code:`$tpc` points to the excepting instruction.

Exceptions in SCHEDULER mode cause the processor to jump to address 0 (the reset vector). Interrupts in SCHEDULER mode are masked and don't take effect.

If an exception or interrupt occurs during the execution of a prefix instruction sequence :code:`$tpc` points to the first prefix instruction. The same precise exception guarantees are honored for prefixed instructions as normal ones. In other words, prefixes are considered part of the instruction when it comes to exception or interrupt handling.

Exception sources
-----------------

The following base set of exceptions are supported by all Brew processors:

======= ======================== =================================
Code    Name                     Description
======= ======================== =================================
0x0000  :code:`exc_reset`        Hardware reset
0x0010  :code:`exc_hwi`          Hardware interrupt (only in TASK mode)
0x0020  :code:`exc_swi_0`        SWI 0 instruction executed (FILL)
0x0021  :code:`exc_swi_1`        SWI 1 instruction executed (BREAK)
0x0022  :code:`exc_swi_2`        SWI 2 instruction executed (SYSCALL)
0x0023  :code:`exc_swi_3`        SWI 3 instruction executed
0x0024  :code:`exc_swi_4`        SWI 4 instruction executed
0x0025  :code:`exc_swi_5`        SWI 5 instruction executed
0x0026  :code:`exc_swi_6`        SWI 6 instruction executed
0x0027  :code:`exc_swi_7`        SWI 7 instruction executed
0x0030  :code:`exc_unknown_inst` Undefined instruction
0x0031  :code:`exc_type`         Type error in instruction operands
0x0032  :code:`exc_unaligned`    Unaligned memory access
======= ======================== =================================

An implementation can define many additional exceptions, such as ones related to memory protection, in the code range of 0x8000...0xffff.

When an exception occurs, the following CSRs are updated:

#. :code:`csr_ecause` is updated to the exception code
#. :code:`csr_eaddr` is updated to the memory reference address causing the exception. This is either the instructions address in case of non memory-related exceptions, or the target memory address in case of memory-related exceptions (of which only :code:`exc_unaligned` is defined).

.. note:: When an exception occurs in SCHEDULER mode, :code:`csr_eacuse` and :code:`csr_eaddr` CSRs are set.

An instruction can raise at most one exception. If multiple exception causes are triggered, the one with the lowest numeric code will take effect. This means:

#. Interrupts take priority over any exception (the exception will be raised when - after the interrupt is handled - code returns to TASK mode and the instruction is retried).
#. All implementation-defined exceptions are lower priority then the common ones

Interrupts
----------

Interrupts can occur both in TASK or SCHEDULER mode. When the processor is in TASK mode, it transfers execution into SCHEDULER mode. When an interrupt occurs in SCHEDULER mode, the execution flow is not modified, but the implementation must provide a way to poll for pending interrupts in SCHEDULER-mode.

Upon handling an interrupt in TASK mode, :code:`$tpc` points to the instruction to be executed after the interrupt is handled. The implementation guarantees that all side-effects of prior instructions are fully carried out while none of the side-effects of the interrupted or subsequent instructions have taken place.

The :code:`WOI` instruction can be used both in SCHEDULER and TASK-mode to stall execution until an interrupt occurs.
