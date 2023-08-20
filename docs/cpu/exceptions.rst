.. _exception_handling:

Exception Handling
==================

Brew defines several exception sources. Many more sources can be defined by the implementation. Exceptions are precise, meaning that when the exception handling mechanism is invoked:

#. All the side-effects of instructions preceding the excepting one have fully taken effect
#. None of the side-effects of any instructions, subsequent to the excepting one have taken effect
#. The excepting instruction can be retried, if their side-effects only involve memory locations.

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

#. :code:`ECAUSE` is updated to the exception code
#. :code:`EADDR` is updated to the memory reference address causing the exception. This is either the instructions address in case of non memory-related exceptions, or the target memory address in case of memory-related exceptions (of which only :code:`exc_unaligned` is defined).

.. note:: When an exception occurs in SCHEDULER mode, :code:`ECAUSE` and :code:`EADDR` CSRs are set.

An instruction can raise at most one exception. If multiple exception causes are triggered, the one with the lowest numeric code will take effect. This means:

#. Interrupts take priority over any exception (the exception will be raised after when - after the interrupt is handled - code returns to TASK mode).
#. If a store or load operation occurs to an unaligned address with an unsupported type (for instance a store-conditional operation with a vector type), the :code:`exc_type` exception will be raised.
#. All implementation-defined exceptions are lower priority then the common ones

Interrupts
----------

Interrupts can occur both in TASK or SCHEDULER mode. When the processor is in TASK mode, it transfers execution into SCHEDULER mode. When an interrupt occurs in SCHEDULER mode, the execution flow is not modified, but the implementation must provide a way to poll for pending interrupts in SCHEDULER-mode.

The :code:`WOI` instruction can be used both in SCHEDULER and TASK-mode to stall execution until an interrupt occurs.
