Exception Handling
==================

Brew defines several exception sources. Many more sources can be defined by the implementation. Exceptions are precise, meaning that when the exception handling mechanism is invoked:

#. All the side-effects of instructions preceding the excepting one have fully taken effect
#. None of the side-effects of any instructions, subsequent to the excepting one have taken effect
#. The excepting instruction can be retried, if their side-effects only involve memory locations.

Instructions that store at most a single, aligned 32-bit quantity and at most modify a single CPU register satisfy a stronger constraint on exceptions:

#. None of the side-effects of the excepting instruction have taken effect.

Exceptions in TASK mode cause the processor to enter SCHEDULER mode. Interrupts are considered one of the possible exceptions.

When a TASK mode exception occurs, execution commences in SCHEDULER mode. :code:`$tpc` points to the excepting instruction.

Exceptions in SCHEDULER mode cause the processor to jump to address 0 (the reset vector). Interrupts in SCHEDULER mode are masked and don't take effect.

If an exception or interrupt occurs during the execution of a prefix instruction :code:`$tpc` points to the first prefix instruction. The same precise exception guarantees are honored for prefixed instructions as normal ones. In other words, prefixes are considered part of the instruction with regards to exception or interrupt handling.

Exception sources
-----------------

The following base set of exceptions are supported by all Brew processors:

======================== =================================
Name                     Description
======================== =================================
:code:`exc_swi_0`        SWI 0 instruction executed (FILL)
:code:`exc_swi_1`        SWI 1 instruction executed (BREAK)
:code:`exc_swi_2`        SWI 2 instruction executed (SYSCALL)
:code:`exc_swi_3`        SWI 3 instruction executed
:code:`exc_swi_4`        SWI 4 instruction executed
:code:`exc_swi_5`        SWI 5 instruction executed
:code:`exc_swi_6`        SWI 6 instruction executed
:code:`exc_swi_7`        SWI 7 instruction executedh
:code:`exc_unknown_inst` Undefined instruction
:code:`exc_unaligned`    Unaligned memory access
:code:`exc_type`         Type error in instruction operands
:code:`exc_hwi`          Hardware interrupt (only in TASK mode)
======================== =================================

An implementation can define many additional exceptions, such as ones related to memory protection.

Interrupts
----------

Interrupts can occur both in TASK or SCHEDULER mode. When the processor is in TASK mode, it transfers execution into SCHEDULER mode. When an interrupt occurs in SCHEDULER mode, the execution flow is not modified, but the implementation must provide a way to poll for pending interrupts in SCHEDULER-mode.

The :code:`WOI` instruction can be used both in SCHEDULER and TASK-mode to stall execution until an interrupt occurs.
