SWI 0
-----

*Instruction code*: 0x0000

*Alternative syntax*: FILL

*Exceptions*: SWI 0

*Type variants*: No

Description
~~~~~~~~~~~

Raises the :code:`exc_swi_0` exception. Used to fill unused space.

SWI 1
-----

*Instruction code*: 0x1000

*Alternative syntax*: BREAK

*Exceptions*: SWI 1

*Type variants*: No

Description
~~~~~~~~~~~

Raises the :code:`exc_swi_1` exception. Used to implement software breakpoints.

SWI 2
-----

*Instruction code*: 0x2000

*Alternative syntax*: SYSCALL

*Exceptions*: SWI 2

*Type variants*: No

Description
~~~~~~~~~~~

Raises the :code:`exc_swi_2` exception. Used to implement operating system calls

SWI 3
-----

*Instruction code*: 0x3000

*Exceptions*: SWI 3

*Type variants*: No

Description
~~~~~~~~~~~

Raises the :code:`exc_swi_3` exception.

SWI 4
-----

*Instruction code*: 0x4000

*Exceptions*: SWI 4

*Type variants*: No

Description
~~~~~~~~~~~

Raises the :code:`exc_swi_4` exception.

SWI 5
-----

*Instruction code*: 0x5000

*Exceptions*: SWI 5

*Type variants*: No

Description
~~~~~~~~~~~

Raises the :code:`exc_swi_5` exception.

SWI 6
-----

*Instruction code*: 0x6000

*Exceptions*: SWI 6

*Type variants*: No

Description
~~~~~~~~~~~

Raises the :code:`exc_swi_6` exception.

SWI 7
-----

*Instruction code*: 0x7000

*Exceptions*: SWI 7

*Type variants*: No

Description
~~~~~~~~~~~

Raises the :code:`exc_swi_7` exception.

STM
---

*Instruction code*: 0x8000

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

Returns execution to TASK mode. If already in TASK mode, the instruction as no effect. Execution continues in TASK mode from the address pointed to by :code:`$tpc`

WOI
---

*Instruction code*: 0x9000

*Exceptions*: HWI

*Type variants*: No

Description
~~~~~~~~~~~

Wake-on-interrupt. The processor enters a low-power state and waits for an interrupt. When an interrupt occurs, the processor continues execution. This operation waits for an interrupt, even if executed in SCHEDULER mode. In TASK mode, of course once execution is resumed, the processor switches to SCHEDULER mode, due to the pending interrupt.

PFLUSH
------

*Instruction code*: 0xa000

*Exceptions*: None

*Type variants*: No

Description
~~~~~~~~~~~

This instruction flushes the internal pipeline. Subsequent instructions must be fetched anew from at least L1 instruction cache. This instruction can be used to enforce proper operation for self-modifying code or for instance when a new executable image is loaded from storage.

.. todo:: PFLUSH is not implemented anywhere. Not in BINUTILS, not in Espresso.

