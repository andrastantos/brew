Control flow
============

The Brew architecture identifies two execution contexts: SCHEDULER and TASK mode. These contexts each have their own, dedicated program counters, called :code:`$spc` and :code:`$tpc`.

Control flow within a context is managed through a pseudo-register (:code:`$pc`) which is an alias to either :code:`$spc` or :code:`$tpc` depending on the execution context.

Since the program counters are not part of the general purpose register set, special instructions are provided to manipulate them. These unconditional and conditional branch operations and direct manipulation of :code:`$tpc`.

Function call and return instructions are not provided. A special short-form instruction is provided for setting up the return address in a general purpose register, similar to the 'link register' style call mechanism of many RISC architectures.

