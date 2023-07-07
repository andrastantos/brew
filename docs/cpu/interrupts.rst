Interrupts
==========

While working on the `Cray simulator <http://www.modularcircuits.com/blog/articles/the-cray-files/>`_, I came across an interesting implementation idea, one that I haven't seen in any modern processor: these machines didn't have an interrupt vector; they had different execution contexts. In my variant of this idea, the processor is maintaining two PCs for two contexts: :code:`$spc` for a context I call the SCHEDULER and another (:code:`$tpc`) for the TASK context. PC (or :code:`$pc`) is just a reference to one or the other, depending on the execution context. The operation is the following:

In SCHEDULER mode, interrupts are *always* disabled. You can't enable them. Period. There is a special instruction, that takes you into TASK mode (:code:`stm`). This instruction simply sets the execution context to TASK mode, so execution continues from the current :code:`$tpc`.

In TASK mode, interrupts are *always* enabled. You can't disable them. Period. Whenever an interrupt occurs, the processor switches back to SCHEDULER mode and continues from wherever :code:`$spc` points to.

This is very confusing at first, because it appears that interrupts just get the processor to start execution from a seemingly random place. To understand what's going on you have to think about how we entered TASK mode to begin with. The only way to enter TASK mode is to execute the :code:`stm` instruction. Where does :code:`$spc` point to when TASK mode execution starts? After this :code:`stm` instruction. So, when the processor returns to SCHEDULER mode, it continues execution *after* the :code:`stm` instruction. Pretty neat: :code:`stm` works almost as a procedure call and TASK mode 'returns', whenever there's an event needing the attention of the SCHEDULER.

In practice, the SCHEDULER mode code is nothing but a ... well ... scheduler loop: it figures out the reason for the interrupt, finds the handler task for it, and enters TASK mode to 'call' the handler. This could involve switching to a different process (in the case of a timer interrupt in a multi-tasking machine) or entering for instance the keyboard driver in case of a keyboard interrupt. It's a very natural way of writing such code.

Exceptions and SW-generated interrupts (system calls, software break-points, what not) are handled the same way: the TASK mode process is simply interrupted and execution is returned to SCHEDULER mode.

There of course needs to be a way to setup a task: there are instructions that can manipulate :code:`$tpc` specifically. This is different from branch operations which work on :code:`$pc`, that is the program counter of the executing context. These :code:`$tpc` manipulation instructions turn into branches if the processor happens to be in TASK mode, but they don't change execution order, if executed in SCHEDULER mode.

