Brew: an Introduction
=====================

Instruction size
----------------

Brew uses instructions of variable length. While most RISC implementations chose a fixed (usually 32-bit) instruction length, this poses a major problem: how to represent immediate values? 32-bit constants in the instruction stream obviously need more than 32-bit instructions. You can chose to only support smaller range of values, but then, how do you describe 32-bit constants? You have two options and various RISC implementations do both: you either use multiple instructions to assemble a constant (for instance load the lower 16-bit, then the upper), or put the values somewhere in memory instead of the instruction stream and get them using a load instruction.

I didn't want to do either: Multiple instructions burn a lot of extra bits. Extra loads are slow: instruction pre-fetcher is about as efficient as you can get in terms of getting data from memory.

Given these considerations, I decided that constants must be part of the instructions and there must be a way to store 32-bit constants directly. That however means that some of my instructions are going to be longer than 32 bits, the next logical value being 48 bits. So, I ended up with three instruction lengths:

1. 16-bit instructions for everything not involving immediate constants
2. 32-bit instructions where a 16-bit immediate is sufficient
3. 48-bit instructions where a full 32-bit immediate is required

After experimentation, this picture got muddled a bit: I realized that there is good value in encoding really small constants in 16-bit instructions, at least for certain often used operations.

Registers and their encoding
----------------------------

The ISA follows the traditional 3-register addressing of almost all modern RISC processors: there is a destination register and (up to) two source registers. There are a total of 15 general-purpose registers, $r0 through $r14. An addition can be described as: `$r3 <- $r10 + $r12` for instance (yes, the assembly is algebraic, you don't have to learn a bunch of cryptic mnemonics).

You probably have two questions at this point: Why 15 and how can this ever work?

To describe one of 15 registers, one needs 4 bits. Three such register indices need 12 bits. I have only 16 bits for an instruction, so that should mean I can have only 16 different instructions. That's surely insufficient. Indeed it is. But I don't have 16 registers, only 15. That means, that in each of the three register-index fields, I can have a special value (I've used 0xf) to denote some alternate behavior.

One way to look at this is that these '0xf' fields 'escape' into an alternate instruction decode plane, allowing for more operations.

Program counter is not a register
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many many RISC architectures put the program counter into one of the general purpose register slots. Index 0 is a common choice. I started out that way as well, but soon realized that many operations just don't make sense on the PC. Other instructions make sense *only* with the PC.

The actual processor implementation also needs to treat PC differently from other registers: it is modified by every instruction, you might need a copy of it in the pre-fetch unit; you have to be careful with it when it comes to exceptions, etc. You also want to be able to easily identify branch instructions (anything that can change program order) if you ever want branch-prediction.

The compiler treats PC separately too; control flow is really different from data-flow, it's better not mixing the two.

Overall, I thought it is just better practice to have dedicated instructions for PC manipulation (a.k.a branches), with the added benefit of an extra general purpose register.

In Brew, there are two program counters, since there are two execution contexts. The one, called SCHEDULER mode has it's program counter (:code:`$spc`), the other, TASK mode has a different one, called :code:`$tpc`.

Execution contexts
------------------

Brew approaches privilege management differently from most processors. Normally, a processor would have several instructions that can only be executed in a privileged context. These include manipulating sensitive machine state, or changing things that could impact the OS-es ability to take control of the system, such as disabling interrupts.

In Brew we also have a privileged context: SCHEDULER mode and the non-privileged TASK mode. Instructions however have the same semantics in both. Process isolation needs to ensure two things:

* No TASK mode process should be able to influence the execution of any other TASK mode process.
* No TASK mode process should be able to influence the execution of SCHEDULER mode.

Notice that there is a slight asymmetry in this description: SCHEDULER mode is allowed to mock around with TASK mode, but not the other way around. By maintaining this asymmetry in the instruction set, and also elevating the existence of SCHEDULER and TASK mode into the user-visible machine state, we can achieve almost all of our goals.

Stack operations, or the lack of them
-------------------------------------

Most processors, in fact, all processors I know of have special instructions for stack management: you can push and pop values off of a stack, call subroutines and return from them. The problem with these operations is that they (the pops at least) modify two registers at the same time: the stack pointer and the register they popped.

The Brew architecture allows up to two register reads but only a *single* register write per instruction. This restriction is crucial if we wanted to use block-RAMs instead of flops for register-file implementation (either in FPGAs or in an ASIC).

The consequence? There are no stack operations in Brew. These instructions instead are replaced by short instruction sequences and helped by the following realization: modern programming languages and their compilers manage the stack in blocks. It's rare that a single-element push or pop operation is useful. Mostly the ABI requires adjusting the stack by a full call-frame worth of data. The overhead of the lack of push/calls and pop/return instructions is miniscule compared to all the work involved in setting up and tearing down these call-frames.

There are special, 16-bit load/store instructions that work with :code:`$r12` and :code:`$r13` as their base-register. These instructions, combined with the ABI that designates these two registers as the stack and frame pointer respectively makes code very compact, almost as compact as the ARM THUMB ISA. (Note that THUMB only supports 8 general purpose registers, we have 15, so we can handle register pressure better and generate less spills into the stack.)

Register types
--------------

Each register has a type associated with it. Types are changed and propagated by instructions. They can be loaded and stored independent of the values contained in the registers.

The dynamic typing of registers gives great flexibility ot the ISA and allows future growth and extensibility. There is however overhead associated with managing register types. This overhead is minimized if the compiler dedicates types to certain registers and keep them constant as much as possible.

.. note::

    Almost all processors have a similar concept for floating-point (and vector) registers: they have their own type, except this type is fixed. Brew in a way makes this partitioning of the register file more flexible, something that can be adapted to the needs of the application at hand.

Register types determine the semantics of many operations, especially in the unary, binary ALU groups and in conditional branches.

Type-less variant
-----------------

A type-less variant of the ISA is possible: in this case, all registers are assumed to have the type of INT32 and type-change instructions have no effect.

.. todo:: The compatibility story of the typeless subset is rather shaky. We need more thought on that!


Interrupts
~~~~~~~~~~

While working on the `Cray simulator <http://www.modularcircuits.com/blog/articles/the-cray-files/>`_, I came across an interesting implementation idea, one that I haven't seen in any modern processor: these machines didn't have an interrupt vector; they had different execution contexts. Crays had a way to save and load (almost) all registers when an interrupt occurred, changing the execution context automatically. I didn't go as far as these supercomputers did. I opted for just maintaining two PCs for two contexts: `$spc` for a context I call the SCHEDULER and another (`$tpc`) for the TASK context. PC (or `$pc`) is just a reference to one or the other, depending on the execution context. The operation is the following:

In SCHEDULER mode, interrupts are *always* disabled. You can't enable them. Period. There is a special instruction, that takes you into TASK mode (`stm`). This instruction simply sets the execution context to TASK mode, so execution continues from the current `$tpc`.

In TASK mode, interrupts are *always* enabled. You can't disable them. Period. Whenever an interrupt occurs, the processor switches back to SCHEDULER mode and continues from wherever `$spc` points to.

This is very confusing at first, because it appears that interrupts just get the processor to start execution from a seemingly random place. To understand what's going you have to think about how we entered TASK mode to begin with. The only way to enter TASK mode is to execute the `stm` instruction. Where does `$spc` point to when TASK mode execution starts? After this `stm` instruction. So, when the processor returns to SCHEDULER mode, it continues execution *after* the `stm` instruction. Pretty neat: `stm` works almost as a procedure call and TASK mode 'returns', whenever there's an event needing the attention of the SCHEDULER.

In practice, the SCHEDULER mode code is nothing but a ... well ... scheduler loop: it figures out the reason for the interrupt, finds the handler task for it, and enters TASK mode to 'call' then handler. This could involve switching to a different process (in the case of a timer interrupt in a multi-tasking machine) or entering for instance the keyboard driver in case of a keyboard interrupt. It's a very natural way of writing such code.

Exceptions and SW-generated interrupts (system calls, software break-points, what not) handled the same way: the TASK mode process is simply interrupted and execution is returned to SCHEDULER mode.

There of course needs to be a way to setup a task: there are instructions that can manipulate `$tpc` specifically. This is different from branch operations which work on `$pc`, that is the program counter of the executing context. These `$tpc` manipulation instructions of course also turn into branches if the processor happens to be in TASK mode, but they don't change execution order, if executed in SCHEDULER mode.

Privileged instructions
~~~~~~~~~~~~~~~~~~~~~~~

There are none. Normally, a processor would have several instructions that can only be executed in a privileged context. These include manipulating sensitive machine state, or changing things that could impact the OS-es ability to take control of the system, such as disabling interrupts.

In the BREW architecture SCHEDULER mode is assumed to have all the rights in the world: it can do anything. It's TASK mode that is limited. In particular, it's various processes in TASK mode that should have controlled ability to influence each other or the SCHEDULER mode environment.

There are two main avenues of interference that needs to be controlled. First: no TASK mode process should be able to influence the execution (the `$tpc`) of any other TASK mode process. Second, no TASK mode process should be able to influence the execution (the `$spc`) of SCHEDULER mode.

Both of these requirements are easily satisfied: there is just one `$tpc`. All other TASK mode processes have their context saved and restored by the SCHEDULER, so as long as that task state storage is not accessible to the TASK, it won't be able to influence it's content.

A TASK mode process also can't modify `$spc`, simply because there is no instruction to do so: instructions can modify `$tpc` or `$pc`. For a TASK mode process `$pc` *is* `$tpc`.

I like the idea that all instructions execute the same way with the same semantics in both TASK and SCHEDULER mode. There is merely this slight asymmetry in the instruction set that makes all the difference.



Stack operations
~~~~~~~~~~~~~~~~

Most processors, in fact, all processors I know of have special instructions for stack management: you can push and pop values off of a stack, call subroutines (in which case the PC gets pushed on the stack) and return from them. The problem with these operations is that they (the pops at least) modify two registers at the same time: the stack pointer and the register they popped.

The BREW implementation allows up to two register reads and a *single* register write per instruction. This restriction is crucial if we wanted to use RAMs instead of flops for register-file implementation (either in FPGAs or in an ASIC).

The consequence? There are no stack operations in BREW. This makes subroutine calls and returns a little bit more expensive then they would otherwise be, but only slightly. Under normal circumstances, the caller has to:

1. Save important caller-saved registers on the stack
2. Put argument values on the stack
3. Save return address in the link register.
4. Jump to the subroutine

At this point, the callee:

1. Sets up the stack-frame, but modifying the frame pointer
2. Save the link register value

On return the reverse needs to happen. This is several instructions and clock cycles, especially on a machine without (efficient) caches.

If no pushes and pulls are supported, really the only thing that needs to be included in the above list is the manual modification of the stack pointer. That's one extra instruction in the long instruction stream, something that doesn't touch memory, so comparatively light-weight.

There are special, 16-bit load/store instructions that work with `$r12` and `$r13` as their base-register. These instructions, combined with the ABI that designates these two registers as the stack and frame pointer respectively makes code very compact, almost as compact as the ARM THUMB ISA. (Note that THUMB only supports 8 general purpose registers, we have 14, so we can handle register pressure better and generate less spills into the stack.)