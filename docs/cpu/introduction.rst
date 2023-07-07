Introduction
============

Brew (as in home-brew) is an Instruction Set Architecture to explore a few ideas that have been rattling in my head for a while. It is a RISC-style design at its heart with a few twists.

..
  Brew has a 32-bit data-path with a variable instruction length. The smallest instructions are 16-bits long, the largest are (without prefixes) 48 bits. For an common instruction sequence, the average instruction length is around 24 bits.

  The ISA defines 15 general purpose registers plus a pair of program counters.

  Each register has a type associated with it. This type is set and propagated by instructions. While types can be loaded and stored in memory, they are not automatically maintained by regular data load and store instructions.

  The instruction set doesn't define any privileged instruction, yet process isolation (not yet virtualization) is fully supported, if paired with an appropriate memory management unit (MMU).

  Brew is supported by a port of BinUtils, GCC (C and C++ front-ends are tested) and NewLib. An instruction set simulator for user-mode processes is part of BinUtils. GDB is not yet ported to the architecture.


In the following you'll see what the third major iteration of the instruction set looks like. In it's current incarnation it is better in code-density then RISC-V, better then ARM, almost as good as THUMB. The first RTL implementation of the Brew architecture is the Espresso core, which is part of the Anachronistic Computer project.

So, what does this architecture look like? Let's get into its features in a little more detail.

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

