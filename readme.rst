Brew: an Introduction
=====================

Brew (as in home-brew) is an Instruction Set Architecture to explore a few ideas that have been rattling in my head for a while. It is a RISC-style design at its heart with a few twists.

Brew has a 32-bit data-path with a variable instruction length. The smallest instructions are 16-bits long, the largest are 64 bits. For an common instruction sequence, the average instruction length is around 24 bits.

The ISA defines 15 general purpose registers plus a pair of program counters. An instruction can specify up to two source registers and a single destination register.

Each register has a type associated with it. This type is set and propagated by instructions. While types can be loaded and stored in memory, they are not automatically maintained by regular data load and store instructions.

In this documentation you'll find what is the fourth major iteration of the architecture. In its current incarnation it is better in code-density then RISC-V, better than ARM, almost as good as THUMB. The first RTL implementation of the Brew architecture is the Espresso core, which is part of the Anachronistic Computer project.

The type-less subset of Brew is supported by a port of BinUtils, GCC (C and C++ front-ends are tested) and NewLib. An instruction set simulator with GDB support is part of BinUtils.

Instruction size
----------------

Brew uses instructions of variable length. While most RISC implementations chose a fixed (usually 32-bit) instruction length, this poses a major problem: how to represent immediate values? 32-bit constants in the instruction stream obviously need more than 32-bit instructions. You can chose to only support smaller range of values, but then, how do you describe 32-bit constants? You have two options and both are employed by various RISC implementations: you either use multiple instructions to assemble a constant (for instance load the lower 16-bit, then the upper), or put the values somewhere in memory instead of the instruction stream and get them using a load instruction.

I didn't want to do either: Multiple instructions burn a lot of extra bits and extra loads are slow; Instruction pre-fetch is about as efficient as you can get in terms of getting bits from memory.

One could argue that most constants are of small values, so the fact that loading a full 32-bit value is slow is not all that important. However, that's only true until you consider branch targets: these are 32-bit in size (barring alignment considerations) and appear rather frequently.

Given these considerations, I decided that constants must be part of the instructions and there must be a way to store 32-bit constants directly. That however means that some of my instructions are going to be longer than 32 bits, the next logical value being 48 bits. So, I ended up with three instruction lengths:

1. 16-bit instructions for everything not involving immediate constants
2. 32-bit instructions where a 16-bit immediate is sufficient
3. 48-bit instructions where a full 32-bit immediate is required

After experimentation, this picture got muddled a bit: I realized that there is good value in encoding really small constants in 16-bit instructions, at least for certain often used operations.

Later, longer (64-bit) instructions also appeared as 'extension groups' and 'prefix operations'. These provide escape hatches for further expansion of the instruction set.

Registers and their encoding
----------------------------

The ISA follows the traditional 3-register addressing of almost all modern RISC processors: there is a destination register and (up to) two source registers. There are a total of 15 general-purpose registers, $r0 through $r14. An addition can be described as: :code:`$r3 <- $r10 + $r12` for instance (yes, the assembly is algebraic, you don't have to learn a bunch of cryptic mnemonics).

You probably have two questions at this point: Why 15 and how can this ever work?

To describe one of 15 registers, one needs 4 bits. Three such register indices need 12 bits. I have only 16 bits for an instruction, so that should mean I can have only 16 different instructions. That's surely insufficient. Indeed it is. But I don't have 16 registers, only 15. That means, that in each of the three register-index fields, I can have a special value (I've used 0xf) to denote some alternate behavior.

One way to look at this is that these '0xf' fields 'escape' into an alternate instruction decode plane, allowing for more operations.

The original intent was that this 'escape' code denotes that the corresponding source value is an immediate one. The ISA over time become considerably more complex in its use of the '0xf' field code, but you can still see this general tendency in a lot of places.


Program counter is not a register
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many many RISC architectures put the program counter into one of the general purpose register slots. Index 0 is a common choice. I started out that way as well, but soon realized that many operations just don't make sense on the PC. Other instructions make sense *only* with the PC.

The processor implementation (sometimes called the micro-architecture) also needs to treat PC differently from other registers: it is modified by every instruction, you might need a copy of it in the pre-fetch unit; you have to be careful with it when it comes to exceptions, etc. You also want to be able to easily identify branch instructions (anything that can change program order) if you ever want branch-prediction.

The compiler treats PC separately too; control flow is really different from data-flow, it's better not mixing the two.

Overall, I thought it is just better practice to have dedicated instructions for PC manipulation (a.k.a branches), with the added benefit of an extra general purpose register.

In Brew, there are two program counters, since there are two execution contexts. The one, called SCHEDULER mode has it's program counter (:code:`$spc`), the other, TASK mode has a different one, called :code:`$tpc`. When an instruction refers to :code:`$pc`, it just means it's working with the context-appropriate program counter. But what are these execution contexts? I'm glad I asked.

Execution contexts
------------------

Brew approaches privilege management differently from most processors. Normally, a processor would have several instructions that can only be executed in a privileged context. These include manipulating sensitive machine state, or changing things that could impact the OS-es ability to take control of the system, such as disabling interrupts.

In Brew we also have a privileged context: SCHEDULER mode. It's counter-part is the non-privileged TASK mode. Instructions however have the same semantics in both.

How does this work? User-mode (and many system-level) processes run in TASK mode. To ensure appropriate process isolation, we need two things:

* No TASK mode process should be able to influence the execution of any other TASK mode process.
* No TASK mode process should be able to influence the execution of SCHEDULER mode.

Notice that there is a slight asymmetry in this description: SCHEDULER mode is allowed to mock around with TASK mode, but not the other way around. By maintaining this asymmetry in the instruction set and also elevating the existence of SCHEDULER and TASK mode into the user-visible machine state, we can achieve almost all of our goals.

A large chunk of the isolation is achieved through memory management, which is outside of the scope of the ISA. It suffice to say here that - with a rather traditional MMU implementation - memory isolation, even I/O isolation, can be achieved as long as TASK mode applications aren't allowed to change page table entries.

The second part of the problem revolves around the two program counters mentioned in the previous chapter. If instructions only manipulate :code:`$pc` (i.e. the context-appropriate program counter), TASK mode processes can't influence SCHEDULER mode execution. Adding instructions that explicitly act on :code:`$tpc` adds the capability for SCHEDULER mode programs to change (set up, really) TASK mode processes. The lack of instructions explicitly operating on :code:`$spc` brings about the slight asymmetry that's needed to achieve isolation. What is important to note here is that semantics of these instructions are the same in both TASK and SCHEDULER mode; there really isn't any concept of privilege here. It's just that instructions operating explicitly on :code:`$tpc` execute a branch in TASK mode (since :code:`$tpc` *is* the program counter) while they don't alter program execution flow in SCHEDULER mode. Privilege emerges from more basic concepts.

Finally, there need to be a way to change execution contexts. There is a special instruction (:code:`stm`) that takes the processor to TASK mode: it starts execution from the current value of :code:`$tpc`. Naturally, in TASK mode (where we're already using :code:`$tpc` for execution), this instruction is a no-op. Only interrupts and exceptions (including software interrupts, such as system calls) can take the processor from TASK mode to SCHEDULER mode.

Interrupts
~~~~~~~~~~

Brew uses the SCHEDULER and TASK-mode execution contexts for managing interrupts as well. The operation is the following:

In SCHEDULER mode, interrupts are *always* disabled. You can't enable them. Period. The only way to enable interrupts is to return to TASK mode, using the :code:`stm` instruction.

In TASK mode, interrupts are *always* enabled. You can't disable them. Period. Whenever an interrupt occurs, the processor switches back to SCHEDULER mode and continues from wherever :code:`$spc` points to.

This might be confusing at first, because it appears that interrupts just get the processor to start execution from a seemingly random place. To understand what's going on you have to think about how we entered TASK mode to begin with. The only way to enter TASK mode is to execute the :code:`stm` instruction. Where does :code:`$spc` point to then when TASK mode execution starts? After the :code:`stm` instruction. So, when the processor returns to SCHEDULER mode, it continues execution *after* the :code:`stm` instruction. Pretty neat: :code:`stm` works almost as a procedure call and TASK mode 'returns', whenever there's an event needing the attention of the SCHEDULER.

In practice, the SCHEDULER mode code is nothing but a ... well ... scheduler loop: it figures out the reason for the interrupt (or exception), finds the handler task for it, and enters TASK mode to 'call' the handler. This could involve switching to a different user process (in case of a timer interrupt in a multi-tasking machine for example) or entering for instance the keyboard driver in case of a keyboard interrupt. It's a very natural way of writing such code.

Exceptions and software-generated interrupts (system calls, software break-points, what not) handled the same way: the TASK mode process is simply interrupted and execution is returned to SCHEDULER mode.


Stack operations, or the lack of them
-------------------------------------

Most processors, in fact, all processors I know of have special instructions for stack management: you can push and pop values off of a stack, call subroutines and return from them. The problem with these operations is that they (the pops at least) modify two registers at the same time: the stack pointer and the register they popped.

The Brew architecture allows up to two register reads but only a *single* register write per instruction. This restriction is crucial if we wanted to use block-RAMs instead of flops for register-file implementation in FPGAs. (In ASICs similar limitations are imposed by many memory-compilers.)

The consequence? There are no stack operations in Brew. These instructions instead are replaced by short instruction sequences and helped by the following realization: modern programming languages and their compilers manage the stack in blocks. It's rare that a single-element push or pop operation is useful. The ABI requires adjusting the stack by a full call-frame worth of data. The overhead of the lack of push and pop instructions is miniscule compared to all the work involved in setting up and tearing down these call-frames.

There are special, 16-bit load/store instructions that work with :code:`$r12` and :code:`$r13` as their base-register. These instructions, combined with the ABI that designates these two registers as the stack and frame pointer respectively makes code very compact, almost as compact as the ARM THUMB ISA. (Note that THUMB only supports 8 general purpose registers, while Brew has 15; It can handle register pressure better and generate less spills into the stack.)

.. note:: Push and pop operations however will eventually find their way into the ISA. This has to do with register types, more specifically types where the effective length of a register depends on its type. When such types are introduced to the ISA, pop and push operations will have to be implemented along with a few other features. These operations are going to be complex, multi-cycle beasts (closer to ARMs `STM and LDM <https://developer.arm.com/documentation/dui0068/b/Writing-ARM-and-Thumb-Assembly-Language/Load-and-store-multiple-register-instructions/ARM-LDM-and-STM-instructions>`_ instructions) so the register file access constraints can easily be worked around. The need for them revolves around the fact that function prolog and epilog doesn't know the effective register length of the caller registers and thus either have to make very pessimistic assumptions or need ISA support for context saving and restoration.

Calls
-----

Many many RISC processors use a link register to implement calls: a set of instructions are provided that save the program counter into this link register before changing the content of the program counter. Returning from the callee can be achieved by simply reloading the program counter from the link register.

While I don't see a tremendous benefit in having these instructions, they are not expansive to implement in a processor: for branch operations the register write-back port is not utilized, so the previous value of :code:`$pc` can be easily presented there.

Brew, thus follows the same time-tested tradition and supports call operations using the link register :code:`$r14`.

Register types
--------------

Each register has a type associated with it. Types are changed and propagated by instructions. They can be loaded and stored independent of the values contained in the registers.

Register types determine the semantics of many operations, especially in the unary, binary ALU groups and in conditional branches. For instance, the operation: :code:`$r0 <- $r1 + $r2` might mean an integer addition if the types associated with :code:`$r1` and :code:`$r2` are integer, or a floating point addition if the types are set as such. When the result is written into :code:`$r0` the type of it is also set accordingly.

.. note::

    Almost all processors have a similar concept for floating-point (and vector) registers: they have their own type, except this type is fixed. Brew in a way makes this partitioning of the register file more flexible, something that can be adapted to the needs of the application at hand.

Type management is probably the most complex and most controversial part of the ISA.

The dynamic typing of registers gives great flexibility ot the ISA and allows future growth and extensibility. There is however overhead associated with managing register types as register types need to be maintained.

**There are several problems to contend with on the architecture level**:

Complexity comes in the form of function calls: a new vulnerability (or just a source of bugs) opens up when functions are called with incorrect or unexpected register types assigned to the registers holding function parameters. Functions need a way to quickly check against type-expectations.

Another complexity arises when dealing with callee-saved registers: these are registers that the caller assumes to retain their values through a function-invocation. In a type-aware ISA, not only the register values, but their type needs to be retained. The callee cannot make any assumptions about the type of the register, so, if the register is to be reused in the callee, it must:

#. Save the original register value in the function prolog
#. Save the original register type in the function prolog
#. Change the register type to the desired one in the function body
#. Restore the original register type in the function epilog
#. Restore the original register value in the function prolog

Special bulk type load and store operations are provided to deal with these extra complexities in an efficient manner; still the overhead is non-zero.

Lastly - though it doesn't apply for the current Brew ISA - there could be register types that impact the effective size of a register. Think about how vector operations, especially a scalable vector extension, would be implemented! Suddenly, if a register is of 'vector of 4 32-bit integer' type, it's effective length is 128 bits. However, if it's just an 'integer', it's a mere 32 bits long. The callee now faced with the problem of not only not knowing the type of the register to be saved, but it's size either. Conservative assumptions (all registers are as large as they can possibly be) results in a lot of extra memory operations that are mostly unnecessary. Type-dependent function prologs and epilogs - if implemented in SW - are slow and convoluted. Lastly, the stack space required to store the registers during function prolog is not known at compile time, which means that stack-offsets cannot be statically computed. All this results in a need for complex context saving and restoring instructions. (Similar problems exist in all context switching use-cases, including when TASK-mode contexts are swapped by the SCHEDULER.)

**Implementation of all these instructions adds complexity of course but a micro-architecture needs to deal with another unique challenge**:

Instruction decode becomes complex. Before an instruction can be dispatched to an execution unit, it's operand types need to be determined. These types might depend on in-flight instructions, generating new scheduling hazards. However, for most instructions (type load/stores are an obvious exception) the result type can be trivially determined in a single cycle, during instruction issue. Thus, while there's more implementation complexity for sure, no true performance penalty is involved.

**The compiler for such an architecture needs some special behavior**:

Since there is overhead associated with changing the type of the register, optimal code-generation requires the compiler to group variables of the same type into the same set of registers in the register allocator. This type-awareness in the register allocator is something I'm still exploring: the current toolchain thus doesn't support types


Type-less variant
-----------------

Due to all the complexities listed above, I made sure that a type-less variant of the ISA is possible: in this case, all registers are assumed to have the type of INT32 and type-change instructions have no effect.

It is important to note though that code compiled for the type-less variant is not inter-operable with code for a typed subset, at least not at the moment: function prologs and epilogs are not type-aware. This can later be mitigated by special calling conventions for non-type-aware functions, but that feature is not implemented yet in the toolchain.

More documentation
------------------

The full documentation for Brew is checked in into this repository, but can be viewed `here <http://htmlpreview.github.io/?https://github.com/andrastantos/brew/blob/main/docs/html/index.html>`_ as well.
