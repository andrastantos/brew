CPU (the story)
===============

For the anachronistic computer, we need a properly anachronistic processor. As it turns out, I just have the design! I have been toying around with an instruction set design lately: the BREW architecture.

What is BREW you might ask? It's my attempt at a RISC-like architecture. I had a few ideas that have been rattling around in my cranium for years, but have always put it off as too crazy a project to endevour on. You see, there are way too many ISAs in the wild already, why should I inflict another one on the world? Plus, it's not exactly trivial to lift an ISA off the ground: one needs an assembler. A linker. A C compiler, runtime libraries, a debugger, a simulator. Ideally other languages, such as C++ as well. This was a daunting list that terrified my. About a year ago, I said, what the hack, why don't I give it a shot? As it turns out, porting GCC (and binutils) to a new target wasn't all that difficult. In a couple of months I had pretty much all of the above accomplished. Except for GDB, the debugger. That I still didn't get around. This experience also allowed me to run some benchmarks (on an instruction-set simulator) and realize that my instruction set sucked in so many ways. I ended up re-writing the ISA three times, following up with all the toolset changes. At the end of the process though, I ended up with an ISA that was better in code-density then RISC-V, better then ARM, almost as good as THUMB.

So, what does this processor look like? It's a 32-bit processor, and as I said, it's RISC-like. There are a few (nowadays) unusual features, so let's get through them!

Instruction size
~~~~~~~~~~~~~~~~

My ISA violates (at least) one of the main tenants of RISC: the instructions are of a variable length. The base instruction set is 16-bits long, but that doesn't allow for room for (almost any) immediate values. This is a problem for almost all 32-bit ISA designs: if you want to be able to store 32-bit constants right in the instruction stream, you obviously need more than 32-bits per instructions to do so. If you don't, then, well, how do you describe 32-bit constants? You have two options and various RISC implementations do both: you either use multiple instructions to assemble a constant (for instance load the lower 16-bit, then the upper), or put the values somewhere in memory instead in the instruction stream and get them using a load instruction.

I didn't want to do either: multiple instructions seem wasteful (you end up using 64-bits of instruction space at least, provided you instructions are 32-bit long) and using loads is slow; using the pre-fetcher to get consecutive data for the instruction stream is about as efficient as you can get to interface to memory.

Given these considerations, I decided that constants must be part of the instructions and there must be a way to store 32-bit constants right then and there. That however means that some of my instructions are going to be longer than 32 bits, the next logical value being 48 bits. So, I ended up with three instruction lengths:

1. 16-bit instructions for everything not involving immediate constants
2. 32-bit instructions where a 16-bit immediate is sufficient
3. 48-bit instructions where a full 32-bit immediate is required

After experimentation, this picture got muddled a bit: I realized that there is good value in encoding really small immediates in 16-bit instructions, at least for certain often used operations.

Registers and their encoding
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ISA follows the traditional 3-register addressing of almost all modern RISC processors: there is a destination register and (up to) two source registers, each being described by individual fields in the instruction set. There are a total of 15 general-purpose registers, $r0 through $r14. So, an addition can be described as: `$r3 <- $r10 + $r12` for instance (yes, the assembly is algebraic, you don't have to learn a bunch of cryptic mnemonics).

You probably have two questions at this point: Why 15 and how can this ever work?

To describe one of 15 registers, one needs 4 bits. Three such register indices need 12 bits. I have only 16 bits for an instruction, so that should mean I can have only 16 instructions? That's surely insufficient. Indeed it is. But I don't have 16 registers, only 15. That means, that in each of the three register-index fields, I can have a special value (I've used 0xf) to denote an alternate behavior. Originally the idea was that an 0xf in a register index field means that the operand value should come from the immediate field after the instruction, instead of a register (see how variable instruction-length would come to be?) but later this idea got muddled quite a bit.

One way to look at this is that these '0xf' fields 'escape' into an alternate instruction decode plane, allowing for more operations. For instance, if the destination register field is set to '0xf', that surely shouldn't mean that the destination is an immediate field after the instruction. That value could be used to decode conditional branches for instance.

In the end, the 16-bit instruction is broken up into four 4-bit fields::

    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
    |    FIELD_D    |    FIELD_C    |    FIELD_B    |    FIELD_A    |
    +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

FIELD_E, if exists is the 16- or 32-bit immediate, following these fields.

Normally, FIELD_D denotes the destination register index, while FIELD_B and FIELD_A describes the source operand registers. As I said, things are more complicated than this, bit it gives you the basic idea.

Program counter is not a register
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many many RISC architectures put the program counter into one of the general purpose register slots. Index 0 is a common choice. I started out that way as well, but soon realized that many instructions just don't make sense on the PC (why would you ever want to XOR the PC with another register, or store the resultant value of a multiply in the PC?). Other instructions mostly make sense only with the PC and not with any other general-purpose register.

The actual processor implementation also needs to treat PC differently from other registers: it is modified by every instruction, you might need a 'copy' of it in the pre-fetch unit; you have to be careful with it when it comes to exceptions, etc. You also want to be able to easily identify branch instructions (anything that can change program order) if you ever want branch-prediction.

The compiler treats PC separately too; control flow is really different from data-flow, it's better not mixing the two.

Overall, I thought it is just better practice to have dedicated instructions for PC manipulation (a.k.a branches), with the added benefit of an extra general purpose register.

Interrupts
~~~~~~~~~~

While working on <<<<<<<<<< ADD LINK >>>>>>>>>> the Cray simulator, I came across an interesting implementation idea, one that I haven't seen in any modern processor: these machines didn't have an interrupt vector; they had different execution contexts. Crays had a way to save and load (almost) all registers when an interrupt occurred, changing the execution context automatically. I didn't go as far as these supercomputers did. I opted for just maintaining two PCs for two contexts: $spc for a context I call the SCHEDULER and another ($tpc) for the TASK context. PC (or $pc) is just a reference to one or the other, depending on the execution context. The operation is the following:

In SCHEDULER mode, interrupts are *always* disabled. You can't enable them. Period. There is a special instruction, that takes you into TASK mode (stm). This instruction simply sets the execution context to TASK mode, so execution continues from the current $tpc.

In TASK mode, interrupts are *always* enabled. You can't disable them. Period. Whenever an interrupt occurs, the processor switches back to SCHEDULER mode and continues from wherever $spc points to.

This is very confusing at first, because it appears that interrupts just get the processor to start execution from a seemingly random place. To understand what's going you have to think about how we entered TASK mode to begin with. The only way to enter TASK mode is to execute the `stm` instruction. Where does $spc point to when TASK mode execution starts? After this instruction. So, when the processor returns to SCHEDULER mode, it continues execution *after* the `stm` instruction. Pretty neat: `stm` works almost as a procedure call and TASK mode 'returns', whenever there's an event needing the attention of the SCHEDULER.

In practice, the SCHEDULER mode code is nothing but a ... well ... scheduler loop: it figures out the reason for the interrupt, finds the handler task for it, and enters TASK mode to 'call' then handler. This could involve switching to a different process (in the case of a timer interrupt in a multi-tasking machine) or entering for instance the keyboard driver in case of a keyboard interrupt. It's a very natural way of writing such code.

Exceptions and SW-generated interrupts (system calls, software break-points, what not) handled the same way: the TASK mode process is simply interrupted and execution is returned to SCHEDULER mode.

<<<<<<<<< ADD SIMPLE CODE FOR SCHEDULER MODE >>>>>>>>>

There of course needs to be a way to setup a task: there are instructions that can manipulate $tpc directly (as opposed to branch operations with work on $pc, which could be an alias to either $tpc or $spc).

Privileged instructions
~~~~~~~~~~~~~~~~~~~~~~~

There are none! Normally, a processor would have several instructions that can only be executed in a privileged context. These include manipulating sensitive machine state, or changing things that could impact the OS-es ability to take control of the system, such as disabling interrupts.

In the BREW architecture SCHEDULER mode is assumed to have all the rights in the world: it can do anything. It's TASK mode that is limited, in particular, it's various processes in TASK mode that should have controlled ability to influence each other or the SCHEDULER mode process.

There are two main avenues of interference that needs to be blocked. First: no TASK mode process should be able to influence the execution (the $tpc) of any other TASK mode process. Second, no TASK mode process should be able to influence the execution (the $spc) of SCHEDULER mode.

Both of these requirements are easily satisfied: there's just one $tpc that the SCHEDULER swaps in and out when it changes TASK mode execution context. Unless a TASK mode process can modify the storage location where the SCHEDULER stores another processes $tpc (which is an access right issue, discussed in a minute) there's no way for such influence to occur. A TASK mode process also can't modify $spc, simply because there is no instruction to do so: instructions can modify $tpc or $pc. For a TASK mode process $pc *is* $tpc, so there's no instruction to touch $spc. For the SCHEDULER mode process, $pc is an alias of $spc, so it can modify either that or $tpc.

It's feels wholesome that all instructions execute the same way with the same semantics in both TASK and SCHEDULER mode. It's this slight asymmetry in the instruction set (that there are instructions directly manipulating $tpc, but there are none to do the same with $spc) that makes all the difference.

Access rights
~~~~~~~~~~~~~

Now, on to access rights. The processor architecture doesn't really define any memory protection scheme, all it really does is to make sure that everything goes through whatever this external protection logic is. This includes CPU-specific CSR registers.

While the canonical way of dealing with access rights and protections is through a paging MMU, the first implementation of BREW doesn't have enough silicon area (remember, we try to work with 1.5um silicon process) to implement that. Thus, a much simpler protection scheme is used:

In TASK mode, every memory access is offset by a `base` register and checked against a `limit` register. This sets up a single, contiguous window in physical memory, that a the process can access. Anything below the `base` is inaccessible (no negative addresses are supported) and anything above the `limit` would generate an access violation exception. In reality, the situation is a bit more complicated: there are a pair of `base` and `limit` registers: one for instruction fetches and one for everything else.

In SCHEDULER mode, these registers are simply assumed to be 0, giving access to the whole physical address space without translation.

The aforementioned `base` and `limit` registers are implemented as CSRs. CSRs are mapped into the physical address space, starting 0x4000_0000, between ROMs and I/O (0x0000_0000) and DRAM (0x8000_0000). This region is normally not used by user code: they interact with HW (I/O) using drivers, don't have code in ROM so, they can have the `base` set to somewhere in DRAM.

Such a simple scheme has limitations. It is sufficient to protect user-mode processes from one another. However, drivers and OS components (which also run in TASK mode, but need access to certain I/Os and CSRs; they could also be partially in ROM) need compete open access to every HW resource: there's no way to be more granular about permissions. This is a problem in that drivers can crash the system.

Stack operations
~~~~~~~~~~~~~~~~

Most processors, in fact, all processors I know of have special instructions for stack management: you can push and pop values off of a stack, call subroutines (in which case the PC gets pushed on the stack) and return from them (when you pop the PC from the stack). The problem with these operations is that they (the pops at least) modify two registers at the same time: the stack pointer and the register they popped.

The BREW implementation allows *strictly* up to two register reads and a single register write per instruction. This setup allows for a single write port into the register file, something that's crucial if we wanted to use RAMs instead of flops for implementation (either in FPGAs or in an ASIC).

So, the solution is that there are no stack operations in BREW. This makes subroutine calls and returns a little bit more expensive then they would otherwise be, but only slightly. Under normal circumstances on a RISC processor, the caller has to:

1. Save important caller-saved registers on the stack
2. Put argument values on the stack
3. Save return address in the link register.
4. Jump to the subroutine

At this point, the callee:

1. Sets up the stack-frame, but modifying the frame pointer
2. Save the link register value

On return the reverse needs to happen. This is several instructions and clock cycles, especially on a machine without (efficient) caches.

If no pushes and pulls are supported, really the only thing that needs to be included is a single, manual modification of the stack pointer. That's one extra instruction in the long instruction stream, something that doesn't touch memory and thus significantly faster then what's already there.

I suppose one other thing that stack-operations help with is the automatic addressing. The BREW processor has register-offset indirect addressing for every load/store operation kind, which not only helps with subroutine calls but also very important for local variables as well as struct-member accesses. Supporting this is much better bang for the buck then the stack operations and either their multi-cycle behavior or second write port.

There are special, 16-bit load/store instructions that work with `$r0` and `$r1` as their base-register. These instructions, combined with the ABI that designates these two registers as the stack and frame pointer respectively makes code very compact, almost as compact as the ARM THUMB ISA. (Note that THUMB only supports 8 general purpose registers, we have 14, so we can handle register pressure better and generate less spills into the stack.)

Unimplemented features
~~~~~~~~~~~~~~~~~~~~~~

There are several features of the original BREW concept that the current implementation doesn't support. These are either things that I deemed too complex for the target technology (and era) or things that I'm on the fence on at the moment.

Memory model operations
-----------------------

In a more complex processor, especially in a multi-core system memory model is a big problem. Write queues, instruction and data-caches, out-of-order execution all mess with the real order of memory operations compared to the SW-apparent one. The BREW architecture has support for 'load-acquire' 'store-release' model of synchronization primitives. It has support for various fence instructions and cache-invalidation operations. None of this makes sense in a single-processor, in-order, cache-less processor, which this simple design is. So these operations either revert to regular loads and stores or just don't do anything.

Floating point operations
-------------------------

Floating point support would be nice, of course, but not within the silicon complexity constraints of the early '80s. This feature must go.

Multiple Load-Stores
--------------------

This is a new idea that I'm toying with: would it be too difficult to add a pair of (multi-cycle) instructions that could load and store any combination of registers? This goes against the RISC approach, it's clearly a complex concept. However, ARM has it and for good reason: it collapses function prologs and epilogs, results in very compact code and - if implemented properly - results in pretty nice memory access patterns with high efficiencies. I have found a right-sized hole in the instruction space to fit these instructions in, but the implementation complexity is rather high. I'm not yet sure if it's worth to have them, I probably need to model it first.

Register Types
--------------

This is probably the most controversial feature of BREW, something that I haven't seen in any other processor (maybe for good reasons). The idea is that along with every register value, the processor maintains the type of the data stored in that register. This type can be set by a set of instructions and - crucially - used by the processor to determine the semantics of various operations. For instance, the operation `$r4 <- $r5 + $r6` could mean an integer addition if `$r5` and `$r6` hold integer values, but the same bit-pattern can mean a floating-point addition if the source operand types set as such. There are many many corner-cases to be ironed out (what if `$r5` is a float and `$r6` is an integer?) but that is mostly just a set of decisions to be made.

Another big problem is that now on function entry/return not only register values, but their types will need to be saved and restored. I have instructions that can handle this, but the previously mentioned multiple load-store operations would shine in this aspect: they can handle the type load/store aspect right then and there.

Yet another problem is compiler support: I don't know how to explain this behavior to GCC: that it can use *any* register as a floating-point one, but really shouldn't: it should try to group operations and register-assignments by type: type-changes are extra instructions, so should be avoided. But now, the register-allocator would need to be type-aware.

Finally, there's the question of how to build a high(er) performance processor with this feature? You see, the problem is that the execution-unit selection can't be done until the source operand types are known. This on the surface would mean that out-of-order execution would be really difficult. The saving-grace though is this: the result type of an operation is known right when the source operand types are determined. So, even though the *values* of the registers might come several clock cycles later, the *type* of those values can be known immediately and scheduling of operations to execution units (and queues) can continue. The complexity of

Extension groups
----------------

There are several holes in the instruction set, that can be used to extend the ISA in the future. Some of these are already called for for more complex operations (linear interpolation is one example).




 I will use that, mostly because ... why not? It's a riff on a variable-instruction-length RISC architecture, which straddles the divide that started to emerge around that time in CPU architecture. In that sense it fits right in. It's also a 32-bit ISA with a 16-bit instruction encoding, something that would have been rather more appealing in those memory-constrained days. It highly depends on an MMU, which I don't think I can afford, so something more simplistic, probably a Cray-style base+limit-based protection scheme would need to be used. It also depends highly on memory-mapped I/O, which - as we will see - is good for pin-count reduction.

The ISA is described in isa.txt, but there are changes to be made for this core:
 - No fence or cache invalidation
 - No extension groups
 - No types, everything is INT32
 - No floating point ops (especially in unary group)
 - No type overrides loads or stores
 - No $rD <- sum $rA
 - No barrel shifter or multiplier: these are multi-cycle operations
 - No lane-swizzle
 - No synchronization (load-acquire; store-release) - these are probably simple regular load/stores

The implementation is also rather simplified:
 - No iCache or dCache. An instruction buffer would be nice, but maybe not even that
 - No decoupled front-end
 - No store-queue
 - No re-order queue (multi-cycle instructions stall)

Now on to pinout: can we fit this 32-bit micro into only 40 pins? We of course can't afford a 32-bit external bus, but how about 16-bits? That would play nicely with the instruction set: most of the instructions are either 16-bits long or 16-bits, followed by another 16-bit constant field.

One thing that annoyed me a lot every time I looked at schematics of these early machines was the interface to DRAM. When I tried to design my own, I also have found the problem very annoying. Now, looking back, it's not only that: it's also very inefficient. Since the muxing of the address bus required two cycles, but since it was almost exclusively done with discrete logic, there was no advantage to reading adjacent words. This was probably fine in the C64 era when memory was so much faster then either video or CPU, but certainly not in the 16- or 32-bit era. Amiga needed two banks of memory to get around the problem. The Macintosh could only really support black-and-white graphics. Yet, many processors (Intel, I'm looking at you) *did* have a multiplexed bus. It's just that they multiplexed data and address on top of each other. So, what if we've multiplexed addresses on top of each other, exactly as DRAM would need it? It would not only reduce pin-count on the CPU (or any bus-master, really) but would also make it possible to directly attach DRAM to these devices. So, how would it work?

Let's say we have the following address-bus muxing:

======== =========== ============
Pin      First cycle Second cycle
======== =========== ============
A8_0     A8          A0
A9_1     A9          A1
A10_2    A10         A2
A11_3    A11         A3
A12_4    A12         A4
A13_5    A13         A5
A14_6    A14         A6
A15_7    A15         A7
A17_16   A17         A16
A19_18   A19         A18
======== =========== ============

This allows for the use of 64kbit DRAMs all the way up to 4Mbit devices. That really carries us through the '80s: the 16Mbit DRAM was introduced in '91. If our little line of machines was still alive by then, we would certainly have revved the CPU for something more capable with more pins, most likely with the full 32-bit address bus exposed. So this is fine.

The external address space is 8MByte, but only 4MByte is available (directly) for DRAMs in two banks. That would work for 8 chips of 1Mbitx4 configuration, or even a single 1Mbitx16 chip.

The full pin-list is as follows:

========== ======== ===========
Pin Number Pin Name Description
========== ======== ===========
1          A8_0     Multiplexed address bus
2          A9_1     Multiplexed address bus
3          A10_2    Multiplexed address bus
4          A11_3    Multiplexed address bus
5          A12_4    Multiplexed address bus
6          A13_5    Multiplexed address bus
7          A14_6    Multiplexed address bus
8          A15_7    Multiplexed address bus
9          A17_16   Multiplexed address bus
10         A19_18   Multiplexed address bus
11         D0       Data bus
12         D1       Data bus
13         D2       Data bus
14         D3       Data bus
15         D4       Data bus
16         D5       Data bus
17         D6       Data bus
18         D7       Data bus
19         D8       Data bus
20         D9       Data bus
21         D10      Data bus
22         D11      Data bus
23         D12      Data bus
24         D13      Data bus
25         D14      Data bus
26         D15      Data bus
27         nRAS_B0  Active low row-select, bank 0
28         nRAS_B1  Active low row-select, bank 1
29         nLCAS    Active low column select, lower byte
30         nUCAS    Active low column select, upper byte
31         nNREN    Active low non-DRAM bus cycle qualifier
32         nWE      Active low write-enable
33         CLK      Clock input
34         nRST     Active low reset input
35         nINT     Active low interrupt input
36         nBREQ    Active low bus-request input
37         nBGRANT  Active low bus-grant output
38         nWAIT    Active low wait-state input
39         VCC      Power input
40         GND      Ground input
========== ======== ===========

To meet timing requirements on the DRAM interface, DRAM chips *directly* interfaced to the processor. No address decode, no latches, no buffers can be in between,

For other devices on the bus, `nLCAS` and `nUCAS` can still work as a byte-select/enable signal. We need another RAS-style qualifier to know that we need to latch the address and start decoding. That's `nNREN` above.

To fit in the 40-pin package, we needed to limit the addressable memory quite a bit. This is not a problem for an early '80-s machine, but for the next iteration (and FPM DRAM support) we will have to go up to a 44-pin package. This allows:

1. Two extra address lines to support 4Mx1 or even 16Mx1 devices
2. Two extra nRAS_Bx signals to support two extra banks

These changes allow to support up to 32MBytes of RAM per bank for a total of 128MByte RAM.

DRAM decode
~~~~~~~~~~~

To support various DRAM sizes, the address decode regions for nRAS_Bx needs to be programmable. They all are qualified by A31, that is they belong to the upper 2GB of the total address space. However, which address bits are used to select between nRAS_Bx has to be programmable, otherwise it can't be guaranteed that DRAM banks create a contiguous space.

This programming can be done at boot time, while testing for memory sizes: the default decode should allow for very large DRAM banks, and by testing for aliasing, the right boundary can be selected.

.. note::
    The same programmability needs to exist in the DMA controller too.

Wait states
~~~~~~~~~~~

The CPU has three programmable address regions:

=============  ===========  ===========
Start address  End address  Description
=============  ===========  ===========
0x0000_0000    0x0003_ffff  ROM space
0x0004_0000    0x0007_ffff  I/O spaces
0x8000_0000    0xffff_ffff  DRAM space
=============  ===========  ===========

For each of these I/O spaces, a different number of wait-states can be programmed as a 4-bit value. The value 0 means 15 wait-states, other wise value N means N-1 wait-states. The register resets to 0.

Generations
-----------

Generation 1
~~~~~~~~~~~~

Very simple, 5- or 6-stage pipeline. No caches, maybe not even branch-prediction. If anything, everything is predicted not taken, i.e. straight line speculative execution. No write buffer, every memory access is stalling. Multiplies could be multi-cycle, if exist at all. Maybe even barrel-shifter is multi-cycle.

Integer-only ISA with no extension groups or prefix instructions.

The 6th stage (if needed) is there to make instruction decode close timing.

No MMU, only offset/length-based memory protection.

Target frequency is ~10MHz.

16-bit external bus.

Virtual market introduction ~'83.

Generation 2
~~~~~~~~~~~~

I think the most important improvement is going to be a very small iCache (maybe direct-mapped 1kB or something rather trivial) and a full MMU.

Target frequency is ~20MHz.

Maybe write-queues are making an appearance.

Support for FPM DRAM.

Virtual market introduction ~'86.

Generation 3
~~~~~~~~~~~~

32-bit external bus, introduction of DCache, probably more capable ICache. External bus is PCI-like, multiplexed 32-bit address-data. If possible, actually PCI.

Actually, PCI is a '92 thingy, so probably would be too early for this processor.

Memory controller goes off-chip, but adds EDO support. <-- this puts is to ~'95, so this is too early for that as well.

Write queues.

More adept branch-prediction.

Maybe types are introduced to support floating points. Still no vector ISA.

Not sure, but maybe de-coupled front-end?

Target frequency is ~33MHz

Virtual market introduction ~'90

Generation 4
~~~~~~~~~~~~

Memory controller moves back into processor, external bus remains PCI for peripherals only. PC100 SDRAM support <-- this puts us to '93.

De-coupled front-end, updated caches (probably write-back DCache).

Maybe introduction of some sort of coherency protocol for multi-processor systems.

Maybe introduction of vector types.

Re-order queues at the back-end, creation of independent execution units.

Target frequency is ~150MHz core, 33MHz front-end bus.

Virtual market introduction ~'93

Comparison
~~~~~~~~~~

A bit old, but a good idea for pricing of processors in the era:

http://www.bitsavers.org/components/edn/EDN-4th-annual-microprocessor-directory-Nov20-1977.pdf

8080: $10, 8085: $20
6502: $10
6800: $20
PIC1650/1655/1670: $20 (2500) $4 (100k)
Z80: $20

MIPS comparison
~~~~~~~~~~~~~~~

Source: https://en.wikipedia.org/wiki/Instructions_per_second

==============   ========   =========
Chip             Year       MIPS/MHz
==============   ========   =========
Intel 8088       1979       0.075
MC68000          1979       0.175
Intel 80286      1982       0.107
MC68010          1984       0.193
MC68020          1984       0.303
Intel 80386      1985       0.134
ARM2             1986       0.5
MC68040          1987       0.36
Intel 80486      1989       0.3
==============   ========   =========

I'm currently standing at 0.23, slightly better then the competition, but not enormously.

The *main* reason for us not being faster is that the memory bus is busy. At least that's what it appears to be.
Now, that's not to say, it's doing useful work: we might be constantly fetching stuff that we'll discard.

Synthesis results
~~~~~~~~~~~~~~~~~

Now that the V1 design is more or less complete, here are some stats:

Using the OpenRoad toolchain and sky130hd PDK, the core area is 0.16mm^2.

============== =============    ========  ==========================================================================================
Core die area   Fmax             Node      Comparison (source: https://en.wikipedia.org/wiki/Transistor_count#Transistor_density)
============== =============    ========  ==========================================================================================
0.16mm^2        100MHz           130nm
21mm^2          8.6MHz           1.5um     49mm^2 for 80286
36mm^2          6.5MHz           2um
85mm^2          4.3MHz           3um       60mm^2 for 80186; 33mm^2 for 8088
============== =============    ========  ==========================================================================================

According to http://www.bitsavers.org/components/rockwell/Trends_in_Microcomputer_Technology_1977.pdf people estimated 40,000mil^2 (62mm^2) dies to be economical in the early '80s. This is to say, that this processor would be rather cheap, if manufactured in 1.5 or 2u process nodes. 3u is not really feasible not just for die-size, but for speed reasons as well: 8-10MHz processors all only appeared in the 1.5u node. 3u node manufacturing tapped out at around 5MHz; too slow for our needs.

Timing-wise, the design seems to be closing at 100MHz (though I'm not quite sure about my constraints) at 130nm. If that's true, we are on target to hit about 8MHz in 1.5u. FPGA-based timing closure is all over the map, making me nervous about the accuracy of these results.

IO cells are apparently missing from the sky130 PDK. The gf180 PDF has them. Here's some data:

https://gf180mcu-pdk.readthedocs.io/en/latest/IPs/IO/gf180mcu_fd_io/features.html#cell-dimensions

Bond-pad guidlines are here:

https://gf180mcu-pdk.readthedocs.io/en/latest/physical_verification/design_manual/drm_09_2.html

From these, I'm guessing that a basic I/O pad is 350x75um large. My expectation is that this includes the bond-pad and that these sizes
won't change all that much with technology. This is a rather standard size, including power pins as well.

So, a 40-pin package would need 750x350um I/O region on each side. The chip would be 1350um x 1350um, the total I/O area (with corners) is 1.4mm^2. The core area is 0.56mm^2.

Our little core in 130nm would be totally I/O limited, but in our target node, I/O is a rounding error: the chip is totally core-limited.

https://lnf-wiki.eecs.umich.edu/wiki/Wire_bonding confirms that ~60ux60u bond pads are OK (they claim 75x75, but oh, well).

RAMs
~~~~

I finally have found a RAM example for the sky130 SDK: it's a 32x1024bit RAM (single-ported, 6T cells).

https://github.com/ShonTaware/SRAM_SKY130#openram-configuration-for-skywater-sky130-pdks

It's size is 0.534mm^2, closes timing at about 80MHz. Back-scaling it to 1.5u, gives us a scaling factor of 133:1.

Taking all of this, gives us 71mm^2 for this 32kbit SRAM or 0.00217mm^2/bit.

What if we wanted to add a 1kByte ICache to the system? That would take 17.78mm^2, just for the SRAM array. In other words, we can expect our die-area to double even with a single kB of ICache. So, no ICache for sure!