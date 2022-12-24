CPU
===

As it turns out, I have a rather interesting little ISA laying around that I've been toying with: the BREW architecture. I will use that, mostly because ... why not? It's a riff on a variable-instruction-length RISC architecture, which straddles the divide that started to emerge around that time in CPU architecture. In that sense it fits right in. It's also a 32-bit ISA with a 16-bit instruction encoding, something that would have been rather more appealing in those memory-constrained days. It highly depends on an MMU, which I don't think I can afford, so something more simplistic, probably a Cray-style base+limit-based protection scheme would need to be used. It also depends highly on memory-mapped I/O, which - as we will see - is good for pin-count reduction.

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
