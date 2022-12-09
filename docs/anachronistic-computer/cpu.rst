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
A16_17   A16         A17
A19_18   A19         A18
A21_20   A21         A20
A22      A22         A22
======== =========== ============

This allows for the use of 64kbit DRAMs all the way up to 4Mbit devices. That really carries us through the '80s: the 16Mbit DRAM was introduced in '91. If our little line of machines was still alive by then, we would certainly have revved the CPU for something more capable with more pins, most likely with the full 32-bit address bus exposed. So this is fine.

The external address space is 16MByte, but only 8MByte is available (directly) for DRAMs. That would work for 16 chips of 4Mbitx1 configuration, or even 4 chips of 16Mbitx4 configuration. 

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
9          A16_17   Multiplexed address bus
10         A19_18   Multiplexed address bus
11         A21_20   Multiplexed address bus
12         A22      Highest order address bit
13         D0       Data bus
14         D1       Data bus
15         D2       Data bus
16         D3       Data bus
17         D4       Data bus
18         D5       Data bus
19         D6       Data bus
20         D7       Data bus
21         D8       Data bus
22         D9       Data bus
23         D10      Data bus
24         D11      Data bus
25         D12      Data bus
26         D13      Data bus
27         D14      Data bus
28         D15      Data bus
29         nRAS     Active low row-select
30         nLCAS    Active low column select, lower byte
31         nUCAS    Active low column select, upper byte
32         nWE      Active low write-enable
33         CLK      Clock input
34         nRST     Active low reset input
35         nINT     Active low interrupt input
36         nBREQ    Active low bus-request input
37         nBGRANT  Active low bus-request output
38         nWAIT    Active low wait-state input
39         VCC      Power input
40         GND      Ground input
========== ======== ===========

