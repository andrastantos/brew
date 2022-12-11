Gate count estimations
======================

Let's look at the internals! Here are some relevant transistor counts:

- '75:  6502    3510 transistors
- '79: Z8000   17500 transistors 16 16-bit registers
- '78: i8086   29000 transistors 13 16-bit registers
- '79: 68000   40000 transistors 16 32-bit registers
- '82: i80186  55000 transistors
- '82: i80286 134000 transistors
- '85: ARM1    25000 transistors
- '86: ARM2    27000 transistors
- VIC II - 13000 transistors
- Amiga Denise - 20000 transistors
- Amiga AA+ - 100000 transistors

The prevailing tech node was 2 or 3um.

Actually, there's a pretty good table here:
https://en.wikipedia.org/wiki/Transistor_count

So, let's say I can afford around 50000 transistors in NMOS, which would translate to about 25000 gates.

That would be roughly (according to Digikey) an XC4028; XC4044 or an EPF6024. An Actel (Microchip) A54SX16 is also about the same size. These all clock in at around 1500 LUTs. Nothing gets even remotely close to them in size these days. Even the smallest Cyclone III has 5000 LEs. Which just goes to show how constrained these designs would need to be. Nevertheless, a straightforward 3-stage (or 5-stage) RISC pipeline should fit as the ARM1/2 cores show. The smallest MAX10 device is in this category as well (2000 LEs), but one can't use any of the on-chip RAMs or DSP blocks, cause that would be cheating. The iCE40UL-1k XO2-2000, MachXO3L-2100, AGL030 all seem to be in the same league. The AGL030 is old enough apparently to be 5V tolerant too.

Of course I can't make ASICs, but how would I compare what I have to what was achievable back then?

So, Google (https://opensource.googleblog.com/2022/10/announcing-globalfoundries-open-mpw-shuttle-program.html) has an open-source process for 0.18u. They have some example projects as well: https://colab.research.google.com/github/chipsalliance/silicon-notebooks/blob/main/digital-inverter-openlane.ipynb

There is also https://github.com/TinyTapeout/tinytapeout-02 which might have a usable PDK + back-end flow or at least something that gets me a gate-count estimate (really I only need the front-end for that).

This thing: https://datasheet.datasheetarchive.com/originals/crawler/xfab.com/a3edb8140abfda7a83325589538d6415.pdf seems to indicate that fmax for a 0.18u flow about 40GHz for P-channel and about 70GHz for N-channel devices (fT is 15 and 50GHz respectively).

https://www.researchgate.net/publication/320841806_History_and_Evolution_of_CMOS_Technology_and_its_Application_in_Semiconductor_Industry Shows some history.

- 1980: 2u   -  50k Transistors
- 1983: 1.5u - 100k Transistors
- 1985: 1u   - 500k Transistors

https://en.wikichip.org/wiki/1.5_%C2%B5m_lithography_process shows a bunch of different process nodes.

There's this too: http://www.fulviofrisone.com/attachments/article/466/CRC,.The.VLSI.Handbook.(2007),.2Ed.Spy.%5B084934199X%5D.pdf

But everything is way too new.

So, let's go with the following:
- 2u process was the rage around the turn of the '70-s ('79-'80). Typical part would be the 68020 @ 12.5 ... 33MHz clock rates and 100,000 transistors. The CVAX was also on this process with 12.5MHz clock rates. ARM2 was here too with 8...12MHz clock rates.
- 1.5u process was available from '81...'85. The 80386 (16...33MHz) would be a typical part but the 80286 (4...25MHz) was one as well. ARM3 clocks in between 20 and 36MHz, but that's '89
- 1.3u around '87 featured the 68030 (16...50MHz) and the MB86900 (16.6MHz)

So, I think it's safe to assume that 20MHz clock rate would be pushing the tech of the early '80s, but would be no problem for the later years of the decade. Comparing this with the selected :ref:`clock rates<clock_rates>` we are in sync with what DRAMs would have dictated anyways.

