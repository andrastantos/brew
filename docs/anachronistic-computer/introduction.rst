Introduction
============

As I'm getting old, I'm thinking more and more of the past. I grew up in the '70 and '80s. My childhood computer was a VIC20, followed by a C64. I was dreaming about owning an Amiga, but never had the money to have one. I admired the Mac and of course was fascinated by the Cray machines.

And I was constantly dreaming about and designing my own computers. Never actually built anything, but had lots of paper designs all the way from processors built from 74-series gates to Z80 based machines connected into massively parallel monsters.

As I said, I'm getting old and thinking about those days a lot again. What would my dream-machine in the early '80s look like? If I was an engineer at - say - Commodore with all the freedom in the world, what would I have designed the follow-on to the C64 to be?

This is the origin of this project. The idea is the following: taking the limitations as they existed in the early '80s, let's design a machine that is as kick-ass as I can make it to be. Of course, some technologies are just not available anymore: I can't make 2-micron NMOS process. But I can mimic that by building small-enough designs that would be implementable in those process nodes. I can make sure I won't run them at ridiculously high clock-rates or assume 200-pin BGA packages. I can make sure I'm using era-accurate memory size and speed assumptions. I can limit myself to peripherals that were accessible and popular in those days.

I will however take advantage of modern design tools. And I will take advantage of all the hind-side knowledge of the intervening decades.

So, let's dive in!

Boundary Conditions
===================

I'm going to assume that I have access to a chip fab and I can have as many custom chips as I wish. I will limit myself to packages of the era, mostly - ideally - below 40-pin DIPs. I will allow myself to break that and go all the way up to 64 pins (after all the MC68000 existed in that package at the time), but that must have been a rather expensive proposition, so I'm going to do my best to avoid it.

I'm going to assume that the minimum shipping memory configuration is 128kB, but I want headroom to go higher. Much higher in fact. I will assume FPM DRAM chips, but that's about it. No EDO, let alone SDRAMs.  

I'm going to want to have decent resolution graphics for the time (ideally VGA, that is 320x240, 8bpp and 640x480, 4bpp). I'm not going to assume and 3D graphics capability (that came much later). Sprites seemed to be a great idea at the time, so I'm going to have them. I will have as many colors as I can get, probably either 4bits/channel, but no true-color requirements.

For sound, I will make digitized, multi-channel sound playback a priority. I will think about FM synthesis, if it fits. Stereo sound is a must. Audio input capability is a nice-to-have. MIDI was starting to come on the scene, I would consider adding support to it (Atari STs had them)

I'm not going to think about adding a cartridge port, but a floppy drive is a must. Hard drives are also important: There were a lot of quazy standards, such as MFM drives, but really the thing that - though expensive - appeared to be the future was SCSI.

Human interface was standardized to mice, keyboards and joysticks at the time. I would consider adding some sort of serial bus for supporting them, following in the footsteps of Apple and IBM. I don't think ADB existed at the time (would have to check when Apple introduced it), but it's not unreasonable to realize that a unified interface to all of these peripherals might be a good idea. Would I have tried to come up with my own standard? Not sure.

For communications, I would think modems, thus serial ports. I don't know when Ethernet and Arcnet became a thing, but I do know that LANs were 'in the air'. The Macintosh implemented them, I think even early PETs had some sort of networking ability. So, that could be something to entertain, whether I would have had the foresight to pick a winner, that's debatable.

Expandability would actually be rather low on my list, but maybe that's bias on my side. Contemporary machines ran the gamut on that, the Apple II and IBM PC being highly expandable. The Amiga in it's first iteration was too, but the A500 was not. Many other machines, the C64, the Atari ST, the later 8-bitters, such as the Amstrad CPCs were not either.

The internals
~~~~~~~~~~~~~

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

So, let's say I can afford around 50000 transistors in NMOS, which
would translate to about 25000 gates.
That would be roughly (according to Digikey) an XC4028; XC4044 or an
EPF6024. An Actel (Microchip) A54SX16 is also about the same size.
These all clock in at around 1500 LUTs. Nothing gets even remotely
close to them in size these days. Even the smallest Cyclone III has
5000 LEs. Which just goes to show how constrained these designs would
need to be. Nevertheless, a straightforward 3-stage (or 5-stage) RISC
pipeline should fit as the ARM1/2 cores show. The smallest MAX10 device
is in this category as well (2000 LEs), but one can't use any of the
on-chip RAMs or DSP blocks, cause that would be cheating.
The iCE40UL-1k XO2-2000, MachXO3L-2100, AGL030 all seem to be in the
same league. The AGL030 is old enough apparently to be 5V tolerant too.

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

So, I think it's safe to assume that 20MHz clock rate would be pushing the tech of the early '80s, but would be no problem for the later years of the decade. What I will do is to assume 20MHz for the I/O, but stick with 10MHz for the core. That seems safe...

## Address decode and interface to things other than DRAM:

We need to identify the two address cycles:

/AC_1: /RAS || ~(/LCAS && /UCAS)
/AC_2: /RAS || (/LCAS && /UCAS)

Address decode is relatively simple:

1. Latch A0..A10 and A22 by /AC_1:
   LA8  <<= latch(A8_0, /AC_1)
   LA9  <<= latch(A9_1, /AC_1)
   LA10 <<= latch(A10_2, /AC_1)
   LA11 <<= latch(A11_3, /AC_1)
   LA12 <<= latch(A12_4, /AC_1)
   LA13 <<= latch(A13_5, /AC_1)
   LA14 <<= latch(A14_6, /AC_1)
   LA15 <<= latch(A15_7, /AC_1)
   LA16 <<= latch(A16_17, /AC_1)
   LA19 <<= latch(A19_18, /AC_1)
   LA21 <<= latch(A21_20, /AC_1)
   LA22 <<= latch(A22, /AC_1) 
2. Use the latched signals to decode 4 address regions, by feeding LA21-22 into a 74LS139. Gate the decode with /RAS
   The decoded regions are used as follows:
        /RGN0: ROMs
        /RGN1: I/O
        /RGN2: DRAM
        /RGN3: DRAM   
3. I/O can be further decoded using a 74LS138. It should be gated by /AC_2, /RGN1 and LA16, decoding LA13,14,15
4. Extension board I/O regions could also be decoded in a similar way, except gating the second 74LS138 by /LA16. This gives each card 16kB (8kW) of I/O space
5. EEPROM can be attached to A0...A8 + LA8...LA16 making it possible to decode 512kBytes (256kW) worth of EEPROM space. Their /CE signal is connected to /AC2, their /OE signal is to /RGN0

## Interface to DRAM

DRAMs are relatively straightforward to interface to, that was the whole point. /LCAS and /UCAS act as byte-selects, but they need to be qualified by /RGN2 and /RNG3 to create space for the 4 RAM banks. These 4 banks then can be either implemented in a single 72-pin (EDO-style) DIMM or 4 30-pin DIMM modules. 

Due to the loading of all the RAM chips, it's quite likely that /RAS /WE, address and data needs to be buffered, but that is to be seen.

## DRAM speeds and clocks

Early NMOS DRAMs had:

t_RAC = 100/120/150ns
t_CAC = 55/60/75ns
t_RC = 190/220/260ns

Later devices were somewhat faster (uPD41464):

t_rcd = 40/50/60
t_cas = 40/50/60
t_cp  = 30/40/50
t_crp = 0

FPM DRAMs later on had:

t_RAC = 80/70/60ns
t_CAC = 20/20/15ns
t_RC = 150/130/110ns

The newest part (41C16000) even gets somewhat faster:

t_rcd = 37/45
t_cas = 13/15
t_cp  = 10
t_rp  = 35/40

FPM was a later ('90) invention and EDO was even later, introduced in '95. 

As we will see later, we will want about 12.6MBps of transfer rate for 320x240 8bpp resolution. That would be 160ns between video accesses over a 16-bit bus. That is almost all the bandwidth that an early DRAM (150ns) could provide. No wonder Amiga had slow and fast RAM. However, what we really want is page-mode access, which even in those days could support 75ns access times within a page. It's not really reasonable though to bank on needing so early parts (4164). and if we go with 41464 parts, those are significantly faster, supporting ~50ns accesses times within a page.

This is what we should be shooting for then: **50ns (20MHz) clock speed on the bus.** This should be the main clock for our machine.
This is quite a bit higher then what most designs used at the time. I might have to clock the CPU (and maybe other chips) as well down to half of that and only keep the bus interface at that high speed (which is a bit weird to say, later split-clock designs were the other way around).

## Glue logic speed

If our cycle-time on the bus is 50ns, we will need to use fast chips: we can't afford a 45ns propagation delay of a 74LS138 for instance. At least for the crucial address-decode logic we'll have to bank on 'F' series parts.

## DMA

There are a lot of things that want bus-master access. Mostly graphics and audio, but HDD and FDD controllers too. Not only that, but both graphics and audio wants several independent data-streams. Finally, we don't really have the pins to implement bus-mastering capability in the graphics or indeed in the audio controller. It appears it's better to centralize this capability in a multi-channel DMA controller.

The DMA controller has 2D DMA capability (at least on some channels). The configuration of the DMA is the following:
  1. Base-address (32 bits)
  2. Line length (12 bits)
  3. Line offset (12 bits)
  4. Transfer size (32 bits)
  5. Wrap size (5 bits)
  6. Burst size (2 bits)
  7. Active (1 bit)
  8. Mode (2 bits) (Read/Write/Chained/RSV)
  9. Word size (1 bit)
  10. Interrupt on termination (1 bit)
  11. Circular (1 bit)

This is a total of 101 bits, probably spread across 4 32-bit and 2 8-bit registers.

The state of the DMA is captured in the following registers:
  1. Current address (32 bits)
  2. Mark address (32 bits)

Chaining allows adjacent channels to act as a pair of memory-to-memory DMAs.

TODO: do we want blitter functionality? If so, we would need read-modify-write cycles.

The DMA chip has the following pinout:

-------- 0
A8_0
A9_1
A10_2
A11_3
A12_4
-------- 5
A13_5
A14_6
A15_7
A16_17
A19_18
-------- 10
A21_20
A22
D0
D1
D2
-------- 15
D3
D4
D5
D6
D7
-------- 20
D8
D9
D10
D11
D12
-------- 25
D13
D14
D15
/DRQ
/DACK
-------- 30
/DCHRD (OC)
/RAS
/LCAS
/UCAS
/WE
-------- 35
/REG_CS
CLK
/RST
/INT <-- output
/BREQ_IN
-------- 40
/BREQ_OUT
/BGRANT
/WAIT (OC)
TC
VCC
GND

It's sad to see that we can't fit in 40 pins, but 46 is still acceptable. According to (https://img.ozdisan.com/content/library/IC_Packages.pdf) DIP packages went up to 48 pins, and PLCC packages actually have been around by then. The Atari ST series used those from the get-go, so we can afford them too!

  > The reason for having full 16-bit data-bus:
  > - We have 32-bit registers, which are awkward to program over a sip-address 8-bit bus. At the same time, we do need access to the unmodified lower 8-bit of the data-bus for the channel ID cycle. While this can be worked around, it would need extra chips - we can't simply participate in the normal 8-bit peripheral bus - the bus-driver is enabled in the wrong direction.
  > - It's really awkward to implement chained DMAs. They would need external registers for data-storage and even then it would not be possible to support 8-bit chained DMAs or anything other than a burst-size of 1.

The operating is the following:

Each DMA client is daisy-chained through their /DRQ lines. That is to say, that each DMA client (as we'll see later) has a /DRQ_IN and a /DRQ_OUT pin. The first in this chain is driving the /DRQ line of the DMA controller:

/DRQ_OUT <<= /DRQ_IN & ~(<any internal DMA request>)
ARB_WON <<= ~/DCHRD & /DRQ_IN 

This establishes a hard-wired priority scheme between them. The furthest device from the controller has the highest priority.

When the DMA controller notices /DRQ asserted, it requests access to the bus by asserting /BREQ. The CPU (eventually) answers by asserting /BGRANT and relinquishes driving of the bus.

The DMA controller at this point asserts /DCHRD. This signal is received by all DMA clients. The client with it's /DRQ_IN de-asserted is the one that will be the one selected as the DMA target (as described by the internal ARB_WON signal above). This client puts the associated channel ID on D0...D5 and the command-code on D6...D7. If multiple DMA sources present in a single client, it is the clients responsibility to select the highest priority internal source. The channel ID in general is programmable in the client and assumed to be unique across the system. The /WAIT signal can be asserted by the client to extend the /DCHRD cycle.

If the channel ID read on the data-bus during the /DCHRD cycle is one of the channel IDs of the DMA controller, it will continue processing the DMA request. If not, it will simply relinquish the bus by de-asserting /BREQ. This allows multiple DMA controllers to work in parallel on the same bus: All controllers are connected 'in parallel', but they have their own unique set of channels they implement. When /DRQ is asserted, all controllers request control of the bus (assert /BREQ, monitor /BGRANT) and issue the /DCHRD cycle. Based on the returned channel code, all but one of the DMA controllers determine that they are not the one to generate the bus-cycle, so they release /BREQ. The controller that got selected (i.e. the one with the requested channel) completes the transaction and releases /BREQ whenever it's ready.

DMA channel IDs handled by a controller are numbered consecutively, starting from a pre-programmed ID. For instance, if there are 16 DMA channels in a controller, it will have a 4-bit start channel ID register, which sets the top 4 channel bits.

There can be a total of 64 DMA channels in a single system.

Now, for the commands: there are four command codes that the client returns to the DMA controller along with its channel ID. These are presented on D15 and D16 and are as follows:

CMD codes:
0 - advance; the DMA serves the next address per it's current state
1 - mark; same as 'advance, but the current (not updated) current-address register is also written to the mark register
2 - restore; current address is restored from mark-register and used to serve the DMA request
3 - reset; current address is reset to DMA base-address and used to serve the DMA request

Using the command and the channel ID, the DMA controller can load the appropriate configuration and context, update the context and start serving the request. Each request is served by a number of transfers, programmed in the burst-size register. In most cases, bursts will be within a single DRAM page, but that's not necessarily the case. Additional page-select (/RAS only) cycles are generated by the DMA controller as the current address pointer crosses page boundaries. Since the smallest supported device is the 4164 part, pages are assumed to be fixed, 256 words long (even for larger devices). At the beginning of a DMA burst, a /RAS cycle is always inserted: we don't know what the selected page is, not to mention that we gain control of the bus with /RAS high.

Whenever the DMA controller asserts /LCAS or /UCAS, it also asserts /DACK to signal to the client the availability of a read or the request for data for a write transfer.

The client is responsible for the handling of the data-bus. There are a few possibilities here:

1. 16-bit client - 16-bit, word aligned transfers:
   in this case both /LCAS and /UCAS are asserted along with /DACK. The 16-bit data is presented on the whole data-bus. /WE signals the direction of the transfer
2. 8-bit client - 16-bit transfers: this is not supported
3. 8-bit client - 8-bit client-->memory transfers
   in this case /LCAS is asserted for even and /UCAS for odd addresses. /DACK is asserted with either. Data is taken from either D0...7 or D8...15, depending on which /xCAS is asserted. The other 8 bits are ignored, so the client can simply replicate data on both bytes, but *it has to do so*. /WE is asserted during these transfers.
4. 8-bit client - 8-bit memory-->client transfers
   in this case /LCAS is asserted for even and /UCAS for odd addresses. /DACK is asserted with either. Data is presented on either D0...D7 or D8...15, depending on which /xCAS is asserted. The other 8 bits are undefined. The client is required to mux the required byte into it's local 8-bit data-bus.

The DMA controller monitors the /WAIT line and allows the extension of the /xCAS cycles as needed.

/DACK remains asserted throughout the whole burst. /WE als remains static. It's /xCAS that toggles for the beats.

Once the requisite number of beats of the burst were completed, /DACK is de-asserted along with /BRQ. The CPU gains back control of the bus.

When a client sees /DACK de-assert, it can remove it's request by clearing it's internal request state. This - depending on /DRQ_IN may or may not propagate down to the DMA controller. The DMA controller will re-examine /DRQ in the next cycle, allowing at least some time for the CPU to make forward progress as well.

It is the responsibility of the programmer to make sure that the DMA burst-size and the client request-logic is properly matched, that is: a client will only request a transaction if it can handle at least the programmed burst-size number of contiguous transactions. If no such guarantee is present, a burst-size of 1 should be used.

The TC output is set to 1 upon the last beat of the last transfer for a programmed DMA. If enabled, an interrupt is also generated. For circular DMAs, the DMA engine is re-initialized to a new transfer. For non-circular DMAs, the channel is disabled.

The DMA controller ignores any transfer-requests on disabled channels.

There is a 16-bit interrupt status register containing '1' for each channel that has a pending interrupt. This register is 'write-1-to-clear'.

TODO: this all sounds very complicated. Many control registers are needed with a lot of internal state. In some sense, more channels are easy to add, but at some point we'll simply run out of transistors for state-storage.

TODO: This is a rather slow way of generating DMA cycles: there's a request cycle, a channel ID read cycle, an address cycle and then the data cycle. It helps a little that we have burst support though, but even then, a 4-beat burst takes 7 cycles. And that assumes that we can toggle the requisite lines within a single clock cycle, for example by driving them on both edges (which makes our already short cycles even shorter).

### Chaining:

Depending in implementation complexity, chained DMAs may only support a burst-size of 1. The driving DMA (even channel) is generating memory reads and capture the result in an internal data registers. The slave DMA (odd channel) then generates a memory write from the internal data register. Byte-lane swizzle must be supported for 8-bit DMAs. Mixed-width DMAs (16-bit master, 8-bit slave or the other way around) are not supported.

Chained DMAs are auto-triggered in that the driving DMA is requested by the completion of the slave DMA and vice versa.

TODO: do we want to add some pacing? If so, how?

## DMA bridge

There are quite a few peripherals that support intel-style DMA transfers. FDD and HDD controllers are the prime examples. Since those were important devices at the time, we need a way to work with them. Comparing our DMA controller to the Intel i8237, we see one key difference: they support single-cycle vs. block vs. demand DMAs (https://docs.freebsd.org/doc/2.1.7-RELEASE/usr/share/doc/handbook/handbook248.html). Demand mode in particular seems to transfer many bytes so long as DRQ is asserted.

We can't really demand mode, because of this: as we complete a DACK cycle, the original requestor may or may not released the bus. So, we have to go back and re-query the requestor channel by asserting /DCHRD for a cycle.

Block transfers are not particular useful (and probably not used all that often) as they hold the bus up for very long time. So we really can only do single-cycle transfers and emulate demand transfers by keep requesting more cycles. Our bust-mode is not really compatible with Intel DMA, so that can be used.

We can create a bridge chip that handles these conversions. It would have the following pinout:

-------- 0
D0
D1
D2
D3
D4
-------- 5
D5
D6
D7
A0
A1
-------- 10
A2
/CS
/DRQ_IN
/DRQ_OUT
/DACK
-------- 15
/DCHRD
/WAIT
/WE
CLK
/RST
-------- 20
/INT
DSRQ0
DSACK0
DSRQ1
DSACK1
-------- 25
DSRQ2
DSACK2
DSRQ3
DSACK3
VCC
-------- 30
GND

TODO: how did DMA wait-states work in the Intel world? Who generated /WAIT? Who monitored it?

## Graphics

According to http://tinyvga.com/vga-timing: VGA pixel clock is 25.175MHz for 640x480. For 320x240, it would be half, 12.5875MHz. That's 40 or 80ns respectively.

If we want to support 8bpp mode in 320x240, and have a 16-bit bus, we would need 160ns access time to DRAM. We would also of course need at
least 75kB of RAM.

With our 50ns cycle-time, 4-beat DMA burst, which would take 7 cycles to complete we would get a byte in about 44ns on average. That's about 50% of the available bus bandwidth. That sounds about right, though of course lower would be nicer.

There is of course the option of not depending on the DMA engine for address generation, but let's first investigate the DMA-based option!
### Video timing generator pinout

-------- 0
D0
D1
D2
D3
D4
-------- 5
D5
D6
D7
D8
D9
-------- 10
D10
D11
D12
D13
D14
-------- 15
D15
A0
A1
A2
/CS
-------- 20
/DRQ_IN
/DRQ_OUT
/DACK
/DCHRD
/WAIT
-------- 25
/WE
VIDEO_CLK
SYS_CLK
/RST
/INT <-- output
-------- 30
R
G
B
HSYNC
VSYNC
-------- 35
VCC
GND

We have to independent clock inputs (and two internal clock-domains): one for the system clock to interface with the bus and the other for the video generation logic. R/G/B output would be analog signals, which of course we can't do on an FPGA: we would need to depend on external DACs. (Side-note: even the Amiga 1000 depended on external resistor-network based DACs for video. In the A500, it became a 'hybrid', which is not much better... The Atari ST did the same thing, except for 3-bits per channel.) So, here I'm assuming more then what the tech of the day supported apparently. If I add those pins (9 extra), we'll end up with 46 pins total.

We have to have internal buffers for a full burst from the DMA controller and then some to weather the latency-jitter: probably 16x8 bytes worth. We would also need a palette RAM, which is 256x12 bits.

If we wanted to support sprites, we would need scan-line buffers for them, probably around 64-bits worth each (16x16 and 4bpp). That would be 512 bits total.

Adding this all up, it's 3712 bits total. Then, of course we have all the timing registers and what not, quite a bit of state to maintain.

We would have 9 DMA channels: one for the main screen and one for each sprite.

### Line-replication

### Smooth-scrolling

### Autonomous device pinout

Net's look at how a device with integrated address generation would look like!

We need to be able to drive the bus but also receive transactions. We
probably want to be able to put the frame buffer wherever, so we want
the whole address bus, and we certainly want all data being available:

-------- 0
A8_0
A9_1
A10_2
A11_3
A12_4
-------- 5
A13_5
A14_6
A15_7
A16_17
A19_18
-------- 10
A21_20
A22
D0
D1
D2
-------- 15
D3
D4
D5
D6
D7
-------- 20
D8
D9
D10
D11
D12
-------- 25
D13
D14
D15
/RAS
/LCAS
-------- 30
/UCAS
/WE
/REG_CS
/RST
/INT <-- output
-------- 35
/BREQ_IN
/BREQ_OUT
/BGRANT
/WAIT (OC)
VIDEO_CLK
-------- 40
SYS_CLK
R
G
B
HSYNC
-------- 45
VSYNC
VCC
GND

So now, we're clocking in at 48 pins (57 if external DACs are used).

Maybe this is better, but I'm afraid the integrated DMA would make the chip too large.

### Smooth scrolling

Talking about smooth scrolling, here's a problem:

By having a 2D DMA, we can display an arbitrary window in a larger
display, but what do we do at the edge? What we would want is the
window to wrap around to the beginning of the *same* scan-line. That
way SW would always only need to update each scan-line in a ring-buffer
fashion and never worry about more than a few columns per frame. This
however is beyond what a normal 2D DMA would be required to do, so...
weirdness.

An easier way of getting around this would be to wrap the whole DMA at
some bit-width. That way the DMA would crawl through - say - 128k of
memory, but at the end would start wrapping back, not on an individual
scan-line level, but globally.

### DMA Descriptor

Base Addr:   22 (in words)
Line length: 16 (in words)
Line offset: 16 (in words)
Total count: 22 (in words)
Flags:       16 (including wrap)

Total: 92 bits
We probably want around 16 channels in a single DMA controller, which
would require ~1500 bits. That's 8800 transistors right there. If my
budget is ~20k transistors, that's quite a bit, though not tragic,
necessarily.



## 8-bit peripherals

We probably would have a ton of 8-bit peripherals in our system. To handle them we need to create an 8-bit bus:

There are two 74F245 chips:

chip 1: 
        A1...A8 <<= peripheral-side D0...D7
        B1...B8 <<= CPU-side D0...D7
        DIR <<= (1 A-->B; 0 B-->A) /WE
        /OE <<= /LCAS && /DCHRD
chip 2: 
        A1...A8 <<= peripheral-side D0...D7
        B1...B8 <<= CPU-side D8...D15
        DIR <<= (1 A-->B; 0 B-->A) /WE
        /OE <<= /UCAS

For addresses, we need to shift the address bus 'up' by 1, and insert /UCAS as A0.

For /CS, we use /LCAS && /UCAS and for /WE we can use /WE unmodified.

Interrupts and DMA requests/responses work as-is, no translation is needed.



## Sound

If we want CD quality sound, we would need a new sample every 11.3us
(stereo, 16-bit). That's... nothing. If we wanted wave-table synthesis,
that's a different animal alltogether, but even if we supported 8
(mono) wave-table sounds, that would be ~3us per access.

Sound should also use the previously established DMA engine. Wave-table
synthesis involves at least linear interpolation between samples and
ADSR envelope generation.

One could also think about sound as simply a 'MOD' accelerator:
https://web.archive.org/web/20100921225940/http://io.debian.net/~tar/debian/xmp/xmp-2.7.1/docs/formats/Ultimate_Soundtracker-format.txt
or
https://www.ocf.berkeley.edu/~eek/index.html/tiny_examples/ptmod/ap12.html

That would include 4 channels of 8-bit samples.

The problem with this is that the access patterns are - yet again -
weird: we need two adjacent samples for the interpolation, but from
cycle-to-cycle we might jump any number of samples. 0 and 1-sample
jumps could theoretically be handled by internal shift-registers, but
more than 2-sample jumps can't be easily accommodated. Luckily loop
points are 16-bit aligned in the MOD format, so at least we don't have
to deal with that.

So, they way trackers get around this issue (and the way Amiga most
likely got around this issue as well) is that they don't do any
interpolation or sample-skipping. Instead, they change the readout-
speed of the sample. What they do is they take the video clock
(7159090.5Hz for NTSC) machines, divide it by 2x<period number> and
that turns out to be the sample rate for the channel. Than, they simply
read (and update the DAC with) the new sample at that sampling period.
Period numbers normally range:


            C    C#   D    D#   E    F    F#   G    G#   A    A#   B
    Octave 1: 856, 808, 762, 720, 678, 640, 604, 570, 538, 508, 480,
453
    Octave 2: 428, 404, 381, 360, 339, 320, 302, 285, 269, 254, 240,
226
    Octave 3: 214, 202, 190, 180, 170, 160, 151, 143, 135, 127, 120,
113

    Octave 0:1712,1616,1525,1440,1357,1281,1209,1141,1077,1017, 961,
907
    Octave 4: 107, 101,  95,  90,  85,  80,  76,  71,  67,  64,  60, 
57

where octave 0 and 4 are non-standard. These can also be changed by
fine-tune values in the samples, but that's not all that interesting
here. The point is, that the lowest sample rate supported is 4181Hz,
while the highest is 31677Hz (62799Hz with octave 4 support).

The interesting note here is the following: if I had 44100Hz sampling
rate in the DAC, my actual sampling rate will always be less then that.
So, no cycle-jumping, only cycle-repetitions. With that, we could have
a linear interpolator per channel inside the audio chip, and request
pairs of samples on different channels consecutively, when needed (we
would need 32-bits of internal storage per channel for the current 2
samples and the upcoming two samples). We would then use the 4 existing
DMA commands to implement repetitions.

So, overall, cool! This works. Now, for pinout:


-------- 0
D0
D1
D2
D3
D4
-------- 5
D5
D6
D7
D8
D9
-------- 10
D10
D11
D12
D13
D14
-------- 15
D15
A0
A1
A2
/CS
-------- 20
/DRQ_IN
/DRQ_OUT
/DACK
/WAIT
/WE
-------- 25
AUDIO_CLK
SYS_CLK
/RST
/INT <-- output
L_OUT
-------- 30
R_OUT
L_IN
R_IN
VCC
GND
-------- 35
AVCC
AGND

As usual, the FPGA implementation would have more pins as it needs to
interface to a DAC (though I2S most likely).
Here I did brake out separate analog supplies as they are most likely
needed.

## Serial ports

Now, that's simple, I probably don't even have to re-invent the wheel
here. The only question is: do I want to support DMAs. The answer is,
probably don't, in those days serial ports were slow enough that CPU
interrupts were sufficient.

## Floppy interface

Here I really don't want to re-invent the wheel, so the more pertinent
question is: can we handle standard floppy-controllers?

Here's one popular in PCs:
https://ardent-tool.com/datasheets/NEC_uPD72065.pdf
http://dunfield.classiccmp.org/r/765.pdf
A much more modern version
https://www.mouser.com/datasheet/2/268/37c78-468028.pdf

This guy is a fairly standard 8-bit interface, except for it's DMA
capability: DRQ is an output, not inverted. /DACK is the DMA
acknowledge and TC is the DMA termination count.

Which brings up an interesting point: why is there a TC signal in PCs
and should we have it?

Also, it appears that there should be some interface logic that allows
for PC-style DMA peripherals to connect to our engine.

A few other FDD interface chips:
http://www.bitsavers.org/components/westernDigital/WD57C65_Floppy_Disk_Subsystem_Controller_May88.pdf

Another *NOT* PC compatible floppy interface:
http://www.bitsavers.org/components/westernDigital/FD179X-02_Data_Sheet_May1980.pdf

This one is interesting as it has a DRQ line, but no DACK. So, it would need some glue to hook DACK into /CS ot something.

## DMA converter


## Hard drive interface

If this computer was to compete with early PCs and MACs, initially HDD support is not a primary concern. Yet, pretty soon it would become mandatory and that would probably be apparent already during development. So how would one interface an HDD in those days?
- MAC went the SCSI way with the AM5380 chip https://amazingdiy.files.wordpress.com/2012/09/am5380pc.pdf
- PC/TX went originally with MFM/RLE interfaces

For SCSI, the interface is rather common. It seems that TC is inverted and a different way (READY) is used to back-pressure 'block mode DMAs'. It seems this chip was specifically designed for PC applications. One important thing here: this is an 8-bit chip, so it's DMA support will need to be 8-bit wide too.

This 5380 chip seems to have been rather popular with many clones:
- Z83C80 seems to still be in production for instance from Zilog of all places (even RoHS compliant!)
- NCR53c80
There is a whole family of these as well: 
- 53c90/94/95/96
- 53c400 (still available at least for some retailers)
- 53c416 (16-bit version)
- 53c7x0 (32-bit, designed for i386 and such, but might work on 16-bit buses as well)
- 53cf90/94/96

Not sure if this is a right controller to look at, but here's something: http://pdf.datasheetcatalog.com/datasheet/nationalsemiconductor/DS005282.PDF - actually, no. This appears to be a SCSI device controller. Interesting, but not interesting.

http://www.bitsavers.org/components/westernDigital/WD33C93A_SCSI_Bus_Controller_1991.pdf is another SCSI controller. It has roughly the same signals as the AM5380 part. Except there's no interrupt. Also an 8-bit part.

For MFM, there's a card that does that: http://bitsavers.trailing-edge.com/pdf/westernDigital/pc_disk_controller/WD1003/WD1003-WAH/WD1003-WAH_T.jpg
It's a chip grave-yard, but some of the important chips:

WD11C00
WD2010 - seems to be the main controller with the bus interface
WD1015
WD1020

Seems to be documented here: http://www.bitsavers.org/components/westernDigital/_dataBooks/1986_Storage_Management_Products_Handbook.pdf

This is a monster. I'm not going to spend more time on it. There are a lot of gates needed for any implementation, mostly because there seems to be an external sector-buffer that's shared on the same (8-bit) bus.

BTW: all MFM/RLL interface boards I can find are based on WD chips and in general are rather complex, but in the end PC-based designs.

Early Amiga HDDs also used some sort of SCSI interface.

Atari apparently had a ACSI to MFM interface in SH-205. It's heart is the AIC-010 chip from Adaptec http://www.bitsavers.org/pdf/adaptec/asic/AIC-010_Programmable_Mass_Storage_Controller.pdf

This chip doesn't have DMA apparently, but does have a multiplexed data/address bus. This would need significant external logic for us. It is a chip-graveyard anyway, as external buffers are also needed to buffer the data, so this is not going to kill the proposal.

##########################################################

# Overall:

We have:
1. CPU in a 40-pin package
2. DMA controller in a 36-pin package
3. DMA converter in a 32-pin package
4. Display controller in a 36-pin package
5. Audio controller in a 36 or 40-pin package
6. FDC controller in a 40-pin package
7. SCSI controller in a 40-pin package
8. Serial controller in a 40-pin package (single serial)

 > The A500 had 7 large chips:
 > 1. Odd CIA
 > 2. Even CIA
 > 3. PAULA
 > 4. FAT AGNUS
 > 5. DENISE
 > 6. MC68k
 > 8. CORY (??)
 > 
 > But that's not the original Amiga chipset. That would be the A1000 (https://twitter.com/TubeTimeUS/status/1261064482257694720/photo/1):
 > 
 > 1. Paula
 > 2. Denise
 > 3. Agnus
 > 4. Odd CIA
 > 5. Even CIA
 > 6. MC68k
 > 
 > A schematic is here: https://erikarn.github.io/amiga/1000/Amiga_A1000_Schematics_2.pdf
 > 
 > So the ROM was 2x 27256 parts.
 > 
 > Surprisingly Denise didn't have the DACs in it: it had 12-bit RGB going to resistor-based DACs and an MC1377 modulator. Paula had DACs in it, but extensive external  > filters were used.
 > 
 > There are 2 banks of memory, a fast-RAM and a video (?) RAM. These are at least driven by multiplexed address lines from AGNUS. The CPU needed external muxing though.t

On top of these, we would need:
- Some sort of keyboard interface. Either some serial port or a parallel interface.
- Mouse support (probably serial a'la PC or shared with keyboard a'la MAC)
- joystick support
- modem (serial)
- networking (NE2000 or something). Here we have modern alternatives such as the CS8900, but the NE2000 was based on the DP83901 (http://pdf.datasheetcatalog.com/datasheets2/72/722905_1.pdf) which is quite a beast. This can be seen from the many many extra chips that are needed for the NE2000, including external packet buffers. It is a weird little chip which it's own bus-master capability, but a multiplexed 16-bit address/data bus. What's interesting is that there's a separate address bus for register access, the m...