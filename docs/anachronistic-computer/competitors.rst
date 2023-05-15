Competitors
===========

Acorn Archimedes
----------------

This is the `definitive store of documentation <http://chrisacorns.computinghistory.org.uk/docs/Acorn/Manuals/Manuals.html>`_ for this design.

This is a late comer to the scene with an introduction date of 1987. It is the debut of the ARM processor family, originally being clocked at 8MHz, and delivering an IPC of about 0.5.

In terms of peripheral set, there's some `info <https://www.retro-kit.co.uk/user/custom/Acorn/32bit/documentation/Acorn_A3xA4xVideoSpec.pdf>`_ though the service manuals are a much better source, except of course they don't offer much on video timings.

According to the document (at least the later A3000 model, launched i '89) used a DMA engine to feed the video generator with bursts of 4x32 bit data, for 'efficient use of page-mode DRAM'.

The video controller supported up to 8bbp. It had 12-bit color, plus a 13th bit for 'external video'. It had a single HW sprite (cursor) with 32xany pixels and 2bpp.

The resolutions on the A3000 series were 640x512 @ 8bpp, with 800x600 maximum.

It supported resolutions up to 1024 horizontal pixels in 'TV' mode, that is to say, with 15.625kHz HSync rate. Pixel rates of 16 or 24MHz were used.

It had 8-channel, stereo audio capability.

The A300 schematic is here: http://chrisacorns.computinghistory.org.uk/docs/Acorn/Manuals/Acorn_A300_SM.pdf. It shows 4 banks of 4464 memories, in banks of 8 to get to the 32-bit memory bus. That's a *ton* of load on the 'RA' bus that their memory controller, the MEMC drives. Also, the CPU doesn't have a multiplexed address bus, it is the MEMC that does that for it.

For ROM, they've use four devices ranging from 27128 to 27010.

They aggregated audio and video into a single chip and many of the IO functions into another one.

For mouse and keyboard, they had a serial interface of sorts (actually, probably TTL-level serial) to an 8051 micro in the keyboard. This micro then interfaced to both the keyboard matrix and the mouse which was - just as with anybody else - nothing but the opto-couplers and the (in their case 3) buttons.

The funny thing is that this machine is in so many ways what I wanted to design anyways. It feels sooo much better then anything on the market at the time. It's unfortunate that it arrived late and thus was a market failure.

General landscape
-----------------

https://ia801609.us.archive.org/16/items/byte-magazine-1984-11/1984_11_BYTE_09-12_New_Chips.pdf

It lists a 10MB hard drive (MFM, with interface card) for $800, a 20MB one for $1100 and a 40MB one for $2300.

Memory price: 64k DRAM 'kits' (I'm guessing 9 chips) were sold for $43.

An original PC was ~$2000 (256kByte, two 360k FDDs)

A Hercules adapter was $335, a CGA $200, mono monitors were about $100-$150, color ones around $450.

Even a Commodore B128 was about $1000. And that's without any FDDs. In fact, by the time you got a dual-FDD for it, it was more expensive then a PC!

Page 227 contains a large list of component prices. This is good, though of course not for large quantities.

DRAM: $5 (150ns) for 64kBit, $32 (200ns) for 256kBit.
EPROM: 2764 for $7 (300ns) - two years earlier in '82, these were $20 parts according to EDN.
8086: $25; 8088: $20; Z80: $3; 6502: $5; 8085: $5; 68k: $40
FD179x FDD controller: $25-$30
74LSxxx series: $1.5 for 20 pin, $0.75 for 14-pin devices (mostly)
Raw Keyboards are $15-$25

EDN: https://archive.org/details/edn-1982_11_24/page/30/mode/2up

Has a SeeQ E2ROM for $10; the 5213. It's 350ns read, 1ms write and 16k(bits?) Xicor als had 2kx8 (X2816A) and 512x8 devices.

By 1986 the Atari 1040ST was on the front page of Byte. So 1MB must have been affordable...

1986:

PCs are $800 (Bare bones, but with FDD and 640k of RAM)
AT: $1900 with no HDD, but 1.2MB FDD, CGA, also 640k of RAM
64kB RAM: $10; 256kB RAM: $30 (9 chips each)
20MB HDD: $450 (with controller)
Hercules card is $100, CGA $70
The Atari 1024ST was $1000

The Elephant in the Room
------------------------

That is IBM, of course. This article: https://en.wikipedia.org/wiki/Influence_of_the_IBM_PC_on_the_personal_computer_market is a good summary of what happened. In essence, by '84, certainly by '85, the PC train was unstoppable, though it might not have had been obvious yet:

  > From mid-1985, what Compute! described as a "wave" of inexpensive clones from American and Asian companies caused prices to decline; by the end of 1986, the equivalent to a $1,600 real IBM PC with 256K RAM and two disk drives cost as little as $600, lower than the price of the Apple IIc.

Since we need 1.5u tech, we are a contemporary of the 286, which means no earlier than '82 introduction.