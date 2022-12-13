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