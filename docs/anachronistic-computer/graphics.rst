WAVEDROM EXAMPLE
================

.. wavedrom::

        { "signal": [
                { "name": "clk",  "wave": "P......" },
                { "name": "bus",  "wave": "x.==.=x", "data": ["head", "body", "tail", "data"] },
                { "name": "wire", "wave": "0.1..0." }
        ]}

Graphics
========

According to http://tinyvga.com/vga-timing: VGA pixel clock is 25.175MHz for 640x480. For 320x240, it would be half, 12.5875MHz. That's 40 or 80ns respectively.

VGA never really had a resolution of 320x340. What it did was double each scan-line (480 scan-lines) and each pixel (640 pixels). It's refresh rate was 60Hz. So, the average datarate for 640x480 is 18.4Mpixel/s. For 320x240 it's only half of it, around 9.2Mpixel/s.

NTSC or PAL TV was different: it used 60Hz (50Hz) field rate, but only 30Hz (25Hz) refresh rate. On top, NTSC only guaranteed 200 or so visible scan-lines. So, for TV it was either 320x200x60Hz or 320x240x50Hz. These both turn into the same, 3.84MPixel/s.

Given that we have give-or-take 16MBps memory bandwidth in the whole system, we can expect to support the following:

1. VGA, 640x480 resolution at 4bpp -> ~9.2MBps bandwidth requirement.
2. QGVA, 320x240 resolution at 8bpp, but *with* internal scan-line doubler -> ~4.6MBps
3. TV resolution 320x200 at 8bpp -> ~3.84MBps

The first resolution is going to slow the CPU to a crawl. It's OK for interactive, non-CPU intensive tasks, such as GUIs, but not for games, I don't think. The second and the third modes are roughly the same. The second one would not have been possible with the chip-technology of the time (the scan-line buffer is too large to fit, I'm afraid), but it's doable for an FPGA implementation. The third would have been the main way of using the machine originally, but pointless to implement today.

So, the two supported resolutions are:
1. VGA: 640x680@60Hz, 4bpp (or less)
2. QVGA: 320x240@60Hz, 8bpp (or less)

Let's see timing-wise where we end up!

VGA resolution needs 156,600 bytes for every refresh. Using 16-byte bursts, each burst would take 16/2+2=10 cycles. A frame take 9600 such bursts, or 96,000 clock-cycles. At 60Hz refresh rate this turns into 5.76M clock cycles. If our system runs at an 8MHz clock rate, that's a whopping 72% of the available bus bandwidth.

We certainly can't use shorter bursts, even even worse bus utilization. Longer ones are problematic from a buffering perspective, but 32-byte bursts would result in a 65% bus utilization. Maybe worth it...

QVGA resolution needs half of all of this, so either 36% or 32.4% of the bus bandwidth.

Things of course get dramatically better with lower bit-depth.

Sprites
-------

If we wanted to support sprites, we would need scan-line buffers for them, probably around 64-bits worth each (16x16 and 4bpp). That would be 512 bits total.

The graphics controller uses several DMA channels to read video-data: one for the main screen buffer and one each for each sprite.

Screen compositing
------------------

We might want to support several layers of screen data. For instance: overlay of two 320x240, 4bpp planes. This is the same memory bandwidth, but with supporting two independent smooth-scroll settings for each, a game can a foreground/background semi-3D illusion.

With 3 such planes supported (4/2/2bpp each), a score-board, a background and a foreground plane can be implemented. Add the sprites and we have a rather formidable set of capabilities.

This is a very expensive proposition though: we would need to replicate:
- 2D DMA engines
- Pixel buffers (can't break the bursts up otherwise we're ruining our bus efficiency)
- pixel shifters (the whole point would be arbitrary relative offsets)
- palette lookups (we can't do compositing before we get RGB space)

Since we can't really expect to close timing beyond ~10MHz, we would certainly need to handle 2 VGA pixels in parallel, maybe even two QVGA pixels, which would mean up to 6 palette lookups per clock cycle.

Pinout
------

========== =========== ===========
Pin Number Pin Name    Description
========== =========== ===========
1          D0          Data bus
2          D1          Data bus
3          D2          Data bus
4          D3          Data bus
5          D4          Data bus
6          D5          Data bus
7          D6          Data bus
8          D7          Data bus
9          D8          Data bus
10         D9          Data bus
11         D10         Data bus
12         D11         Data bus
13         D12         Data bus
14         D13         Data bus
15         D14         Data bus
16         D15         Data bus
17         A0          Register address bus
18         A1          Register address bus
19         A2          Register address bus
20         nCS         Active low chip-select for register accesses
21         nDRQ_IN     Active low DMA request daisy-chain input
22         nDRQ_OUT    Active low DMA request daisy-chain output
23         nDACK       Active low DMA acknowledge input
24         nDCHRD_TC   Active low DMA channel read qualifier and terminal count input
25         nWAIT       Open collector, active low wait-state output
26         nWE         Active low register write-enable input
27         nRST        Active low reset input
28         nINT        Open collector, active low interrupt output
29         SYS_CLK     System clock input
30         VIDEO_CLK   Video clock input
31         R           Analog 'red' channel output
32         G           Analog 'green' channel output
33         B           Analog 'blue' channel output
34         BLANK       Video blanking output with programmable polarity
35         HSYNC       Horizontal video sync output with programmable polarity
36         VSYNC       Vertical video sync output with programmable polarity
37         DVCC        Digital power input
38         DGND        Digital ground input
39         AVCC        Analog power input
40         AGND        Analog ground input
========== =========== ===========

We have to independent clock inputs (and two internal clock-domains): one for the system clock to interface with the bus and the other for the video generation logic. R/G/B output would be analog signals, which of course we can't do on an FPGA: we would need to depend on external DACs.

.. note::
    The Amiga and the Atari ST depended on external resistor-network based DACs for video. In the A500, it became a 'hybrid', which is not much better...

We have to have internal buffers for a full burst from the DMA controller and then some to weather the latency-jitter: probably 16x8 bytes worth. We would also need a palette RAM, which is 256x12 bits.

If we wanted to support sprites, we would need scan-line buffers for them, probably around 64-bits worth each (16x16 and 4bpp). That would be 512 bits total.

Adding this all up, it's 3712 bits total. Then, of course we have all the timing registers and what not, quite a bit of state to maintain.

We would have 9 DMA channels: one for the main screen and one for each sprite.

Line-replication
----------------

320x240 screens were a 'hack' in the VGA standard. Or, to be more precise, the scan-lines would have been too far away from each other on a progressive-scan CRT. As a result, the display worked in 480 scan-line mode and each scan-line is painted twice to make the impression of a 240-pixel vertical resolution. If we were to work with these monitors, and timing, we would need to do the same.

Interlace support
-----------------

If we wanted to do *more* than ~240 scan-lines on a TV screen, we would have had to implement interlaced mode. In that operating mode, even fields would end on a half-scan-line and odd fields would start with them. This way, the CRT would shift the fields half a scan-line from one another, creating the impression of double the vertical resolution.

So, to support 640x480 screens on a TV (or a monitor supporting NTSC-style timings) we would need to support interlaced mode.

.. note::
    It's interesting to see how in the 'old world' 640x480 needed special treatment, while in the 'new world' it's the other, the 320x240 resolution that requires it.

Smooth-scrolling
----------------

Smooth scrolling is a shared feature between the DMA and the graphics controller. The DMA can shift it's starting read-out position, but only by 16 bits. That's (depending on the bit-depth of the screen) either 2,4, 8 or 16 pixels.

The graphics controller will have to support the throwing away of the excess data at the beginning (and end) of the scan-line to implement pixel-level smooth scrolling.

The programmer would need to be careful to set the active portion of the 2D DMA in the fractional pixel cases to include these excess reads.

To allow for 'infinite' smooth horizontal scrolling, the DMA controller supports a wrap-around addressing mode. This way the whole transfer can be kept within a fixed region of memory independent of the start-address. This allows SW to keep scrolling to the left or right, and only ever needing to paint a small section of the screen: the few columns that newly became visible.

Vertical smooth scrolling of course is purely a function of the DMA controller by moving the address of the buffer-start.



Micro-architecture
------------------

2D DMA
~~~~~~

The 2D DMA has the following registers:

1. BASE_ADDR: 32-bit physical address (16-bit aligned, LSB is not implemented)
2. LINE_LEN: length of a scan-line in 16-bit increments. This is an 8-bit register, though occupies a 32-bit location
3. LINE_OFS: offset to the next scan-line in 16-bit increments. This is an 10-bit register, though occupied a 32-bit location
4. END_ADDR: 32-bit (?) physical address of the last memory location to read.

.. note:: After reaching END_ADDR, no more fetches happen (maybe permissible to not early-terminate the burst). Restart of the DMA is directed from the timing module.
.. note:: 2D DMA generates 32-byte (16-beat) bursts, but needs to check for and early-terminate page-crossing bursts.


Sprite DMA
~~~~~~~~~~

1. BASE_ADDR: 32-bit physical address (32-bit aligned, lower two bits are not implemented)

.. note:: Restart of the DMA is directed from the timing module: no need to specify the total DMA size

.. note:: there's one sprite DMA for each HW sprite

.. note:: Sprite DMAs generate 4-byte (2-beat) bursts. They can't generate and thus are not interested in page-crossing bursts.

.. note:: This arrangement is pretty similar to the bus-if of the CPU: one high-speed port for large bursts and a low-speed (multiplexed) port for low-speed, simple bursts.

Pixel FIFO
~~~~~~~~~~

The 2D DMA feeds a CDC-fifo into the pixel domain. This FIFO is somewhere between 1 and 2 bursts deep, again very similar to the fetch queue, except it transitions between two clock domains.

sprite DMAs directly write into their associated sprite shift-registers. This is OK because sprite fills happen during horizontal retrace, so they can't be written and read at the same time.

The kick-off of the sprite DMAs is controlled by the

Compositing
~~~~~~~~~~~

In the pixel domain we have the rasterizer that takes bits from the pixel FIFO and turns them into pixels. This is scarcely more then a shift-register, the complexity comes in the following ways:

- Smooth scrolling requires us to ignore N number of bits in the first word pulled from the FIFO - this is done by starting shifting of data during the front porch (blanking) period and is controlled through the timing module.
- We need the ability to shift by 1/2/4/8 bits at a time
- bits are converted into RGB pixels by doing a palette lookup. This is another problem: we might not be able to afford a 256x12-bit RAM for this purpose. So, maybe go the Archimedes route?

On top of this, there are a number of sprites. We have a separate pipeline for each supported sprite, with different comparators, pixel shifters and palette lookups. Each sprite supports 1 or 2bpp modes, and 32 bits per scan-line. The starting position (x and y) and height is programable.

Compositing order is fixed: bitmap being the lowest priority and sprites follow in a fixed priority order. Collision-detection is TBD.

Hopefully, we can do all of this in one pixel per clock speed for at least 320x240 resolution. For 640x480 we'll need to do this at half the pixel rate.

At any rate, once we're done, we have a pixel-stream in RGB. This stream optionally enters the scan-line doubler ring buffer.

Either the input or the output of this ring-buffer is feeding the DACs.

Operation of this module is gated by the timing module to the active portion of the screen.

Registers:

1. BIT_DEPTH - valid values are 1,2,4,8 bpp. (probably encoded as 0,1,2,3 or something)
2. PIXELS_PER_CLOCK - valid values are 1,2
3. SCAN_LINE_DOUBLING - valid values are 1,2

Timing module
~~~~~~~~~~~~~

Register setup:

1. Horizontal total: 8 bits
2. Visible start: 5 bits
3. Pixel start: 8 bits
4. Visible end: 5 bits
5. HSync start: 5 bits
6. Vertical total: 10 bits
7. Visible start: 5 bits
8. Visible end: 5 bits
9. VSync start: 5 bits

The timing module works on the resolution of 8 (640x480) pixels per clock, but at the same clock rate as compositing (i.e. at the pixel clock rate of 320x240)

Pixel start and visible start are different to support smooth scrolling. Pixel start is actually measured in (QVGA) pixels not in 8 pixels, and controls the start of pixel shifting.

Mainly following this document: http://tinyvga.com/vga-timing/640x480@60Hz for timing

Pixel output
~~~~~~~~~~~~

Pixels coming into this unit in RGB format, but at QVGA speed. Sync data (blanking, sync) also coming at the same rate.

This module generates up to two pixels to the DACs. The first part is easy: it's a mux or shift register. The trick is that it needs to run at a very high speed, at twice the clock rate of anything. We will do the same DDR idea that we do for the DRAM. This not only allows us to *not* have another, higher frequency clock, but also to avoid another CDC crossing, which is tricky as we have to keep all the data in sync - would essentially needs a CDC FIFO.

The down-side - though we have that on the CPU already anyway - is that we require a 50% duty-cycle clock.

Interrupts
~~~~~~~~~~

Interrupts can be generated on the following events:

1. When a complete scan-line is read from DRAM (based on 2D DMA), scan-line index is programmable
2. On horizontal blanking start, scan-line index is programmable
3. When a complete frame is read from DRAM (based on 2D DMA)
4. On vertical blanking start
5. Sprite collision


RAMs
~~~~

I finally have found a RAM example for the sky130 SDK: it's a 32x1024bit RAM (single-ported, 6T cells).

https://github.com/ShonTaware/SRAM_SKY130#openram-configuration-for-skywater-sky130-pdks

It's size is 0.534mm^2, closes timing at about 80MHz. Back-scaling it to 1.5u, gives us a scaling factor of 133:1.

Taking all of this, gives us 71mm^2 for this 32kbit SRAM or 0.00217mm^2/bit.

Now, given that we want a palette SRAM of 256*12 bits, it would need about 6.68mm^2.

A scan-line buffer (320x12 bits) is 8.33mm^2

A circular buffer for 32 bytes is 0.55mm^2.

These are probably optimistic somewhat for the following reasons:
- We need more than a single port
- The support logic overhead is greater for smaller memories.

More on OpenRAM and sky130:

http://ef.content.s3.amazonaws.com/OpenRAM_%20FOSSI%20Dial-Up%202020.pdf

Palette RAM
~~~~~~~~~~~

Palette RAM should be single-ported, and updates are only allowed during horizontal and vertical blanking.