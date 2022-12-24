Graphics
========

According to http://tinyvga.com/vga-timing: VGA pixel clock is 25.175MHz for 640x480. For 320x240, it would be half, 12.5875MHz. That's 40 or 80ns respectively.

If we want to support 8bpp mode in 320x240, and have a 16-bit bus, we would need 160ns access time to DRAM. We would also of course need at least 75kB of RAM.

The original NMOS DRAMs could not work at those data-rates, even with 4-beat bursts. So, for the first iteration of our machine we'll have to compromise on 320x240 at 4bpp, which is slightly worse than Amiga, but a result of 320ns between accesses.

Alternatively, we can chose to support composite (style in terms of timing) output, where the horizontal timing is 15,625Hz and the pixel clock for a 320 pixel horizontal resolution would be 7.14MHz. That of course is about half that of the VGA number above and results in a 280ns access-rate for an 8bpp screen.

For a more advanced FPM-based machine, we can bump our access rate up all the way to standard VGA resolution and 160ns access times.

To conserve memory bandwidth, our DMAs will work with 4-beat bursts, transferring 64 bits at a time. To decouple memory and screen timing, we need a buffer probably twice as large, or 128 bits. The more, the better, we can tolerate more jitter on the memory interface.

If we can fit a full scan-line worth of data in an internal FIFO, we could cut the data-rate required in half, due to double-scan: we can repeat the scan-line from internal memory. If we could fit two scan-lines worth of data internally, we could even space out the loads over two scan-lines, reducing not only our average thirst for data, but our peak as well. That however is probably a second-gen feature. The first generation chips would not have that capability.

.. todo::
    We need to figure out the DMA timing precisely. There's a lot of 'fluff' in our DMA protocol that will slow us down.

The graphics controller uses several DMA channels to convey video-data into itself: one for the main screen buffer and one each for each sprite.

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

