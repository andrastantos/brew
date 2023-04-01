System considerations
=====================

High level specs
~~~~~~~~~~~~~~~~

For the processor:

* Internal data-path: 32-bit
* External data width: 8/16-bit
* Clock speed: ~10MHz (depends on DRAM speed)
* Performance: 4MIPS sustained with graphics, 10MIPS peak

Memory:

* Data width: 16-bit
* Size: 128kB to 4MB
* Number of banks: 2/4
* Technology: NMOS DRAM

Graphics:

* Resolution:
   * 320x240, 8 bits per pixel, 60Hz frame rate, 6.25MHz pixel clock rate
   * 640x240, 4 bits per pixel, 60Hz frame rate, 12.5MHz pixel clock rate
   * 640x480, 4 bits per pixel, 30Hz frame rate (interlaced), 12.5MHz pixel clock rate
* Number of colors: 5-bit per channel; 32768 total colors
* Sprites: 8 sprites
   * 32xN pixels, 1 bit/pixel (0: transparent)
   * 16xN pixels, 2 bits/pixel (0: transparent)
   * 8xN pixels, 4 bits/pixel (0: transparent)

Sound:

* 8 mono sampling sound channels
   * Independent panning control
   * Independent volume control
   * Re-sampling
   * Looping support
   * 8-bit/sample resolution
* Stereo output

Storage:

* 720k, 3.5" floppy drive (PC compatible)

Communication and networking:

* LAN through RS-422
* WAN through modem over RS-232

Human interface:

* Keyboard
* Mouse
* Joystick (switch-based)
* Joystick (analog)

Memory map
~~~~~~~~~~

=============  ===========  ===========
Start address  End address  Description
=============  ===========  ===========
0x0000_0000    0x0001_ffff  16-bit EEPROM space (and boot code from address 0)
0x0002_0000    0x0003_ffff  reserved (alias of EEPROM space)
0x0004_0000    0x0004_ffff  8/16-bit internal peripheral space
0x0005_0000    0x0005_ffff  8/16-bit extension bus address space
0x0006_0000    0x0007_ffff  reserved (alias of I/O spaces)
0x8000_0000    0xbfff_ffff  16-bit DRAM space
0xc000_0000    0xffff_ffff  16-bit CSR space: not decoded externally, but handled by the processor internally
=============  ===========  ===========

Chipsets and Model lineup
~~~~~~~~~~~~~~~~~~~~~~~~~

All-in-one setup (A500 layout)
------------------------------

* custom CPU+DMA
* custom Graphics+sound
* custom Classic I/O 1 (mouse/joystick/serial/I2C)
* custom Classic I/O 2 (keyboard scan)
* FDD

Expandable setup (A1000 layout)
-------------------------------

* custom CPU+DMA
* custom Graphics+sound
* custom Classic I/O (mouse/joystick/serial/external keyboard)
* ISA-bus interface (maybe custom interface chip)
* SCSI
* FDD

All-in-one setup (modern A500 layout)
-------------------------------------

* custom CPU+DMA
* custom Graphics+sound
* custom Nuvou I/O (keyboard/joystick/mouse over USB; SDCard; serial/sysconfig/I2C)
* custom Classic I/O (keyboard scan, unless done through USB)

Expandable setup (modern A1000 layout)
--------------------------------------

* custom CPU+DMA
* custom Graphics+sound
* custom Nuvou I/O 2 (keyboard/joystick/mouse over USB; SDCard; serial/sysconfig/I2C)
* ISA-bus interface (maybe custom interface chip)


Clocking
~~~~~~~~

We will stay with the very common NTSC clock rate of 28.63636MHz (double of what Amiga had). On top of that, we'll need 48MHz for USB (on Nuvou I/O of course)

* 28.63636MHz/2   -> Video clock (14.31818MHz)
* 28.63636MHz/3   -> system clock (~9.54MHz) <-- supports -10 DRAM
* 28.63636MHz/4   -> alternate system clock (~7.16MHz) <-- supports -12 DRAM
* 28.63636MHz/3.5 -> alternate system clock (~8.18MHz) <-- supports -12 DRAM, but needs a PLL, which wasn't really a thing back then, not at these frequencies at least.
* 28.63636MHz/3   -> Audio clock option l (37.28kHz Fs)
* 28.63636MHz/4   -> Audio clock option 2 (27.96kHz Fs)

An alternative would be to use an additional clock source for the system clock (which would allow for highest memory bandwidth and CPU perf.)
We could even add a third (audio) clock, or at least the option to use either clock for audio.

This is getting mad. I think the right answer is the following:

14.31818MHz clock source for video and audio (27.96kHz sampling rate)
6.6/8.3/10/12.5MHz clock source for system
48MHz clock source for USB

These would be three different clocks, provided by three different crystals/oscillators.

ISA bus notes
~~~~~~~~~~~~~

*VGA* cards used both memory and I/O, but really nothing beyond the first 1MB address range. They didn't use DMA. They might have used an interrupt
*Ethernet* cards used memory mapped ring buffers (I think) and I/O of course. Most were 16-bit, but no DMA and a few interrupts.
*Serial/parallel* cards used I/O and interrupt(s)
*IDE* interface used only a few I/O registers (16-bits) and (16-bit) DMA. It used a single interrupt line
*Sound* cards (at least Sound Blasters) used 16-bit I/O and (both 8- and 16-bot) DMA. They used interrupts as well.
*SCSI* cards are a bit tricky. Some Adaptec cards might even have been bus-masters. Others, such as the SYM20403 seems to have not even used DMAs. Many contained on-board BIOS, which of course is problematic.


DRAM interface
~~~~~~~~~~~~~~

There are two banks of DRAM, each divided into two 8-bit words. All DRAM pins are directly connected to the corresponding pins of the CPU and the DMA controller.

Buffer stage
~~~~~~~~~~~~

All other address regions go through a buffer stage to relieve the CPU and DMA controller from excessive loading.

::
    nBLCAS   <<= nLCAS
    nBUCAS   <<= nUCAS
    nBWE     <<= nWE
    nBNREN   <<= nNREN
    BA8_0    <<= A8_0
    BA9_1    <<= A9_1
    BA10_2   <<= A10_2
    BA11_3   <<= A11_3
    BA12_4   <<= A12_4
    BA13_5   <<= A13_5
    BA14_6   <<= A14_6
    BA15_7   <<= A15_7
    BA17_16  <<= A17_16
    BA19_18  <<= A19_18

.. note::
    nNREN does not have extensive loading on it, but it is still buffered to equalize delay between that and the address lines which it qualifies.

The data bus buffers are bi-directional 74LS245 devices. They are controlled by the following signals:

::

    DIR       <<=   nBWE ^ ~nDACK
    nLOE      <<=   (nBNREN & nDACK) | (nLCAS & nWE)
    nUOE      <<=   (nBNREN & nDACK) | (nUCAS & nWE)
    B0..B15   <<=>> D0..D15
    BD0..BD15 <<=>> A0..A15

To support 8-bit transactions on the buffered bus, we need a third 74LS245 connected the following way:

    DIR       <<=   nBWE ^ ~nDACK
    nODDOE    <<=   (nBNREN & nDACK) | (nUCAS & nWE)
    B0..B7    <<=>> D8..D15
    BD0..BD7  <<=>> A0..A7

.. note::
    The DMA controller generates addresses directly on the DRAM bus, but the data transfer is happening though these buffers and the requestor. Thus, during DMA operations, the buffers operate in the reverse directio_silicon_techn compared to CPU cycles. Another consequence of this setup is that DMA can only happen to/from DRAM.

Address decode and address latching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We need to identify the two address cycles:

::

    nBCAS  <<= nBLCAS & nBUCAS
    nBAC_1 <<= nBNREN | ~nBCAS
    nBAC_2 <<= nBNREN | nBCAS

Next, we'll need to latch the high-order address bits, using the first address cycle:

::

    BLA8  <<= latch(BA8_0,  nBAC_1)
    BLA9  <<= latch(BA9_1,  nBAC_1)
    BLA10 <<= latch(BA10_2, nBAC_1)
    BLA11 <<= latch(BA11_3, nBAC_1)
    BLA12 <<= latch(BA12_4, nBAC_1)
    BLA13 <<= latch(BA13_5, nBAC_1)
    BLA14 <<= latch(BA14_6, nBAC_1)
    BLA15 <<= latch(BA15_7, nBAC_1)

This can be done by an 74LS373.

We can now decode 4 address regions:

::

    nBLROM_SEL  <<= ~(BA19_18 == 0 & BA17_16 == 0) | nBAC_2
    nBHROM_SEL  <<= ~(BA19_18 == 0 & BA17_16 == 1) | nBAC_2
    nBIO_SEL    <<= ~(BA19_18 == 1 & BA17_16 == 0) | nBAC_2
    nBEXT_SEL   <<= ~(BA19_18 == 1 & BA17_16 == 1) | nBAC_2

This can be done by one half of a 74LS139.

I/O regions can be further decoded:

::

    nBIO0_SEL <<= ~(BLA13 == 0 & BLA14 == 0 & BLA15 == 0) | nBIO_SEL | nBAC_2
    ...
    nBIO7_SEL <<= ~(BLA13 == 1 & BLA14 == 1 & BLA15 == 1) | nBIO_SEL | nBAC_2

This can be directly implemented using a 74LS138.

Extension board I/O regions could also be decoded in a similar way. This gives each card 16kB (8kW) of I/O space:

::

    nBEXT0_SEL <<= ~(BLA13 == 0 & BLA14 == 0 & BLA15 == 0) | nBEXT_SEL | nBAC_2
    ...
    nBEXT7_SEL <<= ~(BLA13 == 1 & BLA14 == 1 & BLA15 == 1) | nBEXT_SEL | nBAC_2

8-bit peripheral
~~~~~~~~~~~~~~~~

8-bit peripherals are connected to only the lower 8-bits of the data-bus. 8-bit loads/stores work in this manner (due to the three bus-buffers), but 16-bit loads/stores don't. As a consequence, 8-bit peripherals need to be exclusively used with 8-bit loads/stores, even if adjacent registers comprise a 16-bit logical value.

EEPROM connection
~~~~~~~~~~~~~~~~~

There are up to 4 EEPROM devices in the system. They are configured into two banks. Their nCE is connected to nBLROM_SEL and nBHROM_SEL in pairs. Their nOE is connected to nBLCAS for the low-byte and nBUCAS for the high-byte devices.
