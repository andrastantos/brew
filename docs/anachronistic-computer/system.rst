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
0x0000_0000    0x0000_ffff  EEPROM space 1 (and boot code from address 0)
0x0001_0000    0x0001_ffff  reserved (alias of EEPROM space 1)
0x0002_0000    0x0002_ffff  EEPROM space 2
0x0003_0000    0x0003_ffff  reserved (alias of EEPROM space 2)
0x0004_0000    0x0004_ffff  internal I/O space
0x0005_0000    0x0005_ffff  reserved (alias of internal I/O space)
0x0006_0000    0x0006_ffff  ISA extension bus I/O address space
0x0007_0000    0x0007_ffff  ISA extension bus memory address space
0x4000_0000    0x43ff_ffff  CSR space: not decoded externally, but handled by the processor internally
0x8000_0000    0x80ff_ffff  DRAM space (up to 16MByte)
=============  ===========  ===========

.. note::
    Address bits 29...26 select number of wait-states for every address. That is to say, each region has 16 aliases.
    DRAM and CSR space ignores the wait-state setting, but still aliases the addresses 16 times.

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
* ISA-bus interface (maybe custom interface chip, maybe custom Classic I/O)
* SCSI
* FDD

All-in-one setup (modern A500 layout)
-------------------------------------

* custom CPU+DMA
* custom Graphics+sound
* custom Nouveau I/O (keyboard/joystick/mouse over USB; SDCard; serial/sysconfig/I2C)
* custom Classic I/O (keyboard scan, unless done through USB)

Expandable setup (modern A1000 layout)
--------------------------------------

* custom CPU+DMA
* custom Graphics+sound
* custom Nouveau I/O 2 (keyboard/joystick/mouse over USB; SDCard; serial/sysconfig/I2C)
* ISA-bus interface (maybe custom interface chip, maybe custom Classic I/O)


Clocking
~~~~~~~~

We will stay with the very common NTSC clock rate of 28.63636MHz (double of what Amiga had). On top of that, we'll need 48MHz for USB (on Nouveau I/O of course)

* 28.63636MHz/2   -> Video clock (14.31818MHz)
* 28.63636MHz/3   -> system clock (~9.54MHz) <-- supports -10 DRAM
* 28.63636MHz/4   -> alternate system clock (~7.16MHz) <-- supports -12 DRAM
* 28.63636MHz/3.5 -> alternate system clock (~8.18MHz) <-- supports -12 DRAM, but needs a PLL, which wasn't really a thing back then, not at these frequencies at least.
* 28.63636MHz/3   -> Audio clock option l (37.28kHz Fs)
* 28.63636MHz/4   -> Audio clock option 2 (27.96kHz Fs)

An alternative would be to use an additional clock source for the system clock (which would allow for highest memory bandwidth and CPU perf.)
We could even add a third (audio) clock, or at least the option to use either clock for audio.

This is getting mad. I think the right answer is the following:

14.31818MHz clock source for video and audio (27.96kHz sampling rate); In actual implementation, probably twice of that so that VGA doesn't get terribly angry
6.6/8.3/10/12.5MHz clock source for system

These would be two different clocks, provided by three different crystals/oscillators. It's unclear whether a simple 74f04 based oscillator would work. The CMOS version (unbuffered mostly) should, but TTL is different enough that it might not. CMOS inverters of the day (CD4069) were *way* too slow to build inverters in these clock-ranges. Also, there would need to be a schmidt-trigger stage afterwards to create proper signal levels. So that would be two packages for this purpose. It would probably have been cheaper and certainly easier to use oscillators.

For USB, a third clock source of 48MHz is needed, but that is an XTAL connected to I/o Nouveau. clock source for USB

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

There are up to 4 banks of DRAM, each divided into two 8-bit words. All DRAM pins are directly connected to the corresponding pins of the CPU and all other bus-masters.

Buffer stage
~~~~~~~~~~~~

Non-DRAM accesses go through a buffer stage to relieve the bus-masters from excessive loading. The exception is register access to other bus-masters: since the address and data-buses are already connected, only a single chip-select needs to be provided. This chip-select is decoded from the buffered addresses.

::
    nBCAS_0  <<= nCAS_0
    nBCAS_1  <<= nCAS_1
    nBWE     <<= nWE
    nBNREN   <<= nNREN

    BA9_1    <<= A8_0
    BA10_2   <<= A9_1
    BA11_3   <<= A10_2
    BA12_4   <<= A11_3

    BA13_5   <<= A12_4
    BA14_6   <<= A13_5
    BA15_7   <<= A14_6
    BA16_8   <<= A15_7

    BA18_17  <<= A17_16
    BDMA_TC  <<= DMA_TC

.. note::
    nNREN does not have extensive loading on it, but it is still buffered to equalize delay between that and the address lines which it qualifies.

.. note::
    We are renaming buffered addresses: they are 16-bit addresses as they come out of the CPU, but we need byte-addresses on the buffered bus. We're also renaming the top three address bits to match what the actually do during non-DRAM accesses.

.. note::
    We're not buffering the top two multiplexed address lines as they could be extra DRAM bank-selects.

The data bus buffer is a bi-directional 74LS245 device. It is controlled by the following signals:

::
    nDACK     <<=   nDACK_A & nDACK_B & nDACK_C & nDACK_D
    DIR       <<=   nBWE ^ ~nDACK
    nOE       <<=   (nBNREN & nDACK)
    B0..B7    <<=>> D0..D7
    BD0..BD7  <<=>> A0..A7

.. note::
    The DMA controller generates addresses directly on the DRAM bus, but the data transfer is happening though the buffer and the requestor. Thus, during DMA operations, the buffer operates in the reverse direction compared to CPU cycles. Another consequence of this setup is that DMA can only happen to/from DRAM, not between I/O devices or memory-to-memory.

Address decode and address latching
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We need to identify the two address cycles (nBAC_1 and nBAC_2):

::

    nBCAS  <<= nBCAS_0 & nBCAS_1 = ~(~(nBCAS_0 & nBCAS_1))
    nBAC_1 <<= nBNREN | ~nBCAS = ~(~nBNREN & nBCAS)
    nBAC_2 <<= nBNREN | nBCAS  = ~(~nBNREN & ~nBCAS)

Next, we'll need to latch the high-order address bits, using the first address cycle:

::

    BLA9   <<= latch(BA9_1,   nBAC_1)
    BLA10  <<= latch(BA10_2,  nBAC_1)
    BLA11  <<= latch(BA11_3,  nBAC_1)
    BLA12  <<= latch(BA12_4,  nBAC_1)
    BLA13  <<= latch(BA13_5,  nBAC_1)
    BLA14  <<= latch(BA14_6,  nBAC_1)
    BLA15  <<= latch(BA15_7,  nBAC_1)
    BLA16  <<= latch(BA16_8,  nBAC_1)

This can be done by an 74LS373.

    BLA18 <<= latch(BA18_17, nBAC_1)

.. note:: This needs an extra latch. The top 2 address lines could be used as extra DRAM bank-selects, so let's not depend on them.

We can also rename the renaming signals to create the bottom address bits:

    BA0  <<= nBCAS_0
    BA1  <<= BA9_1
    BA2  <<= BA10_2
    BA3  <<= BA11_3
    BA4  <<= BA12_4
    BA5  <<= BA13_5
    BA6  <<= BA14_6
    BA7  <<= BA15_7
    BA8  <<= BA16_8
    BA17 <<= BA18_17

This is just wires, no magic here. But it does help with further explanations.

We can now decode 4 address regions:

::

    nBROM1_SEL   <<= ~((BLA18 == 0) & (BLA16 == 0)) | nBNREN
    nBROM2_SEL   <<= ~((BLA18 == 0) & (BLA16 == 1)) | nBNREN
    nBIO_SEL     <<= ~((BLA18 == 1) & (BLA16 == 0)) | nBNREN
    nBISA_SEL    <<= ~((BLA18 == 1) & (BLA16 == 1)) | nBNREN

This can be done by one half of a 74LS139.

.. note:: By paying the price of the extra latch, we can decode fully based on addresses available in nBAC_1, so we can qualify the decode simply with nBNREN. This is important as it buys us about 100ns of decode time.

.. note:: The address mapping is a bit strange: each 64k region is aliased twice. That's the other price we pay for the extra decode time. We will take advantage of that in the ISA-bus decoder below

I/O region can be further decoded:

::
    nBIO0_SEL <<= ~((BLA15 == 0) & (BLA14 == 0)) | nBAC_2
    nBIO1_SEL <<= ~((BLA15 == 0) & (BLA14 == 1)) | nBAC_2
    nBIO2_SEL <<= ~((BLA15 == 1) & (BLA14 == 0)) | nBAC_2
    nBIO3_SEL <<= ~((BLA15 == 1) & (BLA14 == 1)) | nBAC_2

This is the second half of the same 74LS139. If more address regions are needed (unlikely), we could use a 74LS138 instead.

ISA bus
~~~~~~~

Spec: http://www.ee.nmt.edu/~rison/ee352_spr12/PC104timing.pdf and http://www.bitsavers.org/pdf/intel/_busSpec/Intel_ISA_Spec2.01_Sep89.pdf; pinout https://en.wikipedia.org/wiki/Industry_Standard_Architecture#/media/File:XT_Bus_pins.svg

On the ISA bus, we support only I/O (IOR/IOW) transactions and memory transactions in a windowed fashion:

First, we need to decode the IO and MEM read/write signals:

ISA_nIOR     <<= ((BA17 == 0) & (nBWE == 1)) | nBAC_2 | nBISA_SEL
ISA_nIOW     <<= ((BA17 == 0) & (nBWE == 0)) | nBAC_2 | nBISA_SEL
ISA_nMEMR    <<= ((BA17 == 1) & (nBWE == 1)) | nBAC_2 | nBISA_SEL
ISA_nMEMR    <<= ((BA17 == 1) & (nBWE == 0)) | nBAC_2 | nBISA_SEL

This can be done by a single 74LS138, or half of a 74LS139, plus an OR gate, if we have some left.

The ISA address bits are going as follows:

    ISA_A0 <<= BA0
    ISA_A1 <<= BA1
    ISA_A2 <<= BA2
    ISA_A3 <<= BA3
    ISA_A4 <<= BA4
    ISA_A5 <<= BA5
    ISA_A6 <<= BA6
    ISA_A7 <<= BA7
    ISA_A8 <<= BA8
    ISA_A9 <<= BLA9
    ISA_A10 <<= BLA10
    ISA_A11 <<= BLA11
    ISA_A12 <<= BLA12
    ISA_A13 <<= BLA13
    ISA_A14 <<= BLA14
    ISA_A15 <<= BLA15

These could be wires, but probably best if they are buffered again.

Somehow we would need to re-create the top 4 bits of the address-bus:

    ISA_A16
    ISA_A17
    ISA_A18
    ISA_A19

.. todo:: This must come from either a GPIO group or some other user-programmable window register.

The following signals would need to be re-created:

    ISA_D0-7 <<=>> B0-7

.. todo:: do we need to re-buffer it with another bi-directional buffer??

    ISA_AEN       <<= ~nDACK // active high address enable for DMA cycles
    nWAIT         <<= open_collector(ISA_IO_CH_RDY)
    ISA_ALE       <<= ~nBISA_SEL
    ISA_TC        <<= BDMA_TC
    ISA_nDACK1    <<= nDACK_B
    ISA_nDACK2    <<= nDACK_C
    ISA_nDACK3    <<= nDACK_D
    nDRQ_B        <<= ~ISA_DRQ1
    nDRQ_C        <<= ~ISA_DRQ2
    nDRQ_D        <<= ~ISA_DRQ3
    ISA_RST       <<= ~nRST

There are 7 inverters and 4 OR gates needed here. We also need an open-collector driver for nWAIT.

This leaves with interrupt signals. These need to go ... somewhere. I'm starting to think that a simple I/O controller chip would do the job. It would be an overkill, but would support both the address page generation above and the interrupt routing.

    ISA_IRQ2      =>>
    ISA_IRQ3      =>>
    ISA_IRQ4      =>>
    ISA_IRQ5      =>>
    ISA_IRQ6      =>>
    ISA_IRQ7      =>>

Total chip-count tally
~~~~~~~~~~~~~~~~~~~~~~

74LS244 - address buffer
74LS244 - address buffer
74LS245 - data buffer
74LS13  - dual 4-input AND gate: nDACK and data-buffer nOE generation. Could be 74LS11 triple 3-input AND gate as well.
74LS86  - XOR 1 gate used to generate data-buffer DIR
74LS00  - quad NAND gate; 2 used to generate nBCAS and ~nBCAS, one used to invert nDACK for data-buffer DIR generation, one used to invert nBNREN
74LS00  - quad NAND gate; 2 used to generate nBAC_1 and nBAC_2
74LS373 - address latch
74LS75  - quad latch, one bit used for BLA18.
74LS139 - address decode

For ISA interface, we need:

- ~nDACK (already available)
- 3 inverters for DRQ signals (the three remaining XOR gates)
- 2 inverters for ISA_ALE and ISA_RST: the two remaining NAND gates
74LS138 - control signal decode
74LS244 - ISA address buffer
74LS244 - ISA address buffer
74LS245 - ISA data buffer
74LS09  - quad open-collector AND gate; one used to generate the DIR signal for the ISA data buffer; one used to buffer ISA_IO_CH_RDY

We're left with:

3 transparent latches
2 open-collector AND2 gates

We can probably consolidate quite a few of this into a couple of PLAs, but I won't do it, I don't think as it's much harder to build at home.
This is a total of 16 jelly-bean chips.

An entry-level system would be (single ISA card slot as an expansion slot in the back, no ISA buffers):

4  custom chips
1  FDD ctrl
2  EPROMs
16 DRAM chips
13 jelly-bean chips
2  crystal oscillators
1  RTC chip, if I can find one (I2C something something)
1  SRAM chip (I2C again, if I can find one)

A high-end system would be like

3  custom chips
1  FDD ctrl
1  SCSI ctrl
2  EPROMs
32 DRAM chips
16 jelly-bean chips
2  crystal oscillators
1  RTC chip, if I can find one (I2C something something)
1  SRAM chip (I2C again, if I can find one)

Turns out the delta is not that large, especially because one doesn't have to populate the SCSI chip or the 16 extra DRAMs. Really it boils down to the extra I/O chip for keyboard-scanning. So, if one is smart, a single PCB could be used for both chassis.

RTC
~~~

OkiData M6242 apparently is a parallel-interface (4-bit??) CMOS RTC/Calendar that was used in some A600 expansion boards.
The Archimedes had a different (I2C) based solution: PCF8573/PCF8570, later PCF8583 (all detailed in the '97 I2C handbook from Philips http://www.bitsavers.org/components/philips/_dataBooks/1997_IC12_Philips_I2C_Peripherals.pdf)

The PCF8576/77 LCD drivers are mentioned in an '86 databook. Logic would say, they're newer than the 70/73.

The early MACs used a different RTC chip. There is a project to replace them with an ATTiny: https://www.quantulum.co.uk/blog/new-timepiece-for-a-classic-mac-part-1/ with protocol and everything, except for the part number... Suffice to say, it used 3 GPIOs and provided a 1sec pulse output.

The early PCs used a Motorola MC146818 part. This was a parallel-bus device with a multiplexed data/address interface (a'la 8085). Though even the datasheet shows how to interface to non-multiplexed devices (essentially use 'AS' pin as A0). https://www.nxp.com/docs/en/data-sheet/MC146818.pdf

Logic families
~~~~~~~~~~~~~~

The 74HC/74HCT/74HCU families were available by '85 from Philips. These had rise/fall times in the range of 15-22ns @5V and NAND2 propagation delays of 20-30ns

External connectors
~~~~~~~~~~~~~~~~~~~

Normal connectors of the time:
- Cartridge/expansion connector (for us it would be a single ISA8 connector)
- Centronics printer port
- RS232 serial port
- Audio/Video
- External disk drive connector
- Keyboard/mouse/joystick connector
- SCSI (or other HDD) as of 1986 on the MAC plus, Atari ST at 1985.