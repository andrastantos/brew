New approach to memory accesses
===============================

I've lost my notes, but the idea is the following:

* Double-pumped data interface (8-bits wide)
* half-cycle delayed RAS for the two banks
* individual CAS for the two banks

This will collapse the pin-count on the CPU to 32 pins (will grow back up a little)
This will allow the combination of DMA+Graphics+Sound into a single, 44-pin chip (probably will grow up a little)
This will allow for 8-bit peripheral (nNREN) bus.
This will allow for 8-bit DMAs (on the DMA controller, if it exists)

So the latest run shows that we're 0.12mm^2 (goal is 0.08) and we can't close timing even on 100MHz (goal is 250). My work is cut out for me, I think...
My guess as to why there is such a dramatic change is that there was a rather serious bug in 'decode'. So, consequently, I think most of my area woes are
comming from 'decode' as well.

The DRAM bus::

                        <------- 4-beat burst -------------><---- single ----><---- single ----><---------- 4-beat burst ---------->
    CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
    DRAM_nRAS_A/B   ^^^^^^^^^\_____________________________/^^^^^\___________/^^^^^\___________/^^^^^\_____________________________/
    DRAM_nCAS_0     ^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^
    DRAM_nCAS_1     ^^^^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^
    DRAM_ADDR       ---------<==X=====X=====X=====X=====>--------<==X=====>--------<==X=====>--------<==X=====X=====X=====X=====>---
    DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    DRAM_DATA       --------------<>-<>-<>-<>-<>-<>-<>-<>-------------<>-<>--------------<>-<>-------------<>-<>-<>-<>-<>-<>-<>-<>--
    DRAM_nWE        ^^^^^^^^^\_____________________________/^^^^^\___________/^^^^^\___________/^^^^^\_____________________________/
    DRAM_DATA       ------------<==X==X==X==X==X==X==X==>-----------<==X==>-----------<==X==>-----------<==X==X==X==X==X==X==X==>---
    CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
    req_valid       ___/^^^^^^^^^^^^^^^^^^^^^^^\___________/^^^^^\___________/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\____________
    req_ready       ^^^^^^^^^^^^^^^^^^^^^^^^^^^\___________/^^^^^\___________/^^^^^\___________/^^^^^^^^^^^^^^^^^^^^^^^\___________/
    req_last        _____________________/^^^^^\___________/^^^^^\___________/^^^^^\_____________________________/^^^^^\____________
    req_wr          _________________________________________________________/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\____________
    req_addr        ---<=====X=====X=====X=====>-----------<=====>-----------<=====X=================X=====X=====X=====\____________
    req_data        ---------------------------------------------------------------<=================X=====X=====X=====>------------
                       |----------------->                 |---------------->|----------------->
    rsp_valid       _____________________/^^^^^^^^^^^^^^^^^^^^^^^\___________/^^^^^\___________/^^^^^\______________________________
    rsp_ready       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    rsp_data        ---------------------<=====X=====X=====X=====>-----------<=====>-----------<=====>------------------------------

Notes:
1. req_ready goes low for two cycles after req_last (during a transfer) is asserted. This is to allow for the pre-charge cycle to occur
2. addresses must be consecutive and must not cross page-boundary within a burst. The bus_if doesn't check for this (maybe it should assert???) and blindly puts the address on the DRAM bus. Address incrementing is the responsibility of the requestor (it probably does it anyway).
3. Burst length is not communicated a-priory over the interface: only the 'last' signal is provided.
4. write data is captured with the address on every transaction.
5. rsp_ready is not allowed to go low with outstanding reads.
6. writes don't have any response
7. Reads and writes are not allowed to be mixed within a burst. This is - again - not checked by the bus_if.
8. Client arbitration happens only after the idle cycle: i.e. we don't support clients taking over bursts from each other


Non-DRAM accesses:

                             <-- even read ---><--- odd write ---><- even read w. wait -->
    CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
    nNREN           ^^^^^^^^^\___________/^^^^^\___________/^^^^^\_________________/^^^^^^
    DRAM_nCAS_0     ^^^^^^^^^^^^^^^\_____/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\___________/^^^^^^
    DRAM_nCAS_1     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\_____/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    DRAM_ADDR       ---------<==X========>-----<==X========>-----<==X==============>------
    DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    DRAM_DATA       ---------------------<>----------------<>----------------------<>-----
    DRAM_nWE        ^^^^^^^^^\___________/^^^^^\___________/^^^^^\_________________/^^^^^^
    DRAM_DATA       ------------<========>-----------<=====>--------<==============>------
    nWAIT           ---------------/^^^^^\-----------/^^^^^\-----------\_____/^^^^^\------
    CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
    req_valid       ___/^^^^^\_____/^^^^^^^^^^^\___________/^^^^^\________________________
    req_ready       ^^^^^^^^^\___________/^^^^^\___________/^^^^^\_________________/^^^^^^
    req_last        ___/^^^^^\___________/^^^^^\___________/^^^^^\________________________
    req_wr          _______________/^^^^^^^^^^^\__________________________________________
    req_addr        ---<=====>-----<===========>-----------<=====>------------------------
    req_data        ---------------<===========>------------------------------------------
                       |----------------->                 |----------------------->
    rsp_valid       _____________________/^^^^^\___________________________________/^^^^^\
    rsp_ready       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    rsp_data        ---------------------<=====>-----------------------------------<=====>

1. Bursts are not allowed
2. Only 8-bit transfers are allowed - bus_if will break up 16-bit transfers
3. LSB address can be recovered by using DRAM_nCAS_0 as A0
4. nWAIT is sampled on the rising edge of every cycle, after internal wait-states are accounted for
5. There is at least one internal wait-state
6. For writes, the relevant byte of 'req_data' should be valid.

Number of address lines and banks needed
========================================

Since, I don't think we could either afford or drive more than 32 memory chips on the bus, with up to 4 banks we could support the following memory sizes:

1-bit chips:

====== ======== ========= ======================= ================= =============== ============ ===================
Year   Capacity Word size Number of address lines Capacity per bank Number of banks Max capacity Number of RAM chips
====== ======== ========= ======================= ================= =============== ============ ===================
1978   64kbit   1         8                       128kByte          1               128kByte     16
1978   64kbit   1         8                       128kByte          2               256kByte     32*
1982   256kbit  1         9                       512kByte          1               512kByte     16
1982   256kbit  1         9                       512kByte          2               1MByte       32*
1986   1Mbit    1         10                      2MByte            1               2MByte       16
1986   1Mbit    1         10                      2MByte            2               4MByte       32*
1988   4Mbit    1         11                      8MByte            1               8MByte       16
1988   4Mbit    1         11                      8MByte            2               16MByte      32*
1991   16Mbit   1         12                      32MByte           1               32MByte      16
====== ======== ========= ======================= ================= =============== ============ ===================

4-bit chips:

====== ======== ========= ======================= ================= =============== ============ ===================
Year   Capacity Word size Number of address lines Capacity per bank Number of banks Max capacity Number of RAM chips
====== ======== ========= ======================= ================= =============== ============ ===================
1982   256kbit  4         8                       128kByte          1               128kByte     4
1982   256kbit  4         8                       128kByte          2               256kByte     8
1982   256kbit  4         8                       128kByte          4               512kByte     16
1986   1Mbit    4         9                       512kByte          1               512MByte     4
1986   1Mbit    4         9                       512kByte          2               1MByte       8
1988   4Mbit    4         10                      2MByte            1               1MByte       4
1986   1Mbit    4         9                       512kByte          4               2MByte       16
1988   4Mbit    4         10                      2MByte            2               4MByte       8
1991   16Mbit   4         11                      8MByte            1               8MByte       4
1991   16Mbit   4         11                      8MByte            2               16MByte      8
====== ======== ========= ======================= ================= =============== ============ ===================

Here we assume that we can't have 4 banks of the larger chips since we mux the address lines with the bank-selects.

This shows that at our introduction year ('84) we should have been able to support 0.5M in 4-bit configs and 1M in 1-bit configs.

For modern systems though, (where we have access to all the DRAM sizes) we should probably go with only supporting 4-bit variants as those can also span the full supported ranges.

4-banks work especially well with EDI-style 72-pin SIMM memories. The 2-bank versions would only be able to use one side of the SIMM, so dual-sided SIMMs would only work at half capacity. We should only need a single memory socket, where a 32MByte double-sided module would get us the full 16MByte capacity.

How to make chips at home?
==========================

FPGAs, of course. Right now, it seems that Brew V1 synthesizes into ~800ALMs (LUTs) in a Cyclone V. Assuming a LUT is a LUT (not fair, depends on the number of inputs), any device with about 1000 to 2000 LUTs should fit the CPU. We also assume that the CPU is the biggest custom chip we would ever make, this same FPGA should fit all custom chips.

Looking at the landscape, the following devices seem to work well:

- Any of the MAX10 family, even the smallest one. However, these are *very* expensive, something around $30.
- ICE40LP1k from Lattice at $5.5
- T8Q144C3 from Efinix at $6.6
- LCMXO3L-4300E from Lattice at $7.7

These are all 3.3V compatible parts, but only the latest lists LVTTL compatibility. However, LVTTL and LVCMOS33 is the same, it seems. Even the Mach3 chip can't take truly 5V inputs. So maybe that's another reason to require serial resistors on the DRAM data lines (and protection diodes). After all, those will be the only truly old parts in the real system. However, there seem to be 'modern' 3.3V FPM DRAMs (https://www.digikey.com/htmldatasheets/production/1700164/0/0/1/MSM51V17400F.pdf for instance).

All right, so we have 3.3V I/O and we're cool with that. We might have to build our own memory modules (after all SMD chips were not common back then). 30-pin sockets are not readily available anymore (eBay, others still carry it, but do we want to depend on them)? We can have the equivalent using single-row headers though, so no sweat.

These 3.3V memories are rather large, the above one is 16Mbit device in 4Mx4 config. So, qin a 16-bit (dual-bank) setup, that would create 8MByte of DRAM. That's 11 address lines: A0-A10. Righ now, we have only 9, but with the reduction of the data-bus to 8-bits, we can affort the extra address pin, at least on the CPU.

Current chipset idea
--------------------

The original I/O chip design had essentially 2 8-bit and 1 4-bit GPIO ports. We would need two of those to get keyboard scanning as well, but they were already 8-bit chips with no DMA.

SCSI needs DMA (PC-style); otherwise it's 8-bit. It's pretty speedy with read/write cycles < 100ns
The FDD controller also needs DMA (PC-style); otherwise it's 8-bit. It's rather slow with ~200ns access times.

In other words, we would need 2 DMA channels, probably the chipset would look like this:

Integrated keyboard setup (A500 layout)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* custom CPU
* custom Graphics+sound+DMA
* custom Classic I/O 1 (mouse/joystick/serial/I2C)
* custom Classic I/O 2 (keyboard scan)
* FDD

External keyboard setup (A1000 layout)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* custom CPU
* custom Graphics+sound+DMA
* custom Classic I/O (mouse/joystick/serial/external keyboard)
* custom bus-extender (DMA+interrupt)
* SCSI
* FDD

External keyboard setup (modern A500 layout)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* custom CPU
* custom Graphics+sound+DMA
* custom Nuvou I/O 2 (keyboard/joystick/mouse over USB; SDCard; serial/sysconfig/I2C)

Expandable setup (modern A1000 layout)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* custom CPU
* custom Graphics+sound+DMA
* custom Nuvou I/O 2 (keyboard/joystick/mouse over USB; SDCard; serial/sysconfig/I2C)
* custom bus-extender (DMA+interrupt)


Pinouts
=======

According to https://en.wikipedia.org/wiki/Dual_in-line_package:

Common DIP package pin counts are: 24, 28, 32, and 40; less common are 36, 48, 52, and 64. So if we blow over 40 pins, the next step up is 48.

Classic I/O
-----------

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
9          A0          Register address bus (data/command port select)
10         nCS         Active low chip-select for register accesses
11         nWE         Active low register write-enable input
12         nRST        Active low reset input
13         nINT        Open collector, active low interrupt output
14         SYS_CLK     System clock input
15         PA_0_EN1_A  Port A bit 0; Quarature encoder 1 input A
16         PA_1_EN1_B  Port A bit 1; Quarature encoder 1 input B
17         PA_2_EN2_A  Port A bit 2; Quarature encoder 2 input A
18         PA_3_EN2_B  Port A bit 3; Quarature encoder 2 input B
19         PA_4_TMR1   Port A bit 4; Timer input/output 1
20         PA_5_TMR2   Port A bit 5; Timer input/output 2
21         PA_6_SDA    Port A bit 6; I2C data
22         PA_7_SCL    Port A bit 7; I2C clock
23         PB_0_EN2_A  Port B bit 0; Quarature encoder 3 input A
24         PB_1_EN2_B  Port B bit 1; Quarature encoder 3 input B
25         PB_2_EN3_A  Port B bit 2; Quarature encoder 4 input A
26         PB_3_EN3_B  Port B bit 3; Quarature encoder 4 input B
27         PB_4_TMR2   Port B bit 4; Timer input/output 3
28         PB_5_TMR3   Port B bit 5; Timer input/output 4
29         PB_6        Port B bit 6;
30         PB_7        Port B bit 7;
31         PC_0_TXD    Port C bit 0; serial RX
32         PC_1_RXD    Port C bit 1; serial TX
33         PC_2_RST    Port C bit 2; serial RST
34         PC_3_CTS    Port C bit 3; serial CST
35         PC_4_KB_C   Port C but 4; PS/2 keyboard port clock pin
36         PC_5_KB_D   Port C but 5; PS/2 keyboard port data pin
37         PC_6_MS_C   Port C but 6; PS/2 mouse port clock pin
38         PC_7_MS_D   Port C but 7; PS/2 mouse port data pin
39         VCC         Power input
40         GND         Ground input
========== =========== ===========

Nuvou I/O
---------

========== =========== ===========
Pin Number Pin Name    Description
========== =========== ===========
1          A0          Address bus
2          A1          Address bus
3          A2          Address bus
4          A3          Address bus
5          D0          Data bus
6          D1          Data bus
7          D2          Data bus
8          D3          Data bus
9          D4          Data bus
10         D5          Data bus
11         D6          Data bus
12         D7          Data bus
13         nDRQ        Active low DMA request
14         nDACK       Active low DMA response
15         nDMA_TC     DMA terminal count
16         nCS         Active low chip select
17         nWE         Active low write-enable
18         SYS_CLK     Clock input
19         nRST        Active low reset input
20         nINT        Active low interrupt output
21         D+          USB D+
22         D-          USB D-
23         SD_D0       SD card connector
24         SD_D1       SD card connector
25         SD_D2       SD card connector
26         SD_D3       SD card connector
27         SD_CMD      SD card connector
28         SD_CLK      SD card connector
29         XTAL_IN     48MHz crytal oscillator pins
30         XTAL_OUT    48MHz crytal oscillator pins
31         PA_0_TXD    Port A bit 0; serial RX
32         PA_1_RXD    Port A bit 1; serial TX
33         PA_2_RST    Port A bit 2; serial RST
34         PA_3_CTS    Port A bit 3; serial CST
35         PA_4_KB_C   Port A but 4; PS/2 keyboard port clock pin
36         PA_5_KB_D   Port A but 5; PS/2 keyboard port data pin
37         PA_6_SDA    Port A bit 6; I2C data
38         PA_7_SCL    Port A bit 7; I2C clock
39         VCC         Power input
40         GND         Ground input
========== =========== ===========


Combined graphics/sound/DMA
---------------------------

========== =========== ===========
Pin Number Pin Name    Description
========== =========== ===========
1          A8_0        Multiplexed address bus
2          A9_1        Multiplexed address bus
3          A10_2       Multiplexed address bus
4          A11_3       Multiplexed address bus
5          A12_4       Multiplexed address bus
6          A13_5       Multiplexed address bus
7          A14_6       Multiplexed address bus
8          A15_7       Multiplexed address bus
9          A17_16      Multiplexed address bus
10         A19_18      Multiplexed address bus, nRAS_C for bank C
11         A20_21      Multiplexed address bus, nRAS_D for bank D
12         D0          Data bus
13         D1          Data bus
14         D2          Data bus
15         D3          Data bus
16         D4          Data bus
17         D5          Data bus
18         D6          Data bus
19         D7          Data bus
20         nRAS_A      Active low row-select, bank A
21         nRAS_B      Active low row-select, bank B
22         nCAS_0      Active low column select, byte 0
23         nCAS_1      Active low column select, byte 1
24         nWE         Active low write-enable
25         SYS_CLK     Clock output
26         nRST        Active low reset input
27         nINT        Active low interrupt output
28         nBREQ_IN    Active low bus-request daisy-chain input
29         nBREQ_OUT   Active low bus-request daisy-chain output
30         nBGRANT     Active low bus-grant input
31         nREG_CS     Active low chip-select for register accesses
32         R           Analog 'red' channel output
33         G           Analog 'green' channel output
34         B           Analog 'blue' channel output
35         BLANK       Video blanking output with programmable polarity
36         HSYNC       Horizontal video sync output with programmable polarity
37         VSYNC       Vertical video sync output with programmable polarity
38         AUD_L_OUT   Audio output left channel
39         AUD_R_OUT   Audio output right channel
40         AUD_IN      Audio input
41         XTAL_IN     28.63636MHz crystal oscillator pins
42         XTAL_OUT    28.63636MHz crystal oscillator pins
43
44
45
46
47         VCC         Power input
48         GND         Ground input
========== =========== ===========

CPU
---

========== =========== ===========
Pin Number Pin Name    Description
========== =========== ===========
1          A8_0        Multiplexed address bus
2          A9_1        Multiplexed address bus
3          A10_2       Multiplexed address bus
4          A11_3       Multiplexed address bus
5          A12_4       Multiplexed address bus
6          A13_5       Multiplexed address bus
7          A14_6       Multiplexed address bus
8          A15_7       Multiplexed address bus
9          A17_16      Multiplexed address bus
10         A19_18      Multiplexed address bus, nRAS_C for bank C
11         A20_21      Multiplexed address bus, nRAS_D for bank D
12         D0          Data bus
13         D1          Data bus
14         D2          Data bus
15         D3          Data bus
16         D4          Data bus
17         D5          Data bus
18         D6          Data bus
19         D7          Data bus
20         nRAS_A      Active low row-select, bank A
21         nRAS_B      Active low row-select, bank B
22         nCAS_0      Active low column select, byte 0
23         nCAS_1      Active low column select, byte 1
24         nNREN       Active low non-DRAM bus cycle qualifier
25         nWE         Active low write-enable
26         nWAIT       Active low wait-state input
27         SYS_CLK     Clock input
28         nRST        Active low reset input
29         nINT        Active low interrupt input
30         nBREQ_A     Active low bus-request input
31         nBGRANT_A   Active low bus-grant output
32         nBREQ_B     Active low bus-request input
33         nBGRANT_B   Active low bus-grant output
33         nDRQ_A      DMA channel A request input
34         nDACK_A     DMA channel A acknowledge output
35         nDRQ_B      DMA channel B request input
36         nDACK_B     DMA channel B acknowledge output
37         DMA_TC      DMA terminal count output
39         VCC         Power input
40         GND         Ground input
========== =========== ===========

New additions:

* A second request/grant pair so that I/O and graphics doesn't have to daisy-chain
* Built-in DMA controller for external peripherals (if doesn't fit, more bus request/grant channels as those are cheap)

Bus extender
------------

========== =========== ===========
Pin Number Pin Name    Description
========== =========== ===========
1          A8_0        Multiplexed address bus
2          A9_1        Multiplexed address bus
3          A10_2       Multiplexed address bus
4          A11_3       Multiplexed address bus
5          A12_4       Multiplexed address bus
6          A13_5       Multiplexed address bus
7          A14_6       Multiplexed address bus
8          A15_7       Multiplexed address bus
9          A17_16      Multiplexed address bus
10         A19_18      Multiplexed address bus, nRAS_C for bank C
11         A20_21      Multiplexed address bus, nRAS_D for bank D
12         D0          Data bus
13         D1          Data bus
14         D2          Data bus
15         D3          Data bus
16         D4          Data bus
17         D5          Data bus
18         D6          Data bus
19         D7          Data bus
20         nRAS_A      Active low row-select, bank A
21         nRAS_B      Active low row-select, bank B
22         nCAS_0      Active low column select, byte 0
23         nCAS_1      Active low column select, byte 1
24         nWE         Active low write-enable
25         SYS_CLK     Clock input
26         nRST        Active low reset input
27         nINT        Active low interrupt output
28         nBREQ_IN    Active low bus-request daisy-chain input
29         nBREQ_OUT   Active low bus-request daisy-chain output
30         nBGRANT     Active low bus-grant input
31         nWAIT       Active low wait-state output
32         nREG_CS     Active low chip-select for register accesses
33         nDRQ_A      DMA channel A request input
34         nDACK_A     DMA channel A acknowledge output
35         nDRQ_B      DMA channel B request input
36         nDACK_B     DMA channel B acknowledge output
37         nDRQ_C      DMA channel C request input
38         nDACK_C     DMA channel C acknowledge output
39         DMA_TC      DMA terminal count output
41         IRQ_A       Interrupt signal A
42         IRQ_B       Interrupt signal B
43         IRQ_C       Interrupt signal C
44         IRQ_D       Interrupt signal D
45         IRQ_E       Interrupt signal E
46
47         VCC         Power input
48         GND         Ground input
========== =========== ===========


Clocking
--------

We will stay with the very common NTSC clock rate of 28.63636MHz (double of what Amiga had). On top of that, we'll need 48MHz for USB (on Nuvou I/O of course)

* 28.63636MHz/2 -> Video clock (14.31818MHz)
* 28.63636MHz/3 -> system clock (~9.54MHz)
* 28.63636MHz/3 -> Audio clock option l (37.28kHz Fs)
* 28.63636MHz/4 -> Audio clock option 2 (27.96kHz Fs)

ISA bus notes
=============

*VGA* cards used both memory and I/O, but really nothing beyond the first 1MB address range. They didn't use DMA. They might have used an interrupt
*Ethernet* cards used memory mapped ring buffers (I think) and I/O of course. Most were 16-bit, but no DMA and a few interrupts.
*Serial/parallel* cards used I/O and interrupt(s)
*IDE* interface used only a few I/O registers (16-bits) and (16-bit) DMA. It used a single interrupt line
*Sound* cards (at least Sound Blasters) used 16-bit I/O and (both 8- and 16-bot) DMA. They used interrupts as well.
*SCSI* cards are a bit tricky. Some Adaptec cards might even have been bus-masters. Others, such as the SYM20403 seems to have not even used DMAs. Many contained on-board BIOS, which of course is problematic.
