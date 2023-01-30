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
    DRAM_nRAS_A     ^^^^^^^^^\_____________________________/^^^^^\___________/^^^^^\___________/^^^^^\_____________________________/
    DRAM_nCAS_A     ^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^
    DRAM_nRAS_B     ^^^^^^^^^\_____________________________/^^^^^\___________/^^^^^\___________/^^^^^\_____________________________/
    DRAM_nCAS_B     ^^^^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^
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
    DRAM_nCAS_A     ^^^^^^^^^^^^\________/^^^^^^^^^^^^^^^^^^^^^^^^^^\______________/^^^^^^
    DRAM_nCAS_B     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\_____/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
2. Only 8-bit transfers are allowed
3. LSB can be recovered by an R/S flop: nR <<= DRAM_nCAS_A; nS <<= DRAM_nCAS_B. It is guaranteed
   that these signals never fall at the same time. It is also guaranteed that only one is low at
   any given time.
4. nWAIT is sampled on the rising edge of every cycle, after internal wait-states are accounted for
5. There is at least one internal wait-state
6. For writes, the relevant byte of 'req_data' should be valid.




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
* SCSI
* FDD

External keyboard setup (modern)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* custom CPU
* custom Graphics+sound+DMA
* custom Classic I/O (mouse/joystick/serial/external keyboard)
* custom Nuvou I/O (keyboard/joystick/mouse over USB; SDCard)

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
37         PC_4_MS_C   Port C but 6; PS/2 mouse port clock pin
38         PC_5_MS_D   Port C but 7; PS/2 mouse port data pin
39         VCC         Power input
40         GND         Ground input
========== =========== ===========


Nuvou I/O
---------

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
10         A19_18      Multiplexed address bus
11         A20_21      Multiplexed address bus
12         D0          Data bus
13         D1          Data bus
14         D2          Data bus
15         D3          Data bus
16         D4          Data bus
17         D5          Data bus
18         D6          Data bus
19         D7          Data bus
20         nRAS_B0     Active low row-select, bank 0
21         nCAS_B0     Active low column select, bank 0
22         nRAS_B1     Active low row-select, bank 1
23         nCAS_B1     Active low column select, bank 1
24         nWE         Active low write-enable
25         SYS_CLK     Clock input
26         nRST        Active low reset input
27         nINT        Active low interrupt output
28         nBREQ_IN    Active low bus-request daisy-chain input
29         nBREQ_OUT   Active low bus-request daisy-chain output
30         nBGRANT     Active low bus-grant input
31         nREG_CS     Active low chip-select for register accesses
32         D+          USB D+
33         D-          USB D-
34         SD_D0       SD card connector
35         SD_D1       SD card connector
36         SD_D2       SD card connector
37         SD_D3       SD card connector
38         SD_CMD      SD card connector
39         SD_CLK      SD card connector
40         XTAL_IN     48MHz crytal oscillator pins
41         XTAL_OUT    48MHz crytal oscillator pins
42
43
44
45
46
47         VCC         Power input
48         GND         Ground input
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
10         A19_18      Multiplexed address bus
11         A20_21      Multiplexed address bus
12         D0          Data bus
13         D1          Data bus
14         D2          Data bus
15         D3          Data bus
16         D4          Data bus
17         D5          Data bus
18         D6          Data bus
19         D7          Data bus
20         nRAS_B0     Active low row-select, bank 0
21         nCAS_B0     Active low column select, bank 0
22         nRAS_B1     Active low row-select, bank 1
23         nCAS_B1     Active low column select, bank 1
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
10         A19_18      Multiplexed address bus
11         A20_21      Multiplexed address bus
12         D0          Data bus
13         D1          Data bus
14         D2          Data bus
15         D3          Data bus
16         D4          Data bus
17         D5          Data bus
18         D6          Data bus
19         D7          Data bus
20         nRAS_B0     Active low row-select, bank 0
21         nCAS_B0     Active low column select, bank 0
22         nRAS_B1     Active low row-select, bank 1
23         nCAS_B1     Active low column select, bank 1
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

DMA extender
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
10         A19_18      Multiplexed address bus
11         A20_21      Multiplexed address bus
12         D0          Data bus
13         D1          Data bus
14         D2          Data bus
15         D3          Data bus
16         D4          Data bus
17         D5          Data bus
18         D6          Data bus
19         D7          Data bus
20         nRAS_B0     Active low row-select, bank 0
21         nCAS_B0     Active low column select, bank 0
22         nRAS_B1     Active low row-select, bank 1
23         nCAS_B1     Active low column select, bank 1
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
37         DMA_TC      DMA terminal count output
38
39         VCC         Power input
40         GND         Ground input
========== =========== ===========


Clocking
--------

We will stay with the very common NTSC clock rate of 28.63636MHz (double of what Amiga had). On top of that, we'll need 48MHz for USB (on Nuvou I/O of course)

* 28.63636MHz/2 -> Video clock (14.31818MHz)
* 28.63636MHz/3 -> system clock (~9.54MHz)
* 28.63636MHz/3 -> Audio clock option l (37.28kHz Fs)
* 28.63636MHz/4 -> Audio clock option 2 (27.96kHz Fs)

SPEED NOTES
===========

STEP 1
------

Apparently memory AV testing is a big problem: this is a large adder (memory offset computation), followed by a comparator, which drives do_branch, which in turn goes all over the place. With it, timing closure only reaches 39MHz.

For now, I've commented out to see what the impact is. But can we delay? If we've registered AV, it would fire on the first cycle of memory, the same time we issue the 'request' to the bus IF. That seems to be fine as long as we combine it into the request signal in memory.

There's also an 'is_exception' signal out of execute, plus ecause and rcause things. Those I think could also be easily registered.

The other consequence is that in the same cycle we signal the AV, we potentially accept the next instruction from decode. So we would need to be able to ignore that as well.

This helped quite a bit, getting from 39 to 47MHz.

STEP 2
------

Now the problem seems to be the reservation logic. It seems that this path is very much delay dominated. The path goes through 'mask_for_rd_eq_pc', which would be::

    $rd <- $pc

But judging from the path, it seems that the problem is the reservation board.

I have tried a different selector logic (SelectOne instead of '<<' operation). We'll see what that gets us, if anything. Unfortunately, putting a stage into this path is very bad for speed: it would mean that we woult not know if we've cleared the read-after-write hazards a cycle later.

STEP 3
------

We are hitting some butterfly effect now? The path (with slightly *worse* timing) is in the cbranch path. This *must* be through do_branch again, as we're hitting the fetch stage, but really the path is probably somewhat different as I don't see it on the path.

As a test, I'll add a register to do_branch in 'pipeline'. That of course is wrong, but should break these long paths.

STEP 4
------

We've arrived at the multiplier. I'm just going to comment it out for now, but first record the area as well. So, before area:

Well, shit, I've re-ran the router, instead of looking at the report...

    ---------------------------------------
    Resource Usage Report for BREW_V1

    Mapping to part: ice40hx8kcm225
    Cell usage:
    GND             26 uses
    SB_CARRY        638 uses
    SB_DFF          11 uses
    SB_DFFE         32 uses
    SB_DFFESR       336 uses
    SB_DFFSR        231 uses
    SB_DFFSS        6 uses
    SB_GB           5 uses
    SB_RAM256x16    6 uses
    VCC             26 uses
    SB_LUT4         3699 uses

    I/O ports: 38
    I/O primitives: 38
    SB_GB_IO       2 uses
    SB_IO          36 uses

    I/O Register bits:                  0
    Register bits not including I/Os:   616 (8%)

    RAM/ROM usage summary
    Block Rams : 6 of 32 (18%)

    Total load per clock:
    clk: 1

    @S |Mapping Summary:
    Total  LUTs: 3699 (48%)

    Distribution of All Consumed LUTs = LUT4
    Distribution of All Consumed Luts 3699 = 3699

After:

    ---------------------------------------
    Resource Usage Report for BREW_V1

    Mapping to part: ice40hx8kcm225
    Cell usage:
    GND             27 uses
    SB_CARRY        173 uses
    SB_DFF          11 uses
    SB_DFFE         57 uses
    SB_DFFESR       224 uses
    SB_DFFSR        310 uses
    SB_DFFSS        6 uses
    SB_GB           5 uses
    SB_RAM256x16    6 uses
    VCC             27 uses
    SB_LUT4         2839 uses

    I/O ports: 38
    I/O primitives: 38
    SB_GB_IO       2 uses
    SB_IO          36 uses

    I/O Register bits:                  0
    Register bits not including I/Os:   608 (7%)

    RAM/ROM usage summary
    Block Rams : 6 of 32 (18%)

    Total load per clock:
    clk: 1

    @S |Mapping Summary:
    Total  LUTs: 2839 (36%)

So, we've dropped from 3700 to 2840 LUTs. That's ~25% reduction. Nice!!

Still no benefit in speed though: we're still looking at ~50MHz clock rate.

STEP 5
------

We're not getting too far: we're back to the problem with reservation logic.

This is not looking good. The problem seems to be that the reservation logic holds up the acceptance from fetch (combinatorially), which holds up instruction assemble and through that inst_queue.

