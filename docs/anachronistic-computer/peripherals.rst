Peripherals
===========

Serial ports
------------

For this machine we will simply reuse something that's on the market already. With hind-sight, I will standardize on the Intel 8250, later 16450/16550 UARTs.

Here's a relatively modern datasheet for the original part: https://www.moxsyn.com/Altera/c8250.pdf.

Modern variants are still available, so no issues here.


Floppy interface
----------------

Here I really don't want to re-invent the wheel, so the more pertinent
question is: can we handle standard floppy-controllers?

Here's one popular in PCs:
- https://ardent-tool.com/datasheets/NEC_uPD72065.pdf
- http://dunfield.classiccmp.org/r/765.pdf
A much more modern version (this one is still in stock in some places):
- https://www.mouser.com/datasheet/2/268/37c78-468028.pdf

This guy can interface directly to our bus and our DMA bridge for DMAs. No issues what so ever, except that the reset input is active-high.

There were other controllers as well:
 - http://www.bitsavers.org/components/westernDigital/WD57C65_Floppy_Disk_Subsystem_Controller_May88.pdf
 - http://www.bitsavers.org/components/westernDigital/FD179X-02_Data_Sheet_May1980.pdf
 - http://info-coach.fr/atari/documents/_mydoc/WD1772-JLG.pdf

I will stick with the PC-style FDD controller though, especially with the knowledge of hind-sight.


Hard drive interface
--------------------

If this computer was to compete with early PCs and MACs, initially HDD support is not a primary concern. Yet, pretty soon it would become mandatory and that would probably be apparent already during development. So how would one interface an HDD in those days?
- MAC went the SCSI way with the `AM5380 <https://amazingdiy.files.wordpress.com/2012/09/am5380pc.pdf>`_ chip
- PC/TX went originally with MFM/RLE interfaces

For SCSI, the interface is rather common. It seems that the only thing we would need is to invert TC to connect to nEOP.

This 5380 chip seems to have been rather popular with many clones:
- Z53C80 was until fairly recently in production from Zilog (http://www.zilog.com/docs/serial/ps0108.pdf). Even Mouser has some limited stock and it's still in Digikeys' catalog.
- NCR53c80
There is a whole family of these as well:
- 53c90/94/95/96
- 53c400 (still available at least for some retailers)
- 53c416 (16-bit version)
- 53c7x0 (32-bit, designed for i386 and such, but might work on 16-bit buses as well)
- 53cf90/94/96

http://www.bitsavers.org/components/westernDigital/WD33C93A_SCSI_Bus_Controller_1991.pdf is another SCSI controller. It has roughly the same signals as the AM5380 part. Except there's no interrupt. Also an 8-bit part.

Early MFM controller boards were a chip cemetary, and looking up datasheets from bitsavers confirms this: there was a chipset fro WDC, but it consisted of several parts and required a bazzilion glue-logic chips to make it work. I would have passed on it rather quicly.

Then there was IDE (a.k.a ATA), but that was introduced later, at '87: https://en.wikipedia.org/wiki/Parallel_ATA#IDE_and_ATA-1

.. note:: Early Amiga HDDs also used some sort of SCSI interface.

.. note::
    Atari apparently had a ACSI to MFM interface in SH-205. It's heart is the AIC-010 chip from Adaptec http://www.bitsavers.org/pdf/adaptec/asic/AIC-010_Programmable_Mass_Storage_Controller.pdf

    This chip doesn't have DMA apparently, but does have a multiplexed data/address bus. This would need significant external logic for us. It is a chip-graveyard anyway, as external buffers are also needed to buffer the data.

There was the `HD63463 <https://datasheetspdf.com/pdf-file/1285972/HitachiSemiconductor/HD63463/1>`_ for MFM controllers. It seems a very nice 16-bit, DMA-based controller, not a nuisance to deal with at all. A decent competitor to the SCSI chips above. I just don't know when it was introduced. It was used in the `Archimedes A300 Hard-disk poddle <http://chrisacorns.computinghistory.org.uk/docs/Acorn/Manuals/Acorn_A300_SMCLSup.pdf>`_, so before second half of '80s for sure.

Overall, surveying the landscape, even today - let alone back in the day - I would have come to the conclusion that SCSI is the way to, and most likely would have gone the same route as Apple did: the 5380 or one of it's many variants.

Networking
----------

In the '80s, especially in the beginning of it, networks mostly meant modems for home users, though LANs did exist for corporations and schools. These were mostly proprietary solutions and vendor-specific. Even the venerable PET from Commodore had some support for LANs.

Here I'm going to rely heavily on hind-sight and pick Ethernet. It was developed in the '70s and was `commercialized in 1980 <https://en.wikipedia.org/wiki/Ethernet>`_, so it certainly was a thing by the time our little machine was going to go in the market.

10Base5 (thick Ethernet) was introduced in '82, I can't find any reference to when 10Base2 (thin Ethernet) was introduced, but 10BaseT was commercialized in 1990. So I would venture to say '83 or '84.

So how did people interface to Ethernet back then?

.. note:: the NE2000
    *The NE2000* was by far the most popular card. It (and it's 8-bit version, the NE1000) was introduced in '87 and was based on the `DP8390 <pdf.datasheetcatalog.com/datasheets2/70/706490_1.pdf>`_. There were many many clones and variants. There is a `reference design <http://www.bitsavers.org/components/national/ethernet/DP849x_Demonstration_Kit_1987.pdf>`_ from National for this chip, very similar to the NE1000. This chip uses a local bus and a local SRAM to store the packet ring-buffer, and thus uses only a few I/O registers and no DMA to interface with the system. The chip itself supports bus-mastering (as I gather that's what it calls DMA), but it's pretty far from what we're doing: there's no daisy-chaining of BREQ, it has a multiplexed address-data bus and doesn't generate any nRAS/nCAS signals. So, it's probably easier to follow the reference design and add an external packet buffer SRAM then try to integrate it into system memory.

Nowadays, there are still some alternatives: `LAN91C113 <https://media.digikey.com/pdf/Data%20Sheets/Microchip%20PDFs/LAN91C113.pdf>`_, the `LAN9210 <https://ww1.microchip.com/downloads/en/DeviceDoc/9210.pdf>`_ and the `CS8900 <https://www.digchip.com/datasheets/download_datasheet.php?id=242902&part-number=CS8900A>`_

This is all later though it seems, fine for the second generation machine, but not for the first gen.

For the first generation machines, I would say I would have developed some sort of home-grown, serial (probably `RS-485 <https://en.wikipedia.org/wiki/RS-485>`_-based) protocol.

There is something interesting, called HDLC/SDLC. These are L2 protocols that are developed by IBM, worked probably through RS-232 or RS-485 or similar physical layers, but had controllers, such as the Motorola `mc6854 <https://heyrick.eu/econet/mc6854fixed.pdf>`_. This device could reach about 1Mbps, so not shabby for the time. It was used in the `Acorn Archimedes Econet poddle <http://chrisacorns.computinghistory.org.uk/docs/Acorn/Manuals/Acorn_A300_SMCLSup.pdf>`_, from which I got the idea.

Printers
--------

Centronics was the rage, I would have just rolled with it through some 3rd party GPIO chip, I think. Or, if I could scavenge together enough GPIOs for the handshake control, maybe just a 74LS373 for the data-bus. We would need 5 inputs and 4 outputs on top of the data-bus. That's annoying. So maybe a 2-port GPIO chip (such as whatever the C64 used) and a single extra GPIO down on the HID interface chip?

MIDI
----

Midi is just a serial port, but for some reason people liked to use something else then the 16450. I probably would have stuck with it, but it would have needed a custom crystal to get the baud-rate right.

RTC
---

The Archimedes A300 used the `PCF8573 <https://www.picmicrolab.com/wp-content/uploads/2014/05/PCF8573.pdf>`_, but *also* the `PCF8583 <https://www.nxp.com/docs/en/data-sheet/PCF8583.pdf?>`_. My guess is that one was introduced earlier then the other and they wanted to be able to populate either. The second one has some memory in it for configuration and stuff, so my guess is that they would have gone with just that had it been available on time. Either way, they are both I2C peripherals, which make interfacing them into just a pair of GPIOs, stolen from the HID device below. Or, if I'm so inclined, a full I2C controller in that very same chip.

Human interface
---------------

This was the era of the original MAC, the Amiga 1000 and of course the IBM PC. The common thing about these is that all had some sort of serial protocol for an external keyboard. Mice were not that common and the approach to them was mostly based on either hacked-up joystick ports (Amiga) or some custom interface very similar to it (Mac). PCs didn't have mice and once they did, they were serial based. Of course PS/2 computers introduced the same interface for both, albeit on two different ports. Later Macs introduced the APB connector and interface.

For joysticks, the common interface was similar to what the C64 used: some GPIOs and slow (R/C-based) ADCs.

I probably should have gone down a similar road: a serial interface, probably PC-style for the keyboard, an analog/digital joystick port for ... well, joysticks and mice. The main reason would have been that these were existing standards with many compatible products available and I really would not have enjoyed developing them all from scratch.

So, PS/2 port for Keyboard: `good info on the protocol <http://www.burtonsys.com/ps2_chapweske.htm>`_

The Amiga had a very `simple <http://pavouk.org/hw/en_amigamouse.html>`_ mouse, not much more than the optical encoders. While I don't have the schematic, but the Macintosh mouse was very similar judging from the `pinout <https://old.pinouts.ru/InputCables/MacMouse_pinout.shtml>`_. The Amiga had integrated encoder counters in Denise. The Mac apparently counted the cycles in SW as the pins go to a 6522. I don't quite know how the Atari ST did it, but the mouse itself was very much similar to the Amiga one.

I would have cobbled together something in a custom chip, or tagged it on to one of the existing ones. I would have needed:

6 pins per mouse/joystick (with two-button mice)
1 pin per analog controller (so 4 total for 2 analog joysticks)
2 pins for keyboard (CLK and DATA)

That's a total of... a lot of pins. I would not be able to tag it on to anything I already have. The sound chip can take 10 more pins, so that would be 1 mouse/joy port, including analog inputs and a keyboard interface. The DMA bridge has 8 pins available which could be the second joystick. But... yuck!

I would be really tempted to use a `Z8430 <https://www.zilog.com/docs/z80/ps0181.pdf>`_ CTC for the mouse counters and analog inputs. However it's only good for timing the analog joysticks, not for the mouse: it doesn't support up/down counters. There was the 74LS469 which was an 8-bit up/down counter, but I'm not sure when it was introduced. Certainly after '84. You could use pairs of SN74LS192 to get the same, that was available already in '81. And at that point you have 4 chips just to get the counters and then a few to latch and read the counter values.

GALs had something like 8 registers in them, so they are just a very expensive replacement for the 74LS469.

So, custom chip then!

Pinout
~~~~~~

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
9          A0          Register address bus
10         A1          Register address bus
11         A2          Register address bus
12         nCS         Active low chip-select for register accesses
13         nWE         Active low register write-enable input
14         nRST        Active low reset input
15         nINT        Open collector, active low interrupt output
16         SYS_CLK     System clock input
17         M1_X1       PORT A GPIO / Mouse/Joystick port 1, X direction encoder input 1
18         M1_X2       PORT A GPIO / Mouse/Joystick port 1, X direction encoder input 2
19         M1_Y1       PORT A GPIO / Mouse/Joystick port 1, Y direction encoder input 1
20         M1_Y2       PORT A GPIO / Mouse/Joystick port 1, Y direction encoder input 2
21         M1_BTN1     PORT A GPIO / Mouse/Joystick port 1, button 1 input
22         M1_BTN2     PORT A GPIO / Mouse/Joystick port 1, button 2 input
23         M1_TMR1     PORT A GPIO / Mouse/Joystick port 1, analog timer 1 input
24         M1_TMR2     PORT A GPIO / Mouse/Joystick port 1, analog timer 2 input
25         M2_X1       PORT B GPIO / Mouse/Joystick port 2, X direction encoder input 1
26         M2_X2       PORT B GPIO / Mouse/Joystick port 2, X direction encoder input 2
27         M2_Y1       PORT B GPIO / Mouse/Joystick port 2, Y direction encoder input 1
28         M2_Y2       PORT B GPIO / Mouse/Joystick port 2, Y direction encoder input 2
29         M2_BTN1     PORT B GPIO / Mouse/Joystick port 2, button 1 input
30         M2_BTN2     PORT B GPIO / Mouse/Joystick port 2, button 2 input
31         M2_TMR1     PORT B GPIO / Mouse/Joystick port 2, analog timer 1 input
32         M2_TMR2     PORT B GPIO / Mouse/Joystick port 2, analog timer 2 input
33         KBD_CLK     PS/2 keyboard port clock pin
34         KBD_DATA    PS/2 keyboard port data pin
35         GPIO_0      Gpio port 0; serial RX
36         GPIO_1      Gpio port 1; serial TX
37         GPIO_2      Gpio port 2; serial RST
38         GPIO_3      Gpio port 3; serial DST
39         VCC         Power input
40         GND         Ground input
========== =========== ===========

But in reality, this is not the way I'm going to go. This is where I'm going to draw the line and use USB.
