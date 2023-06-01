Technology Landscape
====================

Now that we have our :ref:`custom silicon <silicon_tech>` sorted, let's look at other limitations of the time!

Memories
~~~~~~~~

DRAM of course was widely available. In those days, they were NMOS devices with page mode, but not 'fast page mode' or FPM. That technology appeared `much later <https://en.bmstu.wiki/FPM_DRAM_(Fast_Page_Mode_DRAM)>`_ in 1990.

As far as capacity goes, there's a `greate source <http://doctord.dyndns.org/Courses/UNH/CS216/Ram-Timeline.pdf>`_ on the timeline:

======    ========
Year      Capacity
======    ========
1970      1kbit
1973      4kbit
1976      16kbit
1978      64kbit
1982      256kbit
1986      1Mbit
1988      4Mbit
1991      16Mbit
1994      64Mbit
1998      256Mbit
======    ========

If we were to ship in 1984, we would have access to 256kBit devices with speed grades between 100 and 150ns. These devices came in 1-bit and 4-bit configurations. Our 16-bit data-bus would mean that we would need either 4 such devices (leading to 128kByte) or 16 (reaching 512kByte). If we did multiple memory banks, intermediate sizes and total capacities of 1MByte would be reachable. In other words, our supported memory sizes will be: 128kbyte to 1MByte.

For ROMs, the timeline from `wikipedia <https://en.wikipedia.org/wiki/EPROM>`_ and `Intel <https://timeline.intel.com>`_ shapes up to something like this:

======    ========
Year      Device
======    ========
1975      2704
1975      2708
1977      2716
1979      2732
1981      2764
1982      27128
?         27256
?         27512
1986      27010
======    ========

Again, our planned date of introduction would allow us to use (probably) all the way up to 256kBit devices. These came in the form of 8-bit by 32kByte chunks. We need pairs of these for the 16-bit bus interface, so we could have had 64 or even 128kByte of ROM space. Having more would have been prohibitively expensive, but less is certainly possible by simply using older, smaller devices.

Glue logic
~~~~~~~~~~

That's easy, the 74xx series was the rage back then. We would have access to the 'LS' variant for general purpose components and the 'F' one where propagation delay and speed matters.

Displays
~~~~~~~~

CRTs ruled the day. TVs were the most common. These used (in America) 60Hz, refresh rates with 525 interlaced scan-lines. Of these about 480 was visible on the screen. On the other side of the pond, the PAL standard ran at 50Hz refresh and 625 scan-lines of which 576 was visible.

Computers at the time mostly used progressive-scan variants (that is, they simply generated the same image for both fields, many times not even bothering with the half-scan-line termination between them).

Depending on over-scan assumptions, these facts percolated into a supported vertical resolution of either 200 (NTSC) or 256 (PAL).

The corresponding horizontal resolution was often chosen as 320 pixels, which (using 8x8 pixel characters) translated into a 40-character wide display.

The horizontal sync-rate was 15.734kHz (NTSC) or 15.625kHz (PAL).

Typical of the time would have been the screen of the Commodore 64, which `used <https://codebase64.org/doku.php?id=base:pixel_aspect_ratio>`_ 6.13636MHz and 7.375MHz pixel clock rates on NTSC and PAL systems respectively.

When proper monitors were used, the same timing was preserved, but the horizontal resolution could be doubled due to the increase in available analog video bandwidth. Typically the vertical resolution was kept the same and interlace was almost never used due to flickering artifacts.

Sound
~~~~~

Most contemporaries had either single-bit beepers or very simple sound generators. The Commodore 64 stands out with it's SID chip: three voices, each with it's own tone and envelope generators.

On the more professional end of the spectrum, FM synthesis was coming of age within Yamaha. MIDI was introduced in 1981 and 1982 saw the first CD player on the market. High quality audio DACs were still a rarity and expensive. ADCs even more so. Delta-sigma modulation was known for a long time, but it was not widely used for audio to my knowledge. Even the `Amiga 1000 <https://erikarn.github.io/amiga/1000/Amiga_A1000_Schematics_2.pdf>`_ introduced a year after our target date used complex analog anti-aliasing filters, indicating that their DACs were rather simple.

Storage
~~~~~~~

Floppies ruled the land of the home in the '80s, though on the low-end of the market, casettes and cartridges were still used. Sony introduced the 3.5" floppy format in 1980, offering 720kB capacity initially, eventually reaching 1.44MB.

Hard drives were available but were very expensive and not widely used. SCSI was introduced in 1981 showing the times to come.

Human Interface
~~~~~~~~~~~~~~~

Human interface was standardized to mice, keyboards and joysticks at the time. Everybody seemed to have their on variant of the - essentially - same interface: a 9-pin DIN port for joysticks and mice, which themselves were not much more then switches and opto-couplers.

Keyboards were clearly a struggle: internal keyboards used wide ribbon or FPC cables to interface with a set of GPIOs, SW doing the scanning of the matrix. External keyboards either used the same interface (leading to many pins and thick, short interface cables) or some sort of home-grown, non-standard, incompatible serial protocol.

Standardization to what became the PS/2 or Apples ADB protocol was still in the future.

Communication and networking
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Networks were clearly a thing, but non-standard. The Apple II, the Commodore PET all supported some sort of networking. These were often serial-port based (maybe RS-422), home-grown interfaces with custom protocols sitting on top.

Modems were a thing, with rates up to, maybe 1200 baud, interfacing to RS-232 ports.

Ethernet, even 10Mbit Ethernet, existed but was not widely deployed or available. ArcNET was also around and became `more popular <https://en.wikipedia.org/wiki/ARCNET>`_ in the '80s. IBMs Token Ring was still in the future.

Expandability
~~~~~~~~~~~~~

Most computers of the time featured a single expansion port, maybe - in the case of the Commodore series - two incompatible ones. These were partly used to add capabilities to the machine, partly as cartridge ports for games. The most popular expansions were either applications (games, fast-loaders etc.) or memory expansions.

The stand-out is the Apple II with its internal expansion bus, and of course the IBM PC from 1983. Others, such as the TI 99/4 had external expansion boxes. Let's not forget of course of the pioneers of the micro-computer age, the Altair 8800 or the IMSAI 8080, which also used internal expansion buses.

It appears to me though that these internal buses were more of a necessity then a goal: early machines couldn't integrate all necessary features onto a single PCB, so a multi-PCB design - and a corresponding inter-PCB interface definition - was necessary.

PCB costs
=========

SMT: $0.13/in^2
PTH: $0.15/in^2

According to https://www.youtube.com/watch?v=nNpuiJitKwk

