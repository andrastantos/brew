Introduction
============

As I'm getting old, I'm thinking more and more of the past. I grew up in the '70 and '80s. My childhood computer was a VIC20, followed by a C64. I was dreaming about owning an Amiga, but never had the money to have one. I admired the Mac and of course was fascinated by the Cray machines.

And I was constantly dreaming about and designing my own computers. Never actually built anything, but had lots of paper designs all the way from processors built from 74-series gates to Z80 based machines connected into massively parallel monsters.

As I said, I'm getting old and thinking about those days a lot again. What would my dream-machine in the early '80s look like? If I was an engineer at - say - Commodore with all the freedom in the world, what would I have designed the follow-on to the C64 to be?

This is the origin of this project. The idea is the following: taking the limitations as they existed in the early '80s, let's design a machine that is as kick-ass as I can make it to be. Of course, some technologies are just not available anymore: I can't make 2-micron NMOS process. But I can mimic that by building small-enough designs that would be implementable in those process nodes. I can make sure I won't run them at ridiculously high clock-rates or assume 200-pin BGA packages. I can make sure I'm using era-accurate memory size and speed assumptions. I can limit myself to peripherals that were accessible and popular in those days.

I will however take advantage of modern design tools. And I will take advantage of all the hind-side knowledge of the intervening decades.

So, let's dive in!

Boundary Conditions
===================

I'm going to assume that I have access to a chip fab and I can have as many custom chips as I wish. I will limit myself to packages of the era, mostly - ideally - below 40-pin DIPs. I will allow myself to break that and go all the way up to 64 pins (after all the MC68000 existed in that package at the time), but that must have been a rather expensive proposition, so I'm going to do my best to avoid it.

I'm going to assume that the minimum shipping memory configuration is 128kB, but I want headroom to go higher. Much higher in fact. I will assume FPM DRAM chips, but that's about it. No EDO, let alone SDRAMs.  

I'm going to want to have decent resolution graphics for the time (ideally VGA, that is 320x240, 8bpp and 640x480, 4bpp). I'm not going to assume and 3D graphics capability (that came much later). Sprites seemed to be a great idea at the time, so I'm going to have them. I will have as many colors as I can get, probably either 4bits/channel, but no true-color requirements.

For sound, I will make digitized, multi-channel sound playback a priority. I will think about FM synthesis, if it fits. Stereo sound is a must. Audio input capability is a nice-to-have. MIDI was starting to come on the scene, I would consider adding support to it (Atari STs had them)

I'm not going to think about adding a cartridge port, but a floppy drive is a must. Hard drives are also important: There were a lot of quazy standards, such as MFM drives, but really the thing that - though expensive - appeared to be the future was SCSI.

Human interface was standardized to mice, keyboards and joysticks at the time. I would consider adding some sort of serial bus for supporting them, following in the footsteps of Apple and IBM. I don't think ADB existed at the time (would have to check when Apple introduced it), but it's not unreasonable to realize that a unified interface to all of these peripherals might be a good idea. Would I have tried to come up with my own standard? Not sure.

For communications, I would think modems, thus serial ports. I don't know when Ethernet and Arcnet became a thing, but I do know that LANs were 'in the air'. The Macintosh implemented them, I think even early PETs had some sort of networking ability. So, that could be something to entertain, whether I would have had the foresight to pick a winner, that's debatable.

Expandability would actually be rather low on my list, but maybe that's bias on my side. Contemporary machines ran the gamut on that, the Apple II and IBM PC being highly expandable. The Amiga in it's first iteration was too, but the A500 was not. Many other machines, the C64, the Atari ST, the later 8-bitters, such as the Amstrad CPCs were not either.
