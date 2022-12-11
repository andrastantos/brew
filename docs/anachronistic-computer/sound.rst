Sound
=====

My original idea was to create 'mod' accelerator. For the uninitiated, 'mod', or tracker files were a popular file-format for audio production and distribution on personal computers in the early '90s. They were capable of producing some quite amazing sounds - for the time at least. So, I thought, something that can do that in HW would have been pretty sweet in the early '80s. Then I looked a little closer, and it turned out that that was exactly what the Amiga audio HW was capable of. In fact, the 'mod' format is not much more than a serialization format for programming the Amiga audio chip.

The format is documented here: https://web.archive.org/web/20100921225940/http://io.debian.net/~tar/debian/xmp/xmp-2.7.1/docs/formats/Ultimate_Soundtracker-format.txt
or here: https://www.ocf.berkeley.edu/~eek/index.html/tiny_examples/ptmod/ap12.html

We would need to be able to play back 4 simultaneous audio samples, each having 8-bit resolution. They had arbitrary sampling rate - which would involve re-sampling or at least linear interpolation. The settings were such that the sampling rate could increased, but not decreased, so there was no reason to support sample-skipping.

Samples had an 'intro' section, followed by a repeat loop. This is something that our DMA engine mark-restore mechanism supports very nicely. The fact, that we always do interpolation means that we don't ever skip samples, though we might repeat them.

Repeating samples - or more advanced interpolation/resampling - can be supported within the sound chip, so all we need to do is to ask for consecutive samples, whenever our internal buffer runs low.

If our internal sample rate is 44100Hz, and we support 4 channels, we would need a new 16-bit data every 11.33us. If our sound-chip supported only an 8-bit bus, it would increase to 5.66us. Even if we supported 16 channels, we would only need a new 8-bit data less then every 1.5us.

So, really the number of channels we can support is a matter of how much state we can/willing to store in the audio chip.

We could easily support 8 16-bit channels as well, depending on what the internal DSP is capable of.

Stereo mixing is also a question of DSP capabilities: we need two extra multiplies per cycle for that.

Same goes for effect processing, though some simple algorithms surely could be supported.

Pinout
------

For the first-generation machine, let's try to save pins and implement only an 8-bit peripheral:


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
13         nDRQ_IN     Active low DMA request daisy-chain input
14         nDRQ_OUT    Active low DMA request daisy-chain output
15         nDACK       Active low DMA acknowledge input
16         nDCHRD_TC   Active low DMA channel read qualifier and terminal count input
17         nWAIT       Open collector, active low wait-state output
18         nWE         Active low register write-enable input
19         nRST        Active low reset input
20         nINT        Open collector, active low interrupt output
21         SYS_CLK     System clock input
22         AUDIO_CLK   Audio clock input
23         L_OUT       Audio output left channel
24         R_OUT       Audio output right channel
25         L_IN        Audio input left channel
26         R_IN        Audio input right channel
27         DVCC        Digital power input
28         DGND        Digital ground input
29         AVCC        Analog power input
30         AGND        Analog ground input
31
32
========== =========== ===========

.. note:: 
    For an FPGA implementation, we would not need analog power pins, but would need to interface to an external (I2S for instance) CODEC, resulting in roughly the same number of pins.

.. note:: 
    The Amiga HW had some rather crazy audio filtering going on on its outputs. Judging from the fact that they couldn't implement a video DAC, I'm assuming they don't have an audio DAC either. They probably simply do a PWM output (if lucky, some sort of sigma-delta output) and external filtering to remove the nasty aliases. We could have also done something similar, removing the need for the analog power supplies.