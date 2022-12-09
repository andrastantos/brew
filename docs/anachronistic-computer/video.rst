Video
=====

As we will see, we will want about 12.6MBps of transfer rate for 320x240 8bpp resolution. That would be 160ns between video accesses over a 16-bit bus.

Looking at :ref:`average access times<average_access_times>` table, it's obvious that that's almost all the bandwidth that the old NMOS RAMs could support, leaving almost nothing to the CPU [#video_speed]_.


We will have to compromise as well: our video support will have to be initially only 320x240 at 4bpp, and only later 'models', ones we can build with FPM DRAM can get up to 320x40 at 8bpp.

.. [#video_speed] The reason the Amiga could get away with its graphics was that it supported only 320x200 at 5bpp resolution and the frame-rate was only 30Hz, not 60 as in VGA.

.. wavedrom::

        { "signal": [
                { "name": "clk",  "wave": "P......" },
                { "name": "bus",  "wave": "x.==.=x", "data": ["head", "body", "tail", "data"] },
                { "name": "wire", "wave": "0.1..0." }
        ]}
