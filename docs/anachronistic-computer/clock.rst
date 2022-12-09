Clock
=====

As we will see, we will want about 12.6MBps of transfer rate for 320x240 8bpp resolution. That would be 160ns between video accesses over a 16-bit bus.

Looking at :ref:`average access times<average_access_times>` table, it's obvious that that's almost all the bandwidth that the old NMOS RAMs could support, leaving almost nothing to the CPU [#video_speed].

We will support two clock speeds. We will pretend that early version of our dream machine is clocked at 12.5MHz. That's an 80ns cycle time, so a single DRAM access would take 160ns in the following timing:
::

     CLK __/^^\__/^^\__/^^\__/^^\__/^^\__/^^\
    nRAS ^^^^^\_____/^^^^^\_____/^^^^^^^^^^^^
    nCAS ^^^^^^^^\__/^^^^^^^^\__/^^^^^^^^^^^^
    nWE  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Data ----------<>----------<>------------

And a 4-beat burst would have this waveform:
::

     CLK __/^^\__/^^\__/^^\__/^^\__/^^\__/^^\
    nRAS ^^^^^\_______________________/^^^^^\
    nCAS ^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^
    nWE  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Data ----------<>----<>----<>----<>------

The other mode is double-speed, where the clock rate is 25MHz (cycle time is 40ns), but the timing is slightly different:

::

     CLK __/^^\__/^^\__/^^\__/^^\__/^^\__/^^\
    nRAS ^^^^^\________/^^\________/^^^^^^^^^
    nCAS ^^^^^^^^^^^\__/^^^^^^^^\__/^^^^^^^^^
    nWE  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Data -------------<>----------<>---------

::

     CLK __/^^\__/^^\__/^^\__/^^\__/^^\__/^^\
    nRAS ^^^^^\__________________________/^^\
    nCAS ^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^
    nWE  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Data -------------<>----<>----<>----<>---

The half-cycle delay is needed because FPM accesses make t_cas much shorted, but don't really change t_rcd all that much.

I suppose there should be a third, compatibility mode as well, at least for the CPU, that works with anything albeit slowly:

::

     CLK __/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\
    nRAS ^^^^^\___________/^^^^^\___________/^^^^^^
    nCAS ^^^^^^^^^^^\_____/^^^^^^^^^^^\_____/^^^^^^
    nWE  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Data ----------------<>----------------<>------

Once configuration is read and processed, the processor cah shift to either of the main timing modes as deemed possible. The DMA controller doesn't have to support the compatibility mode, though that means that DRAM refresh can't be enabled until DRAM configuration is known - which means that we would need to have a SW loop in the background refreshing the memory.

These clock rates support the highest speed-grade DRAMs of the various ages. If we wanted to support middle-of-the-road speed-grades, 10 and 20MHz clock-rates would respectively needed. For really slow parts 8 and 16MHz would be the appropriate selection.

