System considerations
=====================

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

The address bus buffers are bi-directional 74LS245 devices. They are controlled by the following signals:

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
    The DMA controller generates addresses directly on the DRAM bus, but the data transfer is happening though these buffers and the requestor. Thus, during DMA operations, the buffers operate in the reverse direction compared to CPU cycles. Another consequence of this setup is that DMA can only happen to/from DRAM.

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
