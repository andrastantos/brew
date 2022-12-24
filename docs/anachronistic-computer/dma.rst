DMA controller
==============

There are a lot of things that want bus-master access. Mostly graphics and audio, but HDD and FDD controllers too. Not only that, but both graphics and audio wants several independent data-streams. Finally, we don't really have the pins to implement bus-mastering capability in the graphics or indeed in the audio controller. It appears it's better to centralize this capability in a multi-channel DMA controller.

DMA descriptor layout
~~~~~~~~~~~~~~~~~~~~~

The DMA controller has 2D DMA capability (at least on some channels). The configuration of the DMA is the following:
  1. Base-address (32 bits)
  2. Line length (12 bits)
  3. Line offset (12 bits)
  4. Transfer size (32 bits)
  5. Wrap size (5 bits)
  6. Burst size (2 bits)
  7. Active (1 bit)
  8. Mode (2 bits) (Read/Write/Chained/RSV)
  9. Word size (1 bit)
  10. Interrupt on termination (1 bit)
  11. Circular (1 bit)
  12. Signal TC (1 bit)

The state of the DMA is captured in the following registers:
  1. Current address (32 bits)
  2. Mark address (32 bits)

Chaining allows adjacent channels to act as a pair of memory-to-memory DMAs.

.. todo::
    do we want blitter functionality? If so, we would need read-modify-write cycles.


.. todo::
    Due to timing issues on the system-bus, I don't think it's possible to support burst DMA writes. Burst chain operation is already problematic due to the internal register usage

Pinout
~~~~~~

The DMA chip has the following pinout:

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
11         D0          Data bus
12         D1          Data bus
13         D2          Data bus
14         D3          Data bus
15         D4          Data bus
16         D5          Data bus
17         D6          Data bus
18         D7          Data bus
19         nRAS_B0     Active low row-select, bank 0
20         nRAS_B1     Active low row-select, bank 1
21         nLCAS       Active low column select, lower byte
22         nUCAS       Active low column select, upper byte
23         nWE         Active low write-enable
24         CLK         Clock input
25         nRST        Active low reset input
26         nINT        Active low interrupt output
27         nBREQ_IN    Active low bus-request daisy-chain input
28         nBREQ_OUT   Active low bus-request daisy-chain output
29         nBGRANT     Active low bus-grant input
30         nWAIT       Active low wait-state input
31         nDRQ        Active low DMA request input
32         nDACK       Active low, open collector DMA grant output
33         nDCHRD_TC   Active low, open collector DMA channel read cycle qualifier and active low terminal count output
34         nREG_CS     Active low chip-select for register accesses
35         VCC         Power input
36         GND         Ground input
37
38
39
40
========== =========== ===========

Operation
~~~~~~~~~

DMA request
-----------

Each DMA client is daisy-chained through their `nDRQ_IN` and `nDRQ_OUT` lines. The first in this chain is driving the `nDRQ` line of the DMA controller:

::

    nDRQ_OUT <<= nDRQ_IN & ~(<any internal DMA request>)
    ARB_WON  <<= latch(nDRQ_IN, nDCHRD_TC & nDACK)

.. note::
    `nDACK` needs to be part of the determination of the `ARB_WON` state since `nDCHRD_TC` doubles as terminal-count signal during `nDACK` cycles

This establishes a hard-wired priority scheme between them. The furthest device from the controller has the highest priority.

.. todo::
    I hope we don't care about latency through the request chain: it's going to be slow, and even slower for the higher priority clients

Bus request
-----------

When the DMA controller notices `nDRQ` asserted, it requests access to the bus by asserting `nBREQ_OUT`. The CPU (eventually) answers by asserting `nBGRANT` and relinquishes driving of the bus.

.. note::

    For multiple bus-masters, a similar daisy-chained arbitration is used as described for DMA clients, except through `nBREQ_IN` and `nBREQ_OUT`.

Channel read cycle
------------------

The DMA controller at this point asserts `nDCHRD_TC` (leaving `nDACK` high). This signal is received by all DMA clients. Exactly one client will see its internal `ARB_WON` signal getting asserted, which will be the one selected as the DMA target. This client puts its channel ID on D0...D5 and the command-code on D6...D7. If multiple DMA sources present in a single client, it is the clients responsibility to select the highest priority internal source. The channel ID in general is programmable in the client and assumed to be unique across the system. The `nWAIT` signal can be asserted by the client to extend the channel read cycle.

.. note::
    The data transfer happens on the buffered data-bus on the system level. This means of course that the data-bus of the DMA controller needs to be connected to `BD0..BD15`. The buffers connecting `DB0..DB15` to `D0..D15` of the CPU and the DRAMs are disabled during the `nDCHRD` cycle

Multiple DMA controllers
------------------------

If the channel ID read on the data-bus during the channel read cycle is one of the channel IDs known to the DMA controller, it will continue processing the DMA request. If not, it will simply relinquish the bus by de-asserting `nBREQ_OUT`. This allows multiple DMA controllers to work in parallel on the same bus: All controllers are connected 'in parallel', but they have their own unique set of channels they implement. When `nDRQ` is asserted, all controllers request control of the bus (assert `nBREQ_OUT`, monitor `nBGRANT`) and issue the channel read cycle. Based on the returned channel code, all but one of the DMA controllers determine that they are not the one to generate the bus-cycle, so they release `nBREQ_OUT`. The controller that got selected (i.e. the one with the requested channel) completes the transaction and releases `nBREQ_OUT` whenever it's ready.

The DMA channel IDs handled by a controller are numbered consecutively, starting from a pre-programmed value. For instance, if there are 16 DMA channels in a controller, it will have a 2-bit start channel ID register, which sets the top 4 channel bits.

There can be a total of 64 DMA channels in a single system.

DMA command codes
-----------------

There are four command codes that the client returns to the DMA controller along with its channel ID during the channel read cycle. These are presented on D6 and D7 and are as follows:

=========   =======   ============
CMD codes   Action    Explanation
=========   =======   ============
0           advance   The DMA serves the next address per it's current state
1           mark      Same as 'advance', but the (pre-update) current-address register is written to the mark register
2           restore   The current-address register is restored from mark-register and used to serve the DMA request
3           reset     The current-address register is reset to DMA base-address and used to serve the DMA request
=========   =======   ============

Serving the request
-------------------

Using the command and the channel ID, the DMA controller can load the appropriate configuration and context, update the context and start serving the request.

If the requested channel is disabled, the DMA controller doesn't generate any transfers and immediately relinquishes control of the bus by de-asserting `nBREQ`.

.. todo::
    If the requestor doesn't release `nDRQ_OUT` during the channel read cycle, and its corresponding DMA channel is disabled, we're going to hog the bus quite a bit and slow the whole system down. Should we at least generate an interrupt, as if it was the terminal count?

Each request is served by a number of transfers, programmed in the burst-size register. In most cases, bursts will be within a single DRAM page, but that's not necessarily the case. Additional page-select (using `nRAS_Bx`)cycles are generated by the DMA controller as the current address pointer crosses page boundaries. Since the smallest supported device is the 4164 part, pages are assumed to be fixed, 256 words long (even for larger devices). At the beginning of a DMA burst, a `nRAS_Bx` cycle is always inserted: we don't know what the selected page is, not to mention that we inherit control of the bus with `nRAS_Bx` high.

During the bus-cycle, the DMA controller assert `nDACK` to signal the data-transfer to the client. The generation of this signal is tricky though, due to the latency between data on the `BDxx` lines and those on the DRAM bus.

The DMA controller monitors the `nWAIT` line and allows the extension of the `nxCAS` cycles as needed. During wait-states `nDACK` remains asserted.

The DMA controller keeps `nWE` static for the whole cycle: it is asserted along with `nDACK` for write cycles.

The `nDCHRD_TC` output is assert (low) upon the last beat of the last transfer for a programmed DMA, if enabled.

Clients distinguish this situation from the channel read cycle by the fact that for a TC cycle `nDACK` is also asserted.

DMA writes
..........

For DMA writes, `nDACK` is asserted with `nRAS_Bx`. This is fine since write bursts are not supported, but allows for data from the client to wind its way through the buffers to the DRAM by the time of the rising edge of `nxCAS`.

.. note::
    The missing support for write bursts is not all that problematic: I think the only client who really wants bursts is the video controller, which only does reads.

Clients use `nBDACK`, `nBWE` and their internal `ARB_WON` state to determine if they need to drive data on the data-bus.

DMA reads
.........

For DMA reads, `nDACK` is asserted with a delayed version of `nxCAS`.

To match up `nDACK` timing with the data buffers, `nDACK` also needs to be buffered and a buffered version is to be served to clients.

::

    nBDACK <<= nDACK

The clients use `nBDACK`, `nBWE` and their internal `ARB_WON` state to determine if they need to latch the content of the data-bus.

Word size considerations
........................

The DMA transfer word-size and the client word-size must match; Only naturally aligned transfers are supported. This leaves us with only two configurations to consider:

16-bit client - 16-bit, word aligned transfers:
    `nDACK` signals a 16-bit transfer and `nWE` determines the direction.
8-bit client - 8-bit transfers:
    `nLCAS` is asserted for even and `nUCAS` for odd addresses. `nDACK` is asserted with either. The client uses the lower 8 data-bus bits D0..D7 for the transfer. The system level data-buffers take care of routing the odd addressed data to the right place.

Termination of the burst
------------------------

Once the requisite number of beats of the burst transfer are completed, `nDACK` is de-asserted along with `nBRQ_OUT`. The CPU gains back control of the bus.

The DMA controller doesn't handle back-to-back bursts: even if `nDRQ` remains asserted by the end of the burstm `nBRQ_OUT` is de-asserted for one cycle. This to prevent other bus-masters from starvation.

`nDRQ` Handshake
----------------

The client can de-assert `nDRQ_OUT` once `nDCHRD_TC` is asserted. The DMA controller re-examines `nDRQ` in the cycle following the de-assertion of `nDACK`.

Burst-size mismatches
---------------------

It is the responsibility of the programmer to make sure that the DMA burst-size and the client request-logic is properly matched, that is: a client will only request a transaction if it can handle at least the programmed burst-size number of contiguous transactions. If no such guarantee is present, a burst-size of 1 should be used.

Interrupts
----------

If enabled, an interrupt is generated upon the last beat of the last transfer cycle of a DMA transfer. For circular DMAs, the DMA engine is re-initialized to a new transfer. For non-circular DMAs, the channel is disabled.

The DMA controller also raises an interrupt if a transfer request occurs on a disabled channel.

There is a 16-bit interrupt status register containing '1' for each channel that has a pending interrupt. This register is 'write-1-to-clear'.

TODO
----
This is a rather slow way of generating DMA cycles: there's a request cycle, a channel ID read cycle, an address cycle and then the data cycle. It helps a little that we have burst support though, but even then, a 4-beat burst takes 7 cycles. And that assumes that we can toggle the requisite lines within a single clock cycle, for example by driving them on both edges.

We **need** to check how timing works out for the video controller.

Chaining
--------

Chaining two DMA channels allow for memory-to-memory transfers. Combined with the 2D capability of the DMA channels, this allows for seamless blitter operations.

I suspect that memcpy acceleration would be cumbersome and not used, that is more or less the only reason for having chaining support.

Because of that (and that our highest resolution is 8bpp), it makes sense to only support 8-bit transactions for chaining. Of course at this point it's rather slow: not only it's 8-bit only, but no burst is supported either. Maybe the CPU would be faster doing this in the end?

Chained DMAs combine a pair of adjacent DMA channels. The even numbered DMA is the driving, read DMA. The odd numbered DMA is the slave, write DMA.

The transferred data is captured in an internal 8-bit register.

Chained DMAs are auto-triggered in that the driving DMA is requested by the completion of the slave DMA and vice versa.

.. todo::
    Do we want to add some pacing? If so, how?

DMA bridge
==========

There are quite a few peripherals that support intel-style DMA transfers. FDD and HDD controllers are the prime examples. Since those were important devices at the time, we need a way to work with them. Comparing our DMA controller to the Intel i8237, we see one key difference: they support single-cycle vs. block vs. demand DMAs (https://docs.freebsd.org/doc/2.1.7-RELEASE/usr/share/doc/handbook/handbook248.html). Demand mode in particular seems to transfer many bytes so long as DRQ is asserted.

We can't really demand mode, because of this: as we complete a DACK cycle, the original requestor may or may not released the bus. So, we have to go back and re-query the requestor channel by asserting /DCHRD for a cycle.

Block transfers are not particular useful (and probably not used all that often) as they hold the bus up for very long time. So we really can only do single-cycle transfers and emulate demand transfers by keep requesting more cycles. Our bust-mode is not really compatible with Intel DMA, so that can't be used either.

We can create a bridge chip that handles these conversions. It would have the following pinout:


========== =========== ===========
Pin Number Pin Name    Description
========== =========== ===========
1          A0          Register select bus
2          A1          Register select bus
3          A2          Register select bus
4          D0          Data bus
5          D1          Data bus
6          D2          Data bus
7          D3          Data bus
8          D4          Data bus
9          D5          Data bus
10         D6          Data bus
11         D7          Data bus
12         DSRQ0       Active high DMA request input 0
13         nDSACK0     Active low DMA acknowledge output 0
14         DSRQ1       Active high DMA request input 1
15         nDSACK1     Active low DMA acknowledge output 1
16         DSRQ2       Active high DMA request input 2
17         nDSACK2     Active low DMA acknowledge output 2
18         DSRQ3       Active high DMA request input 3
19         nDSACK3     Active low DMA acknowledge output 3
20         TC          Active high terminal count output
21         nDRQ_IN     Active low DMA-request daisy-chain input
22         nDRQ_OUT    Active low DMA-request daisy-chain output
23         nDACK       Active low DMA acknowledge input
24         nDCHRD_TC   Active low, DMA channel read cycle qualifier and active low 26terminal count input
25         nCS         Active low chip-select for register accesses
27         nWE         Active low write-enable
28         CLK         Clock input
29         nRST        Active low reset input
30         nINT        Active low interrupt output
31         VCC         Power input
32         GND         Ground input
========== =========== ===========

A DMA client uses DSRQx and nDSACKx to hand-shake a DMA request. TC conveys Intel-style terminal-count information. The client can request bus-cycle extension by asserting nWAIT. This line is monitored by the DMA controller and not by the DMA bridge.
