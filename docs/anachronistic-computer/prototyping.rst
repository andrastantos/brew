Prototyping on a Tank Nano 9k board
===================================

CPU takes up about 50% of the chip. What we would need on top of it:
- DRAM and ROM simulator (16kB each probably)
  - Since we live within the FPGA, DDR memory is difficult, so maybe only ROM/SRAM simulator (32kByte, unified)
  - This is where we should start, but that doesn't exercise the pipeline too well, so we should go to DRAM emulation
- Serial port (UART to some USB)

Clocks
======
- PLL and/or some other clock generation. I think the primary clock source is 27MHz...
- My plan for the real system was to use 28.63636MHz as the base clock.
    - /3 would result in 52ns access time and 9.5MHz clock rate
    - /4 would result in 69ns access time and 7.1MHz clock rate
    - We could use a PLL to get something closer to 8MHz (62.5ns) target
UART
====

Apparently the standard clock for UARTs is 7.3728MHz. That's not going to be easy to achieve in the final system, I think.
Most common baud-rates are here: https://lucidar.me/en/serialib/most-used-baud-rates-table/

Let's target 115200baud and 16x over-sampling. That would mean a bit-rate of 1.8432Mbps.
From 27MHz, that's a divider of 14.6484
From 8MHz is 4.3402
From 28.63636 it's 15.53

Either way, these values mean that I need a fractional divider. That's not the end of the world, but something we'll need to take care of.
Probably an 8-bit fractional portion, that adds an extra cycle when it overflow should be sufficient. Maybe even less...

Once we have a baud rate clock (really, clock-enable), we need a transmit shift-register (easy), which gets loaded from a shadow-register or a FIFO.

The receive is a bit more interesting. RX over-sampling is mainly used for precise start-bit detection. That is:
1. Start a counter on 1->0 transition
2. When count to 7, restart counter
3. Every time counter overflows, shift bit into RX register
4. Get parity bit, if needed
5. If pedantic, receive and check stop-bits. Otherwise, just wait one bit-time and go back to step 1