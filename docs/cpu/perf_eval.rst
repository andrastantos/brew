
Fetch performance evaluation
============================

==============    =================   ===============   ==============   =========
Queue Length      Fetch Threshold     Total cycles      Instructions     IPC
==============    =================   ===============   ==============   =========
32                16                  0x000015b8        0x000004de       0.224
32                8                   0x000014fd        0x000004de       0.232
16                8                   0x000014fd        0x000004de       0.232
16                4                   0x00001668        0x000004de       0.217
8                 4                   0x00001647        0x000004de       0.218
==============    =================   ===============   ==============   =========

The *main* reason for us not being faster is that the memory bus is busy. At least that's what it appears to be.
Now, that's not to say, it's doing useful work: we might be constantly fetching stuff that we'll discard.

That's right: adding an event for cases when the bus idles, it only occurs 0x10e times, so less then 5% of the cycles.

From the sweep above it appears that I hit smack in the bullseye: the 16/8 config is the most performant.

So, there you have it: we can't really improve IPC by more than ~5%: from 0.232 to 0.245

At an IPC of ~0.25 and 100% bus utilization it means we use up about 4 memory cycles (64 bits) per instruction. That's insane! Our longest instruction is 48 bits, and average should be around 24. Even with load/stores in the mix, this should not be.

The only explanation is that we fetch a bunch of stuff we don't use. Can we measure that? This took a while to implement (because I not only had to fish out the right events, but realize that drops are happening in bunches so events are now not just single wires).

I also reorged the code to be more reasonable and report results in decimal instead of hex. Surprisingly, this (while slow) didn't recover any new RTL bugs. At any rate, the results are:

event_clk_cycles:        5450
event_execute:           1255
event_fetch_wait_on_bus:  532
event_mem_wait_on_bus:    196
event_fetch:             3786
event_fetch_drop:        1223
event_load:                93
event_store:              239
event_bus_idle:           270

All right, so we used 3786-1223=2563 words for 1255 instructions, that's 2.04 words per instruction. That's rather bad: 32-bits and a bit per instruction on average. Now, I know that doesn't represent larger programs, it's just a property (apparently) of this little test loop I have going. And, since we've dropped about as many words as we had instructions executed, essentially we made this artificially high: 48 bits/instruction on average. We've also done 664 cycles of loads and stores (32-bits each) and idled the bus for 270 clock cycles. That's an extra 841 cycles.

So far we've accounted for: 4690 cycles out of the 5450. The rest can easily be bus inefficiency: we add 2 cycles for each burst. Just adding the ones for the load/stores, which are trivial to calculate gets us 5354 cycles.

So, I think we can claim success: we know where the cycles go.

Now, what to do about them?
