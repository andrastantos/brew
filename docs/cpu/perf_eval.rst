
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
event_branch_taken:       270
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

BTW: with 270 branches taken, we get an average run-length of 3.6 instructions of linear execution, again, lower then expectations.

Now, what to do about them?

One idea is to make the prefetch queue only a few items longer then the burst size: as much longer as the memory latency is, that is 2-3 cycles or something. This way, we will only start a burst when the queue is almost empty, that is, when we have a high likelihood of actually using the results.

That actually helped a little, but just a little:

==============    =================   ===============   ==============   =========
Queue Length      Fetch Threshold     Total cycles      Instructions     IPC
==============    =================   ===============   ==============   =========
19                16                  5548              1255
16                8                   5450              1255
11                8                   5398              1255
11                4                   5741              1255
7                 4                   5709              1255
==============    =================   ===============   ==============   =========

So, again it appears that we can get very modest (0.1%) gain by this change and fetching less then 8 words, the bus efficiency starts to kill us. Larger bursts also do too much.

OK, so we didn't get much smarter: we still don't know how to *not* fetch a bunch of stuff we don't need.

Of course another idea would be to terminate a burst as soon as we realize that we *might* jump. But for that, we would need a burst-termination signal that will be a bit more involved to add.

==============    =================   ===============   ==============   ==========================================
Queue Length      Fetch Threshold     Total cycles      Instructions     Note
==============    =================   ===============   ==============   ==========================================
11                8                   5398              1255             No break bursts
11                8                   5114              1255             Breaking bursts on ld/st and branches
11                8                   5404              1255             Breaking only on ld/st
11                8                   5091              1255             Breaking only on branches
==============    =================   ===============   ==============   ==========================================

This is interesting: we're better off not speculating through branches (too much). But we are better off bursting through load-stores, though the gain there is not all that impressive and could be an artifact of the benchmark I'm using.


