
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


A new theory
~~~~~~~~~~~~

It takes 2 clock cycles to get from FETCH response to DECODE IN. It takes another 3 to get from request to reply. So we have a 5-cycle latency in the front-end of the pipeline. That's too high and results in a lot of fetches. So, how about some quick-and-dirty branch recognition in InstBuffer and break the burst right there?

I've coded up something trivial (only catches conditional branches). Let's see!

==============    =================   ===============   ==============   ==========================================
Queue Length      Fetch Threshold     Total cycles      Instructions     Note
==============    =================   ===============   ==============   ==========================================
11                8                   5398              1255             No break bursts
11                8                   5114              1255             Breaking bursts on ld/st and branches
11                8                   5404              1255             Breaking only on ld/st
11                8                   5091              1255             Breaking only on branches
11                8                   5579              1255             Breaking only on cbranches early
==============    =================   ===============   ==============   ==========================================

OK, well, that didn't work out: it's even worse! Of course, looking at it, almost every other instruction is a branch in that code! Maybe it's a particularly bad example? It shows a tight loop with three (normally not-taken) branches plus of course the looping branch.

What's interesting is that we claim we've dropped more fetches here then when we broke on branches only. We also spend the same number of cycles in bus idle, so really, the difference is a combination of more fetch over-head (shorter average burst length) and more dropped fetches, if I can believe that count. Not sure I do though: this result is very counter-intuitive.

Another theory
~~~~~~~~~~~~~~

Actually, I think that dropped fetches (and in general high bus-utilization) is a red herring: since there's no other contender for the bus, it's harmless to fetch a bunch of words that we don't use (BTW the metric on some if these counters is not the same: some are measured in instructions, while others are in words).

There's also an issue with one of the counters, that needs to be tracked down, but I digress.

The point is that the reason the CPU is slow is not that it's waiting on the bus. Or at least high bus utilization is not a clear sign of that. What would be a clear sign is if the fetch would be constantly waiting for the bus interface. But it doesn't - that's the reason it can run so much ahead and fetch a bunch of useless stuff.

So, then: why isn't the processor better at executing instructions?

Wow, a bunch of 'execute class' counters were wrong: they didn't take ready/valid into account neither did they look at do_jump which would indicate an accepted instruction to execute that would get dropped.

So, with all that fixed, I can start staring at the traces. The question is this: when we do NOT execute an instruction, why is that? Because we haven't anything decoded or because we're not ready to execute?

Looking at the traces, it obvious that we mostly are ready, but not valid, that is: we're waiting on something to get decoded.

Looking at the same thing at the input of decode, we see the same pattern: we're almost always ready to decode, except there's nothing available.

Overlaying it with do_branch, it's pretty obvious what's going on: it takes way too long to fill the fetch pipeline after a branch: the long stretches of unavailability from fetch almost always follow a branch.

So, what does it tell us? It says that branch mis-predicts are the main culprit, coupled with high latency of fetch: It takes 6 (**6!!**) clock-cycles from do_branch to get a new instruction to decode.

So, where those 6 clock cycles go?
1. We have to start a new burst, which means we have to terminate the old one. That's 1 clock cycle.
2. We spend another clock cycle in waiting. But WHY??? <========= THIS COULD BE SAVED
3. We spend a clock cycle reading data from DRAM (i.e. outputting the nCAS pulses)
4. We spend another half clock cycle to gather the high-byte, so we can only output our first result here
5. We spend a cycle in the instruction queue. THIS COULD POTENTIALLY BE SAVED, but it's rather difficult as we would need to bypass the FIFO when it's empty and assembly is ready to consume
6. We spend (at least) one cycle in instruction assembly. THIS COULD POTENTIALLY BE SAVED. It can be merged with decode: we get the instruction code in the first clock-cycle, so - as long as we don't support extension groups - we can start decoding, looking at reservations, etc. Then, we wait for FIELD_E to populate if we need it.

So, theoretically we could cut this time in half, but saving cycle 5 and 6 is non-trivial. So let's start looking at cycle 2, that should be low-hanging fruit.

So, the reason we spend an extra cycle in idle has nothing to do with bus_if, it's because fetch (inst_buf) goes through some flush cycle. Oh, and we do *that* because we have to drain our outstanding requests, before we can start a new burst. Otherwise, we won't know when the new responses would start showing up and when to stop dropping. While that's not directly true, this is how it is working now, so changing even *that* is non-trivial.

What should happen is to capture the outstanding requests count upon do_branch and use that as a drop-counter. That way we can start the new burst immediately.

Well, I managed to get clock cycle '2' out of the system using the drop-count idea. Results:

Before:
    event_clk_cycles: 5091
    event_execute: 1255
    event_branch: 502
    event_branch_taken: 270
    event_fetch_drop: 843
    event_load: 31
    event_store: 42
    event_bus_idle: 302

After:
    event_clk_cycles: 5031
    event_execute: 1255
    event_branch: 502
    event_branch_taken: 270
    event_fetch_drop: 843
    event_load: 31
    event_store: 42
    event_bus_idle: 242

Not all that impressive, is it? Still, it's a step in the right direction.

There also seem to be cases where we're idling in the instruction queue, yet we take our sweet time to start a new burst upon getting a branch request. Turns out the state-machine had some vestigial transitions and 'start_new_request' was rather conservative as well. These both can be simplified now with the drop-count (wow, faster and simpler!). Results:

    event_clk_cycles: 4900
    event_execute: 1255
    event_branch: 502
    event_branch_taken: 270
    event_fetch_drop: 884
    event_load: 31
    event_store: 42
    event_bus_idle: 105

This is a decent win, so let's packet it!
