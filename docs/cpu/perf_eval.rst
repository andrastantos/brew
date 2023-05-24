
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

Something else!
~~~~~~~~~~~~~~~

At this point, we start seeing something else:

..image:: image/cpu_inst_assembly_and_other_stalls.png

There are no branches or even load/stores in the selected region, yet fetch_valid is rather unhappy. The clue as to why that is, is in the `fetch_inst_len` signal: most of these instructions are 2 or 3 words long, so it takes 2 or 3 cycles to assemble them. This is because we can pull only 16 bits from the instruction queue every clock-cycle. So, this is expected, and can be calked down as code inefficiency: I know the average instruction size is around 24 bits, so the section we're looking at is just atypical.

However, let's look at how choppy `fetch_ready` is! Compare that to `input_port_ready` on the EXEC_INT section. The latter shows if execute has a stall. The former is if decode has one. Look how little execute applies back-pressure compared to what decode is doing!

What must be going on here is that decode finds a bunch of read-after-write or write-after-write hazards and stalls awaiting those to clear.

Let's see if we have the right event counters to capture these problems.

So, we do, but it doesn't line up with the wait-states decode pushes into the stream. Why then, does it apply back-pressure? Oh, actually the counter is right: just because we're waiting on the RF, that doesn't mean we're actually stalling. For that to happen, there should be something better for us to do. Which in many cases, there isn't.

So, those notches in fetch_valid are valid, legitimate and unless I can reduce the latency of the RF (which is already timing-critical), there isn't much I can do.

So, at this point we're back to the previous problem, with an extra wrinkle: can we somehow decode 32 bits at a time and if we did, what would it buy us?

What I think I should add a counter for is all the times we see 2- or 3-work instructions, or in other words, the average instruction length.

Hmm... After adding this counter, turns out my average instruction length *is* 1.5 words:

    event_clk_cycles: 4900
    event_execute: 1255
    event_branch: 502
    event_inst_word: 1992
    event_fetch: 3481
    event_fetch_drop: 884
    event_load_or_store: 73
    event_bus_idle: 105

So then what gives? At any rate: I should spend *at least* 1992 cycles on this program. Each branch takes an extra (now) 5 clock cycles, so that's 2500 cycles, which gets me pretty close: 4502 clock cycles. The 73 loads and stores would add another few hundred for sure, and we're almost there.

So really, the only thing we can do anything about is still the branch mis-predict penalty.

Removing the queue latency
~~~~~~~~~~~~~~~~~~~~~~~~~~

I've added a new FIFO variant (ZeroDelayFifo) which has a combinatorial bypass circuit. This, in case of an empty FIFO passes on fetched data from the instruction buffer to instruction assembly without any latency.

Now, this can cause timing issues, but let's worry about that later. Going back and forth is a one-line change anyway, so it's easy to mock around with.

It took me a while to get it right, not because of the regular logic, but because I forgot to pass on the 'clear' signal to the underlying FIFO. Such a dumb mistake!

At any rate, drum-roll please!

Before:
    event_clk_cycles: 4900
    event_execute: 1255
    event_branch: 502
    event_inst_word: 1992
    event_fetch: 3481
    event_fetch_drop: 884
    event_load_or_store: 73
    event_bus_idle: 105

After:
    event_clk_cycles: 4902
    event_execute: 1255
    event_branch: 502
    event_inst_word: 1992
    event_fetch: 3372
    event_fetch_drop: 682
    event_load_or_store: 73
    event_bus_idle: 314

Even worse??!!! How could that be? One thing that got better is the number of dropped words, but I don't understand! Let's count the cycles from branch to restart of the pipeline!

Before the change it was 6 cycles (and I actually miscounted above, so our starting point is 7, I think).
After the change it is 5 cycles. So, we do get the reduction we hoped for, but not the improvement in speed.

So, where do the cycles go now? Is it possible that we drain the fetch queue now too often and end up waiting for it? What we seemed to have gained was a bunch of bus idle cycles. That would corroborate this theory...

Old:
    event_mem_wait_on_bus: 279
    event_fetch_wait_on_bus: 80

New:
    event_mem_wait_on_bus: 340
    event_fetch_wait_on_bus: 372

That's it! We are now waiting more for the bus to respond! Where do those waits occur? They happen almost every time (once!) when we take a branch. Indeed, it appears we're wasting a cycle in BusIf: we spend 2 cycles in IDLE.

After realizing that we continue request during a 'do_branch' cycle from fetch towards the bus (and fixing it), we do see some minor improvements:

    event_clk_cycles: 4861
    event_execute: 1255
    event_branch: 502
    event_mem_wait_on_bus: 340
    event_fetch: 3331
    event_fetch_drop: 641
    event_fetch_wait_on_bus: 132
    event_bus_idle: 336

Crucially, we don't wait on bus for fetch nearly as much.

Right now we're taking two cycles to restart requesting from the bus: one cycle *during* and one cycle *after* the branch. Can we make it faster?

OK, there were quite a few dumb decisions in InstBuf that added extra cycles of delay. Things, like going back and forth between idle and request multiple times, which added delay to starting a new request that stuck. After fixing them:

    event_clk_cycles: 4672
    event_execute: 1255
    event_branch: 502
    event_mem_wait_on_bus: 340
    event_fetch: 3331
    event_fetch_drop: 641
    event_fetch_wait_on_bus: 162
    event_bus_idle: 147

This is a decent improvement. We're still waiting on the bus quite a bit more then before, but at least the top-of-the-line number is better. The remaining events correlate with back-to-back not-taken branches. Those are cases when we have a quick succession of burst breaks, which results in fetch going back to idle. While the situation can be improved some by disabling burst-breaking, I think overall that should improve performance, even if this particular benchmark doesn't show it:

Burst breaking removed:
    event_clk_cycles: 4643
    event_execute: 1255
    event_branch: 502
    event_mem_wait_on_bus: 352
    event_fetch: 3324
    event_fetch_drop: 753
    event_fetch_wait_on_bus: 300
    event_bus_idle: 147

The IPC is now 0.268, which is still rather appalling, but improving, I guess.

Next, I guess is removing FIELD_E from the critical path...

FIELD_E speedup
===============

God, this is way more involved then I thought

Right now the issue seems to be that we pick up FIELD_E without it's high word at or around 23235ns. This is a relative jump, but when the jump actually happens, we seem to be using a 16-bit signed offset instead of a 32-bit one. This of course vectors us to the void.

The problem seems to be that the decoder and execute get unsynced. This happens at 22336, I think, where we READY the same instruction twice. There's also the curiosity that VALID disappears for a cycle, but I don't think that's where the problem originates.

Oh, actually it's the other way around: Why does fetch offer the same instruction code twice???

OK, I'm getting tired. The problem now seems to be that $r2 gets corrupted somewhere in memset. So, when this gets executed:

    800006ba:       a2 3e           mem32[$r2] <- $r3

We start writing to some rather strange location. This might not be the root cause of the hang, but a register corruption is nevertheless bad.

That's because $r0 is corrupted.

Actually, there's something majorly wrong with the reservation logic (or obeying it rather in Decode most likely). In this instruction stream:

    800006b6:       10 60           $r6 <- tiny 0
    800006b8:       60 24           $r2 <- $r0 + $r6

Decoded at 24400, we're happily blasting through, accepting the second instruction, even though it's clearly dependent on the previous one. Predictably, we pick up the wrong value to hand over to Execute.

Actually no, we don't. We wait until we have the response for $r6. We actually only issue the second instruction to execute 400ns later.

An in fact, we even write back the write value (0) into $r2, unless of course $r0 is busted. $r0 is our destination pointer:

    8000065a <memset>:  <========= this is 4000032d in $spc terms.
    8000065a:	ff cc       	mem32[$sp - tiny 4 (0xfffffffc)] <- $fp
    8000065c:	fd 8c       	mem32[$sp - tiny 8 (0xfffffff8)] <- $r8
    8000065e:	dd c2       	$fp <- $sp
    80000660:	fd d4 f8 ff 	$sp <- short -8 (0xfffffff8) + $sp
    80000664:	44 22       	$r2 <- $r4
    80000666:	f4 03 03 00 	$r0 <- short 3 (0x3) & $r4
    8000066a:	00 f0 b4 00 	if $r0 == 0 $pc <- $pc + 180 (0xb4) <---- alignment-check destination pointer
    8000066e:	6e 1b       	$r1 <- tiny $r6 - 1
    80000670:	06 f0 9a 00 	if $r6 == 0 $pc <- $pc + 154 (0x9a) <---- 0-check size (?)
    80000674:	55 62       	$r6 <- $r5
    80000676:	44 02       	$r0 <- $r4 <---- $r0 is set to the destination pointer

So let's follow this stream of instructions:

    80000666:	f4 03 03 00 	$r0 <- short 3 (0x3) & $r4          EXEC: 19800
    8000066a:	00 f0 b4 00 	if $r0 == 0 $pc <- $pc + 180 (0xb4) EXEC: 20100
    8000066e:	6e 1b       	$r1 <- tiny $r6 - 1                 EXEC: 20200 (speculative)
    80000670:	06 f0 9a 00 	if $r6 == 0 $pc <- $pc + 154 (0x9a) <---- 0-check size (?)
    80000674:	55 62       	$r6 <- $r5
    80000676:	44 02       	$r0 <- $r4 <---- $r0 is set to the destination pointer

Hmm... The branch seems to be flying to the wrong target: 1 word too early. We should land here:

    8000071e:	44 02       	$r0 <- $r4
    80000720:	66 12       	$r1 <- $r6
    80000722:	00 f1 71 ff 	if $r0 == $r0 $pc <- $pc - 144 (0x90)
    80000726:	11 72       	$r7 <- $r1
    80000728:	00 f1 b7 ff 	if $r0 == $r0 $pc <- $pc - 74 (0x4a)

Instead, we land on the previous two bytes decoded as the instruction word: FFF1. So, we mis-compute the branch target, but WHY?

When we issued the branch, the target was set to 0xb4, which seems correct. At that point $spc was 0x40000334, which apparently is *not* correct.

I think the issue is that instruction len gets passed on to execute wrongly: the NEXT instruction length is passed, instead of the issued one.

I think in fact, we've skipped over one instruction. Since there was no branch, its possible for fetch and $spc to get out of sync.

So, let's back up:

    8000065a <memset>:
    8000065a:	ff cc       	mem32[$sp - tiny 4 (0xfffffffc)] <- $fp
    8000065c:	fd 8c       	mem32[$sp - tiny 8 (0xfffffff8)] <- $r8
    8000065e:	dd c2       	$fp <- $sp
    80000660:	fd d4 f8 ff 	$sp <- short -8 (0xfffffff8) + $sp

We seem to be never executing the first instruction. In fact not even decode it. We put the stuff on the decode input, but never bother to raise 'valid'. This is at 18600ns. And that's because decode_taken is high.

OK, so the problem here was that we haven't properly cleared decode_taken upon a branch.

We are now getting further, but not *that* much further. We start executing this:

    8000024a <frame_dummy>:
    8000024a:	ff cc       	mem32[$sp - tiny 4 (0xfffffffc)] <- $fp
    8000024c:	fd ec       	mem32[$sp - tiny 8 (0xfffffff8)] <- $lr
    8000024e:	dd c2       	$fp <- $sp
    80000250:	fd d4 e8 ff 	$sp <- short -24 (0xffffffe8) + $sp
    80000254:	0f 00 00 00 	$r0 <- 0 (0x0)
    80000258:	00 00
    8000025a:	00 f0 14 00 	if $r0 == 0 $pc <- $pc + 20 (0x14) <----------- GET THIS FAR
    8000025e:	0f 50 20 1e 	$r5 <- 2147491360 (0x80001e20)
    80000262:	00 80
    80000264:	0f 40 ac 19 	$r4 <- 2147490220 (0x800019ac)
    80000268:	00 80
    8000026a:	22 e0       	$lr <- $pc + 4 (0x4)
    8000026c:	02 00       	$pc <- $r0
    8000026e:	24 e0       	$lr <- $pc + 8 (0x8)              <----------- should jump here
    80000270:	ef 20 c2 01 	$pc <- 2147484098 (0x800001c2)
    80000274:	00 80
    80000276:	fd d4 18 00 	$sp <- short 24 (0x18) + $sp
    8000027a:	fd ed       	$lr <- mem32[$sp - tiny 8 (0xfffffff8)]
    8000027c:	ff cd       	$fp <- mem32[$sp - tiny 4 (0xfffffffc)]
    8000027e:	02 e0       	$pc <- $lr

But end up jumping into the void. The reason seems to be to screw up the immediate. Instead of 0x14, we're getting 0x80001e20, which is the *next* immediate. The proper immediate is just on tne bus before. This smells another mix-up with FIELD_E.

At 49300, we should have issued the instruction for execution, but declined. That's because the reg file is still busy serving us. So why doesn't field_e_valid persist to the next cycle?

I think the current problem is that we can issue multiple reg_file_req-s for the same to-be-decoded instruction. This is happening at 33000ns, and then again at 33100ns. But maybe the better question task is: why isn't req.ready go low? And it doesn't because we give the response immediately.

Ah! The problem is that we don't have field_e yet even though it's coming in the same cycle as our other fields. And that's because in the first cycle, we're still pregnant with our previous FIELD_E for the previous instruction.

BTW: the problem is even more complex then that: we're even duplicating the acceptance from fetch: both fetch_ready and fetch_valid are active in these cycles.

Single-stage decode
~~~~~~~~~~~~~~~~~~~

So, I think I decided it's enough with the band-aids, and I'll just re-write InstAssemble and Decode as a single stage.

I will also structure it as a stage with input registers as opposed to output ones. That I think works well for read-valid signalling.

I will need the following states:

**got_inst_word**: This goes high when I accept the first word of an instruction and goes low when the instruction is delivered for execution. It also goes low on branches. It follows the logic of a forward buf in the sense that if we deliver and capture at the same time, it stays high.

**got_field_e_low**: This goes high if the first word of field_e is captured. It goes low when the instruction is delivered for execution. It also goes low on branches. Since it *can't* be the first word for an instruction, it's impossible to set and clear in the same cycle.

**got_field_e_high**: This goes high if the second word of field_e is captured. It goes low when the instruction is delivered for execution. It also goes low on branches. Since it *can't* be the first word for an instruction, it's impossible to set and clear in the same cycle.

**got_field_e**: This, I don't think is a state. It's just a combination of got_field_e_low, got_field_e_high and got_inst_word, based on instruction length.

Handshaking still goes through the register file, in the following way:

reg_file_req.valid is driven by 'valid' of the forward_buf logic of got_inst_word.
reg_file_req.ready drives the 'ready' of forward_buf_logic

reg_file_rsp.ready is driven by decode_out.ready & got_field_e.
reg_file_rsp.valid drives decode_out.valid

Since reg file propagates reg_file_rsp.ready to reg_file_req.ready, we won't accept the next instruction word, until we can deliver field_e to execute.


Wow, this is complicated. At this point I have found a RF bug, realized that decode needs an extra pipeline stage, that field_e capturing is way more complex then I thought it would be.

Now I finally can execute up until the first pipelined branch. I'm guessing the problem is that I don't clear something on do_branch and things get wedged.

So, the last (maybe) problem:

on test_ldst, we generate an unaligned exception at 156810ns.






OTHER NOTES
===========
On the first few instructions out of DRAM: there's no dependency between them, yet RF seems to apply back-pressure. Why???