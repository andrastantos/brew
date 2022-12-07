#!/usr/bin/python3
from typing import *
from silicon import *
from silicon.memory import SimpleDualPortMemory
from .brew_types import *

"""
The instruction FIFO separates the front-end from the back-end.

The front-end is capable of pushing up to two instructions every clock cycle
(in case two back-to-back 16-bit instructions appear in the instruction stream).

The back-end can only pop a single instruction from the FIFO in a clock cycle.

To support the double-wide push interface, the underlying memory is also double-wide.
This is a problem, because the instruction entries are very wide:

1. Instruction code:    64 bits
2. Instruction address: 31 bits

Since we have only 32-bit (maybe 36-bit) wide BRAMs, this means that a double-wide interface
would require us to use 6 BRAMs at minimum. This is a rather significant waste, considering
that we only need a few entries (maybe 5-10) because the FE can only get ahead of the BE a lot
in case the BE gets really stuck on something, such as a cache or TLB miss on a load.

Filling up a deep queue is counter-productive because the BE would never be able to catch up
(the FE is faster on average then the BE) and so, we would be dealing with a full queue until
a branch mis-predict which then flushes the whole thing. The longer the queue, the more fetches
(cache loads, etc.) we did in vain.

So, the queue is relatively shallow, but relatively wide.

I really don't like the idea of variable length entries in this queue, the whole purpose
of the FE was to abstract away the variable instruction length form the BE.

The alternative though is to keep the queue in flops, which is, what 500-1000 flops?

Now, MLABs (on things that have such a thing) might be a compromise, as they can
contain 32-bits of data.

So, let's plan on that:
1. We'll have a single MLAB based FIFO. Entries are wide enough for
   two instructions, plus a bit stating if the top half is valid.
2. On the push side, we push one or two entries. If a single entry
   is pushed, the top side is left unused (wasted if you wish).
3. On the pull side, instructions are pulled one at a time.
   The FIFO is advanced if the top half is not used, or if
   the top half is consumed.
4. This requires a mux on the pull side that allows to
   forward either the top or the bottom half.

Now, one savings is possible: we don't need to store both addresses.
The top instruction will always be consecutive to the bottom one,
if it exists at all. That is to say: we never push two entries
on taken branches.
"""

class InstFifo(Module):
    clk = Input(logic)
    rst = Input(logic)

    push_data = Input(FeBeQueue())
    pop_data = Output(DecodeIn())

    def body(self):
        self.fifo_out = Fifo(depth=10)(self.push_data)
        self.out_fsm = FSM()

        class States(Enum):
            wait_fifo = 0
            wait_be = 1
            wait_be_last = 3

        # The state-machine to control the output
        self.out_fsm.reset_value <<= States.wait_fifo
        self.out_fsm.default_state <<= States.wait_fifo

        self.out_fsm.add_transition(
            States.wait_fifo,
            self.fifo_out.valid & ~self.pop_data.ready & self.fifo_out.has_top, # We're not ready to consume the bottom, but there's also a top
            States.wait_be
        )
        self.out_fsm.add_transition(
            States.wait_fifo,
            (self.fifo_out.valid & ~self.pop_data.ready & ~self.fifo_out.has_top) | # We're not ready to consume the bottom and there is no top
            (self.fifo_out.valid &  self.pop_data.ready &  self.fifo_out.has_top),  # We consume the bottom in this cycle, but there's a top half to consume after
            States.wait_be_last
        )
        self.out_fsm.add_transition(
            States.wait_be,
            self.pop_data.ready,
            States.wait_be_last
        )
        self.out_fsm.add_transition(
            States.wait_be_last,
            self.pop_data.ready,
            States.wait_fifo
        )

        self.pop_data.valid <<= self.fifo_out.valid
        self.pop_data.inst.inst <<= Select(
            self.fifo_out.has_top & self.out_fsm.state != States.wait_be_last,
            self.fifo_out.inst_bottom.inst,
            self.fifo_out.inst_top
        )
        self.pop_data.inst.prefix <<= Select(
            self.fifo_out.has_top & self.out_fsm.state != States.wait_be_last,
            self.fifo_out.inst_bottom.prefix,
            None
        )
        self.pop_data.inst.has_prefix <<= Select(
            self.fifo_out.has_top & self.out_fsm.state != States.wait_be_last,
            self.fifo_out.inst_bottom.has_prefix,
            0
        )
        self.pop_data.inst.inst_len <<= Select(
            self.fifo_out.has_top & self.out_fsm.state != States.wait_be_last,
            self.fifo_out.inst.inst_len,
            1
        )
        self.pop_data.addr <<= Select(
            self.fifo_out.has_top & self.out_fsm.state != States.wait_be_last,
            self.fifo_out.addr,
            self.fifo_out.addr + self.fifo_out.inst.inst_len
        )
        self.fifo_out.ready <<= self.pop_data.ready & self.out_fsm.state != States.wait_be
