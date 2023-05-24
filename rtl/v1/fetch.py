#!/usr/bin/python3
from random import *
from typing import *
try:
    from silicon import *
except ImportError:
    import sys
    from pathlib import Path
    sys.path.append(str((Path() / ".." / ".." / ".." / "silicon").absolute()))
    from silicon import *

try:
    from .brew_types import *
    from .brew_utils import *
    from .scan import ScanWrapper
    from .synth import *
except ImportError:
    from brew_types import *
    from brew_utils import *
    from scan import ScanWrapper
    from synth import *

"""
In V1, fetch works from a 16-bit read port: it gets (up to) 16-bits of data at a time.

It constructs full instructions and supplies them to decode.

We don't do any branch-prediction, or to be more precise, we're following
straight-line execution, until told otherwise.

We don't support any prefix instructions or extension groups either.
As such, the maximum instruction length is 48 bits and can always be decoded by looking
at the first 16-bits of the instruction field.

The composition of fetch comes from 3 modules:

"""

"""
NOTES: On the BusIf, there's no way to cancel a request: once it's out, it's out.
       We don't know if it's taken or not, until the response comes back. This
       has two implications:
       1. We need to keep 'request' active until 'response' comes back.
       2. We need to throw away data during flushing

Things to test:
- Cancellation in all stages of a fetch
    - Cancelling in the same cycle we issue a request is busted: we update ADDR from the request,
      but cancel the first burst, so we start to fetch from the wrong address.
- Test all sorts of invalid instruction sequences.
- Bursts through a page boundary

NOTE: Fetch as-is now has a minimum 4-cycle latency:
    1 cycle to initiate the bus request
    1 cycle for the bus to give the reply
    1 cycle to get the data through the FIFO
    1 cycle to assemble the instruction
"""
"""
Branch handling:

do_branch is a global signal, that is registered at the output of 'execute'. This goes high for one cycle, signalling that
a new (non-consecutive, that is) address was loaded into $scp or $tcp or task_mode was changed. The new value of these
registers gets latched on the same clock cycle when do_branch goes high.

What needs to happen is that we need to cancel all in-process instructions and restart fetching from the new execution point.

1. Execute will not pass the branching instruction to memory. If there is a reservation (such as an AV in a load), it *does*
   pass a fake resoult over that clears the reservation.
2. Execute accepts the next instruction (if any), but cancells it. The way to do that is to override the output to be a simple
   pass-through for memory with write-enable disabled, so no actual change to the target register will occur, but the reservation
   gets deleted. TODO: the RF supports this, but memory/exec not yet.
3. Decode will also accept the next instruction (if any), it will however not attempt any reservations and won't pass it to execute either.
   TODO: decode doesn't actually do any of this yet.
4. inst_assemble will also accept the next 16-bits from the fetch queue, but will delete and restart
5. inst_queue will take the next word, but will reset after.
6. inst_buffer might push one word into the queue, but will cancel subsequent responses from the bus_if, even if they were already queued up.

TODO: should we consider simply cancelling all reservations in the RF upon a branch? The danger is the following:
   1. We have a loooong oustanding load.
   2. The next instruction is a branch that ... well, branches
   3. The first instruction at the branch target is something that depends on the target register of the load.
   We are lucky though: since the load hugs the bus, fetch will not succeed until the load returns the result, so by the time
   the fetched instruction winds its way through fetch, the results are safely in the RF. This is very brittle though and the
   first time we introduce an instruction buffer or some sort of a cache, the assumption is not valid anymore and I would wind up
   with an extremely hard to find heisen-bug.

   So, I'm not going to depend on this behavior.

TODO: yet another alternative is that we artificially hold off fetch with outstanding loads during a branch. That would mean exposing
   another wire, called 'doing_load' or something that is incorporated into the branch handling logic.

OVERALL: I think I'm going to stick with the fake write-back idea: that seems relatively clean at least.
"""
#fetch_queue_length = 16
#fetch_threshold = (fetch_queue_length+1)//2
fetch_queue_length = 11
fetch_threshold = 8
QueuePointerType = Unsigned(fetch_queue_length.bit_length())

def truncate_queue_ptr(ptr):
    return ptr[QueuePointerType.get_length()-1:0]
class InstBuffer(GenericModule):
    """
    This module deals with the interfacing to the bus interface and generating an instruction word stream for the fetch stage.

    Here, all we care about is to generate requests to the bus interface and stuff the responses into the queue.

    We will try to generate long bursts, but keep count of how many requests we've sent out to ensure the
    queue won't overflow, should we get all the responses back. We also make sure that we won't hold the bus
    for too long, limiting our bursts to the size of the free queue.

    To simplify implementation, we won't start a new request, until the queue is at least half empty, but we don't
    monitor subsequent pops from the queue.

    We also of course have to pay attention to branches and restart ourselves as needed.
    """
    clk = ClkPort()
    rst = RstPort()


    # Bus interface
    bus_if_request = Output(BusIfRequestIf)
    bus_if_response = Input(BusIfResponseIf)

    # Interface towards fetch
    queue = Output(FetchDecodeIf)
    queue_free_cnt = Input(QueuePointerType) # We have a queue of 8 words to pre-fetch. The external FIFO logic will provide us with the free count


    # Side-band interfaces
    mem_base = Input(BrewMemBase)
    mem_limit = Input(BrewMemBase)
    spc  = Input(BrewInstAddr)
    tpc  = Input(BrewInstAddr)
    task_mode  = Input(logic)
    do_branch = Input(logic) # do_branch is active for one cycle only; in the same cycle the new spc/tpc/task_mode values are available
    break_burst = Input(logic) # active for one cycle, to break any progressing burst. Doesn't kill the queue or drop outstanding requests, simply stops requesting more...

    # Events
    event_fetch = Output(logic)
    event_drop = Output(logic)

    def construct(self, page_bits: int = 7, break_on_branch_early: bool = False, use_break_burst: bool = True):
        self.page_bits = page_bits # 256 bytes, but we count in 16-bit words
        self.break_on_branch_early = break_on_branch_early
        self.use_break_burst = use_break_burst

    def body(self):
        def truncate_addr(a):
            return a[BrewInstAddr.get_length()-1:0]

        advance_request = self.bus_if_request.valid & self.bus_if_request.ready
        advance_response = self.bus_if_response.valid

        branch_target = Wire()
        branch_target <<= Select(
            self.task_mode,
            self.spc,
            truncate_addr(self.tpc + (self.mem_base << BrewMemShift))
        )

        # Capture task mode into a register to make sure we don't AV in scheduler mode
        task_mode_fetch = Wire(logic)
        task_mode_fetch <<= Reg(
            Select(
                self.do_branch,
                task_mode_fetch,
                self.task_mode
            )
        )

        fetch_addr = Wire(BrewInstAddr)
        fetch_addr <<= Reg(
            Select(
                self.do_branch,
                # Normal incremental fetch
                truncate_addr(fetch_addr + advance_request),
                # Branch - compute new physical address
                branch_target
            )
        )

        class InstBufferStates(Enum):
            idle = 0
            request = 1

        state = Wire()
        next_state = Wire()
        fetch_page_limit = Wire(logic)
        fetch_page_limit <<= Reg(Select(
            state == InstBufferStates.idle,
            (~fetch_addr[self.page_bits-1:0]) == 0,
            0
        ))

        fetch_av = task_mode_fetch & (fetch_addr[BrewInstAddr.get_length()-1:BrewMemShift] > self.mem_limit)

        self.fsm = FSM()

        self.fsm.reset_value <<= InstBufferStates.idle
        self.fsm.default_state <<= InstBufferStates.idle

        state <<= self.fsm.state
        next_state <<= self.fsm.next_state

        # branch_req will remain high until our request is accepted by the bus_if
        # NOTE: Acutally, I don't think that's necessary for two reasons:
        # 1. If there's not active request going on, we can cancel immediately.
        # 2. The branch request will clear the instruction queue, so queue_free_cnt
        #    will soon be at it's maximum value, triggering a new request, even if
        #    start_new_request would otherwise go inactive
        branch_req = Wire(logic)
        branch_req <<= self.do_branch
        #branch_req <<= self.do_branch | Reg(
        #    Select(
        #        advance_request,
        #        Select(
        #            self.do_branch,
        #            branch_req,
        #            1
        #        ),
        #        0
        #    )
        #)
        outstanding_request = Wire(Unsigned(2))
        drop_count = Wire(Unsigned(2))
        next_outstanding_request = SelectOne(
            advance_request &  advance_response, outstanding_request,
            advance_request & ~advance_response, (outstanding_request+1)[1:0],
            ~advance_request &  advance_response, (outstanding_request-1)[1:0],
            ~advance_request & ~advance_response, outstanding_request,
        )
        outstanding_request <<= Reg(next_outstanding_request)
        drop_count <<= Reg(Select(
            branch_req,
            Select(
                drop_count == 0,
                Unsigned(2)(drop_count - advance_response),
                0
            ),
            next_outstanding_request
        ))


        # We have to make sure that we only start a new burst if we know for sure the queue can take all the responses,
        # including all the outstanding ones.
        # We need to delay branch_req to align with the request address, which is delayed
        start_new_request = (self.queue_free_cnt > fetch_threshold + next_outstanding_request) | Reg(branch_req)

        if self.use_break_burst:
            if self.break_on_branch_early:
                field_a = self.bus_if_response.data[ 3: 0]
                field_b = self.bus_if_response.data[ 4: 7]
                field_c = self.bus_if_response.data[11: 8]
                field_d = self.bus_if_response.data[15:12]
                field_a_is_f = field_a == 0xf
                field_b_is_f = field_b == 0xf
                field_c_is_f = field_c == 0xf
                field_d_is_f = field_d == 0xf
                could_be_branch = (field_d_is_f & ~field_c_is_f) # Catches all the conditional branches
                break_burst = advance_response & could_be_branch
            else:
                break_burst = self.break_burst
        else:
            break_burst = 0

        req_len = Wire(QueuePointerType)
        req_len <<= Reg(
            Select(
                start_new_request,
                Select(
                    break_burst,
                    Select(advance_request, req_len, Select(req_len > 0, 0, truncate_queue_ptr(req_len - 1))), # No new request: simply decrement
                    0
                ),
                # TODO: I think this could be just 'fetch_threshold', but I don't want to make the change without extensive test coverage
                Select(
                    self.do_branch,
                    # If we don't have a branch, we need to imit fetch burst length to fetch_threshold (to be good citizens on the bus).
                    Select(self.queue_free_cnt >= fetch_threshold, self.queue_free_cnt, fetch_threshold),
                    #self.queue_free_cnt,
                    # In case of a branch, we're blowing the queue away, so ignore the free counter and start a full burst.
                    fetch_threshold)
            ),
            reset_value_port = fetch_threshold
        )
        # We capture the AV state at the beginning of the burst. We will terminate the burst if the AV state changes.
        # This way, the AV state is the same within a burst.
        req_av = Wire(logic)
        req_av <<= Reg(
            Select(
                start_new_request,
                req_av,
                fetch_av
            )
        )

        self.bus_if_request.valid           <<= (state == InstBufferStates.request) & ~fetch_page_limit & ~self.do_branch
        self.bus_if_request.read_not_write  <<= 1
        self.bus_if_request.byte_en         <<= 3
        self.bus_if_request.addr            <<= fetch_addr[BrewBusAddr.length-1:0]
        self.bus_if_request.data            <<= None

        self.fsm.add_transition(InstBufferStates.idle,         start_new_request,                            InstBufferStates.request)
        self.fsm.add_transition(InstBufferStates.request,      fetch_page_limit | (req_len == 0),  InstBufferStates.idle)

        # The response interface is almost completely a pass-through. All we need to do is to handle the AV flag.
        self.queue.data <<= self.bus_if_response.data
        self.queue.av <<= req_av
        self.queue.valid <<=  self.bus_if_response.valid & (drop_count == 0) & ~branch_req
        #AssertOnClk(
        #    state != InstBufferStates.idle | self.queue.ready | (state != InstBufferStates.flush)
        #)
        self.event_drop <<= self.bus_if_response.valid & (drop_count != 0)
        self.event_fetch <<= self.bus_if_response.valid


# A simple FIFO with some extra sprinkles to handle bursts and flushing. It sits between the instruction buffer and fetch
class InstQueue(Module):
    clk = ClkPort()
    rst = RstPort()

    # Interface towards instruction buffer
    inst = Input(FetchDecodeIf)
    queue_free_cnt = Output(QueuePointerType) # We have a queue of 8 words to pre-fetch. The external FIFO logic will provide us with the free count
    # Interface towards fetch
    output_port = Output(FetchDecodeIf)

    # Side-band interfaces
    do_branch = Input(logic)

    # Events
    event_queue_flush = Output(QueuePointerType) # This is strange: in a single clock we drop a bunch of items in the queue.

    def body(self):
        fifo = ZeroDelayFifo(depth=fetch_queue_length)
        #fifo = Fifo(depth=fetch_queue_length)
        self.output_port <<= fifo(self.inst, clear = self.do_branch)

        empty_cnt = Wire(QueuePointerType)
        dec = self.inst.ready & self.inst.valid
        inc = self.output_port.ready & self.output_port.valid
        empty_cnt <<= Reg(
            Select(
                self.do_branch,
                truncate_queue_ptr(empty_cnt + inc - dec),
                fetch_queue_length
            ),
            reset_value_port = fetch_queue_length
        )
        self.queue_free_cnt <<= empty_cnt

        self.event_queue_flush <<= Select(self.do_branch, 0, QueuePointerType(fetch_queue_length - empty_cnt))




class FetchStage(GenericModule):
    clk = ClkPort()
    rst = RstPort()

    # Bus interface
    bus_if_request = Output(BusIfRequestIf)
    bus_if_response = Input(BusIfResponseIf)

    # Decode interface
    decode = Output(FetchDecodeIf)

    # Side-band interfaces
    mem_base = Input(BrewMemBase)
    mem_limit = Input(BrewMemBase)
    spc  = Input(BrewInstAddr)
    tpc  = Input(BrewInstAddr)
    task_mode  = Input(logic)
    do_branch = Input(logic)
    break_burst = Input(logic) # active for one cycle, to break any progressing burst. Doesn't kill the queue or drop outstanding requests, simply stops requesting more...

    # Events
    event_fetch = Output(logic)
    event_dropped = Output()

    def construct(self, page_bits: int):
        self.page_bits = page_bits

    def body(self):
        inst_buf = InstBuffer(page_bits=self.page_bits)
        inst_queue = InstQueue()

        self.bus_if_request <<= inst_buf.bus_if_request
        inst_buf.bus_if_response <<= self.bus_if_response

        inst_queue.inst <<= inst_buf.queue
        inst_buf.queue_free_cnt <<= inst_queue.queue_free_cnt

        inst_buf.mem_base <<= self.mem_base
        inst_buf.mem_limit <<= self.mem_limit
        inst_buf.spc <<= self.spc
        inst_buf.tpc <<= self.tpc
        inst_buf.task_mode <<= self.task_mode
        inst_buf.do_branch <<= self.do_branch
        inst_buf.break_burst <<= self.break_burst

        inst_queue.do_branch <<= self.do_branch

        self.decode <<= inst_queue.output_port

        self.event_fetch <<= inst_buf.event_fetch
        self.event_dropped <<= inst_buf.event_drop + inst_queue.event_queue_flush # TODO: event_dropped now is incorrect: we have to account for the words dropped in decode too!


def sim():

    class BusEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        bus_if_request = Input(BusIfRequestIf)
        bus_if_response = Output(BusIfResponseIf)

        i16 = (0x1100,                        ) # $r1 <- $r0 ^ $r0
        i32 = (0x20f0, 0x2ddd,                ) # $r1 <- short b001
        i48 = (0x300f, 0x3dd0, 0x3dd1,        ) # $r1 <- 0xdeadbeef

        mem = (
            i16 + i16 +
            i16 + i32 +
            i16 + i48 +
            i32 + i16 +
            i32 + i32 +
            i32 + i48 +
            i48 + i16 +
            i48 + i32 +
            i48 + i48
        )

        def simulate(self, simulator: 'Simulator') -> TSimEvent:
            delay_queue = [None, None, None]

            def wait_clk():
                now = yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    now = yield (self.clk, )
                return now

            self.bus_if_request.ready <<= 0
            self.bus_if_response.valid <<= 0
            self.bus_if_response.data <<= None

            mem_mask = (2 ** len(self.mem).bit_length())-1

            while True:
                now = yield from wait_clk()

                if self.rst == 1:
                    self.bus_if_request.ready <<= 0
                    self.bus_if_response.valid <<= 0
                    self.bus_if_response.data <<= None
                else:
                    self.bus_if_request.ready <<= 1
                    if self.bus_if_request.valid == 1:
                        assert self.bus_if_request.read_not_write == 1
                        assert self.bus_if_request.byte_en == 3
                        # Read request
                        addr = self.bus_if_request.addr.sim_value
                        mem_addr = addr.value & mem_mask
                        if mem_addr >= len(self.mem):
                            data = 0
                        else:
                            data = self.mem[mem_addr]
                        print(f"Reading BUS {addr:x} data:{data:x} at {now}")
                        delay_queue[-1] = data
                    if delay_queue[0] is not None:
                        self.bus_if_response.data <<= delay_queue[0]
                        self.bus_if_response.valid <<= 1
                        print(f"    response data:{data:x} at {now}")
                    else:
                        self.bus_if_response.data <<= None
                        self.bus_if_response.valid <<= 0
                    delay_queue = delay_queue[1:] + [None, ]

    class DecodeEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        from_fetch = Input(FetchDecodeIf)

        def body(self):
            self.from_fetch.ready <<= 1

    class SidebandEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        mem_base = Output(BrewMemBase)
        mem_limit = Output(BrewMemBase)
        spc  = Output(BrewInstAddr)
        tpc  = Output(BrewInstAddr)
        task_mode  = Output(logic)
        do_branch = Output(logic)

        def simulate(self, simulator: 'Simulator') -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            self.mem_base <<= 0
            self.mem_limit <<= 0x3fffff # TODO: Do we want to reset this to all 1-s???
            self.spc <<= 0x00000000
            self.tpc <<= 0x10000000
            self.task_mode <<= 1 # TODO: whops, do we want to invert this signal so that we can reset to '0'?
            self.do_branch <<= 0

            state = "start"
            while True:
                yield from wait_clk()

                if self.rst == 0:
                    if state == "start":
                        yield from wait_clk()
                        self.do_branch <<= 1
                        yield from wait_clk()
                        self.do_branch <<= 0
                        state = "after_start"
                    if state == "after_start":
                        yield from wait_clk()
                        yield from wait_clk()
                        yield from wait_clk()
                        yield from wait_clk()
                        yield from wait_clk()
                        yield from wait_clk()
                        self.do_branch <<= 1
                        yield from wait_clk()
                        self.do_branch <<= 0
                        state = "done"

    class top(Module):
        clk = ClkPort()
        rst = RstPort()

        def body(self):
            seed(0)

            # Side-band interfaces
            self.mem_base = Wire(BrewMemBase)
            self.mem_limit = Wire(BrewMemBase)
            self.spc  = Wire(BrewInstAddr)
            self.tpc  = Wire(BrewInstAddr)
            self.task_mode  = Wire(logic)
            self.do_branch = Wire(logic)

            self.fetch_to_decode = Wire(FetchDecodeIf)

            self.bus_if_req = Wire(BusIfRequestIf)
            self.bus_if_rsp = Wire(BusIfResponseIf)

            self.dut = FetchStage(page_bits=7)
            self.decode_emulator = DecodeEmulator()
            self.bus_emulator = BusEmulator()
            self.sideband_emulator = SidebandEmulator()

            self.bus_if_req <<= self.dut.bus_if_request
            self.bus_emulator.bus_if_request <<= self.bus_if_req
            self.bus_if_rsp <<= self.bus_emulator.bus_if_response
            self.dut.bus_if_response <<= self.bus_if_rsp

            self.fetch_to_decode <<= self.dut.decode
            self.decode_emulator.from_fetch <<= self.fetch_to_decode

            self.dut.mem_base <<= self.mem_base
            self.dut.mem_limit <<= self.mem_limit
            self.dut.spc <<= self.spc
            self.dut.tpc <<= self.tpc
            self.dut.task_mode <<= self.task_mode
            self.dut.do_branch <<= self.do_branch
            self.dut.break_burst <<= 0

            self.mem_base <<= self.sideband_emulator.mem_base
            self.mem_limit <<= self.sideband_emulator.mem_limit
            self.spc <<= self.sideband_emulator.spc
            self.tpc <<= self.sideband_emulator.tpc
            self.task_mode <<= self.sideband_emulator.task_mode
            self.do_branch <<= self.sideband_emulator.do_branch


        def simulate(self) -> TSimEvent:
            def clk() -> int:
                yield 10
                self.clk <<= ~self.clk & self.clk
                yield 10
                self.clk <<= ~self.clk
                yield 0

            print("Simulation started")

            self.rst <<= 1
            self.clk <<= 1
            yield 10
            for i in range(5):
                yield from clk()
            self.rst <<= 0

            for i in range(50):
                yield from clk()
            now = yield 10
            print(f"Done at {now}")

    Build.simulation(top, "fetch.vcd", add_unnamed_scopes=True)


def gen():
    def top():
        return ScanWrapper(FetchStage, {"clk", "rst"})

    netlist = Build.generate_rtl(top, "fetch.sv")
    top_level_name = netlist.get_module_class_name(netlist.top_level)
    flow = QuartusFlow(target_dir="q_fetch", top_level=top_level_name, source_files=("fetch.sv",), clocks=(("clk", 10), ("top_clk", 100)), project_name="fetch")
    flow.generate()
    flow.run()


if __name__ == "__main__":
    #gen()
    sim()

