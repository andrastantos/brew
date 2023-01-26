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
except ImportError:
    from brew_types import *
    from brew_utils import *

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

NOTE: Fetch as-is now has a minimum 4-cycle latency:
    1 cycle to initiate the bus request
    1 cycle for the bus to give the reply
    1 cycle to get the data through the FIFO
    1 cycle to assemble the instruction
"""

class MemToFetchIf(ReadyValid):
    data = Unsigned(16)
    av = logic

class InstBuffer(Module):
    """
    This module deals with the interfacing to the bus interface and generating an instruction word stream for the fetch stage.
    """
    clk = ClkPort()
    rst = RstPort()

    # Bus interface
    bus_if = Output(BusIfPortIf)

    # Interface towards fetch
    queue = Output(MemToFetchIf)
    queue_free_cnt = Input(Unsigned(3)) # We have a queue of 8 words to pre-fetch. The external FIFO logic will provide us with the free count


    # Side-band interfaces
    mem_base = Input(BrewMemBase)
    mem_limit = Input(BrewMemBase)
    spc  = Input(BrewInstAddr)
    tpc  = Input(BrewInstAddr)
    task_mode  = Input(logic)
    do_branch = Input(logic)

    def body(self):
        def truncate_addr(a):
            return a[BrewInstAddr.get_length()-1:0]

        self.fetch_addr = Wire(BrewInstAddr)

        fetch_increment = Wire(logic)

        branch_target = Wire()
        branch_target <<= truncate_addr(Select(
            self.task_mode,
            self.spc,
            self.tpc
        ) + (self.mem_base << BrewMemShift))

        self.fetch_addr <<= Select(
            self.do_branch,
            Reg(
                Select(
                    self.do_branch,
                    # Normal incremental fetch
                    truncate_addr(self.fetch_addr + fetch_increment),
                    # Branch - compute new physical address
                    branch_target
                )
            ),
            branch_target
        )

        fetch_av = self.fetch_addr[BrewInstAddr.get_length()-1:BrewMemShift] > self.mem_limit

        self.fsm = FSM()

        class InstBufferStates(Enum):
            idle = 0
            request_start = 1
            request = 2
            request_last = 3
            flush_start = 4
            flush = 5

        self.fsm.reset_value <<= InstBufferStates.idle
        self.fsm.default_state <<= InstBufferStates.idle

        state = Wire()
        state <<= self.fsm.state

        '''
        4-beat access timing

                                     ...idle>< r_s ><  r ><  r ><  r >< r_l><idle.......
            CLK               /^^\__/^^\__/^^\~__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            b_request         _________/^^^^^^~\______________________________________
            b_addr            ---------<  a  >~---------------------------------------
            b_response        ________________~/^^^^^^^^^^^^^^^^^^^^^^\_______________
            b_last            ________________~_________________/^^^^^\_______________
            b_data_in         ----------------~-----<=====X=====X=====X=====>---------

        '''

        start_new_request = (self.queue_free_cnt >= 4) | self.do_branch
        self.fsm.add_transition(InstBufferStates.idle, ~start_new_request, InstBufferStates.idle)
        self.fsm.add_transition(InstBufferStates.idle,  start_new_request, InstBufferStates.request_start)
        self.fsm.add_transition(InstBufferStates.request_start, ~self.do_branch & ~self.bus_if.response, InstBufferStates.request_start)
        self.fsm.add_transition(InstBufferStates.request_start, ~self.do_branch &  self.bus_if.response & ~self.bus_if.last, InstBufferStates.request)
        self.fsm.add_transition(InstBufferStates.request_start, ~self.do_branch &  self.bus_if.response &  self.bus_if.last, InstBufferStates.request_last)
        self.fsm.add_transition(InstBufferStates.request_start,  self.do_branch & ~self.bus_if.response, InstBufferStates.flush_start)
        self.fsm.add_transition(InstBufferStates.request_start,  self.do_branch &  self.bus_if.response & ~self.bus_if.last, InstBufferStates.flush)
        self.fsm.add_transition(InstBufferStates.request_start,  self.do_branch &  self.bus_if.response &  self.bus_if.last, InstBufferStates.idle)
        self.fsm.add_transition(InstBufferStates.request, ~self.do_branch & ~self.bus_if.response,                     InstBufferStates.request)
        self.fsm.add_transition(InstBufferStates.request, ~self.do_branch &  self.bus_if.response & ~self.bus_if.last, InstBufferStates.request)
        self.fsm.add_transition(InstBufferStates.request, ~self.do_branch &  self.bus_if.response &  self.bus_if.last, InstBufferStates.request_last)
        self.fsm.add_transition(InstBufferStates.request,  self.do_branch & ~self.bus_if.response,                     InstBufferStates.flush)
        self.fsm.add_transition(InstBufferStates.request,  self.do_branch &  self.bus_if.response & ~self.bus_if.last, InstBufferStates.flush)
        self.fsm.add_transition(InstBufferStates.request,  self.do_branch &  self.bus_if.response &  self.bus_if.last, InstBufferStates.idle)
        self.fsm.add_transition(InstBufferStates.request_last, ~start_new_request, InstBufferStates.idle)
        self.fsm.add_transition(InstBufferStates.request_last,  start_new_request, InstBufferStates.request_start)
        self.fsm.add_transition(InstBufferStates.flush_start, ~self.bus_if.response, InstBufferStates.flush_start)
        self.fsm.add_transition(InstBufferStates.flush_start,  self.bus_if.response, InstBufferStates.flush)
        self.fsm.add_transition(InstBufferStates.flush, ~self.bus_if.response,                     InstBufferStates.flush)
        self.fsm.add_transition(InstBufferStates.flush,  self.bus_if.response & ~self.bus_if.last, InstBufferStates.flush)
        self.fsm.add_transition(InstBufferStates.flush,  self.bus_if.response &  self.bus_if.last, InstBufferStates.idle)

        """
                  +--------+--------+--------+--------+
                  | word 0 | word 1 | word 2 | word 3 |
                  +--------+--------+--------+--------+
        address:      0        1        2        3
        burst_len:    3        2        1        0
        """
        self.bus_if.addr <<= self.fetch_addr
        self.bus_if.read_not_write <<= 1
        self.bus_if.burst_len <<= 3 - self.fetch_addr[1:0]
        self.bus_if.byte_en <<= 3
        self.bus_if.data_in <<= None # This is a read-only port
        self.bus_if.request <<= (state == InstBufferStates.request_start) | (state == InstBufferStates.flush_start)

        fetch_increment <<= self.bus_if.response & ((state == InstBufferStates.request_start) | (state == InstBufferStates.request))

        response_d = Wire()
        response_d <<= Reg(self.bus_if.response & ((state == InstBufferStates.request_start) | (state == InstBufferStates.request)) & ~self.do_branch)

        self.queue.data <<= self.bus_if.data_out
        self.queue.av <<= fetch_av
        self.queue.valid <<= response_d

# A simple FIFO with some extra sprinkles to handle bursts and flushing. It sits between the instruction buffer and fetch
class InstQueue(Module):
    clk = ClkPort()
    rst = RstPort()

    # Interface towards instruction buffer
    inst = Input(MemToFetchIf)
    queue_free_cnt = Output(Unsigned(3)) # We have a queue of 8 words to pre-fetch. The external FIFO logic will provide us with the free count
    # Interface towards fetch
    assemble = Output(MemToFetchIf)

    # Side-band interfaces
    do_branch = Input(logic)

    def body(self):
        depth = 7
        self.assemble <<= Fifo(depth=depth)(self.inst, clear = self.do_branch)

        self.empty_cnt = Wire(Unsigned(3))
        dec = self.inst.ready & self.inst.valid
        inc = self.assemble.ready & self.assemble.valid
        self.empty_cnt <<= Reg(
            Select(
                self.do_branch,
                (self.empty_cnt + inc - dec)[2:0],
                depth
            ),
            reset_value_port = depth
        )
        self.queue_free_cnt <<= self.empty_cnt

class InstAssemble(Module):
    clk = ClkPort()
    rst = RstPort()

    inst_buf = Input(MemToFetchIf)
    decode = Output(FetchDecodeIf)

    do_branch = Input(logic)

    def body(self):
        @module(1)
        def inst_len(inst_word):
            """
            Decodes and returns the instruction length:
                0 -> 16 bits
                1 -> 32 bits
                2 -> 48 bits
            """
            multi_parcel_inst = \
                (field_d(inst_word) == 0xf) | \
                ((field_c(inst_word) == 0xf) & ((field_b(inst_word) != 0xf) | (field_a(inst_word) == 0xf))) | \
                ((field_c(inst_word) == 0xe) & (field_a(inst_word) == 0xf)) | \
                ((field_c(inst_word) < 0xc) & ((field_b(inst_word) == 0xf) | (field_a(inst_word) == 0xf)))

            inst_32_bit = (field_d(inst_word) == 0xf) | (field_a(inst_word) != 0xf)

            # 0 -> 16 bits, 1 -> 32 bits, 2 -> 48 bits
            return concat(
                multi_parcel_inst & ~inst_32_bit,
                multi_parcel_inst &  inst_32_bit
            )

        fsm_advance = Wire(logic)
        load_from_top = Wire(logic)

        # Datapath registers
        inst_len_reg = Wire(Unsigned(2))
        inst_reg_0 = Wire(Unsigned(16))
        inst_reg_1 = Wire(Unsigned(16))
        inst_reg_2 = Wire(Unsigned(16))

        # Instruction pre-decode: in this stage, all we care about is whether an instruction is a prefix and what it's length is
        inst_fragment = self.inst_buf.data
        inst_len = inst_len(inst_fragment)

        # State machine
        self.decode_fsm = FSM()

        class InstAssembleStates(Enum):
            have_0_fragments = 0
            need_1_fragments = 1
            need_2_fragments = 2
            have_all_fragments = 3

        self.decode_fsm.reset_value <<= InstAssembleStates.have_0_fragments
        self.decode_fsm.default_state <<= InstAssembleStates.have_0_fragments

        fsm_state = Wire()
        fsm_state <<= self.decode_fsm.state

        # We're in a state where we don't have anything partial
        self.decode_fsm.add_transition(InstAssembleStates.have_0_fragments, ~self.do_branch &  fsm_advance & (inst_len == inst_len_16), InstAssembleStates.have_all_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.have_0_fragments, ~self.do_branch &  fsm_advance & (inst_len == inst_len_32), InstAssembleStates.need_1_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.have_0_fragments, ~self.do_branch &  fsm_advance & (inst_len == inst_len_48), InstAssembleStates.need_2_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.have_0_fragments, ~self.do_branch & ~fsm_advance                            , InstAssembleStates.have_0_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.have_0_fragments,  self.do_branch                                           , InstAssembleStates.have_0_fragments)
        # We're in a state where we have 1 parcel for the bottom
        self.decode_fsm.add_transition(InstAssembleStates.need_1_fragments, ~self.do_branch &  fsm_advance, InstAssembleStates.have_all_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.need_1_fragments, ~self.do_branch & ~fsm_advance, InstAssembleStates.need_1_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.need_1_fragments,  self.do_branch               , InstAssembleStates.have_0_fragments)
        # We're in a state where we have 2 fragments for the bottom
        self.decode_fsm.add_transition(InstAssembleStates.need_2_fragments, ~self.do_branch &  fsm_advance, InstAssembleStates.need_1_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.need_2_fragments, ~self.do_branch & ~fsm_advance, InstAssembleStates.need_2_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.need_2_fragments,  self.do_branch,                InstAssembleStates.have_0_fragments)
        # We have all the fragments: we either advance to the next set of instructions, or reset if the source is not valid
        self.decode_fsm.add_transition(InstAssembleStates.have_all_fragments, ~self.do_branch &  fsm_advance & self.inst_buf.valid & (inst_len == inst_len_16), InstAssembleStates.have_all_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.have_all_fragments, ~self.do_branch &  fsm_advance & self.inst_buf.valid & (inst_len == inst_len_32), InstAssembleStates.need_1_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.have_all_fragments, ~self.do_branch &  fsm_advance & self.inst_buf.valid & (inst_len == inst_len_48), InstAssembleStates.need_2_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.have_all_fragments, ~self.do_branch &  fsm_advance & ~self.inst_buf.valid                           , InstAssembleStates.have_0_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.have_all_fragments, ~self.do_branch & ~fsm_advance                                                  , InstAssembleStates.have_all_fragments)
        self.decode_fsm.add_transition(InstAssembleStates.have_all_fragments,  self.do_branch                                                                 , InstAssembleStates.have_0_fragments)

        # Handshake logic: we're widening the datapath, so it's essentially the same as a ForwardBuf
        terminal_fsm_state = Wire(logic)
        terminal_fsm_state <<= (self.decode_fsm.state == InstAssembleStates.have_all_fragments)
        fetch_ready = ~terminal_fsm_state | self.decode.ready | self.do_branch
        self.inst_buf.ready <<= fetch_ready
        self.decode.valid <<= terminal_fsm_state & ~self.do_branch
        fsm_advance <<= (~terminal_fsm_state & self.inst_buf.valid & fetch_ready) | (terminal_fsm_state & self.decode.ready)

        # Loading of the datapath registers
        fetch_av = Wire(logic)
        with fsm_advance as clk_en:
            fetch_av <<= Reg(
                Select(
                    (self.decode_fsm.state == InstAssembleStates.have_0_fragments) | (self.decode_fsm.state == InstAssembleStates.have_all_fragments),
                    fetch_av,
                    self.inst_buf.av
                )
            )
            inst_reg_0 <<= Reg(
                Select(
                    (self.decode_fsm.state == InstAssembleStates.have_0_fragments) | (self.decode_fsm.state == InstAssembleStates.have_all_fragments),
                    inst_reg_0,
                    inst_fragment
                )
            )
            inst_reg_1 <<= Reg(
                Select(
                    ((self.decode_fsm.state == InstAssembleStates.need_1_fragments) & (inst_len_reg == inst_len_32)) |
                    (self.decode_fsm.state == InstAssembleStates.need_2_fragments),
                    inst_reg_1,
                    inst_fragment
                )
            )
            inst_reg_2 <<= Reg(
                inst_fragment
            )
            inst_len_reg <<= Reg(
                Select(
                    (self.decode_fsm.state == InstAssembleStates.have_0_fragments) | (self.decode_fsm.state == InstAssembleStates.have_all_fragments),
                    inst_len_reg,
                    inst_len
                )
            )

        # Filling the output data
        self.decode.inst_0 <<= inst_reg_0
        self.decode.inst_1 <<= inst_reg_1
        self.decode.inst_2 <<= inst_reg_2
        self.decode.inst_len <<= inst_len_reg
        self.decode.av <<= fetch_av



class FetchStage(Module):
    clk = ClkPort()
    rst = RstPort()

    # Bus interface
    bus_if = Output(BusIfPortIf)

    # Decode interface
    decode = Output(FetchDecodeIf)

    # Side-band interfaces
    mem_base = Input(BrewMemBase)
    mem_limit = Input(BrewMemBase)
    spc  = Input(BrewInstAddr)
    tpc  = Input(BrewInstAddr)
    task_mode  = Input(logic)
    do_branch = Input(logic)

    def body(self):
        inst_buf = InstBuffer()
        inst_queue = InstQueue()
        inst_assemble = InstAssemble()

        self.bus_if <<= inst_buf.bus_if
        inst_queue.inst <<= inst_buf.queue
        inst_buf.queue_free_cnt <<= inst_queue.queue_free_cnt

        inst_buf.mem_base <<= self.mem_base
        inst_buf.mem_limit <<= self.mem_limit
        inst_buf.spc <<= self.spc
        inst_buf.tpc <<= self.tpc
        inst_buf.task_mode <<= self.task_mode
        inst_buf.do_branch <<= self.do_branch

        inst_queue.do_branch <<= self.do_branch

        inst_assemble.inst_buf <<= inst_queue.assemble
        self.decode <<= inst_assemble.decode
        inst_assemble.do_branch <<= self.do_branch

def gen():
    Build.generate_rtl(FetchStage)

"""
def sim():

    inst_choices = (
        (0x1100,                        ), # $r1 <- $r0 ^ $r0
        (0x20f0, 0x2ddd,                ), # $r1 <- short b001
        (0x300f, 0x3dd0, 0x3dd1,        ), # $r1 <- 0xdeadbeef
    )
    inst_stream = []
    class Generator(RvSimSource):
        def construct(self, max_wait_state: int = 0):
            super().construct(MemToFetchIf, None, max_wait_state)
            self.addr = -1
            self.inst_fetch_stream = []
        def generator(self, is_reset):
            if is_reset:
                return 0,0
            self.addr += 1
            while len(self.inst_fetch_stream) < 2:
                inst = inst_choices[randint(0,len(inst_choices)-1)]
                inst_stream.append(inst)
                self.inst_fetch_stream += inst
            # Don't combine the two instructions, because I don't want to rely on expression evaluation order. That sounds dangerous...
            data = self.inst_fetch_stream.pop(0)
            return self.addr, data

    class Checker(RvSimSink):
        def construct(self, max_wait_state: int = 0):
            super().construct(None, max_wait_state)
            self.cnt = 0
        def checker(self, value):
            def get_next_inst():
                inst = inst_stream.pop(0)
                print(f"  --- inst:", end="")
                for i in inst:
                    print(f" {i:04x}", end="")
                print("")
                has_prefix = inst[0] & 0x0ff0 == 0x0ff0
                if has_prefix:
                    prefix = inst[0]
                    inst = inst[1:]
                else:
                    prefix = None
                inst_len = len(inst)-1
                inst_code = 0
                for idx, word in enumerate(inst):
                    inst_code |= word << (16*idx)
                return prefix, has_prefix, inst_code, inst_len

            expected_prefix, expected_has_prefix, expected_inst_code, expected_inst_len = get_next_inst()
            print(f"Received: ", end="")
            if value.inst_bottom.has_prefix:
                print(f" [{value.inst_bottom.prefix:04x}]", end="")
            for i in range(value.inst_bottom.inst_len+1):
                print(f" {(value.inst_bottom.inst >> (16*i)) & 0xffff:04x}", end="")
            if value.has_top:
                print(f" top: {value.inst_top:04x}", end="")
            print("")

            assert expected_has_prefix == value.inst_bottom.has_prefix
            assert not expected_has_prefix or expected_prefix == value.inst_bottom.prefix
            assert expected_inst_len == value.inst_bottom.inst_len
            inst_mask = (1 << (16*(expected_inst_len+1))) - 1
            assert (expected_inst_code & inst_mask) == (value.inst_bottom.inst & inst_mask)
            if value.has_top == 1:
                expected_prefix, expected_has_prefix, expected_inst_code, expected_inst_len = get_next_inst()
                assert not expected_has_prefix
                assert expected_inst_len == 0
                assert expected_inst_code == value.inst_top

    class top(Module):
        clk = ClkPort()
        rst = RstPort()

        def body(self):
            seed(0)
            self.input_stream = Wire(MemToFetchIf)
            self.checker_module = Checker()
            self.generator = Generator()
            self.input_stream <<= self.generator.output_port
            dut = InstAssemble()
            dut.rst <<= self.rst
            dut.clk <<= self.clk
            self.checker_module.input_port <<= dut(self.input_stream)

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

            self.generator.max_wait_state = 2
            self.checker_module.max_wait_state = 5
            for i in range(500):
                yield from clk()
            self.generator.max_wait_state = 0
            self.checker_module.max_wait_state = 0
            for i in range(500):
                yield from clk()
            now = yield 10
            self.generator.max_wait_state = 5
            self.checker_module.max_wait_state = 2
            for i in range(500):
                yield from clk()
            now = yield 10
            print(f"Done at {now}")

    Build.simulation(top, "fetch.vcd", add_unnamed_scopes=True)
"""


def sim():
    class BusEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        bus_if = Input(BusIfPortIf)

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
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            self.bus_if.response <<= 0
            self.bus_if.data_out <<= None
            self.bus_if.last <<= None
            mem_mask = (2 ** len(self.mem).bit_length())-1
            while True:
                yield from wait_clk()

                if self.rst == 1:
                    self.bus_if.response <<= 0
                    self.bus_if.data_out <<= None
                    self.bus_if.last <<= None
                else:
                    if self.bus_if.request == 1:
                        burst_cnt = self.bus_if.burst_len.sim_value+1
                        if self.bus_if.read_not_write:
                            # Read request
                            addr = self.bus_if.addr.sim_value
                            print(f"Reading BUS {addr:x} burst:{self.bus_if.burst_len} byte_en:{self.bus_if.byte_en}")
                            self.bus_if.response <<= 0
                            self.bus_if.last <<= 0
                            self.bus_if.data_out <<= None
                            #yield from wait_clk()
                            self.bus_if.response <<= 1
                            for i in range(burst_cnt):
                                mem_addr = addr.value & mem_mask
                                if mem_addr >= len(self.mem):
                                    data = 0
                                else:
                                    data = self.mem[mem_addr]
                                self.bus_if.last <<= 1 if i == burst_cnt-1 else 0
                                self.bus_if.response <<= 1 if i <= burst_cnt-1 else 0
                                yield from wait_clk()
                                print(f"    data:{data:x} (for addr {addr:x})")
                                self.bus_if.data_out <<= data
                                addr = addr + 1
                            self.bus_if.response <<= 0
                            self.bus_if.last <<= 0
                        else:
                            assert False, "WRITES SHOULD NEVER COME FROM FETCH"
                    else:
                        self.bus_if.response <<= 0
                        self.bus_if.data_out <<= None
                        self.bus_if.last <<= None

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
                        yield from wait_clk()
                        yield from wait_clk()
                        yield from wait_clk()
                        yield from wait_clk()
                        self.do_branch <<= 1
                        yield from wait_clk()
                        self.do_branch <<= 0
                        state = "after_start"

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

            self.bus_if = Wire(BusIfPortIf)

            self.dut = FetchStage()
            self.decode_emulator = DecodeEmulator()
            self.bus_emulator = BusEmulator()
            self.sideband_emulator = SidebandEmulator()

            self.bus_if <<= self.dut.bus_if
            self.bus_emulator.bus_if <<= self.bus_if

            self.fetch_to_decode <<= self.dut.decode
            self.decode_emulator.from_fetch <<= self.fetch_to_decode

            self.dut.mem_base <<= self.mem_base
            self.dut.mem_limit <<= self.mem_limit
            self.dut.spc <<= self.spc
            self.dut.tpc <<= self.tpc
            self.dut.task_mode <<= self.task_mode
            self.dut.do_branch <<= self.do_branch

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



#gen()
sim()

