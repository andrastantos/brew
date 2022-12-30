#!/usr/bin/python3
from random import *
from typing import *
from silicon import *
from brew_types import *
from brew_utils import *

"""
In V1, fetch works from a 16-bit read port: it gets (up to) 16-bits of data at a time.

It decodes the instructions and pushes them into a FIFO connecting to decode. We don't
do any branch-prediction, and we don't support any prefix instructions or extension groups.
As such, the maximum instruction length is 48 bits and can always be decoded by looking
at the first 16-bits of the instruction field.
"""

class Fetch(Module):
    clk = ClkPort()
    rst = RstPort()

    fetch = Input(MemToFetchStream)
    push_data = Output(FetchToDecodeQueue)

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
        inst_reg = Wire(Unsigned(48))
        inst_fetch_addr = Wire(BrewInstAddr)

        # Instruction pre-decode: in this stage, all we care about is whether an instruction is a prefix and what it's length is
        inst_fragment = self.fetch.data
        inst_len = inst_len(inst_fragment)

        # State machine
        self.decode_fsm = FSM()

        class States(Enum):
            have_0_fragments = 0
            need_1_fragments = 1
            need_2_fragments = 2
            have_all_fragments = 3

        self.decode_fsm.reset_value <<= States.have_0_fragments
        self.decode_fsm.default_state <<= States.have_0_fragments

        fsm_state = Wire()
        fsm_state <<= self.decode_fsm.state

        # We're in a state where we don't have anything partial
        self.decode_fsm.add_transition(States.have_0_fragments,  fsm_advance & (inst_len == inst_len_16), States.have_all_fragments)
        self.decode_fsm.add_transition(States.have_0_fragments,  fsm_advance & (inst_len == inst_len_32), States.need_1_fragments)
        self.decode_fsm.add_transition(States.have_0_fragments,  fsm_advance & (inst_len == inst_len_48), States.need_2_fragments)
        self.decode_fsm.add_transition(States.have_0_fragments, ~fsm_advance                            , States.have_0_fragments)
        # We're in a state where we have 1 parcel for the bottom
        self.decode_fsm.add_transition(States.need_1_fragments,  fsm_advance, States.have_all_fragments)
        self.decode_fsm.add_transition(States.need_1_fragments, ~fsm_advance, States.need_1_fragments)
        # We're in a state where we have 2 fragments for the bottom
        self.decode_fsm.add_transition(States.need_2_fragments,  fsm_advance, States.need_1_fragments)
        self.decode_fsm.add_transition(States.need_2_fragments, ~fsm_advance, States.need_2_fragments)
        # We have all the fragments: we either advance to the next set of instructions, or reset if the source is not valid
        self.decode_fsm.add_transition(States.have_all_fragments,  fsm_advance & self.fetch.valid & (inst_len == inst_len_16), States.have_all_fragments)
        self.decode_fsm.add_transition(States.have_all_fragments,  fsm_advance & self.fetch.valid & (inst_len == inst_len_32), States.need_1_fragments)
        self.decode_fsm.add_transition(States.have_all_fragments,  fsm_advance & self.fetch.valid & (inst_len == inst_len_48), States.need_2_fragments)
        self.decode_fsm.add_transition(States.have_all_fragments,  fsm_advance & ~self.fetch.valid, States.have_0_fragments)
        self.decode_fsm.add_transition(States.have_all_fragments, ~fsm_advance                    , States.have_all_fragments)

        # Handshake logic: we're widening the datapath, so it's essentially the same as a ForwardBuf
        terminal_fsm_state = Wire(logic)
        terminal_fsm_state <<= (self.decode_fsm.state == States.have_all_fragments)
        fetch_ready = (~terminal_fsm_state & ~top_inst_pre_cond_reg) | self.push_data.ready
        self.fetch.ready <<= fetch_ready
        self.push_data.valid <<= terminal_fsm_state
        fsm_advance <<= (~terminal_fsm_state & self.fetch.valid & fetch_ready) | (terminal_fsm_state & self.push_data.ready)

        # Loading of the datapath registers
        with fsm_advance as clk_en:
            inst_reg[15:0] <<= RegEn(
                Select(
                    self.decode_fsm.state == States.have_0_fragments | self.decode_fsm.state == States.have_all_fragments,
                    inst_reg[15:0],
                    inst_fragment
                )
            )
            inst_reg[31:16] <<= RegEn(
                Select(
                    (self.decode_fsm.state == States.need_1_fragments & inst_len_reg == inst_len_32) |
                    (self.decode_fsm.state == States.need_2_fragments),
                    inst_reg[31:16],
                    inst_fragment
                )
            )
            inst_reg[47:32] <<= RegEn(
                inst_fragment
            )
            inst_len_reg <<= RegEn(
                Select(
                    self.decode_fsm.state == States.have_0_fragments | self.decode_fsm.state == States.have_all_fragments,
                    inst_len_reg,
                    inst_len
                )
            )

        # Filling the output data
        self.push_data.inst <<= inst_reg
        self.push_data.inst_len <<= inst_len_reg

def gen():
    Build.generate_rtl(Fetch)

def sim():

    inst_choices = (
        (0x1100,                        ), # $r1 <- $r0 ^ $r0
        (0x20f0, 0x2ddd,                ), # $r1 <- short b001
        (0x300f, 0x3dd0, 0x3dd1,        ), # $r1 <- 0xdeadbeef
    )
    inst_stream = []
    class Generator(RvSimSource):
        def construct(self, max_wait_state: int = 0):
            super().construct(MemToFetchStream, None, max_wait_state)
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
            self.input_stream = Wire(MemToFetchStream)
            self.checker = Checker()
            self.generator = Generator()
            self.input_stream <<= self.generator.output_port
            dut = Fetch()
            dut.rst <<= self.rst
            dut.clk <<= self.clk
            self.checker.input_port <<= dut(self.input_stream)

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
            self.checker.max_wait_state = 5
            for i in range(500):
                yield from clk()
            self.generator.max_wait_state = 0
            self.checker.max_wait_state = 0
            for i in range(500):
                yield from clk()
            now = yield 10
            self.generator.max_wait_state = 5
            self.checker.max_wait_state = 2
            for i in range(500):
                yield from clk()
            now = yield 10
            print(f"Done at {now}")

    Build.simulation(top, "fetch.vcd", add_unnamed_scopes=True)

#gen()
sim()

