#!/usr/bin/python3
from random import *
from typing import *
from silicon import *
from brew_types import *
from brew_utils import *

"""
Bus interface of the V1 pipeline.

This module is not part of the main pipeline, it sits on the side.

It communicates with 'fetch' and 'memory' to serve memory requests.

It does the following:
- Handles arbitration (internal and external)
- Creates bursts
- Generates appropriately timed signals for (NMOS) DRAM chips
- Sends data (in case of reads) back to requestors

"""

class Memory(Module):
    clk = ClkPort()
    rst = RstPort()

    # Interface to fetch
    f_request         = Input(logic)
    f_read_not_write  = Input(logic)
    f_burst_len       = Input(Unsigned(2))
    f_byte_en         = Input(Unsigned(2))
    f_addr            = Input(BrewBusAddr)
    f_data_in         = Input(BrewBusData)
    f_response        = Output(logic)
    f_data_out        = Output(BrewBusData)
    f_last            = Output(logic)

    m_request         = Input(logic)
    m_read_not_write  = Input(logic)
    m_burst_len       = Input(Unsigned(2))
    m_byte_en         = Input(Unsigned(2))
    m_addr            = Input(BrewBusAddr)
    m_data_in         = Input(BrewBusData)
    m_response        = Output(logic)
    m_data_out        = Output(BrewBusData)
    m_last            = Output(logic)

    # DRAM interface
    DRAM_nRAS         = Output(logic)
    DRAM_nCAS_l       = Output(logic)
    DRAM_nCAS_h       = Output(logic)
    DRAM_ADDR         = Unsigned(12)
    DRAM_nWE          = Output(logic)
    DRAM_DATA_rd      = Input(BrewBusData)
    DRAM_DATA_wr      = Output(BrewBusData)

    # External bus-request
    ext_req           = Input(logic)
    ext_grnt          = Output(logic)

    def body(self):
        read_not_write  = Wire(logic)
        beats_remaining = Wire(Unsigned(2))
        byte_en         = Wire(Unsigned(2))
        page_addr       = Wire(Unsigned(23))
        page_offs       = Wire(Unsigned(8))
        data_in         = Wire(BrewBusData)
        response        = Wire(logic)
        data_out        = Wire(BrewBusData)
        last            = Wire(logic)

        # Arbitration: we always select fetch as higher priority
        self.arb_fsm = FSM()

        class ArbStates(Enum):
            idle = 0
            fetch = 1
            external = 2
            memory = 3

        self.arb_fsm.reset_value   <<= ArbStates.idle
        self.arb_fsm.default_state <<= ArbStates.idle

        arb_state = Wire()
        arb_next_state = Wire()
        arb_state <<= self.arb_fsm.state
        arb_next_state <<= self.arb_fsm.next_state

        # We're in a state where we don't have anything partial
        self.arb_fsm.add_transition(ArbStates.idle,  self.f_request & ~self.ext_req, ArbStates.fetch)
        self.arb_fsm.add_transition(ArbStates.idle,  ~self.f_request & self.m_request & ~self.ext_req, ArbStates.memory)
        self.arb_fsm.add_transition(ArbStates.idle,  ~self.f_request & ~self.m_request & self.ext_req, ArbStates.external)
        self.arb_fsm.add_transition(ArbStates.idle,  ~self.f_request & ~self.m_request & ~self.ext_req, ArbStates.idle)
        self.arb_fsm.add_transition(ArbStates.fetch, ~last, ArbStates.fetch)
        self.arb_fsm.add_transition(ArbStates.fetch, last, ArbStates.idle)
        self.arb_fsm.add_transition(ArbStates.memory, ~last, ArbStates.memory)
        self.arb_fsm.add_transition(ArbStates.memory, last, ArbStates.idle)
        self.arb_fsm.add_transition(ArbStates.external, self.ext_req, ArbStates.external)
        self.arb_fsm.add_transition(ArbStates.external, ~self.ext_req, ArbStates.idle)

        self.ext_grnt <<= arb_state == ArbStates.external

        m_response <<= arb_state == ArbStates.memory
        m_data_out <<= data_out
        m_last     <<= last

        f_response <<= arb_state == ArbStates.fetch
        f_data_out <<= data_out
        f_last     <<= last

        start = (arb_state == ArbStates.idle) & (self.f_request | self.m_request)
        advance = arb_state != ArbStates.idle

        read_not_write  <<= Reg(Select(start, read_not_write, Select(self.f_request, self.m_read_not_write, self.f_read_not_write)))
        page_addr       <<= Reg(Select(start, page_addr, Select(self.f_request, self.m_addr[31:8], self.f_addr[31:8])))
        page_offs       <<= Reg(Select(start, Select(advance, page_offs, page_offs + 1), Select(self.f_request, self.m_addr[7:0], self.f_addr[7:0])))
        beats_remaining <<= Reg(Select(start, Select(advance, beats_remaining, beats_remaining - 1), Select(self.f_request, self.m_burst_len, self.f_burst_len)))
        byte_en         <<= Reg(Select(start, 3, Select(self.f_request, self.m_byte_en, self.f_byte_en)))
        data_in         <<= Reg(Select(arb_next_state == ArbStates.fetch, self.m_data_in, self.f_data_in))

        last <<= beats_remaining == 0 & (arb_state != ArbStates.idle)

        '''
        ======== =========== ============
        Pin      First cycle Second cycle
        ======== =========== ============
        A8_0     A8          A0
        A9_1     A9          A1
        A10_2    A10         A2
        A11_3    A11         A3
        A12_4    A12         A4
        A13_5    A13         A5
        A14_6    A14         A6
        A15_7    A15         A7
        A17_16   A17         A16
        A19_18   A19         A18
        ======== =========== ============
        '''
        row_addr = {page_addr[11], page_addr[9], page_addr[7:0]}
        col_addr = {page_addr[10], page_addr[8], page_offs[7:0]}

        '''
        Single-access:

                               <--1-><--2-><--1-><--2->
            CLK             /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            DRAM_nRAS       ^^^^^^^^^\_____/^^^^^\_____/^^^^^^^^^^^^
            DRAM_nCASx      ^^^^^^^^^^^^\__/^^^^^^^^\__/^^^^^^^^^^^^
            DRAM_ADDR       ---------<==X==>-----<==X==>------------
            DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            DRAM_DATA_rd    --------------<>----------<>------------
            DRAM_nWE        ^^^^^^^^^^^^\__/^^^^^^^^\__/^^^^^^^^^^^^
            DRAM_DATA_wr    ------------<=====>-----<=====>---------

                               <--1-><--2-><--1-><--2->
            CLK             /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            arb_state       ....idle><ftch><idle><mem ><idle.....
            request         ___/^^^^^X*****X^^^^^X*****\_________
            beats_remaining ---------<  0  >-----<  0  >---------
            addr            ---------<  a  >-----<  a  >---------
            start           ___/^^^^^\_____/^^^^^\_______________
            x_response      _________/^^^^^\_____/^^^^^\_________
            x_last          _________/^^^^^\_____/^^^^^\_________
            x_data_in       ---<=====>-----<=====>---------------
            data_in         ---------<=====>-----<=====>---------
            data_out        ---------------<===========X=========

        2-beat burst:

                               <--1-><--2-><--3->
            CLK             /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            DRAM_nRAS       ^^^^^^^^^\___________/^^^^^^^^^^^^^^^
            DRAM_nCASx      ^^^^^^^^^^^^\__/^^\__/^^^^^^^^^^^^^^^
            DRAM_ADDR       ---------<==X==X**X==>---------------
            DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            DRAM_DATA_rd    --------------<>----<>---------------
            DRAM_nWE        ^^^^^^^^^^^^\__/**\__/^^^^^^^^^^^^^^^
            DRAM_DATA_wr    ------------<=====X=====>------------

                               <--1-><--2-><--3->
            CLK             /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            arb_state       ....idle><   ftch    ><idle...........
            request         ___/^^^^^X***********\_______________
            beats_remaining ---------<  1  X  0  >---------------
            addr            ---------<  a  X a+1 >---------------
            start           ___/^^^^^\___________________________
            x_response      _________/^^^^^^^^^^^\_______________
            x_last          _______________/^^^^^\_______________
            x_data_in       ---<=====X=====>---------------------
            data_in         ---------<=====X=====>---------------
            data_out        ---------------<=====X=====>---------

        4-beat burst:

                               <--1-><--2-><--3-><--4-><--5->
            CLK             /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            DRAM_nRAS       ^^^^^^^^^\_______________________/^^^^^^^^^
            DRAM_nCASx      ^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^^
            DRAM_ADDR       ---------<==X==X**X==X**X==X**X==>---------
            DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            DRAM_DATA_rd    --------------<>----<>----<>----<>---------
            DRAM_nWE        ^^^^^^^^^^^^\__/**\__/**\__/**\__/^^^^^^^^^
            DRAM_DATA_wr    ------------<=====X=====X=====X=====>------

                               <--1-><--2-><--3-><--4-><--5->
            CLK             /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            arb_state       ....idle><         ftch          ><idle....
            request         ___/^^^^^X***********************\_________
            beats_remaining ---------<  3  X  2  X  1  X  0  >---------
            addr            ---------<  a  X a+1 X a+2 X a+3 >---------
            start           ___/^^^^^\_________________________________
            x_response      _________/^^^^^^^^^^^^^^^^^^^^^^^\_________
            x_last          ___________________________/^^^^^\_________
            x_data_in       ---<=====X=====X=====X=====>---------------
            data_in         ---------<=====X=====X=====X=====>---------
            data_out        ---------------<=====X=====X=====X=====>---
        '''

        # This thing (I think) is theoretically glitch-free, but I'm not sure if an FPGA LUT implementation guarantees it.
        self.DRAM_nCAS_h <<= byte_en[1] & (arb_state != ArbStates.idle) & ~self.clk
        self.DRAM_nCAS_l <<= byte_en[0] & (arb_state != ArbStates.idle) & ~self.clk
        self.DRAM_nRAS <<= arb_state == ArbStates.idle
        self.DRAM_nWE <<= read_not_write

        self.DRAM_ADDR <<= Select(self.clk, row_addr, col_addr)
        self.DRAM_DATA_wr <<= Reg(data_in, clk=~self.clk) # This needs a half-cycle delay, so use the negated clock
        data_out <<= Reg(self.DRAM_DATA_rd)



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

