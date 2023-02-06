#!/usr/bin/python3
from random import *
from typing import *
from silicon import *
try:
    from .brew_types import *
    from .brew_utils import *
except ImportError:
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

"""
TODO:

- Should we have double-pumped data-bus?
- Should we delay 'response' and 'last' by one cycle for reads to line up with the data? - this will come into the fore as we firm up 'fetch' and 'memory'
"""
class BusIf(Module):
    clk = ClkPort()
    rst = RstPort()

    # Interface to fetch and memory
    fetch = Input(BusIfPortIf)
    mem = Input(BusIfPortIf)

    # DRAM interface
    DRAM_nRAS         = Output(logic)
    DRAM_nCAS_l       = Output(logic)
    DRAM_nCAS_h       = Output(logic)
    DRAM_ADDR         = Output(Unsigned(12))
    DRAM_nWE          = Output(logic)
    DRAM_DATA_rd_h    = Input(BrewByte)
    DRAM_DATA_rd_l    = Input(BrewByte)
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
        self.arb_fsm.add_transition(ArbStates.idle,   self.fetch.request &                     ~self.ext_req, ArbStates.fetch)
        self.arb_fsm.add_transition(ArbStates.idle,  ~self.fetch.request &  self.mem.request & ~self.ext_req, ArbStates.memory)
        self.arb_fsm.add_transition(ArbStates.idle,  ~self.fetch.request & ~self.mem.request &  self.ext_req, ArbStates.external)
        self.arb_fsm.add_transition(ArbStates.idle,  ~self.fetch.request & ~self.mem.request & ~self.ext_req, ArbStates.idle)
        self.arb_fsm.add_transition(ArbStates.fetch, ~last, ArbStates.fetch)
        self.arb_fsm.add_transition(ArbStates.fetch,  last, ArbStates.idle)
        self.arb_fsm.add_transition(ArbStates.memory, ~last, ArbStates.memory)
        self.arb_fsm.add_transition(ArbStates.memory,  last, ArbStates.idle)
        self.arb_fsm.add_transition(ArbStates.external,  self.ext_req, ArbStates.external)
        self.arb_fsm.add_transition(ArbStates.external, ~self.ext_req, ArbStates.idle)

        self.wait_states = Wire(Unsigned(4))

        self.ext_grnt <<= arb_state == ArbStates.external

        self.mem.response <<= arb_state == ArbStates.memory & (self.wait_states == 0)
        self.mem.data_out <<= data_out
        self.mem.last     <<= last

        self.fetch.response <<= arb_state == ArbStates.fetch & (self.wait_states == 0)
        self.fetch.data_out <<= data_out
        self.fetch.last     <<= last

        start = (arb_state == ArbStates.idle) & (self.fetch.request | self.mem.request)
        advance = arb_state != ArbStates.idle

        self.wait_states <<= Reg(
            Select(
                start,
                Select(
                    self.wait_states == 0,
                    (self.wait_states - 1)[3:0],
                    0
                ),
                (Select(self.fetch.request, self.mem.addr[25:22], self.fetch.addr[25:22]) - 1)[3:0]
            )
        )

        read_not_write  <<= Reg(Select(start, read_not_write, Select(self.fetch.request, self.mem.read_not_write, self.fetch.read_not_write)))
        page_addr       <<= Reg(Select(start, page_addr, Select(self.fetch.request, self.mem.addr[30:8], self.fetch.addr[30:8])))
        page_offs       <<= Reg(Select(start, Select(advance, page_offs, (page_offs + 1)[7:0]), Select(self.fetch.request, self.mem.addr[7:0], self.fetch.addr[7:0])))
        beats_remaining <<= Reg(Select(start, Select(advance, beats_remaining, (beats_remaining - Select(self.wait_states == 0, 0, 1))[1:0]), Select(self.fetch.request, self.mem.burst_len, self.fetch.burst_len)))
        byte_en         <<= Reg(Select(start, Select(self.wait_states == 0, byte_en, 3), Select(self.fetch.request, self.mem.byte_en, self.fetch.byte_en)))
        data_in         <<= Reg(Select(arb_next_state == ArbStates.fetch, self.mem.data_in, self.fetch.data_in))

        last <<= (beats_remaining == 0) & (arb_state != ArbStates.idle)

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
        row_addr = concat(page_addr[11], page_addr[9], page_addr[7:0])
        col_addr = concat(page_addr[10], page_addr[8], page_offs[7:0])

        '''
        Single-access:

                               <--1-><--2-><--1-><--2->
            CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/
            DRAM_nRAS       ^^^^^^^^^\_____/^^^^^\_____/^^^^^^^^^^^^
            DRAM_nCASx      ^^^^^^^^^^^^\__/^^^^^^^^\__/^^^^^^^^^^^^
            DRAM_ADDR       ---------<==X==>-----<==X==>------------
            DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            DRAM_DATA_rd    --------------<>----------<>------------
            DRAM_nWE        ^^^^^^^^^^^^\__/^^^^^^^^\__/^^^^^^^^^^^^
            DRAM_DATA_wr    ------------<=====>-----<=====>---------

                               <--1-><--2-><--1-><--2->
            CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/
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
            CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/
            DRAM_nRAS       ^^^^^^^^^\___________/^^^^^^^^^^^^^^^
            DRAM_nCASx      ^^^^^^^^^^^^\__/^^\__/^^^^^^^^^^^^^^^
            DRAM_ADDR       ---------<==X==X**X==>---------------
            DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            DRAM_DATA_rd    --------------<>----<>---------------
            DRAM_nWE        ^^^^^^^^^^^^\__/**\__/^^^^^^^^^^^^^^^
            DRAM_DATA_wr    ------------<=====X=====>------------

                               <--1-><--2-><--3->
            CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/
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
            CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            DRAM_nRAS       ^^^^^^^^^\_______________________/^^^^^^^^^
            DRAM_nCASx      ^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^^
            DRAM_ADDR       ---------<==X==X**X==X**X==X**X==>---------
            DRAM_nWE        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            DRAM_DATA_rd    --------------<>----<>----<>----<>---------
            DRAM_nWE        ^^^^^^^^^^^^\__/**\__/**\__/**\__/^^^^^^^^^
            DRAM_DATA_wr    ------------<=====X=====X=====X=====>------

                               <--1-><--2-><--3-><--4-><--5->
            CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
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

        '''
        CAS generation:

            CLK             \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\
            ~CLK            /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            DRAM_nRAS       ^^^^^^^^^\_______________________/^^^^^^^^^
            DRAM_nRASd      ^^^^^^^^^^^^\_______________________/^^^^^^
            CAS_nEN         ^^^^^^^^^^^^\____________________/^^^^^^^^^
            DRAM_nCASx      ^^^^^^^^^^^^\__/^^\__/^^\__/^^\__/^^^^^^^^^

        We need to avoid changing the enable signal on opposite edges of the clock.
        That is, CAS_nEN falls with ~CLK falling and rises with ~CLK rising.

        This way timing is not that critical, provided the LUT is just as glitch-free
        as logic gates would be. That's actually up to debate. Apparently Xilinx only
        guarantees glitch-free output for single input toggling, but in practice it
        appears to be true that the output doesn't glitch if normal logic wouldn't.

        From what I've gathered, the glitch-free nature of the output comes from
        depending on the output capacitance of the read-wire and careful timing of
        the switching of the pass-gates that make up the LUT read mux. So, fingers
        crossed, this is a safe circuit...
        '''

        self.DRAM_nRAS <<= Reg(arb_next_state == ArbStates.idle, reset_value_port = 1) # We re-register the state to remove all glitches
        self.DRAM_nRASd = Wire(logic)
        self.DRAM_nRASd <<= NegReg(self.DRAM_nRAS, reset_value_port = 1)
        self.CAS_nEN = Wire(logic)
        self.CAS_nEN <<= self.DRAM_nRAS | self.DRAM_nRASd
        self.DRAM_nCAS_h <<= ~byte_en[1] | self.CAS_nEN | self.clk
        self.DRAM_nCAS_l <<= ~byte_en[0] | self.CAS_nEN | self.clk
        self.DRAM_nWE <<= read_not_write

        self.DRAM_ADDR <<= Select(self.clk, col_addr, row_addr)
        self.DRAM_DATA_wr <<= NegReg(data_in) # This needs a half-cycle delay, so use the negated clock
        data_out <<= Reg(concat(self.DRAM_DATA_rd_h, self.DRAM_DATA_rd_l))

def sim():
    inst_stream = []


    class DRAM_sim(Module):
        addr_bus_len = 12
        addr_bus_mask = (1 << addr_bus_len) - 1

        DRAM_nRAS         = Input(logic)
        DRAM_nCAS_l       = Input(logic)
        DRAM_nCAS_h       = Input(logic)
        DRAM_ADDR         = Input(Unsigned(addr_bus_len))
        DRAM_nWE          = Input(logic)
        DRAM_DATA_rd_h    = Output(BrewByte)
        DRAM_DATA_rd_l    = Output(BrewByte)
        DRAM_DATA_wr      = Input(BrewBusData)

        def simulate(self) -> TSimEvent:
            full_addr = 0
            self.DRAM_DATA_rd_l <<= None
            self.DRAM_DATA_rd_h <<= None
            while True:
                when = yield (self.DRAM_nRAS, self.DRAM_nCAS_l, self.DRAM_nCAS_h)

                if self.DRAM_nRAS.get_sim_edge() == EdgeType.Negative:
                    #assert self.DRAM_nCAS_l.get_sim_edge() == EdgeType.NoEdge
                    #assert self.DRAM_nCAS_h.get_sim_edge() == EdgeType.NoEdge
                    #assert self.DRAM_nCAS_l == 1
                    #assert self.DRAM_nCAS_h == 1
                    # Falling edge or nRAS: capture row address
                    full_addr = full_addr & self.addr_bus_mask | (self.DRAM_ADDR << self.addr_bus_len)
                else:
                    fall_l = self.DRAM_nCAS_l.get_sim_edge() == EdgeType.Negative
                    fall_h = self.DRAM_nCAS_h.get_sim_edge() == EdgeType.Negative
                    if fall_l or fall_h:
                        #assert self.DRAM_nRAS.get_sim_edge() == EdgeType.NoEdge
                        #assert self.DRAM_nRAS == 0
                        # Falling edge of nCAS
                        full_addr = full_addr & (self.addr_bus_mask << self.addr_bus_len) | self.DRAM_ADDR
                        if self.DRAM_nWE == 0:
                            # Write to the address
                            data_h = f"{self.DRAM_DATA_wr[15:8]:x}" if self.DRAM_nCAS_h == 0 else "--"
                            data_l = f"{self.DRAM_DATA_wr[ 7:0]:x}" if self.DRAM_nCAS_l == 0 else "--"
                            print(f"Writing to address {full_addr:x} {data_h}{data_l}")
                        else:
                            data_h = (full_addr >> 8) & 0xff if self.DRAM_nCAS_h == 0 else 0
                            data_l = (full_addr >> 0) & 0xff if self.DRAM_nCAS_l == 0 else 0
                            data_h_str = f"{data_h:x}" if self.DRAM_nCAS_h == 0 else "--"
                            data_l_str = f"{data_l:x}" if self.DRAM_nCAS_l == 0 else "--"
                            print(f"Reading from address {full_addr:x} {data_h_str}{data_l_str} at {when} - {fall_h} {fall_l}")
                            print(f"    L: {self.DRAM_nCAS_l.previous_sim_value} {self.DRAM_nCAS_l.sim_value} {self.DRAM_nCAS_l.last_changed}")
                            print(f"    H: {self.DRAM_nCAS_h.previous_sim_value} {self.DRAM_nCAS_h.sim_value} {self.DRAM_nCAS_h.last_changed}")
                            print(f"    R: {self.DRAM_nRAS.previous_sim_value} {self.DRAM_nRAS.sim_value} {self.DRAM_nRAS.last_changed}")
                            self.DRAM_DATA_rd_h <<= data_h
                            self.DRAM_DATA_rd_l <<= data_l
                    if self.DRAM_nCAS_l != 0:
                        self.DRAM_DATA_rd_l <<= None
                    if self.DRAM_nCAS_h != 0:
                        self.DRAM_DATA_rd_h <<= None

    # These two queues will contain the expected read-back values
    read_data_l = []
    read_data_h = []
    class Generator(Module):
        clk = ClkPort()
        rst = RstPort()

        request_port = Output(BusIfPortIf)

        def construct(self) -> None:
            self.mode = None

        def set_mode(self, mode):
            self.mode = mode

        def simulate(self) -> TSimEvent:
            def reset():
                self.request_port.request <<= 0
                self.request_port.read_not_write <<= None
                self.request_port.burst_len <<= None
                self.request_port.byte_en <<= None
                self.request_port.addr <<= None
                self.request_port.data_in <<= None

            def start_read(addr, burst_len, byte_en):
                self.request_port.request <<= 1
                self.request_port.read_not_write <<= 1
                self.request_port.burst_len <<= burst_len
                self.request_port.byte_en <<= byte_en
                self.request_port.addr <<= addr
                self.request_port.data_in <<= None

            def start_write(addr, burst_len, byte_en, data):
                self.request_port.request <<= 1
                self.request_port.read_not_write <<= 0
                self.request_port.burst_len <<= burst_len
                self.request_port.byte_en <<= byte_en
                self.request_port.addr <<= addr
                self.request_port.data_in <<= data

            def continue_write(data):
                self.request_port.request <<= None
                self.request_port.read_not_write <<= None
                self.request_port.burst_len <<= None
                self.request_port.byte_en <<= None
                self.request_port.addr <<= None
                self.request_port.data_in <<= data

            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            def write(addr, burst_len, byte_en, data):
                idx = 0
                start_write(addr, burst_len, byte_en, data[idx])
                while idx < burst_len:
                    yield from wait_clk()
                    idx += 1
                    continue_write(data[idx])
                yield from wait_clk()
                reset()

            def read(addr, burst_len, byte_en):
                idx = 0
                start_read(addr, burst_len, byte_en)
                while idx < burst_len:
                    yield from wait_clk()
                    idx += 1
                yield from wait_clk()
                reset()

            reset()
            if self.mode == "fetch":
                yield from wait_clk()
                while self.rst == 1:
                    yield from wait_clk()
                yield from read(0xe,0,3)
                yield from wait_clk()
                yield from read(0x12,1,3)
                yield from wait_clk()
                yield from read(0x24,3,3)
                yield from wait_clk()
                yield from read(0x3,0,1)
                yield from wait_clk()
                yield from read(0x4,0,2)
            elif self.mode == "mem":
                pass


    '''
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
    '''

    class top(Module):
        clk = ClkPort()
        rst = RstPort()

        def body(self):
            seed(0)
            self.fetch_if = Wire(BusIfPortIf)
            self.fetch_generator = Generator()
            self.fetch_generator.set_mode("fetch")
            self.fetch_if <<= self.fetch_generator.request_port

            self.mem_if = Wire(BusIfPortIf)
            self.mem_generator = Generator()
            self.mem_generator.set_mode("mem")
            self.mem_if <<= self.mem_generator.request_port

            self.dram_sim = DRAM_sim()

            DRAM_nRAS         = Wire(logic)
            DRAM_nCAS_l       = Wire(logic)
            DRAM_nCAS_h       = Wire(logic)
            DRAM_ADDR         = Wire(Unsigned(12))
            DRAM_nWE          = Wire(logic)
            DRAM_DATA_rd_h    = Wire(BrewByte)
            DRAM_DATA_rd_l    = Wire(BrewByte)
            DRAM_DATA_wr      = Wire(BrewBusData)


            dut = BusIf()
            dut.rst <<= self.rst
            dut.clk <<= self.clk
            dut.fetch <<= self.fetch_if
            dut.mem <<= self.mem_if


            DRAM_nRAS <<= dut.DRAM_nRAS
            DRAM_nCAS_l <<= dut.DRAM_nCAS_l
            DRAM_nCAS_h <<= dut.DRAM_nCAS_h
            DRAM_ADDR <<= dut.DRAM_ADDR
            DRAM_nWE <<= dut.DRAM_nWE
            dut.DRAM_DATA_rd_h <<= DRAM_DATA_rd_h
            dut.DRAM_DATA_rd_l <<= DRAM_DATA_rd_l
            DRAM_DATA_wr <<= dut.DRAM_DATA_wr

            self.dram_sim.DRAM_nRAS <<= DRAM_nRAS
            self.dram_sim.DRAM_nCAS_l <<= DRAM_nCAS_l
            self.dram_sim.DRAM_nCAS_h <<= DRAM_nCAS_h
            self.dram_sim.DRAM_ADDR <<= DRAM_ADDR
            self.dram_sim.DRAM_nWE <<= DRAM_nWE
            DRAM_DATA_rd_h <<= self.dram_sim.DRAM_DATA_rd_h
            DRAM_DATA_rd_l <<= self.dram_sim.DRAM_DATA_rd_l
            self.dram_sim.DRAM_DATA_wr <<= DRAM_DATA_wr

            dut.ext_req <<= 0

            dut.wait_states_0 <<= 1
            dut.wait_states_1 <<= 1
            dut.wait_states_2 <<= 1
            dut.wait_states_3 <<= 1


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

    Build.simulation(top, "bus_if.vcd", add_unnamed_scopes=True)


def gen():
    Build.generate_rtl(BusIf)

if __name__ == "__main__":
    #gen()
    sim()

