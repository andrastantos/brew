#!/usr/bin/python3
from random import *
from typing import *
try:
    from silicon import *
    from silicon.memory import SimpleDualPortMemory
except ImportError:
    import sys
    from pathlib import Path
    sys.path.append(str((Path() / ".." / ".." / ".." / "silicon").absolute()))
    from silicon import *
    from silicon.memory import SimpleDualPortMemory
try:
    from .brew_types import *
    from .brew_utils import *
except ImportError:
    from brew_types import *
    from brew_utils import *

"""
The register file for Brew consists of a single write and two read ports.

The V1 version doesn't implement types, so only values are provided.

For FPGAs, BRAMs can be used to implement the register file.

The register file also implements the score-board for the rest of the pipeline to handle reservations
"""

# TODO: should we have a request-response interface on top of ready-valid?
#       The logic is the same, except that response provides data on the same
#       cycle that request is accepted.
#       For now, I'm going to keep it as individual wires to experiment some more.
class RegFile(Module):
    clk = ClkPort()
    rst = RstPort()

    # Interface towards decode

    # This is important! We have several sub-interfaces, but one global request/response pair
    # That is because we want to handle all sub-requests as a single transaction.

    request = Input(logic)
    response = Output(logic)

    read1_addr = Input(BrewRegAddr)
    read1_data = Output(BrewData)
    read1_valid = Input(logic)

    read2_addr = Input(BrewRegAddr)
    read2_data = Output(BrewData)
    read2_valid = Input(logic)

    rsv_addr = Input(BrewRegAddr)
    rsv_valid = Input(logic)

    # Interface towards the write-back of the pipeline
    write_data = Input(BrewData)
    write_addr = Input(BrewRegAddr)
    write_request = Input(logic)

    '''
    CLK            /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__
    write_request  ______/^^^^^\_______________________/^^^^^\_________________________________________
    write_addr     ------<=====>-----------------------<=====>-----------------------------------------
    write_data     ------------<=====>-----------------------<=====>-----------------------------------
    CLK            /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__
    score_clr      ______/^^^^^\_______________________/^^^^^\_________________________________________
    score_set
    score_value    ^^^^^^^^^^^^\_____________ ^^^^^^^^^^^^^^^\_________________________________________
    CLK            /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__
    request        ______________________________/^^^^^^^^^^^\_________________________________________
    read_valid     ______________________________/^^^^^^^^^^^\_________________________________________
    read_addr      ------------------------------<===========>-----------------------------------------
    read_data      ------------------------------------------<=====>-----------------------------------
    response       ____________________________________/^^^^^\__________________________________________

    '''


    def body(self):
        # We have two memory instances, one for each read port. The write ports of
        # these instances are connected together so they get written the same data
        mem1 = SimpleDualPortMemory(addr_type=BrewRegAddr, data_type=BrewData)
        mem2 = SimpleDualPortMemory(addr_type=BrewRegAddr, data_type=BrewData)

        write_request_d = Wire()
        write_request_d <<= Reg(self.write_request)
        write_addr_d = Wire()
        write_addr_d <<= Reg(self.write_request)
        mem1.port1_write_en <<= write_addr_d
        mem1.port1_data_in <<= self.write_data
        mem1.port1_addr <<= Reg(self.write_addr)
        mem2.port1_write_en <<= write_addr_d
        mem2.port1_data_in <<= self.write_data
        mem2.port1_addr <<= Reg(self.write_addr)

        read1_addr_d = Wire()
        read2_addr_d = Wire()

        read1_addr_d <<= Reg(self.read1_addr)
        read2_addr_d <<= Reg(self.read2_addr)

        # Read ports have bypass logic
        self.read1_data <<= Select(
            (self.write_addr == read1_addr_d) & write_request_d,
            mem1.port2_data_out,
            self.write_data
        )
        mem1.port2_addr <<= self.read1_addr

        self.read2_data <<= Select(
            (self.write_addr == read2_addr_d) & write_request_d,
            mem2.port2_data_out,
            self.write_data
        )
        mem2.port2_addr <<= self.read2_addr

        # Score-board for reservations
        rsv_board = Wire(Unsigned(BrewRegCnt))

        clear_mask = Select(self.write_request, 0, 1 << self.write_addr)

        rsv_response = Wire(logic)

        decode_response = Wire(logic)

        set_mask = Select(self.rsv_valid & decode_response & self.request, 0, 1 << self.rsv_addr)

        rsv_board <<= Reg(rsv_board & ~clear_mask | set_mask)

        # The logic is this: if we don't have a valid request, we always are providing a response.
        read1_response = Wire()
        read2_response = Wire()
        read1_response <<= self.request & (~self.read1_valid | (((rsv_board & (1 << self.read1_addr)) == 0) | ((self.write_addr == self.read1_addr) & self.write_request)))
        read2_response <<= self.request & (~self.read2_valid | (((rsv_board & (1 << self.read2_addr)) == 0) | ((self.write_addr == self.read2_addr) & self.write_request)))
        rsv_response   <<= self.request & (~self.rsv_valid | (((rsv_board & (1 << self.rsv_addr)) == 0) | ((self.write_addr == self.rsv_addr) & self.write_request)))

        decode_response <<= read1_response & read2_response & rsv_response

        self.response <<= decode_response





def sim():

    class Excerciser(Module):
        clk = ClkPort()
        rst = RstPort()

        request = Output(logic)
        response = Input(logic)

        read1_addr = Output(BrewRegAddr)
        read1_data = Input(BrewData)
        read1_valid = Output(logic)

        read2_addr = Output(BrewRegAddr)
        read2_data = Input(BrewData)
        read2_valid = Output(logic)

        rsv_addr = Output(BrewRegAddr)
        rsv_valid = Output(logic)

        # Interface towards the write-back of the pipeline
        write_data = Output(BrewData)
        write_addr = Output(BrewRegAddr)
        write_request = Output(logic)

        def simulate(self) -> TSimEvent:

            self.sim_write_state = "idle"
            self.sim_write_data = None
            def write_reg(addr, data):
                self.sim_write_data = data
                self.sim_write_state = "pre_write"
                self.write_addr <<= addr
                self.write_request <<= 1

            def write_sm():
                if self.rst == 1:
                    self.write_request <<= 0
                    self.write_data <<= None
                    self.write_addr <<= None
                    self.sim_write_state = "idle"
                elif self.sim_write_state == "idle":
                    pass
                elif self.sim_write_state == "pre_write":
                    self.write_request <<= 0
                    self.write_data <<= self.sim_write_data
                    self.sim_write_state = "write"
                elif self.sim_write_state == "write":
                    self.write_request <<= 0
                    self.write_data <<= None
                    self.write_addr <<= None
                    self.sim_write_state = "idle"

            self.sim_req_state = "idle"

            def start_req(read1, read2, rsv):
                assert self.sim_req_state == "idle"
                self.read1_addr <<= read1
                self.read1_valid <<= read1 is not None
                self.read2_addr <<= read2
                self.read2_valid <<= read2 is not None
                self.rsv_addr <<= rsv
                self.rsv_valid <<= rsv is not None
                self.request <<= 1
                self.sim_req_state = "request"

            def req_sm():
                if self.rst == 1:
                    self.read1_addr <<= None
                    self.read1_valid <<= None
                    self.read2_addr <<= None
                    self.read2_valid <<= None
                    self.rsv_addr <<= None
                    self.rsv_valid <<= None
                    self.request <<= 0
                    self.sim_req_state = "idle"
                elif self.sim_req_state == "idle":
                    pass
                elif self.sim_req_state == "request":
                    if self.response == 1:
                        self.read1_addr <<= None
                        self.read1_valid <<= None
                        self.read2_addr <<= None
                        self.read2_valid <<= None
                        self.rsv_addr <<= None
                        self.rsv_valid <<= None
                        self.request <<= 0
                        self.sim_req_state = "idle"

            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )
                    req_sm()
                    write_sm()


            def wait_rst():
                yield from wait_clk()
                while self.rst == 1:
                    yield from wait_clk()

            self.request <<= 0
            self.write_request <<= 0
            yield from wait_rst()
            for i in range(4):
                yield from wait_clk()

            write_reg(0,100)
            yield from wait_clk()
            yield from wait_clk()
            write_reg(1,101)
            yield from wait_clk()
            yield from wait_clk()
            write_reg(2,102)
            yield from wait_clk()
            write_reg(3,103)
            yield from wait_clk()
            yield from wait_clk()
            yield from wait_clk()
            yield from wait_clk()
            start_req(1,2,3)
            yield from wait_clk()
            yield from wait_clk()
            yield from wait_clk()
            start_req(3,0,None)
            yield from wait_clk()
            yield from wait_clk()
            yield from wait_clk()
            yield from wait_clk()
            yield from wait_clk()
            write_reg(3,203)
            yield from wait_clk()
            start_req(1,2,3)
            yield from wait_clk()
            yield from wait_clk()
            yield from wait_clk()



    class top(Module):
        clk = ClkPort()
        rst = RstPort()

        def body(self):
            seed(0)

            self.excericeser = Excerciser()
            self.dut = RegFile()


            self.dut.request <<= self.excericeser.request
            self.excericeser.response <<= self.dut.response

            self.dut.read1_addr <<= self.excericeser.read1_addr
            self.excericeser.read1_data <<= self.dut.read1_data
            self.dut.read1_valid <<= self.excericeser.read1_valid

            self.dut.read2_addr <<= self.excericeser.read2_addr
            self.excericeser.read2_data <<= self.dut.read2_data
            self.dut.read2_valid <<= self.excericeser.read2_valid

            self.dut.rsv_addr <<= self.excericeser.rsv_addr
            self.dut.rsv_valid <<= self.excericeser.rsv_valid

            self.dut.write_data <<= self.excericeser.write_data
            self.dut.write_addr <<= self.excericeser.write_addr
            self.dut.write_request <<= self.excericeser.write_request


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

    Build.simulation(top, "reg_file.vcd", add_unnamed_scopes=True)

def gen():
    Build.generate_rtl(RegFile)

#gen()
sim()
