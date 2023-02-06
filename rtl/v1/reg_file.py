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

class RegFile(Module):
    clk = ClkPort()
    rst = RstPort()

    # Interface towards decode
    read_req = Input(RegFileReadRequestIf)
    read_rsp = Output(RegFileReadResponseIf)

    # Interface towards the write-back of the pipeline
    write = Input(RegFileWriteBackIf)

    '''
    CLK                    /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__
    write.valid            ______/^^^^^\_______________________/^^^^^\_______________________________________________/^^^^^\___________
    write.addr             ------<=====>-----------------------<=====>-----------------------------------------------<=====>-----------
    write.data             ------<=====>-----------------------<=====>-----------------------------------------------<=====>-----------
    CLK                    /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__
    score_clr              ______/^^^^^\_______________________/^^^^^\_______________________________________________/^^^^^\___________
    score_set              ______/^^^^^\_______________________/^^^^^\___________/^^^^^\_____/^^^^^^^^^^^\_____/^^^^^^^^^^^\___________
    score_value            ^^^^^^^^^^^^\_____________ ^^^^^^^^^^^^^^^\______________________________________________ ^^^^^^\___________
    CLK                    /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__
    read_req.valid         ______/^^^^^\_________________/^^^^^\_________________/^^^^^\_____/^^^^^^^^^^^\_____/^^^^^^^^^^^\___________
    read_req.ready         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\_____/^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    read_req.readX_valid   ______/^^^^^\_________________/^^^^^\_________________/^^^^^\_____/^^^^^^^^^^^\_____/^^^^^^^^^^^\___________
    read_req.readX_addr    ------<=====>-----------------<=====>-----------------<=====>-----<=====X=====>-----<=====X=====>-----------
    read_rsp.valid         ____________/^^^^^\_______________________/^^^^^\___________/^^^^^\_____/^^^^^^^^^^^\_____/^^^^^^^^^^^\_____
    read_rsp.readX_data    ------------<=====>-----------------------<=====>-----------<=====>-----<=====X=====>-----<=====X=====>-----
                                 bypass behavior           bypass behavior       simple read   back-to-back read  back-to-back read w bypass
    '''


    read1_rsv_bit = Output(logic)
    read2_rsv_bit = Output(logic)
    rsv_rsv_bit = Output(logic)

    def body(self):
        req_advance = self.read_req.ready & self.read_req.valid
        rsp_advance = self.read_rsp.ready & self.read_rsp.valid

        def remember(thing):
            return Select(req_advance, Reg(thing, clock_en=req_advance), thing)

        read1_valid = remember(self.read_req.read1_valid)
        read1_addr  = remember(self.read_req.read1_addr)
        read2_valid = remember(self.read_req.read2_valid)
        read2_addr  = remember(self.read_req.read2_addr)
        rsv_valid   = remember(self.read_req.rsv_valid)
        rsv_addr    = remember(self.read_req.rsv_addr)

        # We have two memory instances, one for each read port. The write ports of
        # these instances are connected together so they get written the same data
        mem1 = SimpleDualPortMemory(registered_input_b=False, registered_output_b=True, addr_type=BrewRegAddr, data_type=BrewData)
        mem2 = SimpleDualPortMemory(registered_input_b=False, registered_output_b=True, addr_type=BrewRegAddr, data_type=BrewData)

        # We disable forwarding and wirting to the RF if write.data_en is not asserted.
        # This allows for clearing a reservation without touching the data
        # during (branch/exception recovery).
        mem1.port1_write_en <<= self.write.valid & self.write.data_en
        mem1.port1_data_in <<= self.write.data
        mem1.port1_addr <<= self.write.addr
        mem2.port1_write_en <<= self.write.valid & self.write.data_en
        mem2.port1_data_in <<= self.write.data
        mem2.port1_addr <<= self.write.addr

        write_data_d = Wire()
        write_data_d <<= Reg(self.write.data)

        # Read ports have bypass logic, but with the same latency as normal register reads
        # NOTE: Since we are configuring the underlying memories as 'read new data', this
        #       bypass logic is not necessary. I'll leave it here though since some targets
        #       might not support such an arrangement. Quartus for instance complains, but
        #       complies.
        mem1.port2_addr <<= read1_addr
        #self.read_rsp.read1_data <<= Select(
        #    Reg((self.write.addr == read1_addr) & self.write.valid & self.write.data_en),
        #    mem1.port2_data_out,
        #    write_data_d
        #)
        #
        mem2.port2_addr <<= read2_addr
        #self.read_rsp.read2_data <<= Select(
        #    Reg((self.write.addr == read2_addr) & self.write.valid & self.write.data_en),
        #    mem2.port2_data_out,
        #    write_data_d
        #)
        self.read_rsp.read1_data <<= mem1.port2_data_out
        self.read_rsp.read2_data <<= mem2.port2_data_out

        # Score-board for reservations
        rsv_board = Wire(Unsigned(BrewRegCnt))

        def get_rsv_bit(addr):
            return Select(addr, *rsv_board)

        def wait(read_valid, read_addr):
            return read_valid & get_rsv_bit(read_addr) & ~((self.write.addr == read_addr) & self.write.valid)

        rsv_board_as_bits = tuple(rsv_board)
        self.read1_rsv_bit <<= Select(read1_addr, *rsv_board_as_bits)
        self.read2_rsv_bit <<= Select(read2_addr, *rsv_board_as_bits)
        self.rsv_rsv_bit   <<= Select(rsv_addr,   *rsv_board_as_bits)

        wait_for_read1 = wait(read1_valid, read1_addr)
        wait_for_read2 = wait(read2_valid, read2_addr)
        wait_for_rsv   = wait(rsv_valid,   rsv_addr)
        wait_for_some = wait_for_read1 | wait_for_read2 | wait_for_rsv
        wait_for_write = Wire(logic)
        wait_for_write = Select(
            req_advance | self.write.valid,
            Reg(wait_for_some, clock_en = req_advance | self.write.valid),
            wait_for_some
        )


        # Setting and clearing reservation bits (if we set and clear at the same cycle, set takes priority)
        rsv_set_valid = rsv_valid & ~wait_for_write
        rsv_clr_valid = self.write.valid & ~wait_for_rsv
        for i in range(BrewRegCnt):
            rsv_board[i] <<= Reg(
                Select(
                    rsv_set_valid & (rsv_addr == i),
                    Select(
                        rsv_clr_valid & (self.write.addr == i),
                        rsv_board[i],
                        0
                    ),
                    1
                )
            )


        outstanding_req = Wire(logic)
        outstanding_req <<= Reg(Select(
            req_advance,
            Select(
                rsp_advance,
                outstanding_req,
                0
            ),
            1
        ))

        wait_for_write_d = Reg(wait_for_write)
        out_buf_full = Wire(logic)
        out_buf_full <<= Reg(Select(req_advance, Select(rsp_advance, out_buf_full, 0), 1))

        self.read_req.ready <<= ~wait_for_write_d & (self.read_rsp.ready | ~out_buf_full)
        self.read_rsp.valid <<= ~wait_for_write_d & out_buf_full






def sim():

    class Excerciser(Module):
        clk = ClkPort()
        rst = RstPort()

        # Interface towards decode
        read_req = Output(RegFileReadRequestIf)
        read_rsp = Input(RegFileReadResponseIf)

        # Interface towards the write-back of the pipeline
        write = Output(RegFileWriteBackIf)

        def simulate(self) -> TSimEvent:

            self.sim_write_state = "idle"

            def write_reg(addr, data, enable = 1):
                self.sim_write_state = "write"
                self.write.valid <<= 1
                self.write.addr <<= addr
                self.write.data <<= data
                self.write.data_en <<= enable

            def write_sm():
                if self.rst == 1:
                    self.write.valid <<= 0
                    self.write.data <<= None
                    self.write.data_en <<= None
                    self.write.addr <<= None
                    self.sim_write_state = "idle"
                elif self.sim_write_state == "idle":
                    pass
                elif self.sim_write_state == "write":
                    self.write.valid <<= 0
                    self.sim_write_state = "idle"

            self.sim_req_state = "idle"

            def start_req(read1, read2, rsv):
                assert self.sim_req_state == "idle"
                self.read_req.read1_addr <<= read1
                self.read_req.read1_valid <<= read1 is not None
                self.read_req.read2_addr <<= read2
                self.read_req.read2_valid <<= read2 is not None
                self.read_req.rsv_addr <<= rsv
                self.read_req.rsv_valid <<= rsv is not None
                self.read_req.valid <<= 1
                self.sim_req_state = "request"

            def req_sm():
                if self.rst == 1:
                    self.read_req.read1_addr <<= None
                    self.read_req.read1_valid <<= None
                    self.read_req.read2_addr <<= None
                    self.read_req.read2_valid <<= None
                    self.read_req.rsv_addr <<= None
                    self.read_req.rsv_valid <<= None
                    self.read_req.valid <<= 0
                    self.sim_req_state = "idle"
                elif self.sim_req_state == "idle":
                    pass
                elif self.sim_req_state == "request":
                    if self.read_req.valid == 1:
                        self.read_req.read1_addr <<= None
                        self.read_req.read1_valid <<= None
                        self.read_req.read2_addr <<= None
                        self.read_req.read2_valid <<= None
                        self.read_req.rsv_addr <<= None
                        self.read_req.rsv_valid <<= None
                        self.read_req.valid <<= 0
                        self.sim_req_state = "idle"

            self.block_rsp_cnt = 0
            def block_rsp(clock_cnt):
                self.block_rsp_cnt = clock_cnt
                self.read_rsp.ready <<= 0

            def rsp_sm():
                if self.rst == 1:
                    self.read_rsp.ready <<= 1
                elif self.block_rsp_cnt > 0:
                    self.block_rsp_cnt -= 1
                    self.read_rsp.ready <<= self.block_rsp_cnt == 0

            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )
                    req_sm()
                    write_sm()
                    rsp_sm()


            def wait_rst():
                yield from wait_clk()
                while self.rst == 1:
                    yield from wait_clk()

            self.read_req.valid <<= 0
            self.write.valid <<= 0
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
            write_reg(3,303)
            yield from wait_clk()
            start_req(1,2,3)
            write_reg(3,403)
            yield from wait_clk()
            yield from wait_clk()
            start_req(0,1,None)
            yield from wait_clk()
            start_req(3,3,None)
            yield from wait_clk()
            yield from wait_clk()
            write_reg(3,503)
            yield from wait_clk()
            yield from wait_clk()



    class top(Module):
        clk = ClkPort()
        rst = RstPort()

        def body(self):
            seed(0)

            self.excericeser = Excerciser()
            self.dut = RegFile()

            self.dut.read_req <<= self.excericeser.read_req
            self.excericeser.read_rsp <<= self.dut.read_rsp

            self.dut.write <<= self.excericeser.write


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

if __name__ == "__main__":
    #gen()
    sim()
