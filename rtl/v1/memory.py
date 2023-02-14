#!/usr/bin/python3
from random import *
from typing import *
from copy import copy

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
Memory stage of the V1 pipeline.

This stage is sandwiched between 'execute' and 'write-back'.

It does the following:
- Handles handshaking with the bus interface
- Issues loads/stores
- Stalls the pipeline until memory responses come back (and passes results through for non-memory operations)

"""

# TODO: I think the memory stage is completely busted
class MemoryStage(Module):
    clk = ClkPort()
    rst = RstPort()


    # Pipeline input from execute
    input_port = Input(MemInputIf)

    # Pipeline output to register file
    output_port = Output(MemOuputIf)

    # Interface to the bus interface
    bus_req_if = Output(BusIfRequestIf)
    bus_rsp_if = Input(BusIfResponseIf)

    # Interface to the CSR registers
    csr_if = Output(CsrIf)

    #def construct(self, csr_base: int):
    #    assert csr_base & 0x0fffffff == 0, "CSR address decode uses the top 4 bits of the address"
    #    self.csr_base = csr_base >> 28

    def body(self):
        self.csr_base = 0xc

        # Burst and stall generation state-machine
        self.fsm = FSM()

        class MemoryStates(Enum):
            idle = 0
            mem_read_1 = 1
            mem_read_2 = 2
            mem_write_1 = 3
            mem_write_2 = 4
            csr_read = 5
            csr_write = 7

        self.fsm.reset_value   <<= MemoryStates.idle
        self.fsm.default_state <<= MemoryStates.idle

        state = Wire()
        next_state = Wire()
        state <<= self.fsm.state
        next_state <<= self.fsm.next_state

        '''
        8- and 16-bit access timing (two back-to-back reads)

                                                   <idle><rd_1><idle><st_1>
            CLK               /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            e_bubble          _______________________________________________________
            e_is_load         _____________________/^^^^^\___________________________
            e_is_store        _________________________________/^^^^^\_______________
            e_result          ---------------------------------<=====>---------------
            e_mem_addr        ---------------------<=====>-----<=====>---------------
            e_mem_access_len  ---------------------<=====>-----<=====>---------------
            b_request         _____________________/^^^^^^^^^^^^^^^^^^^^^^^\_________
            b_addr            ---------------------<  a  >-----<  a  >---------------
            b_response        ___________________________/^^^^^\_____/^^^^^\_________
            b_last            ___________________________/^^^^^\_____/^^^^^\_________
            b_data_in         ---------------------------------<=====>---------------
            b_data_out        ---------------------------------<=====>---------------

        '''
        is_csr = self.input_port.mem_addr[31:28] == self.csr_base

        csr_request = Wire(logic)

        self.fsm.add_transition(MemoryStates.idle, self.input_port.valid & ~is_csr & self.input_port.is_load  & (self.input_port.mem_access_len == 2), MemoryStates.mem_read_1)
        self.fsm.add_transition(MemoryStates.idle, self.input_port.valid & ~is_csr & self.input_port.is_load  & (self.input_port.mem_access_len != 2), MemoryStates.mem_read_2)
        self.fsm.add_transition(MemoryStates.idle, self.input_port.valid & ~is_csr & self.input_port.is_store & (self.input_port.mem_access_len == 2), MemoryStates.mem_write_1)
        self.fsm.add_transition(MemoryStates.idle, self.input_port.valid & ~is_csr & self.input_port.is_store & (self.input_port.mem_access_len != 2), MemoryStates.mem_write_2)
        self.fsm.add_transition(MemoryStates.idle, self.input_port.valid &  is_csr & self.input_port.is_load,  MemoryStates.csr_read)
        self.fsm.add_transition(MemoryStates.idle, self.input_port.valid &  is_csr & self.input_port.is_store, MemoryStates.csr_write)

        # For memory reads, we'll have to delay acceptance of next instruction until response is back.
        # TODO: we actually could let other non-memory stores through and out-of-order retire those instructions.
        # Since 32-bit reads come back in two parts, we have 2 states. For 16- and 8-bit reads, we immediately start in mem_read_2
        self.fsm.add_transition(MemoryStates.mem_read_1, self.bus_rsp_if.valid,                                MemoryStates.mem_read_2)

        self.fsm.add_transition(MemoryStates.mem_read_2, self.bus_rsp_if.valid & ~self.input_port.valid,                                MemoryStates.idle)
        self.fsm.add_transition(MemoryStates.mem_read_2, self.bus_rsp_if.valid &  self.input_port.valid & ~is_csr & self.input_port.is_load  & (self.input_port.mem_access_len == 2), MemoryStates.mem_read_1)
        self.fsm.add_transition(MemoryStates.mem_read_2, self.bus_rsp_if.valid &  self.input_port.valid & ~is_csr & self.input_port.is_load  & (self.input_port.mem_access_len != 2), MemoryStates.mem_read_2)
        self.fsm.add_transition(MemoryStates.mem_read_2, self.bus_rsp_if.valid &  self.input_port.valid & ~is_csr & self.input_port.is_store & (self.input_port.mem_access_len == 2), MemoryStates.mem_write_1)
        self.fsm.add_transition(MemoryStates.mem_read_2, self.bus_rsp_if.valid &  self.input_port.valid & ~is_csr & self.input_port.is_store & (self.input_port.mem_access_len != 2), MemoryStates.mem_write_2)
        self.fsm.add_transition(MemoryStates.mem_read_2, self.bus_rsp_if.valid &  self.input_port.valid &  is_csr & self.input_port.is_load,  MemoryStates.csr_read)
        self.fsm.add_transition(MemoryStates.mem_read_2, self.bus_rsp_if.valid &  self.input_port.valid &  is_csr & self.input_port.is_store, MemoryStates.csr_write)

        self.fsm.add_transition(MemoryStates.mem_write_1, 1,                                                  MemoryStates.mem_write_2)
        self.fsm.add_transition(MemoryStates.mem_write_2, self.bus_req_if.ready & ~self.input_port.valid,                                MemoryStates.idle)
        self.fsm.add_transition(MemoryStates.mem_write_2, self.bus_req_if.ready &  self.input_port.valid & ~is_csr & self.input_port.is_load  & (self.input_port.mem_access_len == 2), MemoryStates.mem_read_1)
        self.fsm.add_transition(MemoryStates.mem_write_2, self.bus_req_if.ready &  self.input_port.valid & ~is_csr & self.input_port.is_load  & (self.input_port.mem_access_len != 2), MemoryStates.mem_read_2)
        self.fsm.add_transition(MemoryStates.mem_write_2, self.bus_req_if.ready &  self.input_port.valid & ~is_csr & self.input_port.is_store & (self.input_port.mem_access_len == 2), MemoryStates.mem_write_1)
        self.fsm.add_transition(MemoryStates.mem_write_2, self.bus_req_if.ready &  self.input_port.valid & ~is_csr & self.input_port.is_store & (self.input_port.mem_access_len != 2), MemoryStates.mem_write_2)
        self.fsm.add_transition(MemoryStates.mem_write_2, self.bus_req_if.ready &  self.input_port.valid &  is_csr & self.input_port.is_load,  MemoryStates.csr_read)
        self.fsm.add_transition(MemoryStates.mem_write_2, self.bus_req_if.ready &  self.input_port.valid &  is_csr & self.input_port.is_store, MemoryStates.csr_write)
        # CSR interface has fixed timing: a cycle after the read, we get the resoponse and writes are just pipelined through
        self.fsm.add_transition(MemoryStates.csr_read, ~self.input_port.valid,                                MemoryStates.idle)
        self.fsm.add_transition(MemoryStates.csr_read,  self.input_port.valid & ~is_csr & self.input_port.is_load  & (self.input_port.mem_access_len == 2), MemoryStates.mem_read_1)
        self.fsm.add_transition(MemoryStates.csr_read,  self.input_port.valid & ~is_csr & self.input_port.is_load  & (self.input_port.mem_access_len != 2), MemoryStates.mem_read_2)
        self.fsm.add_transition(MemoryStates.csr_read,  self.input_port.valid & ~is_csr & self.input_port.is_store & (self.input_port.mem_access_len == 2), MemoryStates.mem_write_1)
        self.fsm.add_transition(MemoryStates.csr_read,  self.input_port.valid & ~is_csr & self.input_port.is_store & (self.input_port.mem_access_len != 2), MemoryStates.mem_write_2)
        self.fsm.add_transition(MemoryStates.csr_read,  self.input_port.valid &  is_csr & self.input_port.is_load,  MemoryStates.csr_read)
        self.fsm.add_transition(MemoryStates.csr_read,  self.input_port.valid &  is_csr & self.input_port.is_store, MemoryStates.csr_write)

        self.fsm.add_transition(MemoryStates.csr_write, ~self.input_port.valid,                                MemoryStates.idle)
        self.fsm.add_transition(MemoryStates.csr_write,  self.input_port.valid & ~is_csr & self.input_port.is_load  & (self.input_port.mem_access_len == 2), MemoryStates.mem_read_1)
        self.fsm.add_transition(MemoryStates.csr_write,  self.input_port.valid & ~is_csr & self.input_port.is_load  & (self.input_port.mem_access_len != 2), MemoryStates.mem_read_2)
        self.fsm.add_transition(MemoryStates.csr_write,  self.input_port.valid & ~is_csr & self.input_port.is_store & (self.input_port.mem_access_len == 2), MemoryStates.mem_write_1)
        self.fsm.add_transition(MemoryStates.csr_write,  self.input_port.valid & ~is_csr & self.input_port.is_store & (self.input_port.mem_access_len != 2), MemoryStates.mem_write_2)
        self.fsm.add_transition(MemoryStates.csr_write,  self.input_port.valid &  is_csr & self.input_port.is_load,  MemoryStates.csr_read)
        self.fsm.add_transition(MemoryStates.csr_write,  self.input_port.valid &  is_csr & self.input_port.is_store, MemoryStates.csr_write)

        # The only reason we would apply back-pressure is if we're waiting on the bus interface
        exec_ready = Wire()
        exec_ready <<= (
            (((state == MemoryStates.mem_write_2) | (state ==MemoryStates.mem_read_2)) & self.bus_req_if.ready) |
            ((state == MemoryStates.csr_write) | (state ==MemoryStates.csr_read) | (state ==MemoryStates.idle))
        )

        self.input_port.ready <<= exec_ready
        accept_next = self.input_port.valid & exec_ready

        lsb = Reg(self.input_port.mem_addr[0], clock_en=accept_next)
        data_h = Wire(Unsigned(16))
        data_h <<= SelectOne(
            state == MemoryStates.idle, Reg(self.input_port.result[31:16], clock_en=(state == accept_next)),
            state == MemoryStates.mem_read_2, self.bus_rsp_if.data,
            state == MemoryStates.csr_read, self.csr_if.rd_data[31:16]
        )
        data_l = Wire(Unsigned(16))
        data_l <<= SelectOne(
            state == MemoryStates.idle, Reg(self.input_port.result[15:0], clock_en=(state == accept_next)),
            # Have to be careful here with 8-bit reads: we need to move the upper to the lower bytes for odd addresses
            state == MemoryStates.mem_read_1, Reg(Select(lsb, self.bus_rsp_if.data, concat(self.bus_rsp_if.data[15:8], self.bus_rsp_if.data[15:8])), clock_en=self.bus_rsp_if.valid),
            state == MemoryStates.csr_read, self.csr_if.rd_data[15:0]
        )

        self.output_port.data_l <<= data_l
        self.output_port.data_h <<= data_h

        write_back_tick = Wire(logic)
        write_back_tick = Reg(accept_next)

        self.output_port.valid <<= SelectOne(
            (state == MemoryStates.idle) & (next_state == MemoryStates.idle), write_back_tick,
            state == MemoryStates.mem_read_1, 0,
            state == MemoryStates.mem_read_2, self.bus_rsp_if.valid,
            state == MemoryStates.csr_read, write_back_tick,
            state == MemoryStates.csr_write, write_back_tick
        )

        # BUG BUG!!!!! THIS SHOULD BE NEXT_STATE, BUT THAT RESULTS IN A COMB. LOOP.
        #####################################################################################
        self.bus_req_if.valid           <<= (state == MemoryStates.mem_read_1) | (state == MemoryStates.mem_write_1) | (state == MemoryStates.mem_read_2) | (state == MemoryStates.mem_write_2)
        self.bus_req_if.read_not_write  <<= self.input_port.is_load
        self.bus_req_if.byte_en         <<= Select(
            self.input_port.mem_access_len == 0,
            3, # 16- or 32-bit accesses use both byte-enables
            concat(self.input_port.mem_addr[0], ~self.input_port.mem_addr[0]) # 8-bit accesses byte-enables depend on address LSB
        )
        bus_addr = Select(
            accept_next,
            Reg((self.input_port.mem_addr[31:1]+self.input_port.mem_access_len[1])[30:0], clock_en = accept_next),
            self.input_port.mem_addr[31:1]
        )
        self.bus_req_if.addr             <<= bus_addr[21:0]
        self.bus_req_if.dram_not_ext     <<= 0
        self.bus_req_if.data             <<= Select(
            state == MemoryStates.idle,
            data_h,
            concat(
                Select(
                    self.input_port.mem_addr[0] & (self.input_port.mem_access_len == access_len_8),
                    self.input_port.result[15:8],
                    self.input_port.result[7:0]
                ),
                self.input_port.result[7:0]
            )
        )

        csr_request <<= self.input_port.valid & (self.input_port.is_store | self.input_port.is_load) & is_csr
        self.csr_if.request <<= csr_request
        self.csr_if.addr <<= self.input_port.mem_addr[11:2] # CSRs are always 32-bits long, don't care about the low-oreder 2 bits
        self.csr_if.wr_data <<= self.input_port.result
        self.csr_if.read_not_write <<= self.input_port.is_load


def sim():

    class CsrEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        csr_if = Input(CsrIf)

        def simulate(self) -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            self.csr_if.response <<= 0
            self.csr_if.rd_data <<= None
            while True:
                yield from wait_clk()

                if self.rst == 1:
                    self.csr_if.response <<= 0
                    self.csr_if.rd_data <<= None
                else:
                    if self.csr_if.request == 1:
                        if self.csr_if.read_not_write:
                            # Read request
                            print(f"Reading CSR {self.csr_if.addr:x}")
                            self.csr_if.response <<= 0
                            self.csr_if.rd_data <<= None
                            yield from wait_clk()
                            self.csr_if.rd_data <<= self.csr_if.addr
                        else:
                            # Write request
                            print(f"Writing CSR {self.csr_if.addr:x} with {self.csr_if.wr_data:x}")
                            self.csr_if.rd_data <<= None
                    else:
                        self.csr_if.response <<= 0
                        self.csr_if.rd_data <<= None

    class BusEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        bus_if = Input(BusIfPortIf)

        def simulate(self, simulator: 'Simulator') -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            self.bus_if.response <<= 0
            self.bus_if.data_out <<= None
            self.bus_if.last <<= None
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
                            addr = self.bus_if.addr
                            data = self.bus_if.addr[15:0]
                            print(f"Reading BUS {addr:x} burst:{self.bus_if.burst_len} byte_en:{self.bus_if.byte_en}")
                            self.bus_if.response <<= 0
                            self.bus_if.last <<= 0
                            self.bus_if.data_out <<= None
                            #yield from wait_clk()
                            self.bus_if.response <<= 1
                            for i in range(burst_cnt):
                                self.bus_if.last <<= 1 if i == burst_cnt-1 else 0
                                self.bus_if.response <<= 1 if i <= burst_cnt-1 else 0
                                yield from wait_clk()
                                print(f"    data:{data:x}")
                                self.bus_if.data_out <<= data
                                data = data + 1
                            self.bus_if.response <<= 0
                            self.bus_if.last <<= 0
                        else:
                            # Write request
                            print(f"Writing BUS {self.bus_if.addr:x} burst:{self.bus_if.burst_len} byte_en:{self.bus_if.byte_en}")
                            self.bus_if.response <<= 0
                            self.bus_if.last <<= 0
                            self.bus_if.data_out <<= None
                            #print(f"    data:{self.bus_if.data_in:x} at {simulator.now}")
                            #yield from wait_clk()
                            self.bus_if.response <<= 1
                            for i in range(burst_cnt):
                                print(f"    data:{self.bus_if.data_in:x} at {simulator.now}")
                                self.bus_if.last <<= 1 if i == burst_cnt-1 else 0
                                self.bus_if.response <<= 1 if i <= burst_cnt-1 else 0
                                yield from wait_clk()
                            self.bus_if.response <<= 0
                            self.bus_if.last <<= 0
                    else:
                        self.bus_if.response <<= 0
                        self.bus_if.data_out <<= None
                        self.bus_if.last <<= None


    class RegFileEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        write_if = Input(RegFileWriteBackIf)

        def simulate(self) -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            while True:
                yield from wait_clk()
                if self.write_if.valid:
                    print(f"Writing REG $r{self.write_if.addr:x} with value {self.write_if.data:x} enable: {self.write_if.data_en}")




    class ExecEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        exec = Output(MemInputIf)

        def simulate(self) -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            def wait_rst():
                yield from wait_clk()
                while self.rst == 1:
                    yield from wait_clk()

            def wait_transfer():
                self.exec.valid <<= 1
                yield from wait_clk()
                while (self.exec.valid & self.exec.ready) != 1:
                    yield from wait_clk()
                self.exec.valid <<= 0

            def do_load(reg_addr: int, mem_addr: int, mem_access_len: int, sign_extend: bool = False):
                self.exec.is_load <<= 1
                self.exec.is_store <<= 0
                self.exec.result <<= None
                self.exec.result_reg_addr <<= reg_addr
                self.exec.result_reg_addr_valid <<= 1
                self.exec.mem_addr <<= mem_addr
                self.exec.mem_access_len <<= mem_access_len
                if mem_access_len == access_len_8:
                    self.exec.do_bse <<= 1 if sign_extend else 0
                    self.exec.do_bze <<= 0 if sign_extend else 1
                    self.exec.do_wse <<= 0
                    self.exec.do_wze <<= 0
                if mem_access_len == access_len_16:
                    self.exec.do_bse <<= 0
                    self.exec.do_bze <<= 0
                    self.exec.do_wse <<= 1 if sign_extend else 0
                    self.exec.do_wze <<= 0 if sign_extend else 1
                if mem_access_len == access_len_32:
                    self.exec.do_bse <<= 0
                    self.exec.do_bze <<= 0
                    self.exec.do_wse <<= 0
                    self.exec.do_wze <<= 0
                yield from wait_transfer()

            def do_store(value: int, mem_addr: int, mem_access_len: int):
                self.exec.is_load <<= 0
                self.exec.is_store <<= 1
                self.exec.result <<= value
                self.exec.result_reg_addr <<= None
                self.exec.result_reg_addr_valid <<= 0
                self.exec.mem_addr <<= mem_addr
                self.exec.mem_access_len <<= mem_access_len
                self.exec.do_bse <<= None
                self.exec.do_bze <<= None
                self.exec.do_wse <<= None
                self.exec.do_wze <<= None
                yield from wait_transfer()

            self.exec.valid <<= 0
            yield from wait_rst()
            for i in range(4):
                yield from wait_clk()

            yield from do_load(reg_addr=0x3, mem_addr=0x140, mem_access_len=access_len_32)
            yield from do_load(reg_addr=0x3, mem_addr=0x240, mem_access_len=access_len_16, sign_extend=False)
            yield from do_load(reg_addr=0x3, mem_addr=0x244, mem_access_len=access_len_8, sign_extend=False)
            yield from do_load(reg_addr=0x3, mem_addr=0x245, mem_access_len=access_len_8, sign_extend=False)
            for i in range(4):
                yield from wait_clk()
            yield from do_store(value=0x12345678, mem_addr=0xadd0e55, mem_access_len=access_len_32)
            yield from do_store(value=0x23456789, mem_addr=0x0002000, mem_access_len=access_len_16)
            yield from do_store(value=0x3456789a, mem_addr=0x0004000, mem_access_len=access_len_8)
            yield from do_store(value=0x456789ab, mem_addr=0x0005001, mem_access_len=access_len_8)



    class top(Module):
        clk = ClkPort()
        rst = RstPort()

        def body(self):
            seed(0)
            self.exec = Wire(MemInputIf)
            self.bus_if = Wire(BusIfPortIf)
            self.csr_if = Wire(CsrIf)

            self.exec_emulator = ExecEmulator()
            self.csr_emulator = CsrEmulator()
            self.bus_emulator = BusEmulator()
            self.reg_file_emulator = RegFileEmulator()

            self.dut = MemoryStage()

            self.exec <<= self.exec_emulator.exec
            self.dut.input_port <<= self.exec

            self.reg_file_emulator.write_if <<= self.dut.output_port

            self.bus_if <<= self.dut.bus_if
            self.bus_emulator.bus_if <<= self.bus_if

            self.csr_if <<= self.dut.csr_if
            self.csr_emulator.csr_if <<= self.csr_if


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

    Build.simulation(top, "memory.vcd", add_unnamed_scopes=True)

def gen():
    Build.generate_rtl(MemoryStage)

if __name__ == "__main__":
    gen()
    #sim()
