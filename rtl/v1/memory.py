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
    exec = Input(ExecMemIf)

    # Pipeline output to register file
    w_result_reg_addr = Output(BrewRegAddr)
    w_result = Output(BrewData)
    w_request = Output(logic)

    # Interface to the bus interface
    bus_if = Output(BusIfPortIf)

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
            read_1 = 1
            read_2 = 3
            write = 5
            csr_read = 8
            csr_write = 9

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
            b_request         _____________________/^^^^^\_____/^^^^^\_______________
            b_addr            ---------------------<  a  >-----<  a  >---------------
            b_response        ___________________________/^^^^^\_____/^^^^^\_________
            b_last            ___________________________/^^^^^\_____/^^^^^\_________
            b_data_in         ---------------------------------<=====>---------------
            b_data_out        ---------------------------------<=====>---------------

        '''
        is_csr = self.exec.mem_addr[31:28] == self.csr_base

        csr_request = Wire(logic)
        csr_response = Wire(logic)
        csr_response <<= self.csr_if.response

        self.fsm.add_transition(MemoryStates.idle, ~self.exec.valid, MemoryStates.idle)
        self.fsm.add_transition(MemoryStates.idle, self.exec.valid & ~is_csr & self.exec.is_load, MemoryStates.read_1)
        self.fsm.add_transition(MemoryStates.idle, self.exec.valid & ~is_csr & self.exec.is_store, MemoryStates.write)
        self.fsm.add_transition(MemoryStates.idle, self.exec.valid & is_csr & self.exec.is_load & ~csr_response, MemoryStates.csr_read)
        self.fsm.add_transition(MemoryStates.idle, self.exec.valid & is_csr & self.exec.is_store & ~csr_response, MemoryStates.csr_write)
        # For writes all the state management is done by the bus-interface for us, and is reported back through 'last'.
        self.fsm.add_transition(MemoryStates.write, ~self.bus_if.response, MemoryStates.write)
        self.fsm.add_transition(MemoryStates.write, self.bus_if.response & self.bus_if.last, MemoryStates.idle)
        # For read cycles, we get the data back one-cycle delayed compared to 'response'. So we need to stay an extra cycle longer in read states,
        # stalling the rest of the pipeline, thus need an extra read state
        self.fsm.add_transition(MemoryStates.read_1, ~self.bus_if.response, MemoryStates.read_1)
        self.fsm.add_transition(MemoryStates.read_1, self.bus_if.response & ~self.bus_if.last, MemoryStates.read_1)
        self.fsm.add_transition(MemoryStates.read_1, self.bus_if.response & self.bus_if.last, MemoryStates.read_2)

        self.fsm.add_transition(MemoryStates.read_2, ~self.exec.valid, MemoryStates.idle)
        self.fsm.add_transition(MemoryStates.read_2,  self.exec.valid & ~is_csr & self.exec.is_load, MemoryStates.read_1)
        self.fsm.add_transition(MemoryStates.read_2,  self.exec.valid & ~is_csr & self.exec.is_store, MemoryStates.write)
        self.fsm.add_transition(MemoryStates.read_2,  self.exec.valid & is_csr & self.exec.is_load & ~csr_response, MemoryStates.csr_read)
        self.fsm.add_transition(MemoryStates.read_2,  self.exec.valid & is_csr & self.exec.is_store & ~csr_response, MemoryStates.csr_write)

        self.fsm.add_transition(MemoryStates.csr_read, ~csr_response, MemoryStates.csr_read)
        self.fsm.add_transition(MemoryStates.csr_read, csr_response & ~self.exec.valid, MemoryStates.idle)
        self.fsm.add_transition(MemoryStates.csr_read, csr_response & self.exec.valid & ~is_csr & self.exec.is_load, MemoryStates.read_1)
        self.fsm.add_transition(MemoryStates.csr_read, csr_response & self.exec.valid & ~is_csr & self.exec.is_store, MemoryStates.write)
        self.fsm.add_transition(MemoryStates.csr_read, csr_response & self.exec.valid & is_csr & self.exec.is_load & ~csr_response, MemoryStates.csr_read)
        self.fsm.add_transition(MemoryStates.csr_read, csr_response & self.exec.valid & is_csr & self.exec.is_store & ~csr_response, MemoryStates.csr_write)

        self.fsm.add_transition(MemoryStates.csr_write, ~csr_response, MemoryStates.csr_write)
        self.fsm.add_transition(MemoryStates.csr_write, csr_response & ~self.exec.valid, MemoryStates.idle)
        self.fsm.add_transition(MemoryStates.csr_write, csr_response & self.exec.valid & ~is_csr & self.exec.is_load, MemoryStates.read_1)
        self.fsm.add_transition(MemoryStates.csr_write, csr_response & self.exec.valid & ~is_csr & self.exec.is_store, MemoryStates.write)
        self.fsm.add_transition(MemoryStates.csr_write, csr_response & self.exec.valid & is_csr & self.exec.is_load & ~csr_response, MemoryStates.csr_read)
        self.fsm.add_transition(MemoryStates.csr_write, csr_response & self.exec.valid & is_csr & self.exec.is_store & ~csr_response, MemoryStates.csr_write)

        # The only reason we would apply back-pressure is if we're waiting on the bus interface
        exec_ready = Wire()
        exec_ready <<= (
            (state == MemoryStates.idle) |
            (state == MemoryStates.read_2) |
            (state == MemoryStates.csr_read & csr_response) |
            (state == MemoryStates.csr_write & csr_response)
        )
        self.exec.ready <<= exec_ready
        accept_next = self.exec.valid & exec_ready

        lsb = Reg(self.exec.mem_addr[0], clock_en=accept_next)
        data_h = Wire(Unsigned(16))
        data_h <<= Select(
            state == MemoryStates.read_2,
            Select(
                csr_request & csr_response & self.csr_if.read_not_write,
                Reg(self.exec.result[31:16], clock_en=(state == MemoryStates.idle)),
                self.csr_if.rd_data[31:16] # CSR read
            ),
            self.bus_if.data_out
        )
        first = Wire(logic)
        first <<= Reg(
            Select(
                accept_next,
                Select(
                    self.bus_if.response,
                    first,
                    0
                ),
                1
            )
        )
        first_and_last = Wire()
        first_and_last <<= Reg(first & self.bus_if.last)
        data_l = Wire(Unsigned(16))
        data_l <<= \
            Select(
                csr_request & csr_response & self.csr_if.read_not_write,
                Select(
                    first_and_last,
                    Reg(
                        Select(
                            state == MemoryStates.idle,
                            Select(
                                state == MemoryStates.read_1,
                                data_l,
                                self.bus_if.data_out,
                            ),
                            self.exec.result[15:0]
                        )
                    ),
                    # Receiving read response from bus_if
                    # Have to be careful here with 8-bit reads: we need to move the upper to the lower bytes for odd addresses
                    Select(
                        lsb,
                        self.bus_if.data_out,
                        concat(self.bus_if.data_out[15:8], self.bus_if.data_out[15:8])
                    )
                ),
                self.csr_if.rd_data[15:0] # CSR read
            )

        def bse(value):
            return concat(
                value[7], value[7], value[7], value[7], value[7], value[7], value[7], value[7],
                value[7], value[7], value[7], value[7], value[7], value[7], value[7], value[7],
                value[7], value[7], value[7], value[7], value[7], value[7], value[7], value[7],
                value[7:0]
            )

        def wse(value):
            return concat(
                value[15], value[15], value[15], value[15], value[15], value[15], value[15], value[15],
                value[15], value[15], value[15], value[15], value[15], value[15], value[15], value[15],
                value[15:0]
            )

        full_result = concat(data_h, data_l)
        do_bse = Reg(self.exec.do_bse, clock_en=accept_next)
        do_wse = Reg(self.exec.do_wse, clock_en=accept_next)
        do_bze = Reg(self.exec.do_bze, clock_en=accept_next)
        do_wze = Reg(self.exec.do_wze, clock_en=accept_next)
        self.w_result <<= SelectOne(
            do_bse, bse(data_l),
            do_wse, wse(data_l),
            do_bze, data_l[7:0],
            do_wze, data_l[15:0],
            default_port = full_result
        )
        self.w_result_reg_addr <<= BrewRegAddr(Reg(self.exec.result_reg_addr, clock_en=(state == MemoryStates.idle)))
        pass_through = Wire()
        pass_through <<= Reg(~(self.exec.is_load | self.exec.is_store), clock_en=accept_next)
        self.w_request <<= pass_through | state == MemoryStates.read_2

        self.bus_if.request         <<= accept_next & (self.exec.is_store | self.exec.is_load) & ~is_csr
        self.bus_if.read_not_write  <<= self.exec.is_load
        self.bus_if.burst_len       <<= self.exec.mem_access_len[1] # 8- and 16-bit accesses need a burst length of 1, while 32-bit accesses need a burst-length of 2.
        self.bus_if.byte_en         <<= Select(
            self.exec.mem_access_len == 0,
            3, # 16- or 32-bit accesses use both byte-enables
            concat(self.exec.mem_addr[0], ~self.exec.mem_addr[0]) # 8-bit accesses byte-enables depend on address LSB
        )
        self.bus_if.addr            <<= self.exec.mem_addr[31:1]
        self.bus_if.data_in         <<= Select(
            state == MemoryStates.idle,
            data_h,
            concat(
                Select(
                    self.exec.mem_addr[0] & (self.exec.mem_access_len == access_len_8),
                    self.exec.result[15:8],
                    self.exec.result[7:0]
                ),
                self.exec.result[7:0]
            )
        )

        csr_request <<= self.exec.valid & (self.exec.is_store | self.exec.is_load) & is_csr
        self.csr_if.request <<= csr_request
        self.csr_if.addr <<= self.exec.mem_addr[12:2] # CSRs are always 32-bits long, don't care about the low-oreder 2 bits
        self.csr_if.wr_data <<= self.exec.result
        self.csr_if.read_not_write <<= self.exec.is_load


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
                            self.csr_if_response <<= 1
                            self.csr_if.rd_data <<= self.csr_if.addr
                        else:
                            # Write request
                            print(f"Writing CSR {self.csr_if.addr:x} with {self.csr_if.wr_data:x}")
                            self.csr_if_response <<= 1
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

        w_result_reg_addr = Input(BrewRegAddr)
        w_result = Input(BrewData)
        w_request = Input(logic)

        def simulate(self) -> TSimEvent:
            def wait_clk():
                yield (self.clk, )
                while self.clk.get_sim_edge() != EdgeType.Positive:
                    yield (self.clk, )

            while True:
                yield from wait_clk()

                if self.w_request == 1:
                    print(f"Writing REG $r{self.w_result_reg_addr:x} with value {self.w_result:x}")


    class ExecEmulator(Module):
        clk = ClkPort()
        rst = RstPort()

        exec = Output(ExecMemIf)

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
            self.exec = Wire(ExecMemIf)
            self.w_result_reg_addr = Wire(BrewRegAddr)
            self.w_result = Wire(BrewData)
            self.w_request = Wire(logic)
            self.bus_if = Wire(BusIfPortIf)
            self.csr_if = Wire(CsrIf)

            self.exec_emulator = ExecEmulator()
            self.csr_emulator = CsrEmulator()
            self.bus_emulator = BusEmulator()
            self.reg_file_emulator = RegFileEmulator()

            self.dut = MemoryStage()

            self.exec <<= self.exec_emulator.exec
            self.dut.exec <<= self.exec

            self.w_request <<= self.dut.w_request
            self.w_result <<= self.dut.w_result
            self.w_result_reg_addr <<= self.dut.w_result_reg_addr
            self.reg_file_emulator.w_request <<= self.w_request
            self.reg_file_emulator.w_result <<= self.w_result
            self.reg_file_emulator.w_result_reg_addr <<= self.w_result_reg_addr

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

#gen()
sim()
