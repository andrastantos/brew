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

    def body(self):
        # Burst and stall generation state-machine
        self.fsm = FSM()

        class MemoryStates(Enum):
            idle = 0
            read_1 = 1
            read_2 = 3
            write = 5

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
        self.fsm.add_transition(MemoryStates.idle, ~self.exec.valid, MemoryStates.idle)
        self.fsm.add_transition(MemoryStates.idle, self.exec.valid & self.exec.is_load, MemoryStates.read_1)
        self.fsm.add_transition(MemoryStates.idle, self.exec.valid & self.exec.is_store, MemoryStates.write)
        # For writes all the state management is done by the bus-interface for us, and is reported back through 'last'.
        self.fsm.add_transition(MemoryStates.write, ~self.bus_if.response, MemoryStates.write)
        self.fsm.add_transition(MemoryStates.write, self.bus_if.response & self.bus_if.last & ~self.exec.valid, MemoryStates.idle)
        self.fsm.add_transition(MemoryStates.write, self.bus_if.response & self.bus_if.last &  self.exec.valid & self.exec.is_load, MemoryStates.read_1)
        self.fsm.add_transition(MemoryStates.write, self.bus_if.response & self.bus_if.last &  self.exec.valid & self.exec.is_store, MemoryStates.write)
        # For read cycles, we get the data back one-cycle delayed compared to 'response'. So we need to stay an extra cycle longer in read states,
        # stalling the rest of the pipeline, thus need an extra read state
        self.fsm.add_transition(MemoryStates.read_1, ~self.bus_if.response, MemoryStates.read_1)
        self.fsm.add_transition(MemoryStates.read_1, self.bus_if.response & ~self.bus_if.last, MemoryStates.read_1)
        self.fsm.add_transition(MemoryStates.read_1, self.bus_if.response & self.bus_if.last, MemoryStates.read_2)

        self.fsm.add_transition(MemoryStates.read_2, ~self.exec.valid, MemoryStates.idle)
        self.fsm.add_transition(MemoryStates.read_2,  self.exec.valid & self.exec.is_load, MemoryStates.read_1)
        self.fsm.add_transition(MemoryStates.read_2,  self.exec.valid & self.exec.is_store, MemoryStates.write)

        # The only reason we would apply back-pressure is if we're waiting on the bus interface
        exec_ready = Wire()
        exec_ready <<= (state == MemoryStates.idle) | (state == MemoryStates.read_2) | ((state == MemoryStates.write) & self.bus_if.last)
        self.exec.ready <<= exec_ready
        accept_next = self.exec.valid & exec_ready

        data_h = Wire(Unsigned(16))
        data_h <<= Select(
            state == MemoryStates.read_2,
            Reg(self.exec.result[31:16], clock_en=(state == MemoryStates.idle)),
            self.bus_if.data_out
        )
        data_l = Wire(Unsigned(16))
        data_l <<= Reg(
            Select(
                state == MemoryStates.idle,
                Select(
                    state == MemoryStates.read_1,
                    data_l,
                    self.bus_if.data_out
                ),
                self.exec.result[15:0]
            )
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
        self.w_result <<= SelectOne(
            self.exec.do_bse, bse(full_result),
            self.exec.do_wse, wse(full_result),
            self.exec.do_bze, full_result[7:0],
            self.exec.do_wze, full_result[15:0],
            default_port = full_result
        )
        self.w_result_reg_addr <<= Reg(self.exec.result_reg_addr, clock_en=(state == MemoryStates.idle))
        pass_through = Wire()
        pass_through <<= Reg(~(self.exec.is_load | self.exec.is_store), clock_en=accept_next)
        self.w_request <<= pass_through | state == MemoryStates.read_2

        self.bus_if.request         <<= accept_next
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
            self.exec.result[15:0]
        )





def gen():
    Build.generate_rtl(MemoryStage)

#gen()

