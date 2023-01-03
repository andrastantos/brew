#!/usr/bin/python3
from random import *
from typing import *
from silicon import *
from .brew_types import *
from .brew_utils import *

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

        class States(Enum):
            idle = 0
            read_1 = 1
            read_2 = 3
            write = 5

        self.fsm.reset_value   <<= States.idle
        self.fsm.default_state <<= States.idle

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
        do_nothing = self.stall_in | self.e_bubble
        self.fsm.add_transition(States.idle, ~self.exec.valid, States.idle)
        self.fsm.add_transition(States.idle, self.exec.valid & self.exec.is_load, States.read_1)
        self.fsm.add_transition(States.idle, self.exec.valid & self.exec.is_store, States.write)
        # For writes all the state management is done by the bus-interface for us, and is reported back through 'last'.
        self.fsm.add_transition(States.write, ~self.bus_if.response, States.write)
        self.fsm.add_transition(States.write, self.bus_if.response & self.bus_if.last & ~self.exec.valid, States.idle)
        self.fsm.add_transition(States.write, self.bus_if.response & self.bus_if.last &  self.exec.valid & self.exec.is_load, States.read_1)
        self.fsm.add_transition(States.write, self.bus_if.response & self.bus_if.last &  self.exec.valid & self.exec.is_store, States.write)
        # For read cycles, we get the data back one-cycle delayed compared to 'response'. So we need to stay an extra cycle longer in read states,
        # stalling the rest of the pipeline, thus need an extra read state
        self.fsm.add_transition(States.read_1, ~self.bus_if.response, States.read_1)
        self.fsm.add_transition(States.read_1, self.bus_if.response & ~self.bus_if.last, States.read_1)
        self.fsm.add_transition(States.read_1, self.bus_if.response & self.bus_if.last, States.read_2)

        self.fsm.add_transition(States.read_2, ~self.exec.valid, States.idle)
        self.fsm.add_transition(States.read_2,  self.exec.valid & self.exec.is_load, States.read_1)
        self.fsm.add_transition(States.read_2,  self.exec.valid & self.exec.is_store, States.write)

        # The only reason we would apply back-pressure is if we're waiting on the bus interface
        exec_ready <<= state == States.idle | state == States.read_2 | (state == States.write & self.bus_if.last)
        self.exec.ready <<= exec_ready
        accept_next = self.exec.valid & exec_ready

        data_h = Wire(Unsigned(16))
        data_h <<= Select(
            state == States.read_2,
            RegEn(self.exec.result[31:16], clock_en=(state == States.idle)),
            self.bus_if.data_out
        )
        data_l = Wire(Unsigned(16))
        data_l <<= Reg(
            Select(
                state == States.idle,
                Select(
                    state == States.read_1,
                    data_l,
                    self.bus_if.data_out
                ),
                self.exec.result[15:0]
            )
        )

        def bse(value):
            return {
                value[7], value[7], value[7], value[7], value[7], value[7], value[7], value[7],
                value[7], value[7], value[7], value[7], value[7], value[7], value[7], value[7],
                value[7], value[7], value[7], value[7], value[7], value[7], value[7], value[7],
                value[7:0]
            }

        def wse(value):
            return {
                value[15], value[15], value[15], value[15], value[15], value[15], value[15], value[15],
                value[15], value[15], value[15], value[15], value[15], value[15], value[15], value[15],
                value[15:0]
            }

        full_result = {data_h, data_l}
        self.w_result <<= SelectOne(
            self.exec.do_bse, bse(full_result),
            self.exec.do_wse, wse(full_result),
            self.exec.do_bze, full_result[7:0],
            self.exec.do_wze, full_result[15:0],
            default_port = full_result
        )
        self.w_result_reg_addr <<= RegEn(self.exec.result_reg_addr, clk_en=(state == States.idle))
        pass_through <<= RegEn(~(self.exec.is_read | self.exec.is_write), clock_en=accept_next)
        self.w_request <<= pass_through | state == States.read_2

        self.bus_if.request         <<= accept_next
        self.bus_if.read_not_write  <<= self.exec.is_load
        self.bus_if.burst_len       <<= self.exec.mem_access_len[1] # 8- and 16-bit accesses need a burst length of 1, while 32-bit accesses need a burst-length of 2.
        self.bus_if.byte_en         <<= Select(
            self.exec_mem_access_len == 0,
            3, # 16- or 32-bit accesses use both byte-enables
            {self.exec.mem_addr[0], ~self.exec.mem_addr[0]} # 8-bit accesses byte-enables depend on address LSB
        )
        self.bus_if.addr            = self.exec.mem_addr[31:1]
        self.bus_if.data_in         = Select(
            state == States.idle,
            data_h,
            self.exec.result[15:0]
        )





def gen():
    Build.generate_rtl(MemoryStage)

gen()

