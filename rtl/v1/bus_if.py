#!/usr/bin/python3
from random import *
from typing import *
from silicon import *
from .brew_types import *
from .brew_utils import *

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

        self.ext_grnt <<= arb_state == ArbStates.external

        self.mem.response <<= arb_state == ArbStates.memory
        self.mem.data_out <<= data_out
        self.mem.last     <<= last

        self.fetch.response <<= arb_state == ArbStates.fetch
        self.fetch.data_out <<= data_out
        self.fetch.last     <<= last

        start = (arb_state == ArbStates.idle) & (self.fetch.request | self.mem.request)
        advance = arb_state != ArbStates.idle

        read_not_write  <<= Reg(Select(start, read_not_write, Select(self.fetch.request, self.mem.read_not_write, self.fetch.read_not_write)))
        page_addr       <<= Reg(Select(start, page_addr, Select(self.fetch.request, self.mem.addr[31:8], self.fetch.addr[31:8])))
        page_offs       <<= Reg(Select(start, Select(advance, page_offs, page_offs + 1), Select(self.fetch.request, self.mem.addr[7:0], self.fetch.addr[7:0])))
        beats_remaining <<= Reg(Select(start, Select(advance, beats_remaining, beats_remaining - 1), Select(self.fetch.request, self.mem.burst_len, self.fetch.burst_len)))
        byte_en         <<= Reg(Select(start, 3, Select(self.fetch.request, self.mem.byte_en, self.fetch.byte_en)))
        data_in         <<= Reg(Select(arb_next_state == ArbStates.fetch, self.mem.data_in, self.fetch.data_in))

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

        '''
        CAS generation:

            CLK             /^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/
            ~CLK            \__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\__/^^\
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

        self.DRAM_nRAS <<= arb_state == ArbStates.idle
        self.DRAM_nRASd <<= Reg(self.DRAM_nRAS, clk=~self.clk)
        self.CAS_nEN <<= self.DRAM_nRAS & self.DRAM_nRASd
        self.DRAM_nCAS_h <<= ~byte_en[1] | self.CAS_nEN | ~self.clk
        self.DRAM_nCAS_l <<= ~byte_en[0] | self.CAS_nEN | ~self.clk
        self.DRAM_nWE <<= read_not_write

        self.DRAM_ADDR <<= Select(self.clk, row_addr, col_addr)
        self.DRAM_DATA_wr <<= Reg(data_in, clk=~self.clk) # This needs a half-cycle delay, so use the negated clock
        data_out <<= Reg(self.DRAM_DATA_rd)



def gen():
    Build.generate_rtl(BusIf)

gen()

