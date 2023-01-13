#!/usr/bin/python3
from typing import *
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
    clk = Input(logic)
    rst = Input(logic)

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



    def body(self):
        # We have two memory instances, one for each read port. The write ports of
        # these instances are connected together so they get written the same data
        mem1 = SimpleDualPortMemory(addr_type=BrewRegAddr, data_type=BrewData)
        mem2 = SimpleDualPortMemory(addr_type=BrewRegAddr, data_type=BrewData)

        mem1.port1_write_en <<= self.write_request
        mem1.port1_data_in <<= self.write_data
        mem1.port1_addr <<= self.write_addr
        mem2.port1_write_en <<= self.write_request
        mem2.port1_data_in <<= self.write_data
        mem2.port1_addr <<= self.write_addr

        # Read ports have bypass logic
        self.read1_data <<= Select(
            (self.write_addr == self.read1_addr) & self.write_request,
            mem1.port2_data_out,
            self.write_data
        )
        mem1.port2_addr <<= self.read1_addr

        self.read2_data <<= Select(
            (self.write_addr == self.read2_addr) & self.write_request,
            mem2.port2_data_out,
            self.write_data
        )
        mem2.port2_addr <<= self.read2_addr

        # Score-board for reservations
        rsv_board = Wire(Unsigned(BrewRegCnt))

        clear_mask = Select(self.write_request, 0, 1 << self.write_addr)

        rsv_response = Wire(logic)

        decode_response = Wire(logic)

        set_mask = Select(self.rsv_request & decode_response & self.request, 0, 1 << self.rsv_addr)

        rsv_board <<= Reg(rsv_board & ~clear_mask | set_mask)

        # The logic is this: if we don't have a valid request, we always are providing a response.
        read1_response <<= ~self.read1_valid | (((rsv_board & (1 << self.read1_addr)) == 0) | ((self.write_addr == self.read1_addr) & self.write_request))
        read2_response <<= ~self.read2_valid | (((rsv_board & (1 << self.read2_addr)) == 0) | ((self.write_addr == self.read2_addr) & self.write_request))
        rsv_response <<= ~self.rsv_valid | (((rsv_board & (1 << self.rsv_addr)) == 0) | ((self.write_addr == self.rsv_addr) & self.write_request))

        decode_response <<= read1_response & read2_response & rsv_response

        self.response <<= decode_response
