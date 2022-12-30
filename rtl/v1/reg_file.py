#!/usr/bin/python3
from typing import *
from silicon import *
from silicon.memory import SimpleDualPortMemory
#sys.path.append(str(Path(__file__).parent))

from .brew_types import *

"""
The register file for Brew consists of a single write and two read ports.

The V1 version doesn't implement types, so only values are provided.

For FPGAs, BRAMs can be used to implement the register file.
"""

class RegFile(Module):
    clk = Input(logic)
    rst = Input(logic)

    write_data = Input(BrewData)
    write_addr = Input(BrewRegAddr)
    write_data_valid = Input(logic)

    read1_addr = Input(BrewRegAddr)
    read1_data = Output(BrewData)

    read2_addr = Input(BrewRegAddr)
    read2_data = Output(BrewData)

    def body(self):
        # We have two memory instances, one for each read port. The write ports of
        # these instances are connected together so they get written the same data
        mem1 = SimpleDualPortMemory(addr_type=BrewRegAddr, data_type=BrewData)
        mem2 = SimpleDualPortMemory(addr_type=BrewRegAddr, data_type=BrewData)

        mem1.port1_write_en <<= self.write_data_valid
        mem1.port1_data_in <<= self.write_data
        mem1.port1_addr <<= self.write_addr
        mem2.port1_write_en <<= self.write_data_valid
        mem2.port1_data_in <<= self.write_data
        mem2.port1_addr <<= self.write_addr

        # Read ports have bypass logic
        self.read1_data <<= Select(
            (self.write_addr == self.read1_addr) & self.write_data_valid,
            mem1.port2_data_out,
            self.write_data
        )
        mem1.port2_addr <<= self.read1_addr

        self.read2_data <<= Select(
            (self.write_addr == self.read2_addr) & self.write_data_valid,
            mem2.port2_data_out,
            self.write_data
        )
        mem2.port2_addr <<= self.read2_addr
