#!/usr/bin/python3
from typing import *
from silicon import *
from silicon.memory import SimpleDualPortMemory
#sys.path.append(str(Path(__file__).parent))

from .brew_types import *

"""
The register file for Brew consists of a single write and two read ports.

Upon write, both the data and the type can be written (type can be gated)
Upon read, both the data and the type are returned.

A side-band interface provides bulk access to the type information.

Types are stored in flops, while the values are in (a pair of) BRAMs.
"""

class RegFile(Module):
    clk = Input(logic)
    rst = Input(logic)

    write_data = Input(BrewData)
    write_type = Input(BrewRegType)
    write_addr = Input(BrewRegAddr)
    write_type_valid = Input(logic)
    write_data_valid = Input(logic)

    read1_data = Output(BrewData)
    read1_type = Output(BrewRegType)
    read1_addr = Input(BrewRegAddr)

    read2_data = Output(BrewData)
    read2_type = Output(BrewRegType)
    read2_addr = Input(BrewRegAddr)

    bulk_type_write_data = Input(BrewData)
    bulk_type_write_addr = Input(logic)
    bulk_type_write_valid = Input(logic)

    bulk_type_read_data = Output(BrewData)
    bulk_type_read_addr = Input(logic)

    def construct(self):
        # Create the type file
        self.type_file = []
        for i in range(14):
            self.type_file.append(Wire(BrewRegType))


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

        # Hook up the type file
        for idx, type_file_entry in enumerate(self.type_file):
            type_file_entry <<= Reg(Select(
                self.write_type_valid & (self.write_addr == idx),
                type_file_entry,
                Select(
                    self.bulk_type_write_valid & (self.bulk_type_write_addr == (idx & 8)),
                    self.write_type,
                    (self.bulk_type_write_data >> (idx * 4)) & 0xf
                )
            ))

        self.bulk_type_read_data = Select(
            self.bulk_type_read_addr,
            concat(
                self.type_file[7],
                self.type_file[6],
                self.type_file[5],
                self.type_file[4],
                self.type_file[3],
                self.type_file[2],
                self.type_file[1],
                self.type_file[0]
            ),
            concat(
                self.type_file[14],
                self.type_file[13],
                self.type_file[12],
                self.type_file[11],
                self.type_file[10],
                self.type_file[9],
                self.type_file[8]
            )
        )

